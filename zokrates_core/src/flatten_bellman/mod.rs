//! Module containing the `Flattener` to process a program that it is R1CS-able.
//!
//! @file flatten.rs
//! @author Dennis Kuhnert <dennis.kuhnert@campus.tu-berlin.de>
//! @author Jacob Eberhardt <jacob.eberhardt@tu-berlin.de>
//! @date 2017

const BINARY_SEPARATOR: &str = "_b";

use pairing::Engine;
use bellman_absy::LinCombMemory;
use bellman_absy::number_to_fr;
use field::Bls12Field;
use pairing::bls12_381::Bls12;
use bellman_absy::Program;
use types::Signature;
use types::conversions_bellman::cast;
use absy::variable::Variable;
use std::collections::{HashSet, HashMap};
use typed_absy::*;
use field::*;
use flat_absy_bellman::*;
use flat_absy_bellman::flat_parameter::FlatParameter;
use substitution::direct_substitution::DirectSubstitution;
use substitution::Substitution;
use helpers::{BellmanDirectiveStatement, BellmanHelper, RustHelper, RankOneHelper};
use types::Type;
use absy::parameter::Parameter;

use num::{One, Zero};


/// Flattener, computes flattened program.
pub struct Flattener {
    /// Number of bits needed to represent the maximum value.
    bits: usize,
    /// Vector containing all used variables while processing the program.
    variables: HashSet<String>,
    /// Map of renamings for reassigned variables while processing the program.
    substitution: DirectSubstitution,
    /// Map of function id to invocation counter
    function_calls: HashMap<String, usize>,
    /// Index of the next introduced variable while processing the program.
    next_var_idx: usize,
}
impl Flattener {
    /// Returns a `Flattener` with fresh a fresh [substitution] and [variables].
    ///
    /// # Arguments
    ///
    /// * `bits` - Number of bits needed to represent the maximum value.
    pub fn new(bits: usize) -> Flattener {
        Flattener {
            bits: bits,
            variables: HashSet::new(),
            substitution: DirectSubstitution::new(),
            function_calls: HashMap::new(),
            next_var_idx: 0,
        }
    }

    /// Loads the standard library
    fn load_stdlib(
        &mut self,
        functions_flattened: &mut Vec<FlatFunction>,
    ) -> () {

        // Load type casting functions
        functions_flattened.push(cast(&Type::Boolean, &Type::FieldElement));
        functions_flattened.push(cast(&Type::FieldElement, &Type::Boolean));

        // Load IfElse helpers
        let ie = TypedFunction {
            id: "_if_else_field".to_string(),
            arguments: vec![Parameter {
                id: Variable {
                    id: "condition".to_string(),
                    _type: Type::Boolean
                }, 
                private: true 
            },
            Parameter {
                id: Variable {
                    id: "consequence".to_string(),
                    _type: Type::FieldElement
                }, 
                private: true 
            },
            Parameter {
                id: Variable {
                    id: "alternative".to_string(),
                    _type: Type::FieldElement
                }, 
                private: true 
            }],
            statements: vec![
                TypedStatement::Definition(
                    Variable::from("condition_as_field"),
                    FieldElementExpression::FunctionCall(
                        "_bool_to_field".to_string(), 
                        vec![
                            TypedExpression::FieldElement(
                                FieldElementExpression::Identifier("condition".to_string()))
                        ]
                    ).into()
                ),
                TypedStatement::Return(
                    vec![
                        FieldElementExpression::Add(
                            box FieldElementExpression::Mult(
                                box FieldElementExpression::Identifier("condition_as_field".to_string()),
                                box FieldElementExpression::Identifier("consequence".to_string()),
                            ),
                            box FieldElementExpression::Mult(
                                box FieldElementExpression::Sub(
                                    box FieldElementExpression::Number(Bls12Field::one()),
                                    box FieldElementExpression::Identifier("condition_as_field".to_string())
                                ),
                                box FieldElementExpression::Identifier("alternative".to_string())
                            )
                        ).into()
                    ]
                )
            ],
            signature: Signature {
                inputs: vec![Type::Boolean, Type::FieldElement, Type::FieldElement],
                outputs: vec![Type::FieldElement]
            }
        };

        let ief = self.flatten_function(
            functions_flattened,
            ie,
        );
        functions_flattened.push(ief);
    }

    /// Returns (condition true, condition false) `Identifier`s for the given condition.
    /// condition true = 1, if `condition` is true, 0 else
    /// condition false = 1, if `condition` is false, 0 else
    ///
    /// # Arguments
    ///
    /// * `statements_flattened` - Vector where new flattened statements can be added.
    /// * `condition` - `Condition` that will be flattened.
    fn flatten_condition(
        &mut self,
        functions_flattened: &Vec<FlatFunction>,
        arguments_flattened: &Vec<FlatParameter>,
        statements_flattened: &mut Vec<FlatStatement>,
        condition: BooleanExpression<Bls12Field>,
    ) -> BooleanExpression<Bls12Field> { // those will be booleans in the future
        match condition {
            BooleanExpression::Lt(box lhs, box rhs) => {

                // We know from semantic checking that lhs and rhs have the same type
                // What the condition will flatten to depends on that type

                let lhs_flattened = self.flatten_field_expression(
                    functions_flattened,
                    arguments_flattened,
                    statements_flattened,
                    lhs,
                );
                let rhs_flattened = self.flatten_field_expression(
                    functions_flattened,
                    arguments_flattened,
                    statements_flattened,
                    rhs,
                );

                // lhs
                let lhs_name = format!("sym_{}", self.next_var_idx);
                self.next_var_idx += 1;
                statements_flattened
                    .push(FlatStatement::Definition(lhs_name.to_string(), lhs_flattened));

                // rhs
                let rhs_name = format!("sym_{}", self.next_var_idx);
                self.next_var_idx += 1;
                statements_flattened
                    .push(FlatStatement::Definition(rhs_name.to_string(), rhs_flattened));

                // check that lhs and rhs are within the right range, ie, their last two bits are zero

                // lhs
                {
                    // bitness checks
                    for i in 0..self.bits - 2 {
                        let new_name = format!("{}{}{}", &lhs_name, BINARY_SEPARATOR, i);
                        statements_flattened.push(FlatStatement::Condition(
                            FlatExpression::Identifier(new_name.to_string()),
                            FlatExpression::Mult(
                                box FlatExpression::Identifier(new_name.to_string()),
                                box FlatExpression::Identifier(new_name.to_string()),
                            ),
                        ));
                    }

                    // bit decomposition check
                    let mut lhs_sum = FlatExpression::Number(Bls12Field::from(0));

                    for i in 0..self.bits - 2 {
                        lhs_sum = FlatExpression::Add(
                            box lhs_sum,
                            box FlatExpression::Mult(
                                box FlatExpression::Identifier(format!("{}{}{}", &lhs_name, BINARY_SEPARATOR, i)),
                                box FlatExpression::Number(Bls12Field::from(2).pow(i)),
                            ),
                        );
                    }

                    statements_flattened
                        .push(FlatStatement::Condition(
                                FlatExpression::Identifier(lhs_name.clone()), 
                                lhs_sum
                            )
                        );
                }

                // rhs
                {
                    // bitness checks
                    for i in 0..self.bits - 2 {
                        let new_name = format!("{}{}{}", &rhs_name, BINARY_SEPARATOR, i);
                        statements_flattened.push(FlatStatement::Condition(
                            FlatExpression::Identifier(new_name.to_string()),
                            FlatExpression::Mult(
                                box FlatExpression::Identifier(new_name.to_string()),
                                box FlatExpression::Identifier(new_name.to_string()),
                            ),
                        ));
                    }

                    // bit decomposition check
                    let mut rhs_sum = FlatExpression::Number(Bls12Field::from(0));

                    for i in 0..self.bits - 2 {
                        rhs_sum = FlatExpression::Add(
                            box rhs_sum,
                            box FlatExpression::Mult(
                                box FlatExpression::Identifier(format!("{}{}{}", &rhs_name, BINARY_SEPARATOR, i)),
                                box FlatExpression::Number(Bls12Field::from(2).pow(i)),
                            ),
                        );
                    }

                    statements_flattened
                        .push(FlatStatement::Condition(
                                FlatExpression::Identifier(rhs_name.clone()), 
                                rhs_sum
                            )
                        );
                }

                // sym = (lhs * 2) - (rhs * 2)
                let subtraction_result = format!("sym_{}", self.next_var_idx);
                self.next_var_idx += 1;

                statements_flattened.push(FlatStatement::Definition(
                    subtraction_result.to_string(),
                    FlatExpression::Sub(
                        box FlatExpression::Mult(box FlatExpression::Number(Bls12Field::from(2)), box FlatExpression::Identifier(lhs_name.to_string())),
                        box FlatExpression::Mult(box FlatExpression::Number(Bls12Field::from(2)), box FlatExpression::Identifier(rhs_name.to_string())),
                    ),
                ));

                // sym_b{i} = sym_b{i}**2  (bitness checks)
                for i in 0..self.bits {
                    let new_name = format!("{}{}{}", &subtraction_result, BINARY_SEPARATOR, i);
                    statements_flattened.push(FlatStatement::Condition(
                        FlatExpression::Identifier(new_name.to_string()),
                        FlatExpression::Mult(
                            box FlatExpression::Identifier(new_name.to_string()),
                            box FlatExpression::Identifier(new_name.to_string()),
                        ),
                    ));
                }

                // sum(sym_b{i} * 2**i)
                let mut expr = FlatExpression::Number(Bls12Field::from(0));

                for i in 0..self.bits {
                    expr = FlatExpression::Add(
                        box expr,
                        box FlatExpression::Mult(
                            box FlatExpression::Identifier(format!("{}{}{}", &subtraction_result, BINARY_SEPARATOR, i)),
                            box FlatExpression::Number(Bls12Field::from(2).pow(i)),
                        ),
                    );
                }

                statements_flattened
                    .push(FlatStatement::Condition(
                            FlatExpression::Identifier(subtraction_result.clone()), 
                            expr
                        )
                    );

                // rename output to avoid conflicts with suffixes
                let cond_true = format!("sym_{}", self.next_var_idx);
                self.next_var_idx += 1;
                statements_flattened
                    .push(FlatStatement::Definition(
                        cond_true.clone(),
                        FlatExpression::Identifier(format!("{}{}{}", &subtraction_result, BINARY_SEPARATOR, 0)))
                    );

                self.next_var_idx += 1;

                BooleanExpression::Identifier(cond_true)
            }
            BooleanExpression::Eq(box lhs, box rhs) => {

                // We know from semantic checking that lhs and rhs have the same type
                // What the condition will flatten to depends on that type

                // Wanted: (Y = (X != 0) ? 1 : 0)
                // X = a - b
                // # Y = if X == 0 then 0 else 1 fi
                // # M = if X == 0 then 1 else 1/X fi
                // Y == X * M
                // 0 == (1-Y) * X

                let name_x = format!("sym_{}", self.next_var_idx);
                self.next_var_idx += 1;
                let name_y = format!("sym_{}", self.next_var_idx);
                self.next_var_idx += 1;
                let name_m = format!("sym_{}", self.next_var_idx);
                self.next_var_idx += 1;
                let name_1_y = format!("sym_{}", self.next_var_idx);
                self.next_var_idx += 1;

                let x = self.flatten_field_expression(
                    functions_flattened,
                    arguments_flattened,
                    statements_flattened,
                    FieldElementExpression::Sub(box lhs, box rhs),
                );

                statements_flattened.push(FlatStatement::Definition(name_x.to_string(), x));
                statements_flattened.push(
                    FlatStatement::Directive(BellmanDirectiveStatement {
                        outputs: vec![name_y.to_string(), name_m.to_string()],
                        helper: BellmanHelper::Rust(RustHelper::ConditionEq(name_x.to_string()))
                    })
                );
                statements_flattened.push(FlatStatement::Condition(
                    FlatExpression::Identifier(name_y.to_string()),
                    FlatExpression::Mult(box FlatExpression::Identifier(name_x.to_string()), box FlatExpression::Identifier(name_m)),
                ));
                statements_flattened.push(FlatStatement::Definition(
                    name_1_y.to_string(),
                    FlatExpression::Sub(box FlatExpression::Number(Bls12Field::one()), box FlatExpression::Identifier(name_y.to_string())),
                ));
                statements_flattened.push(FlatStatement::Condition(
                    FlatExpression::Number(Bls12Field::zero()),
                    FlatExpression::Mult(box FlatExpression::Identifier(name_1_y.to_string()), box FlatExpression::Identifier(name_x)),
                ));

                BooleanExpression::Identifier(name_1_y)
            }
            _ => unimplemented!(),
        }
    }

    fn flatten_function_call(
        &mut self,
        functions_flattened: &Vec<FlatFunction>,
        arguments_flattened: &Vec<FlatParameter>,
        statements_flattened: &mut Vec<FlatStatement>,
        id: &String,
        return_count: usize,
        param_expressions: &Vec<TypedExpression<Bls12Field>>
    ) -> FlatExpressionList {
        for funct in functions_flattened {
            if funct.id == *id && funct.arguments.len() == (*param_expressions).len() && funct.return_count == return_count {
                match self.function_calls.clone().get(&funct.id) {
                    Some(val) => {
                        self.function_calls.insert(funct.id.clone(),val+1);
                    }
                    None => {
                        self.function_calls.insert(funct.id.clone(),1);
                    }
                }
                let prefix = format!("{}_i{}o{}_{}_", funct.id.clone(), funct.arguments.len(), funct.return_count, self.function_calls.get(&funct.id).unwrap());

                // Handle complex parameters and assign values:
                // Rename Parameters, assign them to values in call. Resolve complex expressions with definitions
                let flat_params = param_expressions.clone().into_iter().enumerate().map(|(i, param_expr)| {
                    let new_var;
                    let param_expr = param_expr.apply_substitution(&self.substitution);

                    match param_expr {
                        TypedExpression::FieldElement(e) => {
                            match e {
                                FieldElementExpression::Identifier(id) => {
                                    new_var = format!("{}param_{}", &prefix, id);
                                    statements_flattened
                                        .push(FlatStatement::Definition(new_var.clone(), FlatExpression::Identifier(id.clone().to_string())));
                                },
                                _ => {
                                    // for field elements, flatten the input and assign it to a new variable
                                    let expr_subbed = e.apply_substitution(&self.substitution);
                                    let rhs = self.flatten_field_expression(
                                        functions_flattened,
                                        arguments_flattened,
                                        statements_flattened,
                                        expr_subbed,
                                    );
                                    new_var = format!("{}param_{}", &prefix, i);
                                    statements_flattened
                                        .push(FlatStatement::Definition(new_var.clone(), rhs));
                                }
                            }
                        },
                        TypedExpression::Boolean(e) => {
                            match e {
                                BooleanExpression::Identifier(id) => {
                                    new_var = format!("{}param_{}", &prefix, id);
                                    statements_flattened
                                        .push(FlatStatement::Definition(new_var.clone(), FlatExpression::Identifier(id.clone().to_string())));
                                },
                                _ => panic!("A boolean argument to a function has to be a identifier")
                            }
                        }
                    }
                    FlatExpression::Identifier(new_var)
                }).collect();

                // funct is now the called function
                statements_flattened.push(
                    FlatStatement::FunctionCall(
                        (0..funct.return_count).map(|index| format!("{}{}", prefix, index)).collect(),
                        funct.id.clone(),
                        flat_params
                    )
                );
                return FlatExpressionList {
                    expressions: (0..funct.return_count).map(|index| format!("{}{}", prefix, index)).map(|x| FlatExpression::Identifier(x)).collect()
                }
            }
        }
        panic!(
            "TypedFunction definition for function {} with {:?} argument(s) not found.",
            id,
            param_expressions
        );
    }

    fn flatten_field_expression(
        &mut self,
        functions_flattened: &Vec<FlatFunction>,
        arguments_flattened: &Vec<FlatParameter>,
        statements_flattened: &mut Vec<FlatStatement>,
        expr: FieldElementExpression<Bls12Field>,
    ) -> FlatExpression {
        match expr {
            FieldElementExpression::Number(x) => FlatExpression::Number(x), // force to be a field element
            FieldElementExpression::Identifier(x) => FlatExpression::Identifier(x),
            FieldElementExpression::Add(box left, box right) => {
                let left_flattened = self.flatten_field_expression(
                    functions_flattened,
                    arguments_flattened,
                    statements_flattened,
                    left,
                );
                let right_flattened = self.flatten_field_expression(
                    functions_flattened,
                    arguments_flattened,
                    statements_flattened,
                    right,
                );
                let new_left = if left_flattened.is_linear() {
                    left_flattened
                } else {
                    let new_name = format!("sym_{}", self.next_var_idx);
                    self.next_var_idx += 1;
                    statements_flattened
                        .push(FlatStatement::Definition(new_name.to_string(), left_flattened));
                    FlatExpression::Identifier(new_name)
                };
                let new_right = if right_flattened.is_linear() {
                    right_flattened
                } else {
                    let new_name = format!("sym_{}", self.next_var_idx);
                    self.next_var_idx += 1;
                    statements_flattened
                        .push(FlatStatement::Definition(new_name.to_string(), right_flattened));
                    FlatExpression::Identifier(new_name)
                };
                FlatExpression::Add(box new_left, box new_right)
            },
            FieldElementExpression::Sub(box left, box right) => {
                let left_flattened = self.flatten_field_expression(
                    functions_flattened,
                    arguments_flattened,
                    statements_flattened,
                    left,
                );
                let right_flattened = self.flatten_field_expression(
                    functions_flattened,
                    arguments_flattened,
                    statements_flattened,
                    right,
                );
                let new_left = if left_flattened.is_linear() {
                    left_flattened
                } else {
                    let new_name = format!("sym_{}", self.next_var_idx);
                    self.next_var_idx += 1;
                    statements_flattened
                        .push(FlatStatement::Definition(new_name.to_string(), left_flattened));
                    FlatExpression::Identifier(new_name)
                };
                let new_right = if right_flattened.is_linear() {
                    right_flattened
                } else {
                    let new_name = format!("sym_{}", self.next_var_idx);
                    self.next_var_idx += 1;
                    statements_flattened
                        .push(FlatStatement::Definition(new_name.to_string(), right_flattened));
                    FlatExpression::Identifier(new_name)
                };
                FlatExpression::Sub(box new_left, box new_right)
            },
            FieldElementExpression::Mult(box left, box right) => {
                let left_flattened = self.flatten_field_expression(
                    functions_flattened,
                    arguments_flattened,
                    statements_flattened,
                    left,
                );
                let right_flattened = self.flatten_field_expression(
                    functions_flattened,
                    arguments_flattened,
                    statements_flattened,
                    right,
                );
                let new_left = if left_flattened.is_linear() {
                    if let FlatExpression::Sub(..) = left_flattened {
                        let new_name = format!("sym_{}", self.next_var_idx);
                        self.next_var_idx += 1;
                        statements_flattened
                            .push(FlatStatement::Definition(new_name.to_string(), left_flattened));
                        FlatExpression::Identifier(new_name)
                    } else {
                        left_flattened
                    }
                } else {
                    let new_name = format!("sym_{}", self.next_var_idx);
                    self.next_var_idx += 1;
                    statements_flattened
                        .push(FlatStatement::Definition(new_name.to_string(), left_flattened));
                    FlatExpression::Identifier(new_name)
                };
                let new_right = if right_flattened.is_linear() {
                    if let FlatExpression::Sub(..) = right_flattened {
                        let new_name = format!("sym_{}", self.next_var_idx);
                        self.next_var_idx += 1;
                        statements_flattened
                            .push(FlatStatement::Definition(new_name.to_string(), right_flattened));
                        FlatExpression::Identifier(new_name)
                    } else {
                        right_flattened
                    }
                } else {
                    let new_name = format!("sym_{}", self.next_var_idx);
                    self.next_var_idx += 1;
                    statements_flattened
                        .push(FlatStatement::Definition(new_name.to_string(), right_flattened));
                    FlatExpression::Identifier(new_name)
                };
                FlatExpression::Mult(box new_left, box new_right)
            },
            FieldElementExpression::Div(box left, box right) => {
                let left_flattened = self.flatten_field_expression(
                    functions_flattened,
                    arguments_flattened,
                    statements_flattened,
                    left,
                );
                let right_flattened = self.flatten_field_expression(
                    functions_flattened,
                    arguments_flattened,
                    statements_flattened,
                    right,
                );
                let new_left = {
                    let new_name = format!("sym_{}", self.next_var_idx);
                    self.next_var_idx += 1;
                    statements_flattened
                        .push(FlatStatement::Definition(new_name.to_string(), left_flattened));
                    FlatExpression::Identifier(new_name)
                };
                let new_right = {
                    let new_name = format!("sym_{}", self.next_var_idx);
                    self.next_var_idx += 1;
                    statements_flattened
                        .push(FlatStatement::Definition(new_name.to_string(), right_flattened));
                    FlatExpression::Identifier(new_name)
                };
                FlatExpression::Div(box new_left, box new_right)
            },
            FieldElementExpression::Pow(box base, box exponent) => {
                // TODO currently assuming that base is number or variable
                match exponent {
                    FieldElementExpression::Number(ref x) if x > &Bls12Field::one() => match base {
                        FieldElementExpression::Identifier(ref var) => {
                            let id = if x > &Bls12Field::from(2) {
                                let tmp_expression = self.flatten_field_expression(
                                    functions_flattened,
                                    arguments_flattened,
                                    statements_flattened,
                                    FieldElementExpression::Pow(
                                        box FieldElementExpression::Identifier(var.to_string()),
                                        box FieldElementExpression::Number(x.clone() - Bls12Field::one()),
                                    ),
                                );
                                let new_name = format!("sym_{}", self.next_var_idx);
                                self.next_var_idx += 1;
                                statements_flattened.push(
                                    FlatStatement::Definition(new_name.to_string(), tmp_expression),
                                );
                                new_name
                            } else {
                                var.to_string()
                            };
                            FlatExpression::Mult(
                                box FlatExpression::Identifier(id.to_string()),
                                box FlatExpression::Identifier(var.to_string()),
                            )
                        }
                        FieldElementExpression::Number(var) => FlatExpression::Mult(box FlatExpression::Number(var.clone()), box FlatExpression::Number(var)),
                        _ => panic!("Only variables and numbers allowed in pow base"),
                    },
                    _ => panic!("Expected number > 1 as pow exponent"),
                }
            },
            FieldElementExpression::IfElse(box condition, box consequent, box alternative) => {
                let condition = self.flatten_condition(
                    functions_flattened,
                    arguments_flattened,
                    statements_flattened,
                    condition,
                );

                self.flatten_function_call(
                    functions_flattened,
                    arguments_flattened,
                    statements_flattened,
                    &"_if_else_field".to_string(),
                    1,
                    &vec![condition.into(), consequent.into(), alternative.into()],
                ).expressions[0].clone()
            },
            FieldElementExpression::FunctionCall(ref id, ref param_expressions) => {
                let exprs_flattened = self.flatten_function_call(
                    functions_flattened,
                    arguments_flattened,
                    statements_flattened,
                    id,
                    1,
                    param_expressions
                );
                assert!(exprs_flattened.expressions.len() == 1); // outside of MultipleDefinition, FunctionCalls must return a single value
                exprs_flattened.expressions[0].clone()
            }
        }
    }

    pub fn flatten_statement(
        &mut self,
        functions_flattened: &mut Vec<FlatFunction>,
        arguments_flattened: &Vec<FlatParameter>,
        statements_flattened: &mut Vec<FlatStatement>,
        stat: TypedStatement<Bls12Field>,
    ) {
        match stat {
            TypedStatement::Return(exprs) => {

                let flat_expressions = exprs.into_iter().map(|expr| {
                    match expr {
                        TypedExpression::FieldElement(e) => {
                            let expr_subbed = e.apply_substitution(&self.substitution);
                            self.flatten_field_expression(
                                functions_flattened,
                                arguments_flattened,
                                statements_flattened,
                                expr_subbed,
                            )
                        },
                        _ => panic!("Functions can only return expressions of type FieldElement")
                    }
                }).collect();

                statements_flattened.push(
                    FlatStatement::Return(
                        FlatExpressionList {
                            expressions: flat_expressions
                        }
                    )
                );
            }
            TypedStatement::Definition(v, expr) => {

                // define n variables with n the number of primitive types for v_type
                // assign them to the n primitive types for expr

                match expr {
                    TypedExpression::FieldElement(expr) => {
                        let expr_subbed = expr.apply_substitution(&self.substitution);
                        let rhs = self.flatten_field_expression(
                            functions_flattened,
                            arguments_flattened,
                            statements_flattened,
                            expr_subbed,
                        );
                        let var = self.use_variable(&v.id);
                        // handle return of function call
                        let var_to_replace = self.get_latest_var_substitution(&v.id);
                        if !(var == var_to_replace) && self.variables.contains(&var_to_replace) && !self.substitution.contains_key(&var_to_replace){
                            self.substitution.insert(var_to_replace.clone().to_string(),var.clone());
                        }

                        statements_flattened.push(FlatStatement::Definition(var, rhs));
                    },
                    _ => panic!("Definitions must have type FieldElement")
                }
            }
            TypedStatement::Condition(expr1, expr2) => {

                // flatten expr1 and expr2 to n flattened expressions with n the number of primitive types for expr1
                // add n conditions to check equality of the n expressions

                match (expr1, expr2) {
                    (TypedExpression::FieldElement(e1), TypedExpression::FieldElement(e2)) => {

                        let e1_subbed = e1.apply_substitution(&self.substitution);
                        let e2_subbed = e2.apply_substitution(&self.substitution);
                        
                        let (lhs, rhs) = if e1_subbed.is_linear() {
                            (
                                self.flatten_field_expression(
                                    functions_flattened,
                                    arguments_flattened,
                                    statements_flattened,
                                    e1_subbed
                                ),
                                self.flatten_field_expression(
                                    functions_flattened,
                                    arguments_flattened,
                                    statements_flattened,
                                    e2_subbed,
                                ),
                            )
                        } else if e2_subbed.is_linear() {
                            (
                                self.flatten_field_expression(
                                    functions_flattened,
                                    arguments_flattened,
                                    statements_flattened,
                                    e2_subbed,
                                ),
                                self.flatten_field_expression(
                                    functions_flattened,
                                    arguments_flattened,
                                    statements_flattened,
                                    e1_subbed,
                                ),
                            )
                        } else {
                            unimplemented!()
                        };
                        statements_flattened.push(FlatStatement::Condition(lhs, rhs));
                    },
                    _ => panic!("Conditions (Assertions) must be applied to expressions of type FieldElement")
                }
            }
            TypedStatement::For(var, start, end, statements) => {
                let mut current = start;
                while current < end {
                    statements_flattened.push(FlatStatement::Definition(
                        self.use_variable(&var.id),
                        FlatExpression::Number(current.clone()),
                    ));
                    for s in statements.clone() {
                        self.flatten_statement(
                            functions_flattened,
                            arguments_flattened,
                            statements_flattened,
                            s,
                        );
                    }
                    current = Bls12Field::one() + &current;
                }
            }
            TypedStatement::MultipleDefinition(vars, rhs) => {

                // flatten the right side to p = sum(var_i.type.primitive_count) expressions
                // define p new variables to the right side expressions 

                let rhs_subbed = rhs.apply_substitution(&self.substitution);
                
                match rhs_subbed {
                    TypedExpressionList::FunctionCall(fun_id, exprs, types) => {
                        let rhs_flattened = self.flatten_function_call(
                            functions_flattened,
                            arguments_flattened,
                            statements_flattened,
                            &fun_id,
                            vars.len(),
                            &exprs,
                        );

                        for (i, v) in vars.into_iter().enumerate() {
                            let var_type = &types[i];

                            match var_type {
                                Type::FieldElement => {
                                    let var = self.use_variable(&v.id);
                                    // handle return of function call
                                    let var_to_replace = self.get_latest_var_substitution(&v.id);
                                    if !(var == var_to_replace) && self.variables.contains(&var_to_replace) && !self.substitution.contains_key(&var_to_replace){
                                        self.substitution.insert(var_to_replace.clone().to_string(),var.clone());
                                    }
                                    statements_flattened.push(FlatStatement::Definition(var, rhs_flattened.expressions[i].clone()));
                                },
                                _ => panic!("MultipleDefinition has to define expressions of type FieldElement")
                            }
                        }
                    },
                }
            },
        }
    }

    /// Returns a flattened `TypedFunction` based on the given `funct`.
    ///
    /// # Arguments
    ///
    /// * `functions_flattened` - Vector where new flattened functions can be added.
    /// * `funct` - `TypedFunction` that will be flattened.
    pub fn flatten_function(
        &mut self,
        functions_flattened: &mut Vec<FlatFunction>,
        funct: TypedFunction<Bls12Field>,
    ) -> FlatFunction {
        self.variables = HashSet::new();
        self.substitution = DirectSubstitution::new();
        self.next_var_idx = 0;
        let mut arguments_flattened: Vec<FlatParameter> = Vec::new();
        let mut statements_flattened: Vec<FlatStatement> = Vec::new();
        // push parameters
        for arg in &funct.arguments {
            let arg_type = arg.id.get_type();

            match arg_type {
                Type::FieldElement => {
                    arguments_flattened.push(FlatParameter {
                        id: self.use_variable(&arg.id.id),
                        private: arg.private
                    });
                },
                Type::Boolean => {
                    arguments_flattened.push(FlatParameter {
                        id: self.use_variable(&arg.id.id), 
                        private: arg.private
                    });
                },
            }
        }
        // flatten statements in functions and apply substitution
        for stat in funct.statements {
            self.flatten_statement(
                functions_flattened,
                &arguments_flattened,
                &mut statements_flattened,
                stat,
            );
        }

        // the flattened return count is the sum of the primitive elements for each type returned
        let return_count = funct.signature.outputs.iter().map(|output_type| output_type.get_primitive_count()).fold(0, |acc, x| acc + x);

        // turn definitions into directive + condition
        let statements_flattened = statements_flattened.into_iter().map(|s| match s {
            FlatStatement::Definition(id, e) => { 

                let (l1, l2) = match e.clone() {
                    FlatExpression::Mult(box e1, box e2) => {
                        (e1.into(), e2.into())
                    },
                    e @ FlatExpression::Identifier(..) => {
                        (LinCombMemory(vec![(number_to_fr::<Bls12>(1), "~one".to_string())]), e.into())
                    },
                    e @ FlatExpression::Add(..) => {
                        (LinCombMemory(vec![(number_to_fr::<Bls12>(1), "~one".to_string())]), e.into())
                    },
                    e @ FlatExpression::Sub(..) => {
                        (LinCombMemory(vec![(number_to_fr::<Bls12>(1), "~one".to_string())]), e.into())
                    }
                    e => panic!("wrong format in constraint: {}", e)
                };

                vec![
                FlatStatement::Directive(BellmanDirectiveStatement {
                    outputs: vec![id.clone()],
                    helper: BellmanHelper::RankOne(RankOneHelper {
                        l1,
                        l2
                    }),
                }),
                FlatStatement::Condition(FlatExpression::Identifier(id), e)
            ]},
            e => vec![e]
        }).flat_map(|x| x).collect();

        FlatFunction {
            id: funct.id.clone(),
            arguments: arguments_flattened,
            statements: statements_flattened,
            return_count: return_count
        }
    }

    /// Returns a flattened `Prog`ram based on the given `prog`.
    ///
    /// # Arguments
    ///
    /// * `prog` - `Prog`ram that will be flattened.
    pub fn flatten_program(&mut self, prog: TypedProg<Bls12Field>) -> Program {
        let mut functions_flattened = Vec::new();

        //self.load_stdlib(&mut functions_flattened);

        // for func in prog.imported_functions {
        //     functions_flattened.push(func);
        // }

        for func in prog.functions {
            let flattened_func = self.flatten_function(&mut functions_flattened, func);
            functions_flattened.push(flattened_func);
        }

        FlatProg {
            functions: functions_flattened
        }.into()
    }


    /// Checks if the given name is a not used variable and returns a fresh variable.
    /// # Arguments
    ///
    /// * `name` - A String that holds the name of the variable
    fn use_variable(&mut self, name: &String) -> String {
        let mut i = 0;
        let mut new_name = name.to_string();
        loop {
            if self.variables.contains(&new_name) {
                new_name = format!("{}_{}", &name, i);
                i += 1;
            } else {
                self.variables.insert(new_name.to_string());
                if i == 1 {
                    self.substitution
                        .insert(name.to_string(), new_name.to_string());
                } else if i > 1 {
                    self.substitution
                        .insert(format!("{}_{}", name, i - 2), new_name.to_string());
                }
                return new_name;
            }
        }
    }

    fn get_latest_var_substitution(&mut self, name: &String)->String{
        let mut latest_var = name.to_string();
        loop {
            match self.substitution.get(&latest_var) {
                Some(x) => latest_var = x.to_string(),
                None => return latest_var,
            }
        }
    }
}

#[cfg(test)]
mod multiple_definition {
    extern crate rand;

    use bellman_absy::Computation;
    use bellman_absy::number_to_fr;
    use super::*;
    // We're going to use the Groth16 proving system.
    use bellman::groth16::{
        create_random_proof, generate_random_parameters, prepare_verifying_key, verify_proof, Proof,
    };
    use field::Bls12Field;
    use types::Type;
    use types::Signature;
    use absy::variable::Variable;

    #[test]
    fn multiple_definition() {

        // def foo()
        //     return 1, 2
        // def main()
        //     a, b = foo()

        let mut flattener = Flattener::new(Bls12Field::get_required_bits());
        let mut functions_flattened = vec![
            FlatFunction {
                id: "foo".to_string(),
                arguments: vec![],
                statements: vec![FlatStatement::Return(
                    FlatExpressionList {
                        expressions: vec![
                            FlatExpression::Number(Bls12Field::from(1)),
                            FlatExpression::Number(Bls12Field::from(2))
                        ]
                    }
                )],
                return_count: 2
            }
        ];
        let arguments_flattened = vec![];
        let mut statements_flattened = vec![];
        let statement = TypedStatement::MultipleDefinition(
            vec![
                Variable::from("a".to_string()),
                Variable::from("b".to_string())
            ],
            TypedExpressionList::FunctionCall("foo".to_string(), vec![], vec![Type::FieldElement, Type::FieldElement])
        );

        flattener.flatten_statement(
            &mut functions_flattened,
            &arguments_flattened,
            &mut statements_flattened,
            statement,
        );

        assert_eq!(
            statements_flattened[0]
            ,
            FlatStatement::FunctionCall(vec!["foo_i0o2_1_0".to_string(), "foo_i0o2_1_1".to_string()], "foo".to_string(), vec![])
        );
    }

    #[test]
    fn multiple_definition2() {

        // def dup(x)
        //     return x, x
        // def main()
        //     a, b = dup(2)

        let mut flattener = Flattener::new(Bls12Field::get_required_bits());
        let mut functions_flattened = vec![
            FlatFunction {
                id: "dup".to_string(),
                arguments: vec![FlatParameter { id: "x".to_string(), private: true }],
                statements: vec![FlatStatement::Return(
                    FlatExpressionList {
                        expressions: vec![
                            FlatExpression::Identifier("x".to_string()),
                            FlatExpression::Identifier("x".to_string()),
                        ]
                    }
                )],
                return_count: 2
            }
        ];
        let arguments_flattened = vec![];
        let mut statements_flattened = vec![];
        let statement = TypedStatement::MultipleDefinition(
            vec![
                Variable::from("a".to_string()),
                Variable::from("b".to_string())
            ],
            TypedExpressionList::FunctionCall("dup".to_string(), vec![TypedExpression::FieldElement(FieldElementExpression::Number(Bls12Field::from(2)))], vec![Type::FieldElement, Type::FieldElement])
        );

        flattener.flatten_statement(
            &mut functions_flattened,
            &arguments_flattened,
            &mut statements_flattened,
            statement,
        );

        assert_eq!(
            statements_flattened[1],
            FlatStatement::FunctionCall(vec!["dup_i1o2_1_0".to_string(), "dup_i1o2_1_1".to_string()], "dup".to_string(), vec![FlatExpression::Identifier("dup_i1o2_1_param_0".to_string())])
        );
    }

    #[test]
    fn simple_definition() {

        // def foo()
        //     return 1
        // def main()
        //     a = foo()

        let mut flattener = Flattener::new(Bls12Field::get_required_bits());
        let mut functions_flattened = vec![
            FlatFunction {
                id: "foo".to_string(),
                arguments: vec![],
                statements: vec![FlatStatement::Return(
                    FlatExpressionList {
                        expressions: vec![
                            FlatExpression::Number(Bls12Field::from(1))
                        ]
                    }
                )],
                return_count: 1         
            }
        ];
        let arguments_flattened = vec![];
        let mut statements_flattened = vec![];
        let statement = TypedStatement::Definition(
            Variable::from("a".to_string()),
            TypedExpression::FieldElement(FieldElementExpression::FunctionCall("foo".to_string(), vec![]))
        );

        flattener.flatten_statement(
            &mut functions_flattened,
            &arguments_flattened,
            &mut statements_flattened,
            statement,
        );

        assert_eq!(
            statements_flattened[0]
            ,
            FlatStatement::FunctionCall(vec!["foo_i0o1_1_0".to_string()], "foo".to_string(), vec![])
        );
    }

    #[test]
    fn redefine_argument() {

        // def foo(a)
        //     a = a + 1
        //     return 1

        // should flatten to no redefinition
        // def foo(a)
        //     a_0 = a + 1
        //     return 1

        let mut flattener = Flattener::new(Bls12Field::get_required_bits());
        let mut functions_flattened = vec![];

        let funct = TypedFunction {
            id: "foo".to_string(),
            signature: Signature {
                inputs: vec![Type::FieldElement],
                outputs: vec![Type::FieldElement]
            },
            arguments: vec![Parameter { id: Variable::from("a"), private: true }],
            statements: vec![
                TypedStatement::Definition(
                    Variable::from("a".to_string()), 
                    FieldElementExpression::Add(
                        box FieldElementExpression::Identifier("a".to_string()), 
                        box FieldElementExpression::Number(Bls12Field::from(1))
                    ).into()
                ),
                TypedStatement::Return(
                    vec![FieldElementExpression::Number(Bls12Field::from(1)).into()]
                )
            ],
        };

        let flat_funct = flattener.flatten_function(
            &mut functions_flattened,
            funct,
        );

        assert_eq!(
            flat_funct.statements[0],
            FlatStatement::Definition("a_0".to_string(), FlatExpression::Add(box FlatExpression::Identifier("a".to_string()), box FlatExpression::Number(Bls12Field::from(1))))
        );
    }

    #[test]
    fn overload() {

        // def foo()
        //      return 1
        // def foo()
        //      return 1, 2
        // def main()
        //      a = foo()
        //      b, c = foo()
        //      return 1
        //
        //      should not panic
        //

        let mut flattener = Flattener::new(Bls12Field::get_required_bits());
        let functions = vec![
            TypedFunction {
                id: "foo".to_string(),
                arguments: vec![],
                statements: vec![TypedStatement::Return(
                    vec![
                            TypedExpression::FieldElement(FieldElementExpression::Number(Bls12Field::from(1)))
                        ]
                )],
                signature: Signature {
                    inputs: vec![],
                    outputs: vec![Type::FieldElement]
                },
            },
            TypedFunction {
                id: "foo".to_string(),
                arguments: vec![],
                statements: vec![TypedStatement::Return(
                        vec![
                            TypedExpression::FieldElement(FieldElementExpression::Number(Bls12Field::from(1))),
                            TypedExpression::FieldElement(FieldElementExpression::Number(Bls12Field::from(2)))
                        ]
                )],
                signature: Signature {
                    inputs: vec![],
                    outputs: vec![Type::FieldElement, Type::FieldElement]
                },
            },
            TypedFunction {
                id: "main".to_string(),
                arguments: vec![],
                statements: vec![
                    TypedStatement::MultipleDefinition(vec![Variable::from("a".to_string())], TypedExpressionList::FunctionCall("foo".to_string(), vec![], vec![Type::FieldElement])),
                    TypedStatement::MultipleDefinition(vec![Variable::from("b".to_string()), Variable::from("c".to_string())], TypedExpressionList::FunctionCall("foo".to_string(), vec![], vec![Type::FieldElement, Type::FieldElement])),
                    TypedStatement::Return(
                        vec![TypedExpression::FieldElement(FieldElementExpression::Number(Bls12Field::from(1)))]
                    )
                ],
                signature: Signature {
                    inputs: vec![],
                    outputs: vec![Type::FieldElement]
                },
            }
        ];

        let res = flattener.flatten_program(
            TypedProg {
                functions: functions,
                imported_functions: vec![],
                imports: vec![]
            }
        );

        // shouldn't panic
    }

    #[test]
    fn if_else() {

        let expression = 
            FieldElementExpression::IfElse(
                box BooleanExpression::Eq(
                    box FieldElementExpression::Number(Bls12Field::from(32)),
                    box FieldElementExpression::Number(Bls12Field::from(4))
                ),
                box FieldElementExpression::Number(Bls12Field::from(12)),
                box FieldElementExpression::Number(Bls12Field::from(51)),
            );


        let mut functions_flattened = vec![];
        let mut flattener = Flattener::new(Bls12Field::get_required_bits());

        flattener.load_stdlib(&mut functions_flattened);

        flattener.flatten_field_expression(
            &functions_flattened,
            &vec![],
            &mut vec![],
            expression
        );
    }

    #[test]
    fn typed_to_proof() {
        use self::rand::thread_rng;

        let rng = &mut thread_rng();

        let functions: Vec<TypedFunction<Bls12Field>> = vec![
            TypedFunction {
                id: "main".to_string(),
                arguments: vec![Parameter { id: Variable::from("x"), private: true }],
                statements: vec![
                    TypedStatement::Definition(
                        Variable::from("y".to_string()),
                        FieldElementExpression::Pow(box FieldElementExpression::Identifier("x".to_string()), box FieldElementExpression::Number(Bls12Field::from(7))).into()
                    ),
                    TypedStatement::Return(
                        vec![TypedExpression::FieldElement(FieldElementExpression::Identifier("y".to_string()))]
                    )
                ],
                signature: Signature {
                    inputs: vec![Type::FieldElement],
                    outputs: vec![Type::FieldElement]
                },
            }
        ];

        let program = TypedProg {
            functions,
            imported_functions: vec![],
            imports: vec![]
        };

        let mut flattener = Flattener::new(Bls12Field::get_required_bits());

        let program = flattener.flatten_program(program);

        let computation = Computation {
            program,
            inputs: vec![number_to_fr::<Bls12>(3)]
        };

        println!("Creating parameters...");

        let params = generate_random_parameters(computation, rng).unwrap();

        // Prepare the verification key (for proof verification)
        let pvk = prepare_verifying_key(&params.vk);

        println!("Creating proofs...");

        let mut proof_vec = vec![];

        let functions: Vec<TypedFunction<Bls12Field>> = vec![
            TypedFunction {
                id: "main".to_string(),
                arguments: vec![Parameter { id: Variable::from("x"), private: true }],
                statements: vec![
                    TypedStatement::Definition(
                        Variable::from("y".to_string()),
                        FieldElementExpression::Pow(box FieldElementExpression::Identifier("x".to_string()), box FieldElementExpression::Number(Bls12Field::from(7))).into()
                    ),
                    TypedStatement::Return(
                        vec![TypedExpression::FieldElement(FieldElementExpression::Identifier("y".to_string()))]
                    )
                ],
                signature: Signature {
                    inputs: vec![Type::FieldElement],
                    outputs: vec![Type::FieldElement]
                },
            }
        ];

        let program = TypedProg {
            functions,
            imported_functions: vec![],
            imports: vec![]
        };

        let mut flattener = Flattener::new(Bls12Field::get_required_bits());

        let program = flattener.flatten_program(program);

        let computation = Computation {
            program,
            inputs: vec![number_to_fr::<Bls12>(3)]
        };

        // Create a groth16 proof with our parameters.
        let proof = create_random_proof(computation, &params, rng).unwrap();

        proof.write(&mut proof_vec).unwrap();

        let proof = Proof::read(&proof_vec[..]).unwrap();

        // Check the proof
        assert!(verify_proof(&pvk, &proof, &[number_to_fr::<Bls12>(2187)]).unwrap());
    }

}
