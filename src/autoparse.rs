#[cfg(test)]
mod tests {
    use zokrates;
    extern crate glob;
    use self::glob::glob;
    use std::fs::File;
    use std::io::{Read, BufReader}; 

    #[test]
    fn parseprogram() {
        let expr = zokrates::progParser::new()
            .parse(r#"
                def main():
                    a = 2 + 3 + foo();
                    return a;
                def foo():
                    return 42;
            "#)
            .unwrap();
        assert_eq!(expr, ());
    }

    #[test]
    fn parse_if_then_else() {
        let expr = zokrates::progParser::new()
            .parse(r#"
                def main():
                    b = 42;
                    a = if b == 33 then 4 else 33 fi;
                    return a;
            "#)
            .unwrap();
        assert_eq!(expr, ());
    }

    #[test]
    fn parse_multidef() {
        let expr = zokrates::progParser::new()
            .parse(r#"
                def main():
                    a, b = foo();
                    return a;
            "#)
            .unwrap();
        assert_eq!(expr, ());
    }

    #[test]
    fn parse_import() {
        let expr = zokrates::progParser::new()
            .parse(r#"
                import "./foo/bar.code" as bar;
                import "./bar/baz.code" as baz

                def main():
                    a, b = foo();
                    return a;
            "#)
            .unwrap();
        assert_eq!(expr, ());
    }

    #[test]
    fn parse_for_loop() {
        let expr = zokrates::progParser::new()
            .parse(r#"
                def main():
                    x = 7;
                    for i in 0..10 do
                        x = x + 1;
                    endfor
            "#)
            .unwrap();
        assert_eq!(expr, ());
    }


    #[test]
    fn parse_power() {
        let expr = zokrates::progParser::new()
            .parse(r#"
                // simple ifelse example
                // x = 2 -> 2 + 1 + 2**3 = 11
                // x = 5 -> 5 + 5 + 5**3 = 135
                def main(x):
                  y = if x < 3 then 1 else 5 fi;
                  z = if y < x then x**3 else y**3 fi;
                  return x + y + z;
            "#)
            .unwrap();
        assert_eq!(expr, ());
    }
}