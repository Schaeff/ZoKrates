
// For more comments about what's going on here, check out the `hello_world`
// example
const zokrates = import("./zokrates_wasm_wrapper");

const source = document.getElementById("source");

source.addEventListener("input", event => {
	zokrates.then(zokrates => {
	  zokrates.exec(document.getElementById("source").value);
	});
});