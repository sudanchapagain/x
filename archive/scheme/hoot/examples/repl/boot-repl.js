let hoot = await import("../../reflect-js/reflect.js");
let [proc] = await hoot.Scheme.load_main("repl.wasm", {
  reflect_wasm_dir: "."
});
proc.call_async();
