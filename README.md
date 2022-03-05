# reglico
[WIP]reglico is AltJsAndWasm language. It generate rust(wasm) and typescript code from one reglico code.

## env
rust env  
node env

## how to use

run only first time.
```
sh setup.sh
```

run this every time you change reglico code(e.g. test.reli).
```sh
sh build.sh
sh run.sh
```

## example
transpile `fibonacci number calculation` to typescript(output/ts_output/output.ts) and rust(output/src/lib.rs) from a reglico code(test.reli). 

https://user-images.githubusercontent.com/49422601/137324566-45acbbc2-3c6a-425c-a0da-88af0dc3c0c8.mov

