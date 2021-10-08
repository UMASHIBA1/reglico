cargo run test.reli
cd output/ts_output
npm run build # transpile output.ts to output.js
cd ..
npm run build # compile rust to wasm
cd ..