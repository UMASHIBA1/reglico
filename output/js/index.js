import("../pkg/index.js").catch(console.error);
import("../pkg").then(module => {
    const result = module.fib(BigInt(20));
    console.log("fib(20)", result);
})
