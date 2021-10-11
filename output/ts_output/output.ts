
const console_log = (value: number) => {
    console.log(value);
};

const performance_now = () => {
    return performance.now();
};
const fib=(n:number):number=>{if(n<=1){return n;}return fib(n-1)+fib(n-2);};console_log(performance_now());console_log(fib(40));console_log(performance_now());