
const console_log = (value: number) => {
    console.log(value);
};
const fib=(n:number):number=>{if(n<=1){return n;}return fib(n-1)+fib(n-2);};console_log(fib(40));