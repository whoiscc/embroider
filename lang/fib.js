function fib(n) {
    if (n < 2) {
        return n;
    } else {
        return fib(n - 1) + fib(n - 2);
    }
}

const start = Date.now() / 1000;
console.log("fib(35) = " + fib(35));
console.log(Date.now() / 1000 - start);