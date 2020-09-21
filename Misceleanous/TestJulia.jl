using Distributed
using SharedArrays

@everywhere function fib_parallel(n)
    if (n < 40) then
        return fib(n)
    else
        x = @spawn fib_parallel(n-1)
        y = fib_parallel(n-2)
        return fetch(x) + y
    end
end

a=SharedArray{Int64}(15);
@everywhere function basicfunc(n)
    a[n]=n;
end

@time begin
    @sync @distributed for i=1:15
        basicfunc(i);
    end
end
