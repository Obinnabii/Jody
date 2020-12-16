# Jody Demo

Below is a program that demonstrates the core feature of Jody: automemoization. 

Note that `rfib` and `dfib` contain exactly the same body, the only difference is
`rfib` is recursive where as `dfib` is dynamic which means it will store previous
computations and automatically reference them. This is simple for a programmer to
take advantadge of and converts `rfib` from running in exponential time to `dfib`
which runs in linear time. The effect can powerfully be seen in the demo below.
`dfib` will finish quite quickly (the number returned after executing line 3 is
the number of milliseconds the function took to run), where as `rfib` will likely
never finish. If one changes the value 50 to be somewhere in the range 25-30,
one can confirm that for smaller values `rfib` does indeed terminate, although still
much slower than `dfib`. However, as soon as the value increases much past 30, 
`rfib` becomes prohibitively slow and some sort of memoization is required which
is where Jody shines.

### Demo Program
```
let rec rfib(a) = if a <= 2 then 1 else rfib(a-2) + rfib(a-1);;
let dyn dfib(b) = if b <= 2 then 1 else dfib(b-2) + dfib(b-1);;
|-; rfib(50); -|
|-; dfib(50); -|
```
