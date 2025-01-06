
x = -1:0.001:1

beta = 0.5
(1 - beta) / beta

function f = f1(x)
  beta = 0.833;
  aux = (1 - beta) / beta;

  f = aux ./ (e.^(-5000*x) + 1);
endfunction

function f = f2(x)
  f = 1 ./ (e.^(5000*x) + 1)
endfunction

##fplot(@f, [-1,1])
plot(x, f1(x), x, f2(x), x, f1(x)+f2(x));
