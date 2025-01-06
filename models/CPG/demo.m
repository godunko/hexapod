
x = []
y = []

#function result = ud_n (u, v, t)
#   result = sigma * (r^2 - u^2 - v^2) - 2* pi * omega * v;
#endfunction

sigma = 1.0;
r = 1.0;
beta = 5/6;
#omega = 1/100
N = 1;

u = 0.01;
v = 0.01;
dt = 0.1;

STEPS = 10000

for i = 1:STEPS

  if (i == floor (STEPS / 4))
    beta = 2/3
  elseif (i == floor (2 * STEPS / 4))
    beta = 1/2;
  elseif (i == floor (3 * STEPS / 4))
##    beta = 1/2;
    r = -0.1;
  endif

  if (v >= 0)
    omega = N / (120*(1-beta));

  else
    omega = N / (120*beta);
  endif

##  du = sigma * (r^2 - u^2 - v^2) * u - 2 * pi * omega * v;
##  dv = sigma * (r^2 - u^2 - v^2) * v + 2 * pi * omega * u;
  du = sigma * (r - u^2 - v^2) * u - 2 * pi * omega * v;
  dv = sigma * (r - u^2 - v^2) * v + 2 * pi * omega * u;
  u = u + du * dt;
  v = v + dv * dt;
##  u = u + du * omega;
##  v = v + dv * omega;

  x(i) = u;
  y(i) = v;
endfor

#plot(x, "", y);
plot(x);
#plot(x, y, 'o-r');

