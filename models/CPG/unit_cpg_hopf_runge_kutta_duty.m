
# Hopf oscillator
# Approximation by fourth order Runge-Kutta
# Duty factor

Tsw_STEPS = 10  # Number of iterations in T_swing period
TURNS = 2

alpha = 1;
mu = 1;
beta = 0.8333;
# Duty factor: 0.5     for tripod gait
#              0.8333  for wave gait

a = 10000;

x = 0;
y = -0.01;

function [dx, dy] = F (alpha, mu, omega, x, y)
  dx = alpha * (mu - x^2 - y^2) * x - omega * y;
  dy = alpha * (mu - x^2 - y^2) * y + omega * x;
endfunction

fx=[];
fy=[];
fdx=[];
fdy=[];

for i = 1:(Tsw_STEPS*TURNS/(1-beta))

  omega = pi*(1 - beta) / (beta * (e^(-a*y) + 1) * Tsw_STEPS) ...
            + pi / ((e^(a*y) + 1) * Tsw_STEPS);
  fx(i) = x;
  fy(i) = y;

  # Аппроксимация Рунге-Кутта
  [K1dx, K1dy] = F(alpha, mu, omega, x, y);
  [K2dx, K2dy] = F(alpha, mu, omega, x + K1dx/2, y + K1dy/2);
  [K3dx, K3dy] = F(alpha, mu, omega, x + K2dx/2, y + K2dy/2);
  [K4dx, K4dy] = F(alpha, mu, omega, x + K3dx, y + K3dy);

  dx = (K1dx + 2*K2dx + 2*K3dx + K4dx) / 6;
  dy = (K1dy + 2*K2dy + 2*K3dy + K4dy) / 6;
  x = x + dx;
  y = y + dy;

  fdx(i) = dx + 2;
  fdy(i) = dy + 2;
endfor

fx(i+1) = x;
fy(i+1) = y;

##plot(fx, fy)
plot(fx, "", fy, "", fdx, "", fdy)
grid on
