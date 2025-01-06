

x = []
y = []

sigma = 1.0
r = 1.0
omega = 1/1000

u = 0.01
v = 0.01

for i = 1:1000
  du = sigma * (r^2 - u^2 - v^2) * u - 2 * pi * omega * v
  dv = sigma * (r^2 - u^2 - v^2) * v + 2 * pi * omega * u
  u = u + du
  v = v + dv

  x(i) = u
  y(i) = v
endfor

plot(x, "", y);
#plot(y);
#plot(x, y, 'o-r');

