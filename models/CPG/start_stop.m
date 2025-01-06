
STEPS = 500

M1_STEP = 10
MN_STEP = 400

t = 2

e = 2.71828

alpha = 1
mu = -1
#omega = 0.5

x = 0.001
y = 0.001

a = 100
##omega_st = 1
beta = 0.75
omega_sw = pi / 4

fx=[x]
fy=[y]
fdx=[]
fdy=[]

omega_st = (1 - beta) / beta * omega_sw

for i = 2:STEPS

  if (i == M1_STEP)
    mu = 1

  elseif (i == MN_STEP)
    mu = -1
  endif

  o1 = omega_st / (e ^ (-a*y) + 1)
  o2 = omega_sw / (e ^ (a*y) + 1)
  omega = omega_st / (e ^ (-a*y) + 1) + omega_sw / (e ^ (a*y) + 1)
  omega = omega / t

  dx = alpha * (mu - x^2 - y^2) * x - omega * y;
  dy = alpha * (mu - x^2 - y^2) * y + omega * x;

  dt = 1 / (2 * alpha * abs(mu))
  x = x + dx * dt; # (1 / 2 * alpha * mu)
  y = y + dy * dt; # (1 / 2 * alpha * mu)

  fdx(i-1) = dx + 2;
  fdy(i-1) = dy + 2;
  fx(i) = x;
  fy(i) = y;
endfor

##plot(fx, fy)
plot(fx, "", fy, "", fdx)
grid on
