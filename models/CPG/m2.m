
STEPS = 20
TURNS = 1

M1_STEP = 10
MN_STEP = 400

t = 1

e = 2.71828

alpha = 1
mu = 1
omega = 1

a = 100
##omega_st = 1
beta = 0.5
##omega_sw = pi / 4
##omega_sw = 1

T_sw = 20

##DT = 1
##dt = 1 / DT
##dt = dt /2
##dt = 1/(1.3*alpha*mu)
dt=1
##omega_st = (1 - beta) / beta * omega_sw
##omega = pi * 0.1 * 2 # 2*pi

omega = 2*pi/STEPS/dt # когда всё стало получаться

x = 0
y = -0.01

fx=[]
fy=[]
fdx=[]
fdy=[]

for i = 1:(STEPS*TURNS)

  fx(i) = x;
  fy(i) = y;
##  if (i == M1_STEP)
##    mu = 1
##
##  elseif (i == MN_STEP)
##    mu = -1
##  endif
##
##  o1 = omega_st / (e ^ (-a*y) + 1)
##  o2 = omega_sw / (e ^ (a*y) + 1)
##  omega = omega_st / (e ^ (-a*y) + 1) + omega_sw / (e ^ (a*y) + 1)
##  omega = omega / t

##  o1 = pi / (beta * (e^(-a*y) + 1) * T_sw)
##  o2 = pi / ((1 - beta) * (e^(a*y)+1) * T_sw)
##  omega = o1 + o2

  dx = alpha * (mu - x^2 - y^2) * x - omega * y
  dy = alpha * (mu - x^2 - y^2) * y + omega * x

##  dt = 1 / (pi * alpha * mu);
##  dt = 1 / (2 * alpha * abs(mu))
  x = x + dx * dt # (1 / 2 * alpha * mu)
  y = y + dy * dt # (1 / 2 * alpha * mu)

  fdx(i) = dx + 2;
  fdy(i) = dy + 2;
endfor

i
fx(i+1) = x
fy(i+1) = y

##plot(fx, fy)
plot(fx, "", fy, "", fdx, "", fdy)
grid on
