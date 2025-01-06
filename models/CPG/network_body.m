
# Hopf oscillator
# Approximation by fourth order Runge-Kutta
# Duty factor

clear all;
##build_body_gait_network_coupling

ControlState.Tsw_STEPS = 20;  # Number of iterations in T_swing period
TURNS = 5;

ControlState.alpha = 1;
ControlState.mu = -1;

ControlState.a = 10000;

##Tripod_Gait.coupling = [  0,  pi,   0,  pi,   0,  pi;
##                        -pi,   0, -pi,   0, -pi,   0;
##                          0,  pi,   0,  pi,   0,  pi;
##                        -pi,   0, -pi,   0, -pi,   0;
##                          0,  pi,   0,  pi,   0,  pi;
##                        -pi,   0, -pi,   0, -pi,   0]
##Tripod_Gait.beta = 0.5;
##
##Quadruped_Gait.coupling = [        0,  2/4*2*pi,  1/4*2*pi,         0,  2/4*2*pi, 3/4*2*pi;
##   -2/4*2*pi,         0, -1/4*2*pi, -2/4*2*pi,         0, 1/4*2*pi;
##   -1/4*2*pi,  1/4*2*pi,         0, -1/4*2*pi,  1/4*2*pi, 2/4*2*pi;
##           0,  2/4*2*pi,  1/4*2*pi,         0,  2/4*2*pi, 3/4*2*pi;
##   -2/4*2*pi,         0, -1/4*2*pi, -2/4*2*pi,         0, 1/4*2*pi;
##   -3/4*2*pi, -1/4*2*pi, -2/4*2*pi, -3/4*2*pi, -1/4*2*pi,        0]
##Quadruped_Gait.beta = 2/3

# Duty factor:      0.5     for tripod gait
#                   0.8333  for wave gait
##phase = [0, 1/3*pi, 2/3*pi, pi, 4/3*pi, 5/6*pi]
##phase = [0, pi, 0, pi, 0, pi]
##phase = [0, 4/3*pi, 2/3*pi, 0, 4/3*pi, 2/3*pi]

connection = [0, 1, 1, 1, 1, 1;
              1, 0, 1, 1, 1, 1;
              1, 1, 0, 1, 1, 1;
              1, 1, 1, 0, 1, 1;
              1, 1, 1, 0, 1, 1;
              1, 1, 1, 1, 1, 0];
##connection = [0, 1, 1, 0, 0, 0;
##              1, 0, 0, 1, 0, 0;
##              1, 0, 0, 1, 1, 0;
##              0, 1, 1, 0, 0, 1;
##              0, 0, 1, 0, 0, 1;
##              0, 0, 0, 1, 1, 0];
##connection = [0, 1, 1, 0, 0, 0;
##              1, 0, 0, 1, 0, 0;
##              1, 0, 0, 0, 1, 0;
##              0, 1, 0, 0, 0, 1;
##              0, 0, 1, 0, 0, 1;
##              0, 0, 0, 1, 1, 0];
##connection = [0, 0, 0, 0, 0, 0;
##              0, 0, 0, 0, 0, 0;
##              0, 0, 0, 0, 0, 0;
##              0, 0, 0, 0, 0, 0;
##              0, 0, 0, 0, 0, 0;
##              0, 0, 0, 0, 0, 0];

##Wave_Gait.beta = 1/2;  ## Для эксперимента, неверное значение.
Wave_Gait.beta = 5/6;
Wave_Gait.phase = [0, 1/3*pi, 2/3*pi, pi, 4/3*pi, 5/3*pi];
Wave_Gait.coupling.connection = connection;
Wave_Gait.coupling.phase = build_body_gait_network_coupling(Wave_Gait);

Quadruped_Gait.beta = 2/3;
Quadruped_Gait.phase = [0, 4/3*pi, 2/3*pi, 0, 4/3*pi, 2/3*pi];
Quadruped_Gait.coupling.connection = connection;
Quadruped_Gait.coupling.phase = build_body_gait_network_coupling(Quadruped_Gait);

Tripod_Gait.beta = 1/2;
Tripod_Gait.phase = [0, pi, 0, pi, 0, pi];
Tripod_Gait.coupling.connection = connection;
Tripod_Gait.coupling.phase = build_body_gait_network_coupling(Tripod_Gait);


x = [0, 0, 0, 0, 0, 0];
##y = [-0.01, 0.01, -0.01, 0.01, -0.01, 0.01];
y = [0.01, 0.01, 0.01, 0.01, 0.01, 0.01];

function [dx, dy] = F (ControlState, omega, x, y)
  dx = ControlState.alpha * (ControlState.mu - x^2 - y^2) * x - omega * y;
  dy = ControlState.alpha * (ControlState.mu - x^2 - y^2) * y + omega * x;
endfunction

# Осциллятор Хопфа с аппроксимацией Рунге-Кутты четвёртого порядка
function [dx, dy] = FRK (ControlState, x, y)
  omega = pi*(1 - ControlState.Gait.beta) ...
            / (ControlState.Gait.beta * (e^(-ControlState.a*y) + 1) * ControlState.Tsw_STEPS) ...
          + pi / ((e^(ControlState.a*y) + 1) * ControlState.Tsw_STEPS);

  # Аппроксимация Рунге-Кутта
  [K1dx, K1dy] = F(ControlState, omega, x, y);
  [K2dx, K2dy] = F(ControlState, omega, x + K1dx/2, y + K1dy/2);
  [K3dx, K3dy] = F(ControlState, omega, x + K2dx/2, y + K2dy/2);
  [K4dx, K4dy] = F(ControlState, omega, x + K3dx, y + K3dy);

  dx = (K1dx + 2*K2dx + 2*K3dx + K4dx) / 6;
  dy = (K1dy + 2*K2dy + 2*K3dy + K4dy) / 6;
endfunction

# Коэффициенты согласования
function [kx, ky] = FK (i, phase, connection, x, y)
  # Вариант из работы "Parameters optimization of central pattern generators
  # for hexapod robot based on multi-objective genetic algorithm",
  # Binrui Wang, Xiaohong Cui, Jianbo Sun and Yanfeng Gao

##  phase
##  connection

  sigma = 0.05;
  k = 0.005;

  kx = 0;
  ky = 0;

  for j = 1:6
    if ((i != j) && (y(i) > 0))
##      ky = ky + (y(j) * cos(Coupling(j)) - x(j) * sin(Coupling(j)));

      ##  Другой сопособ связывания

      z = (x(j) + y(j)) / sqrt(x(j)^2 + y(j)^2);
      kx = kx + connection(j) * k * -sin(phase(j)) * z;
      ky = ky + connection(j) * k * cos(phase(j)) * z;

####      if (y(i) > 0)
##      z = sqrt(x(j)^2 + y(j)^2);
##      ky = ky + connection(j) * k * ...
##             (cos(phase(j)) * y(j) - sin(phase(j)) * x(j)) / z;
####      endif
####    if (abs(kx) > 0.0001)
####      kx
####    endif
    endif
  endfor

##  kx = kx * sigma;
##  ky = ky * sigma;

##  Coupling
##    x
##    y
##  kx = 0;
##  ky
##  ky = 0;
endfunction

function [state, x, y] = UpdateState (i, ControlState, xc, yc)
  state = ControlState;
  x = xc;
  y = yc;

  if (i == ControlState.Tsw_STEPS)
##    state.mu = 0.2;
##    for i = 1:6
##      x(i) = 0.001 * cos (ControlState.Gait.phase (i));
##      y(i) = 0.001 * sin (ControlState.Gait.phase (i));
##    endfor
##  elseif (i == ControlState.Tsw_STEPS + 0.1 * ControlState.Tsw_STEPS)
    state.mu = 1;
  endif
endfunction

##ControlState.Gait = Tripod_Gait;
##ControlState.Gait = Quadruped_Gait;
ControlState.Gait = Wave_Gait;

ControlState.Gait.phase

for i = 1:6
  x(i) = 0.0001; # * cos (ControlState.Gait.phase(i));
  y(i) = 0.0001; # * sin (ControlState.Gait.phase(i));
endfor

x
y


fx1=[];
fy1=[];
fdx1=[];
fdy1=[];
fx2=[];
fy2=[];
fdx2=[];
fdy2=[];
fx3=[];
fy3=[];
fdx3=[];
fdy3=[];
fx4=[];
fy4=[];
fdx4=[];
fdy4=[];
fx5=[];
fy5=[];
fdx5=[];
fdy5=[];
fx6=[];
fy6=[];
fdx6=[];
fdy6=[];

for i = 1:(ControlState.Tsw_STEPS*TURNS/(1-ControlState.Gait.beta))

  fx1(i) = x(1);
  fy1(i) = y(1);
  fx2(i) = x(2)-2;
  fy2(i) = y(2)-2;
  fx3(i) = x(3)-4;
  fy3(i) = y(3)-4;
  fx4(i) = x(4)-6;
  fy4(i) = y(4)-6;
  fx5(i) = x(5)-8;
  fy5(i) = y(5)-8;
  fx6(i) = x(6)-10;
  fy6(i) = y(6)-10;

  [ControlState, x, y] = UpdateState(i, ControlState, x, y);

  for j = 1:6
    [kx, ky] = FK(j, ControlState.Gait.coupling.phase(j,:), ControlState.Gait.coupling.connection(j,:), x, y);
    [dx(j), dy(j)] = FRK(ControlState, x(j), y(j));
    dx(j) = dx(j) + kx;
    dy(j) = dy(j) + ky;
    x(j) = x(j) + dx(j);
    y(j) = y(j) + dy(j);
  endfor

  fdx1(i) = dx(1) + 2;
  fdy1(i) = dy(1) + 2;
  fdx2(i) = dx(2) + 2;
  fdy2(i) = dy(2) + 2;
  fdx3(i) = dx(3) + 2;
  fdy3(i) = dy(3) + 2;
  fdx4(i) = dx(4) + 2;
  fdy4(i) = dy(4) + 2;
  fdx5(i) = dx(5) + 2;
  fdy5(i) = dy(5) + 2;
  fdx6(i) = dx(6) + 2;
  fdy6(i) = dy(6) + 2;
endfor

##fx(i+1) = x;
##fy(i+1) = y;

##plot(fx2, fy2);

##plot(fx, fy)
plot(fx1, "-", fy1, ":", fdx1, "-", fdy1, ":",
     fx2, "-", fy2, ":",
     fx3, "-", fy3, ":",
     fx4, "-", fy4, ":",
     fx5, "-", fy5, ":",
     fx6, "-", fy6, ":")
grid on


