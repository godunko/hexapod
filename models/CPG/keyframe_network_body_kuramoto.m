
# Hopf oscillator (Kuramoto)
# Approximation by fourth order Runge-Kutta


clear all;

ControlState.FRAME_ITERATIONS = 20; # Number of iterations during single keyframe
TURNS = 5;

Wave_Gait.frames = 6;
Wave_Gait.keyframes = ...
  [     0, 7/5*pi, 6/5*pi, 9/5*pi, 8/5*pi,     pi;
       pi, 8/5*pi, 7/5*pi,      0, 9/5*pi, 6/5*pi;
   6/5*pi, 9/5*pi, 8/5*pi,     pi,      0, 7/5*pi;
   7/5*pi,      0, 9/5*pi, 6/5*pi,     pi, 8/5*pi;
   8/5*pi,     pi,      0, 7/5*pi, 6/5*pi, 9/5*pi;
   9/5*pi, 6/5*pi,     pi, 8/5*pi, 7/5*pi,      0];

Quadruped_Gait.frames = 3;
Quadruped_Gait.keyframes = ...
  [     0,     pi, 3/2*pi,      0,     pi, 3/2*pi;
       pi, 3/2*pi,      0,     pi, 3/2*pi,      0;
   3/2*pi,      0,     pi, 3/2*pi,      0,     pi];

####build_body_gait_network_coupling
##
##ControlState.Tsw_STEPS = 20;  # Number of iterations in T_swing period
##TURNS = 5;
##
##ControlState.alpha = 1;
##ControlState.mu = -1;
##
##ControlState.a = 10000;
##
####Tripod_Gait.coupling = [  0,  pi,   0,  pi,   0,  pi;
####                        -pi,   0, -pi,   0, -pi,   0;
####                          0,  pi,   0,  pi,   0,  pi;
####                        -pi,   0, -pi,   0, -pi,   0;
####                          0,  pi,   0,  pi,   0,  pi;
####                        -pi,   0, -pi,   0, -pi,   0]
####Tripod_Gait.beta = 0.5;
####
####Quadruped_Gait.coupling = [        0,  2/4*2*pi,  1/4*2*pi,         0,  2/4*2*pi, 3/4*2*pi;
####   -2/4*2*pi,         0, -1/4*2*pi, -2/4*2*pi,         0, 1/4*2*pi;
####   -1/4*2*pi,  1/4*2*pi,         0, -1/4*2*pi,  1/4*2*pi, 2/4*2*pi;
####           0,  2/4*2*pi,  1/4*2*pi,         0,  2/4*2*pi, 3/4*2*pi;
####   -2/4*2*pi,         0, -1/4*2*pi, -2/4*2*pi,         0, 1/4*2*pi;
####   -3/4*2*pi, -1/4*2*pi, -2/4*2*pi, -3/4*2*pi, -1/4*2*pi,        0]
####Quadruped_Gait.beta = 2/3
##
### Duty factor:      0.5     for tripod gait
###                   0.8333  for wave gait
####phase = [0, 1/3*pi, 2/3*pi, pi, 4/3*pi, 5/6*pi]
####phase = [0, pi, 0, pi, 0, pi]
####phase = [0, 4/3*pi, 2/3*pi, 0, 4/3*pi, 2/3*pi]
##
##connection = [0, 1, 1, 1, 1, 1;
##              1, 0, 1, 1, 1, 1;
##              1, 1, 0, 1, 1, 1;
##              1, 1, 1, 0, 1, 1;
##              1, 1, 1, 0, 1, 1;
##              1, 1, 1, 1, 1, 0];
####connection = [0, 1, 1, 0, 0, 0;
####              1, 0, 0, 1, 0, 0;
####              1, 0, 0, 1, 1, 0;
####              0, 1, 1, 0, 0, 1;
####              0, 0, 1, 0, 0, 1;
####              0, 0, 0, 1, 1, 0];
####connection = [0, 1, 1, 0, 0, 0;
####              1, 0, 0, 1, 0, 0;
####              1, 0, 0, 0, 1, 0;
####              0, 1, 0, 0, 0, 1;
####              0, 0, 1, 0, 0, 1;
####              0, 0, 0, 1, 1, 0];
####connection = [0, 0, 0, 0, 0, 0;
####              0, 0, 0, 0, 0, 0;
####              0, 0, 0, 0, 0, 0;
####              0, 0, 0, 0, 0, 0;
####              0, 0, 0, 0, 0, 0;
####              0, 0, 0, 0, 0, 0];
##
####Wave_Gait.beta = 1/2;  ## Для эксперимента, неверное значение.
##Wave_Gait.beta = 5/6;
##Wave_Gait.phase = [0, 1/3*pi, 2/3*pi, pi, 4/3*pi, 5/3*pi];
##Wave_Gait.coupling.connection = connection;
##Wave_Gait.coupling.phase = build_body_gait_network_coupling(Wave_Gait);
##
##Quadruped_Gait.beta = 2/3;
##Quadruped_Gait.phase = [0, 4/3*pi, 2/3*pi, 0, 4/3*pi, 2/3*pi];
##Quadruped_Gait.coupling.connection = connection;
##Quadruped_Gait.coupling.phase = build_body_gait_network_coupling(Quadruped_Gait);
##
##Tripod_Gait.beta = 1/2;
##Tripod_Gait.phase = [0, pi, 0, pi, 0, pi];
##Tripod_Gait.coupling.connection = connection;
##Tripod_Gait.coupling.phase = build_body_gait_network_coupling(Tripod_Gait);
##
##
##x = [0, 0, 0, 0, 0, 0];
####y = [-0.01, 0.01, -0.01, 0.01, -0.01, 0.01];
##y = [0.01, 0.01, 0.01, 0.01, 0.01, 0.01];
##
##function [dx, dy] = F (ControlState, omega, x, y)
##  dx = ControlState.alpha * (ControlState.mu - x^2 - y^2) * x - omega * y;
##  dy = ControlState.alpha * (ControlState.mu - x^2 - y^2) * y + omega * x;
##endfunction
##
### Осциллятор Хопфа с аппроксимацией Рунге-Кутты четвёртого порядка
##function [dx, dy] = FRK (ControlState, x, y)
##  omega = pi*(1 - ControlState.Gait.beta) ...
##            / (ControlState.Gait.beta * (e^(-ControlState.a*y) + 1) * ControlState.Tsw_STEPS) ...
##          + pi / ((e^(ControlState.a*y) + 1) * ControlState.Tsw_STEPS);
##
##  # Аппроксимация Рунге-Кутта
##  [K1dx, K1dy] = F(ControlState, omega, x, y);
##  [K2dx, K2dy] = F(ControlState, omega, x + K1dx/2, y + K1dy/2);
##  [K3dx, K3dy] = F(ControlState, omega, x + K2dx/2, y + K2dy/2);
##  [K4dx, K4dy] = F(ControlState, omega, x + K3dx, y + K3dy);
##
##  dx = (K1dx + 2*K2dx + 2*K3dx + K4dx) / 6;
##  dy = (K1dy + 2*K2dy + 2*K3dy + K4dy) / 6;
##endfunction
##
### Коэффициенты согласования
##function [kx, ky] = FK (i, phase, connection, x, y)
##  # Вариант из работы "Parameters optimization of central pattern generators
##  # for hexapod robot based on multi-objective genetic algorithm",
##  # Binrui Wang, Xiaohong Cui, Jianbo Sun and Yanfeng Gao
##
####  phase
####  connection
##
##  sigma = 0.05;
##  k = 0.005;
##
##  kx = 0;
##  ky = 0;
##
##  for j = 1:6
##    if (i != j)
####      ky = ky + (y(j) * cos(Coupling(j)) - x(j) * sin(Coupling(j)));
##
##      ##  Другой сопособ связывания
##
##      z = (x(j) + y(j)) / sqrt(x(j)^2 + y(j)^2);
##      kx = kx + connection(j) * k * -sin(phase(j)) * z;
##      ky = ky + connection(j) * k * cos(phase(j)) * z;
##
######      if (y(i) > 0)
####      z = sqrt(x(j)^2 + y(j)^2);
####      ky = ky + connection(j) * k * ...
####             (cos(phase(j)) * y(j) - sin(phase(j)) * x(j)) / z;
######      endif
######    if (abs(kx) > 0.0001)
######      kx
######    endif
##    endif
##  endfor
##
####  kx = kx * sigma;
####  ky = ky * sigma;
##
####  Coupling
####    x
####    y
####  kx = 0;
####  ky
####  ky = 0;
##endfunction
##
##function [state, x, y] = UpdateState (i, ControlState, xc, yc)
##  state = ControlState;
##  x = xc;
##  y = yc;
##
##  if (i == ControlState.Tsw_STEPS)
####    state.mu = 0.2;
####    for i = 1:6
####      x(i) = 0.001 * cos (ControlState.Gait.phase (i));
####      y(i) = 0.001 * sin (ControlState.Gait.phase (i));
####    endfor
####  elseif (i == ControlState.Tsw_STEPS + 0.1 * ControlState.Tsw_STEPS)
##    state.mu = 1;
##  endif
##endfunction
##
####ControlState.Gait = Tripod_Gait;
####ControlState.Gait = Quadruped_Gait;
##ControlState.Gait = Wave_Gait;
##
##ControlState.Gait.phase
##
##for i = 1:6
##  x(i) = 0.0001; # * cos (ControlState.Gait.phase(i));
##  y(i) = 0.0001; # * sin (ControlState.Gait.phase(i));
##endfor
##
##x
##y
##

function [dx, dy] = F_oscillator_base (alpha_x, alpha_y, mu, omega, x, y)
  r2 = x^2 + y^2;
  dx = alpha_x * (mu - r2) * x - omega * y;
  dy = alpha_y * (mu - r2) * y + omega * x;
endfunction

function [dx, dy] = F_oscillator_approximate (alpha_x, alpha_y, mu, omega, x, y)
##  r2 = x^2 + y^2;
##  dx = alpha_x * (mu - r2) * x - omega * y;
##  dy = alpha_y * (mu - r2) * y + omega * x;

  # Аппроксимация Рунге-Кутта
  [K1dx, K1dy] = F_oscillator_base(alpha_x, alpha_y, mu, omega, x, y);
  [K2dx, K2dy] = F_oscillator_base(alpha_x, alpha_y, mu, omega, x + K1dx/2, y + K1dy/2);
  [K3dx, K3dy] = F_oscillator_base(alpha_x, alpha_y, mu, omega, x + K2dx/2, y + K2dy/2);
  [K4dx, K4dy] = F_oscillator_base(alpha_x, alpha_y, mu, omega, x + K3dx, y + K3dy);

  dx = (K1dx + 2*K2dx + 2*K3dx + K4dx) / 6;
  dy = (K1dy + 2*K2dy + 2*K3dy + K4dy) / 6;
endfunction

function phi = build_keyframe_phi (keyframe)
  for i = 1:6
    for j = 1:6
      phi(i,j) = keyframe(j) - keyframe(i);
    endfor
  endfor
endfunction


## ## ## ## ##

ControlState.Gait = Wave_Gait;
ControlState.frame = 1;

alpha_x = 1
alpha_y = 1
mu = -1

index = 1;
x = [0, 0, 0, 0, 0, 0];
y = [0.001, 0.001, 0.001, 0.001, 0.001, 0.001];
##x = [0, 0, 0, 0, 0, 0];
##y = [sin(9/5*pi), 0, 0, 0, 0, 0];
dx = [0, 0, 0, 0, 0, 0];
dy = [0, 0, 0, 0, 0, 0];

##
fx=[];
fy=[];
dfx=[];
dfy=[];

for ti = 1:TURNS*ControlState.Gait.frames
  ti
  if (ti == 2)
    mu = 1;
  elseif (ti == 20)
    ControlState.Gait = Quadruped_Gait;
    ControlState.frame = 2;
  endif

  keyframe = ControlState.Gait.keyframes (ControlState.frame,:)
  phi = build_keyframe_phi (keyframe)

  for ii = 1:ControlState.FRAME_ITERATIONS

    # Фактический фазовый угол каждого осциллятора.

    for i = 1:6
      theta(i) = atan2(y(i), x(i));
      if theta(i) < 0
        theta(i) += 2*pi; # Корректировка в диапазон 0..2pi
      endif
    endfor

    theta
    for i = 1:6
      fx(i, index) = x(i) - 2*(i-1);
      fy(i, index) = y(i) - 2*(i-1);

      k = keyframe(i);
      if (k == 0)
        k = 2*pi;
      endif

      omega = abs (k - theta(i)) / (ControlState.FRAME_ITERATIONS - ii + 1);
      omega_feedback = 0;

      if (y(i) >= 0.0)
        for j = 1:6
          omega_feedback += sin(theta(j) - theta (i) - phi(i,j));
##          sin(theta(j) - theta (i) - phi(i,j))
##          aux = [theta(j) - theta(i), phi(j,i)]

        endfor
      endif

      omega -= 0.1/6 * omega_feedback;

      [dx(i), dy(i)] = ...
        F_oscillator_approximate(alpha_x, alpha_y, mu, omega, x(i), y(i));

      x(i) += dx(i);
      y(i) += dy(i);

      dfx(i, index) = dx(i) + 2*(i);
      dfy(i, index) = dy(i) + 2*(i);
    endfor

    index += 1;
  endfor

  # Select next keyframe
  if (ControlState.frame == ControlState.Gait.frames)
    ControlState.frame = 1;
  else
    ControlState.frame += 1;
  endif
endfor

##for i = 1:(ControlState.Tsw_STEPS*TURNS/(1-ControlState.Gait.beta))
##
##  fx1(i) = x(1);
##  fy1(i) = y(1);
##  fx2(i) = x(2)-2;
##  fy2(i) = y(2)-2;
##  fx3(i) = x(3)-4;
##  fy3(i) = y(3)-4;
##  fx4(i) = x(4)-6;
##  fy4(i) = y(4)-6;
##  fx5(i) = x(5)-8;
##  fy5(i) = y(5)-8;
##  fx6(i) = x(6)-10;
##  fy6(i) = y(6)-10;
##
##  [ControlState, x, y] = UpdateState(i, ControlState, x, y);
##
##  for j = 1:6
##    [kx, ky] = FK(j, ControlState.Gait.coupling.phase(j,:), ControlState.Gait.coupling.connection(j,:), x, y);
##    [dx(j), dy(j)] = FRK(ControlState, x(j), y(j));
##    dx(j) = dx(j) + kx;
##    dy(j) = dy(j) + ky;
##    x(j) = x(j) + dx(j);
##    y(j) = y(j) + dy(j);
##  endfor


plot(fx(1,:), "-", fy(1,:), "--",
     fx(2,:), "-", fy(2,:), "--",
     fx(3,:), "-", fy(3,:), "--",
     fx(4,:), "-", fy(4,:), "--",
     fx(5,:), "-", fy(5,:), "--",
     fx(6,:), "-", fy(6,:), "--");
grid on

for i = 1:6
  js = 1;

  for j = 1:columns(fy)
    offset = 2*(i-1);
    if (fy(i,j) + offset < 0.01)
      if (js == 0)
        js = j;
      endif
    else
      if (js != 0)
        rectangle("position", [js, -0.1-offset, j-js, 0.1],
                  "facecolor", [0,1,0],
                  "edgecolor", [0,1,0])
        js = 0;
      endif
    endif
  endfor

  if (js != 0)
    rectangle("position", [js, -0.1-offset, columns(fy)-js, 0.1],
              "facecolor", [0,1,0],
              "edgecolor", [0,1,0])
  endif
endfor

##for i = 1:6
##  for j = 1:6
##    phi(i,j) = Wave_Gait.keyframes(1,j) - Wave_Gait.keyframes(1,i);
##  endfor
##endfor
##
##phi / pi

