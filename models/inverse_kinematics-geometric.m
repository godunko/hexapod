clear;
clc;

pkg load symbolic

syms b_x b_y b_z # Координата узла 1 лапы
syms gamma_0     # Поворот вокруг оси Z оси лапы
syms e_x e_y e_z # Положения кончика лапы
syms r_1 r_2 r_3 # Длины сегментов

syms theta_1 theta_2 theta_3 # Искомые углы
                             # Углы ограничены -pi/2 .. pi/2 за счёт выбора
                             # системы координат лапы

syms x y z

b = [b_x b_y b_z];
e = [[e_x] [e_y] [e_z]];
e_h = [e_x e_y e_z 1];

function H = T_translation (x, y, z)
  H = [[1  0  0  x],
       [0  1  0  y],
       [0  0  1  z],
       [0  0  0  1]];
endfunction

function H = Rz_rotation (gamma)
  H = [[cos(gamma)  -sin(gamma)  0  0];
       [sin(gamma)   cos(gamma)  0  0];
       [0            0           1  0];
       [0            0           0  1]];
endfunction

#### Уравнение плоскости параллельной оси координат OZ
## XXX Неверно! Вектор нормали плоскости это b-e, а "известная координата точки"
## как раз искомая точка.
## XXX Тоже не верно
##aux = cross(e-b, [0 0 1]);
##eq1 = aux(1)*(x-b_x) + aux(2)*(y-b_y) + aux(3)*(z-b_z)

## Уравнения двух сфер

##eq2 = (x-b_x)^2 + (y-b_y)^2 + (z-b_z)^2 - r_2^2
##eq3 = (x-e_x)^2 + (y-e_y)^2 + (z-e_z)^2 - r_3^2

##  Длина вектора между b и e

##l = sqrt((e_x - b_x)^2 + (e_y - b_y)^2 + (e_z - b_z)^2)
##cos3 = (r_2^2 + r_3^2 - l^2) / (2*r_2*r_3)

##sin3 = sqrt(1 -cos3^2)

# Поиск угла поворота первого узла

T_b1 = T_translation(b_x, b_y, b_z);
Rz_b1 = Rz_rotation(gamma_0);
R_b1 = T_b1 * Rz_b1;
R_1b = simplify(inv(R_b1));

e_1 = (R_1b * e_h')' # Координата кончика лапы в системе координат 1-го узла

theta_1 = atan2(e_1(2), e_1(1))
aux = norm(e_1(1:2));
cos_1 = e_1(1)/aux
sin_1 = e_1(2)/aux

rad2deg(eval (subs(theta_1, [gamma_0 b_x b_y b_z e_x e_y e_z], [sym('pi')/3 sym('0.074') sym('0.048') sym('0.000') sym('0.311') sym('0.048') sym('0.000')])))
rad2deg(eval (subs(acos(cos_1), [gamma_0 b_x b_y b_z e_x e_y e_z], [sym('pi')/3 sym('0.074') sym('0.048') sym('0.000') sym('0.311') sym('0.048') sym('0.000')])))
rad2deg(eval (subs(asin(sin_1), [gamma_0 b_x b_y b_z e_x e_y e_z], [sym('pi')/3 sym('0.074') sym('0.048') sym('0.000') sym('0.311') sym('0.048') sym('0.000')])))

# Поиск угла поворота третьего узла.
#
# Решать удобнее в системе координат первого узла. Точнее, нужно найти
# координаты узла 2 в любой из систем координат. Поскольку направляющие косинусы
# первого элемента уже посчитаны, найти координату в системе координат первого
# узла проще. Координата кончика лапы так же уже известна.

syms cos_1 sin_1

c2_1 = [cos_1*r_1 sin_1*r_1 0]
l = norm(e_1(1:3) - c2_1)

syms l

cos_theta_3 = (r_2^2 + r_3^2 - l^2)/(2*r_2*r_3)

# Поиск угла поворота среднего узла

syms e_x1 e_y1 e_z1 c2_x1 c2_y1 c2_z1
e_1 = [e_x1 e_y1 e_z1]
c2_1 = [c2_x1 c2_y1 c2_z1]
l_xy = norm(e_1(1:2) - c2_1(1:2))

#cos_alpha_1 = (l^2 + e_1(3)^2 - l_xy^2) / (2 * l * e_1(3))
cos_alpha_1 = (l_xy^2 + l^2 - e_1(3)^2) / (2 * l_xy * l)

cos_alpha_2 = (r_2^2 + l^2 - r_3^2) / (2 * r_2 * l)

##eval(subs(cos_1*r_1, [gamma_0 b_x b_y b_z r_1 e_x e_y e_z], [sym('pi')/3 sym('0.074') sym('0.048') sym('0.000') sym('0.029') sym('0.311') sym('0.048') sym('0.000')]))
##eval(subs(sin_1*r_1, [gamma_0 b_x b_y b_z r_1 e_x e_y e_z], [sym('pi')/3 sym('0.074') sym('0.048') sym('0.000') sym('0.029') sym('0.311') sym('0.048') sym('0.000')]))
##eval(subs(0, [gamma_0 b_x b_y b_z e_x e_y r_1 e_z], [sym('pi')/3 sym('0.074') sym('0.048') sym('0.000') sym('0.029') sym('0.311') sym('0.048') sym('0.000')]))
##sin_1*r_1

##L = norm([e_1(1) -  )

##cos1 = atan2
