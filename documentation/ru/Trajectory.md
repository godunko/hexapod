# Траектория движения

## Движение центра тела робота

С пульта управления движение робота задаётся в системе координат тела как вектор линейной скорости (в виде составляющих по осям X и Y) $v_B = [v_{xB}, v_{yB}]$ и вектор угловой скорости (скорости рысканья) $\omega_B$. 
Получается, что центр робота совершает движение по дуге окружности с радиусом $r_m$ вокруг некоторой точки $C_{mB} = [C_{xB}, C_{yB}]$.

Радиус траектории можно вычислить из отношения длин векторов линейной и уголовой скоростей:

$$
r_m = \frac{|v_B|}{\omega_B}
$$

И хотя формально можно считать, что движение по прямой есть движение по окружности с бесконечным радиусом, мне это не нравится; да и вводит сложности при вычислениях с плавающий точкой. 
Видимо случай чисто линейного движения нужно рассматривать отдельно.

Вектор линейной скорости является касательным вектором к окружности траектории, следовательно вектор из точки касания в центр окружности будет ортогонален к вектору линейной скорости.
Получить его можно например из единичного вектора линейной скорости поворотом на 90 градусов против часовой стрелки. Длина вектора равна радиусу траектории.

$$
\vec{C_{mB}} = r_m  R_z  \hat{v_B}
  = r_m \left\lbrack \matrix{cos \theta & -sin \theta \cr sin \theta & cos \theta } \right\rbrack  \left\lbrack \matrix{\hat{v_{xB}} \cr \hat{v_{yB}}} \right\rbrack
  = r_m \left\lbrack \matrix{0 & -1 \cr 1 & 0 } \right\rbrack * \left\lbrack \matrix{\hat{v_{xB}} \cr \hat{v_{yB}}} \right\rbrack
  = r_m \left\lbrack \matrix{-\hat{v_{yB}} \cr \hat{v_{xB}}} \right\rbrack
  = \left\lbrack \matrix{- r_m \hat{v_{yB}} \cr r_m \hat{v_{xB}}} \right\rbrack
$$

Поскольку точка касания находится в начале системы координат тела робота верктор $\vec{C_{mB}}$ содержит координаты цента окружности траектории.
Подставим $r_m$, $\hat{v_{xB}}$ и $\hat{v_{yB}}$ и упростим:

$$
C_{mB} = \left\lbrack \matrix{ C_{xB} \cr C_{yB}} \right\rbrack
 = \left\lbrack \matrix{- r_m \hat{v_{yB}} \cr r_m \hat{v_{xB}}} \right\rbrack
 = \left\lbrack \matrix{- \frac{|v_B|}{\omega_B} \frac{v_{yB}}{|v_B|} \cr \frac{|v_B|}{\omega_B} \frac{v_{xB}}{|v_B|}}  \right\rbrack
 = \left\lbrack \matrix{- \frac{v_{yB}}{\omega_B} \cr \frac{v_{xB}}{\omega_B}}  \right\rbrack
$$

Найденная точка центра окружности будет единой для всех лап, хотя каждая лапа будет перемещаться по собственной траектории.

$$
C_i = \left\lbrack \matrix{r_m cos \phi_i + C_{xB} \cr r_m sin \phi_i + C_{yB}} \right\rbrack
$$

### Дискретный вид

Для управления удобно использовать дискретную запись, когда известно состояние на текущем шаге и длительность такта цикла управления. Пусть $t_c$ есть длительность такта, тогда приращение угла поворота $\Delta \phi (k)$, радиус траектории движения $r_m(k)$ и координаты точки центра движения $[x_m(k), y_m(k)]$ на текущем шаге будут выглядеть как:

$$
\begin{array}{l}
  \Delta \phi(k) = \omega(k) t_c
  \\
  r_m(k) = \frac{v(k)}{\omega(k)}
  \\
  x_m(k) = - \frac{v_y (k)}{\omega (k)}
  \\
  y_m(k) = \frac{v_x (k)}{\omega (k)}
\end{array}
$$

А координаты конечной точки каждой лапы $i$

$$
\begin{array}{l}
  x_i(k+1) = r_{mi}(k) cos(\phi_i(k) + \Delta\phi(k)) + x_m(k)
  \\
  y_i(k+1) = r_{mi}(k) sin(\phi_i(k) + \Delta\phi(k)) + y_m(k)
\end{array}
$$

Значения углов в каждом узле робота получается через формулы обратной кинематики.

### Программирование

Можно заменить вычисление значения угла $\phi_i(k)$, а также минимизировать количество вычислений функций синуса и косинуса если воспользоваться формулой синуса/косинуса суммы аргументов:

$$
\begin{array}{l}
  x_i(k+1) = r_{mi}(k) cos(\phi_i(k) + \Delta\phi(k)) + x_m(k) 
    = r_{mi}(k) (cos(\phi_i(k)) cos(\Delta\phi(k)) - sin(\phi_i(k)) sin(\Delta\phi(k))) + x_m(k)
  \\
  y_i(k+1) = r_{mi}(k) sin(\phi_i(k) + \Delta\phi(k)) + y_m(k)
    = r_{mi}(k) (sin(\phi_i(k)) cos(\Delta\phi(k)) + cos(\phi_i(k)) sin(\Delta\phi(k))) + y_m(k)
\end{array}
$$

и воспользовавшись направляющими косинусами вектора из центра поворота в вершину лапы:

$$
\begin{array}{l}
  cos(\phi_i(k)) = \frac{r_{mix}(k)}{|r_{mi}(k)|}
  \\
  sin(\phi_i(k)) = \frac{r_{miy}(k)}{|r_{mi}(k)|}
\end{array}
$$

получаем

$$
\begin{array}{l}
  x_i(k+1) = r_{mi}(k) (cos(\phi_i(k)) cos(\Delta\phi(k)) - sin(\phi_i(k)) sin(\Delta\phi(k))) + x_m(k) =
  \\
    = r_{mi}(k) (\frac{r_{mix}(k)}{|r_{mi}(k)|} cos(\Delta\phi(k)) - \frac{r_{miy}(k)}{|r_{mi}(k)|} sin(\Delta\phi(k))) + x_m(k) =
  \\
    = r_{mi}(k) (\frac{r_{mix}(k) cos(\Delta\phi(k)) - r_{miy}(k) sin(\Delta\phi(k))}{|r_{mi}(k)|}) + x_m(k) =
  \\
    = r_{mix}(k) cos(\Delta\phi(k)) - r_{miy}(k) sin(\Delta\phi(k)) + x_m(k)
\end{array}
$$

и

$$
\begin{array}{l}
  y_i(k+1) = r_{mi}(k) (sin(\phi_i(k)) cos(\Delta\phi(k)) + cos(\phi_i(k)) sin(\Delta\phi(k))) + y_m(k) =
  \\
    = r_{mi}(k) (\frac{r_{miy}(k)}{|r_{mi}(k)|} cos(\Delta\phi(k)) + \frac{r_{mix}(k)}{|r_{mi}(k)|} sin(\Delta\phi(k))) + y_m(k) =
  \\
    = r_{mi}(k) (\frac{r_{miy}(k) cos(\Delta\phi(k)) + r_{mix}(k) sin(\Delta\phi(k))}{|r_{mi}(k)|}) +y_m(k) =
  \\
    = r_{miy}(k) cos(\Delta\phi(k)) + r_{mix}(k) sin(\Delta\phi(k)) + y_m(k)
\end{array}
$$
