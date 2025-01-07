#!/bin/sh

cat $1 |
  sed -e 's/cos(b_γ)/Cos_Base_Gamma/g' |
  sed -e 's/sin(b_γ)/Sin_Base_Gamma/g' |
  sed -e 's/cos(α_1)/Cos_Alpha_1/g' |
  sed -e 's/sin(α_1)/Sin_Alpha_1/g' |
  sed -e 's/cos(α_2)/Cos_Alpha_2/g' |
  sed -e 's/sin(α_2)/Sin_Alpha_2/g' |
  sed -e 's/cos(θ_1)/Cos_Theta_1/g' |
  sed -e 's/sin(θ_1)/Sin_Theta_1/g' |
  sed -e 's/cos(θ_2)/Cos_Theta_2/g' |
  sed -e 's/sin(θ_2)/Sin_Theta_2/g' |
  sed -e 's/cos(θ_3)/Cos_Theta_3/g' |
  sed -e 's/sin(θ_3)/Sin_Theta_3/g' |
  sed -e 's/cos(%alpha_0)/Cos_Alpha_0/g' |
  sed -e 's/sin(%alpha_0)/Sin_Alpha_0/g' |
  sed -e 's/cos(%gamma_0)/Cos_Gamma_0/g' |
  sed -e 's/sin(%gamma_0)/Sin_Gamma_0/g' |
  sed -e 's/cos(%alpha_1)/Cos_Alpha_1/g' |
  sed -e 's/sin(%alpha_1)/Sin_Alpha_1/g' |
  sed -e 's/cos(%alpha_2)/Cos_Alpha_2/g' |
  sed -e 's/sin(%alpha_2)/Sin_Alpha_2/g' |
  sed -e 's/cos(%alpha_3)/Cos_Alpha_3/g' |
  sed -e 's/sin(%alpha_3)/Sin_Alpha_3/g' |
  sed -e 's/cos(%theta_1)/Cos_Theta_1/g' |
  sed -e 's/sin(%theta_1)/Sin_Theta_1/g' |
  sed -e 's/cos(%theta_2)/Cos_Theta_2/g' |
  sed -e 's/sin(%theta_2)/Sin_Theta_2/g' |
  sed -e 's/cos(%theta_3)/Cos_Theta_3/g' |
  sed -e 's/sin(%theta_3)/Sin_Theta_3/g' |
  sed -e 's/\([^ ._^0-9][0-9]\+\)\([^.]\)/\1.0\2/g' |
  sed -e 's/^\([0-9]\+\)/\1.0/g' |
  sed -e 's/r_1/R_1/g' |
  sed -e 's/r_2/R_2/g' |
  sed -e 's/r_3/R_3/g' |
  sed -e 's/b_x/B_x/g' |
  sed -e 's/b_y/B_y/g' |
  sed -e 's/b_z/B_z/g' |
  sed -e 's/\*\([^$]\)/ \* \1/g' |
  sed -e 's/\*$/ \*/g' |
  sed -e 's/\([^( ]\)-\([^- $]\)/\1 - \2/g' |
  sed -e 's/\([^ ]\)+/\1 + /g' |
  sed -e 's/\^/ ** /g' |
  tee
