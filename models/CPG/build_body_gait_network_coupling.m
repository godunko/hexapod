

##  Build body network couplig matrix for gait
##
##  Gait.phase defines phase offset between legs

function phase = build_body_gait_network_coupling (Gait)
  for i = 1:6
    for j = 1:6
      if (Gait.coupling.connection (i, j) == 0)
        phase(i,j) = 0;
      else
        phase(i,j) = Gait.phase(j) - Gait.phase(i);
      endif;
    endfor
  endfor
endfunction

