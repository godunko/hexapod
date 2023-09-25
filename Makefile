
all:
	gprbuild gnat/kinematics.gpr --target='arm-eabi' --RTS='light-arduino_due_x'
	gprbuild gnat/kinematics.gpr

update:
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-forward-compute_h_be_matrix.ada > source/kinematics/generated/kinematics-forward-compute_h_be_matrix.adb
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-inverse-compute_jv_matrix.ada > source/kinematics/generated/kinematics-inverse-compute_jv_matrix.adb
