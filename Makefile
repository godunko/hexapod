
all: build-core build-arduino

build-core:
	eval `alr printenv`; gprbuild gnat/kinematics.gpr --target='arm-eabi' --RTS='light-cortex-m3'
	# eval `alr printenv`; gprbuild gnat/kinematics.gpr
	# eval `alr printenv`; gprbuild gnat/examples.gpr

build-arduino:
	make -C arduino_due

clean:
	rm -rf .objs bin

update:
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-forward-compute_h_be_matrix.ada > source/kinematics/generated/kinematics-forward-compute_h_be_matrix.adb
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-forward-compute_e_position.ada > source/kinematics/generated/kinematics-forward-compute_e_position.adb
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-inverse-compute_jv_matrix.ada > source/kinematics/generated/kinematics-inverse-compute_jv_matrix.adb
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-inverse-algebraic-generic_compute_t.ada > source/kinematics/generated/kinematics-inverse-algebraic-generic_compute_t.adb
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-inverse-algebraic-generic_compute_12.ada > source/kinematics/generated/kinematics-inverse-algebraic-generic_compute_12.adb
