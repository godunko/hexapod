BOSSAC = bossac

all: build-core build-arduino

build-core:
	eval `alr printenv`; gprbuild gnat/kinematics.gpr --target='arm-eabi' --RTS='light-cortex-m3'
	# eval `alr printenv`; gprbuild gnat/kinematics.gpr
	# eval `alr printenv`; gprbuild gnat/examples.gpr

build-arduino:
	alr build
	eval `alr printenv`; arm-eabi-objcopy -O binary bin/arduino.elf bin/arduino.bin
	ls -l bin/arduino.bin bin/arduino.elf

clean:
	rm -rf .objs bin

update:
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-forward-compute_h_be_matrix.ada > source/kinematics/generated/kinematics-forward-compute_h_be_matrix.adb
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-forward-compute_e_position.ada > source/kinematics/generated/kinematics-forward-compute_e_position.adb
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-inverse-compute_jv_matrix.ada > source/kinematics/generated/kinematics-inverse-compute_jv_matrix.adb
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-inverse-algebraic-generic_compute_t.ada > source/kinematics/generated/kinematics-inverse-algebraic-generic_compute_t.adb
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-inverse-algebraic-generic_compute_12.ada > source/kinematics/generated/kinematics-inverse-algebraic-generic_compute_12.adb

flash: build-arduino
	$(BOSSAC) --arduino-erase
	$(BOSSAC) --info --write --verify --boot bin/arduino.bin

ocd:
	openocd -f interface/cmsis-dap.cfg -c 'cmsis_dap_backend hid' -f maintainers-tools/debug/arduino_due.cfg

stlink:
	openocd -f interface/stlink-dap.cfg -f maintainers-tools/debug/arduino_due.cfg

gdb:
	eval `alr printenv`; arm-eabi-gdb --command="maintainers-tools/debug/gdbinit" bin/arduino.elf
