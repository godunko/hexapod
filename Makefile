BOSSAC = bossac

all: build-core build-arduino

build-core:
	gprbuild gnat/kinematics.gpr --target='arm-eabi' --RTS='light-arduino_due_x'
	gprbuild gnat/kinematics.gpr
	gprbuild gnat/examples.gpr

build-arduino:
	gprbuild gnat/arduino.gpr
	arm-eabi-objcopy -O binary .objs/arm-eabi/arduino/hexapod.elf .objs/arm-eabi/hexapod.bin
	ls -l .objs/arm-eabi/arduino/hexapod.elf .objs/arm-eabi/hexapod.bin

clean:
	rm -rf .objs

update:
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-forward-compute_h_be_matrix.ada > source/kinematics/generated/kinematics-forward-compute_h_be_matrix.adb
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-forward-compute_e_position.ada > source/kinematics/generated/kinematics-forward-compute_e_position.adb
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-inverse-compute_jv_matrix.ada > source/kinematics/generated/kinematics-inverse-compute_jv_matrix.adb
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-inverse-algebraic-generic_compute_t.ada > source/kinematics/generated/kinematics-inverse-algebraic-generic_compute_t.adb
	./maintainers-tools/convert-symbols.sh source/kinematics/templates/kinematics-inverse-algebraic-generic_compute_12.ada > source/kinematics/generated/kinematics-inverse-algebraic-generic_compute_12.adb

flash: build-arduino
	$(BOSSAC) --arduino-erase
	$(BOSSAC) --info --write --verify --boot .objs/arm-eabi/hexapod.bin

ocd:
	openocd -f interface/cmsis-dap.cfg -c 'cmsis_dap_backend hid' -f maintainers-tools/debug/arduino_due.cfg

stlink:
	openocd -f interface/stlink-dap.cfg -f maintainers-tools/debug/arduino_due.cfg

gdb:
	arm-eabi-gdb --command="maintainers-tools/debug/gdbinit" .objs/arm-eabi/arduino/hexapod.elf
