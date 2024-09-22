
all: build-blackpill

build-blackpill:
	alr build
	alr exec -- arm-eabi-objcopy -O binary ../bin/blackpill_stm32f411.elf ../bin/blackpill_stm32f411.bin
	ls -l ../bin/blackpill_stm32f411.bin ../bin/blackpill_stm32f411.elf

clean:
	rm -rf ../.objs ../bin

ocd:
	openocd -f interface/cmsis-dap.cfg -c 'cmsis_dap_backend hid' -f maintainers-tools/debug/arduino_due.cfg

stlink:
	openocd -f interface/stlink-dap.cfg -f maintainers-tools/debug/arduino_due.cfg

gdb:
	eval `alr printenv`; arm-eabi-gdb --command="maintainers-tools/debug/gdbinit" ../bin/blackpill_stm32f411.elf
