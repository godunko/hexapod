
all: build-blackpill

build-blackpill:
	alr build
	alr exec -- arm-eabi-objcopy -O binary ../../bin/blackpill_stm32f401.elf ../../bin/blackpill_stm32f401.bin
	ls -l ../../bin/blackpill_stm32f401.bin ../../bin/blackpill_stm32f401.elf

clean:
	make -C ../.. clean

flash:
	openocd -f interface/cmsis-dap.cfg -f target/stm32f4x.cfg -c 'program ../../bin/blackpill_stm32f401.bin verify reset exit 0x08000000'

ocd:
	openocd -f interface/cmsis-dap.cfg -f target/stm32f4x.cfg

stlink:
	openocd -f interface/stlink-dap.cfg -f target/stm32f4x.cfg

gdb:
	eval `alr printenv` && arm-eabi-gdb --command="maintainers-tools/debug/gdbinit" ../../bin/blackpill_stm32f401.elf
