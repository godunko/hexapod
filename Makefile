
all:
	gprbuild gnat/kinematics.gpr --target='arm-eabi' --RTS='light-arduino_due_x'
	gprbuild gnat/kinematics.gpr
