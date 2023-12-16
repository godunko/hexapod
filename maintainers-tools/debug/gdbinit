set pagination off

target extended-remote localhost:3333

monitor arm semihosting enable

monitor reset halt

# set breakpoint on last chance handler to catch unhandled Ada exceptions.
break __gnat_last_chance_handler

# execute single instruction to load stack pointer and program counter from
# the interrupt table.
stepi
