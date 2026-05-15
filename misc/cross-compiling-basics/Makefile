build:
	rm -rf build
	mkdir build
	riscv64-none-elf-gcc -march=rv64imac -mabi=lp64 -mcmodel=medany -nostdlib -T src/linker.ld src/start.S src/main.c -o build/prog.elf
	# qemu-system-riscv64 -machine virt -nographic -bios none -kernel build/prog.elf
	qemu-system-riscv64 -machine virt -bios none -kernel build/prog.elf
