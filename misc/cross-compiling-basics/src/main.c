#define UART0 0x10000000

void uart_putc(char c) {
    *(volatile char*)UART0 = c;
}

int main() {
    const char *msg = "hello from riscv\n";
    while (*msg) {
        uart_putc(*msg++);
    }
    while (1);
}
