// include standard io library
#include <stdio.h>

/*

    introduction
    ------------

    C language is not a language in the traditional sense with a single compiler toolchain. C should be better
    understood has specification which is implemented by various independent compilers. some of the examples include:
    GCC, Clang, MSVC, etc.

    GCC, Clang, etc are not really compilers either. they are compiler toolchains consisting of various programs that
    orchestrate the compilation process. the command line "application" that we interact is the compiler driver which
    is responsible for performing the compilation steps according to the command line args.

    stages of compilation
    ---------------------

    ```
    [compiler driver] -> parses args and starts compilation
      │
      ╰─[pre-processor] -> expands macros
              │
              ╰─[compiler] -> generates assembly code
                     │
                     ╰─[assembler] -> generates object files
                            │
                            ╰─[linker] -> links with libraries and executable is produced
    ```
*/

// this is the execution start point
int main(void) {
    // data types constitute the semantics & characteristics of storage of data elements.
    // data types also determine the types of operations or methods of processing of data elements.
    int num1 = 20; // integer
    float num2 = 20.3; // floating point number (single precision IEEE 754)
    double num3 = 30.59; // double-precision floating-point IEEE 754
    char character = 'a'; // a character
    char* string = "my name is"; // a string. (pointer to array of characters)
    void *num_is_null = NULL; // absense of value. it's points to void. more

    // we can put two double quotes anywhere in a string
    char *str1 = "string""is one";

    // we can put space line break between two double quotes
    char *str2 = "far"     "apart";
    char *str3 = "new"    
                "lines";
    // this can be used for multi lines strings


    // if we want to specify something that cannot change or reassigned use const
    const int cannot_change_num = 10;
    // cannot_change_num = 11; -> this cannot be compiled


    // puts and printf comes from standard io
    // puts quite literally puts string in the console
    puts("show things at the console");
    // printf allows for formatted strings.
    printf("%s", "hello");
    printf("program in C!\n");
    printf("printing ints %d\n", num1);
    printf("printing floats %f\n", num2);
    printf("printing chars %c\n", character);
    printf("printing strings %s\n", string);


    // type cast
    float int_to_float = num1; // implicit (the int is being upgraded to float automatically)
    int explicit_int_conversion = (int)num2; // explicit (we are specifying the conversion)


    // more subtle implicit conversion is as follows:
    int implicit_conv_1 = 3;
    float implicit_conv_2 = 2.5;
    float implicit_conv_res = implicit_conv_1 + implicit_conv_2; // x is turned to float for sum to be perfomed


    // narrowing conversions are harmful as they are information loss
    double narrow_conv = 3.14;
    int narrow_conv2 = narrow_conv; // fractional part lost

    /*

    we can use compiler flags to detect unsafe parts of C. for example `-Wconversion` will warm on implicit conversions
    when compiling. as a general rule of thumb, we should use as much of warning flags as possible. however,
    some flags might make things harder than necessary for non-strict project. below is example of flags commonly
    used in most project

    -Wall -Wextra -Wpedantic -Wshadow -Wconversion -Wsign-conversion -Wcast-qual -Wcast-align -Wnull-dereference
    -Wformat=2 -Wunused

    */

    // there are more types in C
    unsigned int unsigned_integer_num1 = 10; // unsigned basically translates to cannot be negative
    signed char sig_char_l1 = 'a';
    unsigned char unsig_char_l2 = '2';
    short int short_int_n1 = 29;
    short short_n2 = 2;
    double double_float_n1 = 10.10;
    long double double_long_float_n1 = 10.10;
    long int long_int_n1 = 29;
    long long_n2 = 2;
    long long int long_long_int_n1 = 29;
    long long long_long_n2 = 2;
    unsigned long long unsig_long_long_n1 = 1;


    // The problem with types in C is that the size of types is left to the implementation.
    /*

    | type      | 32 bit | 64 bit | guranteed by the standard |
    | --------- | ------ | ------ | ------------------------- |
    | int       | 32     | 32     | >= 16 bits                |
    | long      | 32     | 64     | >= 32 bits                |
    | long long | 64     | 64     | >= 64 bits                |
    | pointer   | 32     | 64     | none                      |

    contratary to most people on the internet this was not a bad design when C was made rather a ingenious decision
    to solve the portability problem in 1972.

    however, this is a problem as programmers would now be making assumptions of the underlying systems but the
    underlying mechanisms are not guranteed. C99 introduced <stdint.h> as a way to define fixed size types.

    we can just include the header and get going as below
        // the t postfix stands for type.
        #include <stdint.h>

        int8_t int_of_size_eight = 8;
        uint8_t unsigned_int_of_size_8 = 8;
        int16_t int_sixteen = 16;

    there are also least N variants for portability. basically:
        the typedef name `intN_t` designates a signed integer type with width N, no padding bits, and a two's
        complement representation. thus, int8_t denotes a signed integer type with a width of exactly 8 bits.

        the typedef name `int_leastN_t` designates a signed integer type with a width of at least N, such that
        no signed integer type with lesser size has at least the specified width. thus, int_least32_t denotes a
        signed integer type with a width of at least 32 bits.

        the typedef name `int_fastN_t` designates the fastest signed integer type with a width of at least N. the
        typedef name uint_fastN_t designates the fastest unsigned integer type with a width of at least N.

    finally there are also pointer variants. (`intptr_t`, `uintptr_t`)

    for chars: <https://en.wikipedia.org/wiki/Wide_character>

    to use boolean values use <stdbool.h> (bool is a macro that expands to _Bool in stdbool.h)
        #include <stdbool.h>

        int main(void) {
            bool a = true;
            bool b = false;
            printf("%d\n", a);
        }

    size_t from <stddef.h> / <stding.h> is a unsigned interger. it is not same as `unsigned int` for example on
    64 bit linux int is always 32 bit but size_t is 64 bit. so size_t and int are not interchangeable. ssize_t is
    signed counterpart.

    to be happy with state of affairs we should use UBSan and ASan including clang-tidy, cppcheck, etc.

    see: <https://en.wikipedia.org/wiki/C_data_types>

    */


    // we can do arithmetic operations over numbers
    printf("%d\n", 1 + 2);
    printf("%d\n", 1 + 2 - 3 * 4 / 5 % 6); // the order of operations is left to right multiplication, division,
                                           // & remainder, then addition, subtraction.

    // we can order the operations with () as it has higher precedence
    printf("%d\n", ((((1 + 2) - 3) * 4) / 5) % 6);
    /*

    Source: <https://en.cppreference.com/w/c/language/operator_precedence.html>

    | operator     | description                                  | associativity          |
    | ------------ | -------------------------------------------- | ---------------------- |
    | ++, --       | suffix/postfix increment and decrement       | left-to-right          |
    | ()           | function call                                |                        |
    | []           | array subscripting                           |                        |
    | .            | struct & union member access                 |                        |
    | ->           | struct & union member access with pointer    |                        |
    | (type){list} | compound literal(C99)                        |                        |
    | ++, --       | prefix increment and decrement               | right-to-left          |
    | +, -         | unary plus and minus                         |                        |
    | !, ~         | logical not and bitwise not                  |                        |
    | (_type_)     | cast                                         |                        |
    | *            | indirection (dereference)                    |                        |
    | &            | address-of                                   |                        |
    | sizeof       | size-of                                      |                        |
    | _Alignof     | alignment requirement(C11)                   |                        |
    | *, /, %      | multiplication, division, and remainder      | left-to-right          |
    | +, -         | addition and subtraction                     |                        |
    | <<, >>       | bitwise left shift and right shift           |                        |
    | <, <=        | for relational operators < & <= respectively |                        |
    | >, >=        | for relational operators > & >= respectively |                        |
    | ==, !=       | for relational = and != respectively         |                        |
    | &            | bitwise and                                  |                        |
    | ^            | bitwise xor (exclusive or)                   |                        |
    | |            |                                              | bitwise or (inclusive) |
    | &&           | logical and                                  |                        |
    | ||           |                                              | logical or             |
    | ?:           | ternary conditional                          | right-to-left          |
    | =            | simple assignment                            |                        |
    | +=, -=       | assignment by sum and difference             |                        |
    | *=, /=, %=   | assign,, by product, quotient, & remainder   |                        |
    | <<=, >>=     | assign,, by bitwise left shift & right shift |                        |
    | &=, ^=, |=   | assign,, by bitwise and, xor, & or           |                        |
    | ,            | comma                                        | left-to-right          |
    */


    // c execute line by line. we can use conditionals to branch or change flow of program
    if (5 > 20) {
        printf("this will not be run as 5 is not greater than 20\n");
    } else {
        printf("5 is less than 20\n");
    }


    // if else can be nested
    if (5 > 20) {
        // return keyword evalutes a expression and returns the value to the caller (in this case, the shell from
        // where this program is being ran). if another function is calling it, then value is 'returned' to it.
        return 0;
    } else if (5 > 4) {
        if (5 == 5) {
            printf("5 is 5\n");
        } else {
            return 0;
        }
    } else {
        printf("hmm\n");
    }


    // we can use ternary operator to write concise conditionals
    // it's syntax is as follows
    // <expression> ? <on truthy> : <on falsy>
    // the first if else block from above can be written as
    5 > 20 ? printf("this will not be run as 5 is not greater than 20\n") : printf("5 is less than 20");
    // ternary too can be nested but it's not preferred as readability is lost


    // there is also another syntax to write concise if else blocks but with the limitation of only one statement
    // being allowed.
    if (5 > 20)
        printf("this will not be run as 5 is not greater than 20\n");
    else
        printf("5 is less than 20\n");

    // the line breaks are optional it could just be `if (5 > 20) printf("...");` too
    //
    // basically, for single line statements the braces are optional. however, we should be careful with this
    // see: <https://dwheeler.com/essays/apple-goto-fail.html>
    // opting to use {} everywhere is a better and safer choice. the two more keystrokes is not that big of a cost.


    size_t switch_variable_test = 5;
    switch (switch_variable_test) {
        case 1:
            printf("...\n");
            // break keyword breaks from the flow. it can be used inside if else statements, switch, loops, etc.
            // in this case after printing the control flow goes outside the switch block
            break;
        case 2:
            printf("...\n");
            break;
        case 3:
            printf("...\n");
            break;
        case 4:
            printf("...\n");
            break;
        case 5:
            printf("5 is a 5\n");
            break;
        case 6: // falls through as there is no break keyword so after 6 is selected there is nothing to execute or
                // stop it from moving on to next line (default in this case).
        default:
            printf("meow\n");
            break; // this is not needed as the switch ends here.
    }


    // the fall through logic can be used to select multiple options at once
    switch (switch_variable_test) {
        case 1:
        case 2:
        case 3:
        default: printf("hello"); // for 1, 2, 3 cases this would be executed
    }


    // however, its not really needed in this case, as we can just use logical or
    switch (switch_variable_test) {
        case 1 || 2 || 3: // this translates to: if 1 or 2 or 3 do this
        default: printf("hello");
    }


    // labels and goto work together to define to custom code path
    this_is_a_label:
        switch_variable_test = 0;
    if (switch_variable_test == 2) {
        goto this_is_a_label;
    }


    // loops can iterator over codeblocks with conditionals
    while (switch_variable_test > 2) {
        printf("looping\n");
        switch_variable_test--;
    }


    // do while block runs atleast once before checking for conditions
    do {
        switch_variable_test++;
    } while (switch_variable_test == 5);


    // for loops initliazes, checks condition, and updates statement in a concise manner
    for (int i = 0;   i <= 5;   i++    ) {
        /*

         `int i = 0` is initialization

         `i <= 5`    is condition that has to be true and when it turns false the loop is stopped

         `i++`       is the update code

         in this case, we make a new variable i of type int and assign 0
         then the code checks for the condition continuosly as follows

             - is i less than or equal to 5?
                yes
                loop
            - update i to be +1 and run the code inside of {}

            - is i less than or equal to 5?
                yes
                loop
            - update i to be +1 and run the code inside of {}
            ...
            ...

            until i's value turns to 5 and the condition is false so it breaks out.

        */
        printf("%d\n", i);
    }


    // we can use continue keyword to skip iterations inside of loops too. for example
    for (int i = 5; i == 0; i--) {
        if (i == 2) continue; // unlike break which stops everything, continue just skips a single iteration
        printf("this is iteration %d", i); // 2 should not be printed
    }


    // an example of using conditionals to manage control flow is given below
    // description: program to print text based pyramid
    char input_direction[10];
    printf("which direction of pyramid (left or right): ");
    scanf("%9s", input_direction);


    int is_left = (
        input_direction[0] == 'l' &&
        input_direction[1] == 'e' &&
        input_direction[2] == 'f' &&
        input_direction[3] == 't' &&
        input_direction[4] == '\0');

    int is_right = (
        input_direction[0] == 'r' &&
        input_direction[1] == 'i' &&
        input_direction[2] == 'g' &&
        input_direction[3] == 'h' &&
        input_direction[4] == 't' &&
        input_direction[5] == '\0');

    if (!is_left && !is_right) {
        printf("only left or right is accepted.\n");
        return 1;
    }

    for (int i = 1; i <= 5; i++) {
        if (is_right) {
            // for right alignment add spaces
            for (int s = 0; s < 5 - i; s++) {
                printf(" ");
            }
        }

        for (int j = 0; j < i; j++) {
            printf("*");
        }

        printf("\n");
    }


    /*

    working with raw strings is tiresome. there is <string.h> library which has functions for comparing strings
    the logic of comparing string from above can be reduced to this

        #include <string.h>

        if (
            strcmp(input_direction, "left") != 0
            &&
            strcmp(input_direction, "right") != 0
        ) {
            printf("only left or right is accepted.\n");
            return 1;
        }

     the above string comparision with <identifier>[N] was possible with usage of array string is just array of
     chars that ends with '\0' also called NULL terminator

     array is a data structure that stores a linear list of values of same data type.  this means, array need not
     necessarily be of chars.

    */


    // NOTE: if we are providing values during initialization then we do not need to pass size
    int array_of_integers[] = {1, 2, 3, 4, 5, 6}; 

    int uninit_array[3]; // can hold elements at 0th, 1st, 2nd positions (again it's 0 indexed which means we can
                         // declare how many elements. the index position of the last element will always be [N-1])

    // we can access the elements as such
    printf("%d\n", array_of_integers[0]); // prints first element (1).

    // combined with for loop, we can go through each and every element as such
    for (int i = 0; i <= 5 /* 6 elements but since its starts from 0, we go up to 5. */; i++) {
        printf("%d\n", array_of_integers[i]);
    }


    // if we do not know the size of array (num of elements in array), we can use sizeof operator
    int size_of_array_of_integers = sizeof(array_of_integers) / 4;
    // the sizeof operator returns the size in bits. since, int is of 4 bytes in my machine as such dividing by 4
    // results in actual number of elements
    printf("%d\n", size_of_array_of_integers);


    // array can hold other arrays inside. it's also known as multi-dimensional array or nested array
    // multi-dimensional arrays require size to be explicitly known.
    int array_of_arrays_of_arrays_of_integers[4][2][3] = {
        {
            {1, 2, 3},
            {4, 5, 6}
        },
        {
            {7, 8, 9},
            {10, 11, 12}
        },
        {
            {13, 14, 15},
            {16, 17, 18}
        },
        {
            {19, 20, 21},
            {22, 23, 24}
        }
    };
    // we can access it with index
    printf("%d\n", array_of_arrays_of_arrays_of_integers[1][1][1]); // prints 11

    // to iterate for these we can nest for loops
    printf("=========================\n");
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 2; j++) {
            for (int k = 0; k < 3; k++) {
                printf("%d\t", array_of_arrays_of_arrays_of_integers[i][j][k]);
            }
            printf("\n");
        }
        printf("\n");
    }

    // accessing the array's index which is outside of array's size is violation of assumptions made by the
    // C language and the compiler. doing so causes^* segfault (segmentation fault). not really as its actually
    // undefined behaviour (UB). writing to out of bounds would more likely crash the program but reading might be
    // sucessfull but it would be garbage value.

    //    int array[] = {1, 2, 3};
    //    printf("%d", array[5]); // this will be out of bounds

    // to avoid it, consider bounds checking. the integer array's are fairly easy and hard to mess up as using a
    // single conditional with size of array should be enough but working with strings and raw memory is dangerous.

    // instead of this:
    //    char src[] = "Hello, world!";
    //    char dest[50];
    //    strcpy(dest, src); // this code does not check if dest can hold src, potentially leading to buffer overflow.

    // do this:
    //    char src[] = "Hello, world!";
    //    char dest[50];
    //    strcpy_s(dest, sizeof(dest), src); // this code ensures that the copy operation does not exceed dest’s size.

    // ----------------

    // unsafe mem copy:
    //    char src[100] = {0};
    //    char dest[50];
    //    memcpy(dest, src, 100);
    //
    // safe mem copy:
    //    char src[100] = {0};
    //    char dest[50];
    //    memcpy_s(dest, sizeof(dest), src, 100);

    return 0;
}
