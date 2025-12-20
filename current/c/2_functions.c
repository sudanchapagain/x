#include <stdio.h>
#include <string.h>

/*

    since C requires that when a function is called it's type to be known ahead, using functions then defining
    is not correct C program. as such, C also splits the concepts of function declaration (prototype) and function
    definition (implementation)

    // this is not valid
    int main(void) {
        func(); // func is not known till now but we are using it
    }

    void func(void) {
        printf("hello");
    }


    // this is valid
    void func(void) {
        printf("hello");
    }

    int main(void) {
        func();
    }


    // if we want to write functions after main. we can declare them at the top and define later
    void func(void);

    int main(void) {
        func();
    }

    void func(void) {
        printf("hello");
    }

*/


int returner(void);
void header(void);
void storage(void);


// functions have this structure
// return_type function_name(argument_type arguments) { function_body }
int main(int argc, char* argv[]) {
    // here the return type is 'int' which means the function can only return int values
    // the function name is 'main'
    // arguments are:
    //    'argc' of type int representing the number of command-line arguments, including the program name itself.
    //    'argv' of type array of strings where each string is an argument


    // for example, if we run:
    //     ./a.out -v -o file.txt
    // 
    // then:
    //     argc == 4
    //     argv[0] == "./program"
    //     argv[1] == "-v"
    //     argv[2] == "-o"
    //     argv[3] == "file.txt"
    //
    // the names argc, and argv are conventions.


    // as shown above we can extract the arguments to build a cli or arg parser.
    // the following example is for a program that supports the following:
    //
    //    -v or --verbose   - a flag
    //    -o <file name>    - an option requiring a value
    //    --output=<file>   - same as -o but in long form
    //
    // so, we could run the program in either form "./a.out -o file.txt" or "./a.out --output=file.txt"

    int verbose = 0; // to store whether verbose mode is on
    // this will store a file name if passed. by default we are assigning NULL to denote nothing. it is a type that
    // comes with C
    const char *output_file = NULL;

    // we can use argc (number of arguments to loop through all )
    for (int i = 1; i < argc; i++) {
        char *arg = argv[i];

        // check verbose flag
        if (strcmp(arg, "-v") == 0 || strcmp(arg, "--verbose") == 0) {
            verbose = 1;
        } else if (strcmp(arg, "-o") == 0) { // -o which requires a value
            if (i + 1 < argc) { // additional arg is required so check for it
                output_file = argv[++i]; // move to next arg for the value
            } else {
                fprintf(stderr, "error: -o requires a file name\n");
                return 1;
            }
        } else if (strncmp(arg, "--output=", 9) == 0) { // check --output=<file>
            output_file = arg + 9; // to skip '--output=' which is 9 chars long
        } else if (arg[0] == '-') { // unknown flags
            fprintf(stderr, "unknown option: %s\n", arg);
            return 1;
        } else { // anything else is treated as a positional argument
            printf("positional argument: %s\n", arg);
        }
    }

    printf("verbose: %s\n", verbose ? "on" : "off");

    if (output_file) printf("output file: %s\n", output_file);
    else printf("no output file provided.\n");

    /*
       this is fine for simple programs but this becomes more and more of a hassle to handle as our program and
       configuration grows. luckily, there are libraries to help with this process like Getopt, Argp, 
       see examples from Christian Hujer at: <https://stackoverflow.com/a/24479532>
    */

    // the main function should always return something as a way to report back to the os on the status of the program
    // 0 means everything went well. 1 means error occured.
    // todo: read more on status code.
    int res_from_returner = returner();

    header();
    storage();

    return 0;
}


int returner(void) {
    int a = 0;

    printf("return value: ");
    scanf("%d", &a);

    return a;
}

void header(void) {
    /*

    the include statements (#include) being used are preprocessor directives. more clearly, every statement
    that begins with # is a a preprocessor directives. these preprocessor directives extend only across a single
    line of code. as soon as a newline character is found, the preprocessor directive is ends. no semicolon (;)
    is expected at the end of a preprocessor directive. the only way a preprocessor directive can extend through
    more than one line is by preceding the newline character at the end of the line by a backslash (\).

    to define preprocessor macros we can use #define. its syntax is:

    #define identifier replacement

    when the preprocessor encounters this directive, it replaces any occurrence of identifier in the rest of the
    code by replacement. this replacement can be an expression, a statement, a block or simply anything.

    consider #define PI 3.14
    ------------------------
        the #define defines PI as 3.14 which translates to anywhere PI is used, the preprocessor deletes it and
        inserts the literal value of 3.14. we can use `#undef` to undefine the macro.

        #define LIMIT 5
        #undef LIMIT
     
        printf("%d", LIMIT); // since LIMIT has been undefined, this cannot compile.

        we can define functions in #define too

        syntax:
        -------
             #define foo(a, b) a + b
             #define func(r) r * r

        example:
        --------
            #define AREA(l, b) (l * b)

            int a = 10, b = 5;
            printf("%d", AREA(a, b));
            return 0;
*/


/*
     function macro definitions accept two special operators (# and ##) in the replacement sequence: the operator
     #, followed by a parameter name, is replaced by a string literal that contains the argument passed (as if
     enclosed between double quotes):

    example:
    --------
        #define str(x) #x
        printf("%s", str(test)); // translates to printf("%s", "test");


    the operator ## concatenates two arguments leaving no blank spaces between them:

    example:
    --------
        #define glue(a,b) a ## b
        glue(p,rintf("test"));

    */

    /*

    consider `#include <stdio.h>`
    -----------------------------

    the `#include` is 'file inclusion' which means it includes other files. in this case, it is including the
    `stdio.h` file in place where the statement exists.

    the .h in file name is a convention refering to header. header files end with `.h` extension and contains
    functions, data types, macros, etc that can be used by any other C programs that includes that particular
    header file using "#include" preprocessor.

    there are two ways to "include":

    1.  standard header files
        ---------------------

        the header files that are shipped alongside the compiler can be included with the `<>` syntax. it is generally
        called stdlib (standard library). as such, the `stdio.h` file is also a library shipped with the compiler.
        some libraries shipped alongside compiler includes:

        `<stdlib.h>`: various things in one. most notable functions would be malloc family of allocators.
        `<stdint.h>`, `<inttypes.h>`: integer handling for more portable code.
        `<stdbool.h>`: bool type

        `<stdio.h>`: input and output operations
        `<stdarg.h>`: standard argument functions like va_start() and va_arg().

        `<ctype.h>`: char type handling methods and more.
        `<wchar.h>`, `<wctype.h>`: wide character APIs.
        `<uchar.h>`: for UTF support
        `<string.h>`: string manipulation/handling

        `<assert.h>`: defines the assert macro.
        `<errno.h>`: macros for reporting and retrieving error conditions using the symbol errno.

        `<math.h>`: mathematical operations like sqrt(), log2(), pow(), etc.
        `<limits.h>`: information about the actual properties, such as size, of the basic arithmetic types.
        `<float.h>`: same as limits.h but macros for floating-point types.
        `<complex.h`: complex number handling

        `<time.h>`: date handling. contains date(), time(), setdate(), getdate(), etc.
        `<locale.h>`: to deal with localization

        `<signal.h>`: signal handling functions like signal() and raise().
        `<setjump.h>`: <https`://en.wikipedia.org/wiki/Setjmp.h>
        `<stddef.h>`: common type definitions (like definition of size of target processor's arithmetic capabilities, etc.).
       

       NOTE: this is not all of the libraries that are typically found in the system. for a complete list see here:
             - <https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3220.pdf>
             - <https://cppreference.com>
             - <https://gcc.gnu.org/onlinedocs/libstdc++/manual/using_headers.html>

       
    2.  user-defined/file hiearchy header files
        ---------------------------------------
        we can also, include header files that were not shipped with the compiler/system. first, lets use third
        party libraries and then look into building our own. to use header files we would use the path they
        are in with double quotes syntax instead of `<>`.

        example: assume we have a `gtk.h` header file to use the GTK library for building GUI applications in
        current folder then, we would use the following syntax:

            #include "gtk.h"

            // now, we can just start using gtk

            static void activate (GtkApplication *app, gpointer user_data) { ... }

            int main (int argc, char **argv) {
                GtkApplication *app;
                int status;

                app = gtk_application_new ("org.gtk.example", G_APPLICATION_DEFAULT_FLAGS);
                g_signal_connect (app, "activate", G_CALLBACK (activate), NULL);
                status = g_application_run (G_APPLICATION (app), argc, argv);
                g_object_unref (app);

                return status;
            }

        however, this requires that we go and download the header files for a library and then place them in the
        directory of our liking and then link to that path. there is a easier solution where we can point the library's
        path as a argument (we can use system package manager to fetch libraries and then use something that can query
        information of libraries installed in our system. in our case we can use something like pkg-config to find
        path of libraries) to the compiler driver who would then handle it for us.

        --WIP-MARKER

    */

}

void storage(void) {
    // type qualifiers
    // class in C - volatile, static, etc.
}
