//By Dan Hyde; August 9, 1995
//A first digital model in Verilog

module simple;
// Simple Register Transfer Level (RTL) example to demo Verilog.
// The register A is incremented by one.  Then first four bits of B is
// set to "not" of the last four bits of A.  C is the "and" reduction
// of the last two bits of A.

//declare registers and flip-flops
reg [0:7] A, B;
reg       C;

// The two "initial"s and "always" will run concurrently
initial begin: stop_at
   // Will stop the execution after 20 simulation units.
   #20; $stop;    
end

// These statements done at simulation time 0 (since no #k)
initial begin: Init
    // Initialize the register A.  The other registers have values of "x"
    A = 0;   
  
    // Display a header
    $display("Time   A         B    C");  
            
    // Prints the values anytime a value of A, B or C changes
    $monitor("  %0d %b %b %b", $time, A, B, C);
end

//main_process will loop until simulation is over
always begin: main_process

    // #1 means do after one unit of simulation time
    #1 A = A + 1;
    #1 B[0:3] = ~A[4:7];  // ~ is bitwise "not" operator
    #1 C = &A[6:7];       // bitwise "and" reduction of last two bits
of A
    
end

endmodule

/*
In module simple, we declared A and B as 8-bit registers and C a 1-bit
register or flip-flop. Inside of the module, the one "always" and two
"initial" constructs describe three threads of control, i. e., they run at
the same time or concurrently. Within the initial construct, statements
are executed sequentially much like in C or other traditional imperative
programming languages. The always construct is the same as the initial
construct except that it loops forever as long as the simulation runs.

The notation #1 means to execute the statement after delay of one unit
of simulated time. Therefore, the thread of control caused by the first
initial construct will delay for 20 time units before calling the system
task $stop and stop the simulation.

The $display system task allows the designer to print a message much like
printf does in the language C. Every time unit that one of the listed
variables' value changes, the $monitor system task prints a message. The
system function $time returns the current value of simulated time.
*/
