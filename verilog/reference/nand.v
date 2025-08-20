// https://hdlplanet.tripod.com/verilog/verilog-manual.html



// Behavioral Model of a Nand gate
// By Dan Hyde, August 9, 1995
module NAND(in1, in2, out);

  input in1, in2;
  output out;
                 // continuous assign statement
  assign out = ~(in1 & in2);

endmodule

// The continuous assignment statement is used to model combinational
// circuits where the outputs change when one wiggles the input.

//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

module AND(in1, in2, out);
// Structural model of AND gate from two NANDS
  input in1, in2;
  output out;
  wire w1;
                 // two instances of the module NAND
  NAND NAND1(in1, in2, w1);
  NAND NAND2(w1, w1, out);

endmodule

// The general form to invoke an instance of a module is :
// <module name>  <parameter list> <instance name> (<port list>);

