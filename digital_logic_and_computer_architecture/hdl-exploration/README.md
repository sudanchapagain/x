`HDL`: Hardware Description Language
====================================

That is, languages that describes the circuits (wires, registers, combinational
logic, clocks, etc.). In other words, specification of hardware. The simulator
or synthesizer can take in the spec and simulate actual hardware behavior. We
do not need to program `wire` and `registers` with this and that properties
before being able to use them. Those are provided to us as primitives and
we describe `A` is wire and properties of wire would be assigned to `A`.

`Verilog` is the older language from the 1980s. It describes digital circuits
using modules, wires, registers, and always blocks. It is widely used for
`FPGA` and `ASIC` work.

The structure of a module is the following:

```text
module <module name> (<port list>);
<declares>
<module items>
endmodule
```

Example of `and` gate:

```verilog
module and_gate(
    input a,
    input b,
    output y
);
assign y = a & b;
endmodule
```

A better example:

```vh
// modules are like functions but they create hardware.
module top(
  output led_r // Arguments can be input, output or inout PCF file defines pin mappings
);
  wire clk; // wire is like a local variable, (except it doesn’t hold state)
  SB_HFOSC osc(1,1,clk); // Creates a “black box” module HFOSC = High-Frequency oscillator, Output 48 MHz into the clk wire
  reg [25:0] counter; // reg declares a local variable that holds state between clocks. (Can be a single bit or a vector)
                      // 26 bit register. 2^26 == 67,108,864

  always @(posedge clk)
    counter <= counter + 1; // Increments counter on positive edges of clk At 48 MHz, counter overflows every 1.4 s

  assign led_r = counter[25]; // Continuous assignment to the led_r output wire (not clocked)
                              // Turns on the LED when top bit is 0 (50% dutycycle, negative logic)
endmodule
```

Here, we use the primitives (input, output) and assign them to variables. The
output variable has it's property (i.e. `&` operation over `a` and `b`).

Other options include `SystemVerilog` & `VHDL`.

`SystemVerilog` is a modern extension of `Verilog` and `VHDL` is a stricter
and verbose language used by defense and aerospace industry primarily.

Some Fundamental Concepts
-------------------------

**Transistors**

Transistors are basically a controlled switch. When the gate voltage is high,
the transistor conducts. When gate voltage is low, it does not. Modern
CPUs use CMOS transistors. There are two types of MOSFET used in CMOS.

- `NMOS`: good at pulling the signal down to 0.
- `PMOS`: good at pulling the signal up to 1.

In CMOS, `PMOS` transistors are placed in parallel at the top and `NMOS`
transistors are placed in series at the bottom.

watch:

- [MOSFET Explained -How MOSFET Works](https://www.youtube.com/watch?v=AwRJsze_9m4)
- [CMOS Transistors](https://www.youtube.com/watch?v=K4IXY3f-Smw)

> The earliest microprocessors starting in 1970 were all MOS microprocessors,
> fabricated entirely from PMOS logic or fabricated entirely from nMOS logic.
> In the 1970s, MOS microprocessors were often contrasted with CMOS
> microprocessors and bipolar bit-slice processors.

- [Building logic gates from MOSFET transistors](https://www.youtube.com/watch?v=1rZyGL1K5QI)
- [Transistor Logic Gates - NAND, AND, OR, NOR](https://www.youtube.com/watch?v=OWlD7gL9gS0)
- [Implementation of Boolean Expressions using CMOS - NMOS, PMOS, Transistors, Examples, Explained](https://www.youtube.com/watch?v=iWcQN8duQpY)

**Logic Gates**

- [How Transistors Run Code](https://www.youtube.com/watch?v=HjneAhCy2N4)

A logic gate is a device that performs a Boolean function.

> Boolean function means takes in arbitrary number of binary input and produces
> a single binary output.

The primary way of building logic gates uses diodes or transistors acting
as electronic switches. Most logic gates are made from MOSFETs.

> They can also be constructed using vacuum tubes, electromagnetic relays with
> relay logic, fluidic logic, pneumatic logic, optics, molecules, acoustics,
> or even mechanical or thermal elements.

Logic gates can be cascaded which allows for construction of a physical model
of all Boolean logic. Logic circuits includes devices such as multiplexers,
registers, arithmetic logic units (ALUs), and computer memory, all the way up
through complete microprocessors, which may contain more than 100 million
logic gates.

> There are seven basic logic gates: NOT, OR, NOR, AND, NAND, XOR, XNOR.

1. Combinational Logic

  Type of digital logic that is implemented by Boolean circuits, where the
  output is a pure function of the present input only. Examples: adders,
  muxes, gates.
  ```verilog
  assign y = a ^ b
  ```

  see: [half-adder implementation](./src/half-adder.v)

2. Sequential Logic

  Sequential logic is a type of logic circuit whose output depends on the
  present value of its input signals and on the sequence of past inputs, the
  input history. Or in simpler words the output depends on state stored in
  registers.
  > Registers update on clock edges.
  ```verilog
  always @(posedge clk) begin
    q <= d;
  end
  ```
  This describes a flip-flop.
  > `<=` is non-blocking assignment used for registers.

  see: [counter implementation](./src/count.v)

3. Modules

  hardware is built hierarchically:
  ```verilog
  module adder(
    input [31:0] a,
    input [31:0] b,
    output [31:0] y
  );
  assign y = a + b;
  endmodule
  ```
  Then another module can instantiate it:
  ```verilog
  adder myadder(
    .a(x),
    .b(y),
    .y(z)
  );
  ```
  This is just connecting circuits together.

Some Basic Syntax
-----------------

**Signals**

```verilog
wire a; reg b;
```

**Bit vectors**

```verilog
wire [7:0] data;
```

**Clocked block**

```verilog
always @(poseedge clk)
```

**Combinational blocks**

```verilog
always @(*)
```

### Data Types:

- Net: wire (represents a continuous value. default value is `Z`)
- Reg: represents a value stored over time. default value is `X`

Integer: 32-bit signed ints
Real: stores floating point numbers

```verilog
module example(input clk, input A, input B, output C);
	wire w; // wire declaration
	reg r; // reg decl.
	assign w = A & B; // wire continuous assign.

	//      +-------- sensitive to what? (edge of clock in this case)
	//      v
	always @ (posedge clk) begin // procedural assignment.
		r = A;
	end
endmodule
```

### Operators:

- Logical: `&` as AND, `|`, as OR `^`, as XOR `~`, as NOT `^~` as XNOR
- Boolean: `&&` as AND, `||` as OR, `!` as NOT
- Relational: `==`, `!=`, `>`, `<`, `>=`, `<=`
- Arithmetic: `+`, `-`, `*`, `/`
- Shift: `<<`, `>>`, `>>>`
- Reduction:
	- `&` reduce via AND,
	- `~&` reduce via NAND,
	- `|` reduce via OR,
	- `~|` reduce via NOR,
	- `^` reduce via XOR,
	- `^~` reduce via XNOR.
- Others: `{}` Concatenate, `{N{}}` replicate `N` times

```verilog
module adder(
	input [3:0] A, // A(3), A(2), A(1), A(0) i.e A = 1010 (4 bit number)
	input [3:0] B,
	output [3:0] Sum,
	output Cout
);
	assign {Cout, Sum} = A + B;
endmodule
```

### Assignments

| Continuous Assignment | Procedural Assignment |
| --------------------- | --------------------- |
| used outside of procedural blocks | inside of procedural blocks |
| it drives values onto nets | updates register and memory data types |
| automatically active at time zero | evaluated when the statement is encountered |
| contitnues assignments auto updates the variables when RHS operand changes. | assignments made within procedural blocks like `always` or `initial` |
| example: `assign a = b & c` | `always @( ... )` |
| - | the 'blocking' or 'non-blocking' depends on their execution behavior within a block of code. |

### modelling styles

Different levels of abstraction and methodologies / styles used to describe hardware.

#### Gate Level

Model the design using logic gates like AND, OR, etc.

```verilog
module MUX2to1 (
	input a,
	input b,
	input sel,
	output y
);
	wire not_sel, and1, and2;

//   +---------- gates
//   v
	not (and1, a, not_sel);
	and (and1, a, not_sel);
	and (and2, b, sel);

	or (y, and1, and2)
endmodule
```

#### Dataflow

Uses assign statements to model the flow of data thorugh the circuit. Mainly
used to describe combinational circuits. It's an intermediate level of
abstraction between behavioral and gate-level designs.

```verilog
module MUX2to1 (
	input a,
	input b,
	input sel,
	output y
);
	assign y = (sel == 1'b1) ? b : a;
endmodule
```

#### Behavioral

Describes the functionality of the hardware using a high level approach with
always blocks and if-else, case, and other statements. It is used to describe
the complex circuits (primarily sequential ones).

```verilog
module MUX2to1 (
	input a,
	input b,
	input sel,
	output reg y
);
	always @ (sel or a or b) begin
		if (sel)
			y = b;
		else
			y = a;
	end
endmodule
```

```verilog
module mux_8to1_behavioral (
	input wire [7:0] d,
	input wire [2:0] sel,
	output reg y
);
	always @(*) begin
		case (sel)
			3'b000: y = d[0];
			3'b001: y = d[1];
			3'b010: y = d[2];
			3'b011: y = d[3];
			3'b100: y = d[4];
			3'b101: y = d[5];
			3'b110: y = d[6];
			3'b111: y = d[7];
			default: y = 1'b0;
		endcase
	end
endmodule
```

##### Priority Encoder of `8x3` with if-else:

```verilog
module Priority_Encoder_8to3 (
	input [7:0] in,
	output reg [2:0] out,
	output reg valid
);
	always @(*) begin
		if (in[7]) begin
			out = 3'b111;
			valid = 1;
		end
		else if (in[6]) begin
			out = 3'b110;
			valid = 1;
		end
		else if (in[5]) begin
			out = 3'b101;
			valid = 1;
		end
		else if (in[4]) begin
			out = 3'b100;
			valid = 1;
		end
		else if (in[3]) begin
			out = 3'b011;
			valid = 1;
		end
		else if (in[2]) begin
			out = 3'b010;
			valid = 1;
		end
		else if (in[1]) begin
			out = 3'b001;
			valid = 1;
		end
		else if (in[0]) begin
			out = 3'b000;
			valid = 1;
		end
		else begin
			out = 3'bXXX;
			valid = 0;
		end
	end
endmodule
```

with `casex`:

```verilog
module Priority_Encoder_8to3 (
	input [7:0] in,
	output reg [2:0] out,
	output reg valid
);
	always @(*) begin
		casex (in)
			// +--> if this is one but rest is whatever, then this case is triggered
			// |
			// v
			8'b1xxxxxxx: out = 3'b111; // input 7
			8'b01xxxxxx: out = 3'b110; // input 6
			8'b001xxxxx: out = 3'b101; // input 5
			8'b0001xxxx: out = 3'b100; // input 4
			8'b00001xxx: out = 3'b011; // input 3
			8'b000001xx: out = 3'b010; // input 2
			8'b0000001x: out = 3'b001; // input 1
			8'b00000001: out = 3'b000; // input 0
			default: out = 3'b000; // no input
		endcase
		valid = (in != 8'b00000000);
	end
endmodule
```

##### Sequential Circuits

###### D Flip Flop

truth table:

| clk | reset | d | q |
| --- | ----- | - | - |
|  ↑  |   0   | x | 0 |
|  ↑  |   1   | 1 | 1 |
|  ↑  |   1   | 0 | 0 |

```verilog
module D_FF(input clk, input reset, input d, output reg q);
	always @(posedge clk) begin
		if (reset)
			q <= 0;
		else
			q <= d;
	end
endmodule
```

###### SR Flip Flop

| clk | rst | S | R | Q        |
| --- | --- | - | - | -------- |
|  1  |  1  | x | x | 0        |
|  1  |  0  | 0 | 0 | Q (hold) |
|  1  |  0  | 0 | 1 | 0        |
|  1  |  0  | 1 | 0 | 1        |
|  1  |  0  | 1 | 1 | Z        |

```verilog
module sr_flipflop (input S, input R, input clk, output reg Q);
	always @(posedge clk) begin
		case ({S, R})
			1'b00: Q <= Q; // no change
			1'b01: Q <= 1'b0; // Reset (R=1, S=0), set Q to 0
			1'b10: Q <= 1'b1; // set (S=1, R=0), set Q to 1
			1'b11: Q <= 1'bZ; // invalid state (S=1, R=1), output is undefined
		endcase
	end
endmodule
```

###### T Flip Flop

| clk | rst | T | q           |
| --- | --- | - | ----------- |
|  1  |  1  | x | 0           |
|  1  |  0  | 1 | Q' (toggle) |
|  1  |  0  | 0 | Q (hold)    |

```verilog
module t_ff (input clk, rst, input t, output reg 1);
	always @(posedge clk) begin
		if (rst) q <= 0; // active high reset
		else
			begin
				if (t) q <= ~q;
				else q <= q;
			end
	end
endmodule
```

###### JK Flip Flop

| clk | rst | j | k | Q           |
| --- | --- | - | - | ----------- |
|  1  |  1  | x | x | 0           |
|  1  |  0  | 0 | 0 | Q (hold)    |
|  1  |  0  | 0 | 1 | 0           |
|  1  |  0  | 1 | 0 | 1           |
|  1  |  0  | 1 | 1 | Q' (toggle) |

```verilog
module jk_flipflop (input J, input K, input clk, output reg Q);
	always @(posedge clk) begin
		case ({J, K})
			1'b00: Q <= Q; // no change (hold the value)
			1'b01: Q <= 1'b0;
			1'b10: Q <= 1'b1;
			1'b11: Q <= ~Q;
		endcase
	end
endmodule
```

### Blocking Assignment vs Non Blocking Assignment

Blocking assignments are represented by the equal sign `=`. It executes in
series. It is used in combinational circuit design.

Non blocking assignments are represented by the less than equal sign `<=`.
It executes in parallel. It is used in sequential circuit design.

### resource

- [Mastering Verilog | Beginners to Advanced](https://www.youtube.com/watch?v=YUB-OyGr1oA)
- [Mastering System Verilog | Complete Guide](https://www.youtube.com/watch?v=FWtEZHJhdnM)


Simulation and Testing
----------------------

We write a `testbench` that feeds signals to our design and checks outputs.
Example:

```verilog
module tb;

reg clk;
reg a;
reg b;
wire y;

and_gate dut(
  .a(a),
  .b(v),
  .y(y)
);

initial begin
  a = 0;
  b = 0;

  #10 a = 1;
  #10 b = 1;
end

endmodule
```

This can be run with tools like `icarus verilog`, `verilator`. It then can be
viewed with `GTKWave`

`uart` Verification/Simulation
----------------------------

```sh
iverilog -g2012 src/uart.sv testbench/uart.v -o sim.out
vvp sim.out
gtkwave uart.vcd
```

## Links

- <https://github.com/osresearch/fpga-class>
- <https://www.fpga4fun.com>
- <https://github.com/osresearch/up5k>
