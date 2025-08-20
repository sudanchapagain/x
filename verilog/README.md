apparently to work with FPGAs you need the following.

we need to turn verilog to bitstream.

Yosys: Synthesis that turns verilog to logic

```sh
yosys -p "synth_ice40 -top blink -json blink.json" blink.v
nextpnr-ice40 --hx1k --json blink.json --pcf blink.pcf --asc blink.asc
icepack blink.asc blink.bin
iceprog blink.bin
```

Icestorm: Bitstream manipulation tools <http://bit.ly/icestorm-tools>
(unpack it to `/usr/local`). `tar -C /usr/local -xvf ~/Downloads/..` for
Lattice iCE40 chips

arachne-pnr or nextpnr-ice40: place and route (layout designer)

## structure of verilog

 The structure of a module is the following:

```text
module <module name> (<port list>);
<declares>
<module items>
endmodule
```

## example

`blink.v`

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

## Verilog types

- wire
- reg
- arrays
- module
- inferred blocks
- black box modules
- always @(*), always @(posedge clk)
- `define
- `ifdef
- `include
- generate

- <ftp://iii.net/pub/pub-site/wellspring>
- <https://github.com/osresearch/fpga-class>
- <https://www.fpga4fun.com>
- <https://github.com/osresearch/up5k>

see in notes for more.
