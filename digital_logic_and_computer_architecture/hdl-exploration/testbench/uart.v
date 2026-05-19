`timescale 1ns/1ps

module uart_tx_tb;
  reg clk = 0;
  reg rst = 1;
  reg tx_start = 0;
  reg [7:0] tx_data = 8'h00;
  wire tx;
  wire tx_busy;
  uart_tx #(
    .clk_freq(50000000),
    .baud_rate(9600)
  ) dut (
    .clk(clk),
    .rst(rst),
    .tx_start(tx_start),
    .tx_data(tx_data),
    .tx(tx),
    .tx_busy(tx_busy)
  );
  // 50 MHz clock
  always #10 clk = ~clk;
  initial begin
    $dumpfile("uart.vcd");
    $dumpvars(0, uart_tx_tb);     // include testbench
    $dumpvars(0, uart_tx_tb.dut); // include DUT signals
    #100 rst = 0;
    // send a byte
    #50;
    tx_data = 8'hA5;
    tx_start = 1;
    #20;
    tx_start = 0;
    // wait
    #200000;
    $finish;
  end
endmodule
