// simple uart transmitter
// takes 8 bit data input and sends it serially
// with start bit, 8 data bits, stop bit

// trigger transmission by setting tx_start high for 1 cycle
// it outputs the data one bit at a time on tx line
// tx_busy stays high while transmission is happening

// assumptions
// system clock is synchronous and baud rate is fixed

module uart_tx #(
  parameter clk_freq = 50000000, // system clock in hz
  parameter baud_rate = 9600 // baud rate
)(
  input wire clk,
  input wire rst, // active high reset
  input wire tx_start,
  input wire [7:0] tx_data,
  output reg tx,
  output reg tx_busy
);

  // number of clock cycles for one uart bit
  localparam clks_per_bit = clk_freq / baud_rate;

  // states for fsm.
  typedef enum logic [2:0] {
    idle,
    start_bit,
    data_bits,
    stop_bit,
    cleanup
  } state_t;

  state_t state = idle; // current state of fsm

  reg [15:0] clk_count = 0; // count clock cycles to time each bit
  reg [2:0] bit_index = 0; // index of the current data bit
  reg [7:0] data_reg = 0; // hold data to be transmitted

  always @(posedge clk or posedge rst) begin
    if (rst) begin
      // reset everything
      state <= idle;
      clk_count <= 0;
      bit_index <= 0;
      tx <= 1'b1; // idle state of uart line is high
      tx_busy <= 0;
    end else begin
      // state machine to handle transmission
      case (state)

        idle: begin
          tx <= 1'b1; // line stays high on idle
          clk_count <= 0;
          bit_index <= 0;
          tx_busy <= 0;

          if (tx_start) begin
            // start sending
            tx_busy <= 1;
            data_reg <= tx_data;
            state <= start_bit;
          end
        end

        start_bit: begin
          tx <= 1'b0; // start bit is always 0

          if (clk_count < clks_per_bit - 1) begin
            clk_count <= clk_count + 1;
          end else begin
            clk_count <= 0;
            state <= data_bits;
          end
        end

        data_bits: begin
          tx <= data_reg[bit_index]; // send current bit

          if (clk_count < clks_per_bit - 1) begin
            clk_count <= clk_count + 1;
          end else begin
            clk_count <= 0;

            if (bit_index < 7) begin
              bit_index <= bit_index + 1;
            end else begin
              bit_index <= 0;
              state <= stop_bit;
            end
          end
        end

        stop_bit: begin
          tx <= 1'b1; // stop bit is always 1

          if (clk_count < clks_per_bit - 1) begin
            clk_count <= clk_count + 1;
          end else begin
            clk_count <= 0;
            state <= cleanup;
          end
        end

        cleanup: begin
          // go back to idle
          // let tx_busy fall low
          tx_busy <= 0;
          state <= idle;
        end

        default: begin
          state <= idle;
        end

      endcase
    end
  end
endmodule
