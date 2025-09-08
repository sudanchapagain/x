module Counter(
    input wire clk,
    input wire reset,
    output reg [3:0] count
);

always @(posedge clk) begin
    if (reset)
        count <= 4'b0000; // 4' is 4 bit binary number
    else
        count <= count + 1;
end

endmodule
