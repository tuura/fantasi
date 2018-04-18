
module system1 (
	clk_clk,
	input0_extern_con_export,
	input1_extern_con_export,
	input2_extern_con_export,
	input3_extern_con_export,
	output0_extern_con_export,
	output1_extern_con_export,
	output2_extern_con_export,
	output3_extern_con_export,
	reset_reset_n);	

	input		clk_clk;
	input	[31:0]	input0_extern_con_export;
	input	[31:0]	input1_extern_con_export;
	input	[31:0]	input2_extern_con_export;
	input	[31:0]	input3_extern_con_export;
	output	[31:0]	output0_extern_con_export;
	output	[31:0]	output1_extern_con_export;
	output	[31:0]	output2_extern_con_export;
	output	[31:0]	output3_extern_con_export;
	input		reset_reset_n;
endmodule
