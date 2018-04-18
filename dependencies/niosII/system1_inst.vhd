	component system1 is
		port (
			clk_clk                   : in  std_logic                     := 'X';             -- clk
			input0_extern_con_export  : in  std_logic_vector(31 downto 0) := (others => 'X'); -- export
			input1_extern_con_export  : in  std_logic_vector(31 downto 0) := (others => 'X'); -- export
			input2_extern_con_export  : in  std_logic_vector(31 downto 0) := (others => 'X'); -- export
			input3_extern_con_export  : in  std_logic_vector(31 downto 0) := (others => 'X'); -- export
			output0_extern_con_export : out std_logic_vector(31 downto 0);                    -- export
			output1_extern_con_export : out std_logic_vector(31 downto 0);                    -- export
			output2_extern_con_export : out std_logic_vector(31 downto 0);                    -- export
			output3_extern_con_export : out std_logic_vector(31 downto 0);                    -- export
			reset_reset_n             : in  std_logic                     := 'X'              -- reset_n
		);
	end component system1;

	u0 : component system1
		port map (
			clk_clk                   => CONNECTED_TO_clk_clk,                   --                clk.clk
			input0_extern_con_export  => CONNECTED_TO_input0_extern_con_export,  --  input0_extern_con.export
			input1_extern_con_export  => CONNECTED_TO_input1_extern_con_export,  --  input1_extern_con.export
			input2_extern_con_export  => CONNECTED_TO_input2_extern_con_export,  --  input2_extern_con.export
			input3_extern_con_export  => CONNECTED_TO_input3_extern_con_export,  --  input3_extern_con.export
			output0_extern_con_export => CONNECTED_TO_output0_extern_con_export, -- output0_extern_con.export
			output1_extern_con_export => CONNECTED_TO_output1_extern_con_export, -- output1_extern_con.export
			output2_extern_con_export => CONNECTED_TO_output2_extern_con_export, -- output2_extern_con.export
			output3_extern_con_export => CONNECTED_TO_output3_extern_con_export, -- output3_extern_con.export
			reset_reset_n             => CONNECTED_TO_reset_reset_n              --              reset.reset_n
		);

