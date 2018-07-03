LIBRARY ieee;
USE ieee.std_logic_1164.all;

LIBRARY work;

ENTITY TOP IS
	GENERIC (
		RESULT_WIDTH : integer := 9;
		NODES : integer := 15;
		ADDR_SHIFT : integer := 4);
	PORT (
		--//////////// CLOCK //////////
		GCLKIN : IN	std_logic;
		GCLKOUT_FPGA : OUT	std_logic;
		OSC_50_BANK2 : IN	std_logic;
		OSC_50_BANK3 : IN	std_logic;
		OSC_50_BANK4 : IN	std_logic;
		OSC_50_BANK5 : IN	std_logic;
		OSC_50_BANK6 : IN	std_logic;
		OSC_50_BANK7 : IN	std_logic;
		PLL_CLKIN_p : IN	std_logic;

		--//////////// LED x 8 //////////
		LED : OUT std_logic_vector(7 downto 0);

		--//////////// BUTTON x 4, EXT_IO and CPU_RESET_n //////////
		BUTTON : IN std_logic_vector(3 downto 0);
		CPU_RESET_n : IN	std_logic;
		EXT_IO : IN	std_logic;

		--//////////// SLIDE SWITCH x 4 //////////
		SLIDE_SW : IN std_logic_vector(3 downto 0);

		--//////////// SEG7 //////////
		SEG0_D : OUT std_logic_vector(7 downto 0);
		SEG0_DP : OUT	std_logic;
		SEG1_D : OUT std_logic_vector(7 downto 0);
		SEG1_DP : OUT	std_logic;

		--//////////// Temperature //////////
		TEMP_INT_n : IN	std_logic;
		TEMP_SMCLK : OUT	std_logic;
		TEMP_SMDAT : IN	std_logic;

		--//////////// Current //////////
		CSENSE_ADC_FO : OUT	std_logic;
		CSENSE_CS_n : OUT std_logic_vector(1 downto 0);
		CSENSE_SCK : OUT	std_logic;
		CSENSE_SDI : OUT	std_logic;
		CSENSE_SDO : IN	std_logic;

		--//////////// Fan //////////
		FAN_CTRL : OUT	std_logic;

		--//////////// Flash and SRAM Address/Data Share Bus //////////
		FSM_A : OUT std_logic_vector(20 downto 1);
		FSM_D : INOUT std_logic_vector(15 downto 0);

		--//////////// SSRAM Control //////////
		SSRAM_ADV : OUT	std_logic;
		SSRAM_BWA_n : OUT	std_logic;
		SSRAM_BWB_n : OUT	std_logic;
		SSRAM_CE_n : OUT	std_logic;
		SSRAM_CKE_n : OUT	std_logic;
		SSRAM_CLK : OUT	std_logic;
		SSRAM_OE_n : OUT	std_logic;
		SSRAM_WE_n : OUT	std_logic;

		--//////////// 3-Ports High-Speed USB OTG //////////
		OTG_A : OUT std_logic_vector(17 downto 1);
		OTG_CS_n : OUT	std_logic;
		OTG_D : INOUT std_logic_vector(31 downto 0);
		OTG_DC_DACK : OUT	std_logic;
		OTG_DC_DREQ : IN	std_logic;
		OTG_DC_IRQ : IN	std_logic;
		OTG_HC_DACK : OUT	std_logic;
		OTG_HC_DREQ : IN	std_logic;
		OTG_HC_IRQ : IN	std_logic;
		OTG_OE_n : OUT	std_logic;
		OTG_RESET_n : OUT	std_logic;
		OTG_WE_n : OUT	std_logic);
END TOP;

ARCHITECTURE Behavioural OF TOP IS

	COMPONENT system1 IS
		PORT (
			clk_clk							: IN	std_logic;
			reset_reset_n					: IN	std_logic;
			input0_extern_con_export	: IN	std_logic_vector(31 downto 0);
			input1_extern_con_export	: IN	std_logic_vector(31 downto 0);
			input2_extern_con_export	: IN	std_logic_vector(31 downto 0);
			input3_extern_con_export	: IN	std_logic_vector(31 downto 0);
			output0_extern_con_export	: OUT	std_logic_vector(31 downto 0);
			output1_extern_con_export	: OUT	std_logic_vector(31 downto 0);
			output2_extern_con_export	: OUT	std_logic_vector(31 downto 0);
			output3_extern_con_export	: OUT	std_logic_vector(31 downto 0));
	END COMPONENT;
	
	COMPONENT pll_100mhz IS 
		PORT (
			inclk0 : IN std_logic;
			c0 : OUT std_logic);
	END COMPONENT;
	
	-- FANTASI	
	COMPONENT FSM IS
		GENERIC (DATA_WIDTH : integer;
					 NODES : integer);
		PORT (
			CLK	: IN	std_logic;
			RST	: IN	std_logic;
			GO		: IN std_logic;
			COMPL : OUT std_logic;
			EN_NODES : in std_logic_vector(NODES-1 downto 0);
			RESULT	: OUT	std_logic_vector(DATA_WIDTH downto 0));
	END COMPONENT;
	
	COMPONENT FSM_ENABLE_NODES IS
    GENERIC (NODES : integer;
				 ADDR : integer);
    PORT (
        CLK 	: IN    std_logic;
		  RST		: IN	  std_logic;
		  EN		: IN	  std_logic;
        M_SET 	: IN    std_logic;
        ZERO	: IN    std_logic;
        ONE 	: IN    std_logic;
		  DIN		: IN	  std_logic_vector(ADDR-1 downto 0);
		  SH_DONE: OUT	  std_logic;
        DOUT   : OUT   std_logic_vector(NODES-1 downto 0));
	END COMPONENT;
	
	component rgen1 is
		port (
			clock          : in  std_logic                     := 'X'; -- clk
			resetn         : in  std_logic                     := 'X'; -- reset_n
			rand_num_data  : out std_logic_vector(31 downto 0);        -- data
			rand_num_ready : in  std_logic                     := 'X'; -- ready
			rand_num_valid : out std_logic;                            -- valid
			start          : in  std_logic                     := 'X'  -- enable
		);
	end component rgen1;

	signal clk : std_logic;
	signal reset, reset_n : std_logic;
	
	-- Nios
	signal in_port_to_the_input0 : std_logic_vector(31 downto 0);
	signal in_port_to_the_input1 : std_logic_vector(31 downto 0);
	signal in_port_to_the_input2 : std_logic_vector(31 downto 0);
	signal in_port_to_the_input3 : std_logic_vector(31 downto 0);
	signal out_port_from_the_output0 : std_logic_vector(31 downto 0);
	signal out_port_from_the_output1 : std_logic_vector(31 downto 0);
	signal out_port_from_the_output2 : std_logic_vector(31 downto 0);
	signal out_port_from_the_output3 : std_logic_vector(31 downto 0);
	
	-- Wires for FSMs connection
	signal enables : std_logic_vector(NODES-1 downto 0);
	signal en_tmp : std_logic_vector(NODES-1 downto 0);
	
BEGIN

	-- SRAM
	SSRAM_BWA_n <= '0'; -- sync byte lane A write input = 1

	SSRAM_BWB_n <= '0'; -- sync byte lane B write input = 1

	SSRAM_WE_n <= '1'; -- write ena = ?

	SSRAM_CE_n <= '0'; -- synchronous chip ena = 1

	SSRAM_ADV <= '1'; -- address valid = 1

	SSRAM_CKE_n <= '0'; -- clock ena = 1

	SSRAM_CLK <= '0'; -- dont care

	SSRAM_OE_n <= '1'; -- output ena = 1

	FAN_CTRL <= '1';
	
	process (clk)
	begin
		if (clk'event and clk='1') then
			reset_n <= CPU_RESET_n;
			reset <= NOT(CPU_RESET_n);
		end if;
	end process;
	
	process(clk,reset)
	begin
		if(reset = '1') then  --change in reset get immediately reflected on signal 'o'.
			LED <= (others => '0');
			in_port_to_the_input0 <= (others => '0');
		elsif(rising_edge(clk)) then
				LED <= out_port_from_the_output0(7 downto 0);
				in_port_to_the_input0(3 downto 0) <= SLIDE_SW;
		end if;
	end process;
	
	NIOS : system1
		PORT MAP(
			clk_clk							=> clk,
			reset_reset_n					=> reset_n,
			input0_extern_con_export	=> in_port_to_the_input0,
			input1_extern_con_export	=> in_port_to_the_input1,
			input2_extern_con_export	=> in_port_to_the_input2,
			input3_extern_con_export	=> in_port_to_the_input3,
			output0_extern_con_export	=> out_port_from_the_output0,
			output1_extern_con_export	=> out_port_from_the_output1,
			output2_extern_con_export	=> out_port_from_the_output2,
			output3_extern_con_export	=> out_port_from_the_output3);

	PLL : pll_100mhz PORT MAP(OSC_50_BANK2, clk);
	
	FANTASI_TEST : FSM
		GENERIC MAP(RESULT_WIDTH, NODES)
		PORT MAP(
			CLK		=> clk,
			RST		=> out_port_from_the_output1(0),
			GO			=> out_port_from_the_output1(1),
			EN_NODES => enables,
			COMPL => in_port_to_the_input2(0),
			RESULT	=> in_port_to_the_input1(RESULT_WIDTH downto 0));
			
	ENABLE_NODES_CONTROL : FSM_ENABLE_NODES
		GENERIC MAP(NODES, ADDR_SHIFT)
		PORT MAP(
			CLK	=> clk,
			RST	=> out_port_from_the_output1(0),
			EN		=> '1',
			M_SET => out_port_from_the_output1(2),
			ZERO 	=> out_port_from_the_output1(3),
			ONE	=> out_port_from_the_output1(4),
			SH_DONE => in_port_to_the_input2(1),
			DIN	=> out_port_from_the_output2(ADDR_SHIFT-1 downto 0),
			DOUT	=> enables);

	
END Behavioural;
