LIBRARY ieee;
USE ieee.std_logic_1164.all;

LIBRARY work;

ENTITY FSM IS
	GENERIC (DATA_WIDTH : integer := 19;
             NODES : integer := 15);
	PORT (
		CLK	    : IN	std_logic;
		RST	    : IN	std_logic;
		GO		: IN std_logic;
		COMPL   : OUT std_logic;
        EN_NODES: in std_logic_vector(NODES-1 downto 0);
		RESULT	: OUT	std_logic_vector(DATA_WIDTH downto 0));
END FSM;

ARCHITECTURE FSM_S OF FSM IS

	COMPONENT FANTASI IS
		PORT (
			CLK			: IN	std_logic;
			RST			: IN	std_logic;
			RST_SHIFT	: IN	std_logic;
			EN			: IN	std_logic;
            EN_NODES    : IN    std_logic_vector(NODES-1 downto 0);
			START		: IN	std_logic;
			DIN			: IN	std_logic;
			DONE		: OUT	std_logic;
			COMPLETE	: OUT	std_logic;
			RESULT		: OUT	std_logic_vector(DATA_WIDTH-1 downto 0));
	END COMPONENT;

	COMPONENT Generic_accumulator IS
		GENERIC (N : integer);
		PORT (
			CLK	: IN	std_logic;
			RST	: IN	std_logic;
			EN	: IN	std_logic;
			DIN	: IN	std_logic_vector(N-1 downto 0);
			DOUT: OUT	std_logic_vector(N downto 0));
	END COMPONENT;	
	
	type state is (S0,S1,S2,S3,S4,S5,S6,S7,S8,S9);
	signal CR, NX: state;
	signal en, start, din, rstf, rsts, complete, done, sum : std_logic; 
	signal res : std_logic_vector(DATA_WIDTH-1 downto 0);
	
BEGIN

	process (CLK,RST)
	begin
		if (RST='1') then
			CR <= S0;
		elsif (CLK'event and CLK='1') then
			CR <= NX;
		end if;
	end process;

	process (CR, GO, complete, done)
	begin
		case CR is
			when S0	=>
				rstf 	<= '0';
				rsts	<= '0';
				start	<= '0';
				din	<= '0';
				en		<= '0';
				sum	<= '0';
				NX	<= S1;
			when S1	=>
				rstf 	<= '1';
				rsts	<= '1';
				start	<= '0';
				din	<= '0';
				en		<= '0';
				sum	<= '0';
				if GO = '1' then
					NX <= S2;
				else
					NX <= S1;
				end if;
			when S2	=>
				rstf 	<= '0';
				rsts	<= '0';
				start	<= '1';
				din	<= '1';
				en		<= '1';
				sum	<= '0';
				NX		<= S3;
			when S3	=>
				rstf 	<= '0';
				rsts	<= '0';
				start	<= '1';
				din	<= '1';
				en		<= '1';
				sum	<= '0';
				if (done = '1') then
					NX	<= S4;
				else
					NX <= S3;
				end if;
			when S4	=>
				rstf 	<= '0';
				rsts	<= '0';
				start	<= '0';
				din	<= '0';
				en		<= '1';
				sum	<= '1';
				NX		<= S5;
			when S5	=>
				rstf 	<= '1';
				rsts	<= '0';
				start	<= '0';
				din	<= '0';
				en		<= '1';
				sum	<= '0';
				NX		<= S6;
			when S6	=>
				rstf 	<= '0';
				rsts	<= '0';
				start	<= '0';
				din	<= '0';
				en		<= '1';
				sum	<= '0';
				NX		<= S7;
			when S7	=>
				rstf 	<= '0';
				rsts	<= '0';
				start	<= '1';
				din	<= '0';
				en		<= '1';
				sum	<= '0';
				NX		<= S8;
			when S8	=>
				rstf 	<= '0';
				rsts	<= '0';
				start	<= '1';
				din	<= '0';
				en		<= '1';
				sum	<= '0';
				if (complete = '1') then
					NX <= S9;
				elsif (done = '1') then
					NX <= S4;
				else
					NX	<= S8;
				end if;
			when S9	=>
				rstf 	<= '0';
				rsts	<= '0';
				start	<= '1';
				din	<= '0';
				en		<= '1';
				sum	<= '0';
				NX		<= S9;
		end case;
	end process;
	
	TEST_STRUCTURE : FANTASI
		PORT MAP(
			CLK			=> CLK,
			RST			=> rstf,
			RST_SHIFT	=> rsts,
			EN			=> en,
			EN_NODES	=> EN_NODES,
			START		=> start,
			DIN			=> din,
			DONE		=> done,
			COMPLETE	=> complete,
			RESULT		=> res);
			
	ACCUMULATOR : Generic_accumulator
		GENERIC MAP(DATA_WIDTH)
		PORT MAP(
			CLK	=> CLK,
			RST	=> RST,
			EN	=> sum,
			DIN	=> res,
			DOUT	=> RESULT);
			
	COMPL <= complete;
	
END FSM_S;
