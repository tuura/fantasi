-- Generic size shift register for enabling the nodes inside the network
LIBRARY ieee;
USE ieee.std_logic_1164.all;
USE ieee.numeric_std.all;
USE ieee.std_logic_unsigned.all;
LIBRARY work;

ENTITY FSM_ENABLE_NODES IS
    GENERIC (NODES : integer := 8;
				 ADDR : integer := 3);
    PORT (
      CLK 	: IN    std_logic;
		RST		: IN	std_logic;
		EN		: IN	std_logic;
      M_SET 	: IN    std_logic;
      ZERO	: IN    std_logic;
      ONE 	: IN    std_logic;
		DIN		: IN	std_logic_vector(ADDR-1 downto 0);
		SH_DONE : OUT std_logic;
      DOUT    : OUT   std_logic_vector(NODES-1 downto 0));
END FSM_ENABLE_NODES;

ARCHITECTURE structural OF FSM_ENABLE_NODES IS
    
   COMPONENT Generic_shift_register_enable IS
   GENERIC (N : integer);
   PORT (
        CLK 	: IN    std_logic;
        RST 	: IN    std_logic;
        SHF_EN  : IN    std_logic;
        DIN 	: IN    std_logic;
        DOUT    : OUT   std_logic_vector(N-1 downto 0));
   END COMPONENT;
	
   COMPONENT Generic_register IS
   GENERIC (N : integer);
   PORT (
        CLK 	: IN    std_logic;
        RST 	: IN    std_logic;
        EN  	: IN    std_logic;
        DIN 	: IN    std_logic_vector(N-1 downto 0);
        DOUT    : OUT   std_logic_vector(N-1 downto 0));
	END COMPONENT;
	
	COMPONENT Generic_zero_comparator IS
   GENERIC (N : integer);
   PORT (
        OP 	    : IN    std_logic_vector(N-1 downto 0);
        EN      : IN    std_logic;
        EQ      : OUT   std_logic);
	END COMPONENT;
	
	COMPONENT SYNC_LATCH IS
    PORT (
        DIN 	: IN    std_logic;
        CLK 	: IN    std_logic;
        RST 	: IN    std_logic;
        EN 	   : IN    std_logic;
        DOUT	: OUT   std_logic);
	END COMPONENT;
    
	 SIGNAL data_pin 			: std_logic_vector(ADDR-1 downto 0);
	 SIGNAL count 				: std_logic_vector(ADDR-1 downto 0);
	 SIGNAL count_minus_one : std_logic_vector(ADDR-1 downto 0);
	 
	 SIGNAL zero1 			: std_logic;
	 SIGNAL one1 			: std_logic;
	 SIGNAL shift 			: std_logic;
	 SIGNAL shift_zero 	: std_logic;
	 SIGNAL shift_one 	: std_logic;
	 SIGNAL data_in 		: std_logic;
	 SIGNAL count_en 		: std_logic;
	 SIGNAL stop 			: std_logic;
	 SIGNAL one_end		: std_logic;
	 
	 type state_zero is (S0,S1,S2);
	 signal CR_ZERO, NX: state_zero;
	 
	 type state_one is (T0,T1,T2,T3);
	 signal CR_ONE, TX: state_one;
	 
BEGIN

	 -- zero synch latch
	 ZERO_LATCH : SYNC_LATCH
		port map(
			DIN => ZERO,
			CLK => CLK,
			RST => RST,
			EN => EN,
			DOUT => zero1);
	 
	 -- one synch latch
	 ONE_LATCH : SYNC_LATCH
		port map(
			DIN => ONE,
			CLK => CLK,
			RST => RST,
			EN => EN,
			DOUT => one1);

	 -- hold the value to enable and disable the nodes
	 SR : Generic_shift_register_enable
		generic map(NODES)
		port map(
			CLK => CLK,
			RST => M_SET,
			SHF_EN => shift,
			DIN => data_in,
			DOUT => DOUT);
			
	 -- hold the current number of ones to shift in
	 REG : Generic_register
		generic map(ADDR)
		port map(
			CLK => CLK,
			RST => RST,
			EN => EN,
			DIN => data_pin,
			DOUT => count);
	 
	 -- Initial counter value (MUX)
	 OR_GEN : for i in 0 to (ADDR-1) generate
		data_pin(i) <= (count_minus_one(i) AND count_en) OR (DIN(i) AND NOT(count_en));
	 end generate;
	 
	 -- decrease the value of the counter
	 count_minus_one <= count - 1;
	 
	 -- check when to stop shifting ones
	 ZERO_COMP : Generic_zero_comparator
		generic map(ADDR)
		port map(
			OP => count,
			EN => EN,
			EQ => stop);
			
	-- FSMs to control the structure
	process (CLK,RST)
	begin
		if (RST='1') then
			CR_ZERO <= S0;
			CR_ONE <= T0;
		elsif (CLK'event and CLK='1') then
			CR_ZERO <= NX;
			CR_ONE <= TX;
		end if;
	end process;

	process (CR_ZERO, zero1)
	begin
		case CR_ZERO is
			when S0	=>
				shift_zero	<= '0';
				if zero1 = '1' then
					NX	<= S1;
				else
					NX <= S0;
				end if;
			when S1	=>
				shift_zero	<= '1';
				NX <= S2;
			when S2	=>
				shift_zero <= '0';
				NX <= S0;
		end case;
	end process;
	
	process (CR_ONE, one1, stop)
	begin
		case CR_ONE is
			when T0	=>
				count_en <= '0';
				shift_one	<= '0';
				data_in <= '0';
				one_end <= '1';
				if one1 = '1' then
					TX	<= T1;
				else
					TX <= T0;
				end if;
			when T1	=>
				count_en <= '1';
				shift_one <= '0';
				data_in <= '0';
				one_end <= '0';
				TX <= T2;
			when T2 =>
				count_en <= '1';
				shift_one	<= '1';
				data_in <= '1';
				one_end <= '0';
				if stop = '1' then
					TX <= T3;
				else
					TX <= T2;
				end if;
			when T3	=>
				count_en <= '0';
				shift_one <= '0';
				data_in <= '0';
				one_end <= '1';
				TX <= T0;
		end case;
	end process;
	
	shift <= shift_one OR shift_zero;
	SH_DONE <= one_end;

END structural;
