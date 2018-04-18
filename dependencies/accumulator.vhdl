-- Generic accumulator
LIBRARY ieee;
USE ieee.std_logic_1164.all;
use ieee.numeric_std.all;
Use ieee.std_logic_unsigned.all;
LIBRARY work;

ENTITY Generic_accumulator IS
    GENERIC (N : integer := 8);
    PORT (
        CLK 	: IN    std_logic;
        RST 	: IN    std_logic;
        EN  	: IN    std_logic;
        DIN 	: IN    std_logic_vector(N-1 downto 0);
        DOUT    : OUT   std_logic_vector(N downto 0));
END Generic_accumulator;

ARCHITECTURE structural OF Generic_accumulator IS
    
    COMPONENT Generic_register IS
        GENERIC (N : integer);
        PORT (
            CLK 	: IN    std_logic;
            RST 	: IN    std_logic;
            EN  	: IN    std_logic;
            DIN 	: IN    std_logic_vector(N-1 downto 0);
            DOUT    : OUT   std_logic_vector(N-1 downto 0));
    END COMPONENT;
    
    SIGNAL sum      : std_logic_vector(N downto 0);
    SIGNAL data_out : std_logic_vector(N downto 0);

BEGIN

    sum <= ('0' & DIN) + (data_out);

    REG_ACCUMULATOR: Generic_register
        GENERIC MAP( N + 1 )
        PORT MAP (
            CLK  =>   CLK,
	        RST  =>   RST,
            EN   =>   EN,
            DIN  =>   sum,
            DOUT =>   data_out);

    DOUT <= data_out;

END structural;
