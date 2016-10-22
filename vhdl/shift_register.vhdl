-- Generic size shift register with the input synchronised just once
LIBRARY ieee;
USE ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
LIBRARY work;

ENTITY Generic_shift_register IS
    GENERIC (N : integer := 8);
    PORT (
        CLK 	: IN    std_logic;
        RST 	: IN    std_logic;
        EN  	: IN    std_logic;
        START  	: IN    std_logic;
        DIN 	: IN    std_logic;
        DOUT    : OUT   std_logic_vector(N-1 downto 0));
END Generic_shift_register;

ARCHITECTURE structural OF Generic_shift_register IS
    
    COMPONENT SYNC_LATCH IS
        PORT (
            DIN 	: IN    std_logic;
            CLK 	: IN    std_logic;
            RST 	: IN    std_logic;
            EN 	    : IN    std_logic;
            DOUT	: OUT   std_logic);
    END COMPONENT;

    COMPONENT ffd is
       port (
          CLK	: in	std_logic;
          RST	: in	std_logic;
          EN	: in	std_logic;
          D		: in	std_logic;
          Q		: out	std_logic
       );
    end COMPONENT;
    
    SIGNAL data : std_logic_vector(N downto 0);
    SIGNAL sync_data : std_logic;
    SIGNAL sync_en : std_logic;

BEGIN

    SYNC_D : SYNC_LATCH PORT MAP (
            CLK  =>   CLK,
		    RST  =>   RST,
            EN   =>   EN,
            DIN  =>   DIN,
            DOUT =>   sync_data);

    SYNC_E : SYNC_LATCH PORT MAP (
            CLK  =>   CLK,
		    RST  =>   RST,
            EN   =>   EN,
            DIN  =>   START,
            DOUT =>   sync_en);

    REG_GENERATOR : for i in 0 to N-1 generate
	    FFD_I : ffd PORT MAP (
            CLK =>   CLK,
		    RST =>   RST,
            EN  =>   sync_en,
            D   =>   data(i),
            Q   =>   data(i+1));

        DOUT(i) <= data(i+1);
	END GENERATE;

    data(0) <= sync_data;

END structural;
