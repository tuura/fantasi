-- Generic size register made with sync latches
LIBRARY ieee;
USE ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
LIBRARY work;

ENTITY Generic_sync_register IS
    GENERIC (N : integer := 8);
    PORT (
        CLK 	: IN    std_logic;
        RST 	: IN    std_logic;
        EN  	: IN    std_logic;
        DIN 	: IN    std_logic_vector(N-1 downto 0);
        DOUT    : OUT   std_logic_vector(N-1 downto 0));
END Generic_sync_register;

ARCHITECTURE structural OF Generic_sync_register IS
    
    COMPONENT SYNC_LATCH IS
        PORT (
            DIN 	: IN    std_logic;
            CLK 	: IN    std_logic;
            RST 	: IN    std_logic;
            EN 	    : IN    std_logic;
            DOUT	: OUT   std_logic);
    END COMPONENT;
    
BEGIN

    SYNC_REG_GENERATOR : for i in 0 to N-1 generate
	    SYNC_LATCH_I : SYNC_LATCH PORT MAP (
            CLK  =>   CLK,
		    RST  =>   RST,
            EN   =>   EN,
            DIN  =>   DIN(i),
            DOUT =>   DOUT(i));
    end Generate;

END structural;