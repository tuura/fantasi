-- This module make the input visible to the output just for one clock cycle
LIBRARY ieee;
USE ieee.std_logic_1164.all;
LIBRARY work;

ENTITY SYNC_LATCH IS
    PORT (
        DIN 	: IN    std_logic;
        CLK 	: IN    std_logic;
        RST 	: IN    std_logic;
        EN 	    : IN    std_logic;
        DOUT	: OUT   std_logic);
END SYNC_LATCH;

ARCHITECTURE structural_description OF SYNC_LATCH IS
    
    COMPONENT ffd IS
        PORT (
            CLK : IN    std_logic;
            RST : IN    std_logic;
            EN  : IN    std_logic;
            D   : IN    std_logic;
            Q   : OUT   std_logic
        );
    END COMPONENT;

    SIGNAL d_sync : std_logic;
    SIGNAL sync	: std_logic;

BEGIN

    FFD_OUT : ffd PORT MAP (
        CLK =>   CLK,
	    RST =>   RST,
	    EN  =>   EN,
	    D   =>   d_sync,
	    Q   =>   DOUT);

    FFD_SYNC : ffd PORT MAP (
    	CLK =>   CLK,
	    RST =>   RST,
	    EN  =>   EN,
	    D   =>   DIN,
	    Q   =>   sync);

    d_sync <= DIN AND NOT(sync);

END structural_description;
