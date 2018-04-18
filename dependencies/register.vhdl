-- Generic size register made with D Flip Flops
LIBRARY ieee;
USE ieee.std_logic_1164.all;
LIBRARY work;

ENTITY Generic_register IS
    GENERIC (N : integer := 8);
    PORT (
        CLK 	: IN    std_logic;
        RST 	: IN    std_logic;
        EN  	: IN    std_logic;
        DIN 	: IN    std_logic_vector(N-1 downto 0);
        DOUT    : OUT   std_logic_vector(N-1 downto 0));
END Generic_register;

ARCHITECTURE structural OF Generic_register IS
    
    COMPONENT ffd IS
        PORT (
            CLK : IN    std_logic;
            RST : IN    std_logic;
            EN  : IN    std_logic;
            D   : IN    std_logic;
            Q   : OUT   std_logic
        );
    END COMPONENT;
    
BEGIN

    REG_GENERATOR : for i in 0 to N-1 generate
	    FFD_I : ffd PORT MAP (
            CLK =>   CLK,
		    RST =>   RST,
            EN  =>   EN,
            D   =>   DIN(i),
            Q   =>   DOUT(i));
	 END GENERATE;

END structural;
