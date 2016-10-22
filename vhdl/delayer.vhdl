-- FFD to delay a signals of N clock cycles
LIBRARY ieee;
USE ieee.std_logic_1164.all;
LIBRARY work;

ENTITY Delayer IS
    GENERIC (N : integer := 1);
    PORT (
        CLK 	: IN    std_logic;
        RST 	: IN    std_logic;
        EN  	: IN    std_logic;
        DIN 	: IN    std_logic;
        DOUT    : OUT   std_logic);
END Delayer;

ARCHITECTURE structural OF Delayer IS

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

BEGIN

    REG_GENERATOR : for i in 0 to N-1 generate
	    FFD_I : ffd PORT MAP (
            CLK =>   CLK,
		    RST =>   RST,
            EN  =>   EN,
            D   =>   data(i),
            Q   =>   data(i+1));
	END GENERATE;

    data(0) <= DIN;
    DOUT <= data(N);

END structural;
