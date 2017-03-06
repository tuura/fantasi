-- Generic size shift register for enabling the nodes inside the network
LIBRARY ieee;
USE ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
LIBRARY work;

ENTITY Generic_shift_register_enable IS
    GENERIC (N : integer := 8);
    PORT (
        CLK 	: IN    std_logic;
        RST 	: IN    std_logic;
        SHF_EN 	: IN    std_logic;
        DIN 	: IN    std_logic;
        DOUT    : OUT   std_logic_vector(N-1 downto 0));
END Generic_shift_register_enable;

ARCHITECTURE structural OF Generic_shift_register_enable IS
    
    COMPONENT ffd_en is
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
	    FFD_I : ffd_en PORT MAP (
            CLK =>   CLK,
		    RST =>   RST,
            EN  =>   SHF_EN,
            D   =>   data(i),
            Q   =>   data(i+1));

        DOUT(i) <= data(i+1);
	END GENERATE;

    data(0) <= DIN;

END structural;
