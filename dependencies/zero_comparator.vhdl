-- Generic comparator
LIBRARY ieee;
USE ieee.std_logic_1164.all;
LIBRARY work;

ENTITY Generic_zero_comparator IS
    GENERIC (N : integer := 8);
    PORT (
        OP 	    : IN    std_logic_vector(N-1 downto 0);
        EN      : IN    std_logic;
        EQ      : OUT   std_logic);
END Generic_zero_comparator;

ARCHITECTURE structural OF Generic_zero_comparator IS
    
    SIGNAL equals : std_logic;

BEGIN

    equals <= '1' when (OP = (OP'range => '0')) else '0';
    EQ <= EN AND equals;

END structural;
