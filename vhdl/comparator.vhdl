-- Generic comparator
LIBRARY ieee;
USE ieee.std_logic_1164.all;
LIBRARY work;

ENTITY Generic_comparator IS
    GENERIC (N : integer := 8);
    PORT (
        AOP 	: IN    std_logic_vector(N-1 downto 0);
        BOP 	: IN    std_logic_vector(N-1 downto 0);
        EN      : IN    std_logic;
        EQ      : OUT   std_logic);
END Generic_comparator;

ARCHITECTURE structural OF Generic_comparator IS
    
    SIGNAL wires  : std_logic_vector(N-1 downto 0);
    SIGNAL equals : std_logic;

BEGIN

    XNOR_GEN : for i in 0 to N-1 generate
        wires(i) <= AOP(i) XNOR BOP(i);
    end generate;

    equals <= '1' when (wires = (wires'range => '1')) else '0';
    EQ <= EN AND equals;

END structural;
