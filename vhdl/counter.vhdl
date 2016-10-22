-- Generic counter
LIBRARY ieee;
USE ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
LIBRARY work;

ENTITY Generic_counter IS
    GENERIC (N : integer := 8);
    PORT (
        CLK 	: IN    std_logic;
        RST 	: IN    std_logic;
        EN  	: IN    std_logic;
        DOUT    : OUT   std_logic_vector(N-1 downto 0));
END Generic_counter;

ARCHITECTURE behavioural OF Generic_counter IS
    
    SIGNAL count : STD_LOGIC_VECTOR(N-1 downto 0);

BEGIN

    process (CLK, RST)
    begin
        if RST = '1' then
            count <= (others => '0');
        elsif (CLK = '1' AND CLK'event) then
            if EN = '1' then
                count <= count + 1;
            end if;
        end if;
    end process;

    DOUT <= count;

END behavioural;
