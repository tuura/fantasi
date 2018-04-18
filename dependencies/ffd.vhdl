library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

LIBRARY work;

entity ffd is
   port (
      CLK	: in	std_logic;
      RST	: in	std_logic;
      EN	: in	std_logic;
      D		: in	std_logic;
      Q		: out	std_logic
   );
end entity ffd;
 
architecture Behavioral of ffd is

	signal q_tmp : std_logic;

begin
   process (CLK) is
   begin
      if rising_edge(CLK) then  
         if (RST='1') then 
            q_tmp <= '0';
         elsif (EN='1') then
            q_tmp <= D;
         end if;
      end if;
   end process;

   Q <= q_tmp;
end architecture Behavioral;
