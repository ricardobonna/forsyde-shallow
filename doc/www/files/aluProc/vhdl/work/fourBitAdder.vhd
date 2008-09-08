-- Automatically generated by ForSyDe
library forsyde;
library ieee;
use forsyde.types.all;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
library aluProc_lib;
use aluProc_lib.types.all;


entity \fourBitAdder\ is
     port (\C_IN\ : in std_logic;
           \A3\ : in std_logic;
           \A2\ : in std_logic;
           \A1\ : in std_logic;
           \A0\ : in std_logic;
           \B3\ : in std_logic;
           \B2\ : in std_logic;
           \B1\ : in std_logic;
           \B0\ : in std_logic;
           \C_OUT\ : out std_logic;
           \SUM3\ : out std_logic;
           \SUM2\ : out std_logic;
           \SUM1\ : out std_logic;
           \SUM0\ : out std_logic);
end entity \fourBitAdder\;


architecture synthesizable of \fourBitAdder\ is
     signal \add0_out1\ : std_logic;
     signal \add0_out2\ : std_logic;
     signal \add1_out1\ : std_logic;
     signal \add1_out2\ : std_logic;
     signal \add2_out1\ : std_logic;
     signal \add2_out2\ : std_logic;
     signal \add3_out1\ : std_logic;
     signal \add3_out2\ : std_logic;
begin
     \add0\ : entity work.\fullAddProc\
                   port map (\a\ => \A0\,
                             \b\ => \B0\,
                             \c_in\ => \C_IN\,
                             \cout\ => \add0_out1\,
                             \sum\ => \add0_out2\);
     
     \add1\ : entity work.\fullAddProc\
                   port map (\a\ => \A1\,
                             \b\ => \B1\,
                             \c_in\ => \add0_out1\,
                             \cout\ => \add1_out1\,
                             \sum\ => \add1_out2\);
     
     \add2\ : entity work.\fullAddProc\
                   port map (\a\ => \A2\,
                             \b\ => \B2\,
                             \c_in\ => \add1_out1\,
                             \cout\ => \add2_out1\,
                             \sum\ => \add2_out2\);
     
     \add3\ : entity work.\fullAddProc\
                   port map (\a\ => \A3\,
                             \b\ => \B3\,
                             \c_in\ => \add2_out1\,
                             \cout\ => \add3_out1\,
                             \sum\ => \add3_out2\);
     
     \C_OUT\ <= \add3_out1\;
     
     \SUM3\ <= \add3_out2\;
     
     \SUM2\ <= \add2_out2\;
     
     \SUM1\ <= \add1_out2\;
     
     \SUM0\ <= \add0_out2\;
end architecture synthesizable;
