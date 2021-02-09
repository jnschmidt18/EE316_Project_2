--------------------------------------------------------------------------------
-- Filename     : keypad_display_ut.vhd
-- Author(s)    : Chris Lloyd
-- Class        : EE316 (Project 2)
-- Due Date     : 2021-02-23
-- Target Board : Altera DE2 Devkit
-- Entity       : keypad_display_ut
-- Description  : Unit Test (ut) to test I2C Seven Segment Display using an
--                external matrix keypad.
--------------------------------------------------------------------------------

-----------------
--  Libraries  --
-----------------
library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.Numeric_std.all;

--------------
--  Entity  --
--------------
entity keypad_display_ut is
port
(
  I_CLK          : in std_logic;                      -- System clk frequency of (C_CLK_FREQ_MHZ)
  I_RESET_N      : in std_logic;                      -- System reset (active low)

  I_KEYPAD_ROWS  : in std_logic_vector(4 downto 0);   -- Keypad Inputs (rows)
  O_KEYPAD_COLS  : out std_logic_vector(3 downto 0);  -- Keypad Outputs (cols)

  IO_I2C_SDA     : inout std_logic;                   -- Serial data of i2c bus
  IO_I2C_SCL     : inout std_logic                    -- Serial clock of i2c bus
);
end entity keypad_display_ut;

--------------------------------
--  Architecture Declaration  --
--------------------------------
architecture behavioral of keypad_display_ut is

  ----------------
  -- Components --
  ----------------

  component keypad_5x4_wrapper is
  generic
  (
    C_CLK_FREQ_MHZ   : integer                            -- System clock frequency in MHz
  );
  port
  (
    I_CLK            : in std_logic;                      -- System clk frequency of (C_CLK_FREQ_MHZ)
    I_RESET_N        : in std_logic;                      -- System reset (active low)
    I_KEYPAD_ROWS    : in std_logic_vector(4 downto 0);   -- Keypad Inputs (rows)
    O_KEYPAD_COLS    : out std_logic_vector(3 downto 0);  -- Keypad Outputs (cols)

    -- Data of pressed key
    -- 5th bit enabled indicates command button pressed
    O_KEYPAD_DATA    : out std_logic_vector(4 downto 0);

    -- Trigger to indicate a key was pressed (single clock cycle pulse)
    O_KEYPRESSED     : out std_logic
  );
  end component keypad_5x4_wrapper;

  ---------------
  -- Constants --
  ---------------

  constant C_CLK_FREQ_MHZ : integer := 50;  -- System clock frequency in MHz

  -------------
  -- SIGNALS --
  -------------

  signal s_keypad_data    : std_logic_vector(4 downto 0);   -- Data from keypress
  signal s_keypressed     : std_logic;                      -- Whether a key was pressed
  signal s_command_nibble : std_logic_vector(3 downto 0);   -- Command flag to be displayed
  signal s_data_nibble    : std_logic_vector(3 downto 0);   -- Data to be displayed
  signal s_display_data   : std_logic_vector(15 downto 0);  -- Data to be displayed on I2C 7SD
  signal s_display_enable : std_logic;                      -- Display's enable control

begin

  -- Device driver for keypad
  KEYPAD_DRIVER_INST: keypad_5x4_wrapper
  generic map
  (
    C_CLK_FREQ_MHZ   => C_CLK_FREQ_MHZ
  )
  port map
  (
    I_CLK            => I_CLK,
    I_RESET_N        => I_RESET_N,
    I_KEYPAD_ROWS    => I_KEYPAD_ROWS,
    O_KEYPAD_COLS    => O_KEYPAD_COLS,
    O_KEYPAD_DATA    => s_keypad_data,
    O_KEYPRESSED     => s_keypressed
  );

  ---------------
  -- Processes --
  ---------------

  ------------------------------------------------------------------------------
  -- Process Name     : KEYPAD_DISPLAY_TEST
  -- Sensitivity List : I_CLK            : System clock
  --                    I_RESET_N        : System reset (active low logic)
  -- Useful Outputs   : s_command_nibble : hex digit to be displayed on hex1
  --                    s_data_nibble    : hex digit to be displayed on hex0
  --                    s_display_enable : Digit enable for display
  -- Description      : A process to latch triggered inputs from a matrix
  --                    keypad to two seven segment displays.
  ------------------------------------------------------------------------------
  KEYPAD_DISPLAY_TEST: process (I_CLK, I_RESET_N)
  begin
    if (I_RESET_N = '0') then
      s_command_nibble      <= (others=>'0');
      s_data_nibble         <= (others=>'0');
      s_display_enable      <= '0';

    elsif (rising_edge(I_CLK)) then
      -- Enable (turn on) the display
      s_display_enable      <= '1';

      -- Only update key data when a key is pressed
      if (s_keypressed = '1') then
        s_command_nibble(0) <= s_keypad_data(4);
        s_data_nibble       <= s_keypad_data(3 downto 0);
      else
        s_command_nibble    <= s_command_nibble;
        s_data_nibble       <= s_data_nibble;
      end if;
    end if;
  end process KEYPAD_DISPLAY_TEST;
  ------------------------------------------------------------------------------

  s_display_data <= s_command_nibble &

end architecture behavioral;
