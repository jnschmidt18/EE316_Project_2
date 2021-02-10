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
library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

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

  component i2c_7sd_driver is
  generic
  (
    C_CLK_FREQ_MHZ : integer := 50                      -- System clock frequency in MHz
  );
  port
  (
    I_CLK          : in std_logic;                      -- System clk frequency of (C_CLK_FREQ_MHZ)
    I_RESET_N      : in std_logic;                      -- System reset (active low)
    I_DISPLAY_DATA : in std_logic_vector(15 downto 0);  -- Data to be displayed
    O_BUSY         : out std_logic;                     -- Busy signal from I2C master
    IO_I2C_SDA     : inout std_logic;                   -- Serial data of i2c bus
    IO_I2C_SCL     : inout std_logic                    -- Serial clock of i2c bus
  );
  end component i2c_7sd_driver;

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
  signal s_display_data   : std_logic_vector(15 downto 0);  -- Data to be displayed on I2C 7SD
  signal s_7sd_busy       : std_logic;                      -- Busy signal from I2C 7SD

begin

  -- Device driver for 7SD
  DISPLAY_DRIVER_INST: i2c_7sd_driver
  generic map
  (
    C_CLK_FREQ_MHZ   => C_CLK_FREQ_MHZ
  )
  port map
  (
    I_CLK            => I_CLK,
    I_RESET_N        => I_RESET_N,

    I_DISPLAY_DATA   => s_display_data,
    O_BUSY           => s_7sd_busy,
    IO_I2C_SDA       => IO_I2C_SDA,
    IO_I2C_SCL       => IO_I2C_SCL
  );

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
  -- Useful Outputs   :
  -- Description      : A process to latch triggered inputs from a matrix
  --                    keypad
  ------------------------------------------------------------------------------
  KEYPAD_DISPLAY_TEST: process (I_CLK, I_RESET_N)
  begin
    if (I_RESET_N = '0') then
      s_display_data        <= (others=>'1');

    elsif (rising_edge(I_CLK)) then
        -- if (s_7sd_busy = '1') then
        --   s_display_data <= x"0000";
        -- else
        --   s_display_data <= x"DEAD";
        -- end if ;

        -- Only update key data when a key is pressed
      if (s_keypressed = '1' and s_7sd_busy = '0') then
          -- s_display_data <= x"DEAD";
        s_display_data <= s_keypad_data(3 downto 0) &
                          s_keypad_data(3 downto 0) &
                          s_keypad_data(3 downto 0) &
                          s_keypad_data(3 downto 0);
      else
        s_display_data <= s_display_data;
      end if;
    end if;
  end process KEYPAD_DISPLAY_TEST;
  ------------------------------------------------------------------------------
end architecture behavioral;
