--------------------------------------------------------------------------------
-- Filename     : i2c_7sd_driver.vhd
-- Author(s)    : Chris Lloyd
-- Class        : EE316 (Project 2)
-- Due Date     : 2021-02-23
-- Target Board : Altera DE2 Devkit
-- Entity       : i2c_7sd_driver
-- Description  : This I2C Seven Segment Display Driver takes in a 16 bit
--                number to display to an external SparkFun I2C Display.
--                https://github.com/rauenzi/VHDL-Communications/blob/master/i2c_controller.vhd
--                https://www.digikey.com/eewiki/pages/viewpage.action?pageId=10125324
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
entity i2c_7sd_driver is
generic
(
  C_CLK_FREQ_MHZ : integer := 50                      -- System clock frequency in MHz
);
port
(
  I_CLK          : in std_logic;                      -- System clk frequency of (C_CLK_FREQ_MHZ)
  I_RESET_N      : in std_logic;                      -- System reset (active low)

  I_START        : in std_logic;                      -- Start I2C write operation
  I_DISPLAY_DATA : in std_logic_vector(15 downto 0);  -- Data to be displayed
  O_BUSY         : out std_logic;                     -- Busy signal from I2C master

  IO_I2C_SDA     : inout std_logic;                   -- Serial data of i2c bus
  IO_I2C_SCL     : inout std_logic                    -- Serial clock of i2c bus
);
end entity i2c_7sd_driver;

--------------------------------
--  Architecture Declaration  --
--------------------------------
architecture behavioral of i2c_7sd_driver is

  ----------------
  -- Components --
  ----------------
  component i2c_master is
  generic
  (
    input_clk : INTEGER := 50_000_000;               -- Input clock speed from user logic in Hz
    bus_clk   : INTEGER := 400_000                   -- Speed the i2c bus (scl) will run at in Hz
  );
  port
  (
    clk       : in     STD_LOGIC;                    -- System clock
    reset_n   : in     STD_LOGIC;                    -- Active low reset
    ena       : in     STD_LOGIC;                    -- Latch in command
    addr      : in     STD_LOGIC_VECTOR(6 downto 0); -- Address of target slave
    rw        : in     STD_LOGIC;                    -- '0' is write, '1' is read
    data_wr   : in     STD_LOGIC_VECTOR(7 downto 0); -- Data to write to slave
    busy      : out    STD_LOGIC;                    -- Indicates transaction in progress
    data_rd   : out    STD_LOGIC_VECTOR(7 downto 0); -- Data read from slave
    ack_error : buffer STD_LOGIC;                    -- Flag if improper acknowledge from slave
    sda       : inout  STD_LOGIC;                    -- Serial data output of i2c bus
    scl       : inout  STD_LOGIC                     -- Serial clock output of i2c bus
  );
  end component i2c_master;

  ---------------
  -- Constants --
  ---------------
  constant C_CLK_FREQ_HZ         : integer := C_CLK_FREQ_MHZ * 1_000_000;
  constant C_I2C_BUS_CLK_FREQ_HZ : integer := 100_000;  -- CDL=> 100_000 or 400_000?
  constant C_I2C_7SD_ADDR        : std_logic_vector(6 downto 0) := 1110001;  -- 0x71

  -------------
  -- SIGNALS --
  -------------

  signal s_display_data : std_logic_vector(15 downto 0);  -- Data to be displayed on I2C 7SD

  signal s_7sd_address  : std_logic_vector(6 downto 0) := C_I2C_7SD_ADDR;

begin

  -- I2C controller for 4 digit Seven Segment Display
  I2C_MASTER_INST:i2c_master
  generic map
  (
    input_clk => C_CLK_FREQ_HZ,
    bus_clk   => C_I2C_BUS_CLK_FREQ_HZ
  )
  port map
  (
    clk       => I_CLK,
    reset_n   => I_RESET_N,
    ena       => ,
    addr      => s_7sd_address,
    rw        => ,
    data_wr   => s_wr_data_byte,
    busy      => ,
    data_rd   => open,
    ack_error => open,
    sda       => IO_I2C_SDA,
    scl       => IO_I2C_SCL
  );

  ---------------
  -- Processes --
  ---------------

  ------------------------------------------------------------------------------
  -- Process Name     :
  -- Sensitivity List : I_CLK            : System clock
  --                    I_RESET_N        : System reset (active low logic)
  -- Useful Outputs   :
  -- Description      :
  ------------------------------------------------------------------------------
  : process (I_CLK, I_RESET_N)
  begin
    if (I_RESET_N = '0') then

    elsif (rising_edge(I_CLK)) then

    end if;
  end process ;
  ------------------------------------------------------------------------------

  -- CDL=> Need some sort of index control signal or state machine
  -- CDL=> Add constants for commands
  -- CDL=> Why clear three times?
	s_wr_data_byte <= x"76" when  =
               else x"76" when  =
               else x"76" when  =
               else x"7A" when  =
               else x"FF" when  =
               else x"77" when  =
               else x"00" when  =
               else x"0"&I_DISPLAY_DATA(15 downto 12) when  =
               else x"0"&I_DISPLAY_DATA(11 downto 8) when  =
               else x"0"&I_DISPLAY_DATA(7 downto 4) when  =
               else x"0"&I_DISPLAY_DATA(3 downto 0) when  =
               else x"FF";

end architecture behavioral;
