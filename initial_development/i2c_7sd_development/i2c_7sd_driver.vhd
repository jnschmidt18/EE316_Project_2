--------------------------------------------------------------------------------
-- Filename     : i2c_7sd_driver.vhd
-- Author(s)    : Chris Lloyd
-- Class        : EE316 (Project 2)
-- Due Date     : 2021-02-23
-- Target Board : Altera DE2 Devkit
-- Entity       : i2c_7sd_driver
-- Description  : This I2C Seven Segment Display (7SD) Driver takes in a 16 bit
--                number to display to an external SparkFun I2C Display.
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

  -- I_START        : in std_logic;                      -- Start I2C write operation
  I_DISPLAY_DATA : in std_logic_vector(15 downto 0);  -- Data to be displayed
  -- O_BUSY         : out std_logic;                     -- Busy signal from I2C master

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
    input_clk : integer := 50_000_000;               -- Input clock speed from user logic in Hz
    bus_clk   : integer := 400_000                   -- Speed the i2c bus (scl) will run at in Hz
  );
  port
  (
    clk       : in     std_logic;                    -- System clock
    reset_n   : in     std_logic;                    -- Active low reset
    ena       : in     std_logic;                    -- Latch in command
    addr      : in     std_logic_vector(6 downto 0); -- Address of target slave
    rw        : in     std_logic;                    -- '0' is write, '1' is read
    data_wr   : in     std_logic_vector(7 downto 0); -- Data to write to slave
    busy      : out    std_logic;                    -- Indicates transaction in progress
    data_rd   : out    std_logic_vector(7 downto 0); -- Data read from slave
    ack_error : buffer std_logic;                    -- Flag if improper acknowledge from slave
    sda       : inout  std_logic;                    -- Serial data output of i2c bus
    scl       : inout  std_logic                     -- Serial clock output of i2c bus
  );
  end component i2c_master;

  ---------------
  -- Constants --
  ---------------
  constant C_CLK_FREQ_HZ         : integer := 50_000_000; -- C_CLK_FREQ_MHZ * 1_000_000;
  constant C_I2C_BUS_CLK_FREQ_HZ : integer := 400_000;  -- CDL=> 100_000 or 400_000?
  constant C_I2C_7SD_ADDR        : std_logic_vector(6 downto 0) := "1110001";  -- 0x71
  constant C_WR_BYTE_INDEX_MAX   : integer := 12; -- CDL=>?

  constant C_7SD_CLEAR_DISP_CMD  : std_logic_vector(7 downto 0) := x"76"; -- CDL=> Explain and add more?
  constant C_7SD_BRIGHTNESS_CMD  : std_logic_vector(7 downto 0) := x"7A";
  constant C_7SD_DEC_CTRL_CMD    : std_logic_vector(7 downto 0) := x"77";
  constant C_7SD_CURS_CTRL_CMD   : std_logic_vector(7 downto 0) := x"79";

  -------------
  -- SIGNALS --
  -------------


  signal s_wr_data_byte_index : integer;
  signal s_wr_data_byte       : std_logic_vector(7 downto 0);

  signal s_7sd_enable    : std_logic;
  signal s_7sd_wr        : std_logic;  --'0' is write, '1' is read
  signal s_7sd_address   : std_logic_vector(6 downto 0) := C_I2C_7SD_ADDR;
  signal s_7sd_busy      : std_logic;
  signal s_7sd_busy_prev : std_logic;
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
    ena       => s_7sd_enable,
    addr      => s_7sd_address,
    rw        => s_7sd_wr,
    data_wr   => s_wr_data_byte,
    busy      => s_7sd_busy,
    data_rd   => open,
    ack_error => open,
    sda       => IO_I2C_SDA,
    scl       => IO_I2C_SCL
  );

  ---------------
  -- Processes --
  ---------------

  ------------------------------------------------------------------------------
  -- Process Name     : DATA_INDEX_CNTR
  -- Sensitivity List : I_CLK                : System clock
  --                    I_RESET_N            : System reset (active low logic)
  -- Useful Outputs   : s_wr_data_byte_index : Index to select data byte.
  -- Description      : Process to increment index counter representing current
  --                    data byte to send. LUT table in (WRITE_DATA_LUT).
  ------------------------------------------------------------------------------
  DATA_INDEX_CNTR: process (I_CLK, I_RESET_N)
  begin
    if (I_RESET_N = '0') then
      s_7sd_enable <= '0';
      s_7sd_busy_prev <= '1';
      s_wr_data_byte_index <= 0;
    elsif (rising_edge(I_CLK)) then
      s_7sd_enable <= '1';
      -- Store previous busy signal
      s_7sd_busy_prev <= s_7sd_busy;

      if (s_7sd_busy_prev /= s_7sd_busy and
          s_7sd_busy = '0') then  -- If busy falling edge
        if (s_wr_data_byte_index /= C_WR_BYTE_INDEX_MAX) then  -- Increment index, but roll over at max to 7
          s_wr_data_byte_index <= s_wr_data_byte_index + 1;
        else
          s_wr_data_byte_index <= 7;
        end if;
      else
        s_wr_data_byte_index <= s_wr_data_byte_index;
      end if;
    end if;
  end process DATA_INDEX_CNTR;
  ------------------------------------------------------------------------------

  ------------------------------------------------------------------------------
  -- Process Name     : WRITE_DATA_LUT
  -- Sensitivity List : I_CLK          : System clock
  --                    I_RESET_N      : System reset (active low logic)
  -- Useful Outputs   : s_wr_data_byte : Data byte to send to Display
  -- Description      : Process to act as Look Up Table (LUT) to decide what
  --                    byte to send to display based on (s_wr_data_byte_index).
  ------------------------------------------------------------------------------
  WRITE_DATA_LUT: process (I_CLK, I_RESET_N)
  begin
    if (I_RESET_N = '0') then
      s_wr_data_byte                  <= C_7SD_CLEAR_DISP_CMD;
    elsif (rising_edge(I_CLK)) then
      case s_wr_data_byte_index is
        when 0      => s_wr_data_byte <= C_7SD_CLEAR_DISP_CMD;  -- CDL=> Needed to clear display 3 times?
        when 1      => s_wr_data_byte <= C_7SD_CLEAR_DISP_CMD;
        when 2      => s_wr_data_byte <= C_7SD_CLEAR_DISP_CMD;
        when 3      => s_wr_data_byte <= C_7SD_BRIGHTNESS_CMD;
        when 4      => s_wr_data_byte <= x"64";  -- Max brightness  --CDL=> Right value?
        when 5      => s_wr_data_byte <= C_7SD_DEC_CTRL_CMD;
        when 6      => s_wr_data_byte <= x"00";  -- No decimal points  --CDL=> Right value?
        when 7      => s_wr_data_byte <= C_7SD_CURS_CTRL_CMD;
        when 8      => s_wr_data_byte <= x"00";  -- Reset cursor to digit 0
        --when 9      => s_wr_data_byte <= x"0" & I_DISPLAY_DATA(15 downto 12);
        --when 10     => s_wr_data_byte <= x"0" & I_DISPLAY_DATA(11 downto 8);
        --when 11     => s_wr_data_byte <= x"0" & I_DISPLAY_DATA(7 downto 4);
        --when 12     => s_wr_data_byte <= x"0" & I_DISPLAY_DATA(3 downto 0);
        when others => s_wr_data_byte <= x"01";
      end case;
    end if;
  end process WRITE_DATA_LUT;
  ------------------------------------------------------------------------------

  s_7sd_wr <= '0'; -- Always writing

end architecture behavioral;








-- CDL=> OLD
-- --------------------------------------------------------------------------------
-- -- Filename     : i2c_7sd_driver.vhd
-- -- Author(s)    : Chris Lloyd
-- -- Class        : EE316 (Project 2)
-- -- Due Date     : 2021-02-23
-- -- Target Board : Altera DE2 Devkit
-- -- Entity       : i2c_7sd_driver
-- -- Description  : This I2C Seven Segment Display (7SD) Driver takes in a 16 bit
-- --                number to display to an external SparkFun I2C Display.
-- --                https://github.com/rauenzi/VHDL-Communications/blob/master/i2c_controller.vhd
-- --                https://www.digikey.com/eewiki/pages/viewpage.action?pageId=10125324
-- --------------------------------------------------------------------------------

-- -----------------
-- --  Libraries  --
-- -----------------
-- library IEEE;
-- use IEEE.std_logic_1164.all;
-- use IEEE.Numeric_std.all;

-- --------------
-- --  Entity  --
-- --------------
-- entity i2c_7sd_driver is
-- generic
-- (
--   C_CLK_FREQ_MHZ : integer := 50                      -- System clock frequency in MHz
-- );
-- port
-- (
--   I_CLK          : in std_logic;                      -- System clk frequency of (C_CLK_FREQ_MHZ)
--   I_RESET_N      : in std_logic;                      -- System reset (active low)

--   I_START        : in std_logic;                      -- Start I2C write operation
--   I_DISPLAY_DATA : in std_logic_vector(15 downto 0);  -- Data to be displayed
--   O_BUSY         : out std_logic;                     -- Busy signal from I2C master

--   IO_I2C_SDA     : inout std_logic;                   -- Serial data of i2c bus
--   IO_I2C_SCL     : inout std_logic                    -- Serial clock of i2c bus
-- );
-- end entity i2c_7sd_driver;

-- --------------------------------
-- --  Architecture Declaration  --
-- --------------------------------
-- architecture behavioral of i2c_7sd_driver is

--   ----------------
--   -- Components --
--   ----------------
--   component i2c_master is
--   generic
--   (
--     input_clk : INTEGER := 50_000_000;               -- Input clock speed from user logic in Hz
--     bus_clk   : INTEGER := 400_000                   -- Speed the i2c bus (scl) will run at in Hz
--   );
--   port
--   (
--     clk       : in     STD_LOGIC;                    -- System clock
--     reset_n   : in     STD_LOGIC;                    -- Active low reset
--     ena       : in     STD_LOGIC;                    -- Latch in command
--     addr      : in     STD_LOGIC_VECTOR(6 downto 0); -- Address of target slave
--     rw        : in     STD_LOGIC;                    -- '0' is write, '1' is read
--     data_wr   : in     STD_LOGIC_VECTOR(7 downto 0); -- Data to write to slave
--     busy      : out    STD_LOGIC;                    -- Indicates transaction in progress
--     data_rd   : out    STD_LOGIC_VECTOR(7 downto 0); -- Data read from slave
--     ack_error : buffer STD_LOGIC;                    -- Flag if improper acknowledge from slave
--     sda       : inout  STD_LOGIC;                    -- Serial data output of i2c bus
--     scl       : inout  STD_LOGIC                     -- Serial clock output of i2c bus
--   );
--   end component i2c_master;

--   ---------------
--   -- Constants --
--   ---------------
--   constant C_CLK_FREQ_HZ         : integer := C_CLK_FREQ_MHZ * 1_000_000;
--   constant C_I2C_BUS_CLK_FREQ_HZ : integer := 100_000;  -- CDL=> 100_000 or 400_000?
--   constant C_I2C_7SD_ADDR        : std_logic_vector(6 downto 0) := 1110001;  -- 0x71
--   constant C_WR_BYTE_INDEX_MAX   : integer := 13; -- CDL=>?

--   constant C_7SD_CLEAR_DISP_CMD  : std_logic_vector(7 downto 0) := x"76"; -- CDL=> Explain and add more?
--   constant C_7SD_BRIGHTNESS_CMD  : std_logic_vector(7 downto 0) := x"7A";
--   constant C_7SD_DEC_CTRL_CMD    : std_logic_vector(7 downto 0) := x"77";
--   constant C_7SD_CURS_CTRL_CMD   : std_logic_vector(7 downto 0) := x"79";

--   -------------
--   -- SIGNALS --
--   -------------

--   signal s_display_data : std_logic_vector(15 downto 0);  -- Data to be displayed on I2C 7SD


--   signal s_wr_data_byte_index : integer;
--   signal s_wr_data_byte       : std_logic_vector(7 downto 0);

--   signal s_7sd_enable : std_logic;
--   signal s_7sd_wr : std_logic;  --'0' is write, '1' is read
--   signal s_7sd_address  : std_logic_vector(6 downto 0) := C_I2C_7SD_ADDR;
--   signal s_7sd_busy : std_logic;

--   -- State machine related signals
--   type T_7SD_STATE is (READY_STATE, WRITE_STATE);
--   signal s_7sd_prev_state : T_7SD_STATE := READY_STATE;
--   signal s_7sd_curr_state : T_7SD_STATE := READY_STATE;

--   signal s_display_data_prev    : std_logic_vector(15 downto 0);
--   signal s_display_data_latched : std_logic_vector(15 downto 0);


-- begin

--   -- I2C controller for 4 digit Seven Segment Display
--   I2C_MASTER_INST:i2c_master
--   generic map
--   (
--     input_clk => C_CLK_FREQ_HZ,
--     bus_clk   => C_I2C_BUS_CLK_FREQ_HZ
--   )
--   port map
--   (
--     clk       => I_CLK,
--     reset_n   => I_RESET_N,
--     ena       => s_7sd_enable,
--     addr      => s_7sd_address,
--     rw        => s_7sd_wr,
--     data_wr   => s_wr_data_byte,
--     busy      => s_7sd_busy,
--     data_rd   => open,
--     ack_error => open,
--     sda       => IO_I2C_SDA,
--     scl       => IO_I2C_SCL
--   );

--   ---------------
--   -- Processes --
--   ---------------

--   ------------------------------------------------------------------------------
--   -- Process Name     : I2C_STATE_MACHINE
--   -- Sensitivity List : I_CLK            : System clock
--   --                    I_RESET_N        : System reset (active low logic)
--   -- Useful Outputs   : s_7sd_prev_state : State at previous clock edge
--   --                    s_7sd_curr_state : State at current clock edge
--   -- Description      : State machine to control different states for I2C
--   --                    Seven Segment Display (7SD) module.
--   ------------------------------------------------------------------------------
--   I2C_STATE_MACHINE: process (I_CLK, I_RESET_N)
--   begin
--     if (I_RESET_N = '0') then
--       s_7sd_prev_state <= READY_STATE;
--       s_7sd_curr_state <= READY_STATE;

--     elsif (rising_edge(I_CLK)) then
--         -- Set current state to previous state
--         s_7sd_prev_state <= s_7sd_curr_state;

--         -- I2C 7SD state machine logic  -- CDL=> Add state change logic
--         case s_7sd_curr_state is
--           when READY_STATE =>
--             if (s_display_data_prev /= I_DISPLAY_DATA) then
--               s_7sd_curr_state <= WRITE_STATE;


--           when WRITE_STATE =>
--             s_7sd_curr_state <= READY_STATE;

--           -- Error condition, should never occur
--           when others =>
--             s_7sd_curr_state <= READY_STATE;
--         end case;
--     end if;
--   end process I2C_STATE_MACHINE;
--   ------------------------------------------------------------------------------

--   ------------------------------------------------------------------------------
--   -- Process Name     : WRITE_DATA_LUT
--   -- Sensitivity List : I_CLK          : System clock
--   --                    I_RESET_N      : System reset (active low logic)
--   -- Useful Outputs   : s_wr_data_byte : Data byte to send to Display
--   -- Description      : Process to act as Look Up Table (LUT) to decide what
--   --                    byte to send to display based on (s_wr_data_byte_index).
--   ------------------------------------------------------------------------------
--   WRITE_DATA_LUT: process (I_CLK, I_RESET_N)
--   begin
--     if (I_RESET_N = '0') then
--       s_wr_data_byte                  <= C_7SD_CLEAR_DISP_CMD;
--     elsif (rising_edge(I_CLK)) then
--       case s_wr_data_byte_index is
--         when 1      => s_wr_data_byte <= C_7SD_CLEAR_DISP_CMD;  -- CDL=> Needed to clear display 3 times?
--         when 2      => s_wr_data_byte <= C_7SD_CLEAR_DISP_CMD;
--         when 3      => s_wr_data_byte <= C_7SD_CLEAR_DISP_CMD;
--         when 4      => s_wr_data_byte <= C_7SD_BRIGHTNESS_CMD;
--         when 5      => s_wr_data_byte <= x"FF";  -- Max brightness  --CDL=> Right value?
--         when 6      => s_wr_data_byte <= C_7SD_DEC_CTRL_CMD;
--         when 7      => s_wr_data_byte <= x"00";  -- No decimal points  --CDL=> Right value?
--         when 8      => s_wr_data_byte <= C_7SD_CURS_CTRL_CMD;
--         when 9      => s_wr_data_byte <= x"00";  -- Reset cursor to digit 0
--         when 10     => s_wr_data_byte <= x"0" & dataIn(15 downto 12);
--         when 11     => s_wr_data_byte <= x"0" & dataIn(11 downto 8);
--         when 12     => s_wr_data_byte <= x"0" & dataIn(7 downto 4);
--         when 13     => s_wr_data_byte <= x"0" & dataIn(3 downto 0);
--         when others => s_wr_data_byte <= C_7SD_CLEAR_DISP_CMD;
--       end case;
--     end if;
--   end process WRITE_DATA_LUT;
--   ------------------------------------------------------------------------------

--   s_7sd_wr <= '0'; -- Always writing

-- end architecture behavioral;
