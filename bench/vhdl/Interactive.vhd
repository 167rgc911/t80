--
-- Interactive implementation which contains parts of
-- AsyncStim.vhd and AsyncLog.vhd (wit original copyright show below)
--
-- Asynchronous serial generator with input from binary file
--
-- Version : 0146
--
-- Copyright (c) 2001 Daniel Wallner (jesus@opencores.org)
--
-- Asynchronous serial input with binary file log
--
-- Version : 0146
--
-- Copyright (c) 2001 Daniel Wallner (jesus@opencores.org)
--
-- All rights reserved
--
-- Redistribution and use in source and synthezised forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- Redistributions in synthesized form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- Neither the name of the author nor the names of other contributors may
-- be used to endorse or promote products derived from this software without
-- specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
-- Please report bugs to the author, but before you do so, please
-- make sure that this is not a derivative work and that
-- you have the latest version of this file.
--
-- The latest version of this file can be found at:
--	http://www.opencores.org/cvsweb.shtml/t51/
--
-- Limitations :
--
-- File history :
--

library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

use work.ghdl_uart_pkg.all;

entity Interactive is
	generic(
		Baud			: integer;
		InterCharDelay	: time := 0 ns;
		Bits			: integer := 8;		-- Data bits
		Parity			: boolean := false;	-- Enable Parity
		P_Odd_Even_n	: boolean := false	-- false => Even Parity, true => Odd Parity
	);
	port(
		RXD				: in std_logic;
		TXD				: out std_logic
	);
end Interactive;

architecture behaviour of Interactive is

	function to_char(
		constant Byte : std_logic_vector(7 downto 0)
	) return character is
	begin
		return character'val(to_integer(unsigned(Byte)));
	end function;

	signal	Baud16			: std_logic := '0';

	-- Transmit signals
	signal	TX_ShiftReg		: std_logic_vector(Bits - 1 downto 0);
	signal	TX_Bit_Cnt		: integer range 0 to 15 := 0;
	signal	TX_ParTmp			: boolean;

	signal	TX_Data			: std_logic_vector(Bits - 1 downto 0);
	signal	TX_Ready		: std_logic := '0';
	signal	TX_Rdy_Reset		: std_logic := '0';
	signal	TX_Rdy_Set		: std_logic := '0';

	-- Receive signals
	signal	Bit_Phase		: unsigned(3 downto 0) := "0000";
	signal	RX_ShiftReg		: std_logic_vector(Bits - 1 downto 0) := (others => '0');
	signal	RX_Bit_Cnt		: integer := 0;
	signal	RX_ParTmp			: boolean;
begin

	process
		variable char_in : integer;
		variable res : integer;
	begin -- process reading from socket
		res := ghdl_pty_open;
		res := ghdl_pty_setupTerminal(115200);
		while true loop
			if TX_Rdy_Reset = '1' then
				TX_Ready <= '0';
			end if;
			char_in := ghdl_pty_read;
			if char_in = -2 then
				assert false report "Pseudoterminal disconnected?" severity failure;
			end if;
			if char_in = -1 then
			end if;
			if char_in >= 0 then
				if TX_Ready = '0' then
					if TX_Rdy_Set = '1' then
						TX_Data <= std_logic_vector(to_unsigned(char_in, Bits));
						TX_Ready <= '1';
					end if;
				end if;
			end if;
			wait for 31.25 ns;
		end loop;
	end process;

	process
		variable Inited			: boolean := false;
	begin
		if not Inited then
			Inited := true;
			TXD <= '1';
		end if;
		wait for 1000000000 ns / Baud;
		TX_Bit_Cnt <= TX_Bit_Cnt + 1;
		case TX_Bit_Cnt is
		when 0 =>
			TXD <= '1';
			TX_Rdy_Set <= '1';
			if TX_Ready = '0' then
				TX_Rdy_Reset <= '0';
			end if;
			wait for InterCharDelay;
		when 1 => -- Start bit
			if TX_Ready = '1' then
				TX_ShiftReg(Bits - 1 downto 0) <= TX_Data;
				TXD <= '0';
				TX_ParTmp <= P_Odd_Even_n;
			end if;
			TX_Rdy_Set <= '0';
		when others =>
			if TX_Ready = '1' then
				TXD <= TX_ShiftReg(0);
				TX_ParTmp <= TX_ParTmp xor (TX_ShiftReg(0) = '1');
				TX_ShiftReg(Bits - 2 downto 0) <= TX_ShiftReg(Bits - 1 downto 1);
			end if;
			if (TX_Bit_Cnt = Bits + 1 and not Parity) or
				(TX_Bit_Cnt = Bits + 2 and Parity) then -- Stop bit
				TX_Bit_Cnt <= 0;
				if TX_Ready = '1' then
					TX_Rdy_Reset <= '1';
				end if;
			end if;
			if Parity and TX_Bit_Cnt = Bits + 2 then
				if TX_ParTmp then
					TXD <= '1';
				else
					TXD <= '0';
				end if;
			end if;
		end case;
	end process;



	Baud16 <= not Baud16 after 1000000000 ns / 32 / Baud;

	process (Baud16)
		variable char_d : integer;
		variable res : integer;
	begin
		if Baud16'event and Baud16 = '1' then
			if RX_Bit_Cnt = 0 and (RXD = '1' or Bit_Phase = "0111") then
				Bit_Phase <= "0000";
			else
				Bit_Phase <= Bit_Phase + 1;
			end if;
			if RX_Bit_Cnt = 0 then
				if Bit_Phase = "0111" then
					RX_Bit_Cnt <= RX_Bit_Cnt + 1;
				end if;
				RX_ParTmp <= false;
			elsif Bit_Phase = "1111" then
				RX_Bit_Cnt <= RX_Bit_Cnt + 1;
				if (RX_Bit_Cnt = Bits + 1 and not Parity) or
					(RX_Bit_Cnt = Bits + 2 and Parity) then -- Stop bit
					RX_Bit_Cnt <= 0;
					assert RXD = '1'
						report "Framing error"
						severity error;
					char_d := to_integer(unsigned(RX_ShiftReg(7 downto 0)));
					res := ghdl_pty_write(char_d);
				elsif RX_Bit_Cnt = Bits + 1 and Parity then -- Parity bit
					assert RX_ParTmp xor (RXD = '1') = P_Odd_Even_n
						report "Parity error"
						severity error;
				else
					RX_ParTmp <= RX_ParTmp xor (RXD = '1');
					RX_ShiftReg(Bits - 2 downto 0) <= RX_ShiftReg(Bits - 1 downto 1);
					RX_ShiftReg(Bits - 1) <= RXD;
				end if;
			end if;
		end if;
	end process;

end;

