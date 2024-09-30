--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Hexapod.Remote_Control.Configuration is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      A0B.ATSAM3X8E.USART.SPI.USART1_SPI.Configure;
      --  Device.Configure;

      ACK_Pin.Configure_EXTI (A0B.ATSAM3X8E.PIO.Rising_Edge, Pullup => True);
   end Initialize;

end Hexapod.Remote_Control.Configuration;
