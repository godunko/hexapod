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
      SPI.USART2_SPI.Configure;

      ACK_Pin.Configure_EXTI
        (Mode => A0B.STM32F401.GPIO.Rising_Edge,
         Pull => A0B.STM32F401.GPIO.No);
   end Initialize;

end Hexapod.Remote_Control.Configuration;
