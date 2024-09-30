--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with A0B.STM32F401.SVD.RCC; use A0B.STM32F401.SVD.RCC;

package body Hexapod.Remote_Control.Configuration is

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      RCC_Periph.AHB1ENR.GPIOAEN  := True;
      RCC_Periph.AHB1ENR.GPIOBEN  := True;
      RCC_Periph.APB2ENR.SYSCFGEN := True;

      SPI.USART2_SPI.Configure;

      ACK_Pin.Configure_EXTI
        (Mode => A0B.STM32F401.GPIO.Rising_Edge,
         Pull => A0B.STM32F401.GPIO.Pull_Up);
   end Initialize;

end Hexapod.Remote_Control.Configuration;
