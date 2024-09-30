--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  This is version for BalckPill STM32F401 board

with A0B.STM32F401.GPIO.PIOA;
with A0B.STM32F401.GPIO.PIOB;
with A0B.STM32F401.USART.Generic_USART2_SPI;

private package Hexapod.Remote_Control.Configuration is

   package SPI is
     new A0B.STM32F401.USART.Generic_USART2_SPI
       (MOSI_Pin => A0B.STM32F401.GPIO.PIOA.PA2'Access,
        MISO_Pin => A0B.STM32F401.GPIO.PIOA.PA3'Access,
        SCK_Pin  => A0B.STM32F401.GPIO.PIOA.PA4'Access,
        NSS_Pin  => A0B.STM32F401.GPIO.PIOA.PA1'Access);

   SPI_Device : A0B.STM32F401.USART.USART_SPI_Device
     renames SPI.USART2_SPI;
   ACK_Pin    : A0B.STM32F401.GPIO.GPIO_Line
     renames A0B.STM32F401.GPIO.PIOB.PB2;

   procedure Initialize;
   --  Initialize and configure MCU's peripherals.

end Hexapod.Remote_Control.Configuration;
