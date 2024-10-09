--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Hardware configuration and initialization.

with A0B.STM32F401.DMA.DMA1.Stream0;
with A0B.STM32F401.DMA.DMA1.Stream2;
with A0B.STM32F401.DMA.DMA1.Stream6;
with A0B.STM32F401.DMA.DMA1.Stream7;
with A0B.STM32F401.GPIO.PIOB;
with A0B.STM32F401.I2C.Generic_I2C1;
with A0B.I2C.STM32F401_I2C.Generic_I2C2;
--  with A0B.STM32F401.I2C.Generic_I2C2;
--  XXX GNAT FSF 14: unable to find generic's body of Generic_I2C2. With clause
--  below is a workaround:

package Hexapod.Hardware
  --  with Preelaborate
is

   package I2C1 is
     new A0B.STM32F401.I2C.Generic_I2C1
       (Transmit_Stream => A0B.STM32F401.DMA.DMA1.Stream6.DMA1_Stream6'Access,
        Receive_Stream  => A0B.STM32F401.DMA.DMA1.Stream0.DMA1_Stream0'Access,
        SCL_Pin         => A0B.STM32F401.GPIO.PIOB.PB8'Access,
        SDA_Pin         => A0B.STM32F401.GPIO.PIOB.PB9'Access);

   package I2C2 is
     new A0B.STM32F401.I2C.Generic_I2C2
       (Transmit_Stream => A0B.STM32F401.DMA.DMA1.Stream7.DMA1_Stream7'Access,
        Receive_Stream  => A0B.STM32F401.DMA.DMA1.Stream2.DMA1_Stream2'Access);

   --  package Bus_3 is
   --    new A0B.I2C.STM32F401_I2C.Generic_I2C3
   --      (Transmit_Stream  => A0B.STM32F401.DMA.DMA1.Stream4.DMA1_Stream4'Access,
   --       Transmit_Channel => 3,
   --       Receive_Stream   => A0B.STM32F401.DMA.DMA1.Stream1.DMA1_Stream1'Access,
   --       Receive_Channel  => 1,
   --       SDA_Pin          => A0B.STM32F401.GPIO.PIOB.PB4'Access);

   I2C1_Controller : A0B.I2C.I2C_Bus_Master'Class
     renames A0B.I2C.I2C_Bus_Master'Class (I2C1.I2C1);
   I2C2_Controller : A0B.I2C.I2C_Bus_Master'Class
     renames A0B.I2C.I2C_Bus_Master'Class (I2C2.I2C2);

   Left_Motor_Power_Relay  : A0B.STM32F401.GPIO.GPIO_Line
     renames A0B.STM32F401.GPIO.PIOB.PB0;
   Right_Motor_Power_Relay : A0B.STM32F401.GPIO.GPIO_Line
     renames A0B.STM32F401.GPIO.PIOB.PB1;

   IMU_INT_Pin : A0B.STM32F401.GPIO.GPIO_Line
     renames A0B.STM32F401.GPIO.PIOB.PB15;

   procedure Initialize_Hardware;
   --  Do basic initialization of the hardware.

   procedure Enable_Motors_Power;
   --  Switch relays to provide power to motors.

   procedure Disable_Motors_Power;
   --  Switch relays to power off motors.

end Hexapod.Hardware;
