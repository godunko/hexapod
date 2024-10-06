--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Hardware configuration and initialization.

with A0B.PCA9685.Drivers;
--  with A0B.STM32F401.DMA.DMA1.Stream0;
--  --  with A0B.STM32F401.DMA.DMA1.Stream2;
--  with A0B.STM32F401.DMA.DMA1.Stream6;
--  --  with A0B.STM32F401.DMA.DMA1.Stream7;
--  with A0B.STM32F401.GPIO.PIOB;
--  with A0B.STM32F401.I2C.Generic_I2C1;
--  --  XXX GNAT FSF 14: unable to find generic's body of Generic_I2C2. With clause
--  --  below is a workaround:
--  --  with A0B.I2C.STM32F401_I2C.Generic_I2C1;
--  --  with A0B.STM32F401.I2C.Generic_I2C2;
--
--  --  with BBF.Drivers.MPU6050;
--  --  with BBF.Drivers.MPU9250;

package Hexapod.Hardware.PWMs
  --  with Preelaborate
is

   PWM1_Controller :
     constant not null access A0B.PCA9685.PCA9685_Controller'Class;
   PWM2_Controller :
     constant not null access A0B.PCA9685.PCA9685_Controller'Class;

   LF_Motor_1_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;
   LF_Motor_2_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;
   LF_Motor_3_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;

   LM_Motor_1_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;
   LM_Motor_2_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;
   LM_Motor_3_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;

   LH_Motor_1_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;
   LH_Motor_2_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;
   LH_Motor_3_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;

   RF_Motor_1_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;
   RF_Motor_2_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;
   RF_Motor_3_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;

   RM_Motor_1_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;
   RM_Motor_2_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;
   RM_Motor_3_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;

   RH_Motor_1_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;
   RH_Motor_2_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;
   RH_Motor_3_Channel      :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class;

   PWM1 : aliased A0B.PCA9685.Drivers.PCA9685_Controller_Driver
                    (Controller => I2C1_Controller'Access,
                     Address    => 16#40#);
   PWM2 : aliased A0B.PCA9685.Drivers.PCA9685_Controller_Driver
                    (Controller => I2C1_Controller'Access,
                     Address    => 16#41#);

   procedure Initialize;
   --  Initialize and configure PWM controllers.
   --
   --  It is synchronous procedure, it returns only when controllers are
   --  initialized/configured.

private

   PWM_Frequency : constant := 321;
   --  Closest value for supported by PCA 9685 controller.

   PWM1_Controller :
     constant not null access A0B.PCA9685.PCA9685_Controller'Class :=
       PWM1'Access;
   PWM2_Controller :
     constant not null access A0B.PCA9685.PCA9685_Controller'Class :=
       PWM2'Access;

   LF_Motor_1_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_12'Access;
   LF_Motor_2_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_13'Access;
   LF_Motor_3_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_14'Access;

   LM_Motor_1_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_08'Access;
   LM_Motor_2_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_09'Access;
   LM_Motor_3_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_10'Access;

   LH_Motor_1_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_04'Access;
   LH_Motor_2_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_05'Access;
   LH_Motor_3_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_06'Access;

   RF_Motor_1_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_12'Access;
   RF_Motor_2_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_13'Access;
   RF_Motor_3_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_14'Access;

   RM_Motor_1_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_08'Access;
   RM_Motor_2_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_09'Access;
   RM_Motor_3_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_10'Access;

   RH_Motor_1_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_04'Access;
   RH_Motor_2_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_05'Access;
   RH_Motor_3_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_06'Access;

end Hexapod.Hardware.PWMs;
