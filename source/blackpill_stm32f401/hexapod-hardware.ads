--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Hardware configuration and initialization.

with A0B.I2C.STM32F401_I2C.Generic_I2C1;
with A0B.I2C.STM32F401_I2C.Generic_I2C2;
with A0B.STM32F401.DMA.DMA1.Stream0;
with A0B.STM32F401.DMA.DMA1.Stream2;
with A0B.STM32F401.DMA.DMA1.Stream6;
with A0B.STM32F401.DMA.DMA1.Stream7;
with A0B.STM32F401.GPIO.PIOB;

with BBF.Drivers.PCA9685;
--  with BBF.Drivers.MPU6050;
--  with BBF.Drivers.MPU9250;
with BBF.PCA9685;

package Hexapod.Hardware is

   package I2C1 is
     new A0B.I2C.STM32F401_I2C.Generic_I2C1
       (Transmit_Stream => A0B.STM32F401.DMA.DMA1.Stream6.DMA1_Stream6'Access,
        Receive_Stream  => A0B.STM32F401.DMA.DMA1.Stream0.DMA1_Stream0'Access,
        SCL_Pin         => A0B.STM32F401.GPIO.PIOB.PB8'Access,
        SDA_Pin         => A0B.STM32F401.GPIO.PIOB.PB9'Access);

   package I2C2 is
     new A0B.I2C.STM32F401_I2C.Generic_I2C2
       (Transmit_Stream => A0B.STM32F401.DMA.DMA1.Stream7.DMA1_Stream7'Access,
        Receive_Stream  => A0B.STM32F401.DMA.DMA1.Stream2.DMA1_Stream2'Access);

   --  package Bus_3 is
   --    new A0B.I2C.STM32F401_I2C.Generic_I2C3
   --      (Transmit_Stream  => A0B.STM32F401.DMA.DMA1.Stream4.DMA1_Stream4'Access,
   --       Transmit_Channel => 3,
   --       Receive_Stream   => A0B.STM32F401.DMA.DMA1.Stream1.DMA1_Stream1'Access,
   --       Receive_Channel  => 1,
   --       SDA_Pin          => A0B.STM32F401.GPIO.PIOB.PB4'Access);

   Left_Motor_Power_Relay  : A0B.STM32F401.GPIO.GPIO_Line
     renames A0B.STM32F401.GPIO.PIOB.PB0;
   Right_Motor_Power_Relay : A0B.STM32F401.GPIO.GPIO_Line
     renames A0B.STM32F401.GPIO.PIOB.PB1;

   --  Body_Position_Sensor    :
   --    constant not null access BBF.Drivers.MPU9250.MPU9250_Sensor'Class;
   --  Body_Position_Sensor    :
   --    constant not null access BBF.Drivers.MPU6050.MPU6050_Sensor'Class;

   Left_Servo_Controller   :
     constant not null access BBF.PCA9685.PCA9685_Controller'Class;
   Right_Servo_Controller  :
     constant not null access BBF.PCA9685.PCA9685_Controller'Class;

   LF_Motor_1_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   LF_Motor_2_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   LF_Motor_3_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   LM_Motor_1_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   LM_Motor_2_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   LM_Motor_3_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   LH_Motor_1_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   LH_Motor_2_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   LH_Motor_3_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   RF_Motor_1_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   RF_Motor_2_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   RF_Motor_3_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   RM_Motor_1_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   RM_Motor_2_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   RM_Motor_3_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   RH_Motor_1_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   RH_Motor_2_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   RH_Motor_3_Channel      :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   procedure Initialize_Hardware;
   --  Do basic initialization of the hardware.

   procedure Configure_Controllers;

   procedure Enable_Motors_Power;
   --  Switch relays to provide power to motors.

   procedure Disable_Motors_Power;
   --  Switch relays to power off motors.

   Left_PWM_Controller    : aliased BBF.Drivers.PCA9685.PCA9685_Controller_Driver
    (Controller => I2C1.I2C1'Access,
     Address    => 16#40#);
   Right_PWM_Controller   : aliased BBF.Drivers.PCA9685.PCA9685_Controller_Driver
    (Controller => I2C2.I2C2'Access,
     Address    => 16#40#);

private

   PWM_Frequency : constant := 321;
   --  Closest value for supported by PCA 9685 controller.

   Left_Servo_Controller  :
     constant not null access BBF.PCA9685.PCA9685_Controller'Class :=
       Left_PWM_Controller'Access;
   Right_Servo_Controller :
     constant not null access BBF.PCA9685.PCA9685_Controller'Class :=
       Right_PWM_Controller'Access;

   LF_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_00'Access;
   LF_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_01'Access;
   LF_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_02'Access;
   LM_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_03'Access;

   LM_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_04'Access;
   LM_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_05'Access;
   LH_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_06'Access;
   LH_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_07'Access;

   LH_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_08'Access;

   RF_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_00'Access;
   RF_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_01'Access;
   RF_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_02'Access;
   RM_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_03'Access;

   RM_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_04'Access;
   RM_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_05'Access;
   RH_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_06'Access;
   RH_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_07'Access;

   RH_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_08'Access;

   --  Body_Position_Sensor_I : aliased BBF.Drivers.MPU6050.MPU6050_Sensor
   --  Body_Position_Sensor_I : aliased BBF.Drivers.MPU9250.MPU9250_Sensor
   --    (Bus    => BBF.Board.I2C.I2C0,
   --     Device => 16#68#,
   --     --  Pin    => BBF.Board.Pin_50,
   --     Pin    => BBF.Board.Pin_23,
   --     Clocks => BBF.Board.Real_Time_Clock_Controller);

   --  Body_Position_Sensor   :
   --    constant not null access BBF.Drivers.MPU9250.MPU9250_Sensor'Class :=
   --    --  constant not null access BBF.Drivers.MPU6050.MPU6050_Sensor'Class :=
   --      Body_Position_Sensor_I'Access;

end Hexapod.Hardware;
