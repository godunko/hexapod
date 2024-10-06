--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Hardware configuration and initialization.

with A0B.ATSAM3X8E.PIO;
with A0B.I2C.ATSAM3X8E_TWI.TWI0;
with A0B.I2C.ATSAM3X8E_TWI.TWI1;
with A0B.PCA9685.Drivers;

with BBF.Board;
--  with BBF.Drivers.MPU6050;
--  with BBF.Drivers.MPU9250;

package Hexapod.Hardware is

   Left_Motor_Power_Relay  : A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class
     renames A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class (BBF.Board.Pin_51);
   Right_Motor_Power_Relay : A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class 
     renames A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class (BBF.Board.Pin_50);

   --  Body_Position_Sensor    :
   --    constant not null access BBF.Drivers.MPU9250.MPU9250_Sensor'Class;
   --  Body_Position_Sensor    :
   --    constant not null access BBF.Drivers.MPU6050.MPU6050_Sensor'Class;

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

   procedure Initialize_Hardware;
   --  Do basic initialization of the hardware.

   procedure Configure_Controllers;

   procedure Enable_Motors_Power;
   --  Switch relays to provide power to motors.

   procedure Disable_Motors_Power;
   --  Switch relays to power off motors.

   PWM1 : aliased A0B.PCA9685.Drivers.PCA9685_Controller_Driver
                    (Controller => A0B.I2C.ATSAM3X8E_TWI.TWI0.TWI0'Access,
                     Address    => 16#40#);
   PWM2 : aliased A0B.PCA9685.Drivers.PCA9685_Controller_Driver
                    (Controller => A0B.I2C.ATSAM3X8E_TWI.TWI1.TWI1'Access,
                     Address    => 16#40#);

private

   PWM_Frequency : constant := 321;
   --  Closest value for supported by PCA 9685 controller.

   PWM1_Controller  :
     constant not null access A0B.PCA9685.PCA9685_Controller'Class :=
       PWM1'Access;
   PWM2_Controller :
     constant not null access A0B.PCA9685.PCA9685_Controller'Class :=
       PWM2'Access;

   LF_Motor_1_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_00'Access;
   LF_Motor_2_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_01'Access;
   LF_Motor_3_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_02'Access;
   LM_Motor_1_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_03'Access;

   LM_Motor_2_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_04'Access;
   LM_Motor_3_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_05'Access;
   LH_Motor_1_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_06'Access;
   LH_Motor_2_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_07'Access;

   LH_Motor_3_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM1.Channel_08'Access;

   RF_Motor_1_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_00'Access;
   RF_Motor_2_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_01'Access;
   RF_Motor_3_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_02'Access;
   RM_Motor_1_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_03'Access;

   RM_Motor_2_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_04'Access;
   RM_Motor_3_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_05'Access;
   RH_Motor_1_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_06'Access;
   RH_Motor_2_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_07'Access;

   RH_Motor_3_Channel     :
     constant not null access A0B.PCA9685.PCA9685_Channel'Class :=
       PWM2.Channel_08'Access;

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
