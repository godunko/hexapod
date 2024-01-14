--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Hardware configuration and initialization.

with BBF.Board.I2C;
private with BBF.Drivers.PCA9685;
--  with BBF.Drivers.MPU6050;
with BBF.Drivers.MPU9250;
with BBF.GPIO;
with BBF.PCA9685;

package Hexapod.Hardware is

   Left_Motor_Power_Relay  : not null access BBF.GPIO.Pin'Class
     renames BBF.Board.Pin_51;
   Right_Motor_Power_Relay : not null access BBF.GPIO.Pin'Class
     renames BBF.Board.Pin_50;

   Body_Position_Sensor    :
     constant not null access BBF.Drivers.MPU9250.MPU9250_Sensor'Class;
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

private

   Left_PWM_Controller    : aliased BBF.Drivers.PCA9685.PCA9685_Controller_Driver
    (Bus => BBF.Board.I2C.I2C0, Device => 16#40#);
   Right_PWM_Controller   : aliased BBF.Drivers.PCA9685.PCA9685_Controller_Driver
    (Bus => BBF.Board.I2C.I2C1, Device => 16#40#);

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
   Body_Position_Sensor_I : aliased BBF.Drivers.MPU9250.MPU9250_Sensor
     (Bus    => BBF.Board.I2C.I2C0,
      Device => 16#68#,
      --  Pin    => BBF.Board.Pin_50,
      Pin    => BBF.Board.Pin_23,
      Clocks => BBF.Board.Real_Time_Clock_Controller);

   Body_Position_Sensor   :
     constant not null access BBF.Drivers.MPU9250.MPU9250_Sensor'Class :=
     --  constant not null access BBF.Drivers.MPU6050.MPU6050_Sensor'Class :=
       Body_Position_Sensor_I'Access;

end Hexapod.Hardware;
