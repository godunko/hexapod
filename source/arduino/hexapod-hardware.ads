--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Hardware configuration and initialization.

with BBF.Board.I2C;
private with BBF.Drivers.PCA9685;
with BBF.GPIO;
with BBF.PCA9685;

package Hexapod.Hardware is

   pragma Preelaborate;

   Motor_Power_Relay      : not null access BBF.GPIO.Pin'Class
     renames BBF.Board.Pin_52;

   Left_Servo_Controller  :
     constant not null access BBF.PCA9685.PCA9685_Controller'Class;
   Right_Servo_Controller :
     constant not null access BBF.PCA9685.PCA9685_Controller'Class;

   LF_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   LF_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   LF_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   LM_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   LM_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   LM_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   LH_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   LH_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   LH_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   RF_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   RF_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   RF_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   RM_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   RM_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   RM_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   RH_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   RH_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   RH_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   procedure Initialize_Hardware;
   --  Do basic initialization of the hardware.

   procedure Configure_Controllers;

private

   Left_PWM_Controller    : aliased BBF.Drivers.PCA9685.PCA9685_Controller_Driver
    (Bus => BBF.Board.I2C.I2C1, Device => 16#40#);
   Right_PWM_Controller   : aliased BBF.Drivers.PCA9685.PCA9685_Controller_Driver
    (Bus => BBF.Board.I2C.I2C0, Device => 16#40#);

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

end Hexapod.Hardware;
