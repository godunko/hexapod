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

   Servo_Controller_Left  : aliased BBF.Drivers.PCA9685.PCA9685_Controller_Driver
    (Bus => BBF.Board.I2C.I2C1, Device => 16#40#);
   Servo_Controller_Right : aliased BBF.Drivers.PCA9685.PCA9685_Controller_Driver
    (Bus => BBF.Board.I2C.I2C0, Device => 16#40#);

   LF_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Left.Channel_00'Access;
   LF_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Left.Channel_01'Access;
   LF_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Left.Channel_02'Access;
   LM_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Left.Channel_03'Access;

   LM_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Left.Channel_04'Access;
   LM_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Left.Channel_05'Access;
   LH_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Left.Channel_06'Access;
   LH_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Left.Channel_07'Access;

   LH_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Left.Channel_08'Access;

   RF_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Right.Channel_00'Access;
   RF_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Right.Channel_01'Access;
   RF_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Right.Channel_02'Access;
   RM_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Right.Channel_03'Access;

   RM_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Right.Channel_04'Access;
   RM_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Right.Channel_05'Access;
   RH_Motor_1_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Right.Channel_06'Access;
   RH_Motor_2_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Right.Channel_07'Access;

   RH_Motor_3_Channel     :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Servo_Controller_Right.Channel_08'Access;

end Hexapod.Hardware;
