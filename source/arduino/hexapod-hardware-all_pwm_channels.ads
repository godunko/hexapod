--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Direct access to all PWM channels. For use by motors playground only!

pragma Restrictions (No_Elaboration_Code);

package Hexapod.Hardware.All_PWM_Channels is

   L_Channel_00 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   L_Channel_01 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   L_Channel_02 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   L_Channel_03 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   L_Channel_04 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   L_Channel_05 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   L_Channel_06 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   L_Channel_07 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   L_Channel_08 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   L_Channel_09 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   L_Channel_10 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   L_Channel_11 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   L_Channel_12 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   L_Channel_13 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   L_Channel_14 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   L_Channel_15 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   R_Channel_00 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   R_Channel_01 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   R_Channel_02 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   R_Channel_03 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   R_Channel_04 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   R_Channel_05 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   R_Channel_06 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   R_Channel_07 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   R_Channel_08 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   R_Channel_09 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   R_Channel_10 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   R_Channel_11 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;

   R_Channel_12 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   R_Channel_13 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   R_Channel_14 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;
   R_Channel_15 : constant not null access BBF.PCA9685.PCA9685_Channel'Class;

private

   L_Channel_00 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_00'Access;
   L_Channel_01 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_01'Access;
   L_Channel_02 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_02'Access;
   L_Channel_03 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_03'Access;

   L_Channel_04 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_04'Access;
   L_Channel_05 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_05'Access;
   L_Channel_06 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_06'Access;
   L_Channel_07 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_07'Access;

   L_Channel_08 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_08'Access;
   L_Channel_09 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_09'Access;
   L_Channel_10 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_10'Access;
   L_Channel_11 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_11'Access;

   L_Channel_12 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_12'Access;
   L_Channel_13 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_13'Access;
   L_Channel_14 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_14'Access;
   L_Channel_15 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Left_PWM_Controller.Channel_15'Access;

   R_Channel_00 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_00'Access;
   R_Channel_01 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_01'Access;
   R_Channel_02 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_02'Access;
   R_Channel_03 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_03'Access;

   R_Channel_04 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_04'Access;
   R_Channel_05 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_05'Access;
   R_Channel_06 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_06'Access;
   R_Channel_07 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_07'Access;

   R_Channel_08 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_08'Access;
   R_Channel_09 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_09'Access;
   R_Channel_10 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_10'Access;
   R_Channel_11 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_11'Access;

   R_Channel_12 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_12'Access;
   R_Channel_13 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_13'Access;
   R_Channel_14 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_14'Access;
   R_Channel_15 :
     constant not null access BBF.PCA9685.PCA9685_Channel'Class :=
       Right_PWM_Controller.Channel_15'Access;

end Hexapod.Hardware.All_PWM_Channels;
