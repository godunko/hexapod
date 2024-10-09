--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Hardware configuration and initialization.

with A0B.ATSAM3X8E.PIO;
with A0B.I2C.ATSAM3X8E_TWI.TWI0;
with A0B.I2C.ATSAM3X8E_TWI.TWI1;

with BBF.Board;

package Hexapod.Hardware is

   Left_Motor_Power_Relay  : A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class
     renames A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class (BBF.Board.Pin_51);
   Right_Motor_Power_Relay : A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class 
     renames A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class (BBF.Board.Pin_50);

   I2C1_Controller : A0B.I2C.I2C_Bus_Master'Class
     renames A0B.I2C.I2C_Bus_Master'Class (A0B.I2C.ATSAM3X8E_TWI.TWI0.TWI0);
   I2C2_Controller : A0B.I2C.I2C_Bus_Master'Class
     renames A0B.I2C.I2C_Bus_Master'Class (A0B.I2C.ATSAM3X8E_TWI.TWI1.TWI1);

   IMU_INT_Pin  : A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class
     renames A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class (BBF.Board.Pin_51);

   procedure Initialize_Hardware;
   --  Do basic initialization of the hardware.

   procedure Enable_Motors_Power;
   --  Switch relays to provide power to motors.

   procedure Disable_Motors_Power;
   --  Switch relays to power off motors.

end Hexapod.Hardware;
