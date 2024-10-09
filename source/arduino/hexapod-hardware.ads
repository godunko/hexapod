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
--  with BBF.Drivers.MPU6050;
--  with BBF.Drivers.MPU9250;

package Hexapod.Hardware is

   Left_Motor_Power_Relay  : A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class
     renames A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class (BBF.Board.Pin_51);
   Right_Motor_Power_Relay : A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class 
     renames A0B.ATSAM3X8E.PIO.ATSAM3X8E_Pin'Class (BBF.Board.Pin_50);

   I2C1_Controller : A0B.I2C.I2C_Bus_Master'Class
     renames A0B.I2C.I2C_Bus_Master'Class (A0B.I2C.ATSAM3X8E_TWI.TWI0.TWI0);

   --  Body_Position_Sensor    :
   --    constant not null access BBF.Drivers.MPU9250.MPU9250_Sensor'Class;
   --  Body_Position_Sensor    :
   --    constant not null access BBF.Drivers.MPU6050.MPU6050_Sensor'Class;

   procedure Initialize_Hardware;
   --  Do basic initialization of the hardware.

   procedure Enable_Motors_Power;
   --  Switch relays to provide power to motors.

   procedure Disable_Motors_Power;
   --  Switch relays to power off motors.

private

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
