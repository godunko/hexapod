--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Hardware configuration and initialization.

with BBF.Board.I2C;
with BBF.Drivers.PCA9685;

package Hexapod.Hardware is

   Servo_Controller_Left  : aliased BBF.Drivers.PCA9685.PCA9685_Controller
    (Bus => BBF.Board.I2C.I2C1);
   Servo_Controller_Right : aliased BBF.Drivers.PCA9685.PCA9685_Controller
    (Bus => BBF.Board.I2C.I2C0);

   procedure Initialize_Hardware;
   --  Do basic initialization of the hardware.

   procedure Configure_Controllers;

end Hexapod.Hardware;
