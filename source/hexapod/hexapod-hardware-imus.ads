--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  IMU sensor configuration and initialization.

with A0B.MPU6050;
--  with A0B.MPU6500;
--  with A0B.MPU9150;
--  with A0B.MPU9250;

package Hexapod.Hardware.IMUs
  --  with Preelaborate
is

   IMUB_Sensor : constant not null access A0B.MPU6050.MPU6050_Sensor'Class;
   --  IMUB_Sensor : constant not null access A0B.MPU9250.MPU9250_Sensor'Class;

   procedure Initialize;
   --  Initialize and configure IMU.
   --
   --  It is synchronous procedure, it returns only when IMU is
   --  initialized/configured.

private

   --  IMUB : aliased A0B.MPU9250.MPU9250_Sensor
   IMUB : aliased A0B.MPU6050.MPU6050_Sensor
     (Controller => I2C2_Controller'Access,
      Address    => 16#68#,
      INT_Pin    => Hexapod.Hardware.IMU_INT_Pin'Access);

   --  IMUB_Sensor : constant not null access A0B.MPU9250.MPU9250_Sensor'Class :=
   IMUB_Sensor : constant not null access A0B.MPU6050.MPU6050_Sensor'Class :=
     IMUB'Access;

end Hexapod.Hardware.IMUs;
