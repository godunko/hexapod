--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Numerics;
with Interfaces;

with BBF.Board;
with BBF.Drivers.PCA9685;

with Kinematics.Forward;
with Kinematics.Inverse.Algebraic;
with Kinematics.Inverse.Geometric;
with Reals;
with Trajectory;

with Hexapod.Console;
with Hexapod.Hardware;

procedure Hexapod.Driver is

   use type Reals.Real;

   procedure Start_SMCs;

   Position : Kinematics.Position;

   type Motor_Descriptor is record
      Min_PWM   : BBF.Drivers.PCA9685.Value_Type;
      Max_PWM   : BBF.Drivers.PCA9685.Value_Type;
      Min_Angle : Reals.Real;
      Max_Angle : Reals.Real;
   end record;

   M_1 : constant Motor_Descriptor :=
     (170, 1070, -Ada.Numerics.Pi / 2.0, Ada.Numerics.Pi / 2.0);
   M_2 : constant Motor_Descriptor :=
     (170, 1070, -Ada.Numerics.Pi / 2.0, Ada.Numerics.Pi / 2.0);
   M_3 : constant Motor_Descriptor :=
     (170,
      1070,
      -Ada.Numerics.Pi + 0.16, Ada.Numerics.Pi / 2.0 + 0.32);

   function Coordinate_Image (Item : Reals.Real) return String;

   -----------------
   -- Angle_Image --
   -----------------

   function Angle_Image (Item : Reals.Real) return String is
      I  : constant Integer := Integer (Item / Ada.Numerics.Pi * 180_000.0);
      F  : constant Integer := I / 1_000;
      A  : constant Integer := I mod 1_000;
      FI : constant String := Integer'Image (F);
      AI : constant String := Integer'Image (A);
      B  : String := "   0.000";

   begin
      B (4 - (FI'Length) + 1 .. 4) := FI (FI'First .. FI'Last);
      B (8 - (AI'Length - 1) + 1 .. 8) := AI (AI'First + 1 .. AI'Last);

      return B;
   end Angle_Image;

   ----------------------
   -- Coordinate_Image --
   ----------------------

   function Coordinate_Image (Item : Reals.Real) return String is
      I  : constant Integer := Integer (Item * 1_000_000.0);
      F  : constant Integer := I / 1_000;
      A  : constant Integer := I mod 1_000;
      FI : constant String := Integer'Image (F);
      AI : constant String := Integer'Image (A);
      B  : String := "   0.000";

   begin
      B (4 - (FI'Length) + 1 .. 4) := FI (FI'First .. FI'Last);
      B (8 - (AI'Length - 1) + 1 .. 8) := AI (AI'First + 1 .. AI'Last);

      return B;
   end Coordinate_Image;

   ----------
   -- Move --
   ----------

   procedure Move
     (Posture : Kinematics.Posture;
      Wait    : Interfaces.Unsigned_32)
   is
      function Map
        (Descriptor : Motor_Descriptor;
         Angle      : Reals.Real) return BBF.Drivers.PCA9685.Value_Type;

      ---------
      -- Map --
      ---------

      function Map
        (Descriptor : Motor_Descriptor;
         Angle      : Reals.Real) return BBF.Drivers.PCA9685.Value_Type
      is
         use type BBF.Drivers.PCA9685.Value_Type;

         Angle_Middle : constant Reals.Real :=
           (Descriptor.Min_Angle + Descriptor.Max_Angle) / 2.0;
         Angle_Range  : constant Reals.Real :=
           Descriptor.Max_Angle - Descriptor.Min_Angle;
         PWM_Range    : constant Reals.Real :=
           Reals.Real (Descriptor.Max_PWM - Descriptor.Min_PWM);

      begin
         return
           BBF.Drivers.PCA9685.Value_Type
             ((Angle - Descriptor.Min_Angle)
              / ( Descriptor.Max_Angle - Descriptor.Min_Angle) * PWM_Range
                 + Reals.Real (Descriptor.Min_PWM));
      end Map;

      A : BBF.Drivers.PCA9685.Value_Type;
      B : BBF.Drivers.PCA9685.Value_Type;
      C : BBF.Drivers.PCA9685.Value_Type;

   begin
      A := Map (M_1, - Kinematics.Theta_1 (Posture));
      B := Map (M_2, + Kinematics.Theta_2 (Posture));
      C := Map (M_3, - Kinematics.Theta_3 (Posture));
      --  Motors of the first and third joints not installed on the base,
      --  they rotate themself. Thus, '-' is necessary to "revert" rotation,
      --  or mathematical model need to be fixed.

      Console.Put_Line
        ("Motors : "
         & BBF.Drivers.PCA9685.Value_Type'Image (A)
         & BBF.Drivers.PCA9685.Value_Type'Image (B)
         & BBF.Drivers.PCA9685.Value_Type'Image (C));

      Hexapod.Hardware.Servo_Controller_Left.Set_Something (2, C);
      BBF.Board.Delay_Controller.Delay_Milliseconds (Wait);
      Hexapod.Hardware.Servo_Controller_Left.Set_Something (0, A);
      BBF.Board.Delay_Controller.Delay_Milliseconds (Wait);
      Hexapod.Hardware.Servo_Controller_Left.Set_Something (1, B);
   end Move;

   ----------
   -- Move --
   ----------

   procedure Move
     (D_X : Reals.Real;
      D_Y : Reals.Real;
      D_Z : Reals.Real)
   is
      Desired_Position : Kinematics.Position;
      Posture          : Kinematics.Posture;
      Success          : Boolean;

   begin
      Kinematics.Set
        (Desired_Position,
         Kinematics.X (Position) + D_X,
         Kinematics.Y (Position) + D_Y,
         Kinematics.Z (Position) + D_Z);

      Console.Put
        ("Desired: "
         & Coordinate_Image (Kinematics.X (Desired_Position))
         & " "
         & Coordinate_Image (Kinematics.Y (Desired_Position))
         & " "
         & Coordinate_Image (Kinematics.Z (Desired_Position)));

      Kinematics.Inverse.Geometric.RF_Solve
        (Desired_Position, Posture, Success);

      if Success then
         Console.New_Line;
         --  Position := Desired_Position;
         Position := Kinematics.Forward.RF_E_Position (Posture);

         Console.Put_Line
           ("Found  : "
            & Coordinate_Image (Kinematics.X (Position))
            & " "
            & Coordinate_Image (Kinematics.Y (Position))
            & " "
            & Coordinate_Image (Kinematics.Z (Position)));
         Console.Put_Line
           ("Angles : "
            & Angle_Image (Kinematics.Theta_1 (Posture))
            & " "
            & Angle_Image (Kinematics.Theta_2 (Posture))
            & " "
            & Angle_Image (Kinematics.Theta_3 (Posture)));

         Move (Posture, 500);

      else
         Console.Put (" NO SOLUTION");
      end if;
   end Move;

   ----------
   -- Move --
   ----------

   procedure Move is
      From_Position : Kinematics.Position;
      To_Position   : Kinematics.Position;
      Posture       : Kinematics.Posture;
      Success       : Boolean;
   begin
      Kinematics.Set (From_Position, 0.087, -0.138, -0.082);
      Kinematics.Set (To_Position, 0.162, -0.138, -0.082);
      Kinematics.Inverse.Geometric.RF_Solve (From_Position, Posture, Success);
      Move (Posture, 2_000);
      BBF.Board.Delay_Controller.Delay_Milliseconds (5_000);

      if not Success then
         raise Program_Error;
      end if;

      for K in 1 .. 10 loop
         for J in 0 .. 100 loop
            Trajectory.Move_Swing
              (From         => From_Position,
               To           => To_Position,
               Current      => Posture,
               T            => 1.0 / 100.0 * Reals.Real (J),
               New_Posture  => Posture);

            --  Console.Put_Line
            --    ("Angles : "
            --     & Angle_Image (Kinematics.Theta_1 (Posture))
            --     & " "
            --     & Angle_Image (Kinematics.Theta_2 (Posture))
            --     & " "
            --     & Angle_Image (Kinematics.Theta_3 (Posture)));
            Move (Posture, 3);
         end loop;

         for J in 0 .. 100 loop
            Trajectory.Move_Support
              (From         => To_Position,
               To           => From_Position,
               Current      => Posture,
               T            => 1.0 / 100.0 * Reals.Real (J),
               New_Posture  => Posture);

            --  Console.Put_Line
            --    ("Angles : "
            --     & Angle_Image (Kinematics.Theta_1 (Posture))
            --     & " "
            --     & Angle_Image (Kinematics.Theta_2 (Posture))
            --     & " "
            --     & Angle_Image (Kinematics.Theta_3 (Posture)));
            Move (Posture, 3);
         end loop;
      end loop;
   end Move;

   ----------------
   -- Start_SMCs --
   ----------------

   procedure Start_SMCs is
      Posture : Kinematics.Posture;

   begin
      Kinematics.Set
        (Posture, 0.0, -Ada.Numerics.Pi / 6.0, Ada.Numerics.Pi * 4.0 / 6.0);
      Position := Kinematics.Forward.RF_E_Position (Posture);

      --  Kinematics.Set (Position, 0.311, -0.048, 0.0);
      Move (0.0, 0.0, 0.0);
   end Start_SMCs;

   C : Character;

begin
   Hexapod.Hardware.Initialize_Hardware;

   loop
      Console.New_Line;
      Console.Put ("Phoenix Hexapod CLI> ");
      Console.Get_Synchronous (C);
      Console.Put_Line ((1 => C));

      case C is
         when 'U' | 'u' =>
            Hexapod.Hardware.Configure_Controllers;
            Start_SMCs;

         when 'W' | 'w' =>
            Move (0.005, 0.000, 0.000);

         when 'S' | 's' =>
            Move (-0.005, 0.000, 0.000);

         when 'D' | 'd' =>
            Move (0.000, -0.005, 0.000);

         when 'A' | 'a' =>
            Move (0.000, 0.005, 0.000);

         when 'R' | 'r' =>
            Move (0.000, 0.000, 0.005);

         when 'F' | 'f' =>
            Move (0.000, 0.000, -0.005);

         when 'M' | 'm' =>
            Move;

         when others =>
            null;
      end case;
   end loop;
end Hexapod.Driver;
