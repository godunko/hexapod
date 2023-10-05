--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Ada.Numerics;

with BBF.Board;
with BBF.Drivers.PCA9685;

with Kinematics.Forward;
with Kinematics.Inverse.Geometric;
with Reals;

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
     (170, 1070, -Ada.Numerics.Pi, Ada.Numerics.Pi / 2.0);

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
     (D_X : Reals.Real;
      D_Y : Reals.Real;
      D_Z : Reals.Real)
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

      Desired_Position : Kinematics.Position;
      Posture          : Kinematics.Posture;
      Success          : Boolean;
      A                : BBF.Drivers.PCA9685.Value_Type;
      B                : BBF.Drivers.PCA9685.Value_Type;
      C                : BBF.Drivers.PCA9685.Value_Type;

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
         BBF.Board.Delay_Controller.Delay_Milliseconds (700);
         Hexapod.Hardware.Servo_Controller_Left.Set_Something (0, A);
         BBF.Board.Delay_Controller.Delay_Milliseconds (700);
         Hexapod.Hardware.Servo_Controller_Left.Set_Something (1, B);

      else
         Console.Put (" NO SOLUTION");
      end if;
   end Move;

   ----------------
   -- Start_SMCs --
   ----------------

   procedure Start_SMCs is
   begin
      --  Kinematics.Set (Posture, 0.0, 0.0, 0.0);
      --  Position := Kinematics.Forward.RF_E_Position (Posture);

      Kinematics.Set (Position, 0.311, -0.048, 0.0);
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
            Move (0.010, 0.000, 0.000);

         when 'S' | 's' =>
            Move (-0.010, 0.000, 0.000);

         when 'D' | 'd' =>
            Move (0.000, -0.010, 0.000);

         when 'A' | 'a' =>
            Move (0.000, 0.010, 0.000);

         when 'R' | 'r' =>
            Move (0.000, 0.000, 0.010);

         when 'F' | 'f' =>
            Move (0.000, 0.000, -0.010);

         when others =>
            null;
      end case;
   end loop;
end Hexapod.Driver;
