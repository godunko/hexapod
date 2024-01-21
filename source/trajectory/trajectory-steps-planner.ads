--
--  Copyright (C) 2023-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Reals;

package Trajectory.Steps.Planner is

   pragma Preelaborate;

   type Gait_Descriptor is private;

   Stop_Gait   : constant Gait_Descriptor;
   Wave_Gait   : constant Gait_Descriptor;
   Quadro_Gait : constant Gait_Descriptor;
   Tripod_Gait : constant Gait_Descriptor;

   procedure Compute_Step
     (Length_X : Reals.Real;
      Length_Y : Reals.Real;
      Height_Z : Reals.Real;
      Result   : out Step_Plan_Descriptor);

   procedure Transition (Gait : Gait_Descriptor);

private

   type Gait_Step_Index is new Natural;

   type Configuration is record
      L_1 : Integer;  --  LF
      L_2 : Integer;  --  RF
      L_3 : Integer;  --  LM
      L_4 : Integer;  --  RM
      L_5 : Integer;  --  LH
      L_6 : Integer;  --  RH
   end record;

   type Configuration_Array is
     array (Gait_Step_Index range <>) of Configuration;

   Support_Ticks : constant := 10;

   type Gait_Descriptor_Data (Last : Gait_Step_Index) is record
      Step  : Configuration_Array (0 .. Last);
      Ticks : Natural;
   end record
     with Dynamic_Predicate => Integer (Last) * Ticks = Support_Ticks;

   type Gait_Descriptor is
     not null access constant Gait_Descriptor_Data with Storage_Size => 0;

   Stop_Gait_Data   : aliased constant Gait_Descriptor_Data :=
     (Last  => 0,
      Step  => [(L_1 => 0, L_2 => 0, L_3 => 0, L_4 => 0, L_5 => 0, L_6 => 0)],
      Ticks => 0);

   Wave_Gait_Data   : aliased constant Gait_Descriptor_Data :=
     (Last  => 5,
      Step =>
        [(L_1 => -5, L_2 => -3, L_3 => -1, L_4 =>  1, L_5 =>  3, L_6 =>  5),
         (L_1 =>  5, L_2 => -5, L_3 => -3, L_4 => -1, L_5 =>  1, L_6 =>  3),
         (L_1 =>  3, L_2 =>  5, L_3 => -5, L_4 => -3, L_5 => -1, L_6 =>  1),
         (L_1 =>  1, L_2 =>  3, L_3 =>  5, L_4 => -5, L_5 => -3, L_6 => -1),
         (L_1 => -1, L_2 =>  1, L_3 =>  3, L_4 =>  5, L_5 => -5, L_6 => -3),
         (L_1 => -3, L_2 => -1, L_3 =>  1, L_4 =>  3, L_5 =>  5, L_6 => -5)],
      Ticks => 2);

   Quadro_Gait_Data : aliased constant Gait_Descriptor_Data :=
     (Last  => 2,
      Step  =>
        [(L_1 => -5, L_2 =>  5, L_3 =>  0, L_4 => -5, L_5 =>  5, L_6 =>  0),
         (L_1 =>  5, L_2 =>  0, L_3 => -5, L_4 =>  5, L_5 =>  0, L_6 => -5),
         (L_1 =>  0, L_2 => -5, L_3 =>  5, L_4 =>  0, L_5 => -5, L_6 =>  5)],
      Ticks => 5);
   Tripod_Gait_Data : aliased constant Gait_Descriptor_Data :=
     (Last  => 1,
      Step  =>
        [(L_1 => -5, L_2 =>  5, L_3 =>  5, L_4 => -5, L_5 => -5, L_6 =>  5),
         (L_1 =>  5, L_2 => -5, L_3 => -5, L_4 =>  5, L_5 =>  5, L_6 => -5)],
      Ticks => 10);

   Stop_Gait   : constant Gait_Descriptor := Stop_Gait_Data'Access;
   Wave_Gait   : constant Gait_Descriptor := Wave_Gait_Data'Access;
   Quadro_Gait : constant Gait_Descriptor := Quadro_Gait_Data'Access;
   Tripod_Gait : constant Gait_Descriptor := Tripod_Gait_Data'Access;

end Trajectory.Steps.Planner;
