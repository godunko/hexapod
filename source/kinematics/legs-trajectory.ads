--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Generates legs gait on control loop tick.

--  pragma Restrictions (No_Elaboration_Code);

with CGK.Primitives.Points_2D;
private with CGK.Primitives.Transformations_2D;
with CGK.Reals;

package Legs.Trajectory
  with Preelaborate
is

   use type CGK.Reals.Real;

   type Velocity is record
      X : CGK.Reals.Real;
      Y : CGK.Reals.Real;
      W : CGK.Reals.Real;
   end record;

   type Trajectory_Information is private
     with Preelaborable_Initialization;

   procedure Initialize;

   --  procedure Print_Workspace;

   --  procedure Center
   --    (Body_Velocity : Velocity);
   --  procedure Tick;
   --  --  Computes gait and updates legs' trajectory when necessary.
   --
   --  procedure Set_Velocity
   --    (VX : CGK.Reals.Real;
   --     VY : CGK.Reals.Real);
   --  --  Sets desired velocity

   procedure Transform
     (Self  : Trajectory_Information;
      Point : in out CGK.Primitives.Points_2D.Point_2D);

   procedure Set_Relative_Velocity
     (Self       : out Trajectory_Information;
      Velocity_X : CGK.Reals.Real;
      Velocity_Y : CGK.Reals.Real;
      Velocity_W : CGK.Reals.Real)
     with Pre =>
       Velocity_X in -1.0 .. 1.0
         and Velocity_Y in -1.0 .. 1.0
         and Velocity_W in -1.0 .. 1.0;
   --  Compute trajectory by relative velocity.

private

   type Leg_Trajectory_Information is record
      AEP : CGK.Primitives.Points_2D.Point_2D;
   end record;

   type Leg_Trajectory_Information_Array is
     array (Leg_Index) of Leg_Trajectory_Information;

   type Trajectory_Information is record
   --     Trajectory_Center   : CGK.Primitives.Points_2D.Point_2D;
   --     Delta_Gamma         : CGK.Reals.Real;
      Tick_Transformation :
        CGK.Primitives.Transformations_2D.Transformation_2D;
      Leg_Information     : Leg_Trajectory_Information_Array;
   end record;

end Legs.Trajectory;
