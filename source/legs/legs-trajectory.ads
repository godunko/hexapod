--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Generates legs gait on control loop tick.

--  pragma Restrictions (No_Elaboration_Code);

with CGK.Primitives.Circles_2D;
with CGK.Primitives.Points_2D;
private with CGK.Primitives.Transformations_2D;
with CGK.Primitives.Transformations_3D;
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

   function Remaining_Ticks
     (Self      : Trajectory_Information;
      Workspace : CGK.Primitives.Circles_2D.Circle_2D;
      Position  : CGK.Primitives.Points_2D.Point_2D) return Natural;
   --  Returns a number of the control loop cycle ticks, that is necessary to
   --  reach boundary of the workspace from the given point.

   function Anterior_Extreme_Position
     (Self : Trajectory_Information;
      Leg  : Leg_Index) return CGK.Primitives.Points_2D.Point_2D with Inline;
   --  Returns optimal anterior extreme position.

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

   function Get_Bodypath_Tick_Transformation
     (Self : Trajectory_Information)
      return CGK.Primitives.Transformations_3D.Transformation_3D;

   function Get_Velocity
     (Self : Trajectory_Information) return Kinematics.Velocity;
   --  Returns current velocity of the robot's body.

private

   type Leg_Trajectory_Information is record
      AEP : CGK.Primitives.Points_2D.Point_2D;
   end record;

   type Leg_Trajectory_Information_Array is
     array (Leg_Index) of Leg_Trajectory_Information;

   type Trajectory_Information is record
      Velocity              : Kinematics.Velocity;
      --  Absolute value of the robot's body velocity.

      Trajectory_Center     : CGK.Primitives.Points_2D.Point_2D;

      Tick_Transformation   :
        CGK.Primitives.Transformations_2D.Transformation_2D;
      Tick_Transformation_3 :
        CGK.Primitives.Transformations_3D.Transformation_3D;
      Leg_Information       : Leg_Trajectory_Information_Array;
   end record;

   function Get_Velocity
     (Self : Trajectory_Information) return Kinematics.Velocity
        is (Self.Velocity);

end Legs.Trajectory;
