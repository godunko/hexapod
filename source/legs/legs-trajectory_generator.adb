--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;
--  pragma Restrictions (No_Elaboration_Code);

with CGK.Primitives.Vectors_3D;
with CGK.Reals.Elementary_Functions;

with Bodypath_Generators.Constant_Velocity.Builders;
with Footpath_Generators.Stance.Builders;
with Footpath_Generators.Swing.Builders;
with Legs.State;
with Legs.Workspace;

package body Legs.Trajectory_Generator is

   type Trajectory_State is record
      Active : access Footpath_Generators.Abstract_Footpath_Generator'Class;
      Stance : aliased Footpath_Generators.Stance.Stance_Footpath_Generator;
      Swing  : aliased Footpath_Generators.Swing.Swing_Footpath_Generator;
   end record;

   State    : array (Leg_Index) of Trajectory_State;
   Bodypath : aliased
     Bodypath_Generators.Constant_Velocity
       .Constant_Velocity_Bodypath_Generator;

   --------------------------------
   -- Accumulated_Transformation --
   --------------------------------

   procedure Accumulated_Transformation
     (X : in out CGK.Reals.Real;
      Y : in out CGK.Reals.Real;
      W : in out CGK.Reals.Real)
   is
      use type CGK.Reals.Real;

      P0              : CGK.Primitives.Points_3D.Point_3D :=
        CGK.Primitives.Points_3D.As_Point_3D (X => 0.0, Y => 0.0, Z => 0.0);
      P1              : CGK.Primitives.Points_3D.Point_3D :=
        CGK.Primitives.Points_3D.As_Point_3D (X => 1.0, Y => 0.0, Z => 0.0);
      V               : CGK.Primitives.Vectors_3D.Vector_3D;
      Transformation  : CGK.Primitives.Transformations_3D.Transformation_3D;

   begin
      --  Compute rotation vector

      Bodypath_Generators.Constant_Velocity.Transform (Bodypath, P0);
      Bodypath_Generators.Constant_Velocity.Transform (Bodypath, P1);

      V := CGK.Primitives.Vectors_3D.Create_Vector_3D (P0, P1);

      W :=
        @ + CGK.Reals.Elementary_Functions.Arctan
              (X => CGK.Primitives.Vectors_3D.X (V),
               Y => CGK.Primitives.Vectors_3D.Y (V));

      --  Compute coordinates offset

      --  CGK.Primitives.Transformations_2D.Set_Identity (Transformation);
      CGK.Primitives.Transformations_3D.Set_Rotation_Z (Transformation, -W);
      CGK.Primitives.Points_3D.Transform (P0, Transformation);

      X := @ + CGK.Primitives.Points_3D.X (P0);
      Y := @ + CGK.Primitives.Points_3D.Y (P0);
   end Accumulated_Transformation;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Success : Boolean;

   begin
      for Leg in Leg_Index loop
         Set_Stance (Leg);

         Standard.Legs.Workspace.Ground_Center
           (Leg, Legs.State.Legs (Leg).Configuration.Position);

         Standard.Legs.Inverse_Kinematics
           (Self             => Legs.State.Legs (Leg).Kinematics_Parameters,
            Desired_Position => Legs.State.Legs (Leg).Configuration.Position,
            Found_Posture    => Legs.State.Legs (Leg).Configuration.Posture,
            Success          => Success);

         if not Success then
            raise Program_Error;
         end if;
      end loop;
   end Initialize;

   ------------------
   -- Set_Bodypath --
   ------------------

   procedure Set_Bodypath
     (Transformation : CGK.Primitives.Transformations_3D.Transformation_3D)
   is
      Builder : Bodypath_Generators.Constant_Velocity.Builders
                  .Constant_Velocity_Bodypath_Generator_Builder;

   begin
      Bodypath_Generators.Constant_Velocity.Builders.Build
        (Self           => Builder,
         Transformation => Transformation);

      Bodypath :=
        Bodypath_Generators.Constant_Velocity.Builders.Generator (Builder);
   end Set_Bodypath;

   ----------------
   -- Set_Stance --
   ----------------

   procedure Set_Stance (Leg : Leg_Index) is
      Builder :
        Footpath_Generators.Stance.Builders.Stance_Footpath_Generator_Builder;

   begin
      Footpath_Generators.Stance.Builders.Build
        (Self     => Builder,
         Leg      => Legs.State.Legs (Leg)'Access,
         Bodypath => Bodypath'Access);

      State (Leg).Stance :=
        Footpath_Generators.Stance.Builders.Value (Builder);
      State (Leg).Active := State (Leg).Stance'Access;
   end Set_Stance;

   ---------------
   -- Set_Swing --
   ---------------

   procedure Set_Swing
     (Leg    : Leg_Index;
      AEP_X  : CGK.Reals.Real;
      AEP_Y  : CGK.Reals.Real;
      Height : CGK.Reals.Real)
   is
      Builder :
        Footpath_Generators.Swing.Builders.Swing_Footpath_Generator_Builder;

   begin
      Footpath_Generators.Swing.Builders.Build
        (Self   => Builder,
         Leg    => Legs.State.Legs (Leg)'Access,
         PEP_X  =>
           CGK.Primitives.Points_3D.X
             (Legs.State.Legs (Leg).Configuration.Position),
         PEP_Y  =>
           CGK.Primitives.Points_3D.Y
             (Legs.State.Legs (Leg).Configuration.Position),
         AEP_X  => AEP_X,
         AEP_Y  => AEP_Y,
         Base_Z =>
           CGK.Primitives.Points_3D.Z
             (Legs.State.Legs (Leg).Configuration.Position),
         Height => Height,
         Ticks  => 50);  --  XXX Must be configurable!
      State (Leg).Swing  := Footpath_Generators.Swing.Builders.Value (Builder);
      State (Leg).Active := State (Leg).Swing'Access;
   end Set_Swing;

   ----------
   -- Tick --
   ----------

   procedure Tick is
   begin
      for Leg in Leg_Index loop
         State (Leg).Active.Tick;
      end loop;
   end Tick;

end Legs.Trajectory_Generator;
