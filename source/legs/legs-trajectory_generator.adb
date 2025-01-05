--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;
--  pragma Restrictions (No_Elaboration_Code);

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

   State : array (Leg_Index) of Trajectory_State;

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

   ----------------
   -- Set_Stance --
   ----------------

   procedure Set_Stance (Leg : Leg_Index) is
      Builder :
        Footpath_Generators.Stance.Builders.Stance_Footpath_Generator_Builder;

   begin
      Footpath_Generators.Stance.Builders.Build
        (Self => Builder,
         Leg  => Legs.State.Legs (Leg)'Access);

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
