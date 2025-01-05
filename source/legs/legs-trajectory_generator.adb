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

   type Trajectory_Kind is (Stance, Swing);

   type Trajectory_State (Kind : Trajectory_Kind := Stance) is record
      case Kind is
         when Stance =>
            Stance : Footpath_Generators.Stance.Stance_Footpath_Generator;

         when Swing =>
            Swing  : Footpath_Generators.Swing.Swing_Footpath_Generator;
      end case;
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

      State (Leg) :=
        (Kind   => Stance,
         Stance => Footpath_Generators.Stance.Builders.Value (Builder));
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
         PEP_X  => Kinematics.X (Legs.State.Legs (Leg).Configuration.Position),
         PEP_Y  => Kinematics.Y (Legs.State.Legs (Leg).Configuration.Position),
         AEP_X  => AEP_X,
         AEP_Y  => AEP_Y,
         Base_Z => Kinematics.Z (Legs.State.Legs (Leg).Configuration.Position),
         Height => Height,
         Ticks  => 50);  --  XXX Must be configurable!
      State (Leg) :=
        (Kind  => Swing,
         Swing => Footpath_Generators.Swing.Builders.Value (Builder));
   end Set_Swing;

   ----------
   -- Tick --
   ----------

   procedure Tick is
   begin
      for Leg in Leg_Index loop
         case State (Leg).Kind is
            when Stance =>
               State (Leg).Stance.Tick;

            when Swing =>
               State (Leg).Swing.Tick;
         end case;
      end loop;
   end Tick;

end Legs.Trajectory_Generator;
