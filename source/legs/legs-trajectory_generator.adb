--
--  Copyright (C) 2024-2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;
--  pragma Restrictions (No_Elaboration_Code);

with Ada.Numerics;

with CGK.Primitives.Points_2D;

with Legs.State;
with Legs.Workspace;

package body Legs.Trajectory_Generator is

   use Standard.Legs.Trajectory;
   use CGK.Reals;

   subtype Step_Fase is CGK.Reals.Real range 0.0 .. 1.0;

   type Trajectory_Kind is (Linear, Swing);

   type Trajectory_State (Kind : Trajectory_Kind := Linear) is record
      case Kind is
         when Linear =>
            null;

         when Swing =>
            PEP_X  : Reals.Real;
            PEP_Y  : Reals.Real;
            --  Posterior extreme position: lift-off point
            AEP_X  : Reals.Real;
            AEP_Y  : Reals.Real;
            --  anterior extreme position: touch-down point

            Base_Z : Reals.Real;
            Height : Reals.Real;
            --  Height of the swing.

            Fase   : CGK.Reals.Real;
            D_Fase : Step_Fase;
      end case;
   end record;

   State : array (Leg_Index) of Trajectory_State;

   procedure Position_XYZ
     (Plan       : Trajectory_State;
      Trajectory : Trajectory_Information;
      Ratio      : CGK.Reals.Real;
      X          : in out Reals.Real;
      Y          : in out Reals.Real;
      Z          : in out Reals.Real);
   --  Compute coordinates of the leg from base base location, step
   --  description and fase of the step oscillator.
   --
   --  @param Ratio Ratio of the swing and stance speed.

   function T_XY_Swing
     (Ratio : CGK.Reals.Real;
      T     : Step_Fase) return Reals.Real
      with Pre => Ratio in 0.0 .. 1.0;

   function T_Z_Swing (T : Step_Fase) return CGK.Reals.Real;

   ------------------
   -- Position_XYZ --
   ------------------

   procedure Position_XYZ
     (Plan       : Trajectory_State;
      Trajectory : Trajectory_Information;
      Ratio      : CGK.Reals.Real;
      X          : in out Reals.Real;
      Y          : in out Reals.Real;
      Z          : in out Reals.Real)
   is
      Point : CGK.Primitives.Points_2D.Point_2D;

   begin
      if Plan.Kind = Linear then
         --  Strait line in the XY plane

         --  X := @ + Plan.D_X;
         --  Y := @ + Plan.D_Y;

         Point := CGK.Primitives.Points_2D.Create_Point_2D (X, Y);
         Transform (Trajectory, Point);
         X := CGK.Primitives.Points_2D.X (Point);
         Y := CGK.Primitives.Points_2D.Y (Point);

      else
         --  Swing of the leg

         declare
            T_XY : constant Reals.Real := T_XY_Swing (Ratio, Plan.Fase);

         begin
            X := Plan.PEP_X + (Plan.AEP_X - Plan.PEP_X) * (T_XY + 0.5);
            Y := Plan.PEP_Y + (Plan.AEP_Y - Plan.PEP_Y) * (T_XY + 0.5);
            Z := Plan.Base_Z + Plan.Height * T_Z_Swing (Plan.Fase);
         end;
      end if;
   end Position_XYZ;

   ----------------
   -- T_XY_Swing --
   ----------------

   function T_XY_Swing
     (Ratio : CGK.Reals.Real;
      T     : Step_Fase) return Reals.Real is
   begin
      return
        ((12.0 * T ** 5 - 30.0 * T ** 4 + 20.0 * T ** 3 - 2.0 * T) * Ratio
           + 12.0 * T ** 5 - 30.0 * T ** 4 + 20.0 * T ** 3 - 1.0) / 2.0;
   end T_XY_Swing;

   ---------------
   -- T_Z_Swing --
   ---------------

   function T_Z_Swing (T : Step_Fase) return Reals.Real is
      use Ada.Numerics;
      use Reals.Elementary_Functions;

   begin
      if T <= 0.5 then
         return -(Sin (4.0 * Pi * T) - 4.0 * Pi * T) / (2.0 * Pi);

      else
         return (Sin (4.0 * Pi * T) - 4.0 * Pi * T + 4.0 * Pi) / (2.0 * Pi);
      end if;
   end T_Z_Swing;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Success : Boolean;

   begin
      for Leg in Leg_Index loop
         State (Leg) := (Kind => Linear);

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
   begin
      State (Leg) := (Kind => Linear);
   end Set_Stance;

   ---------------
   -- Set_Swing --
   ---------------

   procedure Set_Swing
     (Leg    : Leg_Index;
      AEP_X  : CGK.Reals.Real;
      AEP_Y  : CGK.Reals.Real;
      Height : CGK.Reals.Real) is
   begin
      State (Leg) :=
        (Kind   => Swing,
         PEP_X  => Kinematics.X (Legs.State.Legs (Leg).Configuration.Position),
         PEP_Y  => Kinematics.Y (Legs.State.Legs (Leg).Configuration.Position),
         AEP_X  => AEP_X,
         AEP_Y  => AEP_Y,
         Base_Z => Kinematics.Z (Legs.State.Legs (Leg).Configuration.Position),
         Height => Height,
         Fase   => 0.0,
         D_Fase => 1.0 / 50.0);  --  XXX Must be configurable!
   end Set_Swing;

   ----------
   -- Tick --
   ----------

   procedure Tick is
      X, Y, Z : Real;
      Success : Boolean;

   begin
      for Leg in Leg_Index loop
         if State (Leg).Kind = Swing then
            --  Update fase in swing state.

            State (Leg).Fase := Real'Min (@ + State (Leg).D_Fase, 1.0);
         end if;

         X := Kinematics.X (Legs.State.Legs (Leg).Configuration.Position);
         Y := Kinematics.Y (Legs.State.Legs (Leg).Configuration.Position);
         Z := Kinematics.Z (Legs.State.Legs (Leg).Configuration.Position);

         Position_XYZ (State (Leg), Trajectory.all, 1.0, X, Y, Z);

         Kinematics.Set
           (Legs.State.Legs (Leg).Configuration.Position, X, Y, Z);

         Standard.Legs.Inverse_Kinematics
           (Self             => Legs.State.Legs (Leg).Kinematics_Parameters,
            Desired_Position => Legs.State.Legs (Leg).Configuration.Position,
            Found_Posture    => Legs.State.Legs (Leg).Configuration.Posture,
            Success          => Success);

         if not Success then
            raise Program_Error;
         end if;
      end loop;
   end Tick;

end Legs.Trajectory_Generator;
