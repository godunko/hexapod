--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with Ada.Exceptions;
with Ada.Real_Time;

with CGK.Primitives.Points_2D;
with CGK.Primitives.Points_3D;
with CGK.Primitives.Transformations_2D;
with CGK.Primitives.Vectors_2D;
with CGK.Reals.Elementary_Functions;

with Debug.Log;
with Hexapod.Parameters.Control_Cycle;
with Legs.Gait_Generator;
with Legs.State;
with Legs.Trajectory;
with Legs.Trajectory_Generator;
with Legs.Workspace;

package body Simulation.Control_Loop is

   task Robot_Control_Loop is
      entry Start;
      entry Stop;
   end Robot_Control_Loop;

   protected Scene is

      function Get return GUI.Scene_States.Scene_Information;

      procedure Set (To : GUI.Scene_States.Scene_Information);

   private
      Value : GUI.Scene_States.Scene_Information;
   end Scene;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      Robot_Control_Loop.Stop;
   end Finalize;

   ---------------
   -- Get_Scene --
   ---------------

   function Get_Scene return GUI.Scene_States.Scene_Information is
   begin
      return Scene.Get;
   end Get_Scene;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   ------------------------
   -- Robot_Control_Loop --
   ------------------------

   task body Robot_Control_Loop is

      use type Ada.Real_Time.Time;
      use type CGK.Reals.Real;

      Scene     : GUI.Scene_States.Scene_Information;
      Next_Tick : Ada.Real_Time.Time;
      Aux_Base  : CGK.Primitives.Points_3D.Point_3D;

   begin
      loop
         select
            accept Start;

            Legs.Initialize;
            Legs.Workspace.Compute (Scene.Body_Height);
            Legs.Trajectory.Initialize;
            Legs.Trajectory_Generator.Initialize;
            Legs.Gait_Generator.Initialize;

         or
            accept Stop;

         or
            terminate;
         end select;

         Next_Tick := Ada.Real_Time.Clock;

         loop
            select
               accept Stop;
               exit;

            or
               delay until Next_Tick;
            end select;

            begin
               Legs.Trajectory_Generator.Tick;
               Legs.Gait_Generator.Tick;

            exception
               when E : others =>
                  Debug.Log.Put_Line
                    (Ada.Exceptions.Exception_Information (E));

                  exit;
            end;

            for J in Legs.Leg_Index loop
               --  Extract legs posture, compute position of joints and end
               --  effector.

               Scene.Legs (J).Posture := Legs.State.Posture (J);

               Legs.Forward_Kinematics
                 (Self     => Legs.Legs (J),
                  Posture  => Scene.Legs (J).Posture,
                  Base     => Aux_Base,
                  Joint_1  => Scene.Legs (J).Joint_1,
                  Joint_2  => Scene.Legs (J).Joint_2,
                  Joint_3  => Scene.Legs (J).Joint_3,
                  Effector => Scene.Legs (J).Effector);

               --  Extract support state

               Scene.Legs (J).Is_Support := Legs.Gait_Generator.Is_Support (J);

               --  Extract workspace shapes

               Scene.Legs (J).Workspace :=
                 Legs.Workspace.Get_Workspace_Shape (J);
            end loop;

            --  Compute movement

            declare
               P0              : CGK.Primitives.Points_2D.Point_2D :=
                 CGK.Primitives.Points_2D.Create_Point_2D (X => 0.0, Y => 0.0);
               P1              : CGK.Primitives.Points_2D.Point_2D :=
                 CGK.Primitives.Points_2D.Create_Point_2D (X => 1.0, Y => 0.0);
               V               : CGK.Primitives.Vectors_2D.Vector_2D;
               Transformation  :
               CGK.Primitives.Transformations_2D.Transformation_2D;

            begin
               --  Compute rotation vector

               Legs.Trajectory.Transform
                 (Legs.Trajectory_Generator.Trajectory.all, P0);
               Legs.Trajectory.Transform
                 (Legs.Trajectory_Generator.Trajectory.all, P1);

               V := CGK.Primitives.Vectors_2D.Create_Vector_2D (P0, P1);

               Scene.Ground_Rotate_Z :=
                 @ + CGK.Reals.Elementary_Functions.Arctan
                 (X => CGK.Primitives.Vectors_2D.X (V),
                  Y => CGK.Primitives.Vectors_2D.Y (V));

               --  Compute coordinates offset

               CGK.Primitives.Transformations_2D.Set_Identity (Transformation);
               CGK.Primitives.Transformations_2D.Rotate
                 (Transformation, -Scene.Ground_Rotate_Z);
               CGK.Primitives.Points_2D.Transform (P0, Transformation);

               Scene.Ground_Offset_X := @ + CGK.Primitives.Points_2D.X (P0);
               Scene.Ground_Offset_Y := @ + CGK.Primitives.Points_2D.Y (P0);

               if Scene.Ground_Offset_X < -0.1 then
                  Scene.Ground_Offset_X := @ + 0.1;

               elsif Scene.Ground_Offset_X > 0.1 then
                  Scene.Ground_Offset_X := @ - 0.1;
               end if;

               if Scene.Ground_Offset_Y < -0.1 then
                  Scene.Ground_Offset_Y := @ + 0.1;

               elsif Scene.Ground_Offset_Y > 0.1 then
                  Scene.Ground_Offset_Y := @ - 0.1;
               end if;
            end;

            Simulation.Control_Loop.Scene.Set (Scene);
            Next_Tick :=
              @ + Ada.Real_Time.To_Time_Span
              (Hexapod.Parameters.Control_Cycle.Tick_Duration);
         end loop;
      end loop;
   end Robot_Control_Loop;

   -----------
   -- Scene --
   -----------

   protected body Scene is

      ---------
      -- Get --
      ---------

      function Get return GUI.Scene_States.Scene_Information is
      begin
         return Value;
      end Get;

      ---------
      -- Set --
      ---------

      procedure Set (To : GUI.Scene_States.Scene_Information) is
      begin
         Value := To;
      end Set;

   end Scene;

   -----------
   -- Start --
   -----------

   procedure Start is
   begin
      select
         Robot_Control_Loop.Start;

      or
         delay 0.1;
      end select;
   end Start;

end Simulation.Control_Loop;
