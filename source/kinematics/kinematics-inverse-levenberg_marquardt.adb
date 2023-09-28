--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Kinematics.Forward;

package body Kinematics.Inverse.Levenberg_Marquardt is

   Lambda : constant := 0.000_5;

   function "abs" (Self : Reals.Vectors_3.Vector_3) return Reals.Real;

   -----------
   -- "abs" --
   -----------

   function "abs" (Self : Reals.Vectors_3.Vector_3) return Reals.Real is
      use type Reals.Real;

   begin
      return
        Reals.Elementary_Functions.Sqrt
          (Self.M_1 * Self.M_1 + Self.M_2 * Self.M_2 + Self.M_3 * Self.M_3);
   end "abs";

   -----------
   -- Solve --
   -----------

   procedure Solve
     (Current_Position : Kinematics.Position;
      Current_Posture  : Kinematics.Posture;
      Desired_Position : Kinematics.Position;
      Found_Position   : out Kinematics.Position;
      Found_Posture    : out Kinematics.Posture;
      Success          : out Boolean;
      Count            : out Natural)
   is
      use type Reals.Matricies_3x3.Matrix_3x3;
      use type Reals.Real;
      use type Reals.Vectors_3.Vector_3;

      Step_Position : Kinematics.Position;
      Step_Posture  : Kinematics.Posture       := Current_Posture;
      Step_Error    : Reals.Vectors_3.Vector_3 :=
        Desired_Position.Value - Current_Position.Value;
      Step_Value    : Reals.Vectors_3.Vector_3;
      J             : Kinematics.Inverse.Jacobian_Matrix;
      J_T           : Reals.Matricies_3x3.Matrix_3x3;
      M             : Reals.Matricies_3x3.Matrix_3x3;
      LI            : Reals.Matricies_3x3.Matrix_3x3 :=
        Reals.Matricies_3x3.Scalar (Lambda);

   begin
      Success := False;
      Count   := 0;

      loop
         Kinematics.Inverse.LF_Jacobian (Step_Posture, J);
         Reals.Matricies_3x3.Transpose (J.Matrix, J_T);

         M := (Reals.Matricies_3x3.Inverse (J_T * J.Matrix + LI)) * J_T;

         Step_Value := M * Step_Error;

         Kinematics.Set
           (Step_Posture,
            Step_Posture.Theta.M_1 + Step_Value.M_1,
            Step_Posture.Theta.M_2 + Step_Value.M_2,
            Step_Posture.Theta.M_3 + Step_Value.M_3);

         Step_Position.Value :=
           Reals.Vectors_3D.As_Vector_3
             (Kinematics.Forward.LF_E_Position (Step_Posture));
         Step_Error          := Desired_Position.Value - Step_Position.Value;
         Count               := Count + 1;

         if abs Step_Error < 0.001 then
            Found_Posture  := Step_Posture;
            Found_Position := Step_Position;
            Success        := True;

            exit;

         elsif Count >= 10_000 then
            exit;
         end if;
      end loop;
   end Solve;

end Kinematics.Inverse.Levenberg_Marquardt;
