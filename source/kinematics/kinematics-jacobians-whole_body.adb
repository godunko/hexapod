--
--  Copyright (C) 2025, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

with CGK.Reals.Elementary_Functions;

package body Kinematics.Jacobians.Whole_Body is

   use type CGK.Reals.Real;

   procedure Matrix_Matrix_Solution
     (A, X : Matrix_18x18; MA, MB : out Matrix_18x18);

   procedure Compute_Jacobian
     (Posture : Kinematics.Posture;
      Compute : not null Compute_Leg_Jacobian;
      M_11    : out CGK.Reals.Real;
      M_12    : out CGK.Reals.Real;
      M_13    : out CGK.Reals.Real;
      M_21    : out CGK.Reals.Real;
      M_22    : out CGK.Reals.Real;
      M_23    : out CGK.Reals.Real;
      M_31    : out CGK.Reals.Real;
      M_32    : out CGK.Reals.Real;
      M_33    : out CGK.Reals.Real) with Inline_Always;

   Zero : constant := 0.0;
   One  : constant := 1.0;

   function Is_Non_Zero (X : CGK.Reals.Real'Base) return Boolean is (X /= 0.0);

   ---------
   -- "-" --
   ---------

   function "-" (Right : Vector_18) return Vector_18 is
     ([for J in Right'Range => -Right (J)]);

   ---------
   -- "*" --
   ---------

   function "*" (Left : Vector_18; Right : CGK.Reals.Real) return Vector_18 is
      ([for J in Left'Range => Left (J) * Right]);

   ---------------------
   -- Back_Substitute --
   ---------------------

   procedure Back_Substitute (M, N : in out Matrix_18x18) is
   --     pragma Assertion_Policy (Pre            => Ignore,
   --                              Post           => Ignore,
   --                              Ghost          => Ignore,
   --                              Loop_Invariant => Ignore,
   --                              Assert         => Ignore);
   --     pragma Assert (M'First (1) = N'First (1)
   --                      and then
   --                    M'Last  (1) = N'Last (1));

      procedure Sub_Row
        (M      : in out Matrix_18x18;
         Target : Integer;
         Source : Integer;
         Factor : CGK.Reals.Real);
   --     with
   --       Pre => Target in M'Range (1)
   --         and then Source in M'Range (1);
   --     --  Elementary row operation that subtracts Factor * M (Source, <>) from
   --     --  M (Target, <>)

      -------------
      -- Sub_Row --
      -------------

      procedure Sub_Row
        (M      : in out Matrix_18x18;
         Target : Integer;
         Source : Integer;
         Factor : CGK.Reals.Real)
      is
      begin
         for J in M'Range (2) loop
            M (Target, J) := M (Target, J) - Factor * M (Source, J);
         end loop;
      end Sub_Row;

      --  Local declarations

      Max_Col : Integer := M'Last (2);

   --  Start of processing for Back_Substitute

   begin
      Do_Rows : for Row in reverse M'Range (1) loop
   --
   --        pragma Loop_Invariant (Max_Col <= M'Last (2));
   --
         Find_Non_Zero : for Col in reverse M'First (2) .. Max_Col loop
            if Is_Non_Zero (M (Row, Col)) then

               --  Found first non-zero element, so subtract a multiple of this
               --  element  from all higher rows, to reduce all other elements
               --  in this column to zero.

               declare
                  --  We can't use a for loop, as we'd need to iterate to
                  --  Row - 1, but that expression will overflow if M'First
                  --  equals Integer'First, which is true for aggregates
                  --  without explicit bounds..

                  J  : Integer := M'First (1);
                  NZ : constant CGK.Reals.Real := M (Row, Col);

               begin
                  while J < Row loop
   --                    pragma Loop_Invariant (J in M'Range (1));

                     Sub_Row (N, J, Row, (M (J, Col) / NZ));
                     Sub_Row (M, J, Row, (M (J, Col) / NZ));
                     J := J + 1;
                  end loop;
               end;

               --  Avoid potential overflow in the subtraction below

               exit Do_Rows when Col = M'First (2);

               Max_Col := Col - 1;

               exit Find_Non_Zero;
            end if;
         end loop Find_Non_Zero;
      end loop Do_Rows;
   end Back_Substitute;

   ----------------------
   -- Compute_Jacobian --
   ----------------------

   procedure Compute_Jacobian
     (LF_Posture : Kinematics.Posture;
      LM_Posture : Kinematics.Posture;
      LH_Posture : Kinematics.Posture;
      RF_Posture : Kinematics.Posture;
      RM_Posture : Kinematics.Posture;
      RH_Posture : Kinematics.Posture;
      Result     : out Matrix_18x18)
   is
      Offset : Natural := 0;

   begin
      Result := (others => (others => 0.0));

      Compute_Jacobian
        (Posture => LF_Posture,
         Compute => LF'Access,
         M_11    => Result (Offset + 1, Offset + 1),
         M_12    => Result (Offset + 1, Offset + 2),
         M_13    => Result (Offset + 1, Offset + 3),
         M_21    => Result (Offset + 2, Offset + 1),
         M_22    => Result (Offset + 2, Offset + 2),
         M_23    => Result (Offset + 2, Offset + 3),
         M_31    => Result (Offset + 3, Offset + 1),
         M_32    => Result (Offset + 3, Offset + 2),
         M_33    => Result (Offset + 3, Offset + 3));
      Offset := @ + 3;

      Compute_Jacobian
        (Posture => LM_Posture,
         Compute => LM'Access,
         M_11    => Result (Offset + 1, Offset + 1),
         M_12    => Result (Offset + 1, Offset + 2),
         M_13    => Result (Offset + 1, Offset + 3),
         M_21    => Result (Offset + 2, Offset + 1),
         M_22    => Result (Offset + 2, Offset + 2),
         M_23    => Result (Offset + 2, Offset + 3),
         M_31    => Result (Offset + 3, Offset + 1),
         M_32    => Result (Offset + 3, Offset + 2),
         M_33    => Result (Offset + 3, Offset + 3));
      Offset := @ + 3;

      Compute_Jacobian
        (Posture => LH_Posture,
         Compute => LH'Access,
         M_11    => Result (Offset + 1, Offset + 1),
         M_12    => Result (Offset + 1, Offset + 2),
         M_13    => Result (Offset + 1, Offset + 3),
         M_21    => Result (Offset + 2, Offset + 1),
         M_22    => Result (Offset + 2, Offset + 2),
         M_23    => Result (Offset + 2, Offset + 3),
         M_31    => Result (Offset + 3, Offset + 1),
         M_32    => Result (Offset + 3, Offset + 2),
         M_33    => Result (Offset + 3, Offset + 3));
      Offset := @ + 3;

      Compute_Jacobian
        (Posture => RF_Posture,
         Compute => RF'Access,
         M_11    => Result (Offset + 1, Offset + 1),
         M_12    => Result (Offset + 1, Offset + 2),
         M_13    => Result (Offset + 1, Offset + 3),
         M_21    => Result (Offset + 2, Offset + 1),
         M_22    => Result (Offset + 2, Offset + 2),
         M_23    => Result (Offset + 2, Offset + 3),
         M_31    => Result (Offset + 3, Offset + 1),
         M_32    => Result (Offset + 3, Offset + 2),
         M_33    => Result (Offset + 3, Offset + 3));
      Offset := @ + 3;

      Compute_Jacobian
        (Posture => RM_Posture,
         Compute => RM'Access,
         M_11    => Result (Offset + 1, Offset + 1),
         M_12    => Result (Offset + 1, Offset + 2),
         M_13    => Result (Offset + 1, Offset + 3),
         M_21    => Result (Offset + 2, Offset + 1),
         M_22    => Result (Offset + 2, Offset + 2),
         M_23    => Result (Offset + 2, Offset + 3),
         M_31    => Result (Offset + 3, Offset + 1),
         M_32    => Result (Offset + 3, Offset + 2),
         M_33    => Result (Offset + 3, Offset + 3));
      Offset := @ + 3;

      Compute_Jacobian
        (Posture => RH_Posture,
         Compute => RH'Access,
         M_11    => Result (Offset + 1, Offset + 1),
         M_12    => Result (Offset + 1, Offset + 2),
         M_13    => Result (Offset + 1, Offset + 3),
         M_21    => Result (Offset + 2, Offset + 1),
         M_22    => Result (Offset + 2, Offset + 2),
         M_23    => Result (Offset + 2, Offset + 3),
         M_31    => Result (Offset + 3, Offset + 1),
         M_32    => Result (Offset + 3, Offset + 2),
         M_33    => Result (Offset + 3, Offset + 3));
      Offset := @ + 3;
   end Compute_Jacobian;

   ----------------------
   -- Compute_Jacobian --
   ----------------------

   procedure Compute_Jacobian
     (Posture : Kinematics.Posture;
      Compute : not null Compute_Leg_Jacobian;
      M_11    : out CGK.Reals.Real;
      M_12    : out CGK.Reals.Real;
      M_13    : out CGK.Reals.Real;
      M_21    : out CGK.Reals.Real;
      M_22    : out CGK.Reals.Real;
      M_23    : out CGK.Reals.Real;
      M_31    : out CGK.Reals.Real;
      M_32    : out CGK.Reals.Real;
      M_33    : out CGK.Reals.Real)
   is
      Theta_1 : constant CGK.Reals.Real := Kinematics.Theta_1 (Posture);
      Theta_2 : constant CGK.Reals.Real := Kinematics.Theta_2 (Posture);
      Theta_3 : constant CGK.Reals.Real := Kinematics.Theta_3 (Posture);

      Cos_Theta_1 : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Cos (Theta_1);
      Sin_Theta_1 : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Sin (Theta_1);
      Cos_Theta_2 : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Cos (Theta_2);
      Sin_Theta_2 : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Sin (Theta_2);
      Cos_Theta_3 : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Cos (Theta_3);
      Sin_Theta_3 : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Sin (Theta_3);

   begin
      Compute
        (Cos_Theta_1 => Cos_Theta_1,
         Sin_Theta_1 => Sin_Theta_1,
         Cos_Theta_2 => Cos_Theta_2,
         Sin_Theta_2 => Sin_Theta_2,
         Cos_Theta_3 => Cos_Theta_3,
         Sin_Theta_3 => Sin_Theta_3,
         M_11        => M_11,
         M_12        => M_12,
         M_13        => M_13,
         M_21        => M_21,
         M_22        => M_22,
         M_23        => M_23,
         M_31        => M_31,
         M_32        => M_32,
         M_33        => M_33);
   end Compute_Jacobian;

   -----------------------
   -- Forward_Eliminate --
   -----------------------

   procedure Forward_Eliminate
     (M   : in out Matrix_18x18;
      N   : in out Matrix_18x18;
      Det : out CGK.Reals.Real)
   is
   --     pragma Assertion_Policy (Pre            => Ignore,
   --                              Post           => Ignore,
   --                              Ghost          => Ignore,
   --                              Loop_Invariant => Ignore,
   --                              Assert         => Ignore);
   --     pragma Assert (M'First (1) = N'First (1)
   --                      and then
   --                    M'Last  (1) = N'Last (1));
   --
   --     --  The following are variations of the elementary matrix row operations:
   --     --  row switching, row multiplication and row addition. Because in this
   --     --  algorithm the addition factor is always a negated value, we chose to
   --     --  use  row subtraction instead. Similarly, instead of multiplying by
   --     --  a reciprocal, we divide.

      procedure Sub_Row
        (M      : in out Matrix_18x18;
         Target : Integer;
         Source : Integer;
         Factor : CGK.Reals.Real);
   --     with
   --       Pre => Target in M'Range (1)
   --         and then Source in M'Range (1);
      --  Subtrace Factor * M (Source, <>) from M (Target, <>)

      procedure Divide_Row
        (M, N  : in out Matrix_18x18;
         Row   : Integer;
         Scale : CGK.Reals.Real);
   --     with
   --       Pre => Row in M'Range (1)
   --         and then M'First (1) = N'First (1)
   --         and then M'Last (1) = N'Last (1)
   --         and then Scale /= Zero;
      --  Divide M (Row) and N (Row) by Scale, and update Det

      procedure Switch_Row
        (M, N  : in out Matrix_18x18;
         Row_1 : Integer;
         Row_2 : Integer);
   --     with
   --       Pre  => Row_1 in M'Range (1)
   --         and then Row_2 in M'Range (1)
   --         and then M'First (1) = N'First (1)
   --         and then M'Last (1) = N'Last (1),
   --       Post => (for all J in M'Range (2) =>
   --                  M (Row_1, J) = M'Old (Row_2, J)
   --                    and then M (Row_2, J) = M'Old (Row_1, J))
   --         and then (for all J in N'Range (2) =>
   --                     N (Row_1, J) = N'Old (Row_2, J)
   --                       and then N (Row_2, J) = N'Old (Row_1, J));
   --     --  Exchange M (Row_1) and N (Row_1) with M (Row_2) and N (Row_2),
      --  negating Det in the process.

      ----------------
      -- Divide_Row --
      ----------------

      procedure Divide_Row
        (M, N  : in out Matrix_18x18;
         Row   : Integer;
         Scale : CGK.Reals.Real)
      is
      begin
         Det := Det * Scale;

         for J in M'Range (2) loop
            M (Row, J) := M (Row, J) / Scale;
         end loop;

         for J in N'Range (2) loop
            N (Row, J) := N (Row, J) / Scale;
   --           pragma Annotate
   --             (CodePeer, False_Positive, "divide by zero", "Scale /= 0");
         end loop;
      end Divide_Row;

      -------------
      -- Sub_Row --
      -------------

      procedure Sub_Row
        (M      : in out Matrix_18x18;
         Target : Integer;
         Source : Integer;
         Factor : CGK.Reals.Real)
      is
      begin
         for J in M'Range (2) loop
            M (Target, J) := M (Target, J) - Factor * M (Source, J);
         end loop;
      end Sub_Row;

      ----------------
      -- Switch_Row --
      ----------------

      procedure Switch_Row
        (M, N  : in out Matrix_18x18;
         Row_1 : Integer;
         Row_2 : Integer)
      is
         procedure Swap (X, Y : in out CGK.Reals.Real);
   --        with
   --          Post => X = Y'Old and then Y = X'Old;
   --        --  Exchange the values of X and Y

         ----------
         -- Swap --
         ----------

         procedure Swap (X, Y : in out CGK.Reals.Real) is
            T : constant CGK.Reals.Real := X;
         begin
            X := Y;
            Y := T;
         end Swap;

      --  Start of processing for Switch_Row

      begin
         if Row_1 /= Row_2 then
            Det := Zero - Det;

            for J in M'Range (2) loop
               Swap (M (Row_1, J), M (Row_2, J));
   --              pragma Annotate
   --                (GNATprove, False_Positive,
   --                 "formal parameters ""X"" and ""Y"" might be aliased",
   --                 "Row_1 /= Row_2");
   --
   --              pragma Loop_Invariant
   --                (for all JJ in M'First (2) .. J =>
   --                   M (Row_1, JJ) = M'Loop_Entry (Row_2, JJ)
   --                     and then M (Row_2, JJ) = M'Loop_Entry (Row_1, JJ));
            end loop;

            for J in N'Range (2) loop
               Swap (N (Row_1, J), N (Row_2, J));
   --              pragma Annotate
   --                (GNATprove, False_Positive,
   --                 "formal parameters ""X"" and ""Y"" might be aliased",
   --                 "Row_1 /= Row_2");
   --
   --              pragma Loop_Invariant
   --                (for all JJ in N'First (2) .. J =>
   --                   N (Row_1, JJ) = N'Loop_Entry (Row_2, JJ)
   --                     and then N (Row_2, JJ) = N'Loop_Entry (Row_1, JJ));
            end loop;
         end if;
      end Switch_Row;

      --  Local declarations

      Row : Integer := M'First (1);

   --  Start of processing for Forward_Eliminate

   begin
      Det := One;

      for J in M'Range (2) loop
   --        pragma Loop_Invariant (Row >= M'First (1));
   --
         declare
            Max_Row : Integer := Row;
            Max_Abs : CGK.Reals.Real'Base := 0.0;

         begin
            --  Find best pivot in column J, starting in row Row

            for K in Row .. M'Last (1) loop
   --              pragma Loop_Invariant (Max_Row in M'Range (1));
   --              pragma Loop_Invariant
   --                (if Max_Abs /= 0.0 then Max_Abs = abs M (Max_Row, J));

               declare
                  New_Abs : constant CGK.Reals.Real'Base := abs M (K, J);
               begin
                  if Max_Abs < New_Abs then
                     Max_Abs := New_Abs;
                     Max_Row := K;
                  end if;
               end;
            end loop;

            if Max_Abs > 0.0 then
               Switch_Row (M, N, Row, Max_Row);

   --              pragma Assert (Max_Abs = abs M (Row, J));

               --  The temporaries below are necessary to force a copy of the
               --  value and avoid improper aliasing.

               declare
                  Scale : constant CGK.Reals.Real := M (Row, J);
               begin
                  Divide_Row (M, N, Row, Scale);
               end;

               for U in Row .. M'Last (1) when U /= Row loop
                  declare
                     Factor : constant CGK.Reals.Real := M (U, J);
                  begin
                     Sub_Row (N, U, Row, Factor);
                     Sub_Row (M, U, Row, Factor);
                  end;
               end loop;

               exit when Row >= M'Last (1);

               Row := Row + 1;

            else
               --  Set zero (note that we do not have literals)

               Det := Zero;
            end if;
         end;
      end loop;
   end Forward_Eliminate;

   -------------
   -- Inverse --
   -------------

   procedure Inverse (A : Matrix_18x18; I, MA, MB : out Matrix_18x18) is
   begin
      I := (others => (others => 0.0));

      for J in I'Range loop
         I (J, J) := 1.0;
      end loop;

      Matrix_Matrix_Solution (A, I, MA, MB);
   end Inverse;

   ---------
   -- JX1 --
   ---------

   procedure JX1
     (Alpha  : CGK.Reals.Real;
      Betta  : CGK.Reals.Real;
      Gamma  : CGK.Reals.Real;
      Result : out Matrix_18x6)
   is
      Cos_Alpha : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Cos (Alpha);
      Sin_Alpha : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Sin (Alpha);
      Cos_Betta : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Cos (Betta);
      Sin_Betta : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Sin (Betta);
      Cos_Gamma : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Cos (Gamma);
      Sin_Gamma : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Sin (Gamma);

       procedure Compute_Lambda
        (Base_X   : CGK.Reals.Real;
         Base_Y   : CGK.Reals.Real;
         Base_Z   : CGK.Reals.Real;
         Lambda_X : out CGK.Reals.Real;
         Lambda_Y : out CGK.Reals.Real;
         Lambda_Z : out CGK.Reals.Real);

      -------------
      -- Compute --
      -------------

      procedure Compute
        (Base_X   : CGK.Reals.Real;
         Base_Y   : CGK.Reals.Real;
         Base_Z   : CGK.Reals.Real;
         M_14     : out CGK.Reals.Real;
         M_15     : out CGK.Reals.Real;
         M_16     : out CGK.Reals.Real;
         M_24     : out CGK.Reals.Real;
         M_25     : out CGK.Reals.Real;
         M_26     : out CGK.Reals.Real;
         M_34     : out CGK.Reals.Real;
         M_35     : out CGK.Reals.Real;
         M_36     : out CGK.Reals.Real)
      is
         Lambda_X : CGK.Reals.Real;
         Lambda_Y : CGK.Reals.Real;
         Lambda_Z : CGK.Reals.Real;

      begin
         Compute_Lambda
           (Base_X   => Base_X,
            Base_Y   => Base_Y,
            Base_Z   => Base_Z,
            Lambda_X => Lambda_X,
            Lambda_Y => Lambda_Y,
            Lambda_Z => Lambda_Z);

         M_14 := 0.0;
         M_15 := -Lambda_Z;
         M_16 := Lambda_Y;

         M_24 := Lambda_Z;
         M_25 := 0.0;
         M_26 := -Lambda_X;

         M_34 := -Lambda_Y;
         M_35 := Lambda_X;
         M_36 := 0.0;
      end Compute;

      --------------------
      -- Compute_Lambda --
      --------------------

      procedure Compute_Lambda
        (Base_X   : CGK.Reals.Real;
         Base_Y   : CGK.Reals.Real;
         Base_Z   : CGK.Reals.Real;
         Lambda_X : out CGK.Reals.Real;
         Lambda_Y : out CGK.Reals.Real;
         Lambda_Z : out CGK.Reals.Real)
      is
      begin
         Lambda_X :=
           Base_Z*(Sin_Alpha*Sin_Gamma+Cos_Alpha*Sin_Betta*Cos_Gamma)
           +Base_Y*(Sin_Alpha*Sin_Betta*Cos_Gamma-Cos_Alpha*Sin_Gamma)
           +Base_X*cos_betta*Cos_Gamma;
         Lambda_Y :=
           Base_Y*(Sin_Alpha*Sin_Betta*Sin_Gamma+Cos_Alpha*Cos_Gamma)
           +Base_Z*(Cos_Alpha*Sin_Betta*Sin_Gamma-Sin_Alpha*Cos_Gamma)
           +Base_X*Cos_Betta*Sin_Gamma;
         Lambda_Z :=
           -Base_X*Sin_Betta
           +Base_Y*Sin_Alpha*Cos_Betta
           +Base_Z*Cos_Alpha*Cos_Betta;
      end Compute_Lambda;

      Offset : Natural := 0;

   begin
      Result := (others => (others => 0.0));

      Result (Offset + 1, 1) := 1.0;
      Result (Offset + 2, 2) := 1.0;
      Result (Offset + 3, 3) := 1.0;

      Compute
        (Base_X => Kinematics.Configuration.LF_Base_X,
         Base_Y => Kinematics.Configuration.LF_Base_Y,
         Base_Z => Kinematics.Configuration.LF_Base_Z,
         M_14   => Result (Offset + 1, 4),
         M_15   => Result (Offset + 1, 5),
         M_16   => Result (Offset + 1, 6),
         M_24   => Result (Offset + 2, 4),
         M_25   => Result (Offset + 2, 5),
         M_26   => Result (Offset + 2, 6),
         M_34   => Result (Offset + 3, 4),
         M_35   => Result (Offset + 3, 5),
         M_36   => Result (Offset + 3, 6));
      Offset := @ + 3;

      Result (Offset + 1, 1) := 1.0;
      Result (Offset + 2, 2) := 1.0;
      Result (Offset + 3, 3) := 1.0;

      Compute
        (Base_X => Kinematics.Configuration.LM_Base_X,
         Base_Y => Kinematics.Configuration.LM_Base_Y,
         Base_Z => Kinematics.Configuration.LM_Base_Z,
         M_14   => Result (Offset + 1, 4),
         M_15   => Result (Offset + 1, 5),
         M_16   => Result (Offset + 1, 6),
         M_24   => Result (Offset + 2, 4),
         M_25   => Result (Offset + 2, 5),
         M_26   => Result (Offset + 2, 6),
         M_34   => Result (Offset + 3, 4),
         M_35   => Result (Offset + 3, 5),
         M_36   => Result (Offset + 3, 6));
      Offset := @ + 3;

      Result (Offset + 1, 1) := 1.0;
      Result (Offset + 2, 2) := 1.0;
      Result (Offset + 3, 3) := 1.0;

      Compute
        (Base_X => Kinematics.Configuration.LH_Base_X,
         Base_Y => Kinematics.Configuration.LH_Base_Y,
         Base_Z => Kinematics.Configuration.LH_Base_Z,
         M_14   => Result (Offset + 1, 4),
         M_15   => Result (Offset + 1, 5),
         M_16   => Result (Offset + 1, 6),
         M_24   => Result (Offset + 2, 4),
         M_25   => Result (Offset + 2, 5),
         M_26   => Result (Offset + 2, 6),
         M_34   => Result (Offset + 3, 4),
         M_35   => Result (Offset + 3, 5),
         M_36   => Result (Offset + 3, 6));
      Offset := @ + 3;

      Result (Offset + 1, 1) := 1.0;
      Result (Offset + 2, 2) := 1.0;
      Result (Offset + 3, 3) := 1.0;

      Compute
        (Base_X => Kinematics.Configuration.RF_Base_X,
         Base_Y => Kinematics.Configuration.RF_Base_Y,
         Base_Z => Kinematics.Configuration.RF_Base_Z,
         M_14   => Result (Offset + 1, 4),
         M_15   => Result (Offset + 1, 5),
         M_16   => Result (Offset + 1, 6),
         M_24   => Result (Offset + 2, 4),
         M_25   => Result (Offset + 2, 5),
         M_26   => Result (Offset + 2, 6),
         M_34   => Result (Offset + 3, 4),
         M_35   => Result (Offset + 3, 5),
         M_36   => Result (Offset + 3, 6));
      Offset := @ + 3;

      Result (Offset + 1, 1) := 1.0;
      Result (Offset + 2, 2) := 1.0;
      Result (Offset + 3, 3) := 1.0;

      Compute
        (Base_X => Kinematics.Configuration.RM_Base_X,
         Base_Y => Kinematics.Configuration.RM_Base_Y,
         Base_Z => Kinematics.Configuration.RM_Base_Z,
         M_14   => Result (Offset + 1, 4),
         M_15   => Result (Offset + 1, 5),
         M_16   => Result (Offset + 1, 6),
         M_24   => Result (Offset + 2, 4),
         M_25   => Result (Offset + 2, 5),
         M_26   => Result (Offset + 2, 6),
         M_34   => Result (Offset + 3, 4),
         M_35   => Result (Offset + 3, 5),
         M_36   => Result (Offset + 3, 6));
      Offset := @ + 3;

      Result (Offset + 1, 1) := 1.0;
      Result (Offset + 2, 2) := 1.0;
      Result (Offset + 3, 3) := 1.0;

      Compute
        (Base_X => Kinematics.Configuration.RH_Base_X,
         Base_Y => Kinematics.Configuration.RH_Base_Y,
         Base_Z => Kinematics.Configuration.RH_Base_Z,
         M_14   => Result (Offset + 1, 4),
         M_15   => Result (Offset + 1, 5),
         M_16   => Result (Offset + 1, 6),
         M_24   => Result (Offset + 2, 4),
         M_25   => Result (Offset + 2, 5),
         M_26   => Result (Offset + 2, 6),
         M_34   => Result (Offset + 3, 4),
         M_35   => Result (Offset + 3, 5),
         M_36   => Result (Offset + 3, 6));
      Offset := @ + 3;
   end JX1;

   ---------
   -- JX2 --
   ---------

   procedure JX2
     (Betta  : CGK.Reals.Real;
      Gamma  : CGK.Reals.Real;
      Result : out Matrix_6x6)
   is
      --  Cos_Alpha : constant CGK.Reals.Real :=
      --    CGK.Reals.Elementary_Functions.Cos (Alpha);
      --  Sin_Alpha : constant CGK.Reals.Real :=
      --    CGK.Reals.Elementary_Functions.Sin (Alpha);
      Cos_Betta : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Cos (Betta);
      Sin_Betta : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Sin (Betta);
      Cos_Gamma : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Cos (Gamma);
      Sin_Gamma : constant CGK.Reals.Real :=
        CGK.Reals.Elementary_Functions.Sin (Gamma);

   begin
      Result := (others => (others => 0.0));

      Result (1, 1) := 1.0;
      Result (2, 2) := 1.0;
      Result (3, 3) := 1.0;

      Result (4, 4) := Cos_Betta * Cos_Gamma;
      Result (4, 5) := -Sin_Gamma;
      Result (4, 6) := 0.0;

      Result (5, 4) := Cos_Betta * Sin_Gamma;
      Result (5, 5) := Cos_Gamma;
      Result (5, 6) := 0.0;

      Result (6, 4) := -Sin_Betta;
      Result (6, 5) := 0.0;
      Result (6, 6) := 1.0;
   end JX2;

   ---------------------------
   -- Matrix_Matrix_Product --
   ---------------------------

   procedure Matrix_Matrix_Product
     (Left  : Matrix_18x6;
      Right : Matrix_6x6;
      R     : out Matrix_18x6)
   is
      --  pragma Assertion_Policy (Pre            => Ignore,
      --                           Post           => Ignore,
      --                           Ghost          => Ignore,
      --                           Loop_Invariant => Ignore,
      --                           Assert         => Ignore);
   begin
      --  return R : Result_Matrix (Left'Range (1), Right'Range (2))
      --    with Relaxed_Initialization
      --  do
         --  if Left'Length (2) /= Right'Length (1) then
         --     raise Constraint_Error with
         --       "incompatible dimensions in matrix multiplication";
         --  end if;
         --
         for J in R'Range (1) loop
            for K in R'Range (2) loop
               declare
                  S : CGK.Reals.Real := Zero;

               begin
                  for M in Left'Range (2) loop
                     S := S + Left (J, M) *
                                Right
                                  (M - Left'First (2) + Right'First (1), K);
                  end loop;

                  R (J, K) := S;
               end;
         --
         --        pragma Loop_Invariant
         --          (for all JJ in R'First (1) .. J when JJ /= J =>
         --             (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
         --        pragma Loop_Invariant
         --          (for all KK in R'First (2) .. K => R (J, KK)'Initialized);
            end loop;

         --     pragma Loop_Invariant
         --       (for all JJ in R'First (1) .. J when JJ /= J =>
         --          (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
         --     pragma Loop_Invariant
         --       (for all KK in R'Range (2) => R (J, KK)'Initialized);
         end loop;
      --  end return;
   end Matrix_Matrix_Product;

   ---------------------------
   -- Matrix_Matrix_Product --
   ---------------------------

   procedure Matrix_Matrix_Product
     (Left  : Matrix_18x18;
      Right : Matrix_18x6;
      R     : out Matrix_18x6)
   is
      --  pragma Assertion_Policy (Pre            => Ignore,
      --                           Post           => Ignore,
      --                           Ghost          => Ignore,
      --                           Loop_Invariant => Ignore,
      --                           Assert         => Ignore);
   begin
      --  return R : Result_Matrix (Left'Range (1), Right'Range (2))
      --    with Relaxed_Initialization
      --  do
         --  if Left'Length (2) /= Right'Length (1) then
         --     raise Constraint_Error with
         --       "incompatible dimensions in matrix multiplication";
         --  end if;
         --
         for J in R'Range (1) loop
            for K in R'Range (2) loop
               declare
                  S : CGK.Reals.Real := Zero;

               begin
                  for M in Left'Range (2) loop
                     S := S + Left (J, M) *
                                Right
                                  (M - Left'First (2) + Right'First (1), K);
                  end loop;

                  R (J, K) := S;
               end;
         --
         --        pragma Loop_Invariant
         --          (for all JJ in R'First (1) .. J when JJ /= J =>
         --             (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
         --        pragma Loop_Invariant
         --          (for all KK in R'First (2) .. K => R (J, KK)'Initialized);
            end loop;

         --     pragma Loop_Invariant
         --       (for all JJ in R'First (1) .. J when JJ /= J =>
         --          (for all KK in R'Range (2) => R (JJ, KK)'Initialized));
         --     pragma Loop_Invariant
         --       (for all KK in R'Range (2) => R (J, KK)'Initialized);
         end loop;
      --  end return;
   end Matrix_Matrix_Product;

   ----------------------------
   -- Matrix_Matrix_Solution --
   ----------------------------

   procedure Matrix_Matrix_Solution
     (A, X : Matrix_18x18; MA, MB : out Matrix_18x18)
   is
   --     --  pragma Assertion_Policy (Pre            => Ignore,
   --     --                           Post           => Ignore,
   --     --                           Ghost          => Ignore,
   --     --                           Loop_Invariant => Ignore,
   --     --                           Assert         => Ignore);
   --     --
   --     --  procedure Ignore (M : Matrix)
   --     --  with
   --     --    Ghost,
   --     --    Depends => (null => M);
   --     --
   --     --  procedure Ignore (M : Matrix) is null;
   --     --  --  Ghost procedure to document that the value of argument M is ignored,
   --     --  --  which prevents a warning being issued about the value not being used
   --     --  --  in the rest of the code.

      --  N   : constant Natural := A'Length (1);
   --     MA  : Matrix (A'Range (2), A'Range (2)) with Relaxed_Initialization;
   --     MB  : Matrix (A'Range (2), X'Range (2)) with Relaxed_Initialization;
      Det : CGK.Reals.Real;

   begin
   --     if A'Length (2) /= N then
   --        raise Constraint_Error with "matrix is not square";
   --     end if;
   --
   --     if X'Length (1) /= N then
   --        raise Constraint_Error with "matrices have unequal number of rows";
   --     end if;
   --
      for J in 0 .. A'Length (1) - 1 loop
         for K in MA'Range (2) loop
            MA (MA'First (1) + J, K) := A (A'First (1) + J, K);

   --           pragma Loop_Invariant
   --             (for all JJ in MA'First (1) .. MA'First (1) + J
   --                when JJ /= MA'First (1) + J
   --              =>
   --                (for all KK in MA'Range (2) =>
   --                   MA (JJ, KK)'Initialized));
   --           pragma Loop_Invariant
   --             (for all KK in MA'First (2) .. K =>
   --                MA (MA'First (1) + J, KK)'Initialized);
         end loop;

         for K in MB'Range (2) loop
            MB (MB'First (1) + J, K) := X (X'First (1) + J, K);

   --           pragma Loop_Invariant
   --             (for all JJ in MB'First (1) .. MB'First (1) + J
   --                when JJ /= MB'First (1) + J
   --              =>
   --                (for all KK in MB'Range (2) =>
   --                   MB (JJ, KK)'Initialized));
   --           pragma Loop_Invariant
   --             (for all KK in MB'First (2) .. K =>
   --                MB (MB'First (1) + J, KK)'Initialized);
         end loop;

   --        pragma Loop_Invariant
   --          (for all JJ in MA'First (1) .. MA'First (1) + J =>
   --             (for all KK in MA'Range (2) =>
   --                MA (JJ, KK)'Initialized));
   --        pragma Loop_Invariant
   --          (for all JJ in MB'First (1) .. MB'First (1) + J =>
   --             (for all KK in MB'Range (2) =>
   --                MB (JJ, KK)'Initialized));
      end loop;

      Forward_Eliminate (MA, MB, Det);

      if Det = Zero then
         raise Constraint_Error with "matrix is singular";
   --        pragma Annotate
   --          (GNATprove, Intentional, "exception might be raised",
   --           "An exception should be raised on a singular matrix");
      end if;

      Back_Substitute (MA, MB);
   --     Ignore (MA);
   --
   --     return MB;
      null;
   end Matrix_Matrix_Solution;

   ---------------------------
   -- Matrix_Vector_Product --
   ---------------------------

   procedure Matrix_Vector_Product
     (Left  : Matrix_18x6;
      Right : Vector_6;
      R     : out Vector_18)
   is
   begin
      --  return R : Result_Vector (Left'Range (1)) do
      --     if Left'Length (2) /= Right'Length then
      --        raise Constraint_Error with
      --          "incompatible dimensions in matrix-vector multiplication";
      --     end if;

         for J in Left'Range (1) loop
            declare
               S : CGK.Reals.Real := Zero;

            begin
               for K in Left'Range (2) loop
                  S := S + Left (J, K)
                         * Right (K - Left'First (2) + Right'First);
               end loop;

               R (J) := S;
            end;
         end loop;
      --  end return;
   end Matrix_Vector_Product;

end Kinematics.Jacobians.Whole_Body;
