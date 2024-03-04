--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Ada_2022;

with Ada.Wide_Wide_Text_IO;        use Ada.Wide_Wide_Text_IO;
with Interfaces;                   use Interfaces;

with Mathematics.Max_Plus_Algebra; use Mathematics.Max_Plus_Algebra;

procedure Max_Plus_Test is

   package IEEE_Float_32_IO is
     new Ada.Wide_Wide_Text_IO.Float_IO (Interfaces.IEEE_Float_32);

   use IEEE_Float_32_IO;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Max_Plus) is
   begin
      if Item = ε then
         Put ("   ε   ");

      elsif Item = e then
         Put ("   e   ");

      else
         Put (To_IEEE_Float_32 (Item), Fore => 3, Aft => 3, Exp => 0);
      end if;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Matrix_6x6) is
   begin
      for J in Matrix_6x6_Row_Index loop
         for K in Matrix_6x6_Column_Index loop
            Put (Get (Item, J, K));
         end loop;

         New_Line;
      end loop;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Matrix_12x12) is
   begin
      for J in Matrix_12x12_Row_Index loop
         for K in Matrix_12x12_Column_Index loop
            Put (Get (Item, J, K));
         end loop;

         New_Line;
      end loop;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Item : Vector_12) is
   begin
      for J in Vector_12_Row_Index loop
         Put (Get (Item, J));
         New_Line;
      end loop;
   end Put;

   type Gait_Step_Sequence is
     array (Positive range <>, Positive range <>) of Positive;
   --  Each row is a set of legs that might be in swing phase at the same
   --  time.

   type Gait_Descriptor (Step_Count : Positive; Set_Size : Positive) is record
      Steps : Gait_Step_Sequence (1 .. Step_Count, 1 .. Set_Size);
      τ_δ   : IEEE_Float_32;
      --  "Double stance" time
      τ_f   : IEEE_Float_32;
      --  Swing (flight) time
      τ_g   : IEEE_Float_32;
      --  Stance (ground) time, without "double stance" time.
   end record;

   ---------------
   -- Build_P_Q --
   ---------------

   procedure Build_P_Q
     (Gait : Gait_Descriptor;
      --  τ_δ  : IEEE_Float_32;
      P    : out Matrix_6x6;
      Q    : out Matrix_6x6)
   is
      L_0 : Positive;
      L_1 : Positive;

   begin
      Set_Zero (P);

      --  for J in Gait.Steps'First (1) .. Gait.Steps'Last (1) - 1 loop
      for J in 1 .. Gait.Step_Count - 1 loop
         L_0 := J;
         L_1 := J + 1;

         --  for J_P in Gait.Steps'Range (2) loop
         for J_P in 1 .. Gait.Set_Size loop
            --  for J_Q in Gait'Range (2) loop
            for J_Q in 1 .. Gait.Set_Size loop
               Set
                 (P,
                  Gait.Steps (L_1, J_P),
                  Gait.Steps (L_0, J_Q),
                  To_Max_Plus (Gait.τ_δ));
            end loop;
         end loop;
      end loop;

      Set_Zero (Q);

      L_0 := 1;
      L_1 := Gait.Step_Count;

      for J_V in 1 .. Gait.Set_Size loop
         for J_W in 1 .. Gait.Set_Size loop
            Set
              (Q,
               Gait.Steps (L_0, J_V),
               Gait.Steps (L_1, J_W),
               To_Max_Plus (Gait.τ_δ));
         end loop;
      end loop;
   end Build_P_Q;

   procedure Compute_A_Star (A : Matrix_12x12; Result : out Matrix_12x12);
   --  Note, it use only 12 iterations, it is enough for the task and
   --  applied limitations.

   procedure Compute_A_Star (A : Matrix_12x12; Result : out Matrix_12x12) is
      E          : Matrix_12x12;
      Aux_Power  : Matrix_12x12 := A;
      Aux_Result : Matrix_12x12;
      Aux        : Matrix_12x12;

   begin
      Set_Identity (E);
      Sum (E, A, Aux_Result);

      for J in 2 .. 12 loop
         Product (Aux_Power, A, Aux);
         Sum (Aux_Result, Aux, Result);
         Aux_Power  := Aux;
         Aux_Result := Result;
      end loop;
   end Compute_A_Star;

   procedure Eigen (M : Matrix_12x12) is
      procedure Put (V, X : Vector_12) is
      begin
         for J in Vector_12_Row_Index loop
            Put (To_IEEE_Float_32 (Get (V, J)), Fore => 4, Aft => 3, Exp => 0);
            Put (To_IEEE_Float_32 (Get (X, J)), Fore => 4, Aft => 3, Exp => 0);
            Put (To_IEEE_Float_32 (Get (V, J)) - To_IEEE_Float_32 (Get (X, J)),
                 Fore => 4, Aft => 3, Exp => 0);
            New_Line;
         end loop;
      end Put;

      V   : Vector_12;
      Aux : array (1 .. 500) of Vector_12;
      L   : IEEE_Float_32;
      X   : Vector_12;
      E   : IEEE_Float_32;

   begin
      Set (V, 1, 0.0);
      Set (V, 2, 0.0);
      Set (V, 3, 0.0);
      Set (V, 4, 0.0);
      Set (V, 5, 0.0);
      Set (V, 6, 0.0);
      Set (V, 7, 0.0);
      Set (V, 8, 0.0);
      Set (V, 9, 0.0);
      Set (V, 10, 0.0);
      Set (V, 11, 0.0);
      Set (V, 12, 0.0);

      for J in Aux'Range loop
         Product (M, V, Aux (J));
         V := Aux (J);

         Put_Line ("----------------------" & Integer'Wide_Wide_Image (J));
         Put (V);
         --  New_Line;
         Put_Line ("----------------------");

         for K in Aux'First .. J - 1 loop
            L :=
            --    To_IEEE_Float_32 (Get (Aux (K), 1))
            --      / To_IEEE_Float_32 (Get (Aux (J), 1));
              To_IEEE_Float_32 (Get (Aux (J), 1))
                / To_IEEE_Float_32 (Get (Aux (K), 1));

            Multiply_Ordinary (L, Aux (K), X);

            Put (V, X);
            New_Line;

            E := 0.0;

            for J in Vector_12_Row_Index loop
               E := @ + abs (To_IEEE_Float_32 (Get (V, J)) - To_IEEE_Float_32 (Get (X, J)));
            end loop;

            if E < 0.9 then
               Put (Integer'Wide_Wide_Image (K) & Integer'Wide_Wide_Image (J));
               New_Line;
            --  if Get (X, 1) = Get (V, 1)
            --       and Get (X, 2) = Get (V, 2)
            --       and Get (X, 3) = Get (V, 3)
            --       and Get (X, 4) = Get (V, 4)
            --       and Get (X, 5) = Get (V, 5)
            --       and Get (X, 6) = Get (V, 6)
            --       and Get (X, 7) = Get (V, 7)
            --       and Get (X, 8) = Get (V, 8)
            --       and Get (X, 9) = Get (V, 9)
            --       and Get (X, 10) = Get (V, 10)
            --       and Get (X, 11) = Get (V, 11)
            --       and Get (X, 12) = Get (V, 12)
            --  then
               raise Program_Error;
            end if;
            null;
         end loop;
      end loop;
   end Eigen;

   procedure Eigen (Gait : Gait_Descriptor) is
      M_τ_f : Max_Plus := To_Max_Plus (Gait.τ_f);
      M_τ_δ : Max_Plus := To_Max_Plus (Gait.τ_δ);
      Aux   : Max_Plus;
      V     : Vector_12;

   begin
      null;

      for J in 1 .. Gait.Step_Count loop
         for K in 1 .. Gait.Set_Size loop
            Aux := (M_τ_f * M_τ_δ) ** To_Max_Plus (IEEE_Float_32 (J - 1));

            Set (V, Gait.Steps (J, K), M_τ_f * Aux);
            Set (V, Gait.Steps (J, K) + 6, Aux);
         end loop;
      end loop;

      Put (V);
      New_Line;
   end Eigen;

   --  PQ_Test_Gait : constant Gait_Descriptor (1 .. 3, 1 .. 2) :=
   --    ((1, 2), (3, 4), (5, 6));

   --  type

   Stop_Gait : constant Gait_Descriptor :=
    (Step_Count => 1,
     Set_Size   => 6,
     Steps      => [[1, 2, 3, 4, 5, 6]],
     τ_δ        => 0.0,
     τ_f        => 0.0,
     τ_g        => 1.0);

   Tripod_Test_Gait : constant Gait_Descriptor :=
     (Step_Count => 2,
      Set_Size   => 3,
      Steps      => [[1, 4, 5], [2, 3, 6]],
      τ_δ        => 0.0,
      τ_f        => 1.0,
      τ_g        => 1.0);
   --  Quad_Test_Gait : constant Gait_Descriptor (1 .. 3, 1 .. 2) :=
   --    ((1, 4, 5), (2, 3, 6));
   Wave_Test_Gait   : constant Gait_Descriptor :=
     (Step_Count => 6,
      Set_Size   => 1,
      Steps      => [[1], [2], [3], [4], [5], [6]],
      τ_δ        => 0.0,
      τ_f        => 1.0,
      τ_g        => 5.0);

   --  ε : Max_Plus_Real := "ϵ";
   --  A : Max_Plus := To_Max_Plus (2.0);
   --  C : Max_Plus with Volatile;
   --  D : Max_Plus with Volatile;

   --  Tripod
   --  τ_δ  : constant IEEE_Float_32 := 0.0;
   --  τ_f  : constant IEEE_Float_32 := 5.0;
   --  τ_g  : constant IEEE_Float_32 := 5.0;

   --  Wave
   τ_δ  : constant IEEE_Float_32 := 0.0;
   τ_f  : constant IEEE_Float_32 := 1.0;
   τ_g  : constant IEEE_Float_32 := 5.0;

   τ_f_E   : Matrix_6x6;
   τ_g_E   : Matrix_6x6;
   τ_g_E_Q : Matrix_6x6;
   E       : Matrix_6x6;
   P       : Matrix_6x6;
   Q       : Matrix_6x6;

   A        : Matrix_12x12;
   A_0      : Matrix_12x12;
   A_0_Star : Matrix_12x12;
   A_1      : Matrix_12x12;
   V_0      : Vector_12;
   V_P      : Vector_12;
   V_K      : array (1 .. 5) of Vector_12;

   L_0 : Wide_Wide_String := "123456789 123456789 123456789 123456789";
   L_1 : Wide_Wide_String (1 .. 200) := (others => '*');
   L_2 : Wide_Wide_String (1 .. 200) := (others => '*');
   L_3 : Wide_Wide_String (1 .. 200) := (others => '*');
   L_4 : Wide_Wide_String (1 .. 200) := (others => '*');
   L_5 : Wide_Wide_String (1 .. 200) := (others => '*');
   L_6 : Wide_Wide_String (1 .. 200) := (others => '*');

   Stance_1 : Boolean;
   Stance_2 : Boolean;
   Stance_3 : Boolean;
   Stance_4 : Boolean;
   Stance_5 : Boolean;
   Stance_6 : Boolean;
   K        : Natural := 0;
   M_C      : Natural := 0;
   M_P      : Positive := 1;

begin
   --  C := A + ε;
   --  D := A * ε;

   --  Put (A);
   --  New_Line;
   --  Put (ε);
   --  New_Line;
   --  Put (C);
   --  New_Line;
   --  Put (D);
   --  New_Line;

   --  Set_Zero (E);
   --  Put (E);
   --  New_Line;

   --  Set_Identity (E);
   --  Multiply (To_Max_Plus (5.0), E, E1);
   --  Put (E1);
   --  New_Line;
   --  Put (E);
   --  New_Line;

   Set_Identity (E);

   --  Build_P_Q (PQ_Test_Gait, τ_δ, P, Q);
   Build_P_Q (Wave_Test_Gait, P, Q);

   New_Line;
   Put (P);
   New_Line;
   Put (Q);
   New_Line;

   Multiply (τ_f, E, τ_f_E);
   --  Compute_A_Star (P, E1);
   --  Put (E1);
   --  New_Line;
   Set (τ_f_E, 1, 1, To_Max_Plus (0.0));
   Set (τ_f_E, 2, 2, To_Max_Plus (1.0));
   Set (τ_f_E, 3, 3, To_Max_Plus (2.0));
   Set (τ_f_E, 4, 4, To_Max_Plus (3.0));
   Set (τ_f_E, 5, 5, To_Max_Plus (4.0));
   Set (τ_f_E, 6, 6, To_Max_Plus (5.0));

   Set_Zero (A_0);
   Set_Slice (A_0, 1, 7, τ_f_E);
   Set_Slice (A_0, 7, 1, P);
   Put (A_0);
   New_Line;

   Compute_A_Star (A_0, A_0_Star);
   Put (A_0_Star);
   New_Line;

   Multiply (τ_g, E, τ_g_E);
   Put (τ_g_E);
   New_Line;
   Sum (τ_g_E, Q, τ_g_E_Q);
   Put (τ_g_E_Q);
   New_Line;

   Set_Identity (A_1);
   Set_Slice (A_1, 7, 1, τ_g_E_Q);
   Put (A_1);
   New_Line;

   Product (A_0_Star, A_1, A);
   Put (A);
   New_Line;

   Set (V_0, 1, 0.0);
   Set (V_0, 2, 0.0);
   Set (V_0, 3, 0.0);
   Set (V_0, 4, 0.0);
   Set (V_0, 5, 0.0);
   Set (V_0, 6, 0.0);
   Set (V_0, 7, 0.0);
   Set (V_0, 8, 0.0);
   Set (V_0, 9, 0.0);
   Set (V_0, 10, 0.0);
   Set (V_0, 11, 0.0);
   Set (V_0, 12, 0.0);

   Put (V_0);
   New_Line;

   V_P := V_0;

   Product (A, V_P, V_K (1));
   V_P := V_K (1);
   Put (V_P);
   New_Line;

   Multiply (τ_f, E, τ_f_E);

   Set_Zero (A_0);
   Set_Slice (A_0, 1, 7, τ_f_E);
   Set_Slice (A_0, 7, 1, P);

   Compute_A_Star (A_0, A_0_Star);

   Multiply (τ_g, E, τ_g_E);
   Sum (τ_g_E, Q, τ_g_E_Q);

   Set_Identity (A_1);
   Set_Slice (A_1, 7, 1, τ_g_E_Q);

   Product (A_0_Star, A_1, A);
   Put (A);
   New_Line;

   for K in 2 .. V_K'Last loop
      Product (A, V_P, V_K (K));
      V_P := V_K (K);
      Put (V_P);
      New_Line;
   end loop;

   V_P      := V_0;
   Stance_1 := True;
   Stance_2 := True;
   Stance_3 := True;
   Stance_4 := True;
   Stance_5 := True;
   Stance_6 := True;

   for J in V_K'Range loop
      M_C := Integer'Max (M_C, Integer (To_IEEE_Float_32 (Get (V_K (J), 1))));
      M_C := Integer'Max (M_C, Integer (To_IEEE_Float_32 (Get (V_K (J), 2))));
      M_C := Integer'Max (M_C, Integer (To_IEEE_Float_32 (Get (V_K (J), 3))));
      M_C := Integer'Max (M_C, Integer (To_IEEE_Float_32 (Get (V_K (J), 4))));
      M_C := Integer'Max (M_C, Integer (To_IEEE_Float_32 (Get (V_K (J), 5))));
      M_C := Integer'Max (M_C, Integer (To_IEEE_Float_32 (Get (V_K (J), 6))));
      M_C := Integer'Max (M_C, Integer (To_IEEE_Float_32 (Get (V_K (J), 7))));
      M_C := Integer'Max (M_C, Integer (To_IEEE_Float_32 (Get (V_K (J), 8))));
      M_C := Integer'Max (M_C, Integer (To_IEEE_Float_32 (Get (V_K (J), 9))));
      M_C := Integer'Max (M_C, Integer (To_IEEE_Float_32 (Get (V_K (J), 10))));
      M_C := Integer'Max (M_C, Integer (To_IEEE_Float_32 (Get (V_K (J), 11))));
      M_C := Integer'Max (M_C, Integer (To_IEEE_Float_32 (Get (V_K (J), 12))));

      for M in M_P .. M_C loop
         if M = Integer (To_IEEE_Float_32 (Get (V_K (J), 7))) then
            Stance_1 := False;

         elsif M = Integer (To_IEEE_Float_32 (Get (V_K (J), 1))) then
            Stance_1 := True;
         end if;

         if M = Integer (To_IEEE_Float_32 (Get (V_K (J), 8))) then
            Stance_2 := False;

         elsif M = Integer (To_IEEE_Float_32 (Get (V_K (J), 2))) then
            Stance_2 := True;
         end if;

         if M = Integer (To_IEEE_Float_32 (Get (V_K (J), 9))) then
            Stance_3 := False;

         elsif M = Integer (To_IEEE_Float_32 (Get (V_K (J), 3))) then
            Stance_3 := True;
         end if;

         if M = Integer (To_IEEE_Float_32 (Get (V_K (J), 10))) then
            Stance_4 := False;

         elsif M = Integer (To_IEEE_Float_32 (Get (V_K (J), 4))) then
            Stance_4 := True;
         end if;

         if M = Integer (To_IEEE_Float_32 (Get (V_K (J), 11))) then
            Stance_5 := False;

         elsif M = Integer (To_IEEE_Float_32 (Get (V_K (J), 5))) then
            Stance_5 := True;
         end if;

         if M = Integer (To_IEEE_Float_32 (Get (V_K (J), 12))) then
            Stance_6 := False;

         elsif M = Integer (To_IEEE_Float_32 (Get (V_K (J), 6))) then
            Stance_6 := True;
         end if;

         L_1 (M) := (if Stance_1 then '_' else ' ');
         L_2 (M) := (if Stance_2 then '_' else ' ');
         L_3 (M) := (if Stance_3 then '_' else ' ');
         L_4 (M) := (if Stance_4 then '_' else ' ');
         L_5 (M) := (if Stance_5 then '_' else ' ');
         L_6 (M) := (if Stance_6 then '_' else ' ');
      end loop;

      M_P := M_C;
   end loop;

   Put_Line (L_0);
   Put_Line (L_1);
   Put_Line (L_2);
   Put_Line (L_3);
   Put_Line (L_4);
   Put_Line (L_5);
   Put_Line (L_6);

   --  Eigen (A);

   Eigen (Stop_Gait);
   Eigen (Wave_Test_Gait);
   --  Eigen (0, 0);
end Max_Plus_Test;