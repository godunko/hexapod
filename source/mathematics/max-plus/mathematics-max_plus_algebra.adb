--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

package body Mathematics.Max_Plus_Algebra is

   procedure Sum (Left : Matrix; Right : Matrix; Result : out Matrix)
     with Pre => Left'First (1) = Right'First (1)
                   and Left'Last (1) = Right'Last (1)
                   and Left'First (2) = Right'First (2)
                   and Left'Last (2) = Right'Last (2)
                   and Left'First (1) = Result'First (1)
                   and Left'Last (1) = Result'Last (1)
                   and Left'First (2) = Result'First (2)
                   and Left'Last (2) = Result'Last (2);

   procedure Product (Left : Matrix; Right : Matrix; Result : out Matrix)
     with Pre => Left'First (2) = Right'First (1)
                   and Left'Last (2) = Right'Last (1)
                   and Left'First (1) = Result'First (1)
                   and Left'Last (1) = Result'Last (1)
                   and Right'First (2) = Result'First (2)
                   and Right'Last (2) = Result'Last (2);

   procedure Set_Zero (Self : in out Matrix);

   procedure Set_Identity (Self : in out Matrix)
     with Pre => Self'First (1) = Self'First (2)
                   and Self'Length (1) = Self'Length (2);

   ---------
   -- "+" --
   ---------

   function "+" (Left : Max_Plus; Right : Max_Plus) return Max_Plus is
   begin
      return
        Max_Plus
          (Interfaces.IEEE_Float_32'Max
             (Interfaces.IEEE_Float_32 (Left),
              Interfaces.IEEE_Float_32 (Right)));
   end "+";

   ---------
   -- "*" --
   ---------

   function "*" (Left : Max_Plus; Right : Max_Plus) return Max_Plus is
      use type Interfaces.IEEE_Float_32;

   begin
      return
        Max_Plus
          (Interfaces.IEEE_Float_32 (Left)
             + Interfaces.IEEE_Float_32 (Right));
   end "*";

   ----------
   -- "**" --
   ----------

   function "**" (Left : Max_Plus; Right : Max_Plus) return Max_Plus is
      use type Interfaces.IEEE_Float_32;

   begin
      return
        Max_Plus
          (Interfaces.IEEE_Float_32 (Left)
             * Interfaces.IEEE_Float_32 (Right));
   end "**";

   ---------
   -- Get --
   ---------

   function Get
     (Self   : Matrix_6x6;
      Row    : Matrix_6x6_Row_Index;
      Column : Matrix_6x6_Column_Index) return Max_Plus is
   begin
      return Self (Row, Column);
   end Get;

   --------------
   -- Multiply --
   --------------

   procedure Multiply
     (Left : Max_Plus; Right : Matrix_6x6; Result : out Matrix_6x6) is
   begin
      for J in Right'Range (1) loop
         for K in Right'Range (2) loop
            Result (J, K) := Left * Right (J, K);
         end loop;
      end loop;
   end Multiply;

   --------------
   -- Multiply --
   --------------

   procedure Multiply
     (Left  : Interfaces.IEEE_Float_32;
      Right  : Matrix_6x6;
      Result : out Matrix_6x6) is
   begin
      Multiply (To_Max_Plus (Left), Right, Result);
   end Multiply;

   --------------
   -- Multiply --
   --------------

   procedure Multiply
     (Left : Matrix_6x6; Right : Max_Plus; Result : out Matrix_6x6) is
   begin
      for J in Left'Range (1) loop
         for K in Left'Range (2) loop
            Result (J, K) := Left (J, K) * Right;
         end loop;
      end loop;
   end Multiply;

   -----------------------
   -- Multiply_Ordinary --
   -----------------------

   procedure Multiply_Ordinary
     (Left : Max_Plus; Right : Vector_12; Result : out Vector_12) is
   begin
      Multiply_Ordinary (To_IEEE_Float_32 (Left), Right, Result);
   end Multiply_Ordinary;

   -----------------------
   -- Multiply_Ordinary --
   -----------------------

   procedure Multiply_Ordinary
     (Left   : Interfaces.IEEE_Float_32;
      Right  : Vector_12;
      Result : out Vector_12)
   is
      use type Interfaces.IEEE_Float_32;

   begin
      for J in Right'Range loop
         Result (J, 1) := Max_Plus (Left * To_IEEE_Float_32 (Right (J, 1)));
      end loop;
   end Multiply_Ordinary;

   -------------
   -- Product --
   -------------

   procedure Product
     (Left : Matrix; Right : Matrix; Result : out Matrix) is
   begin
      for I in Left'Range (1) loop
         for K in Right'Range (2) loop
            Result (I, K) := ε;

            for J in Left'Range (2) loop
               Result (I, K) := @ + (Left (I, J) * Right (J, K));
            end loop;
         end loop;
      end loop;
   end Product;

   -------------
   -- Product --
   -------------

   procedure Product
     (Left : Matrix_12x12; Right : Matrix_12x12; Result : out Matrix_12x12) is
   begin
      Product (Matrix (Left), Matrix (Right), Matrix (Result));
   end Product;

   -------------
   -- Product --
   -------------

   procedure Product
     (Left : Matrix_12x12; Right : Vector_12; Result : out Vector_12) is
   begin
      Product (Matrix (Left), Matrix (Right), Matrix (Result));
   end Product;

   -------------
   -- Product --
   -------------

   procedure Product
     (Left : Matrix_6x6; Right : Matrix_6x6; Result : out Matrix_6x6) is
   begin
      Product (Matrix (Left), Matrix (Right), Matrix (Result));
   end Product;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self   : in out Matrix_6x6;
      Row    : Matrix_6x6_Row_Index;
      Column : Matrix_6x6_Column_Index;
      Value  : Max_Plus) is
   begin
      Self (Row, Column) := Value;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self  : in out Vector_12;
      Row   : Vector_12_Row_Index;
      Value : Interfaces.IEEE_Float_32) is
   begin
      Self (Row, 1) := Max_Plus (Value);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self : in out Vector_12; Row : Vector_12_Row_Index; Value : Max_Plus) is
   begin
      Self (Row, 1) := Value;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Self : in out Vector_6; Row : Vector_6_Row_Index; Value : Max_Plus) is
   begin
      Self (Row, 1) := Value;
   end Set;

   ------------------
   -- Set_Identity --
   ------------------

   procedure Set_Identity (Self : in out Matrix) is
   begin
      Set_Zero (Self);

      for I in Self'Range (1) loop
         Self (I, I) := e;
      end loop;
   end Set_Identity;

   ------------------
   -- Set_Identity --
   ------------------

   procedure Set_Identity (Self : in out Matrix_12x12) is
   begin
      Set_Identity (Matrix (Self));
   end Set_Identity;

   ------------------
   -- Set_Identity --
   ------------------

   procedure Set_Identity (Self : in out Matrix_6x6) is
   begin
      Set_Identity (Matrix (Self));
   end Set_Identity;

   ---------------
   -- Set_Slice --
   ---------------

   procedure Set_Slice
     (Self        : in out Matrix_12x12;
      From_Row    : Matrix_12x12_Row_Index;
      From_Column : Matrix_12x12_Column_Index;
      Value       : Matrix_6x6) is
   begin
      for J in Value'Range (1) loop
         for K in Value'Range (2) loop
            Self (From_Row + J - 1, From_Column + K - 1) := Value (J, K);
         end loop;
      end loop;
   end Set_Slice;

   --------------
   -- Set_Zero --
   --------------

   procedure Set_Zero (Self : in out Matrix) is
   begin
      Self := (others => (others => ε));
   end Set_Zero;

   --------------
   -- Set_Zero --
   --------------

   procedure Set_Zero (Self : in out Matrix_12x12) is
   begin
      Set_Zero (Matrix (Self));
   end Set_Zero;

   --------------
   -- Set_Zero --
   --------------

   procedure Set_Zero (Self : in out Matrix_6x6) is
   begin
      Set_Zero (Matrix (Self));
   end Set_Zero;

   ---------
   -- Sum --
   ---------

   procedure Sum (Left : Matrix; Right : Matrix; Result : out Matrix) is
   begin
      for I in Left'Range (1) loop
         for J in Left'Range (2) loop
            Result (I, J) := Left (I, J) + Right (I, J);
         end loop;
      end loop;
   end Sum;

   ---------
   -- Sum --
   ---------

   procedure Sum
     (Left : Matrix_12x12; Right : Matrix_12x12; Result : out Matrix_12x12) is
   begin
      Sum (Matrix (Left), Matrix (Right), Matrix (Result));
   end Sum;

   ---------
   -- Sum --
   ---------

   procedure Sum
     (Left : Matrix_6x6; Right : Matrix_6x6; Result : out Matrix_6x6) is
   begin
      Sum (Matrix (Left), Matrix (Right), Matrix (Result));
   end Sum;

   -----------------
   -- To_Max_Plus --
   -----------------

   function To_Max_Plus (Item : Interfaces.IEEE_Float_32) return Max_Plus is
   begin
      return Max_Plus (Item);
   end To_Max_Plus;

   --  function To_Max_Plus (Image : Wide_Wide_String) return Max_Plus is
   --  begin
   --     return 0.0;
   --  end To_Max_Plus;

end Mathematics.Max_Plus_Algebra;
