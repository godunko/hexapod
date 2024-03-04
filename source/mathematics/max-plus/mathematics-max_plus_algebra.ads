--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Max-Plus algebra.

pragma Restrictions (No_Elaboration_Code);

--  private with Ada.Strings.Text_Buffers;
private with Ada.Unchecked_Conversion;
with Interfaces;

package Mathematics.Max_Plus_Algebra is

   pragma Pure;

   type Max_Plus is private;
    --   with Real_Literal => To_Max_Plus;
          --  String_Literal => To_Max_Plus;

   ε, Zero       : constant Max_Plus;
  --   epsilon : Max_Plus renames ε;
   e, One       : constant Max_Plus;

  --   function To_Max_Plus (Image : String) return Max_Plus;

  --   function To_Max_Plus (Image : Wide_Wide_String) return Max_Plus;

   function To_Max_Plus (Item : Interfaces.IEEE_Float_32) return Max_Plus
     with Inline_Always;

   function To_IEEE_Float_32
     (Item : Max_Plus) return Interfaces.IEEE_Float_32
        with Pre => Item /= ε;

   function "+" (Left : Max_Plus; Right : Max_Plus) return Max_Plus;

   function "*" (Left : Max_Plus; Right : Max_Plus) return Max_Plus;

   function "**" (Left : Max_Plus; Right : Max_Plus) return Max_Plus;

   subtype Vector_6_Row_Index is Positive range 1 .. 6;

   type Vector_6 is private;

   procedure Set
     (Self : in out Vector_6; Row : Vector_6_Row_Index; Value : Max_Plus)
        with Inline;

   subtype Vector_12_Row_Index is Positive range 1 .. 12;

   type Vector_12 is private;

   procedure Set
     (Self : in out Vector_12; Row : Vector_12_Row_Index; Value : Max_Plus)
        with Inline;

   procedure Set
     (Self  : in out Vector_12;
      Row   : Vector_12_Row_Index;
      Value : Interfaces.IEEE_Float_32) with Inline;

   function Get
     (Self : Vector_12; Row : Vector_12_Row_Index) return Max_Plus;

   procedure Multiply_Ordinary
     (Left : Max_Plus; Right : Vector_12; Result : out Vector_12);

   procedure Multiply_Ordinary
     (Left   : Interfaces.IEEE_Float_32;
      Right  : Vector_12;
      Result : out Vector_12);

   subtype Matrix_6x6_Row_Index is Positive range 1 .. 6;
   subtype Matrix_6x6_Column_Index is Positive range 1 .. 6;

   type Matrix_6x6 is private;

   function Get
     (Self   : Matrix_6x6;
      Row    : Matrix_6x6_Row_Index;
      Column : Matrix_6x6_Column_Index) return Max_Plus with Inline;

   procedure Set
     (Self   : in out Matrix_6x6;
      Row    : Matrix_6x6_Row_Index;
      Column : Matrix_6x6_Column_Index;
      Value  : Max_Plus) with Inline;

   procedure Set_Zero (Self : in out Matrix_6x6);

   procedure Set_Identity (Self : in out Matrix_6x6);

   procedure Sum
     (Left : Matrix_6x6; Right : Matrix_6x6; Result : out Matrix_6x6);

   procedure Multiply
     (Left : Max_Plus; Right : Matrix_6x6; Result : out Matrix_6x6);

   procedure Multiply
     (Left  : Interfaces.IEEE_Float_32;
      Right  : Matrix_6x6;
      Result : out Matrix_6x6);

   procedure Multiply
     (Left : Matrix_6x6; Right : Max_Plus; Result : out Matrix_6x6);

   procedure Product
     (Left : Matrix_6x6; Right : Matrix_6x6; Result : out Matrix_6x6);

   subtype Matrix_12x12_Row_Index is Positive range 1 .. 12;
   subtype Matrix_12x12_Column_Index is Positive range 1 .. 12;

   type Matrix_12x12 is private;

   function Get
     (Self   : Matrix_12x12;
      Row    : Matrix_12x12_Row_Index;
      Column : Matrix_12x12_Column_Index) return Max_Plus with Inline;

   procedure Set_Zero (Self : in out Matrix_12x12);

   procedure Set_Identity (Self : in out Matrix_12x12);

   procedure Set_Slice
     (Self        : in out Matrix_12x12;
      From_Row    : Matrix_12x12_Row_Index;
      From_Column : Matrix_12x12_Column_Index;
      Value       : Matrix_6x6);

   procedure Sum
     (Left : Matrix_12x12; Right : Matrix_12x12; Result : out Matrix_12x12);

   procedure Product
     (Left : Matrix_12x12; Right : Matrix_12x12; Result : out Matrix_12x12);

   procedure Product
     (Left : Matrix_12x12; Right : Vector_12; Result : out Vector_12);

private

  --   procedure Put_Image
  --     (Buffer : in out Ada.Strings.Text_Buffers.Root_Buffer_Type'Class;
  --      Item   : Max_Plus);

   type Max_Plus is new Interfaces.IEEE_Float_32;
    --   with Put_Image => Put_Image;

   --  type Max_Plus_Real is record
   --     Value : Interfaces.IEEE_Float_32;
   --  end record;

   function To_Max_Plus is
     new Ada.Unchecked_Conversion (Interfaces.Unsigned_32, Max_Plus);

   ε, Zero : constant Max_Plus := To_Max_Plus (16#FF80_0000#);
   --  ε : constant Max_Plus_Real := (Value => Interfaces.IEEE_Float_32'First);
   e, One : constant Max_Plus := 0.0;
   --  e : constant Max_Plus_Real := (Value => 0.0);

   type Matrix is array (Positive range <>, Positive range <>) of Max_Plus;

   type Vector_6 is new Matrix (1 .. 6, 1 .. 1);

   type Vector_12 is new Matrix (1 .. 12, 1 .. 1);

   type Matrix_6x6 is new Matrix (1 .. 6, 1 .. 6);

   type Matrix_12x12 is new Matrix (1 .. 12, 1 .. 12);

   function Get
     (Self : Vector_12; Row : Vector_12_Row_Index) return Max_Plus is (Self (Row, 1));

   function Get
     (Self   : Matrix_12x12;
      Row    : Matrix_12x12_Row_Index;
      Column : Matrix_12x12_Column_Index) return Max_Plus is (Self (Row, Column));

   function To_IEEE_Float_32
     (Item : Max_Plus) return Interfaces.IEEE_Float_32 is
        (Interfaces.IEEE_Float_32 (Item));

end Mathematics.Max_Plus_Algebra;
