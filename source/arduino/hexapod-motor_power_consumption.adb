--
--  Copyright (C) 2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with BBF.HPL.PMC;
with BBF.HRI.ADC;

with Hexapod.Console;

package body Hexapod.Motor_Power_Consumption is

   P_5         : constant := 5.0;
   P_3         : constant := 3.3;
   R_1         : constant := 9.65;
   R_2         : constant := 4.70;
   Sensetivity : constant := 0.185;  --  V/A
   Channel     : constant := 7;

   function V (Value : Integer) return Float is
      T  : constant Float := P_3 / 4_096.0;
      TU : constant Float := T * (R_1 + R_2) / R_1;

   begin
      --  return TU * Float (Value);
      return ((P_5 / 2.0) - (TU * Float (Value))) / Sensetivity;
   end V;

   function Image (A : Float) return String is
   begin
      declare
         X : constant Integer := Integer (A * 1_000.0);
         I : constant String  := Integer'Image (X / 1_000);
         F : constant String  := Integer'Image (X mod 1_000);
         B : String (1 .. 6) := " 0.000";

      begin
         B (2 - (I'Length - 1) + 1 .. 2) := I (I'First + 1 .. I'Last);
         B (6 - (F'Length - 1) + 1 .. 6) := F (F'First + 1 .. F'Last);

         return B;
      end;

   exception
      when Constraint_Error =>
         return " X.XXX";
   end Image;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      BBF.HPL.PMC.Enable_Peripheral_Clock
        (BBF.HPL.Analog_To_Digital_Converter_Controller);

      --  Reset ADC controller.

      BBF.HRI.ADC.ADC_Periph.CR := (SWRST => True, others => <>);

      --  Reset Mode Register.

      BBF.HRI.ADC.ADC_Periph.MR := (others => <>);

      --  Reset PDC transfer.

      BBF.HRI.ADC.ADC_Periph.PTCR := (RXTDIS | TXTDIS => True, others => <>);
      BBF.HRI.ADC.ADC_Periph.RCR  := (others => <>);
      BBF.HRI.ADC.ADC_Periph.RNCR := (others => <>);

      --  Configure Mode Register (prescale, fast wakeup: enable startup
      --  cycles, triggers: enable, selection).

      BBF.HRI.ADC.ADC_Periph.MR :=
        (TRGEN   => BBF.HRI.ADC.DIS,  --  No hardware triggers,
         PRESCAL => 0,
         others  => <>);

      --  Configure Mode Register (timings: transfer, settling, tracking)

      --  Enable_Channel

      BBF.HRI.ADC.ADC_Periph.CHER :=
        (CH     => (As_Array => True, Arr => (7 => True, others => <>)),
         others => <>);
   end Initialize;

   ----------
   -- Step --
   ----------

   Started : Boolean := False;
   Counter : Natural := 0;
   Min     : Integer := Integer'Last;
   Max     : Integer := Integer'First;

   procedure Step is
      Value : Integer;
   begin
      if not Started then
         BBF.HRI.ADC.ADC_Periph.CR := (START => True, others => <>);
         Started := True;
      end if;

      if BBF.HRI.ADC.ADC_Periph.ISR.EOC.Arr (7) then
         Value := Integer (BBF.HRI.ADC.ADC_Periph.CDR (7).DATA);
         Max   := Integer'Max (Max, Value);
         Min   := Integer'Min (Min, Value);
         BBF.HRI.ADC.ADC_Periph.CR := (START => True, others => <>);
      end if;

      if Counter mod 100 = 0 then
         Console.Put_Line
           ("Motor Power Consumption:"
            & Integer'Image (Min)
            &" .."
            & Integer'Image (Max)
            & " ("
            & Image (V (Max))
            & " .. "
            & Image (V (Min))
            & " A)");

         Min := Integer'Last;
         Max := Integer'First;
      end if;

      Counter := @ + 1;
   end Step;

end Hexapod.Motor_Power_Consumption;
