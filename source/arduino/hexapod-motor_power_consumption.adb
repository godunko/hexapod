--
--  Copyright (C) 2023-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with BBF.HPL.PMC;
with BBF.HRI.ADC;

with Hexapod.Console;

package body Hexapod.Motor_Power_Consumption is

   P_5           : constant := 5.0;
   P_3           : constant := 3.3;
   --  Sensor and CPU voltage.

   Left_R_1      : constant := 9.92;
   Left_R_2      : constant := 4.71;
   Right_R_1     : constant := 9.99;
   Right_R_2     : constant := 4.71;
   --  Resistive divider parameters for left and for right channels.

   Sensetivity   : constant := 0.185;  --  V/A

   Left_Channel      : constant := 6;
   Right_Channel     : constant := 7;
   Calibrate_Channel : constant := 5;

   function Current
     (Value : Integer;
      Ref   : Float;
      R_1   : Float;
      R_2   : Float) return Float;
   --  Convert ADC value into the current value.

   function Sensor_Output_Voltage
     (Value : Integer;
      Ref   : Float;
      R_1   : Float;
      R_2   : Float) return Float;
   --  Convert ADC value into the sensor output voltage.

   -------------
   -- Current --
   -------------

   function Current
     (Value : Integer;
      Ref   : Float;
      R_1   : Float;
      R_2   : Float) return Float is
   begin
      return
        (Sensor_Output_Voltage (Value, Ref, R_1, R_2) - (P_5 / 2.0))
           / Sensetivity;
   end Current;

   -----------
   -- Image --
   -----------

   function Image (Item : Float) return String is
   begin
      declare
         A : constant Float   := abs Item;
         X : constant Integer := Integer (A * 1_000.0);
         I : constant String  := Integer'Image (X / 1_000);
         F : constant String  := Integer'Image (X mod 1_000);
         B : String (1 .. 6) := " 0.000";

      begin
         if Item < 0.0 then
            B (1) := '-';
         end if;

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

      --  Enable_Channels

      BBF.HRI.ADC.ADC_Periph.CHER :=
        (CH     =>
           (As_Array => True,
            Arr      =>
             (Left_Channel | Right_Channel | Calibrate_Channel => True,
              others                                           => <>)),
         others => <>);
   end Initialize;

   ---------------------------
   -- Sensor_Output_Voltage --
   ---------------------------

   function Sensor_Output_Voltage
     (Value : Integer;
      Ref   : Float;
      R_1   : Float;
      R_2   : Float) return Float
   is
      Tick       : constant Float := Ref / 4096.0;
      Voltage    : constant Float := Float (Value) * Tick;
      Multiplier : constant Float := (R_1 + R_2) / R_1;

   begin
      return Voltage * Multiplier;
   end Sensor_Output_Voltage;

   ----------
   -- Step --
   ----------

   Started       : Boolean := False;
   Counter       : Natural := 0;
   Left_Min      : Integer := Integer'Last;
   Left_Max      : Integer := Integer'First;
   Right_Min     : Integer := Integer'Last;
   Right_Max     : Integer := Integer'First;
   Calibrate_Min : Integer := Integer'Last;
   Calibrate_Max : Integer := Integer'First;

   Calibrate_Value  : Integer := 0;
   Calibrate_Loaded : Boolean := True;
   Left_Value       : Integer := 2048;
   Left_Loaded      : Boolean := True;
   Right_Value      : Integer := 2048;
   Right_Loaded     : Boolean := True;

   procedure Step is
      Value : Integer;

   begin
      if not Left_Loaded
        and BBF.HRI.ADC.ADC_Periph.ISR.EOC.Arr (Left_Channel)
      then
         Left_Value :=
           Integer (BBF.HRI.ADC.ADC_Periph.CDR (Left_Channel).DATA);
         Left_Loaded := True;
      end if;

      if not Right_Loaded
        and BBF.HRI.ADC.ADC_Periph.ISR.EOC.Arr (Right_Channel)
      then
         Right_Value :=
           Integer (BBF.HRI.ADC.ADC_Periph.CDR (Right_Channel).DATA);
         Right_Loaded := True;
      end if;

      if not Calibrate_Loaded
        and BBF.HRI.ADC.ADC_Periph.ISR.EOC.Arr (Calibrate_Channel)
      then
         Calibrate_Value :=
           Integer (BBF.HRI.ADC.ADC_Periph.CDR (Calibrate_Channel).DATA);
         Calibrate_Loaded := True;
      end if;

      if Left_Loaded and Right_Loaded and Calibrate_Loaded then
         Left_Max      := Integer'Max (@, Left_Value);
         Right_Max     := Integer'Max (@, Right_Value);
         Calibrate_Max := Integer'Max (@, Calibrate_Value);

         Left_Min      := Integer'Min (@, Left_Value);
         Right_Min     := Integer'Min (@, Right_Value);
         Calibrate_Min := Integer'Min (@, Calibrate_Value);

         Left_Loaded      := False;
         Right_Loaded     := False;
         Calibrate_Loaded := False;

         BBF.HRI.ADC.ADC_Periph.CR := (START => True, others => <>);
      end if;

      if Counter mod 100 = 0 then
         Console.Put_Line
           ("Motor Power Consumption:"
            & " Left "
            & Integer'Image (Left_Min)
            &" .."
            & Integer'Image (Left_Max)
            & " ("
            & Image (Current (Left_Max, 3.3, Left_R_1, Left_R_2))
            & " .. "
            & Image (Current (Left_Min, 3.3, Left_R_1, Left_R_2))
            & " A)"
            & " | Right "
            & Integer'Image (Right_Min)
            &" .."
            & Integer'Image (Right_Max)
            & " ("
            & Image (Current (Right_Max, 3.3, Right_R_1, Right_R_2))
            & " .. "
            & Image (Current (Right_Min, 3.3, Right_R_1, Right_R_2))
            & " A)"
            & " | Calibrate "
            & Integer'Image (Calibrate_Min)
            & " .. "
            & Integer'Image (Calibrate_Max)
            & " / "
            & Image (Sensor_Output_Voltage (Left_Min, 3.3, Left_R_1, Left_R_2))
            & Image (Sensor_Output_Voltage (Left_Max, 3.3, Left_R_1, Left_R_2))
            & Image (Sensor_Output_Voltage (Right_Min, 3.3, Right_R_1, Right_R_2))
            & Image (Sensor_Output_Voltage (Right_Max, 3.3, Right_R_1, Right_R_2))
            );

         Left_Min      := Integer'Last;
         Left_Max      := Integer'First;
         Right_Min     := Integer'Last;
         Right_Max     := Integer'First;
         Calibrate_Min := Integer'Last;
         Calibrate_Max := Integer'First;
      end if;

      Counter := @ + 1;
   end Step;

end Hexapod.Motor_Power_Consumption;
