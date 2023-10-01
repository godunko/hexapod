--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with System.Storage_Elements;

with BBF.Board;
with BBF.GPIO;

with Console;

package body Hexapod.Hardware is

   CHIP_FREQ_CPU_MAX : constant := 84_000_000;
   --  XXX Should be computed based on current settings of the chip

   LED : not null access BBF.GPIO.Pin'Class renames BBF.Board.Pin_13_LED;

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer);
   pragma Export (C, Last_Chance_Handler, "__gnat_last_chance_handler");
   pragma No_Return (Last_Chance_Handler);

   -------------------------
   -- Initialize_Hardware --
   -------------------------

   procedure Initialize_Hardware is
   begin
      --  First, turn off onboard LED (used by last chance handler)

      LED.Set_Direction (BBF.GPIO.Output);
      LED.Set (False);

      --  Second, initialize delay controller (used by last chance handler)

      BBF.Board.Initialize_Delay_Controller;

      --  Initialize console and output logo

      Console.Initialize (CHIP_FREQ_CPU_MAX);

      Console.New_Line;
      Console.Put_Line ("          /\ .---._");
      Console.Put_Line ("       /\/.-. /\ /\/\");
      Console.Put_Line ("     //\\oo //\\/\\\\");
      Console.Put_Line ("    //  /""/`---\\ \\""`-._");
      Console.Put_Line ("_.-'""           ""`-.`-.");
      Console.New_Line;
   end;

   -------------------------
   -- Last_Chance_Handler --
   -------------------------

   procedure Last_Chance_Handler (Msg : System.Address; Line : Integer) is
      J : System.Storage_Elements.Integer_Address :=
        System.Storage_Elements.To_Integer (Msg);
      L : constant String := Integer'Image (Line);

   begin
      Console.Put ("ADA EXCEPTION at ");

      loop
         declare
            use type System.Storage_Elements.Integer_Address;

            C : constant Character
              with Import,
                   Convention => Ada,
                   Address    => System.Storage_Elements.To_Address (J);
            S : constant String (1 .. 1) := (1 => C);

         begin
            exit when C = ASCII.NUL;

            Console.Put (S);

            J := @ + 1;
         end;
      end loop;

      Console.Put_Line (":" & L (L'First + 1 .. L'Last));

      loop
         LED.Set (False);
         BBF.Board.Delay_Controller.Delay_Milliseconds (500);

         LED.Set (True);
         BBF.Board.Delay_Controller.Delay_Milliseconds (500);
      end loop;
   end Last_Chance_Handler;

end Hexapod.Hardware;
