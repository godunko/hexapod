--
--  Copyright (C) 2019-2023, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with BBF.PCA9685;

with Hexapod.Console;
with Hexapod.Hardware.All_PWM_Channels;

procedure Hexapod.Motor_Playground is

   use type BBF.PCA9685.Value_Type;

   type Side is (Left, Right);

   type Channel_Index is range 0 .. 15;

   Channel : constant
     array (Side, Channel_Index)
       of not null access BBF.PCA9685.PCA9685_Channel'Class :=
       (Left =>
          (0  => Hexapod.Hardware.All_PWM_Channels.L_Channel_00,
           1  => Hexapod.Hardware.All_PWM_Channels.L_Channel_01,
           2  => Hexapod.Hardware.All_PWM_Channels.L_Channel_02,
           3  => Hexapod.Hardware.All_PWM_Channels.L_Channel_03,

           4  => Hexapod.Hardware.All_PWM_Channels.L_Channel_04,
           5  => Hexapod.Hardware.All_PWM_Channels.L_Channel_05,
           6  => Hexapod.Hardware.All_PWM_Channels.L_Channel_06,
           7  => Hexapod.Hardware.All_PWM_Channels.L_Channel_07,

           8  => Hexapod.Hardware.All_PWM_Channels.L_Channel_08,
           9  => Hexapod.Hardware.All_PWM_Channels.L_Channel_09,
           10 => Hexapod.Hardware.All_PWM_Channels.L_Channel_10,
           11 => Hexapod.Hardware.All_PWM_Channels.L_Channel_11,

           12 => Hexapod.Hardware.All_PWM_Channels.L_Channel_12,
           13 => Hexapod.Hardware.All_PWM_Channels.L_Channel_13,
           14 => Hexapod.Hardware.All_PWM_Channels.L_Channel_14,
           15 => Hexapod.Hardware.All_PWM_Channels.L_Channel_15),

        Right =>
          (0  => Hexapod.Hardware.All_PWM_Channels.R_Channel_00,
           1  => Hexapod.Hardware.All_PWM_Channels.R_Channel_01,
           2  => Hexapod.Hardware.All_PWM_Channels.R_Channel_02,
           3  => Hexapod.Hardware.All_PWM_Channels.R_Channel_03,

           4  => Hexapod.Hardware.All_PWM_Channels.R_Channel_04,
           5  => Hexapod.Hardware.All_PWM_Channels.R_Channel_05,
           6  => Hexapod.Hardware.All_PWM_Channels.R_Channel_06,
           7  => Hexapod.Hardware.All_PWM_Channels.R_Channel_07,

           8  => Hexapod.Hardware.All_PWM_Channels.R_Channel_08,
           9  => Hexapod.Hardware.All_PWM_Channels.R_Channel_09,
           10 => Hexapod.Hardware.All_PWM_Channels.R_Channel_10,
           11 => Hexapod.Hardware.All_PWM_Channels.R_Channel_11,

           12 => Hexapod.Hardware.All_PWM_Channels.R_Channel_12,
           13 => Hexapod.Hardware.All_PWM_Channels.R_Channel_13,
           14 => Hexapod.Hardware.All_PWM_Channels.R_Channel_14,
           15 => Hexapod.Hardware.All_PWM_Channels.R_Channel_15));

   Command        : Character;
   Active_Side    : Side          := Left;
   Active_Channel : Channel_Index := Channel_Index'Last;
   Value          : BBF.PCA9685.Value_Type := 0;

   function Channel_Image return String;

   procedure Update;

   -------------------
   -- Channel_Image --
   -------------------

   function Channel_Image return String is
      Image : constant String := Channel_Index'Image (Active_Channel);

   begin
      return Result : String (1 .. 3) := "A00" do
         Result (1) :=
           (case Active_Side is when Left => 'L', when Right => 'R');
         Result (3 - Image'Length + 2 .. 3) :=
           Image (Image'First + 1 .. Image'Last);
      end return;
   end Channel_Image;

   ------------
   -- Update --
   ------------

   procedure Update is

      use type BBF.PCA9685.Tick_Duration_Type;

      Tick : constant BBF.PCA9685.Tick_Duration_Type :=
        Channel (Active_Side, Active_Channel).Tick_Duration;

   begin
      Hexapod.Console.Put_Line
        ("  PWM value:"
         & BBF.PCA9685.Value_Type'Image (Value)
         & " ("
         & BBF.PCA9685.Tick_Duration_Type'Image (Tick * Integer (Value))
         & " s of "
         & BBF.PCA9685.Tick_Duration_Type'Image (Tick * 4_096)
         & " s)");
      Channel (Active_Side, Active_Channel).Set (0, Value);
   end Update;

begin
   loop
      Hexapod.Console.New_Line;
      Hexapod.Console.Put
        ("Motor Playground CLI (" & Channel_Image & ")> ");

      Hexapod.Console.Get_Synchronous (Command);

      if Command >= ' ' then
         Hexapod.Console.Put_Line ((1 => Command));
      end if;

      case Command is
         when ASCII.ESC =>
            Hexapod.Hardware.Left_Servo_Controller.Off;
            Hexapod.Hardware.Right_Servo_Controller.Off;

            return;

         when '0' .. '9' =>
            Channel (Active_Side, Active_Channel).Off;
            Active_Channel :=
              Channel_Index (Character'Pos (Command) - Character'Pos ('0'));

         when 'A' .. 'F' =>
            Channel (Active_Side, Active_Channel).Off;
            Active_Channel :=
              Channel_Index
                (Character'Pos (Command) - Character'Pos ('A') + 10);

         when 'a' .. 'f' =>
            Channel (Active_Side, Active_Channel).Off;
            Active_Channel :=
              Channel_Index
                (Character'Pos (Command) - Character'Pos ('a') + 10);

         when 'L' | 'l' =>
            Channel (Active_Side, Active_Channel).Off;
            Active_Side := Left;

         when 'R' | 'r' =>
            Channel (Active_Side, Active_Channel).Off;
            Active_Side := Right;

         when 'N' | 'n' =>
            Value := 658;
            Update;

         when 'I' | 'i' =>
            Value := 1973;
            Update;

         when 'M' | 'm' =>
            Value := 3289;
            Update;

         when '-' =>
            Value := @ - 1;
            Update;

         when '_' =>
            Value := @ - 10;
            Update;

         when '=' =>
            Value := @ + 1;
            Update;

         when '+' =>
            Value := @ + 10;
            Update;

         when others =>
            null;
      end case;
   end loop;
end Hexapod.Motor_Playground;
