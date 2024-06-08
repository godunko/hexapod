--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with A0B.Types;

package body Hexapod.Remote_Control.Internals is

   ---------------
   -- Get_State --
   ---------------

   procedure Get_State
     (Buffer        :
        A0B.PlayStation2_Controllers.Protocol.Communication_Buffer;
      State : out A0B.PlayStation2_Controllers.Controller_State)
   is

      type Digital_Buttons_1 is record
         Select_Button          : Boolean;
         Left_Joystick          : Boolean;
         Right_Joystick         : Boolean;
         Start_Button           : Boolean;
         Up_Direction_Button    : Boolean;
         Right_Direction_Button : Boolean;
         Down_Direction_Button  : Boolean;
         Left_Direction_Button  : Boolean;
      end record with Pack, Size => 8;

      type Digital_Buttons_2 is record
         Left_2_Button   : Boolean;
         Right_2_Button  : Boolean;
         Left_1_Button   : Boolean;
         Right_1_Button  : Boolean;
         Triangle_Button : Boolean;
         Circle_Button   : Boolean;
         Cross_Button    : Boolean;
         Square_Button   : Boolean;
      end record with Pack, Size => 8;

      type Analog_Buttons is record
         Right_Joystick_Horizontal : A0B.Types.Unsigned_8;
         Right_Joystick_Vertical   : A0B.Types.Unsigned_8;
         Left_Joystick_Horizontal  : A0B.Types.Unsigned_8;
         Left_Joystick_Vertical    : A0B.Types.Unsigned_8;

         Right_Direction_Button    : A0B.Types.Unsigned_8;
         Left_Direction_Button     : A0B.Types.Unsigned_8;
         Up_Direction_Button       : A0B.Types.Unsigned_8;
         Down_Direction_Button     : A0B.Types.Unsigned_8;

         Triangle_Button           : A0B.Types.Unsigned_8;
         Circle_Button             : A0B.Types.Unsigned_8;
         Cross_Button              : A0B.Types.Unsigned_8;
         Square_Button             : A0B.Types.Unsigned_8;

         Left_1_Button             : A0B.Types.Unsigned_8;
         Right_1_Button            : A0B.Types.Unsigned_8;
         Left_2_Button             : A0B.Types.Unsigned_8;
         Right_2_Button            : A0B.Types.Unsigned_8;
      end record with Pack, Size => 128;

      --  Buffer        :
      --    A0B.PlayStation2_Controllers.Protocol.Communication_Buffer
      --      renames Receive_Buffer;
        --  renames Self.Buffer (Self.Communication_Buffer + 1);
   --     Configuration : Async.Configuration
   --       renames Self.Configuration (Self.Active_Configuration);

      Digital_1 : Digital_Buttons_1
        with Import, Address => Buffer (2)'Address;
      Digital_2 : Digital_Buttons_2
        with Import, Address => Buffer (3)'Address;
      Analog    : Analog_Buttons
        with Import, Address => Buffer (4)'Address;

   begin
      --  In digital mode, joystick position is not reported. Report of the
      --  middle position is forced.

      State.Right_Joystick_Horizontal := 16#80#;
      State.Right_Joystick_Vertical   := 16#80#;
      State.Left_Joystick_Horizontal  := 16#80#;
      State.Left_Joystick_Vertical    := 16#80#;

      --  Digital state of the buttons is send always. Some buttons doesn't
      --  have analog mode. So, decode digital state of the all buttons and
      --  overwrite values by analog state later, depending from the active
      --  configuration.

      State.Left_Direction_Button :=
        (if Digital_1.Left_Direction_Button then 16#00# else 16#FF#);
      State.Down_Direction_Button :=
        (if Digital_1.Down_Direction_Button then 16#00# else 16#FF#);
      State.Right_Direction_Button :=
        (if Digital_1.Right_Direction_Button then 16#00# else 16#FF#);
      State.Up_Direction_Button :=
        (if Digital_1.Up_Direction_Button then 16#00# else 16#FF#);
      State.Start_Button :=
        (if Digital_1.Start_Button then 16#00# else 16#FF#);
      State.Right_Joystick :=
        (if Digital_1.Right_Joystick then 16#00# else 16#FF#);
      State.Left_Joystick :=
        (if Digital_1.Left_Joystick then 16#00# else 16#FF#);
      State.Select_Button :=
        (if Digital_1.Select_Button then 16#00# else 16#FF#);

      State.Square_Button :=
        (if Digital_2.Square_Button then 16#00# else 16#FF#);
      State.Cross_Button :=
        (if Digital_2.Cross_Button then 16#00# else 16#FF#);
      State.Circle_Button :=
        (if Digital_2.Circle_Button then 16#00# else 16#FF#);
      State.Triangle_Button :=
        (if Digital_2.Triangle_Button then 16#00# else 16#FF#);
      State.Right_1_Button :=
        (if Digital_2.Right_1_Button then 16#00# else 16#FF#);
      State.Left_1_Button :=
        (if Digital_2.Left_1_Button then 16#00# else 16#FF#);
      State.Right_2_Button :=
        (if Digital_2.Right_2_Button then 16#00# else 16#FF#);
      State.Left_2_Button :=
        (if Digital_2.Left_2_Button then 16#00# else 16#FF#);

   --     if Configuration.Analog_Joysticks then
         State.Right_Joystick_Horizontal := Analog.Right_Joystick_Horizontal;
         State.Right_Joystick_Vertical   := Analog.Right_Joystick_Vertical;
         State.Left_Joystick_Horizontal  := Analog.Left_Joystick_Horizontal;
         State.Left_Joystick_Vertical    := Analog.Left_Joystick_Vertical;
   --
   --        if Configuration.Analog_Buttons then
   --           State.Right_Direction_Button := Analog.Right_Direction_Button;
   --           State.Left_Direction_Button  := Analog.Left_Direction_Button;
   --           State.Up_Direction_Button    := Analog.Up_Direction_Button;
   --           State.Down_Direction_Button  := Analog.Down_Direction_Button;
   --
   --           State.Triangle_Button        := Analog.Triangle_Button;
   --           State.Circle_Button          := Analog.Circle_Button;
   --           State.Cross_Button           := Analog.Cross_Button;
   --           State.Square_Button          := Analog.Square_Button;
   --
   --           State.Left_1_Button          := Analog.Left_1_Button;
   --           State.Right_1_Button         := Analog.Right_1_Button;
   --           State.Left_2_Button          := Analog.Left_2_Button;
   --           State.Right_2_Button         := Analog.Right_2_Button;
   --        end if;
   --     end if;
   end Get_State;

end Hexapod.Remote_Control.Internals;
