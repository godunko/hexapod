--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  PlayStation2 communication driver.

with A0B.Callbacks;
with A0B.EXTI;
with A0B.PlayStation2_Controllers.Protocol;
with A0B.SPI;
private with A0B.Timer;
private with A0B.Types;

private package Hexapod.Remote_Control.PS2C is

   type Communication_Driver
     (SPI         : not null access A0B.SPI.SPI_Slave_Device'Class;
      Acknowledge : not null access A0B.EXTI.External_Interrupt_Line'Class)
       is tagged limited private;

   procedure Initialize (Self : in out Communication_Driver'Class);

   procedure Transfer
     (Self            : in out Communication_Driver'Class;
      Transmit_Buffer :
        A0B.PlayStation2_Controllers.Protocol.Communication_Buffer;
      Receive_Buffer  : out
        A0B.PlayStation2_Controllers.Protocol.Communication_Buffer;
      On_Completed    : A0B.Callbacks.Callback;
      Success         : in out Boolean);

private

   type States is
     (Initial,
      Start_Delay,
      Select_Transfer_Await,
      Command_Transfer_Await,
      Data_Transfer_Await,
      Last_Transfer_Await,
      Close_Delay);

   type Flags is array (A0B.Types.Unsigned_32 range 0 .. 31) of Boolean
     with Pack, Size => 32;

   type Communication_Driver
     (SPI         : not null access A0B.SPI.SPI_Slave_Device'Class;
      Acknowledge : not null access A0B.EXTI.External_Interrupt_Line'Class)
   is tagged limited record
      State               : States := Initial;
      Start               : aliased A0B.Timer.Timeout_Control_Block;
      Timeout             : aliased A0B.Timer.Timeout_Control_Block;
      Close               : aliased A0B.Timer.Timeout_Control_Block;
      Index               : A0B.Types.Unsigned_32;
      Select_Buffer       : aliased A0B.Types.Unsigned_8;
      Select_Received     : Boolean;
      Select_Acknowledged : Boolean;
      Buffer              : aliased
        A0B.PlayStation2_Controllers.Protocol.Communication_Buffer;
      Data_Received       : Flags;
      Data_Acknowledged   : Flags;
      Length              : A0B.Types.Unsigned_32;
      Failure             : Boolean := True;

      Transmit_Buffer     : access
        A0B.PlayStation2_Controllers.Protocol.Communication_Buffer;
      Receive_Buffer      : access
        A0B.PlayStation2_Controllers.Protocol.Communication_Buffer;
      On_Completed        : A0B.Callbacks.Callback;
   end record;

end Hexapod.Remote_Control.PS2C;
