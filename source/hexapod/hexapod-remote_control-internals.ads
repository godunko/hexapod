--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with A0B.PlayStation2_Controllers.Protocol;

private package Hexapod.Remote_Control.Internals is

   procedure Get_State
     (Buffer        :
        A0B.PlayStation2_Controllers.Protocol.Communication_Buffer;
      State : out A0B.PlayStation2_Controllers.Controller_State);

end Hexapod.Remote_Control.Internals;
