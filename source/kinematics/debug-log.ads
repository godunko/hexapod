--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Logging facility.

pragma Restrictions (No_Elaboration_Code);

limited with Debug.Log.Loggers;

package Debug.Log
  with Preelaborate
is

   procedure Put (Item : String);

   procedure Put_Line (Item : String);

   procedure New_Line;

private

   Logger : access Debug.Log.Loggers.Abstract_Logger'Class;

end Debug.Log;
