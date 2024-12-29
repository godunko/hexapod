--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Logger for GUI application.

package Debug.Log.Loggers.GUI is

   procedure Initialize;

private

   type GUI_Logger is new Abstract_Logger with null record;

   overriding procedure Output (Self : in out GUI_Logger; Item : String);

end Debug.Log.Loggers.GUI;
