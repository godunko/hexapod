--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

--  Implementation of the debug logger to output information to the console.

with Debug.Log.Loggers;

package Debug.Log.Console is

   procedure Initialize;

private

   type Logger is new Debug.Log.Loggers.Abstract_Logger with null record;

   overriding procedure Output (Self : in out Logger; Item : String);

end Debug.Log.Console;
