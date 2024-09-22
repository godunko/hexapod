--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

package Debug.Log.Loggers
  with Preelaborate
is

   type Abstract_Logger is abstract tagged limited null record;

   not overriding procedure Output
     (Self : in out Abstract_Logger; Item : String) is abstract;

end Debug.Log.Loggers;
