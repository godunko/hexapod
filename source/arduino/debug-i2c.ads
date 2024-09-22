--
--  Copyright (C) 2019-2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

pragma Restrictions (No_Elaboration_Code);

with BBF.I2C;

package Debug.I2C is

   pragma Preelaborate;

   procedure Dump (Data : BBF.I2C.Unsigned_8_Array);

end Debug.I2C;
