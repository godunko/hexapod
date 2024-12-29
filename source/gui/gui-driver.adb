--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Debug.Log.Loggers.GUI;
with GUI.Main_Window;

procedure GUI.Driver is
begin
   Debug.Log.Loggers.GUI.Initialize;
   GUI.Main_Window.Initialize;
end GUI.Driver;
