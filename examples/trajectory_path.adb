--
--  Copyright (C) 2024, Vadim Godunko
--
--  SPDX-License-Identifier: Apache-2.0
--

with Legs.Workspace;

procedure Trajectory_Path is
begin
   Legs.Initialize;
   Legs.Workspace.Compute (0.030);

end Trajectory_Path;
