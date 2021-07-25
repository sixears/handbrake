module HandBrake.Paths where

import FPath.AbsFile  ( AbsFile, absfile )

handbrakeCLI :: AbsFile
handbrakeCLI = [absfile|/nix/store/0a4557wgijwq1d7m54xfk53chhag4bya-handbrake-1.3.3/bin/HandBrakeCLI|]
