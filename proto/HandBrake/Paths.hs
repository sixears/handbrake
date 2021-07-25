module HandBrake.Paths where

import FPath.AbsFile  ( AbsFile, absfile )

handbrakeCLI :: AbsFile
handbrakeCLI = [absfile|__handbrake__/bin/HandBrakeCLI|]
