@echo off

set target="sketchtroids.js"
set sourceDir="Source"
set generatedDir="Generated"
 
if exist %target% del /f %target%
hastec %sourceDir%/Main.hs -i%sourceDir% --outdir=%generatedDir% --out=%target% %* || goto :error
del "%sourceDir%\*.hi"
del "%sourceDir%\*.o"
pause
goto :EOF

:error
pause