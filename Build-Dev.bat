@echo off

set target="sketchtroids.js"
 
if exist %target% del /f %target%
hastec Source/Main.hs --outdir=Generated --out=%target% || goto :error
del "Source\*.hi"
del "Source\*.o"
pause
goto :EOF

:error
pause