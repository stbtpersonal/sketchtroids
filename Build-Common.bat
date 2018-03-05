@echo off

set target="sketchtroids.js"
set sourceDir="Source"
set generatedDir="Generated"
 
if exist %target% del /f %target%
hastec %sourceDir%/Main.hs -i%sourceDir% --outdir=%generatedDir% --out=%target% %*
if exist "%sourceDir%\*.hi" del /f "%sourceDir%\*.hi"
if exist "%sourceDir%\*.o" del /f "%sourceDir%\*.o"
pause