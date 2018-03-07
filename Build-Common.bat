@echo off

set target="sketchtroids.js"
set sourceDir="Source"
set generatedDir="Generated"
 
if exist %target% del /f %target%
hastec %sourceDir%/Main.hs -i%sourceDir% --outdir=%generatedDir% --out=%target% %*
if exist "%sourceDir%\*.hi" del /f /s "%sourceDir%\*.hi" 1>nul
if exist "%sourceDir%\*.o" del /f /s "%sourceDir%\*.o" 1>nul
pause