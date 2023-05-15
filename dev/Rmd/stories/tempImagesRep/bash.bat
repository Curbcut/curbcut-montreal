@echo off
setlocal enabledelayedexpansion
set i=1
for %%f in (*.png) do (
  set "newname=Fig!i!.png"
  set /a i+=1
  ren "%%f" "!newname!"
)
