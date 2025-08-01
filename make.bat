@echo off
echo === Assembling main.asm ===
java -jar KickAss.jar main.asm -o main.prg
if %errorlevel% neq 0 (
    echo *** Error during assembly ***
    pause
    exit /b
)
echo === Done! Output: main.prg ===
pause