@echo off
set "inputFile=output\base_soilchemistry-daily.txt"
set "inputFile2=output\grazed_soilchemistry-daily.txt"


set "inputFile3=output\base_physiology-daily.txt"
set "inputFile4=output\grazed_physiology-daily.txt"



set "outputFile=output\base_soilchemistry-daily.csv"
set "outputFile2=output\grazed_soilchemistry-daily.csv"

set "outputFile3=output\base_physiology-daily.csv"
set "outputFile4=output\grazed_physiology-daily.csv"

powershell -Command "(Get-Content '%inputFile%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile%'"
powershell -Command "(Get-Content '%inputFile2%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile2%'"

powershell -Command "(Get-Content '%inputFile3%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile3%'"
powershell -Command "(Get-Content '%inputFile4%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile4%'"



echo Conversion complete. Output saved to %outputFile%
pause