@echo off
set "inputFile=KE_Kapiti_climate_eddy.txt"

set "outputFile=KE_Kapiti_output_eddy\kapiti_climate_eddy.csv"



powershell -Command "(Get-Content '%inputFile%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile%'"



echo Conversion complete. Output saved to %outputFile%
pause