set echo off

C:\Users\hawkj\Documents\Github\L-DNDC\LandscapeDNDC\ldndc\ldndc.exe -c C:\Users\hawkj\.ldndc\ldndc.conf KE_Kapiti_eddy.ldndc


set "inputFile=KE_Kapiti_output_eddy\KE_Kapiti_soilchemistry-daily.txt"
set "inputFile2=KE_Kapiti_output_eddy\KE_Kapiti_physiology-daily.txt"
set "inputFile3=KE_Kapiti_output_eddy\KE_Kapiti_watercycle-daily.txt"


set "outputFile=KE_Kapiti_output_eddy\KE_Kapiti_soilchemistry-daily.csv"
set "outputFile2=KE_Kapiti_output_eddy\KE_Kapiti_physiology-daily.csv"
set "outputFile3=KE_Kapiti_output_eddy\KE_Kapiti_watercycle-daily.csv"


powershell -Command "(Get-Content '%inputFile%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile%'"
powershell -Command "(Get-Content '%inputFile2%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile2%'"
powershell -Command "(Get-Content '%inputFile3%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile3%'"








pause
