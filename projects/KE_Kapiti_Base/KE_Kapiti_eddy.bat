set echo off

<<<<<<< HEAD
C:\Users\JHawkins\Documents\Github\Soil.C.modelling\Kapiti.soil.C\ldndc\ldndc.exe -c C:\Users\JHawkins\.ldndc\ldndc.conf KE_Kapiti_eddy.ldndc
=======
C:\Users\hawkj\Documents\Github\L-DNDC\Landscape-DNDC\ldndc\ldndc.exe -c C:\Users\hawkj\.ldndc\ldndc.conf KE_Kapiti_eddy.ldndc
>>>>>>> a98e8d105d5b3f0c7d6c4e274b304464a5e18abe


set "inputFile=KE_Kapiti_output_eddy\KE_Kapiti_soilchemistry-daily.txt"
set "inputFile2=KE_Kapiti_output_eddy\KE_Kapiti_physiology-daily.txt"
set "inputFile3=KE_Kapiti_output_eddy\KE_Kapiti_watercycle-daily.txt"
set "inputFile4=KE_Kapiti_climate_eddy_2019_336_onwards.txt"

set "outputFile=KE_Kapiti_output_eddy\KE_Kapiti_soilchemistry-daily.csv"
set "outputFile2=KE_Kapiti_output_eddy\KE_Kapiti_physiology-daily.csv"
set "outputFile3=KE_Kapiti_output_eddy\KE_Kapiti_watercycle-daily.csv"
set "outputFile4=KE_Kapiti_output_eddy\KE_Kapiti_climate_eddy_2019_336_onwards.csv"



powershell -Command "(Get-Content '%inputFile%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile%'"
powershell -Command "(Get-Content '%inputFile2%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile2%'"
powershell -Command "(Get-Content '%inputFile3%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile3%'"
powershell -Command "(Get-Content '%inputFile4%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile4%'"







pause
