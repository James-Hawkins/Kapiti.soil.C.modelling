set echo off


# CG Laptop
C:\Users\JHawkins\Documents\Github\Soil.C.modelling\Kapiti.soil.C\ldndc\ldndc.exe -c C:\Users\JHawkins\.ldndc\ldndc.conf KE_Kapiti_eddy.ldndc

# Personal laptop
#C:\Users\hawkj\Documents\Github\L-DNDC\Landscape-DNDC\ldndc\ldndc.exe -c C:\Users\hawkj\.ldndc\ldndc.conf KE_Kapiti_eddy.ldndc

set "inputFile=KE_Kapiti_output_eddy\KE_Kapiti_soilchemistry-daily.txt"
set "inputFile2=KE_Kapiti_output_eddy\KE_Kapiti_physiology-daily.txt"
set "inputFile3=KE_Kapiti_output_eddy\KE_Kapiti_watercycle-daily.txt"
set "inputFile4=KE_Kapiti_output_eddy\KE_Kapiti_vegstructure-daily.txt"
set "inputFile5=KE_Kapiti_climate_eddy.txt"




set "outputFile=KE_Kapiti_output_eddy\KE_Kapiti_soilchemistry-daily.csv"
set "outputFile2=KE_Kapiti_output_eddy\KE_Kapiti_physiology-daily.csv"
set "outputFile3=KE_Kapiti_output_eddy\KE_Kapiti_watercycle-daily.csv"
set "outputFile4=KE_Kapiti_output_eddy\KE_Kapiti_vegstructure-daily.csv"
set "outputFile5=KE_Kapiti_output_eddy\KE_Kapiti_climate_eddy.csv"



powershell -Command "(Get-Content '%inputFile%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile%'"
powershell -Command "(Get-Content '%inputFile2%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile2%'"
powershell -Command "(Get-Content '%inputFile3%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile3%'"
powershell -Command "(Get-Content '%inputFile4%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile4%'"
powershell -Command "(Get-Content '%inputFile5%' | ForEach-Object { $_ -replace '\t', ',' }) | Set-Content '%outputFile5%'"






pause
