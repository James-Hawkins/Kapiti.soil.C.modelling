@echo off

rem Set the path to the Rscript executable
set RSCRIPT="C:\Program Files\R\R-4.4.3\bin\Rscript.exe"

rem Set the path to the R script to execute

set RSCRIPT_FILE="C:\Users\hawkj\Documents\Github\L-DNDC\LandscapeDNDC\projects\KE_Kapiti_Base\KE_Kapiti_output_eddy\Source_file.R"

rem Execute the R script
%RSCRIPT% %RSCRIPT_FILE%

rem Pause so the user can see the output

"C:\Program Files\R\R-4.4.3\bin\Rscript.exe" CMD BATCH %RSCRIPT_FILE%

echo Scri[pt has been run.


cmd /

