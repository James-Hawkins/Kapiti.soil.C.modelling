@echo off
set "NPP_PATH=C:\Program Files\Notepad++\notepad++.exe" 

set "XML_FILE=C:\Users\JHawkins\Documents\Github\Soil.C.modelling\Kapiti.soil.C\projects\KE_Kapiti_Eddy_July\KE_Kapiti_events_eddy.xml"
set "XML_FILE=C:\Users\JHawkins\Documents\Github\Soil.C.modelling\Kapiti.soil.C\projects\KE_Kapiti_Eddy_July\KE_Kapiti_site_eddy.xml"
set "XML_FILE=C:\Users\JHawkins\Documents\Github\Soil.C.modelling\Kapiti.soil.C\projects\KE_Kapiti_Eddy_July\KE_Kapiti_eddy.ldndc"
set "XML_FILE=C:\Users\JHawkins\Documents\Github\Soil.C.modelling\Kapiti.soil.C\projects\KE_Kapiti_Eddy_July\KE_Kapiti_eddy.ldndc"


start "" "%NPP_PATH%" "%XML_FILE%"

