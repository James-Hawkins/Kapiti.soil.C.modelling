set echo off

python.exe -B C:\Users\rahimi-j\Desktop\LandscapeDNDC_BF_GH\kkplot\kkplot.py --measurements-dir .\measurements  --data-dir .  --providers-dir C:\Users\rahimi-j\Desktop\LandscapeDNDC_BF_GH\kkplot\providers\ --tmp-dir .kkplot-tmp --outputs-dir . --engine matplotlib --output .\kkplot_KE_Kapiti_overview.png --debug .\KE_Kapiti_plotscripts\KE_Kapiti_overview.yaml > plot.py 
python.exe plot.py 


pause
