pl_hplc (Plot Chromatograms)
=======
The main goal for this package is to combine data from LC-UV with measurment from other instruments.

The pl_hplc require R on your computer, and package plotrix for the plotting graphs.

The package looked to the folder where files from HPLC and other measurments are located. The files should have names like this:
experimentalID_typeOfMeasurment_sampleName_sampleID.csv
where:
- experimentalID. The name of experiment, all files with this ID will be plot and if possible will be combined. Variable in the script also should have the same ID (experiment)
- typeOfMeasurment. Define what this file is. Set it hplc for all chromatogram, and set any_name for other measurments. Do not forget to set the same name in the variable 'measurment_name'
- sampleName. What inside the sample. Can be anything. Just do not add dots and underline into the name.
- sampleID. Can be char or/and number with any lenght. 
- For this version of package only csv files are supported.
