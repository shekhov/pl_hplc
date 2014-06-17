pl_hplc (Plot Chromatograms)
=======
The main goal for this package is to combine data from LC-UV with measurment from other instruments.

Settings
----------
The pl_hplc require R on your computer, and package plotrix for the plotting graphs. Script will download it if it will be possible

Before analyzing data, pathes should be set in the variables. It is possible to change it manually in the script (*path_to_data*, *path_to_save*), or by calling function (**path_to_data(path)**, **path_to_save(path)**).
* Note: during the developing, I set this pathes by default to what I need

Set output format as well. Default value is 'screen'. You can set it to:
* 'pdf' or 'png'
* All other formats does not available now, and everything that differ from these two formats will reset default value

How to name files. The package looked to the folder where files from HPLC and other measurments are located. The files should have names like this:
*experimentalID_typeOfMeasurment_sampleName_sampleID.csv*
where:
* experimentalID. The name of experiment, all files with this ID will be plot and if possible will be combined. Variable in the script also should have the same ID (experiment)
* typeOfMeasurment. Define what is this file. Set it hplc for all chromatogram, and set any_name for other measurments. Do not forget to set the same name in the variable 'measurment_name'
* sampleName. What inside the sample. Can be anything. Just do not add dots and underline into the name.
* sampleID. Can be char or/and number with any lenght. 
* For this version of package only csv files are supported.

There are some flags used in the script that can change the way of plotting. Here some of them:
* use_gradient. When chromatogram for data is plotting, with this flag it will plot gradient of acetonitrile (I will let to change this name as well). But you need to make sure that *hplc_method* is also set (you can use **set_hplc_method (time, gradient)** to do so).
