# HDF4

## HDF4Import

Since Mathematica offers only partial HDF Import support I composed a small Mathematica package to access Vdata sets in HDF4 data files. This is still in a very experimental stage and I stopped implementing additional features after the basic Vdata import functionality was achieved. But adding support of furter data formats like Vgroups, Raster Images, etc. seems to be straight forward. Contact me if you are in dire need of thess features.

Annotations in the Cdata sets are somewhat supported but may yield unexpected results.

The package is called HDF4 and contains one public function: HDF4Import. HDF4Import tries to acces Vdata sets in a HDF file (version 4) and returns the actual data or the names of the Vdata sets:

To acces the data use:

``` mathematica
Needs["HDF4`"]
HDF4Import[<insert file name>, "Data"]
```

To get the names of the data sets use:

``` mathematica
Needs["HDF4`"]
HDF4Import[<insert file name>, "Datasets"]
```

Here is an example HDF file with Vdata sets  (courtesy HDFGroup): [hdifftst1.hdf[(https://github.com/markusroellig/HDF4/blob/main/hdiffst1.hdf)

You can download the package her: HDFImport Mathematica Package

I downloaded example hdf files and the corresponding content dumps from the HDFGroup website (Here is the link:http://www.hdfgroup.org/ftp/HDF/HDF_Current/src/unpacked/mfhdf/dumper/testfiles/)
The contents of this directory as zip file can be donwloaded here : HDF4examples.zip

I also uploaded the Mathematica notebook that I used to figure out how the Vdata elements are composed and how to acces the relevant parts of the hdf file: HDF4.nb
