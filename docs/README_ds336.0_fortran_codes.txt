Fortran 90 codes which extract data from SYNOP, METAR, Buoy, and Upper air NetCDF files,
which are available in NCAR RDA dataset ds336.0, are listed below. These programs can be 
downloaded from the NCAR RDA ds336.0 dataset page at 
https://rda.ucar.edu/datasets/ds336.0/#!software. 

The code is structured in a generic format to provide standard variables. Users should 
modify the fortran code to fit their individual needs. NetCDF libraries from Unidata are 
required to compile and run the code. Links to download various NetCDF libraries can be 
found here.

NetCDF Decoders
read_synop_nc.f	    Fortran code to read surface SYNOP observation NetCDF files (e.g. Surface_Synoptic_20130101_0000.nc)
read_metar_nc.f	    Fortran code to read surface METAR observation NetCDF files (e.g. Surface_METAR_20130101_0000.nc)
read_buoy_nc.f	    Fortran code to read surface buoy observation NetCDF files (e.g. Surface_Buoy_20130101_0000.nc)
read_upperair_nc.f	Fortran code to read upper air observation NetCDF files (e.g. Upperair_20130101_0000.nc)

To compile and link these individual programs, use the following syntax:

GNU compiler:
gfortran -o read_synop read_synop_nc.f -L/path/to/netcdf/lib -lnetcdf

Intel compiler:
ifort -o read_synop read_synop_nc.f -L/path/to/netcdf/lib -lnetcdf

and replace "/path/to/netcdf/lib" with the location of your local NetCDF library 
directory. See NetCDF for further information on NetCDF libraries.