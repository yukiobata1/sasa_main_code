********************************************************************
Notes for SEIB-DGVM MPI-version (8/Dec/2020)

Hisashi SATO (JAMSTEC)
http://seib-dgvm.com/hsato/
********************************************************************

(1) This package enables the SEIB-DGVM to compute multigrid cells by using MPI parallel computation. 
This package also contains a R code for visualizing result of the multi-grid simulation. Ask your system
 administrator whether your environment satisfies this condition. You also have to prepare climate data. 
I only distribute sample data (small rectangular area in Siberia) online, because of the large size! 
If you want to get a copy of climatic data,  contact me, then I would I can help.

(2) Following files consist this package
  readme_mpi.txt	This document
  Makefile		Makefile for compiling program code
  go.bat		Specification for MPI jobs
  go.sh			Execusion script
  start_mpi.f90		The alternative of start_point.f90 in the one-grid simulation version
  main.f90		The alternative of main_point.f90 in the one-stand simulation version
  visualizer.R		R code for mapping simulation results
  landmask_0.5deg.txt	Landmask for simulations @ 0.5 degree grid mesh
  index.html		A HTML file that shows visualized simulation results
  parameter.txt		Same as that of point simulation version
 
(3) Following files must be modified to meet machine environment and simulation design. Comment lines are available in these files.
  Makefile
  go_l.bat
  go_l.sh
  start_mpi.f90
  main.f90
  parameter.txt

(4) Following code files are required. These files are common usage of the point simulation version, so are not included in this folder.
modules.f90
initialize.f90
physics.f90
metabolic.f90
spatial_calc.f90
population_regu.f90
output.f90
etc.f90
SFLXALL_SRC_VER_2.7.1.f90

(5) For executing code, type following command sequantially. The first command compiles the code, and the secound command conducts simulation and visualization.
  makefile
  sh go_l.sh&

(6) If simulation year is longer than climatic data, this code use climate data repeatedly. To change this feature, modify folllowing line, which will be found in the main_mpi.f90.
  if (year_climate==YearMaxClimate+1) year_climate=1

For example, if you want to repeat climate data of last year when simulation year exceeds data lenght, change as follows:
  if (year_climate==YearMaxClimate+1) year_climate=YearMaxClimate

(7) Simulation assumes that atmospheric CO2 concentration is fixed at 368.0 ppm.
To employ the historical record or IPCC RCP scenarios, activate one of the following comment line, which will be found in the main.f90.
    co2atm = 368.0
   !co2atm = aco2_annual(min(year_climate,YearMaxCO2)) !Progress
   !co2atm = aco2_annual(150)                          !Fixed at 2000
   !co2atm = aco2_annual(1)                            !Fixed at 1850

(8) Following parameters in parameter.txt will NOT be used for MPI simulations. Corresponding parameters are defined in start_mpi.f90.
  Fn_climate
  Fn_location
  Fn_spnin
  Fn_spnout

(9) In the default code, distribution maps are generated using average results during last 10 years of the simulation. To change this period for average, alter the value in parameter YearForMean in start_mpi.f90.

(10) Followings are the default simulation conditions
Simualtion grid resolution.
-> 0.5deg

Simualtion area.  If you change this setting, change parameters LatNoStart, LatNoEnd, LonNoStart, LonNoEnd in the start_mpi.f90. You also have to modify line 10 of visualization.R to provide corresponding values.
-> East Siberia (N55-60, W120-135)

Simualtion period
-> 1850~1949

Atmospheric CO2 concentration
-> Fixed at 368ppm

Location where code and parameter files exist
-> '/home/hsato/work_distribute/'

Location where climatic data of 0.5deg exists
-> '/data/climate_MirocAR5Base_daily_0.5deg_V3/historical/'

Location where result and restart files will be outputted
-> '/home/hsato/work_distribute/result/'

Location where summary results and visualized images will be outputted. If you change this setting, you also have to change line 10 of visualization.R
-> '/home/hsato/work_distribute/result_visualized/'

Number of processors to employ for MPI computation
->  4 nodes * 40 processors/nodes = 160 processors

(11) Climate data for small test run can be obtained from the followijng URL.
https://ebcrpa.jamstec.go.jp/~hsato/climate_data_Siberia/

All files are is delivered from the Miroc-ESM (Watanabe et al., 2011). 

Miroc_historical.zip contains historical climate reconstruction during years of 1850-2005 (156 years). While, Miroc_RCP85.zip contains forecasted cliate chage with RCP8.5 forcing during years of 2006-2100 (95 years)
Watanabe, S., et al. (2011). "MIROC-ESM 2010: model description and basic results of CMIP5-20c3m experiments." Geoscientific Model Development 4(4): 845-872.

These compressed files contain climate data of small rectangular area in Siberia. Geographical range is as following. 
Range of latitude number: 051-060, which correspond to N55-60
Range of latitude number: 601-630, which correspond to W120-135
