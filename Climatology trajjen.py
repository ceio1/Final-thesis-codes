
"""
====================================================
LONDON AND BEIJING CLIMATOLOGY TRAJECTORY GENERATION 
====================================================
This script was used to generate 5 days daily trajectories for the Climatology analysis of Beijing and London
with a Windows machine. The output files are back-trajectories labelled as follows:
london18jan0200winter2014010100. The basename "london", followed by the year "18", month "jan", 
altitude "0200", season "winter", date and time in the format YYYYMMDDHH "2014010100"
The basename and additional year has added to the standard pysplit format to facilitate
clustering display and the division of the trajectory dataset for the climatology analysis.
Because the script generated 5-day back trajectories, and th emeteorological data (GDAS1) are available in
weekly files, the last week of the files of the month previous to the month of trajectory generation
has to be included in the directory of meteorological filed. For example, to generate trajectories for January 
2018, GDAS data for week 1,2,3,4,5 of january and for week 5 of December 2017 have to be present in
the directory. If the GDAS of the previous month is not in the meteo_dir directory, no error will be raised
and the run will result in incomplete trajectory files. For example, a typical complete trajectory file
will have a size of 12kb, if the GDAS from the previous month is not present in the meteo_dir, the script
will generate files in the storage_dir of size inferior to 12kb (e.g. 5-6 kb).
 """

import calendar
import pysplit
##importing packages needed

##"for" loop for source location (basename), coordinates , months and years.
locs = {
        'london': {
                'coords':(51.50, 0.12),
                'months':[1],
                'years': ["{0:0=2d}".format(i) for i in range(14,18)]
                },
        'beijing': {
                'coords':(40.00, 116.00),
                'months':[11, 12],
                'years': ["{0:0=2d}".format(i) for i in range(13,17)]
                }
        }

"""
'london' and 'beijing' are the chosen basenames of the trajectory files, months are integers corresponding to
the numerical value of the month (e.g. 1=January, 12=December) and 'years' is a range of integers that encompass 
the years for desired trajectory generation. This range goes from the first year of generation to the last +1 
(e.g. to generate trajectories for 2013, 2014, 2015, 2016 and 2017,  
'years': ["{0:0=2d}".format(i) for i in range(13,18)]). The integers in the range will be added to 2000 (see 'years' below) 
"""

for l in locs:
    for y in locs[l]['years']:
        for m in locs[l]['months']:
            month_abbr = calendar.month_abbr[m].lower()
            ##months specified as numerical value (see above )
            curr_loc_month_year = l + y
            ##curr_loc_month_year generates trajectories with a basename reporting location and year (l + y)
            
            working_dir = r'C:/hysplit4/working'
            ##directory containing HYSPLIT exectutable
            storage_dir = r'C:/trajectories/{curr_loc_month_year}'
            ## creates directory labelled with location and year that will contain output trajectory files of that location and year
            meteo_dir = r'C:/Users/User/Desktop/year4/Climatology_data'
            ##directory containing GDAS1 files (input meteorological files)
            
            basename = curr_loc_month_year
            ##basename set to generate trajectories with location and year (see in the description at the top of the page)
            years = [2000 + int(y)]
            ##years specified in the "for" loop 
            months = [m]
            ##months specified in the "for" loop
            hours = [0, 3, 6, 9, 12, 15, 18, 21, 24]
            ## hours of arrival of the back-trajectory at source location
            altitudes = [400]
            ##altitude in meters above ground
            location = locs[l]['coords']
            ##locations specified above in the "for" loop
            runtime = -120
            ##runtime negative to generate 5 days back-trajectories (-120 hours)
             
            pysplit.generate_bulktraj(basename, working_dir, storage_dir, meteo_dir,
                                       years, months, hours, altitudes, location, runtime,
                                       monthslice=slice(0, 32, 1), get_reverse=True,
                                       get_clipped=False)
"""
The slice (0, 32, 1) was used to generate trajectories everyday for an entire month
Multiple options allow the user to change these parameters
For additional informations consult https://github.com/mscross/pysplit
In this case, reverse trajectories were generated at the same time as back-trajectories 
using the command "get_reverse=True". These were used to calculate integration errors.
"""