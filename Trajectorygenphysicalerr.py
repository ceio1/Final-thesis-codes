"""
========================================
TRAJECTORY GENERATION FOR PHYSICAL ERROR
========================================
This script was used to generate 3 trajectories employed in the estimation of resolution error
The trajectories are generated with slightly different source location: my (Marylebone road KCL station),
br (Brixton road KCL station) and lo (London arrival location)
A more detailed description of scripts used for trajectory generation is provided in "Climatology trajjen".
"""

import pysplit


##"for" loop for source location and basename.
locs = {
        'my': {
                'coords':(51.52, -0.15)
                },
        'br': {
                'coords':(51.46, -0.11)
                },
        'lo': {
                'coords':(51.50, 0.12)
                }
        }
##locs of "for" loop to specify location source location coordinates
for l in locs:
            curr_loc = l 
            #curr loc set to identify source location of the trajectory
            working_dir = r'C:/hysplit4/working'
            storage_dir = r'C:/Users/User/Desktop/resuncertainty'
            meteo_dir = r'C:/Users/User/Desktop/year4/Climatology_data'
            
            basename = curr_loc
            years = [2018]
            months = [1]
            hours = [12]
            altitudes = [400]
            location = locs[l]['coords']
            runtime = -120
             
            pysplit.generate_bulktraj(basename, working_dir, storage_dir, meteo_dir,
                                       years, months, hours, altitudes, location, runtime,
                                       monthslice=slice(0, 1, 1), get_reverse=True,
                                       get_clipped=False)
"""
The slice (0, 1, 1) was used to generate 1 trajectory at the hours specified (h12) for one day (01/01/2018)
Multiple options allow the user to change these parameters
For additional informations consult https://github.com/mscross/pysplit
In this case, reverse trajectories were generated at the same time as back-trajectories 
using the command "get_reverse=True". These were used to calculate integration errors.
"""