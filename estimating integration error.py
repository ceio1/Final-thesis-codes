"""
============================
Estimating Integration Error
============================
"""
import numpy as np
import pysplit
import os

trajgroup = pysplit.make_trajectorygroup(r'C:/Users/User/Desktop/londonclim/*')
##creating a trajectorygroup containing BTs for calculation
for traj in trajgroup:
    traj.load_reversetraj()
##loading the corresponding reverse trajectories in "traj"
for traj in trajgroup:
    traj.calculate_integrationerr()
##calulating integration error 
relative_errors = [traj.integration_error for traj in trajgroup]
cutoff = np.mean(relative_errors) + (np.std(relative_errors) * 2)
bad = []
##creating cutoff to separate"bad" trajectories (with int errors larger than two stdev from the mean)
for traj in trajgroup:
    if traj.integration_error > cutoff:
        bad.append(traj.trajid)
##selecting "bad" trajectories and creating a new trajgroup without bad trajectories
print('Expectation: ', trajgroup.trajcount, 'trajectories -', len(bad),
      'bad trajectories =', trajgroup.trajcount-len(bad), 'trajectories')
trajgroup.pop(trajid=bad)
print('Result: ', trajgroup.trajcount, 'trajectories')
##visualising the number of initial trajectories, minus the bad trajectories and the result (trajectories to use)
print(bad) 
##visualizing bad trajectories
badcorr = []
for i in bad:
    badcorr.append(i[:-3])
#creting an object that lists all bad trajectories (badcorr)
    
for i in badcorr:
    os.remove(i)
##removing bad trajectories from the directory where BT were retrieved initially
    
    
    