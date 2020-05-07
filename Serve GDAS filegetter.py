"""
=============================================
BATCH DOWNLOAD FOR GDAS1 METEOROLOGICAL FILES
=============================================
This script was used to import several GDAS meteorological files from the ARL ftp archive server to the ssh burn 
geoscience server. Single weekly GDAS files were imported using the "wget" function. This script downloads files
for all weeks in the month and year specified. The script has to be in the same directory in which the desired
files are to be downloaded.
"""

from ftplib import FTP


ftp = FTP('arlftp.arlhq.noaa.gov')   # connect to host, default port
ftp.login()               # user anonymous, passwd anonymous@
ftp.cwd('archives/gdas1/')        #Change directory to access GDAS1 files on the ftp server

ls = ftp.nlst() #Creates a list of all the file names as strings

month = ['jan', 'nov', 'dec']   #List of desired months 
years = ['15', '16', '17', '18', '19']  #List of desired years

for i in range(len(ls)):    #For looping though each file name
    for j in month:         #scans for desired monts months
        for k in years:     #scans for desired years
            if j in ls[i] and k in ls[i]:       #If the month and year are in the name: print(ls[i])
                print('Starting Download  ' + ls[i]) #printing Starting download and file name when download starts
                ftp.retrbinary('RETR ' + ls[i], open(ls[i], 'wb').write)    #Downlaod the file
                print('Download finished \n') #printing Download finished and file name when download is complete
                break
            
ftp.close() # close connetion to the server after download
            

