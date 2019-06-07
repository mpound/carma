
from subarrayCommands import *
                    
projName = 'engineeringTest'
obsblockName = 'interactive'


#---------------------------------------------------
def yearMonthDay() :
    timeDateValue = time.asctime(time.gmtime()).lower().split()
    if int(timeDateValue[2]) < 10 : timeDateValue[2] = str('0'+str(timeDateValue[2]))
    return '%s%s%s' % (timeDateValue[4],timeDateValue[1],timeDateValue[2])

#---------------------------------------------------


newProject(projName, obsblockName, yearMonthDay(), 1)

blessedSource = [ 'noise', \
    'sun', 'mercury', 'venus', 'moon', 'mars', \
    'jupiter', 'saturn', 'uranus', 'neptune', \
    '3c279', '3c273', '3c84', '3c454.3', '3c345' \
    ]
constraints()

for i in range(len(blessedSource)): 
    intent(blessedSource[i], 'O', True, False)


