import os
import obsdefUtils as utils

RUN_PACS = False

def szaScheduleCommand(schedule):
    """ Execute a SZA schedule command """
    szaBinDir = "/home/szadaq/sza/bin/"
    szaSchDir = "/home/szadaq/sza/array/sch/"
    szaCmd = szaBinDir + "szaUCommand command='schedule " + szaSchDir + \
             schedule + "' | grep -v \"Type 'exit'\"" 
    #return os.system(szaCmd)
    return "ok"


def start(obsblockId, combine):
   """ Initialize a PACS observing run.

       ObsblockId: Name of the obsblock containing the SZA data.
       combine   : combine time in seconds (equivalent to 
                   the CARMA record length)
   """
   com = 'carmaStart.sch("%s",%d)' % (obsblockId, combine)
   return szaScheduleCommand(com)


def tsys(combine=None):
   """ Make tsys measurement """

   if combine == None: combine = 4
   com = "carmaTsys.sch(%d)" % combine
   return szaScheduleCommand(com)


def point():
   """ Perform radio pointing """

   com = "carmaPoint.sch"
   return szaScheduleCommand(com)


def observe(source, intent='A'):
   """ Track 'source' using the SZA and start integrating """

   com = 'carmaObserve.sch(%s,"%s")' % (source, intent)
   return szaScheduleCommand(com)


def noise(combine=None):
   """ Integrate on noise source """

   if combine == None: combine = 4
   com = "carmaNoiseOn.sch(%d)" % combine
   return szaScheduleCommand(com)


def getObsblock(obsblock, prefix="zr"):
   """ Returns SZA obsblock name """

   sza_obsblock = obsblock
   if prefix <> None: sza_obsblock = prefix + sza_obsblock

   return sza_obsblock


def stop(obsblockId):
   """ Stop taking data for SZA obsblock """

   com = 'carmaEnd.sch("%s")' % obsblockId
   return szaScheduleCommand(com)


def fillPacsData(email='obs@mmarray.org', manual=True, log=False):
   """ Send email to fill pacs data """

   # Get starting/ending UT
   startUT = utils.commands.s.getScriptString(utils.INDX_STR_PACS_UT)
   endUT   = utils.getUT(sza=True)

   # Get obsblock name, and split off trial
   obsblock = utils.commands.s.getScriptString(utils.INDX_STR_PACS_OBSBLOCK)
   if manual: obsblock += ".mir"

   # Construct command to fill obsblocks
   message = startUT + ' ' + endUT + ' ' + obsblock + " 1 30"

   # Email fills
   if manual:
       command = "/home/obs/bin/fillSzaMiriad " + message
       #tt = os.popen3(command)
       #tt[0].close()
       #tt[1].close()
       #tt[2].close()
   else:
       pass
       #utils.sendEmailMessage('eml@ovro.caltech.edu', email, message,
       #      subject='SZA Miriad Request', cc=email)
   if log: utils.commands.trackMessage('SZA fill command : ' + message)


def cleanup(obsblock):
   """ Performs clean-up tasks when PACS script ends or is aborted """

   # Stop SZA obsblock
   #utils.commands.trackMessage('Stopping SZA obsblock')
   #stop(obsblock)

   # Fill pacs data
   # fillPacsData(log=True)
