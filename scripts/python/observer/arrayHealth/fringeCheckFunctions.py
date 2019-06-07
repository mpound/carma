import string
import carma
import subarrayCommands as commands
import obsdefUtils as utils
import pacs
import radioPoint as rp
import os
import glob
import calendar
import time
import subprocess
import fnmatch
from printFunctions import *

#BW500 = carma.control.CORR_BW_500MHZ
#BW250 = carma.control.CORR_BW_250MHZ
#BW125 = carma.control.CORR_BW_125MHZ
#BW62  = carma.control.CORR_BW_62MHZ
#BW31  = carma.control.CORR_BW_31MHZ
#BW8   = carma.control.CORR_BW_8MHZ
#BW2   = carma.control.CORR_BW_2MHZ    
#
#USB   = carma.control.SB_UPPER
#LSB   = carma.control.SB_LOWER
#AUTO  = carma.control.SB_AUTO

UMASK = 'umask 0002'

def setScratchDir(root, obsblock):
   scratch = root + "/" + obsblock
   
   return scratch


def setSubObsblock(date, subarray, lofreq):
    t = date + '_' + str(subarray) + '_' + str(lofreq)
    return t


def readCommentString(filename):
   # Read lines
   f = open(filename, "r")
   lines = f.readlines()
   f.close()

   # Find comment string
   comment = None
   for l in lines:
      if l.find('>Comment<') > 0:
         t = l.split("<TD>")
         comment = t[len(t)-1].split('</TD>')[0]
         break

   # Done
   return comment

         
def readCommandLine(p, var):
   """ Read command line arguments """

   # Read command line arguments
   p.processInputParameters(inputParams=commands.scriptKeyVals)

   # Get existing obsblock parameters, if needed
   var['reprocess'] = False
   if p.obsblock <> None:
       var['reprocess'] = True
       p.tune = False
       p.observe = False
       var['obsblockName'] = p.obsblock
       if var['obsblockName'][-4:] == '.mir': 
          var['obsblockName'] = var['obsblockName'][:-4]
       t = var['obsblockName'].split('.')
       var['source'] = t[1]
       var['date']   = t[2]
       var['trial']  = t[3]
       var['subObsblockName'] = t[2]
       z = t[2].split('_')
       if len(z) == 3:
          var['date']     = z[0]
          var['subarray'] = z[1]
          var['lofreq']   = z[2]
       var['flux'] = utils.getSourceFlux(var['source'])
       var['scratch'] = setScratchDir(var['scratchArea'], var['obsblockName'])

   # Set tuning frequencies. 
   if p.tune == False:
       [frest, fif, flo] = commands.freqSetup()
       p.freq = frest
       p.iffreq = fif
       p.sb = 'LSB'
       if p.freq > flo: p.sb = 'USB'
   elif commands.subarrayNo == 2:
       # Set frequency
       if p.freq == None: 
          p.freq = 35.938
       elif p.freq < 40.0:
          p.freq = 35.938
       # else:
#          p.freq = 89.78  # MWP CHANGED Freq 02/03/2011
       #    p.freq = 85.8286
       # Set I.F.
       if p.iffreq == None: p.iffreq = 0.0
       # Set SB
       if p.sb == None: 
          p.sb = 'USB'
          if p.freq < 40.0: p.sb     = 'LSB'
   else:
       if p.iffreq == None: p.iffreq = 2.5
       if p.freq   == None: p.freq   = 97.5
       if p.sb     == None: p.sb     = 'USB'

   # Convert source name to lower case
   if var['source'] <> None: var['source'] = string.lower(var['source'])

def getHtmlHeader():
    htmlHeader  = '<html><head><title>CARMA Fringe Test</title>\n'
    htmlHeader += '<link rel="stylesheet" href="/observing/carma.css" type="text/css">\n'
    htmlHeader += '</head>\n'
    htmlHeader += '<body>\n'
    htmlHeader += '<center><h2>CARMA Fringe Test</h2></center>\n'

    return htmlHeader

def getOldLinks(var):
    oldLinks = '<center>results from previous fringe tests: &nbsp;' + \
      ' <a href="' + var['fringeDirHtml'] + '/' + 'archive_time.html">time-ordered</a>' + \
      ' &nbsp; &nbsp; ' + \
      ' <a href="' + var['fringeDirHtml'] + '/' + 'archive.html">calendar</a>' + \
       '</center>\n';
    return oldLinks

def createHtmlInformative(var):
    """ Initializes index.html page for fringe check. """
    # Create header
    com = getHtmlHeader()
    com += '<BR><BR>\n'
    com += '<H2>Fringe test started on ' + utils.getLocalTime() + '</h2>\n'

    # Make link to previous tests
    com += '<BR><BR>\n'
    com += getOldLinks(var)
    com += '</body></html>\n'

    # Remove old index.html file 
    var['defaultLink'] = var['fringeDir'] + '/index.html'
    os.system('rm ' + var['defaultLink'] + ' >& /dev/null')

    # Add link to message
    outputHtml = var['fringeDir'] + '/index.html'
    f = open(outputHtml, 'w')
    f.write(com)
    f.close()

    # set the permissions so we don't run unto trouble if fringecheck is run
    # by not-obs.
    os.system('chmod 777 '+ outputHtml)

def createHtmlResults(var, antennas, p):
    """ Starts html page containing the fringecheck results """
    # Get various antenna lists
    if antennas == None:
       ant_withdata = "unknown"
       ant_nodata_online = "unknown"
       ant_nodata_offline = "unknown"
       ant_online = "unknown"
       utdate = None
       miriadError = "unknown"
       bad_windows = "unknown"
    else:
       ant_withdata = antennas[0]
       ant_nodata_online = antennas[1]
       ant_nodata_offline = antennas[2]
       ant_online = antennas[3]
       utdate = antennas[4]
       miriadError = antennas[5]
       bad_windows = antennas[6]

    # Subarray and lofreq must be set
    if var['lofreq'] == None or var['subarray'] == None:
       raise Exception, 'Cannot process old fringeCheck files'

    # Modify output directory name based on time stamp
    tsec = ''
    if utdate <> None:
        # Replace colons with spaces and remove subseconds
        sdate = utdate.replace(':', ' ')[0:16]

        # Convert time to seconds
        z = time.strptime(sdate,'%y%b%d %H %M %S')
        tsec = str(calendar.timegm(z)) + '.'

    # Set output directory name, minus the subarray number.
    tmp = var['obsblockName'].split('.')
    var['trial'] = tmp[len(tmp)-1]
    var['outputDir'] = tsec + var['subObsblockName'] + '.' + \
                var['source'].split('.')[0] + '.' + str(var['trial'])
    var['outputDir'] = var['outputDir'].lower()

    # Set final file name
    var['fullOutputDir'] = var['fringeDir'] + '/' + var['fringeSubdir'] + \
           '/' + var['outputDir']
    
    # Make output directory
    # result = glob.glob(var['fullOutputDir'])
    if not os.path.exists(var['fullOutputDir']): 
        os.system(UMASK + '; mkdir ' + var['fullOutputDir'])
    if not os.path.exists(var['fullOutputDir']): 
       raise Exception, 'Error creating output directory ' + var['fullOutputDir']

    # Set HTML directory
    var['htmlDir'] = var['fringeSubdir'] + '/' + var['outputDir']

    # Check if output plots were made, and move them to the output directory
    gifFiles = checkFiles(var, p)

    # Move fringe check log file
    scriptlog = var['scratch'] + '/' + var['fringeLog']
    if len(glob.glob(scriptlog)) > 0:
       com = "mv " + var['scratch'] + '/' + var['fringeLog'] + ' ' + \
             var['fringeDir'] + '/' + var['htmlDir'] + '/'
       if os.system(com) != 0:
          print '*** Error moving fringe check log ***'

    # Format the ut date nicely
    sutdate = utdate
    if sutdate == None: 
        sutdate = 'Unknown'
    else:
        year = sutdate[0:2]
        month = sutdate[2:3] + sutdate[3:5].lower()
        day = sutdate[5:7]
        t = sutdate.split()[1]
        sutdate = "%s %s, 20%s %s" % (month, day, year, t)

    # Start table
    com  = getHtmlHeader()
    com += '<table border=0>\n'
    com += '<TR><TH align=left>UT Date of observations:</TH><TD>' + sutdate + '</TD></TR>\n'
    com += '<TR><TH align=left>Comment</TH><TD>' + str(p.comment) + '</TD></TR>\n'
    com += '<TR><TH align=left>Subarray Number:</TH><TD>' + str(var['subarray']) + '</TD></TR>\n'
    com += '<TR><TH align=left>Obsblock:</TH><TD>' + var['obsblockName'] + \
           '</TD></TR>\n'
    com += '<TR><TH align=left>Source:</TH><TD>' + var['source'] + '</TD></TR>\n'
    com += '<TR><TH align=left>LO Frequency:</TH><TD>' + var['lofreq'] + ' GHz</TD></TR>\n'
    com += '<TR><TH align=left>Expected Flux (95 GHz) :</TH><TD>' + \
           str('%.2f' % utils.getSourceFlux(var['source'], 95.0)) + \
           ' Jy</TD></TR>\n'
    com += '<TR><TH align=left>Expected Flux (230 GHz) :</TH><TD>' + \
           str('%.2f' % utils.getSourceFlux(var['source'], 230.0)) + \
           ' Jy</TD></TR>\n'
    com += '<TR><TH align=left>Antennas online without astronomy data:</TH>'
    if len(ant_nodata_online) > 0:
        com += '<TD nowrap><b>'
        for a in ant_nodata_online:
           com += ' ' + 'C' + a
        com += '&nbsp;&nbsp;&nbsp;<font style="BACKGROUND-COLOR: red" color=yellow>ERROR: Fringe check failed</font></b>'
    else:
        com += '<TD>None'
    com += '</TD></TR>\n'
    com += '<TR><TH align=left>Empty miriad windows:</TH><TD>'
    if bad_windows == '':
       com += 'None'
    else :
       com += bad_windows + '&nbsp;&nbsp;&nbsp;<b><font style="BACKGROUND-COLOR: red" color=yellow>WARNING: Some windows have zero channels</font><b>'
    com += '</TD></TR>'
    if len(ant_nodata_offline) > 0:
        com += '<TR><TH align=left>Antennas offline:</TH><TD>'
        for a in ant_nodata_offline:
           com += ' ' + 'C' + a
        com += '</TD></TR>\n'
    com += '<TR><TH align=left>FringeCheck reduction log:</TH><TD>'
    com += '<a href="' + \
           var['fringeDirHtml'] + '/' + \
           var['htmlDir'] + '/' + var['fringeLog'] + '">miriad log</a>'
    if miriadError: com += '&nbsp;&nbsp;&nbsp;<b><font style="BACKGROUND-COLOR: red" color=yellow>Error running miriad reduction script</font><b>'
    com += '<TR><TH align=left>Web page created:</TH><TD>' + utils.getLocalTime() + '</TD></TR>\n'
    com += '</table>\n'
    com += '<BR><BR>\n'
    com += getOldLinks(var)
    com += '<center>\n'

    # Did one of the plots fail?
    errorMakingGif = False
    for key, value in gifFiles.items():
        if not value['exists']: errorMakingGif = True

    # Add plots
    if errorMakingGif:
        com += '<font style="BACKGROUND-COLOR: red" color=yellow size=+2><b>WARNING: fringe check failed</b></font></h2><BR>\n'

    # Make links to plots
    for key, value in gifFiles.items():
        if value['exists']:
            com += '<h2>' + value['title'] + '</h2>\n'
            if value.has_key('sublabel'): com += value['sublabel'] + '<BR>\n'
            if value.has_key('pdf'): 
               com += '(<a href="' + var['fringeDirHtml'] + '/' + \
                   var['htmlDir'] + '/' + value['pdf'] + '">pdf file for individual baselines</a>)<BR>\n'
            com += '<img src="' + var['fringeDirHtml'] + '/' + \
                   var['htmlDir'] + '/' + value['name'] + '"><BR>\n'
        else:
            com += '<font style="BACKGROUND-COLOR: red" color=yellow size=+2><b>WARNING: Observations for ' \
                   + value['label'] + ' failed</b></font><BR>\n'
        com += '<BR><BR>\n'

    # Make link to previous tests
    com += '<BR><BR>\n'
    com += getOldLinks(var)

    # Save results so far to a file. This is required before createArchiveTime
    # in order to get the comments entered correctly on the web pages. 
    # This is such a kludge. I should do this is a database.
    outputHtml = var['fullOutputDir'] + '/index.html'
    f = open(outputHtml, 'w')
    f.write(com)
    f.close()

    # Print time-ordered list
    com += '<BR><BR>\n'
    com += createArchiveTime(var, nmax=20, header="<h2>Recent fringeCheck results</h2>")

    # End html
    com += '</center>\n'
    com += '</body>\n'
    com += '</html>\n'

    # Remove old default file 
    os.system('rm ' + var['defaultLink'] + ' >& /dev/null')

    # Now write all results to a file
    outputHtml = var['fullOutputDir'] + '/index.html'
    f = open(outputHtml, 'w')
    f.write(com)
    f.close()

    # Print summary to screen
    print "\n\n\n"
    print "********************************************************************"
    if var.has_key('obsblockDone') and not var['obsblockDone']:
        printWarning('The miriad file may not contain all of the data')
    if not errorMakingGif:
        print "Fringe test appears to have completed normally.\n"
    else:
        printWarning("Fringe test did not complete successfully.")
        for key, g in gifFiles.items():
            if not g['exists']:
                printWarning(g['label'] + " plot was not created.")
    print "Check results at"
    print "       http://cedarflat.mmarray.org/fringe/"
    print ""
    print "Sometime there is a delay in filling the obsblock."
    print "To re-fill and re-process this obsblock, type:"
    t = "run('fringeCheck', obsblock='%s'" % var['obsblockName']
    if p.refant <> None: t += ", refant=%d" % p.refant
    if p.conf != None: t += ", conf='%s'" % p.conf
    if p.comment <> None: 
       t += ', comment="%s"' % p.comment.replace('"', "'")
    t += ")"
    print t
    print "********************************************************************"

    #make sure all users can overwrite this directory, in case
    #fringcheck was not run by obs. (bug 424)
    #os.system('chmod 777 ' + var['fullOutputDir'])
    #os.system('chmod 777 ' + var['fullOutputDir'] + '/*')

    # Move default link
    os.system(UMASK + '; cd ' + var['fringeDir'] + '; ln -s ' + var['htmlDir'] + \
              '/index.html index.html')

def observeSources(var, p):
    """ Observe noise and astronomical source
    """
    # Initialize but only clear astrobands if we are tuning too
    commands.radioInit( clearastrobands=p.tune )

    # Sideband
    sb = commands.USB
    if p.sb == 'LSB': sb = commands.LSB

    # Get bright source
    var['source'] = p.source
    if var['source'] == None:
        flimit = 4.0
        while var['source'] == None and flimit >= 1.0:
           brightsources = utils.getBrightSources(flimit)
           if brightsources <> None:
              var['source'] = utils.getSource(brightsources, elmin=30.0, elmax=80.0) 
           if var['source'] == None: flimit -= 1.0;
        if var['source'] == None:
            raise Exception,'Could not find a bright source that is currently assessible'
    flux = utils.getSourceFlux(var['source'], freq=p.freq)

    # Set subarray and LO frequency
    lofreq = p.freq - p.iffreq
    if p.sb == 'LSB': lofreq = p.freq + p.iffreq
    var['subarray'] = commands.subarrayNo
    var['lofreq'] = str(int(lofreq+0.5))

    # Project
    projectCode = 'fringe'
    var['obsblock'] = var['source'].split('.')[0].lower()
    var['date'] = commands.yearMonthDay()
    var['subObsblockName'] = setSubObsblock(var['date'], var['subarray'], var['lofreq'])

    # Initialize sza
    if p.pacs:
       combine = 4
       sza_intent = 'BG'
       print '\nStarting SZA'
       pacs.start('sza_fringecheck', combine)
       print '\nSlewing SZA to ',var['source']
       pacs.observe(var['source'], intent=sza_intent)
       pacs.tsys(combine)

    # Set project, intent, constraints

    # EML adding default for maxsens=True if subarray owns multiple correlators
    if p.maxsens == None:
       p.maxsens=commands.subarrayOwnsMultipleCorrelators(commands.subarrayNo)
       
    commands.newProject(projectCode, var['obsblock'], var['subObsblockName'], maxsens=p.maxsens)
    commands.intent(var['source'],'G',True,False)
    commands.intent('noise','B',False,False)
    commands.constraints(carma.control.SNR,9,maxTsys=1000)

    # Slew CARMA antennas to source
    print '\nSlewing to ' + var['source']
    commands.track(var['source'])

    # Tune
    if p.tune:
       # Tune
       print 'Tuning to fsky = ' + str(p.freq) + ' GHz, IF=' + str(p.iffreq) + ' GHz, and sideband = ' + p.sb
       commands.freq(p.freq, sb, p.iffreq, None)

       # Set number of bits
       bits = commands.CORR_2BIT
       if p.bits == 3:
          bits = commands.CORR_3BIT
       elif p.bits == 4:
          bits = commands.CORR_4BIT

       # Set configuration
       conf = p.conf
       if conf == None:
          conf = 'LL'
          ants = commands.currentAntennaNumbers()
          # EML removing check on subarray number -- it is irrelevant
          #if commands.subarrayNo == 1 and len(ants) > 15 and max(ants) > 15:
          if len(ants) > 15 and max(ants) > 15:
             conf = 'CARMA23'
       conf = conf.upper()

       # Set correlator
       if commands.subarrayNo == 1:
           print '\n\nConfiguring spectral correlator'
           print '\nUsing decimate = ' + str(p.decimate) + '\n'
           commands.configwideastroband(conf=conf, bits=bits, decimate=p.decimate)
           commands.checkbands()
       elif commands.subarrayNo == 2:
           print '\n\nConfiguring wideband correlator'
           commands.configwideastroband(conf=conf)  # Do not include bits option
           commands.checkbands()
    else : 
        print "measuring tsys"
        commands.tsys()
        # Set configuration
        conf = p.conf
        if conf == None:
           conf = 'LL'
           ants = commands.currentAntennaNumbers()
           if commands.subarrayNo == 1 and len(ants) > 15 and max(ants) > 15:
              conf = 'CARMA23'
        conf = conf.upper()


    # Noise source
    trec = 2.0
    nrep = 6
    tint_noise = trec * nrep
    if p.pacs:
       print '\nStarting SZA noise integration'
       pacs.noise(combine)
    print 'Integrating on CARMA noise source for %d seconds with ambient load in beam' % tint_noise
    commands.noiseon()
    commands.amb(0)
    commands.sleep(2.1)
    commands.integrate(trec, nrep)
    commands.noiseoff()
    commands.sky()
    commands.sleep(2.1)

    # Point up if needed
    if p.pnt: rp.radioPoint(var['source'])

    # Observe grids during full-Stokes fringeCheck
    if conf == "FULLSTOKES":
       print ''
       print 'Integrating with polarization grids inserted on the 10m antennas'
       utils.observePol(p.tintPol, p.record, verbose=True, indent=1)
       commands.intent(var['source'],'G',True,False)

    # Bright source
    trec = p.record
    if p.pacs: pacs.observe(var['source'], sza_intent)
    commands.track(var['source'])
    tint = p.tint
    nrep = int(1.0*tint/trec)
    print 'Integrating on ' + var['source'] + ' for ' + \
          str(trec * nrep) + ' seconds'
    print 'Expected flux = ' + str('%.2f' % flux) + ' Jy'
    # TLC - some extra comments added 2/8/2011 to investigate why the big pause
    print 'PACS status: %s' % p.pacs
    print 'Doing tsys'
    commands.tsys()
    print 'Done tsys, now integrating'
    commands.integrate(trec, nrep, antwait=-2)

    # Get obsblock name
    var['obsblockName'] = commands.queryString('Control.Subarray%s.obsBlockId' % commands.s.getSubarrayNo())
    var['scratch'] = setScratchDir(var['scratchArea'], var['obsblockName'])

    # Done
    #od.pacs.cleanup(var['obsblockName'])
    print 'Observations done'

def runFiller(var):
    """ Runs manual fill on dataset.

        Returns obsblock name and antennas online in dataset
    """
    xmlDirs = ["/opt/sdp/astroheader/SLCorrelIntegrated/",
               "/opt/sdp/astroheader/WBCorrelIntegrated/"]
    xmlfile = 'astrohdr_' + var['obsblockName'] + '.xml*'

    xmlFullPath = None
    for d in xmlDirs :
       files = os.listdir(os.path.abspath(d))
       for filename in fnmatch.filter(files, xmlfile):
          xmlFullPath = d + filename
       if xmlFullPath <> None: break
    if xmlFullPath == None: return False

    var['obsblockDone'] = False
    if xmlFullPath.split('.')[-1:][0] == 'done': var['obsblockDone'] = True

    mirfile_dest = var['scratch'] + '/' + var['obsblockName'] + '.mir'
    if not os.path.exists(var['scratchArea']): 
       os.system("mkdir %s; chmod 777 %s" %  (var['scratchArea'], var['scratchArea']))
    command = UMASK + \
              '; rm -rf ' + var['scratch'] + \
              '; mkdir ' + var['scratch'] + \
              '; chmod 777 ' + var['scratch'] + \
              "; sdpFiller infile=" + xmlFullPath + \
              " outfile=" + mirfile_dest

    os.system(command)

    return True

def runFringeCheck(p, var, referenceAntenna=None, pacs=False):
    # Set miriad file name
    miriadFile = var['obsblockName'] + '.mir'

    # Construct miriad command. This has two purposes.
    # "listobs" is used to determine which antennas are online.
    # "uvindex" is used to determine the observing date.
    com = 'cd ' + var['scratch'] + \
          '; ' + 'listobs vis=' + miriadFile + \
          '; uvindex vis=' + miriadFile

    # Execute the command
    (dummy, fout) = os.popen4(com)
    results = fout.read().split('\n')
    fout.close()

    # Loop over the results to find:
    #    a) which antennas are online
    #    b) UT date of the observations. This date is the ending date
    #       of the observations
    ant_online = list()
    utdate = None
    for ir in range(0,len(results)):
        r = results[ir]
        if r.find('Antenna ') == 0:
            ant_online.append(r.split()[1].split(':')[0])
        #if r.find('Total number of records') > 0:
        if r.find('UVINDEX:') == 0:
            # Split row
            j = ir + 7
            if j >= len(results): continue
            r = results[j]
            cols = r.split()
   
            # Get nominal utdate and number of records
            utdate = cols[0]
            nrecords = int(cols[len(cols)-1])
            err = False
            if int(nrecords) == 0:
                err = True
                printError('Miriad data file appears to be empty')
            if utdate.find("*") >= 0:
                err = True
                raise Exception,'Miriad file contains unexpected format for UT date (%s)' % utdate
            if err:
                printWarning('Will continue with fringeCheck anyway...')

    # Reformat utdate
    if utdate <> None: utdate = utdate.replace(':', ' ', 1)

    # Check antenna online
    if len(ant_online) == 0: 
       print 'WARNING: Did not find any antenna online after reading the miriad file'

    # Run fringeCheck.csh
    completed = False
    n = 0
    ant_withdata = list()
    ant_nodata_online = ant_online[:]
    ant_nodata_offline = list()
    useSpecifiedRefant = (referenceAntenna <> None)
    miriadError = True
    bad_windows = ''
    if str(p.conf).upper() == 'FULLSTOKES' :
       fsfc = 1
    else : fsfc = 0
    while not completed and n < len(ant_online):
        # Print message
        if useSpecifiedRefant:
            refant = referenceAntenna
        else:
            refant = string.atoi(ant_online[n])
        print 'Running ' + var['fringeCheck'] + ' on ' + miriadFile + \
              ' with refant=',refant

        # Set command
        com = 'cd ' + var['scratch'] + '; ' + \
             var['fringeCheck'] + ' vis=' + miriadFile + ' output=' + \
             var['output'] + ' refant=' + str(refant)  + ' fsfc=' + str(fsfc)
        print 'Running ',com
        (dummy, fout) = os.popen4(com)
        resultsLog = fout.read()
        results = resultsLog.split('\n')
        fout.close()
        dummy.close()

        # Check for errors
        exitStatus = os.wait()[1]
        miriadError = (exitStatus != 0)

        # Determine if there are any apparent problems with the log file
        # Write results to logfile
        outputLog = var['scratch'] + '/' + var['fringeLog']
        f = open(outputLog, 'w')
        f.write(resultsLog)
        f.close()

        # Determine which antennas are online/offline and with/without data
        ant_withdata = list()
        ant_nodata_online = ant_online[:]
        ant_nodata_offline = list()
        completed = False
        bad_windows = ''
        for r in results:
            # Find bad windows line
            if r.find('*** Bad windows with zero channels :') > 0:
               t = r.split(' ')
               bad_windows = t.pop()

            # Find "Means" line
            if r.find('Means:') != 0: continue

            # Change ****** to 0.0
            r = r.replace('******', '   0.0')

            # Split line
            r = r.split()[1:]
            for i in range(len(r)):
                ant = str(i+1)
                remove_nodata_online = False
                if string.atof(r[i]) == 0.0:
                    try:
                       ant_online.index(ant)
                    except:
                       ant_nodata_offline.append(ant)
                       remove_nodata_online = True
                else:
                    ant_withdata.append(ant)
                    remove_nodata_online = True
                    if string.atoi(ant) == refant: completed = True
                if remove_nodata_online:
                    try:
                        ant_nodata_online.remove(ant)
                    except:
                        pass

        # Increment counter
        if not completed and not useSpecifiedRefant: n += 1
        useSpecifiedRefant = False

    # Remove miriad file
    com = 'rm -rf ' + var['scratch'] + '/' + miriadFile
    os.system(com);

    # Return antennas
    return [ant_withdata, ant_nodata_online, ant_nodata_offline, 
            ant_online, utdate, miriadError, bad_windows]


def checkFiles(var, p):
    """ Checks to see if gif files were created """

    # Initialize root directory
    root = var['scratch'] + '/' + var['obsblockName']

    # Make directory to store each plot. Files are printed in the order
    # they are entered into gifFiles
    gifFiles = dict()

    # Add source amp vs. uvdist plot
    # "nameOrig" is the name of the file created by the miriad script
    # "label" is the label used on the web page
    plot = dict()
    plot['nameOrig']  = root + '.mir_source_uvdist.gif'
    plot['name']  = 'source_uvdist.gif'
    plot['title'] = var['source'] + ": Amplitudes vs. uvdist"
    plot['label'] = 'astronomical source (amp vs uvdist)'
    plot['pdfOrig'] = root + '.mir_source_uvdist_base.pdf'
    plot['pdf'] = 'source_uvdist_base.pdf'
    gifFiles[len(gifFiles)] = plot.copy()

    # Plots for both full-Stokes and dual-pol fringeChecks (LL and RR passband phases/amps)
    if str(p.conf).upper() == 'FULLSTOKES' or str(p.conf).upper() == 'DUALPOL' :
       # Add source spectrum plot - passband amplitudes (RR)
       # "nameOrig" is the name of the file created by the miriad script
       # "label" is the label used on the web page
       plot = dict()
       plot['nameOrig']  = root + '.mir_source_gpplt_pb_amp.gif'
       plot['name']  = 'source_gpplt_pb_amp_RR.gif'
       plot['title'] = var['source'] + ": RR selfcal passband solution (amplitudes)"
       plot['label'] = 'astronomical source (amp vs channel)'
       gifFiles[len(gifFiles)] = plot.copy()

       # Add source spectrum plot - passband phases (RR)
       # "nameOrig" is the name of the file created by the miriad script
       # "label" is the label used on the web page
       plot = dict()
       plot['nameOrig']  = root + '.mir_source_gpplt_pb_pha.gif'
       plot['name']  = 'source_gpplt_pb_pha_RR.gif'
       plot['title'] = var['source'] + ": RR selfcal passband solution (phases)"
       plot['label'] = 'astronomical source (phase vs channel)'
       gifFiles[len(gifFiles)] = plot.copy()

       # Add source spectrum plot - passband amplitudes (LL)
       # "nameOrig" is the name of the file created by the miriad script
       # "label" is the label used on the web page
       plot = dict()
       plot['nameOrig']  = root + '.mir_source_gpplt_pb_amp_LL.gif'
       plot['name']  = 'source_gpplt_pb_amp_LL.gif'
       plot['title'] = var['source'] + ": LL selfcal passband solution (amplitudes)"
       plot['label'] = 'astronomical source (amp vs channel)'
       gifFiles[len(gifFiles)] = plot.copy()

       # Add source spectrum plot - passband phases (LL)
       # "nameOrig" is the name of the file created by the miriad script
       # "label" is the label used on the web page
       plot = dict()
       plot['nameOrig']  = root + '.mir_source_gpplt_pb_pha_LL.gif'
       plot['name']  = 'source_gpplt_pb_pha_LL.gif'
       plot['title'] = var['source'] + ": LL selfcal passband solution (phases)"
       plot['label'] = 'astronomical source (phase vs channel)'
       gifFiles[len(gifFiles)] = plot.copy()

    else :
       # Add source spectrum plot - passband amplitudes
       # "nameOrig" is the name of the file created by the miriad script
       # "label" is the label used on the web page
       plot = dict()
       plot['nameOrig']  = root + '.mir_source_gpplt_pb_amp.gif'
       plot['name']  = 'source_gpplt_pb_amp.gif'
       plot['title'] = var['source'] + ": Selfcal passband solution (amplitudes)"
       plot['label'] = 'astronomical source (amp vs channel)'
       gifFiles[len(gifFiles)] = plot.copy()

       # Add source spectrum plot - passband phases
       # "nameOrig" is the name of the file created by the miriad script
       # "label" is the label used on the web page
       plot = dict()
       plot['nameOrig']  = root + '.mir_source_gpplt_pb_pha.gif'
       plot['name']  = 'source_gpplt_pb_pha.gif'
       plot['title'] = var['source'] + ": Selfcal passband solution (phases)"
       plot['label'] = 'astronomical source (amp vs channel)'
       gifFiles[len(gifFiles)] = plot.copy()

    # Plots for full-Stokes fringeChecks only (XYphase passband)
    if str(p.conf).upper() == "FULLSTOKES" :
       # Add plot - XYphase passband
       # "nameOrig" is the name of the file created by the miriad script
       # "label" is the label used on the web page
       plot = dict()
       plot['nameOrig']  = root + '.mir_xyphase.gif'
       plot['name']  = 'xyphase.gif'
       plot['title'] = var['source'] + ": XYphase passband phases"
       plot['label'] = 'XYphase passband phases'
       gifFiles[len(gifFiles)] = plot.copy()

    # Add source spectrum plot - amplitudes
    # "nameOrig" is the name of the file created by the miriad script
    # "label" is the label used on the web page
    plot = dict()
    plot['nameOrig']  = root + '.mir_source_gpplt_amp.gif'
    plot['name']  = 'source_gpplt_amp.gif'
    plot['title'] = var['source'] + ": Selfcal gain solution (amplitudes)"
    plot['label'] = 'astronomical source (amp vs time)'
    gifFiles[len(gifFiles)] = plot.copy()

    # Add noise plot
    #plot = dict()
    #plot['nameOrig']  = root + '.mir_noise.gif'
    #plot['name']  = 'noise.gif'
    #plot['title'] = 'Noise source'
    #plot['label'] = plot['title']
    #gifFiles[len(gifFiles)] = plot.copy()

    # Add noise amplitude plot
    plot = dict()
    plot['nameOrig']  = root + '.mir_noise_amp.gif'
    plot['name']  = 'noiseamp.gif'
    plot['title'] = 'Noise source amplitude spectrum'
    plot['label'] = plot['title']
    plot['pdfOrig']  = root + '.mir_noise_amp_base.pdf'
    plot['pdf']  = 'noiseamp_base.pdf'
    gifFiles[len(gifFiles)] = plot.copy()

    # Add noise phase plot (selfcal solution)
    plot = dict()
    plot['nameOrig']  = root + '.mir_noise_pha_selfcal.gif'
    plot['name']  = 'noisephase_selfcal.gif'
    plot['title'] = 'Selfcal solution on noise source (phases)'
    plot['label'] = plot['title']
    gifFiles[len(gifFiles)] = plot.copy()

    # Add noise source phase plot (selfcal applied)
    plot = dict()
    plot['nameOrig']  = root + '.mir_noise_pha.gif'
    plot['name']  = 'noisephase.gif'
    plot['title'] = 'Selfcal residuals on noise source (phases)'
    plot['label'] = plot['title']
    plot['sublabel'] = '(<a href="http://cedarflat.mmarray.org/fringe/results/1177033086.2007apr20.0927+390.4/"> example of bad noise source spectrum</a>)'
    plot['pdfOrig']  = root + '.mir_noise_pha_base.pdf'
    plot['pdf']  = 'noisephase_base.pdf'
    gifFiles[len(gifFiles)] = plot.copy()

    # Set file names
    # "name" is the file name stored in the directory
    for f in gifFiles:
       # Gif files
       gifFiles[f]['exists'] = os.path.exists(gifFiles[f]['nameOrig'])
       if gifFiles[f]['exists'] and \
          os.system(UMASK + '; mv -f ' + gifFiles[f]['nameOrig'] + ' ' + \
                    var['fullOutputDir'] + '/' + gifFiles[f]['name']) != 0:
           gifFiles[f]['exists'] = False
           print 'Error moving ' + gifFiles[f]['nameOrig'] + \
                 ' to ' + var['fullOutputDir']
       # PDF files
       if gifFiles[f].has_key('pdfOrig'):
          gifFiles[f]['existsPDF'] = os.path.exists(gifFiles[f]['pdfOrig'])
          if gifFiles[f]['existsPDF'] and \
             os.system(UMASK + '; mv -f ' + gifFiles[f]['pdfOrig'] + ' ' + \
                       var['fullOutputDir'] + '/' + gifFiles[f]['pdf']) != 0:
             gifFiles[f]['exists'] = False
             print 'Error moving ' + gifFiles[f]['pdfOrig'] + \
                   ' to ' + var['fullOutputDir']

    # Done
    return gifFiles


def getFiles(var):
    """ Get list of fringe check files """
    # dirsAll = os.listdir(var['fringeDir'] + '/' + var['fringeSubdir'])
    # dirsAll.sort()
    tmp = glob.glob('%s/%s/??????????.201[1-9]*' % (var['fringeDir'],  var['fringeSubdir']))
    dirsAll =  [f.split('/')[-1:][0] for f in tmp]
    dirsAll.sort()

    return dirsAll


def createArchive(var):
    """ Create archives """
    # Get list of files
    dirsAll = getFiles(var)

    # Create calendar
    result = createArchiveCalendar(var, dirsAll)
    outputHtml = var['fringeDir'] + '/archive.html'
    f = open(outputHtml, 'w')
    f.write(result)
    f.close()

    # Create time
    result = createArchiveTime(var, dirsAll)
    outputHtml = var['fringeDir'] + '/archive_time.html'
    f = open(outputHtml, 'w')
    f.write(result)
    f.close()


def createArchiveTime(var, dirsAll=None, nmax=None, header=None):
    """ Create table in time order of recent fringeCheck results """
    # Get list of files in reverse time order
    if dirsAll == None: dirsAll = getFiles(var)
    dirsAll.reverse()

    # Initialize
    nfiles = 0

    # Start table
    com = header
    if header == None: com = getHtmlHeader()
    com += '\n<center><table border=2>\n' + \
          '<TR bgcolor=yellow>' + \
          '<TH>N</TH>' + \
          '<TH>UT Date</TH>' + \
          '<TH>UT Time</TH>' + \
          '<TH>Source</TH>' + \
          '<TH>Trial</TH>' + \
          '<TH>Subarray</TH>' + \
          '<TH>Frequency</TH>' + \
          '<TH>Comment</TH>' + \
          '</TR>\n'

    # Loop over files
    for d in dirsAll:
        # Skip if this file if it does not contain a timestamp
        z = d.split('.')
        if len(z) != 4 and len(z) != 5: continue
        if not z[0].isdigit(): continue

        # Get information
        sourcename = z[2]
        trial = z[3]
        subarray = "<BR>"
        if len(z) == 5: subarray = z[4]

        # Get subarray and LO frequency
        date   = z[1]
        subarray = "<BR>"
        lofreq = "<BR>"
        t = z[1].split('_')
        if len(t) == 3:
           date = t[0]
           subarray = t[1]
           lofreq = t[2]
        # Convert ut time
        ut = time.asctime(time.gmtime(string.atoi(z[0]))).split()
        ut_date = ut[1] + ' ' + ut[2] + ', ' + ut[4]
        ut_time = ut[3]

        # Read comment string
        fn = "%s/%s/%s/index.html" % (var['fringeDir'], var['fringeSubdir'], d)
        if not os.path.exists(fn): continue
        comment = readCommentString(fn)
        if comment == "" or comment==None or comment == "None": comment = "<BR>"

        # Print entry
        nfiles += 1
        com += "<TR"
        if subarray == "2": com += " bgcolor=\"#E0E0E0\""
        com += '><TH bgcolor="#C0FFC5">%d</TH><TD>%s</TD><TD>%s</TD><TD><a href="%s/%s/%s/index.html">%s</a></TD><TD align=center>%s</TD><TD align=center>%s</TD><TD align=center>%s</TD><TD>%s</TD></TR>' % \
            (nfiles, ut_date, ut_time, var['fringeDirHtml'], var['fringeSubdir'], d, sourcename, trial, subarray, lofreq,comment)

        # If reached limit, then we are done
        if nmax <> None and nfiles >= nmax: break

    # If no files were printed, then say so
    if nfiles == 0:
        com += '<TR bgcolor=red><TD colspan=5><font color=yellow>No files found</font></TD></TR>\n'

    # Close file
    com += '</table></center>\n'

    # Return result
    return com


def createArchiveCalendar(var, dirsAll):
    """ Create archive of previous fringe checks """
    # Get list of files
    dirsAll.sort()
    usedDir = [False] * len(dirsAll)

    # Loop through the directories and find years
    dirs      = list()
    years     = list()
    dirYears  = list()
    dirMonths = list()
    dirDays   = list()
    dirTrials = list()
    dirSource = list()
    for d in dirsAll:
        # Get the fields. The original fringe checks add only the dates.
        # We are ignoring those now.
        z = d.split('.')
        if len(z) < 3: continue

        # Get the date field. I added in midstream the time, which may appear
        # in the first field
        f = z[1]
        if len(z) == 3: f = z[0]
        if not f[0:4].isdigit(): continue
        y = string.atoi(f[0:4])
        if y not in years: years.append(y)

        # Get source name
        sourcename = z[1]
        if len(z) >= 4: sourcename = z[2]

        # Save
        dirs.append(d)
        dirYears.append(y)
        dirMonths.append(string.upper(f[4:7]))
        dirDays.append(string.atoi(f[7:9]))
        dirTrials.append(z[len(z)-1])
        dirSource.append(sourcename)
    years.reverse()

    # Start table
    com = getHtmlHeader()
    com += '<center><table border=0>\n'

    # Loop over years
    months = [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 
               'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
    for y in years:
        # Start year
        com += '<TR><TH colspan=13><H2>' + str(y) + '</h2></TH></TR>\n'
        com += '<TR><TH>Day</TH>'
        for m in months:
            com += '<TH>' + m + '</TH>'
        com += '</TR>\n'

        # Initialize entry
        startedRow = False
        usedMonths = [False] * len(months)

        # Loop over days
        for day in range(1,32):
            # Loop over months
            entryInRow = True
            foundDay = False
            while entryInRow:
                entryInRow = False
                com_row = '<TR><TH>' + str(day) + '</TH>'
                for m in months:
                    # Loop over directories
                    mupper = string.upper(m)
                    foundEntry = False
                    com_row += '<TD nowrap>'
                    for i in range(len(dirs)):
                        # Print any entries matching day, month, and year
                        if usedDir[i] or mupper != dirMonths[i] or \
                           day != dirDays[i] or y != dirYears[i]: continue

                        # Print entry
                        foundEntry = True
                        entryInRow = True
                        foundDay = True
                        com_row += '<a href="' + var['fringeSubdir'] + \
                                '/' + dirs[i] + '/">' + \
                                m + ' ' + str(dirDays[i]) + ' (' + \
                                dirSource[i] + ' ' + dirTrials[i] + ')</a>'
                        usedDir[i] = True
                        break

                    # If did not find an entry, print blank row
                    if not foundEntry: com_row += '<BR>'
                    com_row += '</TD>'
                com_row += '</TR>\n'
                if not (foundDay and not entryInRow): com += com_row

    # End table
    com += '</table></body></html>\n'

    # Done
    return com
