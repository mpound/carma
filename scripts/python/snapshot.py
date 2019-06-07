# snapshot.py
#
# @author John Carpenter
# $Id: snapshot.py,v 1.23 2013/05/01 04:05:18 jmc Exp $
#
#   History
#   2007-Sep-15  JMC    Original

import string
import os
import copy

import subarrayCommands as commands
import obsdefUtils as utils

DB_DEFAULT = "snapshot"    # Default database name
DB_MOSAIC_DEFAULT = "snapshotmosaic"    # Default database name

def execdb(com, db=DB_DEFAULT):
    # Execute command
    os.environ['PGUSER']     = 'obs'
    os.environ['PGPASSWORD'] = 'amrac15'
    os.environ['PGHOST']     = 'cedarflat'
    if db == None: db = DB_DEFAULT
    command = '/usr/bin/psql ' + db + ' -t -c "' + com + '"'
    fout = os.popen(command)
    result = fout.read()

    # Split result by end of line character
    rows = result.split("\n")

    # Split lines
    tokens = dict()
    for j in range(len(rows)):
        # Remove blank lines
        r = rows[j]
        if string.strip(r) == "": continue

        # Get columns
        columns = r.split("|")

        # Trim each entry
        for i in range(len(columns)):
            columns[i] = string.strip(columns[i])

        # Save
        tokens[j] = columns[:]
    
    # Done
    return copy.copy(tokens)


def addTimeDatabase(sid, tint, utstart, nant, obsblock, db,
                    tau=None, phase=None, isuseable=True):
    """ Add time stamp into data base for an observation """
    # Set phase and tau if needed
    if phase == None or phase == '?': phase = 'NULL'
    if tau   == None or tau   == '?': tau   = 'NULL'

    # Is it useable?
    useable = 1
    if isuseable == False: useable = 0

    # Set command
    com = "insert into observations (sid,utstart,tint,nant,tau,phase,obsblock,useable) " + \
           "values(" + str(sid) + \
          ",'" + utstart + "'" + \
          "," + str(tint) + \
          "," + str(nant) + \
          "," + str(tau) + \
          "," + str(phase) + \
          ",'" + str(obsblock) + "'" + \
          "," + str(useable) + \
          ")"

    # Execute command
    execdb(com, db=db);


def addMosaicDatabase(sid, utstart, startingIndex, endingIndex, nposObserved, 
           nant, obsblock, db, tau=None, phase=None, isuseable=True):
    """ Add time stamp into database for an observation """
    # Set phase and tau if needed
    if phase == None or phase == '?': phase = 'NULL'
    if tau   == None or tau   == '?': tau   = 'NULL'

    # Is it useable?
    useable = 1
    if isuseable == False: useable = 0

    # Set command
    com = "insert into observations (sid,utstart,startIndex,endIndex,npos,nant,tau,phase,obsblock,useable) " + \
           "values(" + str(sid) + \
          ",'" + utstart + "'" + \
          "," + str(startingIndex) + \
          "," + str(endingIndex) + \
          "," + str(nposObserved) + \
          "," + str(nant) + \
          "," + str(tau) + \
          "," + str(phase) + \
          ",'" + str(obsblock) + "'" + \
          "," + str(useable) + \
          ")"

    # Execute command
    execdb(com, db=db);


def setUnuseable(project, interval, db=DB_DEFAULT):
    """ Sets observations as unuseable if there were taken more than 
        'interval' ago.

        Inputs
           project :  project name in snapshot database
           interval:  interval in postgresql format
           db      :  database name

        Example:
           setUnuseable('cx198a', '12 hours')
    """

    # set command
    com = "update observations set useable=0 from projects,sources where observations.sid=sources.sid and sources.pid=projects.pid and projects.project='%s' and timestamp '%s' - utstart > interval '%s'" % (project, utils.getUT(timestamp=True), interval)

    # Execute command
    execdb(com, db=db)


def findTuneSnap(snapFreq, snapSB, snapIF, used=None):
    """ Finds the next tuning for the multifrequency snapshot programs.

        The default is to pick the current tuning if it is in the tuning list and not
        used. Otherwise, the first available tuning is used.

        Inputs
            snapFreq : List of rest frequencies in GHz
            snapSB   : List of sidebands (USB, LSB) for each snapFreq
            snapIF   : List of IF frequencies (GHz) for each snapFreq
            used     : List of snapFreq that have already been used.

        Output: [found, indx, current]
            found   - indicates true of false if the next tuning parameters were found
            indx    - The indx of the tuning parameter if found = True. Otherwise indx = -1
            current - True/False if found tuning is current.
    """

    # Get current frequency setup
    [frest, fif, flo] = commands.freqSetup()
    sb = commands.USB
    if flo > frest: sb = commands.LSB

    # Loop over snapshot frequencies to see if current tuning is available
    result = [False, -1, False]
    for i in range(len(snapFreq)):
        if (used == None or not used[i]):
            # Initialize
            ok = True

            # Check frequency
            if abs(snapFreq[i] - frest) > 0.01: ok = False

            # Check SB
            if snapSB <> None and snapSB[i] != sb: ok = False

            # Check IF
            if snapIF <> None and abs(snapIF[i]-fif) > 0.001: ok = False

            # Is this tuning ok?
            if ok:
                result = [True, i, True]
                break
            elif not result[0]:
                result = [True, i, False]

    # Done
    return result


def tuneSnap(io, snapFreq, snapSB, snapIF, snapSource, fresto=None, indent=1):
    """ Tune to the ith frequency in snapFreq """

    # Get current frequency setup
    [frest, fif, flo] = commands.freqSetup()
    if fresto <> None: frest = fresto
    sidebandLO1 = commands.USB
    if flo > frest: sidebandLO1 = commands.LSB

    # Get delta frequencies from frest for each correlator band
    config = utils.getConfigband()
    configNew = utils.getConfigband()
    dfreq = [0.0] * len(config)
    for i in range(len(config)):
        dfreq[i] = frest - config[i][3]

    # See if this frequency has been observed
    if abs(snapFreq[io] - frest) <= 0.01: return False

    # Get sideband
    sbnew = sidebandLO1
    if snapSB <> None: sbnew = snapSB[min([io, len(snapSB)-1])]

    # Get IF
    ifnew = fif
    if snapIF <> None: ifnew = snapIF[min([io, len(snapIF)-1])]

    # Set new correlator configuration
    sgn = 1.0
    if sbnew != sidebandLO1: sgn = -1
    for j in range(len(configNew)):
        configNew[j][3] = snapFreq[io] + sgn * dfreq[j]

    # Set new correlator configuration
    for j in range(len(configNew)):
        configNew[j][4] = commands.AUTO

    # Set correlator
    utils.setConfigband(configNew)

    # Tune
    commands.trackMessage("Tuning freq=" + str("%.3f" % snapFreq[io]) + " GHz, " + 
          str(sbnew)  + ", IFfreq=" + str("%.3f" % ifnew) + " GHz", indent=indent)
    commands.freq(snapFreq[io], sbnew, ifnew, utils.makeListObsdef(snapSource)[0])
    commands.checkbands()

    # Done
    return True


def startTrial(db=DB_DEFAULT, oname=""):
    """ Start trial in pdb """

    # Get full obsblock name
    if oname == "": oname = utils.getObsblockName()
    tokens = oname.split('.')

    # Get project
    project = tokens[0];

    # Get obsblock
    obsblock = tokens[1];

    # Get trial
    trial = int(tokens[len(tokens)-1])

    # Get ut date
    ut = utils.getUT(timestamp=True)

    # Set command
    com = "insert into trials (oid,pid,trial,utstart,fn) select o.oid,p.pid,%d,'%s','%s' from obsblocks as o,projects as p where o.pid=p.pid and project ilike '%s' and obsblock ilike '%s'" % (trial,ut,oname,project,obsblock)

    # Execute command
    execdb(com, db);
