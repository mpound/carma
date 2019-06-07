# $Id: intApeffAnalysis.py,v 1.19 2012/12/19 16:42:42 obs Exp $
#
# This script contains functions for analyzing aperture efficiency
# from interferometric measurements.
#
# It was derived from a collection of scripts and programs by
# Misty LaVigne and Stephen White, UMd.
#
# @Author James W. Lamb, Caltech
#
# History
# 28-Aug-2010: JWL  Corrected header line for SBR.
# 25-Aug-2010: JWL  Modified to handle 8 spectral line bands.
# 05-May-2010: JWL  Changed number of antennas to 23 to match Miriad output
# 21-Aug-2009: JWL  Raise exception and exit if error in shell commands
# 08-May-2009: JWL  Fixed bug in getting source
# 16-Oct-2008: JWL  Check for zero gains. Record average elevation.
# 21-Sep-2008: JWL  Fixed bug in window frequency parsing. Fixed flux bug.
# 19-Sep-2008: JWL  Added subdirectory for year. Check for bad data.
#                   Write new results files rather than overwriting if name is the same.
#                   Added average tau value to output data
# 09-Sep-2008: JWL  Changed 'gblist' to 'gplist' (standard Miriad version)
# 08-Sep-2008: Original version
#

import os
import math
import numpy as np
import carmaConstants as CONSTS
import time
import sysCommand as sy

def analyzeInterfApEff(mirFile = None, outPath = None, refAnt = 13, flux = None) :
  try :
    """ Calculates the aperture efficiency from interferometric measurements of a known source.

        Parameters

            mirFile
                Miriad dataset containing the measurements. This may be a fully
                qualified path (normally "/array/rt/apertureEff/data/<mir data>").
                All temporary files will be written in the same directory.

            outPath
                Directory for storing the results (should normally be set to
                "/array/rt/web/interferometricApEff/<year>"). If not supplied, the
                path supplied with 'mirFile' is used.

            refAnt = 13
                Antenna to use for the phase reference

            flux = None
                Flux for the source used as the standard. If there are multiple sources in
                the file, this will be used for all of them. It is not applied to planets.
                If no flux is supplied, the default Miriad value is assumed.

    """

    idString = "intApeffAnalysis.py, " + "$Revision: 1.19 $".strip('$')
    print("\nStarting %s\n" % idString)
    print "=================================================="
    print "Reduction and Analysis of Aperture Efficiency Data"
    print "=================================================="

    if (mirFile == None) or (refAnt == None) :
        print "\n\nHELP Info:\n", analyzeInterfApEff.__doc__
        err = True
        return err

    planets = ["MARS", "URANUS", "JUPITER", "VENUS", "NEPTUNE"]
    numAntennas = 23
    antennas = range(1, numAntennas + 1)
    numWindows = 15
    windows = range(1, numWindows + 1)

    #----Check and manage paths and directories---
    saveCwd = os.getcwd()
    if mirFile[-1] == '/' :
        mirFile = mirFile[0 : -1]
    baseDir = os.path.dirname(mirFile)
    if os.path.exists(baseDir) :
        os.chdir(baseDir)
    else :
        print "*** Warning: %s does not exist. Trying current directory" % baseDir

    print "Working directory: %s" % os.getcwd()

    if outPath == None :
        outPath = "./"
    if outPath[-1] != '/' :
        outPath += '/'
    if not os.path.exists(outPath) :
        outPath = "./"
    print "Logging directory: %s" % outPath

    # Although the Miriad data set is actually a directory with multiple files,
    # we treat it as though it were a file since we don't need to know anything
    # about its structure.
    mirFile = os.path.basename(mirFile)
    if not os.path.exists(mirFile) :
        print "Error: %s not found" % mirFile
        err = True
        return err
    print "Miriad data set : %s" % mirFile
    print "Reference Antenna: %d" % refAnt
    if (flux == None) or (flux == 0.0) :
        print "Flux: Not specified"
    else :
        print "Flux: %.3f Jy" % flux

    #----Organize data---

    print "\nSplitting data by window"
    sy.runSysCmd("listobs vis=%s log=tempfile.tmp" % mirFile, line1Char = '-', \
                 color = 'blue')
    listobsFile = open("tempfile.tmp")
    listobsData = listobsFile.readlines()
    listobsFile.close()
    sy.runSysCmd("rm -rf tempfile.tmp")
    for line in range(len(listobsData)) :
        if listobsData[line].find("Chronology") != -1 :
            date = listobsData[line].split()[4]
            break
    print date

    sy.runSysCmd("uvlist vis=%s options=spec log=tempfile.tmp" % mirFile, \
                 line1Char = '-', color = 'blue')
    uvlistFile = open("tempfile.tmp")
    uvlistData = uvlistFile.readlines()
    uvlistFile.close()
    sfreq = []
    sy.runSysCmd("rm -rf tempfile.tmp")
    count = 0
    for line in range(len(uvlistData)) :
        if uvlistData[line].find("starting frequency") != -1 :
            i = 3
            for j in range(6) :
                sfreq.append(uvlistData[line].split()[i])
                count += 1
                i += 1
                if (count >= numWindows) :
                    break

    # Plot the tau 230 to a PS file
    sy.runSysCmd("smavarplt vis=%s yaxis=tau230 device=%s%s.tau230.ps/cps" % \
              (mirFile, outPath, date) + " dotsize=8 log=tempfile.tmp", \
              line1Char = '-', color = 'blue')

    tauFile = open("tempfile.tmp")
    tauData = tauFile.readlines()
    tauFile.close()
    sy.runSysCmd("rm -rf tempfile.tmp")
    tau = 0.0
    i = 0
    for line in range(len(tauData)) :
        if tauData[line][0] != "#" :
            vals = tauData[line].split()
            tau += float(vals[2])
            i += 1
    tau /= i
    print "Average tau(230) = %.3f" % tau

    #----Find all sources in data set----
    print "Running uvio to identify sources in the file."
    sy.runSysCmd("/opt/carmaTools/bin/uvio vis=%s > tempfile.tmp" % mirFile, line1Char = '-', color = 'blue')
    uvioFile = open("tempfile.tmp")
    uvioData = uvioFile.readlines()
    uvioFile.close()
    sy.runSysCmd("rm -rf tempfile.tmp")
    sources = []
    for line in range(len(uvioData)) :
        if uvioData[line].find("DATA: source") != -1 :
            source = uvioData[line].split()[3]
            if not source in sources :
                sources.append(source)
    sources.sort()
    print "Sources found: %s" % sources

    for source in sources :
        for win in windows :
            winFile = "sou%swin%d.mir.tmp" % (source, win)
            sy.runSysCmd("rm -rf %s" % winFile)
            print "Splitting out data for %s, window %d ..." % (source, win)
            sy.runSysCmd("uvcat vis=%s out=%s 'select=source(%s),win(%d),-auto' > /dev/null" % \
                     (mirFile, winFile, source, win), line1Char = '-', color = 'blue')
    print
    print "=================================================="
    print "Data Reduction"
    print "=================================================="

    print "----------------------"
    print "Bandpass calibration"
    print "----------------------"

    for source in sources :
        for win in windows :
            winFile = "sou%swin%d.mir.tmp" % (source, win)
            bpFile = "sou%swin%d.bp.mir.tmp" % (source, win)
            sy.runSysCmd("rm -rf %s" % bpFile)
            sy.runSysCmd("mfcal vis=%s refant=%d interval=0.2" % (winFile, refAnt), \
                         line1Char = '-', color = 'blue')
            sy.runSysCmd("uvcat vis=%s options=nocal out=%s" % (winFile, bpFile), \
                         line1Char = '-', color = 'blue')

    print "------------------------------"
    print "Caculating gains using selfcal"
    print "------------------------------"

    for source in sources :
        if (source.upper() in planets) or (flux == None) or (flux == 0.0) :
            print "\n##### Using a planet\n"
            options = "amp,apr,nosc"
            fluxOption = ""
        else :
            options = "amp,nosc"
            fluxOption = "flux=%s" % flux
        for win in windows :
            print "Source: %s, window: %d" % (source, win)
            bpFile = "sou%swin%d.bp.mir.tmp" % (source, win)
            gainsFile = "sou%swin%d.gains.tmp" % (source, win)
            sy.runSysCmd("rm -rf %s" % gainsFile)
            sy.runSysCmd("uvlist vis=%s options=spectra log=tempfile.tmp" % bpFile, \
                         line1Char = '-', color = 'blue')

            uvlistFile = open("tempfile.tmp")
            uvlistData = uvlistFile.readlines()
            uvlistFile.close()
            sy.runSysCmd("rm -rf tempfile.tmp")
            for line in range(len(uvlistData)) :
                if uvlistData[line].find("number of channels") != -1 :
                    numChan = int(uvlistData[line].split()[4])
            sy.runSysCmd("selfcal interval=0.1 vis=%s line=chan,1,1,%d,%d options=%s refant=%d %s" % \
                     (bpFile, numChan, numChan, options, refAnt, fluxOption), line1Char = '-', color = 'blue')
            print("selfcal interval=0.1 vis=%s line=chan,1,1,%d,%d options=%s refant=%d %s" % \
                     (bpFile, numChan, numChan, options, refAnt, fluxOption))
            sy.runSysCmd("gplist vis=%s options=amp > %s" % (bpFile, gainsFile), line1Char = '-', color = 'blue')

    print "------------------------------"
    print "Extracting Tsys values"
    print "------------------------------"

    tsys = []
    src = 0
    for source in sources :
        tsys.append(np.zeros([numWindows, numAntennas]))
        for win in windows :
            winFile = "sou%swin%d.mir.tmp" % (source, win)
            tsysFile = "sou%swin%d.tsys.tmp" % (source, win)
            sy.runSysCmd("rm -rf %s" % tsysFile)
            sy.runSysCmd("varplt vis=%s yaxis=systemp log=%s" % (winFile, tsysFile), line1Char = '-', color = 'blue')
            tsysF = open(tsysFile)
            tsysData = tsysF.readlines()
            tsysF.close()
            sy.runSysCmd("rm -rf tempfile.tmp")
            count = 0
            for line in range(len(tsysData)) :
                if tsysData[line][0] != '#' :
                    dataList = tsysData[line].split()
                    if len(dataList) > 4 :
                        i = 0
                        count += 1
                        dataList = dataList[2 : 6]
                    for val in dataList :
                        tsys[src][win - 1][i] += float(val)
                        i += 1
            for ant in antennas :
                tsys[src][win - 1][ant - 1] /= count
        src += 1

    print "------------------------------"
    print "Extracting gain values"
    print "------------------------------"

    gains = []
    src = 0
    for source in sources :
        gains.append(np.zeros([numWindows, numAntennas]))
        for win in windows :
            gainsFile = "sou%swin%d.gains.tmp" % (source, win)
            gainsF = open(gainsFile)
            gainsData = gainsF.readlines()
            gainsF.close()
            for line in range(4, len(gainsData)) :
                if gainsData[line].find("Medians:") != -1 :
                    dataList = gainsData[line]
                    i = 10
                    for ant in antennas :
                        try :
                            gains[src][win - 1][ant - 1] = float(dataList[i : i + 7])
                            if gains[src][win - 1][ant - 1] == 0.0 :
                                gains[src][win - 1][ant - 1] = 1.0e6
                        except ValueError :
                            gains[src][win - 1][ant - 1] = 1.0e6
                        i += 7
                    break
        src += 1

    print "------------------------------"
    print "Extracting elevation values"
    print "------------------------------"

    meanElList = []
    src = 0
    for source in sources :
        # Extract data set for this source
        if os.path.exists("tempData.mir") :
            sy.runSysCmd("rm -rf tempData.mir")
        sy.runSysCmd("uvcat vis=%s 'select=source(%s)' out=tempData.mir" %(mirFile, source), \
                     line1Char = '-', color = 'blue')
        sy.runSysCmd("varplt vis=tempData.mir yaxis=antel log=varlog.tmp", \
                     line1Char = '-', color = 'blue')
        varLog = open("varlog.tmp")
        elData = varLog.readlines()
        varLog.close()

        els = []
        meanEl = 0.0
        count = 0
        for line in range(4, len(elData)) :
            i = 13
            if elData[line][0 : 3] == " 0 " :
                if len(els) > 0 :
                    meanEl += np.median(els)
                    count += 1
                elList = elData[line]
                els = np.zeros(numAntennas)
                ant = 1
            for j in range(4) :
                if ant > numAntennas :
                    break
                try :
                    els[ant - 1] = float(elList[i : i + 15])
                except ValueError :
                    els[ant - 1] = 0.0
                i += 15
                ant += 1

        src += 1
        meanElList.append(meanEl / count)

    print "=================================================="
    print "Data Analysis"
    print "=================================================="

    # Calculate scale factors based on antenna diameter
    conv = 2.0 * CONSTS.K * math.pi / CONSTS.JY / (math.pi**2 / 4.0) * 100
    eff = []
    src = 0
    for source in sources :
        eff.append(np.zeros([numWindows, numAntennas]))
        jyPerK = gains[src]**2 / tsys[src]
        for win in windows :
            for ant in antennas :
                if ant < 7 :
                    fact = conv / CONSTS.OVRO_DIA**2
                else :
                    fact = conv / CONSTS.BIMA_DIA**2
                eff[src][win - 1][ant - 1] = fact / jyPerK[win - 1][ant - 1]
        src += 1
    src = 0
    ef = np.zeros(numWindows)
    for source in sources :

        # Create file for output data
        if float(sfreq[0]) < 120 :
            band = "3mm"
        else :
            band = "1mm"
        effFile = "%s.%s.%s.eff" % (date, band, source)
        seq = 0
        while os.path.exists(outPath + effFile) :
            seq += 1
            effFile = "%s.%s.%s.%d.eff" % (date, band, source, seq)

        df = open(outPath + effFile, "w")
        print "Saving data to file: %s" % effFile
        df.write(37 * "-" + "\nInterferometric Aperture Efficiencies\n" + 37 * "-" + "\n")
        df.write("\n%s\n" % idString)
        df.write("\nData set: %s\nDate: %s\n" % (mirFile, date))
        df.write("\nSource: %s\n" % source)

        # If a flux is not supplied, look up value in Miriad database
        if source.upper() not in planets :
            if (flux == 0.0) or (flux == None) :
                sy.runSysCmd("calflux source=%s date=0 freq=%s delfreq=30 >> tempfile.tmp" % \
                             (source, sfreq[1]), line1Char = '-', color = 'blue')
                listFlux = open("tempfile.tmp")
                fluxData = listFlux.readlines()
                listFlux.close()
                sy.runSysCmd("rm -rf tempfile.tmp")
                for line in range(len(fluxData)) :
                    if fluxData[line].find("Flux of:") != -1 :
                        df.write("Flux from Miriad: %s Jy\n" % fluxData[line].split()[7])
            else :
                df.write("Flux: %.3f Jy\n" % flux)
        df.write("\nReference antenna: %d\n" % refAnt)
        df.write("\nWindow start frequencies:\n")
        for win in windows :
            df.write("   Win%02d: %s GHz\n" % (win, sfreq[win - 1]))
        df.write("\nAverage tau(225): %.3f" % tau)
        df.write("\nAverage elevation: %.1f deg\n" % meanElList[src])
        df.write("\nSide band ratio (SBR) is (eff LSB)/(eff USB)\n")
        df.write("\nAnt  ")
        for win in windows :
            df.write("Win%02i " % win)
        df.write("Mean  STDEV    ")
        for win in windows :
            df.write("Tsys%02i   " % win)
        for win in range(numWindows // 2) :
            df.write("SBR%02i " % (win + 1))
        df.write("\n")
        for ant in antennas :
            df.write("%2d " % ant)
            for win in windows :
                ef[win - 1] = eff[src][win - 1][ant - 1]
                if ef[win - 1] < 0.1 :
                    df.write("  ----")
                else :
                    df.write("%6.1f" % ef[win - 1])
            efMean = np.mean(ef, 0)
            efStd = np.std(ef, 0)
            if efMean < 0.1 :
                df.write("   ----   ----")
            else :
                df.write(" %6.1f %6.1f" % (efMean, efStd))
            for win in windows :
                ef[win - 1] = eff[src][win - 1][ant - 1]
                df.write(" %8.1f" % tsys[src][win - 1][ant - 1])
            df.write(" ")

            # Calculate sideband ratios from USB and LSB efficiencies
            for win in range(numWindows // 2) :
                e1 = eff[src][win + numWindows // 2][ant - 1]
                e2 = eff[src][win][ant - 1]
                if e1 > 0.0 :
                    sbr = e2 / e1
                    if efMean < 0.1 :
                        df.write("  ----")
                    else :
                        df.write(" %5.2f" % sbr)
                else :
                    sbr = 1.0
                    df.write("  ----")
            df.write("\n")
        df.write(time.strftime("\n\nProcessed: %d-%b-%Y, %H:%M:%S UT\n", time.gmtime()))
        df.close()

        src += 1
    sy.runSysCmd("rm -rf *.tmp")
    os.chdir(saveCwd)

    err = False
    return err

  except sy.ShellCommandError :
    print "\n*** There was some error running the above command -- please check"
  except :
    print "\n*** Unexpected error"
    raise

# end apeff.py
