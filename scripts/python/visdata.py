# @file
# Visibility data retrieval and processing
# @author Steve Scott
# $Id: visdata.py,v 1.23 2012/02/06 21:24:38 friedel Exp $

#
# $CarmaCopyright$
#
"""Module: visdata.py"""

from subarrayCommands import *
import obsdefUtils as odutils
import time

class Complex(object) :
    """Complex data"""
    def __init__(self, x):
        """Constructor:
        Parameter:
          x: interpretation depends on variable type
              list: assumed to contain real/imag pair
              int: assumed to be a phase - converted to unitVector
              float: assumed to be a phase - converted to unitVector"""
        self.radPerDeg = math.pi/180  
        self.degPerRad = 180/math.pi    
        if self.__isnumeric(x) :
            self.__setPhase(x)
        elif isinstance(x, types.ListType) :
            if len(x) <> 2:
                m  = "Complex initializer must be a list of exactly"
                m += " two elements"   
                raise Exception, m
            self.__setValue(x[0], x[1])    
        elif isinstance(x, Complex) :
            self.__setValue(x.real, x.imag)
        else :
            m = "Illegal arguments to constructor for Complex\n"
            m += Complex.__init__.__doc__       
            raise Exception, m
    def getPhase(self) :
        "Returns phase in deg; zero if both real and imag are zero"
        return self.degPerRad*math.atan2(self.imag, self.real)
    def getPhaseDiff(self, x) :
        "Input a complex number, Returns phase difference in deg"
        return self.getPhaseDiffComplex(x).getPhase()
    def getPhaseDiffComplex(self, x) :
        "Returns self/x (after first converting to unit vectors)"
        if not isinstance(x, Complex) :
            st = str(type(x)) 
            raise Exception, "Input must be of type Complex not %s" %st         
        return self.getUnit()/x.getUnit()
    def conjugate(self) :
        "Change sign of imaginary part, effectively changing sign of phase"
        self.imag = -self.imag
    def norm(self) :
        "Normalize vector; turn into unit vector with same phase"
        try :
            a = 1.0/self.__abs__()
        except:
            a = 0    
        self.real *= a
        self.imag *= a 
    def getUnit(self):
        "Returns a unit vector with same phase"
        try :
            a = 1.0/self.__abs__()
        except:
            return Complex([1,0])    
        return Complex([a*self.real, a*self.imag])                
    def __setValue(self, re, im) :
        self.real = float(re)
        self.imag = float(im)
    def __setPhase(self, p) :
        prad = p*self.radPerDeg
        self.__setValue(math.cos(prad), math.sin(prad))
    def __abs__(self) :
        "Return length of vector"
        return math.hypot(self.real, self.imag)
    def __str__(self) :
        return "(" + str(self.real) + ", " + str(self.imag) + ")"    
    def __repr__(self) : return self.__str__()
    def __radd__(self, x) :
        return self.__add__(x)
    def __add__(self, x) :
        if isinstance(x, Complex) :
            return Complex([x.real+self.real, x.imag+self.imag])
        else :
            st = str(type(x)) 
            raise Exception, "Add of Complex and %s not supported" %st    
    def __rsub__(self, x) :
        if isinstance(x, Complex) :
            return Complex([x.real-self.real, x.imag-self.imag])
        else :
            st = str(type(x)) 
            raise Exception, "Subtraction of %s and Complex not supported" %st    
    def __sub__(self, x) :
        if isinstance(x, Complex) :
            return Complex([self.real-x.real, self.imag-x.imag])
        else :
            st = str(type(x)) 
            raise Exception, "Subtraction of Complex and %s not supported" %st    
    def __rmul__(self, x) :
        return self.__mul__(x)
    def __mul__(self, x) :
        rtn = Complex([0,0])
        if self.__isnumeric(x) :
            rtn.__setValue(x*self.real, x*self.imag)
            return rtn
        if isinstance(x, Complex) :
            r = self.real*x.real - self.imag*x.imag
            i = self.real*x.imag + self.imag*x.real
            rtn.__setValue(r,i)
            return rtn  
        else :
            st = str(type(x)) 
            raise Exception, "Multiply of Complex and %s not supported" %st    
    def __div__(self, x) :
        """Return self/x; x can be numeric or Complex"""
        if self.__isnumeric(x) :
            r = 0
            i = 0
            try :
                r = self.real/x
                i = self.imag/x
            except: pass
            return Complex([r,i])    
        if isinstance(x, Complex) :
            try:
                a = 1.0/x.__abs__()
                a2 = a*a
            except:
                a2 = 0                    
            r = (self.real*x.real + self.imag*x.imag)*a2
            i = (-self.real*x.imag + self.imag*x.real)*a2
            return Complex([r,i])
        else :
            st = str(type(x)) 
            raise Exception, "Divide of Complex by %s not supported" %st    
    def __rdiv__(self, x) :
        """Return x/self; x can be numeric or Complex"""
        #print "Doing rdiv"
        if self.__isnumeric(x) or isinstance(x, Complex) :
            try:
                # Make an inverse of self
                inv = self.getUnit()
                inv.conjugate()
                inv /= self.__abs__()
            except:
                inv = Complex([0,0])    
            return inv.__mul__(x)    
        else :
            st = str(type(x)) 
            raise Exception, "Divide of %s by Complex not supported" %st    
    def __isnumeric(self, x) :
       return isinstance(x, types.IntType) or isinstance(x, types.FloatType)
   
class VisdataBase(object) :
    """Gets the last integrated antenna based visibilities"""
    def __init__(self, nBands=3, nAnts=15):
        """Constructor:
        Parameter:
          nBands: number of bands (default=3)
          nAnts:  number of antenna (default=15)"""
        self.nBands     = nBands
        self.nAnts      = nAnts
        self.calibrated = False
        self.grandAveAmp   = list()
        self.grandAvePhase = list()
        for a in range(nAnts) : 
            self.grandAveAmp.append(0.0)             # Scalar
            self.grandAvePhase.append(Complex(0.0))
        self.lastSourceOutput = ""
        # Time of getting data (not time data was taken)
        self.getDataTimestamp = time.time()
        self.getDataLocaltime = time.localtime(time.time())
        
        # Get lists of MP names to query
        self.mpvis = []  # MP names for vis data
        self.mpsnr = []  # MP names for vis data SNR
        self.mptsys = [] # MP names for tsys
        for b in range(self.nBands):
            self.mpvis.append(self._plNames(b+1, "vis"))                  
            self.mpsnr.append(self._plNames(b+1, "snr"))                  
            self.mptsys.append(self._plNames(b+1, "tsys"))                  
        #for i in self.mptsys: print i    
        last = ["SlPipeline.LastIntegration.source"]            
        last.append("SlPipeline.LastIntegration.integNumber")          
        s1 = "Control.Subarray1."
        last.append(s1 + "project")
        last.append("SlPipeline.IntegratorStageContainer.IntegratorStage.desiredIntegTime")
        last.append("Control.Subarray1.timestamp")
        last.append("OpacityMonitor.tau225")
        last.append("PhaseMonitor.skyRMS")
        fc  = s1+"Commands.Freq."
        freq = [s1+"loFreq",fc+"restFreq",fc+"ifFreq",fc+"sideband"]
        bs = "Control.SpectralLineCorrelator.SlcBand%d.ControlBandPoints."
        band = list()
        for i in range(self.nBands):
            b   = bs %(i+1)
            mps = [b+"bandwidth", b+"lo2Freq",b+"ifFreq",b+"lo2Sideband"]
            band.append(mps)
            
        # Get the data and put it into r   
        r = self.atomicQuery([last, freq, band, self.mpvis, self.mpsnr, self.mptsys]) 
        # Unpack the data
        self.source = r[0][0]
        if self.source == None: self.source = "None"
        else :                  self.source = self.source.rstrip("\0")  
        self.integ     = r[0][1]         
        self.proj      = r[0][2] 
        self.integTime = r[0][3]
        self.msTime    = helpers.mjd2ctime(r[0][4])
        self.tau       = r[0][5]
        self.skyrms    = r[0][6]
        if self.integ == None: self.integ = 0
        if self.proj  == None: self.proj = "None"
        if self.integTime == None: self.integTime = 0
        self.dataTimestamp = time.localtime(helpers.frame2ctime(self.integ)) 
        self.lo     = r[1][0]
        self.fr     = r[1][1]
        self.fif    = r[1][2]
        self.islsb  = r[1][3]
        class Bandobject(object):  pass
        self.band = list()
        self.vis  = list()  # vis[band][sb][ant]; sb=0=LSB
        for b in range(self.nBands):
            self.band.append(Bandobject()) 
            self.band[b].bw     = r[2][b][0]
            self.band[b].lo2    = r[2][b][1]
            self.band[b].ifFreq = r[2][b][2]
            self.band[b].islsb  = r[2][b][3]
            self.band[b].is500  = abs(self.band[b].bw-500.0) < 2
            visband = list()
            self.vis.append(visband)
            for s in range(2):
                vissb = list()
                visband.append(vissb)
                for a in range(self.nAnts):
                    visant = list()
                    #print "vis[%d][%d][%d] %s" %(b,s,a,str(r[3][b][s][a]))
                    try:
                        vissb.append(Complex(r[3][b][s][a]))
                    except Exception :
                        x  = "Exception on this data: vis[%d][%d][%d] %s" 
                        print x %(b,s,a,str(r[3][b][s][a]))
        self.snr    = r[4] # snr[band][sb][ant];  sb=0=LSB
        self.tsys   = r[5] # tsys[band][sb][ant]; sb=0=LSB
        self.all500 = True
        for b in range(self.nBands):
            if not self.band[b].is500: self.all500 = False
            
    def atomicQuery(self, mplists) : 
        """Does the requested query bracketed by pipeline integration 
        numbers to make sure all the data are consistent. If the integ 
        numbers do not agree then it retries the full query again until 
        they match."""   
        intNumMP = "SlPipeline.LastIntegration.integNumber"
        keepGoing = True
        retries = 0
        while keepGoing:
            r = queryMpValues([[intNumMP], mplists, [intNumMP]], True) 
            if r[0][0] == r[2][0] : keepGoing = False
            else :
                if retries > 4:
                    m = "Atomic query giving up after %d retries" %retries
                    raise Exception, m
                else:
                    retries += 1
        if retries > 0: print "atomicQuery: retries = %d" %retries    
        return r[1]   
    def _plNames(self, b, mptype) :
        """Helper to create list of pipeline mp's for all ants
         in a single band. The return is a list of two lists, each containing
         all the ant MPs for a sideband [LSB, USB]"""
        mp = []
        if mptype == "vis":
            r = "Band%d.VisAverages.Integrated." %b
        elif mptype == "snr" :
            r = "Band%d.SignalToNoise.IntegratedSNR." %b
        elif mptype == "tsys" :
            r = "Tsys." %b
        else :
            raise Exception, "%s is not a known type" %mptype
        for sb in range(2):
            s = r
            if sb: s += "Usb"
            else:   s += "Lsb"
            mpsb = []
            for i in range(self.nAnts):
                mpsb.append("SlPipeline.Input%d." %(i+1) + s)
            mp.append(mpsb)    
        return mp       

    def strPhase(self, vis, l) :
        "vis is a list of Complex values"    
        for v in vis:  l += " %5d" %(v.getPhase())       
        return  l+'\n'       
    def phaseDiff(self, vis1, vis2) :
        """Takes two complex numbers and returns their phase difference as a
        normalized (amplitude=1) complex number"""
        return vis1.getPhaseDiffComplex(vis2)
    def strDiff(self, vis, visref, l, w=5) :
        " Two vectors of Complex visibilities"
        for i in range(len(vis)):          
            p = vis[i].getPhaseDiff(visref[i]) 
            l += " %*d" %(w, p)       
        return l+'\n'         
    def strAmp(self, vis, l) :
        "List of Complex visibilities"    
        for v in vis: l += " %5.1f" %(abs(v))       
        return l+'\n'   
    def strSnr(self, snr, l) :    
        for s in snr:  l += " %5.1f"  %s 
        return l+'\n'   
    def makeHeader(self, text) :
        sbstr = "USB"
        if self.islsb: sbstr = "LSB"  
        m = time.strftime("%H:%M:%S %D  ", self.getDataLocaltime)
        if self.all500: m += "ALL500"
        else:           m += "MixedBW"
        if self.calibrated: m += " CAL"
        else:               m += " raw"
        m += "  "
        m += text
        m += "\n"
        m += time.strftime("%H:%M:%S ", self.dataTimestamp)  
        m += "%8s %5.1fGHz %4.2f %s  " %(self.source, self.lo, self.fif, sbstr)
        for i in range(self.nBands):
            b = self.band[i]
            sb = "U"
            if b.islsb == 1: sb = "L"
            m += " B%d:%.0f/%4.2f%s" %(i+1,b.bw,b.lo2,sb)
        return m+'\n'              
    def makeSummaryHeader(self) :
        """Used by summary data display"""
        m = time.strftime("%H:%M:%S %D ", self.dataTimestamp)
        m += "%8s %5.1f/" %(self.source, self.lo)
        if self.all500: m += "500"
        else:           m += "mix"
        if self.calibrated: m += " Cal"
        else:               m += " Raw"
        for i in range(self.nBands):
            b = self.band[i]
            sb = "U"
            if b.islsb == 1: sb = "L"
            m += " B%d:%.0f/%4.2f%s" %(i+1,b.bw,b.lo2,sb)
        m += " dF:"
        for i in range(1, self.nBands):
            m += " %.3f" %(self.band[i].lo2-self.band[0].lo2)
        return m+'\n'
                  
    def applyCalData(self, phaseOffset, lo2Delay) :
        """Modify the data in place by applying calibrations"""
        if self.calibrated:
            raise Exception, "Cannot cal data that is already calibrated"
        self.calibrated = True
        vc = list()            
        for b in range(self.nBands):
            bsb = self.band[b].islsb
            blist = list()
            vc.append(blist)
            df = self.band[b].lo2 - self.band[0].lo2
            for s in range(2) :
                slist = list()
                blist.append(slist) 
                c = (2*bsb-1)
                for i in range(15):
                    p = phaseOffset[2*b+s][i]
                    phaseCor = Complex(p)
                    d = Complex([1,0])
                    if abs(df) > 0.002:
                        d = Complex(360*(1-2*s)*lo2Delay[b][i]*df)
                        phaseCor *= d
                    phaseCor.conjugate()  
                    t  = "b=%d s=%d i=%d:" %(b,s,i)
                    t += " p=%d d=%d pcor=%d" %(p,d.getPhase(),phaseCor.getPhase())
                    #print t
                    slist.append(phaseCor*self.vis[b][s][i])
        self.vis = vc 
        
    def applyCal(self) :
        cd = Caldata()
        o,d = cd.readCal(self.lo) 
        self.applyCalData(o, d)
        # Now form the band average phases and amplitudes
        self.grandAvePhase = list() # Unit vectors
        self.grandAveAmp   = list() # Scalars
        for a in range(self.nAnts) :
            # Compute average phase        
            ave = Complex([0.0, 0.0])
            for b in range(self.nBands):
                for s in range(2):
                    ave += self.vis[b][s][a].getUnit()
            ave.norm() 
            self.grandAvePhase.append(ave)   
            #print a, "%.0f" %self.grandAvePhase[a].getPhase(), a
            sum = Complex([0.0, 0.0])
            for b in range(self.nBands):
                l = self.vis[b][0][a]/ave
                u = self.vis[b][1][a]/ave
                u.conjugate()
                sum += l + u
            sum /= 2*self.nBands
            self.grandAveAmp.append(abs(sum))  
                      
    def data2str(self, headerText="", file=None, screen=True, delimit=True, 
                       full=True, diff=True, delay=True) :
        """Forms a string representation of the data; appends to file and
         optionally prints to screen.
        Parameters:
          headerText: appended to first line of header
          file: main part of filename. Directory automatically chosen with '.txt' extension.
            A file of None will suppress file output.
          screen: print to screen
          full: print phases/amp/snr for all bands/sb
          diff: output phase diffs for all bands/sb wrt band1 
          delay: output phase diff for all bands, one selected sb, wrt band1
                     + delay estimate"""                
        m  = ""  # Output message accumulator
        if delimit:
            m += "========================================================="
            m += "======================================\n"
        m += self.makeHeader(headerText)                                   
        m += "BD SB"
        for i in range(15) :
            m += "   C%02d" %(i+1)  
        m += '\n'
        
        # Make a list of freq differences
        dF = list()
        dFmsg = "Diff wrt Band1: "
        for b in range(self.nBands): 
            df = self.band[b].lo2 - self.band[0].lo2
            dF.append(df),
            if b <> 0: dFmsg += " dF%d=%6.3f" %(b+1, df)
        dFmsg += "\n" 
        
        if full:   
            for s in range(2): 
                for b in range(self.nBands): 
                    u = " U"
                    if s == 0: u = " L"
                    l = " %d %2s" %(b+1, u) 
                    snr    = self.snr[b][s]   
                    vis    = self.vis[b][s]  
                    if s == 0: visl = vis
                    else:      visu = vis
                    m += self.strPhase(vis, l)
                    m += self.strAmp(vis,l)
                    m += self.strSnr(snr, l)
        if diff or delay:
            m += dFmsg
            for b in range(1, self.nBands): 
                for s in range(2): 
                    u = "dU"
                    if s == 0: u = "dL"
                    l = " %d %2s" %((b+1), u) 
                    vis    = self.vis[b][s]  
                    visref = self.vis[0][s]
                    dbg = "Band:%d SB:%d %.0f %.0f %.0f %s %s" \
                        %(b,s,vis[0].getPhase(),visref[0].getPhase(), \
                          vis[0].getPhaseDiff(visref[0]), vis[0], visref[0])
                    if False: print dbg      
                    m += self.strDiff(vis, visref, l) 
        if delay:
            # Difference wrt band1, one sb only, with residual delay estimate            
            sb = 0 
            c = (1-2*sb)*2*math.pi
            if sb == 0: sbchar = "L"
            else :      sbchar = "U"
            # Output delay error estimate for all except Band1                   
            for b in range(1, self.nBands): 
                df = dF[b]
                if abs(df) < 0.002: continue
                m += " %d dD" %(b+1) 
                vis    = self.vis[b][sb]  
                visref = self.vis[0][sb]
                for i in range(15):
                    r = vis[i].getPhaseDiff(visref[i])/360.0/df
                    if r >  9.99: r =  9.99
                    if r < -9.99: r = -9.99 
                    m += "%6.2f" %r 
                m += "\n"            
        if file <> None:
            fd = open(file+".txt", 'a')
            fd.write(m) 
        if screen: print m               
        return m
        
    def datadir(standard=False):
        standardLocation  = '/array/rt'
        alternateLocation = '/home/scott'
        defaultLocation = '/tmp'
        if standard: location = standardLocation
        else:        location = alternateLocation
        try :
            os.stat(location)
        except Exception :
            firstLocation = location
            location = defaultLocation
            try :
                os.stat(location)
            except Exception :
                raise Exception, "Could not open directory %s or %s" %(firstLocation, location)                
        location += "/rpoint"
        try :
            os.stat(location)
        except Exception :
            try :
                os.mkdir(location)
            except Exception :
                raise Exception, "Could not make directory %s" %location               
        return location   
    datadir = staticmethod(datadir)
                          
class Visdata(VisdataBase) :
    """Gets the last integrated antenna based visibilities"""
    def __init__(self, snrlimit=3.0, nBands=3):
        """Constructor:
        Parameter:
          snrlimit: snr limit to check for data (default=3.0)
          nBands: number of bands (default=3)"""
        VisdataBase.__init__(self, nBands=nBands)  
        self.snrlimit = snrlimit
        # Good ants as defined by snr criterion
        self.antGood = []
        self.minGoodAnts = 10
        self.goodEnough = True
        for a in range(15): self.antGood.append(True)
        self.__selectAnts(self.snrlimit)

    def __selectAnt(self, ant, snrlim) :
        """Compares all 500MHz band/sb for a single antenna to snr limit; 
        returns True if one band is good, false otherwise."""
        for b in range(self.nBands) :
            if self.band[b].is500:
                snr = self.snr[b]
                if (snr[0][ant] >= snrlim) and (snr[1][ant] >= snrlim):
                    return True                    
        return False        
    def __selectAnts(self, snrlimit) :
        "Compares band/sb to snr limit for each ant; sets goodAnts[] to T/F"
        self.antGood = list()
        self.nGood = 0
        for a in range(15) :
            isGood = self.__selectAnt(a, snrlimit)
            if isGood: self.nGood += 1                 
            self.antGood.append(isGood)
        self.goodEnough = self.nGood >= self.minGoodAnts                   
        #print self.goodEnough, self.nGood, self.antGood       

    def checkSource(self) :
        goodSources = ["3C84", "3C454.3", "3C345","3C273","3C279"]
        badSources  = ["MARS", "URANUS", "NEPTUNE", "NOISE"]
        for s in goodSources:
            if s == self.source: return True
        for s in badSources:
            if s == self.source: return False
        return True  
          
    def filter(self, chkSource=True, only500=True, goodNuff=True) :
        if chkSource and not self.checkSource(): return "Rejecting source %s" %self.source
        if only500   and not self.all500 :       return "Rejecting, all bands not 500MHz"
        if self.source <> "NOISE" :
            if goodNuff  and not self.goodEnough:    return "Rejecting, data not good enough"
        if self.proj == "rpnt":                      return "Rejecting rpnt"
        return True

    def summaryLine(self, vc, b1, b2, sbsel, text) :
        if sbsel == 0 :   sbchar = 'L'
        elif sbsel == 1 : sbchar = 'U'
        else :            sbchar = '?'
        pre = " %s %d-%d%s" %(text, b1+1, b2+1, sbchar) 
        return self.strDiff(vc[b1][sbsel], vc[b2][sbsel], pre, 4) 
    
    def cohLine(self, prefix, c) :
        """Take a list of coherence estimates and produce line of text"""
        l = prefix
        for a in range(self.nAnts): l += " %4.2f" %c[a] 
        return l 
            
    def writePhaseSummaryFile(self, file, summaryText, coh=False):    
        """Summary file output phase diffs wrt Band1 for all except Band1""" 
        summary  = self.makeSummaryHeader()  
        summary += self.summaryLine(self.vis, 1, 0, 0, summaryText)         
        summary += self.summaryLine(self.vis, 1, 0, 1, summaryText)         
        summary += self.summaryLine(self.vis, 2, 0, 0, summaryText)         
        summary += self.summaryLine(self.vis, 2, 0, 1, summaryText)         
        summary += self.summaryLine(self.vis, 2, 1, 0, summaryText) 
        summary += self.summaryLine(self.vis, 2, 1, 1, summaryText) 
        if coh :
            summary += self.cohLine("   Coh ", self.cohCheck())  + '\n'
            summary += self.cohLine("   Coh2", self.cohCheck2()) + '\n'
            summary += self.cohLine("   Coh3", self.cohCheck3()) + '\n'
        fd = open(file+"summary.txt", 'a') 
        fd.write(summary) 
        fd.close()
        
    def writeSummaryFile(self, file, text):            
        try :
            catFlux = flux(self.source)
            catFluxStr = "%.1f" %catFlux
        except :
            catFlux = None
            catFluxStr = "----"
        aveFlux =   sum(self.grandAveAmp)/len(self.grandAveAmp)
        fluxStr = "%.1f/%s" %(aveFlux,catFluxStr)            
        m = time.strftime("%H:%M:%S %D ", self.dataTimestamp)
        m += "%8s %9sJy %5.1fGHz " %(self.source, fluxStr, self.lo)
        prefix = ""
        for i in range(self.nBands):
            b  = self.band[i]
            sb = "U"
            if b.islsb == 1: sb = "L"
            m += "%s%4.2f%s" %(prefix, b.lo2,sb)
            prefix = '/'
        if self.tau == None: tauStr = "None"
        else :               tauStr = "%4.2f" %self.tau
        if self.skyrms == None: skyrmsStr = "?   "
        else :                  skyrmsStr = "%4.2f" %(0.001*self.skyrms)
        m += " tau=%s rms=%smm" %(tauStr, skyrmsStr)    
        m += '\n'
        m += "   Amp "
        for a in range(self.nAnts) :
            m += " %4.1f" %self.grandAveAmp[a]
        m += '\n'
        m += self.cohLine("   Coh ", self.cohCheck())  + '\n'
        m += self.cohLine("   Coh2", self.cohCheck2()) + '\n'
        m += self.cohLine("   Coh3", self.cohCheck3()) + '\n'
        fd = open(file + "flux.txt", 'a') 
        fd.write(m) 
        fd.close()

    def output(self, file="pb", screen=True, coh=True,
               chkSource=True, only500=True, goodNuff=False) :  
        filt = self.filter(chkSource=chkSource, only500=only500,goodNuff=goodNuff)
        if filt <> True:
            return [False, filt]
        if self.goodEnough: 
            text = "goodEnough"
            phaseSummaryText = " "
        else :                
            text = ""
            phaseSummaryText = "B"
        if file <> None: 
            file = self.datadir() + "/" + file 
            self.writePhaseSummaryFile(file, phaseSummaryText, coh=coh)  
            if coh and self.calibrated :
                summaryText = ""     
                self.writeSummaryFile(file, summaryText)   
        str = self.data2str(headerText=text, file=file, screen=screen)
        return [True, str]

    def writeCal(self, ovis) :
        """Use visibilities to create a calibration file.
        A visibility set with all bands with the same lo2 frequencies
        is required as an input parameter.
        This visibility set is expected to have lo2 freq differences
        between the various bands and the reference band.
        Parameters:
          offsetVis: a Visdata object taken with all lo2 the same."""
        ovisFilter = ovis.filter() 
        if ovisFilter <> True: 
            raise Exception, "writeCal failed, input offsets " + ovisFilter
        filt = self.filter() 
        if filt <> True: 
            raise Exception, "writeCal failed, data " + filt
           
        # Check ovis lo2 freqs
        for b in range(self.nBands) :
            if abs(ovis.band[b].lo2 - ovis.band[0].lo2) > 0.002:
                m = "All lo2 frequencies in offset visibility set "
                m += "must be the same.\n"
                m += "Instead we have:\n"
                for b in range(self.nBands) :
                    m += "  Band%d: %.3f GHz" %((b+1),ovis.band[i].lo2)
                raise Exception, m
        # Get the offsets        
        offsets   = list()
        for b in range(self.nBands) :
            for s in range(2):
                o = list()
                for a in range(self.nAnts):
                    o.append(ovis.vis[b][s][a].getPhase())
                offsets.append(o) 
        # Now compute the delays               
        lo2delays = list()
        for b in range(self.nBands) :
            d = list()
            df = self.band[b].lo2 - self.band[0].lo2
            for a in range(self.nAnts):
                loff = Complex(offsets[2*b][a])
                uoff = Complex(offsets[2*b+1][a])
                l  = self.vis[b][0][a].getPhaseDiffComplex(loff)
                u  = self.vis[b][1][a].getPhaseDiffComplex(uoff)
                #Average sideband phase
                avePhase  = 0.5*l.getPhaseDiff(u)
                #print b, a, int(u), int(l), int(dp)
                if abs(df) < 0.02 : delay = 0.0
                else :              delay = avePhase/360.0/df
                d.append(delay)         
            lo2delays.append(d)         
        cd = Caldata()
        cd.writeCal(self.lo, offsets, lo2delays)
    def cohCheck(self) :
        """Check the coherence across bands"""
        coh = list()
        for a in range(self.nAnts) :
            sum = Complex([0.0, 0.0])
            for b in range(self.nBands):
                for s in range(2):
                    sum += self.vis[b][s][a].getUnit()
            sum /= 2*self.nBands        
            coh.append(abs(sum)) 
        return coh 
    def _rms(self, phases) :
        sum  = 0
        sum2 = 0
        n    = len(phases)
        for p in phases:
            sum  += p
            sum2 += p*p
        m = sum/n
        return math.sqrt(sum2/n-m*m)    
    def _unwrap(self, phases) :
        """Go through all possible combos of adding a single turn 
        and choose the combination with the lowest rms.
        Parameter:
         phases: a list of phases"""
        nPhases = len(phases)
        minRMS = 1e10
        bestWrappedPhases = None
        combos = int(math.pow(2,nPhases))
        for c in range(combos):
            wrappedPhases = list()
            for i in range(nPhases):
                p = phases[i]
                if c & (1<<i): p += 360
                wrappedPhases.append(p) 
            rms = self._rms(wrappedPhases)
            if rms < minRMS:
                minRMS = rms
                bestWrappedPhases = wrappedPhases  
        return bestWrappedPhases                         
    def cohCheck2(self) :
        """After correction we often see what appears to be band delay 
         (complementary USB/LSB phases wrt band1). So a more coherent sum can
         be obtained by first subtracting off the overall average phase and then
         conjugating the USB phases."""
        coh = list()
        for a in range(self.nAnts) :
            # Unwrap phases
            phases = list()
            for b in range(self.nBands):
                for s in range(2):
                    phases.append(self.vis[b][s][a].getPhase())
            phases = self._unwrap(phases)
            # Compute average phase        
            ave = Complex([1.0, 0.0])
            for pha in phases:
                # This is normalized first to avoid 2pi wraps
                p = pha/(2*self.nBands)
                ave *= Complex(p) # Multiply unit vecs to add phases
            pave = ave.getPhase()
            sum = Complex([0.0, 0.0])
            for b in range(self.nBands):
                l = Complex(self.vis[b][0][a].getPhase()-pave)/(2*self.nBands)
                u = Complex(self.vis[b][1][a].getPhase()-pave)/(2*self.nBands)
                u.conjugate()
                sum += l + u
            coh.append(abs(sum)) 
        return coh                     
    def cohCheck3(self) :
        """After correction we often see what appears to be band delay 
         (complementary USB/LSB phases wrt band1). So a more coherent sum can
         be obtained by first subtracting off the overall average phase and then
         conjugating the USB phases."""
        coh = list()
        for a in range(self.nAnts) :
            ave = self.grandAvePhase[a] # This is a unit vector
            #print a, "%.0f" %ave.getPhase()
            sum = Complex([0.0, 0.0])
            for b in range(self.nBands):
                l = self.vis[b][0][a]/ave # Division is equiv to subtracting phase
                u = self.vis[b][1][a]/ave
                l.norm()  # All bands/sb have the same weight
                u.norm()
                u.conjugate()
                sum += l + u
            sum /= 2*self.nBands
            coh.append(abs(sum)) 
        return coh                     
                 
# ================================== Caldata ====================================       
class Caldata(object) :
    """Calibration measurement access"""
    def __init__(self, nBands=3, nAnts=15):
        """Constructor:
        Parameter:
          nBands: number of bands (default=3)
          nAnts: number of antennas (default=15)"""
        self.nBands = nBands
        self.nAnts  = nAnts
    def __timestampString(self):
        t = time.time()   
        gmt = time.gmtime(t)   
        return time.strftime("%Y%m%d",gmt) + ".%03d" %(int(1000*math.fmod(t/86400.0,1)))     
    def norm(self, p): 
        """Normalize a phase in degrees to +/-180"""   
        p = p%360.0
        if p < -180: p += 360.0
        if p >  180: p -= 360.0
        return p    
    def writeCal(self, freq, offsets, lo2delays) :
        """Writes cal data to a file.
        Parameters:
          freq: LO frequency in GHz
          offsets: list containing band/sidebands containing list
             of antenna phase offsets in degrees
          lo2delays: like offsets except no sidebands and 
             lo2delays in nanoseconds"""
        if len(offsets) <> 2*self.nBands:
            m = "Number of bands in offsets(%d) != twice nBands(%d)" \
                    %(len(offsets), self.nBands)
            raise Exception, m  
        if len(lo2delays) <> self.nBands:
            m = "Number of bands in lo2delays(%d) != nBands(%d)" \
                    %(len(lo2delays), self.nBands)
            raise Exception, m  
        f = self.calFile(freq, create=True)
        s = "nBands: %d\n" %self.nBands
        for b in range(self.nBands):
            for sb in range(2) :
                m = "%2d" %(b+1)
                if sb == 0: m += "L:"
                else :      m += "U:"
                for a in range(self.nAnts):
                    m += " %5d" %(self.norm(offsets[2*b+sb][a]))
                s += m + "\n"        
        for b in range(self.nBands):
            m = "%2dD:" %(b+1)
            for a in range(self.nAnts):
                m += " %5.2f" %(lo2delays[b][a])
            s += m + "\n"
        f.write(s)         
    def readCal(self, freq) :
        """Reads cal data from file and returns in two lists;
        phase offsets and delays.
        Parameters:
          freq: LO frequency in GHz"""
        f = self.calFile(freq)
        keepGoing = True
        nBlanks   = 0
        while keepGoing:
            line = f.readline()
            if len(line) > 0: nBlanks  = 0
            else :            
                nBlanks += 1
                if nBlanks > 4:
                    raise Exception, "Too many blank lines in cal file"
            if line[0] == '#': continue
            if line.find("nBands") <> -1:
                nb = int(line.split(':')[1])
                if nb != self.nBands:
                    m="Number of bands in calFile(%d) " %nb
                    m += "not equal to requested number(%d)" %self.nBands  
                    raise Exception, m
                keepGoing = False    
        offsets = list()        
        for b in  range(2*self.nBands):
            offs = list()
            line = f.readline()
            offstr = (line.split(":")[1]).split()
            for a in range(self.nAnts):
                offs.append(int(offstr[a])) 
            offsets.append(offs)      
        lo2delays = list()        
        for b in  range(self.nBands):
            dels = list()
            line = f.readline()
            delstr = (line.split(":")[1]).split()
            for a in range(self.nAnts):
                dels.append(float(delstr[a]))
            lo2delays.append(dels)
        return offsets, lo2delays
    def printCal(self, freq) :
        """Reads cal data from file and prints results.
        Parameters:
          freq: LO frequency in GHz""" 
        o,d = self.readCal(freq)
        for b in range(len(o)):
            nb = b/2 + 1
            m = "B%d" %nb
            if b%2 == 0: m += "L:"
            else:        m += "U:"
            for a in o[b]:
                m += " %5d" %a
            print m    
        for b in range(len(d)):
            m = "B%dD:" %(b+1)
            for a in d[b]:
                m += " %5.2f" %a
            print m    
                     
    def writeSim(self):
        o = list()               
        for b in range(self.nBands):
            x = list()
            for a in range(self.nAnts):
                x.append( 1*(b*10+a+1))
                x.append(-1*(b*10+a+1))
            o.append(x)        
        d = list()               
        for b in range(self.nBands):
            x = list()
            for a in range(self.nAnts):
                x.append(0.10*b + 0.01*a + 0.01)
            d.append(x)
        self.writeCal(95,o,d)    
                    
    def calDir(self):
        location = Visdata.datadir() + "/cal"    
        try :
            os.stat(location)
        except Exception :
            try :
                os.mkdir(location)
            except Exception :
                raise Exception, "Could not make directory %s" %location               
        return location 
    def calFile(self, freq, create=False):
        fnroot = self.calDir() + "/cal.f"+ str(int(round(freq))) 
        fn = fnroot + ".data" 
        if create:   
            try :
                os.stat(fn)
            except Exception :
                print "Info: could not stat %s" %fn
                pass
            else:
                try :
                    os.unlink(fn)
                    print "Info: unlink successful on %s" %fn
                except Exception :
                    raise Exception, "Could not unlink %s" %fn          
            try :
                f = open(fn, mode='w')
            except Exception :
                raise Exception, "Could not create %s" %fn 
            #print self.__timestampString() 
            try :
                fnWithTs = fnroot + "." + self.__timestampString()
                os.link(fn, fnWithTs)
            except Exception :
                raise Exception, "Could not create link of %s to file %s" %(fn, fnWithTs)                
        else :   
            try :
                os.stat(fn)
            except Exception :
                m = "Could not open %s; calibration file does not exist" %fn               
                raise Exception, m              
            try :
                f = open(fn)
            except Exception :
                raise Exception, "Open of file %s failed" %fn                           
        return f   

#===========================================================================
# Operational routines
# These help with scripts to collect data

def b2bcal(foffset=0.4, sbenum=AUTO, inttime=10, sleeptime=1) :
    """Collect data for a new phase calibration using the current LO setup.
    """ 
    print "b2bcal: collecting phase offsets with all bands aligned"
    mp = "Control.Subarray1.Commands.Freq.restFreq"
    f = queryDouble(mp)
    configband(1, BW500, f, sbenum)
    configband(2, BW500, f, sbenum)
    configband(3, BW500, f, sbenum)
    sleep(1)
    integrate(inttime, 1, antwait=ALL)
    #comment("Sleeping...")
    sleep(sleeptime)

    try :
        voff = Visdata()
        voff.output()
        print "b2bcal: collecting phases with bands offset" 
        configband(2, BW500, f + foffset, sbenum)
        configband(3, BW500, f + foffset, sbenum)
        sleep(1)
        integrate(inttime, 1, antwait=ALL)
        comment("Sleeping...")
        sleep(sleeptime)
        v = Visdata()
        v.output()
        try :
            v.writeCal(voff)
        except Exception, ex :
            print "Calibration failed: " + str(ex)
            return str(ex)    
        print "b2bcal: calibration complete" 
        print "Applying calibration to offset band data" 
        v.applyCal()
        v.output() 
    except Exception, ex :
        print "b2bcal failed: " + str(ex)
        return str(ex)    
    return True 
     
def cohCheck(inttime=10, sleeptime=1) :
    """Collect data to check coherence using the current LO 
    and bandsetup.
    """ 
    integrate(inttime, 1, antwait=ALL)
    #comment("Sleeping...")
    sleep(sleeptime)
    v = Visdata()
    v.output()
    v.applyCal()
    v.output(coh=True)
        
def sendEmail(tbegin, p, source, specialMsg = ""):
    to      = "scott@mmarray.org"
    if p.emailObs: to += ",obs@mmarray.org"
    from_   = "system@mmarray.org"
    subject = "pbcal run"
    deltat = (time.time() - tbegin)/60.
    st = p.inputString()
    if len(st) == 0: st = "no options"
    else :           st = "options '%s'" %p.inputString()
    m = "Script '%s' with %s" %(scriptName,st)
    m += " has completed using source %s\n" %source
    m += " Keyword/values:\n"
    m += "  " + p.keywordValueString() + "\n"
    if specialMsg <> "": m += specialMsg + "\n"
    m += "Elapsed time=%.1f minutes" %deltat
    odutils.sendEmailMessage(to, from_, m, subject)

def hasC1() :
    return (currentAntennaNumbers().count(1) != 0)  
    
def getProjectState() :
    """Gets and returns the current project and obsblock state""" 
    pre = 'Control.Subarray%d.' %subarrayNo
    obid = queryString(pre + 'obsBlockID').split('.')
    if len(obid) == 3:
        # No subObsblock, slip in a null
        return [obid[0], obid[1], "", obid[2]]
    return obid

def restoreProjectState(oldState) :
    s.setObsblock(oldState[0],oldState[1],oldState[2],oldState[3])
    #s.project(oldState[0])
    #s.obsblock(oldState[1])                                                       
    #s.subObsblock(oldState[2])                                                       
    #s.trial(int(oldState[3]))                                                      

def setNullProject() :
    s.setObsblock("MAINTENANCE","NONE","STANDBY",1)
    #s.project("MAINTENANCE")
    #s.obsblock("NONE")                                                       
    #s.subObsblock('STANDBY')                                                       
    #s.trial(1) 
    
                                                                                                                
