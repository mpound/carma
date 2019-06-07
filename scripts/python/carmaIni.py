# CARMA python startup file
#
# $Id: carmaIni.py,v 1.14 2011/12/19 20:41:57 abeard Exp $

# Setup carma and corba environment   

import sys, os, string, re
import CORBA, PortableServer, CosNaming
import ImplementationRepository
import omniORB
import carma

CORBA_CLIENT_CALL_TIMEOUT_S = 60 * 60 

# Taken from the Python Tutorial
#
# Add auto-completion and a stored history file of commands to your Python
# interactive interpreter. Requires Python 2.0+, readline. Autocomplete is
# bound to the Esc key by default (you can change it - see readline docs).

import atexit
import readline
import rlcompleter
            

readline.parse_and_bind('\C-space: complete')
historyPath = os.path.expanduser("~/.pyhistory")

def save_history(historyPath=historyPath):
    import readline
    readline.write_history_file(historyPath)

if os.path.exists(historyPath):
    readline.read_history_file(historyPath)

atexit.register(save_history)
del atexit, readline, rlcompleter, save_history, historyPath

# ---- End of auto-completion and stored history file setup
    
def transientHandler(cookie, retries, exc):
    """TRANSIENT exception handler for CARMA.  By default,
    omniORB handles TRANSIENT exceptions from location forward requests
    by continuously retrying using an exponential backoff scheme putting 
    a client on hold indefinitely.  This method instead will only try
    twice."""
    if exc.minor == omniORB.TRANSIENT_FailedOnForwarded and retries < 1:
        return True
    else:
        return False
        

# CORBA environment
class carmaCorba:
    "Set up corba environment for carma"
    def __init__(self):
        self.statusOK_     = False
        self.nameservice_  = 0
        self.imr_          = 0
        # I'm not sure I like getting something from the environment...
        if os.getenv('imr') is not None:
            sys.argv += ['-ORBDefaultInitRef', 'corbaloc::' + os.getenv('imr')]
        try :
            orb = CORBA.ORB_init( sys.argv, CORBA.ORB_ID )
            obj = orb.resolve_initial_references('NameService')
            self.nameservice_ = obj._narrow( CosNaming.NamingContext )
            obj = orb.resolve_initial_references('ImplRepoService')
            self.imr_ = obj._narrow(ImplementationRepository.Administration)
            self.statusOK_ = True
            omniORB.setClientConnectTimeout( 15 * 1000 )
            omniORB.setClientCallTimeout( CORBA_CLIENT_CALL_TIMEOUT_S * 1000 )
            omniORB.installTransientExceptionHandler(self, transientHandler)
        except Exception:
            emsg  = "\n-------------------------------------------------\n"
            emsg += "Error: Could not initialize CORBA environment.\n"
            emsg += "  Check that imr is correctly specified and \n"
            emsg += "  that the imr and nameserver are running.\n"
            emsg += "-------------------------------------------------\n"
            print emsg
            raise Exception, "CORBA initialization error"

    def reset(self):
        self.statusOK_ = False
        self.__init__(self)
    def getObj(self, names, kind=None):
        """Get a reference to a CORBA remote object; the names can be a list of 
        naming contexts as strings, or they can be concatenated using . or /
        as separators."""
        if type(names) is list: names = string.join(names,'.')
        nameContext = [
            CosNaming.NameComponent( name, "" ) for name in re.split('[./]',names)
        ]
        try:
            obj = self.nameservice_.resolve( nameContext )
            if kind is not None: obj = obj._narrow(kind)
            return obj
        except Exception, ex:
            if str(ex).rfind("missing_node"): 
                errorMessage = "Can't find '" + names + "' in the nameservice"
                raise Exception, errorMessage 
            else: raise ex


# The file devices.py relies on these definitions    
corba = carmaCorba()

# For convenience
def getObj(names, kind=None ) : return corba.getObj(names, kind)

