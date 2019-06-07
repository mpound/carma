import sys
import threading

class KeyboardInterruptable(threading.Thread):
    """Class to make a cancellable command KeyboardInterruptable."""
    def __init__(self, targetcmd, cancelException=None, args=(), kwargs=None):
        threading.Thread.__init__(self)
        self.target           = targetcmd
        self.cancelException  = cancelException
        self.args             = args
        self.kwargs           = kwargs
        self.ret              = None
        self.done             = False
        self.doneCond         = threading.Condition()
        self.exc_info         = None
        self.fullTrace        = None
        self.cleanFinish      = False
        self.successfulCancel = False

    def doneWaiting(self):
        try:
            self.doneCond.acquire()
            self.doneCond.wait(0.2)
            return self.done
        finally:
            self.doneCond.release()
            
    def getExceptionInfo(self) :
        return self.exc_info

    def getFullTrace(self) :
        return self.fullTrace

    def run(self):
        """Thread run overload.""" 
        try:
            try: 
                self.ret = self.target( *self.args, **self.kwargs )
                self.cleanFinish = True
            except Exception, ex:
                if type(ex) == self.cancelException:
                    self.successfulCancel = True
                else :
                    self.exc_info  = sys.exc_info()
                    import traceback
                    t = traceback.extract_tb(self.exc_info[2])
                    self.fullTrace = traceback.format_list(t)
        finally:
            self.doneCond.acquire()
            self.done = True
            self.doneCond.notify()
            self.doneCond.release()


