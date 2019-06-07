#!/opt/carmaTools/bin/python

import CORBA
import sys
import re
import subprocess
import time

import ImplementationRepository

class TerminalController:
    """
    A class that can be used to portably generate formatted output to
    a terminal.

    `TerminalController` defines a set of instance variables whose
    values are initialized to the control sequence necessary to
    perform a given action.  These can be simply included in normal
    output to the terminal:

        >>> term = TerminalController()
        >>> print 'This is '+term.GREEN+'green'+term.NORMAL

    Alternatively, the `render()` method can used, which replaces
    '${action}' with the string required to perform 'action':

        >>> term = TerminalController()
        >>> print term.render('This is ${GREEN}green${NORMAL}')

    If the terminal doesn't support a given action, then the value of
    the corresponding instance variable will be set to ''.  As a
    result, the above code will still work on terminals that do not
    support color, except that their output will not be colored.
    Also, this means that you can test whether the terminal supports a
    given action by simply testing the truth value of the
    corresponding instance variable:

        >>> term = TerminalController()
        >>> if term.CLEAR_SCREEN:
        ...     print 'This terminal supports clearning the screen.'

    Finally, if the width and height of the terminal are known, then
    they will be stored in the `COLS` and `LINES` attributes.
    """
    # Cursor movement:
    BOL = ''             #: Move the cursor to the beginning of the line
    UP = ''              #: Move the cursor up one line
    DOWN = ''            #: Move the cursor down one line
    LEFT = ''            #: Move the cursor left one char
    RIGHT = ''           #: Move the cursor right one char

    # Deletion:
    CLEAR_SCREEN = ''    #: Clear the screen and move to home position
    CLEAR_EOL = ''       #: Clear to the end of the line.
    CLEAR_BOL = ''       #: Clear to the beginning of the line.
    CLEAR_EOS = ''       #: Clear to the end of the screen

    # Output modes:
    BOLD = ''            #: Turn on bold mode
    BLINK = ''           #: Turn on blink mode
    DIM = ''             #: Turn on half-bright mode
    REVERSE = ''         #: Turn on reverse-video mode
    NORMAL = ''          #: Turn off all modes

    # Cursor display:
    HIDE_CURSOR = ''     #: Make the cursor invisible
    SHOW_CURSOR = ''     #: Make the cursor visible

    # Terminal size:
    COLS = None          #: Width of the terminal (None for unknown)
    LINES = None         #: Height of the terminal (None for unknown)

    # Foreground colors:
    BLACK = BLUE = GREEN = CYAN = RED = MAGENTA = YELLOW = WHITE = ''

    # Background colors:
    BG_BLACK = BG_BLUE = BG_GREEN = BG_CYAN = ''
    BG_RED = BG_MAGENTA = BG_YELLOW = BG_WHITE = ''

    _STRING_CAPABILITIES = """
    BOL=cr UP=cuu1 DOWN=cud1 LEFT=cub1 RIGHT=cuf1
    CLEAR_SCREEN=clear CLEAR_EOL=el CLEAR_BOL=el1 CLEAR_EOS=ed BOLD=bold
    BLINK=blink DIM=dim REVERSE=rev UNDERLINE=smul NORMAL=sgr0
    HIDE_CURSOR=cinvis SHOW_CURSOR=cnorm""".split()
    _COLORS = """BLACK BLUE GREEN CYAN RED MAGENTA YELLOW WHITE""".split()
    _ANSICOLORS = "BLACK RED GREEN YELLOW BLUE MAGENTA CYAN WHITE".split()

    def __init__(self, term_stream=sys.stdout):
        """
        Create a `TerminalController` and initialize its attributes
        with appropriate values for the current terminal.
        `term_stream` is the stream that will be used for terminal
        output; if this stream is not a tty, then the terminal is
        assumed to be a dumb terminal (i.e., have no capabilities).
        """
        # Curses isn't available on all platforms
        try: import curses
        except: return

        # If the stream isn't a tty, then assume it has no capabilities.
        if not term_stream.isatty(): return

        # Check the terminal type.  If we fail, then assume that the
        # terminal has no capabilities.
        try: curses.setupterm()
        except: return

        # Look up numeric capabilities.
        self.COLS = curses.tigetnum('cols')
        self.LINES = curses.tigetnum('lines')

        # Look up string capabilities.
        for capability in self._STRING_CAPABILITIES:
            (attrib, cap_name) = capability.split('=')
            setattr(self, attrib, self._tigetstr(cap_name) or '')

        # Colors
        set_fg = self._tigetstr('setf')
        if set_fg:
            for i,color in zip(range(len(self._COLORS)), self._COLORS):
                setattr(self, color, curses.tparm(set_fg, i) or '')
        set_fg_ansi = self._tigetstr('setaf')
        if set_fg_ansi:
            for i,color in zip(range(len(self._ANSICOLORS)), self._ANSICOLORS):
                setattr(self, color, curses.tparm(set_fg_ansi, i) or '')
        set_bg = self._tigetstr('setb')
        if set_bg:
            for i,color in zip(range(len(self._COLORS)), self._COLORS):
                setattr(self, 'BG_'+color, curses.tparm(set_bg, i) or '')
        set_bg_ansi = self._tigetstr('setab')
        if set_bg_ansi:
            for i,color in zip(range(len(self._ANSICOLORS)), self._ANSICOLORS):
                setattr(self, 'BG_'+color, curses.tparm(set_bg_ansi, i) or '')

    def _tigetstr(self, cap_name):
        # String capabilities can include "delays" of the form "$<2>".
        # For any modern terminal, we should be able to just ignore
        # these, so strip them out.
        import curses
        cap = curses.tigetstr(cap_name) or ''
        return re.sub(r'\$<\d+>[/*]?', '', cap)

    def render(self, template):
        """
        Replace each $-substitutions in the given template string with
        the corresponding terminal control string (if it's defined) or
        '' (if it's not).
        """
        return re.sub(r'\$\$|\${\w+}', self._render_sub, template)

    def _render_sub(self, match):
        s = match.group()
        if s == '$$': return s
        else: return getattr(self, s[2:-1])

class TaoServerNotFoundException(Exception):
    pass

class TaoServerInfo(object):
    def __init__(self, si):
        self.name = si.server
        self.ior = si.partial_ior

        if self.ior == '':
            self.running = False
        else:
            self.running = True

        self.command_line = si.startup.command_line
        self.env = si.startup.environment
        self.cwd = si.startup.working_directory
        self.activator = si.startup.activator
        self.startup_options = si.startup

    def __str__(self):
        print 'NAME: %s ACT: %s IOR: %s' % (self.name, self.activator, self.ior)

    def check_running(self, verbose=False):
        # if we're not running, exit immediately (no ior to check)
        if not self.running:
            return False

        # parse the hostname + port out of the string
        pat = r'corbaloc:iiop:1.2@(.+?):(\d+?)/'
        match = re.match(pat, self.ior)
        if not match:
            return False

        # try to find the process on the correct port using lsof
        (host, port) = match.groups()
        cmd = ['ssh', '-t', host, 'sudo', '/usr/sbin/lsof', '-i', '4TCP:' + port]
        proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        (stdout, stderr) = proc.communicate()
        ret = proc.wait()

        if verbose:
            sys.stdout.write(stdout)

        if ret != 0:
            return False

        return True

    def __eq__(self, rhs):
        return self.name == rhs.name

    def __gt__(self, rhs):
        return self.name > rhs.name

    def __lt__(self, rhs):
        return self.name < rhs.name

class TaoProcessTotal(object):
    def __init__(self):
        self.num_running = 0
        self.num_stopped = 0

    def __str__(self):
        return 'Total: %d running, %d stopped' % (self.num_running, self.num_stopped)

    def add_process_status(self, running):
        if running:
            self.num_running += 1
        else:
            self.num_stopped += 1

class TaoImrClient(object):
    def __init__(self, imr):
        self.imr = imr
        self.server_list = []
        self.server_dict = {}
        self.tc = TerminalController()

    # update the internal data structures
    def update_server_info(self):
        self.server_list = []
        self.server_dict = {}
        (servers, it) = self.imr.list(0)
        for s in servers:
            tsi = TaoServerInfo(s)
            self.server_list.append(tsi)

            try:
                self.server_dict[tsi.activator].append(tsi)
            except KeyError:
                self.server_dict[tsi.activator] = [tsi]

        # sort the flat list of servers
        self.server_list.sort()

        # sort each individual list of servers
        for (k, v) in self.server_dict.iteritems():
            v.sort()

    # list all registered servers
    def list_servers_mach(self, machine=None, running=True, stopped=True):

        if machine is None:
            print 'ERROR: you must specify the machine= parameter'
            return False

        possible_machines = []
        for k in self.server_dict:
            if k.lower().startswith(machine.lower()):
                possible_machines.append(k)

        # check for no matches
        if len(possible_machines) == 0:
            print 'ERROR: unable to find machine: %s' % machine
            return False

        # check for multiple matches
        if len(possible_machines) > 1:
            print 'ERROR: ambiguous machine name, choices are:'
            for m in possible_machines:
                print '\t%s' % m

            return False

        # ok, we have exactly one match: use it
        machine = possible_machines[0]
        servers = self.server_dict[machine]

        total = TaoProcessTotal()
        print self.tc.render('\n${BOLD}%s:${NORMAL}' % (machine, ))
        for s in servers:

            # count the process status
            total.add_process_status(s.running)

            # skip servers as requested
            if self.should_skip(s, running, stopped):
                continue

            print '--> %-20s    %-10s' % (s.name, self.format_status(s.running))

        print total

    # list all registered servers
    def list_servers_tree(self, running=False, stopped=True):
        machines = self.server_dict.keys()[:]
        machines.sort()
        for m in machines:

            # some bad processes publish extra POAs
            # they are broken: we don't need to see these
            if m == '':
                continue

            self.list_servers_mach(m, running, stopped)

    # list all registered servers
    def list_servers_flat(self, running=False, stopped=True, machine=True, ior=False):
        total = TaoProcessTotal()
        for s in self.server_list:

            # accumulate the status
            total.add_process_status(s.running)

            # skip servers as requested
            if self.should_skip(s, running, stopped):
                continue

            # the server name and status are always printed
            sys.stdout.write('--> %-20s    %-10s' % (s.name, self.format_status(s.running)))

            # now we have extra printing options
            if machine:
                sys.stdout.write('    %-20s' % (s.activator, ))

            if ior:
                sys.stdout.write('    %-20s' % (s.ior, ))

            # and now the final newline
            sys.stdout.write('\n')

        print total

    def find_server(self, name):
        for s in self.server_list:
            if s.name.lower() == name.lower():
                return s

        msg = 'unable to find server: %s' % name
        raise TaoServerNotFoundException, msg

    def format_status(self, status):
        if status:
            return self.tc.render('${GREEN}running${NORMAL}')
        else:
            return self.tc.render('${RED}${BOLD}STOPPED${NORMAL}')

    def should_skip(self, server, running, stopped):
        """should this status be skipped?"""
        if server.running == True and running == False:
            return True

        if server.running == False and stopped == False:
            return True

        return False

    # check the true process status
    def process_status(self, name, verbose=False):
        try:
            s = self.find_server(name)
            print '%-22s %s' % ('Process name:', s.name)
            print '%-22s %s' % ('Runs on host:', s.activator)

            # get both statuses
            corba_state = s.running
            linux_state = s.check_running()
            print_state = True

            if corba_state == False or linux_state == False:
                print_state = False

            # different stuff is printed in quiet and verbose mode
            if verbose:
                so = s.startup_options
                print
                print '%-22s %s' % ('Partial IOR:', s.ior)
                print '%-22s %s' % ('Activation:', so.activation)
                print '%-22s %s' % ('Activator:', so.activator)
                print '%-22s %s' % ('Command line:', so.command_line)
                print '%-22s %s' % ('Environment:', so.environment)
                print '%-22s %s' % ('Start limit:', so.start_limit)
                print '%-22s %s' % ('Working directory:', so.working_directory)
                print
                print '%-22s %s' % ('CORBA Process State:', self.format_status(corba_state))
                print '%-22s %s' % ('Linux Process State:', self.format_status(linux_state))
            else:
                print '%-22s %s' % ('Process state:', self.format_status(print_state))

        except TaoServerNotFoundException, e:
            msg = 'ERROR: %s' % str(e)
            print msg

    # stop a server
    def process_stop(self, name):
        try:
            s = self.find_server(name)
            self.imr.shutdown_server(s.name)
            return True
        except TaoServerNotFoundException, e:
            msg = 'ERROR: %s' % str(e)
            print msg
            return False
        except ImplementationRepository.NotFound, e:
            msg = 'ERROR: unable to stop %s: not found' % (name, )
            print msg
            return False

    # start a server
    def process_start(self, name):
        try:
            s = self.find_server(name)

            # this is a dirty trick to force the start_limit to be reset
            self.imr.add_or_update_server(s.name, s.startup_options)

            # and now actually start the server
            self.imr.activate_server(s.name)
            return True
        except TaoServerNotFoundException, e:
            msg = 'ERROR: %s' % str(e)
            print msg
            return False
        except ImplementationRepository.CannotActivate, e:
            msg  = 'ERROR: unable to start %s: %s' % (name, e.reason)
            print msg
            return False

    # restart a server
    def process_restart(self, name):
        try:
            s = self.find_server(name)
            self.imr.shutdown_server(s.name)
            time.sleep(3)
        except TaoServerNotFoundException, e:
            msg = 'ERROR: %s' % str(e)
            print msg
            return False
        except ImplementationRepository.NotFound, e:
            # this just means the server is not running
            pass

        try:
            # this is a dirty trick to force the start_limit to be reset
            self.imr.add_or_update_server(s.name, s.startup_options)

            # and now actually start the server
            self.imr.activate_server(s.name)
        except ImplementationRepository.CannotActivate, e:
            msg = 'ERROR: unable to start %s: %s' % (s.name, e.reason)
            print msg
            return False

        # it worked!
        return True

    # process list
    def process_list(self, machine=None, tree=False, running=None, stopped=None):

        # single machine view
        if machine is not None:
            if running is None:
                running = True
            if stopped is None:
                stopped = True
            self.list_servers_mach(machine, running, stopped)
            return

        # site-wide tree view
        if tree is True:
            if running is None:
                running = False
            if stopped is None:
                stopped = True
            self.list_servers_tree(running, stopped)
            return

        # site wide list view
        if running is None:
            running = False
        if stopped is None:
            stopped = True
        self.list_servers_flat(running, stopped)
        return

################################################################################
### A very basic test harness
################################################################################

def test_header(header):
    print '=' * 80
    print '=== %-72s ===' % header
    print '=' * 80

def main():
    import socket

    # default to the current machine for testing
    fqdn = socket.getfqdn()
    imrhost = 'corbaloc::' + fqdn + ':20000'
    sys.argv += ['-ORBDefaultInitRef', imrhost]
    orb = CORBA.ORB_init(sys.argv, CORBA.ORB_ID)

    # now get the reference to the IMR
    obj = orb.resolve_initial_references('ImplRepoService')
    imr = obj._narrow(ImplementationRepository.Administration)

    # and now the control interface object
    tao = TaoImrClient(imr)

    # yay, test code
    test_header('Updating Server Info')
    tao.update_server_info()

    test_header('Flat Process List')
    tao.process_list()

    test_header('Per machine list for %s' % fqdn)
    tao.process_list(machine=fqdn)

    test_header('Tree Process List')
    tao.process_list(tree=True)

    test_header('Tree Process List (full)')
    tao.process_list(tree=True, running=True)

    test_header('Individual Process Status')
    tao.process_status('faultsystem')

    test_header('Individual Process Status (bogus)')
    tao.process_status('bogus')

if __name__ == '__main__':
    main()

# vim: set ts=4 sts=4 sw=4 noet:
