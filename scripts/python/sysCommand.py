# $Id: sysCommand.py,v 1.1 2009/08/22 00:39:12 lamb Exp $
#
# Functions for running system commands in a shell, with formatting options.
#
# @Author James W. Lamb, Caltech
#
# History
# 14-Aug-2009: JWL  Original version
#

import os
import subprocess
import printFunctions
import exceptions

#
#----
#
class ShellCommandError(exceptions.Exception) :
    """Error definition for runSysCmd"""

    def __init__(self):
        return

    def __str__(self):
        print "","Error running a system command"

# end ShellCommandError

#
#----
#
def runSysCmd(cmd, errorLevel = 2, printToScreen = True, line1Char = '', line2Char = '', \
              count = 60, color = 'off') :

    """ Runs the specified command line and prints it to the screen.

        The command is run as a child process using the 'subprocess.Popen()' command.

        Example:
            runSysCmd('ls -l', errorLevel = 1, printToScreen = True, line1Char = '=', \
                      line2Char = '-', count= 60, color = 'blue')

        Parameters

        cmd
            Command string, including command line arguments, to
            execute in the shell.

        errorLevel = 2
            0:    Ignores errors running the command, but returns the error code
            1:    Prints a warning and returns the error code
            2:    Prints a warning and raises the exception 'ShellCommandError'

        printToScreen = True
            Prints the command line to the screen if True

        linesChar1 = ''
            If not empty, a line of the supplied character(s) printed 'count'
            times before the command is executed.

        linesChar2 = ''
            If not empty, a line of the supplied character(s) printed 'count'
            times after the command is executed. If empty, defaults to 'line1Char'.

        count = 60
            Number of times to print delimiters

        color
            Color for delimiters and command line ('red', 'blue', etc). Does
            not apply to the output of the function.
    """

#
#----Leading delimiter line if required----
#
    if not line2Char :
        line2Char = line1Char
    if (line1Char != '') :
        printFunctions.printInColor(count * line1Char, color = color, linefeed = True)

#
#----Print command line to screen (if required) and then execute command----
#
    if printToScreen :
        printFunctions.printInColor(cmd, color = color, linefeed = True)
    p = subprocess.Popen(cmd, shell = True)
    status = os.waitpid(p.pid, 0)
#
#----Check return status and decide whether an exception is to be thrown----
#
    if status[1] != 0 :
        if errorLevel > 0 :
            printFunctions.printWarning("Command failed. Error code = %d" % (status[1]))
            if errorLevel > 1 :
                raise ShellCommandError

#
#----Trailing delimiter line if required----
#
    if (line2Char != '') :
        printFunctions.printInColor(count * line2Char, color = color, linefeed = True)
    return (status[1])

# end runSysCmd