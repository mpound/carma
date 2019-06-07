from mod_python import apache
from mod_python import Session
from mod_python import util
import os
import sys
import traceback
import smtplib
import pprint
import types
import hashlib
import datetime

_backend = apache.import_module("backend")
db = None
text = apache.import_module("text")

page = apache.import_module("page")

_connector = apache.import_module("connector")
connector = None

Template = apache.import_module("Template/__init__")

config = { 'html_base' : "http://carma-prop.astro.illinois.edu/summerschool/",
           'base_directory' : '/srv/www/htdocs/summerschool/',
           'files_directory' : 'files/',
           'data_directory' : '/home/carmaweb/cpss-data/',
           'sendemail' : True,
           'error_email' : '',
           'db' : { 'host' : "",
                    'user' : "",
                    'passwd' : "",
                    'db' : "",
                    'unix_socket' : '/var/run/mysqld/mysqld.sock',
                    },
           }

options = None # This will be filled with the SQL options.
session = None
req = None

def w(string):
    if req != None:
        req.write(string)
    return

def print_exc_plus(skip=None):
    """
    Print the usual traceback information, followed by a listing of all the
    local variables in each frame.
    """
    buff = ''

    tb = sys.exc_info()[2]
    while 1:
        if not tb.tb_next:
            break
        tb = tb.tb_next
    stack = []
    f = tb.tb_frame
    while f:
        stack.append(f)
        f = f.f_back
    stack.reverse()
    traceback.print_exc()
    buff += "Locals by frame, innermost last\n"
    for frame in stack:
        buff += '\n'
        buff += "Frame %s in %s at line %s\n" % (frame.f_code.co_name,
                                             frame.f_code.co_filename,
                                             frame.f_lineno)
        for key, value in frame.f_locals.items():
            buff += "%20s = " % key
            #We have to be careful not to cause a new error in our error
            #printer! Calling str() on an unknown object could cause an
            #error we don't want.
            try:                   
                if value == skip:
                    buff += "<<VALUE SKIPPED>>\n"
                else:
                    buff += str(value) + '\n'
            except:
                buff += "<ERROR WHILE PRINTING VALUE>\n"
    return buff

def handler(request):
    os.umask(2)

    global req
    req = request

    #Figure out which page is requested.
    pathstr = []
    for item in req.path_info.split('/'):
        if item != '':
            pathstr.append(item)

    #begin session and read information 6 hour timeout
    global session
    session = Session.Session(req, timeout=21600)
    if session.is_new():
        session['authenticated'] = False
        session['activated'] = 0
        session['admin'] = False
        session['maint_mode'] = 0
        session['maint_allow'] = False
    session.save()
        
    # Initialize database and connect. ##TODO: Add error checking.
    global db
    db = _backend.Backend()

    global options
    options = db.options_get()

    global connector
    connector = _connector.Connector()

    if (session['admin'] == False):
        try:
            result = connector.Dispatch(pathstr)
        except apache.SERVER_RETURN:
            # This exception is raised when there is a page redirect.
            # If we dont handle it here a logon/redirect will generate an
            # error.
            return apache.OK
        except:
            mail = smtplib.SMTP()
            mail.connect()
            msg = "Session Variables: \n"
            msg += pprint.pformat(session) + '\n\n'

            msg += traceback.format_exc()

            msg += '\n'
            msg += "Apache Request: \n"
            for i in dir(req):
                if i in ['finfo']:
                    continue
                if callable(getattr(req, i)):
                    continue
                itemval = getattr(req, i)
                if type(itemval) == type(req.headers_in):
                    itemstr = pprint.pformat(dict(itemval)).replace('\n', 
                                                                    '\n\t\t')
                else:
                    itemstr = pprint.pformat(itemval)
                msg += "\t %s = %s\n" % (i, itemstr) 

            msg += '\n'
            msg += "Apache Connection: \n"
            for i in dir(req.connection):
                if callable(getattr(req.connection, i)):
                    continue
                msg += "\t" + i + " = " + str(getattr(req.connection, i)) \
                    + '\n'

            msg += '\n'
            msg += "Apache Server: \n"
            for i in dir(req.server):
                if callable(getattr(req.server, i)):
                    continue
                if i.startswith("__"):
                    continue
                msg += "\t" + i + " = " + \
                    pprint.pformat(getattr(req.server, i)) + '\n'

            msg += print_exc_plus(skip=msg) + '\n\n'

            hash_msg = hashlib.md5(msg).hexdigest()[0:10]
            date_msg = datetime.datetime.now()

            header = 'From: "CARMA Proposal System" <no_not_reply@carma-prop.astro.illinois.edu>\n'
            header += "To: %s\n" % config['error_email']
            header += 'Subject: CPSS Error -- ID: %s Time: %s\n' % (hash_msg, date_msg)
            header += 'Content-Type: text/plain;\n'

            msg = header + msg

            mail.sendmail("do_not_reply@carma-prop.astro.illinois.edu", 
                          config['error_email'], msg)
            mail.quit()

            req.content_type = 'text/html'
            req.write(text.page_error % (hash_msg, date_msg))
            result = apache.OK
        finally:
            db.close()
    else:
        try:
            result = connector.Dispatch(pathstr)
        finally:
            db.close()

    return result
