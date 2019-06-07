# system imports
import email
import email.utils
import sys
import re
import os
import mimetypes
import smtplib

import carma
import device as dv
import subarrayCommands as sc
from time import sleep
import projectManage as pm

COMMASPACE = ', '

from email.mime.text import MIMEText
from email.MIMEMultipart import MIMEMultipart

from_email = "Fast-track Robot <no-reply@mmarray.org>"
fail_list = ["Douglas Friedel <friedel@astro.illinois.edu>"]
success_list = ["Douglas Friedel <friedel@astro.illinois.edu>"]
srcDir = "/opt/sdp/fast_track"
scriptDir = "/misc/array/rt/scripts/fastTrack"
catalogDir = "/array/rt/catalogs"
def send_email(message,pid,success=False):
    global fail_list
    global success_list
    global from_email

    send_list = success_list
    subject = "[FAST-TRACK] " + pid
    if(not success) :
        subject += " import issue"
        send_list = fail_list
    else :
        time = message
        message = "Project %s has been added to the PDB by the fast track proposal system and is ready to have time allocated. The PI has requested %s hours" % (pid,time)

    mail = MIMEText(message)
    mail['Subject'] = subject
    mail['To'] = COMMASPACE.join(send_list)
    mail['From'] = from_email
    mail['Date'] = email.utils.formatdate(localtime=True)
    mail['Message-ID'] = email.utils.make_msgid()

    sender = smtplib.SMTP("mail.ovro.caltech.edu")
    sender.sendmail(from_email, send_list, mail.as_string())
    sender.quit()

def send_script(sendList,pid,oid) :
    fileName = scriptDir + "/" + pid + "_" + oid + ".obs"
    catalog = catalogDir + "/" + pid + ".cat"
    text = "This email is a confirmation that project %s has been received. It should be added to the observing queue within 2 business days (see http://cedarflat.mmarray.org/schedule for the most recent queue). A copy of your observing script and catalog file are attached to this email and are for reference only.\n\nYour project will be observed when time becomes available; you need do nothing further to acquire the data. You will received an email when your observations start on the array, and one when the observations are completed." % (pid)
    msg = MIMEMultipart()
    msg['From'] = from_email
    msg['To'] = COMMASPACE.join(sendList)
    msg['Subject'] = "CARMA fast track confirmation"
    msg['Date'] = email.utils.formatdate(localtime=True)
    msg['Message-ID'] = email.utils.make_msgid()
    msg.attach( MIMEText(text) )
    part1 = MIMEText(file(fileName).read(),_subtype="plain")
    part1.add_header('Content-Disposition', 'attachment', filename="%s_%s.obs" % (pid,oid))
    part2 = MIMEText(file(catalog).read(),_subtype="plain")
    part2.add_header('Content-Disposition', 'attachment', filename="%s.cat" % (pid))
    msg.attach(part1)
    msg.attach(part2)

    sender = smtplib.SMTP("mail.ovro.caltech.edu")
    sender.sendmail(from_email, sendList, msg.as_string())
    sender.quit()




def parseMail(fl) :
    headers = email.Parser.Parser().parse(open(fl,'r'))
    em = email.message_from_file(open(fl,'r'))

    if(not 'no-reply@carma-prop.astro.illinois.edu' in headers['from']) :
        sys.exit(1)

    pid = headers['subject']

    #send_email("Starting parse of %s" % (pid),pid)

    for part in em.walk() :
        if part.get_content_maintype() == 'multipart':
            continue
        filename = part.get_filename()
        if(filename == None) :
            continue
        if('export' in filename) :
            tpdbData = part.get_payload(decode=True)
        elif('script' in filename) :
            script = part.get_payload(decode=True)

    #print tpdbData,"\n\n"
    ttpdbData = tpdbData.replace("\n ","")
    pdbData = ttpdbData.replace("\n","")
    # get current obsblock
    s = pdbData.find('<obsblockID>')
    f = pdbData.find('</obsblockID>')
    oid = pdbData[s+12:f]

    # get time requested
    s = pdbData.find('<time>')
    f = pdbData.find('</time>')
    time = pdbData[s+6:f]

    # get PI's email
    s1 = script.find('<email>')
    f1 = script.find('</email>')
    pi_email = script[s1+7:f1]

    #print pi_email,"\n\n"

    #print pdbData,"\n\n"
    tempPdb = pdbData[:s]
    #print tempPdb,"\n\n"
    newPdbData = tempPdb.replace("\n","")
    
    pdb = dv.getProjectDbMgr()

    try:
        #print newPdbData
        success = pdb.projectAddAsString(newPdbData)
        
        if(not success) :
            send_email("The project %s was not successfully added to the PDB by the fast track proposal system. This is an informational email only." % (pid),pid)
            return False
    except Exception, e :
        #print "FAIL ",e
        send_email("The project %s was not successfully added to the PDB by the fast track proposal system, an exception was thrown. This is an informational email only.\n\n%s" % (pid,e),pid)
        return False
    sleep(10.0)
    try:
        aiv = carma.observertools.ItemValue("project",pid)
        prj = pdb.projectQuery([aiv])
        if(len(prj) != 1) :
            send_email("The project %s was loaded in the PDB by the fast track proposal system but it cannot be found. This is an informational message only." % (pid),pid)
            return False
    except Exception, e :
        send_email("The project %s was loaded in the PDB by the fast track proposal system but it cannot be found, an exception was thrown. This is an informational message only.\n\n%s" % (pid,e),pid)
        return False

    try:
        multiplier = float(pid[2:])
        priority = 50 - multiplier * 0.001
        success = pm.changePriority(pid, oid, priority)
        if(not success) :
            send_email("The project %s did not have its priority entered as it should be in the PDB. This is an informational email only." % (pid),pid)
            return False
        success	= pm.changeItem(pid, oid,"isFlex","True")
        if(not success) :
            send_email("The project %s did not have its flex status entered as it should be in the PDB. This is an informational email only." % (pid),pid)
            return False

    except Exception, e:
        send_email("The project %s did not have its priority entered as it should be in the PDB, an exception was thrown. This is an informational email only.\n\n%s" % (pid,e),pid)
        return False

    fl = open(srcDir + "/" + pid + "-script.xml",'w')
    fl.write(script)
    fl.close()

    # now generate the script
    try:
        ret = os.system("export LD_LIBRARY_PATH=/array/utilities/jmc/db/sw/cgicc-3.2.3/cgicc/.libs;/array/utilities/apache2.0.54/cgi-bin/mkscript/mkscript %s/%s-script.xml" % (srcDir,pid))
        if(ret != 0):
            send_email("The project %s did not have a script generated, the call returned an error. This is an imformational email only." % (pid),pid)
            return False
    except Exception, e:
        send_email("The project %s did not have its script generated, an exception was thrown. This is an informational email only.\n\n%s" % (pid,e),pid)
        return False

    # success
    send_email(time,pid,True)
    send_script([pi_email],pid,oid)
    return True


def scan_for_files() :
    global srcDir
    entries = os.listdir(srcDir)
    files = []
    for entry in entries :
        if re.match(".*\.eml", entry) :
            files.append(entry)
    return files

def import_fast_track() :
    global srcDir
    try:
        files = scan_for_files()
        if(len(files) == 0) :
            return
        for fl in files :

            name = fl.replace(".eml",".rd")
            os.rename(srcDir + "/" + fl, srcDir + "/" + name)

            success = parseMail(srcDir + "/" + name)

            if(success) :
                os.remove(srcDir + "/" + name)
    except Exception, e :
        send_email("Fast track import failed %s" % (e),"INIT")
