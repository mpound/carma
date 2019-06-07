import sys
import email
import email.utils
import os
import mimetypes
import smtplib
from email.mime.text import MIMEText

COMMASPACE = ', '

def send_email(message):
    from_email = "Fast-track Robot <no-reply@mmarray.org>"
    send_list = ["Douglas Friedel <friedel@astro.illinois.edu>"]

    subject = "[FAST-TRACK] import error"

    mail = MIMEText(message)
    mail['Subject'] = subject
    mail['To'] = COMMASPACE.join(send_list)
    mail['From'] = from_email
    mail['Date'] = email.utils.formatdate(localtime=True)
    mail['Message-ID'] = email.utils.make_msgid()

    sender = smtplib.SMTP("mail.ovro.caltech.edu")
    sender.sendmail(from_email, send_list, mail.as_string())
    sender.quit()

try:
    content = sys.stdin.read()

    tfl = open("/tmp/fast_track_tmp",'w')

    headers = email.Parser.Parser().parsestr(content)

    pid = headers['subject']

    fl = open("/opt/sdp/fast_track/" + pid + ".eml",'w')
    fl.write(content)
    fl.close()

    ret = os.system("/opt/rt/scripts/sac --imr acc --noinit <<EOF\nimport fast_track as ft\nft.import_fast_track()\nexit\nEOF")

    if(ret != 0):
        send_email("Import failed for fast track project %s. The error was encounterred in the main import script. This is just an informational message." % (pid))

except Exception, e:
     send_email("Import failed for a fast track project. The error was encounterred in the main import script: %s. This is just an informational message." % (e))
