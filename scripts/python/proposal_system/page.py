from mod_python import apache
cpss = apache.import_module("cpss")

def header(login=False, refresh=None, logon=False):
    logout_bar = [["Login", "login/"], ["Help","help/"], 
                  ["Create Account","create/"]]
    login_bar =  [["Proposals", "proposal/"], ["User Info","user/"],
                  ["Help","help/"], ["Logout", "logout/"]]

    if (refresh != None):
        cpss.req.headers_out['location'] = cpss.config['html_base']+refresh
        cpss.req.status = apache.HTTP_MOVED_TEMPORARILY
        raise apache.SERVER_RETURN, apache.OK

    cpss.req.headers_out['Expires'] = 'Thu, 19 Nov 1981 09:52:00 GMT'
    cpss.req.headers_out['Cache-Control'] = 'no-store, no-cache, must-revalidate, post-check=0, pre-check=0'
    cpss.req.headers_out['Pragma'] = 'no-cache'
    cpss.req.content_type = "text/html"

    maintain = ""
    if (cpss.session.__contains__('maint_mode') == True):
        if (cpss.session['maint_mode'] == 1):
            maintain = """<div class="maintenance">""" + cpss.options['maint_warn'] + "</div>"
        elif ((cpss.session['maint_mode'] == 2) and
              (cpss.session['maint_allow'] == True)):
            maintain = """<div class="maintenance">A maintenance or debugging cycle is currently in effect. Beware that some features currently are not working as intended. To see the maintenance page that is shown to people who do not have access during a maintenance cycle, please click <a href='invalidate/'>here</a>. This will log out out if you are currently logged in.</div>"""

    if (logon==True):
        onLoad = "document.getElementById('login').style.visibility='visible';"
    else:
        onLoad = ""
            
    cpss.w(cpss.text.page_header % (cpss.config['html_base'], onLoad, 
                                    maintain, cpss.config['html_base']))

    if (login == False):
        bar = logout_bar
    else:
        bar = login_bar

    for entry in bar:
        cpss.w("""<li><a href="%s">%s</a></li>""" % (entry[1], entry[0]))

    cpss.w("""</ul></div><div class="content">""")

def footer():
    #The second set of head tags assure that Internet Explorer will not
    #cache these pages. This would be bad...
    cpss.w(cpss.text.page_footer)
        
def do_404():
    cpss.w("<center>This page cannot be found</center>")
        
def logon(Error='', Username=''):
    cpss.w(cpss.text.page_logon % (Error, Username))

def user_create(Name="", Email="", Error="", random=""):
    buffer = ("""
        If you wish to create an account, please fill out this form and submit
        it. You will recieve an e-mail shortly after which will contain
        additional instructions on how to login for the first time.""")

    if (Error != ""):
        buffer = buffer + ("""<br><center><font color=red>%s
                                  </font></center>""" % Error)

    buffer = buffer + ("""<form action="create/" method="post">
        <center>
        <table><tr><td>Full Name</td><td><input type="text" name="name"
                       value='%s'></td></tr>
               <tr><td>E-Mail Address</td><td><input type="text" name="email"
                       value='%s'></td></tr>
               <tr><td>Password</td><td><input type="password"
                       name="password1"></td></tr>
               <tr><td>Retype Password</td><td><input type="password"
                       name="password2"></td></tr>
               <tr><td></td><td><input type="hidden" name="random"
                       value='%s'><input type="submit" name="submit"
                       value="Submit"></td></tr>
        </table>
        </center>
        </form>""" % (Name, Email, random))
    cpss.w(buffer)       

def user_create_success(name, email):
    cpss.w("""<center>
        Welcome, %s, you have successfully registered yourself in the Carma
        Proposal Submission System. You will shortly recieve an e-mail at
        the address you registered with, %s. You must log-in and enter the
        supplied code in order to gain access. This is done as a security
        measure to make sure that your e-mail address is accurate.</center>
        """ % (name, email))

def main():
    cpss.w(cpss.text.page_main)

def activate(Error=""):
    cpss.w("""
        You have not yet confirmed your account. Please enter your activation
        code below, or, if you have lost or not recieved the e-mail, press the
        generate button to generate a new e-mail.

        <br><font color="red">%s</font>

        <center><form action="proposal/" method="post">
        <table><tr><td>Activation Code:</td><td>
        <input type=text name="activate"></td><tr></table>
        <input type=submit name="sub_activate" value="Activate Account">
        </form>
        <br>
        If you need a new activation code sent, please click the button below.
        <form action="" method="post">
        <input type=submit name="sub_activate" value="Resend Activation Code">
        </form>
        </center>""" % Error)

def userpage(name, email, error=""):
    if (error == ""):
        error_style = "display : none;"
    else:
        error_style = ""

    cpss.w(cpss.text.page_user % { 'errorsty' : error_style,
                                   'error' : error,
                                   'name'  : name,
                                   'email' : email } )

def proposal_list(list, name):
    if cpss.options["create"] == True:
        cpss.w("""<h3>%s's Proposals
                 (<a href="proposal/add">Add New Proposal</a>)</h3>""" % name)
    else:
        cpss.w("""<h3>%s's Proposals</h3>""" % name)

    if (len(list) == 0):
        cpss.w("""<div id="proplist"><p><i>You have no saved
            proposals. Click the new button above to add one.</i></p></div>""")
    else:
        buf = """<div id="proplist"><table><tr>
                     <th>ID</th><th>Title</th><th>Status</th>
                     <th>Action</th><th>Data Password</th></tr>"""
        for entry in list:
            if (entry['status'] == 0):
                carmaid = 'None'
                status = "Unsubmitted"
                pdf = ("""<a href="proposal/delete/%s">delete</a>""" %
                       entry['proposalid'])
                password = ""
            else:
                carmaid = str(entry['carmaid'])
                status = 'Submitted'
                pdf =  ("""<a href="proposal/finalpdf/%s">view final pdf</a>""" %
                        (entry['proposalid']))
                password = str(entry['carmapw'])
            if ((entry['cyclename'] != cpss.options['cyclename']) and (entry['status'] == 1)):
                #placeholder
                buf += ("""<tr><td>%s</td><td id="title">%s</td><td>%s</td>
                    <td>  %s </td><td>%s</td></tr>"""
                        % (carmaid, entry['title'], status, pdf, password))
            else:
                buf += ("""<tr><td id="id">%s</td><td id="title">%s</td><td>%s</td>
                    <td><a href="proposal/edit/%s">edit</a> | %s </td><td>%s</td></tr>"""
                    % (carmaid, entry['title'], status, entry['proposalid'],
                       pdf, password))
        buf += """</table></div>""" 
        cpss.w(buf)
        
def delete_verify(pathtext, title):
    cpss.w("""<center>Do you wish to delete the proposal, "%s"?

        <form action="%s" method="post">
        <input type=submit name="delete" value="Delete Proposal"></form>

        <form action="proposal/" method="post">
        <input type=submit name="cancel" value="Cancel"></form> """ %
                       (title, pathtext))
        
