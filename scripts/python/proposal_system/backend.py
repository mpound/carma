import MySQLdb
import hashlib
import string
import os.path
import os
import gzip
from random import choice
from mod_python import apache
cpss = apache.import_module("cpss")

class Backend:
    def __init__(self):
        self.prefix = 'cs_'
        self.Database = MySQLdb.connect(host = cpss.config['db']['host'],
                                        user = cpss.config['db']['user'],
                                        passwd = cpss.config['db']['passwd'],
                                        db = cpss.config['db']['db'],
                                        unix_socket = cpss.config['db']['unix_socket'])
        self.literal = self.Database.literal
        self.options = self.options_get()
        self.path_justification = cpss.config['data_directory'] + self.prefix + 'justifications/'
        self.path_pdf = cpss.config['data_directory'] + self.prefix + 'pdf/'
        self.path_images = cpss.config['data_directory'] + self.prefix + 'images/'


    def verify_user(self, username, password):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        response = cursor.execute("""SELECT * FROM users
                                     WHERE email=%(username)s LIMIT 1"""
                                  % {'username': self.literal(username)})
        result = cursor.fetchall()
        if (len(result) == 0):
            return (False, 0)
        user = result[0]
        md5_pass = hashlib.md5(password).hexdigest()
        cursor.close()
        if((user['email'] == username) and (user['password'] == md5_pass)):
            return (True, user)
        return (False, 0)


    def get_user(self, username):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        response = cursor.execute("""SELECT * FROM users
                                     WHERE email=%(username)s LIMIT 1"""
                                  % {'username': self.literal(username)})
        result = cursor.fetchone()
        return result

    def test_userflag(self, username, flag):
        result = self.get_user(username)

        # list of userflags:
        # 1 : STATS - Can see stats page of calls.
        flags = {
            'STATS' : 1,
            }
        if flag in flags.keys():
            if (result['flags'] & flags[flag]) == flags[flag]:
                return True
            else:
                return False
        else:
            return False


    def user_exists(self, username):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        response = cursor.execute("""SELECT * FROM users
                                     WHERE email=%(username)s LIMIT 1""" %
                                  {'username' : self.literal(username)})
        result = cursor.fetchall()
        cursor.close()
        if (len(result) == 1):
            return True
        return False

    def add_user(self, Name, Email, Password, Code):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        response = cursor.execute("""INSERT INTO users SET"""  +
                                  """ name=%(name)s, email=%(email)s,
                                     password=MD5(%(password)s),
                                     activated=%(code)s""" %
                                  {'name'     : self.literal(Name),
                                   'email'    : self.literal(Email),
                                   'password' : self.literal(Password),
                                   'code'     : self.literal(Code)})
        cursor.close()

    def password_change(self, Email, Newpassword):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        response = cursor.execute("""UPDATE users SET
                                     `password`=MD5(%(code)s) WHERE
                                     `email`=%(email)s""" %
                                  {'code'   : self.literal(Newpassword),
                                   'email'  : self.literal(Email)})
        cursor.close()
        
    def update_code(self, user, code):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        response = cursor.execute("""UPDATE users SET
                                     `activated`=%(code)s WHERE
                                     `email`=%(email)s""" %
                                  {'code'   : self.literal(code),
                                   'email'  : self.literal(user)})
        cursor.close()

    def cycles(self):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        ret = cursor.execute("""SELECT `cyclename`, `final_date`, `proposal`
                                FROM `%(prefix)scycles`
                                ORDER BY `final_date` DESC""" % 
                             {'prefix' : self.prefix})
        result = cursor.fetchall()
        cursor.close()
        return result

    def proposal_list_by_cycle(self, cyclename):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        ret = cursor.execute("""SELECT *
                                FROM `%(prefix)sproposals` as `proposals`,
                                     `users`
                                WHERE `proposals`.`cyclename`='%(cyclename)s' AND
                                      `proposals`.`user`=`users`.`email`
                                ORDER BY `proposals`.`status` DESC,
                                      `proposals`.`carmaid` DESC""" %
                             {'prefix' : self.prefix,
                              'cyclename' : cyclename})
        result = cursor.fetchall()
        cursor.close()
        return result

    def proposal_get_propinfo(self, proposalid, tablename):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        response = cursor.execute("""SELECT *
                                     FROM `%(prefix)s%(tablename)s` as propinfo
                                     WHERE `propinfo`.`proposalid`='%(proposalid)s'
                                     LIMIT 1""" % {'prefix' : self.prefix,
                                                   'tablename' : tablename,
                                                   'proposalid' : proposalid})
        result = cursor.fetchone()
        cursor.close()
        return result

    def proposal_list(self, user):
        #unfortunately this requires many calls to the db. Any other idea
        #without restructuring the database?
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        response = cursor.execute(
            """SELECT %(prefix)sproposals.proposalid,
               %(prefix)sproposals.cyclename, %(prefix)sproposals.user,
               %(prefix)sproposals.carmaid, %(prefix)sproposals.carmapw,
               %(prefix)scycles.template,
               %(prefix)scycles.proposal as proposal_table,
               %(prefix)scycles.final_date,
               %(prefix)sproposals.status
               FROM %(prefix)sproposals, %(prefix)scycles
               WHERE %(prefix)sproposals.user=%(user)s AND
                     %(prefix)scycles.cyclename=%(prefix)sproposals.cyclename
               ORDER BY %(prefix)scycles.final_date
               DESC, %(prefix)sproposals.proposalid""" %
            {'prefix' : self.prefix,
             'user'   : self.literal(user)})
        result = cursor.fetchall()
        list=[]
        for proposal in result:
            cursor.execute("""SELECT * FROM %(prefix)s%(table)s WHERE
                              proposalid=%(propid)s""" %
                          {'prefix' : self.prefix,
                           'table'  : proposal['proposal_table'],
                           'propid' : self.literal(proposal['proposalid'])})
            res = cursor.fetchone()
            proposal['title'] = res['title']
            proposal['date'] = res['date']
            list.append(proposal)

        cursor.close()
        return list

    def proposal_list_multi(self, user):
        #unfortunately this requires many calls to the db. Any other idea
        #without restructuring the database?
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        response = cursor.execute(
            """SELECT %(prefix)sproposals.proposalid,
               %(prefix)sproposals.cyclename, %(prefix)sproposals.user,
               %(prefix)sproposals.carmaid, %(prefix)sproposals.carmapw,
               %(prefix)scycles.template,
               %(prefix)scycles.proposal as proposal_table,
               %(prefix)scycles.final_date,
               %(prefix)sproposals.status
               FROM %(prefix)sproposals, %(prefix)scycles
               WHERE %(prefix)sproposals.user=%(user)s AND
                     %(prefix)scycles.cyclename=%(prefix)sproposals.cyclename
               ORDER BY %(prefix)scycles.final_date
               DESC, %(prefix)sproposals.proposalid DESC""" %
            {'prefix' : '',
             'user'   : self.literal(user)})
        result = cursor.fetchall()

        response = cursor.execute(
            """SELECT %(prefix)sproposals.proposalid,
               %(prefix)sproposals.cyclename, %(prefix)sproposals.user,
               %(prefix)sproposals.carmaid, %(prefix)sproposals.carmapw,
               %(prefix)scycles.template,
               %(prefix)scycles.proposal as proposal_table,
               %(prefix)scycles.final_date,
               %(prefix)sproposals.status
               FROM %(prefix)sproposals, %(prefix)scycles
               WHERE %(prefix)sproposals.user=%(user)s AND
                     %(prefix)scycles.cyclename=%(prefix)sproposals.cyclename
               ORDER BY %(prefix)scycles.final_date
               DESC, %(prefix)sproposals.proposalid DESC""" %
            {'prefix' : 'ddt_',
             'user'   : self.literal(user)})
        result2 = cursor.fetchall()

        response = cursor.execute(
            """SELECT %(prefix)sproposals.proposalid,
               %(prefix)sproposals.cyclename, %(prefix)sproposals.user,
               %(prefix)sproposals.carmaid,
               %(prefix)scycles.template,
               %(prefix)scycles.proposal_table as proposal_table,
               %(prefix)scycles.final_date,
               %(prefix)sproposals.status
               FROM %(prefix)sproposals, %(prefix)scycles
               WHERE %(prefix)sproposals.user=%(user)s AND
                     %(prefix)scycles.cyclename=%(prefix)sproposals.cyclename
               ORDER BY %(prefix)scycles.final_date
               DESC, %(prefix)sproposals.proposalid DESC""" %
            {'prefix' : 'cs_',
             'user'   : self.literal(user)})
        result3 = cursor.fetchall()

        list=[]
        for proposal in result:
            cursor.execute("""SELECT title FROM %(prefix)s%(table)s WHERE
                              proposalid=%(propid)s""" %
                          {'prefix' : '',
                           'table'  : proposal['proposal_table'],
                           'propid' : self.literal(proposal['proposalid'])})
            res = cursor.fetchone()
            proposal['title'] = res['title']
            list.append(proposal)
        for proposal in result2:
            cursor.execute("""SELECT title FROM %(prefix)s%(table)s WHERE
                              proposalid=%(propid)s""" %
                          {'prefix' : 'ddt_',
                           'table'  : proposal['proposal_table'],
                           'propid' : self.literal(proposal['proposalid'])})
            res = cursor.fetchone()
            proposal['title'] = res['title']
            list.append(proposal)
        for proposal in result3:
            cursor.execute("""SELECT title FROM %(prefix)s%(table)s WHERE
                              proposalid=%(propid)s""" %
                          {'prefix' : 'cs_',
                           'table'  : proposal['proposal_table'],
                           'propid' : self.literal(proposal['proposalid'])})
            res = cursor.fetchone()
            proposal['title'] = res['title']
            proposal['carmapw'] = ""
            list.append(proposal)

        cursor.close()
        return list

    def proposal_fetch(self, user, proposalid):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        cursor.execute("""SELECT * FROM %(prefix)sproposals, %(prefix)scycles
                          WHERE proposalid=%(propid)s AND
                          %(prefix)sproposals.cyclename=
                          %(prefix)scycles.cyclename""" %
                       {'prefix' : self.prefix,
                        'propid' : self.literal(proposalid)})
        res = cursor.fetchone()
        cursor.close()
        if user == 'admin':
            return res
        if (res == None):
            return False
        if (res['user'] != user):
            return False
        return res

    def proposal_status(self, propid):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        cursor.execute("""SELECT status FROM %(prefix)sproposals
                          WHERE proposalid=%(propid)s LIMIT 1""" %
                       {'prefix' : self.prefix,
                        'propid' : self.literal(propid)})
        ret = cursor.fetchone()
        cursor.close()
        return ret

    def proposal_get(self, tables, cyclename, proposalid, subid=False):
        result = {}
        table_list = tables.keys()
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        for key in table_list:
            if (tables[key]['type'] == 'single'):
                cursor.execute("""SELECT * FROM %(prefix)s%(cyclename)s
                                  WHERE proposalid=%(propid)s LIMIT 1""" %
                               {'prefix'    : self.prefix,
                                'cyclename' : self.options[key],
                                'propid'    : self.literal(proposalid)})
                temp = cursor.fetchall()

                result[key] = temp
            if (tables[key]['type'] == 'repeat'):
                cursor.execute("""SELECT * FROM %(prefix)s%(cyclename)s
                                  WHERE proposalid=%(propid)s
                                  ORDER BY numb""" %
                               {'prefix'    : self.prefix,
                                'cyclename' : self.options[key],
                                'propid'    : self.literal(proposalid)})
                temp = cursor.fetchall()

                result[key] = temp
        cursor.close()
        return result

    def proposal_tagset(self, table, propid, tags, id=False):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        tagtext = ""
        wheretext = ""
        for tag in tags:
            if (tag['fieldname'] == 'numb'):
                continue
            elif (tag['fieldname'] == 'date'):
                tagtext += """`%s`= CURDATE(), """ % (tag['fieldname'])
            elif (tag['data'] == None):
                tagtext += """`%s`= NULL, """ % (tag['fieldname'])
            elif (tag['fieldtype'] == 'array'):
                tagtext = "`%s`='" % (tag['fieldname'])
                for array in tag['data']:
                    tagtext += array + ","
                tagtext = tagtext[:-1]
                tagtext += "',"

            else:
                tagtext += """`%s`=%s, """ % (tag['fieldname'],
                                            self.literal(tag['data']))
        tagtext = tagtext[:-2]

        if (id == False):
            wheretext = """proposalid=%s""" % self.literal(propid)
        else:
            wheretext = ("""proposalid=%s AND numb=%s""" %
                         (self.literal(propid), self.literal(id)))
        text = ("""UPDATE %(prefix)s%(table)s SET %(tag)s WHERE %(where)s""" %
                {'prefix' : self.prefix,
                 'table'  : self.options[table],
                 'tag'    : tagtext,
                 'where'  : wheretext}) 
        
        cursor.execute(text)
        cursor.close()
    
    def proposal_table_addrow(self, table, propid, numb=False):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        if (numb != False):
            cursor.execute("""SELECT * FROM %(prefix)s%(table)s
                              WHERE proposalid=%(propid)s
                              ORDER BY numb DESC""" %
                           {'prefix' : self.prefix,
                            'table'  : self.options[table],
                            'propid' : self.literal(propid)})
            res = cursor.fetchone()
            numbtext = ", numb=%s" % self.literal((res['numb'] + 1))
        else:
            numbtext = ""
            res = {'numb' : -1}
            
        cursor.execute("""INSERT INTO %(prefix)s%(table)s
                          SET proposalid=%(propid)s %(numbtext)s""" %
                       {'prefix'   : self.prefix,
                        'table'    : self.options[table],
                        'propid'   : self.literal(propid),
                        'numbtext' : numbtext})
        cursor.close()
        return str(res['numb'] + 1)

    def proposal_table_delrow(self, table, propid, numb=False):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        if (numb != False):
            numbtext = "AND numb=%s" % (self.literal(numb))
            cursor.execute("""SELECT numb FROM %(prefix)s%(table)s
                              WHERE proposalid=%(propid)s
                              ORDER BY numb""" %
                           {'prefix' : self.prefix,
                            'table'  : self.options[table],
                            'propid' : self.literal(propid)})
            res = cursor.fetchall()
            if (len(res) == 1):
                cursor.close()
                return False
        else:
            numbtext = ""
            
        cursor.execute("""DELETE FROM %(prefix)s%(table)s
                          WHERE `proposalid`=%(prop)s %(numbtext)s
                          LIMIT 1""" %
                       {'prefix'   : self.prefix,
                        'table'    : self.options[table],
                        'prop'     : self.literal(propid),
                        'numbtext' : numbtext})

        if (numb != False):
            new_numb = 1
            for line in res:
                if (str(line['numb']) == str(numb)):
                    continue
                elif (line['numb'] == new_numb):
                    new_numb += 1
                    continue
                else:
                    cursor.execute("""UPDATE %(prefix)s%(table)s
                                      SET numb=%(new_numb)s WHERE
                                      proposalid=%(propid)s AND
                                      numb=%(linenumb)s LIMIT 1""" %
                                   {'prefix'   : self.prefix,
                                    'table'    : self.options[table],
                                    'new_numb' : self.literal(new_numb),
                                    'propid'   : self.literal(propid),
                                    'linenumb' : self.literal(line['numb'])})
                    new_numb += 1
        cursor.close()
        return True

    def options_get(self):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        cursor.execute("""SELECT %(prefix)soptions.cyclename,
                          %(prefix)scycles.template,
                          %(prefix)scycles.proposal,
                          %(prefix)scycles.author,
                          %(prefix)scycles.source,
                          %(prefix)soptions.admin_pw,
                          %(prefix)soptions.maint_mode,
                          %(prefix)soptions.maint_key,
                          %(prefix)soptions.maint_warn,
                          %(prefix)soptions.maint_message,
                          %(prefix)soptions.next_propno,
                          %(prefix)soptions.create
                          FROM %(prefix)soptions, %(prefix)scycles WHERE
                          %(prefix)soptions.cyclename=
                          %(prefix)scycles.cyclename
                          LIMIT 1""" % {'prefix':self.prefix})
        res = cursor.fetchone()
        cursor.close()
        return res

    def set_next_propno(self, nextpropno, cyclename):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        cursor.execute("""UPDATE %(prefix)soptions SET next_propno=%(propno)s
                          WHERE cyclename=%(cyclename)s""" %
                       {'prefix'    : self.prefix,
                        'propno'    : self.literal(nextpropno),
                        'cyclename' : self.literal(cyclename)})
        cursor.close()

    def proposal_setcarmaid(self, proposalid, carmaid):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        cursor.execute("""UPDATE %(prefix)sproposals SET carmaid=%(carmaid)s
                          WHERE proposalid=%(propid)s """ %
                       {'prefix'  : self.prefix,
                        'carmaid' : self.literal(carmaid),
                        'propid'  : self.literal(proposalid)})
        cursor.close()

    def proposal_submit(self, proposalid):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        cursor.execute("""UPDATE %(prefix)sproposals SET status=%(status)s
                          WHERE proposalid=%(propid)s """ %
                       {'prefix' : self.prefix,
                        'status' : self.literal(str(1)),
                        'propid' : self.literal(proposalid)})
        cursor.close()

    def pw_generate(self, proposalid):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        #Generate the random password
        size=8
        #password = ''.join([choice(string.letters + string.digits + "!" + "@" + "_") for i in range(size)])
        password = ''
        cursor.execute("""UPDATE %(prefix)sproposals SET carmapw=%(pw)s
                          WHERE proposalid=%(propid)s """ %
                       {'prefix' : self.prefix,
                        'pw'     : self.literal(password),
                        'propid' : self.literal(proposalid)})        
        cursor.close()

    def proposal_add(self, user, tables, cyclename):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        cursor.execute("""INSERT INTO %(prefix)sproposals SET user=%(user)s,
                          cyclename=%(cyclename)s, status='none'""" %
                       {'prefix'    : self.prefix,
                        'user'      : self.literal(user),
                        'cyclename' : self.literal(cyclename)})
        cursor.execute("""SELECT proposalid FROM %(prefix)sproposals WHERE
                          user=%(user)s ORDER BY proposalid DESC""" %
                       {'prefix' : self.prefix,
                        'user'   : self.literal(user)})
        proposalid = cursor.fetchone()['proposalid']

        for table in tables.keys():
            self.proposal_table_addrow(table, proposalid)

        cursor.close()
        return proposalid

    def proposal_delete(self, user, proposalid, tables, cyclename):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        the_tables = tables.keys()
        for table_name in tables.keys():
            cursor.execute("""DELETE FROM %(prefix)s%(cyclename)s
                              WHERE proposalid=%(propid)s""" %
                           {'prefix'    : self.prefix,
                            'cyclename' : self.options[table_name],
                            'propid'    : self.literal(proposalid)})
        cursor.execute("""DELETE FROM %(prefix)sproposals
                          WHERE proposalid=%(propid)s""" %
                       {'prefix' : self.prefix,
                        'propid' : self.literal(proposalid)})
        # Delete images
        result = self.images_list(proposalid)
        for res in result:
            self.images_delete(proposalid, res['numb'])
        cursor.close()
        # Justification delete
        self.justification_delete_data(proposalid)

    def close(self):
        self.Database.close()

    def images_list(self, proposalid, numb=False):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        if (numb == False):
            numb_text = ""
        else:
            numb_text = " AND numb=%s" % self.literal(numb)

        cursor.execute("""SELECT * FROM %(prefix)simages
                          WHERE proposalid=%(propid)s%(numb)s
                          ORDER BY numb""" %
                       {'prefix' : self.prefix,
                        'propid' : self.literal(proposalid),
                        'numb'   : numb_text})
        result = cursor.fetchall()
        cursor.close()
        return result

    def images_get(self, proposalid, numb=False):
        result = self.images_list(proposalid, numb)
        # Pull out the raw data from files and merge into the result structure.
        # Using number offsets since we are adjusting the original structure instead of
        # returning a modified data.
        for i in xrange(0, len(result)):
            if os.path.isfile(self.path_images + str(result[i]['proposalid']) + '.' + str(result[i]['numb']) + '.gz') == True:
                im = gzip.open(self.path_images + str(result[i]['proposalid']) + '.' + str(result[i]['numb']) + '.gz', 'rb')
                result[i]['ps_data'] = im.read()
                im.close()
        return result

    def images_add(self, proposalid):
        result = self.images_list(proposalid)
        if (len(result) == 0):
            numb = 1
        else:
            numb = result[-1]['numb'] + 1
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        cursor.execute("""INSERT INTO %(prefix)simages
                          SET proposalid=%(propid)s, numb=%(numb)s""" %
                       {'prefix' : self.prefix,
                        'propid' : self.literal(proposalid),
                        'numb'   : self.literal(str(numb))})
        cursor.close()
        return numb

    def images_update(self, proposalid, filename, numb, bin_data):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        cursor.execute("""UPDATE %(prefix)simages SET file=%(filename)s
                          WHERE
                          proposalid=%(propid)s AND numb=%(numb)s""" %
                       {'prefix'   : self.prefix,
                        'filename' : self.literal(filename),
                        'propid'   : self.literal(proposalid),
                        'numb'     : self.literal(numb)})
        cursor.close()
        # File is now written after the tag is written to the database.
        im = gzip.open(self.path_images + str(proposalid) + '.' + str(numb) + '.gz', 'wb')
        im.write(bin_data)
        im.close()

    def images_delete(self, proposalid, numb):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        cursor.execute("""DELETE FROM %(prefix)simages
                          WHERE proposalid=%(propid)s AND numb=%(numb)s""" %
                       {'prefix' : self.prefix,
                        'propid' : self.literal(proposalid),
                        'numb'   : self.literal(numb)})
        cursor.close()
        # File is deleted now that the tag is not in database anymore.
        if os.path.isfile(self.path_images + str(proposalid) + '.' + str(numb) + '.gz') == True:
            os.unlink(self.path_images + str(proposalid) + '.' + str(numb) + '.gz')

    def pdf_add_update(self, proposalid, pdfdata):
        pdf_file = open(self.path_pdf + str(proposalid) + '.pdf', 'wb')
        pdf_file.write(pdfdata)
        pdf_file.close()

    def pdf_get_data(self, proposalid):
        pdf_file = open(self.path_pdf + str(proposalid) + '.pdf', 'rb')
        pdf = pdf_file.read()
        pdf_file.close()
        return pdf

    def justification_add_update(self, proposalid, pdfdata):
        just_file = open(self.path_justification + str(proposalid) + ".pdf", 'wb')
        just_file.write(pdfdata)
        just_file.close()

    def justification_get_data(self, proposalid):
        # Even though all justifications are now LaTeX files (.tex) they were originally
        # pdf files.
        if os.path.isfile(self.path_justification + str(proposalid) + ".pdf") == True:
            just_file = open(self.path_justification + str(proposalid) + ".pdf", 'r')
            just = just_file.read()
            just_file.close()
            return just
        else:
            return None

    def justification_delete_data(self, proposalid):
        if os.path.isfile(self.path_justification + str(proposalid) + ".pdf") == True:
            return os.unlink(self.path_justification + str(proposalid) + ".pdf")
        else:
            return None        

    def justification_type_set(self, proposalid, type):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        cursor.execute("""UPDATE %(prefix)sproposals
                          SET pdf_justification=%(type)s
                          WHERE proposalid=%(propid)s""" %
                       {'prefix' : self.prefix,
                        'type'   : self.literal(type),
                        'propid' : self.literal(proposalid)})
        cursor.close()

    def justification_type_latex(self, proposalid):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        cursor.execute("""SELECT pdf_justification FROM %(prefix)sproposals
                          WHERE proposalid=%(propid)s""" %
                       {'prefix' : self.prefix,
                        'propid' : self.literal(proposalid)})
        result = cursor.fetchone()
        cursor.close()

        if result['pdf_justification'] == 1 :
            return True
        else:
            return False

    def is_key_project(self, proposalid):
        cursor = self.Database.cursor(cursorclass=MySQLdb.cursors.DictCursor)
        cursor.execute("""SELECT * FROM %(prefix)sproposals,
                          %(prefix)scycles WHERE 
                          %(prefix)sproposals.cyclename=%(prefix)scycles.cyclename AND
                          %(prefix)sproposals.proposalid=%(propid)s LIMIT 1""" %
                       {'prefix' : self.prefix,
                        'propid' : self.literal(proposalid)})
        result = cursor.fetchone()
        proptable = result['proposal']

        cursor.execute("""SELECT * FROM %(prefix)s%(proptable)s WHERE
                          proposalid=%(propid)s LIMIT 1""" %
                       {'proptable' : proptable,
                        'propid' : self.literal(proposalid),
                        'prefix' : self.prefix})

        result = cursor.fetchone()
        cursor.close()
        if result.__contains__('key_project') == True:
            if result['key_project'] == 1 :
                return True
            else:
                return False
        else:
            return False
