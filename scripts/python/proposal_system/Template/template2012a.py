class template:
    def __init__(self):
        author = [{'name':'ProposalID',
                   'fieldname':'proposalid',
                   'fieldtype':'integer',
                   'section': False},
                  {'name' : 'Number',
                   'fieldname' : 'numb',
                   'fieldtype' : 'integer',
                   'section'   : 'author',
                   'shortname' : '#'},
                  {'name':'Name',
                   'fieldname':'name',
                   'fieldtype':'text',
                   'section':'author'},
                  {'name':'E-mail',
                   'fieldname':'email',
                   'fieldtype':'text',
                   'section':'author'},
                  {'name':'Phone',
                   'fieldname':'phone',
                   'fieldtype':'text',
                   'section':'author'},
                  {'name':'Institution',
                   'fieldname':'institution',
                   'fieldtype':'institution',
                   'section':'author'},
                  {'name':'Thesis',
                   'fieldname':'thesis',
                   'fieldtype':'bool',
                   'section':'author'},
                  {'name':'Graduate Student',
                   'fieldname':'grad',
                   'fieldtype':'bool',
                   'section':'author',
                   'shortname' : 'Grad'}]

        self.author_order = { 1 : 'numb',
                         2 : 'name',
                         3 : 'email',
                         4 : 'phone',
                         5 : 'institution',
                         6 : 'thesis',
                         7 : 'grad' }

        proposal = [{'name':'ProposalID',
                     'fieldname':'proposalid',
                     'fieldtype':'integer',
                     'section': False},
                    {'name':'User',
                     'fieldname':'user',
                     'fieldtype':'text',
                     'section': False},
                    {'name':'Title',
                     'fieldname':'title',
                     'fieldtype':'text',
                     'size':'250',
                     'section':'propinfo'},
                    {'name':'Scientific Category',
                     'fieldname':'scientific_category',
                     'fieldtype':['Cometary', 'Planetary', 'Solar', 'Stellar',
                                  'High-mass Star Formation',
                                  'Low-mass Star Formation', 
                                  'Chemistry / Interstellar Medium',
                                  'Other Galactic', 'Galaxies - Detection', 
                                  'Galaxies - Mapping',
                                  'Cosmology', 'Other Extragalactic'],
                     'section':'propinfo'},
                    {'name':'Date',
                     'fieldname':'date',
                     'fieldtype':'date',
                     'section':'propinfo',
                     'info' : 'YYYY-MM-DD',
                     'line' : 2},
                    {'name':'Key Project',
                     'fieldname':'key_project',
                     'fieldtype':'bool',
                     'section':'propinfo',
                     'info':'Select if your project meets the requirements for a Key Project. See <a href="http://cedarflat.mmarray.org/observing/proposals/KP_call2011b.html" target="_blank">here</a> for more information.',
                     'line' : 2},
                    {'shortname':'TOO/Time Critical',
                     'name':'Target of Opportunity/Time Critical',
                     'fieldname':'toe',
                     'fieldtype':'bool',
                     'section':'propinfo',
                     'line':2},
                    {'name':'Level of Help Required',
                     'fieldname':'help_required',
                     'fieldtype':['None', 'Consultation',
                                  'Request Collaborator'],
                     'section':'propinfo',
                     'line' : 2},
                    {'name':'1cm Project',
                     'fieldname':'1cm',
                     'fieldtype':'bool',
                     'section':'propinfo',
                     'line':2},
                    {'name':'3mm Project',
                     'fieldname':'3mm',
                     'fieldtype':'bool',
                     'section':'propinfo',
                     'line':2},
                    {'name':'1mm Project',
                     'fieldname':'1mm',
                     'fieldtype':'bool',
                     'section':'propinfo',
                     'line':2},
                    {'name':'Abstract',
                     'fieldname':'abstract',
                     'fieldtype':'longtext',
                     'section':'abstract'},
                    {'name':'Special Requirements',
                     'fieldname':'special_requirements',
                     'fieldtype':'longtext',
                     'section':'special_requirements',
                     'check':[]},
                    {'name':'Status of Prior CARMA Observations',
                     'fieldname':'prior_obs',
                     'fieldtype':'longtext',
                     'section':'prior_obs',
                     'check':[]},
                    {'name':'Scientific Justification',
                     'fieldname':'scientific_justification',
                     'fieldtype':'longtext',
                     'section':'scientific_justification'},
                    {'name':'Technical Justification',
                     'fieldname':'technical_justification',
                     'fieldtype':'longtext',
                     'section':'technical_justification'}]
         
        source = [{'name':'ProposalID',
                   'fieldname':'proposalid',
                   'fieldtype':'integer',
                   'section' :False},
                  {'name' : 'Number',
                   'shortname':'#',
                   'fieldname' : 'numb',
                   'fieldtype' : 'integer',
                   'section'   : 'source',
                   'line'      : 1},
                  {'name':'Source Name',
                   'shortname':'Source',
                   'fieldname':'name',
                   'fieldtype':'text',
                   'section':'source',
                   'line' : 1},
                  {'name':'Right Ascension',
                   'shortname':'RA',
                   'fieldname':'ra',
                   'fieldtype':'text',
                   'section':'source',
                   'line' : 1,
                   'info' : 'J2000',
                   'check' : ['NoNull', 'NoSpaces', 'raCheck']},
                  {'name':'Declination',
                   'shortname':'DEC',
                   'fieldname':'dec',
                   'fieldtype':'text',
                   'section':'source',
                   'line' : 1,
                   'info' : 'J2000',
                   'check' : ['NoNull', 'NoSpaces', 'decCheck']},
                  {'name':'Frequency of Observation',
                   'shortname':'Freq',
                   'fieldname':'corr_frequency',
                   'fieldtype':'text',
                   'section':'source',
                   'line' : 1,
                   'info' : 'GHz',
                   'check' : ['NoNull', 'Numeric', 'Only3mmInC23', 'PolFreq']},
#                  {'name':'A Array Time',
#                   'shortname':'A<sup>1</sup>',
#                   'fieldname':'hrs_a',
#                   'fieldtype':'text',
#                   'section':'source',
#                   'line' : 1,
#                   'info' : 'Hours',
#                   'check' : ['NoNull', 'Numeric', 'CARMAFreq']},
#                  {'name':'B Array Time',
#                   'shortname':'B<sup>1</sup>',
#                   'fieldname':'hrs_b',
#                   'fieldtype':'text',
#                   'section':'source',
#                   'line' : 1,
#                   'info' : 'Hours',
#                   'check' : ['NoNull', 'Numeric', 'CARMAFreq']},
                  {'name':'C Array Time',
                   'shortname':'C',
                   'fieldname':'hrs_c',
                   'fieldtype':'text',
                   'section':'source',
                   'line' : 1,
                   'info' : 'Hours',
                   'check' : ['NoNull', 'Numeric', 'CARMAFreq']},
                  {'name':'D Array Time',
                   'shortname':'D',
                   'fieldname':'hrs_d',
                   'fieldtype':'text',
                   'section':'source',
                   'line' : 1,
                   'info' : 'Hours',
                   'check' : ['NoNull', 'Numeric', 'CARMAFreq']},
                  {'name':'E Array Time',
                   'shortname':'E',
                   'fieldname':'hrs_e',
                   'fieldtype':'text',
                   'section':'source',
                   'line' : 1,
                   'info' : 'Hours',
                   'check' : ['NoNull', 'Numeric', 'CARMAFreq']},
                  {'name':'SH Array Time',
                   'shortname':'SH',
                   'fieldname':'hrs_sh',
                   'fieldtype':'text',
                   'section':'source',
                   'line' : 1,
                   'info' : 'Hours',
                   'check' : ['NoNull', 'Numeric', 'NoC23', 'NoDualPol', 'NoFullPol', 'SZAFreq']},
                  {'name':'SL Array Time',
                   'shortname':'SL',
                   'fieldname':'hrs_sl',
                   'fieldtype':'text',
                   'section':'source',
                   'line' : 1,
                   'info' : 'Hours',
                   'check' : ['NoNull', 'Numeric', 'NoC23', 'NoDualPol', 'NoFullPol', 'SZAFreq']},
                  {'name':'Observation Type',
                   'shortname':'Type',
                   'fieldname':'observation_type',
                   'fieldtype':'observation_type',
                   'section':'source',
                   'line' : 1,
                   'check' : ['NoNull', 'OBType']},
                  {'name':'Number of Mosaic Fields',
                   'shortname':'# Fields',
                   'fieldname':'numb_fields',
                   'fieldtype':'text',
                   'section':'source',
                   'line' : 1,
                   'check': ['NoNull', 'Integer',
                             'NoZero']},
                  #Begin Correlator Stuff
                  {'name':'Species or Transition Name',
                   'shortname':'Species',
                   'fieldname':'species',
                   'fieldtype':'text',
                   'section':'source',
                   'line' : 1},
                  #Begin Optional A Array Info
                  {'name':'Can Self-Calibrate',
                   'shortname':'Self Cal',
                   'fieldname':'self_cal',
                   'fieldtype':'bool',
                   'section':'source',
                   'line' : 1,
                   'nosummary' : True},
                  {'name':'Imaging/SNR',
                   'shortname':'Imag/SNR',
                   'fieldname':'imaging',
                   'fieldtype':['Imaging', 'SNR'],
                   'section':'source',
                   'line':1 },                   
                  {'name':'Flexible Hour Angle?',
                   'shortname':'Flex.HA',
                   'fieldname':'flexha',
                   'fieldtype':'bool',
                   'section':'source',
                   'line' : 1,
                   'info' : 'Check if yes'}]

        self.source_order = { 1 : 'numb',
                              2 : 'name',
                              3 : 'ra',
                              4 : 'dec',
                              5 : 'corr_frequency',
#                              6 : 'hrs_a',
#                              7 : 'hrs_b',
                              6 : 'hrs_c',
                              7 : 'hrs_d',
                              8 : 'hrs_e',
                              9 : 'hrs_sh',
                              10: 'hrs_sl',
                              11: 'observation_type',
                              12: 'numb_fields',
                              13: 'species',
                              14: 'imaging',
                              15: 'flexha' }
                              
       
        self.sections = [{'section' : 'propinfo',
                          'name'    : 'General Proposal Information',
                          'type'    : 'general',
                          'table'   : 'proposal'},
                         {'section' : 'author',
                          'name'    : 'Authors List',
                          'type'    : 'repeat',
                          'table'   : 'author'},
                         {'section' : 'abstract',
                          'name'    : 'Abstract',
                          'type'    : 'general',
                          'table'   : 'proposal'},
                         {'section' : 'source',
                          'name'    : 'Source Information',
                          'type'    : 'repeat',
                          'table'   : 'source'},
                         {'section' : 'special_requirements',
                          'name'    : 'Special Requirements',
                          'type'    : 'general',
                          'table'   : 'proposal'},
                         {'section' : 'prior_obs',
                          'name'    : 'Status of Prior CARMA Observations',
                          'type'    : 'general',
                          'table'   : 'proposal'},
                         {'section' : 'scientific_justification',
                          'name'    : 'Scientific Justification',
                          'type'    : 'general',
                          'table'   : 'proposal'},
                         {'section' : 'technical_justification',
                          'name'    : 'Technical Justification',
                          'type'    : 'general',
                          'table'   : 'proposal'},
                         {'section' : 'image',
                          'name'    : 'Image Attachments',
                          'type'    : 'image',
                          'table'   : None}]

        self.tables = {'proposal' : { 'value' : proposal,
                                      'type'  : 'single'},
                       'source'   : { 'value' : source,
                                      'type'  : 'repeat'},
                       'author'   : { 'value' : author,
                                      'type'  : 'repeat'}}
