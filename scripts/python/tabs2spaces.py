# Replaces tabs with spaces

import carmaHelpers as helper


def tabs2spaces(script, spaces=8) :
    """Finds a script in search path with script name and extension
    of .py or .obs and replaces tabs with 8 spaces. New file has same
    name as old with .notabs appended
    Parameters:
      script: script name without the path or extension (.py or .obs)
      spaces: number of spaces replacing a tab (default is 8)"""
    
    fname = helper.getScriptFullPathname(script)
    f = open(fname)
    ofname = fname + ".notabs"
    outf = open(ofname, 'w')    
    lines = f.readlines()
    lcnt = 0
    badlcnt = 0
    for l in lines:
        lcnt += 1
        if l.find('\t') != -1: badlcnt += 1
        outf.write(l.expandtabs(spaces))
    f.close()
    outf.close()
    print "Output written to %s; %i bad lines fixed" %(ofname, badlcnt)
    
