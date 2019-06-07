#
# Python module containing utilities for parsing astroheaders.
#
# @author: Andy Beard
#
import xml.sax as sax
import xml.sax.saxutils
import os
import sys

def extractFrameFromVisbrickFilename( filename, prefix, suffix ):
    """Extract the frame time from a visbrick filename of the form:
    prefix + frame + [suffix] where the suffix is an optional 
    write suffix. For example: visBrickData_1235566.write.
    """
    if filename.startswith( prefix ):  

        frameTokenIdx = filename.find( prefix ) + len( prefix )
        frameToken = filename[frameTokenIdx:]

        if frameToken.endswith( suffix ): 
            suffixIdx = frameToken.rfind( suffix )
            frameToken = frameToken[0:suffixIdx]
            
        if frameToken.isdigit():
            return long( frameToken )
        else:
            return None

def createVisbrickMap( visbrickDir, visbrickPrefix, writeSuffix):
    """Given a directory and prefix/suffix for visbrick filenames, return 
    a map of visbrick filenames keyed by frame times.
    """
    filelist = os.listdir( visbrickDir )

    if len( filelist ) == 0:
        raise Exception, (visbrickDir + " contains no files." )

    # Tokenize and create map of filenames keyed on frame time 
    visbrickMap = {} # Empty dictionary of visbrick filenames keyed by frame
    for file in filelist:
        frame = extractFrameFromVisbrickFilename( file, 
                                                  visbrickPrefix, 
                                                  writeSuffix )
        if frame != None:
            visbrickMap[ frame ] = os.path.abspath( visbrickDir + "/" + file )

    return visbrickMap 

def getVisbrickFileForIntegration( integration, visbrickMap ):
    """Return the appropriate filename for the given integration.
    There is no guarantee that the integration is part of the visbrick file
    returned, only that it is the filename it should be in.
    """
    # Find highest frame in visbrickMap less than the integration number. 
    maxVbFrame = 0
    for frame in visbrickMap.keys():
        if frame > maxVbFrame and frame <= integration:
            maxVbFrame = frame 
    return visbrickMap[maxVbFrame]
        

class AstroHeaderParser(sax.saxutils.XMLGenerator):
    """SAX Parser and generator for astroheaders.
    Parses astroheader files and verifies that integration frames match
    the associated visbrick.  A transformed astroheader will be output
    and fixed if the fix flag is specified.  Toggling the fix
    flag allows one to output a standard non-fixed astroheader and diff it to
    one that was fixed.  Note that even with a 1-1 transform, the output 
    astroheader file generally won't textually match the original due to
    different formatting.  After parsing, a summary is output.
    """
    def __init__( self, outfile, fix=False,
                  visBrickDir="/opt/visbrick", 
                  visBrickPrefix="visBrickData_",
                  visBrickSuffix=".write" ): 
        """Create an AstroHeaderParser object.
        Parameters:
         visBrickDir Directory containing visbricks.
         visBrickPrefix Common prefix for visbrick files (e.g. visBrickData)
         visBrickSuffix Optional write suffix for visBricks (e.g. .write).
         outfile Specifies output file for transformed astorheader.
         fix If true, fix visbrick-integration mismatches.
        """
        sax.saxutils.XMLGenerator.__init__( self, outfile )
        self.fix = fix
        self.prefix = visBrickPrefix
        self.suffix = visBrickSuffix
        self.currentIntegration = 0
        self.mismatchedVisbricks = 0
        self.totalIntegrations = 0
        self.visBrickMap = createVisbrickMap( visBrickDir, 
                                              visBrickPrefix, 
                                              visBrickSuffix )

    def startElement(self,tag,attrs):
        if tag == "INTEGRATION":
            self.currentIntegration = long(attrs.getValueByQName("startframe"))
            self.totalIntegrations += 1
            if self.totalIntegrations % 100 == 0:
                print ".",
                sys.stdout.flush()
        elif tag == "KW":
            if attrs.getValueByQName( "name" ) == "visbrick":
                ahwVbFilename = attrs.getValueByQName( "value" ) 
                vbframe = extractFrameFromVisbrickFilename( ahwVbFilename,
                                                            self.prefix,
                                                            self.suffix )
                vbFilename = getVisbrickFileForIntegration(
                    self.currentIntegration, self.visBrickMap )
                # Check that visbrick name matches integration 
                if ahwVbFilename.strip() != vbFilename.strip():
                    self.mismatchedVisbricks += 1
                    if self.fix:
                        attrs._attrs["value"] = vbFilename
        
        sax.saxutils.XMLGenerator.startElement( self, tag, attrs )

    def endDocument(self):
        if self.totalIntegrations < 1:
            outstring = "No integrations processed!"
        elif self.totalIntegrations == 1:
            outstring = "Processed 1 integration "
            if self.mismatchedVisbricks:
                outstring += "which was matched to the wrong visbrick."
            else:
                outstring += "which was matched to the correct visbrick."
        else:
            outstring = "Processed %d integrations, "%self.totalIntegrations

            if self.mismatchedVisbricks == 1:
                outstring += "one of which was matched to the wrong visbrick." 
            elif self.mismatchedVisbricks > 1:
                outstring += "%d of which were" %self.mismatchedVisbricks,
                outsttring += " matched to the wrong visbricks."
            else:
                outstring += "all with correctly matched visbricks."

        print outstring

        sax.saxutils.XMLGenerator.endDocument(self)

def verifyAstroHeaderIntegrity( inFilename ):
    nowhere = open("/dev/null", 'w')
    ahParser = AstroHeaderParser( nowhere, False )
    sax.parse( inFilename, ahParser )

def fixAstroHeaderFile( inFilename, outFilename=None ):
    """Fix visbrick-integration mismatches in input astroheader and 
    place fixed astroheader in outFilename.  If outFilename is not
    specified, the output will be placed in a file with the same name
    as the input file appended with a .fixed suffix.
    """
    if outFilename is None:
        outFilename = inFilename + ".fixed"
    fixedFile = open( outFilename, 'w') 
    ahParser = AstroHeaderParser( fixedFile, True )
    sax.parse( inFilename, ahParser )

def oneToOneTransformAstroHeader( inFilename, outFilename=None ):
    """Perform a direct transform on input astroheader file.  This
    simply runs the input astroheader through the parser/generator and
    outputs a new astroheader.  No information is changed though the 
    formatting will be different thus allowing one to text diff
    with a 'fixed' astroheader to verify results.  If outFilename is
    not specified, the inFilename with ".notfixed" appended to it will
    be used.
    """
    if outFilename is None:
        outFilename = inFilename + ".notfixed"
    transformedFile = open( outFilename, 'w' )
    ahParser = AstroHeaderParser( transformedFile, False )
    sax.parse( inFilename, ahParser )
