
#include "carma/dbms/LogProcessor.h"
#include "carma/dbms/SyslogMMAPFile.h"
#include "carma/util/Program.h"
#include "carma/util/Trace.h"
#include "carma/util/ErrorException.h"
#include "carma/util/StringUtils.h"

#include <iosfwd>
#include <iostream>
#include <iomanip>
#include <sys/mman.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <math.h>

using namespace ::std;
using namespace ::log4cpp;
using namespace carma;
using namespace carma::dbms;
using namespace carma::util;

const char *SyslogMMAPFile::MYID = "$Id: SyslogMMAPFile.cc,v 1.6 2009/03/20 22:52:32 abeard Exp $";
SemaphoreOperator *SyslogMMAPFile::semOp_ = (SemaphoreOperator *)0;

::std::string *SyslogMMAPFile::SEM_NAME = new string( "lastPosWait" );
unsigned short SyslogMMAPFile::num_ = 5;

SyslogMMAPFile::SyslogMMAPFile( const string& fileName, bool writer ) 
  :
mmapFileName_( fileName ),
  writer_( writer ),
  mmapFD_( -1 ),
  mmapSize_( (MMAP_SIZE + (MMAP_SIZE % sysconf(_SC_PAGESIZE))) ),
  log_( Program::getLogger() )
{
  CPTRACE( Trace::TRACE6, "      c'tor" );

  int mmapFlags = MAP_SHARED;
  int mmapProt = 0;
  int openFlags = 0;
  int openMode = 0;

  // TODO, register these sem nums so that they are unique to program classes
  semOp_ = new SemaphoreOperator( 200 );

  if ( writer_ )
  {
    CPTRACE( Trace::TRACE6, "       c'tor called as writer" );
    mmapProt = PROT_WRITE|PROT_READ;
    openFlags = O_CREAT | O_RDWR;
    openMode = S_IRUSR | S_IWUSR;
  }
  else
  {
    CPTRACE( Trace::TRACE6, "       c'tor called as reader" );
    openFlags = O_RDONLY;
    mmapProt = PROT_READ;
    openMode = S_IRUSR;
  }

  CPTRACE( Trace::TRACE6, "       c'tor mapping file" << mmapFileName_ );
  if ( ( mmapFD_ = open( mmapFileName_.c_str(), openFlags, openMode ) ) < 0 )
  {
    ostringstream os;
    os << "Unable to open MMAP File: '" << mmapFileName_ << "' " <<
      strerror(errno);
    CPTRACE( Trace::TRACE6, "       c'tor filed to open file:"
	<< os.str() );
    throw CARMA_ERROR( os.str() );
  }

  if ( writer_ )
  {
    // Need to make the file big enough
    if ( lseek( mmapFD_, mmapSize_, SEEK_SET ) != mmapSize_ )
    {
      ostringstream os;
      os << "Unable to lseek to end of MMAP File: '" << mmapFileName_ << "' " <<
	strerror(errno);
      CPTRACE( Trace::TRACE6, "       c'tor "
	  << os.str() );
      throw CARMA_ERROR( os.str() );
    }

    if ( write( mmapFD_, "\0", 1 ) != 1 )
    {
      ostringstream os;
      os << "Unable to lseek to end of MMAP File: '" << mmapFileName_ << "' " <<
	strerror(errno);
      CPTRACE( Trace::TRACE6, "       c'tor "
	  << os.str() );
      throw CARMA_ERROR( os.str() );
    }
  }

  if ( ( mmapPtr_ = (unsigned char *)mmap( 0, mmapSize_,
	  mmapProt, mmapFlags, mmapFD_, 0 ) ) == MAP_FAILED )
  {
    ostringstream os;
    os << "Unable to mmap MMAP File: '" << mmapFileName_ << "' " <<
      strerror(errno);
    CPTRACE( Trace::TRACE6, "       c'tor "
	<< os.str() );
    throw CARMA_ERROR( os.str() );
  }
  CPTRACE( Trace::TRACE6, "       c'tor mapped with size: "
      << mmapSize_ << " addr: 0x" << (hex) << mmapPtr_ );

  // Map pointers into MMAP locations
  hashPtr_ = (unsigned char *)mmapPtr_;
  CPTRACE( Trace::TRACE6, "      c'tor hptr:0x"<<(hex)<<hashPtr_ );
  IDPtr_ = (unsigned char *)hashPtr_+VERSIONID_SIZE;
  CPTRACE( Trace::TRACE6, "      c'tor idptr:0x"<<(hex)<<IDPtr_ );
  header_size_ = (size_t *)(IDPtr_ + VERSIONID_SIZE);
  CPTRACE( Trace::TRACE6, "      c'tor vis:"<<(hex)<<header_size_ );
  versionID_size_ = (size_t *)(header_size_+1);
  CPTRACE( Trace::TRACE6, "      c'tor vids:"<<(hex)<<versionID_size_ );
  syslogMedian_size_ = (size_t *)(versionID_size_+1);
  CPTRACE( Trace::TRACE6, "      c'tor sms:"<<(hex)<<syslogMedian_size_ );
  currentState_id_ = (long long *)(syslogMedian_size_+1);
  CPTRACE( Trace::TRACE6, "      c'tor csi:"<<(hex)<<currentState_id_ );
  currentAtomicPos_ = (long *)(currentState_id_+1);
  CPTRACE( Trace::TRACE6, "      c'tor cap:"<<(hex)<<currentAtomicPos_ );
  lastPos_ = (long *)(currentAtomicPos_+1);
  lastBytePos_ = (size_t *)(lastPos_+1);
  CPTRACE( Trace::TRACE6, "      c'tor lp:"<<(hex)<<lastPos_ );
  headerPtrStart_ = (size_t *)(lastPos_+1);
  CPTRACE( Trace::TRACE6, "      c'tor pointers mapped" );

  headerPtrEnd_ = (size_t *)(headerPtrStart_+SyslogMMAPFile::MAX_MESSAGES);
  CPTRACE( Trace::TRACE6, "      c'tor hps:"<<(hex)<<headerPtrStart_<<" hpe:" << headerPtrEnd_ );
  // This should provide for a buffer that covers somewhere between 8-80 hours
  // depending on how many messages are coming and how large they are.

  ringBufferStart_ = (unsigned char *)(headerPtrEnd_+1);
  ringBufferEnd_ = mmapPtr_ + mmapSize_ - 1;

  CPTRACE( Trace::TRACE6, "      c'tor determining versionInfo size" );
  CPTRACE( Trace::TRACE6, "      c'tor rbs:0x"<<(hex)<<ringBufferStart_<<" rbe:0x"
      << ringBufferEnd_ << " mmp:0x"<<mmapPtr_);
  if ( writer_ )
    *header_size_ = (ringBufferStart_ - mmapPtr_);

  // Determine if a pre-existing stateid in the mmap file is consistent
  // with the computed layout of the file.  If not, then zero out the header
  // pointer fields and start fresh.  If it is consistent, try to pick up
  // where things were left off.
  CPTRACE( Trace::TRACE6, "      c'tor computing hash" );
  string myHash = computeVersionHash();
  CPTRACE( Trace::TRACE6, "      c'tor computing stateid" );
  myStateID_ = computeStateID( myHash.c_str() );
  if ( writer_ && isConsistent() == false )
  {
    CPTRACE( Trace::TRACE6, "       c'tor found inconsistent mmap file, initializing" );
    CPTRACE( Trace::TRACE6, "       writer setting up header values" );
    CPTRACE( Trace::TRACE6, "        zeroing version header size:" << "TODO0"
	<< " mmapPtr_: 0x" << (hex) << mmapPtr_ );
    //    memset( mmapPtr_, 0, VERSIONINFO_SIZE ); // zero out the version info

    *currentState_id_ = computeStateID( myHash.c_str() );
    memset( (void *)headerPtrStart_, 0, headerPtrEnd_ - headerPtrStart_ - 1);
  }

  // Make some decisions about how this file will be laid out
  // and then post them to the file
  if ( writer_ )
  {
    CPTRACE( Trace::TRACE6, "        copying hash string:"  << myHash );

    // Check if hashes match up, if so, do not initialize values
    // and proceed.  *cross fingers*
    if ( myHash != string( (const char *)hashPtr_ )  )
    {
      CPTRACE( Trace::TRACE2, "  Intializing mmap file");
      CPTRACE( Trace::TRACE6, "        copying hash string:"  << myHash );
      log_ << Priority::INFO << "opening syslog mmap file and initializing";

      const char *hashChars = myHash.c_str();
      size_t hashSize = strnlen(hashChars, VERSIONID_SIZE-1);

      memcpy( (void*)hashPtr_, hashChars, hashSize );

      string myID( MYID );
      CPTRACE( Trace::TRACE6, "        creating const char copy of MYID:" << myID );
      string myIDsubstr( myID.substr(0,VERSIONID_SIZE-1) );
      CPTRACE( Trace::TRACE6, "        copying const char copy of MYID" );
      memcpy( (void *)IDPtr_, (void *)myIDsubstr.c_str(),
	  strnlen(myIDsubstr.c_str(),VERSIONID_SIZE-1)-1 );

      CPTRACE( Trace::TRACE6, "        setting version header variables" );
      *versionID_size_ = VERSIONID_SIZE;
      *syslogMedian_size_ = SYSLOGMEDIAN_SIZE;
      *currentAtomicPos_ = 0;
      *lastPos_ = 0;
      *lastBytePos_ = 0;
    }
    else
    {
      CPTRACE( Trace::TRACE2, "  Re-opening mmap file");
      CPTRACE( Trace::TRACE6, "        header size:" << getHeaderSize() );
      log_ << Priority::INFO << "re-opening syslog mmap file and keeping old header"
	<< " info, and messages";
    }

    CPTRACE( Trace::TRACE6, "        header size:" << getHeaderSize() );
    CPTRACE( Trace::TRACE6, "        id size:" << getVersionIDSize() );
    CPTRACE( Trace::TRACE6, "        median size:" << getSyslogMedianSize() );
  }


  CPTRACE( Trace::TRACE6, "      c'tor csid: " << getMapStateID() );
  CPTRACE( Trace::TRACE6, "      c'tor exiting successfully" );
}

SyslogMMAPFile::~SyslogMMAPFile()
{
  CPTRACE( Trace::TRACE6, "      SyslogMMAPFile d'tor" );

  if ( mmapPtr_ != NULL )
  {
    if ( munmap( mmapPtr_, mmapSize_ ) < -1 )
    {
      cerr << "Unable to munmap( 0x" << (hex) << mmapPtr_ << ", "
	<< (dec) << mmapSize_ << " ): " << strerror(errno);
    }
    CPTRACE( Trace::TRACE6, "       SyslogMMAPFile d'tor munmap" );
  }

  if ( mmapFD_ != -1 )
  {
    if ( close( mmapFD_ ) < -1 )
    {
      cerr << "Unable to close( " << mmapFD_ << " ): "
	<< strerror(errno);
    }
    CPTRACE( Trace::TRACE6, "       SyslogMMAPFile d'tor close" );
  }

  if ( semOp_ != (SemaphoreOperator *)0 )
  {
    // SEM_UNDO removes any locks for us when the program exits
    delete semOp_;
  }

  CPTRACE( Trace::TRACE6, "     SyslogMMAPFile d'tor exiting successfully" );
}

string SyslogMMAPFile::computeVersionHash()
{
  ostringstream oss;
  oss << string(MYID).substr(0,VERSIONID_SIZE-1)
    << " + " << mmapSize_ << " + " << *header_size_;
  return string(StringUtils::computeMessageDigest( oss.str(), SHA1 ));
}

// Creates a very good unique id based on the current hash
long long SyslogMMAPFile::computeStateID( const char *hash )
{
  size_t len = strnlen( hash, VERSIONID_SIZE-1 );
  long long state = 0L;

  for ( size_t i = 0; i < len; i++ )
    state += (((long long)hash[i]) << i );

  return state;
}

long long SyslogMMAPFile::getMapStateID()
{
  return ( *currentState_id_ );
}

string SyslogMMAPFile::getVersionHash()
{

  return ( string( (const char *)mmapPtr_ ) );
}

string SyslogMMAPFile::getMyID()
{
  if ( writer_ )
  {
    return string(MYID);
  }
  else
  {
    // This string starts at VERSIONID_SIZE+1 position in the array
    // and is \0 terminated
    return string((const char *)IDPtr_);
  }
}

size_t SyslogMMAPFile::getHeaderSize()
{
  return( *header_size_ );
}

size_t SyslogMMAPFile::getMapSize()
{
  return ( *header_size_ + (ringBufferEnd_ - ringBufferStart_) );
}

size_t SyslogMMAPFile::getVersionIDSize()
{
  if ( writer_ )
    return( VERSIONID_SIZE );
  else
    return( (*versionID_size_) );
}

size_t SyslogMMAPFile::getSyslogMedianSize()
{
  if ( writer_ )
    return( SYSLOGMEDIAN_SIZE );
  else
    return( *syslogMedian_size_ );
}

string SyslogMMAPFile::toString()
{
  ostringstream os;

  os << "Id:" << getMyID()
    << ",idsize:" << getVersionIDSize()
    << ",headersize:" << getHeaderSize()
    << ",median:" << getSyslogMedianSize()
    << ",hash:" << getVersionHash()
    << ",stateID:" << getMapStateID();

  return os.str();
}

string SyslogMMAPFile::toVerboseString()
{
  ostringstream os;

  os
    << "   ID:                     " << getMyID() << endl
    << "   Version ID Size:        " << getVersionIDSize() << endl
    << "   Header Length (bytes):  " << getHeaderSize() << endl
					<< "   Total map size (bytes): " << getMapSize() << endl
									    << "   Median message Size:    " << getSyslogMedianSize() << "(not dynamic)" << endl
									      << "   Hash:                   " << getVersionHash() << endl
									      << "   State ID on Disk:       " << getMapStateID() << endl
									      << "   Header consistent?      " << (boolalpha) << isConsistent();

  return os.str();
}

void SyslogMMAPFile::atomicUpdate( SyslogMessage *msg )
{
  size_t needed = msg->length();
  long newPos, newLastPos;

  newPos = (*currentAtomicPos_) + 1;
  newLastPos = *lastPos_;

  // If our newPos is greater than max messages
  // wrap around
  if ( newPos > SyslogMMAPFile::MAX_MESSAGES )
    newPos = 0;

  // If our newPos is the same as our last Pos
  // bump lastPos up.  lastPos in this context
  // is the last known position insert by this
  // program of known good quantity.
  if ( newPos == newLastPos )
  {
    newLastPos++;

    // wrap if necessary
    if ( newLastPos > SyslogMMAPFile::MAX_MESSAGES )
      newLastPos = 0;
  }

  // Determine if the required length of message space
  // is available in the ring buffer.  If not, wrap
  // around.  If so, keep going
  if ( (ringBufferStart_
	+ *lastBytePos_
	+ needed) > ringBufferEnd_ )
  {
    headerPtrStart_[newPos] = 0;
    *lastBytePos_ = 0;
  }
  else
  {
    headerPtrStart_[newPos] = *lastBytePos_;
    *lastBytePos_ = headerPtrStart_[newPos] + needed;
  }

  // Make sure that the new message does not overlap its way into
  // the back end of the old messages by upping the message lastPos
  // until we don't meet an overlap condition
  while ( (headerPtrStart_[newPos] < headerPtrStart_[newLastPos]) &&
      (*lastBytePos_ > headerPtrStart_[newLastPos] ) )
  {
    newLastPos++;

    if ( newLastPos > SyslogMMAPFile::MAX_MESSAGES )
      newLastPos = 0;
  }

  // Now move the last message position forward.  
  *lastPos_ = newLastPos;

  // Lock around the actual data update, because we might now be
  // modifying memory associated with one or more of the oldest
  // messages in the ring buffer.  Above, the pointers for where
  // the last messages are have already been updated.
  // If another process is reading off this bit of the mmap, this
  // Lock will force this process to wait until they are finished
  // then it can proceed to stomp on the memory.
  semOp_->Lock();

  // Okay, now copy data into place
  double mjd = msg->getDate();
  char *buf;
  size_t len;
  unsigned char *ptr;
  ptr = ringBufferStart_ + headerPtrStart_[newPos];
  len = sizeof(double);
  memcpy( ptr, &mjd, len );

  ptr = ptr+len;
  buf = msg->getLog();
  len = strnlen(buf, SyslogMessage::S_STD_LEN-1)+1;
  memcpy( ptr, buf, len );
  buf[len] = '\0';

  ptr = ptr+len;
  buf = msg->getHost();
  len = strnlen(buf, SyslogMessage::S_STD_LEN-1)+1;
  memcpy( ptr, buf, len );
  buf[len] = '\0';

  ptr = ptr+len;
  buf = msg->getProgram();
  len = strnlen(buf, SyslogMessage::S_STD_LEN-1)+1;
  memcpy( ptr, buf, len );
  buf[len] = '\0';

  ptr = ptr+len;
  buf = msg->getCharPrio();
  len = strnlen(buf, SyslogMessage::S_SHORT_LEN-1)+1;
  memcpy( ptr, buf, len );
  buf[len] = '\0';

  ptr = ptr+len;
  buf = msg->getFullyQualifiedProgramName();
  len = strnlen(buf, SyslogMessage::S_STD_LEN-1)+1;
  memcpy( ptr, buf, len );
  buf[len] = '\0';

  ptr = ptr+len;
  buf = msg->getThreadInfo();
  len = strnlen(buf, SyslogMessage::S_STD_LEN-1)+1;
  memcpy( ptr, buf, len );
  buf[len] = '\0';

  ptr = ptr+len;
  buf = msg->getMessage();
  len = strnlen(buf, SyslogMessage::S_LONG_LEN-1)+1;
  memcpy( ptr, buf, len );
  buf[len] = '\0';

  semOp_->Unlock();

  // Now that all data and pointers have been updated,
  // we can update the current position
  *currentAtomicPos_ = newPos;
}

// No need to lock around current Pos because it is
// atomically written before updating the current pos.
long SyslogMMAPFile::getCurrentPos()
{
  return *currentAtomicPos_;
}

// Make sure to use the lock wrapped around this
// unless you're into gambling.
long SyslogMMAPFile::getLastPos()
{
  return *lastPos_;
}


long SyslogMMAPFile::getPosLoc( long pos )
{
  return headerPtrStart_[pos];
}

long SyslogMMAPFile::getNearestAgedPos( double mjd )
{
  CPTRACE( Trace::TRACE1, "  getNearestAgedPos: " << std::setprecision(10) << mjd );
  semOp_->Lock();

  int start = (int)getCurrentPos();
  int end = (int)getLastPos();
  int adj = 0;
  int mid = 0;
  int jmp = 0;
  float epsilon = .00027; // One minute in MJD

  if ( end > start )
    adj = MAX_MESSAGES;


  mid = (int)((( (start + adj) - end ) / 2) + end);
  jmp = (int)(mid/2);

  while ( true )
  {
    // if we're within one minute, lets break
    double foreShadowMJD = peekMJD( abs(adj-mid) );

    CPTRACE( Trace::TRACE1, "    converging residual: " << std::setprecision(10) << fabs(foreShadowMJD-mjd) << " epsilon:" << epsilon << " mid:" << mid << " jmp:" << jmp << " adj:" << adj );

    if ( fabs( foreShadowMJD - mjd ) <= epsilon )
      break;


    if ( foreShadowMJD > mjd )
      mid = mid - jmp;
    else
      mid = mid + jmp;

    jmp = (int)(jmp/2);
    if ( jmp < 2 )  // Getting closer than this is rather pointless
      break;
  }

  CPTRACE( Trace::TRACE1, "    returning nearest: " << abs(adj-mid) );
  semOp_->Unlock();

  return ( abs(mid - adj) );
}

double SyslogMMAPFile::peekMJD( long pos )
{
  return *( (double *)(ringBufferStart_ + getPosLoc(pos)));
}

SyslogMessage *SyslogMMAPFile::getCurrentMessage()
{
  return LogProcessor::fromMMAP( *this, (ringBufferStart_ + getPosLoc(getCurrentPos())) );
}

// Need to lock accesses to anything to do with
// the lastPos because it might be in the middle
// of being overwritten with the next atomic
// write to the "head" of the ring buffer
SyslogMessage *SyslogMMAPFile::getLastMessage()
{
  SyslogMessage *msg;

  semOp_->Lock();
  msg = LogProcessor::fromMMAP( *this, (ringBufferStart_ + getPosLoc(getLastPos())) );
  semOp_->Unlock();

  return msg;
}


// Need to lock accesses because we might
// be near an area that is being overwritten
// by the atomic "head" of the ring buffer
// TODO, logic to ensure that we're getting a message
// from an area sandwiched between lastPos and currentAtomicPos
SyslogMessage *SyslogMMAPFile::getMessageAt( long pos )
{
  SyslogMessage *msg;

  semOp_->Lock();
  msg = LogProcessor::fromMMAP( *this, (ringBufferStart_ + getPosLoc(pos)) );
  semOp_->Unlock();

  return msg;
}


// Need to lock accesses because we might
// be near an area that is being overwritten
// by the atomic "head" of the ring buffer
// TODO, logic to ensure that we're getting a message
// from an area sandwiched between lastPos and currentAtomicPos
SyslogMessage *SyslogMMAPFile::getMessageAtAndUpdateNext( long &pos )
{
  SyslogMessage *msg;
  long newpos;

  if ( pos >= MAX_MESSAGES || pos < 0 )
  {
    pos = 0;
  }

  if ( pos+1 >= MAX_MESSAGES )
  {
    newpos = 0;
  }
  else
  {
    newpos = pos+1;
  }

  semOp_->Lock();
  msg = LogProcessor::fromMMAP( *this, (ringBufferStart_ + getPosLoc(pos)) );
  semOp_->Unlock();

  pos = newpos;

  return msg;
}

long SyslogMMAPFile::getNumMessages()
{
  long len = 0;
  long cur = getCurrentPos();
  long last = getLastPos();

  // Not going to lock positions, because it's this count
  // could potentially change immediately after the lock
  // anyway...
  if ( cur < last )
    len = cur + ( SyslogMMAPFile::MAX_MESSAGES - last );
  else
    len = cur - last;

  return len;
}

