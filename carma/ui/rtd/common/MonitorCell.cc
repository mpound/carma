/*
 * Implementation for the MonitorCell realtime display class.
 *
 * @author Steve Scott
 *
 * $CarmaCopyright$
 *
 */


#include "carma/ui/rtd/common/MonitorCell.h"

#include "carma/monitor/MonitorPoint.h"
#include "carma/monitor/CanbusCommon.h"
#include "carma/util/ErrorException.h"
//#include "carma/util/programLogging.h"
#include "carma/util/StringUtils.h"
#include "carma/util/programLogging.h"

#include <iostream>

using namespace ::std;
using namespace carma::monitor;
using namespace carma::ui;
using namespace carma::ui::rtd;
using namespace carma::util;


namespace {


class MonitorCellStateMonitorPointEnum : public MonitorCell {
    friend class MonitorCell;
    
    protected:
        MonitorCellStateMonitorPointEnum(
            const int             cellWidth,
            const bool            setMpWidth,
            StateMonitorPointEnum & stateMonitorPointEnum,
            const int             sampleNo );
};


Format
makeFormat( const int cellWidth )
{
    Format result( cellWidth, 0, cellWidth );
    
    if ( result.indent < 0 )
        result.indent = 0;
        
    if ( result.len > result.width )
        result.len = result.width;
        
    return result;
}


int
calcTrueSampleNo( const MonitorPoint & mp,
                  const int            requestedSampleNo )
{
    // Strings really have only one sample, even though they internally can
    // use many to store longer strings
    if ( requestedSampleNo > 1 ) {
        if ( dynamic_cast< const MonitorPointString * >( &mp ) != 0 )
            return 666; // If this is out of bounds then ave will be used
    }

    if ( false &&
         (requestedSampleNo == 0) &&
         (mp.getNumSamples() > 1) &&
         (dynamic_cast< const MonitorPointString * >( &mp ) == 0) )
        return 1;
    
    return requestedSampleNo;
}


}  // namespace < anonymous >

MonitorCell::MonitorCell(const int     cellWidth,
                         const bool    setMpWidth,
                         MonitorPoint& mp,
                         MonitorPoint& mpc,
                         const int     sampleNo) :
Cell(makeFormat(cellWidth)),
mp_(mp),
mpc_(mpc),
sampleNo_( calcTrueSampleNo( mp, sampleNo ) ),
errorColor_( RED_CELL_COLOR ),
warnColor_( YELLOW_CELL_COLOR ),
defaultGoodColor_( WHITE_CELL_COLOR ),
goodColorTextOverrides_( ),
truncateFlag_( true ),
antenna_(0)
{
    initialize(cellWidth, setMpWidth);
}

MonitorCell::MonitorCell(const int     cellWidth,
                         const bool    setMpWidth,
                         MonitorPoint& mp,
                         const int     sampleNo ) :
Cell( makeFormat( cellWidth ) ),
mp_( mp ),
mpc_(mp),
sampleNo_( calcTrueSampleNo( mp, sampleNo ) ),
errorColor_( RED_CELL_COLOR ),
warnColor_( YELLOW_CELL_COLOR ),
defaultGoodColor_( WHITE_CELL_COLOR ),
goodColorTextOverrides_( ),
truncateFlag_( true ),
antenna_(0)
{
    initialize(cellWidth, setMpWidth);
}

void MonitorCell::initialize(const int cellWidth, const bool setMpWidth) 
{
    if ( setMpWidth )
        mp_.setWidth( cellWidth );

    {
        if ( dynamic_cast< const MonitorPointNumeric * >( &mp_ ) != 0 ) {
            if ( dynamic_cast< const MonitorPointAbstime * >( &mp_ ) == 0 )
                setPlottable( true );
            else
                setPlottable( false );
        } else
            setPlottable( false );
    }
    
    setValidity( false );

    if ( (format_.indent < 0) || (format_.len > format_.width) )
        throw CARMA_ERROR( "Bad format" );

    len = format_.len;
    indent = format_.indent;

    // Force region to include the center of the cell
    if (indent > format_.width/2) {
        len += indent - format_.width/2;
        indent = format_.width/2;
    }

    if (indent + len < (format_.width+1)/2)
        len = indent - (format_.width+1)/2;

    setCellName(mp_.getCanonicalName());
    setToolTipText(mp_.getDescription());
}


MonitorCell::~MonitorCell( )
try {
} catch ( ... ) {
    // Just stifle any exception
    
    return;
}

std::string MonitorCell::getName()
{
    return mp_.getName();
}

void
MonitorCell::setNotruncate( ) {
    truncateFlag_ = false;
}


void
MonitorCell::update( ) {
    bool valid = false;
    bool isNoHardware = false;

    if ( sampleNo_ > mp_.getNumSamples( ) ) {
        valid = false;
        isNoHardware = false;
    } else {
        if ( sampleNo_ == 0 ) {
            valid = mp_.isAveValid( );
            isNoHardware =
                (mp_.getAveValidity( ) == MonitorPoint::INVALID_NO_HW);
        } else {
            const int index = sampleNo_ - 1;
            
            valid = mp_.isValid( index );
            isNoHardware =
                (mp_.getValidity( index ) == MonitorPoint::INVALID_NO_HW);
        }
    }

    if ( isNoHardware )
        valid = true;
        
    setValidity( valid );
    setNohw( isNoHardware );

    if ( isReplaceText( ) == false ) {
        const string text = computeText( ).substr( 0, format_.len );
        
        fmtOss_.str("");
        
        const Layout layout = getLayout( );

        if ( layout == EOL_CENTERED_LAYOUT ) {
            fmtOss_ << centerStringFmt( text );
        } else if ( layout == EOL_LEFT_JUSTIFIED_LAYOUT ) {
            fmtOss_ << text << setw( format_.len - text.size() ) << "";
        } else {
            fmtOss_ << setw( format_.len ) << text;
        }
    }

    updateColor();
}


void
MonitorCell::updateColor( ) {
    if ( isReplaceText( ) )
        Cell::updateColor( );
    else
        setColor( computeColor( ) );
}


int
MonitorCell::getSampleNo( ) const {
    return sampleNo_;
}

int 
MonitorCell::setMpWidthToFormatLength() 
{
    if ( format_.len < mp_.getWidth( ) )
        mp_.setWidth( format_.len );
    return mp_.getWidth();
}

string
MonitorCell::computeText( ) {

    setMpWidthToFormatLength() ;

    string text;

    if ( sampleNo_ == 0 )
        text = mp_.getAverageToString( );
    else
        text = mp_.getValueToString( sampleNo_ - 1 );

    return text;
}

CellColor
MonitorCell::computeColor( ) {

    if (isGrayedOut()) return LIGHT_GRAY_TEXT_CELL_COLOR;
    
    MonitorPoint::VALIDITY validity;
    
    if ( sampleNo_ == 0 )
        validity = mpc_.getAveValidity( );
    else
        validity = mpc_.getValidity( sampleNo_ - 1 );

    CellColor result = defaultGoodColor_;

    switch ( validity ) {
        case MonitorPoint::VALID_ERROR_HIGH:
        case MonitorPoint::VALID_ERROR_LOW:
        case MonitorPoint::VALID_ERROR:
            result = computeErrorColor( );
            break;

        case MonitorPoint::VALID_WARNING_HIGH:
        case MonitorPoint::VALID_WARNING_LOW:
        case MonitorPoint::VALID_WARNING:
            result = computeWarnColor( );
            break;

        default:
            result = computeGoodColor( );
            break;
    }
    
    return result;
}


void
MonitorCell::setErrorColor( const CellColor errorColor ) {
    errorColor_ = errorColor;
}


CellColor
MonitorCell::computeErrorColor( ) {
    return errorColor_;
}


void
MonitorCell::setWarnColor( const CellColor warnColor ) {
    warnColor_ = warnColor;
}


CellColor
MonitorCell::computeWarnColor( ) {
    return warnColor_;
}


void
MonitorCell::setGoodColor( const CellColor goodColor ) {
    defaultGoodColor_ = goodColor;
}


void
MonitorCell::setGoodColorTextOverride( const string &  text,
                                       const CellColor color )
{
    if ( goodColorTextOverrides_.get() == 0 ) {
        goodColorTextOverrides_ =
            auto_ptr< StringToColorMap >( new StringToColorMap );
    }
    
    const StringToColorMap::iterator i = goodColorTextOverrides_->find( text );
        
    if ( i == goodColorTextOverrides_->end() )
        goodColorTextOverrides_->insert( make_pair( text, color ) );
    else
        i->second = color;
}


void
MonitorCell::removeGoodColorTextOverride( const string & text )
{
    if ( goodColorTextOverrides_.get() == 0 )
        return;

    const StringToColorMap::iterator i = goodColorTextOverrides_->find( text );
    
    if ( i == goodColorTextOverrides_->end() )
        return;
        
    goodColorTextOverrides_->erase( i );
    
    if ( goodColorTextOverrides_->empty() )
        goodColorTextOverrides_ = auto_ptr< StringToColorMap >();
}


CellColor
MonitorCell::computeGoodColor( )
{
    if ( goodColorTextOverrides_.get() != 0 ) {
        const string text = computeText();

        const StringToColorMap::const_iterator i =
            goodColorTextOverrides_->find( text );
            
        if ( i != goodColorTextOverrides_->end() )
            return i->second;
    }
    
    return defaultGoodColor_;
}


MonitorCellPtr
MonitorCell::makeCell(const int     cellWidth,
                      const bool    setMpWidth,
                      MonitorPoint& mp,
                      MonitorPoint& mpc,
                      const int     sampleNo ) {
    {
        // Semi-hack to get all the canbus state enums to show green for good
        
        StateMonitorPointEnum * const stateMonitorPointEnum =
            dynamic_cast< StateMonitorPointEnum * >( &mp );
            
        if ( stateMonitorPointEnum != 0 ) {
            return MonitorCellPtr(new MonitorCellStateMonitorPointEnum(cellWidth,
                                                       setMpWidth,
                                                       *stateMonitorPointEnum,
                                                       sampleNo ));
        }
    }
    
    {
        MonitorPointComplex * const mpComplex =
            dynamic_cast< MonitorPointComplex * >( &mp );
            
        if ( mpComplex != 0 ) {
            return MonitorCellPtr(new MonitorCellComplex( cellWidth,
                                           setMpWidth,
                                           *mpComplex,
                                           sampleNo ));
        }
    }

    {
        MonitorPointBool * const mpBool =
            dynamic_cast< MonitorPointBool * >( &mp );
            
        if ( mpBool != 0 ) {
            return MonitorCellPtr(new MonitorCellBool( cellWidth,
                                        setMpWidth,
                                        *mpBool,
                                        sampleNo ));
        }
    }

    return MonitorCellPtr(new MonitorCell( cellWidth,
                            setMpWidth,
                            mp,
                            mpc,
                            sampleNo ));
}

MonitorCellPtr
MonitorCell::makeCell(const int     cellWidth,
                      const bool    setMpWidth,
                      MonitorPoint& mp,
                      const int     sampleNo ) {
    return makeCell(cellWidth, setMpWidth, mp, mp, sampleNo);
}

MonitorCellPtr
MonitorCell::makeCell(const int     cellWidth,
                      MonitorPoint& mp,
                      const int     sampleNo ) {
    return makeCell( cellWidth, true, mp, mp, sampleNo);
}


MonitorCellPtr
MonitorCell::makeCell(MonitorPoint& mp,
                      const int     sampleNo ) {
    return makeCell( mp.getWidth(), false, mp, mp, sampleNo);
}


MonitorCellPtr
MonitorCell::makeCell( const int      cellWidth,
                       MonitorPoint & mp ) {
    return makeCell( cellWidth, mp, mp, 0);
}


MonitorCellPtr
MonitorCell::makeCell( MonitorPoint & mp ) {
    return makeCell(mp, mp, 0);
}
MonitorCellPtr
MonitorCell::makeCell(const int     cellWidth,
                      MonitorPoint& mp,
                      MonitorPoint& mpc,
                      const int     sampleNo ) {
    return makeCell( cellWidth, true, mp, mpc, sampleNo);
}


MonitorCellPtr
MonitorCell::makeCell(MonitorPoint& mp, 
                      MonitorPoint& mpc,
                      const int     sampleNo ) {
    return makeCell( mp.getWidth(), false, mp, mpc, sampleNo);
}


MonitorCellPtr
MonitorCell::makeCell(const int     cellWidth,
                      MonitorPoint& mp, 
                      MonitorPoint& mpc ) {
    return makeCell( cellWidth, mp, mpc, 0);
}


MonitorCellPtr
MonitorCell::makeCell(MonitorPoint& mp, MonitorPoint& mpc) {
    return makeCell(mp, mpc, 0);
}


MonitorCellBool::MonitorCellBool( const int          cellWidth,
                                  const bool         setMpWidth,
                                  MonitorPointBool & mpBool,
                                  const int          sampleNo ) :
MonitorCell( cellWidth, setMpWidth, mpBool, sampleNo ),
mpBool_( mpBool ),
trueStateTextOverridden_( false ),
trueStateColourOverridden_( false ),
falseStateTextOverridden_( false ),
falseStateColourOverridden_( false ) 
{
    setPlottable( false );
}


void
MonitorCellBool::overrideStateAppearances(
    const string & trueStateText,  const CellColor trueStateColour,
    const string & falseStateText, const CellColor falseStateColour ) {
    trueStateOverrideText_ = trueStateText;
    trueStateOverrideColour_ = trueStateColour;
    
    falseStateOverrideText_ = falseStateText;
    falseStateOverrideColour_ = falseStateColour;
    
    trueStateTextOverridden_ = true;
    trueStateColourOverridden_ = true;
    
    falseStateTextOverridden_ = true;
    falseStateColourOverridden_ = true;
}


void
MonitorCellBool::clearOverrides( ) {
    trueStateTextOverridden_ = false;
    trueStateColourOverridden_ = false;
    
    falseStateTextOverridden_ = false;
    falseStateColourOverridden_ = false;
}


string
MonitorCellBool::computeText( ) {
    bool overridden = false;
    string overrideText;
    
    if ( trueStateTextOverridden_ || falseStateTextOverridden_ ) {
        bool state;
        
        {
            const int sampleNo = getSampleNo( );
        
            if ( sampleNo == 0 )
                state = mpBool_.getAve( );
            else
                state = mpBool_.getValue( sampleNo - 1 );
        }
        
        if ( state ) {
            overridden = trueStateTextOverridden_;
            overrideText = trueStateOverrideText_;
        } else {
            overridden = falseStateTextOverridden_;
            overrideText = falseStateOverrideText_;
        }
    }

    if ( overridden )
        return overrideText;
    else
        return MonitorCell::computeText( );
}


CellColor
MonitorCellBool::computeGoodColor( ) {
    bool overridden = false;
    CellColor overrideColour = WHITE_CELL_COLOR;
    
    if ( trueStateColourOverridden_ || falseStateColourOverridden_ ) {
        bool state;
        
        {
            const int sampleNo = getSampleNo( );
        
            if ( sampleNo == 0 )
                state = mpBool_.getAve( );
            else
                state = mpBool_.getValue( sampleNo - 1 );
        }
        
        if ( state ) {
            overridden = trueStateColourOverridden_;
            overrideColour = trueStateOverrideColour_;
        } else {
            overridden = falseStateColourOverridden_;
            overrideColour = falseStateOverrideColour_;
        }
    }

    if ( overridden )
        return overrideColour;
    else
        return MonitorCell::computeGoodColor( );
}

MonitorCellComplex::MonitorCellComplex(
    const int                                        cellWidth,
    const bool                                       setMpWidth,
    MonitorPointComplex &                            mpComplex,
    const int                                        sampleNo,
    const MonitorPointComplex::stringValueReturnType complexRep ) :
MonitorCell( cellWidth, setMpWidth, mpComplex, sampleNo ),
mpComplex_( mpComplex ),
complexRep_( complexRep )
{
    mpComplex.setStringRepresentation( complexRep );
    setPlottable( true );
}

MonitorCellComplex::MonitorCellComplex(
    const int                                        cellWidth,
    const bool                                       setMpWidth,
    MonitorPointComplex &                            mpComplex,
    const int                                        sampleNo ) : 
MonitorCell( cellWidth, setMpWidth, mpComplex, sampleNo ),
mpComplex_( mpComplex ),
complexRep_( mpComplex.getStringRepresentation( ) )
{
    setPlottable( true );
}

string 
MonitorCellComplex::computeText( ) {
    mpComplex_.setStringRepresentation( complexRep_ );
    return MonitorCell::computeText( );

}

MonitorCellStateMonitorPointEnum::MonitorCellStateMonitorPointEnum(
    const int             cellWidth,
    const bool            setMpWidth,
    StateMonitorPointEnum & stateMonitorPointEnum,
    const int             sampleNo) :
MonitorCell( cellWidth, setMpWidth, stateMonitorPointEnum, sampleNo ) {
    setGoodColor( GREEN_CELL_COLOR );
}

void MonitorCell::setAntenna(AntennaMapper::Antenna* ant)
{
  antenna_ = ant;
}

bool MonitorCell::hasAntenna()
{
  return (antenna_ != 0);
}

bool MonitorCell::conditionsAreMet()
{
  bool satisfied = true;

  ostringstream os;
  os << "Inside conditionAreMet for " << getName() << " size = " << conditions_.size() << std::endl;
  RtDisplay::appendToFile(os.str());

  if(conditions_.size() > 0) {
    for(unsigned iCond=0; iCond < conditions_.size(); iCond++)
      satisfied = satisfied && conditions_[iCond].isSatisfied();
  }

  os.str("");
  os << "Inside 2 conditionAreMet for " << getName() << " size = " << conditions_.size() << std::endl;
  RtDisplay::appendToFile(os.str());

  return satisfied;
}

void MonitorCell::colorIf(monitor::MonitorPoint& mp, 
				sza::util::MonitorCondition condition,
				int sampleNo)
{
  Condition cond(mp, condition, sampleNo);
  conditions_.push_back(cond);
}

/**.......................................................................
 * Special Condition for current LO frequency
 */
void MonitorCell::colorIfFrequencyInGHz(sza::util::MonitorCondition condition)
{
  Condition cond(antenna_, condition);
  conditions_.push_back(cond);
}
