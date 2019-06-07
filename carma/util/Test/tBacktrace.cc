//
// @version $Revision: 1.8 $ $Date: 2014/03/10 16:33:35 $
//
// @usage use it
//
// @description
//  Test program for testing the backtrace class.
//
// @key spitOne false bool wther to spit one out to cout
// @key depth 9 int depth of recursion/call stack to test
//
// @logger TEST_FACILITY carma.test.util.tBacktrace
//

#include "carma/util/Program.h"

#include <iostream>

#include "carma/util/compileTimeCheck.h"
#include "carma/util/Backtrace.h"
#include "carma/util/ErrorException.h"
#include "carma/util/programLogging.h"

using namespace ::std;
using namespace carma;
using namespace carma::util;


namespace {


bool
testDemangler( const char * const mangled,
               const char * const demangled )
{
    const string actualDemangled1 =
        Backtrace::demangleSymbolLine( mangled );
    
    const string mangledAsString = mangled;
    
    const string actualDemangled2 =
        Backtrace::demangleSymbolLine( mangledAsString );

    string actualDemangled3 = mangled;
    
    Backtrace::demangleSymbolLineInPlace( actualDemangled3 );

    if ( (actualDemangled1 != actualDemangled2) ||
         (actualDemangled2 != actualDemangled3) ||
         (actualDemangled3 != actualDemangled1) ) {
        programLogErrorIfPossible(
            "Mismatched demangles: \"" + string( mangled ) +
            "\" -> \"" + actualDemangled1 + "\", \"" + actualDemangled2 +
            "\", \"" + actualDemangled3 + "\" instead of \"" +
            string( demangled ) + "\"" );
        
        return false;
    }
    
    if ( (actualDemangled1 != demangled ) ||
         (actualDemangled2 != demangled ) ||
         (actualDemangled3 != demangled ) ) {
        programLogErrorIfPossible(
            "Bad demangle(s): \"" + string( mangled ) +
            "\" -> \"" + actualDemangled1 + "\" instead of \"" +
            string( demangled ) + "\"" );

        return false;
    }
    
    programLogInfoIfPossible(
        "Good demangle: \"" + string( mangled ) +
        "\" -> \"" + actualDemangled1 + "\" thrice" );

    return true;
}


const char * const kMangledBt1Line[] = {
    /* #0 */ "/home/control/rt/UPDATE-2006-03-16-0928-2-6-15-4-reiser4/lib/libcarmadbms.so(_ZN5carma4dbms30MonitorDataTableFinisherThread6actionEv+0x4c) [0x26f124]",
    /* #1 */ "/home/control/rt/UPDATE-2006-03-16-0928-2-6-15-4-reiser4/lib/libcarmautil.so(_ZN5carma4util11CarmaThread3runEv+0x5e) [0xc03be6]",
    /* #2 */ "/opt/carmaTools/lib/libJTC.so.2.0.1(_ZN9JTCThread3runEv+0x1c) [0xcd80a8]",
    /* #3 */ "/opt/carmaTools/lib/libJTC.so.2.0.1(_ZN9JTCThread13entrance_hookEv+0x125) [0xcdb59d]",
    /* #4 */ "/opt/carmaTools/lib/libJTC.so.2.0.1(lsf_thread_adapter+0x1d) [0xcdb5c5]",
    /* #5 */ "/lib/tls/libpthread.so.0 [0xcee341]",
    /* #6 */ "/lib/tls/libc.so.6(__clone+0x5e) [0x1deffee]"
};

const char * const kDemangledBt1Line[] = {
    /* #0 */ "/home/control/rt/UPDATE-2006-03-16-0928-2-6-15-4-reiser4/lib/libcarmadbms.so(carma::dbms::MonitorDataTableFinisherThread::action()+0x4c) [0x26f124]",
    /* #1 */ "/home/control/rt/UPDATE-2006-03-16-0928-2-6-15-4-reiser4/lib/libcarmautil.so(carma::util::CarmaThread::run()+0x5e) [0xc03be6]",
    /* #2 */ "/opt/carmaTools/lib/libJTC.so.2.0.1(JTCThread::run()+0x1c) [0xcd80a8]",
    /* #3 */ "/opt/carmaTools/lib/libJTC.so.2.0.1(JTCThread::entrance_hook()+0x125) [0xcdb59d]",
    /* #4 */ "/opt/carmaTools/lib/libJTC.so.2.0.1(lsf_thread_adapter+0x1d) [0xcdb5c5]",
    /* #5 */ "/lib/tls/libpthread.so.0 [0xcee341]",
    /* #6 */ "/lib/tls/libc.so.6(__clone+0x5e) [0x1deffee]"
};


const char * const kMangledBt2Line[] = {
    /*  #0 */ "/home/control/Carma/rt/UPDATE-2007-07-18-0839-2-6-9-42-ELsmp/lib/libcarmautil.so(_ZN76_GLOBAL__N__home_control_Carma_carma_carma_util_Program.cc_9D2164D3_3317194913signalHandlerEiP7siginfoPv+0x66) [0x227ff6]",
    /*  #1 */ "/lib/tls/libpthread.so.0 [0x532890]",
    /*  #2 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(_ZNK17keys_compare_lessclEPKN5DbXml9QueryPlanES3_+0x2d) [0x4f20307]",
    /*  #3 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(_ZSt25__unguarded_linear_insertIN9__gnu_cxx17__normal_iteratorIPPN5DbXml9QueryPlanESt6vectorIS4_SaIS4_EEEES4_17keys_compare_lessEvT_T0_T1_+0x49) [0x4f22679]",
    /*  #4 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(_ZSt16__insertion_sortIN9__gnu_cxx17__normal_iteratorIPPN5DbXml9QueryPlanESt6vectorIS4_SaIS4_EEEE17keys_compare_lessEvT_SB_T0_+0x105) [0x4f205fb]",
    /*  #5 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(_ZSt22__final_insertion_sortIN9__gnu_cxx17__normal_iteratorIPPN5DbXml9QueryPlanESt6vectorIS4_SaIS4_EEEE17keys_compare_lessEvT_SB_T0_+0xac) [0x4f1da5a]",
    /*  #6 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(_ZSt4sortIN9__gnu_cxx17__normal_iteratorIPPN5DbXml9QueryPlanESt6vectorIS4_SaIS4_EEEE17keys_compare_lessEvT_SB_T0_+0x7c) [0x4f1b898]",
    /*  #7 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(_ZNK5DbXml11IntersectQP7executeERNS_16OperationContextERNS_21QueryExecutionContextE+0xdf) [0x4f0b529]",
    /*  #8 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(_ZN5DbXml17QueryPlanFunction15QueryPlanResult4initEP14DynamicContext+0xb4) [0x4f96886]",
    /*  #9 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(_ZN5DbXml17QueryPlanFunction15QueryPlanResult4nextEP14DynamicContext+0x42) [0x4f96948]",
    /* #10 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(_ZN5DbXml19QueryPlanResultImpl4nextEP14DynamicContext+0x116) [0x4fbd6e6]",
    /* #11 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(_ZN5DbXml11DbXmlFilter12FilterResult4nextEP14DynamicContext+0x3a) [0x4fb43ee]",
    /* #12 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libxqilla.so.0(_ZN7XQQuery11QueryResult4nextEP14DynamicContext+0x7f6) [0x1aa27a8]",
    /* #13 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(_ZN5DbXml13LazyDIResults4nextERNS_8XmlValueE+0x123) [0x4ed2f0d]",
    /* #14 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(_ZN5DbXml10XmlResults4nextERNS_8XmlValueE+0x4d) [0x4eea251]",
    /* #15 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(_ZN5DbXml12ValueResultsC1EPNS_7ResultsE+0xc8) [0x4ed1554]",
    /* #16 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(_ZN5DbXml15QueryExpression7executeEPNS_11TransactionEPNS_5ValueERNS_15XmlQueryContextEj+0x140) [0x4ea5e40]",
    /* #17 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(_ZNK5DbXml18XmlQueryExpression7executeERNS_15XmlQueryContextEj+0x54) [0x4edeac8]",
    /* #18 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(_ZN5DbXml10XmlManager5queryERKSsRNS_15XmlQueryContextEj+0x3e) [0x4efd0f8]",
    /* #19 */ "/home/control/Carma/rt/UPDATE-2007-07-18-0839-2-6-9-42-ELsmp/lib/libcarmaobservertools.so(_ZN5carma13observertools26ProjectDatabaseManagerImpl12projectQueryERKN2OB6VarSeqINS0_9ItemValueENS0_26OBUnique_ItemValueSequenceEEE+0x1be) [0x783c2e]",
    /* #20 */ "/home/control/Carma/rt/UPDATE-2007-07-18-0839-2-6-9-42-ELsmp/lib/libcarmaobservertools.so(_ZN9POA_carma13observertools22ProjectDatabaseManager19_OB_op_projectQueryEPN2OB6UpcallE+0x83) [0x74303f]",
    /* #21 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libOB.so.4.2.0(_ZN16OBPortableServer8POA_impl12_OB_dispatchERKN2OB6FixSeqIhN14PortableServer17OBUnique_ObjectIdEEEPNS1_6UpcallE+0x97) [0x109eeb3]",
    /* #22 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libOB.so.4.2.0(_ZN2OB20DispatchRequest_impl6invokeEv+0x24) [0x10869fc]",
    /* #23 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libOB.so.4.2.0(_ZN2OB23DispatchSameThread_impl8dispatchEPNS_15DispatchRequestE+0xf) [0x1085927]",
    /* #24 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libOB.so.4.2.0(_ZN2OB6Upcall6invokeEv+0x30) [0x113697c]",
    /* #25 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libOB.so.4.2.0(_ZN2OB29ThreadedGIOPConnectionHandler9doReceiveEv+0x315) [0x112b7a1]",
    /* #26 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libOB.so.4.2.0(_ZN2OB18GIOPReceiverThread3runEv+0x8a) [0x112b842]",
    /* #27 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libJTC.so.2.0.1(_ZN9JTCThread13entrance_hookEv+0x125) [0x49c595]",
    /* #28 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libJTC.so.2.0.1(lsf_thread_adapter+0x1d) [0x49c5bd]",
    /* #29 */ "/lib/tls/libpthread.so.0 [0x52c371]",
    /* #30 */ "/lib/tls/libc.so.6(__clone+0x5e) [0x1f01ffe]"
};

const char * const kDemangledBt2Line[] = {
    /*  #0 */ "/home/control/Carma/rt/UPDATE-2007-07-18-0839-2-6-9-42-ELsmp/lib/libcarmautil.so((anonymous namespace)::signalHandler(int, siginfo*, void*)+0x66) [0x227ff6]",
    /*  #1 */ "/lib/tls/libpthread.so.0 [0x532890]",
    /*  #2 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(keys_compare_less::operator()(DbXml::QueryPlan const*, DbXml::QueryPlan const*) const+0x2d) [0x4f20307]",
    /*  #3 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(void std::__unguarded_linear_insert<__gnu_cxx::__normal_iterator<DbXml::QueryPlan**, std::vector<DbXml::QueryPlan*, std::allocator<DbXml::QueryPlan*> > >, DbXml::QueryPlan*, keys_compare_less>(__gnu_cxx::__normal_iterator<DbXml::QueryPlan**, std::vector<DbXml::QueryPlan*, std::allocator<DbXml::QueryPlan*> > >, DbXml::QueryPlan*, keys_compare_less)+0x49) [0x4f22679]",
    /*  #4 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(void std::__insertion_sort<__gnu_cxx::__normal_iterator<DbXml::QueryPlan**, std::vector<DbXml::QueryPlan*, std::allocator<DbXml::QueryPlan*> > >, keys_compare_less>(__gnu_cxx::__normal_iterator<DbXml::QueryPlan**, std::vector<DbXml::QueryPlan*, std::allocator<DbXml::QueryPlan*> > >, __gnu_cxx::__normal_iterator<DbXml::QueryPlan**, std::vector<DbXml::QueryPlan*, std::allocator<DbXml::QueryPlan*> > >, keys_compare_less)+0x105) [0x4f205fb]",
    /*  #5 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(void std::__final_insertion_sort<__gnu_cxx::__normal_iterator<DbXml::QueryPlan**, std::vector<DbXml::QueryPlan*, std::allocator<DbXml::QueryPlan*> > >, keys_compare_less>(__gnu_cxx::__normal_iterator<DbXml::QueryPlan**, std::vector<DbXml::QueryPlan*, std::allocator<DbXml::QueryPlan*> > >, __gnu_cxx::__normal_iterator<DbXml::QueryPlan**, std::vector<DbXml::QueryPlan*, std::allocator<DbXml::QueryPlan*> > >, keys_compare_less)+0xac) [0x4f1da5a]",
    /*  #6 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(void std::sort<__gnu_cxx::__normal_iterator<DbXml::QueryPlan**, std::vector<DbXml::QueryPlan*, std::allocator<DbXml::QueryPlan*> > >, keys_compare_less>(__gnu_cxx::__normal_iterator<DbXml::QueryPlan**, std::vector<DbXml::QueryPlan*, std::allocator<DbXml::QueryPlan*> > >, __gnu_cxx::__normal_iterator<DbXml::QueryPlan**, std::vector<DbXml::QueryPlan*, std::allocator<DbXml::QueryPlan*> > >, keys_compare_less)+0x7c) [0x4f1b898]",
    /*  #7 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(DbXml::IntersectQP::execute(DbXml::OperationContext&, DbXml::QueryExecutionContext&) const+0xdf) [0x4f0b529]",
    /*  #8 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(DbXml::QueryPlanFunction::QueryPlanResult::init(DynamicContext*)+0xb4) [0x4f96886]",
    /*  #9 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(DbXml::QueryPlanFunction::QueryPlanResult::next(DynamicContext*)+0x42) [0x4f96948]",
    /* #10 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(DbXml::QueryPlanResultImpl::next(DynamicContext*)+0x116) [0x4fbd6e6]",
    /* #11 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(DbXml::DbXmlFilter::FilterResult::next(DynamicContext*)+0x3a) [0x4fb43ee]",
    /* #12 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libxqilla.so.0(XQQuery::QueryResult::next(DynamicContext*)+0x7f6) [0x1aa27a8]",
    /* #13 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(DbXml::LazyDIResults::next(DbXml::XmlValue&)+0x123) [0x4ed2f0d]",
    /* #14 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(DbXml::XmlResults::next(DbXml::XmlValue&)+0x4d) [0x4eea251]",
    /* #15 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(DbXml::ValueResults::ValueResults(DbXml::Results*)+0xc8) [0x4ed1554]",
    /* #16 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(DbXml::QueryExpression::execute(DbXml::Transaction*, DbXml::Value*, DbXml::XmlQueryContext&, unsigned int)+0x140) [0x4ea5e40]",
    /* #17 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(DbXml::XmlQueryExpression::execute(DbXml::XmlQueryContext&, unsigned int) const+0x54) [0x4edeac8]",
    /* #18 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libdbxml-2.3.so(DbXml::XmlManager::query(std::string const&, DbXml::XmlQueryContext&, unsigned int)+0x3e) [0x4efd0f8]",
    /* #19 */ "/home/control/Carma/rt/UPDATE-2007-07-18-0839-2-6-9-42-ELsmp/lib/libcarmaobservertools.so(carma::observertools::ProjectDatabaseManagerImpl::projectQuery(OB::VarSeq<carma::observertools::ItemValue, carma::observertools::OBUnique_ItemValueSequence> const&)+0x1be) [0x783c2e]",
    /* #20 */ "/home/control/Carma/rt/UPDATE-2007-07-18-0839-2-6-9-42-ELsmp/lib/libcarmaobservertools.so(POA_carma::observertools::ProjectDatabaseManager::_OB_op_projectQuery(OB::Upcall*)+0x83) [0x74303f]",
    /* #21 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libOB.so.4.2.0(OBPortableServer::POA_impl::_OB_dispatch(OB::FixSeq<unsigned char, PortableServer::OBUnique_ObjectId> const&, OB::Upcall*)+0x97) [0x109eeb3]",
    /* #22 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libOB.so.4.2.0(OB::DispatchRequest_impl::invoke()+0x24) [0x10869fc]",
    /* #23 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libOB.so.4.2.0(OB::DispatchSameThread_impl::dispatch(OB::DispatchRequest*)+0xf) [0x1085927]",
    /* #24 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libOB.so.4.2.0(OB::Upcall::invoke()+0x30) [0x113697c]",
    /* #25 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libOB.so.4.2.0(OB::ThreadedGIOPConnectionHandler::doReceive()+0x315) [0x112b7a1]",
    /* #26 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libOB.so.4.2.0(OB::GIOPReceiverThread::run()+0x8a) [0x112b842]",
    /* #27 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libJTC.so.2.0.1(JTCThread::entrance_hook()+0x125) [0x49c595]",
    /* #28 */ "/misc/carmaToolsDir/carmaTools2007_06_28/lib/libJTC.so.2.0.1(lsf_thread_adapter+0x1d) [0x49c5bd]",
    /* #29 */ "/lib/tls/libpthread.so.0 [0x52c371]",
    /* #30 */ "/lib/tls/libc.so.6(__clone+0x5e) [0x1f01ffe]"
};


bool
testDemangler( )
{
    bool allOkay = true;
    
    {
        const size_t kMangledBt1Lines =
            (sizeof( kMangledBt1Line ) / sizeof( kMangledBt1Line[0] ));
            
        const size_t kDemangledBt1Lines =
            (sizeof( kDemangledBt1Line ) / sizeof( kDemangledBt1Line[0] ));
        
        compileTimeCheck< (kMangledBt1Lines == kDemangledBt1Lines) >();
    
        for ( size_t i = 0; i < kMangledBt1Lines; ++i ) {
            if ( testDemangler( kMangledBt1Line[i],
                                kDemangledBt1Line[i] ) != true )
                allOkay = false;
        }
    }

    {
        const size_t kMangledBt2Lines =
            (sizeof( kMangledBt2Line ) / sizeof( kMangledBt2Line[0] ));
            
        const size_t kDemangledBt2Lines =
            (sizeof( kDemangledBt2Line ) / sizeof( kDemangledBt2Line[0] ));
        
        compileTimeCheck< (kMangledBt2Lines == kDemangledBt2Lines) >();
    
        for ( size_t i = 0; i < kMangledBt2Lines; ++i ) {
            if ( testDemangler( kMangledBt2Line[i],
                                kDemangledBt2Line[i] ) != true )
                allOkay = false;
        }
    }
    
    return allOkay;
}


}  // namespace < anonymous >


void Fu( int level = 9, const string & prefix = "" );


int
Program::main( )
{
    const bool spitOne = getBoolParameter( "spitOne" );
    const int depth = getIntParameter( "depth" );
    
    if ( testDemangler() != true )
        throw CARMA_ERROR( "testDemangler() failure" );
    
    if ( spitOne )
        Fu( depth );
    
    return 0;
}


void
SpitItOut( )
{
    Backtrace bt;
    
    bt.captureNoThrow( );
    
    cout << "Backtrace:\n" << bt.formattedAsString( ) << endl;
}


void
Bar( const int level = 9, const string & prefix = "" )
{
    if ( level <= 1 )
        SpitItOut( );
    else {
        cout << prefix << "Fu( " << level << " ); " << endl;
        
        Fu( level - 1, prefix + "    " );
    }
}


void
Fu( const int level, const string & prefix )
{
    if ( level <= 1 )
        SpitItOut( );
    else {
        cout << prefix << "Bar( " << level << " ); " << endl;

        Bar( level - 1, prefix + "    " );
    }
}
