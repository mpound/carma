#ifndef CARMA_UTIL_BACKTRACE_H
#define CARMA_UTIL_BACKTRACE_H

//! @file
//! Interface file for the carma::util::Backtrace class.

#include <string>
#include <vector>


namespace carma {
namespace util {


//! Class to hold a captured call stack backtrace.
class Backtrace {
    public:
        //! @brief Constructor that sets this instance to invalid.
        //!
        //! Construction does not implicitly capture a backtrace.
        explicit Backtrace( );
        
        //! Destructor.
        ~Backtrace( );
        
        bool captured() const;
        bool capturedAndValid() const;
        
        //! Method that captures a backtrace if it can and stores it in
        //! this instance. If the capture fails then return value is false
        //! and this instance is set to invalid.
        //! @return @c true if the capture succeeds,
        //!         @c false otherwise.
        bool captureNoThrow( );
        
        bool captureNoThrow( size_t maxDepth );
        
        bool captureNoThrow( bool convertToSymbols );
        
        bool captureNoThrow( size_t maxDepth,
                             bool   convertToSymbols );

        void convertToSymbols( );
        
        //! Returns a formatted multiline string representation of any valid
        //! backtrace stored in this instance.
        //! @return Formatted string representation of any valid backtrace 
        //!         stored in this instance.
        ::std::string formattedAsString( ) const;

        //! Returns a formatted multiline string representation of any valid
        //! backtrace stored in this instance.
        //! @param linePrefix String prefixed to each line of the result
        //! @param lineSuffix String suffixed to each line of the result
        //! @return Formatted string representation of any valid backtrace 
        //!         stored in this instance.
        ::std::string formattedAsString( const char * linePrefix,
                                         const char * lineSuffix ) const;

        //! Returns a formatted multiline string representation of any valid
        //! backtrace stored in this instance.
        //! @param linePrefix String prefixed to each line of the result
        //! @param lineSuffix String suffixed to each line of the result
        //! @return Formatted string representation of any valid backtrace 
        //!         stored in this instance.
        ::std::string
        formattedAsString( const ::std::string & linePrefix,
                           const ::std::string & lineSuffix ) const;

        //! Capture backtrace for the active call stack as a string.
        static ::std::string captureAsString( );

        //! Capture backtrace for the active call stack as a string.
        //! @param linePrefix String prefixed to each line of the result
        //! @param lineSuffix String suffixed to each line of the result
        static ::std::string captureAsString( const char * linePrefix,
                                              const char * lineSuffix );

        //! Capture backtrace for the active call stack as a string.
        //! @param linePrefix String prefixed to each line of the result
        //! @param lineSuffix String suffixed to each line of the result
        static ::std::string
        captureAsString( const ::std::string & linePrefix,
                         const ::std::string & lineSuffix );
        
        static void demangleSymbolLineInPlace( ::std::string & line );

        static ::std::string demangleSymbolLine( const ::std::string & line );
        
        static ::std::string demangleSymbolLine( const char * line );
        
    private:
        typedef enum {
            NOT_CAPTURED_STATE,
            INVALID_CAPTURE_STATE,
            NUMERIC_STATE,
            MANGLED_STATE,
            DEMANGLED_STATE
        } State;
        
        
        State                         state_;
        bool                          complete_;
        ::std::vector< void * >       numeric_;
        ::std::vector< ::std::string> symbolic_;
};


}  // namespace carma::util
}  // namespace carma


#endif
