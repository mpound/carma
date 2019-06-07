
#ifndef CARMA_UI_RTD_WINDOWLIST_H
#define CARMA_UI_RTD_WINDOWLIST_H

/**
 * There are two classes in this module. The first, Window, defines 
 * characteristics of a window, including pgm name, etc.
 * The second, WindowList, is a collection of Windows.
 * 
 * @author Steve Scott
 *
 * $CarmaCopyright$
 *
 */


#include <vector>


namespace carma {
    namespace ui {
        namespace rtd { 



class Window {
public:
    /**
     * Constructor for a window
     * @param window name of the window, used as key or code
     * @param program name of the rtd program binary (no directory)
     * @param integer1 an integer parameter passed to the rtd program
     * @param guest window only requires guest capabilities
     */
    Window(const std::string& window, const std::string& program, 
        int integer = 0, bool guest = true); 

    /**
     * Constructor for a window
     * @param window name of the window, used as key or code
     * @param program name of the rtd program binary (no directory)
     * @param string1 a parameter string passed to the rtd program
     * @param integer1 an integer parameter passed to the rtd program
     * @param guest window only requires guest capabilities
     */
    Window(const std::string& window, const std::string& program, 
        const std::string& string1,  
        int integer = 0, bool guest = true); 

    /**
     * Constructor for a window
     * @param window name of the window, used as key or code
     * @param program name of the rtd program binary (no directory)
     * @param string1 a parameter string passed to the rtd program
     * @param string2 a parameter string passed to the rtd program
     * @param integer1 an integer parameter passed to the rtd program
     * @param guest window only requires guest capabilities
     */
    Window(const std::string& window, const std::string& program, 
        const std::string& string1,  const std::string& string2,  
        int integer = 0, bool guest = true); 

    /**
     * Constructor for a window
     * @param window name of the window, used as key or code
     * @param program name of the rtd program binary (no directory)
     * @param string1 a parameter string passed to the rtd program
     * @param string2 a parameter string passed to the rtd program
     * @param string3 a parameter string passed to the rtd program
     * @param integer1 an integer parameter passed to the rtd program
     * @param guest window only requires guest capabilities
     */
    Window(const std::string& window, const std::string& program, 
        const std::string& string1,  const std::string& string2,  
        const std::string& string3, 
        int integer = 0, bool guest = true); 

     /**
     * Constructor for a window
     * @param window name of the window, used as key or code
     * @param program name of the rtd program binary (no directory)
     * @param string1 a parameter string passed to the rtd program
     * @param string2 a parameter string passed to the rtd program
     * @param string3 a parameter string passed to the rtd program
     * @param string4 a parameter string passed to the rtd program
     * @param integer1 an integer parameter passed to the rtd program
     * @param guest window only requires guest capabilities
     */
    Window(const std::string& window, const std::string& program, 
        const std::string& string1,  const std::string& string2,  
        const std::string& string3,  const std::string& string4,  
        int integer = 0, bool guest = true); 

    /**
     * Destructor
     */
    virtual ~Window() {};

    /**
     * Get the keyword for the window
     * @return keyword
     */
    std::string getWindowName() const;
    
    /**
     * Get the program name that is run to feed this window
     * @return program name
     */
    std::string getProgramName() const;
    
    /**
     * Get the first string parameter
     * @return string parameter #1
     */
    std::string getString1() const;
    
    /**
     * Get the second string parameter
     * @return string parameter #2
     */
    std::string getString2() const;
    
    /**
     * Get the third string parameter
     * @return string parameter #3
     */
    std::string getString3() const;
    
    /**
     * Get the fourth string parameter
     * @return string parameter #4
     */
    std::string getString4() const;
    
    /**
     * Get the first integer parameter, as a string
     * @return integer#1 as a string
     */
    std::string getInteger1() const;
    
    /**
     * Get the whether control is required for this window
     * @return control required flag
     */
    bool  isControl() const;
    
private:
    explicit Window(); // Hide the default constructor
    
    const std::string windowName_;
    const std::string programName_;
    const std::string string1_;
    const std::string string2_;
    const std::string string3_;
    const std::string string4_;
    const int         integer1_;
    
    // Whether the window needs control state to be invoked
    const bool        guest_;
};



/**
 * A list of all possible windows on a system and methods to find them.
 * This is an abstract base class that needs to have the "load()" method
 * defined to become functional.
 * 
 * A key (window code) is used to find the name of the executable to 
 * invoke for each window.
 *
 */

class WindowList {
public:
    /// Constructor
    explicit WindowList( );

    // destructor
    virtual ~WindowList( );
    
    /**
     * Pure virtual - define it with a bunch of statements like:
     * <BR>
     *  add(new Window("demo", "demoprogramName"));
     */
    virtual void load() = 0;

    /// Add a window definition to the list
    void add( Window * w );
    
    /**
     * Find a window in the list with the given name.
     * @return pointer to window if found, zero is not found
     */
    Window* find(const std::string& windowName) const;

    ::std::vector< ::std::string > getNames() const;

private:
    ::std::vector< Window * > list_;
};

}}} // End namespace carma::ui::rtd

#endif    // CARMA_UI_RTD_WINDOWLIST_H

