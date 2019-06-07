/** @file
 * Carma Singleton Adaptor class.
 * Initial motivation was provided by work done by Eric Crahen on the ZThreads 
 * library. Based strongly on Loki::Singleton by Andrei Alexandrescu and 
 * somewhat on the ACE_Singleton by Doug Shmidt and others. The Loki and 
 * ACE implementations are far superior to what I've provide here, however,
 * using either of these would have required bringing in the whole package AND 
 * coding paradigm.  In general, these packages threading models would interfere
 * with Carma thread constructs so I've opted for a minimal homegrown 
 * implementation that should satisfy most common uses.
 *
 * References:
 * <ol>
 * <li>J. Vlissides, <i>"To Kill A Singleton"</i> C++ Report "Pattern Hatching"
 * column for June 1996.</li>
 * <li>D. Shmidt, T. Harrison, <i>"Double-Checked Locking"</i> Pattern Languages
 * of Program Design 3. Addison Wesley, 1997.</li>
 * </ol>
 * 
 * <dl><dt><b>Author </b></dt><dd>Andy Beard</dl>
 * $Revision: 1.23 $
 * $Date: 2005/02/22 21:29:15 $
 * $Id: Singleton.h,v 1.23 2005/02/22 21:29:15 tcosta Exp $
 */
#ifndef CARMA_UTIL_SINGLETON_H
#define CARMA_UTIL_SINGLETON_H

#include <stdexcept>
#include <typeinfo>

#include "carma/util/demangle.h"
#include "carma/util/PthreadMutex.h"
#include "carma/util/ScopedPthreadMutexLock.h"


namespace carma {
namespace util {

    /**
     * @class Singleton
     * Thread-safe, templatized Singleton class with policy based creation.
     *
     * Singleton attempts to mimic the Loki and ACE libraries policy based 
     * approach towards Singleton instance creation.  It was designed around
     * two basic use cases.  The first, allows a user to subclass from the
     * templatized Singleton class and create a singleton in the classical 
     * sense:
     * @code 
     * class Lonely : public Singleton<Lonely, CreateWithNewPolicy> {
     * private:
     *   
     *     // Must declare CreationPolicy<TYPE> a friend to allow 
     *     // internal access to your c'tor and d'tor.
     *     friend class CreateWithNewPolicy<Lonely>;
     *
     *     // Tribute to the Beatles dark side...
     *     Lonely() { cout << "Yes I'm Lonely, want to die..." << endl;}
     *     ~Lonely() { cout << "If I ain't dead already "
     *                      << "Ooh girl you know the reason why!" << endl;}
     * };
     * @endcode
     * This defines a true Singleton in that only one instance of Lonely will
     * ever exist. Please note the friend access to the Creation Policy - it 
     * is required.
     * 
     * So the steps to create a Singleton become:
     * <ol>
     * <li>Make your class constructor Private.</li>
     * <li>Publicly subclass from Singleton<Yourself, YourCreationPolicy>.</li>
     * <li>Declare your creation policy as a friend using:
     *  <ul>
     *  <li>carma::util::CreateWithNewPolicy</li>
     *  <li>carma::util::CreateStaticPolicy</li>
     *  <li>carma::util::CreateWithNewUnmanagedPolicy</li>
     *  </ul></li>
     * </ol>
     *
     * The second major use case centers around using Singleton as a convenient 
     * way to access a single instance of a class.  This technique is not a 
     * Singleton in the classical sense, as multiple instances of TYPE may 
     * be created independently of Singleton.  However, it does allow one to 
     * conveniently access a certain single instance of TYPE if need be 
     * (think carma::monitor subsystem access).  
     * @code
     * class PseudoLonely {
     * public:
     *
     *     // Constructor - note that it's public and anybody can use it!
     *     PseuedoLonely();
     *
     *     ~PseudoLonely();
     * ...
     * }; 
     * 
     * typedef Singleton<PseudoLonely, CreateWithNewPolicy> LonelyAccessor;
     * @endcode
     *   
     * The above concluding typedef allows one to easily change the creation 
     * policy of the Singleton accessor.  Changing it to:
     * 
     * @code
     * typedef StaticSingleton<PseudoLonely, CreateStaticPolicy> LonelyAccessor;
     * @endcode
     *
     * will cause the PseudoLonely instance to be created in static 
     * memory instead of on the heap.
     *
     * Regardless of which use case is used, the instance storage policy can be 
     * easily changed by either inheriting from or typdefing to an alternate
     * creation policy.  So far the following three exist:
     * @see carma::util::CreateWithNewPolicy
     * @see carma::util::CreateStaticPolicy
     * @see carma::util::CreateWithNewUnmanagedPolicy
     * 
     * Unfortunately, Singleton falls short in several areas.  It is based
     * only on creation policies and does not allow one to change the lifetime
     * of the instance nor the threading model. One of the main drawbacks
     * of this is that Singletons which depend on Singletons cannot guarantee
     * destruction order.  Nonetheless, Singleton, will throw exceptions if 
     * any invariants are violated.
     *
     * Please visit the documentation for Singleton.h for more information and
     * references.
     */
    template <class TYPE, template <class> class InstantiationPolicy>
    class Singleton;

    /**
     * @class CreateWithNewPolicy
     * Singleton creation policy which causes instances to be allocated
     * with new (on the heap).  Initialization is done upon first call to 
     * Singleton::instance (lazy initialization).
     * @see carma::util::Singleton For use case.
     */
    template <class TYPE>
    class CreateWithNewPolicy;

    /**
     * @class CreateStaticPolicy 
     * Singleton creation policy which causes instances to be created 
     * in function local static memory. Initialization is done upon first
     * call to Singleton::instance (lazy initialization).
     * @see carma::util::Singleton For use case.
     */
    template <class TYPE>
    class CreateStaticPolicy;

    /**
     * @class CreateWithNewUnmanagedPolicy
     * Singleton creation policy which causes instances to be allocated 
     * with new (on the heap) but which provides no deletion of memory.
     * With this creation policy, the user is completely responsible for making
     * sure that the instance is destroyed.  Here is an example which uses
     * an auto_ptr to delete the instance when the auto_ptr goes out of scope:
     * @code
     * #include <memory>
     * #include "carma/util/Program.h"
     *
     * using namespace carma::util;
     *
     * class Automanaged : 
     *      public Singleton<Automanaged, CreateWithNewUnmanagedPolicy> {
     * public:
     *      void doStuff() {};
     * private:
     *      friend class CreateWithNewUnmanagedPolicy<Automanaged>;
     *      friend class std::auto_ptr<Automanaged>;
     *      Automanaged();
     *      ~Automanaged() {
     *        Singleton<Automanaged, 
     *                  CreateWithNewUnmanagedPolicy>::destroyInstance();
     *      };
     * };
     *
     * int Program::main() {
     *   
     *   std::auto_ptr<Automanaged> managed(&(Automanaged::instance()));
     * 
     *   // Instance of Automanaged will automatically be destroyed when
     *   // we leave the scope of Program::main for whatever reason.
     * }
     * @endcode
     *
     * In general this is a pretty dangerous policy simply because
     * it allows for instances to more easily be deleted out from under one's 
     * feet. However, if created in the manner above, any access to 
     * Automanaged::instance() following destruction of the instance 
     * will result in a std::logic_error exception being thrown. 
     * 
     */
    template <class TYPE>
    class CreateWithNewUnmanagedPolicy;

// -----------------------------------------------------------------------------
    template <class TYPE, template<class> class InstantiationPolicy>
    class Singleton {
    public:

        /** 
         * Retrieve a reference to underlying instance.
         * @return Reference to TYPE
         * @throw ::std::logic_error on invalid Singleton state.
         */
        static TYPE& instance();

    protected:
        
        typedef enum INSTANCE_STATE {
                NO_INSTANCE,  // Make sure this stays first... 
                INSTANCE_CREATED,
                INSTANCE_DESTROYED,
                NUM_INSTANCE_STATES
        } InstanceStateType;
    
        explicit Singleton(); // Allow subclassing...
        virtual ~Singleton();  

        // Let instantiation policies have access to destroyInstance().
        // This allows the policies to have finer grained control over their
        // lifetime and destruction. Prior solution used a single lifetime. 
        friend class InstantiationPolicy<TYPE>;

        /**
         * Should be called when instance is destroyed.  
         * Responsible for setting state to INSTANCE_DESTROYED and 
         * setting instancePtr to NULL.  
         */
        static void destroyInstance();

        static TYPE* instancePtr_; // Guaranteed init to 0 (thanks Tom).
        static InstanceStateType state_; // Initializes to NO_INSTANCE
        
    private:

    };

    template <class TYPE, template<class> class InstantiationPolicy>
    TYPE& Singleton<TYPE, InstantiationPolicy>::instance() 
    {
        static carma::util::PthreadMutex lock;

        // Use Double-Checked Locking - See  'Double-Checked Locking - An 
        // Optimization Pattern for Efficiently Initializing and Accessing 
        // Thread-safe Objects' 
        // http://www.cs.wustl.edu/~schmidt/PDF/DC-Locking.pdf
        if (!instancePtr_) {
             ScopedPthreadMutexLock guard(lock);
             if (!instancePtr_) {
                
                if (state_ != NO_INSTANCE)
                    throw std::logic_error(carma::util::demangleTypeName(
                        typeid(Singleton<TYPE, InstantiationPolicy>)) 
                        + "::instance() - Invalid Singleton state.");
                
                instancePtr_ = InstantiationPolicy<TYPE>::createInstance();
                state_ = INSTANCE_CREATED;
             }
         }

         if (state_ != INSTANCE_CREATED)
            throw std::logic_error(carma::util::demangleTypeName(
                typeid(Singleton<TYPE, InstantiationPolicy>)) 
                + "::instance() - Invalid Singleton state.");

         return *instancePtr_;
    }

    template <class TYPE, template<class> class InstantiationPolicy>
    void Singleton<TYPE, InstantiationPolicy>::destroyInstance()
    {
        if (state_ != INSTANCE_CREATED)
            throw std::logic_error(carma::util::demangleTypeName(
                typeid(Singleton<TYPE, InstantiationPolicy>)) 
                + "::instance() - Invalid Singleton state.");
        InstantiationPolicy<TYPE>::deleteInstance(instancePtr_);
        instancePtr_ = 0;
        state_ = INSTANCE_DESTROYED;
    }

    template <class TYPE, template<class> class InstantiationPolicy>
    Singleton<TYPE, InstantiationPolicy>::Singleton() {};

    template <class TYPE, template<class> class InstantiationPolicy>
    Singleton<TYPE, InstantiationPolicy>::~Singleton() {}; 

    template <class TYPE, template<class> class InstantiationPolicy>
    TYPE* Singleton<TYPE, InstantiationPolicy>::instancePtr_; 
    
    template <class TYPE, template<class> class InstantiationPolicy>
    typename Singleton<TYPE, InstantiationPolicy>::InstanceStateType 
        Singleton<TYPE, InstantiationPolicy>::state_; 
    
// -----------------------------------------------------------------------------
// Create With New Instantiation Policy.
// -----------------------------------------------------------------------------
    template <class TYPE>
    class CreateWithNewPolicy {
    public:
        static TYPE* createInstance();
        static void deleteInstance(TYPE * ptr);
    private:

        CreateWithNewPolicy();
        ~CreateWithNewPolicy();
    };

    template <class TYPE>
    CreateWithNewPolicy<TYPE>::CreateWithNewPolicy() {}

    template <class TYPE>
    CreateWithNewPolicy<TYPE>::~CreateWithNewPolicy() {
        Singleton<TYPE, carma::util::CreateWithNewPolicy>::destroyInstance(); 
    }
        
    template <class TYPE>
    TYPE* CreateWithNewPolicy<TYPE>::createInstance()
    {
        static CreateWithNewPolicy doomed;  // Use self instance to bound life.
        return new TYPE; // Create instance
    } 
        
    template <class TYPE>
    void CreateWithNewPolicy<TYPE>::deleteInstance(TYPE * ptr)
    {
        delete ptr; // Delete instance
    }

// -----------------------------------------------------------------------------
// Create Static Instantiation Policy.
// -----------------------------------------------------------------------------
    template <class TYPE>
    class CreateStaticPolicy { 
    public:
        static TYPE* createInstance();
        static void deleteInstance(TYPE * ptr);
    private:
        
        CreateStaticPolicy();
        ~CreateStaticPolicy();
        
    };
    
    template <class TYPE>
    CreateStaticPolicy<TYPE>::CreateStaticPolicy() {}

    template <class TYPE>
    CreateStaticPolicy<TYPE>::~CreateStaticPolicy() {
        Singleton<TYPE, carma::util::CreateStaticPolicy>::destroyInstance(); 
    }
        
    template <class TYPE>
    TYPE* CreateStaticPolicy<TYPE>::createInstance()
    {
        static CreateStaticPolicy doomed; // Use self instance to bound life.
        static TYPE instance; // Create instance as a static variable
        return &instance;
    }

    template <class TYPE>
    void CreateStaticPolicy<TYPE>::deleteInstance(TYPE * ptr)
    {
        // Nothing - let the system cleanup. 
    }

// -----------------------------------------------------------------------------
// Create With New Unmanaged Instantiation Policy.
// -----------------------------------------------------------------------------
    template <class TYPE>
    class CreateWithNewUnmanagedPolicy {
    public:
        static TYPE* createInstance();
        static void deleteInstance(TYPE * ptr);
    };

    template <class TYPE>
    TYPE* CreateWithNewUnmanagedPolicy<TYPE>::createInstance()
    {
        return new TYPE;
    }

    template <class TYPE>
    void CreateWithNewUnmanagedPolicy<TYPE>::deleteInstance(TYPE * ptr)
    {
        // Nothing - User is responsible for their own deletion scheme.
    }
        
}} // End namespace carma::util
#endif
