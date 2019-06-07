#ifndef CARMA_PIPELINE_DATACOLLECTORTEST_H
#define CARMA_PIPELINE_DATACOLLECTORTEST_H

namespace carma {
  namespace pipeline {
    
      class DataContainer;
    
      /**
       * Concrete class mimics a DataCollector but doesn't need CORBA.
       * It just creates fake data and fills in the Container.
       */
      class DataCollectorTest {
      public:

        /**
         * Constructor
         */
        DataCollectorTest();

        /**
         * Constructor
         */
        DataCollectorTest(int bandNumber);

        /**
         * Destructor
         */
        virtual ~DataCollectorTest();

      private:    
        DataContainer*      _dataContainer;
        long                _numWritten; // number of consecutive writes
        int                 _bandNumber;

        /**
         *  starts listening in a separate thread
         */
        void start();

        // must use a static method in pthread
        static void* staticStart(void* arg);
        void fillDataContainer();
        void update();
      };

  } // End namespace pipeline
} // End namespace carma

#endif
