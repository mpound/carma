#ifndef SZA_UTIL_DATE_H
#define SZA_UTIL_DATE_H

/**
 * @file Date.h
 * 
 * Tagged: Thu Dec 29 09:51:01 PST 2005
 * 
 * @author tcsh: username: Command not found.
 */
#include <string>

namespace sza {
  namespace util {
    
    class Date {
    public:
      
      /**
       * Constructor.
       */
      Date();
      Date(Date& date);
      Date(Date* date);
      Date(const Date& date);
      Date(std::string date);
      Date(int day, std::string month, int year);
      Date(double mjd);
      
      void initialize(unsigned day, std::string month, int year);
      
      /**
       * Destructor.
       */
      virtual ~Date();
      
      static double calToMjd(int iy, int im, int id);
      static double calToMjd(unsigned day, std::string month, int year);
      static double calToMjd(int iy, int im, int id, int hour, int min, int sec);

      static std::string mjdToCal(double mjd);
      std::string mjdToCal();

      static std::string mjdToArcCal(double mjd);
      std::string mjdToArcCal();

      static std::string mjdToHorizonsCal(double mjd);
      std::string mjdToHorizonsCal();

      static void mjdToCalDate(double mjd, 
			       unsigned& year, unsigned& month, unsigned& day);
      
      static std::string mjdToBuildTime(double mjd);
      static std::string mjdToBuildDate(double mjd);

      double getMjd() {
        return mjd_;
      }

      unsigned year();
      std::string month();
      unsigned day();
      
      void setTo(std::string date);
      void setTo(std::string date, std::string time);
      void setToDateAndTime(std::string dateAndTime);

      // Assignment operator
      
      void operator=(std::string start);
      
      bool operator>(Date& date);
      bool operator>=(Date& date);
      bool operator<(Date& date);
      bool operator<(const Date& date);
      bool operator<=(Date& date);
      bool operator==(Date& date);
      Date operator-(double days);
      Date operator-(Date& days);
      Date operator+(double days);
      void operator+=(double days);
      
      friend std::ostream& operator<<(std::ostream& os, Date& date);
      friend std::ostream& operator<<(std::ostream& os, const Date& date);
      
      static int deltaDays(Date& date1, Date& date2);
      
      static const char* months[];
      
      void addDays(double days);
      void addHours(double hours);
      
      unsigned numberOfDays();
      
      bool isEmpty();
      
      unsigned dayInYear();

      double mjd() {
	return mjd_;
      }

    private:
      
      double mjd_;
      
      static void slaDjcal(int ndp, double djm, int iymdf[4]);
      static void slaDjcl(double djm, int* iy, int* im, int* id, double* df, int* status);
      static void slaClyd(int iy, int im, int id, int *ny, int *nd, int *jstat );

      static unsigned validateMonth(std::string month);
      static unsigned validateDay(unsigned day, std::string month);
      static int validateYear(int year);
      
    }; // End class Date
    
  } // End namespace util
} // End namespace sza

#endif // End #ifndef SZA_UTIL_DATE_H

