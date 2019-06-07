
/**@file
 * Class definition for DriveCommand on the BIMA antennas.
 *
 * <dl><dt><b>Author </b></dt><dd>Colby Gutierrez-Kraybill </dl>
 * $Revision: 1.9 $
 * $Date: 2007/10/02 22:18:24 $
 * $Id: DriveCommand.h,v 1.9 2007/10/02 22:18:24 colby Exp $
 */


#ifndef CARMA_BIMA_DRIVECOMMAND_H
#define CARMA_BIMA_DRIVECOMMAND_H

#include <vector>

#define DRIVESIPQ  "drives.ipq"

#define ATTRIB(T,N) \
  private: T _ ## N ; \
  public: void set_ ## N (T v) { _ ## N = v; } \
  public: T get_ ## N () { return _ ## N ; }

// TODO Put in bounds checking to ensure (0 <= i < C)
#define ATTRIBA(T,N,C) \
  private: T _ ## N [ C ] ; \
  public: static const unsigned int N ## Count = C ; \
  public: void set_ ## N (unsigned int i, T v) { _ ## N [ i ] = v; } \
  public: T get_ ## N (unsigned int i) { return _ ## N [ i ]; }

#define DCSET(P,N,V) P -> set_ ## N ( V )
#define DCGET(P,N)   P -> get_ ## N ( )
#define DCSETA(P,N,I,V) P -> set_ ## N ( I , V )
#define DCGETA(P,N,I)   P -> get_ ## N ( I )

namespace carma
{
  namespace antenna
    {
      namespace bima
        {
          class DriveCommand
            {

            public:
              typedef enum {
                NOOP,
                STOW,
                STOP,
                SNOW,
                SET_ANTENNA_LOCATION,
                SET_NEXT_SEQ_NO,
                SET_WRAP,
                SET_AZ,
                SET_EL,
                SET_MAX_AZ_RATE,
                SET_MAX_EL_RATE,
                SET_RA_DEC,
                UPDATE_WEATHER,
                SET_AZ_MOUNT_OFFSET,
                SET_EL_MOUNT_OFFSET,
                SET_AZ_OFFSET,
                SET_EL_OFFSET,
                SET_OFFSET_PATH,
                SET_APERTURE_POINTING_CONSTANTS,
                SELECT_APERTURE,
                SET_POINTING_MODEL_COEFS,
                SET_TOLERANCE,
                SET_SAFE_RANGE,
              } CommandType;

              typedef enum {
                OPTICAL,
                RADIO1MM,
                RADIO3MM,
                RADIO1CM,
              } ApertureType;

              typedef enum {
                ZENITH,
                SERVICE,
                SAFE,
                UNSAFE,
              } PositionType;

              typedef enum {
                ZERO,
                ADD,
                SUB,
              } AzWrapModeType;

            ATTRIB(CommandType,command);
            ATTRIB(PositionType,stowPosition);
            ATTRIB(double,longitude);
            ATTRIB(double,latitude);
            ATTRIB(double,altitude);
            ATTRIB(unsigned long,seq);
            ATTRIB(AzWrapModeType,azWrapMode)
            ATTRIB(bool,overTheTop)
            ATTRIB(double,az);
            ATTRIB(double,el);
            ATTRIB(double,mjd);
            ATTRIB(double,ra);
            ATTRIB(double,dec);
            ATTRIB(bool,discontinuity);
            ATTRIB(float,ambientTemp);
            ATTRIB(float,barometricPressure);
            ATTRIB(float,relativeHumidity);
            ATTRIB(float,dewpointTemp);
            ATTRIB(float,windSpeed);
            ATTRIB(float,windDirection);
            ATTRIB(ApertureType,aperture);
            ATTRIB(double,apertureOffsetAz);
            ATTRIB(double,apertureOffsetEl);
            ATTRIB(double,azOffset);
            ATTRIB(double,elOffset);
            ATTRIB(double,azOffsetMount);
            ATTRIB(double,elOffsetMount);
            ATTRIB(double,maxRateAz);
            ATTRIB(double,maxRateEl);
            ATTRIB(double,sag);
            ATTRIBA(double,dazCoefs,10);
            ATTRIBA(double,delCoefs,10);
            ATTRIB(double,tolerance);
            ATTRIB(float,azLow);
            ATTRIB(float,azHigh);
            ATTRIB(float,elLow);
            ATTRIB(float,elHigh);

            };
        }
    }
}



#endif // CARMA_BIMA_DRIVECOMMAND_H

// vim: set expandtab sw=2 ts=2 cindent :
