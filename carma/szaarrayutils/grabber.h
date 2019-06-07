#ifndef grabber_h
#define grabber_h

#ifndef szacontrol_h
#define szacontrol.h
#endif

void grabber_offset_info(ControlProg *cp, double& xoff, double& yoff);
int grabber_peak_info(ControlProg *cp, double *peak, double *snr);

namespace sza {
  namespace util {
    class Angle;
  }
}

/**.......................................................................
 * Public method to set the optical camera FOV
 */
void setOpticalCameraFov(const sza::util::Angle& fov);
void setOpticalCameraFov();

/**.......................................................................
 * Public method to set the optical camera FOV
 */
void setOpticalCameraAspect(double aspect=0.0);

/**.......................................................................
 * Public method to set the optical camera collimation
 */
void setOpticalCameraCollimation(const sza::util::Angle& collimation);
void setOpticalCameraCollimation();

#endif
