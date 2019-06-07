#include <stdlib.h>
#include <iostream>
#include <string>

#include "carma/antenna/sza/antenna/corba/PointingModel.h"

using namespace std;
using namespace sza::antenna::corba;

#ifdef SZA_PM
void PointingModel::scheduleNextSequenceNo(unsigned long seq)
{
  std::cout << "PointingModel::scheduleNextSequenceNo() stub" << std::endl;
}

void PointingModel::setAzTilts(double tilt1, double tilt2)
{
  cout << "PointingModel::setAzTilts() stub" << endl;
}

void PointingModel::setElTilts(double tilt1, double tilt2)
{
  cout << "PointingModel::setElTilts() stub" << endl;
}

void PointingModel::setTilts(double azTilt1, double azTilt2,
			     double elTilt1, double elTilt2)
{
  cout << "PointingModel::setTilts() stub" << endl;
}

void PointingModel::setAzElMisalignment(double misalignment)
{
  cout << "PointingModel::setAzElMisalignment() stub" << endl;
}

void PointingModel::setFlexure(carma::antenna::common::PointingModelControl::Model model, double flexure)
{
  cout << "PointingModel::setFlexure() stub" << endl;
}

void PointingModel::setCollimation(carma::antenna::common::PointingModelControl::Model model, double magnitude, double direction)
{
  cout << "PointingModel::setCollimation() stub" << endl;
}

void PointingModel::setEncoderZero(double az, double el )
{
  cout << "PointingModel::setEncoderZero() stub" << endl;
}

void PointingModel::setEncoderCal(double az, double el )
{
  cout << "PointingModel::setEncoderCal() stub" << endl;
}

void PointingModel::setAzResiduals(double res1, double res2)
{
  cout << "PointingModel::setAzResiduals() stub" << endl;
}

void PointingModel::setElResiduals(double res1, double res2)
{
  cout << "PointingModel::setElResiduals() stub" << endl;
}

void PointingModel::setFourierResidual(double residual)
{
  cout << "PointingModel::setFourierResidual() stub" << endl;
}
#endif
