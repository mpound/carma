#include <iostream>
#include <vector>

#include "carma/util/Program.h"
#include "carma/services/Matrix.h"

using namespace std;
using namespace carma::util;
using namespace carma::services;

//
// @version $Revision: 1.5 $ $Date: 2006/04/29 16:43:34 $
//
// @usage      Testing Matrix
//
// @description
//     This program tests out the Matrix and Vector class
//
// @noKeys
// @logger DEFAULT_FACILITY carma.services.Test.tMatrix
//

int Program::main()
{
  /*
  Matrix<float> mat1(2,3);

  mat1[0][0] = 1; mat1[0][1] = 1; mat1[0][2] = 1;
  mat1[1][0] = 1; mat1[1][1] = 2; mat1[1][2] = 1;

  sza::util::Matrix<float> mat2(3,3);

  mat2[0][0] = 0; mat2[0][1] = 1; mat2[0][2] = 3;
  mat2[1][0] = 1; mat2[1][1] = 2; mat2[1][2] = 2;
  mat2[2][0] = 1; mat2[2][1] = 1; mat2[2][2] = 1;

  sza::util::Matrix<float> mat3(4,4);

  mat3[0][0] = 0; mat3[0][1] = 1; mat3[0][2] = 3; mat3[0][3] = 2;
  mat3[1][0] = 1; mat3[1][1] = 2; mat3[1][2] = 2; mat3[1][3] = 2;
  mat3[2][0] = 1; mat3[2][1] = 1; mat3[2][2] = 1; mat3[2][3] = 1;
  mat3[3][0] = 1; mat3[3][1] = 1; mat3[3][2] = 1; mat3[3][3] = 2;

  Matrix<float>trans;
  Matrix<float>red;

  cout << mat1 << endl;
  trans = mat1.transpose();
  cout << trans << endl;
  red = mat1.reduce(0,0);
  cout << red << endl;

  cout << mat2 << endl;
  trans = mat2.transpose();
  cout << trans << endl;

  red = mat2.reduce(0,0);
  cout << red << endl;

  red = mat2.reduce(0,1);
  cout << red << endl;

  red = mat2.reduce(1,1);
  cout << red << endl;

  red = mat2.reduce(1,1) * 2;
  cout << red << endl;

  red = mat2.reduce(1,1) / 2;
  cout << red << endl;

  red = mat2.reduce(1,1) + 2;
  cout << red << endl;

  red = mat2.reduce(1,1) - 2;
  cout << red << endl;

  cout << red.det() << endl;

  cout << mat2 << endl;
  cout << mat2.det() << endl;
  cout << mat2.cofactor(0,0) << endl;
  cout << mat2.cofactor(0,1) << endl;
  cout << mat2.cofactor(0,2) << endl;

  red = mat2.adj();
  cout << red << endl;

  cout << "Inverse = " << endl;

  sza::util::Matrix<float> inv;

  inv = mat2.inv();
  cout << inv << endl;
  red = mat2 * inv;
  cout << red << endl;

  inv = mat3.inv();
  cout << inv << endl;
  red = mat3 * inv;
  cout << red << endl;

  sza::util::Matrix<float> mat4(2,2), inv, red;

  mat4[0][0] = 0;   mat4[0][1] = 0;
  mat4[1][0] = 0;   mat4[1][1] = 1;

  inv = mat4.inv();
  cout << inv << endl;

  red = mat4 * inv;
  cout << red << endl;
*/

  Matrix<float> mat4(2,2), cop;

  mat4[0][0] = 0;   mat4[0][1] = 0;
  mat4[1][0] = 0;   mat4[1][1] = 1;

  cout << mat4 << endl;
  cop = mat4;
  cout << cop << endl;

  return 0;
}
