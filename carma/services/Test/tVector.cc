#include <iostream>
#include <vector>

#include "carma/util/Program.h"
#include "carma/services/Vector.h"

using namespace ::std;
using namespace carma::util;
using namespace carma::services;

//
// @version $Revision: 1.5 $
//
// @usage      Testing Vector
//
// @description
//     This program tests out the Matrix and Vector class
//
// @key vec3    f    b   Initialize vec3?
//
// @logger DEFAULT_FACILITY carma.services.Test.tVector

int Program::main()
{

  try {
  Vector<float> vec1(2), vec2, vec3(3);
  bool Qvec3 = getBoolParameter("vec3");

  vec1[0] = 1;
  vec1[1] = 2;

  if (Qvec3) {
   try {
    vec2 = vec1 + vec3;   // this should fail since they don't match size
   } catch (const carma::util::ErrorException& ee) {
       cout << "Correctly caught exception: " << ee.getMessage() << endl;
   }
    // vec3 = vec1 + vec2 + vec2;
    // vec3 = vec1 + 2*vec2;
  }

  vec2 = vec1;

  float dotproduct = vec1*vec2;
  cout << "dot product = " << dotproduct << endl;
  cout << "cos(theta) = " << dotproduct/(vec1.magnitude()*vec2.magnitude()) 
       << " [should be 1]" <<endl;
  Vector<float> avec(3);
  Vector<float> bvec(3);
  avec[0] = 1.0; avec[1] = 2.0; avec[2] = 3.0;
  bvec[0] = 2.0; bvec[1] = 3.0; bvec[2] = 4.0;


  // vector crossed into itself should be zero
  Vector<float> crossproduct = avec.cross(avec);
  cout << "should be zero-length vector: " << crossproduct << endl;
  crossproduct = avec.cross(bvec);
  // ( 2*4-3*3 ,  3*2-1*4, 1*3-2*2 ) = (-1, 2, -1 )
  cout << "crossproduct : " << crossproduct 
      << "[should be (-1,2,-1) ]" <<endl;

  // cross product non-commutative; sign is reversed.
  crossproduct = bvec.cross(avec);
  cout << "should be negative of previous vector: " << crossproduct << endl;

  } catch (const carma::util::ErrorException& err) {
      cerr << "Caught exception: " << err.getMessage() << endl;
      return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;

}
