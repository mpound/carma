/**
 * @file meanmed.h
 *
 * A class to calculate the mean, median, sum, min, max, and sigma of a vector
 *  of floats
 *
 * @author Douglas N Friedel
 */

#ifndef CARMA_OBSERVERTOOLS_MEANMED_H
#define CARMA_OBSERVERTOOLS_MEANMED_H

#include <vector>
#include <string>
#include <fstream>

namespace carma{
    namespace observertools{
	class Meanmed
	    {
	    public:
		/** Constructor.
		 * Just creates the object
		 */
		Meanmed();

		/** Destructor
		 */
		~Meanmed();

		/** Read in the float vector from the input file
		 * @param string containing the file name
		 */
		void readFile(const std::string &fileName);

		/** Do the actual work
		 */
		void calculate();

		/** check that the number is valid
		 */
		bool validate(std::string &value);

	    private:
// adapted from numerical recipes
		void siftDown(int i, int n);
// adapted from numerical recipes
		void heapSort();
		std::vector<float> data_;
		float median_,sum_,min_,max_,sumsq_,mean_,sigma_;
	    };
    }; // namespace observertools
}; // namespace carma

#endif //CARMA_OBSERVERTOOLS_MEANMED_H
