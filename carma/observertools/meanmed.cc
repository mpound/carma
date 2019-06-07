/**
 *
 * @usage meanmed infile=<input log file>
 *
 * @description
 * This program reads in a text log file and calculates the median, mean
 *  rms, min and max of the data. These values are written out to a text file.
 *  This program is only used by the data quality analysis script and has been
 *  converted from meanmed.for from Ted Yu. Any line preceeded with a # will be
 *  ignored and any value <= 0 is ignored.
 *
 * @key infile "" string the name of the log file to read
 *
 * @logger DEFAULT_FACILITY carma.observertools.meanmed
 *
 * @author Douglas Friedel
 */

// Carma includes
#include "carma/observertools/meanmed.h"
#include "carma/util/Program.h"
#include "carma/util/programLogging.h"
#include "carma/util/KeyValueConfigFile.h"
#include "carma/util/ErrorException.h"
#include "carma/util/Trace.h"

// other includes
#include <string>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <exception>
#include <cmath>

//namespaces
using namespace std;
using namespace carma::observertools;
using namespace carma::util;

//Main
int carma::util::Program::main(){
    try{
// get the input parameters
	const std::string infile = getStringParameter("infile");
	if(infile == ""){
	    std::string errString = "An input file must be given with infile=";
	    throw errString;
	}
// create the meanmed object
	Meanmed meanmed;
// read in the data
	meanmed.readFile(infile);
// do thw work
	meanmed.calculate();
        return EXIT_SUCCESS;
    } catch(const string & errString){
        std::ostringstream errStream;
        errStream << "carma::observertools::meanmed - " << errString << endl;
        cerr << "carma::observertools::meanmed - " << errString << endl;
        programLogCriticalIfPossible( errStream.str() );
        CARMA_CPTRACE(carma::util::Trace::TRACE3,errString.c_str());
        return EXIT_FAILURE;
    }
    catch (const std::exception & exc) { std::ostringstream errStream;
        errStream << "carma::observertools::meanmed - " 
              << "std exception caught " << exc.what() << endl;
        cerr << "carma::observertools::meanmed - " 
             << "std exception caught " << exc.what() << endl;
        programLogCriticalIfPossible( errStream.str() );
        CARMA_CPTRACE(carma::util::Trace::TRACE3,
                  "std exception" << exc.what());
        return EXIT_FAILURE;

    } catch (...) {
        std::ostringstream errStream;
        errStream << "carma::observertools::meanmed - "
              << "unknown exception caught" << endl;
        cerr << "carma::observertools::meanmed - "
             << "unknown exception caught" << endl;
        programLogCriticalIfPossible( errStream.str() );
        CARMA_CPTRACE(carma::util::Trace::TRACE3, "unknown exception");
        return EXIT_FAILURE;
    }
}


Meanmed::Meanmed(){
    
}

Meanmed::~Meanmed(){

}

void Meanmed::readFile(const std::string &fileName){
// open the input file
    ifstream inStream;
    std::string temp;
    inStream.open(fileName.c_str());

// if the file will not open for any reason
    if(inStream.fail()){
	std::string errString =
	    "Unable to open file: " + fileName + " (meanmed)";
	throw errString;
    }

// read in the data
    while(inStream >> temp){
	if(validate(temp)){
	    data_.push_back(atof(temp.c_str()));
	}
    }
// close the input file
    inStream.close();
}

// method to make sure that the string contains a valid number
bool Meanmed::validate(std::string &value){
    std::string::size_type start,end;
    start = value.find_first_not_of(" ");
    end = value.find_last_not_of(" ");
// remove leading and trailing spaces
    if(start != std::string::npos && end != std::string::npos){
	value = value.substr(start,end-start+1);
    }
    else{
	return false;
    }
// make sure that the string only contains numbers
// decimals and exponentials are allowed,
// including negative numbers for the exponential
    for(std::string::size_type i = 0; i < value.length(); i++){
	if(!isdigit(value.at(i)) && value.at(i) != '.'
	   && value.at(i) != 'e' && value.at(i) != 'E' && value.at(i) != '-'){
	    return false;
	}
    }
// make sure it is a valid number
// value will always be positive and non-zero
    float number = atof(value.c_str());
    if(number <= 0.0)
	return false;

    return true;
}

void Meanmed::calculate(){
// sort the data
    heapSort();
// get the median value
    if(data_.empty()){
	std::string errString = "The input file contained no valid data.";
	throw errString;
    }
    if((data_.size() % 2) == 1){
	median_ = data_[((data_.size() + 1)/2) - 1];
    }
    else{
	median_ = (data_[data_.size()/2 - 1] + data_[(data_.size() / 2)])/2.0;
    }
// get the sum, min and max values
    sum_ = 0.0;
    min_ = data_[0];
    max_ = data_[data_.size() - 1];
    for(unsigned int j = 0; j < data_.size(); j++){
	sum_ += data_[j];
	sumsq_ += data_[j]*data_[j];
    }
// calculate the mean
    mean_ = sum_/data_.size();
    float tmp = sumsq_/data_.size() - mean_*mean_;
// calculate the rms (sigma)
    sigma_ = sqrt((data_.size() * tmp)/(data_.size() - 1));
// write out the results
    cout << setw(6) << "Npts" << setw(13) << "Median" << setw(12) << "Mean"
	 << setw(12) << "Rms" << setw(13) << "Min" << setw(13) << "Max"
	 << endl;
    cout << setw(6) << data_.size();
    cout.setf(ios::fixed);
    cout.setf(ios::showpoint);
    cout.precision(3);
    cout << setw(13) << median_ << setw(13) << mean_ << setw(13) << sigma_
	 << setw(13) << min_ << setw(13) << max_ << endl;
}

// method to move an item to another point in the list and shift all other
// items accordingly to make room
void Meanmed::siftDown(int i, int n) {
    int j;
    float tmp;
    while (2*i <= n) {
        j = 2*i;
        if (j<n)
            if (data_[j] < data_[j+1])
		j++;
        if (data_[i] < data_[j]) {
            tmp=data_[i];
	    data_[i]=data_[j];
	    data_[j]=tmp;
            i=j;
        }       
        else {
            i=n;
            i++;
        }    
    }        
}

// main sorting function
void Meanmed::heapSort() {
    float tmp;
    int n = data_.size()-1;
    for (int i=n/2; i>=0; i--)
	siftDown(i, n);
    for (int i=n; i>=1;  i--) {
        tmp=data_[i];
	data_[i]=data_[0];
	data_[0]=tmp;
        siftDown(0, i-1);
    }
}
