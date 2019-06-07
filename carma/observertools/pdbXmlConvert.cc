/**
 * @version $Revision: 1.1 $
 * @usage @autogen
 *
 * @description
 *  Project Database XML Conversion Utility. This will allow you to test-convert
 *  the PDB XML before you actually try it with the PDB addProject() command.
 *
 *  The output is appended to any existing files in the directory. It DOES NOT
 *  truncate a file before adding to it. This allows it to be used on truly
 *  massive numbers of files in conjunction with the xargs program.
 *
 *  To run this program, do something like:
 *  pdbXmlConvert outputDirectory=/path -- file1.xml file2.xml file3.xml ...
 *
 *  The '--' is extremely important! Don't forget about it!
 *
 * @key outputDirectory "" string
 *  The path to the output directory to use. If not specified, output goes
 *  to stdout.
 *
 * @key pretty false bool
 *  Output the JSON in pretty format if true.
 *
 * @key verbose true bool
 *  Output information to stderr about each file as it is processed.
 *
 * @logger DEFAULT_FACILITY carma.observertools.pdbXmlConvert
 */

#include <carma/observertools/PDB_XML_Convert.h>
#include <carma/observertools/PDB_Util.h>

#include <carma/util/Program.h>

#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>

#include <iostream>
#include <fstream>
#include <string>
#include <vector>

using namespace carma::observertools;

static void outputJSON(
		const std::string &directory,
		const std::string &filename,
		const std::vector<std::string> &vec)
{
	// output to stdout
	if (directory.empty()) {
		BOOST_FOREACH(const std::string &json, vec) {
			std::cout << json << std::endl;
		}

		return;
	}

	// output to the filesystem
	boost::filesystem::create_directories(directory);
	const std::string dirandfilename = directory + "/" + filename;
	std::ofstream fout(dirandfilename.c_str(), std::ios::out | std::ios::app);

	BOOST_FOREACH(const std::string &json, vec) {
		fout << json << std::endl;
	}
}

int carma::util::Program::main()
{
	const bool pretty = getBoolParameter("pretty");
	const bool verbose = getBoolParameter("verbose");
	const std::string outputDirectory = getStringParameter("outputDirectory");

	// collect the filenames into a vector of strings
	std::vector<std::string> filenames;

	// the "1" skips the program name (argv[0])
	for (int i = 1; i < getExtraArgc(); i++) {
		const std::string filename = getExtraArgv()[i];
		filenames.push_back(filename);
	}

	if (filenames.empty()) {
		std::cerr << "ERROR: no filenames specified, please add some" << std::endl;
		return EXIT_FAILURE;
	}

	// run the conversion for each file
	PDB_JSON_Results results;
	std::vector<std::string> failures;

	BOOST_FOREACH(const std::string &filename, filenames) {
		try {
			if (verbose) {
				std::cerr << "PROCESSING: " << filename << std::endl;
			}

			const std::string xmlContents = readFile(filename);
			convertXmlToJson(xmlContents, pretty, results);

		} catch (const std::exception &ex) {
			std::cerr << "ERROR: caught exception while processing: " << filename << std::endl;
			std::cerr << ex.what() << std::endl;
			failures.push_back(filename);
		} catch (...) {
			std::cerr << "ERROR: caught unknown exception while processing: " << filename << std::endl;
			failures.push_back(filename);
		}
	}

	// output the results
	outputJSON(outputDirectory, "projects.json", results.projects);
	outputJSON(outputDirectory, "obsblocks.json", results.obsblocks);
	outputJSON(outputDirectory, "subobsblocks.json", results.subobsblocks);
	outputJSON(outputDirectory, "trials.json", results.trials);
	outputJSON(outputDirectory, "scripts.json", results.scripts);

	// output the failures (if any) to stderr
	BOOST_FOREACH(const std::string &filename, failures) {
		std::cerr << "FAILURE: " << filename << std::endl;
	}

	return (failures.empty() ? EXIT_SUCCESS : EXIT_FAILURE);
}

/* vim: set ts=4 sts=4 sw=4 noet: */
