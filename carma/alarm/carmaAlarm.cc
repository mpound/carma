/*
 * $Id: carmaAlarm.cc,v 1.19 2011/08/30 23:09:27 iws Exp $
 *
 * @usage carmaAlarm
 * @key sounds "alarm/sounds.tab" s Location of table describing sound files
 * @key mode   @noDefault s The mode this program should run in.
 *                          Valid modes are:\n
 *        \t control - Handle DO and audio device on alarm.carma.pvt\n
 *        \t testDO  - Attempt to activate alarm through DO call\n
 *        \t testdev - Attempt to activate alarm directly through device\n
 * @key device "/dev/audio" s Default audio device file to write to.
 *                            Only used when mode=device or mode=testdev
 * @key emulate false b Emulation mode.
 * @logger DEFAULT_FACILITY carma.alarm.Alarm
 *
 */

#include <iostream>
#include <errno.h>
using namespace std;

#include <log4cpp/Category.hh>
using namespace log4cpp;

#include <carma/corba/corba.h>
#include <carma/corba/Client.h>
#include <carma/corba/Server.h>

#include <carma/util/Program.h>
#include <carma/util/loggingUtils.h>
#include <carma/util/programLogging.h>
#include <carma/util/ErrorException.h>
#include <carma/util/ExceptionUtils.h>
using namespace carma::util;

#include <carma/alarm/Trace.h>
#include <carma/alarm/AudioControlThread.h>
#include <carma/alarm/AlarmControl.h>
#include <carma/alarm/AlarmControl_skel.h>
#include <carma/alarm/AlarmControl_skel_tie.h>
#include <carma/alarm/AlarmControlImpl.h>
using namespace carma::alarm;

#include <carma/monitor/MonitorSystem.h>
#include <carma/monitor/AlarmSubsystem.h>
using namespace carma::monitor;

int Program::main()
{
	CPTRACE1("Program::main() - carmaAlarm entry");

	try {

		if (parameterWasSpecified("mode") == false)
			throw CARMA_ERROR("mode parameter not specified");

		const std::string mode = getStringParameter("mode");
		const bool emulate = getBoolParameter("emulate");
		const std::string soundsTab = getConfFile(getStringParameter("sounds"));
		const std::string device = getStringParameter("device");

		/* change the logger name based on the current mode */
		if (mode.empty() == false) {
			std::string s = getLogname() + "." + mode;
			setInstanceLogname(s);
		}

		if (mode == "testdev") {
			/*
			 * Launch into testdev mode
			 *
			 * Test the audio device by using the aplay program
			 */
			std::ostringstream oss;
			oss << "aplay " << Program::getConfDir() << "/" << "alarm/pcm/alarm1.wav";

			int ret = system(oss.str().c_str());
			if (ret != 0) {
				programLogErrorIfPossible("Audio test failed. Command: " + oss.str());
				return EXIT_FAILURE;
			}

		} else if (mode == "testDO") {

			CPTRACE2("Instantiating AlarmControl");
			carma::corba::Client ns(getExtraArgc(), getExtraArgv());
			AlarmControl_var alarm = ns.resolveName<AlarmControl>(ALARM_NAME);

			CPTRACE2("Turning on alarm");
			alarm->turnOn("alarm", "testing", "testing", true);
			CPTRACE2("sleeping 10s");
			sleep(10);
			CPTRACE2("Turning off alarm");
			alarm->turnOff();

		} else if (mode == "control") {
			/*
			 * Launch into control mode
			 *
			 * This is all of the CORBA related stuff. We start the alarm DO,
			 * which controls the sound device.
			 */
			CPTRACE1("Launching control DO thread");

			AudioControlThread2 control(device, emulate, soundsTab);
			AlarmControlImpl impl(control);
			CPTRACE2("Tie servant was successfully created");

			carma::corba::Server &server = carma::util::Program::getCorbaServer();
			server.addServant<POA_carma::alarm::AlarmControl_tie>(impl, ALARM_NAME);
			CPTRACE2("Tie servant was successfully published");

			// Block on orb forever
			CPTRACE2("START: blocking in server.run()");
			server.run(false);
			CPTRACE2("END: blocking in server.run()");
		} else {
			throw CARMA_ERROR("Unrecognized mode: see output of --keywords for modes");
		}

	} catch (...) {
		std::ostringstream oss;

		oss << "Caught exception in carmaAlarm Program::main: "
			<< getStringForCaught() << "\n"
			<< getCaughtBacktraceAsString();

		CPTRACE1(oss.str());
		std::cerr << oss.str() << std::endl;
		logMultipleLines(Program::getLogger(), log4cpp::Priority::ERROR, oss.str());
		return EXIT_FAILURE;
	}

	return EXIT_SUCCESS;
}

/* vim: set ts=4 sts=4 sw=4 noet: */
