/**
 * @author Dave Mehringer
 *
 * @version 1.0
 * @usage columnTest
 * @description
 * tests Column classes
 *
 */

#include "carma/util/Program.h"
#include "carma/dbms/IntegerColumn.h"
#include "carma/dbms/ShortColumn.h"
#include "carma/dbms/StringColumn.h"
#include "carma/dbms/Table.h"

#include <iostream>

using namespace std;
using namespace carma::dbms;
using namespace carma::util;

int Program::main() {
    IntegerColumn ic("joe");
    cout << ic.getName() << endl;
    ic.push_back(10);
    ic.push_back(20);
    ic.push_back(45);
    ic.push_back(-1200);

    cout << ic.size() << endl;
    cout << ic.getColumnType() << endl;

    StringColumn sc("a string column");
    cout << sc.getName() << endl;
    sc.push_back("ginger");
    sc.push_back("buttons");
    sc.push_back("oreo");
    sc.push_back("candy");
    //m->push_back("co co");

    cout << sc.size() << endl;
    cout << sc.getColumnType() << endl;

    FloatColumn fc("float col");
    fc.push_back(8.5);
    fc.push_back(80.2);
    fc.push_back(3.14159);
    fc.push_back(-20.991);

    Table t("test table");
    cout << t.getName() << endl;
    t.addColumn(ic);
    cout << t.columnCount() << " " << t.rowCount() << endl;
    t.addColumn(sc);
    cout << t.columnCount() << " " << t.rowCount() << endl;
    t.addColumn(fc);
    cout << t.columnCount() << " " << t.rowCount() << endl;

    vector<int> colTypes = t.getColumnTypes();
    for(int i = 0; i < t.columnCount(); i++) {
        cout << colTypes[i] << " ";
    }
    cout << endl;

    FloatColumn m = t.getFloatColumn(2);
    cout << fc << endl;
        

    return 0;
}

