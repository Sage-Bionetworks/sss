// Model.cpp: implementation of the Model class.
//
//////////////////////////////////////////////////////////////////////
#pragma warning(disable:4786)
#include "Model.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

Model::Model()
{
	DATAFILE = "";
	RESPONSEFILE = "";
	WEIGHTSFILE = "";
	OUTFILE = "";
	ITEROUT = "";
	SUMMARYFILE = "";
	CENSORFILE = "";
	NULLFILE = "nullfile.txt";
	mstrErrorMessage = "";
	NOBSERVATIONS = 0;
	NVARIABLES = 0;
	NOISY = 1;
	NORMSTAND = 1;
	DSTART = 2;
	ONEVAR = 1;
	
	modtype = 2;	//binreg
	iters = 10000;
	NBest = 10000;
	kmax = 20;

	innerAnneal1 = 0.6;
	innerAnneal2 = 1.0;
	innerAnneal3 = 0.8;
	outerAnneal = 0.4;
	penalty = 4;
}

string Model::ToLower(string str)
{
	//new implementation for GNU
	char *newstr = strdup(str.c_str());
	int i = 0;
	while (newstr[i] != '\0') {
		newstr[i] = tolower(newstr[i]);
		i++;
	}
	return newstr;
	//return strlwr(strdup(str.c_str())); 
}

int Model::ToInt(string str)
{
	return atoi(str.c_str());
}

double Model::ToDouble(string str)
{
	return atof(str.c_str());
}

string Model::ToString(int value) {
	char  buffer[10]; 
	sprintf(buffer, "%d", value);
	string result(buffer);
	return result;
	cout << result.c_str() << endl;
	
}

string Model::ToString(double value) {
	char  buffer[10]; 
	sprintf(buffer, "%4.1f", value);
	string result(buffer);
	return result;
	
}

string Model::trim(string s)
{
    if (s.empty()) return s;
	string ret;
	for (int i = 0; i < s.length();  i++) {
		if (!isspace(s.at(i)) && !iscntrl(s.at(i)))
			ret.append(s.substr(i,1));
	}
	return ret;
}


bool Model::Load(string FileName){
	int BufferSize = 4096;
	char* theLine = new char[BufferSize];
	ifstream theFile(FileName.c_str());
	if (theFile.fail()) {
		mstrErrorMessage = "Failed to open the description file!";
		return false;
	}

	int nLineCount = 0;
	while (!theFile.eof()) {
		theFile.getline(theLine, BufferSize);
		nLineCount++;
		string theline(theLine);
		string Name(""), Value("");
		theline = trim(theline);
		if (theline.length() && (theline.c_str()[0] != '#'))
		{
			int pos = 0;
			if ((pos = theline.find("=")) != -1) {
				Name = theline.substr(0, pos);
				Value = theline.substr(pos + 1);
			}
			if (Name == "" && Value == "") {
			} else if (Name == "" || Value == "") {
				mstrErrorMessage = "Invalid <Name = Value> pair format";
				cout << theLine << endl;
				return false;
			} else {
				string name = ToLower(Name);
				string value = ToLower(Value);
				if (name == "datafile") {
					DATAFILE = Value;
				} else if (name == "responsefile") {
					RESPONSEFILE = Value;
				} else if (name == "nullfile") {
					NULLFILE = Value;
				}  else if (name == "weightsfile") {
					WEIGHTSFILE = Value;
				} else if (name == "outfile") {
					OUTFILE = Value;
				} else if (name == "iterout") {
					ITEROUT = Value;
				} else if (name == "summaryfile") {
					SUMMARYFILE = Value;
				} else if (name == "censorfile") {
					CENSORFILE = Value;
				} else if (name == "nobservations") {
					NOBSERVATIONS = ToInt(value);
				} else if (name == "nvariables") {
					NVARIABLES = ToInt(value);
				} else if (name == "noisy" || name == "debugout") {
					NOISY = ToInt(value);
				}else if (name == "normstand") {
					NORMSTAND = ToInt(value);
				} else if (name == "dstart" || name == "pstart") {
					DSTART = ToInt(value);
				} else if (name == "onevar") {
					ONEVAR = ToInt(value);
				} else if (name == "modtype") {
					modtype = ToInt(value);
				} else if (name == "iters") {
					iters = ToInt(value);
				} else if (name == "nbest") {
					NBest = ToInt(value);
				} else if (name == "kmax" || name == "pmax") {
					kmax = ToInt(value);
				} else if (name == "inneranneal1") {
					innerAnneal1 = ToDouble(value);
				} else if (name == "inneranneal2") {
					innerAnneal2 = ToDouble(value);
				} else if (name == "inneranneal3") {
					innerAnneal3 = ToDouble(value);
				} else if (name == "outeranneal") {
					outerAnneal = ToDouble(value);
				} else if (name == "penalty" || name == "priormeanp") {
					penalty = ToDouble(value);
				} else {
					mstrErrorMessage = "Unknown <Name = Value> pair!"; //to be refined later
					cout << theLine << endl;
					return false;
				}
				//cout << theLine << endl;	
			}
		}
	}
	delete[] theLine;
	mstrErrorMessage = "";
	return true;
}
