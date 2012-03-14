// Model.h: interface for the Model class.
//
//////////////////////////////////////////////////////////////////////
#include <map>
#include <cstring>
#include <string>
#include <cstdio>
#include <cstdlib>
#include <fstream>
#include <iostream>

using namespace std;

class Model  
{
public:
	Model();
	string& GetErrorMessage(){return mstrErrorMessage;};
	bool Load(string FileName);

public:
	string DATAFILE;
	string RESPONSEFILE;
	string WEIGHTSFILE;
	string OUTFILE;
	string ITEROUT;
	string SUMMARYFILE;
	string CENSORFILE;
	string NULLFILE;
	string mstrErrorMessage;
	int NOBSERVATIONS;
	int NVARIABLES;
	int NOISY;
	int NORMSTAND;
	int DSTART;
	int ONEVAR;
	
	int modtype;
	int iters;
	int NBest;
	int kmax;

	double innerAnneal1;
	double innerAnneal2;
	double innerAnneal3;
	double outerAnneal;
	double penalty;
	static double ToDouble(string str);
	static int ToInt(string str);
	static string ToLower(string str);
	static string ToString(int value);
	static string ToString(double value);
	static string trim(string s);

};

