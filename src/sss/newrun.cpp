// newran.cpp -----------------------------------------------------------
//#include "stdafx.h"
#include "newrun.h"
#include "newmat/NEWMAT.H"

double Random::seed;
double Random::Buffer[128];

double Random::Raw()                           // get new uniform random number
{
   long iseed = (long)seed;
   long hi = iseed / 127773L;                 // integer division
   long lo = iseed - hi * 127773L;            // modulo
   iseed = 16807 * lo - 2836 * hi;
   if (iseed <= 0) iseed += 2147483647L;
   seed = (double)iseed; return seed*4.656612875e-10;
}

double Random::Next()                          // get new mixed random number
{
	if (!seed) {
      //cout << "Random number generator not initialised";
	}
   int i = (int)(Raw()*128);               // 0 <= i < 128
   double f = Buffer[i]; Buffer[i] = Raw();  // Microsoft release gets this wrong
   return f;
}

double Random::Get()                  // get random number seed
{ return seed/2147483648L; }

void Random::Set(double s)            // set random number seed
                                      // s must be between 0 and 1
{  
   seed = (long)(s*2147483648L);
   for (int i = 0; i<128; i++) Buffer[i] = Raw();
}

char* Random::Name()            { return "Random";  }
