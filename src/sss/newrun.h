class Random                              // uniform random number generator
{
   static double seed;                    // seed
   static double Buffer[128];               // for mixing random numbers
   static double Raw();                     // unmixed random numbers
   void operator=(const Random&) {}       // private so can't access

public:
   static void Set(double s);             // set seed (0 < seed < 1)
   static double Get();                   // get seed
   virtual double Next();                   // get new value
   virtual char* Name();                  // identification
   Random() {}                            // do nothing
   virtual ~Random() {}                   // make destructors virtual
   virtual double Mean() const { return 0.5; }  // mean of distribution
   virtual double Variance() const { return 1.0/12.0; }	  // variance of distribution
};




