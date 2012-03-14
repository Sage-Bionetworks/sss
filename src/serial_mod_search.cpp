// Implements a shotgun stochastic search for regression type models.
// Possible models currently include normal linear models (using either
// proper priors or a BIC approximation, and binary response models using 
// a logistic link function (where the marginal likelihood is computed
// using a laplace approximation). A model selection prior is included.
// The search evaluates every possible neighboring model at each iteration
// and is able to jump between models of neighboring dimensions.
//
// Extensions (will) include survival analysis with censored outcomes, using
// a laplace approximation to compute the marginal likelihood.
//
// Note that all possible one variable models are computed (if so 
// instructed). After this, we do not allow moves to one variable models.


/**************
 * Header info
 * ************/
#pragma warning(disable:4786)
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <float.h>
#include <math.h>
#include <iomanip>
#include <ctime>


#include "Model.h"
#include "marglik.h"
#include "newrun.h"
using namespace std;
/************************************
 *     Data Specific Parameters     *
 ************************************/
#define EPSILON 0.001


// The type of models to consider
// The actual model used is defined under ``global variables''
#define LINREG 1                // normal linear models
#define BINREG 2                // binary regression models
#define SURVMOD 3               // survival models

#define REPLACE 1
#define DELETE 2
#define ADDVAR 3
#define UNIVAR 4
#define UPDATE_MOD 5
#define DIETAG 10

/**************************************************
 *              Global Variables                  *
 **************************************************/
const double randomseed = 0.5;
int             NOISY;    // should alg spew output?
int             NORMSTAND;  // should we standardize data??
int             DSTART;    // what size model to start with?
int             ONEVAR;    // should we look at one variable models
 

int		modtype; // which model?
int		Iters;	// how long to run the algorithm
int		NBest;	// how many models to store
int		kmax;	// largest regression we'll look at
double		innerAnneal1; // annealing parameter for replacement
double		innerAnneal2; // annealing parameter for deletion
double		innerAnneal3; // annealing parameter for addition
double		outerAnneal; // annealing parameter for d-jump

Model           model;


int		wnobs = 0;	// the number of observations we use
int		maxdim = 0;	// maximum dimension we've recorded

double*		Y = NULL;	// the response vector
double*		W = NULL;	// weights file
double**	data = NULL;	// the full X matrix
double*		logY = NULL;
double*		Nu	= NULL;

int*		Regressors = NULL; // the current regression model

int		curdim; // current dimension of model

int		maxSampleSize;
				// bigger than we need; can be reduced
				// but it's a bound anyway...

int		maxJumpSize=3;	// only consider three dimensions at
				// each iteration

int		WorstBestReg=0;	// keep track of where worst reg. is

double		ybar=0, yy=0, syy; // used for linear reg. search
double myPI = 3.141592653589793;

int		nobservations;
int		nvariables;

Random unifRandom; // random generation stuff

/***********************
 *      Prior info     *
 ***********************/

// Model selection penalty term
double		penalty;
//double		penalty = 1.0/5.0;

// Prior for binary regressors
double		prvar = 1.0;	// prior variance for normal dist.

// Prior for linear regression
int		BIC=0;
double		tauPrior = 1.0;		// Wishart prior scale matrix
double		deltaPrior = 3.0;	// Wishart prior df

/************************
 *    End Prior Info    *
 ************************/
#include "node.cpp"
#include "calc.cpp"

//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////

void
slave(double *work, double *workresults, int tag)
{
	int		location, candidate;
	int		j, tmpdim;	// tmpdim is for univar case

	// Compute som constants for BIC based regression
	if(BIC==1) {
		for(j=0; j<nobservations; j++)     {
			ybar += Y[j]*W[j];
			yy += Y[j]*Y[j]*W[j];
		}
		ybar = ybar/(double)wnobs;
		syy = yy - (double)wnobs*ybar*ybar;
	}
    
    location = (int)work[0];
    candidate = (int)work[1];

	switch(tag) {
        case REPLACE:
			workresults[0] = (double)location;
			workresults[1] = (double)candidate;
			workresults[2] = GetScore(location, candidate);
			if(BIC==1 && modtype==LINREG)   {
				// edit workresults here
			} else  {
				workresults[2] += curdim*(log(penalty) - log(1-penalty));
			}         
			break;

		case DELETE:
			workresults[0] = (double)location;
			workresults[1] = (double)candidate;
			workresults[2] = GetScore(location, -1);

			if(BIC==1 && modtype==LINREG)  {
				// edit workresults here
			} else  {
				workresults[2] += (curdim-1)*(log(penalty) - log(1-penalty)); 
			}
            break;

		case ADDVAR:
			workresults[0] = (double)location;
			workresults[1] = (double)candidate;
			workresults[2] = GetScore(-1, candidate);

			if(BIC==1 && modtype==LINREG) {
				// edit workresults here
			} else {
				workresults[2] += (curdim+1)*(log(penalty) - log(1-penalty));
			}
            break;

		case UPDATE_MOD:
            UpdateMasterModel(location, candidate);      
			break;

		case UNIVAR:
			tmpdim = curdim;
            curdim = 1;

			workresults[0] = (double)location;
			workresults[1] = (double)candidate;
			workresults[2] = GetScore(location, candidate);

            if(BIC==1 && modtype==LINREG) {
				// edit workresults here
			} else {
				workresults[2] += curdim*(log(penalty) - log(1-penalty));
			}

			// Reset the current dimension!!
			curdim = tmpdim;
	    break;
	} // switch
}

void
master()
{
   int		i,j,k;
   int		location;	// looping variable for swapping
   int		decide; 	// the sampled model
   int		ret_loc;	// location on which we were working
   int		ret_cand;	// variable on which we were working
   int		tmpdim;		// used while computing 1-d models

   double	work[3];	// used to send out work requests
   double	workresults[3];	// used to receive work requests
   double	score;		// used to evaluate a model

   double	nullmod;	// marginal likelihood for the null model
   double	*nullpoint, *nullvar; // MAP for intercept and associated 
   				      // variance estimated under null model

   // Used for keeping tack of regressors
   multimap<double, LPNode> Root;
   multimap<double, LPNode> JumpRoot; 

   // Used for sampling
   int		nmax;		// size of tree?
   double*	weights = new double[maxSampleSize];
   double*	cumw;
   int*		candidates = new int[maxSampleSize];
   int*		locations = new int[maxSampleSize];
   double*	scores = new double[maxSampleSize];

   // Storage for best models
   multimap<double, LPRegList> bestRegs;

   FILE*	iterout;
   FILE*	fout;
   FILE*	nullout;

   /////////////////////////////////////////
   LPRegList newreg = new  RegList(model.NOBSERVATIONS - 1);
   newreg->dim = -2;
   newreg->score = -DBL_MAX + 1;
   newreg->iter = -2;
   bestRegs.insert(multimap<double, LPRegList>::value_type(-DBL_MAX + 1, newreg));
   //fprintf(stderr, "Master is awake!!\n\n");

   iterout = fopen(model.ITEROUT.c_str(), "w");
   if(iterout == NULL)
   {
      fprintf(stderr, "Bad iterations output file!!\n\n");
      return;
   }

   // Compute some constants for BIC based regression
   if(BIC==1)
   {
      for(j=0; j<nobservations; j++)
      {
         ybar += Y[j]*W[j];
	 yy += Y[j]*Y[j]*W[j];
      }
      ybar = ybar/wnobs;
      syy = syy - wnobs*ybar*ybar;
   }

   //////////////////////////////////////////////////////////////
   // Null model calculations
   //////////////////////////////////////////////////////////////
   // Need to compute the null model here
   // It should get printed out if requested REGARDLESS of whether it ends 
   // up being competitive. The MAP estimate and associated variance is 
   // computed as well

   nullout = fopen(model.NULLFILE.c_str(), "w");
   if(nullout == NULL)
   {
      fprintf(stderr, "Bad null model file!!\n\n");
      return;
   }
   if(modtype == LINREG) {
      // Assumes the data are scaled to have y'y = n-1 (and mean zero)
      nullmod = LinRegML(NULL, 0);

      fprintf(nullout, "0 %.7f\n", nullmod);
   }
   else if(modtype == BINREG)
   {
      nullpoint = new double[1]; nullvar = new double[1];
      nullmod = BinRegML(NULL, 0, nullpoint, nullvar);
      fprintf(nullout, "0 %.7f %.6f %.6f\n", nullmod, nullpoint[0], nullvar[0]);
   }
   else // assume it is weibull survival model
   {
      nullpoint = new double[2]; nullvar = new double[4];
      static double garbage = 1.0;
      nullmod = SurvModML(NULL, 0, garbage, nullpoint, nullvar);
      fprintf(nullout, "0 %.7f %.6f %.6f %.6f %.6f %.6f %.6f\n", nullmod, nullpoint[0], nullpoint[1], nullvar[0], nullvar[1], nullvar[2], nullvar[3]);
   }
   fclose(nullout);
	
   // Add the null model to the "best of" list
   LPRegList nullreg = new RegList(model.NOBSERVATIONS - 1);
   nullreg->dim = 0;
   nullreg->score = nullmod;
   nullreg->iter = 0;
   bestRegs.insert(multimap<double, LPRegList>::value_type(nullmod, nullreg));

   /////////////////////////////////////////////////////////////////

   /***********************************************/
   /*     Before starting, compute 1-d models     */
   /***********************************************/
   if(ONEVAR) {
      if(NOISY)
      {
         printf("Computing 1 variable models!\n\n");
         fflush(stdout);
      }

      // All this does is compute the 1-d models. It does NOT
      // sample from them. Once the algorithm starts, we will
      // not be allowed to visit them anymore. Note that the
      // starting model is not permanantly changed

      // **TEMPORARILY** switch to one dimension
      tmpdim = curdim;
      curdim = 1;

      for(j=0; j<nvariables; j++)
      {
         // Zero is the location here -- put the candidate in the
         // first slot
         work[0] = (double)0;
         work[1] = (double)j;
         work[2] = -1; // before iteration zero...
		 
		 slave(work, workresults, UNIVAR);
         

            // record results
	     ret_loc = (int) workresults[0];
	     ret_cand = (int) workresults[1];
	     score = workresults[2];
	     CheckAddModel(ret_loc, ret_cand, curdim, score, bestRegs, -1);

      } // loop over univariate models
   
      // reset the current dimension!!!
      curdim = tmpdim;
   }
   /*****************************************/
   /*        Done with 1-d models!!         */
   /*****************************************/

   if(NOISY)
   {
      printf("Starting the algorithm!!\n\n");
      fflush(stdout);
   }

   /*****************************/
   /*   Start the algorithm!!   */
   /*****************************/

   // A note on bookkeeping:
   // if candidate >= 0 and location >= 0, this means swapping
   // if candidate >= 0 and location == -1, this means adding
   // if candidate = -1 and location >= 0, this means deleting

   for(i=0; i<Iters && kmax>1; i++) {
      if(NOISY) {
			printf("%4d : r : ", i+1);
			fflush(stdout);
      }

      if(i==-1) {
			printf("\n");
			for(j=0; j<curdim; j++) printf("Reg[%d] = %d\n", j, Regressors[j]);
			printf("\n");
      }

      // Set up the tree for step 1
	  Root.clear();

      // Set up the Jumping tree
	  JumpRoot.clear();

      ///////////////////////////////////////////////////////
      //        First, compute all replacement models      //
      ///////////////////////////////////////////////////////
	  if (curdim < kmax) {
		  for(location = 0; location<curdim; location++) {
			  for(j=0; j<nvariables; j++) {
				// Check to see if we can replace the variable
				if( CanAdd(j) == 0 ) continue;

				work[0] = (double) location;
				work[1] = (double) j;
				work[2] = (double) i;
				slave(work, workresults, REPLACE);
			
				// record the results
				ret_loc = (int)workresults[0];
				ret_cand = (int)workresults[1];
				score = workresults[2];
			   
				CheckAddModel(ret_loc, ret_cand, curdim, score, bestRegs, i);
			   
				Insert(maxSampleSize, Root,score,ret_loc, ret_cand);	
			  } // loop over variables for this location
		  } // loop over current dimension
       
     
		  // Sample a model!!
		  // Transform the binary tree into a list of weights
		  InorderWalk(Root, weights, locations, candidates);
		  nmax = Root.size();
		  Delete(Root);
		  cumw = new double[nmax+1];

		  // normalize and set up the weights
		  NormalizeWeights(weights, cumw, scores, nmax, innerAnneal1);

		  // do the sampling
		  decide = WeightedSampling(nmax, cumw);

		  delete[] cumw; cumw = NULL;

		  //printf("\n\nscore = %f, loc = %d, cand = %d\n\n", scores[decide],locations[decide], candidates[decide]);

		  // Now add this to the Jumping Tree
		  Insert(maxJumpSize, JumpRoot,scores[decide], locations[decide], candidates[decide]);
      }

      ///////////////////////////////////////////////////////////
      //           Second, compute all deletion models         //
      ///////////////////////////////////////////////////////////

      if(curdim>2)
      {
         if(NOISY)
         {
            printf("- : ");
	    fflush(stdout);
	 }

	 for(location=0; location<curdim; location++) {
        work[0] = (double)location;
	    work[1] = -1.0;
	    work[2] = (double) i;
		slave(work, workresults, DELETE);
	    
	    ret_loc = (int)workresults[0];
	    ret_cand = (int)workresults[1];
	    score = workresults[2];

	    CheckAddModel(ret_loc, ret_cand, curdim-1, score, bestRegs, i);

		Insert(maxSampleSize, Root,score,ret_loc, ret_cand);

	 } // loop over variables to delete
	 
	 // Sample a model!!
	 // First transform the binary tree into a list of weights
	 InorderWalk(Root, weights, locations, candidates);
	 nmax = Root.size();
	 Delete(Root);

	 cumw = new double[nmax+1];

	 // normalize and set up the weights
	 NormalizeWeights(weights, cumw, scores, nmax, innerAnneal2);

	 // do the sampling
	 decide = WeightedSampling(nmax, cumw);

	 delete[] cumw; cumw = NULL;

	 //printf("\n\nscore = %f, loc = %d, cand = %d\n\n",scores[decide],locations[decide], candidates[decide]);

	 // Now add this to the Jumping Tree
	 Insert(maxJumpSize, JumpRoot,scores[decide], locations[decide], candidates[decide]);

      } // if we can delete
      else
      {
         // otherwise print output to keep fields lined up
	 if(NOISY)
	 {
	    printf("  : ");
	    fflush(stdout);
	 }
      }

	
      /////////////////////////////////////////////////////////
      //             Third, try adding a variable            //
      /////////////////////////////////////////////////////////
      // limit on size of model, kmax is the largest
      if( curdim < kmax )  {
			if(NOISY) {
				printf("+ : ");
				fflush(stdout);
			}

		 for(j=0; j<nvariables; j++)
		 {
			// See if we can add this variable
				if( CanAdd(j) == 0 ) continue;

			work[0] = -1.0;
			work[1] = (double) j;
			work[2] = (double) i;
			slave(work, workresults, ADDVAR);
			
			ret_loc = (int)workresults[0];
			ret_cand = (int)workresults[1];
			score = workresults[2];
			CheckAddModel(ret_loc, ret_cand, curdim+1, score, bestRegs, i);
			Insert(maxSampleSize, Root,score,ret_loc, ret_cand);
		 } // loop over variables to add

		 // Sample a model!!
		 // First transform the binary tree into a list of weights
		 InorderWalk(Root, weights, locations, candidates);
		 nmax = Root.size();
		 Delete(Root);
         
		 cumw = new double[nmax+1];

		 // normalize and set up the weights
		 NormalizeWeights(weights, cumw, scores, nmax, innerAnneal3);

		 // do the sampling
		 decide = WeightedSampling(nmax, cumw);

		 delete[] cumw; cumw = NULL;

		 //printf("\n\nscore = %f, loc = %d, cand = %d\n\n", scores[decide], locations[decide], candidates[decide]);

		 // Now add this to the Jumping Tree
		 Insert(maxJumpSize, JumpRoot,scores[decide], locations[decide], candidates[decide]);

      } // if we can add
       else
      {
         // print output to keep fields lined up
			if(NOISY) {
				printf("  : ");
			fflush(stdout);
		}
      }

      // Finally, sample the model to which we move!
      // Transform the binary tree into a list of weights
	  InorderWalk(JumpRoot, weights, locations, candidates);
	  nmax = JumpRoot.size();
	  Delete(JumpRoot);
      cumw = new double[nmax+1];

      // normalize and set up the weights
      NormalizeWeights(weights, cumw, scores, nmax, outerAnneal);

      // do the sampling
      decide = WeightedSampling(nmax, cumw);

      delete[] cumw; cumw = NULL;

      //printf("\n\nscore = %f, loc = %d, cand = %d\n\n", scores[decide], locations[decide], candidates[decide]);

      // Print some output
      if(NOISY)
      {
         if( (locations[decide] >= 0) && (candidates[decide] >= 0) )
         {
            printf("-%-4d : +%-4d : ", Regressors[locations[decide]]+1,candidates[decide]+1);
	    fflush(stdout);
         }
         else if( candidates[decide]<0 ) // we're moving down a dimension
         {
            printf("-%-4d :       : ", Regressors[locations[decide]]+1);
	    fflush(stdout);
         }
         else // we're moving up a dimension
         {
            printf("      : +%-4d : ", candidates[decide]+1);
	    fflush(stdout);
         }
      }

      // Update the current model
      UpdateMasterModel(locations[decide], candidates[decide]);

      if(NOISY) {
           printf("dim = %d : score = %-.4f\n", curdim, scores[decide]);
			fflush(stdout);
      }

      // Update the slave models!
	  /*
      work[0] = (double) locations[decide];
      work[1] = (double) candidates[decide];
      work[2] = (double) i;
	  slave(work, workresults, UPDATE_MOD);
	  */

      // Write the iteration results
      fprintf(iterout, "%d %.5f ", i+1, scores[decide]);
      for(j=0; j<curdim; j++) fprintf(iterout, "%d ", Regressors[j]+1);
      fprintf(iterout,"\n");
      fflush(iterout);

      //////////////////////////////////////////
      // Every so often, print out top models //
      //////////////////////////////////////////

	  //Jan 31, 2007
	  //we need to recalculare 'maxdim' here since it is possible that the model with maximum 
	  //dim is already gone
      if((i+1)%1000 == 0)
      {
         if(NOISY)
	 {
            printf("Saving results...");
	    fflush(stdout);
	 }
	 fout = fopen(model.OUTFILE.c_str(), "w");
	 // Need to do something here for BIC
	 maxdim = 0;
	 multimap<double,LPRegList>::iterator it;
	 for (it = bestRegs.begin(); it != bestRegs.end(); it++) {
	    LPRegList currentreg= (*it).second;
		if (currentreg->dim > maxdim) { maxdim = currentreg->dim; }
	 }

     for (it = bestRegs.begin(); it != bestRegs.end(); it++) {
	    LPRegList currentreg= (*it).second;
		if (currentreg->dim >=0) {
			fprintf(fout, "%-4d %d %.6f ", currentreg->iter,  currentreg->dim,  currentreg->score);
			for(k=0; k<(currentreg->dim); k++) {
				   fprintf(fout, "%-4d ", (currentreg->vars[k])+1);
			}
			while(k<maxdim)
			{
				   fprintf(fout, "NA    ");
				   k++;
			}
			fprintf(fout,"\n");
		}
	   
	 }
	 fflush(fout);
	 fclose(fout);
	 if(NOISY)
	 {
            printf("done!!\n");
	    fflush(stdout);
	 }
      } // save best models

   } // big iterations loop

   fflush(iterout);
   fclose(iterout);

   // Print output!!
   if(NOISY)
   {
      printf("\nSaving Results...");
      fflush(stdout);
   }

   fout = fopen(model.OUTFILE.c_str(), "w");

   maxdim = 0;
   multimap<double,LPRegList>::iterator it1;
   for (it1 = bestRegs.begin(); it1 != bestRegs.end(); it1++) {
		LPRegList currentreg= (*it1).second;
		if (currentreg->dim > maxdim) { maxdim = currentreg->dim; }
   }
   for (multimap<double, LPRegList>::reverse_iterator it = bestRegs.rbegin(); it != bestRegs.rend(); it++) {
	    LPRegList currentreg= (*it).second;
		if (currentreg->dim >=0) {
			fprintf(fout, "%-4d %d %.5f ", currentreg->iter,  currentreg->dim,  currentreg->score);
			for(k=0; k<(currentreg->dim); k++) {
				   fprintf(fout, "%-4d ", (currentreg->vars[k])+1);
			}
			while(k<maxdim)
			{
				   fprintf(fout, "NA    ");
				   k++;
			}
			fprintf(fout,"\n");
		}	   
	 }

   fflush(fout);
   fclose(fout);

   if(NOISY)
   {
      printf("done!!\n\n");
      fflush(stdout);
   }
   for (multimap<double, LPRegList>::reverse_iterator rit = bestRegs.rbegin(); rit != bestRegs.rend(); rit++) {		   
		LPRegList currentreg = (*rit).second;
		delete currentreg; 
   }
   bestRegs.clear();
   return;
}


//////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////
int
main(int argc, char* argv[])
{
   int		i,j;		// looping variables, etc
   double	s;		// used to read in data
   FILE		*fin, *yin, *win, *censor; // datafiles
   

   if (argc != 2) {
     cout << "usage: " <<  argv[0] << " " << "modelfile.txt" << endl;
     exit(1);
   }
   if (!model.Load(argv[1])) {
     cout << model.GetErrorMessage() << endl;
     exit(1);
   }
   //cout << argv[1] << endl;
   NOISY = model.NOISY;
   NORMSTAND = model.NORMSTAND;
   DSTART = model.DSTART;
   ONEVAR = model.ONEVAR;
   
   modtype = model.modtype;
   NBest = model.NBest;
   Iters = model.iters;
   kmax = model.kmax;
   
   innerAnneal1 = model.innerAnneal1;
   innerAnneal2 = model.innerAnneal2;
   innerAnneal3 = model.innerAnneal3;
   outerAnneal = model.outerAnneal;
   penalty = model.penalty/(double)model.NVARIABLES;
   if (penalty <= 0) {
		cout << "PriorMeanP must be positive" << endl;
		exit(1);
	   //penalty = 0.01;
   }
   if (penalty >= 1) {
		cout << "PriorMeanP must be less than the total number of variables" << endl;
		exit(1);
	   //penalty = 0.99;
   }
   curdim = DSTART;
   
   nobservations = model.NOBSERVATIONS;
   nvariables =model.NVARIABLES;
   maxSampleSize = model.NOBSERVATIONS*(model.NVARIABLES+1);   

   // Read the data
   fin = fopen(model.DATAFILE.c_str(), "r");
   if(NULL == fin)
   {
      printf("Cannot open datafile!\n");
      exit(1);
   }

   yin = fopen(model.RESPONSEFILE.c_str(), "r");
   if(NULL==yin)
   {
      printf("Cannot open response file!\n");
      exit(1);
   }

   win = fopen(model.WEIGHTSFILE.c_str(), "r");
   if(NULL == win)
   {
      printf("Cannot open weights file!\n");
      exit(1);
   }

   if (modtype == SURVMOD) {
		censor = fopen(model.CENSORFILE.c_str(), "r");
		if(NULL == censor) {
			printf("Cannot open censorship file!\n");
			exit(1);
		}
		logY = new double[nobservations];
		Nu = new double[nobservations];
   }

   Y = new double[nobservations];
   W = new double[nobservations];
   data = new double*[nobservations];
   

   for(i=0; i<nobservations; i++)
   {
      data[i] = new double[nvariables];

      fscanf(yin, "%lf", &s);
      Y[i] = (double) s;

      fscanf(win, "%lf", &s);
      W[i] = (double) s;

	  // If we're using the observations
      if(s>0) wnobs++;
	
	  if (modtype == SURVMOD) {
			fscanf(censor, "%lf", &s);
			Nu[i] = (double) s;
			logY[i] = log(Y[i]);
	  }

      
   }

   fclose(yin);
   fclose(win);
   if (modtype == SURVMOD) { fclose(censor);}

   for(i=0; i<nobservations; i++)
   {
      for(j=0; j<nvariables; j++)
      {
         fscanf(fin, "%lf", &s);
	 data[i][j] = (double) s;
      }
   }

   fclose(fin);

   // Set random seeds
   srand( (unsigned)time( NULL ) );
   Random::Set(((double)rand()) / ((double)RAND_MAX));
   //unifRandom.Set(randomseed);

   // Normalize Standardize data??
   if(NORMSTAND)
   {
      NormalizeData();
      StandardizeData();
   }

   // Set the vector with the regressors variables
   Regressors = new int[wnobs-1];
   for(i=0; i<curdim; i++) {
	   Regressors[i] = i;
   }
   master();
   // Clean memory
   
   for(i=0; i<nobservations; i++)
   {
      delete[] data[i]; data[i] = NULL;
   }
   delete[] data; data = NULL;
   delete[] Regressors; Regressors = NULL;
   delete[] Y; Y = NULL;
   delete[] W; W = NULL;
   if (modtype == SURVMOD) {
		delete[] Nu; Nu = NULL;
		delete[] logY; logY = NULL;
   }
   printf("Starting model summary program...");
   string summarystring = "modelsummary.exe ";
   summarystring.append("\"");
   summarystring.append(argv[1]);
   summarystring.append("\"");
   system(summarystring.c_str());
   printf("done!\n");
   return 0;
}

