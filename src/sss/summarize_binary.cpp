#pragma warning(disable:4786)

#define WANT_STREAM                  // include.h will get stream fns
#define WANT_MATH                    // include.h will get math fns
                                     // newmatap.h will get include.h
#include "newmat\newmatap.h"                // need matrix applications
#include "newmat\newmatio.h"                // need matrix output routines


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <float.h>
#include <math.h>
#include <iomanip>

#include "Model.h"
#include "marglik.h"

//using namespace std;

#define EPSILON 0.001
#define TOL 0.00001               // tolerance for NR convergence

#define LINREG 1                // normal linear models
#define BINREG 2                // binary regression models
#define SURVMOD 3               // survival models

//#define PRVAR 1.0
int             NORMSTAND;  // should we standardize data??
Model           model;

double* Y = NULL;
double* W = NULL;
double** data = NULL;
double*		logY = NULL;
double*		Nu	= NULL;

double myPI = 3.141592653589793;

int		nobservations;
int		nvariables;

int* RegMod;
int kmax=0;
int wnobs = 0;
//int maxdim = 0;

double       prvar = 1.0;
FILE* fout;

double		tauPrior = 1.0;		// Wishart prior scale matrix
double		deltaPrior = 3.0;	// Wishart prior df

double AdjustDouble(double a)
{
   double b = fabs(a);
   if((b>EPSILON)&&(b<DBL_MAX)) return a;
   return 0.0;
}

// This function demeans the data for ALL samples!!
void NormalizeData(int linear)
{
   int i,j;
   double s;

   for(j=0; j<nvariables; j++)
   {
      s = 0.0;
      for(i=0; i<nobservations; i++)
      {
         if(W[i] > 0.0) s += data[i][j];
      }
      s = s/((double)wnobs);
      for(i=0; i<nobservations; i++)
      {
         if(W[i] > 0.0) data[i][j] -= s;
      }
   }
   // Same for Y!
   if( linear>0 )
   {
      s = 0.0;
      for(i=0; i<nobservations; i++)
      {
         if(W[i] > 0.0) s += Y[i];
      }

      s = s/(double) wnobs;

      for(i=0; i<nobservations; i++)
      {
         if(W[i] > 0.0) Y[i] -= s;
      }
   }

   return;
}

// This function assumes that the data has MEAN ZERO
// Call the NormalizeData function first!!!
void StandardizeData(int linear)
{
   int i,j;
   double ss;


   for(j=0; j<nvariables; j++)
   {
      ss = 0.0;
      for(i=0; i<nobservations; i++)
      {
        if(W[i] > 0.0) ss += data[i][j]*data[i][j];
      }

      ss = sqrt(ss/( ((double)wnobs - 1.0)));

      for(i=0; i<nobservations; i++)
      {
        if(W[i] > 0.0) data[i][j] = data[i][j]/ss;
      }
   }
	
   // do the same for Y
   if( linear>0)
   {
      ss = 0.0;
      for(i=0; i<nobservations; i++)
      {
         if(W[i] > 0.0) ss += Y[i]*Y[i];
      }

      ss = sqrt( ss/( (double)wnobs - 1.0));

      for(i=0; i<nobservations; i++)
      {
         if(W[i] > 0.0) Y[i] = Y[i]/ss;
      }
   }
   return;
}

int
Solve(int M, double** A)
{
   int i,j;
   int n = M;
   Matrix T(M,M);
   for(i=0; i<n; i++)
   {
      for(j=0; j<n; j++)
      {
		T(i + 1, j + 1) = A[i][j];
      }
   }
   Try { 
	   Matrix B = T.i();
	   for(i=0; i<n; i++)
	   {
		  for(j=0; j<n; j++)
		  {
			 A[i][j] = B(i + 1, j + 1);
		  }
	   }
	   return(1);
   }
   CatchAll {
	   return(0);
   }
}

// Given a model, this returns the marginal likelihood under the prior
void getsummaryLinReg(int dim, double marglik)
{
   int          i,j,k;
   int          npred = dim;
   static int   n = wnobs;
   Matrix X(wnobs, dim);
   double       nu = tauPrior;
   double       s;
   

   //SymmetricMatrix A(npred);

   // Set the X matrix
   for(i=0; i<dim; i++)
   {
      for(j=k=0; j<nobservations; j++)
      {
         if(W[j]==1)
         {
			X(k + 1, i + 1) = data[j][RegMod[i]]; 
            k++;
         }
      }
   }

   for(i=0; i<nobservations; i++)
   {
      if(W[i] == 1) nu += pow(Y[i],2);
   }

   // initialize the matrix M
   double** M = new double*[npred];
   for(i=0; i<npred; i++)
   {
      M[i] = new double[npred];
      memset(M[i],0,npred*sizeof(double));
   }

   // determine the matrix M
   for(i=0; i<npred; i++)
   {
      for(j=i; j<npred; j++)
      {
         s = 0.0;
         for(k=0; k<n; k++) 
         {
            s += X(k+1, i + 1)*X(k + 1,j + 1);
         }

         if(i==j)
         {
			M[i][i] = s+tauPrior;
 //           A(i+1,i+1) = M[i][i];
         }
         else
         {
            M[i][j] = M[j][i] = s;
 //           A(i+1,j+1) = s;
         }
      }
   } // determine M
   
   // calculate the inverse of M
   // this inverse will be stored in M
   if (npred>0) {
	   if(!Solve(npred, M))
	   {
			printf("Can't do inverse!\n\n");
			exit(1);
	   }
   }

   double* Zy = new double[npred];
   memset(Zy,0,npred*sizeof(double));
   for(i=0; i<npred; i++)
   {
      s = 0.0;
      for(j=k=0; j<nobservations; j++)
      {
         if(W[j] == 1)
         {
            s += X(k + 1, i + 1)*Y[j];
            k++;
         }
      }
      Zy[i] = s;
   }

   //now calculate the posterior mean b = V * (X'*y) = M * Zy
   double* db = new double[npred];
   for(i=0; i<npred; i++) {
		db[i] = 0.0;
		for(j=0; j<npred; j++) {
			db[i] += M[i][j]*Zy[j];
		}
   }


   //now calculate d = tau + y'*(y-Xb) = tau + y'*y - y'*y'*X*b = tau + y'*y - Zy'*M*Zy
   s = 0.0;
   for(i=0; i<npred; i++)
   {
      for(j=0; j<npred; j++)
      {
         s += Zy[i]*M[i][j]*Zy[j];
      }
   }
	
   // finish calculating nu, nu is the d in Mike's note 
   nu -= s;

   double da = n + deltaPrior + npred;
   double s_square = nu / da;

   // Print out info
   fprintf(fout, "%d %.5f ", dim, marglik);
   for(i=0; i<dim; i++) fprintf(fout, "%d ",RegMod[i]+1);
   for(i=0; i<npred; i++)
   {
      fprintf(fout, "%.4f ", db[i]);
   }
   for(i=0; i<npred; i++)
   {
      for(j=0; j<npred; j++)
      {
         fprintf(fout,"%.4f ", M[i][j] * s_square);
      }
   }

   // Add the NA values
   if(dim < kmax)
   {
      int total = kmax*kmax + 2*kmax + 2;
      int current = dim*dim + 2*dim + 2;

      for(i=current; i<total; i++) fprintf(fout, "NA ");
   }
   fprintf(fout,"%.4f ",sqrt(s_square * da / (da+1)));
   fprintf(fout,"%.4f ",da);
   fprintf(fout,"\n");


   // clean memory
   for(i=0; i<npred; i++)
   {
      delete[] M[i]; M[i] = NULL;
   }
   delete[] M; M = NULL;
   delete[] Zy; Zy = NULL;
   delete[] db; db = NULL;

}

void getsummaryBinReg(int dim, double marglik) {
	//copy codes from BinRegML
   double       diff = DBL_MAX;
   double       tol = TOL;
   double       tmpsum;
   Matrix X(nobservations, dim+1);
   int          npred = dim + 1; // this includes an intercept
   int          i,j,k;
   int          count=0;
	
   SymmetricMatrix sigma(npred);
   for(i=0; i<nobservations; i++) X(i + 1, 1) = 1.0;
   sigma = 0.0;

   for(i=0; i<nobservations; i++)
   {
      for(j=1; j<npred; j++)
      {
         X(i+ 1, j + 1) = data[i][RegMod[j-1]];
      }
   }

   //////////////////////////////////////////////////////////////////////////
   // First need to compute the MAP estimates via Newton Raphson
   // Set starting values
   double* theta = new double[npred];
   memset(theta, 0, npred*sizeof(double));
   double* newtheta = new double[npred];
   memset(newtheta, 0, npred*sizeof(double));

   double* pi = new double[nobservations];
   double* dtheta = new double[npred];
   double** ddtheta = new double*[npred];
   for(i=0; i<npred; i++) ddtheta[i] = new double[npred];

   while(diff>tol)
   {
      // initialize derivative vector and matrix
      memset(dtheta, 0, npred*sizeof(double));
      for(i=0; i<npred; i++)
         memset(ddtheta[i], 0, npred*sizeof(double));

      // Compute the pi vector and part of first two derivatives
      for(i=0; i<nobservations; i++)
      {
         tmpsum = 0.0;
         for(j=0; j<npred; j++)
         {
            tmpsum += -X(i+ 1, j +1)*theta[j]*W[i];
         }
         pi[i] = 1.0 / (1.0 + exp(tmpsum));

         for(j=0; j<npred; j++)
         {
            // this is for the first derivative
            dtheta[j] += W[i]*(Y[i] - pi[i]) * X(i+1, j + 1);
            // this is for second derivative
            for(k=0; k<npred; k++)
            {
               ddtheta[j][k] += W[i]*(-X(i + 1,j + 1)*X(i + 1,k + 1)*pi[i]*(1-pi[i]));
            }
         }
      }

      // Complete the derivatives
      for(i=0; i<npred; i++)
      {
         dtheta[i] += -theta[i]/prvar;
         ddtheta[i][i] += -1/prvar;
      }

      // Invert the second derivative matrix
      if(!Solve(npred, ddtheta))
      {
			printf("Can't do inverse!\n\n");
			exit(1);
      }

      for(i=0; i<npred; i++)
      {
         tmpsum = 0.0;
         for(j=0; j<npred; j++)
         {
            tmpsum += ddtheta[i][j]*dtheta[j];
         }
         newtheta[i] = theta[i] - tmpsum;
      }

      // Compute the euclidean distance
      diff = 0.0;
      for(i=0; i<npred; i++)
         diff += fabs(theta[i] - newtheta[i])*fabs(theta[i] - newtheta[i]);

      diff = sqrt(diff);

      // set the new theta
      for(i=0; i<npred; i++)
      {
         theta[i] = newtheta[i];
      }
		
	  //debug
      if(count++ == 100)
      {
         printf("NR taking too long\n");
         fflush(stdout);
		 break;
      }
   }

   // Compute the new pi
   for(i=0; i<nobservations; i++)
   {
      tmpsum = 0.0;
      for(j=0; j<npred; j++)
      {
         tmpsum += W[i]*(-X(i + 1, j + 1))*newtheta[j];
      }
      pi[i] = 1.0/(1.0 + exp(tmpsum));
   }

   // Get sigma
   for(i=0; i<npred; i++) {
	   for(j=0; j<npred; j++) {
		sigma(i + 1, j + 1) = -ddtheta[i][j];
	   }
   }
   ////////////////////////////////////////////////////////////////////////

   // Print out info
   fprintf(fout, "%d %.5f ", dim, marglik);
   for(i=0; i<dim; i++) fprintf(fout, "%d ",RegMod[i]+1);
   for(i=0; i<npred; i++)
   {
      fprintf(fout, "%.4f ", theta[i]);
   }
   for(i=0; i<npred; i++)
   {
      for(j=0; j<npred; j++)
      {
         fprintf(fout,"%.4f ", sigma(i+1,j+1));
      }
   }

   // Add the NA values
   if(dim < kmax)
   {
      int total = kmax*kmax + 4*kmax + 4;
      int current = dim*dim + 4*dim + 4;

      for(i=current; i<total; i++) fprintf(fout, "NA ");
   }
   
   fprintf(fout,"\n");


   //clean up
   for(i=0; i<npred; i++)
   {
      delete[] ddtheta[i]; ddtheta[i] = NULL;
   }
   delete[] ddtheta; ddtheta = NULL;
   delete[] theta; theta = NULL;
   delete[] newtheta; newtheta = NULL;
   delete[] pi; pi = NULL;
   delete[] dtheta; dtheta = NULL;

   return;
}


void getsummarySurvMod(int dim, double marglik) {
	//copy codes from SurvModML
	double       diff = DBL_MAX;
   double       tol = TOL;
   double       tmpsum;
   Matrix X(nobservations, dim+1);
   int          npred = dim + 1;	// this includes an intercept
   int			nThetaSize = npred + 1; // also include alpha
   int          i,j,k;
   int          count=0;
	
   //prior parameters
   double SMtao = 1.0;		
   double SMalpha0 = 1.0;
   double SMk0 = 0.8;

   //
   SymmetricMatrix sigma(nThetaSize);
   sigma = 0.0;

   for(i=0; i<nobservations; i++)
   {
		X(i + 1, 1) = 1.0;
		for(j=1; j<npred; j++) {
			X(i+ 1, j + 1) = data[i][RegMod[j-1]];
		}
   }

   //////////////////////////////////////////////////////////////////////////
   // First need to compute the MAP estimates via Newton Raphson
   // Set starting values
   double* theta = new double[nThetaSize];
   memset(theta, 0, nThetaSize*sizeof(double));
   double* newtheta = new double[nThetaSize];
   memset(newtheta, 0, nThetaSize*sizeof(double));
   theta[0] = 1.0;			//carry over the alpha value from iteration to iteration

   double* dtheta = new double[nThetaSize];
   double** ddtheta = new double*[nThetaSize];
   for(i=0; i<nThetaSize; i++) ddtheta[i] = new double[nThetaSize];

   //optimization variables
   double* xprimebeta = new double[nobservations];
   double* ypoweralpha = new double[nobservations];
   double* exp_xprimebeta = new double[nobservations];

   int nObserved = 0;		//number of observed responses
   for (i = 0; i < nobservations; i++) {
	   if (Nu[i] > 0 && W[i] > 0) { nObserved ++;}
   }

   while(diff>tol) {
		
		//compute intermediate variables
		for(i=0; i<nobservations; i++) {
			xprimebeta[i] = 0;
			for (j = 0; j < npred; j++) {
				xprimebeta[i] += X(i+1,j+1) * theta[j+1];
			}
			exp_xprimebeta[i] = exp(xprimebeta[i]);
			ypoweralpha[i] = pow(Y[i],theta[0]);

		}
	  
		// initialize derivative vector and matrix
		memset(dtheta, 0, nThetaSize*sizeof(double));
		for(i=0; i<nThetaSize; i++) {
			memset(ddtheta[i], 0, nThetaSize*sizeof(double));
		}

		//calculate dtheta
		//g1(theta)
		dtheta[0] = (SMalpha0 + nObserved -1 ) / theta[0] - SMk0;
		for(i=0; i<nobservations; i++) {
			dtheta[0] += W[i] * (Nu[i]*logY[i] - ypoweralpha[i] * logY[i] * exp_xprimebeta[i]);
		}

		//g2(theta)
		for (j= 1; j < nThetaSize; j++) {
			dtheta[j] = -theta[j] / SMtao;
			for (i = 0; i < nobservations; i++) {
				dtheta[j] += W[i] * (Nu[i] * X(i+1,j) - ypoweralpha[i] * exp_xprimebeta[i] * X(i+1,j)); 
			}
		}

		//g11(theta)
		ddtheta[0][0] = ( 1 - nObserved - SMalpha0) / theta[0] / theta[0];
		for (i = 0; i < nobservations; i++) {
			ddtheta[0][0] -= W[i] * (ypoweralpha[i] * logY[i] * logY[i] * exp_xprimebeta[i]);
		}

		//g12(theta) & g21(theta)
		for (j= 1; j < nThetaSize; j++) {
			ddtheta[0][j] = 0;
			for (i = 0; i < nobservations; i++) {
				ddtheta[0][j] -= W[i] * (ypoweralpha[i] * exp_xprimebeta[i] * logY[i] * X(i+1,j)); 
			}
			ddtheta[j][0] = ddtheta[0][j];
		}

		//g22(theta)
		for (k = 1; k < nThetaSize; k++) {
			ddtheta[k][k] = -1.0 / SMtao;
		}
		for (i = 0; i < nobservations; i++ ) {
			if (W[i] > 0) {
				double dtemp = -ypoweralpha[i] * exp_xprimebeta[i];
				for (j = 1; j < nThetaSize; j++) {
					for (k = 1; k < nThetaSize; k++) {
						ddtheta[j][k] += dtemp * X(i + 1, j) * X(i + 1, k);
					}
				}
			}
			
		}
		

      // Invert the second derivative matrix
      if(!Solve(nThetaSize, ddtheta)) {
			printf("Can't do inverse!\n\n");
			exit(1);
      }

      for(i=0; i<nThetaSize; i++) {
			tmpsum = 0.0;
			for(j=0; j<nThetaSize; j++) {
				tmpsum += ddtheta[i][j]*dtheta[j];
			}
			newtheta[i] = theta[i] - tmpsum;
      }

      // Compute the euclidean distance
      diff = 0.0;
      for(i=0; i<nThetaSize; i++)
         diff += fabs(theta[i] - newtheta[i])*fabs(theta[i] - newtheta[i]);

      diff = sqrt(diff);

      // set the new theta
      for(i=0; i<nThetaSize; i++)
      {
         theta[i] = newtheta[i];
      }

   }


   // Get sigma
   for(i=0; i<nThetaSize; i++)
      for(j=0; j<nThetaSize; j++) sigma(i + 1, j + 1) = -ddtheta[i][j];



 ///////////////////////////////////////////////////////////////////////////////////////////////////////
   // Print out info
   fprintf(fout, "%d %.5f ", dim, marglik);
   for(i=0; i<dim; i++) fprintf(fout, "%d ",RegMod[i]+1);
   for(i=0; i<nThetaSize; i++)
   {
      fprintf(fout, "%.4f ", theta[i]);
   }
   for(i=0; i<nThetaSize; i++)
   {
      for(j=0; j<nThetaSize; j++)
      {
         fprintf(fout,"%.4f ", sigma(i+1,j+1));
      }
   }

   // Add the NA values
   if(dim < kmax)
   {
      int total = kmax*kmax + 6*kmax + 8;
      int current = dim*dim + 6*dim + 8;

      for(i=current; i<total; i++) fprintf(fout, "NA ");
   }
   
   fprintf(fout,"\n");
////////////////////////////////////////////////////////////////////////////////////////////////////////////

   // clean up
   for(i=0; i<nThetaSize; i++)
   {
      delete[] ddtheta[i]; ddtheta[i] = NULL;
   }
   delete[] ddtheta; ddtheta = NULL;
   delete[] theta; theta = NULL;
   delete[] newtheta; newtheta = NULL;
   delete[] dtheta; dtheta = NULL;

   delete[] xprimebeta;
   delete[] ypoweralpha;
   delete[] exp_xprimebeta;
   return;
}


int
main(int argc, char* argv[])
{
   int i,j,k;
   int tmp1, tmp2;
   double marglik;
   int nummodels;
   FILE *fin, *yin, *win, *modfile,*censor;
   double s;
   char buffer[100000];
   
   if (argc != 2) {
	 printf("usage: %s modelfile.txt\n", argv[0]);
     exit(1);
   }
   if (!model.Load(argv[1])) {
     printf("%s\n", model.GetErrorMessage().c_str());
     exit(1);
   }
   
   NORMSTAND = model.NORMSTAND;
   nummodels = model.NBest;
   nobservations = model.NOBSERVATIONS;
   nvariables =model.NVARIABLES;
   //maxdim = model.kmax;
   int modtype = model.modtype;
 
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

   modfile = fopen(model.OUTFILE.c_str(),"r");
   if(NULL==modfile)
   {
      printf("Cannont open model file!!\n\n");
      exit(1);
   }

   Y = new double[nobservations];
   W = new double[nobservations];
   data = new double*[nobservations];

   for(i=0; i<nobservations; i++)
   {
      data[i] = new double[nvariables];

      fscanf(yin,"%lf",&s);
      Y[i] = (double)s;

      fscanf(win, "%lf", &s);
      W[i] = (double)s;
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
         fscanf(fin,"%lf",&s);
         data[i][j] = (double)s;
      }
   }

   fclose(fin);

   if(NORMSTAND) {
		int nlinear = (modtype == LINREG)? 1: 0;
		NormalizeData(nlinear);
		StandardizeData(nlinear);
   }
  
   fout = fopen(model.SUMMARYFILE.c_str(), "w");
   if(NULL == fout)
   {
      printf("Cannot open outfile!\n\n");
      exit(1);
   }
	
   int nTotalModels = 0;
   // First figure out what the largest dimension is
   
   while (!feof( modfile ) ) {
   //for(i=0; i<nummodels; i++) {
	    k = -100;
		fscanf(modfile, "%d %d",&tmp1,&k);
		fgets(buffer, 100000, modfile);
		if(k>kmax) kmax = k;
		if (k >=0) {nTotalModels++;}
   }
   
   fclose(modfile);
   modfile = fopen(model.OUTFILE.c_str(),"r");

   nummodels = nTotalModels;	// use the real number of models instead of supposed number of models
   // Read in and print out
   for(i=0; i<nummodels; i++)
   {
      fscanf(modfile, "%d %d %lf",&tmp1, &k, &marglik);

      RegMod = new int[k];

      for(j=0; j<k; j++)
      {
         fscanf(modfile, "%d ", &tmp2);
	 RegMod[j] = (tmp2-1);
      }
      // Read in rest of the line
      if(k<kmax) fgets(buffer, 100000, modfile);

      // Print out MAP estimate and Hessian
	  if (model.modtype == BINREG) {
			getsummaryBinReg(k, marglik);	
	  } else if (model.modtype == SURVMOD){
			getsummarySurvMod(k, marglik);	
	  } else {
			getsummaryLinReg(k,marglik);
	  }
      
      delete[] RegMod; RegMod = NULL;
   }
   fclose(fout);
   fclose(modfile);

   for(i=0; i<nobservations; i++)
   {
      delete[] data[i]; data[i] = NULL;
   }
   delete[] Y; Y = NULL;
   delete[] data; data = NULL;
   delete[] Nu; Nu = NULL;
   delete[] logY; logY = NULL;
   return 0;
}
