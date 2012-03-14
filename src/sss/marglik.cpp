/*
Copyright (C) 2007 Chris Hans, Quanli Wang, Adrian Dobra and Mike West

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/
#include <limits>

using namespace std;

#ifndef DBL_MAX 
#define DBL_MAX numeric_limits<double>::max()
#endif

#include <limits.h>
//#include <values.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define WANT_STREAM                  // include.h will get stream fns
#define WANT_MATH                    // include.h will get math fns
                                     // newmatap.h will get include.h
#include "newmatap.h"                // need matrix applications
#include "newmatio.h"                // need matrix output routines

#define TOL 0.0001		// tolerance for NR convergence

extern double** data;
extern int	wnobs;
extern int	nobservations;
extern double*	W;
extern double*	Y;
extern double	syy;
extern double	tauPrior, deltaPrior;
extern double	myPI;
extern double	prvar;
extern double* Nu;
extern double* logY;

double Gammaln(double x)
{
	double d1 = -5.772156649015328605195174e-1;
	double p1[] = {4.945235359296727046734888e0, 2.018112620856775083915565e2,
			2.290838373831346393026739e3, 1.131967205903380828685045e4, 
			2.855724635671635335736389e4, 3.848496228443793359990269e4, 
			2.637748787624195437963534e4, 7.225813979700288197698961e3};
	double q1[] = {6.748212550303777196073036e1, 1.113332393857199323513008e3, 
			7.738757056935398733233834e3, 2.763987074403340708898585e4, 
			5.499310206226157329794414e4, 6.161122180066002127833352e4, 
			3.635127591501940507276287e4, 8.785536302431013170870835e3};
	double d2 = 4.227843350984671393993777e-1;
	double p2[] = {4.974607845568932035012064e0, 5.424138599891070494101986e2, 
           1.550693864978364947665077e4, 1.847932904445632425417223e5, 
           1.088204769468828767498470e6, 3.338152967987029735917223e6, 
           5.106661678927352456275255e6, 3.074109054850539556250927e6};
    double q2[] = {1.830328399370592604055942e2, 7.765049321445005871323047e3, 
           1.331903827966074194402448e5, 1.136705821321969608938755e6, 
           5.267964117437946917577538e6, 1.346701454311101692290052e7, 
           1.782736530353274213975932e7, 9.533095591844353613395747e6};
    double d4 = 1.791759469228055000094023e0;
    double p4[] = {1.474502166059939948905062e4, 2.426813369486704502836312e6, 
           1.214755574045093227939592e8, 2.663432449630976949898078e9, 
           2.940378956634553899906876e10, 1.702665737765398868392998e11, 
           4.926125793377430887588120e11, 5.606251856223951465078242e11};
    double q4[] = {2.690530175870899333379843e3, 6.393885654300092398984238e5, 
           4.135599930241388052042842e7, 1.120872109616147941376570e9, 
           1.488613728678813811542398e10, 1.016803586272438228077304e11, 
           3.417476345507377132798597e11, 4.463158187419713286462081e11};
    double c[] = {-1.910444077728e-03, 8.4171387781295e-04, 
          -5.952379913043012e-04, 7.93650793500350248e-04, 
          -2.777777777777681622553e-03, 8.333333333333333331554247e-02, 
		  5.7083835261e-03};
	if ( (x > 0) && (x <= 2.2204e-016)) {  //x < eps
		return  -log(x);
	} else if ((x > 2.2204e-016) && ( x <= 0.5)) {
		double xden = 1;
		double xnum = 0;  
		for (int i = 0; i < 8; i++) {
			xnum = xnum * x + p1[i];
			xden = xden * x + q1[i];
		}
		return -log(x) + (x * (d1 + x * (xnum / xden)));
	} else if((x > 0.5) && (x <= 0.6796875)) {
		double xm1 = (x - 0.5) - 0.5;
		double xden = 1;
		double xnum = 0;
		for (int i = 0; i < 8; i++) {
			xnum = xnum * xm1 + p2[i];
			xden = xden * xm1 + q2[i];
		}
		return -log(x) + xm1 * (d2 + xm1 * (xnum / xden));
	} else if ((x > 0.6796875) && (x <= 1.5)) {
		double xm1 = (x - 0.5) - 0.5;
		double xden = 1;
		double xnum = 0;
		for (int i = 0; i < 8; i++) {
			xnum = xnum * xm1 + p1[i];
			xden = xden * xm1 + q1[i];
		}
		return xm1 * (d1 + xm1 * (xnum / xden));
	} else if ((x > 1.5) && (x <= 4)) {
		double xm2 = x - 2;
		double xden = 1;
		double xnum = 0;
		for (int i = 0; i < 8; i++) {
			xnum = xnum * xm2 + p2[i];
			xden = xden * xm2 + q2[i];
		}
		return xm2 * (d2 + xm2 * (xnum / xden));
	} else if ((x > 4) && (x <= 12)) {
		double xm4 = x - 4;
		double xden = -1;
		double xnum = 0;
		for (int i = 0; i < 8; i++) {
			xnum = xnum * xm4 + p4[i];
			xden = xden * xm4 + q4[i];
		}
		return d4 + xm4 * (xnum / xden);
	} else {
      double r = c[6];
      double ysq = x * x;
	  for (int i = 0; i < 6; i++) {
		  r = r / ysq + c[i];
	  }
      r = r / x;
      double corr = log(x);
      double spi = 0.9189385332046727417803297;
      return r + spi - 0.5 * corr + x * ( corr - 1);
	}
}

// inverts a MxM matrix A

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
double
LinRegML(int* RegMod, int dim)
{
   int          i,j,k;
   int          npred = dim;
   static int   n = wnobs;
   Matrix X(wnobs, dim);
   double       marglik;
   double       nu = tauPrior;
   double       s;
   
// First take care of the null model case
   if(dim==0)
   {
		double temp = Gammaln((n+deltaPrior)/2.0);
		temp -= Gammaln(deltaPrior/2.0);
		temp -= n*log(myPI)/2.0;
		temp += deltaPrior*log(tauPrior)/2.0;
		temp -= (n + deltaPrior)*log(tauPrior + n - 1.0)/2.0;
		return temp;
        
   }
   
   //if (dim == 3 && RegMod[0] == 11 && RegMod[1] == 20 && RegMod[2] == 49)
   //{
	//   s = 0;
   //}
   SymmetricMatrix A(npred);

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
            A(i+1,i+1) = M[i][i];
         }
         else
         {
            M[i][j] = M[j][i] = s;
            A(i+1,j+1) = s;
         }
      }
   } // determine M
   
   // calculate the determinant of M
   double logDetM = A.LogDeterminant().LogValue();
   
   // calculate the inverse of M
   // this inverse will be stored in M
   if(!Solve(npred, M))
   {
      return(DBL_MAX);
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

   s = 0.0;
   for(i=0; i<npred; i++)
   {
      for(j=0; j<npred; j++)
      {
         s += Zy[i]*M[i][j]*Zy[j];
      }
   }

   // finish calculating nu
   nu -= s;

   marglik = 0.0;
   marglik -= n*log(myPI)/2.0;
   marglik += (deltaPrior+2*npred)*log(tauPrior)/2.0;
   marglik -= (logDetM/2.0);
   marglik += Gammaln((n + deltaPrior + npred)/2.0);
   marglik -= Gammaln((deltaPrior + npred)/2.0);
   marglik -= (n + deltaPrior + npred)*log(nu)/2.0;

   // clean memory
   for(i=0; i<npred; i++)
   {
      delete[] M[i]; M[i] = NULL;
   }
   delete[] M; M = NULL;
   delete[] Zy; Zy = NULL;

   return(marglik);
}

// Given a binary regression model, this returns the marginal likelihood 
double
BinRegML(int* RegMod, int dim,double* bhat, double* shat)
{
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
         return(DBL_MAX);
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
      }
   }


	// Record the maximized coefficients if requested
   if(bhat != NULL)
   {
      for(i=0; i<npred; i++) bhat[i] = theta[i];
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
   int cnt = 0;
   for(i=0; i<npred; i++) {
	   for(j=0; j<npred; j++) {
		   sigma(i + 1, j + 1) = -ddtheta[i][j];
		   if(shat != NULL) {
				shat[cnt] = sigma(i+1,j+1);
				cnt++;
		   }
	   }
   }

   //////////////////////////////////////////////////////////////////////

   // Do the Laplace approximation

   double logChat = 0.0;

   // calculate the det of sigma
   double logDetSig = sigma.LogDeterminant().LogValue();
   
   logChat += 0.5*logDetSig;
   logChat -= 0.5*npred * log(prvar);

   tmpsum = 0.0;
   for(i=0; i<npred; i++) tmpsum += newtheta[i]*newtheta[i];

   logChat -= 0.5*tmpsum / prvar;

   for(i=0; i<nobservations; i++)
   {
      logChat += W[i] * (Y[i]*log(pi[i]) + (1-Y[i])*log(1-pi[i]));
   }
	
   // clean up
   for(i=0; i<npred; i++)
   {
      delete[] ddtheta[i]; ddtheta[i] = NULL;
   }
   delete[] ddtheta; ddtheta = NULL;
   delete[] theta; theta = NULL;
   delete[] newtheta; newtheta = NULL;
   delete[] pi; pi = NULL;
   delete[] dtheta; dtheta = NULL;

   return(logChat);
}

double SurvModML(int* RegMod, int dim, double& alpha,double* bhat,double* shat)
{
   double       diff = DBL_MAX;
   double       tol = TOL;
   double       tmpsum;
   Matrix X(nobservations, dim+1);
   int          npred = dim + 1;	// this includes an intercept
   int			nThetaSize = npred + 1; // also include alpha
   int          i,j,k;
   int          count=0;
	
   //prior parameters
   double SMtao = tauPrior;
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
   theta[0] = alpha;			//carry over the alpha value from iteration to iteration
   if (alpha < 1) theta[0] = 1;	//force alpha >=1
	
   theta[0] = 1.0;

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
			if(W[i] > 0.0) dtheta[0] += Nu[i]*logY[i] - ypoweralpha[i] * logY[i] * exp_xprimebeta[i];
			//dtheta[0] += W[i] * (Nu[i]*logY[i] - ypoweralpha[i] * logY[i] * exp_xprimebeta[i]);
		}

		//g2(theta)
		for (j= 1; j < nThetaSize; j++) {
			dtheta[j] = -theta[j] / SMtao;
			for (i = 0; i < nobservations; i++) {
			     if(W[i] > 0.0) dtheta[j] += Nu[i] * X(i+1,j) - ypoweralpha[i] * exp_xprimebeta[i] * X(i+1,j);
				//dtheta[j] += W[i] * (Nu[i] * X(i+1,j) - ypoweralpha[i] * exp_xprimebeta[i] * X(i+1,j)); 
			}
		}

		//g11(theta)
		ddtheta[0][0] = ( 1 - nObserved - SMalpha0) / theta[0] / theta[0];
		for (i = 0; i < nobservations; i++) {
		     if(W[i] > 0.0) ddtheta[0][0] -= ypoweralpha[i] * logY[i] * logY[i] * exp_xprimebeta[i];
			//ddtheta[0][0] -= W[i] * (ypoweralpha[i] * logY[i] * logY[i] * exp_xprimebeta[i]);
		}

		//g12(theta) & g21(theta)
		for (j= 1; j < nThetaSize; j++) {
			ddtheta[0][j] = 0;
			for (i = 0; i < nobservations; i++) {
			     if(W[i] > 0.0) ddtheta[0][j] -= ypoweralpha[i] * exp_xprimebeta[i] * logY[i] * X(i+1,j);
				//ddtheta[0][j] -= W[i] * (ypoweralpha[i] * exp_xprimebeta[i] * logY[i] * X(i+1,j)); 
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
			return(DBL_MAX);
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

	  //cout << alpha << "\t" << theta[0] << endl;
	  alpha = theta[0];
   }
   
   // Record the maximized coefficients
   if(bhat!= NULL) {
      for(i=0; i<nThetaSize; i++) bhat[i] = theta[i];
   }

   // Get sigma
   int cnt = 0;
   for(i=0; i<nThetaSize; i++) {
      for(j=0; j<nThetaSize; j++) {
         sigma(i+1,j+1) = -ddtheta[i][j];
         if(shat!=NULL)	{
            shat[cnt] = sigma(i+1,j+1);
            cnt++;
         }
      }
   }

   //////////////////////////////////////////////////////////////////////

   // Do the Laplace approximation

   // calculate the det of sigma
   double logDetSig = sigma.LogDeterminant().LogValue();
   double logChat = 0.0;    
   logChat += 0.5*logDetSig;
   logChat += 0.5 * log(myPI * 2);
   logChat -= 0.5 * (nThetaSize - 1) * log(SMtao);
   logChat += SMalpha0 *log(SMk0) - Gammaln(SMalpha0);
   logChat += (SMalpha0 + nObserved - 1) * log(theta[0]);
   
   for(i=0; i<nobservations; i++)
   {
	   double xpbeta = 0;
	   for (j = 0; j < npred; j++) {
			xpbeta += W[i] * (X(i+1,j+1) * theta[j+1]);
	   }
	   if(W[i] > 0.0)
	   {
           logChat += Nu[i] * xpbeta + Nu[i] * (theta[0] - 1) * logY[i];
           logChat -= pow(Y[i],theta[0]) * exp(xpbeta);
	   }
	   //logChat += W[i] * (Nu[i] * xpbeta + Nu[i] * (theta[0] - 1) * logY[i]);
	   //logChat -= W[i] * (pow(Y[i],theta[0]) * exp(xpbeta));
   }
   logChat -= SMk0 * theta[0];
   for (j = 1; j < nThetaSize; j++) {
	   logChat -= 0.5 / SMtao * theta[j] * theta[j];
   }
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

   return(logChat);
}

