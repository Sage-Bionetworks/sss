/***********************************
 * Structs to keep track of models *
 ***********************************/
typedef class myList* LPRegList;
typedef class myList RegList;
class myList
{
public:
  int* vars; //variables in this model
  int dim;   //dimension od this model
  double score; //score for this model
  int iter; //iteration model was found
  myList(int size){ vars = new int[size];};
  ~myList() {delete [] vars; vars = NULL;}
};



//struct myList
//{
//   int		vars[NOBSERVATIONS-1];	// variables in this model
//   int		dim;			// dimension of this model
//   double	score;			// score for this model
//   int		iter;			// iteration model was found
//};

///////////////////////////////////////////////////////
//////////////    Functions    ////////////////////////
///////////////////////////////////////////////////////

// This function demeans the data across ALL samples!!
void
NormalizeData()
{
   int		i,j;
   double	s;

   for(j=0; j<nvariables; j++)
   {
      s = 0.0;
      for(i=0; i<nobservations; i++)
      {
         if(W[i] > 0.0) s += data[i][j];
      }
      s = s/(double)wnobs;

      for(i=0; i<nobservations; i++)
      {
         if(W[i] > 0.0) data[i][j] -= s;
      }
   }

   // Same for Y!
   if( modtype == LINREG )
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

// This function standardizes the data across ALL samples!!
// IMPORTANT -- data MUST have mean zero first!!
// Call NormalizeData first!!
void
StandardizeData()
{
   int		i,j;
   double	ss;

   for(j=0; j<nvariables; j++)
   {
      ss = 0.0;
      for(i=0; i<nobservations; i++)
      {
         if(W[i] > 0.0) ss += data[i][j]*data[i][j];
      }

      ss = sqrt(ss / ((double)wnobs - 1.0));

      for(i=0; i<nobservations; i++)
      {
         if(W[i] > 0.0) data[i][j] = data[i][j] / ss;
      }
   }

   // do the same for Y
   if( modtype == LINREG)
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
CanAdd(int candidate)
{
   int i;

   for(i=0; i<curdim; i++)
   {
      if(Regressors[i] == candidate) return(0);
   }
   
   return(1);
}

double
AdjustDouble(double a)
{
   double b = fabs(a);
   if((b>EPSILON)&&(b<DBL_MAX)) return(a);
   return 0.0;
}

void
NormalizeWeights(double* w,
		 double* cumw,
		 double* savew,
		 int nmax,
		 double anneal)
{
   int i;

   for(i=0; i<nmax; i++) 
   {
      savew[i] = w[i]; // this saves the scores so we don't recompute
      w[i] -= w[nmax-1];
   }

   // set the annealing and exponentiate
   for(i=0; i<nmax; i++)
   {
      w[i] *= anneal;
      w[i] = AdjustDouble(exp(w[i]));
   }

   cumw[0] = 0;
   for(i=1; i<=nmax; i++)
   {
      cumw[i] = cumw[i-1] + w[i-1];      
   }
   for(i=1; i<=nmax; i++)
   {
      cumw[i] /= cumw[nmax];
   }

   return;
}

int WeightedSampling(int maxk, double* weights)
{
   double s = unifRandom.Next();
   int q1 = 0;
   int q2 = maxk;
   int q;

   while( (q1+1) != q2 )
   {
      q = (q1+q2)/2;

      if(weights[q]<s)
      {
         q1 = q;
      }
      else
      {
         q2 = q;
      }
   }
   return(q1);
}

void
SortReg(int* Reg,
	int dim)
{
   int tmp, i;

   // They will all be in increasing order except one of them
   for(i=0; i<(dim-1); i++)
   {
      if(Reg[i] > Reg[i+1])
      {
         tmp = Reg[i];
	 Reg[i] = Reg[i+1];
	 Reg[i+1] = tmp;
      }
   }

   // Now go backwards and fix it up
   for(i=(dim-1); i>0; i--)
   {
      if(Reg[i] < Reg[i-1])
      {
         tmp = Reg[i];
	 Reg[i] = Reg[i-1];
	 Reg[i-1] = tmp;
      }
   }

   return;
}

void
MakeTempReg(int location,
	    int candidate,
	    int* TempReg,
	    int propdim)
{
   int i,j;
   
   if(propdim == curdim) // we're doing replacement
   {
      for(i=0; i<propdim; i++) TempReg[i] = Regressors[i];
      // Make the swap and sort
      TempReg[location] = candidate;
      SortReg(TempReg, propdim);
   }
   else if(propdim < curdim) // we're deleting
   {
      for(i=j=0; i<curdim; i++)
      {
         if(i!=location)
	 {
            TempReg[j] = Regressors[i];
	    j++;
	 }
      }
   }
   else // we're adding
   {
      for(i=0; i<curdim; i++)
      {
         if(Regressors[i] < candidate) TempReg[i] = Regressors[i];
	 else break;
      }
      TempReg[i] = candidate;
      i++;
      while(i<propdim)
      {
         TempReg[i] = Regressors[i-1];
	 i++;
      }
   }

   return;
}

double
GetScore(int location, int candidate)
{
   int		propdim;
   int* 	TempReg;
   double 	score;
   static double SMalpha = 1;

   // Figure out the 
   if( (location>=0) && (candidate>=0) ) propdim = curdim;
   else if(candidate==-1) propdim = curdim - 1;
   else propdim = curdim + 1;

   TempReg = new int[propdim];

   // Get the temporary model to check
   MakeTempReg(location, candidate, TempReg, propdim);

   // Figure out which model we are using and evaluate!
   if(modtype == LINREG)
   {
      if(BIC==1)
      {
         //double R2 = GetR2(TempReg, propdim); 
	 //score = wnobs*log(1.0-R2) + propdim*log(wnobs);
      }
      else
      {
         score = LinRegML(TempReg, propdim);
      }
   }
   else if(modtype == BINREG)
   {
      score = BinRegML(TempReg, propdim,NULL,NULL);
   }
   else
   {
      // other models go here
	   score = SurvModML(TempReg, propdim, SMalpha,NULL,NULL);
   }

   delete[] TempReg; TempReg = NULL;

   return(score);
}

void
UpdateMasterModel(int location, int candidate)
{
   int j, cnt = 0;

   // If we're swapping
   if( (location >= 0) && (candidate >= 0) )
   {
      // Don't updim curdim variable
      Regressors[location] = candidate;
      SortReg(Regressors, curdim);
   }
   else if(candidate<0) // we're deleting
   {
      curdim--;

      for(j=location; j<curdim; j++)
      {
         Regressors[j] = Regressors[j+1];
      }
   }
   else // we're moving up a dimension
   {
      while((Regressors[cnt] < candidate) && (cnt < curdim)) cnt++;

      for(j=curdim; j>cnt; j--)
      {
         Regressors[j] = Regressors[j-1];
      }
      Regressors[j] = candidate;

      // Update the dimension
      curdim++;
   }

}

void
CheckAddModel(int location,
              int candidate,
	      int dim,
	      double score,
		  multimap<double, LPRegList>& BestRegList,
	      int iter)
{
   int l;
   int* TempReg;
   
   // First see if this one is a top model
   if( !(score > (*BestRegList.begin()).first)) return;

   // Otherwise see if we've found it before
   // Set up the new regression
   TempReg = new int[dim];
   MakeTempReg(location, candidate, TempReg, dim);
   multimap<double, LPRegList>::iterator itstart = BestRegList.lower_bound(score - EPSILON/10);
   if (itstart == BestRegList.end()) itstart = BestRegList.begin();
   multimap<double, LPRegList>::iterator itend = BestRegList.upper_bound(score + EPSILON/10);
	
   // See if we've found this before
   int foundreg = 0;
     
   for (; itstart != itend; itstart++) {
	   double currentscore= (*itstart).first;
	   LPRegList currentreg= (*itstart).second;
	   if((fabs(score-currentscore)<EPSILON)&&((currentreg->dim)==dim)) {
			for(l=0; l<dim; l++)	 {
				if(TempReg[l] != (currentreg->vars[l]))	{
					break;
				}
			}
			if(l==dim)	 {
				foundreg = 1;
				break;
			}
	   }
   }
   // if this is a new best of regression...
   if(!foundreg)
   {
		LPRegList newreg = new  RegList(model.NOBSERVATIONS - 1);
		for (l = 0; l < dim; l++) {
			newreg->vars[l] = TempReg[l];
		}
		newreg->dim = dim;
		newreg->score = score;
		newreg->iter = iter + 1;
		BestRegList.insert(multimap<double, LPRegList>::value_type(score, newreg));
		if (dim > maxdim) maxdim = dim;
		if (BestRegList.size() > NBest) {
			LPRegList current = (*BestRegList.begin()).second;
			delete current;
			BestRegList.erase(BestRegList.begin());
		}
   }

   delete[] TempReg; TempReg = NULL;

   return;
}
