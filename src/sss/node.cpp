typedef class myNode* LPNode;
typedef class myNode Node;

class myNode
{
public:
   int location;
   int candidate;
};

void DeleteMin(multimap<double, LPNode>& Root) 
{
	LPNode currentnode = (*Root.begin()).second;
	delete currentnode;
	Root.erase(Root.begin());
}

double GetMin(multimap<double, LPNode>& Root) 
{
	if (Root.size() == 0) {
		return(DBL_MIN);
	} else {
		return (*Root.begin()).first;
	}
}

void InsertOne(multimap<double, LPNode>& Root,
                     LPNode newnode,
					 double v)
{
	Root.insert(multimap<double, LPNode>::value_type(v, newnode));
}

void Insert(int maxSize,  
			multimap<double, LPNode>& Root,
            double newelem,
			int location,
            int candidate)
{
	//keep the tree at the required size
	if (Root.size() == maxSize)
	{
		if (newelem <= GetMin(Root)) return;
		DeleteMin(Root);
   }

   //create a new node
   LPNode newnode = new Node;
   newnode->location = location;
   newnode->candidate = candidate;
   InsertOne(Root, newnode, newelem);
}

void InorderWalk(multimap<double, LPNode>& Root,
                     double* weights,
					 int* locations,
                     int* candidates)
{
   if (Root.size() > 0) 
   {
	   int nmax = 0;
	   for (multimap<double, LPNode>::iterator it = Root.begin(); it != Root.end(); it++) {
			LPNode currentnode = (*it).second;
			weights[nmax] = (*it).first;
			locations[nmax] = currentnode->location;
			candidates[nmax] = currentnode->candidate;	
			nmax++;
	   }
   }  
}

//deletes the entire tree
void Delete(multimap<double, LPNode>& Root)
{
	for (multimap<double, LPNode>::iterator it = Root.begin(); it != Root.end(); it++) {
		LPNode currentnode = (*it).second;
		delete currentnode;
    }
	Root.clear();
}
