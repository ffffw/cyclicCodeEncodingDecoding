
#include<iostream>
#include<math.h>
#include<stdio.h>


using namespace std;


class cyclicCode;

// class polynomial

class polynomial
{
public:
	polynomial(int);
	int getDegree();

	friend ostream& operator <<(ostream&, polynomial&);
	friend istream& operator >>(istream&, polynomial&);

	bool operator == (polynomial) const;

	polynomial operator ^ (polynomial) const;
	polynomial operator * (polynomial) const;
	polynomial operator % (polynomial) const;

	friend void findParity( cyclicCode, polynomial);
	friend void decode(cyclicCode, polynomial);
	~polynomial();

private:
	bool *coefficents;
	int trueDegree, degree;

};



//class cyclicCode containing necessary elements 

class cyclicCode
{
public:
	cyclicCode(int, int);
	~cyclicCode();
	void input();
	void generateMatrices();
	void generateCodeVectors();
	
	friend void findParity( cyclicCode, polynomial);
	friend void decode(cyclicCode, polynomial);

private:
	int n, k, x, y, dMin, s, ts, power;
	bool **ipMatrix, **genMatrix, **hMatrix, **pMatrix, **symbols,  **checkBitMatrix, **codeVectors, **syndromeMatrix, **hTMatrix;
	int **noOfOnes, *weight;
	void print(bool**, int, int);
	void print(int**, int, int);
	void printCodeVectorTable();
	void capacity();
	void initDecode();
};


// definition of function getDegree() to get the degree of polynomial, i.e the highest power of x that exists with a non zero coefficent

int polynomial::getDegree()
{
	for (int i = degree; i >= 0; i--)
		if (coefficents[i] != 0)
		{
			trueDegree = i;
			return i;
		}

	return 0;
}

// constructor for polynomial, which takes degree of the polynomial as parameter, creates a array of coefficents, and assigns 0 to each one of them

polynomial::polynomial(int a)
{
	degree = a;
	coefficents = new bool[degree + 1];
	for (int i = 0; i < degree + 1; i++) coefficents[i] = 0;
}

// destructor

polynomial::~polynomial()
{

}

// definition of a overridien == (equal to) operator

bool polynomial::operator==(polynomial secondPoly) const
{
	if (trueDegree != secondPoly.trueDegree) return 0;

	for (int i = trueDegree; i >= 0; i--) if (coefficents[i] != secondPoly.coefficents[i]) return 0;

	return 1;

}

// definition overridden insertion operator to take input

istream & operator>>(istream& streamLoc, polynomial &polyLoc)
{


	cout << "Enter the coefficents of the terms as asked \n";
	for (int i = polyLoc.degree; i >= 0; i--)
	{
		cout << "x^" << i << "\t";
		streamLoc >> polyLoc.coefficents[i];
	}

	polyLoc.trueDegree = polyLoc.getDegree();

	return streamLoc;

}

// definition of overridden extraction operator to print the polynomial

ostream& operator << (ostream& streamLoc, polynomial& polyLoc)
{
	if (polyLoc.trueDegree != 0)
	{
		if (polyLoc.coefficents[polyLoc.degree] != 1)  streamLoc << polyLoc.coefficents[polyLoc.degree];
		streamLoc << "x^" << polyLoc.degree;
	}


	for (int i = polyLoc.trueDegree - 1; i > 0; i--)
	{
		if (polyLoc.coefficents[i] == 1)    	streamLoc << " +x^" << i;
		else     streamLoc << " +" << polyLoc.coefficents[i] << "x^" << i;
	}

	if (polyLoc.coefficents[0] != 0) cout << " +" << polyLoc.coefficents[0];

	streamLoc << endl;

	if (polyLoc.trueDegree == 0 && polyLoc.coefficents[0] == 0) cout << "0\n";

	return streamLoc;
}


//definition of overridden EX-OR (^) operator

polynomial polynomial::operator^(polynomial secondPoly) const
{
	int tempDegree;
	if (degree > secondPoly.degree) tempDegree = degree;
	else tempDegree = secondPoly.degree;

	polynomial tempPoly(tempDegree);

	for (int i = tempPoly.degree; i >= 0; i--)
	{
		if ((i <= degree) && (i <= secondPoly.degree))
		{
			tempPoly.coefficents[i] = coefficents[i] ^ secondPoly.coefficents[i];
		}
		else if (i <= degree && i>secondPoly.degree)
		{
			tempPoly.coefficents[i] = coefficents[i];
		}
		else
		{
			tempPoly.coefficents[i] = 0 ^ secondPoly.coefficents[i];
		}
	}

	tempPoly.trueDegree = tempPoly.getDegree();
	return tempPoly;
}


polynomial polynomial::operator*(polynomial secondPoly) const
{
	int tempDegree;
	tempDegree = degree + secondPoly.degree;

	polynomial tempPoly(tempDegree);

	for (int i = 0; i <= degree; i++)
	{
		for (int j = 0; j <= secondPoly.degree; j++)
		{
			tempPoly.coefficents[i + j] = tempPoly.coefficents[i + j] + (coefficents[i] * secondPoly.coefficents[j]);
		}
	}

	tempPoly.trueDegree = tempPoly.getDegree();
	return tempPoly;
}


//definition of overridden remainder (%) operator


polynomial polynomial::operator%(polynomial secondPoly) const
{
	polynomial remainder(trueDegree), tempPoly(trueDegree);

	for (int i = trueDegree; i >= 0; i--)   remainder.coefficents[i] = coefficents[i];


	if (trueDegree < secondPoly.trueDegree)  goto theEnd;

	{
		polynomial  quotient(trueDegree - secondPoly.trueDegree);

		if (remainder == secondPoly)
		{
			quotient.coefficents[0] = 1;
			goto divide;
		}

		quotient.coefficents[trueDegree - secondPoly.trueDegree] = 1;

	divide:
		tempPoly = quotient * secondPoly;
		remainder = remainder ^ tempPoly;

		remainder = (remainder%secondPoly);

	}

theEnd:
	remainder.trueDegree = remainder.getDegree();
	return remainder;
}



// constructor for class cyclicCode

cyclicCode::cyclicCode(int n1, int k1)
{
	ipMatrix = new bool*[k1];
	for (int i = 0; i < k1; ++i)   ipMatrix[i] = new bool[n1-k1];

	n = n1;
	k = k1;
	x = k1;
	y = n1-k1;
	power = pow(2, k);

	
	

	for (int i = 0; i < k; i++)
		for (int j = 0; j < n - k; j++)
			ipMatrix[i][j] = 0;

}

// a function to take a matrix of defined order as input from user

void cyclicCode::input()
{
	cout << "enter elements\n";
	for (int i = 0; i < x; i++)
	{
		for (int j = 0; j < y; j++)
		{
			cout << "(" << i << "," << j << ") \t";
			cin >> ipMatrix[i][j];
		} cout << "\n";
	}
}



void cyclicCode::print(bool **mat, int row, int col)
{
	for (int i = 0; i < row; i++)
	{
		cout << "\t";
		for (int j = 0; j < col; j++)
		{
			//cout << bitset<8>(ipMatrix[i * j]) << "\t";
			cout << mat[i][j] << "\t";
		} cout << "\n";


	}
}

void cyclicCode::print(int **mat, int row, int col)
{
	for (int i = 0; i < row; i++)
	{
		cout << "\t";
		for (int j = 0; j < col; j++)
		{
			//cout << bitset<8>(ipMatrix[i * j]) << "\t";
			cout << mat[i][j] << "\t";
		} cout << "\n";


	}
}

void cyclicCode::printCodeVectorTable()
{
	cout << "\n\t Message bits \t | \t Check Bits \t | \t Code Vectors\n";
	cout << "\n\t------------------------------------------------------------------\n";
	for (int i = 0; i < pow(2, k); i++)
	{
		cout << "\t \t";
		for (int j = 0; j < k; j++) cout << symbols[i][j];
		cout << "\t | \t";
		for (int j = 0; j < n - k; j++) cout << checkBitMatrix[i][j];
		cout << "\t\t | \t     ";
		for (int j = 0; j < n; j++) cout << codeVectors[i][j];
		cout << "\n";

	} cout << "\n";
}



// a function to generate H, G and P

void cyclicCode::generateMatrices()
{
	cout << "\n...............................Generating Matrices .............................\n";


	genMatrix = new bool*[k];
	for (int i = 0; i < k; ++i)   genMatrix[i] = new bool[n];

	hMatrix = new bool*[n - k];
	for (int i = 0; i < (n - k); ++i)   hMatrix[i] = new bool[n];

	pMatrix = new bool*[k];
	for (int i = 0; i < k; ++i)   pMatrix[i] = new bool[n - k];

		pMatrix = ipMatrix;

		cout << "\n Parity Matrix  : \n";
		print(ipMatrix, k, n - k);

		//**********************************************************generator matrix  remember G=[I:P]

		for (int i = 0; i < k; i++)
		{
			for (int j = 0; j <= (n - k); j++)
			{
				if (i != j)  genMatrix[i][j] = 0;
				else if (i == j) genMatrix[i][j] = 1;
			}
		}


		for (int i = 0; i < k; i++)
		{
			for (int j = (n - k) + 1, p = 0; j < n && p<(n - k); j++, p++)
			{
				genMatrix[i][j] = pMatrix[i][p];
			}
		}

		cout << "\n Generator Matrix Generated : \n";
		print(genMatrix, k, n);


		//**********************************************************Parity Check Matrix  remember H=[Pt:I]


		int diff = k;

		for (int i = 0; i < (n - k); i++)
		{
			for (int j = 0; j < k; j++)
			{
				hMatrix[i][j] = pMatrix[j][i];

			}
		}

		for (int i = 0; i < (n - k); i++)
		{
			for (int j = k; j < n; j++)
			{
				if ((i + j == diff)) hMatrix[i][j] = 1;

				else if ((i + j) != diff) hMatrix[i][j] = 0;


			}  diff += 2;

		}

		cout << "\n Parity Check Matrix Generated : \n";
		print(hMatrix, n - k, n);

		
	}

	

// a function to generate code vectors

void cyclicCode::generateCodeVectors()
{

	// generate a matrix of binary message bits from 0 to 2^k



	symbols = new bool*[power];
	for (int i = 0; i < power; ++i)  symbols[i] = new bool[k];

	int count;
	bool bit;

	for (int bitPos = 0; bitPos < k; bitPos++)
	{
		count = 0;
		bit = 0;

		for (int j = 0; j < power; j++)
		{
			symbols[j][k - (bitPos + 1)] = bit;
			++count;
			if (count == pow(2, bitPos))
			{
				count = 0;
				if (bit)
				{
					bit = 0;
				}
				else
				{
					bit = 1;
				}


			}
		}


	}

	cout << "\nwe have following message bits : \n";
	print(symbols, power, k);

	// generate check bit matrix for each bit (c1, c2, c3,....) for ex-oring i.e (n-k) matrices of order (power * k)



	//*************** moduli 2  on the basis of logic that if a there are odd number of ones then op is 1 otherwise 0

	checkBitMatrix = new bool*[power];
	for (int i = 0; i < power; i++) checkBitMatrix[i] = new bool[n - k];


	for (int i = 0; i < power; i++)
	{
		for (int j = 0; j < n-k; j++)
		{  
			checkBitMatrix[i][j] = 0;
			for (int l = 0; l < k; l++)
			{
				checkBitMatrix[i][j] = checkBitMatrix[i][j] ^ (symbols[i][l] * pMatrix[l][j]);
				
			}
			
			
		}
	} 
	
	//****************** creating code vector matrix 

	codeVectors = new bool*[power];
	for (int i = 0; i < power; i++) codeVectors[i] = new bool[n];

	for (int i = 0; i < power; i++)
	{
		for (int j = 0; j < k; j++)
			codeVectors[i][j] = symbols[i][j];

		for (int j = k; j < n; j++)
			codeVectors[i][j] = checkBitMatrix[i][j - k];
	}

	cout << "\n code vectors :";
	//	print(codeVectors, power, n);
	printCodeVectorTable();
	capacity();



}

// a fuunction to calculate the channel capacity

void cyclicCode::capacity()
{
	//****************************** calculate weight of each code vector
	weight = new int[power];
	for (int i = 0; i < power; i++)
	{
		weight[i] = 0;
		for (int j = 0; j < n; j++)
			if (codeVectors[i][j])
				weight[i]++;
	}

	//************************************* find dMin
	dMin = weight[1];
	for (int i = 2; i < power; i++) if (weight[i] < dMin) dMin = weight[i];

	//************************************calculate s and ts
	s = dMin - 1;
	ts = s / 2;

	//************** print the capacity of code

	cout << " \n \there we have dmin =" << dMin << "\t therefore  the code can detect   " << s << "  errors and correct   " << ts << "  of them\n";
}

void cyclicCode::initDecode()
{
	cout << "\n\n...............................Generating Error Vector Matrix..............................";

	syndromeMatrix = new bool*[n + 1];
	for (int i = 0; i <= n; i++) syndromeMatrix[i] = new bool[n - k];

	hTMatrix = new bool*[n];
	for (int i = 0; i < n; i++) hTMatrix[i] = new bool[n - k];

	for (int i = 0; i < n - k; i++) syndromeMatrix[0][i] = 0;

	for (int i = 1, l = 0; i <= n, l<n; i++, l++)
	{
		for (int j = 0; j < n - k; j++)
		{
			syndromeMatrix[i][j] = hMatrix[j][l];
			hTMatrix[l][j] = hMatrix[j][l];
		}
	}


	cout << "\n\nGenerated Error Vector Matrix : \n\n";
	print(syndromeMatrix, n + 1, n - k);


}

void decode(cyclicCode tempCode, polynomial tempGenFunc)
{
	tempCode.initDecode();

	char choice = 'y';

	char *ipVector = new char[tempCode.n];

	bool *vector = new bool[tempCode.n];

	bool **syndromeMul = new bool*[tempCode.n];

	bool *syndrome = new bool[tempCode.n - tempCode.k];
	for (int i = 0; i < tempCode.n; i++) syndromeMul[i] = new bool[tempCode.n - tempCode.k];

	int *countOne = new int[tempCode.n - tempCode.k];


	while (choice == 'y' || choice == 'Y') {
		cout << "\n\n*************************************************************************************************\n enter the receieved vector (" << tempCode.n << " bits long) you want to decode/correct  \t";
		cin >> ipVector; cout << endl;
		for (int i = 0; i < tempCode.n; i++)
		{
			if (ipVector[i] == '1') vector[i] = 1;
			else vector[i] = 0;
		}

		polynomial tempPoly(tempCode.n-1),tempRemain(tempGenFunc.trueDegree-1);


		for (int i = 0, j=tempCode.n-1 ; i < tempCode.n, j>=0; i++,j--)  tempPoly.coefficents[j] = vector[i];

		tempPoly.trueDegree = tempPoly.getDegree();

		tempRemain = tempPoly % tempGenFunc;

		cout << "generated vector polynomial : " << tempPoly;
		cout<<"Syndrome Polynomial after dividing vector function by generator function " << tempRemain;
  

		for (int i = 0, j = tempCode.n - tempCode.k - 1; i < tempCode.n - tempCode.k, j >= 0; i++, j--)
		{   
			cout << endl << tempRemain.trueDegree;
			if (tempRemain.trueDegree >=j)
				syndrome[i] = tempRemain.coefficents[j];
			else
				syndrome[i] = 0;
		}


		cout << "\n from syndrome function we get syndrome :\t";
		for (int i = 0; i < tempCode.n - tempCode.k; i++) cout << syndrome[i];


		cout << endl << "\n\n............................ checking Error Vector Matrix for syndrome.............................\n ";

		bool flag;
		int position;
		for (int i = 0; i < tempCode.n + 1; i++)
		{
			position = i;
			flag = true;
			for (int j = 0; j < tempCode.n - tempCode.k; j++)
			{
				if (tempCode.syndromeMatrix[i][j] != syndrome[j]) flag = false;
			}
			if (flag) break;
		}



		cout << "\n found the syndrome at position " << position << " in Error Vector table \n ";



		if (position == 0) cout << "\n there is no error in the receieved codeword " << "\n\n ...................searching for recieved code word in code vector table....................... ";
		else
		{
			if (vector[position - 1] == 1)
			{
				vector[position - 1] = 0;
			}
			else
			{
				vector[position - 1] = 1;
			}

			cout << "\n recieved vector corrected to : ";
			for (int i = 0; i < tempCode.n; i++) cout << vector[i];


			cout << "\n\n ...................searching for recieved code word in code vector table....................... ";

		}

		for (int i = 0; i < tempCode.power; i++)
		{
			flag = true;
			for (int j = 0; j < tempCode.n; j++)
			{
				if (vector[j] != tempCode.codeVectors[i][j]) flag = false;
			}
			if (flag)
			{
				position = i;
				break;
			}


		}
		cout << "\n\n received codeword found and can be looked up in code vector table at position " << position + 1 << " and is code word for : ";
			
			for (int j = 0; j < tempCode.k; j++) 
				cout << tempCode.symbols[position][j];
			cout<< endl;
		cout << "\n\n --------------------DO YOU WANT TO CHECK DECODE/CORRECT ANOTHER : ";
		cin >> choice;

	}
}

// a function to generate parity bit matrix from generator function
 void findParity(cyclicCode tempCode, polynomial tempGenFunction )
{ 
	for (int i = 1; i <= tempCode.k; i++)
	{
		polynomial tempPoly(tempCode.n - i), remains(tempGenFunction.trueDegree);

		tempPoly.coefficents[tempCode.n - i] = 1;
		tempPoly.trueDegree = tempPoly.getDegree();

		remains = tempPoly % tempGenFunction;
		
		for (int j = 0, l = (tempCode.n - tempCode.k - 1); j < tempCode.n - tempCode.k, l >= 0; j++, l--)
		{   
			if(remains.trueDegree>=l)
			tempCode.ipMatrix[i - 1][j] = remains.coefficents[l];
		}

		
	}

	
}

cyclicCode::~cyclicCode()
{

}




//*************************************** Function Main


int main()
{
	int n, k, degree;


	cout <<"\n enter the order of code (n,k) first n and then k \t";
	cin >> n >> k;

	cout << "\n enter the degree of generator polynomial\t";
	cin >> degree;

	polynomial genPoly(degree);

	cout << "\n enter the generator polynomial \n\t";
	cin >> genPoly;

	cyclicCode container(n,k);

	

	findParity(container, genPoly);
	container.generateMatrices();
	container.generateCodeVectors();
	decode(container, genPoly);



    return 0;
}

