/**
 * PRAM
 *
 * copyright: Organisation For Economic Co-Operation And Development
 *
 * 	This program is free software; you can redistribute it and/or modify it under the terms of the
 * 	GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 	2.1 of the License, or (at your option) any later version.
 *
 * 	This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
 * 	without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 	See the GNU Lesser General Public License for more details.
 *
 *  The full text of the license is available at http://www.gnu.org/copyleft/lesser.html
 */

#include "glpk.h"	// GLPK library can be downloaded at http://ftp.gnu.org/gnu/glpk/

#ifdef _MSC_VER
	#pragma comment(lib, "glpk.lib")
#endif


//* ======================================================================== *
//* 				                  Enum & Globals
//* ======================================================================== *

typedef double TData;
typedef float TDist;

#define es_Var_Type	e_Var_Double		//Important: es_Var_Type much match TData..!

struct SAttribute
{
	int Strata, Pram;
};

struct SPram
{
	int Row, Frequency;
};


int g_NbRow_Pram, g_NbVar_Pram,
	g_NbStrataVar, g_StrataVarIndex,
	g_NbPramVar, g_PramVarIndex,
	g_OutVarIndex;

int *g_pSorted = NULL;
TData *g_pPramVarWeight = NULL;

TData g_MissingValue_Pram;

Rcpp::NumericMatrix *Mat2;

//* ======================================================================== *
//* 		Sorting by several Vars in descending order, missing values last
//* ======================================================================== *
int g_NbSortVar, g_1stSortVarIndex;

int QsortCompare(const void *pIndexRow1, const void *pIndexRow2)
{
	TData *pRow1 = g_pLocalDataSet->m_pSet + g_pLocalDataSet->m_NbVar * *((int *)pIndexRow1) + g_1stSortVarIndex,
			*pRow2 = g_pLocalDataSet->m_pSet + g_pLocalDataSet->m_NbVar * *((int *)pIndexRow2) + g_1stSortVarIndex;
			
	ForLoopD (i, g_NbSortVar)
	{
		if (pRow1[i] == g_MissingValue_Pram)
		{
			if (pRow2[i] == g_MissingValue_Pram)
				continue;

			return 1;
		}

		if (pRow2[i] == g_MissingValue_Pram)
			return -1;

		if (pRow1[i] < pRow2[i])
			return 1;

		if (pRow1[i] > pRow2[i])
			return -1;
	}

	return 0;
}

void Sort(int FirstVarIndex, int NbVar, int *pSubsetRow = NULL, int NbSubsetRow = 0)
{
	if (!NbSubsetRow)
		NbSubsetRow = g_NbRow_Pram;

	if (pSubsetRow)
	{
		ForLoopD (i, NbSubsetRow)
			g_pSorted[i] = pSubsetRow[i];
	}
	else
	{
		ForLoopD (i, g_NbRow_Pram)
			g_pSorted[i] = i;
	}

	g_NbSortVar = NbVar;
	g_1stSortVarIndex = FirstVarIndex;

	qsort(g_pSorted, NbSubsetRow, sizeof(g_pSorted[0]), QsortCompare);

}


//* ======================================================================== *
//* 				                     Strata
//* ======================================================================== *

int Strata(SAttribute *pAttribute)
{
	Sort(g_StrataVarIndex, g_NbStrataVar);

	int j;
	pAttribute[g_pSorted[0]].Strata = 0;

	for (int i = 1; i < g_NbRow_Pram; ++i)
	{
		TData *pRow = g_pLocalDataSet->m_pSet + g_pLocalDataSet->m_NbVar * g_pSorted[i] + g_StrataVarIndex,
			*pPrevRow = g_pLocalDataSet->m_pSet + g_pLocalDataSet->m_NbVar * g_pSorted[i-1] + g_StrataVarIndex;

		pAttribute[g_pSorted[i]].Strata = pAttribute[g_pSorted[i-1]].Strata;

		ForLoop (j, g_NbStrataVar)
		{
			if (pRow[j] != pPrevRow[j])
				break;
		}

		if (j < g_NbStrataVar)		// Current row different from previous row ?
			++pAttribute[g_pSorted[i]].Strata;
	}

	int NbStrata = pAttribute[g_pSorted[g_NbRow_Pram-1]].Strata + 1;
	return NbStrata;
}

//* ======================================================================== *
//* 				                     Frequency
//* ======================================================================== *

int Frequency(int PramVarIndex, int NbPramVar, SAttribute *pAttribute,
				 SPram *pPram, int *pSubsetRow = NULL, int NbSubsetRow = 0)
{
	Sort(PramVarIndex, NbPramVar, pSubsetRow, NbSubsetRow);

	if (!NbSubsetRow)
		NbSubsetRow = g_NbRow_Pram;

	pAttribute[g_pSorted[0]].Pram = 0;
	pPram[0].Row = g_pSorted[0];

	int j,
		Frequency = 1,
		NbPram = 1;

	for (int i = 1; i < NbSubsetRow; ++i)
	{
		TData *pRow = g_pLocalDataSet->m_pSet + g_pLocalDataSet->m_NbVar * g_pSorted[i] + PramVarIndex,
			*pPrevRow = g_pLocalDataSet->m_pSet + g_pLocalDataSet->m_NbVar * g_pSorted[i-1] + PramVarIndex;

		ForLoop (j, NbPramVar)
		{
			if (pRow[j] != pPrevRow[j])
				break;
		}

		if (j < NbPramVar)		// Current row different from previous row ?
		{
			pPram[NbPram-1].Frequency = Frequency;
			pPram[NbPram].Row = g_pSorted[i];
			++NbPram;
			Frequency = 1;
		}
		else
			++Frequency;

		pAttribute[g_pSorted[i]].Pram = NbPram - 1;
	}

	pPram[NbPram-1].Frequency = Frequency;

	return NbPram;
}

//* ======================================================================== *
//* 				                     Weight Matrix
//* ======================================================================== *

TDist PramDistance(TData *pRow1, TData *pRow2, int NbVar)
{
	TDist Dist = (TDist) 0.0;
	int NbNonMissing = 0;

	ForLoopD (i, NbVar)
	{
		if (pRow1[i] == g_MissingValue_Pram || pRow2[i] == g_MissingValue_Pram)
			continue;

		++NbNonMissing;
		Dist += (TDist) (Abs(pRow1[i] - pRow2[i]) * g_pPramVarWeight[i]);
	}

	if (NbNonMissing)
		Dist /= NbNonMissing;

	return Dist;
}


TDist *ComputeWeightMatrix(SPram *pPram, int NbPram)
{
	TDist *pWeightMat = new TDist[Squared(NbPram)];

	ForLoopD (i, NbPram)
	{
		ForLoopD (j, NbPram)
		{
			TData *pRowI = g_pLocalDataSet->m_pSet + g_pLocalDataSet->m_NbVar * pPram[i].Row + g_PramVarIndex,
					*pRowJ = g_pLocalDataSet->m_pSet + g_pLocalDataSet->m_NbVar * pPram[j].Row + g_PramVarIndex;

			pWeightMat[i * NbPram + j] = PramDistance(pRowI, pRowJ, g_NbPramVar) + 1.0f;
		}
	}

		//=== Swap the weight between the diagonal elements and their adjacent elements in the matrix
	ForLoopD (i, NbPram)
	{
		if (i > 0 && i < NbPram - 1)
		{
//			TDist Weight = pWeightMat[i * NbPram + i + 1];
//			pWeightMat[i * NbPram + i + 1] = pWeightMat[i * NbPram + i];
//			pWeightMat[(i + 1) * NbPram + i] = pWeightMat[i * NbPram + i];
//			pWeightMat[i * NbPram + i] = Weight;

			if (pWeightMat[i * NbPram + i + 1] > pWeightMat[i * NbPram + i - 1])
			{
				TDist Weight = pWeightMat[i * NbPram + i + 1];
				pWeightMat[i * NbPram + i + 1] = pWeightMat[i * NbPram + i];
				pWeightMat[i * NbPram + i] = Weight;
			}

			if (pWeightMat[i * NbPram + i + 1] < pWeightMat[i * NbPram + i - 1])
			{
				TDist Weight = pWeightMat[i * NbPram + i - 1];
				pWeightMat[i * NbPram + i - 1] = pWeightMat[i * NbPram + i];
				pWeightMat[i * NbPram + i] = Weight;
			}

			if (pWeightMat[i * NbPram + i + 1] == pWeightMat[i * NbPram + i - 1])
			{
				TDist Weight = pWeightMat[i * NbPram + i + 1];
				pWeightMat[i * NbPram + i + 1] = pWeightMat[i * NbPram + i];
				pWeightMat[i * NbPram + i - 1] = pWeightMat[i * NbPram + i];
				pWeightMat[i * NbPram + i] = Weight;
			}
		}

		if (i == 0 && i < NbPram - 1)
		{
			TDist Weight = pWeightMat[i * NbPram + i + 1];
			pWeightMat[i * NbPram + i + 1] = pWeightMat[i * NbPram + i];
			pWeightMat[i * NbPram + i] = Weight;
		}

		if (i == NbPram - 1 && i > 0)
		{
			TDist Weight = pWeightMat[i * NbPram + i - 1];
			pWeightMat[i * NbPram + i - 1] = pWeightMat[i * NbPram + i];
			pWeightMat[i * NbPram + i] = Weight;
		}

	}

	return pWeightMat;
}

//* ======================================================================== *
//* 				                 Invariant Pram Matrix
//* ======================================================================== *

TDist *Invariant(TDist *pWeightMat, SPram *pPram, int NbPram)
{
//	TDist *pPramMat = new TDist[Squared(NbPram)];	// Use pWeightMat to stock the results instead of allocating new memory
	int NbConstraint = 2 * Squared(NbPram);
	int i, j,
		*ia = new int[NbConstraint],
		*ja = new int[NbConstraint];
	double *ar = new double[NbConstraint];

		//=== Setting up the minimising problem
	glp_prob *pProblem = glp_create_prob();
	glp_set_prob_name(pProblem, "invariantM");

	glp_set_obj_dir(pProblem, GLP_MIN);

		//=== Setting up the object function and adding constraints #1 p[i,j]>=0 for  i=1, ..., f and j=1, ..., f
	glp_add_cols(pProblem, Squared(NbPram));

	ForLoop (i, NbPram)
	{
		ForLoop (j, NbPram)
		{
			char Name[32];
			snprintf(Name, sizeof(Name), "Col%06d_%06d", i, j);
			int ColIndex = i * NbPram + j + 1;

			glp_set_col_name(pProblem, ColIndex, Name);
			glp_set_col_bnds(pProblem, ColIndex, GLP_LO, 0.0, 0.0);
			glp_set_obj_coef(pProblem, ColIndex, pWeightMat[i * NbPram +  j]);
		}
	}

		//=== Adding constraints #2 for each i=1, ..., f,  1 = sum p[i,j] over j=1, ..., f
	glp_add_rows(pProblem, NbPram);

	ForLoop (i, NbPram)
	{
		char Name[32];
		snprintf(Name, sizeof(Name), "RowTotal%06d", i);
		glp_set_row_name(pProblem, i+1, Name);
		glp_set_row_bnds(pProblem, i+1, GLP_FX, 1.0, 1.0);
	}

		//=== Adding constraints #3 for each j=1, ..., f,  pram_freq[j] = sum p[i,j]*pram_freq[i] over i=1, ..., f
	glp_add_rows(pProblem, NbPram);

	ForLoop (i, NbPram)
	{
		char Name[32];
		snprintf(Name, sizeof(Name), "PramFreq%06d", i);

		int RowIndex = i + NbPram + 1;
		glp_set_row_name(pProblem, RowIndex, Name);
		glp_set_row_bnds(pProblem, RowIndex, GLP_FX, pPram[i].Frequency, pPram[i].Frequency);
	}


		//=== Setting up coefficients in constraints #2

	ForLoop (i, NbPram)
	{
		ForLoop (j, NbPram)
		{
			int ConstraintIndex = i * NbPram + j ;

			ia[ConstraintIndex] = i + 1;
			ja[ConstraintIndex] = i * NbPram + j + 1;
			ar[ConstraintIndex] = 1.0;

		}
	}

		//=== setting up coefficients in constraints #3

	ForLoop (i, NbPram)
	{
		ForLoop (j, NbPram)
		{
			int ConstraintIndex = i * NbPram + j + Squared(NbPram) ;

			ia[ConstraintIndex] = i + NbPram + 1;
			ja[ConstraintIndex] = j * NbPram + i + 1;
			ar[ConstraintIndex] = pPram[j].Frequency;
		}
	}

	glp_load_matrix(pProblem, NbConstraint, ia-1, ja-1, ar-1);		// -1 coz glp doesn't use the index 0..! 8-/

	glp_simplex(pProblem, NULL);

		//=== obtain the final result
	double z = glp_get_obj_val(pProblem);


	ForLoop (i, NbPram)
	{
		ForLoop (j, NbPram)
		{
			int ColIndex = i * NbPram + j + 1;
//			pPramMat[i * NbPram + j] = (TDist) glp_get_col_prim(pProblem, ColIndex);
			pWeightMat[i * NbPram + j] = (TDist) glp_get_col_prim(pProblem, ColIndex);
		}
	}


	glp_delete_prob(pProblem);

	CleanDeleteT(ia);
	CleanDeleteT(ja);
	CleanDeleteT(ar);
//	return pPramMat;
	return pWeightMat;
}

//* ======================================================================== *
//* 				                     FrequencyCheck
//* ======================================================================== *

void FrequencyCheck(int IndexVarV, int IndexVarW, int NbVar)
{
	SAttribute *pAttributeV = new SAttribute[g_NbRow_Pram],
					*pAttributeW = new SAttribute[g_NbRow_Pram];
	SPram *pPramV = new SPram[g_NbRow_Pram],
			*pPramW = new SPram[g_NbRow_Pram];

	ClearMemT(pAttributeV, g_NbRow_Pram);
	ClearMemT(pAttributeW, g_NbRow_Pram);

	int NbPramV = Frequency(IndexVarV, NbVar, pAttributeV, pPramV),
		NbPramW = Frequency(IndexVarW, NbVar, pAttributeW, pPramW);

	BOOL *pZeroFreqV = new BOOL[NbPramV],
			*pZeroFreqW = new BOOL[NbPramW];

	ClearMemT(pZeroFreqV, NbPramV);
	ClearMemT(pZeroFreqW, NbPramW);

	ForLoopD (i, NbPramV)
	{
		ForLoopD (j, NbPramW)
		{
			int k;

			TData *pRowV = g_pLocalDataSet->m_pSet + g_pLocalDataSet->m_NbVar * pPramV[i].Row + IndexVarV,
					*pRowW = g_pLocalDataSet->m_pSet + g_pLocalDataSet->m_NbVar * pPramW[j].Row + IndexVarW;

			ForLoop (k, NbVar)
			{
				if (pRowV[k] != pRowW[k])
					break;
			}

			if (k == NbVar)	// Identical Values ?
			{
				pZeroFreqV[i] = TRUE;
				pZeroFreqW[j] = TRUE;
				break;
			}
		}
	}

	int NbDisappeared = 0,
		NbInvented = 0;

	ForLoopD (i, NbPramV)
	{
		if (!pZeroFreqV[i])
			NbDisappeared += pPramV[i].Frequency;
	}

	ForLoopD (i, NbPramW)
	{
		if (!pZeroFreqW[i])
			NbInvented += pPramW[i].Frequency;
	}


		//=== Clean
	CleanDeleteT(pAttributeV);
	CleanDeleteT(pAttributeW);
	CleanDeleteT(pPramV);
	CleanDeleteT(pPramW);
	CleanDeleteT(pZeroFreqV);
	CleanDeleteT(pZeroFreqW);
}

//* ======================================================================== *
//* 				                     Main
//* ======================================================================== *

void Uninit(void)
{
	CleanDeleteT(g_pSorted);
	CleanDeleteT(g_pPramVarWeight);
	// FIXME:
	//	DeleteDataSet();
}

//extern CGhostDataSet g_GhostDataSet;
RcppExport SEXP Pram(SEXP data, SEXP g_MissingValue_Pram_R, SEXP g_NbStrataVar_R,
SEXP g_pPramVarWeight_R, SEXP seed_R) {
  BEGIN_RCPP
  int i, j, k, l;
  int StartTime = TimeGetMilliSecond();
  Rcpp::NumericMatrix Mat(data);  // creates Rcpp matrix from SEXP
  Mat2 = &Mat;
  g_NbRow_Pram = Mat.rows();
  g_NbVar_Pram = Mat.cols();
  int seed = Rcpp::as<int>(seed_R);
  g_MissingValue_Pram = Rcpp::as<double>(g_MissingValue_Pram_R);
  g_pPramVarWeight = new TData[g_NbVar_Pram];
  Rcpp::NumericVector g_pPramVarWeight_RR(g_pPramVarWeight_R); // creates Rcpp matrix from SEXP
  ForLoop (i, g_NbVar_Pram)
    g_pPramVarWeight[i] = g_pPramVarWeight_RR(i);
  g_NbStrataVar = Rcpp::as<int>(g_NbStrataVar_R);

  // result matrix
  Rcpp::NumericMatrix Res(g_NbEntry, g_NbVar);

  // float g_TopRatio = Rcpp::as<double>(g_TopRatio_R);
  // float g_BottomRatio = Rcpp::as<double>(g_BottomRatio_R);
  // float g_K0 = Rcpp::as<double>(g_K0_R);
  // float g_R0 = Rcpp::as<double>(g_R0_R);
  // float g_P = Rcpp::as<double>(g_P_R);
  if(seed==-1)
    SetRandSeed(TimeGetMilliSecond());
  else
    SetRandSeed(seed);

  double *pNewValue = new double[g_NbRow_Pram],
      *pOrderedValue = new double[g_NbRow_Pram];
  double MinV=0 ,MaxV=0;

	g_pSorted = new int[g_NbRow_Pram];

	if (g_NbStrataVar <= 0)	//============================ Check Frequency
	{

		if (g_NbVar_Pram & 1)
		{
			Uninit();
			return -1;
		}

		g_NbVar_Pram >>= 1;
		FrequencyCheck(0, g_NbVar_Pram, g_NbVar_Pram);
	}
	else							//============================ Pram
	{
		g_NbPramVar = g_NbVar_Pram - g_NbStrataVar;

		if (g_NbPramVar & 1)
		{
			Uninit();
			return -1;
		}

		g_NbPramVar >>= 1;
		g_StrataVarIndex = 0;
		g_PramVarIndex = g_NbStrataVar;
		g_OutVarIndex = g_PramVarIndex + g_NbPramVar;

		//MainPram();
	  SAttribute *pAttribute = new SAttribute[g_NbRow_Pram];
	  SPram *pPram = new SPram[g_NbRow_Pram],
	      *pPramResult = g_Debug ? new SPram[g_NbRow_Pram] : NULL;
	  int *pSubsetRow = new int[g_NbRow_Pram];

	    //=== Compute PramVar Ranges & Divide each Weight by each Range
	  ForLoop (i, g_NbPramVar)
	  {
	    TData MinV, MaxV;
	    MinV = MaxV = Mat(0,g_PramVarIndex + i);//GetValue(g_PramVarIndex + i, 0);

	    for (j = 1; j < g_NbRow_Pram; ++j)
	    {
	      TData Value = Mat(j,g_PramVarIndex + i);//GetValue(g_PramVarIndex + i, j);
	      MinV = Min(MinV, Value);
	      MaxV = Max(MaxV, Value);
	    }

	    if (MaxV != MinV)
	      g_pPramVarWeight[i] /= MaxV - MinV;
	  }

	    //=== Get All Strata & process them
	  int NbStrata = Strata(pAttribute);

	  ForLoop (i, NbStrata)
	  {
	    int NbSubsetRow = 0;

	    ForLoop (j, g_NbRow_Pram)
	    {
	      if (pAttribute[j].Strata == i)
	        pSubsetRow[NbSubsetRow++] = j;
	    }

	    int NbPram = Frequency(g_PramVarIndex, g_NbPramVar, pAttribute, pPram, pSubsetRow, NbSubsetRow);
	    TDist *pWeightMat = ComputeWeightMatrix(pPram, NbPram);
	    Invariant(pWeightMat, pPram, NbPram);

	    //Pram(pWeightMat, pAttribute, pPram, NbPram, pSubsetRow, NbSubsetRow);
	    //* ======================================================================== *
	    //*                              Pram
	    //* ======================================================================== *

	    //void Pram(TDist *pPramMat, SAttribute *pAttribute, SPram *pPram,
	    //      int NbPram, int *pSubsetRow, int NbSubsetRow)
	    //{
	    TDist *pPramMat;
	    pPramMat=pWeightMat;
	        //=== Convert Matrix
	      ForLoop (i, NbPram)
	      {
	        ForLoop (j, NbPram - 1)
	          pPramMat[i * NbPram + j + 1] += pPramMat[i * NbPram + j];
	      }

	      ForLoop (i, NbSubsetRow)
	      {
	        TDist r = (TDist) Random();
	        int MaxPramRow = 0,
	          l = pAttribute[pSubsetRow[i]].Pram;

	          // Now that the values are ordered in descending order
	        for (j = 0; j < NbPram; ++j)
	        {
	          if (r >= pPramMat[l * NbPram + j])
	            MaxPramRow = j+1;
	        }

	        TData *pPramRow = g_pLocalDataSet->m_pSet
	                    + g_pLocalDataSet->m_NbVar * pPram[MaxPramRow].Row
	                    + g_PramVarIndex;

	        ForLoop (j, g_NbPramVar)
	          Res(pSubsetRow[i],j)=pPramRow[j];
	          //SetValue(j, pSubsetRow[i], pPramRow[j]);
	      }
	    //}



	    /////////////

	    CleanDeleteT(pWeightMat);
	  }

	  CleanDeleteT(pSubsetRow);
	  CleanDeleteT(pAttribute);
	  CleanDeleteT(pPram);
	  CleanDeleteT(pPramResult);
	}

		//============================ Uninit
	Uninit();

  return Rcpp::List::create(
      Rcpp::Named( "Res" ) = Res
  ) ;

  END_RCPP

}
