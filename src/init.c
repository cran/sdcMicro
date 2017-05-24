#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP LocalRecProg_cpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Mdav(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP RankSwap(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP Suda2(SEXP, SEXP, SEXP, SEXP);
extern SEXP gowerD_cpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP measure_hierachical(SEXP);
extern SEXP measure_risk_cpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP measure_threshold(SEXP, SEXP);
extern SEXP sdcMicro_cpp_calcSuppInds(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"LocalRecProg_cpp",          (DL_FUNC) &LocalRecProg_cpp,          9},
    {"Mdav",                      (DL_FUNC) &Mdav,                      5},
    {"RankSwap",                  (DL_FUNC) &RankSwap,                  9},
    {"Suda2",                     (DL_FUNC) &Suda2,                     4},
    {"gowerD_cpp",                (DL_FUNC) &gowerD_cpp,                6},
    {"measure_hierachical",       (DL_FUNC) &measure_hierachical,       1},
    {"measure_risk_cpp",          (DL_FUNC) &measure_risk_cpp,          6},
    {"measure_threshold",         (DL_FUNC) &measure_threshold,         2},
    {"sdcMicro_cpp_calcSuppInds", (DL_FUNC) &sdcMicro_cpp_calcSuppInds, 2},
    {NULL, NULL, 0}
};

void R_init_sdcMicro(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
