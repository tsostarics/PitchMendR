// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// find_path
Rcpp::List find_path(const Rcpp::List& pitchobj, const Rcpp::CharacterVector& filename);
RcppExport SEXP _PitchMendR_find_path(SEXP pitchobjSEXP, SEXP filenameSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::List& >::type pitchobj(pitchobjSEXP);
    Rcpp::traits::input_parameter< const Rcpp::CharacterVector& >::type filename(filenameSEXP);
    rcpp_result_gen = Rcpp::wrap(find_path(pitchobj, filename));
    return rcpp_result_gen;
END_RCPP
}
// swapFrameValue
double swapFrameValue(List frame, int i, int j);
RcppExport SEXP _PitchMendR_swapFrameValue(SEXP frameSEXP, SEXP iSEXP, SEXP jSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type frame(frameSEXP);
    Rcpp::traits::input_parameter< int >::type i(iSEXP);
    Rcpp::traits::input_parameter< int >::type j(jSEXP);
    rcpp_result_gen = Rcpp::wrap(swapFrameValue(frame, i, j));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_PitchMendR_find_path", (DL_FUNC) &_PitchMendR_find_path, 2},
    {"_PitchMendR_swapFrameValue", (DL_FUNC) &_PitchMendR_swapFrameValue, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_PitchMendR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
