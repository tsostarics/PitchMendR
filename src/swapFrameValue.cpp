#include <Rcpp.h>
using namespace Rcpp;

//' Swap frame candidates
//'
//' Given a list of frames, swap the candidates at the given 1-indexed positions
//' in place and then return the new value at i
//'
//' @param frame List of frames from a Pitch object
//' @param i first index, must be integer
//' @param j second index, must be integer
//'
//' @export
// [[Rcpp::export]]
double swapFrameValue(List frame, int i, int j) {
  DoubleVector freqs(frame["frequency"]);

  if (i == j)
    return freqs[i-1];

  DoubleVector strengths(frame["strength"]);

  std::swap(freqs[i-1], freqs[j-1]);
  std::swap(strengths[i-1], strengths[j-1]);

  frame["frequency"] = freqs;
  frame["strength"] = strengths;

  return freqs[i-1];
}

