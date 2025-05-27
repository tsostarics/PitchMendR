#include <Rcpp.h>
using namespace Rcpp;

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

