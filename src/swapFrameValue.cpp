#include <Rcpp.h>
using namespace Rcpp;

//' @export
// [[Rcpp::export]]
List swapFrameValue(List frame, int i, int j) {
  DoubleVector freqs(frame["frequency"]);
  DoubleVector strengths(frame["strength"]);

  // double temp = freqs[i];
  // freqs[i] = freqs[j];
  // freqs[j] = temp;
  //
  // temp = strengths[i];
  // strengths[i] = strengths[j];
  // strengths[j] = temp;

  std::swap(freqs[i-1], freqs[j-1]);
  std::swap(strengths[i-1], strengths[j-1]);

  frame["frequency"] = freqs;
  frame["strength"] = strengths;

  return frame;
}

