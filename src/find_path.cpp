#include <Rcpp.h>
using namespace Rcpp;

//' @title find path
//'
//' @description creates a data.table by loading the relevant f0 info from the pitch obj
//' @param pitchobj A pitch object (i.e., a List) from rPraat::pitch.read or PitchMendR::pitch.read2
//' @param filename Character vector of length 1, filename to use
//'
//' @export
// [[Rcpp::export]]
Rcpp::List find_path(const Rcpp::List &pitchobj, const Rcpp::CharacterVector &filename) {
  const int nx = static_cast<int>(pitchobj["nx"]);

  LogicalVector is_voiced(nx);
  NumericVector f0(nx);
  IntegerVector zero_index(nx);  // Using a List to store vectors of indices
  CharacterVector file = rep(filename, nx);
  IntegerVector frame_indices(seq_len(nx));
  List frames = pitchobj["frame"];


  for (int i = 0; i < nx; ++i) {
    // file[i] = filename[0];
    List frame = frames[i];
    NumericVector frequency = frame["frequency"];

    f0[i] = frequency[0];
    is_voiced[i] = f0[i] > 1e-8;

    if (!is_voiced[i]) {
      zero_index[i] = 1;
    } else {
      // Note: Assumes there exists a 0 candidate, undefined behavior if there isn't one
      for (int j = 0; j < frequency.size(); ++j) {
        if (frequency[j] < 1) {
          zero_index[i] = j+1;  // R is 1-indexed
          break;
        }
      }
    }
  }

  List dt = List::create(Named("f0") = f0,
                         _["t"] = pitchobj["t"],
                         _["is_voiced"] = is_voiced,
                         _["zero_index"] = zero_index,
                         _["file"] = file,
                         _["frame_i"] = frame_indices);


  dt.attr("class") = Rcpp::CharacterVector::create("data.table", "data.frame");

  return dt;
}

