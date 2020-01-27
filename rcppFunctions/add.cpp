// add.cpp
// Author Rene Locher, ZHAW
// Version 2019-11-12

#include <Rcpp.h>

// [[Rcpp::export]]
int addInt(int x, int y) {
  int sum = x + y;
  return sum;
} // addInt

// [[Rcpp::export]]
double addNum(double x, double y) {
  double sum = x + y;
  return sum;
} // addNum
