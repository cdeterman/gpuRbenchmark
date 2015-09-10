#include <RcppArmadillo.h>

using namespace Rcpp;

// [[Rcpp::export]]
SEXP arma_sgemm(
    const arma::fmat A,
    const arma::fmat B)
{
    arma::fmat C = A * B;
    return wrap(C);
}

// [[Rcpp::export]]
SEXP arma_dgemm(
    NumericMatrix Asexp,
    NumericMatrix Bsexp)
{
    const int N = Asexp.rows();
    const int M = Asexp.cols();
    const int P = Bsexp.rows();
    const int R = Bsexp.cols();
    
    arma::mat A(Asexp.begin(), N, M, false, true);
    arma::mat B(Bsexp.begin(), P, R, false, true);
    
    arma::mat C = A * B;
    return wrap(C);
}
    
// [[Rcpp::export]]
Rcpp::List arma_deigen(
    NumericMatrix Asexp)
{
    const int N = Asexp.rows();
    const int M = Asexp.cols();
    
    arma::mat A(Asexp.begin(), N, M, false, true);
    
    arma::vec eigval;
    arma::mat eigvec;
    
    arma::eig_sym(eigval, eigvec, A);
    
    return Rcpp::List::create(wrap(eigvec), wrap(eigval));
}
    
// [[Rcpp::export]]
List arma_seigen(
    const arma::fmat A)
    {
        arma::fvec eigval;
        arma::fmat eigvec;
        
        arma::eig_sym(eigval, eigvec, A);
        
        return List::create(wrap(eigvec), wrap(eigval));
    }
    