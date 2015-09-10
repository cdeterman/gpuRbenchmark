#include <RcppEigen.h>


using namespace Rcpp;

// [[Rcpp::export]]
SEXP eigen_sgemm(
    const SEXP A_in,
    const SEXP B_in)
{
	const Eigen::MatrixXf A = as<Eigen::MatrixXf>(A_in);
	const Eigen::MatrixXf B = as<Eigen::MatrixXf>(B_in);
	
  Eigen::MatrixXf C = A * B;
  return wrap(C);
}
    
// [[Rcpp::export]]
SEXP eigen_dgemm(
    const Eigen::Map<Eigen::MatrixXd> A,
    const Eigen::Map<Eigen::MatrixXd> B)
{
    Eigen::MatrixXd C = A * B;
    return wrap(C);
}
    
    
// [[Rcpp::export]]
List eigen_deigen(
    const Eigen::Map<Eigen::MatrixXd> A)
{
    Eigen::VectorXd eigval;
    Eigen::MatrixXd eigvec;
    
    Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> es(A);
    return List::create(wrap(es.eigenvectors()), wrap(es.eigenvalues()));
}
    
    // [[Rcpp::export]]
List eigen_seigen(
    const SEXP A_in)
{
  Eigen::VectorXf eigval;
  Eigen::MatrixXf eigvec;
    
	const Eigen::MatrixXf A = as<Eigen::MatrixXf>(A_in);
    
  Eigen::SelfAdjointEigenSolver<Eigen::MatrixXf> es(A);
  return List::create(wrap(es.eigenvectors()), wrap(es.eigenvalues()));
}
    