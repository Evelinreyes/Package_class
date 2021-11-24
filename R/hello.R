## Answer 2

set.seed(123)
x<-matrix(rpois(100,.1), 10,10)

#' To remove rows that only have zero values
#'
#' Description of the function
#'
#' @param dado Is the input data.frame or matrix in which the function is going to remove rows that only have zeroes
#' @export
rm_0s_by_row<-function(dado){
  out<-numeric()
  for(i in 1:nrow(dado)){
    if(length(which(dado[i,]==0))==ncol(dado)){
      out<-c(out,i)
    }
  }
  return(dado[-out,])
}
usethis::use_mit_license()
teste<-rm_0s_by_row(x)

#' To remove columns that only have zero values
#'
#' Description of the function
#'
#' @param dado Is the input data.frame or matrix in which the function is going to remove columns that only have zeroes

#' @export
rm_0s_by_col<-function(dado){
  out<-numeric()
  for(i in 1:nrow(dado)){
    if(length(which(dado[,i]==0))==nrow(dado)){
      out<-c(out,i)
    }
  }
  return(dado[,-out])
}

teste<-rm_0s_by_col(x)
