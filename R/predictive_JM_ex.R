#' Title External Validation Function for Joint model-Based Dynamic Prediction Models
#'
#' @param object an objects inheriting from class "jm"
#' @param newdata An external validation dataset with variable names identical to those in the model-building dataset.
#' @param seq_len a numeric vector of future times to calculate predictions
#' @param w prediction window
#'
#' @return A list of class predictive_JM with components:
#' \describe{
#' \item {time} landmark time points
#' \item {cindex} C-index
#' \item {BS} Brier score
#' \item {AUC} AUC
#' }
predictive_JM_ex<-function(object,newdata,seq_len,w){
  c_index<-Brier_score<-AUC<-c()
  for (i in seq_len ){
    c_index<-tvC_index(object,newdata, Tstart =i, Dt =w)
    Brier_score<-tvBrier(object,newdata, Tstart =i, Dt =w)$Brier
    AUC<-tvAUC(object,newdata, Tstart =i, Dt =w)$auc
  }
  return(list(time=seq_len,cindex=c_index,BS=Brier_score,AUC=AUC))
}
