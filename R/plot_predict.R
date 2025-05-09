#' Title Function for individual dynamic prediction plots.
#' @usage plot_predict(object,newdata,ylim,main="Patient",xlab="landmark time points (s)",ylab="5-year survival",legend=NULL,...)
#' @param object An objects inheriting from class "LMf" or "Vs_LM".
#' @param newdata The data.frame.
#' @param ylim The scale range of the vertical axis is, by default, from 0 to 1.
#' @param main Title of the plot, with default being "Patient".
#' @param xlab Label of the horizontal axis, with default being "landmark time points (s)".
#' @param ylab Label of the vertical axis, with default being "5-year survival".
#' @param legend Label of the plot, with default NULL.
#' @param ... extra aguments; currently none is used.
#' @export
#' @examples
#' library(DynamicModel)
#' data(renal)
#' fit<-LMf(renal,tw=list(sl=seq(0, 10, by=0.1),w=5),cov=list(fixed=c("age", "weight", "sex"),vary=c("GFR","proteinuria")),id="id",rtime="yearse",time="time",status="status",inter=TRUE)
#' a1<-VS_LM(fit,cov=list(fixed=c("age", "weight", "sex"),vary=c("GFR","proteinuria")))
#' newdata<-renal[which(renal$id==5619),]
#' plot_predict(object=a1,newdata=newdata,main="")
plot_predict<-function(object,newdata,ylim,main="Patient",xlab="landmark time points (s)",ylab="5-year survival",legend=NULL,...){
  pp<-predict_LM(object,newdata)
  if (missing(ylim)) set_ylim <- TRUE
  else set_ylim <- FALSE
  if(set_ylim) ylim<-c(0,1)
  par(xaxs="i",yaxs="i",mar=c(5,5,3,2))
  plot(pp[,1],pp[,2],type="l",lwd=6,lty=1,xlim=c(min(pp[,1]),max(pp[,1])),ylim=ylim,bty="l",xaxt="n",yaxt="n",xlab="",ylab="")
  axis(1,las=1,pos=0,cex.axis=1.5,tcl=-0.4,padj=-0.3,lwd=1.6,...)
  axis(2,las=1,pos=0,cex.axis=1.5,tcl=-0.4,hadj=0.9,lwd=1.6,...)
  title(main=list(main,font=7,cex=1.5),line=1)
  title(xlab=xlab,font.lab=7,cex.lab=1.5,line=2.6,...)
  title(ylab=ylab,font.lab=7,cex.lab=1.5,line=2.4,...)
  if (is.null(legend)==FALSE)
    legend("topright", legend = legend,lty=1,lwd=6, bty = "n")
}
