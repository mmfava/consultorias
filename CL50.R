dose <- c(5.00E-05,5.00E-04,5.00E-03,5.00E-02,1)
CL48 <- matrix(c(62,295, 27,331, 19,339, 17,343,20,399),ncol=2,byrow=T)
CL24 = matrix(c(32,325,20,338,9,349,4,355,10,406),ncol=2,byrow = T)

calcula_lc24=function(CL24,dose,lc=0.5,alfa=0.05,func='probit',
                      arred=2){
  if (!is.matrix(CL24)) stop('status must be a matrix with 2 columns')
  else {
    if (dim(CL24)[2]!=2)stop('The status matrix must have 2 columns')
  }
  #A function to invert the linear function from a model object, i.e. x=(y-a)/b
  invlin=function(x,y){
    return((y-x$coef[1])/x$coef[2])
  }
  # The model itself with the matrix as the outcome and the 
  # concentration vector as the independent variable - the link function
  # is probit by default, but other links of the binomial family can be
  # used, e.g. logit
  modelo=glm(CL24~dose,family=binomial(func))
  # Getting the significance fot the model
  signif=as.numeric(anova(modelo,test='Chisq')[2,c(1,4:5)])
  # Getting the point estimate from the model for the desired LC (default =50%-0.5)
  fit=as.numeric(invlin(modelo,family(modelo)$linkfun(lc)))
  # Getting the standard error of the estimate, in the linf function scale
  erro=predict(modelo,se.fit=T,type='response',
               newdata=data.frame(dose=fit))$se.fit
  # Calculating the lower and upper CI based on the error and the 
  # chosen alpha (defaults to 95% CI)
  lwr=invlin(modelo,family(modelo)$linkfun(lc+qnorm(alfa/2)*erro))
  upr=invlin(modelo,family(modelo)$linkfun(lc-qnorm(alfa/2)*erro))
  #Packaging everything in an object to be returned
  lc_resp=round(data.frame(lc,fit,lwr,upr),arred)
  # For the ANOVA, special roundin is required
  lc_resp=cbind(lc_resp,df=round(signif[1]),
                chisq=round(signif[2],2),pval=signif[3])
  # Returning the object
  return(lc_resp)
}
calcula_lc24(CL24,dose)

CL24=matrix(c(rep(0,9),2,6,2,7,1,6,6,4,5,9,4,6,7,7,6,10,10,10,rep(10,9),8,
              4,8,3,9,4,4,6,5,1,6,4,3,3,4,0,0,0),ncol=2,byrow = F)
dose=rep(c(0,1000,2000,3000,4000,5000,10000,15000,20000),each=3)
