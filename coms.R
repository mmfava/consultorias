coms <- function(qvar=NULL,mma=NULL,siglevel=0.05) {

  ## Create a ordered levels of desired variable
  ## levelsord <- sort(tapply(eval(mma$formula[[2]]),
  ##                            eval(as.name(qvar)),mean,na.rm=TRUE))
  levelsord <- sort(tapply(eval(mma$call[[2]][[2]]),
                           eval(as.name(qvar)),mean,na.rm=TRUE))
  
  
  ## Starting the ordered qvar
  qvarmma <- factor(eval(as.name(qvar)),levels=names(levelsord))

  ## Starting the qvartemp, at begin it is the same of qvarmma
  qvartemp <- qvarmma

  ## Starting the analysis output
  cat("---------------------------------------------------\n")
  cat(paste("--- Starting the contrast analysis at",siglevel*100,"% ---\n"))
  cat("---------------------------------------------------\n")
  
  cont <- 1
  for(i in seq(1,length(levels(eval(as.name(qvar))))-1,by=1)){
    cat("Actual levels in increasing order mean:\n")
    cat("| ")
    cat(paste(levels(qvartemp),"|"))
    cat("\n")
    cat("\n")
    cat(paste("Result of contrast:",levels(qvartemp)[cont],"versus",levels(qvartemp)[cont+1],"\n"))
    cat("\n")
    
    if(length(levels(qvartemp))==2){
      cat(paste(levels(qvartemp)[cont],"and",
                  levels(qvartemp)[cont+1],"are differents\n"))
    }
    else{
      
      levels(qvartemp)[cont] <- paste(levels(qvarmma)[cont],
                                       levels(qvarmma)[cont+1],sep="")
      levels(qvartemp)[cont+1] <- paste(levels(qvarmma)[cont],
                                         levels(qvarmma)[cont+1],sep="")
      
      new.form <- as.formula(gsub(qvar,"qvartemp",as.expression(mma$call[[2]])))

      if(mma$call[[1]]=="lm"||mma$call[[1]]=="aov"){
        environment(mma$terms) <- new.env(parent=environment(as.formula(mma$call[[2]])))
        
        environment(mma$terms)$qvartemp <- qvartemp 
      }
      else{
        environment(mma$formula) <- new.env(parent=environment(as.formula(mma$call[[2]])))
        
        environment(mma$formula)$qvartemp <- qvartemp
      }
    
      mmaa <- update(mma,new.form)
      
      if(mma$call[[1]]=="lm"||mma$call[[1]]=="aov"){
        anova.result <- anova(mma,mmaa)[2,6]
      }
      else{
        if(mma$call[[1]]=="glm"){
          if(mma$family[[1]]=="gaussian"){
            anova.result <- anova(mma,mmaa,test="F")[2,6]
          }
          else{
            anova.result <- anova(mma,mmaa,test="Chisq")[2,5]
          }
        }
      }
      ##cat(paste("P =",anova.result,"\n\n"))
      
      if(anova.result<=siglevel){
        cat(paste(levels(qvarmma)[cont],"and",
                    levels(qvarmma)[cont+1],"are differents\n"))
        cat("---------------------------------------------------\n")

        qvartemp <- qvarmma
        qvarmma <- qvartemp
        
      }
      else{
        cat(paste(levels(qvarmma)[cont],"and",
                    levels(qvarmma)[cont+1],"are not differents\n"))
        cat("---------------------------------------------------\n")
        qvarmma <- qvartemp
        qvartemp <- qvarmma
        cont <- cont-1
      }
    }
    cont <- cont+1
  }
  cat("\n")
  cat("----- Final result of the analysis of contrast -----\n")
  cat("| ")
  cat(paste(levels(qvarmma),"|"))
  cat("\n")
  cat("-------------------------\n")
  cat("Mean by factor levels considering the analysis of contrast\n")
  print(sort(tapply(eval(mma$call[[2]][[2]]),qvarmma,mean,na.rm=TRUE)))
}
