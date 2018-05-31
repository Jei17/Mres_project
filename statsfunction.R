summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.99) {
  library(doBy)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

unis <- summarySE(Uni, measurevar = 'Data', groupvars = 'month')

pd <- position_dodge(0.1)

ggplot(unis, aes(x=month, y=Data)) + 
  geom_errorbar(aes(ymin=Data-ci, ymax=Data+ci), width=.1, position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd)

bis <- summarySE(Bi, measurevar = 'Data', groupvars = 'month')

#pd <- position_dodge(0.1)

ggplot(unis, aes(x=month, y=Data)) + 
  geom_errorbar(aes(ymin=Data-ci, ymax=Data+ci), width=.1) +
  geom_line() +
  geom_point()


cox.stuart.test =
  function (x)
  {
    method = "Cox-Stuart test for trend analysis"
    leng = length(x)
    apross = round(leng) %% 2
    if (apross == 1) {
      delete = (length(x)+1)/2
      x = x[ -delete ] 
    }
    half = length(x)/2
    x1 = x[1:half]
    x2 = x[(half+1):(length(x))]
    difference = x1-x2
    signs = sign(difference)
    signcorr = signs[signs != 0]
    pos = signs[signs>0]
    neg = signs[signs<0]
    if (length(pos) < length(neg)) {
      prop = pbinom(length(pos), length(signcorr), 0.5)
      names(prop) = "Increasing trend, p-value"
      rval <- list(method = method, statistic = prop)
      class(rval) = "htest"
      return(rval)
    }
    else {
      prop = pbinom(length(neg), length(signcorr), 0.5)
      names(prop) = "Decreasing trend, p-value"
      rval <- list(method = method, statistic = prop)
      class(rval) = "htest"
      return(rval)
    }
  }
