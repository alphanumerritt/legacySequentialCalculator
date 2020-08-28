# FIXED SAMPLE SIZE
sampleSize <- function(conf, base, mde, tls, pwr) {
  cvrB = base * (1 + mde)
  alph = 1 - conf
  tls = as.integer(tls) #for some reason tls gets interpreted as a string w/o this in the gsDesign functions
  sides = if (tls > 1) "two.sided" else "one.sided"
  
  # FIXED SAMPLE ----
  return(power.prop.test(n = NULL, p1 = base, p2 = cvrB,
                            sig.level = alph,
                            power = pwr,
                            alternative = sides)$n)
}

# SEQUENTIAL TEST DESIGN
runDesign <- function(conf, base, mde, tls, pwr, checks) {
  
  # VARIABLES ----
  cvrB = base * (1 + mde)
  alph = 1 - conf
  tls1 = as.integer(tls) #for some reason tls gets interpreted as a string w/o this in the gsDesign functions
  upBnd = 3 #upper boundary exponent value (higher is more conservative, typically use 2 or 3)
  lowBnd = 2 #lower boundary exponent values (higher is more conservative, typically use 2 or 3)
  k_checks <- length(checks) + 1
  sides = if (tls1 > 1) "two.sided" else "one.sided"
  testType = if (tls1 > 1) 2 else 4
  
  # FIXED SAMPLE ----
  n_fixed = power.prop.test(n = NULL, p1 = base, p2 = cvrB,
                            sig.level = alph,
                            power = pwr,
                            alternative = sides)$n
  
  # 1ST DESIGN ----
  # initital gsDesign object with evenly spaced checkpoints,
  # used to calculate the maximum samples we would require
  design = gsDesign(k=k_checks, test.type = testType, alpha = alph/tls1, 
                    sfu=sfPower, sfupar = upBnd, sfl=sfPower, 
                    sflpar=lowBnd, n.fix = n_fixed,   beta = 1 - pwr)
  
  # MAX SAMPLE PER VARIATION ----
  n_sequential = tail(design$n.I, 1)
  
  # ANALYSIS CHECKPOINTS ----
  checkpoints <- list()
  for (i in checks) {
    checkpoints <- c(checkpoints, ceiling(n_sequential * as.numeric(i)))
  }
  
  checkpoints <- c(checkpoints, ceiling(n_sequential))
  checkpoints <- as.numeric(checkpoints)
  
  # 2ND DESIGN ----
  # Generates updated gsDesign object with chosen checkpoints
  finalDesign <- gsDesign(k=k_checks, test.type = testType, alpha = alph/tls1, sfu=sfPower, 
                          sfupar = upBnd, sfl=sfPower, sflpar=lowBnd, n.fix = n_fixed,  
                          n.I = checkpoints, beta = 1 - pwr)

  
  # DATA TABLE ----
  # Creates a table of key data from the design output
  designSum <- data.frame(
    finalDesign$timing,
    finalDesign$n.I * 2,
    finalDesign$lower$bound,
    finalDesign$upper$bound
  )
  
  # PVALUES ----
  # Adds pvalues and formats the table a bit
  names(designSum) <- c('checkPct','checkN','lowerZ','upperZ')
  designSum$'checkPct' <- percent(designSum$'checkPct', 0)
  designSum$'lowerZ' <- round(designSum$'lowerZ', digits = 2)
  designSum$'upperZ' <- round(designSum$'upperZ', digits = 2)
  designSum$'upperPval' <- round(1-pnorm(designSum$'upperZ'), digits = 4)
  designSum$'lowerPval' <- round(1-pnorm(designSum$'lowerZ'), digits = 4)

  # SAMPLE INCREASE ----
  # max sample size increase
  testChange <- round((n_sequential / n_fixed - 1) * 100,2)

  # PAYLOAD ----
  # compiles final payload
  payload <-  list(designSum, finalDesign, n_fixed, n_sequential, testChange)
  
  return(
    payload
  )
}

# CONFIDEND INTERVAL AND PVAL
confidenceInterval <- function(Ns, Zs, xA, xB, nA, nB, ciZ, tls) {
  tls = as.integer(tls) #for some reason tls gets interpreted as a string w/o this in the gsDesign functions
  # Ns is list of sample sizes at check-ins, these are for 1 variation, not total
  # Zs are the upper boundary z scores for all prior check-ins and the calculated z score for the last one
  # X1 is number of conversions in control
  # X2 is number of conversions in test
  # n1 is number of samples in control
  # n2 is number of samples in test
  # ciZ is the upper boundary z score for the final check in point
  
  # Adjusted p-value once a critical boundary has been crossed
  probs <- gsProbability(k=length(Ns), theta=0, n.I=Ns, a=array(-20,length(Ns)),b=Zs)
  
  pval <- sum(probs$upper$prob) * tls

  conf <- abs(round((1-pval)*100, digits=1))
  textConf <- paste0(conf,'%')
  
  # Compute adjusted CI 
  ci <- ciBinomial(x1=xB, x2=xA, n1=nB, n2=nA,
                   alpha=2*(1-pnorm(ciZ)))

  ci$lower <- round(ci$lower/(xA/nA)*100, digits=1)
  ci$upper <- round(ci$upper/(xA/nA)*100, digits=1)
  
  textLci <- paste0(ci$lower,'%')
  textUci <- paste0(ci$upper,'%')
  
  ciPayload <- list(textConf, textLci, textUci)
  
  return(
    ciPayload
    )
}


