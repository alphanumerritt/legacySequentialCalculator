# NOTES ----
#
# created 4/14/2020 by Merritt Aho 
# modeled closely after work shared here https://deliveroo.engineering/2018/10/22/how-to-experiment-rapidly-without-losing-rigour.html
#

library(shiny)
library(gsDesign)
library(formattable)

shinyServer(function(input, output, session) {
  # VARIABLES ----
  source('service.R', local = TRUE)
  testResults <- data.frame()
  fixedInt <- 0
  zlist <- c()
  nlist <- c()
  ilist <- c()
  cvsAlist <- c()
  cvsBlist <- c()
  nAlist <- c()
  nBlist <- c()
  nonfM <- 0
  
  # NON-INFERIORITY OPPS ----
  # Sets nonfM variable on change of tails and nonf fields  
  observeEvent(
    c(input$tls, input$nonf), {
      nonfM <<- if (input$tls == 1) input$nonf/100 else 0
    }, ignoreNULL = FALSE, ignoreInit = TRUE
  )
  
  # Disables/Enables Nonf field on change of tails field
  observeEvent(input$tls, {toggleState('nonf')}, ignoreNULL = FALSE, ignoreInit = TRUE
  )

  # FIXED RUNTIME ----
  output$fixedHorizon <- renderText({
    fixedInt <<- sampleSize(input$alpha/100,
              input$cvra/100,
              input$mde/100,
              input$tls,
              input$pwr/100,
              input$nonf/100)*2
    fixedSample <- paste(comma(fixedInt, digits=0), 'total')
    fixedSample
  })
  
  # EST DURATION ----
  output$estDuration <- renderText({
    dummy <- input$cvra + input$mde + as.numeric(input$tls) + input$pwr + input$nonf + input$alpha #this is here to force updates on field changes
    duration <- fixedInt / (input$traff / 7)
    duration <- paste(round(duration, digits=0),'days')
    duration
  })
  
  # DYNAMIC CHECKPOINTS ----
  # produces dynamic number of inputs for the test checkpoints based on slider
  output$checkct <- renderUI({
    checks <- as.integer(input$checknum - 1)
    
    lapply(1:checks, function(i) {
      div(class="inputRow",
          div(class="inputLabs",
              h5(paste("Sample at check-in ", i, " (% of ttl.)"))
          ),
          div(class='fixInputs',
      numericInput(
        inputId = paste0("check", i),
        label = "",
        value = round(100 / (1 + checks) * i)
      ))
              )
      
    })
  })
  
  # SHOW/HIDE TABS ----
  observeEvent(input$checknum, {
    checks <- as.integer(input$checknum - 1)
    lapply(checks:9, function(i) {
      #removeTab(inputId = 'rsltstabs', target = paste0('chk',i))
      hideTab(inputId = 'rsltstabs', target = paste0('Chk',i))
    })
    
    lapply(1:checks, function(i) {
      showTab(inputId = 'rsltstabs', target = paste0('Chk',i))
    })
    
  })
  
  # CLEAR RESULTS ----
  observeEvent(input$clrResults, {
    testResults <<- data.frame()
    zlist <<- c()
    nlist <<- c()
    ilist <<- c()
    cvsAlist <<- c()
    cvsBlist <<- c()
    nAlist <<- c()
    nBlist <<- c()
  
    lapply(1:10, function(i) {
      a <- paste0("cvsA",i)
      b <- paste0("sampleA",i)
      c <- paste0("cvsB",i)
      d <- paste0("sampleB",i)
      
      lapply(c(a,b,c,d), function(x) {
        updateNumericInput(session, inputId = x, value = 0)
      })
      
    })
    
     }, ignoreNULL = FALSE, ignoreInit = TRUE)

  # CONFIDENCE INFO ----
  observeEvent(input$confi, {
    toggle(id = 'confp')
    toggle(id = 'confx')
    toggle(id = 'confi')

  })
  
  observeEvent(input$confx, {
    toggle(id = 'confp')
    toggle(id = 'confx')
    toggle(id = 'confi')
    
  })
  
  # POWER INFO ----
  observeEvent(input$pwri, {
    toggle(id = 'pwrp')
    toggle(id = 'pwrx')
    toggle(id = 'pwri')
    
  })
  
  observeEvent(input$pwrx, {
    toggle(id = 'pwrp')
    toggle(id = 'pwrx')
    toggle(id = 'pwri')
    
  })
  
  # CVRA INFO ----
  observeEvent(input$cvrai, {
    toggle(id = 'cvrap')
    toggle(id = 'cvrax')
    toggle(id = 'cvrai')
    
  })
  
  observeEvent(input$cvrax, {
    toggle(id = 'cvrap')
    toggle(id = 'cvrax')
    toggle(id = 'cvrai')
    
  })
  
  # NONF INFO ----
  observeEvent(input$nonfi, {
    toggle(id = 'nonfp')
    toggle(id = 'nonfx')
    toggle(id = 'nonfi')
    
  })
  
  observeEvent(input$nonfx, {
    toggle(id = 'nonfp')
    toggle(id = 'nonfx')
    toggle(id = 'nonfi')
    
  })
  # TECH BITS HIDE/SHOW ----
  observeEvent(input$techi, {
    toggle(id = 'techp')
    toggle(id = 'techx')
    toggle(id = 'techi')
    
  })
  
  observeEvent(input$techx, {
    toggle(id = 'techp')
    toggle(id = 'techx')
    toggle(id = 'techi')
    
  })
  
  # CREDITS HIDE/SHOW ----
  observeEvent(input$credi, {
    toggle(id = 'credp')
    toggle(id = 'credx')
    toggle(id = 'credi')
    
  })
  
  observeEvent(input$credx, {
    toggle(id = 'credp')
    toggle(id = 'credx')
    toggle(id = 'credi')
    
  })
  
  # INTERPRETATION HIDE/SHOW ----
  observeEvent(input$inti, {
    toggle(id = 'intp')
    toggle(id = 'intx')
    toggle(id = 'inti')
    
  })
  
  observeEvent(input$intx, {
    toggle(id = 'intp')
    toggle(id = 'intx')
    toggle(id = 'inti')
    
  })
  
  # MDE INFO ----
  observeEvent(input$mdei, {
    toggle(id = 'mdep')
    toggle(id = 'mdex')
    toggle(id = 'mdei')
    
  })
  
  observeEvent(input$mdex, {
    toggle(id = 'mdep')
    toggle(id = 'mdex')
    toggle(id = 'mdei')
    
  })
  
  # TAILS INFO ----
  observeEvent(input$tlsi, {
    toggle(id = 'tlsp')
    toggle(id = 'tlsx')
    toggle(id = 'tlsi')
    
  })
  
  observeEvent(input$tlsx, {
    toggle(id = 'tlsp')
    toggle(id = 'tlsx')
    toggle(id = 'tlsi')
    
  })
  
  # FIXED N INFO ----
  observeEvent(input$fixedNi, {
    toggle(id = 'fixedNp')
    toggle(id = 'fixedNx')
    toggle(id = 'fixedNi')
    
  })
  
  observeEvent(input$fixedNx, {
    toggle(id = 'fixedNp')
    toggle(id = 'fixedNx')
    toggle(id = 'fixedNi')
    
  })
  
  # SEQ N INFO ----
  observeEvent(input$seqNi, {
    toggle(id = 'seqNp')
    toggle(id = 'seqNx')
    toggle(id = 'seqNi')
    
  })
  
  observeEvent(input$seqNx, {
    toggle(id = 'seqNp')
    toggle(id = 'seqNx')
    toggle(id = 'seqNi')
    
  })
  
  # MAX N INFO ----
  observeEvent(input$maxNi, {
    toggle(id = 'maxNp')
    toggle(id = 'maxNx')
    toggle(id = 'maxNi')
    
  })
  
  observeEvent(input$maxNx, {
    toggle(id = 'maxNp')
    toggle(id = 'maxNx')
    toggle(id = 'maxNi')
    
  })
  
  # GENERATE DESIGN ----
  # Listener for clicks on the Generate Design button 
  observeEvent(
    c(input$doNow,input$doResults, input$clrResults),
    {
      # CHECKPOINTS ----
      # compiles checkpoints for test design inputs
      checks <- as.integer(input$checknum - 1)
      checkpts <-
        list(
          input$check1/100,
          input$check2/100,
          input$check3/100,
          input$check4/100,
          input$check5/100,
          input$check6/100,
          input$check7/100,
          input$check8/100,
          input$check9/100
        )
      checkpts2 <- checkpts[1:checks]
      
      # DESIGN FUNCTION ----
      # calls test design function with specified inputs
      design <<- runDesign(input$alpha/100,
                          input$cvra/100,
                          input$mde/100,
                          input$tls,
                          input$pwr/100,
                          checkpts2,
                          input$nonf/100)
    
      # FORMAT OUTPUTS ----
      # formats sample outputs before going into the outputs
      fixed <- design[[3]] * 2
      fixed <- comma(fixed, digits = 0)
      seq <- design[[4]] * 2
      seq <- comma(seq, digits = 0)
      n1 <- paste0('Fixed-horizon sample size:  ', fixed)
      n2 <- paste0('Sequential max sample size: ', seq)
      n3 <- paste0('Max increase: ', design[[5]], '%')
      
      # SAMPLE OUTPUTS ----
      #output$fixedn <- renderText(paste(fixed))
      output$sequentialn <- renderText(paste(n1, n2, n3, sep = "\n"))
      #output$nchange <- renderText(paste(design[[5]], '%'))
      
    
      # TABLE OUTPUT ----
      tbl1 <- as.data.frame(design[[1]])
      tbl1$checkN <- comma(tbl1$checkN, digits = 0)
      
      daysList <- c()
      daysIndex <- 1
      for (i in tbl1$checkN) {
        daysList[daysIndex] <- paste(ceiling(tbl1$checkN[daysIndex] / (input$traff / 7)),'Days')
        daysIndex <- daysIndex + 1
      }
      tbl1$days <- daysList
      tbl1 <- tbl1[,c('checkPct','checkN','days','lowerZ','upperZ')]

      tbl1$checkPct <- lapply(tbl1$checkPct, as.character)
      tbl1$checkN <- lapply(tbl1$checkN, as.character)
      
      names(tbl1) <-
        c('% of Test',
          'Total Sample',
          'Est. Days',
          'Lower Z-score',
          'Upper Z-score')
          #'Upper Pval',
          #'Lower Pval',
          
      
      
      #output$table <- renderPrint(tbl1)
      
      #tbl2 <- tbl1
      
      
      output$seqTable <- renderTable(tbl1, 
                                     digits = c(0,0,0,0,2,2), 
                                     align = 'c', 
                                     hover = TRUE, 
                                     display = c('d','s','s','s','f','f'),
                                     width = '100%'
                                     )
      
      
      
      # OUTCOME CHECK ----
      # na, inconclusive, win, lose
      
      rsltsRows <- nrow(testResults)
      
      if (rsltsRows < 1) {
        outcome <- "na"
        
      } else if (testResults[rsltsRows,'zscores']>design[[1]]$upperZ[rsltsRows]) {
        outcome <- "winner"

      } else if (testResults[rsltsRows,'zscores']<design[[1]]$lowerZ[rsltsRows]) {
        outcome <- "loser"
        
      } else {
        outcome <- "inconclusive"
      }

      
      # ZSCORE PLOT ----
      output$bounds <- renderPlot({
        designPlot(design[[1]], testResults, outcome) 

      })
    
    show(id = "allOutputs", anim = TRUE, animType = "slide")
    
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  

  # DESIGN PLOT FUNCTION ----
  designPlot <- function(seqTable, rsltsDf, outcome) {
    lim1 <- head(seqTable$checkN,1)/4
    lim2 <- tail(seqTable$checkN,1)+lim1
    rsltsRows <- nrow(rsltsDf)
    
    seqPlot <- ggplot(
      seqTable, 
      aes(checkN),
      size = 8
    ) + geom_line(
      mapping = aes(y = upperZ),
      color = '#456990',
      size = 1.25,
      stat = "identity"
    ) + geom_line(
      mapping = aes(y = lowerZ),
      stat = "identity",
      color = '#e95420',
      size = 1.25
    ) + labs(
      x = "Sample Size",
      y = "Z-score"
    ) + geom_text(
      mapping = aes(x = checkN, y = lowerZ, label = lowerZ),
      size = 5.5,
      nudge_y = -.2
    ) + geom_text(
      mapping = aes(x = checkN, y = upperZ, label = upperZ),
      size = 5.5,
      nudge_y = .2
    ) + geom_point(
      mapping = aes(x = checkN, y = lowerZ),
      colour = "#e95420",
      shape = 20,
      size = 3
    ) + geom_point(
      mapping = aes(x = checkN, y = upperZ),
      colour = "#456990",
      shape = 20,
      size = 3
    ) +  scale_x_continuous(
      breaks = comma(seqTable$checkN, digits = 0)
    ) + coord_cartesian(
      expand = TRUE, 
      clip = "off"
    ) + theme_light(
      base_size = 16
    ) + theme(
      text = element_text(family = 'mono', face = "plain")
    ) + theme(
      axis.title.x = element_text(vjust = -0.99)
    ) + theme(
      panel.grid.minor.x = element_blank()
    ) + theme(
      plot.background = element_rect(fill = "#FCFCFC")
    ) 
    
    
    rsltsLayers <- list(
      geom_point(data = rsltsDf,
                 mapping = aes(samples, zscores),
                 color = '#49beaa',
                 stat = "identity"
      ), geom_text(data = rsltsDf,
                   mapping = aes(samples, zscores, label = zscores),
                   size = 5,
                   colour = "#3c362a",
                   nudge_y = .3
                   )

    )
    
    rsltsLine <- list(
      geom_line(data = rsltsDf,
                             mapping = aes(samples, zscores),
                             stat = "identity",
                             color = '#49beaa',
                             size = 1.25
                )
    )
    
    tryCatch(
      {
    # vars for confidence interval
    NsCi <- seqTable$checkN[1:rsltsRows] # list of sample sizes at check-ins, these are for 1 variation, not total
    ZsCi <- if (rsltsRows > 1) c(seqTable$upperZ[1:(rsltsRows-1)], rsltsDf[rsltsRows,'zscores']) else rsltsDf[rsltsRows,'zscores'] # the upper boundary z scores for all prior check-ins and the calculated z score for the last one
    x1Ci <- rsltsDf[rsltsRows,'cvsAi']    # X1 is number of conversions in control
    x2Ci <- rsltsDf[rsltsRows,'cvsBi']    # X2 is number of conversions in test
    nAci <- rsltsDf[rsltsRows,'nAi']    # n1 is number of samples in control
    nBci <- rsltsDf[rsltsRows,'nBi']    # n2 is number of samples in test
    gsCiZ <- seqTable$upperZ[rsltsRows]    # ciZ is the upper boundary z score for the final check in point

    # confidence interval and pvalue
    adjCi <- confidenceInterval(NsCi, ZsCi, x1Ci, x2Ci, nAci, nBci, gsCiZ, input$tls)
        
      },
      error = function(e) {print("No results")}
    )
    
    loser <- list(
      geom_text(
        mapping = aes(x=lim2, y=lowerZ[1]-1, label="Boundary crossed", family="mono"),
        size = 7,
        hjust = 1,
        colour = "#49beaa"
      )
    )
    
      loser2tail <- list(
        geom_text(
          mapping = aes(x=lim2, y=lowerZ[1] - 1.5, label=paste0('Confidence Level: ',adjCi[1]), family="mono"),
          size = 5,
          hjust = 1,
          colour = "#3c362a"
      ), 
      geom_text(
        mapping = aes(x=lim2, y=lowerZ[1] - 1.9, label=paste0('Confidence Interval: ',adjCi[2], ' - ', adjCi[3]), family="mono"),
        size = 5,
        hjust = 1,
        colour = "#3c362a"
      ))
    
    winner <- list(
      geom_text(
        mapping = aes(x=lim2, y=upperZ[1]+2, label="Boundary crossed", family="mono"),
        size = 7,
        hjust = 1,
        colour = "#49beaa"
      ),
      geom_text(
        mapping = aes(x=lim2, y=upperZ[1] + 1.5, label=paste0('Confidence Level: ',adjCi[1]), family="mono"),
        size = 5,
        hjust = 1,
        colour = "#3c362a"
      ),
      geom_text(
        mapping = aes(x=lim2, y=upperZ[1] + 1.1, label=paste0('Confidence Interval: ',adjCi[2], ' - ', adjCi[3]), family="mono"),
        size = 5,
        hjust = 1,
        colour = "#3c362a"
      ) 
    )
    
    
    
    if (length(rsltsDf)>0) {
      seqPlot <- seqPlot + rsltsLayers
      
      if (nrow(rsltsDf)>1) {
        seqPlot <- seqPlot + rsltsLine
      }
      
      if(outcome == 'winner') {
        seqPlot <- seqPlot + winner
      } else if (outcome == 'loser' && input$tls > 1) {
        seqPlot <- seqPlot + loser + loser2tail
      } else if (outcome == 'loser') {
        seqPlot <- seqPlot + loser
      }
      
    } 
    seqPlot
  }
  

  
  # RESULTS TAB CREATION FUNCTION ----
  popRslts <- function(index) {
    sampleA <- paste0('sampleA',index)
    cvsA <- paste0('cvsA',index)
    sampleB <- paste0('sampleB',index)
    cvsB <- paste0('cvsB',index)
    effect <- paste0('effect',index)
    
    renderUI({
      tagList(
               # INTRO ---
               h4("2. Enter results for selected checkpoint"),
               
               p(
                 "Enter conversions and traffic for test to date. ",
                 "A z-score will output to the boundary plot.",
                 style = "font-size: 12px"
               ),
               
               # N-A INPUT ---
               div(class="inputRow",
                   div(class="inputLabs",
                       h5("Traffic in Control",
                          class = "h5results")
                   ),  
                 numericInput(
                   inputId = sampleA,
                   label = "",
                   value = NULL
                 )
               ),
               
               # CV-A INPUT ---
               div(class="inputRow",
                   div(class="inputLabs",
                       h5("Conversions in Control",
                          class = "h5results")
                   ),
                 numericInput(
                   inputId = cvsA,
                   label = "",
                   value = NULL
                 )
               ),
               
               # test inputs
               # N-B INPUT ---
               div(class="inputRow",
                   div(class="inputLabs",
                       h5("Traffic in Test",
                          class = "h5results")
                   ),
                 numericInput(
                   inputId = sampleB,
                   label = "",
                   value = NULL
                 )
               ),
               
               # CV-B INPUT ---
               div(class="inputRow",
                   div(class="inputLabs",
                       h5("Conversions in Test",
                          class = "h5results")
                   ),
                 numericInput(
                   inputId = cvsB,
                   label = "",
                   value = NULL
                 )
               ),
               
               div(class="outputRow",
               h5(class="h5output", "Calculations",
                  class = "h5results"),
               div(class='field-outputs results',
               verbatimTextOutput(effect)
                   )
   )) })
  }

  # CREATE RESULTS TABS ----
  output$Chk1 <- popRslts(1)
  output$Chk2 <- popRslts(2)
  output$Chk3 <- popRslts(3)
  output$Chk4 <- popRslts(4)
  output$Chk5 <- popRslts(5)
  output$Chk6 <- popRslts(6)
  output$Chk7 <- popRslts(7)
  output$Chk8 <- popRslts(8)
  output$Chk9 <- popRslts(9)
  output$Chk10 <- popRslts(10)
  
  
  # CALC RSLTS FUNCTION ----
  calcRslts <- function(a,b,c,d,index) {
      tryCatch({
        print(nonfM)
        nonfD <- a/b*nonfM
        print(nonfD)
        zindex <- index
        h <- round(((c/d)/(a/b)-1)*100, digits = 2)
        z <- round(-testBinomial(x1=a, x2=c, n1=b, n2=d, delta0 = nonfD), digits = 2)
        if (index < 2) { 
        zlist <<- z
        nlist <<- d+b
        ilist <<- paste0('Chk',index,': ')
        cvsAlist <<- a
        cvsBlist <<- c
        nAlist <<- b
        nBlist <<- d
        
        } else if (index == 10) {
          
          adjIndex <- length(zlist) + 1
          zlist[adjIndex] <<- z
          nlist[adjIndex] <<- d+b
          ilist[adjIndex] <<- paste0('Chk',adjIndex,': ') 
          cvsAlist[adjIndex] <<- a
          cvsBlist[adjIndex] <<- c
          nAlist[adjIndex] <<- b
          nBlist[adjIndex] <<- d
          zindex <- adjIndex
          
          } else {
          
          zlist[index] <<- z
          nlist[index] <<- d+b
          ilist[index] <<- paste0('Chk',index,': ')
          cvsAlist[index] <<- a
          cvsBlist[index] <<- c
          nAlist[index] <<- b
          nBlist[index] <<- d
          
        }
        
        testResults <<- data.frame(
          samples = nlist,
          zscores = zlist,
          chki = ilist,
          cvsAi = cvsAlist,
          cvsBi = cvsBlist,
          nAi = nAlist,
          nBi = nBlist
        )
        
        k <- round(a/b*100, digits = 2)
        l <- round(c/d*100, digits = 2)
        m <- b + d # total sample now
        n <- round((m / (design[[4]] * 2))*100, digits = 0)

        e <- paste0("Control: ", k, "%")
        f <- paste0("Test: ", l, "%")
        g <- paste0("Difference = ", h, "%")
        j <- paste0("Z-score = ", zlist[zindex])
        o <- paste0("% Sampled = ", n, "%")

        paste(e,f,g,j,o, sep = "\n")},
        error = function(e) {print("N/A")})
  
  }

  # CALC RSLTS OUTPUTS ----
  output$effect1 <- renderText({calcRslts(input$cvsA1,input$sampleA1,input$cvsB1,input$sampleB1,1)})
  output$effect2 <- renderText({calcRslts(input$cvsA2,input$sampleA2,input$cvsB2,input$sampleB2,2)})
  output$effect3 <- renderText({calcRslts(input$cvsA3,input$sampleA3,input$cvsB3,input$sampleB3,3)})
  output$effect4 <- renderText({calcRslts(input$cvsA4,input$sampleA4,input$cvsB4,input$sampleB4,4)})
  output$effect5 <- renderText({calcRslts(input$cvsA5,input$sampleA5,input$cvsB5,input$sampleB5,5)})
  output$effect6 <- renderText({calcRslts(input$cvsA6,input$sampleA6,input$cvsB6,input$sampleB6,6)})
  output$effect7 <- renderText({calcRslts(input$cvsA7,input$sampleA7,input$cvsB7,input$sampleB7,7)})
  output$effect8 <- renderText({calcRslts(input$cvsA8,input$sampleA8,input$cvsB8,input$sampleB8,8)})
  output$effect9 <- renderText({calcRslts(input$cvsA9,input$sampleA9,input$cvsB9,input$sampleB9,9)})
  output$effect10 <- renderText({calcRslts(input$cvsA10,input$sampleA10,input$cvsB10,input$sampleB10,10)})


})
