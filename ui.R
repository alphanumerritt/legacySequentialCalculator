library(shinythemes)
library(shinyjs)



# UI START -----
ui <- fluidPage(
  theme = shinytheme("united"),
  # setup shiny js
  useShinyjs(),
  
  # COMMENT TAG ----
  HTML("
    <!-- 
    # If you're curious enough to find this note. We share more than one thing in common. 
    # If you want to reverse engineer anything here, please don't hesitate to reach out: merrittaho@searchdiscovery.com.
    -->
  "),
  
    # HEAD TAG ----
  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lato|Cabin:400,700');
      
      body {
        font-family: Lato; 
        background-color: #FCFCFC;
        max-width: 1500px;
      }
        
      a {color: #337ab7;}
      
      a:hover, a:focus {color: #23527c;}
      
      h1, h2, h3, h4, h5, h6 {font-family: Lato;}
      
      h1 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #48ca3b;
      }
      
      h4 {margin-top: 25px;}
      
      h5 {
        font-weight: 600;
        font-family: 'Lato';
        width: 225px;
        display: inline-block;
        margin-right: 15px;
      }
      
      .h5output {
        position: absolute;
        top: 0px;
        left: 0px;
      }
      
      .h5results {
        width: 150px;
      }
      
      #col1, #col2 {
        background-color: #f5f5f5;
        border: 1px solid #cccccc;
        border-radius: 4px;
        margin: 5px 0px 15px 10px;
        padding: 30px;
        min-width: 480px;
      }
      
      #col3, #col4 {
        border: 1px solid #cccccc;
        border-radius: 4px;
        margin: 5px 0px 10px 10px;
        padding: 30px;
        display: inline-block;
      }
      
      @media screen and (max-width: 1000px) {
        .col-sm-6 {
        clear: both;
        }
      }
      
      @media screen and (max-width: 1015px) {
        .col-sm-7, .col-sm-5 {
          clear: both;
        }
      }

      #allOutputs {
        max-width: 1200px;
        min-width: 480px;
      }
      
      .form-control.shiny-bound-input {width: 110px; display: inline;}
      
      .shiny-options-group, .radio {display: inline;}
      
      .radio {padding-left: 10px; margin-right: 5px;}
      
      .field-outputs {
        display: inline-block;
        padding-left: 245px;
      }
      
      .field-outputs.sampleBlock {padding-left: 0px;}
      
      .field-outputs.results {
        padding-left: 170px;
      }
      
      .shiny-bound-output {
        display: inherit;
        margin: 0px;
        min-width: 150px;
      }
      
      .nav-pills.shiny-bound-input {width: 100%;}
      
      .pinline {
        display: inline-block;
        padding-left: 10px;
      }
      
      #mainContent {max-width: 1250px; min-width: 300px;}
      
      #rsltsInner {border: 1px solid #cccccc; border-radius: 4px;}
      
      #techp {padding: 0px 10px 0px 10px;}
      
      .cta {display: inline-block; padding-left: 20px;}
      
      .infoP {font-size: 12px; color: #676666; clear: left}
      
      #doNow, #doResults {background-color: #337ab7} 
      
      #doNow:hover, #doResults:hover {background-color: #337ab799} 
      
      .secondary > button {background-color: #ffffff; color: #333;}
      
      .secondary:hover > button {background-color: #978e83; color: #ffffff;}
      
      .ctasContainer {padding-top: 20px; margin: auto;}
      
      .shiny-input-container:not(.shiny-input-container-inline) {
        width: 200px;
        display: inline;
      }
      
      #seqTbl {
        margin: 20px 0 20px 0;
        border: 1px solid #cccccc;
        border-radius: 4px;
        background-color: #ffffff;
      }
      
      .table {
        margin-bottom: 0px;
      }
      
      #feedback, #credits {
        margin-left: 20px;
        margin-top: 10px;
        margin-bottom: 30px;
        margin-right: 30px;
        display: block;
      }
      
      .inputContainer {display: inline;}
      
      .fixInputs {display: inline-block;}
      
      .inputRow {display: block; padding-bottom: 15px;}
      
      .outputRow {
        display: block;
        position: relative;
        padding-bottom: 15px;
      }
      
      i {padding-left: 5px;}
      
      .outputIcons {position: relative; top: -13px; display: inherit;}
      
      .outputIcons.outputSample {top: -55px;}
      
      .inputLabs {display: inline-block;}
      
      #checkSlider {max-width: 300px;}
      
      #boundaryPlot {margin-top: 30px;}
      
      #designTable {max-width: 600px;}
      
      .rsltsNote {padding: 20px 10px 0px 10px;}
      
      .infopop {padding: 20px 0 5px 0;}
      

    "))
  ), # close head
  
  
  # PAGE TITLE ----
  #headerPanel("New Application"),
  titlePanel("Design & Analyze a Sequential Test"),
  
  # MAIN CONTENT ----
  div(id = 'mainContainer',
      fluidRow(
        
        # DESIGN PANEL ----
        column(
          width = 6,
          div(id = "col1",
              # INTRO TEXT ----
              h3("Input test parameters"),
              
              p(
                "Begin planning your sequential test by inputting the same",
                "parameters as those for any other fixed-horizon test.",
              ),
              
              div(class="inputContainer",
                  
                  
                  # CONFIDENCE INPUT ----
                  # Input: confidence level in test design or 1 - alpha (type 1 error)
                  div(class="inputRow",
                      div(class="inputLabs",
                          h5("Confidence Level (%)")
                      ),
                      
                      div(class='fixInputs',
                          numericInput(
                            inputId = "alpha",
                            label = "",
                            value = 95,
                            min = 1,
                            max = 100
                          ),
                          
                          actionLink('confi', label = " ", icon = icon('info-circle')),
                          hidden(actionLink('confx', label = " ", icon = icon('times'))),
                      ),
                      hidden(
                        p(
                          id = 'confp',
                          class = 'infoP',
                          "Confidence Level = (1 - alpha). It is your tolerance for",
                          "Type 1--false positive--errors in your test. 95% Confidence",
                          "Level means 5% of flat tests will show as statistically significant",
                          "result."
                        )
                      )
                  ), # close confidence input 
                  
                  
                  # POWER INPUT ----
                  # Input: power or 1 - beta (type 2 error)
                  div(class="inputRow",
                      div(class="inputLabs",
                          h5("Power (%)")
                      ),
                      div(class='fixInputs',
                          numericInput(
                            inputId = "pwr",
                            label = "",
                            value = 80,
                            min = 1,
                            max = 100
                          ),
                          actionLink('pwri', label = " ", icon = icon('info-circle')),
                          hidden(actionLink('pwrx', label = " ", icon = icon('times'))),
                      ),
                      hidden(
                        p(
                          id = 'pwrp',
                          class = 'infoP',
                          "Power = (1 - beta). It is your tolerance for",
                          "Type 2--or false negative--errors in your test. 80% Power",
                          "means a difference equal to the size of your MDE will",
                          "be observed 80% of the time it is present."
                        )
                      )
                  ),
                  
                  # CVR INPUT ----
                  # Input: base conversion rate considered for test design
                  div(class="inputRow",
                      div(class="inputLabs",
                          h5("Base conversion rate (%)")
                      ),
                      div(class='fixInputs',
                          numericInput(
                            inputId = "cvra",
                            label = "",
                            value = 10,
                            min = 1,
                            max = 100
                          ),
                          actionLink('cvrai', label = " ", icon = icon('info-circle')),
                          hidden(actionLink('cvrax', label = " ", icon = icon('times'))),
                      ),
                      hidden(
                        p(
                          id = 'cvrap',
                          class = 'infoP',
                          "Enter the current conversion rate of the audience you will be",
                          "testing on. Or guess. Note, lower conversion rates require",
                          "greater sample sizes to measure similar differences."
                        )
                      )
                  ),
                  
                  # MDE INPUT ----
                  # Input: MDE or minimum effect of interest
                  div(class="inputRow",
                      div(class="inputLabs",
                          h5("Minimum effect of interest (%)")
                      ),
                      div(class='fixInputs',
                          numericInput(
                            inputId = "mde",
                            label = "",
                            value = 10,
                            width = "85%",
                            min = 0,
                            max = 1000
                          ),
                          actionLink('mdei', label = " ", icon = icon('info-circle')),
                          hidden(actionLink('mdex', label = " ", icon = icon('times'))),
                      ),
                      hidden(
                        p(
                          id = 'mdep',
                          class = 'infoP',
                          "Minimum Effect of Interest a.k.a. Minimum Detectable",
                          "Effect is the smallest true difference in conversion",
                          "that your test will be designed to detect. ",
                          "Smaller effects will be observed with higher Type 2 ",
                          "error rates."
                        )
                      )
                  ),
                  
                  # TAILS INPUT ----
                  # Input: tails considered in test 
                  div(class="inputRow",
                      div(class="inputLabs",
                          h5("Tails in the test (fixed)")
                      ),
                      div(class='fixInputs',
                          radioButtons(
                            "tls",
                            "",
                            choices = list("1-tail" = 1, "2-tail" = 2),
                            selected = 1
                          ),
                          actionLink('tlsi', label = " ", icon = icon('info-circle')),
                          hidden(actionLink('tlsx', label = " ", icon = icon('times'))),
                      ),
                      hidden(
                        p(
                          id = 'tlsp',
                          class = 'infoP',
                          "Selecting 1-tail for a 'superiority test' will produce a ",
                          "futility boundary, allowing you to end the test early when test",
                          " results are poor and a comeback is not likely while preserving power. ",
                          "Selecting 2-tail will produce",
                          " identical (symmetrical) upper and lower decision boundaries."
                        )
                      )
                  ),
                  
                  # NONINFERIORITY INPUTS ----
                  # Input: Option to add a non-inferiority margin to the test as well
                  div(class="inputRow",
                      div(class="inputLabs",
                          h5("Non-inferiority Margin (%)")
                      ),
                      div(class='fixInputs',
                          numericInput(
                            "nonf",
                            "",
                            value = 0,
                            min = 0,
                            max = 50
                          ),
                          actionLink('nonfi', label = " ", icon = icon('info-circle')),
                          hidden(actionLink('nonfx', label = " ", icon = icon('times'))),
                          p(class="pinline","[Optional] ")
                      ),
                      hidden(
                        p(
                          id = 'nonfp',
                          class = 'infoP',
                          "If you want to demonstrate that the test variant is better than",
                          "some negative margin below the control, enter it here as a ",
                          "relative % of the control conversion rate. I.e. 0.5 means",
                          "the test conversion rate will be compared to ControlCvr*0.995.",
                          "Note: any value assigned will be ignored in a 2-tailed experiment."
                        )
                      )
                  ),
                  
                  # TRAFFIC INPUTS ----
                  div(class="inputRow",    
                      div(class="inputLabs",
                          h5("Average weekly traffic to test page(s)")
                      ),    
                      div(class='fixInputs',
                          numericInput(
                            inputId = "traff",
                            label = NULL,
                            value = 10000
                          )
                      ),
                      p(class="pinline","[Optional] ")
                  ),
                  
                  # FIXED SAMPLE ----
                  div(class="outputRow",
                      h5(class="h5output", "Sample size with fixed-horizon test"),
                      div(class="field-outputs",
                          verbatimTextOutput('fixedHorizon'))
                  ),
                  div(class="outputRow",
                      h5(class="h5output", "Expected test duration with fixed-horizon design"),
                      div(class="field-outputs",
                          verbatimTextOutput('estDuration'))
                      
                  ),
                  p(class = 'infoP',
                    "If a normal fixed-horizon test would run for 2-weeks or less, ",
                    "then a sequential test probably isn't adviseable. We recommend running",
                    "every test for at least 2 weeks in order to cover a few business cycles."
                    )
              ) 
              
          )),
        
        
        # SEQUENTIAL ELEMENTS ----
        column(
          width = 6,
          div(id = "col2",
              h3("Plan check-ins to make it 'Sequential'"),
                         p("The essence of a sequential test is performing intermittent analyses of the data.",
                           "These analyses should be planned in good faith ahead of test launch.",
                           "Don't worry, if your number and timing of analyses don't match up perfectly.",
                           "You can realign the test design later."),
                         # CHECKS INPUT ----
                         #Input: how many checks you want to do
                         h5("Number of check-ins (including final)"),
                         div(id="checkSlider",
                             sliderInput(
                               "checknum",
                               NULL,
                               min = 2,
                               max = 10,
                               value = 4
                             )),
                         
                         # p(class = 'infoP',
                         #   "In the literature, this is your k parameter."
                         # ),
                         
                         # DYNAMIC CHECKS ----
                         # dynamic input fields based on slider input above
              h4("If desired, adjust the timing of check-ins"),
                         uiOutput('checkct'),
                         
                         p(class = 'infoP',
                           "Pro tip: you might want to plan your first check for early in the test.",
                           "This could help identify problematic test experiences and prevent loss."
                         ),
                         
                         # GENERATE CTA ----
                         # Action button to refresh test design
                         div(class="ctasContainer",
                             div(class = "cta",
                                 actionButton("doNow", "View Test Design")
                             ))


              
              
          ))),
      
      # OUTPUTS ----
      fluidRow(
        
        # HIDE/SHOW ----
        hidden(
          tags$div(id = "allOutputs",
                   div(id="outputColumns",
                   column(width = 7,
                        div(id="col3", 
                          
                          
                   
                   h3("Your Sequential Test Design"),
                   # INTRO ----
                   p(
                     "Here are the parameters of your sequential test design. If you enter results",
                     ", those will be plotted in the boundary chart as well."
                   ),
                   
                   # SAMPLE SIZE ----
                   # row with sample size outputs
                   div(class='outputRow',
                       #h5(class="h5output", 'Sample size details'),
                       div(class="field-outputs sampleBlock",
                           verbatimTextOutput("sequentialn"),
                           
                           
                           div(class="outputIcons outputSample",
                               actionLink('seqNi', label = " ", icon = icon('info-circle')),
                               hidden(actionLink('seqNx', label = " ", icon = icon('times'))),
                           ))),
                   hidden(
                     p(
                       id = 'seqNp', class = 'infoP',
                       "Remember, most sequential tests won't run to their max sample size.",
                       "This is a worst case scenario, most likely to occur if observed lifts are",
                       " between 0 (null) and your MDE/MEI.",
                       tags$a(href="https://cxl.com/blog/peeking-sequential-testing/", target="_blank", 
                              "Click here"),
                       " to learn a bit more about how much savings to expect from a sequential test."
                     )
                   ),
                   

                   # TABLE ----
                   # output for test design table
                   div(id='designTable',
                       h4("Sequential test design details"),
                     p(
                       "The table below contains all the information you need to run a sequential test.",
                       "Each row represents a checkpoint in the test analysis."
                     
                   ),
                       #verbatimTextOutput("table"),
                   div(id = 'seqTbl',
                      tableOutput('seqTable')
                       
                   )),    
                   
                   
                   # PLOT ----
                   # output for plot of the design
                   div(id='boundaryPlot',
                       h4("Decision boundary plot"),
                       plotOutput("bounds")
                   ),
                   # RSLTS TECH BITS ----
                   div(class="infopop",
                       actionLink('inti', label = "How do I interpret all this?", icon = icon('info-circle')),
                       hidden(actionLink('intx', label = "Close", icon = icon('times')))
                   ),
                   
                   hidden(
                     div(
                       id = 'intp',
                       p(
                       "The table and chart above provide you with all the information you need to judge test results ",
                       "at each of your planned analysis check-ins. The z-score is the key test statistic here.",
                       "Statistical significance is achieved when the z-score exceeds the corresponding upper boundary z-score or falls",
                       " below the corresponding lower boundary z-score."
                       ),
                       p(
                       "You are likely more accustomed to seeing p-values, which are derived from the z-score. But we don't show",
                       " the p-value because it cannot be interpretted as it normally would be in a fixed-horizon test.",
                       " In other words, we're trying to avoid misinterpretation and statistical faux pas."
                       ),
                       p(
                       "When you enter results that cross a decision boundary (except in the case of a 1-tail futility boundary),",
                       " we provide you with an adjusted ",
                       "Confidence Level and Confidence Interval corresponding to the confidence level chosen for your deisgn."
                       ),
                       p(
                         "The upper decision boundary, or efficacy boundary requires you to stop the test once the boundary has been crossed.",
                         "This is know as a 'binding' boundary. In a 1-tail test, the lower boundary, however, is 'non-binding' which means",
                         " when crossed, you may decide whether to end the test early or to continue. In a 2-tail test, both boundaries are binding."
                       )
                     )
                   )
                   
                   )),
                   # ANALYSIS COLUMN ----
                   column(width = 5,
                          div(id="col4", 
                              
                          h3("Analyze your test results"),
                              h4("1. Select a checkpoint"),
                          
                          tabsetPanel(id='rsltstabs', 
                                      type='pills', 
                                      
                                      # RESULTS TABS ----
                                      tabPanel('Chk1', uiOutput('Chk1')),
                                      tabPanel('Chk2', uiOutput('Chk2')),
                                      tabPanel('Chk3', uiOutput('Chk3')),
                                      tabPanel('Chk4', uiOutput('Chk4')),
                                      tabPanel('Chk5', uiOutput('Chk5')),
                                      tabPanel('Chk6', uiOutput('Chk6')),
                                      tabPanel('Chk7', uiOutput('Chk7')),
                                      tabPanel('Chk8', uiOutput('Chk8')),
                                      tabPanel('Chk9', uiOutput('Chk9')),
                                      tabPanel('Final', uiOutput('Chk10'))
                                      
                          ),
                          # RESULTS CTA ----
                          # Action button to refresh test design
                          div(class="ctasContainer",
                              div(class = "cta",
                                  actionButton("doResults", "Plot Results")),
                              div(class = "cta secondary",
                                  actionButton("clrResults", "Clear Results"))
                          ),  
                   # NOTICE ----
                   # cautionary note for test analysis
                   p(class="rsltsNote",
                     "NOTE: When analysis timing does not line up with planned timing, ",
                     "use the '% sampled' value from the results calculations and update the test ",
                     "design to match. This is standard practice. However, changing the number ",
                     "of interim checks impacts max sample size and should be avoided. "),
                   p(class="rsltsNote",
                     "Also, standard Confidence Interval and p-value calculations will be understated and ",
                     "cannot be taken at face value. We provide adjusted p-values and confidence intervals when",
                     "the upper (efficacy) boundary is crossed."
                   ),
                   
                   # TECH BITS ----
                   div(class="infopop",
                       actionLink('techi', label = "Some technical bits", icon = icon('info-circle')),
                   hidden(actionLink('techx', label = "Close", icon = icon('times')))
                       ),
                      
                   hidden(
                     p(
                       id = 'techp',
                       "This application uses the R package",
                       tags$a(href="https://cran.r-project.org/package=gsDesign", target="_blank", 
                              "gsDesign"),
                       " to compute the sequential test sample sizes and decision boundaries.", 
                       "You can find some educational documentation on the methods", 
                       "(not just help modules)",
                       tags$a(href="https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/gsDesign/inst/doc/gsDesignManual.pdf?revision=382&root=gsdesign", target="_blank", 
                              "here"),
                       ". There are, in fact, many ways to formulate and customize a sequential test with decision boundaries.",
                       " The approaches differ in terms of the shape of the boundary curve (or lack thereof) and how ",
                       " conservative the approach to alpha and beta spending. For 1-tail tests, we use test.type=4, sfu=sfPower, sfupar=3, and sflpar=2 ",
                       "as parameters for the design function. These specify a Kim-Dements power spending function with a non-binding lower boundary ",
                       "and a binding upper boundary as well as a more conservative (harder to pass)",
                       "early upper boundary than lower boundary. For 2-tail tests, we use test.type=2 instead which makes both boundaries binding."
                     )
                   )
                          )))
                   
                   
                   
                   
                   
                   
                   # END TAGS ----
          )
          
      )
        ),
      fluidRow(
        # FEEDBACK ----
        div(id = 'feedback',
            "Thanks for using! We welcome ",
            tags$a(href="https://docs.google.com/forms/d/e/1FAIpQLSfHiI150WWY1cTTE1UaKHKuNxoeL9zxck1v-UZWRkWl1eVsQw/viewform", 
                   target="_blank", 
                   "your feedback.")
        ),
        # CREDITS ----
        div(id = 'credits',
        div(class="creditspop",
            actionLink('credi', label = "Credits", icon = icon('info-circle')),
            hidden(actionLink('credx', label = "Close", icon = icon('times')))
        ),
        
        hidden(
        div(id = 'credp',
            p(
            'This application was only made possible by the work and contributions of so many others. ',
            'Many academic adventurers did the real work on developing the statistical methods we all rely on. ',
            'Pocock, O\'Brien, Flemming, Lan, Kim, Demets, Jennison, Turnbill and many others blazed the trails. '
            ),
            p(
              'Georgi Georgiev synthesized the academics\' work and appropriated it for online a/b testing. ',
            'His body of literature over at',
            tags$a(href="https://www.analytics-toolkit.com", 
                   target="_blank", 
                   "analytics-toolkit.com"),
            ' provides an excellent introduction to the statistics and a guidebook on how to use them. ',
            'We based the alpha spending functions in our calculator on those documented in his AGILE method. ',
            'Also, his highly affordable suite of calculators ',
            'is superior to anything you can find in the free domain, including this one. ',
            'In fact, it\'s worth noting that Georgiev\'s treatment of bias in confidence calculations is preferrable ',
            'to the method offered in the gsDesign R package which we use here.'
            ),
            p(
            'Speaking of gsDesign, Keaven Anderson of Merck Research Laboratories did an amazing job crafting and ',
            'documenting this package. Our application is little more than a highly restrictive GUI for it. '
            )
            )))
        )
  ))
# END ----



