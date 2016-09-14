#TODO : if less than 5 possible results : show 5, careful about duplicates

#TODO : use "enter" key to press go button as well
#TODO : find a way to show scores? as a color of btns for example
#TODO : profanity filter update in real time

if (!require(shiny)) {install.packages("shiny")}

shinyUI(fluidPage(
    theme = "slate.css",
    navbarPage("JCS Word Predictor",
               tabPanel("App",
                        fluidRow(
                            column(8,
                                   wellPanel(
                                       h4("Get started!", align = "center"),
                                       tags$td("Please type the start of a sentence in the text box 
                                               below, then press the 'Predict!' button. The 'Word 
                                               Predictor' will suggest the words that make the most 
                                               sense, according to its model."), br(),
                                       tags$td("You can choose the number of suggestions you'd 
                                               like to see, and whether or not to allow potentially 
                                               offensive suggestions. Activating the Details panel will 
                                               provide additional information about the quality of the 
                                               suggestions.")
                                       ),
                                   wellPanel(
                                       h4("Your text here :", align = "center"),
                                       div(textInput("userText", "", width = "100%")),
                                       div(style="display:inline-block", actionButton("goBtn", "Predict!")),
                                       div(style="display:inline-block", actionButton("clearBtn", "Clear"))
                                   ),
                                   wellPanel(
                                       h4("Suggestions", align = "center"),
                                       div(style="display:inline-block", uiOutput("resultBtn1")),
                                       div(style="display:inline-block", uiOutput("resultBtn2")),
                                       div(style="display:inline-block", uiOutput("resultBtn3")),
                                       div(style="display:inline-block", uiOutput("resultBtn4")),
                                       div(style="display:inline-block", uiOutput("resultBtn5"))
                                   )
                                       ),
                            column(4,
                                   wellPanel(
                                       h4("Options", align = "center"),
                                       radioButtons("suggNumber", label = "Word Suggestions", 
                                                    choices = list("1" = 1, "3" = 3, "5" = 5), 
                                                    selected = 1),
                                       checkboxInput("profanity", label = "Profanity Filter", value = TRUE),
                                       checkboxInput("details", label = "Show Details", value = FALSE)
                                   ),
                                   conditionalPanel(
                                       condition = "input.details == true",
                                       h4("Details", align = "center"),
                                       fluidRow(div(dataTableOutput("detailsTable1"), style = "width: 95%")),
                                       fluidRow(div(dataTableOutput("detailsTable3"), style = "width: 95%")),
                                       fluidRow(div(dataTableOutput("detailsTable5"), style = "width: 95%")), br(),
                                       strong("Confidence in suggestion :"), br(),
                                       tags$td("Score > 0.15 : really good"), br(),
                                       tags$td("Score > 0.05 : pretty good"), br(),
                                       tags$td("Score < 0.01 : mediocre"), br()
                                   )
                            )
                                   )
                        ),
               tabPanel("About", 
                    wellPanel(
                        h4("About this product"),  
                        tags$td("The "),
                        strong("JCS Word Predictor"),
                        tags$td("is a data product aiming to highlight 
                                the word prediction algorithm it is based on, and to provide an 
                                interface to it that can be accessed by anyone with a computer or a 
                                phone with an internet connection."), br(), br(),
                        tags$td("Around the world, people are spending an increasing amount of time 
                                on their mobile devices for email, social networking, banking and a 
                                whole range of other activities. But typing on mobile devices can be 
                                a serious pain. Smart keyboards make it easier for people to type on 
                                their mobile devices. One cornerstone of these smart keyboards is 
                                predictive text models. When someone types "),
                        em("I went to the"),
                        tags$td(", the keyboard presents three options for what the next word might be. 
                                For example, the three words might be gym, store, restaurant. For this 
                                product, I built a predictive text model like those used by smart 
                                keyboards."), br(), br(),
                        tags$td("This product was originally implemented as the Capstone Project of 
                                the "),
                        em("Data Science"),
                        tags$td("Specialization of Johns Hopkins University, using state-of-the-art 
                                Natural Language Processing techniques.")
                )),
               tabPanel("Technical Details", 
                    wellPanel(
                        h4("The Corpus"),
                        tags$td("To be able to provide correct word suggestions, a predictive text 
                                model must first be trained on a corpus of documents. For this product, 
                                I used the "),
                        a("HC Corpora", href = "http://www.corpora.heliohost.org/aboutcorpus.html"),
                        tags$td(". It is free to use, and consists of sentences collected from publicly 
                                available sources by a web crawler. There was no metadata attached, 
                                just plain text. We just know that the source is either a tweet, a blog 
                                post or a news article."), br(), br(),
                        tags$td("All in all, this corpus consists of more than 100M words. To keep a 
                                reasonable prediction speed, only 25% of the corpus has been sampled for 
                                this app though."), 
                        tags$hr(),
                        h4("Data Preprocessing"),
                        tags$td("The efficiency of a model can be greatly enhanced by doing some cleaning 
                                work on the training data before feeding it to the model. The main 
                                preprocessing phases that are being used in this product are :"), br(), br(),
                        tags$ul(
                            tags$li("Removal of all non-UTF-8 characters"), 
                            tags$li("Removal of 'internet terms' like URLs, email adresses, twitter handles"), 
                            tags$li("Removal of numbers"),        
                            tags$li("Removal of most punctuation"), 
                            tags$li("Removal of excessive white space between words"), 
                            tags$li("Correction of common word contractions and misspells")
                        ),        
                        tags$hr(),
                        h4("Dictionaries Creation"),
                        tags$td("Once the training data has been cleaned, the next step is to create 
                                dictionaries which the model will refer to, in order to suggest words 
                                that make sense. These dictionaries will store "),
                        em("n-grams"),
                        tags$td(". N-grams are contiguous sequences of n words. This product stores 
                                dictionaries for 1, 2, 3 and 4-grams. Each n-gram is linked to a number, 
                                representing the frequency at which this n-gram was observed in the 
                                training data."), br(), br(),
                        tags$td("A lot of work has been put into allowing the product to provide quick, 
                                yet efficient word suggestions. For that to happen, there has been a 
                                focus on making these dictionaries as small as possible on disk, and 
                                also quick to access, while not losing too much information. Encoding 
                                the words as integers helped a lot in that aspect, as did the removal 
                                of unique 2+ grams, since the bigger your training data is, the more 
                                probable it is that 2+ grams appearing just once are just misspelled 
                                words, and as such don't provide quality information."),
                        tags$hr(),
                        h4("The Algorithm"),
                        tags$td("At the time of this product's implementation, there are several 
                                algorithms that work based on n-grams dictionaries to provide word 
                                suggestions, but there isn't a unique one that could be considered 
                                head and shoulders above all of the others."), br(), br(), 
                        tags$td("Using only high-count grams (3 or 4) has the advantage of conditioning 
                                on a lot of context, so given sufficient training data the n-grams counts 
                                will be high and will converge to the “true value”. The drawback 
                                is that many counts will be equal to zero, so we need a huge sample to 
                                get a good estimate. This is a low bias, high variance approach, it 
                                needs "),
                        em("a lot"),
                        tags$td("of data to be able to generalize. If it has insufficient data it will 
                                not generalize correctly enough."), br(), br(), 
                        tags$td("Using only low-count grams (1 or 2) ignores context, and as such it will 
                                converge to a an  estimator that won't be as good. It has high bias, 
                                but will converge relatively quickly. It doesn't need a lot 
                                of samples."), br(), br(), 
                        tags$td("The "),
                        strong("Linear MLE Interpolation"),
                        tags$td("takes the best of both worlds, using counts from all n-grams sizes it 
                                has at its disposal, and weighing these counts (the higher the gram, the 
                                bigger the weight is, and weights must sum to 1). The value of these weights 
                                can be optimized in order to maxmize the predictive power of the model, 
                                by testing combinations on a subset of the corpus that wasn't used for 
                                training. For this app, time lacked for weight optimization. Various 
                                combinations were tested, the following values were chosen : 0.55 for 4-grams, 
                                0.35 for 3-grams, 0.099 for 2-grams and 0.001 for 1-grams.")
                    )
                ),
               tabPanel("Competitive Advantages", 
                    wellPanel(
                        h4("Competitive Advantages"),
                        tags$ul(
                            tags$li("High quality word suggestions"), 
                            tags$ul(
                                tags$li("Astute data cleaning leads to good predictive power"), 
                                tags$li("Choice of Linear Interpolation over Backoff methods to systematically take advantage of all of the available dictionaries"), 
                                tags$li("High score (22% top 3 precision) on independent prediction benchmark (https://github.com/jan-san/dsci-benchmark)"), br()
                            ),
                            tags$li("Fast response"), 
                            tags$ul(
                                tags$li("Encoded dictionaries (words stored as integers) to reduce disk storage size and lookup time"), 
                                tags$li("Pruning of low-frequency 2+ grams"), 
                                tags$li("Dictionaries and decoder loaded just once at application startup"), br()
                            ),
                            tags$li("Highly modulable"),        
                            tags$ul(
                                tags$li("Choice of 1, 3 or 5 ordered suggestions"),
                                tags$li("Safe mode available to remove profanities in suggestions"),
                                tags$li("Optional Details panel for information about suggestions quality"), br()
                            ),
                            tags$li("User-friendly graphical interface"),
                            tags$ul(
                                tags$li("Bootstrap look-and-feel"), 
                                tags$li("Suggestions shown as buttons to allow for quick sentence completing"), br()
                            )
                        )
                    )
                ),
               tabPanel("Author", 
                    wellPanel(
                        h4("About the author"), 
                        tags$ul(
                            tags$li("Blog : ", a("http://jcohensolal.blogspot.com", href = "http://jcohensolal.blogspot.com")), 
                            tags$li("Twitter : ", a("https://twitter.com/_jcohensolal_", href = "https://twitter.com/_jcohensolal_")),
                            tags$li("Github : ", a("https://github.com/juliencohensolal", href = "https://github.com/juliencohensolal")), br()
                        ),        
                        tags$td("Not a lot of content as of yet, as I only promote on my blog/twitter 
                                the projects I find the most interesting. I'm also still very much 
                                a rookie in the data science game, and as such haven't accomplished 
                                much of importance yet (but count on me to keep working on it!).")                   )
                )
    )
))
