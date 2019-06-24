library(GO.db)
library(ggplot2)
library(shiny)
library(DT)


# Gets the ancestor and children data
bpAncestors <- as.list(GOBPANCESTOR)
ccAncestors <- as.list(GOCCANCESTOR)
mfAncestors <- as.list(GOMFANCESTOR)
ancestors <- append(bpAncestors, ccAncestors)
ancestors <- append(ancestors, mfAncestors)

bpChildren <- as.list(GOBPCHILDREN)
ccChildren <- as.list(GOCCCHILDREN)
mfChildren <- as.list(GOMFCHILDREN)
children <- append(bpChildren, ccChildren)
children <- append(children, mfChildren)
children <- children[which(!is.na(children))]


ui <- fluidPage(
  titlePanel("GO Term Visualizer"), 
  sidebarLayout(
    sidebarPanel(
      textAreaInput("query", label = "GO Terms"),
      actionButton("getData", "GO"),
      textInput("active", label = "Select a term from the graph")
      #textI("active")
    ),
    mainPanel(
      plotOutput("coolplot"),
      tableOutput("table")
    )
  )
)
server <- function(input, output) {
  lstOfTerms <- reactive({
    req(input$query)
    unique(strsplit(input$query, "\n")[[1]])
  })
  
  data <- eventReactive(input$getData , {
    # This block goes through the user-given list of terms and creates a named list
    # where each element of the list is an ancestor to at least one of the terms in
    # lstOfTerms. Each item contains a list of the query terms that are descendents
    # of the list item.
    terms <- list()
    terms <- unlist(lapply(unique(lstOfTerms()), function(x) {
      theAncestors <- ancestors[[x]]
      sapply(theAncestors, function(y) {
        terms[[y]][1] <- c(terms[[y]][1], x)
      })
    }))
    queryDescendents <- split(unname(terms), names(terms))
    queryDescendents$all <- NULL
    
    # For each term in queryDescendents, it returns the list of children of the term.
    childs <- lapply(names(queryDescendents), function(x) {
      children[[x]]
    })
    names(childs) <- names(queryDescendents)
    
    # The two list of lists (queryDescendents and childs) are consolidated into one list.
    data <- lapply(c(1:length(queryDescendents)), function(x) {
      return(list(queryDescendents = queryDescendents[[x]], children = childs[[x]]))
    })
    names(data) <- names(queryDescendents)
    return(data)
  })
  
  observe ({ print(head(data())) })
  
  activeTerm <- reactive({
    if (input$active == "") { NULL }
    else { input$active }
  })
  
  #observe ({ print(activeTerm())})

  termsToPlot <- reactive({
    if (is.null(activeTerm())) {
      c("GO:0005575", "GO:0003674", "GO:0008150")
    } else {
      #c("GO:0005575", "GO:0003674", "GO:0008150")
      data()[[activeTerm()]][[2]]
    }
  })
  
  currDF <- reactive({
    curr_df <- data.frame(goID = termsToPlot(),
                          num = sapply(termsToPlot(), function(x) {return(length(data()[[x]][[1]]))}),
                          term = sapply(termsToPlot(), function(x) {return(Term(x))}),
                          def = sapply(termsToPlot(), function(x) {return(Definition(x))}))
    curr_df <- curr_df[order(curr_df$num, decreasing = T),]
    curr_df <- curr_df[which(curr_df$num > 0),]
    curr_df
  })
  
  output$coolplot <- renderPlot({
    ggplot(currDF(), aes(x = factor(goID, levels = goID[order(num, decreasing = T)]), y = num)) + 
      geom_col() + 
      ggtitle(paste0(activeTerm(), ": ", "biological process"), subtitle = "Plo")
  })
  output$table <- renderTable({
    currDF()
    #currDF()[order(currDF()$num, decreasing = T),]
  })

}
shinyApp(ui = ui, server = server)