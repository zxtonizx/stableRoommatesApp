library(matchingMarkets)
library(shiny)
library(rJava)
library(rsconnect)
library(rowr)
library(readxl)
library(xlsx)
library(dplyr)

server <- function(input, output) {
  
  #####inputs#####
  
  nameslist <- reactive({
    
    req(input$file1)
    tryCatch(
      {
        readfile <- read_excel(input$file1$datapath, sheet = 1)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    options(digits=2)
    listofnames <- unique(names(readfile))
    listofnames
  })
  
  df <- reactive({
    
    req(input$file1)
    tryCatch(
      {
        readfile <- read_excel(input$file1$datapath, sheet = 1)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )

    #convert names to numbers
    n <- ncol(readfile)
    m <- nrow(readfile)
    options(digits=2)
    for(i in 1:n){
      names(readfile)[i] <- i
      for(j in 1:m){
        if (is.na(readfile[[j,i]])) {
          readfile[j,i] <- readfile[j,i]
        } else {
          readfile[j,i] <- which(nameslist() == readfile[[j,i]])
        }
      }
      readfile[[i]] <- as.numeric(readfile[[i]])
    }
    
    for(i in 1:n){
      #replicate column i without duplicates, fill blanks with NAs
      readfile <-  cbind.fill(readfile,unique(readfile[,1]), fill = NA)
      #rename the new column
      colnames(readfile)[n+1] <- colnames(readfile)[1]
      #delete the old column
      readfile[,1] <- NULL}
  
    readfile  
  })
  
  matchings1 <- reactive({
    result = tryCatch({
      sri(prefs=df())$matchings 
    }, error = function(e) {
      tblresult <- c("There is an issue with the data")
      names(tblresult) <- c("There is an issue with the data")
    })
    })
  
  supermatchings1 <- reactive({
    result = tryCatch({
      match <-sri(prefs=df())$matchings
      matchlist <- unique(unlist(match[,2:3], use.names = FALSE))
      nameslen <- len(nameslist())
      
      for(i in 1:nameslen){
        if (len(which(matchlist == as.numeric(i))) == 0) {
          m <- nrow(match)
          match[m+1,1] <- "unmatched"
          match[m+1,2] <- i
          match[m+1,3] <- " "
        } else {
          #do nothing
        }
      }
      
      match
      
    }, error = function(e) {
      tblresult <- c("There is an issue with the data")
      names(tblresult) <- c("There is an issue with the data")
    })
  })
  
  df2 <- reactive({
    
    req(input$file1)
    tryCatch(
      {
        readfile <- read_excel(input$file1$datapath, sheet = 2)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    #convert names to numbers for df2
    n <- ncol(readfile)
    m <- nrow(readfile)
    options(digits=2)
    for(i in 1:n){
      names(readfile)[i] <- i
      for(j in 1:m){
        if (is.na(readfile[[j,i]])) {
          readfile[j,i] <- readfile[j,i]
        } else {
          readfile[j,i] <- which(nameslist() == readfile[[j,i]])
        }
      }
      readfile[[i]] <- as.numeric(readfile[[i]])
    }
    
    #remove duplicates
    n <- ncol(readfile)
    for(i in 1:n){
      #replicate column i without duplicates, fill blanks with NAs
      readfile <-  cbind.fill(readfile,unique(readfile[,1]), fill = NA)
      #rename the new column
      colnames(readfile)[n+1] <- colnames(readfile)[1]
      #delete the old column
      readfile[,1] <- NULL}
    
    #for matchings 1, remove the partners from preferences
    matching1 <-matchings1()[matchings1()[,1] == 1,]
    n1 <- nrow(matching1)
    
    for(i in 1:n1){
      a <- matching1[i,2]
      b <- matching1[i,3]
      colhead<-paste("X",a,sep = "")
      replacerow <- NA
      
      if (len(which(readfile[[colhead]] == b)) == 0) {
        replacerow <- NA
      } else {
        replacerow <-  which(readfile[[colhead]] == b)
        readfile[replacerow,a] <- NA
      }
      a <- matching1[i,2]
      b <- matching1[i,3]
      colhead<-paste("X",b,sep = "")
      replacerow <- NA
      if (len(which(readfile[[colhead]] == a)) == 0) {
        replacerow <- NA
      } else {
        replacerow <-  which(readfile[[colhead]] == a)
        readfile[replacerow,b] <- NA
      }
      
    }
    
    #move NAs to bottom
    row1 <- nrow(readfile) -1
    col1 <- ncol(readfile)
    for(i in 1:col1){
      for(j in 1:row1){
        if (is.na(readfile[j,i])) {
          readfile[j,i] <- readfile[j + 1,i]
          readfile[j + 1,i] <- NA
        }
      }
    }
    
    readfile
    
  })
  
  matchings2 <- reactive({
    #   result = tryCatch({
      sri(prefs=df2())$matchings 
    #}, error = function(e) {
    #  tblresult <- c("There is an issue with the data")
    #  names(tblresult) <- c("There is an issue with the data")
    #  })
  })
  
  supermatchings2 <- reactive({
    result = tryCatch({
      match <-sri(prefs=df2())$matchings
      matchlist <- unique(unlist(match[,2:3], use.names = FALSE))
      nameslen <- len(nameslist())
      
      for(i in 1:nameslen){
        if (len(which(matchlist == as.numeric(i))) == 0) {
          m <- nrow(match)
          match[m+1,1] <- "unmatched"
          match[m+1,2] <- i
          match[m+1,3] <- " "
        } else {
          #do nothing
        }
      }
      
      match
      
    }, error = function(e) {
      tblresult <- c("There is an issue with the data")
      names(tblresult) <- c("There is an issue with the data")
    })
  })
  
  df3 <- reactive({
    
    req(input$file1)
    tryCatch(
      {
        readfile <- read_excel(input$file1$datapath, sheet = 3)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    #convert names to numbers for df3
    n <- ncol(readfile)
    m <- nrow(readfile)
    options(digits=2)
    for(i in 1:n){
      names(readfile)[i] <- i
      for(j in 1:m){
        
        if (is.na(readfile[[j,i]])) {
          readfile[j,i] <- readfile[j,i]
        } else {
          readfile[j,i] <- which(nameslist() == readfile[[j,i]])
        }
      }
      readfile[[i]] <- as.numeric(readfile[[i]])
    }
    
    #remove duplicates
    n <- ncol(readfile)
    for(i in 1:n){
      #replicate column i without duplicates, fill blanks with NAs
      readfile <-  cbind.fill(readfile,unique(readfile[,1]), fill = NA)
      #rename the new column
      colnames(readfile)[n+1] <- colnames(readfile)[1]
      #delete the old column
      readfile[,1] <- NULL}
    
    #move NAs to bottom
    row1 <- nrow(readfile) -1
    col1 <- ncol(readfile)
    for(i in 1:col1){
      for(j in 1:row1){
        if (is.na(readfile[j,i])) {
          readfile[j,i] <- readfile[j + 1,i]
          readfile[j + 1,i] <- NA
        }
      }
    }
    
    #for matchings 1, remove the partners from preferences
    matching1 <-matchings1()[matchings1()[,1] == 1,]
    n1 <- nrow(matching1)
    
    for(i in 1:n1){
      a <- matching1[i,2]
      b <- matching1[i,3]
      colhead<-paste("X",a,sep = "")
      replacerow <- NA
      
      if (len(which(readfile[[colhead]] == b)) == 0) {
        replacerow <- NA
      } else {
        replacerow <-  which(readfile[[colhead]] == b)
        readfile[replacerow,a] <- NA
      }
      a <- matching1[i,2]
      b <- matching1[i,3]
      colhead<-paste("X",b,sep = "")
      replacerow <- NA
      if (len(which(readfile[[colhead]] == a)) == 0) {
        replacerow <- NA
      } else {
        replacerow <-  which(readfile[[colhead]] == a)
        readfile[replacerow,b] <- NA
      }
    }
    
    #remove duplicates
    n <- ncol(readfile)
    for(i in 1:n){
      #replicate column i without duplicates, fill blanks with NAs
      readfile <-  cbind.fill(readfile,unique(readfile[,1]), fill = NA)
      #rename the new column
      colnames(readfile)[n+1] <- colnames(readfile)[1]
      #delete the old column
      readfile[,1] <- NULL}
    
    #move NAs to bottom
    row1 <- nrow(readfile) -1
    col1 <- ncol(readfile)
    for(i in 1:col1){
      for(j in 1:row1){
        if (is.na(readfile[j,i])) {
          readfile[j,i] <- readfile[j + 1,i]
          readfile[j + 1,i] <- NA
        }
      }
    }
    
    #for matchings 2, remove the partners from preferences
    matching1 <-matchings2()[matchings2()[,1] == 1,]
    n1 <- nrow(matching1)
    
    for(i in 1:n1){
      a <- matching1[i,2]
      b <- matching1[i,3]
      colhead<-paste("X",a,sep = "")
      replacerow <- NA
      
      if (len(which(readfile[[colhead]] == b)) == 0) {
        replacerow <- NA
      } else {
        replacerow <-  which(readfile[[colhead]] == b)
        readfile[replacerow,a] <- NA
      }
      a <- matching1[i,2]
      b <- matching1[i,3]
      colhead<-paste("X",b,sep = "")
      replacerow <- NA
      if (len(which(readfile[[colhead]] == a)) == 0) {
        replacerow <- NA
      } else {
        replacerow <-  which(readfile[[colhead]] == a)
        readfile[replacerow,b] <- NA
      }
    }
    
    #remove duplicates
    n <- ncol(readfile)
    for(i in 1:n){
      #replicate column i without duplicates, fill blanks with NAs
      readfile <-  cbind.fill(readfile,unique(readfile[,1]), fill = NA)
      #rename the new column
      colnames(readfile)[n+1] <- colnames(readfile)[1]
      #delete the old column
      readfile[,1] <- NULL}
    
    #move NAs to bottom
    row1 <- nrow(readfile) -1
    col1 <- ncol(readfile)
    for(i in 1:col1){
      for(j in 1:row1){
        if (is.na(readfile[j,i])) {
          readfile[j,i] <- readfile[j + 1,i]
          readfile[j + 1,i] <- NA
        }
      }
    }
    
    readfile
    
  })
  
  matchings3 <- reactive({
    
    result = tryCatch({
      sri(prefs=df3())$matchings 
    }, error = function(e) {
      tblresult <- c("There is an issue with the data")
      names(tblresult) <- c("There is an issue with the data")
    })
  
  })
  
  supermatchings3 <- reactive({
    result = tryCatch({
      match <-sri(prefs=df3())$matchings
      matchlist <- unique(unlist(match[,2:3], use.names = FALSE))
      nameslen <- len(nameslist())
      
      for(i in 1:nameslen){
        if (len(which(matchlist == as.numeric(i))) == 0) {
          m <- nrow(match)
          match[m+1,1] <- "unmatched"
          match[m+1,2] <- i
          match[m+1,3] <- " "
        } else {
          #do nothing
        }
      }
      
      match
      
    }, error = function(e) {
      tblresult <- c("There is an issue with the data")
      names(tblresult) <- c("There is an issue with the data")
    })
  })
  
  scores1 <- reactive({
    
    req(input$file1)
    
    data <- df()
    matchx <- matchings1()
    
    tryCatch(
      {
        
        umatrix <- unique(matchx[1])
        umatrix <- umatrix[order(umatrix$matching, decreasing = TRUE),]
        matchingscount <-umatrix[1]
        umatrix <- unique(matchx[1])
        
        for(i in 1:matchingscount){
          
          rown<- length(which(matchx$matching == i))
          smallmatrix<-matchx[matchx[,1] == i,]
          counter<-0
          
          for(j in 1:rown){
            a <- smallmatrix[j,2]
            b <- smallmatrix[j,3]
            colhead<-paste("X",a,sep = "")
            counter<- counter + which(data[[colhead]] == b)
            colhead<-paste("X",b,sep = "")
            counter<- counter + which(data[[colhead]] == a)
          }
          
          umatrix[i,2]<- counter/rown
          colnames(umatrix)[2] <- "score"
          
        }
        umatrix
        
      },
      error = function(e) {
        matchx
      }
    )
    
     
  })
  
  scores2 <- reactive({
    
    req(input$file1)
    
    data <- df2()
    matchx <- matchings2()
    
    tryCatch(
      {
        
        umatrix <- unique(matchx[1])
        umatrix <- umatrix[order(umatrix$matching, decreasing = TRUE),]
        matchingscount <-umatrix[1]
        umatrix <- unique(matchx[1])
        
        for(i in 1:matchingscount){
          
          rown<- length(which(matchx$matching == i))
          smallmatrix<-matchx[matchx[,1] == i,]
          counter<-0
          
          for(j in 1:rown){
            a <- smallmatrix[j,2]
            b <- smallmatrix[j,3]
            colhead<-paste("X",a,sep = "")
            counter<- counter + which(data[[colhead]] == b)
            colhead<-paste("X",b,sep = "")
            counter<- counter + which(data[[colhead]] == a)
          }
          
          umatrix[i,2]<- counter/rown
          colnames(umatrix)[2] <- "score"
          
        }
        umatrix
        
      },
      error = function(e) {
        matchx
      }
    )
    
  })
  
  scores3 <- reactive({
    
    req(input$file1)
    
    data <- df3()
    matchx <- matchings3()
    
    tryCatch(
      {
        
        umatrix <- unique(matchx[1])
        umatrix <- umatrix[order(umatrix$matching, decreasing = TRUE),]
        matchingscount <-umatrix[1]
        umatrix <- unique(matchx[1])
        
        for(i in 1:matchingscount){
          
          rown<- length(which(matchx$matching == i))
          smallmatrix<-matchx[matchx[,1] == i,]
          counter<-0
          
          for(j in 1:rown){
            a <- smallmatrix[j,2]
            b <- smallmatrix[j,3]
            colhead<-paste("X",a,sep = "")
            counter<- counter + which(data[[colhead]] == b)
            colhead<-paste("X",b,sep = "")
            counter<- counter + which(data[[colhead]] == a)
          }
          
          umatrix[i,2]<- counter/rown
          colnames(umatrix)[2] <- "score"
          
        }
        umatrix
        
      },
      error = function(e) {
        matchx
      }
    )
    
    
  })
  
  dftext <- reactive({
    req(input$file1)
    texttable <- df()
    #convert numbers to names
    n <- ncol(texttable)
    m <- nrow(texttable)
    for(i in 1:n){
      for(j in 1:m){
        texttable[j,i] <- nameslist()[as.numeric(texttable[[j,i]])]
      }
      names(texttable)[i] <- nameslist()[as.numeric(gsub("X", "", names(texttable)[[i]]))]
    }
    
    texttable
  })
  
  dftext2 <- reactive({
    req(input$file1)
    texttable <- df2()
    #convert numbers to names
    n <- ncol(texttable)
    m <- nrow(texttable)
    for(i in 1:n){
      for(j in 1:m){
        texttable[j,i] <- nameslist()[as.numeric(texttable[[j,i]])]
      }
      names(texttable)[i] <- nameslist()[as.numeric(gsub("X", "", names(texttable)[[i]]))]
    }
    texttable
  })
  
  dftext3 <- reactive({
    req(input$file1)
    texttable <- df3()
    #convert numbers to names
    n <- ncol(texttable)
    m <- nrow(texttable)
    for(i in 1:n){
      for(j in 1:m){
        texttable[j,i] <- nameslist()[as.numeric(texttable[[j,i]])]
      }
      names(texttable)[i] <- nameslist()[as.numeric(gsub("X", "", names(texttable)[[i]]))]
    }
    texttable
  })
  
  matchings1text <- reactive({
    req(input$file1)
    texttable <- supermatchings1()
    #convert numbers to names
    n <- ncol(texttable)
    m <- nrow(texttable)
    
    tryCatch(
      {
        for(i in 2:n){
          for(j in 1:m){
            texttable[j,i] <- nameslist()[as.numeric(texttable[[j,i]])]
          }
        }
        texttable
      },
      error = function(e) {
        result = tryCatch({
          matchings1()
        }, error = function(e) {
          tblresult <- c("There is an issue with the data")
          names(tblresult) <- c("There is an issue with the data")
        })
      }
    )
  })
  
  matchings2text <- reactive({
    req(input$file1)
    texttable <- supermatchings2()
    #convert numbers to names
    n <- ncol(texttable)
    m <- nrow(texttable)
    
    tryCatch(
      {
        for(i in 2:3){
          for(j in 1:m){
            texttable[j,i] <- nameslist()[as.numeric(texttable[[j,i]])]
          }
        }
        texttable
      },
      error = function(e) {
        result = tryCatch({
          matchings2()
        }, error = function(e) {
          tblresult <- c("There is an issue with the data")
          names(tblresult) <- c("There is an issue with the data")
        })
      }
    )
    
  })
  
  matchings3text <- reactive({
    req(input$file1)
    texttable <- supermatchings3()
    #convert numbers to names
    n <- ncol(texttable)
    m <- nrow(texttable)

    
    tryCatch(
      {
        for(i in 2:3){
          for(j in 1:m){
            texttable[j,i] <- nameslist()[as.numeric(texttable[[j,i]])]
          }
        }
        texttable
      },
      error = function(e) {
        
        result = tryCatch({
          matchings3()
        }, error = function(e) {
          tblresult <- c("There is an issue with the data")
          names(tblresult) <- c("There is an issue with the data")
        })
        
        
      }
    )
    
  })
  
  #####Tab 1#####
  
  output$contents1 <- renderTable({dftext()})
  
  output$matches1 <- renderTable({matchings1text()})
  
  output$scores1 <- renderTable({scores1()})
  
  #####Tab 2#####
  
  output$contents2 <- renderTable({dftext2()})
  
  output$matches2 <- renderTable({matchings2text()})
  
  output$scores2 <- renderTable({scores2()})
  
  #####Tab 3#####
  
  output$contents3 <- renderTable({dftext3()})
  
  output$matches3 <- renderTable({matchings3text()})
  
  output$scores3 <- renderTable({scores3()})
  
  ####Download#####
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("matchings", ".xlsx", sep = "")
    },
    content = function(file) {
      
      ####copied from above#####  
      
      req(input$file1)
      
      write.xlsx(matchings1text(), file, sheetName = "Study 1", append = FALSE)
      
      result = tryCatch({
        write.xlsx(matchings2text(), file, sheetName = "Study 2", append = TRUE)
      }, error = function(e) {
        write.xlsx(c(0), file, sheetName = "Study 2", append = TRUE)
      })
      
      result = tryCatch({
        write.xlsx(matchings3text(), file, sheetName = "Study 3", append = TRUE)
      }, error = function(e) {
        write.xlsx(c(0), file, sheetName = "Study 3", append = TRUE)
      })
      
    }
  )
  
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("template", ".xlsx", sep = "")
    },
    content = function(file) {
      file.copy("example.xlsx", file)
    }
  )
  
}
