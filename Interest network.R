    library('readr')
    
  #  Network<-read_csv('A&E twitter mentions - 5000.csv')
      
    UK<-Network[which(Network$`Author Country Code`=='uk'),] #only UK tweets
    
    U<-UK[-which(is.na(UK$`Mentioned Authors`)),] #only tweets that @Mention
    
    interest<-U[-which(is.na(U$Interest)),] #only tweets with an author interest
    
    interest$`Mentioned Authors`<-gsub('@','',interest$`Mentioned Authors`)
    
    interests<-data.frame(authors = as.character(interest$Author) ,
                          interest = as.character(interest$Interest),
                          Mentioned = as.character(interest$`Mentioned Authors`))
    
    interests<-unique(interests) #only unique authors
    
    write_csv(interests,'interests.csv')
    
    
    
    source<-NULL
    target<-NULL
    ID<-NULL
    Label<-NULL
   for (x in 1:nrow(interests)){ #loop over all authors with interests
     ma<-(data.frame(do.call('rbind', strsplit(as.character(interests$Mentioned[x]),',',fixed=TRUE))))
     if (length(ma)==1) { #if there is 1 mentioned author
      if (sum(as.character(interests$authors)==as.character(interests$Mentioned[x]))==1){ #if the mentioned author is in the author list
         im<-as.character(interests$interest[as.character(interests$authors)==as.character(interests$Mentioned[x])]) #create a list of interests for all singaly mentioned authors
         ia<-as.character(interests$interest[x]) #create a list of interests for all authors
         im.df<-data.frame(do.call('rbind', strsplit(im,',',fixed=TRUE))) #split the mentioned authors interests
         ia.df<-data.frame(do.call('rbind', strsplit(ia,',',fixed=TRUE))) #split the author interests
        if ((length(im.df)==1)&(length(ia.df)==1)) { #if author and mentioned have 1 interest each, append to target and source vectors
           source<-c(source,ia)
           target<-c(target,im)
           ID<-c(ID,ia)
           Label<-c(Label,ia)
        } 
        if ((length(ia.df)==1)&(length(im.df)>1)) { #if author has 1 interest and mentioned has > 1, append to target and source vectors
          for (y in 1:length(im.df)) {
            target<-c(target,as.character(im.df[[y]]))
            source<-c(source,ia)
          }
        }
         if ((length(ia.df)>1)&(length(im.df)==1)) { #if author has > 1 interest and mentioned has 1, append to target and source vectors
           for (y in 1:length(ia.df)) {
             target<-c(target,im)
             source<-c(source,as.character(ia.df[[y]]))
           }
         }
         if ((length(ia.df)>1)&(length(im.df)>1)) { #if author and mentioned both have > 1 interest, append to target and source vectors
            for (y in 1:length(ia.df)) {
             target<-c(target,as.character(im.df[[1]]))
             source<-c(source,as.character(ia.df[[y]]))
            }
           for (y in 1:length(im.df)) {
             target<-c(target,as.character(im.df[[y]]))
             source<-c(source,as.character(ia.df[[1]]))
           }
         }
      } 
      }
    }
    
    Edges<-data.frame(cbind(source,target))
    
    Nodes<-data.frame(cbind(ID,Label))
    Nodes<-unique(Nodes)
    
    write_csv(Nodes,'Interest-Nodes.csv')
    write_csv(Edges,'Interest-Edges.csv')
    