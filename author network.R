library('readr')

Network<-read_csv('Care_Quality,_Workforce_&_7_Day_Services-weekend.csv')
  
UK<-Network[which(Network$`Author Country Code`=='uk'),] #only UK tweets

U<-UK[-which(is.na(UK$`Mentioned Authors`)),] #only tweets that @Mention

U$`Mentioned Authors`<-gsub('@','',U$`Mentioned Authors`)

U<-U[-which(is.na(U$`Author County`)),]

authors<-data.frame(authors = as.character(U$Author) ,
                    Mentioned = as.character(U$`Mentioned Authors`),
                    location = as.character(U$`Author County`))

authors<-unique(authors) #only unique authors

write_csv(authors,'authors.csv')

source<-NULL
target<-NULL
location<-NULL
for (x in 1:nrow(authors)){ #loop over all authors 
  ma<-(data.frame(do.call('rbind', strsplit(as.character(authors$Mentioned[x]),',',fixed=TRUE)))) #split mutiple mentions
  for (p in 1:length(ma)) { #make vectors of authers, mentions, auther locations
    source<-c(source,as.character(authors$authors[x]))
    target<-c(target,as.character(ma[[p]]))
    location<-c(location,as.character(authors$location[x]))
  }
}

Edges<-data.frame(Source = source, #create edge file
                  Target = target)

Nodes<-data.frame(ID = source, #create node file
                  Label = source,
                  Location = location)

Nodes<-unique(Nodes) #only unique authers in node file

write_csv(Nodes,'Author-Nodes.csv')
write_csv(Edges,'Author-Edges.csv')
