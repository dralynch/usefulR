trimWhiteSpace<-function(x){gsub("^\\s+|\\s+$", "", x)}

tidybib<-function(inbib,outbib){
  bib<-scan(inbib,what="character",sep="\n")
  
  bib<-gsub("\t","",bib)
  bib<-gsub("^\\s+|\\s+$", "", bib)
  classes<-sapply(bib,function(x){unlist(strsplit(x,"="))[1]})
  classes<-classes[sapply(classes,substr,1,1)!="@"]
  classes<-classes[sapply(classes,nchar)<100]
  
  classes<-sapply(classes,function(x){gsub("^\\s+|\\s+$", "", x)})
  keeplist<-c("author","journal","number","pages","booktitle","chapter","edition","isbn","title","publisher","volume","year")
  
  entrystarts<-which(gregexpr("^@",bib)>0)
  authorlines<-which(gregexpr("^author",bib)>0)
  yearlines<-which(gregexpr("^year",bib)>0)
  
  
  
  authorinds<-rep("",length(entrystarts))
  yearinds<-rep("",length(entrystarts))
  
  for(i in 1:length(entrystarts)){
    tmp<-bib[authorlines[i]]
    tmp<-unlist(strsplit(tmp,"="))[2]
    tmp<-gsub("{", " ", tmp,fixed=T)
    tmp<-gsub(",", " ", tmp,fixed=T)
    tmp<-gsub(".", " ", tmp,fixed=T)
    tmp<-gsub("^\\s+|\\s+$", "", tmp)
    authorinds[i]<-unlist(strsplit(tmp," "))[1]
    tmp<-bib[yearlines[i]]
    tmp<-unlist(strsplit(tmp,"="))[2]
    tmp<-gsub("{", " ", tmp,fixed=T)
    tmp<-gsub("}", " ", tmp,fixed=T)
    tmp<-gsub(",", " ", tmp,fixed=T)
    tmp<-gsub(".", " ", tmp,fixed=T)
    tmp<-gsub("^\\s+|\\s+$", "", tmp)
    yearinds[i]<-unlist(strsplit(tmp," "))[1]
  }
  
  fullinds<-paste(authorinds,yearinds,sep="")
  indorder<-order(fullinds)
  
  for(i in indorder){
    thisline<-entrystarts[i]
    write(bib[thisline],file=outbib,append=T)
    nextline<-T
    while(nextline){
      thisline<-thisline+1
      if(bib[thisline]=="}"){
        write(bib[thisline],file=outbib,append=T)
        nextline<-F
      }
      if(nextline){
        if(i<length(entrystarts)){
          if(thisline==entrystarts[i+1]){
            nextline<-F
          }   
        }
      }
      if(nextline){
        tmp<-bib[thisline]
        tmp<-unlist(strsplit(tmp,"="))[1]
        tmp<-gsub("^\\s+|\\s+$", "", tmp)
        if(tmp %in% keeplist){
          write(bib[thisline],file=outbib,append=T)
        }
      }
      
    }
    
  }
  
  message("Duplicated entries ")
  message(message(paste(fullinds[which(duplicated(fullinds))],collapse=" ")))
} 


   




