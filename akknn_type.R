# init


libs<-c("tm","plyr","class","kknn")
lapply(libs,require,character.only=TRUE)
#packages are saved as strings so we need some argument,"character.only" 
#in order to make R accept strings as packages
# set options
options(stringsAsFactors = FALSE)
path<-"getwd()"


source(paste(path,"/kknn.ordinal.R",sep=""))
load(paste(path,"/dat.RData",sep=""))
# data and function are in current directory so have to add "/"


# set parameters

mstone<-"2012-01-01"
#in paper, start date is 2012-01-01
  
  dir.create(paste(path,"/output",sep="")
#first, make a directory called "output" to save all outputs in this folder                        
  dir.create(paste(path,"/output/",mstone,sep=""))
 for(i in 0:5) {
   dir.create(paste(path,"/output/",mstone,"/",i,sep=""))
 }

 #in output folder, make folders according to level.            
             
  no.levels<-c()
  dat_lv0=c()
  dat_lv15=c()
  for (lv in 0:5){
    
    level<-dat[dat$level==lv,]
    
    level$Date<-as.Date(substr(as.character(level$Date),1,8),"%Y%m%d")
    level<-level[level$Date>=mstone,]
    if(lv==0) dat_lv0=rbind(dat_lv0,level)
    if(lv>0) dat_lv15=rbind(dat_lv15,level)
    no.levels<-c(no.levels,nrow(level))
    for(i in 1:nrow(level)){
      filename<-paste(path,mstone,"/",lv,"/",i,".txt",sep="")
      write(level$Description[i],file=filename)
    }  
  }
  # according to level, classifiy the description and save them into folder which is same to level of descirption 
  # clean text
  
  cleanCorpus<-function(corpus){
    corpus.tmp<-tm_map(corpus,removePunctuation)
    corpus.tmp<-tm_map(corpus.tmp,stripWhitespace)
    corpus.tmp<-tm_map(corpus.tmp,content_transformer(tolower))
    corpus.tmp<-tm_map(corpus.tmp,removeWords, stopwords("english"))
    corpus.tmp<-Corpus(VectorSource(corpus.tmp))
    return(corpus.tmp)
  }
  # build TDM to make a word frequency.
  generateTDM<-function(level,path){
    s.dir<-sprintf("%s/%s",path,level)
    s.cor<-Corpus(DirSource(directory=s.dir,encoding="UTF-8"))
    s.cor.cl<-cleanCorpus(s.cor)
    s.tdm<-TermDocumentMatrix(s.cor.cl)
    s.tdm<-removeSparseTerms(s.tdm,0.7)
    return(list(name=level,tdm=s.tdm))
  }
  
  tdm=lapply(c("1","2","3","4","5") , generateTDM,path=paste(path,mstone,sep=""))
  tdm0=lapply(c("0") , generateTDM,path=paste(path,mstone,sep=""))

  
  # attch level.
  bindCandidatetoTDM<-function(tdm){
    s.mat<-t(data.matrix(tdm[["tdm"]]))
    s.df <- as.data.frame(s.mat)
  # already stringsAsFacotrs is set up.
    s.df <- cbind(s.df, rep(tdm[["name"]],nrow(s.df)))
    colnames(s.df)[ncol(s.df)] <-"targetCandidate"
    return(s.df)
  }
  
  candTDM <- lapply(tdm,bindCandidatetoTDM)
  candTDM0 <- lapply(tdm0,bindCandidatetoTDM)

  
  # stack 
  tdm.stack <- do.call(rbind.fill,candTDM)
  # to sum up the results from level 1~5.
  tdm.stack[is.na(tdm.stack)]<-0
  
  
  tdm0.stack <- do.call(rbind.fill,candTDM0)
  tdm0.stack <- rbind.fill(tdm.stack[1,],tdm0.stack)
  # to make same frame compairng to level 0 and level 1~5    
  tdm0.stack<-tdm0.stack[-1,]
  tdm0.stack[is.na(tdm0.stack)]<-0
  
  
  cvkknn<-function(valid.idx,train){
    train.idx<-(1:nrow(train)) [-valid.idx]
    
    
    train$targetCandidate<-ordered(train$targetCandidate)
    
    train.kknn<-train[train.idx,]
    valid.kknn<-train[valid.idx,]
    
    valid.cand<-valid.kknn$targetCandidate
    valid.cand.nl<-valid.kknn[,!colnames(valid.kknn)%in%"targetCandidate"]
    
    accuracy.stack=c()
    accuracy2.stack=c()
    alphas<-seq(0.3,0.6,0.01)
    for(pp in alphas){
      
      kknn.pred<-kknn.ordinal(targetCandidate~., train.kknn,valid.cand.nl,distance =2,k=7,kernel = "triangular",param=pp)
      
      fit.test <- fitted(kknn.pred)
      
      tmp<-sum((as.numeric(fit.test)-as.numeric(valid.cand))!=0)/length(valid.idx)
      tmp2<-sum(abs(as.numeric(fit.test)-as.numeric(valid.cand)))/length(valid.idx)
      
      accuracy.stack<-c(accuracy.stack,tmp)
      accuracy2.stack<-c(accuracy2.stack,tmp2)
      
    }

    
    return(accuracy.stack)
  }
  
  findalpha<-function(train){  
    tmp<-rep(sample(10), ceiling(nrow(train)/10))
    tmp<-sample(tmp)[1:nrow(train)]
    list.train<-c()
    for(i in c(1:10)){
      list.train[[i]]<-c(1:nrow(train))[tmp==i]
    }
  
    leaveoneout2<-ldply(list.train,cvkknn,train)
   )
    errors<-colMeans(leaveoneout2)
    names(errors)<-seq(0.3,0.6,0.01)
    out<-seq(0.3,0.6,0.01)[errors==min(errors)]
    
    return(out[1])
    # find the alph which makes the minimum error.
   
  }
  
  knn.accuracy=c()
  avg.accuracy=c()
  opt.accuracy=c()
  mat.5=list()
  mat.opt=list()
  optalphastack<-c()
  tdm.stack1<-tdm.stack[tdm.stack$targetCandidate==1,]
  tdm.stack2<-tdm.stack[tdm.stack$targetCandidate==2,]
  tdm.stack3<-tdm.stack[tdm.stack$targetCandidate==3,]
  tdm.stack4<-tdm.stack[tdm.stack$targetCandidate==4,]
  tdm.stack5<-tdm.stack[tdm.stack$targetCandidate==5,]

  for(nn in 1:100){
    

   
    train.idx1<-sample(nrow(tdm.stack1), ceiling(nrow(tdm.stack1)*0.7))
    test.idx1<-(1:nrow(tdm.stack1)) [-train.idx1]
    train.idx2<-sample(nrow(tdm.stack2), ceiling(nrow(tdm.stack2)*0.7))
    test.idx2<-(1:nrow(tdm.stack2)) [-train.idx2]
    train.idx3<-sample(nrow(tdm.stack3), ceiling(nrow(tdm.stack3)*0.7))
    test.idx3<-(1:nrow(tdm.stack3)) [-train.idx3]
    train.idx4<-sample(nrow(tdm.stack4), ceiling(nrow(tdm.stack4)*0.7))
    test.idx4<-(1:nrow(tdm.stack4)) [-train.idx4]
    train.idx5<-sample(nrow(tdm.stack5), ceiling(nrow(tdm.stack5)*0.7))
    test.idx5<-(1:nrow(tdm.stack5)) [-train.idx5]
    tdm.idx<-rbind(train.idx1,train.idx2,train.idx3,train.idx4,train.idx5)
    test.idx<-rbind(test.idx1,test.idx2,test.idx3,test.idx4,test.idx5)
    #to select 70% train data set on each level.
    tdm.stack$targetCandidate<-ordered(tdm.stack$targetCandidate)
    
    train.kknn<-tdm.stack[train.idx,]
    test.kknn<-tdm.stack[test.idx,]
    
    train.cand<-train.kknn$targetCandidate
    train.cand.nl<-train.kknn[,!colnames(train.kknn)%in%"targetCandidate"]

    

    test.cand<-test.kknn$targetCandidate
    test.cand.nl<-test.kknn[,!colnames(test.kknn)%in%"targetCandidate"]
    
    opt.alpha<-findalpha(train.kknn)
    optalphastack<-c(optalphastack,opt.alpha)

    knn.pred<-knn(train.cand.nl,test.cand.nl,train.cand)
    conf.knn<-table("Predictions"= knn.pred,Actual=test.cand)
    accuracy.knn<-sum(diag(conf.knn)/length(test.idx)*100)    
    
    
    kknn.opt<-kknn.ordinal(targetCandidate~., train.kknn,test.cand.nl,distance =2,k=7,kernel = "triangular",param=opt.alpha)
    fit.opt <- fitted(kknn.opt)
    conf.opt<-table("Predictions"= fit.opt,Actual=test.cand)
    accuracy.opt<-sum(diag(conf.opt)/length(test.idx)*100)    
    
    
    kknn.5<-kknn.ordinal(targetCandidate~., train.kknn,test.cand.nl,distance =2,k=7,kernel = "triangular",param=0.5)
    # to compare wknn and awknn.
    fit.test5 <- fitted(kknn.5)
    conf.mat5<-table("Predictions"= fit.test5,Actual=test.cand)
    accuracy5<-sum(diag(conf.mat5)/length(test.idx)*100)
    

    
    mat.5[[nn]]<-conf.mat5
    mat.opt[[nn]]<-conf.opt
    
    knn.accuracy<-c(knn.accuracy,accuracy.knn)
    avg.accuracy<-c(avg.accuracy,accuracy5)
    opt.accuracy<-c(opt.accuracy,accuracy.opt)
    
    
    print(c(mean(avg.accuracy), mean(opt.accuracy)))
    
  }

  tmp3<-cbind(avg.accuracy,opt.accuracy)
  colMeans(tmp3)


  postscript(paste(path,"/output/optalpha.eps",sep=""),width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
  hh<-hist(optalphastack[],breaks =seq(0.175,0.8,0.05),xlab=substitute(alpha),main="",cex.lab=1.5)
  dev.off()
  histsumm<-cbind(hh$mids,hh$counts)
  
  
  acc<-cbind(kNN=knn.accuracy[1:100],wkNN=tmp3[,1],"Adaptive wkNN"=tmp3[,2])
  
  postscript(paste(path,"/output/boxplot.eps",sep=""),width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
  boxplot(as.data.frame(acc),ylab="Accuracy",cex.lab=1.5)
  dev.off()
  
  
  
