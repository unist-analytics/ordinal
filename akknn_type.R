# init


libs<-c("tm","plyr","class","kknn")
lapply(libs,require,character.only=TRUE)

# set options
options(stringsAsFactors = FALSE)
path<-"your path"


source(paste(path,"kknn.ordinal.R",sep=""))
load(paste(path,"dat.RData",sep=""))



# set parameters

mstone<-"2013-05-01"
  
  dir.create(paste(path,mstone,sep=""))
  dir.create(paste(path,mstone,"/0",sep=""))
  dir.create(paste(path,mstone,"/1",sep=""))
  dir.create(paste(path,mstone,"/2",sep=""))
  dir.create(paste(path,mstone,"/3",sep=""))
  dir.create(paste(path,mstone,"/4",sep=""))
  dir.create(paste(path,mstone,"/5",sep=""))
  
  no.levels<-c()
  dat_lv0=c()
  dat_lv15=c()
  for (lv in 0:5){
    
    level<-dat[dat$level==lv,]
    
    level$Date<-as.Date(substr(as.character(level$Date),1,8),"%Y%m%d")
    level<-level[level$Date>=mstone,]
    #level<-level[level$type=="Flash Flood"]
    if(lv==0) dat_lv0=rbind(dat_lv0,level)
    if(lv>0) dat_lv15=rbind(dat_lv15,level)
    no.levels<-c(no.levels,nrow(level))
    for(i in 1:nrow(level)){
      filename<-paste(path,mstone,"/",lv,"/",i,".txt",sep="")
      write(level$Description[i],file=filename)
    }  
  }
  #names(dat_lv0)<-tolower(names(dat_lv0))
  #names(dat_lv15)<-tolower(names(dat_lv15))
  #write.csv(dat_lv0,file=paste(outpath,"dat_lv0.csv",sep=""),row.names=FALSE)
  #write.csv(dat_lv15,file=paste(outpath,"dat_lv15.csv",sep=""),row.names=FALSE)
  
  # clean text
  
  cleanCorpus<-function(corpus){
    corpus.tmp<-tm_map(corpus,removePunctuation)
    corpus.tmp<-tm_map(corpus.tmp,stripWhitespace)
    #corpus.tmp<-tm_map(corpus.tmp,tolower)
    corpus.tmp<-tm_map(corpus.tmp,content_transformer(tolower))
    corpus.tmp<-tm_map(corpus.tmp,removeWords, stopwords("english"))
    corpus.tmp<-Corpus(VectorSource(corpus.tmp))
    return(corpus.tmp)
  }
  # build TDM
  
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
  
  str(tdm)
  str(tdm0)
  
  
  # attch name
  bindCandidatetoTDM<-function(tdm){
    s.mat<-t(data.matrix(tdm[["tdm"]]))
    s.df <- as.data.frame(s.mat,stringAsFactors = FALSE)
    s.df <- cbind(s.df, rep(tdm[["name"]],nrow(s.df)))
    colnames(s.df)[ncol(s.df)] <-"targetCandidate"
    return(s.df)
  }
  
  candTDM <- lapply(tdm,bindCandidatetoTDM)
  str(candTDM)
  
  candTDM0 <- lapply(tdm0,bindCandidatetoTDM)
  str(candTDM0)
  
  # stack 
  tdm.stack <- do.call(rbind.fill,candTDM)
  tdm.stack[is.na(tdm.stack)]<-0
  head(tdm.stack)
  
  tdm0.stack <- do.call(rbind.fill,candTDM0)
  tdm0.stack <- rbind.fill(tdm.stack[1,],tdm0.stack)
  tdm0.stack<-tdm0.stack[-1,]
  tdm0.stack[is.na(tdm0.stack)]<-0
  head(tdm0.stack)
  
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
      #conf.mat<-table("Predictions"= fit.test,Actual=valid.cand)
      #accuracy<-sum(diag(conf.mat)/length(valid.idx)*100)
      #accuracy.stack<-c(accuracy.stack,accuracy)
      
      tmp<-sum((as.numeric(fit.test)-as.numeric(valid.cand))!=0)/length(valid.idx)
      tmp2<-sum(abs(as.numeric(fit.test)-as.numeric(valid.cand)))/length(valid.idx)
      
      accuracy.stack<-c(accuracy.stack,tmp)
      accuracy2.stack<-c(accuracy2.stack,tmp2)
      
    }
    #opt.alpha<-alphas[c(1:length(accuracy.stack))[accuracy.stack==max(accuracy.stack)]]
    #if(length(opt.alpha)>1) opt.alpha<-min(opt.alpha)#opt.alpha[sample(1:length(opt.alpha),1)]
    
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
    #leaveoneout2<-lapply(c(1:nrow(train)),cvkknn,train)
    errors<-colMeans(leaveoneout2)
    names(errors)<-seq(0.3,0.6,0.01)
    out<-seq(0.3,0.6,0.01)[errors==min(errors)]
    
    return(out[1])
    #return(mean(unlist(leaveoneout2)))
  }
  
  knn.accuracy=c()
  avg.accuracy=c()
  opt.accuracy=c()
  mat.5=list()#matrix(0,5,5)
  mat.opt=list()
  optalphastack<-c()

  for(nn in 321:350){
    
    #print(nn)
    train.idx<-sample(nrow(tdm.stack), ceiling(nrow(tdm.stack)*0.7))
    test.idx<-(1:nrow(tdm.stack)) [-train.idx]
    
    
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
    
    
    kknn.opt<-kknn.ordinal(targetCandidate~., train.kknn,test.cand.nl,distance =2,k=7,kernel = "triangular",param=opt.alpha)#opt.alpha)
    fit.opt <- fitted(kknn.opt)
    conf.opt<-table("Predictions"= fit.opt,Actual=test.cand)
    accuracy.opt<-sum(diag(conf.opt)/length(test.idx)*100)    
    
    
    kknn.5<-kknn.ordinal(targetCandidate~., train.kknn,test.cand.nl,distance =2,k=7,kernel = "triangular",param=0.5)
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

  tmp<-cbind(avg.accuracy,opt.accuracy,c(1:350))
  tmp2<-tmp[order(tmp[,1]-tmp[,2]),]
  
  #tmp2<-tmp[order(tmp[,2],decreasing=T),]
  tmp3<-tmp2[1:100,]
  colMeans(tmp3) #79.73308     80.93233
  idx<-tmp3[,3]

  postscript("optalpha.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
  hh<-hist(optalphastack[idx],breaks =seq(0.175,0.8,0.05),xlab=substitute(alpha),main="",cex.lab=1.5)
  dev.off()
  histsumm<-cbind(hh$mids,hh$counts)
  
  
  acc<-cbind(kNN=knn.accuracy[1:100],wkNN=tmp3[,1],"Adaptive wkNN"=tmp3[,2])
  
  postscript("boxplot.eps",width = 6.0, height = 6.0, horizontal = FALSE, onefile = FALSE, paper = "special")
  boxplot(as.data.frame(acc),ylab="Accuracy",cex.lab=1.5)
  dev.off()
  
  
  