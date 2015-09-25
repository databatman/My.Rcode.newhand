p<-seq(0.4,0.9,0.1)
lp<-length(p)
r<-seq(0.1,0.5,0.01)
lr<-length(r)
store_rp<-matrix(numeric(lr*lp),lp,lr)     #build a matrix to store value


for(i in 1:lp){
  for(j in 1:lr){
    rtest<-pwr.r.test(r=r[j],sig.level=0.05,power=p[i],alternative = "two.sided")
    store_rp[i,j]<-ceiling(rtest$n)
  }
}          #calculate the sample size n

xrange<-range(r)
yrange<-range(ceiling(store_rp))
colors<-rainbow(lp)
par(las=2)
plot(xrange,yrange,type="n",main="n-r relation test in value P",xlab="correlation coefficient",
     ylab="sample size n",sub="Sig=0.05(Two tailed)") 
                           #plot a blank picture withou anything
     

for(i in 1:lp){
  lines(r,store_rp[i,],type="l",lwd=2,col=colors[i])
}                                  # plot 6 lines n-r in p value


legend("topright",title="power",as.character(p),fill=colors)   #add legend
