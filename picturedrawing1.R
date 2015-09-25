# 5 Orange trees over time

Orange$Tree <- as.numeric(Orange$Tree)      
ntrees <- max(Orange$Tree)

xrange <- range(Orange$age)                 
yrange <- range(Orange$circumference)        

plot(xrange, yrange,                            #生成一个空坐标
    type="n",                               
    xlab="Age (days)",                      
    ylab="Circumference (mm)"               
 )                                          

colors <- rainbow(ntrees)                   
linetype <- c(1:ntrees)                     
plotchar <- seq(18, 18+ntrees, 1)              #确定图形的参数

for (i in 1:ntrees) {                       
    tree <- subset(Orange, Tree==i)          
    lines(tree$age, tree$circumference,      
        type="b",                           
        lwd=2,                              
        lty=linetype[i],                     
        col=colors[i],                      
        pch=plotchar[i]                     
    )                                       
}                                            

title("Tree Growth", "example of line plot")

legend(xrange[1], yrange[2],                    #刻画图形的图例，其中前两个为图例的摆放位置
    1:ntrees,                                    
    cex=0.8,                                
    col=colors,                             
    pch=plotchar,                           
    lty=linetype,                           
    title="Tree"                            
    )                