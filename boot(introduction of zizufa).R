
require(boot)

rsq <- function(data,formula,indices){      
  d <-data[indices,]                      #indices是必要的，用去重复抽样
  fit <- lm(formula,data=d)
  return(summary(fit)$r.square)          #用str可查看summary里面的赋值情况，返回需要的值
}                                       #此rsq函数是用于boot中循环取样所要确定置信区间的值


result <- boot(data=mtcars,statistic=rsq,R=1000,formula=mpg~wt+disp)  #formula是常见的...返回给statistic
print(result)
summary(result)
par(mfrow=c(1,2))
plot(result)
par(no.readonly = TRUE)

boot.ci(result,conf=0.95，type=c("perc","bca"))      #获得所需的置信区间
                                          #常见的有perc分位数和bca根据偏差作调整


###########################################################################################
                  #下面的函数是求多个统计量的自助法#


bs <- function(formula, data, indices) {
  d <- data[indices, ]
  fit <- lm(formula, data = d)
  return(coef(fit))                        #通过coef返回所有的参数
}

library(boot)
set.seed(1234)
results <- boot(data = mtcars, statistic = bs, 
                R = 1000, formula = mpg ~ wt + disp)
print(results)
plot(results, index = 2)                      #与单个的区别在于要指定index即现在需要哪个
boot.ci(results, type = "bca", index = 2)

  
#一般认为初始样本>20，重复抽样次数在1000次，会获得比较好的结果

