
require(boot)

rsq <- function(data,formula,indices){      
  d <-data[indices,]                      #indices�Ǳ�Ҫ�ģ���ȥ�ظ�����
  fit <- lm(formula,data=d)
  return(summary(fit)$r.square)          #��str�ɲ鿴summary����ĸ�ֵ�����������Ҫ��ֵ
}                                       #��rsq����������boot��ѭ��ȡ����Ҫȷ�����������ֵ


result <- boot(data=mtcars,statistic=rsq,R=1000,formula=mpg~wt+disp)  #formula�ǳ�����...���ظ�statistic
print(result)
summary(result)
par(mfrow=c(1,2))
plot(result)
par(no.readonly = TRUE)

boot.ci(result,conf=0.95��type=c("perc","bca"))      #����������������
                                          #��������perc��λ����bca����ƫ��������


###########################################################################################
                  #����ĺ���������ͳ������������#


bs <- function(formula, data, indices) {
  d <- data[indices, ]
  fit <- lm(formula, data = d)
  return(coef(fit))                        #ͨ��coef�������еĲ���
}

library(boot)
set.seed(1234)
results <- boot(data = mtcars, statistic = bs, 
                R = 1000, formula = mpg ~ wt + disp)
print(results)
plot(results, index = 2)                      #�뵥������������Ҫָ��index��������Ҫ�ĸ�
boot.ci(results, type = "bca", index = 2)

  
#һ����Ϊ��ʼ����>20���ظ�����������1000�Σ����ñȽϺõĽ��
