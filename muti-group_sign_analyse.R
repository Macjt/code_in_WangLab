根据这些代码给出一份画图的数据
# 读取数据；
dt <- read.csv("test.csv",header = T)
# 载入rstatix包；
library(rstatix)
library(dplyr)
# 按分组统计信息；
stats_summary <- dt %>% df_group_by(Group) %>% get_summary_stats()
# 对数据进行正态性检验(Shapiro-Wilk Normality Test)；
dt %>% shapiro_test(vars = "Expressions")
# 使用两种方法进行方差齐性检验；
# Levene’s test for homogeneity of variance；
dt$Group <- as.factor(dt$Group)
dt %>% levene_test(Expressions ~ Group)
# Bartlett test of homogeneity of variances；
bartlett.test(Expressions~Group,data = dt)
# 从两种方法的统计结果来看（p-value 分别为0.1629 和0.278），方差是齐性的。
# 使用rstatix包进行方差分析；
dt %>% anova_test(Expressions~Group)
# 使用基础函数进行方差分析，整体来看差异显著;
oneway<-aov(Expressions~Group,data = dt)
anova(oneway)

# 使用Fisher LSD法进行均值比较；
# LSD法（Fisher’s Least Significant Difference）；
# LSD法检验微小的差异，比较方便的是直接得出显著性字母标记，不需人工标记；
# install.packages("agricolae")
library("agricolae")
out <- LSD.test(oneway,"Group",p.adj="bonferroni")
# 整理用于作图的数据框
rowname<-row.names(out$means)
mean<-out$means[,1]
sd<-out$means[,2]
marker<-out$groups$groups
plotdata<-data.frame(rowname,mean,sd,marker)
# ggplot2 绘制带显著性标记的柱状图
library("ggplot2")
p1<-ggplot(plotdata,aes(x=factor(rowname),y=mean))+geom_bar(position =position_dodge(0),fill="orange",width = 0.52,stat = "identity")
p2<-p1+geom_errorbar(aes(ymin=mean-sd,ymax=mean+sd),position=position_dodge(0.6),width=0.25)
p3<-p2+geom_text(aes(x=factor(rowname),y=mean+sd+0.1,label=marker),size=4,position = position_dodge(0.6))
p4<-p3+xlab("")+ylab("Lesion diameter (mm)")
p5<-p4+scale_y_continuous(limits = c(0, 4),expand=expansion(add = c(0, 0)))
# 更改y轴显示范围，这里的expand默认为TRUE。
mytheme<-theme_classic()+theme(axis.title = element_text(size = 12),
                               axis.text = element_text(size=12),
                               panel.grid.major = element_line(color = "white"),
                               panel.grid.minor = element_line(colour = "white"),
                               axis.text.x = element_text(size = 12,angle=45,vjust=0.7,hjust=0.8,color = "black"),
                               axis.text.y = element_text(size = 12,color = "black"),
                               legend.text = element_text(size = 12),legend.title = element_blank(),
                               plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
p5+mytheme
