```{r}
data<-read.csv("Spreadsheet.csv")
```
<!-- I loaded the data. -->

```{r}
str(data)
```
<!-- I read the data and checked the structure. -->

```{r}
data$Subject<-factor(data$Subject)
```

<!-- I changed Subject into a factor variable. -->

```{r}
data$Time<-factor(data$Time)
```

<!-- I changed Time into a factor variable. -->

```{r}
str(data)
```
<!-- I checked the structure again. Looks correct. -->


```{r}
summary(data$Total[data$Time==1])
```

<!-- I found the minimum, median, and mean of Pretest scores. At or above
median of 40.50 is high motivation, below 40.50 is low motivation.-->


```{r}
summary(data$Total[data$Time==2])
```

<!-- I found the minimum, median, and mean of Test 2 scores. At or above 
median of 43.00 is high motivation, below 43.00 is low motivation. -->

  
```{r}
summary(aov(Total[data$Time==2]~Motivation[data$Time==2]*Feedback[data$Time==2],
data=data))
``` 

<!--I ran a 2 way mixed design Anova to analyse Time (Posttest total IMI scores)
as a function of Motivation and Feedback. For Motivation, there was no significance
at F(1,21)=0.14,p=0.71. For Feedback, there was no significance at
F (1,21)=0.09,p=0.77.  -->


```{r}
summary(aov(Total~Time*Feedback+Error(Subject/Time),data=data))
``` 

<!-- I ran a mixed design Anova for Total Pretest and Posttest IMI scores as a 
function of Time and Feedback, this time accounting for the variability of the 
subjects in the error term for the within subjects variables. At F(1,21)=27.97
p<0.001, Feedback interacted with Time. -->


```{r}
t.test(data$Total[data$Time==1 & data$Feedback=="P"],data$Total[data$Time==1 
& data$Feedback=="N"])  
``` 

<!-- I wanted to see if there was a difference in Pretest scores (Time==1) before 
the manipulation of Positve or Negative Feedback. Because there were 2 different
groups of subjects, I ran a Welch Two Sample t-test that shows at df = 21.053, 
p-value = 0.25, there is no signifant difference. -->



```{r}
t.test(data$Total[data$Time==2 & data$Feedback=="P"],data$Total[data$Time==2 
& data$Feedback=="N"]) 
``` 

<!-- I wanted to see if there was a difference in Posttest scores (Time==2) 
after manipulation, between Positve or Negative Feedback. Because there were 2 
different groups of subjects, I ran a Welch Two Sample t-test that shows at 
df = 21.67, p-value = 0.81, there is no signifant difference. -->


```{r}
t.test(data$Total[data$Time==1 & data$Feedback=="N"],data$Total[data$Time==2  
& data$Feedback=="N"],paired=TRUE)                                                                         
``` 

<!--  I wanted to see if there was a difference in Pretest and Posttest scores 
(Time==1 & Time==2) for subjects in the Negative Feedback condition. Because 
subjects were one group,I ran a Paired t-test that shows at 
df = 11, p-value = 0.5123, there is no signifant difference. --> 

```{r}
t.test(data$Total[data$Time==1 & data$Feedback=="P"],data$Total[data$Time==2 
& data$Feedback=="P"],paired=TRUE)  
``` 

<!--  I wanted to see if there was a difference in Pretest and Posttest scores 
(Time==1 & Time==2) for subjects in the Positive Feedback condition. Because 
subjects were one group,I ran a Paired t-test that shows at 
df = 11, p-value = 0.04 (p<0.05), there is signifant difference. -->


```{r}
library("gplots")
library("ggplot2")
library("dplyr")
col1=col2hex("deeppink")
col2=col2hex("deepskyblue2")
temp<-data%>%group_by(Feedback,Time)%>%
summarize(means=mean(Total),sems=sd(Total)/sqrt(length(Total)))
f<-ggplot(temp, aes(x=Time, y=means, fill=Feedback))+
  geom_bar(stat="identity",position=position_dodge())+
  scale_fill_manual(values=c(col1,col2),name="Feedback",breaks=c("N","P"),labels=c("Negative", "Positive"))+
  theme(legend.key=element_rect(color="black"))+
  geom_errorbar(aes(ymax=means+sems, ymin=means-sems),width=.2,position=position_dodge(.9))+
  ggtitle("IMI Total Score by Time and Feedback")+
  labs(x="Time",y="Total IMI Score")+
  scale_x_discrete(breaks=c("1","2"),labels=c("Pretest","Posttest"))+
  theme(plot.title=element_text(size=15,face="bold",vjust=.5))+
   theme(axis.title.x=element_text(size=12,face="bold",vjust=-.25))+
   theme(axis.title.y=element_text(size=12,face="bold",vjust=1))+
   theme(axis.text.x=element_text(size=10,face="bold",color="black"))+
   theme(axis.text.y=element_text(size=10,face="bold",color="black"))+
   coord_cartesian(ylim=c(min(temp$means)-2*max(temp$sems),
                           + max(temp$means)+2*max(temp$sems)))+
  theme(panel.border=element_blank(),axis.line=element_line())+
  theme(panel.grid.major.x=element_blank())+
theme(panel.grid.major.y=element_line(color="darkgrey"))+
  theme(panel.grid.minor.y=element_blank())+
   theme(legend.position=c(.37,.85))+
   theme(legend.background=element_blank())+
   theme(legend.background=element_rect(color="black"))+
   theme(legend.title=element_blank())+
   theme(legend.title=element_text(size=12))+
   theme(legend.title.align=.5)+
   theme(legend.text=element_text(size=10,face="bold"))
f
``` 

<!-- I created a bar graph to represent the results. -->













