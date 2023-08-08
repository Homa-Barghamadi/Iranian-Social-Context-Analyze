rm(list=ls(all=TRUE))

Data=read.csv("~/Downloads/data-11.csv",header=T)
head(Data) 
View(Data)
Data<-Data[,-c(1,3,4,71)]
M<-Data[,c(5,6,14)]
D<-Data[,-c(5,6,14)]
D[is.na(D)]<-0

Data<-cbind(D,M)
Income=(Data[,62]+Data[,63]+Data[,64]+Data[,65])/(12*10^3)
D1=Data[,-(62:65)]

D2=cbind(D1,Income)
D2[,60]<-D2$Hazine_Tafrihat
D2[D2[,60]==0,60]<-10^-3
D2[,61]<-D2$Hazine_Pushak
D2[D2[,61]==0,61]<-10^-3
D2[,59]<-D2$Hazine_Noshidani
D2[D2[,59]==0,59]<-10^-3
D2[,58]<-D2$Hazine_Maskan
D2[D2[,58]==0,58]<-10^-3
D2[,56]<-D2$Hazine_lavazemkhanegi
D2[D2[,56]==0,56]<-10^-3
D2[,55]<-D2$Hazine_Khorakivadokhani
D2[D2[,55]==0,55]<-10^-3
D2[,54]<-D2$Hazine_kalavakhadamat
D2[D2[,54]==0,54]<-10^-3
D2[,53]<-D2$Hazine_Hamlonaghl
D2[D2[,53]==0,53]<-10^-3
D2[,52]<-D2$Hazine_Ghazayeamade
D2[D2[,52]==0,52]<-10^-3
D2[,51]<-D2$Hazine_Ertebatat
D2[D2[,51]==0,51]<-10^-3
D2[,50]<-D2$Hazine_Behdashti
D2[D2[,50]==0,50]<-10^-3


options(digits = 3,scipen = 999)
summary(D2)
dim(D2)
########################tasvir_sazi
#D2$C.O <- factor(D2$C.O, levels = c(1,2,23,30), labels=c("Gilan","Mazandaran","Tehran","Alborz"))
D2$Jens <- factor(D2$Jens, levels = c(1,2), labels=c("M","F"))
View(D2)

# plot(y=D2$Incomem ,x= D2$C.O, xlab = "C.O", ylab = "Incomem")

summary(D2$C.O)

library(ggplot2)
qplot(x = Sen, y = Income, data = D2, geom = c("point", "smooth"))
qplot(x = log(Hazine_Behdashti) , y = log(Income), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_Ertebatat), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_Ghazayeamade), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_Hamlonaghl), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_kalavakhadamat), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_Khorakivadokhani), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_kalavakhadamat), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_Maskan), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_Noshidani), data = D2, geom = c("point"))
qplot(x = log(Income), y = log(Hazine_Pushak), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_Tafrihat), data = D2, geom = c("point", "smooth"))
qplot(x = log(s.z), y = log(Income), data = D2, geom = c("point", "smooth"))
qplot(x = log(Sen), y = log(Income), data = D2, geom = c("point","smooth"))
# color = C.O

ggplot(D2, aes(x = log(Income))) + stat_density(color= "black", fill="#00AFBB")
qplot(log(Income), geom="histogram")
hist(log(Income))
plot(density(na.omit(Income)))
#boxplot
k <- table(D2$C.O)
p<-barplot(k, xlab="C.O",col = "darkolivedarkolivegreen44",cex.names=1,cex=0.8)
text(x = p, y = k - 100, labels = k,cex=0.8)


k <- table(D2$Jens)/765
p<-barplot(k, xlab="Jens",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.8)

table(D2$Faaliat)/765
k <- table(D2$Faaliat)
p<-barplot(k, xlab="Faaliat",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.8)

k <- table(D2$T.shaghel)
p<-barplot(k, xlab="T.shaghel",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.8)
table(D2$T.shaghel)/765

k <- table(D2$Tedad.a)
p<-barplot(k, xlab="tedad.a",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.8)
table(D2$Tedad.a)/765

k <- table(D2$t.o)
p<-barplot(k, xlab="t.o",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.7)
table(D2$t.o)/765

k <- table(D2$n.e)
p<-barplot(k, xlab="n.e",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.7)
table(D2$n.e)/765

k <- table(D2$Oto)
p<-barplot(k, xlab="Oto",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.7)
table(D2$Oto)/765

k <- table(D2$Do)
p<-barplot(k, xlab="DO",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.7)
table(D2$Do)/765

k <- table(D2$Mo)
p<-barplot(k, xlab="Mo",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.7)
table(D2$Mo)/765

k <- table(D2$tv.s+D2$tv.r)
p<-barplot(k, xlab="TV",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.7)
table(D2$tv.s+D2$tv.r)/765

k <- table(D2$Pc)
p<-barplot(k, xlab="Pc",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.7)
table(D2$Pc)/765

k <- table(D2$Mobile)
p<-barplot(k, xlab="Mobile",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.7)
table(D2$Mobile)/765

k <- table(D2$Internet)
p<-barplot(k, xlab="Internet",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.7)
table(D2$Internet)/765

k <- table(D2$Freeizer+D2$yakhchal.f)
p<-barplot(k, xlab="yakhcal",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.7)
table(D2$Freeizer+D2$yakhchal.f)/765

k <- table(D2$cooler.a)
p<-barplot(k, xlab="cooler",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.7)
table(D2$cooler.a)/765


k <- table(D2$cooler.g)
p<-barplot(k, xlab="cooler",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.7)
table(D2$cooler.g)/765

k <- table(D2$broodat.m)
p<-barplot(k, xlab="broodat.m",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.7)
table(D2$broodat.m)/765

k <- table(D2$hararat.m)
p<-barplot(k, xlab="hararat.m",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.7)
table(D2$hararat.m)/765

k <- table(D2$m.zarf)
p<-barplot(k, xlab="m.zarf",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.7)
table(D2$m.zarf)/765

k <- table(D2$sookht.p)
p<-barplot(k, xlab="sookht.p",col = "darkolivegreen4",cex.names=1,cex=0.8)
text(x = p, y = k - 150, labels = k,cex=0.7)
table(D2$sookht.p)/765

hist(D2$Sen,xlab="Sen",col = "darkolivegreen4")
boxplot(D2$Sen,xlab="sen",col = "darkolivegreen1")
summary(D2$Sen)

hist(D2$Hazine_Behdashti,xlab="Hazine_Behdashti",col = "darkolivegreen4")
boxplot(D2$Hazine_Behdashti,xlab="Hazine_Behdashti",col = "darkolivegreen1")
summary(D2$Hazine_Behdashti)

hist(D2$Hazine_Ertebatat,xlab="Hazine_Ertebatat",col = "darkolivegreen4")
boxplot(D2$Hazine_Ertebatat,xlab="Hazine_Ertebatat",col = "darkolivegreen1")
summary(D2$Hazine_Ertebatat)

hist(D2$Hazine_Khorakivadokhani,xlab="Hazine_Khorakivadokhani",col = "darkolivegreen4")
boxplot(D2$Hazine_Khorakivadokhani,xlab="Hazine_Khorakivadokhani",col = "darkolivegreen1")
summary(D2$Hazine_Khorakivadokhani)

hist(D2$Sen,xlab="sen",col = "darkolivegreen4")
boxplot(D2$Sen,xlab="Sen",col = "darkolivegreen1")
summary(D2$Sen)

qplot(D2$Income, geom="histogram",color ="blue")
qplot(log(D2$Income), geom="histogram",color ="blue")
ggplot(D2, aes(x = log(Income))) + stat_density(color= "black", fill="#00AFBB")
summary(D2$Income)
shapiro.test(D2$Income)

qplot(C.O,Income, data = D2,geom = "boxplot")
qplot(Jens,Income, data = D2,geom = "boxplot")
qplot(Sen, data = D2, geom = "histogram",color ="darkolivegreen1")
x11()
boxplot(Income~C.O,D2)


D2$Savad <-as.factor(D2$Savad)
qplot(Income,log(Rahn) , data = D2 , color=C.O)


x11()
a<-ggplot(D2, aes(x =log(Income)))
a + geom_bar(stat="bin")
a + geom_histogram(aes(color = Jens, fill = Jens),
                   alpha = 0.4, position = "identity") +
  scale_fill_manual(values=c("#00AFBB", "#E7B800")) +
  scale_color_manual(values=c("#00AFBB", "#E7B800"))
x11()
f <- ggplot(D2, aes(x = C.O, y = Income))
f + geom_bar(aes(fill = Jens), stat="identity") +
  scale_fill_manual(values = c("#E69F00", "#56B4E9","#00AFBB")) + geom_bar(stat="identity")

k<-table(D2$Jens)
e <- ggplot(D2, aes(x = Jens,y= mean(Income)))
e+ geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=mean(Income)), vjust=-2, color="black", size=4)


a1<-ggplot(D2, aes(x =C.O,y=Income))
a1+geom_bar(stat="identity")

a1<-ggplot(D2, aes(x =Sen,y=Income))
a1+geom_bar(stat="identity")
qplot(Sen,Income , data = D2 , color='darkolivegreen1')
qplot(x = Sen, y = Income, data = D2, geom = c("point", "smooth"))

a2<-ggplot(D2, aes(x =C.O,y=Income))
a2+geom_boxplot(aes(fill = Jens))

D2$Jens <- factor(D2$Jens)
a3<-ggplot(D2, aes(x =Jens,y=Income))
a3+geom_boxplot()

D2$Savad <- factor(D2$Savad)
a3<-ggplot(D2, aes(x =Savad,y=Income))
a3+geom_boxplot()

D2$Faaliat <-as.factor(D2$Faaliat)
a4<-ggplot(D2, aes(x =Faaliat,y=Income))
a4+geom_boxplot()

D2$T.shaghel <- as.factor(D2$T.shaghel)
a5<-ggplot(D2, aes(x =T.shaghel,y=Income))
a5+geom_boxplot()

D2$tedad.a <- factor(D2$tedad.a)
a3<-ggplot(D2, aes(x =tedad.a,y=Income))
a3+geom_boxplot()

D2$n.t.m <- as.factor(D2$n.t.m)
a3<-ggplot(D2, aes(x =n.t.m,y=Income))
a3+geom_boxplot()

D2$C.O <- factor(D2$C.O)
a3<-ggplot(D2, aes(x =C.O,y=Income))
a3+geom_bar()

D2$t.o <- as.integer(D2$t.o)
a3<-ggplot(D2, aes(x =t.o,y=Income))
a3+geom_boxplot()

D2$n.e <- as.factor(D2$n.e)
a3<-ggplot(D2, aes(x =n.e,y=Income),color=C.O)
a3+geom_boxplot()

D2$oto <-as.factor(D2$Oto)
a3<-ggplot(D2, aes(x =oto,y=Income),color=C.O)
a3+geom_boxplot()

D2$mo <- as.factor(D2$Mo)
a3<-ggplot(D2, aes(x =mo,y=Income),color=C.O)
a3+geom_boxplot()


D2$pc <- as.factor(D2$Pc)
a3<-ggplot(D2, aes(x =pc,y=Income),color=C.O)
a3+geom_boxplot()

D2$mobile <- as.factor(D2$Mobile)
a3<-ggplot(D2, aes(x =mobile,y=Income),color=C.O)
a3+geom_boxplot()


D2$internet <- as.factor(D2$Internet)
a3<-ggplot(D2, aes(x =internet,y=Income),color=C.O)
a3+geom_boxplot()

D2$hamam <- as.factor(D2$hamam)
a3<-ggplot(D2, aes(x =hamam,y=Income),color=C.O)
a3+geom_boxplot()

D2$ashpazkhane <- as.factor(D2$ashpazkhane)
a3<-ggplot(D2, aes(x =ashpazkhane,y=Income),color=C.O)
a3+geom_boxplot()

D2$fazelab <- as.factor(D2$fazelab)
a3<-ggplot(D2, aes(x =fazelab,y=Income),color=C.O)
a3+geom_boxplot()

D2$sookht.p <- as.factor(D2$sookht.p)
a3<-ggplot(D2, aes(x =sookht.p,y=Income),color=C.O)
a3+geom_boxplot()

D2$sookht.g <- as.factor(D2$sookht.g)
a3<-ggplot(D2, aes(x =sookht.g,y=Income),color=C.O)
a3+geom_boxplot()

D2$sookht.ab <- as.factor(D2$sookht.ab)
a3<-ggplot(D2, aes(x =sookht.ab,y=Income),color=C.O)
a3+geom_boxplot()

D2$Tahsil.Mikonad <- as.factor(D2$Tahsil.Mikonad)
a3<-ggplot(D2, aes(x =Tahsil.Mikonad ,y=Income))
a3+geom_boxplot()

D2$Madrak <- as.factor(D2$Madrak)
a3<-ggplot(D2, aes(x =Madrak ,y=Income))
a3+geom_boxplot()

D2$m.o.b <- as.factor(D2$m.o.b)
a3<-ggplot(D2, aes(x =m.o.b ,y=Income))
a3+geom_boxplot()

D2$tedad.a <- factor(D2$tedad.a)
a3<-ggplot(D2, aes(x =tedad.a ,y=Income))
a3+geom_boxplot()
names(D2)

qplot(x = log(Hazine_Behdashti) , y = log(Income), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_Ertebatat), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_Ghazayeamade), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_Hamlonaghl), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_kalavakhadamat), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_Khorakivadokhani), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_kalavakhadamat), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_Maskan), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_Noshidani), data = D2, geom = c("point"))
qplot(x = log(Income), y = log(Hazine_Pushak), data = D2, geom = c("point", "smooth"))
qplot(x = log(Income), y = log(Hazine_Tafrihat), data = D2, geom = c("point", "smooth"))
qplot(x = log(s.z), y = log(Income), data = D2, geom = c("point", "smooth"))
qplot(x = log(Sen), y = log(Income), data = D2, geom = c("point","smooth"))


library("GGally")
#D3 <- D2[, c(1,3,4,5,60,62)]
D4 <- D2[, c(50,51,52,53,54,55,56,57,58,59,60,61,65)]
names(D4)
names(D4)=c("H.b","H.e","H.gh","H.h","H.k","H.kh","H.l","rahn","H.m","H.n","H.p","H.t","In")
ggcorr(D4, label = TRUE)
ggpairs(D4, label = TRUE)
#####################################################################################
library(stats)
q=quantile(Income,0.7)
rade=as.numeric(Income>q)
D2<-cbind(D2,rade)
View(D2)
length(D2)
summary(Income)
D3=D2[-3132,]
View(D3)
summary(D3$Income)
a<-qplot(x = log(Income), y = rade, data = D3, geom = c("point"))


###PCA
names(D2)
options(scipen=99,digits = 2)
pcs <- prcomp(D2[,c(3,10,50,51,52,53,54,55,56,57,58,59,60,61,65)],scale. = T) 
summary(pcs)
pcs$rot
scores <- pcs$x
head(scores,10)


# TREE

#install.packages("rpart")
#install.packages("rpart.plot")
library(rpart)
library(ggplot2)
library("e1071")
library("caret")
library(rpart.plot)
library(lattice)

D7<-D2[,c(1,3,5,7,12,13,18,25,29,32,38,43,51,52,53,55,58,60,66)]

set.seed(123)
train.index<-sample(c(1:dim(D7)[1]),dim(D7)[1]*0.6)
train<-D7[train.index,]
valid<-D7[-train.index,]

class.tree <- rpart(rade ~ ., data = train, method = "class",model=TRUE)
prp(class.tree, type = 1, extra = 1, under = TRUE, split.font = 4, varlen = -5,
    cex=.7,branch =1 ,box.palette=c("skyblue"))

class.tree.t <- predict(class.tree,train,type = "class")
confusionMatrix(as.factor(class.tree.t),as.factor(train$rade))

class.tree.v<-rpart(rade~., data = valid ,method = "class", model=TRUE)
prp(class.tree.v,type =0,extra = 1,under = T,split.font = 1,
    varlen = -10,cex=.7 ,box.palette=c("skyblue"))

class.tree.p <- predict(class.tree.v,valid,type = "class")
confusionMatrix(as.factor(class.tree.p),as.factor(valid$rade))


deeper.ct <- rpart(rade ~ ., data = train, method = "class", cp = 0, minsplit = 1)
# count number of leaves
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])
# plot tree
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10,
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'red', 'blue'))
######################################################################
deeper.tree<- rpart(rade ~ ., data = train, method = "class",cp= .00001,minsplit =5)
options(digits=8)
printcp(deeper.tree)
which.min(deeper.tree$cptable[,"xerror"])
######################################################################

cv.tree<-rpart(rade~.,data = train ,method = "class", 
               cp = 0.01071429, minsplit =5,xval =5, model=TRUE)         
prp(cv.tree,type =0,extra = 1,under = T,split.font = 1,
    varlen = -10,cex=.5 ,box.palette=c("skyblue"))

cv.tree.p <- predict(cv.tree,train,type = "class")
confusionMatrix(as.factor(cv.tree.p),as.factor(train$rade))

cv.tree.v<-rpart(rade ~., data = valid,method = "class",cp = 0.01071429 , minsplit =5,
                 xval =5, model=TRUE)
prp(cv.tree.v,type =0,extra = 1,under = T,split.font = 3,
    varlen = -5,cex=.4,branch =1 ,box.palette=c("skyblue"))

cv.tree.p.v <- predict(cv.tree.v,valid,type = "class")
confusionMatrix(as.factor(cv.tree.p.v),as.factor(valid$rade))


# K-N-N
length(D2)
D6<-D2[,c(1,3,5,7,12,13,18,20,25,29,32,38,43,51,52,53,55,58,60,66)]
#D6<-as.matrix(D6)
names(D6)
set.seed(123)
Train <- sample(rownames(D6), dim(D6)[1]*0.5)
Valid <- sample(setdiff(rownames(D6), Train),dim(D6)[1]*0.3)
Test <- setdiff(rownames(D6), union(Train, Valid))

#s<-D2$Sen
#plot(s[1:20] ~ Income[1:20], data=train,pch=ifelse(train$rade==1, 2, 3))
#text(x=Income[1:50], y=s[1:50], labels=rownames(train)[1:50], pos=4,cex = 0.8)
#text(31030, 50.9, "X",col="red")
#legend("topright", c("3dahak bala", "7dahak paeen", "data new"), pch = c(2, 3, 4),bty="]", cex=0.7)

# initialize normalized training, validation data, complete data frames to originals
train.norm <- D6[Train,]
valid.norm <- D6[Valid,]
test.norm <- D6[Test,]

library(caret)
# use preProcess() from the caret package to normalize Income and Lot_Size.
norm.values <- preProcess(train.norm[,1:19], method=c("center", "scale"))
train.norm[, 1:19] <- predict(norm.values, train.norm[, 1:19])
valid.norm[, 1:19] <- predict(norm.values, valid.norm[, 1:19])
test.norm[, 1:19] <- predict(norm.values, test.norm[, 1:19])


#install.packages("FNN")
library(FNN)
nn <- knn(train = train.norm[, 1:19], test = valid.norm[,1:19],cl = train.norm[,20], k = 3)
row.names(train.norm)[attr(nn, "nn.index")]

knn.pred1 <- knn(train.norm[, 1:19], valid.norm[, 1:19],cl = train.norm[, 20], k = 2)
confusionMatrix(as.factor(knn.pred1),as.factor(valid.norm[,20]))

knn.pred2 <- knn(train.norm[, 1:19], test.norm[, 1:19],cl = train.norm[, 20], k = 9)
confusionMatrix(as.factor(knn.pred2),as.factor(test.norm[,20]))

knn.pred3 <- knn(train.norm[, 1:19], train.norm[, 1:19],cl = train.norm[, 20], k = 3)
confusionMatrix(as.factor(knn.pred3),as.factor(train.norm[,20]))

a=matrix(0,nrow=20,ncol=2)
for(i in 1:20) {
  knn.pred <- knn(train.norm[, 1:19], valid.norm[, 1:19],
                  cl = train.norm[, 20], k = i)
  a[i, 2] <- confusionMatrix(as.factor(knn.pred),as.factor(valid.norm[,20]))$overal[1]
  a[i,1]=i
}
a
