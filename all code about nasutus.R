#Second check
#nasutus
setwd("D:/Gibbon/Sound/Data analyse/Second check/east")
Zdata<-read.csv("Z_slim_add_boom.csv")
#note count for each note type
library(plyr)
file <- count(Zdata$note_name)
sum(file$freq)  #the number of notes
library(ggplot2)
ggplot(data=file, aes(x=x,y=freq))+
  geom_bar(stat = "identity",position=position_dodge(0.9),width = 0.5)+
  labs(x="Note Type",y="Number")+
  theme_bw()+theme(panel.grid=element_blank(),
                   axis.text= element_text(size=23),
                   axis.title= element_text(size=27))
#note count for each individual
file<- ddply(Zdata,.(individual_ID),function(x){
  length(x$note_name)
  }
  )
library(ggplot2)
ggplot(data=file, aes(x=individual_ID,y=V1))+
  geom_bar(stat = "identity",position=position_dodge(0.9),width = 0.5)+
  labs(x="Individual ID",y="Number of Notes")+
  theme_bw()+
  theme(panel.grid=element_blank(),axis.text= element_text(size=20),axis.title= element_text(size=25))
#note count for each note type for each individual
library(plyr)
file<- ddply(Zdata,.(individual_ID),function(x){count(x$note_name)
  }
  )
library(ggplot2)
ggplot(data=file, aes(x=x,y=freq))+
  geom_bar(stat = "identity",position=position_dodge(0.9),width = 0.5)+
  labs(y="Number of Notes",x = "Note Type")+
  facet_wrap(~individual_ID,scale="free")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.text= element_text(size=13),
        axis.title= element_text(size=25),
        panel.border = element_blank(),
        axis.line = element_line(colour = "black"),
        strip.background = element_rect(fill = 'white', colour = 'black', size = rel(2), linetype = "blank"),
        strip.text=element_text(size=rel(1.2)))
#boxplot of each note type
Zdata1<-read.csv("Z_slim(add_boom_for_boxplot)CORRECTED.csv")
library(ggplot2)
ggplot(data=Zdata1, aes(x=note_name,y=note_duration))+
  geom_boxplot()+
  labs(y="Duration (s)",x = "Note Type")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.title= element_text(size=27),
        axis.text= element_text(size=23))
Mseq_data<- read.csv("Mseq_slim.csv",header=T,sep=",")
length(unique(Mseq_data$seqID))    #the number of sequences
#seq count for each individual
Mseq_data<- read.csv("Mseq_slim.csv",header=T,sep=",")
library(plyr)
file<- ddply(Mseq_data,.(individual_ID),function(x){length(unique(x$seqID))
  }
  )
library(ggplot2)
ggplot(data=file, aes(x=individual_ID,y=V1))+
  geom_bar(stat = "identity",position=position_dodge(0.9),width = 0.5)+
  labs(x="Individual ID",y="Number of Sequences")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.title= element_text(size=25),
        axis.text= element_text(size=20))

#zlaw note mean
setwd("D:/Gibbon/Sound/Data analyse/Second check/east") #set work directory
Zdata<-read.csv("Z_slim_add_boom.csv",sep = ",",stringsAsFactors = F)
group.name <- as.character(unique(Zdata$individual_ID)) #individual names table
dat.sound <- data.frame()
for (i in group.name) {
  #i = group.name[1]
  group.table <- Zdata[Zdata$individual_ID == i,] #select sequences for each ivdividual
  sound <- as.character(unique(group.table$note_name)) #note type table
  for (j in sound) {
    #j=sound[1]
    sound.count <- length(group.table[group.table$note_name == j, 1]) #count the notes for each individual
    sound.time <- sum(group.table[group.table$note_name == j, "note_duration"])/as.numeric(sound.count)
    raw.dat <- cbind(i,j, sound.count,sound.time)
    colnames(raw.dat) <- c("individual_ID","note_name","quantity","mean_duration") #give colnames
    
    dat.sound <<- rbind(dat.sound,raw.dat)
  }
}
write.table(dat.sound,file = "Z_law_mean_add_boom.csv",
            row.names = F,sep = ",")
#LMM Z model
library(lme4)
library(lmerTest)
Z.mean <- read.csv("Z_law_mean_add_boom.csv",stringsAsFactors = F)
Z.mean$quantity <- as.numeric(Z.mean$quantity)
z.model <- lmer(mean_duration~quantity+(1|individual_ID),data=Z.mean)
summary(z.model)


#linear plot of note mean ~ quantity
setwd("D:/Gibbon/Sound/Data analyse/Second check/east")
Zdata<-read.csv("Z_slim_add_boom.csv")
dat.sound <- data.frame()
sound <- as.character(unique(Zdata$note_name)) #for each note type
for (j in sound) {#j=sound[1]
  sound.count <- as.numeric(length(Zdata[Zdata$note_name == j, 1]))
  sound.time <- sum(Zdata[Zdata$note_name == j, "note_duration"])/as.numeric(sound.count)
  STD <- sd(Zdata[Zdata$note_name == j,"note_duration"])
  raw.dat <- cbind(j, sound.count,sound.time,STD)
  colnames(raw.dat) <- c("note_name","quantity","mean_duration","SD")
  dat.sound <<- rbind(dat.sound,raw.dat)
  }
dat.sound$quantity <- as.numeric(as.character(dat.sound$quantity))#factor must be first transfered to character
dat.sound$mean_duration <- as.numeric(as.character(dat.sound$mean_duration))
dat.sound$SD <- as.numeric(as.character(dat.sound$SD))
str(dat.sound)
#begin to plot 
library(ggplot2)
ggplot(dat.sound, aes(x=dat.sound$quantity, y=dat.sound$mean_duration))+
  geom_point(size=3)+
  geom_text(aes(y = mean_duration + .2, label = note_name),size=8)+
  geom_errorbar(aes(ymax=dat.sound$mean_duration+dat.sound$SD,ymin=dat.sound$mean_duration-dat.sound$SD),
                position=position_dodge(0.9),width=0.80)+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.title= element_text(size=27),
        axis.text= element_text(size=23))+
  labs(x="Frequency",y="Duration (s)")


#linear plot of "delete pre" (dat.sound need second generation when run the next program twice)
dat.sound <- dat.sound[-which(dat.sound$note_name=="pre"),] #delete pre
library(ggplot2)
ggplot(dat.sound, aes(x=dat.sound$quantity, y=dat.sound$mean_duration))+
  geom_point()+
  geom_text(aes(y = mean_duration + .2, label = note_name))+
  geom_errorbar(aes(ymax=dat.sound$mean_duration+dat.sound$SD,ymin=dat.sound$mean_duration-dat.sound$SD),
                position=position_dodge(0.9),width=0.50)+geom_smooth(method = lm)+
  theme_bw()+
  theme(panel.grid=element_blank())+
  labs(x="Frequency",y="Duration (s)",title = "Relationship between Duration and Frequency (exclude pre)")


#delete pre&trill
Z.mean <- read.csv("Z_law_mean.csv",stringsAsFactors = F)
Z.mean <- Z.mean[-which(Z.mean$note_name=="pre"),] #delete pre
#LMM Z model (delete pre)
Z.mean$quantity <- as.numeric(Z.mean$quantity)
z.model <- lmer(mean_duration~quantity+(1|individual_ID),data=Z.mean)
summary(z.model)


#M law
#M law
Mseq_data<- read.csv("Mseq_slim.csv",header = T,sep = ",",stringsAsFactors = F) #read file
#model selection
library(MuMIn)
m<-lmer(note_duration~seqsize+note_position_in_sequence+note_position_in_bout+(1|individual_ID)+(1|bout_ID),
        data=Mseq_data,na.action=na.pass,REML=FALSE)
dd <- dredge(m, rank = "AIC")
dd
#M LMM model
Mseq.model <- lmer(note_duration~seqsize+note_position_in_sequence+note_position_in_bout+
                   (1|individual_ID)+(1|bout_ID),data=Mseq_data)#good result
plot(Mseq.model) # fitting diagnosis by residual
summary(Mseq.model)


###linear plot
###linear plot
#lm plot&model of "notemean ~ seqsize"
Mseq_data<- read.csv("Mseq_slim.csv",header=T,sep=",",stringsAsFactors = F) #read file
seqnumber <- data.frame()
seqsize <- as.numeric(unique(Mseq_data$seqsize)) #seqsize table
for (i in seqsize) {
  seq.table <- subset(Mseq_data,Mseq_data$seqsize==i) #select particular sequences which size==1
  notemean <- mean(seq.table$note_duration) #calculate mean note duration for those sequences
  STD <- sd(seq.table$note_duration) #calculate note sd for those sequences
  seqi <- cbind(i,notemean,STD) #combine
  colnames(seqi) <- c("seqsize","notemean","SD") #give column names
  seqnumber<<- rbind(seqnumber,seqi) #save the result in "seqnumber" table
  }
library(ggplot2)
ggplot(seqnumber, aes(x=seqsize, y=notemean))+
  geom_point(size=3)+
  geom_errorbar(aes(ymax=seqnumber$notemean+seqnumber$SD,ymin=seqnumber$notemean-seqnumber$SD),
                position=position_dodge(0.9),width=0.3)+
  labs(x="Sequence Size",y="Mean Duration of Notes (s)")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.title= element_text(size=27),
        axis.text= element_text(size=23))


#Durations of a Particular Type ~ Sequence Size
setwd("D:/Gibbon/Sound/Data analyse/Second check/east") #set work directory
Mseq_data<- read.csv("Mseq_slim.csv",header=T,sep=",",stringsAsFactors = F)
notetype <- as.character(unique(Mseq_data$note_name))
file <- data.frame()
for (i in notetype) {
  seq.table <- Mseq_data[Mseq_data$note_name==i,]#select note name
  seqsize.table <- as.character(unique(seq.table$seqsize))
  for (j in seqsize.table) {
    seq.table2 <- seq.table[seq.table$seqsize==j,]#select seqsize
    notemean <- mean(seq.table2$note_duration)
    STD <- sd(seq.table2$note_duration)
    A<- cbind(i,j,notemean,STD)
    colnames(A)=c("notename","seqsize","note_mean_duration","SD")
    file <- rbind(file,A)
    }
  }
str(file)
file$seqsize <- as.numeric(as.character(file$seqsize))
file$note_mean_duration <- as.numeric(as.character(file$note_mean_duration))
file$SD <- as.numeric(as.character(file$SD))
str(file)
library(ggplot2)
ggplot(file, aes(x=seqsize,y=note_mean_duration))+ 
  geom_point(stat="identity")+facet_wrap(~notename,scales = "free")+
  geom_errorbar(aes(ymax=note_mean_duration+SD,ymin=note_mean_duration-SD),
                position=position_dodge(0.9),width=0.15)+
  labs(x="Sequence Size",y="Mean Duration of Notes (s)")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        axis.text= element_text(size=15),
        axis.title= element_text(size=25),
        strip.text=element_text(size=rel(1.5)),
        strip.background = element_rect(fill = 'white', colour = 'black', size = rel(2), linetype = "blank"))

#linear mixed model results
Mseq_data<- read.csv("Mseq_slim.csv",header=T,sep=",",stringsAsFactors = F)
notetype <- as.character(unique(Mseq_data$note_name))
file <- data.frame()
individual <- as.character(unique(Mseq_data$individual_ID))
for (i in individual) {
  seq <- Mseq_data[Mseq_data$individual_ID==i,]
  for (j in notetype) {
    seq.table <- seq[seq$note_name==j,]#select note name
    seqsize.table <- as.character(unique(seq.table$seqsize))
    for (k in seqsize.table) {
      seq.table2 <- seq.table[seq.table$seqsize==k,]#select seqsize
      notemean <- mean(seq.table2$note_duration)
      STD <- sd(seq.table2$note_duration)
      A<- cbind(i,j,k,notemean,STD)
      colnames(A)=c("individual_ID","notename","seqsize","note_mean_duration","SD")
      file <- rbind(file,A)
      }
    }
  }
str(file)
file$seqsize <- as.numeric(as.character(file$seqsize))
file$note_mean_duration <- as.numeric(as.character(file$note_mean_duration))
file$SD <- as.numeric(as.character(file$SD))
str(file)
library(lme4)
for (i in notetype) {
  seq.table <- file[file$notename==i,]
  A <- lmer(note_mean_duration ~seqsize+(1|individual_ID),data=seq.table)
  print(i)
  print(summary(A))
  }
seq.table <- file[file$notename == "boom",]
A <- lm(note_mean_duration ~seqsize,data=seq.table)
print(summary(A))
seq.table <- file[file$notename == "mR4",]
A <- lm(note_mean_duration ~seqsize,data=seq.table)
print(summary(A))



#Proportion of a Particular Type ~ Sequence Size
Mseq_data<- read.csv("Mseq_slim.csv",header=T,sep=",",stringsAsFactors = F)
file <- data.frame()
seqsize.table <- as.character(unique(Mseq_data$seqsize))
notetype <- as.character(unique(Mseq_data$note_name))
for (i in seqsize.table) {
  seq.table1 <- Mseq_data[Mseq_data$seqsize==i,]#select all sequences whose size==i   
  IDtable <- as.character(unique(seq.table1$seqID)) #all seqID of sequences whose size==j
  for (j in IDtable) {
    seq.table2 <- seq.table1[seq.table1$seqID==j,]#select particular sequence
    for (k in notetype) {note.count <- length(seq.table2[seq.table2$note_name==k,]$note_name) #particular note type's quantity in sequence j
      prop <- note.count/as.numeric(i)
      A<- cbind(i,j,k,prop)
      colnames(A)=c("seqsize","seqID","notename","proportion")
      file <- rbind(file,A)
      }
    }
  }

file$proportion <- as.numeric(as.character(file$proportion))
propt <- data.frame()

notetype <- as.character(unique(Mseq_data$note_name))
for (i in seqsize.table) {
  seq.table3 <- file[file$seqsize==i,]
  for (k in notetype) {
    seq.table4 <- seq.table3[seq.table3$notename==k,]
    MEAN <- mean(seq.table4$proportion)
    STD <- sd(seq.table4$proportion)
    B<- cbind(i,k,MEAN,STD)
    colnames(B)=c("seqsize","notename","proportion","sd")
    propt <- rbind(propt,B)
    }
  }

propt$seqsize <- as.numeric(as.character(propt$seqsize))
propt$proportion <- as.numeric(as.character(propt$proportion))
propt$sd <- as.numeric(as.character(propt$sd))
str(propt)
library(ggplot2)
ggplot(propt, aes(x=seqsize,y=proportion))+ 
  geom_point(stat="identity")+
  geom_errorbar(aes(ymax=proportion+sd,ymin=proportion-sd),
                position=position_dodge(0.9),width=0.15)+
  facet_wrap(~notename,scales = "free")+
  labs(x="Sequence Size",y="Proportion of Notes within Sequence")+
  theme_bw()+
  theme(panel.grid=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_blank(),
        axis.text= element_text(size=15),
        axis.title= element_text(size=25),
        strip.text=element_text(size=rel(1.5)),
        strip.background = element_rect(fill = 'white', colour = 'black', size = rel(2), linetype = "blank"))

#linear model results
Mseq_data<- read.csv("Mseq_slim.csv",header=T,sep=",",stringsAsFactors = F)
file <- data.frame()
seqsize.table <- as.character(unique(Mseq_data$seqsize))
notetype <- as.character(unique(Mseq_data$note_name))
individual <- as.character(unique(Mseq_data$individual_ID))
for (q in individual) {
  seq <- Mseq_data[Mseq_data$individual_ID==q,]#select individual
  for (i in seqsize.table) {
    seq.table1 <- seq[seq$seqsize==i,]#select all sequences whose size==i   
    IDtable <- as.character(unique(seq.table1$seqID)) #all seqID of sequences whose size==j
    for (j in IDtable) {
      seq.table2 <- seq.table1[seq.table1$seqID==j,]#select particular sequence
      for (k in notetype) {
        note.count <- length(seq.table2[seq.table2$note_name==k,]$note_name) #particular note type's quantity in sequence j
        prop <- note.count/as.numeric(i)
        A<- cbind(q,i,j,k,prop)
        colnames(A)=c("individual_ID","seqsize","seqID","notename","proportion")
        file <- rbind(file,A)
        }
      }
    }
  }
file$proportion <- as.numeric(as.character(file$proportion))
file$seqsize <- as.numeric(as.character(file$seqsize))
#output the results of LMM for each notetype respectively
for (i in notetype) {
  seq.table <- file[file$notename==i,]
  A <- lmer(proportion~seqsize +(1|individual_ID),data=seq.table)
  print(i)
  print(summary(A))
  }