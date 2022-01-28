#### Exploratory Work Tracing High Earmarkers


library(googlesheets4)
library(googledrive)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(readxl)

drive_find(n_max = 30)
#drive_find(type = "spreadsheet")

#read_sheet()

#drive_ls("Correspondence")
#drive_ls("Correspondence/datasheets")

### Creating an object called doe_ferc that has the ferc sheet in it.  Note that these two are identical methods of how to call the sheet via link or via sheet name
doe_ferc<-drive_get("DOE_FERC") %>% 
  read_sheet()
#read_sheet("https://docs.google.com/spreadsheets/d/1iUwHnVd6jdrDh39-XUT685xMmT6ffCAN-L7pkorVrvM/edit#gid=1008446043")

#### Searching for "SCHUMER" in the FERC data
as.matrix(doe_ferc)[grepl("SCHUMER",as.matrix(doe_ferc)),]
tmp_cochranferc<-doe_ferc[str_detect(doe_ferc$last_name,"COCHRAN"),]
tmp_earmark<-doe_ferc[str_detect(doe_ferc$SUBJECT,"earmark"),]

#### # Looking at the USDA Agricultural Research Service
usda_ars<-read_sheet("https://docs.google.com/spreadsheets/d/1AUNlUQY4crfJ6AHdNvZXB9lbGhveL7HqZOvKUH0TYg0/edit#gid=0")
tmp_cochranusdaars<-usda_ars[str_detect(usda_ars$FROM,"Cochran"),]

### Looking at DOT_FHWA - Department of Transportation Federal Highway Adminsitration
## commenting out the code to look at the raw googlesheets
### Note that historically this agency has been involved in a lot of earmarks
#dot_fhwa<-read_sheet("https://docs.google.com/spreadsheets/d/1WHEU8f73opKs13smHX8NVbitXgpv83zGfp_DhnU6NEI/edit#gid=1436701610")
### Note inconsistent date format.  Some: YYYY-MM-DD, others mm/dd/yy
#unique(nchar(dot_fhwa$DATE))
### WORKING ON THE BELOW
#dot_fhwa$year<-NA
#for (i in 1:nrow(dot_fhwa)){
#  if(grepl("/",dot_fhwa$DATE[i])){dot_fhwa$year[i]<-paste0(20,strsplit(dot_fhwa$DATE[i],"/")[[1]][3])}
#  if(grepl("-",dot_fhwa$DATE[i])){dot_fhwa$year[i]<-strsplit(dot_fhwa$DATE[i],"-")[[1]][1]}
#}
#dot_fhwa$year<-as.numeric(as.character(dot_fhwa$year))
#unique(dot_fhwa$year)
#dot_fhwa$year[dot_fhwa$`Control Number`=="FHWA-110330-005"]<-2011
## note the 2001 is likely a typo for 2011
### looking at breakdown of FHWA by year. 
#signif(prop.table(table(dot_fhwa$year)),digits=2)
#pdf("~/Dropbox/Correspondence/figs/earmark/dot_fhwa_googlesheet.pdf")
#ggplot(data=dot_fhwa,aes(year))+
#  geom_bar(aes(y = (..count..)/sum(..count..))) + 
#  geom_vline(xintercept=2010.5)+
#  geom_text(x=2009,y=.15,aes(label="Earmark Ban"),size=6)+
#  scale_y_continuous(labels=scales::percent) +
#  ylab("")+
#  xlab("Year")+
#   ggtitle("FHWA Letters GoogleSheet")+
#  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))
#dev.off()
#### the above is the proportion data.  This is the count plot
#ggplot(data=dot_fhwa,aes(year))+
#  geom_bar(aes(y = (..count..))) + 
#  geom_vline(xintercept=2010.5)+
#  geom_text(x=2009,y=.15,aes(label="Earmark Ban"),size=6)+
#  #scale_y_continuous(labels=scales::percent) +
#  ylab("")+
#  xlab("Year")+
#  ggtitle("FHWA Letters GoogleSheet")+
#  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))



#### Setting the wd from the dropbox
setwd("~/Dropbox/Correspondence/data")
### This should be the cleaned datafile
load("~/Dropbox/Correspondence/data/all_contacts.Rdata")
#### To do -- look at the pre-post earmark bans for different agencies. 
### looking at breakdown of FHWA by year. 
dot_fhwa<-subset(all_contacts,agency=="DOT_FHWA")
table(dot_fhwa$year)

#pdf("~/Dropbox/Correspondence/figs/earmark/dot_fhwa_cleaned.pdf")
#ggplot(data=dot_fhwa,aes(year))+
#  geom_bar(aes(y = (..count..)/sum(..count..))) + 
#  geom_vline(xintercept=2010.5,size=3)+
#  geom_text(x=2009,y=.15,aes(label="Earmark Ban"),size=6)+
#  scale_y_continuous(labels=scales::percent) +
#  ylab("")+
#  xlab("Year")+
#  ggtitle("FHWA Letters Cleaned Data")+
#  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))
#dev.off()

### the above is the proportion data.  This is the raw counts by year
pdf("~/Dropbox/Correspondence/figs/earmark/dot_fhwa_cleaned.pdf")
ggplot(data=dot_fhwa,aes(year))+
  geom_bar(aes(y = (..count..))) + 
  geom_vline(xintercept=2010.5,size=2)+
  geom_text(x=2009.3,y=1250,aes(label="Earmark Ban"),size=6)+
  #scale_y_continuous(labels=scales::percent) +
  ylab("Letters to FHWA")+
  xlab("Year")+
  ggtitle("")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))
dev.off()


#### Note the TIGER grants are discretionary grant programs
### https://www.transportation.gov/sites/dot.gov/files/docs/TIGER_DISCRETIONARY_GRANT_PROGRAM.pdf
### Lots of letters from legislators in support of a local municipality's application
### seems like it could be lettermarking

### look for words: "support", "allocate", "TIGER", "endorse"
supportida<-grep("support",dot_fhwa$SUBJECT,ignore.case=TRUE)
supportidb<-grep("allocate",dot_fhwa$SUBJECT,ignore.case=TRUE)
supportidc<-grep("endorse",dot_fhwa$SUBJECT,ignore.case=TRUE)
supportid<-c(supportida,supportidb,supportidc)
supportid<-unique(supportid)

supportmini<-dot_fhwa[supportid,]
dot_fhwa$support<-0
dot_fhwa$support[supportid]<-1
table(supportmini$year)
### proportion plot
#pdf("~/Dropbox/Correspondence/figs/earmark/dot_fhwa_support.pdf")
#ggplot(data=supportmini,aes(year))+
#  geom_bar(aes(y = (..count..)/sum(..count..))) + 
#  geom_vline(xintercept=2010.5,size=3)+
#  geom_text(x=2009,y=.18,aes(label="Earmark Ban"),size=6)+
#  scale_y_continuous(labels=scales::percent) +
#  ylab("")+
#  xlab("Year")+
#  ggtitle("Letters in Support of a Project to FHWA")+
#  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))
#dev.off()
### raw counts
pdf("~/Dropbox/Correspondence/figs/earmark/dot_fhwa_support.pdf")
ggplot(data=supportmini,aes(year))+
  geom_bar(aes(y = (..count..))) + 
  geom_vline(xintercept=2010.5,size=2)+
  geom_text(x=2009.3,y=850,aes(label="Earmark Ban"),size=6)+
  #scale_y_continuous(labels=scales::percent) +
  ylab("Letters in Support of a Project to FHWA")+
  xlab("Year")+
  ggtitle("")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))
dev.off()





#### TIGER DISCRETIONARY GRANT PROGRAM
supportid<-grep("TIGER",dot_fhwa$SUBJECT,ignore.case=TRUE)
supportmini<-dot_fhwa[supportid,]
dot_fhwa$support<-0
dot_fhwa$support[supportid]<-1
table(supportmini$year)
#### proportion plot
#pdf("~/Dropbox/Correspondence/figs/earmark/dot_fhwa_tiger.pdf")
#ggplot(data=supportmini,aes(year))+
#  geom_bar(aes(y = (..count..)/sum(..count..))) + 
#  geom_vline(xintercept=2010.5,size=3)+
#  geom_text(x=2009,y=.18,aes(label="Earmark Ban"),size=6)+
#  scale_y_continuous(labels=scales::percent) +
#  ylab("")+
#  xlab("Year")+
 # ggtitle("Letters in Support of a Project to FHWA")+
#  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))
#dev.off()
### raw counts
pdf("~/Dropbox/Correspondence/figs/earmark/dot_fhwa_tiger.pdf")
ggplot(data=supportmini,aes(year))+
  geom_bar(aes(y = (..count..))) + 
  geom_vline(xintercept=2010.5,size=2)+
  geom_text(x=2009.3,y=550,aes(label="Earmark Ban"),size=6)+
  #scale_y_continuous(labels=scales::percent) +
  ylab("TIGER Letters to FHWA")+
  xlab("Year")+
  ggtitle("")+
  theme(axis.text=element_text(size=16), axis.title=element_text(size=16))
dev.off()



#### EXPLORING OTHER PROGRAMS THAT WERE FREQUENTLY EARMARKED
### High Priority Grant Program
supportid<-grep("HPP",dot_fhwa$SUBJECT,ignore.case=TRUE)
supportid<-grep("High Priority",dot_fhwa$SUBJECT,ignore.case=TRUE)
supportmini<-dot_fhwa[supportid,]
dim(supportmini)
supportmini$SUBJECT
supportmini[5,]
#### Earmark example: LetterID = 001669
### Jeff Sessions 2009
## "Support the Alabama Department of Transportation's Grant Application for the High Priority Grant Program GRANT10419742"     
## See CRS REport discussion of the HIgh Priority Grant Program as earmarkrich
## The only below the line earmark program in SAFETEA is the High Priority Project program
#(HPP). It is, however, by far the largest of the earmarked programs in the act. SAFETEA provided
#almost $15 billion under the HPP.
#HPP earmarks allow Members of Congress to define their project priorities through the
#authorization process to their State DOTs.4





resulttab<-table(dot_fhwa$support, dot_fhwa$year)
ggplot(data=supportmini,aes(year))+
  geom_point(aes(y= (..count..))) #+
  geom_point()


### Reading in Senate Earmark Data Totals 2008-2010
searmark<-read_excel("~/Dropbox/Correspondence/data/earmark/senateearmark20082010.xlsx")
names(searmark)<-c("name","e2008","e2009","e2010","e3yeartot")
searmark$name<-str_to_upper(searmark$name)  ### making the names uppercase consistent with naming convention in correspondence data

### creating a subset of the correspondence data for senate from 2008-2010
contacttinysenate<-subset(all_contacts,year>2007 & year<2011 & chamber=="Senate")

