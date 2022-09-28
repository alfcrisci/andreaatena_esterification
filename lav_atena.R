library(openxlsx)
library(ggplot2)
library(ggstatsplot)
library(reshape2)
library(reshape)

setwd("C://aaa_lavori//lav_atena")

###############################################################

data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

###############################################################
# read data 

dati_ester=read.xlsx("Ester PCM_6_MN_atena.xlsx",1)
dati_ester_contr=dati_ester[which(dati_ester$treat=="contr"),]
dati_ester_treat=dati_ester[which(dati_ester$treat=="ester"),]

################################################################
# t student


res_t_student=list()
j=1

for ( i in 4:10) {
  res_t_student[[j]]=t.test(dati_ester_contr[,i],dati_ester_treat[,i])
  
  j=j+1
  
}

names(res_t_student)=names(dati_ester)[4:10]
################################################################

res_t_student

################################################################

dati_ester_p=dati_ester[,1:10] # eliminate note column
dati_ester_p=melt(dati_ester_p) # ggplot data preparation molten format 

names_unit=c("kg/m3","kg/m3","N/mm2","MPa","MPa","MPa","N/mm2")
names_vars=c("Massa volumica anidra",
             "Massa volumica condizionato",
             "Resistenza a compressione",
             "Resistenza a taglio",
             "Modulo elastico a flessione",
             "Resistenza a flessione",
             "Durezza Brinell")

###########################################################
# grouped barplot 

dati_ester_p_SP=split(dati_ester_p,dati_ester_p$variable)

for ( jj in 1:length(dati_ester_p_SP)) {
#####################################################################################
# factor elimination

dati_ester_p_SP[[jj]]$variable=as.character(dati_ester_p_SP[[jj]]$variable)

#####################################################################################
# plot

temp_summaries=data_summary(dati_ester_p_SP[[jj]],"value","treat")

ggplot(temp_summaries, aes(x = treat, y = value, fill = treat, colour = treat)) + 
  geom_bar(stat = "identity", position = "dodge", alpha = 0.5)  +
  geom_errorbar(aes(ymin=value-sd, ymax=value+sd), position = position_dodge(0.9), width = 0.25)+
  ylab(names_unit[jj]) +
  xlab(names_vars[jj])

ggsave(paste0("barplot_",names(dati_ester_p_SP)[jj],".png"))

ggplot(dati_ester_p_SP[[1]], aes(fill=treat, y=value, x=variable)) + 
  geom_boxplot()+
  stat_summary(fun=mean, geom="point", shape=23, size=4,position = position_dodge(0.75))+
  ylab(names_unit[jj]) +
  xlab(names_vars[jj])

ggsave(paste0("boxplot_",names(dati_ester_p_SP)[jj],".png"))

ggbetweenstats(
  data  = dati_ester_p_SP[[jj]],
  x     = treat,
  y     = value,
  ylab=names_unit[jj],
  title = names_vars[jj],
  bf.message = FALSE
)

ggsave(paste0("boxviolin_",names(dati_ester_p_SP)[jj],".png"))

}

# References

# https://vsni.co.uk/blogs/t-test_student_or_welch


# https://www.geeksforgeeks.org/melting-and-casting-in-r-programming/?ref=lbp

# http://sthda.com/english/wiki/ggplot2-error-bars-quick-start-guide-r-software-and-data-visualization

