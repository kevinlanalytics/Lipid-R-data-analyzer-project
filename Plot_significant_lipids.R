library(stringr)
library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(tm)
library(ggpubr)
library(ggrepel)
library(RColorBrewer)
library(ggthemes)
library(scales)
library(reshape2)
library(gridExtra)
library(grid)
library(tibble)
# library(rJava)
# library(xlsx)
# library(xlsxjars)
#library(ggdendro)
library(cluster)
#library(factoextra)
#library(magrittr)
#library(corrplot)
#library(viridis)
library(pheatmap)
#library(RColorBrewer)

# install.packages("dendsort")
library(dendsort)


#used after lipidanalyst_upto_mol
#Used to plot significant lipids per class seen in the heatmap
#reads the file
read.plot.sig.treat<- read.csv("/Users/sonicboy66/Documents/Research/r/List_of_signficant_lipids_per_class.csv")
#spltis the file per class
split.sig.summed.melt<- split(read.plot.sig.treat,read.plot.sig.treat$class)
#Gives the user option to input class 
print ("Select lipid class to plot significant lipids")
#Provides the class required
names(split.sig.summed.melt)
#Command args input taken from drop down

args= (commandArgs(TRUE))

plotClass= as.character(args[1]);

#prepares subset data from user input
        df<- split.sig.summed.melt[[plotClass]]
                height<-as.integer(length(unique(df$variable))/3)
                your_font_size <- 3
                
                fileName_sigLipids= paste0("public/",plotClass,"_sigLipids.png")
        #plots the data
                png(filename=fileName_sigLipids, width = 500, height = 150*height, res = 72)
        
        p<- ggboxplot(df, x = "Group", 
                      y = "value", color = "Group",
                      ggtheme = theme_minimal(),
                      palette = "jco", fill= "Group", 
                      order = c("PBS","CAR","EPI","JE2","MN8","COL","NEW"))+ 
                stat_compare_means(aes(group= Group), 
                                   label= "p.format", label.y.npc = c("top"), 
                                   label.x.npc = c("left"), size= your_font_size)+
                theme(plot.title = element_text(hjust=0.5,
                                                face = "bold"),
                      strip.text.x = element_text(colour = "black", 
                                                  face = "bold",size= 10), 
                      strip.text = element_text(face="bold"), 
                      axis.text.x=element_text(face="bold",
                                               angle = 90, size= 10),
                      axis.text.y=element_text(face="bold", size = 10),
                      axis.text = element_text(size = 6), 
                      aspect.ratio = 0.5) +
                labs(x="Treatment", y="Mol%",size= 6)+
                # stat_compare_means(comparisons = my_comparisons,
                #                                  size= your_font_size )+
                border()+ 
                coord_fixed()+ 
                rremove("legend")
        # p$layers[[2]]$aes_params$textsize <- 2
        p+ facet_wrap(~variable, scales = "free", ncol=3)
        # facet(p, facet.by = "variable", ncol=3, scales = "free")
        dev.off()


