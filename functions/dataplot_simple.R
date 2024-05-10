dataplot_simple<-
  function(temp, tempm, gsi){
    bin_width=2
    nbins <- seq(min - bin_width,
                 max + bin_width,
                 by = bin_width)
    p1 <- ggplot(data=temp, aes(x=Length.cm.))+geom_histogram(binwidth=2,breaks=nbins, color="black", aes(fill=Sex)) + 
      xlab("Fork Length (cm)") +  ylab("Frequency") + ggtitle("Size Distribution")+
      scale_fill_manual(values=c("red", "blue", "white"))+
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")+scale_x_continuous(limits = c(min-2, max+2))
    
    
    #monthly samples
    p2<-ggplot(tempm, aes(x=Month, y=tempm$N_month)) + 
      geom_bar(stat = "identity")+xlab("") +  ylab("Frequency") + ggtitle("Monthly Sample Distribution")+
      geom_hline(yintercept = 20, colour="red", linetype = "dashed")+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")
    
    #GSI
    p3<-ggplot(gsi, aes(x=Length.cm., y=GSI, color=Sex)) +  xlab("Fork Length (cm)") +  ylab("GSI") + ggtitle("GSI & Fish Length")+
      geom_point(aes(colour=Sex), size=2) +
      geom_point(shape = 1,size=2, colour = "black") + scale_color_manual(values=c("red", "blue", "white"))+
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")
    
    
    #Spawning Season
    gsi_f<-gsi %>% 
      subset(Sex=="F")
    #gsi_f$Month<-month(gsi_f$Date, label=TRUE)
    
    p4<-ggplot(gsi_f, aes(x=as.factor( Month), y=GSI)) + 
      geom_boxplot(fill="red") +
      xlab("") +  ylab("GSI") + ggtitle("Spawning Season")+
      theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")+scale_x_discrete(limits = month.abb)
    grid.arrange(p1,p2,p3,p4 , nrow = 2)
  }

