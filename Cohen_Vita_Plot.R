p <- ggplot(years.prop.actas,aes(x=year,y=weight)) +
        # blue plot
        geom_point(aes(colour="Constituciones del Ecuador")) + 
        geom_smooth(data=years.prop.actas,method="loess", fill="grey60", color="blue",
                    se=T) + scale_y_continuous(labels=waiver()) +
        labs(x="date", y="proportion of topic weight",
             title= "Topic trend over time") +
        
        geom_point(data = years.prop.actas,aes(colour="Management Plans")) + 
        geom_smooth(data=years.prop.actas,method="loess", fill="grey60", color="red",
                    se=F)



p

###This is a plot with al the four data sets
years.prop.sci$corpus <- rep("Scientific Articles")
years.prop.grey$corpus <- rep("Management Plans")
years.prop.notgal.10$corpus <- rep("CDF")
years.prop.notgal.22$corpus <- rep("CDF")
years.prop.notgal.30$corpus <- rep("CDF")

#years.prop.galreport$corpus <- rep("Galapagos Report")
years.prop.sci$document <- NULL

years.prop.sci$type <- rep("all")
years.prop.grey$type <- rep("all")
years.prop.notgal.10$type <- rep("vertebrates")
years.prop.notgal.22$type <- rep("invertebrates")
years.prop.notgal.30$type <- rep("plants")

cohen.plot.df <- rbind(years.prop.sci[years.prop.sci$year>=1960,],years.prop.grey,years.prop.notgal.10, years.prop.notgal.22,years.prop.notgal.30)#,years.prop.galreport)

p <- ggplot(cohen.plot.df)+geom_point(aes(x=year, y=weight, colour=corpus,shape=corpus))+
        scale_colour_manual(name="Legend",values=c("black","red","dark green","blue"))+
        scale_shape_manual(name="Legend",values=c(1,2,3,4))+
        geom_smooth(data=cohen.plot.df,aes(x=year, y=weight, colour=corpus),method="loess", fill="grey60",
                    se=F) + scale_y_continuous(labels=waiver()) +
        labs(x="date", y="proportion of topic weight",
             title= "Topic trend over time") +
        
        
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+theme(legend.position = "right")



p

## Plot without smoothing
library(scales)
cohen.plot.df$year <- as.Date(as.character(cohen.plot.df$year),"%Y")

cohen.plot.df$mean.y <- as.Date(cut(cohen.plot.df$year,
                         breaks = "5 years"))

total.cdf.plants <-cohen.plot.df%>%group_by(corpus,mean.y,type)%>% 
        summarise(weight = mean(weight))



p <- ggplot(total.cdf.plants,aes(x=mean.y, y=weight,group=corpus,color=corpus,linetype=corpus),size=2)+
        stat_summary(fun.y=sum,geom="line")+
        scale_colour_manual(name="Legend",values=c("black","dark green","dark blue"))+
        scale_linetype_manual(name="Legend",values=c(1,8,12))+
        labs(x="date", y="proportion of topic weight",
        title= "Topic trend over time")+  #ylim(0,0.3) +
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+theme(legend.position = "right")

p
        
p+ facet_grid(corpus~.)


### correlation plots

cdf.sum <- total.cdf.plants[total.cdf.plants$mean.y>="1965-01-01"&total.cdf.plants$corpus=="CDF",]
cdf.sum <-total.cdf.plants%>%group_by(corpus,mean.y)%>% 
        summarise(weight = sum(weight))

## correlation science articles vs cdf
#x1=cdf.sum[cdf.sum$corpus=="CDF"&cdf.sum$mean.y>"1975-01-01",]
#x2=cdf.sum[cdf.sum$corpus=="Scientific Articles"&cdf.sum$mean.y>"1965-01-01"&cdf.sum$mean.y<"2005-01-01",]

x1<- cdf.sum[cdf.sum$corpus=="CDF",]
x2<- cdf.sum[cdf.sum$corpus=="Scientific Articles",]
x<-x1$weight
y<-x2$weight
df<-as.data.frame(cbind(x,y))

### correaltion science vs management
x1<- total.cdf.plants[total.cdf.plants$corpus=="Management Plans",]
x2<- cdf.sum[cdf.sum$corpus=="Scientific Articles"&cdf.sum$mean.y!="1965-01-01"&cdf.sum$mean.y!="1980-01-01"&cdf.sum$mean.y!="1990-01-01"&cdf.sum$mean.y!="2000-01-01",] #&total.cdf.plants$mean.y>"1970-01-01"&total.cdf.plants$mean.y<"2010-01-01",]

x<-x1$weight
y<-x2$weight
df<-as.data.frame(cbind(x,y))


### correlation cdf vs managemetn plans

x2<- cdf.sum[cdf.sum$corpus=="CDF"&cdf.sum$mean.y!="1960-01-01"&cdf.sum$mean.y!="1965-01-01"&cdf.sum$mean.y!="1975-01-01"&cdf.sum$mean.y!="1990-01-01"&cdf.sum$mean.y!="2000-01-01",]
x1<- cdf.sum[cdf.sum$corpus=="Management Plans"&cdf.sum$mean.y!="2015-01-01",]

x<-x1$weight
y<-x2$weight
df<-as.data.frame(cbind(x,y))



lm_eqn <- function(df){
        m <- lm(y ~ x,df);
        eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                         list(a = format(coef(m)[1], digits = 2), 
                              b = format(coef(m)[2], digits = 2), 
                              r2 = format(summary(m)$r.squared, digits = 3)))
        as.character(as.expression(eq));                 
}


corr_eqn <- function(x,y, digits = 2) {
        corr_coef <- round(cor(x, y), digits = digits)
        paste("italic(r) == ", corr_coef)
}

p <- ggplot(df,aes(x=y,y=x))+geom_point()+geom_smooth(method = "lm", color="red",formula = (y~x))

p1 <- p + geom_text(x=0.03,y=0.07,label = lm_eqn(df), parse = TRUE)+xlab("Science") +
        ylab("CDF")

p1

