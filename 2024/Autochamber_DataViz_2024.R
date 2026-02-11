
library(dplyr)
library(ggplot2)
#Look at tempest autochamber data with 7810s attached 

#read in data & add a plot column to each 
con1 <- read.csv("SFP Exported Data/CONTROL_export_20240603.csv", header=T, skip=1)
con2 <- read.csv("SFP Data Exports/CONTROL_export_20240609.csv", header=T, skip=1)


con$Plot <- "Control"
fw1 <- read.csv("FRESH_export_20240603.csv", header=T, skip=1)
fw$Plot <- "Fresh"
sw <- read.csv("SALT_export_20240603.csv", header=T, skip=1)
sw$Plot <- "Salt"



#combine the data into one data frame 
dat <- rbind(con, fw, sw)
dat=dat[-c(1,2),]
head(dat)

dat$datetime = parsedate::parse_date(dat$DATE.initial_value)
dat$FCO2_DRY.LIN <- as.numeric(dat$FCO2_DRY.LIN )
dat$FCO2.LIN <- as.numeric(dat$FCO2.LIN )
dat$FCH4.LIN <- as.numeric(dat$FCH4.LIN )

#let's just plot the CO2 and CH4 fluxes from the 7810 and the 8100 
co2a <- ggplot()+
  geom_boxplot(data=dat, aes(x=Plot, y=FCO2_DRY.LIN, fill=Plot)) + 
  theme_classic() + 
  labs(title= "8100 CO2 Flux", x=" ", y= expression(paste("CO2 Flux (umol m-2 s-1)"))) +
  scale_fill_manual(values=c("mediumseagreen", "cornflowerblue", "indianred3")) +
  theme(panel.background = element_rect(colour = "black", linewidth  =1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12), 
        strip.text.x = element_text(size = 12)) 
co2a


#7810
co2b <- ggplot()+
  geom_boxplot(data=dat, aes(x=Plot, y=FCO2.LIN, fill=Plot)) + 
  theme_classic() + 
  labs(title= "7810 CO2 Flux", x=" ", y= expression(paste("CO2 Flux (umol m-2 s-1)"))) +
  scale_fill_manual(values=c("mediumseagreen", "cornflowerblue", "indianred3")) +
  ylim(-30,30) +
  theme(panel.background = element_rect(colour = "black", linewidth  =1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12), 
        strip.text.x = element_text(size = 12)) 
co2b

ch4b <- ggplot()+
  geom_boxplot(data=dat, aes(x=Plot, y=FCH4.LIN, fill=Plot)) + 
  theme_classic() + 
  labs(title= "7810 CH4 Flux", x=" ", y= expression(paste("CH4 Flux (nmol m-2 s-1)"))) +
  scale_fill_manual(values=c("mediumseagreen", "cornflowerblue", "indianred3")) +
  ylim(-20,20)+
  theme(panel.background = element_rect(colour = "black", linewidth  =1.2), 
        legend.position = "NONE", 
        legend.title= element_blank(), 
        axis.text=element_text(size=12), 
        axis.title=element_text(size=12), 
        strip.text.x = element_text(size = 12)) 
ch4b

dat$FCO2.LIN_R2 <- as.numeric(dat$FCO2.LIN_R2 )
dat_mod <- filter(dat, FCO2.LIN_R2 > -100)
co2br <- ggplot(dat_mod, aes(datetime, FCO2.LIN, color = FCO2.LIN_R2)) + 
  facet_wrap(~Plot, ncol = 3) + 
  geom_point(size=2) + theme_classic()+
  ylim(-30,30)+
  ylab("CO2 flux (µmol/m2/s)")
co2br


dat_mod$FCH4.LIN_R2 <- as.numeric(dat_mod$FCH4.LIN_R2 )
dat_mod <- filter(dat_mod, FCH4.LIN_R2 > -100)
ch4br <- ggplot(dat_mod, aes(datetime, FCH4.LIN, color = FCH4.LIN_R2)) + 
  facet_wrap(~Plot, ncol = 3) + 
  labs(title= "7810 CH4 Flux", x=" ", y= expression(paste("CH4 Flux (nmol m-2 s-1)"))) +
  geom_point(size=2) + theme_classic()+
  ylim(-30,30)+
ch4br

dat_mod <- dat_mod[!(dat_mod$PORT_LABEL == "SALT"), ]

#make color = port label 
co2br <- ggplot(dat_mod, aes(datetime, FCO2.LIN, color = PORT_LABEL)) + 
  facet_wrap(~Plot, ncol = 3) + 
  geom_point(size=2) + theme_classic()+
  geom_hline(yintercept = 0) + 
  labs(title= "7810 Auto Chamber Data", x=" ") +
  ylab(expression(paste( CO [2], " Flux (μmol m"^-2* " s"^-1*")"))) +
  ylim(-5,20)+
  theme(legend.title = element_blank()) + 
  theme(axis.title.x = element_text(size=12), axis.text = element_text(size=12),
        axis.title.y = element_text(size=12), legend.text=element_text(size=12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth =1), 
        strip.text = element_text( size = 12))
co2br

ch4br <- ggplot(dat_mod, aes(datetime, FCH4.LIN, color = PORT_LABEL)) + 
  facet_wrap(~Plot, ncol = 3) + 
  geom_point(size=2) + theme_classic()+
  ylim(-20,10)+
  labs(title= "7810 Auto Chamber Data", x=" ") +
  ylab(expression(paste( CH [4], " Flux (nmol m"^-2* " s"^-1*")"))) +
  theme(legend.title = element_blank()) + 
  theme(axis.title.x = element_text(size=12), axis.text = element_text(size=12),
        axis.title.y = element_text(size=12), legend.text=element_text(size=12),
        panel.border = element_rect(colour = "black", fill=NA, linewidth =1), 
        strip.text = element_text( size = 12))
ch4br


#head(dat_mod)


