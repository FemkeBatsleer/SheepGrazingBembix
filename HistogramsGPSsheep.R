library(raster)
library(rgdal)
library(sf)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggrepel)

#load raster file
dens_wz <- raster(x="DensityGPS_WesthoekZuid.tif")
dens_ca <- raster(x="DensityGPS_Cabour.tif")
#load points
points_plots <- readOGR("study plots midpoints.shp")
polygon_buff <- readOGR("study plots polygon buffer 1m.shp")

#extract rastervalues for points
dens_points <- points_plots@data %>% tibble::add_column(raster::extract(dens_wz, points_plots)) %>%
  drop_na()
head(dens_points)
colnames(dens_points) <- c("id", "Naam", "value")

#extract rastervalues for polygones (buffered)
#Westhoek zuid
df_wz <- data.frame(Naam=character(),
                    Number=numeric(),
                    MeanValue=numeric())

for (i in 1:nrow(polygon_buff@data)){
  val <- raster::extract(dens_wz, polygon_buff[i,], weights=TRUE)
  if(!is.null(val[[1]])){
    val.df <- as.data.frame(val[[1]])
    mean_value <- val.df %>% mutate(valueweighted=value*weight) %>%
      summarise(sumvalues = sum(valueweighted))
    df_wz <- df_wz %>% add_row(Naam=as.character(polygon_buff@data[i,3]),
                               Number=i, MeanValue=mean_value$sumvalues)
  }
}

#Cabour
#Westhoek zuid
df_ca <- data.frame(Naam=character(),
                    Number=as.numeric(),
                    MeanValue=numeric())

for (i in 1:nrow(polygon_buff@data)){
  val <- raster::extract(dens_ca, polygon_buff[i,], weights=TRUE)
  if(!is.null(val[[1]])){
    val.df <- as.data.frame(val[[1]])
    mean_value <- val.df %>% mutate(valueweighted=value*weight) %>%
      summarise(sumvalues = sum(valueweighted))
    df_ca <- df_ca %>% add_row(Naam=as.character(polygon_buff@data[i,3]),
                               Number=i, MeanValue=mean_value$sumvalues)
  }
}

#plot map
plot(dens_wz)
plot(points_plots, add=T)
plot(dens_ca)
plot(points_plots, add=T)

#make histogram with values of plots added as lines
dens_wz_df <- as.data.frame(values(dens_wz))
colnames(dens_wz_df) <- c("values")
head(dens_wz_df)

#basic histogram, to extract breaks etc
hist_wz <- ggplot(data=dens_wz_df, aes(x=values)) +
  geom_histogram(binwidth=0.05, show.legend=FALSE)
g.data.wz <- ggplot_build(hist_wz)$data[[1]]
g.breaks.wz <- c(g.data.wz$xmin, tail(g.data.wz$xmax, n=1))
df_wz$y <- g.data.wz$count[findInterval(df_wz$MeanValue, g.breaks.wz[!is.na(g.breaks.wz)])]

#colours
bins <- 28
cols <- c("#F7FBFF00", "#DFECF8", "#C8DDF0", "#A3CCE3", "#73B3D8", "#4A97C9", "#2879B9", "#0D58A1", "#08306B")
colGradient <- colorRampPalette(cols)
cut.cols.wz <- colGradient(bins)
cut.cols.wz <- cut.cols.wz[1:22]
cuts.wz <- cut(dens_wz_df$values, breaks=g.breaks.wz)
names(cuts.wz) <- sapply(cuts.wz,function(t) cut.cols.wz[which(as.character(t) == levels(cuts.wz))])

plot(rep(1,10),col=colGradient(10),pch=19,cex=3)


hist_wz <- ggplot(data=dens_wz_df) +
  geom_histogram(aes(x=values, fill=cut(values,g.breaks.wz)), colour="#999999", binwidth=0.05, show.legend=FALSE) +
  
  xlab("Number of expected sheep positions/m²") + ylab("Frequency")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(-0.05, 1.45)
                     )+
  #scale_color_manual(values=cut.cols,labels=levels(cuts))+
  scale_fill_manual(values=cut.cols.wz,labels=levels(cuts.wz))+
  theme_minimal() + 
  theme(text=element_text(size=16))
hist_wz


hist_wz + geom_point(data=df_wz, aes(x=MeanValue, y=y)) +
  geom_text_repel(data=df_wz, aes(x=MeanValue, y=y, label=Naam),
                   nudge_x = 0.2, direction="x",
                  segment.size=0.5, ylim=c(120,200)
                  )

#CABOUR
#make histogram with values of plots added as lines
dens_ca_df <- as.data.frame(values(dens_ca))
colnames(dens_ca_df) <- c("values")
head(dens_ca_df)

#basic histogram, to extract breaks etc
hist_ca <- ggplot(data=dens_ca_df, aes(x=values)) +
  geom_histogram(binwidth=0.05, show.legend=FALSE)
g.data.ca <- ggplot_build(hist_ca)$data[[1]]
g.breaks.ca <- c(g.data.ca$xmin, tail(g.data.ca$xmax, n=1))
df_ca$y <- g.data.ca$count[findInterval(df_ca$MeanValue, g.breaks.ca[!is.na(g.breaks.ca)])]

#colours
bins <- 28
cols <- c("#F7FBFF00", "#DFECF8", "#C8DDF0", "#A3CCE3", "#73B3D8", "#4A97C9", "#2879B9", "#0D58A1", "#08306B")
colGradient <- colorRampPalette(cols)
cut.cols.ca <- colGradient(bins)
cut.cols.ca <- cut.cols.ca[1:28]
cuts.ca <- cut(dens_ca_df$values, breaks=g.breaks.ca)
names(cuts.ca) <- sapply(cuts.ca,function(t) cut.cols.ca[which(as.character(t) == levels(cuts.ca))])

plot(rep(1,10),col=colGradient(10),pch=19,cex=3)


hist_ca <- ggplot(data=dens_ca_df) +
  geom_histogram(aes(x=values, fill=cut(values,g.breaks.ca)), colour="#999999", binwidth=0.05, show.legend=FALSE) +
  
  xlab("Number of expected sheep positions/m²") + ylab("Frequency")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(-0.05, 1.45)
  )+
  #ylim(c(0,120))+
  #scale_color_manual(values=cut.cols,labels=levels(cuts))+
  scale_fill_manual(values=cut.cols.ca,labels=levels(cuts.ca))+
  theme_minimal() + 
  theme(text=element_text(size=16))
hist_ca


hist_ca + geom_point(data=df_ca, aes(x=MeanValue, y=y)) +
  geom_text_repel(data=df_ca, aes(x=MeanValue, y=y, label=Naam),
                  nudge_x = 0.2, direction="x",
                  segment.size=0.5, ylim=c(7000,20000)
  )

#subplot
hist_ca_sub <- ggplot(data=dens_ca_df) +
  geom_histogram(aes(x=values, fill=cut(values,g.breaks.ca)), colour="#999999", binwidth=0.05, show.legend=FALSE) +
  
  xlab("Number of expected sheep positions/m²") + ylab("Frequency")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10),limits=c(-0.05, 1.45)
  )+
  ylim(c(0,225))+
  #scale_color_manual(values=cut.cols,labels=levels(cuts))+
  scale_fill_manual(values=cut.cols.ca,labels=levels(cuts.ca))+
  theme_minimal() + 
  theme(text=element_text(size=16))
hist_ca_sub

