#### plotting 

library(tidyr)
library(tidyverse)
library(here)
library(plyr)
library(cowplot)
library(grid)
library(jpeg)
library(RCurl)
library(gridExtra)
library(xtable)

clust_shots <- readRDS(here::here("saved_robjs/shots_with_clusters_1920"))



### Curry, DeRozan, KAT plot
vol_shots <- clust_shots %>% 
  filter(PLAYER_NAME %in% 
           c("Stephen Curry", "Karl-Anthony Towns", "DeMar DeRozan"))



### shot density plots
img <- readJPEG(here::here('nba_court.jpg'))
court <- rasterGrob(img, interpolate = FALSE)


comp_zone_pct <- function(all_shots, raw_shots){
  atts <- all_shots %>% 
    dplyr::select(which(str_detect(colnames(all_shots), "^(?=.*attempt).*$")==TRUE)) %>% 
    dplyr::select(-total_attempts)
  
  
  pcts <- all_shots %>% 
    dplyr::select(which(str_detect(colnames(all_shots), "^(?=.*pct).*$")==TRUE))
  
  cart_zone_ctrs <- raw_shots %>% 
    dplyr::filter(SHOT_ZONE_BASIC!="Backcourt") %>% 
    group_by(zone) %>% 
    dplyr::summarize(mean_x = mean(LOC_X), 
                     mean_y = mean(LOC_Y))
  
  return(list(pcts=colSums(atts*pcts)/colSums(atts), ctr=cart_zone_ctrs))
}


rezoned_clusts <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster, Pos)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC != "Backcourt")


gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

region_labs <- c("AB3 Ctr", "AB3 L", "AB3 R", "Paint", "LC3", "MR Ctr", 
                 "MR LCtr", "MR L", "MR RCtr", "MR R", "RA", "RC3")
region_full <- c("Above Break 3 Ctr",
                 "Above Break 3 Left",
                 "Above Break 3 Right",
                 "Left Corner 3",
                 "Mid-Range Ctr",
                 "Mid-Range Left Ctr",
                 "Mid-Range Left",
                 "Mid-Range Right Ctr",
                 "Restricted Area (RA)",
                 "Paint (non-RA)",
                 "Mid-Range Right",
                 "Right Corner 3")


### UPDATE THIS
clust_inds <- c(2, 3, 6, 7, 8, 9, 11, 12, 13)
dens_color <- "lightsteelblue2"
tsize <- 3
gpsize <- 1
gpshape <- 16

spaceLegend <- 0.75

cl <- clust_inds[1]
test1 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                       rezoned_clusts %>% dplyr::filter(cluster==cl))

dens_data1 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)

data1 <- data.frame(test1$ctr[2:13,])
data1$zone <- region_full

labs1 <- paste0(round(test1$pcts, digits=4)*100,'%')

p0 <- ggplot(data=data1, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.5, size = tsize, show.legend=F) +
  ggtitle(paste("Zones")) +
  #guides(alpha = FALSE, size = FALSE, 
  #       color = guide_legend(override.aes = list(size = gpsize), 
  #                            shape=as.character(1:12))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone), show.legend=T) + 
  scale_color_manual(labels = region_full, name="Zone", values=gg_color_hue(12)) +
  #scale_x_continuous(breaks=c(x1,x2,x3), labels=as.character()) + 
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1))


p1 <- ggplot(data=data1, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.5, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) + 
  scale_color_manual(labels = labs1, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data1,aes(x=-LOC_X, y=LOC_Y),color=dens_color, show.legend = F)


cl <- clust_inds[2]

test2 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                       rezoned_clusts %>% dplyr::filter(cluster==cl))

data2 <- data.frame(test2$ctr[2:13,])
data2$zone <- region_full

dens_data2 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)



labs2 <- paste0(round(test2$pcts, digits=4)*100,'%')

p2 <- ggplot(data=data2, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.5, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) +
  scale_color_manual(labels = labs2, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data2,aes(x=-LOC_X, y=LOC_Y),color=dens_color)


cl <- clust_inds[3]
test3 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                       rezoned_clusts %>% dplyr::filter(cluster==cl))

data3 <- data.frame(test3$ctr[2:13,])
data3$zone <- region_full

labs3 <- paste0(round(test3$pcts, digits=4)*100,'%')

dens_data3 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)

p3 <- ggplot(data=data3, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.5, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) +
  scale_color_manual(labels = labs3, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data3,aes(x=-LOC_X, y=LOC_Y),color=dens_color)

cl <- clust_inds[4]
test4 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                       rezoned_clusts %>% dplyr::filter(cluster==cl))

data4 <- data.frame(test4$ctr[2:13,])
data4$zone <- region_full

labs4 <- paste0(round(test4$pcts, digits=4)*100,'%')



dens_data4 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)

p4 <- ggplot(data=data4, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.5, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) +
  scale_color_manual(labels = labs4, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data4,aes(x=-LOC_X, y=LOC_Y),color=dens_color)

cl <- clust_inds[5]
test5 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                       rezoned_clusts %>% dplyr::filter(cluster==cl))

data5 <- data.frame(test5$ctr[2:13,])
data5$zone <- region_full
labs5 <- paste0(round(test5$pcts, digits=4)*100,'%')


dens_data5 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)

p5 <- ggplot(data=data5, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  # geom_text(aes(colour = zone, label = region_labs), vjust = 1.2, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) +
  scale_color_manual(labels = labs5, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data5,aes(x=-LOC_X, y=LOC_Y),color=dens_color)

cl <- clust_inds[6]
test6 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                       rezoned_clusts %>% dplyr::filter(cluster==cl))

data6 <- data.frame(test6$ctr[2:13,])
data6$zone <- region_full

labs6 <- paste0(round(test6$pcts, digits=4)*100,'%')


dens_data6 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)

p6 <- ggplot(data=data6, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.2, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) +
  scale_color_manual(labels = labs6, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data6,aes(x=-LOC_X, y=LOC_Y),color=dens_color)

cl <- clust_inds[7]
test7 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                       rezoned_clusts %>% dplyr::filter(cluster==cl))

data7 <- data.frame(test7$ctr[2:13,])
data7$zone <- region_full

labs7 <- paste0(round(test7$pcts, digits=4)*100,'%')

dens_data7 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)


p7 <- ggplot(data=data7, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.2, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) +
  scale_color_manual(labels = labs7, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data7,aes(x=-LOC_X, y=LOC_Y),color=dens_color)

cl <- clust_inds[8]
test8 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                       rezoned_clusts %>% dplyr::filter(cluster==cl))

data8 <- data.frame(test8$ctr[2:13,])
data8$zone <- region_full

labs8 <- paste0(round(test8$pcts, digits=4)*100,'%')


dens_data8 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)

p8 <- ggplot(data=data8, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.2, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) +
  scale_color_manual(labels = labs8, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data8,aes(x=-LOC_X, y=LOC_Y),color=dens_color)

cl <- clust_inds[9]
test9 <- comp_zone_pct(clust_shots %>% dplyr::filter(cluster==cl),
                       rezoned_clusts %>% dplyr::filter(cluster==cl))

data9 <- data.frame(test9$ctr[2:13,])
data9$zone <- region_full

labs9 <- paste0(round(test9$pcts, digits=4)*100,'%')



dens_data9 <- clust_shots %>% 
  arrange(cluster) %>% 
  dplyr::select(c(PLAYER_NAME, cluster)) %>% 
  left_join(rezoned_shots) %>% 
  dplyr::filter(SHOT_ZONE_BASIC!='Backcourt') %>% 
  dplyr::filter(cluster==cl)

p9 <- ggplot(data=data9, aes(x=-mean_x, y=mean_y), color=zone) +
  annotation_custom(court, -250, 250, -52, 418) +
  #geom_text(aes(colour = zone, label = region_labs), vjust = 1.2, size = tsize, show.legend=F) +
  ggtitle(paste("Cluster", cl)) +
  guides(alpha = FALSE, size = FALSE, 
         shape = guide_legend(override.aes = list(size = gpsize)),
         color = guide_legend(override.aes = list(size = gpsize))) +
  xlim(250, -250) +
  ylim(-52, 418) +
  coord_fixed() +
  geom_point(aes(color=zone, size=gpsize), show.legend=T, shape=gpshape) +
  scale_color_manual(labels = labs9, values=gg_color_hue(12), name="Zone FG%") +
  theme(line = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        legend.key.size = unit(spaceLegend, "lines"),
        plot.title = element_text(size = 12, lineheight = 1)) +
  geom_density_2d(data=dens_data9,aes(x=-LOC_X, y=LOC_Y), color=dens_color)

