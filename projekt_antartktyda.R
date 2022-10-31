library(maps)
library(mapproj)
library(gganimate)
#zaladowanie mapy antarktydy
library(matrireal_lattats)
library(tmap)
library(pastecs)
library(ggplot2)
library(maptools)
library(matrixStats)

#library(SOmap)


#Wczytanie plików z środowiskiem do pracy

Krance_lodu <- read.csv("data/daily_ice_edge.csv")


 #0
 #------------------------------------------------------------------------------



rel_granica_lodu<-colMins(as.matrix(Krance_lodu[1,2:362]))
long<-c(-180:180)
rel_granica_lodu_df<-data.frame(longitude=long,lat=rel_granica_lodu)

 Antarktyda_pkt<-ggplot(rel_granica_lodu_df, aes(x=longitude,y=lat),colour="blue")+
   geom_line()+geom_path()+coord_polar()+ylim(-90,-50)+ggtitle("Zasieg rzeczywisty")+
   ylab("Szerokosc_geograficzna")+ 
   xlab("Dlugosc_geograficzna")
 
 Antarktyda_pkt
 
 
 
 #1
 #------------------------------------------------------------------------------
 library(MatrixModels)
 
 real_long<-cos(2*pi*rel_granica_lodu_df$longitude/180)
 real_lat<-sin(2*pi*rel_granica_lodu_df$lat/90)
 
 obl<-lm(rel_granica_lodu_df$lat~real_long+real_lat)

 pred<-predict(obl, newdata = data.frame(rel_granica_lodu_df$longitude))
 rel_granica_lodu_df$pred<-pred
 

 Antarktyda_mat<-ggplot(rel_granica_lodu_df, aes(x=long,y=rel_granica_lodu_df$pred),col="red")+
   geom_line()+
   geom_path()+coord_polar()+ylim(-90,-50)+
   ggtitle("Zasieg matematyczny")+
   ylab("Szerokosc_geograficzna")+ 
   xlab("Dlugosc_geograficzna")
 
 Antarktyda_mat
 
 
 #2
 #------------------------------------------------------------------------------
library(animation)
 
 oopts = if (.Platform$OS.type == "windows") {
   ani.options(ffmpeg = "C:/Users/szymi/Downloads/ffmpeg-5.0.1-full_build/bin/ffmpeg.exe")
 }

i<-1

saveVideo(
        {
          
          while (i<=9288)
          {
            
            rel_granica_lodu<-colMins(as.matrix(Krance_lodu[i,2:362]))
            long<-c(-180:180)
            rel_granica_lodu_df<-data.frame(longitude=long,lat=rel_granica_lodu)
          
            
            
            
            library(MatrixModels)
            
            
            real_long <- cos(2*pi*rel_granica_lodu_df$longitude/180)
            real_lat <- sin(2*pi*rel_granica_lodu_df$lat/90)
            obl <- lm(rel_granica_lodu_df$lat~real_long+real_lat)
           
            pred <- predict(obl, newdata = data.frame(rel_granica_lodu_df$longitude))
            rel_granica_lodu_df$pred<-pred
         
          
            
            Antarktyda<-ggplot()+
              ylab("Szerokosc_geograficzna")+
              xlab("Dlugosc_geograficzna")+
              ggtitle("Zasieg lodu na Antarktydzie", Krance_lodu[i,1])+
              geom_line(data=rel_granica_lodu_df,aes(x=rel_granica_lodu_df$longitude,y=rel_granica_lodu_df$lat, col="Zasieg rzeczywisty"))+
              geom_line(data=rel_granica_lodu_df,aes(x=rel_granica_lodu_df$longitude,y=rel_granica_lodu_df$pred, col="Zasieg matematyczny"))+
              geom_path()+
              coord_polar()+
              ylim(-90,-50)
            
            plot(Antarktyda)
            i=i+1
          }
          
        },interval=0.1,video.name = "zasieg_antartktyda.mp4"
)

#3
#------------------------------------------------------------------------------

par(mfrow=c(2,2))

image(im,yaxt="n",xaxt="n",xlab="dni",ylab="azymuty")
axis(1, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*366,0))
axis(2, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*360,0))



image(l,yaxt="n",xaxt="n",xlab="dni",ylab="azymuty")
axis(1, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*366,0))
axis(2, at=seq(0,1,by=0.1),labels=round(seq(0,1,by=0.1)*360,0))


#animacja okres roczny

k=1
oopts = if (.Platform$OS.type == "windows") {
  ani.options(ffmpeg = "C:/Users/szymi/Downloads/ffmpeg-5.0.1-full_build/bin/ffmpeg.exe")
}

saveVideo (interval=0.1, video.name="calosc.mp4",{
  
  im<-as.matrix(Krance_lodu[1:183,2:361])
  #transorfacja Laplace'a
  lnew<-im
  l<-im
  
  for (i in 1:183) {
    for (i in 2:(183-1)) 
      for (j in 2:(360-1)) {
        lnew[i,j]<-0.25*(l[i-1,j]+l[i+1,j]+l[i,j-1]+l[i,j+1])
      }
    l<-lnew
  }
  
  while (k<=183)
  { 
    
    rel_granica_lodu<-colMins(as.matrix(Krance_lodu[k,2:362]))
    long<-c(-180:180)
    rel_granica_lodu_df<-data.frame(longitude=long,lat=rel_granica_lodu)
    
    
    library(MatrixModels)
    real_long<-cos(2*pi*rel_granica_lodu_df$longitude/180)
    real_lat<-sin(2*pi*rel_granica_lodu_df$lat/90)
    obl<-lm(rel_granica_lodu_df$lat~real_long+real_lat)
    o<-fitted(obl)
    pred<-predict(obl, newdata = data.frame(rel_granica_lodu_df$longitude))
    rel_granica_lodu_df$pred<-pred
    
    rel_granica_lodu2<-as.matrix(l[k,1:360 ])
    
    long2<-c(-179:180)
    rel_granica_lodu2_df<-data.frame(longitude=long2,lat=rel_granica_lodu2)
    
    
    Antarktyda<-ggplot()+
      ylab("Szerokosc_geograficzna")+
      xlab("Dlugosc_geograficzna")+
      
      ggtitle("Zasieg lodu na Antarktydzie", Krance_lodu[k,1])+
      geom_line(data=rel_granica_lodu2_df,aes(x=longitude,y=lat,col="Po transformacji"))+
      geom_line(data=rel_granica_lodu_df,aes(x=rel_granica_lodu_df$longitude,y=rel_granica_lodu_df$lat, col="Zasieg rzeczywisty"))+
      geom_line(data=rel_granica_lodu_df,aes(x=rel_granica_lodu_df$longitude,y=rel_granica_lodu_df$pred, col="Zasieg matematyczny"))+
      
      geom_path()+
      coord_polar()+
      ylim(-90,-50)
    plot(Antarktyda)
    
    
    k=k+1
  }
})



