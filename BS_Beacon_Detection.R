# require(ggplot)
require(ggplot2)
require(lubridate)
require(dplyr)
require(RSQLite)
require(gridExtra)


Det_Data_from_ATLAS_server <- function(Query_Time_df,FullTag,BS=NA, SYS="Harod")
{
   if (SYS=="Harod") #connects to Harod server
   {
      dbc <- dbConnect(RMySQL::MySQL(),
                     user = 'roatlasharod',            # username
                     password = 'roatlasHarodOrr5678#',# password
                     host = '132.66.79.21',            # host ip address
                     port=5900,                        # port Number
                     dbname='harod')                   # name of data base
   }
   else
   {stop("system not defined")}
   Ntag <- length(FullTag)
   AllTagsDet <- list() #make an empty list for detections
   Q <- data.frame(query=rep("c",nrow(Query_Time_df)*length(FullTag)))
   for (j in 1:nrow(Query_Time_df))
   {
      for (i in 1:length(FullTag)) 
      { # build a  DETECTIONS query for the system, the results include the variables listed below
         if (is.na(BS)) {
            Q$query[(j-1)*Ntag+i] <- paste('select TAG,TIME,BS,RSSI,GAIN,SNR from DETECTIONS WHERE TAG=',FullTag[i],
                                           'AND TIME >=', Query_Time_df$Start[j]*1000, 'AND TIME <=', Query_Time_df$End[j]*1000)
         #    query = paste('select TAG,TIME,BS,RSSI,GAIN,SNR from DETECTIONS WHERE TAG=',FullTag[i],
         #                               'AND TIME >=', Query_Time_df$Start[j]*1000, 'AND TIME <=', Query_Time_df$End[j]*1000)
         } else
         Q$query[(j-1)*Ntag+i] <- paste('select TAG,TIME,BS,RSSI,GAIN,SNR from DETECTIONS WHERE TAG=',FullTag[i],
                                        'AND TIME >=', Query_Time_df$Start[j]*1000, 'AND TIME <=', Query_Time_df$End[j]*1000,'AND BS =', BS)
         # query = paste('select TAG,TIME,BS,RSSI,GAIN,SNR from DETECTIONS WHERE TAG=',FullTag[i],
         #               'AND TIME >=', Query_Time_df$Start[j]*1000, 'AND TIME <=', Query_Time_df$End[j]*1000, 'AND BS =', BS)
         # All_Data <- dbGetQuery(dbc,query)
         # AllTagsDet[[(j-1)*Ntag+i]] <- All_Data
      }
   }
   All_Data <- apply(X = Q,MARGIN = 1,FUN = dbGetQuery,conn = dbc)
   dbDisconnect(dbc)
   RawDet0 <- do.call(rbind.data.frame, All_Data)
   return(RawDet0)
}

Query_time_df <- function(Start_Time_Str,End_Time_Str,sample_timesPerDay=24,sample_lengthMinute=1)
      {
      num_Start_Time<-as.numeric(as.POSIXct(Start_Time_Str,
                                              "%Y-%m-%d %H:%M:%S", tz="UTC"))
      num_End_Time<-as.numeric(as.POSIXct(End_Time_Str,
                                            "%Y-%m-%d %H:%M:%S", tz="UTC"))
      times <- seq(from = num_Start_Time,to = num_End_Time,by = 60*60*24/sample_timesPerDay)
      Query_time=data.frame(Start=times,End  =times+(sample_lengthMinute*60))
      Query_time$posStart <- as.POSIXct(Query_time$Start, tz="UTC", origin="1970-01-01")
      Query_time$posEnd <- as.POSIXct(Query_time$End, tz="UTC", origin="1970-01-01")
      return(Query_time) 
      }
Get_ATLAS_Det_DATA <- function(Start_Time_Str,End_Time_Str,sample_timesPerDay,sample_lengthMinute,FullTag,BS=NA)
      {
      Query_Time <- Query_time_df(Start_Time_Str,End_Time_Str,sample_timesPerDay,sample_lengthMinute)
      RawDet0 <- Det_Data_from_ATLAS_server(Query_Time,FullTag,BS)
      RawDet0$BS<-gsub("9720060", '', RawDet0$BS)
      RawDet0$TAG<-gsub("972006000", '', RawDet0$TAG)
      
      RawDet0$dateTime<-as.POSIXct((RawDet0$TIME)/1000, tz="UTC", origin="1970-01-01")
      RawDet1 <- RawDet0 %>% mutate(M=minute(RawDet0$dateTime),H=hour(RawDet0$dateTime),D=date(RawDet0$dateTime)) %>% 
         group_by(TAG,BS) %>%
         mutate(Tag_BS=factor(paste(BS,"-",TAG ))) %>% 
         add_count()%>%
         ungroup() %>% 
         group_by(M,H,D,TAG,BS) %>% 
         slice_head() %>% 
         ungroup()
      return(RawDet1)
      }
plotCountDet <- function(RawDet1,xAxisRes='3 hours')
      {start_time <- RawDet1$dateTime[1]
RawDet1 %>% ggplot(aes(dateTime, factor(paste(BS,"-",TAG ))))+
   geom_point(aes(x=dateTime,y= factor(paste(BS,"-",TAG )),group=TAG, color=TAG) )+
   # geom_line(aes(x=dateTime,y=BS,group=TAG, color=TAG) ) +
   geom_text(data=RawDet1 %>% distinct(Tag_BS,.keep_all = TRUE) %>% dplyr::select(Tag_BS,n),
             aes(x=start_time, y=Tag_BS, label = n, hjust = +1.1))+
   scale_x_datetime(date_breaks = xAxisRes)+
   theme(axis.text.x=element_text(angle=60, hjust=1)) +
   labs(title= paste(' Beacons detection by Base station'),
        subtitle = paste('period= ',RawDet1$dateTime[1],' - ',max(RawDet1$dateTime)),
        x = "", 
        y = "Base Stations- Beacons")}
plotBSperformance <- function(RawDet1,BS2plot,TAG2plot=NA ,xAxisRes='12 hours')
      {
   if(is.na(TAG2plot))
   {TAG2plot <- unique(RawDet1$TAG)}
   
   p1 <- RawDet1 %>% filter(BS %in% BS2plot ) %>% filter(TAG %in% TAG2plot ) %>%
      ggplot(aes(dateTime, RSSI))+
      geom_point(aes(x=dateTime,y= RSSI,group=interaction(TAG,BS), color=interaction(TAG,BS),shape =BS) )+
      geom_line(aes(x=dateTime,y= RSSI,group=interaction(TAG,BS), color=interaction(TAG,BS)) ) +
      scale_x_datetime(date_breaks = xAxisRes)+
      theme(axis.text.x=element_blank(),
            legend.position=c(0.97, 0.5),
            legend.background = element_rect(color="black",fill ="grey",#transparent",
                                             size=0.5, linetype="solid")) +
      labs(color="TAG.BS",
           # title= paste(' RSSI'),
           title = paste('period= ',RawDet1$dateTime[1],' - ',max(RawDet1$dateTime)),
           x = "", 
           y = "RSSI")
   p2 <- RawDet1 %>% filter(BS %in% BS2plot ) %>% filter(TAG %in% TAG2plot ) %>%
      ggplot(aes(dateTime, SNR))+
      geom_point(aes(x=dateTime,y= SNR,group=interaction(TAG,BS), color=interaction(TAG,BS),shape =BS) )+
      geom_line(aes(x=dateTime,y= SNR,group=interaction(TAG,BS), color=interaction(TAG,BS)) ) +
      scale_x_datetime(date_breaks = xAxisRes)+
      theme(axis.text.x=element_blank(),
            legend.position='none') +
      labs(
           # title= paste(' SNR'),
           # subtitle = paste('period= ',RawDet1$dateTime[1],' - ',max(RawDet1$dateTime)),
           x = "", 
           y = "SNR")
   p3 <- RawDet1 %>% filter(BS %in% BS2plot )  %>%
      ggplot(aes(dateTime, GAIN))+
      geom_point(aes(x=dateTime,y= GAIN,group=BS, color=BS) )+
      geom_line(aes(x=dateTime,y= GAIN,group=BS, color=BS) ) +
      scale_x_datetime(date_breaks = xAxisRes)+
      theme(axis.text.x=element_text(angle=60, hjust=1),
            legend.position=c(0.97, 0.4),
            legend.background = element_rect(color="black",fill ="grey",
                                             size=0.5, linetype="solid")) +
      labs(
           # title= paste(' GAIN'),
           # subtitle = paste('period= ',RawDet1$dateTime[1],' - ',max(RawDet1$dateTime)),
           x = "", 
           y = "GAIN")
   grid.arrange(p1,p2,p3, nrow = 3)
   
}

# Start_Time_Str     <- c('2021-05-17 00:00:00')
# End_Time_Str       <- c('2021-05-19 00:01:00')
# sample_timesPerDay <- 24
# sample_lengthMinute<- 1
# FullTag <- c(972006000003,972006000004,972006000006)
# RawDet1 <- Get_ATLAS_Det_DATA (Start_Time_Str,End_Time_Str,sample_timesPerDay,sample_lengthMinute,FullTag )
# plotCountDet(RawDet1,xAxisRes='1 hours')
# plotBSperformance(RawDet1,BS2plot=c("04", "07") ,xAxisRes='1 hours')
 
