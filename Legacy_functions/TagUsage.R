# require(ggplot)
require(ggplot2)
require(lubridate)
require(dplyr)
require(RSQLite)
require(gridExtra)
require(tidyr)
# require(ggpubr)



#"HOURLY_TAG_BS_SUMMARIES"
Det_Summary <- function(SYS,Time_Str=NA,FullTag=NA,FullBS=NA,stringSub='9720060')
{
  STime <- Sys.time()
  # downloads HOURLY_TAG_BS_SUMMARIES data from server
  # FullTag default is NA resulting in downloding all tags
  if(all(c("user","password","host","port","dbname" ) %in% names(SYS)))
  {   
    dbc <- dbConnect(RMySQL::MySQL(),
                     user = SYS$user,            # username
                     password = SYS$password,    # password
                     host = SYS$host,            # host ip address
                     port=SYS$port ,             # port Number
                     dbname=SYS$dbname )         # name of data base
  }else   {stop("some system parameters are not defined")}
  # Ntag <- length(FullTag)
  if(is.na(FullBS[1]))
  {  if (is.na(Time_Str[1])&is.na(FullTag[1]))
  { 
    query = paste('select * from HOURLY_TAG_BS_SUMMARIES')
    Data <- dbGetQuery(dbc,query)
    dbDisconnect(dbc)
  }
    
    if (!is.na(Time_Str[1])&is.na(FullTag[1]))
    {
      if (length(Time_Str)!=2)
      {stop("time string should be a vector of two times in character format Y-m-d H:M:S")}
      
      num_Start_Time<-as.numeric(as.POSIXct(Time_Str[1],
                                            "%Y-%m-%d %H:%M:%S", tz="UTC"))
      num_End_Time<-as.numeric(as.POSIXct(Time_Str[2],
                                          "%Y-%m-%d %H:%M:%S", tz="UTC"))
      query = paste('select * from HOURLY_TAG_BS_SUMMARIES WHERE HOUR >=', num_Start_Time*1000,
                    'AND HOUR <=',num_End_Time*1000)
      Data <- dbGetQuery(dbc,query)
      dbDisconnect(dbc)
    }
    
    if (is.na(Time_Str[1])&!is.na(FullTag[1]))
    { 
      AllDatalist <- list()
      for (i in 1:length(FullTag)) 
      { # build a  DETECTIONS query for the system, the results include the variables listed below
        query = paste('select * from HOURLY_TAG_BS_SUMMARIES WHERE TAG=',FullTag[i])
        All_Data <- dbGetQuery(dbc,query)
        AllDatalist[[i]] <- All_Data
      }  
      Data <- do.call(rbind.data.frame, AllDatalist)
      dbDisconnect(dbc)
    }
    
    if (!is.na(Time_Str[1])&!is.na(FullTag[1]))
    { 
      if (length(Time_Str)!=2)
      {stop("time string should be a vector of two times in character format Y-m-d H:M:S")}
      
      num_Start_Time<-as.numeric(as.POSIXct(Time_Str[1],
                                            "%Y-%m-%d %H:%M:%S", tz="UTC"))
      num_End_Time<-as.numeric(as.POSIXct(Time_Str[2],
                                          "%Y-%m-%d %H:%M:%S", tz="UTC"))
      AllDatalist <- list()
      for (i in 1:length(FullTag)) 
      { # build a  DETECTIONS query for the system, the results include the variables listed below
        query = paste('select * from HOURLY_TAG_BS_SUMMARIES WHERE TAG =',FullTag[i],
                      'AND HOUR >=', num_Start_Time*1000, 'AND HOUR <=', num_End_Time*1000)
        All_Data <- dbGetQuery(dbc,query)
        AllDatalist[[i]] <- All_Data
      }  
      Data <- do.call(rbind.data.frame, AllDatalist)
      dbDisconnect(dbc)
    }}
  
  if(!is.na(FullBS[1]))
  {  if (is.na(Time_Str[1])&is.na(FullTag[1]))
  { 
    AllDatalist <- list()
    for (j in 1:length(FullBS)) 
    { # build a  DETECTIONS query for the system, the results include the variables listed below
      query = paste('select * from HOURLY_TAG_BS_SUMMARIES WHERE BS=',FullBS[j])
      All_Data <- dbGetQuery(dbc,query)
      AllDatalist[[j]] <- All_Data
    }  
    
    Data <- do.call(rbind.data.frame, AllDatalist)
    dbDisconnect(dbc)
  }
    
    if (!is.na(Time_Str[1])&is.na(FullTag[1]))
    {
      if (length(Time_Str)!=2)
      {stop("time string should be a vector of two times in character format Y-m-d H:M:S")}
      
      num_Start_Time<-as.numeric(as.POSIXct(Time_Str[1],
                                            "%Y-%m-%d %H:%M:%S", tz="UTC"))
      num_End_Time<-as.numeric(as.POSIXct(Time_Str[2],
                                          "%Y-%m-%d %H:%M:%S", tz="UTC"))
      AllDatalist <- list()
      for (j in 1:length(FullBS)) 
      { # build a  DETECTIONS query for the system, the results include the variables listed below
        query = paste('select * from HOURLY_TAG_BS_SUMMARIES WHERE BS=',FullBS[j],'AND HOUR >=', num_Start_Time*1000,
                      'AND HOUR <=',num_End_Time*1000)
        All_Data <- dbGetQuery(dbc,query)
        AllDatalist[[j]] <- All_Data
      }  
      
      Data <- do.call(rbind.data.frame, AllDatalist)
      dbDisconnect(dbc)
    }
    
    if (is.na(Time_Str[1])&!is.na(FullTag[1]))
    { 
      AllDatalist <- list()
      for (i in 1:length(FullTag)) 
      { # build a  DETECTIONS query for the system, the results include the variables listed below
        for (j in 1:length(FullBS)) 
        { # build a  DETECTIONS query for the system, the results include the variables listed below
          query = paste('select * from HOURLY_TAG_BS_SUMMARIES WHERE BS=',FullBS[j],'AND TAG=',FullTag[i])
          All_Data <- dbGetQuery(dbc,query)
          AllDatalist[[paste(as.character(i),as.character(j))]] <- All_Data
        }  
      }  
      Data <- do.call(rbind.data.frame, AllDatalist)
      dbDisconnect(dbc)
    }
    
    if (!is.na(Time_Str[1])&!is.na(FullTag[1]))
    { 
      if (length(Time_Str)!=2)
      {stop("time string should be a vector of two times in character format Y-m-d H:M:S")}
      
      num_Start_Time<-as.numeric(as.POSIXct(Time_Str[1],
                                            "%Y-%m-%d %H:%M:%S", tz="UTC"))
      num_End_Time<-as.numeric(as.POSIXct(Time_Str[2],
                                          "%Y-%m-%d %H:%M:%S", tz="UTC"))
      AllDatalist <- list()
      for (i in 1:length(FullTag)) 
      { # build a  DETECTIONS query for the system, the results include the variables listed below
        for (j in 1:length(FullBS)) 
        { # build a  DETECTIONS query for the system, the results include the variables listed below
          query = paste('select * from HOURLY_TAG_BS_SUMMARIES WHERE BS=',FullBS[j],'AND TAG=',FullTag[i],
                        'AND HOUR >=', num_Start_Time*1000,'AND HOUR <=',num_End_Time*1000)
          All_Data <- dbGetQuery(dbc,query)
          AllDatalist[[paste(as.character(i),as.character(j))]] <- All_Data
        }  
      }  
      Data <- do.call(rbind.data.frame, AllDatalist)
      dbDisconnect(dbc)
    }}
  
  print('The time to download and reshpe data was')
  print( Sys.time()-STime)
  
  Data$BS<-gsub(stringSub, '', Data$BS)
  Data$TAG<-gsub(paste0(stringSub,'00'), '', Data$TAG)
  
  Data$dateTime<-as.POSIXct((Data$HOUR)/1000, tz="UTC", origin="1970-01-01")
  
  return(Data)
  
}
#"HOURLY_TAG_SUMMARIES"
Loc_Summary <- function(SYS,Time_Str=NA,FullTag=NA,TagPrefix=c(972006000000,972001000000))
{
  STime <- Sys.time()
  # downloads HOURLY_TAG_SUMMARIES data from server
  # FullTag default is NA resulting in downloding all tags
  if(all(c("user","password","host","port","dbname" ) %in% names(SYS)))
  {   
    dbc <- dbConnect(RMySQL::MySQL(),
                     user = SYS$user,            # username
                     password = SYS$password,    # password
                     host = SYS$host,            # host ip address
                     port=SYS$port ,             # port Number
                     dbname=SYS$dbname )         # name of data base
  }else   {stop("some system parameters are not defined")}
  # Ntag <- length(FullTag)
  if (is.na(Time_Str[1])&is.na(FullTag[1]))
  { 
    query = paste('select * from HOURLY_TAG_SUMMARIES')
    Data <- dbGetQuery(dbc,query)
    dbDisconnect(dbc)
  }
  
  if (!is.na(Time_Str[1])&is.na(FullTag[1]))
  {
    if (length(Time_Str)!=2)
    {stop("time string should be a vector of two times in character format Y-m-d H:M:S")}
    
    num_Start_Time<-as.numeric(as.POSIXct(Time_Str[1],
                                          "%Y-%m-%d %H:%M:%S", tz="UTC"))
    num_End_Time<-as.numeric(as.POSIXct(Time_Str[2],
                                        "%Y-%m-%d %H:%M:%S", tz="UTC"))
    query = paste('select * from HOURLY_TAG_SUMMARIES WHERE HOUR >=', num_Start_Time*1000,
                  'AND HOUR <=',num_End_Time*1000)
    Data <- dbGetQuery(dbc,query)
    dbDisconnect(dbc)
  }
  
  if (is.na(Time_Str[1])&!is.na(FullTag[1]))
  { 
    AllDatalist <- list()
    for (i in 1:length(FullTag)) 
    { # build a  DETECTIONS query for the system, the results include the variables listed below
      query = paste('select * from HOURLY_TAG_SUMMARIES WHERE TAG=',FullTag[i])
      All_Data <- dbGetQuery(dbc,query)
      AllDatalist[[i]] <- All_Data
    }  
    Data <- do.call(rbind.data.frame, AllDatalist)
    dbDisconnect(dbc)
  }
  
  if (!is.na(Time_Str[1])&!is.na(FullTag[1]))
  { 
    if (length(Time_Str)!=2)
    {stop("time string should be a vector of two times in character format Y-m-d H:M:S")}
    
    num_Start_Time<-as.numeric(as.POSIXct(Time_Str[1],
                                          "%Y-%m-%d %H:%M:%S", tz="UTC"))
    num_End_Time<-as.numeric(as.POSIXct(Time_Str[2],
                                        "%Y-%m-%d %H:%M:%S", tz="UTC"))
    AllDatalist <- list()
    for (i in 1:length(FullTag)) 
    { # build a  DETECTIONS query for the system, the results include the variables listed below
      query = paste('select * from HOURLY_TAG_SUMMARIES WHERE TAG=',FullTag[i],
                    'AND HOUR >=', num_Start_Time*1000, 'AND HOUR <=', num_End_Time*1000)
      All_Data <- dbGetQuery(dbc,query)
      AllDatalist[[i]] <- All_Data
    }  
    Data <- do.call(rbind.data.frame, AllDatalist)
    dbDisconnect(dbc)
  }
  
  print('The time to download and reshpe data was')
  print( Sys.time()-STime)
  
  # Data$TAG<-gsub(paste0(stringSub,'00'), '', Data$TAG)
  # TAGs=data.frame(t1=as.numeric(Data$TAG),t2=abs(as.numeric(Data$TAG)-TagPrefix[1]),t3=abs(as.numeric(Data$TAG)-TagPrefix[2]))
  # Data$TAG<-as.character(apply(TAGs,1,FUN=min,na.rm=T))
  Data$TAG<-as.character(as.numeric(Data$TAG) %% 1e6)
  
  Data$dateTime<-as.POSIXct((Data$HOUR)/1000, tz="UTC", origin="1970-01-01")
  
  return(Data)
  
}

TagsCondition <- function(Data,TagList=NA, plot = 1,YEARS = NA, minLocperDay = 1000) 
{
  ####Function for tag usage plotting:####
  
  ## This function prints and returns a list of 4 plots:
  # 'DaysperTag'   - plots the number of active days (locations> minLocperDay) per tag
  # 'tagsPerMonth' - plots a count of active tags per month (at least one day with locations> minLocperDay)
  # 'TagsPerDay'   - plots the number of active tags per day
  # 'TagActiveChart' - plots the active days for each tag (color represents number of locations)
  ## The function accepts as parameters:
  # Data         - a data.frame created Loc_Summary (with 4 columns: "HOUR","TAG","LOCALIZATIONS","dateTime"  )
  # TagList      - optional: a data.frame with at least one column : "TAG", it may contain also "date_capture"
  #                          and "date_end" at either POSIXct or character %d/%m/%Y format. 
  #                          This will limit the tag plotted and starting date and end date
  # YEARS        - optional - numerical, this will limit the plotted years(start of track)
  # minLocperDay - default 1000 - set the threshold for counting a day 
  Tdata  <- Data %>% arrange(TAG,dateTime) %>% 
    mutate(Date=date(dateTime)) %>% 
    group_by(TAG,Date) %>% 
    mutate(TagLocPerDay=sum(LOCALIZATIONS)) %>% 
    slice_tail() %>% 
    ungroup() %>% 
    select("TAG", "dateTime",'Date','TagLocPerDay')
  
  if(is.na(YEARS[1]))
  {YEARS=sort(unique(year(Tdata$dateTime)))
  print('showing all years in dataset:')
  print(YEARS)}
  Tdata$TAG  <- (as.numeric(Tdata$TAG) %% 1e6)
  # This will create tags as factors with three digits to combine the CSV file with the localizations and ditections
  if(!is.na(TagList))
  {
    print('showing tags from TagList')
    TagList$TAG  <-(as.numeric(TagList$TAG) %% 1e6)
    if ('date_capture' %in% names(TagList))
    {
      print('showing dates starting on date_capture from TagList')
      if(!is.POSIXct(TagList$captureDate))
          {TagList$captureDate <-  as.POSIXct(TagList$date_capture,format='%d/%m/%Y',tz='UTC')}
    }else TagList$captureDate <- min(Tdata$dateTime)
    if ('date_end' %in% names(TagList))
    {
      print('showing dates ending on date_end from TagList')
      if(!is.POSIXct(TagList$endDate))
          {TagList$endDate <-  as.POSIXct(TagList$date_end,format='%d/%m/%Y',tz='UTC')}
    }else TagList$endDate <- max(Tdata$dateTime)
    if(!("species" %in% names(TagList)))
      TagList$species='unspecified species'
  }else 
    TagList <- data.frame(TAG=unique(Tdata$TAG),captureDate=min(Tdata$dateTime),endDate=max(Tdata$dateTime),species='unspecified species')
  
  Tdata <- merge(Tdata, TagList, by = 'TAG')
  
  Tdata<-Tdata %>% arrange(TAG,dateTime) %>% 
    filter(dateTime>captureDate) %>% 
    filter(dateTime<endDate)  
  Tdata$TAG <- as.factor(Tdata$TAG)
  
  
  # if (plot==1) # "number of Days with more than 1000 locations per tag for tags with start at ",YEAR
  { 
    dayData <- Tdata %>% filter(TagLocPerDay>minLocperDay) %>% 
      group_by(TAG)%>% 
      mutate(sday=min(Date)) %>% filter(year(sday) %in% YEARS) %>% 
      mutate(nDays=n()) %>% 
      slice(1) %>%
      ungroup() 
    
    plot1 <- ggplot(data=dayData, aes(x = reorder(TAG, -nDays), y=nDays)) +
       geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer(palette="Blues") +
       geom_text(aes(label=nDays),angle = 90, hjust=-0.3, size=3.5,color='red')+
       theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0,color='blue'))+
       ggtitle(paste("Days with more than 1000 locations for tags mounted on ",first(YEARS),':',last(YEARS))) +
       xlab("Tag") + ylab("# valid days") + # + labs(fill = "Tag Condition", color= " ")
       theme(plot.title = element_text(size=14, face="bold"),
                   axis.title.x = element_text(size=10, face="bold"),
                   axis.title.y = element_text(size=10, face="bold"),
                   axis.text.x = element_text(size=10,face="bold"),
                   axis.text.y = element_text(size=10,face="bold"))+
             theme(plot.title = element_text(hjust = 0.5))
    print(plot1)

  }
  
  # if (plot==2) # Number of unique tags tracked per month in for all tracking period "
  { 
    monthData <- Tdata %>% 
      filter(TagLocPerDay>minLocperDay) %>%
      mutate(YearMonth=paste0(year(dateTime),'-',month(dateTime))) %>% 
      group_by(YearMonth)%>% 
      mutate(nTags=length(unique(TAG))) %>% 
      slice(1) %>% 
      ungroup() 
      
    
    plot2 <- ggplot(data=monthData, aes(x = reorder(YearMonth, dateTime), y=nTags)) +
            geom_bar(stat="identity", position=position_dodge()) + scale_fill_brewer(palette="Blues") +
            geom_text(aes(label=nTags),angle = 90, hjust=-0.3, size=3.5,color='blue')+
            theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0,color='blue'))+
            ggtitle(paste("Number of unique tags tracked per month ","")) +
            xlab("Tags") + ylab("Unique tags") + # + labs(fill = "Tag Condition", color= " ")
            # axis
            theme(plot.title = element_text(size=16, face="bold"),
                  axis.title.x = element_text(size=12, face="bold"),
                  axis.title.y = element_text(size=12, face="bold"),
                  axis.text.x = element_text(size=12,face="bold"),
                  axis.text.y = element_text(size=10,face="bold"))+
      # legend title
            theme(legend.title =element_text(size=12,face="bold"))+
            theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
    
    print(plot2)
  }

  # if (plot==3) # Number of unique tags tracked per month "
  { 
    dateData <- Tdata %>% 
      filter(TagLocPerDay>minLocperDay) %>%
      group_by(TAG) %>% 
      mutate(sday=min(Date)) %>% filter(year(sday) %in% YEARS) %>% 
      group_by(Date)%>% 
      mutate(nTags=length(unique(TAG))) %>% 
      slice(1) %>% 
      ungroup() 

      plot3 <- ggplot(data=dateData, aes(x = Date, y=nTags)) +
      geom_line(size=3)+
      theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0,color='blue'))+
      labs(title = paste("Tags per day, (#Loc > ",minLocperDay,')'),
           subtitle =paste("for tags with track start in ", first(YEARS),':',last(YEARS) )) +
      scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
      xlab("Tags") + ylab("Unique tags")+  # + labs(fill = "Tag Condition", color= " ")
      theme(plot.title = element_text(size=16, face="bold"),
            axis.title.x = element_text(size=10, face="bold"),
            axis.title.y = element_text(size=10, face="bold"),
            axis.text.x = element_text(size=10,face="bold"),
            axis.text.y = element_text(size=10,face="bold"))+
      # legend title
      theme(legend.title =element_text(size=10,face="bold"))+
      theme(plot.title = element_text(hjust = 0.5))
    
    print(plot3)
  }
  
  # if (plot==4) # tag active days "
  { 
    tagDateData <- Tdata %>% filter(TagLocPerDay>minLocperDay)%>% 
      group_by(TAG) %>% 
      mutate(dayPerTag=n(),
             sday=min(Date)) %>% 
      ungroup() %>% 
      filter(year(dateTime) %in% YEARS)
      
    
    plot4 <- ggplot(data=tagDateData, aes(x = Date, y=as.factor(TAG),label=dayPerTag,color=TagLocPerDay)) +
      geom_point()+
      geom_text(aes(x=sday,y=as.factor(TAG), label=dayPerTag), hjust=2.0, size=3,color='red')+
      theme(axis.text.x = element_text(angle = 90, vjust = 0, hjust=0))+
      ggtitle(paste("Tag active days on" ,first(YEARS),':',last(YEARS), '(#Loc > ',minLocperDay,')')) +
      scale_x_date(date_breaks = "months" , date_labels = "%b-%y")+
      xlab("date") + ylab("Tag") +
            # axis
            theme(plot.title = element_text(size=12, face="bold"),
                  axis.title.x = element_text(size=10, face="bold"),
                  axis.title.y = element_text(size=10, face="bold"),
                  axis.text.x = element_text(size=10,face="bold"),
                  axis.text.y = element_text(size=6,face="bold"))+
            # legend title
       theme(legend.position = "none")
                #theme(legend.title =element_text(size=10,face="bold"))
    print(plot4)
    # dev.off()
    
  }
  return(list('DaysperTag'=plot1,'tagsPerMonth'=plot2,'TagsPerDay'=plot3,'TagActiveChart'=plot4))
}


# connParms <- list(user='XXXXXX',
#                   password = 'XXXXXX',
#                   host = '132.66.79.21',
#                   port=5900,      
#                   dbname='harod')
# 

# source('C:/Users/97254/Google Drive/POST/ATLAS/scripts/logins.R')
# Start_Time_Str     <- c('2021-09-15 00:00:01')
# End_Time_Str       <- c('2021-10-07 24:00:00')
# FullTag <- c(972006000003,972006000004,972006000006)
# B <- Det_Summary(SYS = connParms)
# A <- Loc_Summary(SYS = connParms)
# TagListExpmle <- data.frame(TAG=c(681,688),date_capture=c('10/11/2021','10/11/2021'),date_end=c('10/3/2022','10/3/2022'))
# plots <- TagsCondition(A,TagList=TagListExpmle)
# plots <- TagsCondition(A)
# plots <- TagsCondition(A,YEARS = c(2020,2021,2022))


