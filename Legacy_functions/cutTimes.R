#insert into functions in Heverdata
cutTimes <- function(data,TagTimeTable,keep=NA,
                     dataTimevarName='dateTime',dataIdentifierName='TAG',
                     tableStartName='startTime', tableEndName='endTime',tableIdentifierName='TAG')
{ # this function accepts two dataframes:
  # data, which include many sampled points with time variable in Posixct format
  # TagTimeTable which include a list of tags and startTime and endTime for each tag
  # it filter (in or out) these times from the data according to the keep argument
  # the default columns names are 'dateTime' and 'TAG' in the data data.frame 
  # and 'startTime','endTime', and 'TAG' in the TagTimeTable data.frame, 
  # other names can be specified as function arguments!
  if (!all(c(dataTimevarName,dataIdentifierName) %in% names(data)) )
    simpleError(sprintf( 'data doesnt contain the required variable: %s, %s' , dataTimevarName,dataIdentifierName)  )
  if (!all(c(tableStartName,tableEndName,tableIdentifierName) %in% names(TagTimeTable)) )
    simpleError(sprintf( 'TagTimeTable doesnt contain the required variable: %s, %s, %s' , tableStartName,tableEndName,tableIdentifierName)  )
  if (is.na(keep) )
    simpleError(sprintf( 'you must specify whether to keep or not'))
  
  colnames(data)[(colnames(data)==dataTimevarName)] <- 'dateTime'
  colnames(data)[(colnames(data)==dataIdentifierName)] <- 'TAG'
  colnames(TagTimeTable)[(colnames(TagTimeTable)==tableStartName)] <- 'startTime'
  colnames(TagTimeTable)[(colnames(TagTimeTable)==tableEndName)] <- 'endTime'
  colnames(TagTimeTable)[(colnames(TagTimeTable)==tableIdentifierName)] <- 'TAG'
  if(keep)
  {
    data1 <- list()
    for (Idx in seq(1,nrow(TagTimeTable)))
    {
      tag <- TagTimeTable$TAG[Idx]
      startTime <- TagTimeTable$startTime[Idx]
      endTime <- TagTimeTable$endTime[Idx]
      if (tag %in% unique(data$TAG))
      { 
        print(sprintf('TAG, %s, keeping times between %s and %s', TagTimeTable$TAG[Idx],TagTimeTable$startTime[Idx],TagTimeTable$endTime[Idx]))
        data1[[as.character(Idx)]] <- dplyr::filter(data,dateTime>startTime&dateTime<endTime&TAG==tag)
      }
    }
    data <- bind_rows(data1)
    data <- data[which(data$TAG %in% unique(TagTimeTable$TAG)),]
  }else
  {
    for (Idx in seq(1,nrow(TagTimeTable)))
    {tag <- TagTimeTable$TAG[Idx]
    startTime <- TagTimeTable$startTime[Idx]
    endTime <- TagTimeTable$endTime[Idx]
    if (tag %in% unique(data$TAG))
    {    
      print(sprintf('TAG, %s, discarding times between %s and %s', TagTimeTable$TAG[Idx],TagTimeTable$startTime[Idx],TagTimeTable$endTime[Idx]))
      data <- dplyr::filter(data,dateTime<startTime|dateTime>endTime|TAG!=tag)
    }
    }
  }
  
  colnames(data)[(colnames(data)=='dateTime')] <- dataTimevarName
  colnames(data)[(colnames(data)=='TAG')] <-dataIdentifierName
  return(data)
}

# Example building  a table to define the filter

# TagTimeTable <- data.frame(ds_name=c("430", "228", "229"),
#                            startTime=as.POSIXct(c("2021-12-26 00:00:06","2021-12-26 04:00:06","2021-12-26 12:00:06 "),tz='UTC'),
#                            endTime=  as.POSIXct(c("2021-12-26 24:00:0","2021-12-26 20:00:06","2021-12-26 16:00:06 "),tz='UTC'))
# data1 <- cutTimes(data,TagTimeTable,keep=T,
#                    dataTimevarName='dateTime',dataIdentifierName='TAG',
#                    tableStartName='startTime', tableEndName='endTime',tableIdentifierName='ds_name')

subsetTimes <- function(data,TimevarName='dateTime',IdentifierName='TAG',mingaptokeep)
{
  library(dplyr)
  colnames(data)[(colnames(data)==TimevarName)] <- 'dateTimeTemp'
  colnames(data)[(colnames(data)==IdentifierName)] <- 'TAGTemp'
  data <- data %>%  arrange(TAGTemp,dateTimeTemp) %>% 
                    mutate(reducedTime=floor(as.numeric(dateTimeTemp)/60/mingaptokeep)) %>% 
                    group_by(TAGTemp,reducedTime) %>% 
                    slice(1) %>% 
                    ungroup() %>% 
                    dplyr::select(-c(reducedTime))
  colnames(data)[(colnames(data)=='dateTimeTemp')] <- TimevarName
  colnames(data)[(colnames(data)=='TAGTemp')] <-IdentifierName
  return(as.data.frame(data))
}

