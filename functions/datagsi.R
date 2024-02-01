datagsi<-function(dataframe){
  temp=dataframe
  gsi<-temp %>% 
    mutate(GSI=(GonWeight/Weight.g.)*100)
  return(gsi)
}
