library(httr)
library(readxl)
library(dplyr)

Valmyndigheten_api<-function(path){
  path=paste('download/',path,sep='')
  url<-modify_url('https://www.val.se/',path=path)
  resp<-GET(url)
  
  if(http_type(resp)!='application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'){
    stop('API did not return proper output',call. = FALSE)
  }

  raw<-resp$content
  tmp<-tempfile(fileext = '.xlsx')
  writeBin(raw,tmp)
  parsed<-read_excel(tmp)
  
  if(status_code(resp)!=200){
    stop('API Request failed',call. = FALSE)
  }
  structure(
    list(
      content=parsed,
      path=path,
      response=resp
    ),
    class='Valmyndigheten_api'
  )
}

get_p<-function(){
  response=Valmyndigheten_api(paste('18.14c1f613181ed0043d567ae/1663009000443/valresultat-riksdagen-preliminar-jamforande-statistik.xlsx',sep=''))
  response$content<-response$content[,colSums(is.na(response$content))<nrow(response$content)]
  response$content<-na.omit(response$content)
  response$content<-response$content%>% select(-8)
  colnames(response$content)<-c('parties','voices 2022','shares 2022','Diff voices','Diff shares','voices 2018','shares 2018','mandate 2022','diff mandate','mandate 2018')
  return(as.data.frame(response$content))
}
get_p()

result_p<-get_p()

get_p_a<-function(){
  response=Valmyndigheten_api(paste('18.14c1f613181ed0043d56f51/1663745020932/preliminar-riksdagsval-jamforande-statistik-2018-2022-med-uppsamlingsdistrikt-ny.xlsx',sep=''))
  response$content<-response$content[,colSums(is.na(response$content))<nrow(response$content)]
  response$content<-na.omit(response$content)
  response$content<-response$content%>% select(-8)
  colnames(response$content)<-c('parties','voices 2022(a)','shares 2022(a)','Diff voices(a)','Diff shares(a)','voices 2018(a)','shares 2018(a)','mandate 2022(a)','diff mandate(a)','mandate 2018(a)')
  return(as.data.frame(response$content))
}
get_p_a()

result_p_a<-get_p_a()

get_combined_data<-function(){
  merged_data<-merge(result_p,result_p_a,by='parties')
  return(merged_data)
}
View(get_combined_data())
