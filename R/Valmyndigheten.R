#' Class For Converting Raw Data to Parsed Data From Valmyndigheten XML.
#'
#' @param path specifies the path for excel file to be downloaded
#'
#' @return Retrieve Parsed data Contents of the Excel for the given path(url)
#' @import httr
#' @import readxl
#' @import dplyr
#' @export Valmyndigheten_api
#'

Valmyndigheten_api<-function(path){
  if (length(path)>1){
    stop('path cannot take more than one input')
  }
  
  path=paste('download/',path,sep='')
  url<-httr::modify_url('https://www.val.se/',path=path) # using modify_url to attach path
  resp<-httr::GET(url)
  
  # stopping it if the http_type is not expected output.
  if(httr::http_type(resp)!='application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'){
    stop('API did not return proper output',call. = FALSE)
  }

  raw<-resp$content
  
  # writing an empty xlxs file
  tmp<-tempfile(fileext = '.xlsx')
  writeBin(raw,tmp)
  
  # getting parsed data
  parsed<-readxl::read_excel(tmp)
  
  # stopping if the status code is not 200
  if(httr::status_code(resp)!=200){
    stop('API Request failed',call. = FALSE)
  }
  
  # assigning the attributes in class
  structure(
    list(
      content=parsed,
      path=path,
      response=resp
    ),
    class='Valmyndigheten_api'
  )
}

#' Getting The election result data from polling station count excel file
#'
#' @return The cleaned data from excel file
#' @import httr
#' @import readxl
#' @import dplyr
#' @importFrom stats na.omit
#' @export
#'


get_p<-function(){
  response=Valmyndigheten_api(paste('18.14c1f613181ed0043d567ae/1663009000443/valresultat-riksdagen-preliminar-jamforande-statistik.xlsx',sep=''))
  
  # removing columns having all the values na
  response$content<-response$content[,colSums(is.na(response$content))<nrow(response$content)]
  
  # removing na value in rows
  response$content<-na.omit(response$content)
  
  # removing duplicate column
  response$content<-response$content%>% dplyr::select(-8)
  colnames(response$content)<-c('parties','voices 2022','shares 2022(%)','Diff voices','Diff shares(%)','voices 2018','shares 2018(%)','mandate 2022','Diff mandate','mandate 2018')
  
  return(as.data.frame(response$content)) # returning as data.frame
}
get_p()


#' Getting The election result data including assembly district
#'
#' @return The cleaned data from excel file
#' @import httr
#' @import readxl
#' @import dplyr
#' @importFrom stats na.omit
#' @export
#'

get_p_a<-function(){
  response=Valmyndigheten_api(paste('18.14c1f613181ed0043d56f51/1663745020932/preliminar-riksdagsval-jamforande-statistik-2018-2022-med-uppsamlingsdistrikt-ny.xlsx',sep=''))
  
  # removing columns having all the values na
  response$content<-response$content[,colSums(is.na(response$content))<nrow(response$content)]
  
  # removing na value in rows
  response$content<-na.omit(response$content)
  
  # removing duplicate column
  response$content<-response$content%>% dplyr::select(-8)
  colnames(response$content)<-c('parties','voices 2022 including assembly districts','shares 2022 including assembly districts(%)','Diff voices including assembly districts','Diff shares including assembly districts(%)','voices 2018 including assembly districts','shares 2018 including assembly districts(%)','mandate 2022 including assembly districts','Diff mandate including assembly districts','mandate 2018 including assembly districts')
  
  return(as.data.frame(response$content))
}
get_p_a()


#' Combined data of polling station counts and assembly districts
#'
#' @return combined data from get_p() and get_p_a() functions.
#' @export
#'
#' @examples
get_combined_data<-function(){
  
  #merging the above two data.frames
  merged_data<-merge(get_p(),get_p_a(),by='parties')
  return(merged_data)
}
get_combined_data()

