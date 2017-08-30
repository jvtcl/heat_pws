library(rwunderground)
library(rgdal)
library(maptools)
library(ggmap)
library(RColorBrewer)
mykey='Insert your Wunderground API key here'

#### Generate/load station list #### 
if(!'maricopa_weather_stations.RData' %in% list.files()){
  
  ## Get search locations
  # cities=c('Phoenix','Tempe','Mesa','Gilbert','Chandler',
  #          'Scottsdale','Glendale','Peoria','Avondale','Tolleson','Surprise','Queen Creek',
  #          'Litchfield Park','Goodyear','Buckeye','Cave Creek','Apache Junction','Fountain Hills',
  #          'El Mirage','Sun City','Sun City West','Carefree','Paradise Valley')
  county=readOGR(dsn='cb_2016_us_county_500k',layer='cb_2016_us_county_500k')
  zip=readOGR(dsn = 'cb_2016_us_zcta510_500k',layer='cb_2016_us_zcta510_500k')
  
  # subset Maricopa county
  county=county[county$GEOID=='04013',]
  
  # ZIP locations within Maricopa county
  zip=SpatialPointsDataFrame(coords=coordinates(zip),
                             data=zip@data[,c('ZCTA5CE10','GEOID10')],
                             proj4string=CRS(proj4string(county)))
  zcheck=(zip %over% county)$GEOID
  search_zip=sort(as.character(zip@data[!is.na(zcheck),]$ZCTA5CE10))
  
  ## Get station data
  ct=0
  start_time=Sys.time()
  stations=do.call('rbind',lapply(search_zip,function(z){
    assign('ct',ct+1,envir=.GlobalEnv)
    elapsed=Sys.time()-start_time
    print(z)
    if(ct==99 & elapsed<=60){
      Sys.sleep(65) # adding 5sec to be cautious
      assign('ct',0,envir=.GlobalEnv)
    }
    geolookup(location=set_location(z),key=mykey)
  }))
  stations=unique(stations)
  
  ## save station list
  save(stations,file='maricopa_weather_stations.RData')
  
}else{
  load('maricopa_weather_stations.RData')
}

## Map Stations
stcd=SpatialPoints(stations[,c('lon','lat')])
qmplot(x=lon,y=lat,data=stations,colour = I('red'), size = I(3), darken = .3)


#### Get current conditions for all target stations ####

ct=0
start_time=Sys.time()
current_conditions=apply(stations,1,function(x){
  assign('ct',ct+1,envir=.GlobalEnv)
  elapsed=Sys.time()-start_time
  if(ct==99 & elapsed<=60){
    Sys.sleep(65) # adding 5sec to be cautious
    assign('ct',0,envir=.GlobalEnv)
  }
  x=data.frame(t(x),stringsAsFactors = F)
  print(x)
  tryCatch({
    if(x$type=='pws'){
      cc=conditions(location=set_location(territory=x$state,city=x$city,PWS_id = x$id),key=mykey)
    }else if(x$type=='airport'){
      cc=conditions(location=set_location(territory=x$state,city=x$city,airport_code = x$id),key=mykey)
    }
    list(sta=x$id,dat=data.frame(cc))
  },error=function(e){
    cat("could not pull",x$id,"\n\n")
  })
})

# unpack the current conditions into a data frame
ccdf=do.call(rbind,lapply(current_conditions,function(x){
  data.frame(station=x$sta,x$dat,stringsAsFactors = F)
}))
ccdf
# save(ccdf,file='maricopa_test_results_08_09.RData')
save(ccdf,file='maricopa_test_results_08_29.RData')


#### Map Example Results ####

temps=merge(stations[,c('id','lat','lon')],ccdf[,c('station','temp')],by.x='id',by.y='station')

tmin=10*floor(min(temps$temp)/10)
tmax=10*ceiling(max(temps$temp)/10)
temp_bk=seq(tmin,tmax,by=5)
temps$bk=as.character(cut(temps$temp,breaks=temp_bk,include_lowest=T))
temps$bk=substr(temps$bk,2,(nchar(temps$bk)-1)) # strip brackets
# ensure temperature intervals plot properly
temps$bk=sapply(temps$bk,function(b){
  bx=strsplit(b,',')
  bx=unlist(sapply(bx,function(x){
    sprintf('%03d',as.numeric(x))
  }))
  paste(bx,collapse=' - ')
})
temps$bk=factor(temps$bk,levels=sort(unique(temps$bk)))

ggmap(get_map('Phoenix, AZ',zoom=10,source='stamen',maptype='toner-background'))+
  geom_point(aes(lon,lat,color=bk),size=3,data=temps)+
  scale_colour_manual(values=brewer.pal(length(unique(temps$bk)),'YlOrRd'),guide=guide_legend(title='Temp (F)'))+
  ggtitle('Phoenix Metro Afternoon Temperatures\nAugust 29, 2017')
  