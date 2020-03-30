library(tidyverse)
library(scales)
if(T) {
  resp<-GET("https://data.humdata.org/hxlproxy/data/download/time_series_covid19_confirmed_global_narrow.csv?dest=data_edit&filter01=explode&explode-header-att01=date&explode-value-att01=value&filter02=rename&rename-oldtag02=%23affected%2Bdate&rename-newtag02=%23date&rename-header02=Date&filter03=rename&rename-oldtag03=%23affected%2Bvalue&rename-newtag03=%23affected%2Binfected%2Bvalue%2Bnum&rename-header03=Value&filter04=clean&clean-date-tags04=%23date&filter05=sort&sort-tags05=%23date&sort-reverse05=on&filter06=sort&sort-tags06=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv")
  csv <- content(resp)
  csv <- tail(csv,-1)
  csv %>% write.csv("./covid19.csv")
}
#d<-read_csv("~/Downloads/time_series_covid19_confirmed_global_narrow.csv")
d<-read_csv("./covid19.csv")


dd <- d %>% transmute(date=as_datetime(Date), place=`Country/Region`, cases=as.numeric(Value)) 
countries <- c('Italy','US', 'Spain')
country<-countries[1]
#for(country in countries) 
  {
  d0 <- dd %>% filter(place==country) %>% mutate(log_cases=log(cases)) %>% arrange(date)
  d1 <- d0 %>% filter(cases>5000) %>% mutate(x=as.numeric((date-date[1])/86400),xx=x*x)
  #print(d1 %>% ggplot(aes(x=date,y=cases)) + geom_line() + ggtitle(country))
  model<-lm(log_cases~x+xx, d1)
  print(country)
  print(model)
  mc<-as.numeric(model$coefficient)
  mc<-mc%>% c(rep(0, 3-length(mc)))
  dt0<-d1$date[1]
  est_cases <- function(x) exp(mc[1]+mc[2]*x+mc[3]*x*x)
  d2 <- d1 %>% mutate(cases.est=est_cases(x))
  d3 <- tibble(x=seq(0,40),cases.est=est_cases(x), date=dt0+days(x)) %>% left_join(d1 %>% select(x,cases),by="x")
  print(d3 %>% gather(stat, value, -date, -x) %>% ggplot(aes(x=date,y=value,colour=stat)) + geom_line() +
          scale_y_log10(labels = unit_format(unit = "K", scale = 1e-3), breaks=pretty_breaks(n=5)) + ggtitle(sprintf("%s: exp(%g+(%g)*x+(%g)*x*x)", country, mc[1],mc[2],mc[3])))
}