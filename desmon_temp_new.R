library(httr)
library(jsonlite)
library(rvest)
library(tidyverse)
library(twilio)
library(glue)
library(lubridate)

rm(list = ls())


## NYC temp range


a <- 73
b <- 74
c <- 75
d <- 76



## Chicago temp range



e <- 72
f <- 73
g <-74
h <- 75



##accuweather temp chi

if (minute(Sys.time()) == 10){
  accu_temp_chi <- GET("http://dataservice.accuweather.com/currentconditions/v1/26492_PC?apikey=p2ZLkSyTpDbgjKeoo2286lHH5Jb3VAST&details=true")
  
  acc_chi_data <- fromJSON(rawToChar(accu_temp_chi$content))%>%
    reduce(data.frame) %>%
    select(out, Imperial, Imperial.1)%>%
    reduce(data.frame) %>%
    select(time_output_chi=out, temp_chi = Value, temp_feel_chi = Value.1)
  
  ##accuweather temp nyc
  
  accu_temp_nyc <- GET("http://dataservice.accuweather.com/currentconditions/v1/2627448?apikey=p2ZLkSyTpDbgjKeoo2286lHH5Jb3VAST&details=true")
  
  acc_nyc_data <- fromJSON(rawToChar(accu_temp_nyc$content)) %>%
    reduce(data.frame) %>%
    select(out, Imperial, Imperial.1)%>%
    reduce(data.frame) %>%
    select(time_output_nyc=out, temp_nyc = Value, temp_feel_nyc = Value.1)
}else if (minute(Sys.time()) == 30){
  accu_temp_chi <- GET("http://dataservice.accuweather.com/currentconditions/v1/26492_PC?apikey=p2ZLkSyTpDbgjKeoo2286lHH5Jb3VAST&details=true")
  
  acc_chi_data <- fromJSON(rawToChar(accu_temp_chi$content))%>%
    reduce(data.frame) %>%
    select(out, Imperial, Imperial.1)%>%
    reduce(data.frame) %>%
    select(time_output_chi=out, temp_chi = Value, temp_feel_chi = Value.1)
  
  ##accuweather temp nyc
  
  accu_temp_nyc <- GET("http://dataservice.accuweather.com/currentconditions/v1/2627448?apikey=p2ZLkSyTpDbgjKeoo2286lHH5Jb3VAST&details=true")
  
  acc_nyc_data <- fromJSON(rawToChar(accu_temp_nyc$content)) %>%
    reduce(data.frame) %>%
    select(out, Imperial, Imperial.1)%>%
    reduce(data.frame) %>%
    select(time_output_nyc=out, temp_nyc = Value, temp_feel_nyc = Value.1)
}else if (minute(Sys.time()) == 50){
  accu_temp_chi <- GET("http://dataservice.accuweather.com/currentconditions/v1/26492_PC?apikey=p2ZLkSyTpDbgjKeoo2286lHH5Jb3VAST&details=true")
  
  acc_chi_data <- fromJSON(rawToChar(accu_temp_chi$content))%>%
    reduce(data.frame) %>%
    select(out, Imperial, Imperial.1)%>%
    reduce(data.frame) %>%
    select(time_output_chi=out, temp_chi = Value, temp_feel_chi = Value.1)
  
  ##accuweather temp nyc
  
  accu_temp_nyc <- GET("http://dataservice.accuweather.com/currentconditions/v1/2627448?apikey=p2ZLkSyTpDbgjKeoo2286lHH5Jb3VAST&details=true")
  
  acc_nyc_data <- fromJSON(rawToChar(accu_temp_nyc$content)) %>%
    reduce(data.frame) %>%
    select(out, Imperial, Imperial.1)%>%
    reduce(data.frame) %>%
    select(time_output_nyc=out, temp_nyc = Value, temp_feel_nyc = Value.1)
}else{
  acc_nyc_data <- tibble(time_output_nyc = NA, temp_nyc = NA,temp_feel_nyc = NA)
  acc_chi_data <- tibble(time_output_chi = NA, temp_chi = NA, temp_feel_chi = NA)
}




##wunder temp nyc

wunder_temp_nyc <- read_html("https://www.wunderground.com/weather/KNYC")%>%
  html_node(".wu-value-to")%>%
  html_text()

wunder_feel_temp_nyc<- read_html("https://www.wunderground.com/weather/KNYC")%>%
  html_node(".temp")%>%
  html_text()

wunder_temp_nyc_ts <- read_html("https://www.wunderground.com/weather/us/ny/central-park/10019")%>%
  html_node(".timestamp")%>%
  html_text()
wunder_temp_nyc <- as.numeric(wunder_temp_nyc)

wunder_feel_temp_nyc <- parse_number(wunder_feel_temp_nyc)

wunder_nyc <- tibble( wunder_temp_nyc,wunder_temp_nyc_ts,wunder_feel_temp_nyc )

##wunder temp chi

wunder_temp_chi <- read_html("https://www.wunderground.com/weather/KMDW")%>%
  html_node(".wu-value-to")%>%
  html_text()

wunder_feel_temp_chi <- read_html("https://www.wunderground.com/weather/KMDW")%>%
  html_node(".temp")%>%
  html_text()

wunder_temp_chi_ts <- read_html("https://www.wunderground.com/weather/KMDW")%>%
  html_node(".timestamp")%>%
  html_text()

wunder_temp_chi <- as.numeric(wunder_temp_chi)

wunder_feel_temp_chi<- parse_number(wunder_feel_temp_chi)

wunder_chi <- tibble( wunder_temp_chi,wunder_temp_chi_ts,wunder_feel_temp_chi )

## NYC current temp gov
nyc_current_temp <- read_html("https://forecast.weather.gov/MapClick.php?lat=40.7823&lon=-73.9654#.YxOV8OzMK3L") %>%
  html_nodes(".myforecast-current-lrg")%>%
  html_text()

nyc_temp_gov_web<-parse_number(nyc_current_temp)
nyc_temp_gov_web



## Chicago current temp gov
chi_current_temp <- read_html("https://forecast.weather.gov/MapClick.php?lat=41.7875&lon=-87.7416#.YxOJL-zMK3K") %>%
  html_nodes(".myforecast-current-lrg")%>%
  html_text()

chi_temp_gov_web<-parse_number(chi_current_temp)
chi_temp_gov_web

##weather ch nyc

weather_ch_nyc <- read_html("https://weather.com/weather/hourbyhour/l/2206:19:US")%>%
  html_node(".DetailsSummary--tempValue--1K4ka")%>%
  html_text()

weather_ch_nyc <- parse_number(weather_ch_nyc)

##weather ch chi

weather_ch_chi <- read_html("https://weather.com/weather/hourbyhour/l/47895a57832fa42b0f27ccd62ba1cace382d58779e776bf5a066b1f42d6cc5a1")%>%
  html_node(".DetailsSummary--tempValue--1K4ka")%>%
  html_text()

weather_ch_chi <- parse_number(weather_ch_chi)

# combined_temp <- tibble(datetime = Sys.time()-14400, weather_ch_chi,weather_ch_nyc,nyc_temp_gov_web,chi_temp_gov_web)%>%
#   bind_cols(wunder_chi,wunder_nyc)

combined_temp <- tibble(datetime = Sys.time()-14400, weather_ch_chi,weather_ch_nyc,nyc_temp_gov_web,chi_temp_gov_web)%>%
  bind_cols(wunder_chi,wunder_nyc,acc_chi_data,acc_nyc_data)


combined_temp <- combined_temp %>%
  select(datetime, chi_temp_gov_web, weather_ch_chi, wunder_temp_chi, wunder_feel_temp_chi,temp_chi , temp_feel_chi, wunder_temp_chi_ts,
         time_output_chi, nyc_temp_gov_web, weather_ch_nyc, wunder_temp_nyc,wunder_feel_temp_nyc, temp_nyc,temp_feel_nyc, wunder_temp_nyc_ts , time_output_nyc)

# combined_temp <- combined_temp %>%
#   select(datetime, chi_temp_gov_web, weather_ch_chi, wunder_temp_chi,temp_chi , wunder_temp_chi_ts, time_output_chi,
#          nyc_temp_gov_web, weather_ch_nyc, wunder_temp_nyc, temp_nyc, wunder_temp_nyc_ts, time_output_nyc)


## one time to save temp

combined_temp <-combined_temp%>%
  rowwise()%>%
  mutate(max_temp_chi = max(c_across(chi_temp_gov_web:temp_chi),na.rm =T),
         max_temp_nyc = max(c_across(nyc_temp_gov_web:temp_nyc),na.rm =T)
  )

# test <- temp_savings_test %>%
#   select(max_temp_chi)%>%
#  slice_max(max_temp_chi)
# 
# test1 <- temp_savings_test %>%
#   select(max_temp_nyc)%>%
#   slice_max(max_temp_nyc, n=1)%>%
#   slice_head()
# 
# test2 <- if_else(test$max_temp_chi > combined_temp$max_temp_chi,test$max_temp_chi,
#                  combined_temp$max_temp_chi)
# 
# test2 <- if_else(test$max_temp_nyc > combined_temp$max_temp_nyc,test$max_temp_nyc,
#                  combined_temp$max_temp_nyc)

combined_temp <- combined_temp %>%
  mutate(nyc_temp_bucket = case_when(
    max_temp_nyc  < a ~ glue("< {a}"),
    max_temp_nyc >= a & max_temp_nyc <= b ~glue("Between {a} - {b}"),
    max_temp_nyc >= c & max_temp_nyc <=d ~glue("Between {c} - {d}"),
    max_temp_nyc >d ~glue("> {d}"),
    TRUE ~"nada"
    
  ))

combined_temp <- combined_temp %>%
  mutate(chi_temp_bucket = case_when(
    max_temp_chi  < e ~glue("< {e}"),
    max_temp_chi  >= e & max_temp_chi <= f ~glue("Between {e} - {f}"),
    max_temp_chi  >= g & max_temp_chi <=h ~glue("Between {g} - {h}"),
    max_temp_chi >h ~glue("> {h}"),
    TRUE ~"nada"
    
  ))


# combined_temp <- combined_temp %>%
#   mutate(nyc_temp_bucket = case_when(
#     temp_nyc  < a ~ glue("< {a}"),
#     temp_nyc >= a & temp_nyc <= b ~glue("Between {a} - {b}"),
#     temp_nyc >= c & temp_nyc <=d ~glue("Between {c} - {d}"),
#     temp_nyc >d ~glue("> {d}"),
#     TRUE ~"nada"
#     
#   ))
# 
# combined_temp <- combined_temp %>%
#   mutate(chi_temp_bucket = case_when(
#     temp_chi  < e ~glue("< {e}"),
#     temp_chi  >= e & temp_chi <= f ~glue("Between {e} - {f}"),
#     temp_chi  >= g & temp_chi <=h ~glue("Between {g} - {h}"),
#     temp_chi  >h ~glue("> {h}"),
#     TRUE ~"nada"
#     
#   ))

combined_temp <- combined_temp %>%
  add_column(nyc_bucketcheck = NA)

combined_temp <- combined_temp %>%
  add_column(chi_bucketcheck = NA)

combined_temp <- combined_temp %>%
  add_column(temp_text = NA)


##write.csv(combined_temp, "temp_savings_test.csv", row.names = F)


temp_savings <- read_csv("temp_savings_test.csv")
##combine temp_savings and temp_recording


combined_temp <- rbind(temp_savings, combined_temp )


##check if previous bucket is the same as current bucket


##temp_slice <- slice_tail(temp_savings , n=2 )

combined_temp$nyc_bucketcheck <- if_else(combined_temp$nyc_temp_bucket > lag(combined_temp$nyc_temp_bucket), 1,0)

combined_temp$chi_bucketcheck <- if_else(combined_temp$chi_temp_bucket > lag(combined_temp$chi_temp_bucket), 1,0)

combined_temp <- combined_temp %>%
  mutate(temp_text = case_when(
    nyc_bucketcheck == 1 &  chi_bucketcheck == 1 ~ "Both cities new tier",
    nyc_bucketcheck == 1 &  chi_bucketcheck == 0 ~ "NYC new tier",
    nyc_bucketcheck == 0 &  chi_bucketcheck == 1 ~ "CHI new tier",
    TRUE~"No new tier"
  ))




write.csv(combined_temp,"temp_savings_test.csv", row.names = F)


view(slice_tail(combined_temp))

 

##JJJJ



# ## if bucketcheck equals one then send text
# 
# library(gmailr)
# use_secret_file("/users/omarbadran/downloads/desmond_temp_gmail.JSON")
# 
# my_email_message <- mime() %>%
#   to("omar.badran37@gmail.com") %>%
#   from("omar.badran37@gmail.com") %>%
#   subject("My test message") %>%
#   gm_text_body("Test")
# 
# send_message(my_email_message)
# 
# gm_auth_configure(path="/users/omarbadran/downloads/desmond_temp_gmail.JSON")
# 
# ### this chucnk is to send text
# 
# # First you need to set up your accound SID and token as environmental variables
# Sys.setenv(TWILIO_SID = "AC0f0fbd802538d90d23cd66b9b793e547")
# Sys.setenv(TWILIO_TOKEN = "072dc13eaaa340d485e2ca61f4d80925")
# 
# # Then we're just going to store the numbers in some variables
# my_phone_number <- "4129809101"
# twilios_phone_number <- "9705571371"
# 
# # Now we can send away!
# z <- 1
# 
# if_else(z ==  2,tw_send_message(from = twilios_phone_number, to = my_phone_number, 
#                                body = "test code"), "no msg send")
# 
# tw_send_message(from = twilios_phone_number, to = my_phone_number, 
#                 body = "test code")
# 
# 
# if(z == 1){
#   tw_send_message(from = twilios_phone_number, to = my_phone_number, 
#                   body = "test code")
# }else {
#   "no msg sent" 
# }
# 
# Sys.sleep(900)
# 
# ##################################################################
# 
# ## 15 minutes later re-run script, should run around 00:15 hours
# 
# ##################################################################
# 
# 
# ##accuweather temp chi
# 
# accu_temp_chi <- GET("http://dataservice.accuweather.com/currentconditions/v1/26492_PC?apikey=p2ZLkSyTpDbgjKeoo2286lHH5Jb3VAST")
# 
# acc_chi_data <- fromJSON(rawToChar(accu_temp_chi$content))%>%
#   reduce(data.frame) %>%
#   select(out, Imperial)%>%
#   reduce(data.frame) %>%
#   select(time_output_chi=out, temp_chi = Value)
# 
# ##accuweather temp nyc
# 
# accu_temp_nyc <- GET("http://dataservice.accuweather.com/currentconditions/v1/2627448?apikey=p2ZLkSyTpDbgjKeoo2286lHH5Jb3VAST")
# 
# acc_nyc_data <- fromJSON(rawToChar(accu_temp_nyc$content)) %>%
#   reduce(data.frame) %>%
#   select(out, Imperial)%>%
#   reduce(data.frame) %>%
#   select(time_output_nyc=out, temp_nyc = Value)
# 
# ##wunder temp nyc
# 
# wunder_temp_nyc <- read_html("https://www.wunderground.com/weather/KNYC")%>%
#   html_node(".wu-value-to")%>%
#   html_text()
# 
# wunder_temp_nyc_ts <- read_html("https://www.wunderground.com/weather/us/ny/central-park/10019")%>%
#   html_node(".timestamp")%>%
#   html_text()
# 
# wunder_nyc <- tibble( wunder_temp_nyc,wunder_temp_nyc_ts )
# 
# ##wunder temp chi
# 
# wunder_temp_chi <- read_html("https://www.wunderground.com/weather/KMDW")%>%
#   html_node(".wu-value-to")%>%
#   html_text()
# 
# wunder_temp_chi_ts <- read_html("https://www.wunderground.com/weather/KMDW")%>%
#   html_node(".timestamp")%>%
#   html_text()
# 
# wunder_chi <- tibble( wunder_temp_chi,wunder_temp_chi_ts )
# 
# ## NYC current temp gov
# nyc_current_temp <- read_html("https://forecast.weather.gov/MapClick.php?lat=40.7823&lon=-73.9654#.YxOV8OzMK3L") %>%
#   html_nodes(".myforecast-current-lrg")%>%
#   html_text()
# 
# nyc_temp_gov_web<-parse_number(nyc_current_temp)
# nyc_temp_gov_web
# 
# 
# 
# ## Chicago current temp gov
# chi_current_temp <- read_html("https://forecast.weather.gov/MapClick.php?lat=41.7875&lon=-87.7416#.YxOJL-zMK3K") %>%
#   html_nodes(".myforecast-current-lrg")%>%
#   html_text()
# 
# chi_temp_gov_web<-parse_number(chi_current_temp)
# chi_temp_gov_web
# 
# ##weather ch nyc
# 
# weather_ch_nyc <- read_html("https://weather.com/weather/hourbyhour/l/2206:19:US")%>%
#   html_node(".DetailsSummary--tempValue--1K4ka")%>%
#   html_text()
# 
# weather_ch_nyc <- parse_number(weather_ch_nyc)
# 
# ##weather ch chi
# 
# weather_ch_chi <- read_html("https://weather.com/weather/hourbyhour/l/47895a57832fa42b0f27ccd62ba1cace382d58779e776bf5a066b1f42d6cc5a1")%>%
#   html_node(".DetailsSummary--tempValue--1K4ka")%>%
#   html_text()
# 
# weather_ch_chi <- parse_number(weather_ch_chi)
# 
# combined_temp <- tibble(datetime = Sys.time()-14400, weather_ch_chi,weather_ch_nyc,nyc_temp_gov_web,chi_temp_gov_web)%>%
#   bind_cols(wunder_chi,wunder_nyc, acc_chi_data, acc_nyc_data)
# 
# combined_temp <- combined_temp %>%
#   select(datetime, chi_temp_gov_web, weather_ch_chi, wunder_temp_chi, temp_chi, wunder_temp_chi_ts, time_output_chi,
#          nyc_temp_gov_web, weather_ch_nyc, wunder_temp_nyc, temp_nyc, wunder_temp_nyc_ts, time_output_nyc)
# 
# 
# ## one time to save temp
# 
# ##write.csv(combined_temp, "temp_savings_test.csv", row.names = F)
# 
# combined_temp <- combined_temp %>%
#   mutate(nyc_temp_bucket = case_when(
#     temp_nyc  < a ~ glue("< {a}"),
#     temp_nyc >= a & temp_nyc <= b ~glue("Between {a} - {b}"),
#     temp_nyc >= c & temp_nyc <=d ~glue("Between {c} - {d}"),
#     temp_nyc >d ~glue("> {d}"),
#     TRUE ~"nada"
# 
#   ))
# 
# combined_temp <- combined_temp %>%
#   mutate(chi_temp_bucket = case_when(
#     temp_chi  < e ~glue("< {e}"),
#     temp_chi  >= e & temp_chi <= f ~glue("Between {e} - {f}"),
#     temp_chi  >= g & temp_chi <=h ~glue("Between {g} - {h}"),
#     temp_chi  >h ~glue("> {h}"),
#     TRUE ~"nada"
# 
#   ))
# 
# combined_temp <- combined_temp %>%
#   add_column(nyc_bucketcheck = NA)
# 
# combined_temp <- combined_temp %>%
#   add_column(chi_bucketcheck = NA)
# 
# combined_temp <- combined_temp %>%
#   add_column(temp_text = NA)
# 
# 
# 
# 
# temp_savings <- read_csv("temp_savings_test.csv")
# ##combine temp_savings and temp_recording
# 
# 
# combined_temp <- rbind(temp_savings, combined_temp )
# 
# 
# ##check if previous bucket is the same as current bucket
# 
# 
# ##temp_slice <- slice_tail(temp_savings , n=2 )
# 
# combined_temp$nyc_bucketcheck <- if_else(combined_temp$nyc_temp_bucket > lag(combined_temp$nyc_temp_bucket), 1,0)
# 
# combined_temp$chi_bucketcheck <- if_else(combined_temp$chi_temp_bucket > lag(combined_temp$chi_temp_bucket), 1,0)
# 
# combined_temp <- combined_temp %>%
#   mutate(temp_text = case_when(
#     nyc_bucketcheck == 1 &  chi_bucketcheck == 1 ~ "Both cities new tier",
#     nyc_bucketcheck == 1 &  chi_bucketcheck == 0 ~ "NYC new tier",
#     nyc_bucketcheck == 0 &  chi_bucketcheck == 1 ~ "CHI new tier",
#     TRUE~"No new tier"
#   ))
# 
# 
# write.csv(combined_temp,"temp_savings_test.csv", row.names = F)
# 
# 
# 
# 
# ##JJJJ
# 
# 
# 
# # ## if bucketcheck equals one then send text
# #
# # library(gmailr)
# # use_secret_file("/users/omarbadran/downloads/desmond_temp_gmail.JSON")
# #
# # my_email_message <- mime() %>%
# #   to("omar.badran37@gmail.com") %>%
# #   from("omar.badran37@gmail.com") %>%
# #   subject("My test message") %>%
# #   gm_text_body("Test")
# #
# # send_message(my_email_message)
# #
# # gm_auth_configure(path="/users/omarbadran/downloads/desmond_temp_gmail.JSON")
# #
# # ### this chucnk is to send text
# #
# # # First you need to set up your accound SID and token as environmental variables
# # Sys.setenv(TWILIO_SID = "AC0f0fbd802538d90d23cd66b9b793e547")
# # Sys.setenv(TWILIO_TOKEN = "072dc13eaaa340d485e2ca61f4d80925")
# #
# # # Then we're just going to store the numbers in some variables
# # my_phone_number <- "4129809101"
# # twilios_phone_number <- "9705571371"
# #
# # # Now we can send away!
# # z <- 1
# #
# # if_else(z ==  2,tw_send_message(from = twilios_phone_number, to = my_phone_number,
# #                                body = "test code"), "no msg send")
# #
# # tw_send_message(from = twilios_phone_number, to = my_phone_number,
# #                 body = "test code")
# #
# #
# # if(z == 1){
# #   tw_send_message(from = twilios_phone_number, to = my_phone_number,
# #                   body = "test code")
# # }else {
# #   "no msg sent"
# # }
# 
# 
# Sys.sleep(900)
# 
# ##################################################################
# 
# ## 15 minutes later re-run script, should run around 00:30 hours
# 
# ##################################################################
# 
# 
# ##accuweather temp chi
# 
# accu_temp_chi <- GET("http://dataservice.accuweather.com/currentconditions/v1/26492_PC?apikey=p2ZLkSyTpDbgjKeoo2286lHH5Jb3VAST")
# 
# acc_chi_data <- fromJSON(rawToChar(accu_temp_chi$content))%>%
#   reduce(data.frame) %>%
#   select(out, Imperial)%>%
#   reduce(data.frame) %>%
#   select(time_output_chi=out, temp_chi = Value)
# 
# ##accuweather temp nyc
# 
# accu_temp_nyc <- GET("http://dataservice.accuweather.com/currentconditions/v1/2627448?apikey=p2ZLkSyTpDbgjKeoo2286lHH5Jb3VAST")
# 
# acc_nyc_data <- fromJSON(rawToChar(accu_temp_nyc$content)) %>%
#   reduce(data.frame) %>%
#   select(out, Imperial)%>%
#   reduce(data.frame) %>%
#   select(time_output_nyc=out, temp_nyc = Value)
# 
# ##wunder temp nyc
# 
# wunder_temp_nyc <- read_html("https://www.wunderground.com/weather/KNYC")%>%
#   html_node(".wu-value-to")%>%
#   html_text()
# 
# wunder_temp_nyc_ts <- read_html("https://www.wunderground.com/weather/us/ny/central-park/10019")%>%
#   html_node(".timestamp")%>%
#   html_text()
# 
# wunder_nyc <- tibble( wunder_temp_nyc,wunder_temp_nyc_ts )
# 
# ##wunder temp chi
# 
# wunder_temp_chi <- read_html("https://www.wunderground.com/weather/KMDW")%>%
#   html_node(".wu-value-to")%>%
#   html_text()
# 
# wunder_temp_chi_ts <- read_html("https://www.wunderground.com/weather/KMDW")%>%
#   html_node(".timestamp")%>%
#   html_text()
# 
# wunder_chi <- tibble( wunder_temp_chi,wunder_temp_chi_ts )
# 
# ## NYC current temp gov
# nyc_current_temp <- read_html("https://forecast.weather.gov/MapClick.php?lat=40.7823&lon=-73.9654#.YxOV8OzMK3L") %>%
#   html_nodes(".myforecast-current-lrg")%>%
#   html_text()
# 
# nyc_temp_gov_web<-parse_number(nyc_current_temp)
# nyc_temp_gov_web
# 
# 
# 
# ## Chicago current temp gov
# chi_current_temp <- read_html("https://forecast.weather.gov/MapClick.php?lat=41.7875&lon=-87.7416#.YxOJL-zMK3K") %>%
#   html_nodes(".myforecast-current-lrg")%>%
#   html_text()
# 
# chi_temp_gov_web<-parse_number(chi_current_temp)
# chi_temp_gov_web
# 
# ##weather ch nyc
# 
# weather_ch_nyc <- read_html("https://weather.com/weather/hourbyhour/l/2206:19:US")%>%
#   html_node(".DetailsSummary--tempValue--1K4ka")%>%
#   html_text()
# 
# weather_ch_nyc <- parse_number(weather_ch_nyc)
# 
# ##weather ch chi
# 
# weather_ch_chi <- read_html("https://weather.com/weather/hourbyhour/l/47895a57832fa42b0f27ccd62ba1cace382d58779e776bf5a066b1f42d6cc5a1")%>%
#   html_node(".DetailsSummary--tempValue--1K4ka")%>%
#   html_text()
# 
# weather_ch_chi <- parse_number(weather_ch_chi)
# 
# combined_temp <- tibble(datetime = Sys.time()-14400, weather_ch_chi,weather_ch_nyc,nyc_temp_gov_web,chi_temp_gov_web)%>%
#   bind_cols(wunder_chi,wunder_nyc, acc_chi_data, acc_nyc_data)
# 
# combined_temp <- combined_temp %>%
#   select(datetime, chi_temp_gov_web, weather_ch_chi, wunder_temp_chi, temp_chi, wunder_temp_chi_ts, time_output_chi,
#          nyc_temp_gov_web, weather_ch_nyc, wunder_temp_nyc, temp_nyc, wunder_temp_nyc_ts, time_output_nyc)
# 
# 
# ## one time to save temp
# 
# ##write.csv(combined_temp, "temp_savings_test.csv", row.names = F)
# 
# combined_temp <- combined_temp %>%
#   mutate(nyc_temp_bucket = case_when(
#     temp_nyc  < a ~ glue("< {a}"),
#     temp_nyc >= a & temp_nyc <= b ~glue("Between {a} - {b}"),
#     temp_nyc >= c & temp_nyc <=d ~glue("Between {c} - {d}"),
#     temp_nyc >d ~glue("> {d}"),
#     TRUE ~"nada"
# 
#   ))
# 
# combined_temp <- combined_temp %>%
#   mutate(chi_temp_bucket = case_when(
#     temp_chi  < e ~glue("< {e}"),
#     temp_chi  >= e & temp_chi <= f ~glue("Between {e} - {f}"),
#     temp_chi  >= g & temp_chi <=h ~glue("Between {g} - {h}"),
#     temp_chi  >h ~glue("> {h}"),
#     TRUE ~"nada"
# 
#   ))
# 
# combined_temp <- combined_temp %>%
#   add_column(nyc_bucketcheck = NA)
# 
# combined_temp <- combined_temp %>%
#   add_column(chi_bucketcheck = NA)
# 
# combined_temp <- combined_temp %>%
#   add_column(temp_text = NA)
# 
# 
# 
# 
# temp_savings <- read_csv("temp_savings_test.csv")
# ##combine temp_savings and temp_recording
# 
# 
# combined_temp <- rbind(temp_savings, combined_temp )
# 
# 
# ##check if previous bucket is the same as current bucket
# 
# 
# ##temp_slice <- slice_tail(temp_savings , n=2 )
# 
# combined_temp$nyc_bucketcheck <- if_else(combined_temp$nyc_temp_bucket > lag(combined_temp$nyc_temp_bucket), 1,0)
# 
# combined_temp$chi_bucketcheck <- if_else(combined_temp$chi_temp_bucket > lag(combined_temp$chi_temp_bucket), 1,0)
# 
# combined_temp <- combined_temp %>%
#   mutate(temp_text = case_when(
#     nyc_bucketcheck == 1 &  chi_bucketcheck == 1 ~ "Both cities new tier",
#     nyc_bucketcheck == 1 &  chi_bucketcheck == 0 ~ "NYC new tier",
#     nyc_bucketcheck == 0 &  chi_bucketcheck == 1 ~ "CHI new tier",
#     TRUE~"No new tier"
#   ))
# 
# 
# write.csv(combined_temp,"temp_savings_test.csv", row.names = F)
# 
# 
# 
# ##JJJJ
# 
# 
# 
# # ## if bucketcheck equals one then send text
# #
# # library(gmailr)
# # use_secret_file("/users/omarbadran/downloads/desmond_temp_gmail.JSON")
# #
# # my_email_message <- mime() %>%
# #   to("omar.badran37@gmail.com") %>%
# #   from("omar.badran37@gmail.com") %>%
# #   subject("My test message") %>%
# #   gm_text_body("Test")
# #
# # send_message(my_email_message)
# #
# # gm_auth_configure(path="/users/omarbadran/downloads/desmond_temp_gmail.JSON")
# #
# # ### this chucnk is to send text
# #
# # # First you need to set up your accound SID and token as environmental variables
# # Sys.setenv(TWILIO_SID = "AC0f0fbd802538d90d23cd66b9b793e547")
# # Sys.setenv(TWILIO_TOKEN = "072dc13eaaa340d485e2ca61f4d80925")
# #
# # # Then we're just going to store the numbers in some variables
# # my_phone_number <- "4129809101"
# # twilios_phone_number <- "9705571371"
# #
# # # Now we can send away!
# # z <- 1
# #
# # if_else(z ==  2,tw_send_message(from = twilios_phone_number, to = my_phone_number,
# #                                body = "test code"), "no msg send")
# #
# # tw_send_message(from = twilios_phone_number, to = my_phone_number,
# #                 body = "test code")
# #
# #
# # if(z == 1){
# #   tw_send_message(from = twilios_phone_number, to = my_phone_number,
# #                   body = "test code")
# # }else {
# #   "no msg sent"
# # }
# 
# 
# Sys.sleep(900)
# 
# ##################################################################
# 
# ## 15 minutes later re-run script, should run around 00:45 hours
# 
# ##################################################################
# 
# 
# ##accuweather temp chi
# 
# accu_temp_chi <- GET("http://dataservice.accuweather.com/currentconditions/v1/26492_PC?apikey=p2ZLkSyTpDbgjKeoo2286lHH5Jb3VAST")
# 
# acc_chi_data <- fromJSON(rawToChar(accu_temp_chi$content))%>%
#   reduce(data.frame) %>%
#   select(out, Imperial)%>%
#   reduce(data.frame) %>%
#   select(time_output_chi=out, temp_chi = Value)
# 
# ##accuweather temp nyc
# 
# accu_temp_nyc <- GET("http://dataservice.accuweather.com/currentconditions/v1/2627448?apikey=p2ZLkSyTpDbgjKeoo2286lHH5Jb3VAST")
# 
# acc_nyc_data <- fromJSON(rawToChar(accu_temp_nyc$content)) %>%
#   reduce(data.frame) %>%
#   select(out, Imperial)%>%
#   reduce(data.frame) %>%
#   select(time_output_nyc=out, temp_nyc = Value)
# 
# ##wunder temp nyc
# 
# wunder_temp_nyc <- read_html("https://www.wunderground.com/weather/KNYC")%>%
#   html_node(".wu-value-to")%>%
#   html_text()
# 
# wunder_temp_nyc_ts <- read_html("https://www.wunderground.com/weather/us/ny/central-park/10019")%>%
#   html_node(".timestamp")%>%
#   html_text()
# 
# wunder_nyc <- tibble( wunder_temp_nyc,wunder_temp_nyc_ts )
# 
# ##wunder temp chi
# 
# wunder_temp_chi <- read_html("https://www.wunderground.com/weather/KMDW")%>%
#   html_node(".wu-value-to")%>%
#   html_text()
# 
# wunder_temp_chi_ts <- read_html("https://www.wunderground.com/weather/KMDW")%>%
#   html_node(".timestamp")%>%
#   html_text()
# 
# wunder_chi <- tibble( wunder_temp_chi,wunder_temp_chi_ts )
# 
# ## NYC current temp gov
# nyc_current_temp <- read_html("https://forecast.weather.gov/MapClick.php?lat=40.7823&lon=-73.9654#.YxOV8OzMK3L") %>%
#   html_nodes(".myforecast-current-lrg")%>%
#   html_text()
# 
# nyc_temp_gov_web<-parse_number(nyc_current_temp)
# nyc_temp_gov_web
# 
# 
# 
# ## Chicago current temp gov
# chi_current_temp <- read_html("https://forecast.weather.gov/MapClick.php?lat=41.7875&lon=-87.7416#.YxOJL-zMK3K") %>%
#   html_nodes(".myforecast-current-lrg")%>%
#   html_text()
# 
# chi_temp_gov_web<-parse_number(chi_current_temp)
# chi_temp_gov_web
# 
# ##weather ch nyc
# 
# weather_ch_nyc <- read_html("https://weather.com/weather/hourbyhour/l/2206:19:US")%>%
#   html_node(".DetailsSummary--tempValue--1K4ka")%>%
#   html_text()
# 
# weather_ch_nyc <- parse_number(weather_ch_nyc)
# 
# ##weather ch chi
# 
# weather_ch_chi <- read_html("https://weather.com/weather/hourbyhour/l/47895a57832fa42b0f27ccd62ba1cace382d58779e776bf5a066b1f42d6cc5a1")%>%
#   html_node(".DetailsSummary--tempValue--1K4ka")%>%
#   html_text()
# 
# weather_ch_chi <- parse_number(weather_ch_chi)
# 
# combined_temp <- tibble(datetime = Sys.time()-14400, weather_ch_chi,weather_ch_nyc,nyc_temp_gov_web,chi_temp_gov_web)%>%
#   bind_cols(wunder_chi,wunder_nyc, acc_chi_data, acc_nyc_data)
# 
# combined_temp <- combined_temp %>%
#   select(datetime, chi_temp_gov_web, weather_ch_chi, wunder_temp_chi, temp_chi, wunder_temp_chi_ts, time_output_chi,
#          nyc_temp_gov_web, weather_ch_nyc, wunder_temp_nyc, temp_nyc, wunder_temp_nyc_ts, time_output_nyc)
# 
# 
# ## one time to save temp
# 
# ##write.csv(combined_temp, "temp_savings_test.csv", row.names = F)
# 
# combined_temp <- combined_temp %>%
#   mutate(nyc_temp_bucket = case_when(
#     temp_nyc  < a ~ glue("< {a}"),
#     temp_nyc >= a & temp_nyc <= b ~glue("Between {a} - {b}"),
#     temp_nyc >= c & temp_nyc <=d ~glue("Between {c} - {d}"),
#     temp_nyc >d ~glue("> {d}"),
#     TRUE ~"nada"
# 
#   ))
# 
# combined_temp <- combined_temp %>%
#   mutate(chi_temp_bucket = case_when(
#     temp_chi  < e ~glue("< {e}"),
#     temp_chi  >= e & temp_chi <= f ~glue("Between {e} - {f}"),
#     temp_chi  >= g & temp_chi <=h ~glue("Between {g} - {h}"),
#     temp_chi  >h ~glue("> {h}"),
#     TRUE ~"nada"
# 
#   ))
# 
# combined_temp <- combined_temp %>%
#   add_column(nyc_bucketcheck = NA)
# 
# combined_temp <- combined_temp %>%
#   add_column(chi_bucketcheck = NA)
# 
# combined_temp <- combined_temp %>%
#   add_column(temp_text = NA)
# 
# 
# 
# 
# temp_savings <- read_csv("temp_savings_test.csv")
# ##combine temp_savings and temp_recording
# 
# 
# combined_temp <- rbind(temp_savings, combined_temp )
# 
# 
# ##check if previous bucket is the same as current bucket
# 
# 
# ##temp_slice <- slice_tail(temp_savings , n=2 )
# 
# combined_temp$nyc_bucketcheck <- if_else(combined_temp$nyc_temp_bucket > lag(combined_temp$nyc_temp_bucket), 1,0)
# 
# combined_temp$chi_bucketcheck <- if_else(combined_temp$chi_temp_bucket > lag(combined_temp$chi_temp_bucket), 1,0)
# 
# combined_temp <- combined_temp %>%
#   mutate(temp_text = case_when(
#     nyc_bucketcheck == 1 &  chi_bucketcheck == 1 ~ "Both cities new tier",
#     nyc_bucketcheck == 1 &  chi_bucketcheck == 0 ~ "NYC new tier",
#     nyc_bucketcheck == 0 &  chi_bucketcheck == 1 ~ "CHI new tier",
#     TRUE~"No new tier"
#   ))
# 
# 
# write.csv(combined_temp,"temp_savings_test.csv", row.names = F)
# 
# rm(list = ls())
# 
# 
# 
# 
# 
# # ## if bucketcheck equals one then send text
# #
# # library(gmailr)
# # use_secret_file("/users/omarbadran/downloads/desmond_temp_gmail.JSON")
# #
# # my_email_message <- mime() %>%
# #   to("omar.badran37@gmail.com") %>%
# #   from("omar.badran37@gmail.com") %>%
# #   subject("My test message") %>%
# #   gm_text_body("Test")
# #
# # send_message(my_email_message)
# #
# # gm_auth_configure(path="/users/omarbadran/downloads/desmond_temp_gmail.JSON")
# #
# # ### this chucnk is to send text
# #
# # # First you need to set up your accound SID and token as environmental variables
# # Sys.setenv(TWILIO_SID = "AC0f0fbd802538d90d23cd66b9b793e547")
# # Sys.setenv(TWILIO_TOKEN = "072dc13eaaa340d485e2ca61f4d80925")
# #
# # # Then we're just going to store the numbers in some variables
# # my_phone_number <- "4129809101"
# # twilios_phone_number <- "9705571371"
# #
# # # Now we can send away!
# # z <- 1
# #
# # if_else(z ==  2,tw_send_message(from = twilios_phone_number, to = my_phone_number,
# #                                body = "test code"), "no msg send")
# #
# # tw_send_message(from = twilios_phone_number, to = my_phone_number,
# #                 body = "test code")
# #
# #
# # if(z == 1){
# #   tw_send_message(from = twilios_phone_number, to = my_phone_number,
# #                   body = "test code")
# # }else {
# #   "no msg sent"
# # }
# 
# ##############
# ## END SCRIPT
# ##############
# 
# 
# 
