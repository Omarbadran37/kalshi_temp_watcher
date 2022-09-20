library(rvest)
library(tidyverse)
library(twilio)




## NYC temp range


a <- 81
b <- 82
c <- 83
d <- 84



## Chicago temp range



e <- 85
f <- 86
g <-87
h <- 88





# pull current temp from NYC and Chicago


## NYC current temp
nyc_current_temp <- read_html("https://forecast.weather.gov/MapClick.php?lat=40.7823&lon=-73.9654#.YxOV8OzMK3L") %>%
  html_nodes(".myforecast-current-lrg")%>%
  html_text()

nyc_temp<-parse_number(nyc_current_temp)
nyc_temp




## Chicago current temp
chi_current_temp <- read_html("https://forecast.weather.gov/MapClick.php?lat=41.7875&lon=-87.7416#.YxOJL-zMK3K") %>%
  html_nodes(".myforecast-current-lrg")%>%
  html_text()

chi_temp<-parse_number(chi_current_temp)
chi_temp


## current time and date

datetime <- Sys.time()-14400



## add data to a dataframe

temp_recording <- data.frame(datetime = datetime,
                             nyc_temp = nyc_temp,
                             chi_temp = chi_temp
                             
)


## find out which bucket the 


temp_recording <- temp_recording %>%
  mutate(nyc_temp_bucket = case_when(
    nyc_temp  < a ~1,
    nyc_temp >= a & nyc_temp <= b ~2,
    nyc_temp >= c & nyc_temp <=d ~3,
    nyc_temp >d ~4,
    TRUE ~0
    
  ))

temp_recording <- temp_recording %>%
  mutate(chi_temp_bucket = case_when(
    chi_temp  < e ~1,
    chi_temp >= e & chi_temp <= f ~2,
    chi_temp >= g & chi_temp <=h ~3,
    chi_temp >h ~4,
    TRUE ~0
    
  ))

temp_recording <- temp_recording %>%
  add_column(nyc_bucketcheck = NA)

temp_recording <- temp_recording %>%
  add_column(chi_bucketcheck = NA)


## one time to save temp

# write.csv(temp_recording,"temp_savings.csv", row.names = F)

temp_savings <- read_csv("temp_savings.csv")
##combine temp_savings and temp_recording


temp_savings <- rbind(temp_savings, temp_recording )


##check if previous bucket is the same as current bucket


##temp_slice <- slice_tail(temp_savings , n=2 )

temp_savings$nyc_bucketcheck <- if_else(temp_savings$nyc_temp_bucket > lag(temp_savings$nyc_temp_bucket), 1,0)

temp_savings$chi_bucketcheck <- if_else(temp_savings$chi_temp_bucket > lag(temp_savings$chi_temp_bucket), 1,0)


write.csv(temp_savings,"temp_savings.csv", row.names = F)

rm(list = ls())



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







