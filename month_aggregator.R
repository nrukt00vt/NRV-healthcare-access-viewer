dataset = read.csv("healthcarevisits_VA_new.csv")


dataset = dataset[!is.na(dataset$number),]

library(lubridate)

dataset$date = as.Date(dataset$date)

week(dataset$date)


new_dataset = data.frame()
month(ymd(dataset$date))
unique_dates = unique(dataset$date)
unique_combos = unique(dataset[c("safegraph_place","visitor_home_cbg")])
unique_combos$num = 0
start_dates = floor_date(unique_dates,'month')
end_dates = ceiling_date(unique_dates,'month')-1

for (i in 1:length(start_dates)){
  for (date in seq(start_dates[i],end_dates[i],by=1)){
    placeholder_date = date
    print(placeholder_date)
    while (is.element(placeholder_date, unique_dates) == F){
      print(placeholder_date)
      placeholder_date = placeholder_date + 1
      print("adding")
    }
    if (is.element(placeholder_date, unique_dates) == T){
      print(placeholder_date)
      print("analyzing")
      sub_data = subset(dataset, date == placeholder_date)
      sub_data$number_day = sub_data$number / 7
      for (k in 1:nrow(unique_combos)){
        sub_data_2 = subset(sub_data, safegraph_place == unique_combos$safegraph_place[k] &
                              visitor_home_cbg == unique_combos$visitor_home_cbg[k])
        if (nrow(sub_data_2)>0){
          unique_combos$num[k] = unique_combos$num[k] + sum(sub_data_2$number_day)
        }
      }
    }
  }
}
dataset$month = month(ymd(dataset$date))

dataset$year = year(ymd(dataset$date))

testsub = subset(dataset, month == 1 & year == 2019)
testsub_2 = aggregate(testsub$number, by=list(testsub$safegraph_place, testsub$visitor_home_cbg),FUN = sum)
names(testsub_2) = c("safegraph_place", "visitor_home_cbg", "numtest")
testmerge = merge(unique_combos,testsub_2)
month_data = expand.grid(start_date = , 
                        safegraph_place = unique_combos$safegraph_place, visitor_home_cbg = unique_combos$visitor_home_cbg)


