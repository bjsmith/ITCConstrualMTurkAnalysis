bonus_date<-"2019-01-01"
bonus_start_date<-"2018-05-29" 
#don't pay bonuses BEFORE this date because they've already been paid out.
#This isn't necessary to prevent overpayment because turkprime takes a "top-up" command to top up a bonus to a specified amount, 
#rather than an "add" command which adds to what has already been paid.
#but it will save me a bit of time if I see that a generated add command has zero entres.
source("get_data_exp20180330.R")

source("pay_exp20180329.R")

source("pay_exp20180327.R")

#these should have been all paid now.
#source("pay_exp20180315.R")