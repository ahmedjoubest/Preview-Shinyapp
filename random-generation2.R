library(data.table)
library(stringi)
library(feather)
library(lubridate)

# Faster reading than readRDS if we're not compromised by storage. More details in https://appsilon.com/fast-data-loading-from-files-to-r/
item_names <- c("shaker_A","shaker_B","shaker_C","shaker_D","supplement_A","supplement_B","supplement_C","supplement_D","supplement_E","supplement_F","bag_A","bag_B","bag_C","bag_D","bag_E","shoes_A","shoes_B","shoes_C","shoes_D","shoes_E","shoes_F","shoes_G","shoes_H","bike_A","bike_B","bike_C","bike_D","bike_E","bike_F","bike_G","bike_H","bike_I","bike_J","bike_K","bike_L","scooter_A","scooter_B","scooter_C","scooter_D","scooter_E","scooter_F","scooter_G","scooter_H","scooter_I","scooter_J","Dumbbell_A","Dumbbell_B","Dumbbell_C","Dumbbell_D","Dumbbell_E","jacket_A","jacket_B","jacket_C","jacket_D","jacket_E","watch_A","watch_B","watch_C","watch_D","watch_E","watch_F","watch_G","watch_H","treadmill_A","treadmill_B","treadmill_C","treadmill_D","treadmill_E","treadmill_F","treadmill_G","w_machine_A","w_machine_B","w_machine_C","w_machine_D","w_machine_E","w_machine_F","w_machine_G","w_machine_H","w_machine_I","w_machine_J")
store_names <- c("Evere","Charleroi","Mons","Verviers","Wavre","Louviere","Bruxelles","Namur","Nivelles","Hasselt","Alleur","OLEN","Ghent", "Roeselare","Dunkerque","Maldegem")

# Des nombres paires ! 
# Number of stores
Nb_store <- 16 # donner des maximum azbi !
# Total number of items in all stores (80 max)
Nb_items <- 80 # 80 max !
# Number of day of the period analyzed
Nb_jours <- 365*10
# Mean number of transaction per day, per store and per item 
mean_Nb_Transactions <- 0.222
# Random seed
seed <- 100

Generate_random_data <- function(Nb_store, Nb_items, Nb_jours, mean_Nb_Transactions,
                                 seed, store_names ,item_names){
  
  # Set random seed
  set.seed(seed)
  
  # Total number of transaction (= number of row of the data frame)
  Nb_Transactions <- Nb_items * Nb_jours * mean_Nb_Transactions * Nb_store
  print(paste0("nrow = ", as.character(Nb_Transactions)))
  ### Initialization
  set.seed(seed)
  DT = data.table(
    ### Store ID
    but_idr_business_unit = sample(stri_pad(1:Nb_store, 4, pad = "0"),
                                   size =  Nb_Transactions,
                                   replace = TRUE, # sampling be with replacement
                                   prob = c(abs(rnorm(Nb_store/2,4,3)), # sampling in different proportions
                                            abs(rnorm(Nb_store/2,5,2))
                                            ) 
    ),
    ### Transaction ID
    the_transaction_id = stri_pad(1:Nb_Transactions,
                                  8, pad = "0"),
    ### Item ID
    sku_idr_sku = sample(stri_pad(1:Nb_items, 5, pad = "0"),
                         size =  Nb_Transactions,
                         replace = TRUE, # sampling be with replacement
                         prob = c(abs(rnorm(Nb_items/2,3,2)), # sampling in different proportions
                                  abs(rnorm(Nb_items/2,6,4))
                                  )
    ),
    ### Transaction Type
    tdt_type_detail = sample(c("sale","return"),
                             size = Nb_Transactions,
                             replace = TRUE, # sampling be with replacement
                             prob = c(.75,.25) # assuming that 25% of transactions concern items returned
    )
  )
  print("Data.table created")
  
  ### add store names (for more realistic visualizations)
  DT[,store_name:= factor(but_idr_business_unit)]
  levels(DT$store_name) <- store_names[1:Nb_store]
  DT[,store_name:=as.vector(store_name)]
  print("store_name added")
  
  ### add item names (for more realistic visualizations)
  DT[,item_name:= factor(sku_idr_sku)]
  levels(DT$item_name) <- item_names[sample(1:Nb_items)]
  DT[,item_name:=as.vector(item_name)]
  print("item_name added")

  ### add Prices of items
  DT[,prices:= factor(sku_idr_sku)]
  levels(DT$prices) <- sample(1:1000,
                              Nb_items,
                              prob = sort(abs(rnorm(1000,0,50))
                                          , decreasing = T)
                              )
  DT[,prices:=as.numeric(as.vector(prices))]
  print("prices added")
  
  ### Add quantities
  DT[,quantity:= round(abs((rnorm(Nb_Transactions,0,2)))+0.51)]
  print("quantities added")
  
  ### Add Turnover
  DT[,turnover := quantity*prices]
  print("turnovers added")
  
  ### Add Dates (with accentuating the weekend effect)
  Dates <- seq(from = as.Date("2012-01-01"), # Historic data starting from 2015-01-01
            to = as.Date("2012-01-01") + Nb_jours -1,
            by ="day")
  days <- wday(Dates)
  # Creating probability vector for the sample function
  probs <- sapply(days,function(day){
    case_when(
      day==1 | day == 7  ~ 0.56, #(weekends)
      TRUE  ~ 0.44,
    )
  })
  DT[,the_date_transaction:= sample(Dates,
                                      size = Nb_Transactions,
                                      replace = TRUE, # sampling be with replacement
                                      prob = probs
                                    )
  ]
  
  ### add day/month/year/quarter/ columns
  DT[,month:= lubridate::month(the_date_transaction,label = TRUE, locale = "English")]
  DT[,year:= as.character(
    lubridate::year(the_date_transaction)
  )]
  DT[,quarter:= paste0("Qr.",
                       quarter(the_date_transaction)
  )]
  DT[,day:= lubridate::wday(the_date_transaction, label = TRUE, locale = "English")]
  # Correct the weekday level of Lubridate (starting with Monday)
  DT[,day:= factor(day,levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))]
  print("day/month/year/quarter added")
  
  ### Add aggregated weeks (for chronological heat map)
  # => Attribute for every day the "Monday" date of the week it's a part
  DT[,aggregated_date_week:= floor_date(the_date_transaction, "week",
                                        week_start = getOption("lubridate.week.start", 1))
     ]
  print("aggregated_date_week added")
  
  ### Remark : I would have add the aggregated Data directly in our plot functions, in helpers,
  ###          thanks to the rapidity of the {Lubridate} package. But I prefer add it to the this
  ###          parent Data as I am not compromised with the storage, for the time being....
              
  
  ### shuffle by rows
  DT <- DT[sample(nrow(DT))]
  
  ### save Rds
  setwd(choose.dir(caption = "Select a folder to save your Data.Rds"))
  write_feather(DT,"Data-Decathlon.feather")
  print("Data saved")
  
  ### Delete order
  
  #return(DT)
  return("done")
}

# test function .. (bhal li dar khona)

Generate_random_data(Nb_store, Nb_items, Nb_jours, mean_Nb_Transactions,
                     seed, store_names = store_names, item_names = item_names )

