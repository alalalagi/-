#data = read.delim("clipboard")

for(i in c(nrow(data))){
  data$漲跌[i] = (data$收盤[i]-data$收盤[i-1])/data$收盤[i-1]
}

#Start Simulation-------------------------------------------------
hold = 0
buycount = 0; sellcount = 0
totalreturn = 0
for (i in c(2:nrow(data))){
  lastdate = unclass(as.Date(as.POSIXct(as.character(data$X[(i-1)]))))
  nowdate = unclass(as.Date(as.POSIXct(as.character(data$X[i]))))
  #print(nowdate)
  if(hold == 0){
    if(nowdate-lastdate>0){
      # print("經過一天")
      if((data$開盤[i]-data$收盤[i-1])/data$收盤[i-1]>0.015){
        #買在開盤價
        hold = 1
        buycount = buycount+1
        buyprice = data$開盤[i]
        maxafterbuy = buyprice #這裡會有bug
        print("Jump day buy")
        print(as.POSIXct(as.character(data$X[i])))
      }
    }
    else if (data$漲跌[i]>0.18){ #intra day
      hold = 1
      buycount = buycount+1
      buyprice = data$收盤[i]
      maxafterbuy = buyprice
      print("intraday buy")
      print(as.POSIXct(as.character(data$X[i])))
    }
  }
  else if(hold ==1){
    if (data$收盤[i]>maxafterbuy){
      maxafterbuy = data$收盤[i]
    }
    if (data$收盤[i]<0.985*buyprice){
      #sell
      print("Stop loss")
      print(as.POSIXct(as.character(data$X[i])))
      hold = 0
      sellcount = sellcount+1
      totalreturn = totalreturn-0.015
    }
    if (data$收盤[i]<0.97*maxafterbuy){
      #sell
      hold = 0
      sellcount = sellcount+1
      totalreturn = totalreturn+(data$收盤[i]-buyprice)/buyprice
      print("Earn")
      print(as.POSIXct(as.character(data$X[i])))
    }
  }
}


#處理時間語法------------------------------------
x = as.POSIXct(as.character(data$X[1])) #標準化
x = unclass(as.Date(x)) #算差幾天

x = unclass(as.Date(as.POSIXct(as.character(data$X[200]))))
x = unclass(as.Date("1970-02-02"))



