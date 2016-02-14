library(reshape2)
library(ggplot2)
library(dplyr)

oil<-c(3.4,3.6,3.58,4.15,9.07,10.38,10.89,11.96,12.46,17.72,28.07,35.24,31.87,28.99,28.63,26.75,
       14.55,17.9,14.67,17.97,22.22,19.06,18.43,16.41,15.59,17.23,20.71,19.04,	
       12.52,17.51,28.26,22.95,24.1,28.53,36.98,50.24,60.24,67.94,94.74,59.29,	
       76.69,101.87,100.93,100.49,92.02)

elc<-c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,6.57,6.57,6.57,6.57,6.57,6.89,6.89,6.89,6.89,6.89,6.81,6.81,6.81,6.81,6.81,
       8.14,8.9,9.13,9.74,9.82,9.83,9.9,9.84,10.07,10.45)

####################

set.seed(1234)
t_f=c(2015:2030)
oil_f=c(53.82,74.68,59.79,93.73,69.29,99.18,78.32,76.11,103.88,81.29,87.51,90.58,93.63,96.67,99.70,102.71)
gas_f=oil/42+0.221887722+0.931621482
diesel_f=oil/42+0.245274826+1.282128929

gas_f=matrix(gas_f,nrow=1,ncol=16)
diesel_f=matrix(diesel_f,nrow=1,ncol=16)

oil_h=c(14.51,15.95,31.99,33.50,33.32,34.46,33.54,44.06,64.03,73.52,62.60,54.78,52.25,47.30,25.22,30.26,23.96,	
        28.25,33.68,27.96,26.44,22.99,21.39,23.15,27.33,24.70,16.07,22.14,34.93,27.74,28.69,33.30,42.00,55.28,	
        64.31,70.65,96.63,60.01,76.69,99.81,97.14,95.30,86.01)
#gas_h=c()


gas_lag=c(3.06,gas_f[1:15])

e85_sim=matrix(NA,nrow=1000,ncol=16)
el_sim=matrix(NA,nrow=1000,ncol=16)
cng_sim=matrix(NA,nrow=1000,ncol=16)
pro_sim=matrix(NA,nrow=1000,ncol=16)

e85_f=matrix(NA,nrow=1,ncol=16)
el_f=matrix(NA,nrow=1,ncol=16)
cng_f=matrix(NA,nrow=1,ncol=16)
pro_f=matrix(NA,nrow=1,ncol=16)

for (j in 1:1000){
  for (i in 1:16){
    if(i==1){
      e85_f[i]=1.022513*gas_f[i]-0.5725263*gas_lag[i]-0.123186+rnorm(1,mean=0,sd=0.0886995)+0.6729827*3.73
      el_f[i]=0.0351997*gas_f[i]+0.276893+rnorm(1,mean=0,sd=0.0373941)+0.6586658*1.1369
      cng_f[i]=0.2493429*gas_f[i]-0.1289041*3.73+0.205904+rnorm(1,mean=0,sd=0.076607)+0.7594448*1.98
      pro_f[i]=0.5556694*diesel_f[i]-0.3459676*3.73+1.39269*1.1369+0.7297167*3.8967-0.8839494+rnorm(1,mean=0,sd=0.1989561)
    }
    else{
      e85_f[i]=1.022513*gas_f[i]-0.5725263*gas_lag[i]-0.123186+rnorm(1,mean=0,sd=0.0886995)+0.6729827*e85_f[i-1]
      el_f[i]=0.0351997*gas_f[i]+0.276893+rnorm(1,mean=0,sd=0.0373941)+0.6586658*el_f[i-1]
      cng_f[i]=0.2493429*gas_f[i]-0.1289041*e85_f[i-1]+0.205904+rnorm(1,mean=0,sd=0.076607)+0.7594448*cng_f[i-1]
      pro_f[i]=0.5556694*diesel_f[i]-0.3459676*e85_f[i-1]+1.39269*el_f[i-1]+0.7297167*pro_f[i-1]-0.8839494+rnorm(1,mean=0,sd=0.1989561)
    }
  }
  e85_sim[j,]=e85_f
  el_sim[j,]=el_f
  cng_sim[j,]=cng_f
  pro_sim[j,]=pro_f
}

shinyServer(function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    # Create artificial data
    #set.seed(123)
    dd = data.frame(time = 2015:2030,
                    Gas = gas_f[1,],
                    Diesel = diesel_f[1,],
                    E85 = e85_sim[1,], 
                    Electricity = el_sim[1,],
                    CNG = cng_sim[1,],
                    Propane = pro_sim[1,]
                   )
    dd_m = melt(dd, id.vars = 'time')
    if (input$timescale == '5') {
      dd_m = dd_m %>%
        group_by(time = floor(dd_m$time / 5), variable) %>%
        summarise(value = mean(value))
    }
    
    filter(dd_m, variable %in% input$var,
           time %in% seq(input$range[1],
                         input$range[2]))
    
  })
  
  
  output$plot1 <- renderPlot({
    ggplot(selectedData(), aes(x = time, y = value, color = variable)) +
      geom_line(size = 2, alpha = 0.5) + 
      geom_point(size = 3) +
      theme_bw()+
      theme(text = element_text(size = 18),legend.position = 'bottom')+
      xlab("Year")+
      ylab("Dollar per Gallon")+
      ggtitle("Fuel Price Time Series Curve")
  })
  
  
  output$plot2 <- renderPlot({
    ggplot(selectedData(), aes(x=value, color = variable)) +
      geom_density(size = 1, alpha = 0.5) +
      theme_bw()+
      theme(text = element_text(size = 18),legend.position = 'bottom')+
      xlab("Fuel Price")+
      ylab("Density")+
      ggtitle("Fuel Price Density Curve")
  })
  
  output$table <- renderDataTable({avg})
  
  
})
