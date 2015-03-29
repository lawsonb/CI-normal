library(shiny)


shinyServer(function(input, output) {
      
  #
  # create samples
  #
  # creates t = input$trials, each of size n = input$size samples
  # from the parameters specified by the user 
  # input$mean is the specified mean and input$var is the specified variance
  #
  # output is an array of t rows and 3 columns 
  # each row is one trial
  #      the 1st column is the sample mean
  #      the 2nd column is the sample variance
  #      the 3rd column is used to calculate CI for variance when mean is known
  #
  trials = reactive({ 
    a = input$action # causes refresh of all plots (a not used)
    t( sapply( 1:input$trials, function(k) {
       sample = rnorm(input$size, input$mean, sqrt(input$var)) 
       c(mean(sample), var(sample), sum((sample - input$mean)^2))
    }) )
  })
  
  #
  # convert the randomly generated means and variances to 
  # confidence intervals for means (for plotting)
  #
  #
  # functions to create confidence intervals for mean
  # 
  # if both mean and variance are unknown, then use t-distribution
  # to create confidence interval for the mean
  #
  both.unknown = function(cr, n) qt( (cr + 1)/2, n-1 ) / sqrt(n)
  #
  # if the variance is known and the mean is unknown then 
  # use the normal distribution to create confidence interal for the mean
  #
  mean.unknown = function(cr, n) qnorm( (cr + 1)/2 ) / sqrt(n)
  #
  #
  # function below outputs an array that has t rows (1 for each trial) and 10 columns
  #
  # columns 1-4 are for mean and variance unknown
  # the first 2 columns are lower and upper bound of interval
  # the 3rd column is the length of the interval
  # the 4th column is 1 if the interval contains the specified mean (blue)
  #                   2 if it does not contain the specified mean (red)
  # columns 5-8 are the same thing for variance known, mean unknown
  # column 9 is the sample variance
  # column 10 is the sample mean
  #
  make.interval.mean = function(trials, cr, n, sd, m) {
    
    half.length1 = both.unknown(cr, n) * sqrt(trials[,2])
    lower1 = trials[,1] - half.length1
    upper1 = trials[,1] + half.length1
    cover1 = 2L - as.integer(lower1 <= m & m <= upper1)
    
    half.length2 = mean.unknown(cr, n) * sd 
    lower2 = trials[,1] - half.length2
    upper2 = trials[,1] + half.length2
    cover2 = 2L - as.integer(lower2 <= m & m <= upper2)

    length1 = upper1 - lower1
    length2 = upper2 - lower2
    s.var = trials[,2]
    s.mean = trials[,1]
    data.frame(lower1, upper1, length1, cover1, lower2, upper2, length2, cover2, 
               s.var, s.mean)
   }
  
  interval.mean = reactive({ make.interval.mean( trials(), 
      input$cr, input$size, sqrt(input$var), input$mean ) })

  #
  # functions for confidence interal for variance
  #   
  #   this function searches for shortest CI for given coverage rate and df
  # 
  grid.min.chisq = function(cr, df) {
    inc = 0.0001 # increment for search 
    bpinit = (1 + cr) / 2 # begin at (1 + cr) / 2
    m = (1 - bpinit)/inc # search until left point is zero
    
    # determine length of interval at each increment
    len = sapply(0:m, function(i) {
      bp = bpinit - inc*i
      ap = bp - cr
      qchisq(bp, df) - qchisq(ap, df)
    })
    
    # pick out index of minium interval
    n = order(len)[1] - 1 
    bp = bpinit - inc * n
    ap = bp - cr
    # return quantiles that give minimum interval on points searched
    c(qchisq(ap, df), qchisq(bp, df))  
  }

  #
  # make intervals for variance
  #
  #   if both mean and variance unknown, use chisq_{n-1} and sum(x_i - sample(mean))^2
  #   if mean is known and variance is unknown use chisq_n and sum(x_i - mu)^2
  #
  # output is t by 10, columns analogous to make.interval.mean above
  #
  make.interval.variance = function(trials, cr, n, v, m) {
        
    qab = grid.min.chisq(cr, n - 1) 
    lower1 = (n - 1) * trials[,2] / qab[2] 
    upper1 = (n - 1) * trials[,2] / qab[1] 
    cover1 = 2L - as.integer(lower1 <= v & v <= upper1)
    
    qab = grid.min.chisq(cr, n) 
    lower2 = trials[,3] / qab[2] 
    upper2 = trials[,3] / qab[1] 
    cover2 = 2L - as.integer(lower2 <= v & v <= upper2)
    
    length1 = upper1 - lower1
    length2 = upper2 - lower2
    s.var = trials[,2]
    s.mean = trials[,1]
    data.frame(lower1, upper1, length1, cover1, lower2, upper2, length2, cover2, 
               s.var, s.mean)
  }
  
  interval.variance = reactive({ make.interval.variance( trials(), 
    input$cr, input$size, input$var, input$mean ) })
  
  #
  # creates 2 plots, the first is the confidence intervals 
  # the second is a summary of their lengths
  #
  make.plots = function(type.unknown, mv) {
    #
    # blue for intervals containing parameter (mean or variance), 
    # red for intervals that do not
    infocol = c("blue", "red") 
    cr = input$cr
    t = input$trials
    n = input$size
    m = input$mean
    v = input$var
    sd = sqrt(v)
    
    # set options unique to different types of plots    
    if (mv == 1) {
      title = paste("Mean Unknown, Variance", v)
      vline = m
      # set range of x-axis for plots so comparable across options
      xrange = m + c(-1,1)*sd*1.1
      interval = interval.mean() 
      xrangeLen = c(0, 2*interval[1,7])
      # yrangeLen = c(0, sqrt(n)/v)
    } else {
      title = paste("Variance Unknown, Mean", m)      
      vline = v
      # set range of x-axis for plots so comparable across options
      xrange = c(0, 6*v)
      interval = interval.variance()
      xrangeLen = c(0, 5*v)
      # yrangeLen = c(0, 1)
    }
    if (type.unknown == "both") {
      icol = 0
      title = paste("Mean and Variance Unknown")
    } else icol = 4
    
    par(mfrow=c(1,2))
    # draw empty plot area
    notcover = t - sum(interval[, 4 + icol] - 1)
    subtitle1 = paste0(notcover, " (",round(notcover/t,3),") CI include the")
    if (mv == 1) subtitle2 = paste("mean of", m) 
    else subtitle2 = paste("variance of",v)
    plot(0,0, type = "n", 
         xlim = xrange, xlab = paste(subtitle1, subtitle2), 
         ylim = c(t,1), # this reverses ordering on y-axis so matches table 
         ylab = "Trial",
         main = title )
    mtext( paste("Coverage",cr,"  ",t, "trials; each trial has",n,"samples") )
    # plot the trials, going from 1 at top to t at bottom
    for (k in 1:t) lines( interval[k, c(1:2) + icol], c(k, k),
                           col = infocol[ interval[k, 4 + icol] ] )
    abline(v = vline, col = "green")
    
    #
    # 2nd plot: lengths of intervals
    #
    # display both lengths on one plot 
    # solid for lengths associated with 1st plot
    # dashed for lengths associated with plot not shown
    #
    if (type.unknown == "both") linetype = 1:2 else linetype = 2:1
    if (mv == 1) denplot = density(interval[,3]) else denplot = density(interval[,7])
    plot(denplot, main="Interval Lengths",
      # xlim = xrangeLen, 
      xlab = "interval length", 
      # ylim = yrangeLen, 
      lty = linetype
    )
    if (mv == 1) {
      abline( v = interval[1,7], lty = linetype[2] )
      pos = "topleft"
    } else {
      lines(density(interval[,3]), lty = linetype[2])
      pos = "topright"
    }
    legend(pos, c("both unknown", "one unknown"), lty = linetype, cex = 0.75)
  }
  
  #
  # radio button
  # determines whether displaying confidence intervals for mean or variance
  # mv = 1 is mean, mv = 2 is variance
  #
  mv = reactive({ input$radio })
  #
  # create plots and display data for each tab
  #
  output$both.unknown.Plot <- renderPlot( make.plots("both", mv() ) )
  output$one.unknown.Plot  <- renderPlot( make.plots("one",  mv() ) )
  output$interval = renderTable({ if (mv()==1) interval.mean() else interval.variance() })
})
