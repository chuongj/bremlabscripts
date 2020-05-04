# logistic curve fitting of Carly's flask data, 9/17/17
# https://datascienceplus.com/first-steps-with-non-linear-regression-in-r/

# function that does the guts of the fit
do_fit <- function(ODs, times) {

  # estimate best initial starting parameters for logistic fit
  SS<-getInitial(ODs~SSlogis(times,alpha,xmid,scale),
    data=data.frame(ODs=ODs,times=times))

  K_start<-SS["alpha"]
  R_start<-1/SS["scale"]
  N0_start<-SS["alpha"]/(exp(SS["xmid"]/SS["scale"])+1)

  #the formula for the model
  log_formula<-formula(ODs~K*N0*exp(R*times)/(K+N0*(exp(R*times)-1)))
  #fit the model
  m<-nls(log_formula,start=list(K=K_start,R=R_start,N0=N0_start))

  return(m)
}

make_plot <- function(ODs, times, my_hex_color) {
  m <- do_fit(ODs, times)
  plot(times, ODs, col=my_hex_color, ylim=c(0, y_max), ylab="", xlab="", yaxt="n", xaxt="n", pch=16, bty="n")
  lines(times,predict(m),col=my_hex_color,lty=1,lwd=3, ylim=c(0, y_max), ylab="", xlab="", yaxt="n", xaxt="n", bty="n")
}

# 7:25, 10, 11:51, 2:01, 3:42, 5:52, 7:50, 10:35)
times <- c(0, 155, 266, 396, 497, 627, 745, 910)	# in minutes

# open for reading
pdf("temp.pdf")

flask_1_ODs <- c(0.066, 0.197, 0.286, 0.424, 0.648, 1.008, 1.8, 2.96)
flask_4_ODs <- c(0.0066, 0.346, 0.954, 2.568, 3.148, 3.6, 3.65, 3.62)
flask_5_ODs <- c(0.067, 0.208, 0.392, 0.735, 1.114, 1.87, 2.71, 2.64)
flask_7_ODs <- c(0.064, 0.19, 0.222, 0.279, 0.295, 0.3, 0.298, 0.274)
flask_9_ODs <- c(0.07, 0.186, 0.247, 0.321, 0.318, 0.321, 0.321, 0.221)
flask_11_ODs <- c(0.067, 0.19, 0.237, 0.289, 0.299, 0.291, 0.284, 0.262)
y_max <- max(c(flask_1_ODs, flask_4_ODs, flask_5_ODs, flask_7_ODs, flask_9_ODs, flask_11_ODs))

make_plot(flask_1_ODs, times, "#00A5CD")
par(new=T)
make_plot(flask_4_ODs, times, "#2834ED")
par(new=T)
make_plot(flask_5_ODs, times, "#000080")
par(new=T)
make_plot(flask_7_ODs, times, "#FAB900")
par(new=T)
make_plot(flask_9_ODs, times, "#FF6600")
par(new=T)
make_plot(flask_11_ODs, times, "#CC0000")
axis(2, at=c(0, 3.5), labels=c("0","3.5"))
axis(1, at=c(0, 900), labels=c("0","15"))

dev.off()
