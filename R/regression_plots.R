plot_regression <- function(model){
  cowplots <- list()

  p1<-ggplot2::ggplot(model, aes(.fitted, .resid))+ggplot2::geom_point()
  p1<-p1+ggplot2::stat_smooth(formula = y ~ x, method="loess", se = FALSE)+ggplot2::geom_hline(yintercept=0, col="red", linetype="dashed")
  p1<-p1+ggplot2::xlab("Fitted values")+ylab("Residuals")
  p1<-p1+ggplot2::labs(title = "Residual vs Fitted Plot")
  cowplots[["residvfitted"]] <- p1


  p2 <- ggplot2::ggplot(model, aes(sample = .stdresid))+ ggplot2::stat_qq() + ggplot2::stat_qq_line()+
    ggplot2::xlab("Theoretical Quantiles")+
    ggplot2::ylab("Standardized Residuals")
  p2<-p2+ggplot2::ggtitle("Normal Q-Q")
  cowplots[["qq"]] <- p2


  p3<-ggplot2::ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+ggplot2::geom_point(na.rm=TRUE)
  p3<-p3+ggplot2::stat_smooth(formula = y ~ x, method="loess", na.rm = TRUE, se = FALSE)+ggplot2::xlab("Fitted Value")
  p3<-p3+ggplot2::ylab(expression(sqrt("|Standardized residuals|")))
  p3<-p3+ggplot2::ggtitle("Scale-Location")
  cowplots[["scaleloc"]] <- p3


  p5<-ggplot2::ggplot(model, aes(.hat, .stdresid))+ggplot2::geom_point(aes(size=.cooksd), na.rm=TRUE)
  p5<-p5+ggplot2::stat_smooth(formula = y ~ x, method="loess", na.rm=TRUE, se = FALSE)
  p5<-p5+ggplot2::xlab("Leverage")+ylab("Standardized Residuals")
  p5<-p5+ggplot2::ggtitle("Residual vs Leverage Plot")
  p5<-p5+ggplot2::labs(size = "")
  p5<-p5+ggplot2::theme(legend.position="none")
  cowplots[["residvleverage"]] <- p5

  return(cowplots)
}


plot_regression_bw <- function(model){
  cowplots <- list()

  p1<-ggplot2::ggplot(model, aes(.fitted, .resid))+ggplot2::geom_point()
  p1<-p1+ggplot2::stat_smooth(formula = y ~ x, method="loess", se = FALSE)+ggplot2::geom_hline(yintercept=0, col="red", linetype="dashed")
  p1<-p1+ggplot2::xlab("Fitted values")+ylab("Residuals")
  p1<-p1+ggplot2::labs(title = "Residual vs Fitted Plot")
  p1 <- p1 +ggplot2::scale_color_grey(start = 0, end = .8) +
    ggplot2::scale_fill_grey(start = 0, end = .8)+
    theme_pub_bw1()
  cowplots[["residvfitted"]] <- p1


  p2 <- ggplot2::ggplot(model, aes(sample = .stdresid))+ ggplot2::stat_qq() + ggplot2::stat_qq_line()+
    ggplot2::xlab("Theoretical Quantiles")+
    ggplot2::ylab("Standardized Residuals")
  p2<-p2+ggplot2::ggtitle("Normal Q-Q")
  p2 <- p2 +ggplot2::scale_color_grey(start = 0, end = .8) +
    ggplot2::scale_fill_grey(start = 0, end = .8)+
    theme_pub_bw1()
  cowplots[["qq"]] <- p2


  p3<-ggplot2::ggplot(model, aes(.fitted, sqrt(abs(.stdresid))))+ggplot2::geom_point(na.rm=TRUE)
  p3<-p3+ggplot2::stat_smooth(formula = y ~ x, method="loess", na.rm = TRUE, se = FALSE)+ggplot2::xlab("Fitted Value")
  p3<-p3+ggplot2::ylab(expression(sqrt("|Standardized residuals|")))
  p3<-p3+ggplot2::ggtitle("Scale-Location")
  p3 <- p3 +ggplot2::scale_color_grey(start = 0, end = .8) +
    ggplot2::scale_fill_grey(start = 0, end = .8)+
    theme_pub_bw1()
  cowplots[["scaleloc"]] <- p3


  p5<-ggplot2::ggplot(model, aes(.hat, .stdresid))+ggplot2::geom_point(aes(size=.cooksd), na.rm=TRUE)
  p5<-p5+ggplot2::stat_smooth(formula = y ~ x, method="loess", na.rm=TRUE, se = FALSE)
  p5<-p5+ggplot2::xlab("Leverage")+ylab("Standardized Residuals")
  p5<-p5+ggplot2::ggtitle("Residual vs Leverage Plot")
  p5<-p5+ggplot2::labs(size = "")
  p5 <- p5 + theme_pub_bw1()
  p5<-p5+ggplot2::theme(legend.position="none")
  cowplots[["residvleverage"]] <- p5

  return(cowplots)
}

