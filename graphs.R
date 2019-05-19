library(ggplot2)
library(tibble)

##-- Plots

plot.plain <- function(values,ss,title="",subtitle="",ylbl="", col = "#00ba38",caption=NULL){
  
  theme_set(theme_bw())
  tbl <- tibble(y = values, x =  1:length(values))
  
  ggplot(tbl, aes(x=x)) + 
    geom_line(aes(y=y),col=col) +
    geom_line(aes(y=ss),linetype="dashed") +
    labs(title=title, 
         subtitle=subtitle, 
         caption=caption, 
         y=ylbl,
         x="Time") +
    theme(panel.grid.minor = element_blank()) 
}

plot.series <- function(series,title="",subtitle="",ylbl="", col = "#4169e1",caption=NULL){
  
  theme_set(theme_bw())
  
  dates <- as.Date(as.character(start(series)[1]:end(series)[1]),format="%Y")
  lbls <- lubridate::year(dates)
  tbl <- tibble(y = as.vector(series), x = dates)
  
  ggplot(tbl, aes(x=x)) + 
    geom_line(aes(y=y),col=col) +
    labs(title=title, 
         subtitle=subtitle, 
         caption=caption, 
         y=ylbl,
         x="Time") +
    scale_x_date(labels = lbls, breaks = dates) +
    theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 90, vjust=0.5)) 
}

plot.compared <- function(vals1,vals2, ss1, ss2, name1 = "", name2 = "", title="", subtitle="",ylbl="", cols = c("#00ba38","#4169e1"),caption=""){
  
  theme_set(theme_bw())
  tbl <- tibble(y1 = vals1, y2 = vals2, x =  1:length(vals1))
  
  ggplot(tbl, aes(x=x)) + 
    geom_line(aes(y=y1, col = "col1")) +
    geom_line(aes(y=ss1),linetype="dashed") +
    geom_line(aes(y=y2, col = "col2")) +
    geom_line(aes(y=ss2),linetype="dashed") +
    labs(title=title, 
         subtitle=subtitle, 
         caption=caption, 
         y=ylbl,
         x="Time") +
    scale_color_manual(name="", labels = c(name1,name2),
                       values = c("col1"=cols[1], "col2"=cols[2])) +
    theme(panel.grid.minor = element_blank()) 
}

plot.real <- function(vals1,vals2, title="", subtitle="",ylbl="", cols = c("#00ba38","#4169e1"),caption=""){
  
  theme_set(theme_bw())
  tbl <- tibble(y1 = vals1, y2 = vals2, x =  1982:(1981+length(vals1)))
  
  ggplot(tbl, aes(x=x)) + 
    geom_line(aes(y=y1, col = "col1")) +
    geom_point(aes(y=y2, col = "col2"), size = 0.3) +
    labs(title=title, 
         subtitle=subtitle, 
         caption=caption, 
         y=ylbl,
         x="Time") +
    scale_color_manual(name="", labels = c("Simulated","Real"),
                       values = c("col1"=cols[1], "col2"=cols[2])) +
    theme(panel.grid.minor = element_blank(),axis.text.x = element_text(angle = 45, vjust=0.5))
}

