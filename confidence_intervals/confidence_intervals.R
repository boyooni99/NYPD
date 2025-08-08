# percentage 
percentage=c(0.05, 0.1, 0.15, 0.25, 0.5)
# Sample size: 
sample= c(33518, 10056, 23462)

# Function to calculate 95% CI
ci= function(p, n){
uci= p + 1.96*sqrt(p*(1-p)/n)
lci= p - 1.96*sqrt(p*(1-p)/n)

return(c(n, p, lci, uci))                   
}

# apply

list= c()
for (n in sample){
for (p in percentage){
  if (n==sample[[1]] & p==percentage[[1]]){ list= ci(p, n)} 
  else{
  list= rbind(list, ci(p, n))
  }
  }
}
list_percentage = list[,2:4]*100
grp <- rep(c("Older adult overall\n(N=33,518)", "Frail older adult\n(N=10,056)", "Non-frail older adult\n(N=23,462)"), each = 5)   # three groups × 5 cut-offs

df <- as.data.frame(list_percentage) |>
  setNames(c("point", "lb", "ub")) |>
  mutate(
    group  = factor(grp, levels = c("Older adult overall\n(N=33,518)", 
                                    "Frail older adult\n(N=10,056)", 
                                    "Non-frail older adult\n(N=23,462)")),
    cutoff = factor(point, levels = c(5, 10, 15, 25, 50))  # keeps order
  )
df

library(ggplot2)
library(scales)

ggplot(df, aes(x = group, y = point)) +
  geom_errorbar(aes(ymin = lb, ymax = ub),
                width = 0.05, size = .3) +          # vertical CI 
  geom_point(size = .5) +  
  geom_text(
    aes(label = sprintf("%.0f%% (%.1f%%, %.1f%%)", point, lb, ub)),
    hjust = 0.2,                       # left-align text
    nudge_x = 0.21,                  # move a bit right of the bar
    size   = 3.5                     # adjust font size to taste
  ) + 
  scale_y_continuous(
    breaks = c(5, 10, 15, 25, 50),
    labels = function(y) paste0(y, "%")) + # y-axis shows the cut-offs
    coord_cartesian(clip = "off") +  # let text extend outside panel
    scale_x_discrete(expand = expansion(mult = c(.2, .5))) +
    labs(x = NULL, y = "Proportion of treatment use") +
    theme_minimal(base_size = 15) +
    theme(
      axis.text.x  = element_text(face = "bold", size = 10, lineheight = .9),
      axis.line.x  = element_line(colour = "black"),
      axis.line.y  = element_line(colour = "black"),
      axis.ticks   = element_line(colour = "black"),
      panel.grid.major = element_blank(),   # strip out grey grid & background
      panel.grid.minor = element_blank(),
      panel.border     = element_blank(),
      plot.margin = margin(t = 10, r = 70, b = 22, l = 5.5)  # extra margin
    )




