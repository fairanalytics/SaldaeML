SA_ml_sekned_AUC <- function(tisefka_roc = NULL){
  
  
  tisefka_roc %>% plot_ly( y = ~Hits, x = ~Falsealarm, hoverinfo = "none") %>% 
    
    add_lines(name = "Model",
              line = list(shape = "spline", color = "#737373", width = 7), 
              fill = "tozeroy", fillcolor = "#2A3356") %>% 
    
    add_annotations(y = labels$y, x = labels$x, text = labels$text,
                    ax = 20, ay = 20,
                    arrowcolor = "white",
                    arrowhead = 3,
                    font = list(color = "white")) %>% 
    
    add_segments(x = 0, y = 0, xend = 1, yend = 1, 
                 line = list(dash = "7px", color = "#F35B25", width = 4), 
                 name = "Random") %>% 
    
    add_segments(x = 0, y = 0, xend = 0, yend = 1, 
                 line = list(dash = "10px", color = "black", width = 4), 
                 showlegend = F) %>%
    
    add_segments(x = 0, y = 1, xend = 1, yend = 1, 
                 line = list(dash = "10px", color = "black", width = 4), 
                 showlegend = F) %>% 
    
    add_annotations(x = 0.8, y = 0.2, showarrow = F, 
                    text = paste0("Area Under Curve: ", AUC),
                    font = list(family = "serif", size = 18, color = "#E8E2E2")) %>%
    
    add_annotations(x = 0, y = 1, showarrow = F, xanchor = "left", 
                    xref = "paper", yref = "paper",
                    text = paste0("Receiver Operator Curve"),
                    font = list(family = "arial", size = 30, color = "#595959")) %>%
    
    add_annotations(x = 0, y = 0.95, showarrow = F, xanchor = "left", 
                    xref = "paper", yref = "paper",
                    text = paste0("Charts the percentage of correctly identified defaults (hits) against the percentage of non defaults incorrectly identifed as defaults (false alarms)"),
                    font = list(family = "serif", size = 14, color = "#999999")) %>% 
    
    
    layout(xaxis = list(range = c(0,1), zeroline = F, showgrid = F,
                        title = "Number of False Alarms"),
           yaxis = list(range = c(0,1), zeroline = F, showgrid = F,
                        domain = c(0, 0.9),
                        title = "Number of Hits"),
           plot_bgcolor = "#E8E2E2",
           height = 800, width = 1024)
}