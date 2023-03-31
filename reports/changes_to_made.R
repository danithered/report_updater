#should restyle in output_graph
## 1 time_A
## 2 time_everything
## 9? maps - at least for the last time - or frames -> animation??? - it could be a long df
## 10 - properties (no plotly - only animate it properly if i do the lonlong df)
## 11 - kompl_plot (zooming would be nice)


library(data.table)
library(plotly)
library(magrittr)

# Fake data
data <- as.data.table(data.frame(x = c(1, 2, 3), 
                   y = c(1000, 10000, 100000),
                   y2 = c(5000, 10000, 90000)))

# Initial plot with two traces, one off

fig <- ggplotly(ggplot(iris) + geom_point(aes(x=Sepal.Length, y=Sepal.Width)))

fig <- plot_ly(data) %>% 
  add_trace(x = ~x, y = ~y, type = 'scatter', mode = 'lines', name = 'trace1') %>%
  add_trace(x = ~x, y = ~y2, type = 'scatter', mode = 'lines', name = 'trace2', visible = F)

# Update plot using updatemenus, keep linear as first active, with first trace; second trace for log
fig %>% layout(title = 'myplot',
                      updatemenus = list(#list(
                        #active = 0,
                        visible=T,
                        type="buttons",
                        buttons= list(
                          list(label = 'log x',
                               method = 'update',
                               args = list(
                                 list(visible = c(T,T)), 
                                 list(yaxis = list(type = 'linear'), xaxis = list(type = 'linear'))
                                 ),
                               args2 = list(
                                 list(visible = c(T,T)), 
                                 list(yaxis = list(type = 'linear'), xaxis = list(type = 'linear'))
                               )
                          #)
                        ))))



add_log <- function(x) {
  layout(x, #title = 'myplot',
            updatemenus = list(list(
                active = 0,
                direction="right",
                #type="buttons",
                buttons= list(
                  list(label = 'linear',
                       method = 'update',
                       args = list(
                         list(visible = c(T,F)), 
                         list(yaxis = list(type = 'linear'), xaxis = list(type = 'linear'))
                         )
                       ),
                  list(label = 'log-log',
                       method = 'update', 
                       args = list(list(visible = c(T,F)), list(yaxis = list(type = 'log'), xaxis = list(type = 'log')))), 
                  list(label = 'log y',
                       method = 'update', 
                       args = list(list(visible = c(T,F)), list(yaxis = list(type = 'log'), xaxis = list(type = 'linear')))), 
                  list(label = 'log x',
                       method = 'update', 
                       args = list(list(visible = c(T,F)), list(xaxis = list(type = 'log'), yaxis = list(type = 'linear')))) 
                )
            ))
  ) |> config(displaylogo = FALSE)
}

fig2 |> config(displaylogo = FALSE)

fig |> add_log()

add_log <- function(x) layout(x, title = 'myplot',
       updatemenus = list(list(
         active = 1,
         direction="right",
         #type="buttons",
         buttons= list(
           list(label = 'linear',
                method = 'update',
                args = list(
                  list(visible = c(T,F)), 
                  list(yaxis = list(type = 'linear'))
                )
           ),
           list(label = 'log',
                method = 'update', 
                args = list(list(visible = c(T,F)), list(yaxis = list(type = 'log'))))))))


fig |> add_log()

plot_ly 
add_heatmap(p)
add_contour(p)
add_surface(p)

p <- plot_ly(x = rep(1:300, each=300), y = rep(1:300, 300), z = st$M)





ggplotly(ggplot(data.frame(x= rep(1:300, each=300), y=rep(1:300, 300), fill = st$M), aes(x=x,y=y, fill=fill))+
  geom_raster()+
  scale_fill_gradientn(colours = heat.colors(200)[c(185, 1)] ) +
  theme_map()
)




library(XML)
fileName= "/home/danielred/data/programs/serialisation_test/demo.xml"

get.xml <- function(fileName) {
  lines   <- readLines(fileName)
  start   <- grep('<?xml version=',lines,fixed=T)
  #end     <- c(start[-1]-1,length(lines))
  txt <- paste(lines[start[2]:length(lines)],collapse="\n")
  xmlTreeParse(txt,asText=T)
}

doc <- get.xml(fileName)
xmlToList(doc)
