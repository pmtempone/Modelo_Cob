#

library(arules)
library(funModeling)
library(arulesViz)
library(visNetwork)
library(igraph)

status_train <- df_status(train_barrido)

ar_df <- train[,c("NombreLinea","sexo","prov_nombre","quincena","target","nro_dia_habil","Tasa")]

ar_df$target <- factor(ar_df$target)
ar_df$nro_dia_habil <- factor(ar_df$nro_dia_habil)
ar_df$Tasa <- factor(ar_df$Tasa)



apriori_df <- apriori(ar_df,parameter = list(support=0.00001))
summary(apriori_df)

inspect(apriori_df)
inspect(head(apriori_df, n = 25, by = "lift"))

inspect(subset(apriori_df,rhs %in% 'target=1'))

#visualizacion

subrules2 <- head(subset(apriori_df,rhs %in% 'target=1'), by = "lift",n=30)#head(sort(apriori_df, by="lift"), 25)
ig <- plot( subrules2, method="graph", control=list(type="items") )


# saveAsGraph seems to render bad DOT for this case
tf <- tempfile( )
saveAsGraph( subrules2, file = tf, format = "dot" )
# clean up temp file if desired
#unlink(tf)

# let's bypass saveAsGraph and just use our igraph
ig_df <- get.data.frame( ig, what = "both" )
visNetwork(
  nodes = data.frame(
    id = ig_df$vertices$name
    ,value = ig_df$vertices$support # could change to lift or confidence
    ,title = ifelse(ig_df$vertices$label == "",ig_df$vertices$name, ig_df$vertices$label)
    ,ig_df$vertices
  )
  , edges = ig_df$edges
) %>%
  visEdges( smooth = FALSE ) %>%
  visOptions( highlightNearest = T )
