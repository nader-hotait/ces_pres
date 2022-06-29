require(rtweet)

df <- get_timelines(c("islam_realitaet", "genislam1", "MInteraktiv"), n = 3200)

saveRDS(df, "twitter_df.RDS")

graph <- network_graph(df)
