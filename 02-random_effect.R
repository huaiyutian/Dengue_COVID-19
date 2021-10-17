# visualise random effects for selected model
load("01.output/1.history_model/basemodel.RData")
basemodel <- model

# explore spatial and temporal random effects 
## plot monthly random effects per state (Appendix Fig S8)
load("01.output/1.history_model/model_2.6.RData")
month_effects <- data.table(cbind(rep(unique(data$country_index), each = 12),
                                  model$summary.random$T1))
names(month_effects)[1:2] <- c("country_index", "Month")
write.csv(month_effects,file = "02.fig/Fig_S8/Fig S8_month_effectsi.csv",row.names = F)

#########################################################################################################################################
## make maps of spatial random effects per year (Appendix Fig S9)
# extract posterior mean estimates for combined unstructured and structured random effects
n_country <- length(unique(data$country_index))
nyear <- length(unique(data$year))

space <- data.table(model$summary.random$S)
space$year <- rep(min(data$year):max(data$year), each = 2*n_country)
space$re <- rep(c(rep(1,n_country),rep(2,n_country)),nyear)
space <- space[space$re == 1,]
space$country_index <- rep(unique(data$country_index), nyear)
mn <-min(space$mean)
mx <-max(space$mean)

write.csv(space,file = "02.fig/Fig_S9/Fig S9_spatial random effects per yeari.csv",row.names = F)

# Add the map geometry to the space dataframe
space <- left_join(map, space, by = c("c_index" = "country_index"))
space_effects <- ggplot() + 
  geom_sf(data = space, aes(fill = mean), lwd = 0, color = NA) +
  scale_fill_distiller(palette = "RdBu", direction = -1, 
                       limits = c(min(mn,-mx),max(mx,-mn))) +
  labs(fill = "Contribution to \n log(DIR)") +
  theme_void() +
  facet_wrap(~space$year, ncol = 5)

ggsave("02.fig/Fig_S9/Fig S9_year_spatial_effect.pdf", height = 20, width = 30, units = "cm")

#########################################################################################################################################
# compare selected model to the baseline model using the mean absolute error (Appendix Fig S10)
# add baseline fitted model result summaries (2.5%, 50%, 97.5% percentiles) to data
data$base.fit <- basemodel$summary.fitted.values$`0.5quant`
data$base.fit.lci <- basemodel$summary.fitted.values$`0.025quant`
data$base.fit.uci <- basemodel$summary.fitted.values$`0.975quant`

# add selected fitted model result summaries (2.5%, 50%, 97.5% percentiles) to data
data$fit <- model$summary.fitted.values$`0.5quant`
data$fit.lci<-model$summary.fitted.values$`0.025quant`
data$fit.uci<-model$summary.fitted.values$`0.975quant`

# compute mean absolute error and compare base model to final model
MAE <- as.data.frame(matrix(NA, nrow = n_country, ncol = 2))
names(MAE) <-c("base", "new")

# calculate the MAE for observed and mean fit cases
for (i in 1:n_country)
{
  
  # cases
  MAE$base[i] <- hydroGOF::mae(data$base.fit[data$country_index == i], 
                               data$dengue_case[data$country_index == i], 
                               na.rm = TRUE)
  MAE$new[i] <- hydroGOF::mae(data$fit[data$country_index == i], 
                              data$dengue_case[data$country_index == i], 
                              na.rm = TRUE)
  
}

# calculate difference between MAE from the baseline model and MAE from the selected model
MAE$diff <- MAE$base - MAE$new
mn <-min(MAE$diff)
mx <-max(MAE$diff)
write.csv(MAE,file = "02.fig/Fig_S10/Fig S10 MAEi.csv",row.names = F)

MAE$value <- 1
# Specify the region where the difference is greater than or equal to 0 as '2', that is, the MAE of the new model is smaller (better than that of the base model)
MAE$value[MAE$diff >= 0 ] <- 2

# plot map to show areas where the new model provided 'added value' over the basemodel (e.g. MAE is smaller for new model) (Appendix Fig S1)
value_map <- ggplot(map) + 
  geom_sf(aes(fill = factor(MAE$value)), lwd = 0) +
  scale_fill_manual(values = c("#17becf","#dc5fbd"), breaks = 1:2, 
                    labels = c("No added value", "Added value")) +
  labs(fill = "") +
  theme_void()

ggsave(value_map, filename = "02.fig/Fig_S10/fig_S10_value_map.pdf")
