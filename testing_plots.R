library(tidyverse)
library(lubridate)
library(oce)
library(gsw)
library(readxl)
library(ggrepel)
library(scales)
library(ggpubr)
library(cowplot)

budget_data <- read_excel("BudgetData2020.xlsx")

### --- SPENDING WITH DATE PARTITIONED INTO YEAR | MONTH | DAY COLUMNS --- ### 

spending_data_factored <- budget_data %>%
  select(Date, Cost, Category, Details, `Establishment | Service`, Occasion, Comments, Type) %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-") %>%
  mutate(Month = gsub("01", "January", Month),
         Month = gsub("02", "February", Month),
         Month = gsub("03", "March", Month), 
         Month = gsub("04", "April", Month),
         Month = gsub("05", "May", Month),
         Month = gsub("06", "June", Month),
         Month = gsub("07", "July", Month),
         Month = gsub("08", "August", Month),
         Month = gsub("09", "September", Month),
         Month = gsub("10", "October", Month),
         Month = gsub("11", "November", Month),
         Month = gsub("12", "December", Month)) 

spending_data_factored$Month <- factor(spending_data_factored$Month, ordered = T, 
                         levels = c("January", "February", "March", "April",
                                    "May", "June", "July", "August",
                                    "September", "October", "November", "December"))

### --- SPENDING USING LUBRIDATE DATE --- ###

spending_data <- spending_data_factored %>%
  filter(!is.na(Cost)) %>%
  group_by(Day, Year, Month) %>%
  unite(Date, Day, Month, Year, sep = "-") %>%
  ungroup() %>%
  mutate(Date = dmy(Date)) %>%
  arrange(Date)



glimpse(spending_data_factored)

polar_barplot_data <- spending_data_factored %>%
  group_by(Category) %>%
  summarize(Cost = sum(Cost)) %>% 
  arrange(Category, Cost) %>%
  ungroup() %>%
  mutate(id = row_number(), 
         angle = 90 - 360 * (id - 0.5) / id,
         hjust = ifelse(angle < -90, 1, 0),
         angle = angle < -90, 
         angle + 180, 
         angle,
         Proportion = Cost / sum(Cost)) %>%
  filter(!is.na(Category))

A <- ggplot(polar_barplot_data, aes(x = factor(Category), y = Cost, label = Category)) +
  geom_bar(aes(fill = Category), stat = "identity", position = "dodge", alpha = 0.5) +
  geom_bar(aes(y = max(Cost) + 1000, fill = Category), stat = "identity", 
           position = "dodge", alpha = 0.10) +
  geom_text(aes(x = Category, y = max(Cost) + 250, label = round(Cost, digits = 2), angle = angle, 
                colour = Category), family = "Century Gothic", hjust = 0.5, inherit.aes = F, size = 3.5) +
  geom_text(data = polar_barplot_data, aes(x = Category, y = max(Cost) / 2, label = Category, angle = angle), 
              family = "Century Gothic", hjust = 0.5, inherit.aes = F, size = 3.5, colour = "black") +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02","wheat3",
                               "#A6761D", "plum4", "steelblue", "darkred", "honeydew4"), guide = F) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02","wheat3",
                                "#A6761D", "plum4", "steelblue", "darkred", "honeydew4"), guide = F) +
  coord_polar(start = 0) +
  expand_limits(y = -(max(polar_barplot_data$Cost) / 4)) +
  ggtitle("Polar Barplot for Categorical Spending") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        strip.text.x = element_text(size = 14, face = "bold", family = "Century Gothic"),
        text = element_text(size = 14, family = "Century Gothic"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(face = "bold")) 

### --- PROPORTIONAL POLAR BARPLOT --- ###

proportional_polar_barplot_data <- polar_barplot_data %>%
  mutate(Proportion = Cost / sum(Cost))

B <- ggplot(proportional_polar_barplot_data, aes(x = reorder(factor(Category), -Proportion), y = Proportion, 
                                        label = Category, group = Category)) +
  geom_bar(aes(fill = Category), stat = "identity", position = "dodge", alpha = 0.5) +
  geom_bar(aes(y = 1, fill = Category), stat = "identity", position = "dodge", alpha = 0.10) +
  geom_text(aes(x = Category, y = 0.90, label = round(Proportion, digits = 3), angle = angle, colour = Category), 
            family = "Century Gothic", hjust = 0.5, inherit.aes = F, size = 3.5) +
  geom_text(data = proportional_polar_barplot_data, aes(x = Category, y = .5, label = Category, angle = angle),
            family = "Century Gothic", hjust = 0.5, inherit.aes = F, size = 3.5, colour = "#404040") +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02","wheat3",
                               "#A6761D", "plum4", "steelblue", "darkred", "honeydew4"), guide = F) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02","wheat3",
                                "#A6761D", "plum4", "steelblue", "darkred", "honeydew4"), guide = F) +
  coord_polar(start = 0) +
  expand_limits(y = -0.25) +
  theme_minimal() +
  ggtitle("Proprtional Spending by Category") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        strip.text.x = element_text(size = 14, face = "bold", family = "Century Gothic"),
        text = element_text(size = 14, family = "Century Gothic"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(face = "bold")) 

### --- Donut plot --- ###

## PERCENTAGES FOR EACH CATEGORY

donut_plot_data <- polar_barplot_data %>%
  dplyr::select(Category, Cost) 

# Percentages
donut_plot_data$Fraction <- donut_plot_data$Cost / sum(donut_plot_data$Cost)

# Cumulative percentages (top of each rectangle)
donut_plot_data$Y_Max <- cumsum(donut_plot_data$Fraction)

# Bottom of each rectangle
donut_plot_data$Y_Min <- c(0, head(donut_plot_data$Y_Max, n = -1))

# Label position
donut_plot_data$LabelPosition <- (donut_plot_data$Y_Max + donut_plot_data$Y_Min) / 2

# Label
donut_plot_data$AbsLabel <- paste0(donut_plot_data$Category, ":\n$", round(donut_plot_data$Cost, digits = 2))
donut_plot_data$RelLabel <- paste0(donut_plot_data$Category, ":\n", round(donut_plot_data$Fraction, digits = 2))



C <- ggplot(donut_plot_data, aes(ymax = Y_Max, ymin = Y_Min, xmax = 4, xmin = 3, fill = Category)) + 
  geom_rect(alpha = 0.4) + 
  geom_text(x = 4.25, aes(y = LabelPosition, label = AbsLabel, color = Category), size = 3, 
            family = "Century Gothic") +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02","wheat3",
                               "#A6761D", "plum4", "steelblue", "darkred", "honeydew4"), guide = F) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02","wheat3",
                               "#A6761D", "plum4", "steelblue", "darkred", "honeydew4"), guide = F) +
  coord_polar(theta = "y") + 
  xlim(c(0, 4)) +
  theme_void() + 
  theme(legend.position = "none",
        text = element_text(size = 21, family = "Century Gothic"))

D <- ggplot(donut_plot_data, aes(ymax = Y_Max, ymin = Y_Min, xmax = 4, xmin = 3, fill = Category)) + 
  geom_rect(alpha = 0.4) + 
  geom_text(x = 4.25, aes(y = LabelPosition, label = RelLabel, color = Category, size = 2), 
            family = "Century Gothic") +
  scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02","wheat3",
                               "#A6761D", "plum4", "steelblue", "darkred", "honeydew4"), guide = F) +
  scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02","wheat3",
                                "#A6761D", "plum4", "steelblue", "darkred", "honeydew4"), guide = F) +
  coord_polar(theta = "y") + 
  xlim(c(0, 4)) +
  theme_void() + 
  theme(legend.position = "none",
        strip.text.x = element_text(size = 14, face = "bold", family = "Century Gothic"),
        text = element_text(size = 14, family = "Century Gothic"))

plot_grid(A, B)

### <---> WORDCLOUD <---> ###

library(wordcloud2)
 
wordcloud_data <- budget_data %>%
  dplyr::select(Details, Comments)

details_words <- count(wordcloud_data, vars = Details) %>%
  filter(!is.na(vars))
comments_words <- count(wordcloud_data, vars = Comments)



wordcloud2(details_words, size = 0.7, shape = 'star', color = "black")

### <---> Monthly Spending <---> ###

spending_lineplot <- spending_data_factored %>%
  group_by(Year, Month, Category) %>%
  summarize(Cost = sum(Cost)) %>%
  arrange(Category, Cost) %>%
  ungroup() %>%
  filter(!is.na(Category))

monthly_spending_plot <- spending_data_factored %>%
  dplyr::select(Year, Month, Day, Cost, Category, Details) %>%
  filter(!is.na(Cost)) %>%
  ggplot(aes(x = Month, y = Cost, group = Category, colour = Category)) +
    geom_bar(aes(fill = Category), 
             stat = "identity", position = "stack", alpha = 0.400, colour = "#F5F5F5") +
    facet_grid(Category ~ Year, scales = "free") +
    geom_point(data = spending_lineplot, 
               mapping = aes(x = Month, y = Cost, group = Category, colour = Category),
               size = 3, alpha = 0.5) +
    geom_line(data = spending_lineplot, 
              mapping = aes(x = Month, y = Cost, group = Category, colour = Category),
              size = 0.25, alpha = 0.75) +
    theme_minimal() +
    theme(
      text = element_text(size = 14, family = "Century Gothic"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.text = element_text(size = 12, family = "Century Gothic", colour = "white"),
      strip.text.y = element_text(angle = 0),
      
      panel.background = element_blank(),
      panel.grid = element_blank(),
      strip.background = element_rect(fill = alpha("#B37559", 0.75), colour = "#F5F5F5")
       ) + 
    scale_fill_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02","wheat3",
                               "#A6761D", "plum4", "steelblue", "darkred", "honeydew4"), guide = F) +
    scale_color_manual(values = c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02","wheat3",
                                "#A6761D", "plum4", "steelblue", "darkred", "honeydew4"), guide = F) +
    labs(y = "Cost (CAD)")

g1 <- ggplot_gtable(ggplot_build(monthly_spending_plot))
stripr1 <- which(grepl('strip-r', g1$layout$name))
fills1 <- alpha(c("#1B9E77", "#D95F02", "#7570B3", "#E6AB02","wheat3",
                  "#A6761D", "plum4", "steelblue", "darkred", "honeydew4"), 0.5)
k1 <- 1
for (i in stripr1) {
  j1 <- which(grepl('rect', g1$grobs[[i]]$grobs[[1]]$childrenOrder))
  g1$grobs[[i]]$grobs[[1]]$children[[j1]]$gp$fill <- fills1[k1]
  k1 <- k1+1
}
plot_grid(g1)    

### <---> Details line plots <---> ###

details_lineplot <- spending_data_factored %>%
  group_by(Year, Month, Category, Details) %>%
  summarize(Cost = sum(Cost)) %>%
  arrange(Category, Cost) %>%
  ungroup() %>%
  filter(!is.na(Category))

## Food & Beverages

details_lineplot %>% 
  filter(Category == "Food & Beverages") %>%
  filter(Details != "Coffee" & Details != "Drinks") %>%
  ggplot(aes(x = Month, y = Cost, group = Details, colour = Details)) + 
  geom_point(size = 3, alpha = .75) +
  geom_line(aes(group = Details), size = 0.5, alpha = 0.35) + 
  scale_color_manual(values = c("#E37222", "#66B9BF", "#EEAA7B")) +
  theme_bw() +
  theme(text = element_text(size = 14, family = "Century Gothic"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y = element_text(colour = "white"),
        strip.background = element_rect(fill = alpha("#07889B", 0.75), 
                                        colour = "#F5F5F5")) + 
  labs(y = "Cost (CAD)") +
  facet_wrap(~ Year)

## Personal

details_lineplot %>% 
  filter(Category == "Personal") %>%
  filter(Details != "Haircut") %>%
  ggplot(aes(x = Month, y = Cost, group = Details, colour = Details)) + 
  geom_point(size = 3, alpha = .75) +
  geom_line(aes(group = Details), size = 0.5, alpha = 0.35) + 
  scale_color_manual(values = c("#565656", "#76323F", "#7D849B", "#C09F80")) +
  theme_bw() +
  theme(text = element_text(size = 14, family = "Century Gothic"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y = element_text(colour = "white"),
        strip.background = element_rect(fill = alpha("#D7CEC7", 0.75), 
                                        colour = "#F5F5F5")) + 
  labs(y = "Cost (CAD)") +
  facet_wrap(~ Year)
  

#### focusing on specific expenditures
library(DT)
# coffee

spending_data %>%
  filter(Details == "Coffee") %>%
  ggplot(aes(x = Date, y = cumsum(Cost), group = 1)) +
    geom_point(size = 4, alpha = 0.5, colour = "#85A99B") +
    geom_area(alpha = 0.1, fill = "#383838") +
    geom_line(size = 0.25, alpha = 0.5, colour = "#383838") +
    scale_x_date(date_labels = "%b-%Y", breaks = "2 month") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      text = element_text(size = 14, family = "Century Gothic"),
      strip.background = element_rect(fill = alpha("#B37559", 0.75)) 
    ) +
    labs(x = "Month", y = "Total Coffee expenditure (CAD)")


coffee_tracker <- spending_data_factored %>%
  filter(Details == "Coffee") %>%
  group_by(Year, Month) %>%
  summarize(count = n(),
            Total = sum(Cost),
            Average = Total / count)

datatable(coffee_tracker, rownames = FALSE, filter="top", options = list(pageLength = 12))


### <---> Cumulative spending <---> ###

monthly_spending_table <- spending_data_factored %>%
  group_by(Year, Month) %>%
  filter(!is.na(Cost)) %>%
  summarize(Total = sum(Cost))

datatable(monthly_spending_table, rownames = FALSE, filter="top", options = list(pageLength = 12))
###

monthly_consumption_slope <- 
  diff(quantile(monthly_spending_table$Total, c(0.25, 0.75), na.rm = T)) / 
  diff(qnorm(c(0.25, 0.75)))

monthly_consumption_slope_intercept <- 
  quantile(monthly_spending_table$Total, 0.25, na.rm = T) - 
  monthly_consumption_slope * qnorm(0.25)

QQ_Density_panel1 <- ggplot(monthly_spending_table, aes(sample = Total)) +
  stat_qq(pch = 1, colour = "black",  fill = "black", alpha = 0.35, size = 4) +
  stat_qq(pch = 19, colour = "black",  fill = "black", alpha = 0.15, size = 4) +
  geom_abline(aes(slope = monthly_consumption_slope, 
                  intercept = monthly_consumption_slope_intercept),
              size = 0.75, col = "#B37559", alpha = 0.5, lty = 1) +
  labs(y = "Aggregated monthly expenditure (CAD)", x = "Theoretical distribution",
       subtitle = "Quantile-Quantile and Density plot")  +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic", size = 14), 
        axis.text = element_text(family = "Century Gothic", size = 14)) 

library(ggridges)
QQ_Density_panel2 <- axis_canvas(QQ_Density_panel1, axis = "y") +
  geom_vridgeline(data = monthly_spending_table, aes(y = Total, x = 0, width = ..density..),
                  stat = "ydensity", alpha = 0.5, size = 0.25, trim = F, fill = "#85A99B") 

QQ_Density_plot <- insert_yaxis_grob(QQ_Density_panel1, QQ_Density_panel2, grid::unit(0.2, "null"),
                                             position = "right")

ggdraw(QQ_Density_plot)
###
cumulative_spending_data <- spending_data_factored %>%
  filter(!is.na(Cost)) %>%
  group_by(Day, Year, Month) %>%
  summarize(Cost = sum(Cost)) %>%
  unite(Date, Day, Month, Year, sep = "-") %>%
  ungroup() %>%
  mutate(Date = dmy(Date)) %>%
  arrange(Date)

library(scales)

ggplot(cumulative_spending_data, aes(x = Date, y = cumsum(Cost), group = 1)) +
  geom_point(size = 4, alpha = 0.5, colour = "#85A99B") +
  geom_area(alpha = 0.1, fill = "#383838") +
  geom_line(size = 0.25, alpha = 0.5, colour = "#383838") +
  scale_x_date(date_labels = "%b-%Y", breaks = "2 month") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 14, family = "Century Gothic"),
    strip.background = element_rect(fill = alpha("#B37559", 0.75)) 
  ) +
  labs(x = "Month", y = "Aggregate expenditure (CAD)")
         
### --- Running average aggregate spending --- ###

Date_seq <- data.frame(Date = seq(as.Date("2019/3/1"), to = as.Date(Sys.Date()), 
                                  by = "day"))

spending_data_omit_rent <- spending_data_factored %>%
  filter(!is.na(Cost) & 
        Details != "LaCasa rent" & 
        Details != "Pineview Place rent") %>%
  group_by(Day, Year, Month) %>%
  summarize(Cost = sum(Cost)) %>%
  unite(Date, Day, Month, Year, sep = "-") %>%
  ungroup() %>%
  mutate(Date = dmy(Date)) %>%
  full_join(Date_seq, by = "Date") %>%
  arrange(Date) %>%
  mutate(Cost = ifelse(is.na(Cost), 0, Cost))


cumvar <- function (x, sd = F) {
  x <- x - x[sample.int(length(x), 1)]
  n <- seq_along(x)
  v <- (cumsum(x^2) - cumsum(x)^2 / n) / (n - 1)
  if (sd) v <- sqrt(v)  
  v
}

cumulative_joined_active <- spending_data_omit_rent %>%
  mutate(cum_mean = cumsum(spending_data_omit_rent$Cost) / 
                    seq_along(spending_data_omit_rent$Cost),
         CUMMEAN = cummean(Cost),
         CUMSD = cumvar(Cost, sd = T),
         sd_upper = CUMMEAN + CUMSD,
         sd_lower = CUMMEAN - CUMSD)

average_daily_spending_month <- cumulative_joined_active %>%
  group_by(month = lubridate::floor_date(Date, "month")) %>%
  summarize(avg_month = mean(Cost)) %>%
  mutate(month = month + 15)

Average_expenditure_plot <- ggplot(cumulative_joined_active, aes(x = Date, y = Cost)) +
  geom_bar(data = spending_data_omit_rent, mapping = aes(y = Cost),
           stat = "identity", position = "stack", alpha = 0.4, fill = "#7D849B") + 
  geom_bar(data = average_daily_spending_month, 
           mapping = aes(x = month, y = avg_month),
           stat = "identity", alpha = 0.5, fill = "#85A99B") + 
  geom_errorbar(mapping = aes(ymin = sd_lower, ymax = sd_upper),
                alpha = 0.2, size = 0.5, colour = "#383838") + 
  geom_line(aes(y = CUMMEAN), colour = "#383838", alpha = 0.85, size = 0.5) +
  scale_x_date(date_labels = "%b-%Y", breaks = "1 month") +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.ticks.x = element_blank(),
    text = element_text(size = 14, family = "Century Gothic"),
    strip.background = element_rect(fill = alpha("#B37559", 0.75))
  ) +
  annotate("text", x = min(cumulative_joined_active$Date), 
           y = max(cumulative_joined_active$Cost),
           label = paste("Average expenditure per day: $",
                         round(mean(cumulative_joined_active$Cost), digits = 2)),
                         colour = "#B37559", family = "Century Gothic", size = 5,
           hjust = 0) +
  annotate("text", x = min(cumulative_joined_active$Date), 
           y = max(cumulative_joined_active$Cost) * 0.90,
           label = paste("Median expenditure per day: $",
                         round(median(cumulative_joined_active$Cost), digits = 2)),
                         colour = "#85A99B", family = "Century Gothic", size = 5,
           hjust = 0) +
  labs(y = "Expenditure (CAD)")

Moving_average_plot <- ggplot(cumulative_joined_active, aes(x = Date, y = Cost)) +
  geom_bar(data = average_daily_spending_month, 
           mapping = aes(x = month, y = avg_month),
           stat = "identity", alpha = 0.5, fill = "#85A99B") + 
  geom_line(aes(y = rollmean(Cost, 7, align = "right", fill = NA)), 
            colour = "#B37559", alpha = 0.75) +
  scale_x_date(date_labels = "%b-%Y", breaks = "1 month") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 14, family = "Century Gothic"),
    strip.background = element_rect(fill = alpha("#B37559", 0.75)),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 9)
  ) +
  labs(y = " ")

plot_grid(Average_expenditure_plot, Moving_average_plot, 
          ncol = 1, align = "v", rel_heights = c(3, 1))

### --- INCOME --- ###

income_data_factored <- budget_data %>%
  filter(!is.na(`Income (CAD)`)) %>%
  select(Date, Source, `Type of Income`, Account, `Income (CAD)`) %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-") %>%
  mutate(Month = gsub("01", "January", Month),
         Month = gsub("02", "February", Month),
         Month = gsub("03", "March", Month), 
         Month = gsub("04", "April", Month),
         Month = gsub("05", "May", Month),
         Month = gsub("06", "June", Month),
         Month = gsub("07", "July", Month),
         Month = gsub("08", "August", Month),
         Month = gsub("09", "September", Month),
         Month = gsub("10", "October", Month),
         Month = gsub("11", "November", Month),
         Month = gsub("12", "December", Month)) 
  
income_data_factored$Month <- factor(income_data_factored$Month, ordered = T, 
                               levels = c("January", "February", "March", "April",
                                          "May", "June", "July", "August",
                                          "September", "October", "November", "December"))

polar_income_barplot_data <- income_data_factored %>%
  group_by(`Type of Income`) %>%
  summarize(total = sum(`Income (CAD)`)) %>%
  arrange(`Type of Income`, total) %>%
  ungroup() %>%
  mutate(id = row_number(), 
         angle = 90 - 360 * (id - 0.5) / id,
         hjust = ifelse(angle < -90, 1, 0),
         angle = angle < -90, angle + 180, angle) %>%
  filter(!is.na(`Type of Income`))

monthly_income <- income_data_factored %>%
  group_by(Year, Month, `Type of Income`) %>%
  summarize(Income = sum(`Income (CAD)`))

polar_income_plot <- ggplot(polar_income_barplot_data, aes(x = `Type of Income`, y = total, group = total)) +
  geom_bar(aes(fill = `Type of Income`), stat = "identity", position = "dodge", alpha = 0.5) +
  geom_bar(aes(y = max(total) * 1.5, fill = `Type of Income`),
           stat = "identity", position = "dodge", alpha = 0.20) +
  geom_text(aes(x = `Type of Income`, y = max(total) * 1.25, label = round(total, digits = 2), angle = angle, 
                colour = `Type of Income`),
            family = "Century Gothic", hjust = 0.5, inherit.aes = F, size = 3.5) +
  geom_text(data = polar_income__barplot_data, aes(x = `Type of Income`, y = max(total) / 2,
                                           label = `Type of Income`, angle = angle), 
            family = "Century Gothic", hjust = 0.5, inherit.aes = F, size = 3.5, colour = "black") +
  scale_fill_manual(values = c("#E27D60", "#95CBC9", "#E8A87C", "#C38D9E","#41B3A3"), guide = F) +
  scale_color_manual(values = c("#E27D60", "#95CBC9", "#E8A87C", "#C38D9E","#41B3A3"), guide = F) +
  coord_polar(start = 0) +
  expand_limits(y = -(max(polar_barplot_data$Cost) )) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = 'bold'),
        strip.text.x = element_text(size = 14, face = "bold", family = "Century Gothic"),
        text = element_text(size = 14, family = "Century Gothic"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_text(face = "bold"),
        plot.margin = unit(c(0, 0, 0, 0), "cm")) 

categorical_income_plot <- ggplot(monthly_income, aes(x = Month, y = Income, group = `Type of Income`)) +
  geom_line(aes(colour = `Type of Income`)) +
  geom_area(aes(fill = `Type of Income`), alpha = 0.5) +
  facet_wrap(Year ~ `Type of Income`, ncol = 5, scales = "free_y") +
  theme_minimal() +
  theme(text = element_text(size = 14, family = "Century Gothic"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y = element_text(colour = "white"),
        strip.background = element_rect(fill = alpha("#07889B", 0.75), 
                                        colour = "#F5F5F5")) +
  scale_fill_manual(values = c("#E27D60", "#95CBC9", "#E8A87C", "#C38D9E","#41B3A3"), guide = F) +
  scale_color_manual(values = c("#E27D60", "#95CBC9", "#E8A87C", "#C38D9E","#41B3A3"), guide = F) +
  labs(title = "Categorical Expenditure")

aggregated_monthy_income_plot <- monthly_income %>%
  group_by(Year, Month) %>%
  summarize(Income = sum(Income)) %>%
  ggplot(aes(x = Month, y = Income)) +
  geom_point(size = 4, alpha = 0.5, colour = "#66B9BF") +
  geom_area(alpha = 0.1, fill = "#EEAA7B", group = 1) +
  geom_line(group = 1, size = 0.25, alpha = 0.5, colour = "#07889B") +
  facet_wrap(~ Year, scales = "free_y", ncol = 1) +
  theme_minimal() +
  theme(text = element_text(size = 14, family = "Century Gothic"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.y = element_text(colour = "white"),
        strip.background = element_rect(fill = alpha("#E37222", 0.75), 
                                        colour = "#F5F5F5"),
        axis.title.x = element_blank()) +
  labs(y = "Income (CAD)", title = "Aggregate Monthly Income")


ggdraw() + 
  draw_plot(aggregated_monthy_income_plot, x = 0, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(polar_income_plot, x = 0.5, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(categorical_income_plot, x = 0, y = 0, width = 1, height = 0.5) 

### --- Net income (showing both expenditure and income in a single graph) --- ###

net_financial_data <- budget_data %>%
  dplyr::select(Date, `Income (CAD)`, Cost) %>%
  separate(Date, into = c("Year", "Month", "Day"), sep = "-") %>%
  mutate(Month = gsub("01", "January", Month),
         Month = gsub("02", "February", Month),
         Month = gsub("03", "March", Month), 
         Month = gsub("04", "April", Month),
         Month = gsub("05", "May", Month),
         Month = gsub("06", "June", Month),
         Month = gsub("07", "July", Month),
         Month = gsub("08", "August", Month),
         Month = gsub("09", "September", Month),
         Month = gsub("10", "October", Month),
         Month = gsub("11", "November", Month),
         Month = gsub("12", "December", Month)) %>%
  rename(Income = `Income (CAD)`) %>%
  filter(!is.na(Day)) %>%
  mutate(Cost = Cost / -1) %>%
  group_by(Year, Month, Day) %>%
  gather(key = "Source", value = "Net", -Year, -Month, -Day) %>%
  ungroup() %>%
  group_by(Year, Month, Day, Source) %>%
  dplyr::filter(!is.na(Net)) %>%
  summarize(Net = sum(Net))

net_financial_data$Day <- as.numeric(net_financial_data$Day)
net_financial_data$Month <- factor(net_financial_data$Month, ordered = T, 
                                   levels = c("January", "February", "March", "April",
                                              "May", "June", "July", "August",
                                              "September", "October", "November", "December"))

ggplot(net_financial_data, aes(x = 1:length(Day), y = Net)) +
  geom_bar(aes(x = Day, fill = Source), stat = "identity", alpha = 0.5) +
  scale_fill_manual(values = c("#C38D9E", "#95CBC9"), labels = c("Expenditure", "Income")) +
  geom_abline(slope = 0, intercept = 0, colour = "#7D849B", alpha = 0.75, size = 0.15) +
  facet_grid(Month ~ Year, scales = "free_y") +
  theme_minimal() +
      theme(
        text = element_text(size = 14, family = "Century Gothic"),
        strip.text = element_text(size = 12, family = "Century Gothic", colour = "white"),
        strip.text.y = element_text(angle = 0),
        
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill = alpha("#07889B", 0.65), 
                                        colour = "#F5F5F5")
      ) + 
  labs(y = "Net funds (CAD)", x = "Day")

income_expenditure_cumsum <- net_financial_data %>%
  group_by(Year, Month, Day) %>%
  summarize(Net = sum(Net)) %>%
  unite(Date, Day, Month, Year, sep = "-")

income_expenditure_cumsum$Date <- dmy(income_expenditure_cumsum$Date)

income_expenditure_cumsum %>%
  arrange(Date) %>%
  ggplot(aes(x = Date, y=cumsum(Net), group = 1)) + 
  geom_point(size = 4, alpha = 0.5, colour = "#66B9BF") +
  geom_line(group = 1, size = 0.25, alpha = 0.5, colour = "#07889B") +
  scale_x_date(date_labels = "%b-%Y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        plot.title = element_text(hjust = 0.5, size = 16),
        text = element_text(size = 14, family = "Century Gothic"),
        panel.background = element_blank(),
        panel.grid.minor = element_blank()
        ) +
  labs(y = "Tracking net funds (CAD)", x = "Date")

### --- Recurring Payments --- ###

recurPayments <- spending_data_factored %>%
  filter(Type == "Recurring payment") %>%
  dplyr::select(-Occasion) %>%
  mutate(`Recurring Type` = NA) %>%
  mutate(
    `Recurring Type` = replace(`Recurring Type`, Details == "Pineview Place rent" | Details == "LaCasa rent", "Rent"),
    `Recurring Type` = replace(`Recurring Type`, Details == "Internet", "Internet bill"),
    `Recurring Type` = replace(`Recurring Type`, Details == "Phone bill", "Phone bill"),
    `Recurring Type` = replace(`Recurring Type`, Comments == "iCloud storage", "iCloud storage"),
    `Recurring Type` = replace(`Recurring Type`, Comments == "Spotify premium", "Spotify premium"),
    `Recurring Type` = replace(`Recurring Type`, Details == "Medical Services Plan", "Medical Services Plan"),
    `Recurring Type` = replace(`Recurring Type`, Details == "Tenant insurance", "Tenant insurance"),
    `Recurring Type` = replace(`Recurring Type`, `Establishment | Service` == "Chegg", "Chegg subscription"),
    `Recurring Type` = replace(`Recurring Type`, `Establishment | Service` == "Namecheap", "Website domain (Namecheap)"),
    `Recurring Type` = replace(`Recurring Type`, Details == "Electricity bill", "Electricity bill"),
    `Recurring Type` = replace(`Recurring Type`, `Establishment | Service` == "Linode", "Website server (Linode)"),
    `Recurring Type` = replace(`Recurring Type`, Details == "Credit card annual fee", "Credit card fee"),
    `Recurring Type` = replace(`Recurring Type`, Comments == "Amazon Prime membership fee", "Amazon Prime membership")
  ) %>%
  dplyr::select(-Category, -Details, -`Establishment | Service`, -Comments) %>%
  mutate(Priority = NA) %>%
  mutate(
    Priority = replace(Priority, (`Recurring Type` == "Rent" | `Recurring Type` == "Internet bill" | `Recurring Type` == "Phone bill" 
                                 | `Recurring Type` == "Medical Services Plan" | `Recurring Type` == "Tenant insurance"
                                 | `Recurring Type` == "Electricity bill" | `Recurring Type` == "Credit card fee"), 
                       "Essential"),
    Priority = replace(Priority, is.na(Priority), "Non-essential")
  ) %>%
  mutate(Status = NA) %>%
  mutate(
    Status = replace(Status, `Recurring Type` == "Chegg subscription", "Inactive"),
    Status = replace(Status, is.na(Status), "Active")
  )

recurPayments_date <- recurPayments %>%
  unite(Date, Day, Month, Year, sep = "-") %>%
  ungroup() %>%
  mutate(Date = dmy(Date)) %>%
  arrange(Date)

## Monthly summation
recurPayments %>%
  group_by(Month) 
  
  
  
## Totals for each recurCategory of recurring payment
recurPayments %>% 
  group_by(`Recurring Type`) %>%
  summarize(Count = n(),
            Total = sum(Cost),
            Average = mean(Cost),
            `Standard deviation` = sd(Cost)) %>%
  arrange(desc(Total))

## Recurring payments (Essential)

recurPayments_date %>%
  filter(Priority == "Essential") %>%
  ggplot(aes(x = Date, y = Cost, colour = Status)) +
  geom_point(size = 3, alpha = 0.25) +
  geom_line(alpha = 0.75) + 
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_date(date_labels = "%b %Y") +
  scale_color_manual(values = c("#B37559", "#7D849B")) +
  facet_wrap(~ Type, scales = "free") +
  theme_minimal() +
  labs(x = "", y = "Cost (CAD)") +
  ggtitle("Essential Recurring Payments") +
  theme(
    text = element_text(family = "Century Gothic"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text.x = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 16)
  ) 
  

## Recurring payments (Non-essential)

recurPayments_date %>%
  filter(Priority == "Non-essential") %>%
  ggplot(aes(x = Date, y = Cost, colour = Status)) +
  geom_point(size = 3, alpha = 0.25) +
  geom_line(alpha = 0.75) + 
  scale_y_continuous(limits = c(0, NA)) +
  scale_x_date(date_labels = "%b %Y") +
  scale_color_manual(values = c("#B37559", "#7D849B")) +
  facet_wrap(~ Type, scales = "free") +
  theme_minimal() +
  labs(x = "", y = "Cost (CAD)") +
  ggtitle("Essential Recurring Payments") +
  theme(
    text = element_text(family = "Century Gothic"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    strip.text.x = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, size = 16)
  ) 

recurring_payments_graph <- function(priority) {
  recurPayments_date %>%
    filter(Priority == priority) %>%
    ggplot(aes(x = Date, y = Cost, colour = Status)) +
    geom_point(size = 3, alpha = 0.25) +
    geom_line(alpha = 0.5) + 
    scale_y_continuous(limits = c(0, NA)) +
    scale_x_date(date_labels = "%b %Y") +
    scale_color_manual(values = c("#B37559", "#80B08F")) +
    facet_wrap(~ `Recurring Type`, scales = "free") +
    theme_minimal() +
    labs(x = "", y = "Cost (CAD)") +
    theme(
      text = element_text(family = "Century Gothic"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      strip.text.x = element_text(size = 12, color = "#D1AD7C")
    ) 
}

recurring_payments_graph("Essential")
recurring_payments_graph("Non-essential")

##### Summing recurring payments and plotting them on a monthyl basis

recurring_payments_byMonth <- recurPayments %>% 
  group_by(Month, Year, Priority) %>%
  summarize(Total = sum(Cost)) %>%
  spread(Priority, Total) 

recurring_payments_byMonth$`Non-essential`[is.na(recurring_payments_byMonth$`Non-essential`)] <- 0

all_payments_byMonth <- spending_data_factored %>%
  filter(!is.na(Cost)) %>%
  filter(Details != "Tuition") %>%
  group_by(Month, Year) %>%
  summarize(Total = sum(Cost)) %>%
  left_join(recurring_payments_byMonth, by = c("Month", "Year")) %>%
  mutate(`Non-recurring Payment` = Total - (Essential + `Non-essential`)) %>%
  dplyr::select(-Total) %>%
  dplyr::rename(`Recurring: Essential` = Essential,
                `Recurring: Non-essential` = `Non-essential`) %>%
  gather(key = "Classification", value = "Aggregated Cost", -Month, -Year)

p1 <- ggplot(all_payments_byMonth, aes(x = Month, y = `Aggregated Cost`, fill = Classification)) + 
  geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
  facet_wrap(~ Year, ncol = 1, scales = "free") + 
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  scale_fill_manual(values = c("#76BFA7", "#E6B05E", "#AA5D57"), guide = F) +
  theme(
    text = element_text(family = "Century Gothic", size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text.x = element_text(size = 14, colour = "#404040"),
    axis.title.x = element_blank()
  )

all_payments_byMonth_average <- spending_data_factored %>%
  filter(!is.na(Cost)) %>%
  filter(Details != "Tuition") %>%
  group_by(Month, Year) %>%
  summarize(Total = sum(Cost)) %>%
  ungroup() %>%
  left_join(recurring_payments_byMonth, by = c("Month", "Year")) %>%
  mutate(`Non-recurring Payment` = Total - (Essential + `Non-essential`)) %>%
  dplyr::select(-Total) %>%
  dplyr::rename(`Recurring: Essential` = Essential,
                `Recurring: Non-essential` = `Non-essential`) %>%
  group_by(Year) %>%
  summarize(`Recurring: Essential` = mean(`Recurring: Essential`),
            `Recurring: Non-essential` = mean(`Recurring: Non-essential`),
            `Non-recurring Payment` = mean(`Non-recurring Payment`)) %>%
  gather(key = "Classification", value = "Aggregated Cost", -Year)

p2 <- ggplot(all_payments_byMonth_average, aes(x = "", y = `Aggregated Cost`, fill = Classification)) +
  geom_bar(stat = "identity", position = "stack", alpha = 0.5) +
  facet_wrap(~ Year, ncol = 1, scales = "free") + 
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  scale_fill_manual(values = c("#76BFA7", "#E6B05E", "#AA5D57")) +
  theme(
    text = element_text(family = "Century Gothic", size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(colour = "#AA5D57"),
    axis.title.y = element_blank(),
    strip.text.x = element_blank(),
    axis.title.x = element_blank()
  ) 

cowplot::plot_grid(p1, p2, align = "h", axis = "bt", rel_widths = c(3, 1))



### TESTING

recurring_payments <- spending_data_factored %>%
  filter(Type == "Recurring payment") %>%
  dplyr::select(-Occasion) %>%
  mutate(`Recurring Type` = NA) %>%
  mutate(
    `Recurring Type` = replace(`Recurring Type`, Details == "Pineview Place rent" | Details == "LaCasa rent", "Rent"),
    `Recurring Type` = replace(`Recurring Type`, Details == "Internet", "Internet bill"),
    `Recurring Type` = replace(`Recurring Type`, Details == "Phone bill", "Phone bill"),
    `Recurring Type` = replace(`Recurring Type`, Comments == "iCloud storage", "iCloud storage"),
    `Recurring Type` = replace(`Recurring Type`, Comments == "Spotify premium", "Spotify premium"),
    `Recurring Type` = replace(`Recurring Type`, Details == "Medical Services Plan", "Medical Services Plan"),
    `Recurring Type` = replace(`Recurring Type`, Details == "Tenant insurance", "Tenant insurance"),
    `Recurring Type` = replace(`Recurring Type`, `Establishment | Service` == "Chegg", "Chegg subscription"),
    `Recurring Type` = replace(`Recurring Type`, `Establishment | Service` == "Namecheap", "Website domain (Namecheap)"),
    `Recurring Type` = replace(`Recurring Type`, Details == "Electricity bill", "Electricity bill"),
    `Recurring Type` = replace(`Recurring Type`, `Establishment | Service` == "Linode", "Website server (Linode)"),
    `Recurring Type` = replace(`Recurring Type`, Details == "Credit card annual fee", "Credit card fee"),
    `Recurring Type` = replace(`Recurring Type`, Comments == "Amazon Prime membership fee", "Amazon Prime membership")
  ) %>%
  dplyr::select(-Category, -Details, -`Establishment | Service`, -Comments) %>%
  mutate(Priority = NA) %>%
  mutate(
    Priority = replace(Priority, (`Recurring Type` == "Rent" | `Recurring Type` == "Internet bill" | `Recurring Type` == "Phone bill" 
                                  | `Recurring Type` == "Medical Services Plan" | `Recurring Type` == "Tenant insurance"
                                  | `Recurring Type` == "Electricity bill" | `Recurring Type` == "Credit card fee"), 
                       "Essential"),
    Priority = replace(Priority, is.na(Priority), "Non-essential")
  ) %>%
  mutate(Status = NA) %>%
  mutate(
    Status = replace(Status, `Recurring Type` == "Chegg subscription", "Inactive"),
    Status = replace(Status, is.na(Status), "Active")
  )

recurring_payments_date <- recurring_payments %>%
  unite(Date, Day, Month, Year, sep = "-") %>%
  ungroup() %>%
  mutate(Date = dmy(Date)) %>%
  arrange(Date)

#### Function to display graphs for monthly spending, faceted by the type (argument determine the priority)

recurring_payments_graph <- function(priority) {
  recurring_payments_date %>%
    filter(Priority == priority) %>%
    ggplot(aes(x = Date, y = Cost, colour = Status)) +
    geom_point(size = 5, alpha = 0.25) +
    geom_line(alpha = 0.5) + 
    scale_y_continuous(limits = c(0, NA)) +
    scale_x_date(date_labels = "%b %Y") +
    scale_color_manual(values = c("#B37559", "#80B08F")) +
    facet_wrap(~ `Recurring Type`, scales = "free") +
    theme_minimal() +
    labs(x = "", y = "Cost (CAD)") +
    theme(
      text = element_text(family = "Century Gothic"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      strip.text.x = element_text(size = 14, color = "#D1AD7C"),
      axis.text.y = element_text(size = 12),
      legend.text = element_text(size = 12)
    ) 
}

### Monthly aggregates of recurrent payments

monthly_rp<- recurring_payments %>% 
  group_by(Month, Year, Priority) %>%
  summarize(Total = sum(Cost)) %>%
  spread(Priority, Total) 

monthly_rp$`Non-essential`[is.na(monthly_rp$`Non-essential`)] <- 0

### (Excludes tuition)
monthly_expenditure_rp <- spending_data_factored %>%
  filter(!is.na(Cost)) %>%
  filter(Details != "Tuition") %>%
  group_by(Month, Year) %>%
  summarize(Total = sum(Cost)) %>%
  left_join(monthly_rp, by = c("Month", "Year")) %>%
  mutate(`Non-recurring Payment` = Total - (Essential + `Non-essential`)) %>%
  dplyr::select(-Total) %>%
  dplyr::rename(`Recurring: Essential` = Essential,
                `Recurring: Non-essential` = `Non-essential`) %>%
  gather(key = "Classification", value = "Aggregated Cost", -Month, -Year)

avg_expenditure_inc_rp <- monthly_expenditure_rp %>%
  spread(key = Classification, value = `Aggregated Cost`) %>%
  ungroup() %>%
  group_by(Year) %>%
  summarize(`Recurring: Essential` = mean(`Recurring: Essential`),
            `Recurring: Non-essential` = mean(`Recurring: Non-essential`),
            `Non-recurring Payment` = mean(`Non-recurring Payment`)) %>%
  gather(key = "Classification", value = "Aggregated Cost", -Year)





