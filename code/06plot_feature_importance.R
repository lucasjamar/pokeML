library(data.table)
library(ggplot2)

dt <- data.table::fread("data/06_models/feature_importance.csv")

# Plot information gain
plt <- ggplot2::ggplot(dt) +
  aes(x = reorder(Feature, Gain), y = Gain) +
  geom_col(fill = "#0c4c8a") +
  coord_flip() +
  labs(y = "Information Gain", x = "Feature Name")
ggplot2::ggsave("data/08_reporting/information_gain.png", plot = plt)
print(plt)

# Plot coverage
plt <- ggplot2::ggplot(dt) +
  aes(x = reorder(Feature, Cover), y = Cover) +
  geom_col(fill = "#0c4c8a") +
  coord_flip() +
  labs(y = "Coverage", x = "Feature Name")
ggplot2::ggsave("data/08_reporting/coverage.png", plot = plt)
print(plt)

# Plot frequency
plt <- ggplot2::ggplot(dt) +
  aes(x = reorder(Feature, Frequency), y = Frequency) +
  geom_col(fill = "#0c4c8a") +
  coord_flip() +
  labs(y = "Frequency", x = "Feature Name")
ggplot2::ggsave("data/08_reporting/frequency.png", plot = plt)
print(plt)
