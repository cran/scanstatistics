## ---- echo=FALSE---------------------------------------------------------
knitr::opts_chunk$set(fig.width=7.15, fig.height=5)

## ------------------------------------------------------------------------
library(scanstatistics)
library(ggplot2)

# Load map data
data(NM_map)

# Place the county names at the centroid of the counties. The following function
# calculates the geographical centroids from the polygons in NM_map.
# See: https://en.wikipedia.org/wiki/Centroid#Centroid_of_polygon
polygon_centroid <- function(x0, y0) {
  x1 <- c(x0[-1], x0[1])
  y1 <- c(y0[-1], y0[1])
  A <- sum(x0 * y1 - x1 * y0) / 2
  Cx <- sum((x0 + x1) * (x0 * y1 - x1 * y0)) / (6 * A)
  Cy <- sum((y0 + y1) * (x0 * y1 - x1 * y0)) / (6 * A)
  data.frame(long = Cx, lat = Cy)
}

# Calculate geographic centroids for each county
centroids <- NM_map[, polygon_centroid(long, lat), by = .(subregion)]

# Plot map with labels at centroids
ggplot() + 
  geom_polygon(data = NM_map,
               mapping = aes(x = long, y = lat, group = group),
               color = "grey", fill = "white") +
  geom_text(data = centroids, 
            mapping = aes(x = long, y = lat, label = subregion)) +
  ggtitle("Counties of New Mexico")

## ------------------------------------------------------------------------
library(sp)
library(magrittr)

data(NM_geo)

zones <- NM_geo[, c("long", "lat"), with = FALSE] %>%
  as.matrix %>%
  spDists(x = ., y = ., longlat = TRUE) %>%
  dist_to_knn(k = 15) %>%
  knn_zones

## ------------------------------------------------------------------------
data(NM_popcas)

tab <- NM_popcas[year >= 1986 & year < 1990, ]
tab[, duration := max(year) - year + 1]
tab[, location := county]

## ------------------------------------------------------------------------
mod_poisson <- glm(count ~ offset(log(population)) + 1 + I(year - 1985), 
                   data = NM_popcas[year < 1986, ], 
                   family = poisson(link = "log"))

# Add the expected value parameter column
tab[, mu := predict(mod_poisson, tab, type = "response")]

## ------------------------------------------------------------------------
set.seed(1)
poisson_result <- scan_poisson(tab, zones, n_mcsim = 99)
print(poisson_result)

## ---- comment=NA---------------------------------------------------------
counties <- as.character(NM_geo$county)
counties[c(15, 26)]

## ------------------------------------------------------------------------
# Calculate scores and add column with county names
county_scores <- score_locations(poisson_result)
county_scores[, county := counties]

# Create a table for plotting
score_map_df <- merge(NM_map, county_scores, by = "county", all.x = TRUE)
score_map_df[subregion == "cibola", 
             relative_score := county_scores[county == "valencia", relative_score]]

ggplot() + 
  geom_polygon(data = score_map_df,
               mapping = aes(x = long, y = lat, group = group, 
                             fill = relative_score),
               color = "grey") +
  scale_fill_gradient(low = "#e5f5f9", high = "darkgreen",
                      guide = guide_colorbar(title = "Relative\nScore")) +
  geom_text(data = centroids, 
            mapping = aes(x = long, y = lat, label = subregion),
            alpha = 0.5) +
  ggtitle("County scores")

## ------------------------------------------------------------------------
top5 <- top_clusters(poisson_result, k = 5, overlapping = FALSE)

# Find the counties corresponding to the spatial zones of the 5 clusters.
top_counties <- top5$zone %>%
  purrr::map(get_zone, zones = zones) %>%
  purrr::map(function(x) counties[x])

# Add the counties corresponding to the zones as a column
top5[, counties := top_counties]

## ------------------------------------------------------------------------
top5[, pvalue := mc_pvalue(statistic, poisson_result$replicated)]

top5

## ------------------------------------------------------------------------
suppressWarnings(
mod_negbin <- MASS::glm.nb(count ~ offset(log(population)) + 1 + I(year - 1985), 
                         data = NM_popcas[year < 1986, ],
                         link = log)
)

# Add the parameters as columns
tab[, mu := predict(mod_negbin, tab, type = "response")]
tab[, theta := mod_negbin$theta]

## ------------------------------------------------------------------------
set.seed(1)
negbin_result1 <- scan_negbin(tab, zones, n_mcsim = 99, version = "ordinary")
print(negbin_result1)

## ------------------------------------------------------------------------
set.seed(1)
negbin_result2 <- scan_negbin(tab, zones, n_mcsim = 99, version = "increasing")
print(negbin_result2)

## ------------------------------------------------------------------------
library(pscl, quietly = TRUE)
mod_zip <- zeroinfl(count ~ offset(log(population)) + 1 + I(year - 1985),
                    data = NM_popcas[year < 1986, ],
                    dist = "poisson", link = "logit")

# Add the parameters as columns
tab[, mu := predict(mod_zip, tab, type = "count")]
tab[, p := predict(mod_zip, tab, type = "zero")]

## ------------------------------------------------------------------------
set.seed(1)
zip_result <- scan_zip(tab, zones, n_mcsim = 99)
print(zip_result)

