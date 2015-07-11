ua <- "Mozilla/5.0 (Windows NT 6.1; WOW64; rv:33.0) Gecko/20100101 Firefox/33.0"

SKU <- 195905
SKU <- 457028

liquor_pg <- try(rvest::html(paste0("http://www.bcliquorstores.com/product/", SKU),
                             httr::user_agent(ua)), silent = TRUE)

# if any retrieval error, return NA
if (inherits(liquor_pg, "try-error")) {return(NA) }

# smoke out png or jpg!
img_bag <- liquor_pg %>% html_nodes("a img") %>%
  html_attr("src")

img_bag[grep(SKU, img_bag)]

# Catch no matching element
img <- ifelse(length(grep(SKU, img_bag)) == 0, "No Image", img(src = img_bag[grep(SKU, img_bag)], height = 100))


if(length(grep(SKU, img_bag)) == 0) {
  img <- "No Image"
} else {
  img <-  img(src = img_bag[grep(SKU, img_bag)], height = 100)
}