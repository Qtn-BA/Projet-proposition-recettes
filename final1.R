library(purrr)
library(httr)
library(stringr)
library(xml2)
library(stringr)
library(tools)
library(rvest)
library(dplyr)

user_agent <- httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36")
res = GET('https://www.whatismybrowser.com/detect/what-is-my-user-agent/', user_agent)
str_match_all(content(res, "text"), '<div class="value" id="detected_value">(.*?)</div>')[[1]][,2]


# Initialiser le data frame
detailed_recipes <- data.frame(
  Title = character(),
  Link = character(),
  stringsAsFactors = FALSE
)



df <- function(URLchoisie) {
  # Initialiser un data frame pour stocker les résultats
  detailed_recipes <- data.frame(Title = character(), Link = character(), stringsAsFactors = FALSE)
  
  # Scraper les pages 1 à 5
  for (page in 1:5) {
    cat("Scraping page:", page, "\n")
    
    # Construire l'URL complète
    url <- paste0(URLchoisie, page)
    
    # Lire la page
    tryCatch({
      webpage <- read_html(url)
      
      # Extraire les titres et les liens des recettes
      titles <- webpage %>% html_nodes(".recipe-card__title") %>% html_text(trim = TRUE)
      links <- webpage %>% html_nodes(".recipe-card-link") %>% html_attr("href")
      
      # Construire le data frame
      for (i in seq_along(links)) {
        # Construire le lien complet
        recipe_url <- links[i]
        if (!startsWith(recipe_url, "https")) {
          recipe_url <- paste0("https://www.marmiton.org", recipe_url)
        }
        
        # Ajouter au data frame
        detailed_recipes <- rbind(detailed_recipes, data.frame(
          Title = titles[i],
          Link = recipe_url,
          stringsAsFactors = FALSE
        ))
      }
      
    }, error = function(e) {
      cat("Error scraping page:", url, "\n")
    })
  }
  
  # Retourner les résultats
  return(detailed_recipes)
}


base_urls <- c(
  "https://www.marmiton.org/recettes/recherche.aspx?aqt=entree&page=",
  "https://www.marmiton.org/recettes/recherche.aspx?aqt=plat-principal&page=",
  "https://www.marmiton.org/recettes/recherche.aspx?aqt=dessert&page="
)

detailed_recipes <- map_df(base_urls, df)


#Récupérer les ingrédients pour chaque aliment
ingrédients <- function(x){
  res2 = GET(detailed_recipes$Link[x], user_agent)
  html2 = xml2::read_html(httr::content(res2, "text"))
  clean_html2 = str_replace_all(as.character(html2),'[\\t\\r\\n\\f]','')
  ingredients2 <- html2 %>%
    html_nodes(".card-ingredient") %>%
    html_text(trim = TRUE)%>%          
    str_replace_all(c("\\s+" =  " ", "cl"= " ", "[0-9]" = " ", "de." = " ", "\\b[g]\\b" = " ", "\\b[l]\\b" = " ", "ml" = " ", "\\b[.]\\b" = " ", "%" = " ")) %>%
    str_trim()
  ingredients2 <- unique(ingredients2)
  print(ingredients2)
}


#Ajouter les ingrédients
liste_ingredient <- map(c(1:40, 42: 225), ingrédients)
detailed_recipes <- detailed_recipes[-41, ]
detailed_recipes <- detailed_recipes %>%
  mutate("Liste_des_ingrédients" = str_trim(liste_ingredient))


##Création d'une fonction


request <- function(...) {
  ing <- c(...)
  detailed_recipes <- detailed_recipes %>%
    mutate(Note = 0)
  
  detailed_recipes <- detailed_recipes %>%
    rowwise() %>%
    mutate(Note = sum(sapply(ing, function(ingredient) grepl(ingredient, Liste_des_ingrédients, ignore.case = TRUE)))) %>%
    ungroup()
  
  max_note <- max(detailed_recipes$Note, na.rm = TRUE)
  meilleures_propositions <- detailed_recipes %>%
    filter(Note == max_note) %>%
    select(Title) %>%
    pull()
  URL2 <- detailed_recipes %>%
    filter(Note == max_note) %>%
    select(Link) %>%
    pull()
  
  if(max_note >= 2){
    print(paste("Nous vous recommandons cet aliment:", meilleures_propositions, ". Vous pouvez trouver davantage d'informations sur le lien suivant:", URL2))
  }else{
    print("Malheureusement, aucune recette ne correspond à vos ingrédients")
  }
}

request("sel", "melon", "caramel", "poulet", "dinde", "huile d'olive", "jambon", "saumon")
        
## ATtention, il faut écrire exactement les ingrédients de la manière dont ils sont écrits