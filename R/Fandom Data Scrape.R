scrape.corrie.data <- function(save.results) {

  # base URL code for scraping character names
  base.url.letters <- "https://coronationstreet.fandom.com/wiki/Category:List_of_main_character_appearances?from="

  # populate all.char.names.list with all character names that have pages on fandom.wiki
  # to do this, cycle through each letter of alphabet, scrape from the page that has corrie characters under that letter

  all.char.names.list <- list()

  for (lett in LETTERS) {
    char.list <- str_c(base.url.letters, lett, sep = "")

    character.names <- rvest::read_html(char.list) %>%
      rvest::html_elements(".category-page__member-link") %>%
      rvest::html_text() %>%
      str_remove_all(" - List of appearances")

    all.char.names.list[[lett]] <- character.names
  }

  # get vector of all unique character names
  all.char.names <- all.char.names.list %>%
    unlist() %>% # turn list into vector
    as.character() %>% # convert data type to character
    unique() # remove duplicates


  # base URL code for scraping character names
  base.url.characters <- "https://coronationstreet.fandom.com/wiki/"

  # function to scrape fandom wiki for a character
  # function is calibrated for Coronation St. wiki, although might be more transferable to others

  scrape.fandom.wiki <- function(character,
                                 base.url) {
    char.text <- character %>% str_replace_all(" ", "_")
    char.bio <- str_c(base.url, char.text)

    # pull the key stats for the character
    char.stat <- rvest::read_html(char.bio) %>%
      rvest::html_elements(".pi-secondary-font") %>%
      rvest::html_text() %>%
      .[-1]

    char.value <- rvest::read_html(char.bio) %>%
      rvest::html_elements(".pi-font") %>%
      rvest::html_text()

    char.data <- data.frame(Field = char.stat, Value = char.value)

    return(char.data)
  }

  # populate all.char.data by mapping scrape.fandom.wiki across character vector... can take a while (10 mins)
  char.data.list <- map(
    .x = all.char.names,
    .f = scrape.fandom.wiki,
    base.url = base.url.characters
  )

  # tidy the list up into data.frame

  names(char.data.list) <- all.char.names
  character.data <- char.data.list %>% bind_rows(.id = "Character")

  # write csv file
  if (save.results) {
    write.csv(character.data, "character.data.csv")
  }

  return(character.data)
}
