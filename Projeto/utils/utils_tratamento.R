### 

# --------------------------- PACOTES NECESSARIOS -----------------------------#

pacotes <- c("tidyverse", "bib2df", "janitor", "rscopus", "biblionetwork","RColorBrewer",
"tidygraph", "ggraph", "ggnewscale",'stringi',"data.table",'openxlsx','ggwordcloud',
 "bibliometrix", "ggpubr","broom","viridis","treemapify","ggrepel",'igraph',
'ggh4x','stringdist','maps','devtools')

githubpacotes <- c("thomasp85/scico","agoutsmedt/networkflow","ParkerICI/vite",
                   'hrbrmstr/pluralize')

for(i in pacotes){
  
  if(!i %in% rownames(installed.packages())){
utils::install.packages(i, dependencies = TRUE, quiet = TRUE,
                      ask = FALSE,character.only = TRUE)

  }}

for(i in githubpacotes){
  
  if(! gsub(".*/", "",i) %in% rownames(installed.packages())){
    devtools::install_github(i)
    
  }}

for(i in c(pacotes,gsub(".*/", "",githubpacotes))){
  library(i, character.only = TRUE)
}

rm(pacotes,i,githubpacotes)

# -----------------------------------------------------------------------------#

#### NOMES INSTITUICOES

name_insti <- function(x){
  
  lapply(x, function(y){ w = str_replace_all(y,
        fixed(c('UNIVERSITY'='UNIV','.'='','DEPARTMENTS'='DEPARTMENT',
        'UNIV OF '='UNIV ','UNIV ' = 'UNIV OF ','INSTITUTE '='INST ',
        'DEPARTMENT'='DEPT','SCIENCES'='SCIENCE',
        'SCIENCE' = 'SCIENCES','UNIVERSITAET'='UNIVERSITAT'))) |>
      stri_trans_general('latin-ascii')
  w = gsub('["–-]', ' ', w)
  gsub("'",' ',w) |> str_squish()
  
  }) |>
    unlist()
  
}

#### PALAVRAS CHAVES

palavras_chave <- function(x){
  
  lapply(x, function(y){ 
    if (!is.na(y)) {  
      y |> str_split(';') |> as_vector() |>
        singularize() |> str_flatten(";") |> 
        str_squish()}else{
          NA
        }
  }) |> unlist() 
}

#### DISTANCIA NOMES

distancias <- function(x){
  
  afiliacoes <- x |> strsplit(';') |> unlist() |> str_squish() |> unique()
  
  dist_matrix <- stringdistmatrix(afiliacoes, method = "lv") |> as.matrix()
  
  indices_superior <- which(upper.tri(dist_matrix), arr.ind = TRUE)
  
  valores_superior <- dist_matrix[indices_superior]
  
  dist_matrix <- data.frame(row = indices_superior[, 1],
                            col = indices_superior[, 2], valor = valores_superior)
  
  dist_matrix <- dist_matrix |> filter(valor == 1) |> select(!valor) |>
    mutate(row = afiliacoes[row], col = afiliacoes[col]) 
  
  dist_matrix
  
}

#### REMOVER DUPLICATAS

rem_dupli <- function(y){
  
 # nas = apply(y, 1, function(x){sum(is.na(x))})
  
  y = y |> mutate(idaux = 1:n())
  
  ids<- y |> filter(!is.na(DI),DT %in% c('ARTICLE','REVIEW')) |>
    mutate(DI2 = str_to_upper(gsub("[^a-zA-Z0-9 ]", "", DI))) |> 
    group_by(DI) |> filter(n()>1) |> arrange(DT) |>
    distinct(DI2, .keep_all = T) |> ungroup() |>
    select(idaux) |> as_vector()
  
  y <- y |> filter(!idaux %in% ids) |> select(!idaux) |> 
    mutate(TI2 = str_to_upper(gsub("[^a-zA-Z0-9 ]", "", TI)))

  y = split(y,y$PY)
  
  y = lapply(y, function(x){
    duplicatedMatching(x, Field = "TI2", exact = FALSE, tol = 0.85) |> 
      select(!TI2)
  })
  
  y <- rbindlist(y)
  
}
