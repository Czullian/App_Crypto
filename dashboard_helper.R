clean_class <- . %>% 
  select(consensus) %>% 
  distinct() %>% 
  arrange(consensus)

clean_crypto <- . %>% 
  select(crypto) %>% 
  distinct() %>% 
  arrange(crypto)

create_elo_data <- function(k){
  temp_df <- elo.run(winner ~ crypto + opponent, k = k, 
                     data = elo_df %>% arrange(crypto , date)) %>% 
    as_tibble() %>% 
    cbind(elo_df %>% arrange(crypto, date) %>% select(match_id)) %>% 
    select(team.A, team.B, elo.A, elo.B, match_id)
  
  rbind(temp_df %>% 
          select_at(vars(contains(".A"), contains("match_id"))) %>% 
          rename_all(.funs = function(x) str_replace(x, ".A", "")),
        temp_df %>% 
          select_at(vars(contains(".B"), contains("match_id"))) %>% 
          rename_all(.funs = function(x) str_replace(x, ".B", ""))) %>% 
    rename("crypto" = "team") %>% 
    left_join(df %>% 
                select(crypto, date, consensus, match_id),
              by = c("crypto", "match_id")) %>% 
    mutate(date = as.Date(date))
}
elo_df <- read.csv2("elo_df.csv")
df <- read.csv2("elo.csv")

