### get_game_officials Function
Helper function for `bigballR` that scrapes game refs

Please install `bigballR` in order to use this function. You can do so at https://github.com/jflancer/bigballR

A few notes regarding this function and output.
- The output returns a team schedule with 3 additional columns - Official 1, Official 2, Official 3. The scraping is a little slower than a typical schedule scrape because to get ref names, the function must loop through each ID within a team's schedule
- Unfortunately, yes - the only way I know to get all of the refs is by team schedule, not by individual ref.
- Additionally, if you would like each official to have their own row (rather than all 3 officials in the same row for the same game), you can do so with `tidyr::pivot_longer` (example below)

```r
  officials = get_game_officials(team = "Purdue",season="2025-26")

single_output = officials %>% 
  tidyr::pivot_longer(cols = c(`Official 1`, `Official 2`, `Official 3`),
               names_to = "Official Number",
               values_to = "Official Name")

```

- Because ref names are typically manually inputted within StatBroadcast, sometimes there are small typos/missmatches (such as DJ Carstensen vs D.J. Carstensen). You may need to go through and clean those up if doing some larger project. When I've done that before, I've just grabbed a `distinct` of all ref names and then manually clean up the matching.
- This is currently just for MBB - however I assume the function would work all the same (with some cleaning up) with WBB as long as women's Team IDs are available.


### get_game_officials_wbb Function
function to scrape women's basketball schedules + officials

- still helpful to install `bigballR` even though it doesn't use the same `get_team_schedule` function, it uses some `chromote` functions in the background

- This function also requires adding the `get_team_schedule_wbb` function which exists in the same `.R` file. Only difference is that it won't reference the men's ids, but rather reference the women's IDs
- I was able to remember how to easily scrape women's IDs and scraped them for all D1 teams back to the 2019-2020 season. Those exist in a file called `team_ids_wbb_20_26.RData`. This has to be loaded to use the function, and you can name this dataframe whatever you want. You just then have to use that name to then call on the dataframe in the function.

- Here's an example of how you would use the functions (after running the functions to add them to your environment)

```r

womens_ids = load("team_ids_wbb_20_26.RData")

schedule_w_officials = get_game_officials_wbb(team = "Purdue", season = "2025-26", ids_df = womens_ids) #ids_df is just whatever you named the .Rdata file of women's ids
```



