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
