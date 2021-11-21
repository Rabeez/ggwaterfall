library(tidyverse)


## data and reference ----

df <- tibble(
  x.axis.Var = rep(c("Widgets", "Gridgets", "Groms", "Wobs"), 3),
  cat.Var = rep(c("High End", "Mid Range", "Low End"), each = 4),
  values = c(600, 500, 300, 200, # high end
             300, 200, 300, 250, # mid range
             100, 80, 200, 150   # low end
             )
  )

# stacked bar graph (for reference)
ggplot(df, aes(x = x.axis.Var, y = values, fill = cat.Var)) +
  geom_col()

## preparation function ----

my_prepare_df <- function(df, 
                          x_axis_var, x_axis_order, 
                          cat_var, cat_order,
                          value_var,
                          bar_width, 
                          stacked) {
  
  # calculate control variables
  n_segments <- length(cat_order)
  group_offset = bar_width / 2
  
  temp <- df %>% 
    # ensure the categories are in the required order
    mutate(
      {{x_axis_var}} := fct_relevel({{x_axis_var}}, x_axis_order),
      {{cat_var}} := fct_relevel({{cat_var}}, cat_order),
    ) %>% 
    arrange({{x_axis_var}}, {{cat_var}}) %>% 
    # create numeric indicator for x_axis_var
    group_by({{x_axis_var}}) %>% 
    mutate(
      group_id = cur_group_id()
    ) %>% 
    ungroup() %>% 
    # create numeric indicator for cat_var
    group_by({{cat_var}}) %>% 
    mutate(
      sub_group_id = cur_group_id()
    ) %>% 
    ungroup() %>% 
    # calculate the segment vertical edges
    mutate(
      segment_bottom = cumsum(c(0, head({{value_var}}, -1))),
      segment_top = {{value_var}} + segment_bottom,
    )
  
  if (stacked) {
    # if the segments are supposed to be stacked then
    temp <- temp %>% 
      # calculate the segment horizontal edges 
      # these will be same for all segments within a bar
      group_by({{x_axis_var}}) %>% 
      mutate(
        segment_left = group_id - group_offset,
        segment_right = group_id + group_offset,
      ) %>%
      ungroup()
  } else {
    # if the segments re not supposed to be stacked (internal waterfall)
    temp <- temp %>%
      # scale up the group_id since inter-bar spacing is determined by this
      mutate(
        group_id = group_id * n_segments,
      ) 
    
    # ensure this is atleast spaced by 2*bar_width
    if (min(temp$group_id) < 2) {
      temp <- temp %>% 
        mutate(
          group_id = group_id + 1
        )
    }
    
    temp <- temp %>% 
      # calculate the segment horizontal edges 
      # these are different for all segments now
      group_by({{x_axis_var}}) %>% 
      mutate(
        segment_left = group_id + group_offset * seq.int(from = -n_segments, by = 2, length.out = n_segments),
        segment_right = group_id + group_offset * seq.int(from = -n_segments + 2, by = 2, length.out = n_segments),
      ) %>% 
      ungroup()
  }
  
  temp %>% 
    # calculate all 4 bar edges
    # these are defined as the bounding box of all segments in a bar
    group_by({{x_axis_var}}) %>% 
    mutate(
      bar_bottom = first(segment_bottom),
      bar_left = first(segment_left),
      bar_top = last(segment_top),
      bar_right = last(segment_right),
    ) %>% 
    ungroup() %>% 
    # ensure resulting dataframe has a consistent, sensible column ordering
    select(
      group_id, {{x_axis_var}}, 
      sub_group_id, {{cat_var}}, 
      {{value_var}},
      segment_bottom, segment_top, segment_left, segment_right,
      bar_bottom, bar_top, bar_left, bar_right,
    )
}



my_prepare_df_facet <- function(df, 
                                x_axis_var, x_axis_order, 
                                cat_var, cat_order,
                                value_var,
                                facet_var,
                                bar_width, 
                                stacked) {
  if (missing(facet_var)) {
    my_prepare_df(
      df,
      {{x_axis_var}}, x_axis_order,
      {{cat_var}}, cat_order,
      {{value_var}},
      bar_width,
      stacked
    )
  } else {
    purrr::map_dfr(unique(df[[deparse(substitute(facet_var))]]),
                 function(facet_val) {
                   my_prepare_df(
                     df %>% 
                       filter({{facet_var}} == facet_val) %>%
                       select(-{{facet_var}}), 
                     {{x_axis_var}}, x_axis_order, 
                     {{cat_var}}, cat_order,
                     {{value_var}},
                     bar_width, 
                     stacked
                     ) %>% 
                     mutate(
                       {{facet_var}} := facet_val
                     )
                 })
  }
}

## stacked ----

my_prep_df <- my_prepare_df_facet(
  df, 
  x.axis.Var, c("Widgets", "Gridgets", "Groms", "Wobs"),
  cat.Var, c("High End", "Mid Range", "Low End"),
  values,
  bar_width = 0.5,
  stacked = T
)

# stacked version
ggplot(my_prep_df) + 
  geom_rect(aes(
    x = x.axis.Var, 
    fill = cat.Var,
    xmin = bar_left,
    xmax = bar_right,
    ymin = segment_bottom,
    ymax = segment_top,
  ))


# stacked version (with reference line)
# TODO: find a way to skip last line ???
ggplot(my_prep_df) + 
  geom_rect(aes(
    x = x.axis.Var,
    fill = cat.Var,
    xmin = bar_left,
    xmax = bar_right,
    ymin = segment_bottom,
    ymax = segment_top,
  )) +
  geom_segment(aes(
    x = bar_right,
    y = bar_top,
    xend = bar_right + 0.5, # this is the spacing between bars (by default this is same as bar_width)
    yend = bar_top,
  ))


## side-by-side ----

my_prep_df_stack <- my_prepare_df_facet(
  df, 
  x.axis.Var, c("Widgets", "Gridgets", "Groms", "Wobs"),
  cat.Var, c("High End", "Mid Range", "Low End"),
  values,
  bar_width = 0.5,
  stacked = F
)

# side-by-side version
ggplot(my_prep_df_stack) + 
  geom_rect(aes(
    x = group_id,
    fill = cat.Var,
    xmin = segment_left,
    xmax = segment_right,
    ymin = segment_bottom,
    ymax = segment_top,
  )) + 
  scale_x_continuous(breaks = unique(my_prep_df_stack$group_id), 
                     labels = levels(my_prep_df_stack$x.axis.Var))


# side-by-side version (with reference line)
# TODO: find a way to skip last line ???
ggplot(my_prep_df_stack) + 
  geom_rect(aes(
    x = group_id,
    fill = cat.Var,
    xmin = segment_left,
    xmax = segment_right,
    ymin = segment_bottom,
    ymax = segment_top,
  )) +
  geom_segment(aes(
    x = bar_right,
    y = bar_top,
    xend = bar_right + 1.5, # this is the spacing between bars (by default this is same as bar_width)
    yend = bar_top,
  )) +
  scale_x_continuous(breaks = unique(my_prep_df_stack$group_id), 
                     labels = levels(my_prep_df_stack$x.axis.Var))


## Total bars ----


# demonstrative example of creating a 'Total' x_axis_var before calling prepare to have big total bar



# demonstrative example of creating 2 'Total' x_axis_var columns before calling prepare to have 2 groupwise total bars


## negative steps ----


df_neg <- tibble(
  x.axis.Var = rep(c("Widgets", "Gridgets", "Groms", "Wobs"), 3),
  cat.Var = rep(c("High End", "Mid Range", "Low End"), each = 4),
  values = c(600, 500, -300, 200, # high end
             -200, -800, 300, 250, # mid range
             100, 80, 200, -150   # low end
  )
)
df_neg

my_prep_df_neg <- my_prepare_df_facet(
  df_neg, 
  x.axis.Var, c("Widgets", "Gridgets", "Groms", "Wobs"),
  cat.Var, c("High End", "Mid Range", "Low End"),
  values,
  bar_width = 0.5,
  stacked = F
)

ggplot(my_prep_df_neg) + 
  geom_rect(aes(
    x = group_id,
    fill = cat.Var,
    color = segment_top < segment_bottom,
    xmin = segment_left,
    xmax = segment_right,
    ymin = segment_bottom,
    ymax = segment_top,
  )) + 
  geom_segment(aes(
    x = bar_right,
    y = bar_top,
    xend = bar_right + 1.5, # this is the spacing between bars (by default this is same as bar_width)
    yend = bar_top,
  )) +
  scale_x_continuous(breaks = unique(my_prep_df_neg$group_id), 
                     labels = levels(my_prep_df_neg$x.axis.Var)) + 
  scale_color_manual(values = c("Green", "Red"))



# raise error/warning if values_var has negative numbers and stacked == T 
# this implies that if steps are negative the waterfall should be side-by-side



## different number of x vars ----

df_n <- tibble(
  x.axis.Var = rep(c("Widgets", "Gridgets", "Groms"), 3),
  cat.Var = rep(c("High End", "Mid Range", "Low End"), each = 3),
  values = c(600, 500, -300, # high end
             -200, -800, 300, # mid range
             100, 80, 200   # low end
  )
)
df_n

my_prep_df_n <- my_prepare_df_facet(
  df_n, 
  x.axis.Var, c("Widgets", "Gridgets", "Groms"),
  cat.Var, c("High End", "Mid Range", "Low End"),
  values,
  bar_width = 0.5,
  stacked = F
)

ggplot(my_prep_df_n) + 
  geom_rect(aes(
    x = group_id,
    fill = cat.Var,
    color = segment_top < segment_bottom,
    xmin = segment_left,
    xmax = segment_right,
    ymin = segment_bottom,
    ymax = segment_top,
  )) + 
  geom_segment(aes(
    x = bar_right,
    y = bar_top,
    xend = bar_right + 1.5, # this is the spacing between bars (by default this is same as bar_width)
    yend = bar_top,
  )) +
  scale_x_continuous(breaks = unique(my_prep_df_n$group_id), 
                     labels = levels(my_prep_df_n$x.axis.Var)) + 
  scale_color_manual(values = c("Green", "Red"))


## different number of cat vars ----

# odd categories
df_nc <- tibble(
  x.axis.Var = rep(c("Widgets", "Gridgets", "Groms", "Wobs"), 5),
  cat.Var = rep(c("High End", "Mid Range", "Low End", "l4", "l5"), each = 4),
  values = c(600, 500, -300, 200, # high end
             -200, -800, 300, 250, # mid range
             100, 80, 200, -150,   # low end
             300, -80, 30, -150,   # l4
             -100, 250, -10, -150   # l5
  )
)
df_nc

my_prep_df_nc <- my_prepare_df_facet(
  df_nc, 
  x.axis.Var, c("Widgets", "Gridgets", "Groms", "Wobs"),
  cat.Var, c("High End", "Mid Range", "Low End", "l4", "l5"),
  values,
  bar_width = 0.5,
  stacked = F
)

ggplot(my_prep_df_nc) + 
  geom_rect(aes(
    x = group_id,
    fill = cat.Var,
    color = segment_top < segment_bottom,
    xmin = segment_left,
    xmax = segment_right,
    ymin = segment_bottom,
    ymax = segment_top,
  )) + 
  geom_segment(aes(
    x = bar_right,
    y = bar_top,
    xend = bar_right + 2.5, # this is the spacing between bars (by default this is same as bar_width)
    yend = bar_top,
  )) +
  scale_x_continuous(breaks = unique(my_prep_df_nc$group_id), 
                     labels = levels(my_prep_df_nc$x.axis.Var)) + 
  scale_color_manual(values = c("Green", "Red"))


# even categories
df_nc2 <- tibble(
  x.axis.Var = rep(c("Widgets", "Gridgets", "Groms", "Wobs"), 2),
  cat.Var = rep(c("High End", "Mid Range"), each = 4),
  values = c(600, 500, -300, 200, # high end
             -200, -800, 300, 250 # mid range
  )
)
df_nc2


my_prep_df_nc2 <- my_prepare_df_facet(
  df_nc2, 
  x.axis.Var, c("Widgets", "Gridgets", "Groms", "Wobs"),
  cat.Var, c("High End", "Mid Range"),
  values,
  bar_width = 0.5,
  stacked = F
)

ggplot(my_prep_df_nc2) + 
  geom_rect(aes(
    x = group_id,
    fill = cat.Var,
    color = segment_top < segment_bottom,
    xmin = segment_left,
    xmax = segment_right,
    ymin = segment_bottom,
    ymax = segment_top,
  )) + 
  geom_segment(aes(
    x = bar_right,
    y = bar_top,
    xend = bar_right + 1, # this is the spacing between bars (by default this is same as bar_width)
    yend = bar_top,
  )) +
  scale_x_continuous(breaks = unique(my_prep_df_nc2$group_id), 
                     labels = levels(my_prep_df_nc2$x.axis.Var)) + 
  scale_color_manual(values = c("Green", "Red"))


## faceting ----

df_fac <- tibble(
  x.axis.Var = rep(c("Widgets", "Gridgets", "Groms", "Wobs"), 3),
  cat.Var = rep(c("High End", "Mid Range", "Low End"), each = 4),
  values = c(600, 500, -300, 200, # high end
             -200, -800, 300, 250, # mid range
             100, 80, 200, -150   # low end
  )
)
df_fac <- bind_rows(df_fac, df_fac)
df_fac <- df_fac %>%
  mutate(facet_col = rep(c("l1","l2"), each = nrow(df_fac)/2),
         values = if_else(facet_col == "l1", values + rnorm(nrow(df_fac), 0, 500), values))
df_fac

my_prep_df_fac <- my_prepare_df_facet(
  df_fac, 
  x.axis.Var, c("Widgets", "Gridgets", "Groms", "Wobs"),
  cat.Var, c("High End", "Mid Range", "Low End"),
  values,
  facet_col,
  bar_width = 0.5,
  stacked = F
)
# my_prep_df_fac <- bind_rows(my_prep_df_fac, my_prep_df_fac)
# my_prep_df_fac <- my_prep_df_fac %>% 
#   mutate(facet_col = rep(c("l1","l2"), each = nrow(my_prep_df_fac)/2))
my_prep_df_fac


ggplot(my_prep_df_fac) + 
  geom_rect(aes(
    x = group_id,
    fill = cat.Var,
    color = segment_top < segment_bottom,
    xmin = segment_left,
    xmax = segment_right,
    ymin = segment_bottom,
    ymax = segment_top,
  )) + 
  geom_segment(aes(
    x = bar_right,
    y = bar_top,
    xend = bar_right + 1.5, # this is the spacing between bars (by default this is same as bar_width)
    yend = bar_top,
  )) +
  facet_wrap(~facet_col) + 
  scale_x_continuous(breaks = unique(my_prep_df_fac$group_id), 
                     labels = levels(my_prep_df_fac$x.axis.Var)) + 
  scale_color_manual(values = c("Green", "Red"))


