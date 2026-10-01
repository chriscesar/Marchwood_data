# plank_00_import_data.R ----

# load packages ----
ld_pkgs <- c("tidyverse","tictoc")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)
tictoc::tic.clearlog() ##clear log

ggplot2::theme_set(ggthemes::theme_few())

tictoc::tic("Load data")
# Load data ----
df0 <- readxl::read_xlsx(
  path = "data_raw/Marchwood boat plankton up to 05-2026..xlsx",
  sheet = "All"
    ) %>% janitor::clean_names()

tictoc::toc(log=TRUE)

# widen by taxa & plot ----
df0 %>% 
  dplyr::select(c(id_to_sort_by,date,site,trawl_number,a_or_b_sample,
                  species,number_per_100m3)) %>% 
  dplyr::group_by(dplyr::across(-number_per_100m3)) %>% 
  summarise(
    number_per_100m3 = sum(number_per_100m3),
    .groups = "drop") %>% ungroup() %>% 
  tidyr::pivot_wider(.,
                     names_from = species,
                     values_from = number_per_100m3,
                     values_fill = 0
                     ) %>% 
  # back to long
  tidyr::pivot_longer(.,
                      cols = -c(id_to_sort_by, date,
                                site, trawl_number,
                                a_or_b_sample)
                      ) %>% 
  dplyr::filter(!name %in% c("Unknown", "No sample", "Mud")) %>% 
  dplyr::filter(value != 0) %>%
  ggplot(.,
         aes(x = date,
             y = log(value+1),
             group = name))+
  geom_point(aes(
    colour = site
  ))+
  geom_line(aes(group=name,
                colour = site
                ),
            alpha = 0.2
            )+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = 2),
    axis.text.y = element_text(face = 2),
    axis.text.x = element_text(face = 2, size = 12),
    strip.text = element_text(face = 2),
    legend.title = element_blank(),
    legend.text = element_text(face = 2),
  )

# widen by taxa & boxplot ----
df0 %>% 
  dplyr::select(c(id_to_sort_by,date,site,trawl_number,a_or_b_sample,
                  species,number_per_100m3)) %>% 
  dplyr::group_by(dplyr::across(-number_per_100m3)) %>% 
  summarise(
    number_per_100m3 = sum(number_per_100m3),
    .groups = "drop") %>% ungroup() %>% 
  tidyr::pivot_wider(.,
                     names_from = species,
                     values_from = number_per_100m3,
                     values_fill = 0
  ) %>% 
  # back to long
  tidyr::pivot_longer(.,
                      cols = -c(id_to_sort_by, date,
                                site, trawl_number,
                                a_or_b_sample)
  ) %>% 
  dplyr::filter(!name %in% c("Unknown", "No sample", "Mud")) %>% 
  dplyr::filter(value != 0) %>%
  ggplot(.,
         aes(x = factor(year(date)),
             y = log(value+1),
             ))+
  geom_boxplot(aes(
    colour = site
  ))+
  # geom_line(aes(group=name,
  #               colour = site
  # ),
  # alpha = 0.2
  # )+
  facet_wrap(.~site)+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = 2),
    axis.text.y = element_text(face = 2),
    axis.text.x = element_text(face = 2, size = 12),
    strip.text = element_text(face = 2),
    legend.title = element_blank(),
    legend.text = element_text(face = 2),
  )

# widen by type & plot ----
df0 %>% 
  dplyr::select(c(id_to_sort_by,date,site,trawl_number,a_or_b_sample,
                  type,number_per_100m3)) %>% 
  dplyr::group_by(dplyr::across(-number_per_100m3)) %>% 
  summarise(
    number_per_100m3 = sum(number_per_100m3),
    .groups = "drop") %>% ungroup() %>% 
  tidyr::pivot_wider(.,
                     names_from = type,
                     values_from = number_per_100m3,
                     values_fill = 0
  ) %>% 
  # back to long
  tidyr::pivot_longer(.,
                      cols = -c(id_to_sort_by, date,
                                site, trawl_number,
                                a_or_b_sample)
  ) %>% 
  dplyr::filter(!name %in% c("Unknown", "No sample", "Mud")) %>% 
  dplyr::filter(value != 0) %>% 
  ggplot(.,
         aes(x = date,
             # y = log(value),
             y = value/100,
             group = name))+
  geom_point(aes(
    colour = site
  )) +
  facet_wrap(.~name, scales = "free_y")+
  geom_smooth(aes(group = site,colour = site), se=FALSE)+
  labs(
    y = "Abundance per m3"
  ) +
  ylim(0,NA)+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(face = 2),
    axis.text.y = element_text(face = 2),
    axis.text.x = element_text(face = 2, size = 12),
    strip.text = element_text(face = 2),
    legend.title = element_blank(),
    legend.text = element_text(face = 2),
  )
