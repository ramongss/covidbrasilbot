# readr
# dplyr
# zoo
# ggplot2
# scales
# ggpubr
# here
# rtweet

# supress warnings
options(warn = -1)

# Treat data ----

# dowload and treat data
url <- paste0(
  "https://raw.githubusercontent.com/",
  "wcota/covid19br/master/cases-brazil-states.csv"
)

print(paste0(Sys.time(), " - Retrieving data from 'wcota/covid19br'..."))

data_br <-
  readr::read_csv(url, show_col_types = FALSE) |>
  dplyr::filter(state == "TOTAL") |>
  dplyr::select(date, new_confirmed = newCases, new_deaths = newDeaths) |>
  dplyr::mutate(
    ma_conf = zoo::rollmean(new_confirmed, k = 7, fill = NA, align = "right"),
    ma_deaths = zoo::rollmean(new_deaths, k = 7, fill = NA, align = "right")
  )

print(paste0(Sys.time(), " - Creating plot and saving figure..."))

# plot function - deaths
ma_plot_deaths <- function(data, colour = "red") {
  ggplot2::ggplot(data = data, ggplot2::aes(x = as.Date(date))) +
  ggplot2::geom_col(
    ggplot2::aes(y = new_deaths),
    size = 1,
    fill = colour,
    alpha = .3
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = ma_deaths), group = 1, size = 3.5, color = "white"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
      y = ma_deaths,
      lty = "Média móvel de mortes diárias nos últimos sete dias no Brasil"
    ), group = 1, size = 1.2, color = colour
  ) +
  ggplot2::geom_point(
    data = data[data$new_deaths == max(data$new_deaths), ],
    ggplot2::aes(y = new_deaths), size = 4, color = colour
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      y = new_deaths,
      label = ifelse(
        new_deaths == max(new_deaths),
        scales::number(max(new_deaths)), ""
      )
    ), vjust = -1, size = 5
  ) +
  ggplot2::scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  ggplot2::theme(
    text = ggplot2::element_text(size = 25),
    axis.title = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "white"),
    axis.line.x.bottom = ggplot2::element_line(size = 1),
    panel.grid.major.y = ggplot2::element_line(size = 0.5, color = "gray95"),
    panel.grid.minor.y = ggplot2::element_line(size = 0.5, color = "gray95"),
    panel.grid.major.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(vjust = -0.3, color = "gray50"),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    axis.ticks.x = ggplot2::element_line(size = 1),
    axis.ticks.length = ggplot2::unit(.25, "cm"),
    legend.text = ggplot2::element_text(size = 18, color = "gray50"),
    legend.key = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = "top",
    legend.justification = "left",
    plot.caption = ggplot2::element_text(size = 12, color = "gray50"),
  ) +
  ggplot2::scale_y_continuous(
    expand = c(0, 0),
    labels = scales::number_format()
  ) +
  ggplot2::labs(
    caption = paste0(
      "Fonte: https://github.com/wcota/covid19br - Última atualização\n",
      format(
        `attr<-`(Sys.time(), "tzone", "America/Sao_Paulo"),
        "%d %b %Y, %H:%M"
      ),
      " (UTC -3:00) | Twitter: @covidbrasilbot"
    )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, max(data$new_deaths) * 1.1)
  )
}

# plot function - confirmed cases
ma_plot_conf <- function(data, colour = "blue") {
  ggplot2::ggplot(data = data, ggplot2::aes(x = as.Date(date))) +
  ggplot2::geom_col(
    ggplot2::aes(y = new_confirmed),
    size = 1,
    fill = colour,
    alpha = .3
  ) +
  ggplot2::geom_line(
    ggplot2::aes(y = ma_conf), group = 1, size = 3.5, color = "white"
  ) +
  ggplot2::geom_line(
    ggplot2::aes(
    y = ma_conf,
    lty = "Média móvel dos casos diários nos últimos sete dias no Brasil"
    ), group = 1, size = 1.2, color = colour
  ) +
  ggplot2::geom_point(
    data = data[data$new_confirmed == max(data$new_confirmed), ],
    ggplot2::aes(y = new_confirmed), size = 4, color = colour
  ) +
  ggplot2::geom_text(
    ggplot2::aes(
      y = new_confirmed,
      label = ifelse(
        new_confirmed == max(new_confirmed),
        scales::number(max(new_confirmed)), ""
      )
    ), vjust = -1, size = 5
  ) +
  ggplot2::scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y"
  ) +
  ggplot2::theme(
    text = ggplot2::element_text(size = 25),
    axis.title = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "white"),
    axis.line.x.bottom = ggplot2::element_line(size = 1),
    panel.grid.major.y = ggplot2::element_line(size = 0.5, color = "gray95"),
    panel.grid.minor.y = ggplot2::element_line(size = 0.5, color = "gray95"),
    panel.grid.major.x = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_text(vjust = -0.3, color = "gray50"),
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    axis.ticks.x = ggplot2::element_line(size = 1),
    axis.ticks.length = ggplot2::unit(.25, "cm"),
    legend.text = ggplot2::element_text(size = 18, color = "gray50"),
    legend.key = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.position = "top",
    legend.justification = "left",
    plot.caption = ggplot2::element_text(size = 12, color = "gray50"),
  ) +
  ggplot2::scale_y_continuous(
    expand = c(0, 0),
    labels = scales::number_format()
  ) +
  ggplot2::labs(
    caption = paste0(
      "Fonte: https://github.com/wcota/covid19br - Última atualização\n",
      format(
        `attr<-`(Sys.time(), "tzone", "America/Sao_Paulo"),
        "%d %b %Y, %H:%M"
      ),
      " (UTC -3:00) | Twitter: @covidbrasilbot"
    )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, max(data$new_confirmed) * 1.1)
  )
}

# plot figures
c <- ma_plot_conf(data_br, colour = "#2171B5")
d <- ma_plot_deaths(data_br, colour = "#9B0000")

# join figures
figure <- ggpubr::ggarrange(c, d, ncol = 2)

# save figure
figure |>
  ggplot2::ggsave(
    filename = here::here("figures", "fig.png"),
    device = "png",
    units = "in",
    width = 24,
    height = 6.75
  )

# Tweet report ----

print(paste0(Sys.time(), " - Enviando tweet..."))

# create Twitter token
covidbrasilbot_token <- rtweet::create_token(
  app = "covidbrasilbot",
  consumer_key = Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

# create Tweet status
tweet_text <-
  paste0(
    "📅 ", format.Date(max(data_br$date), "%d de %b. %Y"),
    "\n\n🇧🇷 #Brasil soma ",
    format(sum(data_br$new_confirmed), big.mark = ".", decimal.mark = ","),
    " casos confirmados e ",
    format(sum(data_br$new_deaths), big.mark = ".", decimal.mark = ","),
    " óbitos por #COVID19, com ",
    format(tail(data_br$new_confirmed, 1), big.mark = ".", decimal.mark = ","),
    " novos casos e ",
    format(tail(data_br$new_deaths, 1), big.mark = ".", decimal.mark = ","),
    " novos óbitos.\n\n",
    "📊 A incidência máxima de:\n",
    "🤒️ casos foi de ",
    format(max(data_br$new_confirmed), big.mark = ".", decimal.mark = ","),
    " em ",
    format.Date(dplyr::pull(data_br[data_br["new_confirmed"] == max(data_br$new_confirmed), "date"], date), "%d %b. %Y"), #nolint
    ";\n",
    "💀️ óbitos foi de ",
    format(max(data_br$new_deaths), big.mark = ".", decimal.mark = ","),
    " em ",
    format.Date(dplyr::pull(data_br[data_br["new_deaths"] == max(data_br$new_deaths), "date"], date), "%d %b. %Y"), #nolint
    "."
  )

# post the image to Twitter
rtweet::post_tweet(
  status = tweet_text,
  media = here::here("figures", "fig.png"),
  token = covidbrasilbot_token
)

print(paste0(Sys.time(), " - Tweet enviado!"))