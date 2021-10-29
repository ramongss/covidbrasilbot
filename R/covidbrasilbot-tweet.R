# Treat data ----
# set portuguese language
Sys.setlocale("LC_ALL", "Portuguese")

# load libraries
devtools::install_github("hadley/emo")
library(extrafont)

# dowload and treat data
data_br <-
  data.table::fread(input = "https://data.brasil.io/dataset/covid19/caso_full.csv.gz",
                    encoding = "UTF-8") |>
  dplyr::filter(place_type == "state") |>
  dplyr::group_by(date) |>
  dplyr::summarize(new_confirmed = sum(new_confirmed),
                   new_deaths = sum(new_deaths)) |>
  dplyr::mutate(date = as.Date(date),
                ma_conf = zoo::rollmean(new_confirmed, k = 7, fill = NA, align = "right"),
                ma_deaths = zoo::rollmean(new_deaths, k = 7, fill = NA, align = "right")) |>
  dplyr::ungroup() |>
  as.data.frame()

# function to round up the plot breaks
roundUp <- function(x, to = 10) {
  to*(x%/%to + as.logical(x%%to))
}

# plot function - deaths
ma_plot_deaths <- function(data, colour = "red") {
  ggplot2::ggplot(data = data, ggplot2::aes(x = as.Date(date))) +
    ggplot2::geom_col(
      ggplot2::aes(y = new_deaths),
      size = 1,
      fill = colour,
      alpha = .3
    ) +
    ggplot2::geom_line(ggplot2::aes(y = ma_deaths), group = 1, size = 3.5, color = "white") +
    ggplot2::geom_line(
      ggplot2::aes(y = ma_deaths,
          lty = "Média móvel de mortes diárias nos últimos sete dias no Brasil"),
      group = 1, size = 1.2, color = colour
    ) +
    ggplot2::geom_point(
      data = data[which(data$new_deaths == max(data$new_deaths)),],
      ggplot2::aes(y = new_deaths), size = 4, color = colour
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        y = new_deaths,
        label = ifelse(new_deaths == max(new_deaths), scales::number(max(new_deaths)), "")
      ), vjust = -1, family = "Times New Roman", size = 5
    ) +
    ggplot2::scale_x_date(
      date_breaks = '1 month',
      date_labels = '%b %Y'
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 25, family = 'Times New Roman'),
      axis.title = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white"),
      axis.line.x.bottom = ggplot2::element_line(size = 1),
      panel.grid.major.y = ggplot2::element_line(size = 0.5, color = 'gray95'),
      panel.grid.minor.y = ggplot2::element_line(size = 0.5, color = 'gray95'),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(vjust = -0.3, color = "gray50"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.ticks.x = ggplot2::element_line(size = 1),
      axis.ticks.length = ggplot2::unit(.25, "cm"),
      legend.text = ggplot2::element_text(size = 18, color = "gray50"),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.position = 'top',
      legend.justification = 'left',
      plot.caption = ggplot2::element_text(size = 12, color = "gray50"),
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0,0),
      breaks = seq(0, roundUp(max(data$new_deaths), 500), 1000),
      labels = scales::number_format()
    ) +
    ggplot2::annotate(geom = 'text', x = as.Date(min(data$date)),
             y = max(seq(0, roundUp(max(data$new_deaths), 500), 1000)),
             hjust = .30, vjust = -.30,
             label = 'mortes diárias', color = 'gray50',
             family = 'Times New Roman', size = 7) +
    ggplot2::labs(
      caption = paste0(
        'Fonte: Brasil.IO COVID-19 Dataset - Última atualização\n',
        format(Sys.time(), '%d %b %Y, %H:%M'),
        '(UTC) | Twitter: @covidbrasilbot'
      )
    )+
    ggplot2::coord_cartesian(
      ylim = c(0, roundUp(max(data$new_deaths), 300)+300)
    )
}

# save figure deaths
ma_plot_deaths(data_br, colour = "#9B0000") |>
  ggplot2::ggsave(filename = here::here("figures","deaths.png"),
                  device = "png",
                  units = "in",
                  width = 12,
                  height = 6.75)

# plot function - confirmed cases
ma_plot_conf <- function(data, colour = "blue") {
  ggplot2::ggplot(data = data, ggplot2::aes(x = as.Date(date))) +
    ggplot2::geom_col(
      ggplot2::aes(y = new_confirmed),
      size = 1,
      fill = colour,
      alpha = .3
    ) +
    ggplot2::geom_line(ggplot2::aes(y = ma_conf), group = 1, size = 3.5, color = "white") +
    ggplot2::geom_line(
      ggplot2::aes(y = ma_conf,
      lty = "Média móvel dos casos confirmados diários nos últimos sete dias no Brasil"),
      group = 1, size = 1.2, color = colour
    ) +
    ggplot2::geom_point(
      data = data[which(data$new_confirmed == max(data$new_confirmed)),],
      ggplot2::aes(y = new_confirmed), size = 4, color = colour
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        y = new_confirmed,
        label = ifelse(new_confirmed == max(new_confirmed), scales::number(max(new_confirmed)), "")
      ), vjust = -1, family = "Times New Roman", size = 5
    ) +
    ggplot2::scale_x_date(
      date_breaks = '1 month',
      date_labels = '%b %Y'
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 25, family = 'Times New Roman'),
      axis.title = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white"),
      axis.line.x.bottom = ggplot2::element_line(size = 1),
      panel.grid.major.y = ggplot2::element_line(size = 0.5, color = 'gray95'),
      panel.grid.minor.y = ggplot2::element_line(size = 0.5, color = 'gray95'),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(vjust = -0.3, color = "gray50"),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.ticks.x = ggplot2::element_line(size = 1),
      axis.ticks.length = ggplot2::unit(.25, "cm"),
      legend.text = ggplot2::element_text(size = 18, color = "gray50"),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.position = 'top',
      legend.justification = 'left',
      plot.caption = ggplot2::element_text(size = 12, color = "gray50"),
    ) +
    ggplot2::scale_y_continuous(
      expand = c(0,0),
      breaks = seq(0, roundUp(max(data$new_confirmed), 1000), 25000),
      labels = scales::number_format()
    ) +
    ggplot2::annotate(geom = 'text', x = as.Date(min(data$date)),
             y = max(roundUp(max(data$new_confirmed), 1000)),
             hjust = .15, vjust = -.30,
             label = 'casos diários', color = 'gray50',
             family = 'Times New Roman', size = 7) +
    ggplot2::labs(
      caption = paste0(
        'Fonte: Brasil.IO COVID-19 Dataset - Última atualização\n',
        format(Sys.time(), '%d %b %Y, %H:%M'),
        '(UTC) | Twitter: @covidbrasilbot'
      )
    )+
    ggplot2::coord_cartesian(
      ylim = c(0, roundUp(max(data$new_confirmed), 500)+20000)
    )
}

# save figure confirmed cases
ma_plot_conf(data_br, colour = "#2171B5") |>
  ggplot2::ggsave(filename = here::here("figures","confirmed.png"),
                  device = "png",
                  units = "in",
                  width = 12,
                  height = 6.75)


c <- ma_plot_conf(data_br, colour = "#2171B5")
d <- ma_plot_deaths(data_br, colour = "#9B0000")

figure <- ggpubr::ggarrange(c, d, ncol = 2)

figure |>
  ggplot2::ggsave(filename = here::here("figures","combined.png"),
                  device = "png",
                  units = "in",
                  width = 24,
                  height = 6.75)

# Tweet report ----
# create Twitter token
covidbrasilbot_token <- rtweet::create_token(
  app = "covidbrasilbot",
  consumer_key = Sys.getenv("TWITTER_CONSUMER_API_KEY"),
  consumer_secret = Sys.getenv("TWITTER_CONSUMER_API_SECRET"),
  access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
  access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET")
)

# build the status message
tweet_text <-
  paste0(
    emo::ji("calendar"),
    " ",
    format.Date(max(data_br$date), "%d %B de %Y"),
    "\n\n",
    emo::ji("brazil"),
    " Brasil soma ",
    format(sum(data_br$new_confirmed), nsmall=1, big.mark=".", decimal.mark = ","),
    " casos confirmados e ",
    format(sum(data_br$new_deaths), nsmall=1, big.mark=".", decimal.mark = ","),
    " óbitos, com ",
    format(tail(data_br$new_confirmed, 1), nsmall=1, big.mark=".", decimal.mark = ","),
    " novos casos e ",
    format(tail(data_br$new_deaths, 1), nsmall=1, big.mark=".", decimal.mark = ","),
    " novos óbitos. \n\n",
    emo::ji("bar_chart"),
    " A incidência máxima de: \n",
    emo::ji("up_arrow"),
    " casos confirmados foi de ",
    format(max(data_br$new_confirmed), nsmall=1, big.mark=".", decimal.mark = ","),
    " em ",
    format.Date(data_br[which(data_br$new_confirmed == max(data_br$new_confirmed)), 'date'], "%d %B de %Y"),
    "; \n",
    emo::ji("up_arrow"),
    " óbitos foi de ",
    format(max(data_br$new_deaths), nsmall=1, big.mark=".", decimal.mark = ","),
    " em ",
    format.Date(data_br[which(data_br$new_deaths == max(data_br$new_deaths)), 'date'], "%d %B de %Y"),
    "."
  )

# post the image to Twitter
rtweet::post_tweet(status = tweet_text,
                   media = here::here("figures","combined.png"),
                   token = covidbrasilbot_token)
