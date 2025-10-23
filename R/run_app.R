#' Run the Shiny Application
#'
#' This function launches the Shiny application for the 'Trust Game' paper's
#' interactive appendix.
#'
#' @export
#' @import shiny
#' @importFrom bslib bs_theme tooltip
#' @importFrom DT datatable dataTableOutput renderDataTable
#' @importFrom ggplot2 ggplot aes geom_bar geom_text geom_vline annotate labs theme_minimal theme element_text coord_flip scale_y_continuous expansion
#' @importFrom dplyr %>% filter group_by summarise n n_distinct arrange desc slice_head mutate left_join pull select rename any_of distinct
#' @importFrom tidyr separate_rows
#' @importFrom stringr str_wrap str_to_title
#' @importFrom htmltools withTags tags
#' @importFrom stats setNames reorder

run_app <- function() {

  # ===================================================================
  # PRÉPARER LES DONNÉES POUR LES CONTRÔLES (logique déplacée ici)
  # ===================================================================
  cluster_full_names <- c(
    "Social Correlates of Trust", "Social Preferences and Trust", "Structural Foundations of Trust",
    "Social Psychology of Trust", "Neurocognitive Foundations of Trust", "Complex Systems",
    "Measurement and Validity", "Online Social Psychology of Trust", "Socio-demographic Determinants",
    "Social Thermometer", "The Psychology of Promises", "Trust at First Sight"
  )
  cluster_display_names <- sub("^\\[.*?\\]\\s*", "", cluster_full_names)
  cluster_choices <- stats::setNames(cluster_full_names, cluster_display_names)

  # ===================================================================
  # UI
  # ===================================================================
  ui <- shiny::navbarPage(
    "Online Appendix",
    theme = bslib::bs_theme(bootswatch = "lumen"),

    shiny::tabPanel("Home",
                    shiny::fluidPage(
                      htmltools::tags$head(
                        htmltools::tags$style(shiny::HTML("
            .home-section {
              padding-top: 20px;
              padding-bottom: 20px;
              border-bottom: 1px solid #eee;
            }
            .home-section h2 {
              color: #0056b3;
              border-bottom: 2px solid #0056b3;
              padding-bottom: 10px;
              margin-bottom: 20px;
            }
            .citation-box {
              background-color: #f8f9fa;
              border-left: 5px solid #007bff;
              padding: 15px;
              margin-top: 15px;
              font-style: italic;
            }
          "))
                      ),

                      shiny::titlePanel("An Interactive Appendix for 'The Trust Game: A Historical and Methodological Analysis on the Frontier of Experimental and Behavioral Economics'"),

                      shiny::div(class = "home-section",
                                 shiny::h2("About the Article"),
                                 shiny::p(shiny::strong("Authors:"), "Author 1, Author 2, and Author 3."),
                                 shiny::p(shiny::strong("Abstract:")),
                                 shiny::p("This paper provides a life-cycle analysis of the Trust Game, using its trajectory as a lens to clarify the boundaries between experimental and behavioral economics. We first trace its 1995 creation by Berg et al. as a challenge to calculative trust paradigms. A bibliometric study then maps its diffusion, revealing two divergent paths in economics: one, rooted in experimental economics, prioritizes measurement; the other, in behavioral economics, theory-testing. These paths differ in methods and validity standards, constituting an epistemic divide that illuminates the fields' evolving relationship."),

                                 shiny::div(class = "citation-box",
                                            shiny::p(shiny::strong("How to cite this work:")),
                                            shiny::p("Author, A., Author, B., & Author, C. (Year). Title of the article. ",
                                                     htmltools::tags$em("Journal Name, Volume"), "(Issue), pages."),
                                            shiny::p(shiny::strong("DOI: "),
                                                     htmltools::tags$a(href = "https://doi.org/YOUR_DOI_HERE", "https://doi.org/YOUR_DOI_HERE", target = "_blank"))
                                 )
                      ),

                      shiny::div(class = "home-section",
                                 shiny::h2("How to Use This Interactive Appendix"),
                                 shiny::p("This web application is designed to provide an interactive exploration of the research clusters discussed in our paper.
              It allows you to dynamically filter and view the data behind the figures."),
                                 shiny::p("Navigate to the ", shiny::strong("Alluvial Explorer"), " tab to begin. You will find:"),
                                 htmltools::tags$ul(
                                   htmltools::tags$li(shiny::strong("Overall View:"), " Provides a global perspective on the entire corpus of articles,
                        showing distributions by year, discipline, country, and journal."),
                                   htmltools::tags$li(shiny::strong("Cluster View:"), " Allows you to select a specific research cluster from the dropdown menu.
                        All plots and tables will then dynamically update to reflect only the articles belonging to that cluster.
                        This is useful for in-depth analysis of a particular research stream.")
                                 )
                      ),

                      shiny::div(class = "home-section",
                                 shiny::h2("Code, Data, and Contact"),
                                 shiny::p("We believe in open and reproducible science. All resources related to this project are publicly available."),
                                 htmltools::tags$ul(
                                   htmltools::tags$li(
                                     shiny::icon("github"), " ",
                                     shiny::strong("Shiny App Code:"), " The source code for this interactive application is available on ",
                                     htmltools::tags$a(href = "https://github.com/YOUR_USERNAME/YOUR_APP_REPO", "GitHub.", target = "_blank")
                                   ),
                                   htmltools::tags$li(
                                     shiny::icon("github"), " ",
                                     shiny::strong("Data Processing Code:"), " The scripts used for data collection and preparation are available in a separate ",
                                     htmltools::tags$a(href = "https://github.com/YOUR_USERNAME/YOUR_DATA_REPO", "repository on GitHub.", target = "_blank")
                                   ),
                                   htmltools::tags$li(
                                     shiny::icon("database"), " ",
                                     shiny::strong("Data Source:"), " The bibliographic data was extracted from the Web of Science Core Collection on [Date].
                  A processed version of the data used in this app is available at [Link to Zenodo/OSF/Data Repository if applicable]."
                                   ),
                                   htmltools::tags$li(
                                     shiny::icon("envelope"), " ",
                                     shiny::strong("Contact:"), " For any questions, suggestions, or issues, please contact [Your Name] at ",
                                     htmltools::tags$a(href = "mailto:your.email@university.edu", "your.email@university.edu.")
                                   )
                                 )
                      )
                    )
    ),

    shiny::tabPanel("Alluvial Explorer",
                    shiny::fluidPage(
                      shiny::titlePanel("Evolution of Research Clusters"),

                      shiny::fluidRow(
                        shiny::column(3,
                                      shiny::div(class = "sidebar-panel",
                                                 shiny::wellPanel(
                                                   shiny::h4("Explore Data by:"),
                                                   shiny::radioButtons(
                                                     inputId = "mode_select",
                                                     label = "Choose exploration mode:",
                                                     choices = c("Overall View" = "overall", "Cluster" = "cluster"),
                                                     selected = "overall"
                                                   ),
                                                   shiny::conditionalPanel(
                                                     condition = "input.mode_select == 'cluster'",
                                                     shiny::selectInput(
                                                       inputId = "cluster_select",
                                                       label = "Select a cluster:",
                                                       choices = c("Select a cluster..." = "", cluster_choices)
                                                     )
                                                   ),
                                                   shiny::hr(),
                                                   shiny::htmlOutput("selection_info")
                                                 )
                                      )
                        ),

                        shiny::column(9,
                                      shiny::div(class = "main-panel-custom",
                                                 shiny::div(class = "alluvial-container",
                                                            shiny::div(
                                                              shiny::imageOutput("alluvial_image", width = "100%", height = "auto"),
                                                              style = "text-align: center;"
                                                            )
                                                 )
                                      )
                        )
                      ),

                      shiny::div(class = "full-width-section",

                                 shiny::conditionalPanel(
                                   condition = "(input.mode_select == 'cluster' && input.cluster_select != '') || input.mode_select == 'overall'",
                                   shiny::div(class = "section-title", shiny::h4("Article Distribution per Year")),
                                   shiny::plotOutput("year_distribution_plot", height = "400px"),
                                   shiny::br(), shiny::hr()
                                 ),

                                 shiny::conditionalPanel(
                                   condition = "(input.mode_select == 'cluster' && input.cluster_select != '') || input.mode_select == 'overall'",
                                   shiny::fluidRow(
                                     shiny::column(4,
                                                   shiny::div(class = "section-title",
                                                              shiny::div(style = "display: inline-block;",
                                                                         shiny::h4(style = "display: inline; margin-right: 5px;", "Most Frequent Discipline Tags"),
                                                                         bslib::tooltip(
                                                                           trigger = shiny::icon("info-circle", class = "info-icon"),
                                                                           shiny::HTML("Since articles can have multiple tags, percentages may sum to more than 100%.<br>
                                        Reading example: 46% of articles in the selection contain the tag 'Economics'."),
                                                                           placement = "top"
                                                                         )
                                                              )
                                                   ),
                                                   shiny::plotOutput("discipline_plot", height = "400px")
                                     ),
                                     shiny::column(4,
                                                   shiny::div(class = "section-title",
                                                              shiny::div(style = "display: inline-block;",
                                                                         shiny::h4(style = "display: inline; margin-right: 5px;", "Most Represented Countries"),
                                                                         bslib::tooltip(
                                                                           trigger = shiny::icon("info-circle", class = "info-icon"),
                                                                           "Since an article can have authors from multiple countries, percentages may sum to more than 100%.
                                    Reading example: 46% of articles in the selection have at least one author affiliated with 'United States'.",
                                                                           placement = "top"
                                                                         )
                                                              )
                                                   ),
                                                   shiny::plotOutput("country_plot", height = "400px")
                                     ),
                                     shiny::column(4,
                                                   shiny::div(class = "section-title",
                                                              shiny::div(style = "display: inline-block;",
                                                                         shiny::h4(style = "display: inline; margin-right: 5px;", "Most Represented Journals"),
                                                                         bslib::tooltip(
                                                                           trigger = shiny::icon("info-circle", class = "info-icon"),
                                                                           "This shows the top 10 journals that publish articles in this selection.
                                    Reading example: 15% of articles in the selection are published in 'Journal of Economic Behavior'.",
                                                                           placement = "top"
                                                                         )
                                                              )
                                                   ),
                                                   shiny::plotOutput("journal_plot", height = "400px")
                                     )
                                   ),
                                   shiny::br(), shiny::hr()
                                 ),

                                 shiny::conditionalPanel(
                                   condition = "(input.mode_select == 'cluster' && input.cluster_select != '') || input.mode_select == 'overall'",
                                   shiny::fluidRow(
                                     shiny::column(9,
                                                   shiny::div(class = "section-title",
                                                              shiny::div(style = "display: inline-block;",
                                                                         shiny::h4(style = "display: inline; margin-right: 5px;", shiny::textOutput("table_title", inline = TRUE)),
                                                                         bslib::tooltip(
                                                                           trigger = shiny::icon("info-circle", class = "info-icon"),
                                                                           "All articles in the selection are shown here, sorted by citation count. Use search and filters to explore specific entries.",
                                                                           placement = "top"
                                                                         )
                                                              )
                                                   ),
                                                   DT::dataTableOutput("cluster_table")
                                     ),
                                     shiny::column(3,
                                                   shiny::div(class = "section-title",
                                                              shiny::div(style = "display: inline-block;",
                                                                         shiny::h4(style = "display: inline; margin-right: 5px;", "Most Frequent References in Bibliographies"),
                                                                         bslib::tooltip(
                                                                           trigger = shiny::icon("info-circle", class = "info-icon"),
                                                                           shiny::HTML("This table lists the references most frequently cited by the articles in the current selection.<br>
                                        Reading example: a value of '46%' means that 46% of articles in the selection cite the reference."),
                                                                           placement = "top"
                                                                         )
                                                              )
                                                   ),
                                                   DT::dataTableOutput("top_references_table")
                                     )
                                   )
                                 )
                      )
                    )
    )
  )

  # ===================================================================
  # SERVER
  # ===================================================================
  server <- function(input, output, session) {

    # Charger dplyr pour rendre le pipe %>% disponible
    library(dplyr)

    get_selection <- shiny::reactive({
      if (input$mode_select == "cluster") {
        return(input$cluster_select)
      } else {
        return("")
      }
    })

    active_data <- shiny::reactive({
      if (input$mode_select == "overall") {
        return(alluv_content_shiny)
      }
      if (input$mode_select == "cluster" && input$cluster_select != "") {
        return(alluv_content_shiny[alluv_content_shiny$intertemporal_name == input$cluster_select, ])
      }
      return(NULL)
    })

    active_title_context <- shiny::reactive({
      if (input$mode_select == "overall") {
        return("for the Entire Corpus")
      }
      if (input$mode_select == "cluster" && input$cluster_select != "") {
        clean_name <- trimws(sub("^\\[.*?\\]\\s*", "", input$cluster_select))
        return(paste("for Cluster:", clean_name))
      }
      return("")
    })

    output$alluvial_image <- shiny::renderImage({
      selection_prefix <- get_selection()
      image_name <- if (is.null(selection_prefix) || selection_prefix == "") {
        "alluvial_plot.png"
      } else {
        paste0("[", selection_prefix, "] alluvial_plot.png")
      }

      image_path <- system.file("app", "www", image_name, package = "app.trustgame.appendix")
      if (!file.exists(image_path)) {
        image_path <- system.file("app", "www", "alluvial_plot.png", package = "app.trustgame.appendix")
      }

      list(
        src = image_path,
        contentType = "image/png",
        width = "100%",
        height = "auto",
        class = "alluvial-image"
      )
    }, deleteFile = FALSE)

    output$selection_info <- shiny::renderUI({
      if (input$mode_select == "overall") {
        shiny::HTML("<h4>Overall View</h4><p>You are viewing statistics for the entire corpus of articles.</p>")
      } else if (input$mode_select == "cluster" && input$cluster_select != "") {
        shiny::HTML(paste("<h4>You have selected the cluster:</h4><p style='font-size:1.2em; font-weight:bold; color:#007bff;'>", input$cluster_select, "</p>"))
      } else {
        shiny::HTML("<p>Select a cluster to explore the data.</p>")
      }
    })

    output$year_distribution_plot <- shiny::renderPlot({
      filtered_data <- active_data()
      shiny::req(filtered_data, length(unique(filtered_data$ID_Art)) > 0)

      min_year <- min(filtered_data$Year, na.rm = TRUE)
      max_year <- max(filtered_data$Year, na.rm = TRUE)
      all_years <- data.frame(Year = seq(min_year, max_year))

      year_counts <- filtered_data %>%
        group_by(Year) %>%
        summarise(n = n(), .groups = "drop")
      year_counts <- merge(all_years, year_counts, by = "Year", all.x = TRUE)
      year_counts$n[is.na(year_counts$n)] <- 0

      median_year <- stats::median(filtered_data$Year, na.rm = TRUE)
      median_position <- match(median_year, year_counts$Year)
      total_articles <- length(unique(filtered_data$ID_Art))

      ggplot2::ggplot(year_counts, ggplot2::aes(x = as.factor(Year), y = n)) +
        ggplot2::geom_bar(stat = "identity", fill = "#007bff", alpha = 0.7) +
        ggplot2::geom_text(ggplot2::aes(label = n), vjust = -0.5, size = 4) +
        ggplot2::geom_vline(xintercept = median_position, color = "red", linetype = "dashed", size = 1.2) +
        ggplot2::annotate("text",
                          x = median_position + 0.1,
                          y = max(year_counts$n) * 0.9,
                          label = paste("Median:", median_year),
                          color = "red", fontface = "bold", hjust = 0) +
        ggplot2::labs(
          title = paste("Number of Articles per Year", active_title_context()),
          subtitle = paste("Total articles in selection:", total_articles),
          x = "Year",
          y = "Number of Articles"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
          plot.title = ggplot2::element_text(hjust = 0, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0, face = "italic", color = "darkblue")
        )
    })

    output$discipline_plot <- shiny::renderPlot({
      filtered_data <- active_data()
      shiny::req(filtered_data, length(unique(filtered_data$ID_Art)) > 0)

      tags_df <- filtered_data %>%
        mutate(WC = as.character(WC)) %>%
        tidyr::separate_rows(WC, sep = ";") %>%
        mutate(WC = trimws(WC),
               WC = sapply(strsplit(WC, ","), `[`, 1)) %>%
        filter(!is.na(WC) & WC != "")

      tag_counts <- tags_df %>%
        group_by(WC) %>%
        summarise(n = n_distinct(ID_Art), .groups = "drop") %>%
        mutate(percentage = round(n / length(unique(filtered_data$ID_Art)) * 100, 1)) %>%
        arrange(desc(percentage)) %>%
        slice_head(n = 10)

      ggplot2::ggplot(tag_counts, ggplot2::aes(x = stats::reorder(WC, percentage), y = percentage)) +
        ggplot2::geom_bar(stat = "identity", fill = "#28a745", alpha = 0.8) +
        ggplot2::geom_text(ggplot2::aes(label = paste0(percentage, "%")),
                           hjust = -0.1, color = "black", size = 4) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
        ggplot2::labs(
          title = paste("Most Frequent Discipline Tags", active_title_context()),
          x = "Discipline Tag",
          y = "Percentage of Articles (%)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0, face = "bold"),
          axis.text = ggplot2::element_text(size = 12)
        )
    })

    output$country_plot <- shiny::renderPlot({
      filtered_data <- active_data()
      shiny::req(filtered_data, length(unique(filtered_data$ID_Art)) > 0)

      country_data <- filtered_data %>%
        mutate(University = as.character(University)) %>%
        tidyr::separate_rows(University, sep = ";") %>%
        mutate(University = trimws(University)) %>%
        left_join(university_location_en, by = "University") %>%
        filter(!is.na(Country)) %>%
        distinct(ID_Art, Country) %>%
        group_by(Country) %>%
        summarise(n_articles = n(), .groups = "drop") %>%
        mutate(percentage = round(n_articles / length(unique(filtered_data$ID_Art)) * 100, 1)) %>%
        arrange(desc(percentage)) %>%
        slice_head(n = 10)

      shiny::req(length(unique(filtered_data$ID_Art)) > 0)

      ggplot2::ggplot(country_data, ggplot2::aes(x = stats::reorder(Country, percentage), y = percentage)) +
        ggplot2::geom_bar(stat = "identity", fill = "#fd7e14", alpha = 0.8) +
        ggplot2::geom_text(ggplot2::aes(label = paste0(percentage, "% (", n_articles, ")")),
                           hjust = -0.1, color = "black", size = 3.5) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
        ggplot2::labs(
          title = paste("Most Represented Countries", active_title_context()),
          x = "Country",
          y = "Percentage of Articles (%)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0, face = "bold"),
          axis.text = ggplot2::element_text(size = 12)
        )
    })

    output$journal_plot <- shiny::renderPlot({
      filtered_data <- active_data()
      shiny::req(filtered_data, length(unique(filtered_data$ID_Art)) > 0)

      journal_data <- filtered_data %>%
        filter(!is.na(SO), SO != "") %>%
        group_by(SO) %>%
        summarise(n_articles = n(), .groups = "drop") %>%
        mutate(percentage = round(n_articles / length(unique(filtered_data$ID_Art)) * 100, 1)) %>%
        arrange(desc(percentage)) %>%
        slice_head(n = 10) %>%
        mutate(SO_wrapped = stringr::str_wrap(SO, width = 35))

      shiny::req(nrow(journal_data) > 0)

      ggplot2::ggplot(journal_data, ggplot2::aes(x = stats::reorder(SO_wrapped, percentage), y = percentage)) +
        ggplot2::geom_bar(stat = "identity", fill = "#6f42c1", alpha = 0.8) +
        ggplot2::geom_text(ggplot2::aes(label = paste0(percentage, "% (", n_articles, ")")),
                           hjust = -0.1, color = "black", size = 3.5) +
        ggplot2::coord_flip() +
        ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15))) +
        ggplot2::labs(
          title = paste("Most Represented Journals", active_title_context()),
          x = "Journal",
          y = "Percentage of Articles (%)"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0, face = "bold"),
          axis.text.y = ggplot2::element_text(size = 10)
        )
    })

    output$top_references_table <- DT::renderDataTable({
      filtered_data <- active_data()
      shiny::req(filtered_data, length(unique(filtered_data$ID_Art)) > 0)

      cluster_articles <- filtered_data %>% pull(ID_Art)

      cluster_refs <- references_collapse %>%
        filter(ID_Art %in% cluster_articles)

      if(nrow(cluster_refs) == 0) {
        return(DT::datatable(data.frame(Reference = "No reference data found for this selection"), options = list(pageLength = 5)))
      }

      tryCatch({
        top_refs <- cluster_refs %>%
          mutate(ItemID_Ref = as.character(ItemID_Ref)) %>%
          tidyr::separate_rows(ItemID_Ref, sep = ",") %>%
          mutate(ItemID_Ref = as.numeric(trimws(ItemID_Ref))) %>%
          filter(!is.na(ItemID_Ref)) %>%
          group_by(ItemID_Ref) %>%
          summarise(article_count = n_distinct(ID_Art), .groups = "drop") %>%
          arrange(desc(article_count))

        total_articles <- length(cluster_articles)
        top_refs <- top_refs %>%
          mutate(Citations = round((article_count / total_articles) * 100, 1))

        final_refs <- top_refs %>%
          left_join(ref_for_alluv, by = "ItemID_Ref") %>%
          mutate(Reference = ifelse(is.na(AYR), paste("Unknown Ref", ItemID_Ref), AYR)) %>%
          select(Reference, Citations) %>%
          mutate(Reference = stringr::str_to_title(tolower(Reference))) %>%
          filter(!grepl("^[0-9]", Reference)) %>%
          filter(Citations >= 5) %>%
          rename("Frequency (in %)" = Citations) %>%
          arrange(desc(`Frequency (in %)`))

        DT::datatable(
          final_refs,
          rownames = FALSE,
          options = list(
            pageLength = 25, scrollY = '500px', scrollCollapse = TRUE,
            order = list(list(1, 'desc')),
            columnDefs = list(list(className = 'dt-center', targets = 1))
          )
        )

      }, error = function(e) {
        DT::datatable(data.frame(Reference = paste("Error:", e$message), Frequency = 0), options = list(pageLength = 5))
      })
    })

    output$table_title <- shiny::renderText({
      paste("Articles", active_title_context())
    })

    output$cluster_table <- DT::renderDataTable({
      filtered_data <- active_data()
      shiny::req(filtered_data, length(unique(filtered_data$ID_Art)) > 0)

      table_data <- filtered_data %>%
        mutate(Z9 = as.integer(Z9)) %>%
        arrange(desc(Z9)) %>%
        select(-any_of(c("intertemporal_name", "ID_Art", "SC", "Label", "ItemID_Ref", "University"))) %>%
        rename(Title = TI, Journal = SO, Citations = Z9, Categories = WC) %>%
        unique()

      clusters_with_experiment <- c(
        "Social Correlates of Trust",
        "Social Preferences and Trust"
      )

      show_experiment <- input$mode_select == "cluster" &&
        input$cluster_select %in% clusters_with_experiment

      if (!show_experiment && "Experiment Type" %in% names(table_data)) {
        table_data <- table_data %>% select(-"Experiment Type")
      }

      column_names <- colnames(table_data)

      header_tags <- lapply(column_names, function(col_name) {
        if (col_name == "Experiment Type" && show_experiment) {
          htmltools::tags$th(
            shiny::div(style = "display: inline-block;",
                       "Experiment Type",
                       bslib::tooltip(
                         trigger = shiny::icon("info-circle", class = "info-icon", style = "margin-left: 5px;"),
                         shiny::HTML("Experiment types in this table were labeled by the author according to the taxonomy of Harrison and List (2004)"),
                         placement = "top"
                       )
            )
          )
        } else {
          htmltools::tags$th(col_name)
        }
      })

      sketch <- htmltools::withTags(table(
        class = 'display',
        thead(
          tr(
            !!!header_tags
          )
        )
      ))

      DT::datatable(table_data,
                    container = sketch,
                    rownames = FALSE,
                    options = list(
                      scrollX = TRUE, scrollY = '500px', scrollCollapse = TRUE,
                      pageLength = 25,
                      columnDefs = list(list(className = 'dt-center', targets = c(1, 4)))
                    ))
    })
  }

  # ===================================================================
  # LANCER L'APPLICATION
  # ===================================================================
  shiny::shinyApp(ui = ui, server = server)
}
