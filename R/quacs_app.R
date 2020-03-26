#' Quacs Shiny App
#'
#' This can be called as as function or run as an addin
#'
#' @return The table created by the app
#' @export
#' @import shinydashboard
#' @import shiny
quacs <- function() {
    # UI ######################################################################
    ui <- dashboardPage(
        dashboardHeader(title="QUACS"),
        dashboardSidebar(
            sidebarMenu(
                id = "tabs",
                menuItem("Load Data Catalogue", tabName = "load_data_tab", icon = icon("download")),
                menuItem("Select Table Subjects", tabName = "subject_tab", icon = icon("tasks")),
                menuItem("Select ACS Tables", tabName = "table_tab", icon = icon("tasks")),
                menuItem("Select ACS Variables", tabName = "var_tab", icon = icon("tasks")),
                menuItem("Save/Load Data Dictionary", tabName = "dd_tab", icon = icon("share-square")),
                menuItem("Download Data", tabName = "dl_tab", icon = icon("file-download"))
            )
        ),
        dashboardBody(
            tabItems(
                # Load Data Tab ###############################################
                tabItem(tabName="load_data_tab",
                        fluidRow(
                            box(width=2, title="Load ACS-5 Variables",

                                p("Load the catalogue of census variables. It is downloadable via the census API: "),
                                a("ACS Data Dictionary", href="https://api.census.gov/data/2017/acs/acs5/variables.html"),
                                p("Download or load data dictionary:"),
                                selectInput("dd_year", "Year", list("2013", "2014", "2015", "2016", "2017", "2018"), selected="2018"),
                                p(""),
                                actionButton("download_dd", "Download ACS Variable Catalogue"),
                                p(""),
                                actionButton("load_dd", "Load Saved ACS Variable Catalogue")
                            ),
                            box(width=10,
                                title="Data Dictionary Viewer",
                                textOutput("tab1_output_text"),
                                DT::dataTableOutput("dd_table")
                            )
                        )
                ),

                # Select Subject Tab ##########################################
                tabItem(tabName="subject_tab",
                        fluidRow(
                            column(width=2,
                                   box(width=12, title="Select ACS Table Subjects",
                                       p("At the top of the heirarchy, ACS data tables can be grouped by subject.
                           Review the table to the right and click table rows to select subjects of interest."),
                                       checkboxInput("tab3_checkbox", "Include Race Specific Tables", value = FALSE),
                                       actionButton("use_subjects_button", "Use Selected Table Subjects")
                                   ),
                                   valueBoxOutput(width=12, "tab_2_valuebox")
                            ),
                            box(width=10, title="Table Subject Viewer",
                                DT::dataTableOutput("ts_table")
                            )
                        )
                ),

                # Select Tables Tab ###########################################
                tabItem(tabName="table_tab",
                        fluidRow(width=12,
                                 box(width=10, title="Select ACS Table Subjects",
                                     checkboxInput("tab3_checkbox", "Include Race Specific Tables", value = FALSE),
                                     actionButton("use_tables_button", "Use Selected Table Subjects")
                                 ),
                                 valueBoxOutput(width=2, "tab_3_valuebox")
                        ),
                        fluidRow(

                            box(width=6,title="Table List Viewer",
                                DT::dataTableOutput("tl_table")
                            ),

                            box(width=6,title="Variable Tree",
                                networkD3::diagonalNetworkOutput("diagNet"),
                                plotOutput("tt_plot")
                            )
                        )
                ),

                # Select Variables Tab ########################################
                tabItem(tabName="var_tab",
                        fluidRow(
                            column(width=2,
                                   box(width=12, title="Select ACS Variables",
                                       p("Select variables of interest and finalize your choice with the button."),
                                       actionButton("final_vars_button", "Use Highlighted Variables"),
                                       actionButton("final_vars_use_all_btn", "Use All")
                                   ),
                                   valueBoxOutput(width=12, "tab_4_valuebox")
                            ),
                            box(width=10, title="Variable List Viewer",
                                DT::dataTableOutput("var_table")
                            )
                        )
                ),

                # Data Dictionary Tab #########################################
                tabItem(tabName="dd_tab",
                        fluidRow(
                            box(width=4, title="Manage Data Dictionary",
                                p("Saved csv append cataloque with 1/0 flags: SUBJECT_FLAG,
                           GROUP_FLAG, and VARIABLE_FLAG"),
                                textInput("dd_loc", label = h3("Data Dictionary Export Location"), value = "ACS_dd.csv"),
                                actionButton("export_data_dictionary", "Export Data Dictionary"),
                                textInput("dd_loc_in", label = h3("Data Dictionary Import Location"), value = "ACS_dd.csv"),
                                actionButton("import_data_dictionary", "Import Data Dictionary")
                            ),
                            box(width=8, title="Data Dictionary Viewer",
                                textOutput("tab5_output_Text"),
                                DT::dataTableOutput("output_dd_table")
                            )
                        )
                ),

                # Data Download Tab ###########################################
                tabItem(tabName="dl_tab",
                        fluidRow(
                            column(width=4,
                                   box(width=12, title="Pull ACS Data",
                                       p("Select optional 'Include Attributes' for margins of error. ACS API Key required"),
                                       p("Select a geography to pull data on ACS (just does Indiana right now)"),

                                       selectInput("geo_select", label = h3("Geography Selection"),
                                                   choices = c("Census Tract" = "for=tract:*",
                                                               "US" = "for=us:*",
                                                               "Region" = "for=region:*",
                                                               "Division" = "for=division:*",
                                                               "County" = "for=county:*",
                                                               "State" = "for=state:*",
                                                               "Block Group" = "for=block group:*"
                                                               )),
                                       selectInput("loc_select", label = h3("Location Selection"),
                                                   choices = c("Indiana" = "in=state:18",
                                                               "All States" = "-")
                                       ),
                                       checkboxInput("tab6_attr_cb", "Include attributes", value = FALSE),
                                       textInput("api_key", label = h3("Enter a valid ACS API Key"), value = ""),
                                       p("Use the button to use the ACS API to pull data based on your data dictionary"),
                                       actionButton("pull_acs_data", "Pull ACS Data")
                                   ),
                                   box(width=12, title="Data Export",
                                       textInput("data_loc_out", label = h3("Data Export Location"), value = "ACS_data.csv"),
                                       actionButton("save_acs_data", "Save ACS Data")
                                   )
                            ),
                            column(width=8,
                                   infoBoxOutput(width=12, "tab_6_infobox"),
                                   box(width=12, title="Result",
                                       DT::dataTableOutput(width="auto", "acs_data_table")
                                   )
                            )
                        )
                )

            )
        )
    )

    # Server ##################################################################
    server <- function(input, output, session) {
        # Define Server Varaiables ############################################
        # The table_subjects data load is a little unconvential, but we want
        # to load data if the user runs quacs:::quacs() via the Addins button

        table_subjects <- NA
        utils::data("table_subjects", package="quacs", envir = environment())

        ACS_DD <- NA
        user_table_subjects <- NA
        past_trs <- NA
        table_table <- NA
        yeet <- NA
        group_names <- NA
        var_names <- NA
        ACS_DD_subset <- NA
        ACS_DD_subset_view <- NA
        user_table_subjects_names <- NA
        output_dd <- NA
        output_col_names <- NA
        acs_data <- NA

        # Load Data ###########################################################
        observeEvent(input$download_dd, {
            withProgress(message = 'Getting ACS Data Diciontionary', value=0, {
                incProgress(1/3, detail = 'Downloading and Parsing from API (may take a minute or two)')
                ACS_DD <<- get_acs_dd(year=input$dd_year)

                incProgress(2/3, detail = 'Downloading')
                saveRDS(ACS_DD, file='ACS_DD.RDS')

                incProgress(3/3, detail = 'Loading into R')
                output$tab1_output_text <- renderText({
                    sprintf("ACS Data Dictionary Downloaded: %s", as.character(Sys.time()))
                })
                output$dd_table = DT::renderDataTable({
                    ACS_DD
                })
            })
        })

        observeEvent(input$load_dd, {
            withProgress(message = 'Reading ACS Data Dictionary', value=0, {
                incProgress(1/3, detail = 'Loading acs_dd.RDS from working directory')
                ACS_DD <<- readRDS('ACS_DD.RDS')

                incProgress(2/3, detail = 'Loading into R')
                output$tab1_output_text <- renderText({
                    sprintf("ACS Data Dictionary Loaded: %s", as.character(Sys.time()))
                })
                output$dd_table = DT::renderDataTable({
                    ACS_DD
                })
            })
        })

        # Table Subjects ######################################################
        observeEvent(input$use_subjects_button, {

            rows_selected <- input$ts_table_rows_selected

            output$tab_2_valuebox <- renderValueBox(valueBox(length(rows_selected),
                                                             "Subjects Selected",
                                                             icon=icon("list"),
                                                             color = "purple"))

            # subset to just table subjects
            user_table_subjects <<- table_subjects[rows_selected,]

            # select table concepts only
            user_table_subjects %>%
                dplyr::inner_join(ACS_DD, by="table_subject") %>%
                dplyr::select(.data$group, .data$concept, .data$table_subject, .data$table_subject_name) %>%
                dplyr::distinct() ->
                table_table_1

            # ignore race specific tables unless checkbox is selected
            if (input$tab3_checkbox == FALSE) {
                table_table_1 %>%
                    dplyr::filter(nchar(.data$group) == 6) %>%
                    dplyr::distinct() ->
                    table_table_1
            }
            else {
                table_table_1 %>%
                    dplyr::distinct() ->
                    table_table_1
            }

            past_trs <<- NA

            table_table <<- table_table_1

            output$tl_table <- DT::renderDataTable({
                DT::datatable(table_table,
                          callback = DT::JS("table.on('click.dt', 'td', function() {
                        var row_=table.cell(this).index().row;
                        var col=table.cell(this).index().column;
                        var rnd= Math.random();
                        var data = [row_, col, rnd];
                       Shiny.onInputChange('rows', data);
                        });"
                          ))
            })
        })


        # ACS Tables ##########################################################
        # when the table row selection is updated
        observeEvent(input$rows, {
            new_rows <- setdiff(input$tl_table_rows_selected, past_trs)
            past_trs <<- input$tl_table_rows_selected

            output$tab_3_valuebox <- renderValueBox(valueBox(length(past_trs),
                                                             "Tables Selected",
                                                             icon=icon("list"),
                                                             color = "purple"))
            if (length(new_rows) >= 1) {
                new_row <- max(new_rows)
                tt1_sub <- table_table[new_row,]$group
                ACS_DD %>%
                    dplyr::filter(.data$group == tt1_sub) %>%
                    dplyr::select(.data$label) %>%
                    dplyr::distinct() ->
                    tree_t

                tree_t %>%
                    dplyr::mutate(PATH1 = gsub("!!", "/", .data$label, fixed=TRUE)) %>%
                    dplyr::mutate(pathString = paste(tt1_sub, .data$PATH1, sep="/")) ->
                    tree_t2

                w_tree <- data.tree::as.Node(tree_t2)
                yeet <<- w_tree

                # I need a better solution than as.list.Node, as it's not exported by data.tree
                output$diagNet <- networkD3::renderDiagonalNetwork({
                    as.list.Node <- data.tree:::as.list.Node
                    aln <- as.list.Node(yeet, unname=TRUE, mode="explicit")
                    networkD3::diagonalNetwork(List = aln, fontSize = 10, opacity = 0.9)
                })
            }
        })
        # Select Variables ####################################################
        observeEvent(input$use_tables_button, {
            tl_rows_selected <- input$tl_table_rows_selected
            group_names <<- table_table[tl_rows_selected,]$group

            ACS_DD %>%
                dplyr::filter(.data$group %in% group_names) %>%
                dplyr::left_join(user_table_subjects, by="table_subject") %>%
                dplyr::arrange(.data$table_subject, .data$group, .data$name) ->>
                ACS_DD_subset

            ACS_DD_subset %>%
                dplyr::select(.data$label, .data$concept, .data$name, .data$group,
                       .data$table_subject_name, .data$table_subject) ->>
                ACS_DD_subset_view


            output$var_table <- DT::renderDataTable({
                ACS_DD_subset_view
            })
        })
        # Finalize Variables ##################################################
        observeEvent(input$final_vars_button, {

            rows_selected <- input$ts_table_rows_selected
            tl_rows_selected <- input$tl_table_rows_selected
            var_rows_selected <- input$var_table_rows_selected

            group_names <<- table_table[tl_rows_selected,]$group
            var_names <<- ACS_DD_subset[var_rows_selected,]$name
            user_table_subjects_names <<- table_subjects[rows_selected,]$table_subject

            ACS_DD %>%
                dplyr::mutate(
                    SUBJECT_FLAG = as.numeric(.data$table_subject %in% user_table_subjects_names),
                    GROUP_FLAG = as.numeric(.data$group %in% group_names),
                    VARIABLE_FLAG = as.numeric(.data$name %in% var_names)
                ) ->>
                output_dd

            output$output_dd_table <- DT::renderDataTable({
                output_dd
            })

            output$tab_4_valuebox <- renderValueBox(valueBox(length(var_rows_selected),
                                                             "Variables Selected",
                                                             icon=icon("list"),
                                                             color = "purple"))
        })

        observeEvent(input$final_vars_use_all_btn, {

            rows_selected <- input$ts_table_rows_selected
            tl_rows_selected <- input$tl_table_rows_selected
            var_rows_selected <- 1:nrow(ACS_DD_subset)

            group_names <<- table_table[tl_rows_selected,]$group
            var_names <<- ACS_DD_subset[var_rows_selected,]$name
            user_table_subjects_names <<- table_subjects[rows_selected,]$table_subject

            ACS_DD %>%
                dplyr::mutate(
                    SUBJECT_FLAG = as.numeric(.data$table_subject %in% user_table_subjects_names),
                    GROUP_FLAG = as.numeric(.data$group %in% group_names),
                    VARIABLE_FLAG = as.numeric(.data$name %in% var_names)
                ) ->>
                output_dd

            output$output_dd_table <- DT::renderDataTable({
                output_dd
            })

            output$tab_4_valuebox <- renderValueBox(valueBox(length(var_rows_selected),
                                                             "Variables Selected",
                                                             icon=icon("list"),
                                                             color = "purple"))
        })

        # Export Data Dictionary ##############################################
        observeEvent(input$export_data_dictionary, {
            withProgress(message = 'Saving Data Dictionary For Output', value=0, {
                incProgress(1/5, detail = 'Acquiring Selections')

                file_out_loc <- input$dd_loc
                rows_selected <- input$ts_table_rows_selected
                tl_rows_selected <- input$tl_table_rows_selected
                var_rows_selected <- input$var_table_rows_selected


                group_names <<- table_table[tl_rows_selected,]$group
                var_names <<- ACS_DD_subset[var_rows_selected,]$name
                user_table_subjects <<- table_subjects[rows_selected,]$table_subject


                incProgress(2/5, detail = 'Creating Flags')
                ACS_DD ->> output_dd

                ACS_DD %>%
                    dplyr::mutate(
                        SUBJECT_FLAG = as.numeric(.data$table_subject %in% user_table_subjects),
                        GROUP_FLAG = as.numeric(.data$group %in% group_names),
                        VARIABLE_FLAG = as.numeric(.data$name %in% var_names)
                    ) ->>
                    output_dd

                incProgress(3/5, detail = 'Rendering Table')

                output$output_dd_table <- DT::renderDataTable({
                    output_dd
                })

                incProgress(4/5, detail = 'Writing csv')
                readr::write_csv(output_dd, file_out_loc)

                incProgress(5/5, detail = 'Finishing')
                output$tab5_output_text <- renderText({
                    sprintf("Saved data dictionary of %s variables to %s",
                            nrow(output_dd),
                            file_out_loc
                    )
                })
            })

        })

        # Import Data Dictionary ##############################################
        observeEvent(input$import_data_dictionary, {
            withProgress(message = 'Importing Data Dictionary ', value=0, {
                incProgress(1/3, detail = 'Reading csv')
                file_in_loc <- input$dd_loc_in
                output_dd <<- readr::read_csv(file_in_loc)

                incProgress(2/3, detail = 'Rendering Table')
                output$output_dd_table <- DT::renderDataTable({
                    output_dd
                })

                incProgress(3/3, detail = 'Finishing')
                output$tab5_output_text <- renderText({
                    sprintf("Loaded data dictionary of %s variables from %s",
                            nrow(output_dd),
                            file_in_loc
                    )
                })

            })

        })
        # Download Data #######################################################
        observeEvent(input$pull_acs_data, {
            withProgress(message = 'Pulling ACS Data', value=0, {
                tryCatch({
                    incProgress(1/4, detail = 'Selecting Variables From Data Dictionary')

                    # Get Variable names
                    output_dd %>%
                        dplyr::filter(.data$VARIABLE_FLAG == 1) %>%
                        dplyr::pull(.data$name) ->
                        var_names

                    # Get attribute variables if checked
                    if (input$tab6_attr_cb == TRUE) {
                        output_dd %>%
                            dplyr::filter(.data$VARIABLE_FLAG == 1) %>%
                            dplyr::pull(.data$attributes) ->
                            attribs
                        L <- lapply(attribs, function(x) {
                            y <- strsplit(x, ",", fixed=TRUE)[[1]]
                            return(y)
                        })
                        Z <- as.character(unlist(L))
                        var_names <- c(var_names, Z)
                    }

                    # Get labels for better variable names
                    output_dd %>%
                        dplyr::filter(.data$VARIABLE_FLAG == 1) %>%
                        dplyr::pull(.data$label) %>%
                        janitor::make_clean_names() ->
                        label_clean_names
                    output_col_names <<- paste0(var_names, "_", label_clean_names)


                    all_var_names <- c(var_names, "NAME")

                    incProgress(2/4, detail = 'Constructing API Call')
                    geo <- input$geo_select
                    where  <- input$loc_select
                    key <- paste0("key=", input$api_key)

                    incProgress(3/4, detail = 'Downloading data')
                    acs_data <<- census_api_acs5_multi(all_var_names, geo=geo, key=key, where=where, year=input$dd_year)

                    # rename variables to have better labels
                    new_names_1 <- names(acs_data)
                    new_names_1[1:length(output_col_names)] <- output_col_names
                    names(acs_data) <<- new_names_1

                    incProgress(4/4, detail = 'Saving to R')
                    output$acs_data_table <- DT::renderDataTable({
                        .named(acs_data)
                    })

                    output$tab_6_infobox <- renderInfoBox(infoBox("Success",
                                                                  value=sprintf("Downloaded %s data: %s columns, %s rows",
                                                                                input$dd_year,
                                                                                ncol(acs_data),
                                                                                nrow(acs_data)),
                                                                  icon=icon("check"),
                                                                  color = "green"))
                }
                , error=function(e) {
                    output$tab_6_infobox <- renderInfoBox(infoBox("Error",
                                                                  value=e,
                                                                  icon=icon("exclamation"),
                                                                  color = "red"))
                })
            })
        })
        # Save ACS Data #######################################################
        observeEvent(input$save_acs_data, {
            withProgress(message = 'Saving Data to CSV ', value=0, {
                incProgress(2/4, detail = 'Writing to csv')
                data_out_csv <- input$data_loc_out
                readr::write_csv(acs_data, data_out_csv, na='')
            })

        })

        # UI Setup ############################################################
        output$ts_table <- DT::renderDataTable({
            table_subjects
        })
    }
    # Run the gadget ##########################################################
    viewer <- shiny::dialogViewer("QUACS", width=1600, height=900)
    shiny::runGadget(ui, server, viewer = viewer)

}
