suppressPackageStartupMessages({
  require(DT)
  require(reshape2)
  require(plyr)
  require(data.table)
  library(shiny)
  library(stringi)
  library(dplyr)
  library(plotly)
  library(tidyr)
  library(lubridate)
  library(openxlsx)
  library(shinydashboard)
  library(rlang)
  library(shinyjs)
  library(webshot)
  library(leaflet)
  library(leafletCN)
  library(shinyWidgets)
  library(feather)
})

options(shiny.maxRequestSize = 1000 * 1024 ^ 2)

load("./data/data.RData")
######################## load function graph1 #################################

source("./functions/99_graph1_corp.R")

######################## load function graph1m #################################

source("./functions/99_graph1_corp_m.R")

######################## shiny server core code#################################


server <- function(input, output, session) {
  summary <- reactive({
    if (is.null(input$summary))
      return(NULL)
    inFile.summary <- input$summary
    read.csv(
      inFile.summary$datapath,
      na.strings = "NA",
      stringsAsFactors = F,
      fileEncoding = 'GB2312'
    )
  })
  
  reglist <- reactive({
    reglist <- summary()$AUDIT.DESC[!duplicated(summary()$AUDIT.DESC)]
    reglist <- reglist [reglist %in% c(
      "China",
      "East Region Audit",
      "South Region Audit",
      "West Region Audit",
      "North Region Audit",
      "Beijing Hospital Audit"
    )]
    reglist <- reglist[order(reglist)]
  })
  
  observeEvent(input$summary, {
    # updateSelectInput(session, "region", choices = reglist(),  selected = "China")
    updateSelectInput(session,
                      "region",
                      choices = reglist(),
                      selected = reglist())
  })
  
  province <- reactive({
    provlist <- summary()$AUDIT.DESC[!duplicated(summary()$AUDIT.DESC)]
    provlist <- provlist [!provlist %in% c(
      "China",
      "East Region Audit",
      "South Region Audit",
      "West Region Audit",
      "North Region Audit"
    )]
    provlist <- provlist[order(provlist)]
    provlist <- c("ALL", provlist)
  })
  
  observeEvent(input$summary, {
    updateSelectInput(session, "province", choices =  province()) })
  
 
  observe(
    {
      if (any(grepl("Region", summary()$AUDIT.DESC))) {
        show("province")
      } 
    }
  )
  
  
  toplist <- reactive({
    # summary1<-summary()[which(summary()$AUDIT.DESC =="China"), ]
    if (any(grepl("China|Region", summary()$AUDIT.DESC))) {
      summary1 <- summary()[grepl("China|Region", summary()$AUDIT.DESC), ]
    } else {
      summary1 <- summary()
    }
    
    if ("ALL" %in% input$manu) {
      summary1 <- summary1
    } else {
      summary1 <-
        summary1[which(summary1$MANUF.TYPE.DESC %in% c(input$manu)), ]
    }
    
    ggg <-  rbind.fill(
      graph1(
        summary1,
        # cate=input$category,
        # subcate=input$subcategory,
        value = "RENMINBI",
        period = input$period,
        kpi = "abs",
        window = 1,
        level = "corporation"
      )
    )
    ggg <- distinct(ggg) %>%
      filter((CORPORATE.DESC != "Overall" &
                COMPS.DESC != "Overall") |
               (CORPORATE.DESC == "Overall" &
                  COMPS.DESC == "Overall")
      )
    tmp <-
      head(ggg[order(-ggg[, length(ggg)]), ], (as.integer(input$top) + 1))$PRODUCT.DESC
    
    if (length(tmp) == 1 & !("Overall" %in% tmp)) {
      "Overall"
    } else {
      tmp
    }
  })
  
  toplist_c <- reactive({
    # summary1<-summary()[which(summary()$AUDIT.DESC =="China"), ]
    if (any(grepl("China|Region", summary()$AUDIT.DESC))) {
      summary1 <- summary()[grepl("China|Region", summary()$AUDIT.DESC), ]
    } else {
      summary1 <- summary()
    }
    
    if ("ALL" %in% input$manu) {
      summary1 <- summary1
    } else {
      summary1 <-
        summary1[which(summary1$MANUF.TYPE.DESC %in% c(input$manu)), ]
    }
    
    ggg <-  rbind.fill(
      graph1(
        summary1,
        # cate=input$category,
        # subcate=input$subcategory,
        value = "RENMINBI",
        period = input$period,
        kpi = "abs",
        window = 1,
        level = "corporation"
      )
    )
    
    ggg <- distinct(ggg) %>%
      filter((CORPORATE.DESC != "Overall" &
                COMPS.DESC == "Overall") |
               (CORPORATE.DESC == "Overall" &
                  COMPS.DESC == "Overall")
      )
    
    tmp <-
      head(ggg[order(-ggg[, length(ggg)]), ], (as.integer(input$top) + 1))$CORPORATE.DESC
    
    if (length(tmp) == 1 & !("Overall" %in% tmp)) {
      "Overall"
    } else {
      tmp
    }
  })
  
  toplist_m <- reactive({
    # summary1<-summary()[which(summary()$AUDIT.DESC =="China"), ]
    if (any(grepl("China|Region", summary()$AUDIT.DESC))) {
      summary1 <- summary()[grepl("China|Region", summary()$AUDIT.DESC), ]
    } else {
      summary1 <- summary()
    }
    
    if ("ALL" %in% input$manu) {
      summary1 <- summary1
    } else {
      summary1 <-
        summary1[which(summary1$MANUF.TYPE.DESC %in% c(input$manu)), ]
    }
    
    ggg <- rbind.fill(
      graph1(
        summary1,
        # cate=input$category,
        # subcate=input$subcategory,
        value = "RENMINBI",
        period = input$period,
        kpi = "abs",
        window = 1,
        level = "molecule"
      )
    )
    
    ggg <- distinct(ggg) %>%
      filter((CORPORATE.DESC == "Overall" &
                COMPS.DESC != "Overall") |
               (CORPORATE.DESC == "Overall" &
                  COMPS.DESC == "Overall")
      )
    
    tmp <-
      head(ggg[order(-ggg[, length(ggg)]), ], (as.integer(input$top) + 1))$COMPS.DESC
    if (length(tmp) == 1 & !("Overall" %in% tmp)) {
      "Overall"
    } else {
      tmp
    }
  })
  
  result1 <- reactive({
    if ("RENMINBI"  %in% input$value) {
      if ("ALL" %in% input$province) {
        summary <- summary()[which(summary()$AUDIT.DESC %in% c("China",
                                                               input$region,
                                                               province())), ]
      } else {
        summary <- summary()[which(summary()$AUDIT.DESC %in% c("China",
                                                               input$region,
                                                               input$province)), ]
      }
      
      if ("ALL" %in% input$manu) {
        summary <- summary
      } else {
        summary <-
          summary[which(summary$MANUF.TYPE.DESC %in% c(input$manu)), ]
      }
      
      rmb <- rbind.fill(
        graph1(
          summary,
          # cate=input$category,
          # subcate=input$subcategory,
          value = "RENMINBI",
          period = input$period,
          kpi = input$kpi,
          window = as.numeric(input$window),
          level = "corporation"
        ),
        graph1(
          summary,
          # cate=input$category,
          # subcate=input$subcategory,
          value = "RENMINBI",
          period = input$period,
          kpi = input$kpi,
          window = as.numeric(input$window),
          level = "molecule"
        )
      )
      rmb <- distinct(rmb)
    } else{
      rmb <- NULL
    }
    
    if ("UNIT" %in% input$value) {
      if ("ALL" %in% input$province) {
        summary <-
          summary()[which(summary()$AUDIT.DESC %in% c("China",
                                                      input$region,
                                                      province())), ]
      } else{
        summary <-
          summary()[which(summary()$AUDIT.DESC %in% c("China",
                                                      input$region,
                                                      input$province)), ]
      }
      
      if ("ALL" %in% input$manu) {
        summary <- summary
      } else {
        summary <-
          summary[which(summary$MANUF.TYPE.DESC %in% c(input$manu)), ]
      }
      
      unit <- rbind.fill(
        graph1(
          summary,
          # cate=input$category,
          # subcate=input$subcategory,
          value = "UNIT",
          period = input$period,
          kpi = input$kpi,
          window = as.numeric(input$window),
          level = "corporation"
        ),
        graph1(
          summary,
          # cate=input$category,
          # subcate=input$subcategory,
          value = "UNIT",
          period = input$period,
          kpi = input$kpi,
          window = as.numeric(input$window),
          level = "molecule"
        )
      )
      unit <- distinct(unit)
    } else{
      unit <- NULL
    }
    result1 <- rbind(rmb, unit)
  })
  
  ##-- for the KPI summary
  date_flag <- NULL
  result2 <- reactive({
    contain <- vector("list", 2)
    # summary<-summary()
    summary <- read_feather("./data/chpa.feather")
    rmb <- rbind.fill(
      graph1(
        summary,
        # cate=input$category,
        # subcate=input$subcategory,
        value = "RENMINBI",
        period = input$period,
        kpi = c("abs", "gr", "ms"),
        window = 1,
        level = "corporation"
      ),
      graph1(
        summary,
        # cate=input$category,
        # subcate=input$subcategory,
        value = "RENMINBI",
        period = input$period,
        kpi = c("abs", "gr", "ms"),
        window = 1,
        level = "molecule"
      )
    )
    rmb1 <- distinct(rmb) %>%
      select(AUDIT.DESC:Index, ncol(.))
    date_flag <<- colnames(rmb1)[ncol(rmb1)]
    colnames(rmb1)[ncol(rmb1)] <- "final"
    result2_m <- rmb1 %>%
      filter(
        PRODUCT.DESC == "Overall",
        CORPORATE.DESC == "Overall" |
          CORPORATE.DESC == "B.INGELHEIM",
        COMPS.DESC == "Overall"
      ) %>%
      select(AUDIT.DESC, CORPORATE.DESC, Index, final) %>%
      group_by(AUDIT.DESC, CORPORATE.DESC, Index) %>%
      summarise(final = sum(as.numeric(final), na.rm = TRUE)) %>%
      ungroup() %>%
      filter(
        CORPORATE.DESC == "Overall" | (CORPORATE.DESC == "B.INGELHEIM" &
                                         Index == "MS%"),
        !(CORPORATE.DESC == "Overall" &
            Index == "MS%")
      )
    tmp <- expand.grid(
      AUDIT.DESC = unique(result2_m$AUDIT.DESC),
      Index = c("ABS", "GR%", "MS%"),
      stringsAsFactors = FALSE
    )
    result2_m1 <- tmp %>%
      left_join(result2_m)
    result2_m1$final[is.na(result2_m1$final)] <- 0
    if (input$period == "yrl" &&
        as.numeric(substr(date_flag, 5, 6)) < 12) {
      result2_m1$final <- NA
    } else {
      result2_m1 <- result2_m1
    }
    contain[[1]] <- result2_m1
    
    rmb <- rbind.fill(
      graph1(
        summary(),
        # cate=input$category,
        # subcate=input$subcategory,
        value = "RENMINBI",
        period = input$period,
        kpi = c("abs", "gr", "ms"),
        window = 1,
        level = "corporation"
      ),
      graph1(
        summary(),
        # cate=input$category,
        # subcate=input$subcategory,
        value = "RENMINBI",
        period = input$period,
        kpi = c("abs", "gr", "ms"),
        window = 1,
        level = "molecule"
      )
    )
    rmb1 <- distinct(rmb) %>%
      select(AUDIT.DESC:Index, ncol(.))
    date_flag <<- colnames(rmb1)[ncol(rmb1)]
    colnames(rmb1)[ncol(rmb1)] <- "final"
    result2_m <- rmb1 %>%
      filter(
        PRODUCT.DESC == "Overall",
        CORPORATE.DESC == "Overall" |
          CORPORATE.DESC == "B.INGELHEIM",
        COMPS.DESC == "Overall"
      ) %>%
      select(AUDIT.DESC, CORPORATE.DESC, Index, final) %>%
      group_by(AUDIT.DESC, CORPORATE.DESC, Index) %>%
      summarise(final = sum(as.numeric(final), na.rm = TRUE)) %>%
      ungroup() %>%
      filter(
        CORPORATE.DESC == "Overall" | (CORPORATE.DESC == "B.INGELHEIM" &
                                         Index == "MS%"),
        !(CORPORATE.DESC == "Overall" &
            Index == "MS%")
      )
    tmp <- expand.grid(
      AUDIT.DESC = unique(result2_m$AUDIT.DESC),
      Index = c("ABS", "GR%", "MS%"),
      stringsAsFactors = FALSE
    )
    result2_m1 <- tmp %>%
      left_join(result2_m)
    result2_m1$final[is.na(result2_m1$final)] <- 0
    if (input$period == "yrl" &&
        as.numeric(substr(date_flag, 5, 6)) < 12) {
      result2_m1$final <- NA
    } else {
      result2_m1 <- result2_m1
    }
    contain[[2]] <- result2_m1
    contain
  })
  
  tablefor3 <- reactive ({
    result1 <- result1()
    if (!"China"  %in% c(input$region, input$province)) {
      result1 <- result1[which(result1$AUDIT.DESC != "China"),]
    }
    
    result2 <- result1[which(result1$PRODUCT.DESC %in% toplist()), ]
    result2_c <-
      result1[which(result1$CORPORATE.DESC %in% toplist_c()), ]
    result2_m <-
      result1[which(result1$COMPS.DESC %in% toplist_m()), ]
    
    result2$AUDIT.DESC <-
      factor(
        result2$AUDIT.DESC,
        levels = c(
          "China",
          "East Region Audit",
          "South Region Audit",
          "West Region Audit",
          "North Region Audit",
          "Shanghai Hospital Audit",
          "Beijing Hospital Audit"  ,
          "Anhui Provincial Audit",
          "Hebei Provincial Audit",
          "Hubei Provincial Audit",
          "Hunan Provincial Audit",
          "Guangdong Provincial Audit",
          "Hainan Provincial Audit",
          "Ningxia Provincial Audit",
          "Tianjin Hospital Audit",
          "Neimenggu Provincial Audit",
          "Jiangsu Provincial Audit",
          "Guangxi Provincial Audit",
          "Shanxi Provincial Audit",
          "Qinghai Provincial Audit",
          "Shandong Provincial Audit",
          "Henan Provincial Audit",
          "Zhejiang Provincial Audit",
          "Fujian Provincial Audit",
          "Jiangxi Provincial Audit",
          "Liaoning Provincial Audit",
          "Heilongjiang Provincial Audit",
          "Jilin Provincial Audit",
          "Sichuan Provincial Audit",
          "Xinjiang Provincial Audit",
          "Chongqing Hospital Audit",
          "Yunnan Provincial Audit",
          "Shaanxi Provincial Audit",
          "Xizang Provincial Audit",
          "Gansu Provincial Audit",
          "Guizhou Provincial Audit"
        )
      )
    
    result2_c$AUDIT.DESC <-
      factor(
        result2_c$AUDIT.DESC,
        levels = c(
          "China",
          "East Region Audit",
          "South Region Audit",
          "West Region Audit",
          "North Region Audit",
          "Shanghai Hospital Audit",
          "Beijing Hospital Audit"  ,
          "Anhui Provincial Audit",
          "Hebei Provincial Audit",
          "Hubei Provincial Audit",
          "Hunan Provincial Audit",
          "Guangdong Provincial Audit",
          "Hainan Provincial Audit",
          "Ningxia Provincial Audit",
          "Tianjin Hospital Audit",
          "Neimenggu Provincial Audit",
          "Jiangsu Provincial Audit",
          "Guangxi Provincial Audit",
          "Shanxi Provincial Audit",
          "Qinghai Provincial Audit",
          "Shandong Provincial Audit",
          "Henan Provincial Audit",
          "Zhejiang Provincial Audit",
          "Fujian Provincial Audit",
          "Jiangxi Provincial Audit",
          "Liaoning Provincial Audit",
          "Heilongjiang Provincial Audit",
          "Jilin Provincial Audit",
          "Sichuan Provincial Audit",
          "Xinjiang Provincial Audit",
          "Chongqing Hospital Audit",
          "Yunnan Provincial Audit",
          "Shaanxi Provincial Audit",
          "Xizang Provincial Audit",
          "Gansu Provincial Audit",
          "Guizhou Provincial Audit"
        )
      )
    
    result2_m$AUDIT.DESC <-
      factor(
        result2_m$AUDIT.DESC,
        levels = c(
          "China",
          "East Region Audit",
          "South Region Audit",
          "West Region Audit",
          "North Region Audit",
          "Shanghai Hospital Audit",
          "Beijing Hospital Audit"  ,
          "Anhui Provincial Audit",
          "Hebei Provincial Audit",
          "Hubei Provincial Audit",
          "Hunan Provincial Audit",
          "Guangdong Provincial Audit",
          "Hainan Provincial Audit",
          "Ningxia Provincial Audit",
          "Tianjin Hospital Audit",
          "Neimenggu Provincial Audit",
          "Jiangsu Provincial Audit",
          "Guangxi Provincial Audit",
          "Shanxi Provincial Audit",
          "Qinghai Provincial Audit",
          "Shandong Provincial Audit",
          "Henan Provincial Audit",
          "Zhejiang Provincial Audit",
          "Fujian Provincial Audit",
          "Jiangxi Provincial Audit",
          "Liaoning Provincial Audit",
          "Heilongjiang Provincial Audit",
          "Jilin Provincial Audit",
          "Sichuan Provincial Audit",
          "Xinjiang Provincial Audit",
          "Chongqing Hospital Audit",
          "Yunnan Provincial Audit",
          "Shaanxi Provincial Audit",
          "Xizang Provincial Audit",
          "Gansu Provincial Audit",
          "Guizhou Provincial Audit"
        )
      )
    
    result2$PRODUCT.DESC <- factor(result2$PRODUCT.DESC ,
                                   levels =  c("Overall",
                                               setdiff(unique(toplist(
                                                 
                                               )),
                                               "Overall")))
    result2_c$CORPORATE.DESC <- factor(result2_c$CORPORATE.DESC ,
                                       levels = c("Overall",
                                                  setdiff(unique(toplist_c(
                                                    
                                                  )),
                                                  "Overall")))
    result2_c$PRODUCT.DESC <- factor(result2_c$PRODUCT.DESC ,
                                     levels = c("Overall",
                                                setdiff(
                                                  unique(result2_c$PRODUCT.DESC),
                                                  "Overall"
                                                )))
    result2_m$COMPS.DESC <- factor(result2_m$COMPS.DESC ,
                                   levels = c("Overall",
                                              setdiff(unique(toplist_m(
                                                
                                              )),
                                              "Overall")))
    result2_m$PRODUCT.DESC <- factor(result2_m$PRODUCT.DESC ,
                                     levels = c("Overall",
                                                setdiff(
                                                  unique(result2_m$PRODUCT.DESC),
                                                  "Overall"
                                                )))
    
    result3 <-
      result2[order(result2$AUDIT.DESC, result2$PRODUCT.DESC), ]
    result3_c <-
      result2_c[order(result2_c$AUDIT.DESC,
                      result2_c$CORPORATE.DESC,
                      result2_c$PRODUCT.DESC), ]
    result3_m <-
      result2_m[order(result2_m$AUDIT.DESC,
                      result2_m$COMPS.DESC,
                      result2_m$PRODUCT.DESC), ]
    list(result3 = result3,
         result3_c = result3_c,
         result3_m = result3_m)
  })
  
  
  ot <- reactive({
    if (input$goButton == 0)
      return(NULL)
    input$goButton
    isolate({
      outputtable <- tablefor3()
      outputtable
    })
  })
  
  pagenumber <- reactive({
    if (input$top == "3") {
      pageno = 12
    } else if (input$top == "5") {
      pageno = 12
    } else if (input$top == "10") {
      pageno = 11
    } else{
      pageno = 15
    }
    pageno
  })
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##-- KPIs
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$BI_mat_mkt_ms <- renderValueBox({
    input$goButton
    if (input$goButton == 0) {
      return(valueBox(
        0,
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " BI MKT Share(%)"
        ),
        icon = icon("list"),
        width = 4,
        color = "yellow"
      ))
    }
    
    isolate({
      tmp <- result2()[[1]] %>%
        filter(AUDIT.DESC == "China", Index == "MS%") %>%
        select(final) %>%
        unlist()
      
      if (!is.na(tmp)) {
        idx <- nchar(tmp)
        icon_text <-
          ifelse(substr(tmp, 1, 1) == "B",
                 "list",
                 ifelse(
                   substr(tmp, 1, 1) == "-",
                   "thumbs-down",
                   ifelse(as.numeric(substr(
                     tmp, 1, idx - 1
                   ) == 0) ,
                   "list",  "thumbs-up")
                 ))
        color_option =
          ifelse(substr(tmp, 1, 1) == "B",
                 "yellow",
                 ifelse(
                   substr(tmp, 1, 1) == "-",
                   "red",
                   ifelse(as.numeric(substr(
                     tmp, 1, idx - 1
                   ) == 0) ,
                   "yellow",  "green")
                 ))
      } else {
        icon_text <- "list"
        color_option <- "yellow"
      }
      
      valueBox(
        # round(tmp, 2),
        format(tmp, nsmall = 2),
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " BI MKT Share(%)"
        ),
        icon = icon(icon_text),
        width = 4,
        color = color_option
      )
    })
  })
  
  output$BI_mat_mkt_gr <- renderValueBox({
    input$goButton
    if (input$goButton == 0) {
      return(valueBox(
        0,
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " MKT GR(%)"
        ),
        icon = icon("list"),
        width = 4,
        color = "yellow"
      ))
    }
    isolate({
      tmp <- result2()[[1]] %>%
        filter(AUDIT.DESC == "China", Index == "GR%") %>%
        select(final) %>%
        unlist()
      if (!is.na(tmp)) {
        idx <- nchar(tmp)
        icon_text <-
          ifelse(substr(tmp, 1, 1) == "B",
                 "list",
                 ifelse(
                   substr(tmp, 1, 1) == "-",
                   "thumbs-down",
                   ifelse(as.numeric(substr(
                     tmp, 1, idx - 1
                   ) == 0) ,
                   "list",  "thumbs-up")
                 ))
        color_option =
          ifelse(substr(tmp, 1, 1) == "B",
                 "yellow",
                 ifelse(
                   substr(tmp, 1, 1) == "-",
                   "red",
                   ifelse(as.numeric(substr(
                     tmp, 1, idx - 1
                   ) == 0) ,
                   "yellow",  "green")
                 ))
      } else {
        icon_text <- "list"
        color_option <- "yellow"
      }
      
      valueBox(
        # round(tmp, 2),
        format(tmp, nsmall = 2),
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " MKT GR(%)"
        ),
        icon = icon(icon_text),
        width = 4,
        color = color_option
      )
    })
  })
  
  output$BI_mat_mkt_size <- renderValueBox({
    input$goButton
    if (input$goButton == 0) {
      return(valueBox(
        0,
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " MKT Size(Mil)"
        ),
        icon = icon("list"),
        width = 4,
        color = "yellow"
      ))
    }
    
    isolate({
      tmp <- result2()[[1]] %>%
        filter(AUDIT.DESC == "China", Index == "ABS") %>%
        select(final) %>%
        unlist()
      
      valueBox(
        format(
          # round(tmp / 1000000, 2),
          tmp / 1000000,
          big.mark = ",",
          scientific = FALSE,
          digits = 0
        ),
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " MKT Size(Mil)"
        ),
        icon = icon("list"),
        width = 4,
        color = "yellow"
      )
    })
    
  })
  
  output$vis_p <- renderLeaflet({
    input$goButton
    if (input$goButton == 0) {
      return(leaflet() %>%
               amap())
    }
    isolate({
      vis <- result2()[[2]] %>%
        inner_join(geom_mapping, by = "AUDIT.DESC") %>%
        inner_join(geom, by = "province") %>%
        select(AUDIT.DESC, long, lat, Index, final) %>%
        spread(Index, final)
      leaflet(vis) %>%
        setView(lng = 116.45650,
                lat = 40.25063,
                zoom = 4) %>%
        amap() %>%
        addMarkers(~ long,
                   ~ lat,
                   popup = ~ as.character(
                     paste(
                       gsub(" .*$", "", AUDIT.DESC),
                       ": ",
                       "<br>",
                       "<b>",
                       toupper(input$period),
                       " MKT Size(Mil):",
                       round(ABS / 1000000, 2),
                       "<br>",
                       "<b>",
                       toupper(input$period),
                       " MKT GR(%):",
                       round(`GR%`, 2),
                       "<br>",
                       "<b>",
                       toupper(input$period),
                       " BI MKT Share(%):",
                       round(`MS%`, 2),
                       sep = ""
                     )
                   ))
    })
  })
  
  output$BI_mat_mkt_ms_c <- renderValueBox({
    input$goButton
    if (input$goButton == 0) {
      return(valueBox(
        0,
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " BI MKT Share(%)"
        ),
        icon = icon("list"),
        width = 4,
        color = "yellow"
      ))
    }
    isolate({
      tmp <- result2()[[1]] %>%
        filter(AUDIT.DESC == "China", Index == "MS%") %>%
        select(final) %>%
        unlist()
      if (!is.na(tmp)) {
        idx <- nchar(tmp)
        icon_text <-
          ifelse(substr(tmp, 1, 1) == "B",
                 "list",
                 ifelse(
                   substr(tmp, 1, 1) == "-",
                   "thumbs-down",
                   ifelse(as.numeric(substr(
                     tmp, 1, idx - 1
                   ) == 0) ,
                   "list",  "thumbs-up")
                 ))
        color_option =
          ifelse(substr(tmp, 1, 1) == "B",
                 "yellow",
                 ifelse(
                   substr(tmp, 1, 1) == "-",
                   "red",
                   ifelse(as.numeric(substr(
                     tmp, 1, idx - 1
                   ) == 0) ,
                   "yellow",  "green")
                 ))
      } else {
        icon_text <- "list"
        color_option <- "yellow"
      }
      
      valueBox(
        # round(tmp, 2),
        format(tmp, nsmall = 2),
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " BI MKT Share(%)"
        ),
        icon = icon(icon_text),
        width = 4,
        color = color_option
      )
    })
  })
  
  output$BI_mat_mkt_gr_c <- renderValueBox({
    input$goButton
    if (input$goButton == 0) {
      return(valueBox(
        0,
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " MKT GR(%)"
        ),
        icon = icon("list"),
        width = 4,
        color = "yellow"
      ))
    }
    isolate({
      tmp <- result2()[[1]] %>%
        filter(AUDIT.DESC == "China", Index == "GR%") %>%
        select(final) %>%
        unlist()
      
      if (!is.na(tmp)) {
        idx <- nchar(tmp)
        icon_text <-
          ifelse(substr(tmp, 1, 1) == "B",
                 "list",
                 ifelse(
                   substr(tmp, 1, 1) == "-",
                   "thumbs-down",
                   ifelse(as.numeric(substr(
                     tmp, 1, idx - 1
                   ) == 0) ,
                   "list",  "thumbs-up")
                 ))
        color_option =
          ifelse(substr(tmp, 1, 1) == "B",
                 "yellow",
                 ifelse(
                   substr(tmp, 1, 1) == "-",
                   "red",
                   ifelse(as.numeric(substr(
                     tmp, 1, idx - 1
                   ) == 0) ,
                   "yellow",  "green")
                 ))
      } else {
        icon_text <- "list"
        color_option <- "yellow"
      }
      
      valueBox(
        # round(tmp, 2),
        format(tmp, nsmall = 2),
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " MKT GR(%)"
        ),
        icon = icon(icon_text),
        width = 4,
        color = color_option
      )
    })
  })
  
  output$BI_mat_mkt_size_c <- renderValueBox({
    input$goButton
    if (input$goButton == 0) {
      return(valueBox(
        0,
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " MKT Size(Mil)"
        ),
        icon = icon("list"),
        width = 4,
        color = "yellow"
      ))
    }
    isolate({
      tmp <- result2()[[1]] %>%
        filter(AUDIT.DESC == "China", Index == "ABS") %>%
        select(final) %>%
        unlist()
      
      valueBox(
        # round(tmp / 1000000, 2),
        format(
          # round(tmp / 1000000, 2),
          tmp / 1000000,
          big.mark = ",",
          scientific = FALSE,
          digits = 0
        ),
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " MKT Size(Mil)"
        ),
        icon = icon("list"),
        width = 4,
        color = "yellow"
      )
    })
  })
  
  output$vis_p_c <- renderLeaflet({
    input$goButton
    if (input$goButton == 0) {
      return(leaflet() %>%
               amap())
    }
    isolate({
      vis <- result2()[[2]] %>%
        inner_join(geom_mapping, by = "AUDIT.DESC") %>%
        inner_join(geom, by = "province") %>%
        select(AUDIT.DESC, long, lat, Index, final) %>%
        spread(Index, final)
      leaflet(vis) %>%
        setView(lng = 116.45650,
                lat = 40.25063,
                zoom = 4) %>%
        amap() %>%
        addMarkers(~ long,
                   ~ lat,
                   popup = ~ as.character(
                     paste(
                       gsub(" .*$", "", AUDIT.DESC),
                       ": ",
                       "<br>",
                       "<b>",
                       toupper(input$period),
                       " MKT Size(Mil):",
                       round(ABS / 1000000, 2),
                       "<br>",
                       "<b>",
                       toupper(input$period),
                       " MKT GR(%):",
                       round(`GR%`, 2),
                       "<br>",
                       "<b>",
                       toupper(input$period),
                       " BI MKT Share(%):",
                       round(`MS%`, 2),
                       sep = ""
                     )
                   ))
    })
  })
  
  output$BI_mat_mkt_ms_m <- renderValueBox({
    input$goButton
    if (input$goButton == 0) {
      return(valueBox(
        0,
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " BI MKT Share(%)"
        ),
        icon = icon("list"),
        width = 4,
        color = "yellow"
      ))
    }
    isolate({
      tmp <- result2()[[1]] %>%
        filter(AUDIT.DESC == "China", Index == "MS%") %>%
        select(final) %>%
        unlist()
      if (!is.na(tmp)) {
        idx <- nchar(tmp)
        icon_text <-
          ifelse(substr(tmp, 1, 1) == "B",
                 "list",
                 ifelse(
                   substr(tmp, 1, 1) == "-",
                   "thumbs-down",
                   ifelse(as.numeric(substr(
                     tmp, 1, idx - 1
                   ) == 0) ,
                   "list",  "thumbs-up")
                 ))
        color_option =
          ifelse(substr(tmp, 1, 1) == "B",
                 "yellow",
                 ifelse(
                   substr(tmp, 1, 1) == "-",
                   "red",
                   ifelse(as.numeric(substr(
                     tmp, 1, idx - 1
                   ) == 0) ,
                   "yellow",  "green")
                 ))
      } else {
        icon_text <-  "list"
        color_option = "yellow"
      }
      valueBox(
        # round(tmp, 2),
        format(tmp, nsmall = 2),
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " BI MKT Share(%)"
        ),
        icon = icon(icon_text),
        width = 4,
        color = color_option
      )
    })
  })
  
  output$BI_mat_mkt_gr_m <- renderValueBox({
    input$goButton
    if (input$goButton == 0) {
      return(valueBox(
        0,
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " MKT GR(%)"
        ),
        icon = icon("list"),
        width = 4,
        color = "yellow"
      ))
    }
    isolate({
      tmp <- result2()[[1]] %>%
        filter(AUDIT.DESC == "China", Index == "GR%") %>%
        select(final) %>%
        unlist()
      if (!is.na(tmp)) {
        idx <- nchar(tmp)
        icon_text <-
          ifelse(substr(tmp, 1, 1) == "B",
                 "list",
                 ifelse(
                   substr(tmp, 1, 1) == "-",
                   "thumbs-down",
                   ifelse(as.numeric(substr(
                     tmp, 1, idx - 1
                   ) == 0) ,
                   "list",  "thumbs-up")
                 ))
        color_option =
          ifelse(substr(tmp, 1, 1) == "B",
                 "yellow",
                 ifelse(
                   substr(tmp, 1, 1) == "-",
                   "red",
                   ifelse(as.numeric(substr(
                     tmp, 1, idx - 1
                   ) == 0) ,
                   "yellow",  "green")
                 ))
      } else {
        icon_text <- "list"
        color_option = "yellow"
      }
      
      valueBox(
        # round(tmp, 2),
        format(tmp, nsmall = 2),
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " MKT GR(%)"
        ),
        icon = icon(icon_text),
        width = 4,
        color = color_option
      )
    })
  })
  
  output$BI_mat_mkt_size_m <- renderValueBox({
    input$goButton
    if (input$goButton == 0) {
      return(valueBox(
        0,
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " MKT Size(Mil)"
        ),
        icon = icon("list"),
        width = 4,
        color = "yellow"
      ))
    }
    isolate({
      tmp <- result2()[[1]] %>%
        filter(AUDIT.DESC == "China", Index == "ABS") %>%
        select(final) %>%
        unlist()
      
      valueBox(
        # round(tmp / 1000000, 2),
        format(
          # round(tmp / 1000000, 2),
          tmp / 1000000,
          big.mark = ",",
          scientific = FALSE,
          digits = 0
        ),
        paste(
          gsub("\\.", "", date_flag),
          " ",
          toupper(input$period),
          " MKT Size(Mil)"
        ),
        icon = icon("list"),
        width = 4,
        color = "yellow"
      )
    })
  })
  
  output$vis_p_m <- renderLeaflet({
    input$goButton
    if (input$goButton == 0) {
      return(leaflet() %>%
               amap())
    }
    isolate({
      vis <- result2()[[2]] %>%
        inner_join(geom_mapping, by = "AUDIT.DESC") %>%
        inner_join(geom, by = "province") %>%
        select(AUDIT.DESC, long, lat, Index, final) %>%
        spread(Index, final)
      leaflet(vis) %>%
        setView(lng = 116.45650,
                lat = 40.25063,
                zoom = 4) %>%
        amap() %>%
        addMarkers(~ long,
                   ~ lat,
                   popup = ~ as.character(
                     paste(
                       gsub(" .*$", "", AUDIT.DESC),
                       ": ",
                       "<br>",
                       "<b>",
                       toupper(input$period),
                       " MKT Size(Mil):",
                       round(ABS / 1000000, 2),
                       "<br>",
                       "<b>",
                       toupper(input$period),
                       " MKT GR(%):",
                       round(`GR%`, 2),
                       "<br>",
                       "<b>",
                       toupper(input$period),
                       " BI MKT Share(%):",
                       round(`MS%`, 2),
                       sep = ""
                     )
                   ))
    })
  })
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##-- data table
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$contents <- renderDataTable({
    input$goButton
    isolate({
      if (is.null(ot()))
        return(NULL)
      ot <- ot()$result3
      ot <-
        filter(
          ot,
          (CORPORATE.DESC != "Overall" & COMPS.DESC != "Overall") |
            (CORPORATE.DESC == "Overall" &
               COMPS.DESC == "Overall")
        )
      
      ot$AUDIT.DESC <- gsub(" .*$", "", ot$AUDIT.DESC)
      ot$PRODUCT.DESC <- as.character(ot$PRODUCT.DESC)
      ot$CORPORATE.DESC <- as.character(ot$CORPORATE.DESC)
      
      ot$PRODUCT.DESC[which(ot$CORPORATE.DESC == "Overall")] <-
        ot$AUDIT.DESC[which(ot$CORPORATE.DESC == "Overall")]
      KK <- which(ot$PRODUCT.DESC == "Overall")
      
      ot$PRODUCT.DESC[KK] <- ot$CORPORATE.DESC[KK]
      ot$CORPORATE.DESC[KK] <- "Company"
      
      names(ot)[names(ot) == 'AUDIT.DESC'] <- 'AUDIT'
      names(ot)[names(ot) == 'CORPORATE.DESC'] <- 'CORPORATE'
      names(ot)[names(ot) == 'PRODUCT.DESC'] <- 'PRODUCT'
      
      ot$PRODUCT <- as.factor(ot$PRODUCT)
      ot$CORPORATE <- as.factor(ot$CORPORATE)
      ot$AUDIT <- as.factor(ot$AUDIT)
      
      ot <- as.data.frame(ot)
      otnum <- which(grepl("Index", names(ot)))
      #number here need to be updated for new update
      # ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)] <-
      #   format(ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)],
      #          big.mark = ",",
      #          scientific = FALSE)
      ot[, (otnum + 1):length(ot)] <-
        format(
          ot[, (otnum + 1):length(ot)],
          nsmall = 2,
          big.mark = ",",
          scientific = FALSE
        )
      
      ot$Measure[which(ot$Measure == "RENMINBI")] <- "RMB"
      
      if (input$period == "mat" |
          input$period == "ytd" |
          input$period == "yrl" |
          (input$period == "qtr" & as.numeric(input$window) <= 3) |
          (input$period == "rqtr" & input$window == "1") |
          (input$period == "mth" & input$window == "1")) {
        dat <- DT::datatable(
          ot,
          rownames = FALSE,
          extensions = c('FixedColumns', 'Buttons'),
          #filter = 'bottom',
          ##### this sentence need to be changed when new variables added
          options = list(
            columnDefs = list(list(
              visible = FALSE,
              targets =
                c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16)
            )),
            columnDefs = list(list(
              width = '20px',
              targets = "_all"
            )),
            # dom = 'Bfrtpl',
            dom = '<"bottom">Bfrtpl',
            buttons = I('colvis'),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"
            ),
            paging = TRUE,
            scrollX = TRUE,
            lengthMenu = c(
              pagenumber(),
              2 * pagenumber(),
              3 * pagenumber(),
              4 * pagenumber(),
              5 * pagenumber()
            ),
            pageLength = pagenumber(),
            fixedColumns = list(leftColumns = otnum,
                                rightColumns = 0)
          )
        ) %>%
          formatStyle(
            "CORPORATE",
            target = "row",
            fontWeight = styleEqual(c("Company", "Overall"), c('bold', 'bold')),
            backgroundColor = styleEqual(c("Company", "Overall"), c('deepskyblue', "grey")),
            color = styleEqual(c("Company", "Overall"), c('white', 'white'))
          )
      } else {
        dat <- DT::datatable(
          ot,
          rownames = FALSE,
          #filter = 'bottom',
          extensions = c('FixedColumns', 'Buttons'),
          options = list(
            columnDefs = list(list(
              visible = FALSE,
              targets =
                c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16)
            )),
            columnDefs = list(list(
              width = '20px', targets = "_all"
            )),
            autoWidth = TRUE,
            dom = '<"bottom">Bfrtpl',
            buttons = I('colvis'),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"
            ),
            scrollX = TRUE,
            paging = TRUE,
            lengthMenu = c(
              pagenumber(),
              2 * pagenumber(),
              3 * pagenumber(),
              4 * pagenumber(),
              5 * pagenumber()
            ),
            pageLength = pagenumber(),
            fixedColumns = list(leftColumns = otnum,
                                rightColumns = 0)
          )
        ) %>%
          formatStyle(
            "CORPORATE",
            target = "row",
            fontWeight = styleEqual(c("Company", "Overall"), c('bold', 'bold')),
            backgroundColor = styleEqual(c("Company", "Overall"), c('deepskyblue', "grey")),
            color = styleEqual(c("Company", "Overall"), c('white', 'white'))
          )
        #  formatCurrency("CORPORATE",target = "row", currency = "", interval = 3, mark = ",")
        #    formatCurrency( "Index",target = "row",  currency = styleEqual(c("ABS"), c('')),
        #                 interval = styleEqual(c("ABS"), c(3)),  mark = styleEqual(c("ABS"), c( ",")))
        
      }
      return(dat)
    })
  })
  
  output$contents_c <- renderDataTable({
    input$goButton
    isolate({
      if (is.null(ot()))
        return(NULL)
      ot <- ot()$result3_c
      ot <- filter(
        ot,
        (CORPORATE.DESC != "Overall") |
          (CORPORATE.DESC == "Overall" &
             COMPS.DESC == "Overall")
      )
      
      if ("ALL" %in% input$province) {
        summary <-
          summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, province())), ]
      } else {
        summary <-
          summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)), ]
      }
      
      top_brand <- graph1m(
        summary,
        # cate=input$category,
        # subcate=input$subcategory,
        value = "RENMINBI",
        period = input$period,
        kpi = "abs",
        window = 1,
        level = "corporation",
        top = as.integer(input$top2)
      ) %>%
        select(AUDIT.DESC, PRODUCT.DESC, CORPORATE.DESC)
      
      ot <- ot %>% inner_join(top_brand)
      
      ot$AUDIT.DESC <- gsub(" .*$", "", ot$AUDIT.DESC)
      
      ot$PRODUCT.DESC <- as.character(ot$PRODUCT.DESC)
      ot$CORPORATE.DESC <- as.character(ot$CORPORATE.DESC)
      
      ot$PRODUCT.DESC[which(ot$CORPORATE.DESC == "Overall")] <-
        ot$AUDIT.DESC[which(ot$CORPORATE.DESC == "Overall")]
      KK <- which(ot$PRODUCT.DESC == "Overall")
      
      ot$PRODUCT.DESC[KK] <- ot$CORPORATE.DESC[KK]
      ot$CORPORATE.DESC[KK] <- "Company"
      
      names(ot)[names(ot) == 'AUDIT.DESC'] <- 'AUDIT'
      names(ot)[names(ot) == 'CORPORATE.DESC'] <- 'CORPORATE'
      names(ot)[names(ot) == 'PRODUCT.DESC'] <- 'PRODUCT'
      
      ot$PRODUCT <- as.factor(ot$PRODUCT)
      ot$CORPORATE <- as.factor(ot$CORPORATE)
      ot$AUDIT <- as.factor(ot$AUDIT)
      
      ot <- as.data.frame(ot)
      otnum <- which(grepl("Index", names(ot)))
      #number here need to be updated for new update
      # ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)] <-
      #   format(ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)],
      #          big.mark = ",",
      #          scientific = FALSE)
      
      ot[, (otnum + 1):length(ot)] <-
        format(
          ot[, (otnum + 1):length(ot)],
          nsmall = 2,
          big.mark = ",",
          scientific = FALSE
        )
      
      ot$Measure[which(ot$Measure == "RENMINBI")] <- "RMB"
      
      if (input$period == "mat" |
          input$period == "ytd" |
          input$period == "yrl" |
          input$period == "qtr" & as.numeric(input$window) <= 3 |
          (input$period == "rqtr" & input$window == "1") |
          (input$period == "mth" & input$window == "1")) {
        dat <- DT::datatable(
          ot,
          rownames = FALSE,
          extensions = c('FixedColumns', 'Buttons'),
          #filter = 'bottom',
          ##### this sentence need to be changed when new variables added
          options = list(
            columnDefs = list(list(
              visible = FALSE,
              targets =
                c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16)
            )),
            columnDefs = list(list(
              width = '20px', targets = "_all"
            )),
            dom = 'Bfrtpl',
            buttons = I('colvis'),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"
            ),
            paging = TRUE,
            scrollX = TRUE,
            lengthMenu = c(
              pagenumber(),
              2 * pagenumber(),
              3 * pagenumber(),
              4 * pagenumber(),
              5 * pagenumber()
            ),
            pageLength = pagenumber(),
            fixedColumns = list(leftColumns = otnum,
                                rightColumns = 0)
          )
        ) %>%
          formatStyle(
            "CORPORATE",
            target = "row",
            fontWeight = styleEqual(c("Company", "Overall"), c('bold', 'bold')),
            backgroundColor = styleEqual(c("Company", "Overall"), c('deepskyblue', "grey")),
            color = styleEqual(c("Company", "Overall"), c('white', 'white'))
          )
      } else{
        dat <- DT::datatable(
          ot,
          rownames = FALSE,
          #filter = 'bottom',
          extensions = c('FixedColumns', 'Buttons'),
          options = list(
            columnDefs = list(list(
              visible = FALSE,
              targets =
                c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16)
            )),
            columnDefs = list(list(
              width = '20px', targets = "_all"
            )),
            autoWidth = TRUE,
            dom = '<"bottom">Bfrtpl',
            buttons = I('colvis'),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"
            ),
            scrollX = TRUE,
            paging = TRUE,
            lengthMenu = c(
              pagenumber(),
              2 * pagenumber(),
              3 * pagenumber(),
              4 * pagenumber(),
              5 * pagenumber()
            ),
            pageLength = pagenumber(),
            fixedColumns = list(leftColumns = otnum,
                                rightColumns = 0)
          )
        ) %>%
          formatStyle(
            "CORPORATE",
            target = "row",
            fontWeight = styleEqual(c("Company", "Overall"), c('bold', 'bold')),
            backgroundColor = styleEqual(c("Company", "Overall"), c('deepskyblue', "grey")),
            color = styleEqual(c("Company", "Overall"), c('white', 'white'))
          )
        #  formatCurrency("CORPORATE",target = "row", currency = "", interval = 3, mark = ",")
        #    formatCurrency( "Index",target = "row",  currency = styleEqual(c("ABS"), c('')),
        #                 interval = styleEqual(c("ABS"), c(3)),  mark = styleEqual(c("ABS"), c( ",")))
        
      }
      return(dat)
    })
  })
  
  output$contents_m <- renderDataTable({
    input$goButton
    isolate({
      if (is.null(ot()))
        return(NULL)
      ot <- ot()$result3_m
      ot <- filter(
        ot,
        (COMPS.DESC != "Overall") |
          (CORPORATE.DESC == "Overall" &
             COMPS.DESC == "Overall")
      )
      
      if ("ALL" %in% input$province) {
        summary <-
          summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())), ]
      } else {
        summary <-
          summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)), ]
      }
      
      top_brand <- graph1m(
        summary,
        # cate=input$category,
        # subcate=input$subcategory,
        value = "RENMINBI",
        period = input$period,
        kpi = "abs",
        window = 1,
        level = "molecule",
        top = as.integer(input$top3)
      ) %>%
        select(AUDIT.DESC, PRODUCT.DESC, COMPS.DESC)
      ot <- ot %>% inner_join(top_brand)
      ot$AUDIT.DESC <- gsub(" .*$", "", ot$AUDIT.DESC)
      
      ot$PRODUCT.DESC <- as.character(ot$PRODUCT.DESC)
      ot$COMPS.DESC <- as.character(ot$COMPS.DESC)
      
      ot$PRODUCT.DESC[which(ot$COMPS.DESC == "Overall")] <-
        ot$AUDIT.DESC[which(ot$COMPS.DESC == "Overall")]
      KK <- which(ot$PRODUCT.DESC == "Overall")
      
      ot$PRODUCT.DESC[KK] <- ot$COMPS.DESC[KK]
      ot$COMPS.DESC[KK] <- "Molecule"
      
      names(ot)[names(ot) == 'AUDIT.DESC'] <- 'AUDIT'
      names(ot)[names(ot) == 'CORPORATE.DESC'] <- 'CORPORATE'
      names(ot)[names(ot) == 'PRODUCT.DESC'] <- 'PRODUCT'
      
      ot$PRODUCT <- as.factor(ot$PRODUCT)
      ot$CORPORATE <- as.factor(ot$CORPORATE)
      ot$AUDIT <- as.factor(ot$AUDIT)
      
      ot <- as.data.frame(ot)
      otnum <- which(grepl("Index", names(ot)))
      #number here need to be updated for new update
      # ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)] <-
      #   format(ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)],
      #          big.mark = ",",
      #          scientific = FALSE)
      ot[, (otnum + 1):length(ot)] <-
        format(
          ot[, (otnum + 1):length(ot)],
          nsmall = 2,
          big.mark = ",",
          scientific = FALSE
        )
      
      ot$Measure[which(ot$Measure == "RENMINBI")] <- "RMB"
      
      if (input$period == "mat" |
          input$period == "ytd" |
          input$period == "yrl" |
          input$period == "qtr" & as.numeric(input$window) <= 3 |
          (input$period == "rqtr" & input$window == "1") |
          (input$period == "mth" & input$window == "1")) {
        dat <- DT::datatable(
          ot ,
          rownames = FALSE,
          extensions = c('FixedColumns', 'Buttons'),
          #filter = 'bottom',
          ##### this sentence need to be changed when new variables added
          options = list(
            columnDefs = list(list(
              visible = FALSE,
              targets =
                c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16)
            )),
            columnDefs = list(list(
              width = '20px', targets = "_all"
            )),
            dom = 'Bfrtpl',
            buttons = I('colvis'),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"
            ),
            
            scrollX = TRUE,
            paging = TRUE,
            fixedColumns = list(leftColumns = otnum, rightColumns = 0),
            lengthMenu = c(
              pagenumber(),
              2 * pagenumber(),
              3 * pagenumber(),
              4 * pagenumber(),
              5 * pagenumber()
            ),
            pageLength = pagenumber(),
            fixedColumns = list(leftColumns = otnum, rightColumns = 0)
          )
        ) %>%
          formatStyle(
            "COMPS.DESC",
            target = "row",
            fontWeight = styleEqual(c("Molecule", "Overall"), c('bold', 'bold')),
            backgroundColor = styleEqual(c("Molecule", "Overall"), c('deepskyblue', "grey")),
            color = styleEqual(c("Molecule", "Overall"), c('white', 'white'))
          )
      } else{
        dat <- DT::datatable(
          ot,
          rownames = FALSE,
          #filter = 'bottom',
          extensions = c('FixedColumns', 'Buttons'),
          options = list(
            columnDefs = list(list(
              visible = FALSE,
              targets =
                c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16)
            )),
            columnDefs = list(list(
              width = '20px', targets = "_all"
            )),
            autoWidth = TRUE,
            dom = '<"bottom">Bfrtpl',
            buttons = I('colvis'),
            initComplete = JS(
              "function(settings, json) {",
              "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
              "}"
            ),
            
            scrollX = TRUE,
            paging = TRUE,
            lengthMenu = c(
              pagenumber(),
              2 * pagenumber(),
              3 * pagenumber(),
              4 * pagenumber(),
              5 * pagenumber()
            ),
            pageLength = pagenumber(),
            fixedColumns = list(leftColumns = otnum, rightColumns = 0)
          )
        ) %>%
          formatStyle(
            "COMPS.DESC",
            target = "row",
            fontWeight = styleEqual(c("Molecule", "Overall"), c('bold', 'bold')),
            backgroundColor = styleEqual(c("Molecule", "Overall"), c('deepskyblue', "grey")),
            color = styleEqual(c("Molecule", "Overall"), c('white', 'white'))
          )
        #  formatCurrency("CORPORATE",target = "row", currency = "", interval = 3, mark = ",")
        #    formatCurrency( "Index",target = "row",  currency = styleEqual(c("ABS"), c('')),
        #                 interval = styleEqual(c("ABS"), c(3)),  mark = styleEqual(c("ABS"), c( ",")))
        
      }
      return(dat)
    })
  })
  
  
  ################################### download data############################
  
  writeDown <- function(data1, data2, data3) {
    wb <- createWorkbook()
    ## 1
    addWorksheet(wb, "Product")
    writeDataTable(
      wb,
      sheet = "Product",
      x = data1,
      withFilter = F,
      startRow = 1,
      rowNames = F,
      colNames = T
    )
    ## 2
    addWorksheet(wb, "Corporation")
    writeDataTable(
      wb,
      sheet = "Corporation",
      x = data2,
      withFilter = F,
      startRow = 1,
      rowNames = F,
      colNames = T
    )
    ## 3
    addWorksheet(wb, "Molecule")
    writeDataTable(
      wb,
      sheet = "Molecule",
      x = data3,
      withFilter = F,
      startRow = 1,
      rowNames = F,
      colNames = T
    )
    return(wb)
  }
  
  otdown <- reactive({
    if (input$goButton == 0)
      return(NULL)
    input$goButton
    isolate({
      outputtable <- tablefor3()$result3
      outputtable <- filter(
        outputtable,
        (CORPORATE.DESC != "Overall" &
           COMPS.DESC != "Overall") |
          (CORPORATE.DESC == "Overall" &
             COMPS.DESC == "Overall")
      )
      names(outputtable) <- paste0(names(outputtable), "\t")
      outputtable
    })
  })
  
  otdown_c <- reactive({
    if (input$goButton == 0)
      return(NULL)
    input$goButton
    isolate({
      outputtable <- tablefor3()$result3_c
      outputtable <-
        filter(
          outputtable,
          (CORPORATE.DESC != "Overall") |
            (CORPORATE.DESC == "Overall" &
               COMPS.DESC == "Overall")
        )
      if ("ALL" %in% input$province) {
        summary <-
          summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, province())), ]
      } else {
        summary <-
          summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)), ]
      }
      
      top_brand <- graph1m(
        summary,
        # cate=input$category,
        # subcate=input$subcategory,
        value = "RENMINBI",
        period = input$period,
        kpi = "abs",
        window = 1,
        level = "corporation",
        top = as.integer(input$top2)
      ) %>%
        select(AUDIT.DESC, PRODUCT.DESC, CORPORATE.DESC)
      
      outputtable <- outputtable %>% inner_join(top_brand)
      names(outputtable) <- paste0(names(outputtable), "\t")
      outputtable
    })
  })
  
  otdown_m <- reactive({
    if (input$goButton == 0)
      return(NULL)
    input$goButton
    isolate({
      outputtable <- tablefor3()$result3_m
      outputtable <- filter(
        outputtable,
        (COMPS.DESC != "Overall") |
          (CORPORATE.DESC == "Overall" &
             COMPS.DESC == "Overall")
      )
      if ("ALL" %in% input$province) {
        summary <-
          summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())), ]
      } else {
        summary <-
          summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)), ]
      }
      
      top_brand <- graph1m(
        summary,
        # cate=input$category,
        # subcate=input$subcategory,
        value = "RENMINBI",
        period = input$period,
        kpi = "abs",
        window = 1,
        level = "molecule",
        top = as.integer(input$top3)
      ) %>%
        select(AUDIT.DESC, PRODUCT.DESC, COMPS.DESC)
      outputtable <- outputtable %>% inner_join(top_brand)
      names(outputtable) <- paste0(names(outputtable), "\t")
      outputtable
    })
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("CHPA_Data_", input$period, "_", input$window, "year",  '.xlsx', sep = '')
    },
    content = function(file) {
      saveWorkbook(writeDown(otdown(), otdown_c(), otdown_m()),
                   file,
                   overwrite = TRUE)
      
      
    }
  )
  ################################ for plot ####################################
  value_m <- c("UNIT", "RMB")
  names(value_m) <- c("UNIT", "Value In RMB")
  value_choice = list("UNIT" = "UNIT",
                      "Value In RMB" = "RMB")
  
  kpi_m <- c("abs", "ms", "mc", "gr", "ev", "pi")
  names(kpi_m) <- c("Sales", "MS%", "MS+/- %", "GR%", "EV", "PI")
  kpi_choice = list(
    "Sales" = "abs",
    "MS%" = "ms",
    "MS+/- %" = "mc",
    "GR%" = "gr",
    "EV" = "ev",
    "PI" = "pi"
  )
  
  
  observeEvent(input$goButton, {
    toggleState(id = "bp_p", condition = as.integer(input$top) > 0)
    toggleState(id = "rpp_p", condition = as.integer(input$top) > 0)
    
    toggleState(id = "sub_top", condition = as.integer(input$top) > 0)
    toggleState(id = "sub_top_c", condition = as.integer(input$top) > 0)
    toggleState(id = "sub_top_m", condition = as.integer(input$top) > 0)
    
    toggleState(id = "sub_region", condition = as.integer(input$top) > 0)
    toggleState(id = "sub_region_c", condition = as.integer(input$top) > 0)
    toggleState(id = "sub_region_m", condition = as.integer(input$top) > 0)
    
    
    if ("ALL" %in% input$province) {
      tmp_region <- setdiff(province(), "ALL")
    } else {
      tmp_region <- input$province
    }
    
    brand_choice <- unique(toplist())
    
    if ("RENMINBI" %in% input$value) {
      tmp_value <- c("RMB", input$value[input$value != "RENMINBI"])
    } else {
      tmp_value <- input$value
    }
    
    corporation_choice <- unique(toplist_c())
    molecule_choice <- unique(toplist_m())
    
    ##-- for brand
    observe({
      if (input$bp_p) {
        updateMaterialSwitch(session, inputId = "rpp_p",
                             value = FALSE)
      } else {
        updateMaterialSwitch(session, inputId = "rpp_p",
                             value = TRUE)
      }
    })
    
    observe({
      if (input$rpp_p) {
        updateMaterialSwitch(session, inputId = "bp_p",
                             value = FALSE)
      } else {
        updateMaterialSwitch(session, inputId = "bp_p",
                             value = TRUE)
      }
    })
    
    
    observe({
      if (input$bp_p) {
        output$sub_top_ui <- renderUI(selectInput(
          "sub_top",
          "Top Brand",
          choices = brand_choice[-1],
          multiple = TRUE
        ))
      } else {
        output$sub_top_ui <- renderUI(selectInput(
          "sub_top",
          "Top Brand",
          choices = brand_choice[-1],
          multiple = FALSE
        ))
      }
    })
    
    output$sub_measure_ui <- renderUI(
      selectInput(
        "sub_measure",
        "Measure",
        choices = value_choice[names(value_m[value_m %in% tmp_value])],
        selected = tmp_value[1],
        multiple = FALSE
      )
    )
    
    output$sub_index_ui <- renderUI(
      selectInput(
        "sub_index",
        "Index",
        choices = kpi_choice[names(kpi_m[kpi_m %in% input$kpi])],
        selected = input$kpi[1],
        multiple = FALSE
      )
    )
    
    observe({
      if (input$rpp_p) {
        output$sub_region_ui <- renderUI(selectInput(
          "sub_region",
          "Region/Province",
          choices = c(input$region, tmp_region),
          multiple = TRUE
        ))
      } else {
        output$sub_region_ui <- renderUI(selectInput(
          "sub_region",
          "Region/Province",
          choices = c(input$region, tmp_region),
          multiple = FALSE
        ))
      }
    })
    
    ##-- for corporation
    observe({
      if (input$bp_p_c) {
        updateMaterialSwitch(session, inputId = "rpp_p_c",
                             value = FALSE)
      } else {
        updateMaterialSwitch(session, inputId = "rpp_p_c",
                             value = TRUE)
      }
    })
    
    observe({
      if (input$rpp_p_c) {
        updateMaterialSwitch(session, inputId = "bp_p_c",
                             value = FALSE)
      } else {
        updateMaterialSwitch(session, inputId = "bp_p_c",
                             value = TRUE)
      }
    })
    
    observe({
      # toggleState(id = "bp_p_c", condition = as.integer(input$rpp_p_c) == 0)
      # toggleState(id = "rpp_p_c", condition = as.integer(input$bp_p_c) == 0)
      if (input$bp_p_c) {
        output$sub_top_c_ui <- renderUI(
          selectInput(
            "sub_top_c",
            "Top Corporation",
            choices = corporation_choice[-1],
            multiple = TRUE
          )
        )
      } else {
        output$sub_top_c_ui <- renderUI(
          selectInput(
            "sub_top_c",
            "Top Corporation",
            choices = corporation_choice[-1],
            multiple = FALSE
          )
        )
      }
      output$sub_measure_c_ui <- renderUI(
        selectInput(
          "sub_measure_c",
          "Measure",
          choices = value_choice[names(value_m[value_m %in% tmp_value])],
          selected = tmp_value[1],
          multiple = FALSE
        )
      )
      
      output$sub_index_c_ui <- renderUI(
        selectInput(
          "sub_index_c",
          "Index",
          choices = kpi_choice[names(kpi_m[kpi_m %in% input$kpi])],
          selected = input$kpi[1],
          multiple = FALSE
        )
      )
      
      if (input$rpp_p_c) {
        output$sub_region_c_ui <- renderUI(selectInput(
          "sub_region_c",
          "Region/Province",
          choices = c(input$region, tmp_region),
          multiple = TRUE
        ))
      } else {
        output$sub_region_c_ui <- renderUI(
          selectInput(
            "sub_region_c",
            "Region/Province",
            choices = c(input$region, tmp_region),
            multiple = FALSE
          )
        )
      }
    })
    
    ##-- for molecule
    observe({
      if (input$bp_p_m) {
        updateMaterialSwitch(session, inputId = "rpp_p_m",
                             value = FALSE)
      } else {
        updateMaterialSwitch(session, inputId = "rpp_p_m",
                             value = TRUE)
      }
    })
    
    observe({
      if (input$rpp_p_m) {
        updateMaterialSwitch(session, inputId = "bp_p_m",
                             value = FALSE)
      } else {
        updateMaterialSwitch(session, inputId = "bp_p_m",
                             value = TRUE)
      }
    })
    
    observe({
      # toggleState(id = "bp_p_m", condition = as.integer(input$rpp_p_m) == 0)
      # toggleState(id = "rpp_p_m", condition = as.integer(input$bp_p_m) == 0)
      if (input$bp_p_m) {
        output$sub_top_m_ui <- renderUI(
          selectInput(
            "sub_top_m",
            "Top Molecule",
            choices = molecule_choice[-1],
            multiple = TRUE
          )
        )
      } else {
        output$sub_top_m_ui <- renderUI(
          selectInput(
            "sub_top_m",
            "Top Molecule",
            choices = molecule_choice[-1],
            multiple = FALSE
          )
        )
      }
      
      output$sub_measure_m_ui <- renderUI(
        selectInput(
          "sub_measure_m",
          "Measure",
          choices = value_choice[names(value_m[value_m %in% tmp_value])],
          selected = tmp_value[1],
          multiple = FALSE
        )
      )
      
      output$sub_index_m_ui <- renderUI(
        selectInput(
          "sub_index_m",
          "Index",
          choices = kpi_choice[names(kpi_m[kpi_m %in% input$kpi])],
          selected = input$kpi[1],
          multiple = FALSE
        )
      )
      
      if (input$rpp_p_m) {
        output$sub_region_m_ui <- renderUI(selectInput(
          "sub_region_m",
          "Region/Province",
          choices = c(input$region, tmp_region),
          multiple = TRUE
        ))
      } else {
        output$sub_region_m_ui <- renderUI(
          selectInput(
            "sub_region_m",
            "Region/Province",
            choices = c(input$region, tmp_region),
            multiple = FALSE
          )
        )
      }
    })
  })
  
  product_plot <- NULL
  observeEvent(input$goButton_p, {
    product_plot <<- reactive({
      input$goButton_p
      isolate({
        if (is.null(ot()))
          return(NULL)
        
        ot <- ot()$result3
        ot <-
          filter(
            ot,
            (CORPORATE.DESC != "Overall" &
               COMPS.DESC != "Overall") |
              (CORPORATE.DESC == "Overall" &
                 COMPS.DESC == "Overall")
          )
        ot$AUDIT.DESC <- gsub(" .*$", "", ot$AUDIT.DESC)
        
        ot$PRODUCT.DESC <- as.character(ot$PRODUCT.DESC)
        ot$CORPORATE.DESC <- as.character(ot$CORPORATE.DESC)
        
        ot$PRODUCT.DESC[which(ot$CORPORATE.DESC == "Overall")] <-
          ot$AUDIT.DESC[which(ot$CORPORATE.DESC == "Overall")]
        KK <- which(ot$PRODUCT.DESC == "Overall")
        
        ot$PRODUCT.DESC[KK] <- ot$CORPORATE.DESC[KK]
        ot$CORPORATE.DESC[KK] <- "Company"
        
        names(ot)[names(ot) == 'AUDIT.DESC'] <- 'AUDIT'
        names(ot)[names(ot) == 'CORPORATE.DESC'] <- 'CORPORATE'
        names(ot)[names(ot) == 'PRODUCT.DESC'] <- 'PRODUCT'
        
        ot$PRODUCT <- as.factor(ot$PRODUCT)
        ot$CORPORATE <- as.factor(ot$CORPORATE)
        ot$AUDIT <- as.factor(ot$AUDIT)
        
        ot <- as.data.frame(ot)
        otnum <- which(grepl("Index", names(ot)))
        #number here need to be updated for new update
        ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)] <-
          format(ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)],
                 big.mark = ",", scientific = FALSE)
        ot$Measure[which(ot$Measure == "RENMINBI")] <- "RMB"
        
        if (input$sub_index == "ms") {
          tmp_sub_index <- "MS%"
        } else if (input$sub_index == "gr") {
          tmp_sub_index <- "GR%"
        } else if (input$sub_index == "mc") {
          tmp_sub_index <- "MS+/- %"
        } else {
          tmp_sub_index <- input$sub_index
        }
        
        if (as.integer(input$top) > 0) {
          plot_data <- ot %>%
            filter(
              Measure == input$sub_measure,
              Index == toupper(tmp_sub_index),
              AUDIT %in% gsub(" .*$", "", input$sub_region),
              # COMPS.DESC %in% c("Molecule", "Overall"),
              # (as.character(PRODUCT) == as.character(AUDIT) |
              as.character(PRODUCT) %in% input$sub_top
              # )
            ) %>%
            # toplist()[1:(1 + as.integer(input$sub_top))])) %>%
            gather(
              Date,
              Value,-AUDIT,-MARKET.DESC,-MANUF.TYPE.DESC,
              -TC.I.SHORT.DESC,-TC.I.DESC,-TC.II.SHORT.DESC,
              -TC.II.DESC,-TC.III.SHORT.DESC,-TC.II.DESC,
              -TC.III.SHORT.DESC,-TC.III.DESC,
              -CORPORATE,-CORP.CN,-PRD.CODE,-PRODUCT,
              -PRD.CN,-COMPS.ABBR,-COMPS.DESC,-COMPS.CN,
              -Measure,-Index
            ) %>%
            mutate(
              Date = as.Date(ymd(paste(
                Date, "01", "."
              ))),
              Value = as.numeric(gsub(",", "", Value)),
              Date1 = substr(as.character(Date), 1, 7)
            )
        } else {
          plot_data <- ot %>%
            filter(
              Measure == input$sub_measure,
              Index == toupper(tmp_sub_index),
              # AUDIT %in% gsub(" .*$", "", input$sub_region),
              # COMPS.DESC %in% c("Molecule", "Overall"),
              # (
              as.character(PRODUCT) == as.character(AUDIT)
              # |
              # as.character(PRODUCT) %in% input$sub_top
              # )
            ) %>%
            # toplist()[1:(1 + as.integer(input$sub_top))])) %>%
            gather(
              Date,
              Value,-AUDIT,-MARKET.DESC,-MANUF.TYPE.DESC,
              -TC.I.SHORT.DESC,-TC.I.DESC,-TC.II.SHORT.DESC,
              -TC.II.DESC,-TC.III.SHORT.DESC,-TC.II.DESC,
              -TC.III.SHORT.DESC,-TC.III.DESC,
              -CORPORATE,-CORP.CN,-PRD.CODE,-PRODUCT,
              -PRD.CN,-COMPS.ABBR,-COMPS.DESC,-COMPS.CN,
              -Measure,-Index
            ) %>%
            mutate(
              Date = as.Date(ymd(paste(
                Date, "01", "."
              ))),
              Value = as.numeric(gsub(",", "", Value)),
              Date1 = substr(as.character(Date), 1, 7)
            )
        }
        
        tickval <- unlist(distinct(plot_data, Date1))
        
        if ((as.integer(input$top) > 0 && input$bp_p) ||
            as.numeric(input$top) == 0) {
          if (input$label_p) {
            if (input$sub_index %in% c("abs")) {
              p <- plot_ly(hoverinfo = "x+y+name")
              for (i in as.character(unique(plot_data$PRODUCT))) {
                if (grepl("B.I$", i)) {
                  p <- add_trace(
                    p,
                    x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
                    y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"] / 1000000,
                    type = "scatter",
                    mode = "lines",
                    color = list("blue"),
                    colors = "Set1",
                    name = i
                  ) %>%
                    add_text(
                      x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
                      y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"] / 1000000,
                      text = round(plot_data[as.character(plot_data$PRODUCT) == i, "Value"]  / 1000000, 1),
                      textfont = list(size = 10),
                      textposition = "top",
                      hoverinfo = "none",
                      showlegend = FALSE
                    )
                } else {
                  p <- add_trace(
                    p,
                    x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
                    y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"] / 1000000,
                    type = "scatter",
                    mode = "lines",
                    colors = "Set1",
                    name = i
                  ) %>%
                    add_text(
                      x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
                      y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"]  / 1000000,
                      text = round(plot_data[as.character(plot_data$PRODUCT) == i, "Value"]  / 1000000, 1),
                      textfont = list(size = 10),
                      textposition = "top",
                      hoverinfo = "none",
                      showlegend = FALSE
                    )
                }
                
              }
              
              p %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value(Million)",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
            } else {
              p <- plot_ly(hoverinfo = "x+y+name")
              for (i in as.character(unique(plot_data$PRODUCT))) {
                if (grepl("B.I$", i)) {
                  p <- add_trace(
                    p,
                    x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
                    y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"],
                    type = "scatter",
                    mode = "lines",
                    color = list("blue"),
                    colors = "Set1",
                    name = i
                  ) %>%
                    add_text(
                      x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
                      y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"],
                      text = round(plot_data[as.character(plot_data$PRODUCT) == i, "Value"], 1),
                      textfont = list(size = 10),
                      textposition = "top",
                      hoverinfo = "none",
                      showlegend = FALSE
                    )
                } else {
                  p <- add_trace(
                    p,
                    x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
                    y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"],
                    type = "scatter",
                    mode = "lines",
                    colors = "Set1",
                    name = i
                  ) %>%
                    add_text(
                      x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
                      y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"],
                      text = round(plot_data[as.character(plot_data$PRODUCT) == i, "Value"], 1),
                      textfont = list(size = 10),
                      textposition = "top",
                      hoverinfo = "none",
                      showlegend = FALSE
                    )
                }
                
              }
              
              p %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "<b>Value</b>",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
            }
            
          } else {
            if (input$sub_index %in% c("abs")) {
              p <- plot_ly(hoverinfo = "x+y+name")
              for (i in as.character(unique(plot_data$PRODUCT))) {
                if (grepl("B.I$", i)) {
                  p <- add_trace(
                    p,
                    x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
                    y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"] / 1000000,
                    type = "scatter",
                    mode = "lines",
                    color = list("blue"),
                    colors = "Set1",
                    name = i
                  )
                } else {
                  p <- add_trace(
                    p,
                    x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
                    y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"] / 1000000,
                    type = "scatter",
                    mode = "lines",
                    colors = "Set1",
                    name = i
                  )
                }
              }
              
              p %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value(Million)",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
              
            } else {
              p <- plot_ly(hoverinfo = "x+y+name")
              for (i in as.character(unique(plot_data$PRODUCT))) {
                if (grepl("B.I$", i)) {
                  p <- add_trace(
                    p,
                    x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
                    y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"],
                    type = "scatter",
                    mode = "lines",
                    color = list("blue"),
                    colors = "Set1",
                    name = i
                  )
                } else {
                  p <- add_trace(
                    p,
                    x = plot_data[as.character(plot_data$PRODUCT) == i, "Date"],
                    y = plot_data[as.character(plot_data$PRODUCT) == i, "Value"],
                    type = "scatter",
                    mode = "lines",
                    colors = "Set1",
                    name = i
                  )
                }
              }
              p %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
            }
          }
        } else {
          if (input$label_p) {
            if (input$sub_index %in% c("abs")) {
              p <- plot_ly(hoverinfo = "x+y+name")
              for (i in as.character(unique(plot_data$AUDIT))) {
                if (grepl("B.I$", i)) {
                  p <- add_trace(
                    p,
                    x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
                    y = plot_data[as.character(plot_data$AUDIT) == i, "Value"] / 1000000,
                    type = "scatter",
                    mode = "lines",
                    color = list("blue"),
                    colors = "Set1",
                    name = i
                  ) %>%
                    add_text(
                      x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
                      y = plot_data[as.character(plot_data$AUDIT) == i, "Value"] / 1000000,
                      text = round(plot_data[as.character(plot_data$AUDIT) == i, "Value"]  / 1000000, 1),
                      textfont = list(size = 10),
                      textposition = "top",
                      hoverinfo = "none",
                      showlegend = FALSE
                    )
                } else {
                  p <- add_trace(
                    p,
                    x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
                    y = plot_data[as.character(plot_data$AUDIT) == i, "Value"] / 1000000,
                    type = "scatter",
                    mode = "lines",
                    colors = "Set1",
                    name = i
                  ) %>%
                    add_text(
                      x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
                      y = plot_data[as.character(plot_data$AUDIT) == i, "Value"]  / 1000000,
                      text = round(plot_data[as.character(plot_data$AUDIT) == i, "Value"]  / 1000000, 1),
                      textfont = list(size = 10),
                      textposition = "top",
                      hoverinfo = "none",
                      showlegend = FALSE
                    )
                }
              }
              p %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value(Million)",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
            } else {
              p <- plot_ly(hoverinfo = "x+y+name")
              for (i in as.character(unique(plot_data$AUDIT))) {
                if (grepl("B.I$", i)) {
                  p <- add_trace(
                    p,
                    x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
                    y = plot_data[as.character(plot_data$AUDIT) == i, "Value"],
                    type = "scatter",
                    mode = "lines",
                    color = list("blue"),
                    colors = "Set1",
                    name = i
                  ) %>%
                    add_text(
                      x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
                      y = plot_data[as.character(plot_data$AUDIT) == i, "Value"],
                      text = round(plot_data[as.character(plot_data$AUDIT) == i, "Value"], 1),
                      textfont = list(size = 10),
                      textposition = "top",
                      hoverinfo = "none",
                      showlegend = FALSE
                    )
                } else {
                  p <- add_trace(
                    p,
                    x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
                    y = plot_data[as.character(plot_data$AUDIT) == i, "Value"],
                    type = "scatter",
                    mode = "lines",
                    colors = "Set1",
                    name = i
                  ) %>%
                    add_text(
                      x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
                      y = plot_data[as.character(plot_data$AUDIT) == i, "Value"],
                      text = round(plot_data[as.character(plot_data$AUDIT) == i, "Value"], 1),
                      textfont = list(size = 10),
                      textposition = "top",
                      hoverinfo = "none",
                      showlegend = FALSE
                    )
                }
              }
              p %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
            }
          } else {
            if (input$sub_index %in% c("abs")) {
              p <- plot_ly(hoverinfo = "x+y+name")
              for (i in as.character(unique(plot_data$AUDIT))) {
                if (grepl("B.I$", i)) {
                  p <- add_trace(
                    p,
                    x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
                    y = plot_data[as.character(plot_data$AUDIT) == i, "Value"] / 1000000,
                    type = "scatter",
                    mode = "lines",
                    color = list("blue"),
                    colors = "Set1",
                    name = i
                  )
                } else {
                  p <- add_trace(
                    p,
                    x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
                    y = plot_data[as.character(plot_data$AUDIT) == i, "Value"] / 1000000,
                    type = "scatter",
                    mode = "lines",
                    colors = "set1",
                    name = i
                  )
                }
              }
              
              p %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value(Million)",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
              
            } else {
              p <- plot_ly(hoverinfo = "x+y+name")
              for (i in as.character(unique(plot_data$AUDIT))) {
                if (grepl("B.I$", i)) {
                  p <- add_trace(
                    p,
                    x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
                    y = plot_data[as.character(plot_data$AUDIT) == i, "Value"],
                    type = "scatter",
                    mode = "lines",
                    color = list("blue"),
                    colors = "Set1",
                    name = i
                  )
                } else {
                  p <- add_trace(
                    p,
                    x = plot_data[as.character(plot_data$AUDIT) == i, "Date"],
                    y = plot_data[as.character(plot_data$AUDIT) == i, "Value"],
                    type = "scatter",
                    mode = "lines",
                    colors = "Set1",
                    name = i
                  )
                }
              }
              
              p %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
            }
          }
        }
      })
      
    })
    output$chart <- renderPlotly(product_plot())
    
  })
  
  
  
  output$downloadPlot_p <- downloadHandler(
    filename = function() {
      paste("Product_Visulization_", input$period, "_", input$window, "year", '.png', sep = '')
    },
    content = function(file) {
      export(product_plot(), file = file)
    }
  )
  
  
  corporation_plot <- NULL
  observeEvent(input$goButton_c, {
    corporation_plot <<- reactive({
      input$goButton_c
      isolate({
        if (is.null(ot()))
          return(NULL)
        ot <- ot()$result3_c
        ot <- filter(
          ot,
          (CORPORATE.DESC != "Overall") |
            (CORPORATE.DESC == "Overall" &
               COMPS.DESC == "Overall")
        )
        
        if ("ALL" %in% input$province) {
          summary <-
            summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())), ]
        } else {
          summary <-
            summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)), ]
        }
        top_brand <- graph1m(
          summary,
          # cate=input$category,
          # subcate=input$subcategory,
          value = "RENMINBI",
          period = input$period,
          kpi = "abs",
          window = 1,
          level = "corporation",
          top = as.integer(input$top2)
        ) %>%
          select(AUDIT.DESC, PRODUCT.DESC, CORPORATE.DESC)
        
        ot <- ot %>% inner_join(top_brand)
        
        ot$AUDIT.DESC <- gsub(" .*$", "", ot$AUDIT.DESC)
        
        
        ot$PRODUCT.DESC <- as.character(ot$PRODUCT.DESC)
        ot$CORPORATE.DESC <- as.character(ot$CORPORATE.DESC)
        
        ot$PRODUCT.DESC[which(ot$CORPORATE.DESC == "Overall")] <-
          ot$AUDIT.DESC[which(ot$CORPORATE.DESC == "Overall")]
        KK <- which(ot$PRODUCT.DESC == "Overall")
        
        ot$PRODUCT.DESC[KK] <- ot$CORPORATE.DESC[KK]
        ot$CORPORATE.DESC[KK] <- "Company"
        
        names(ot)[names(ot) == 'AUDIT.DESC'] <- 'AUDIT'
        names(ot)[names(ot) == 'CORPORATE.DESC'] <- 'CORPORATE'
        names(ot)[names(ot) == 'PRODUCT.DESC'] <- 'PRODUCT'
        
        ot$PRODUCT <- as.factor(ot$PRODUCT)
        ot$CORPORATE <- as.factor(ot$CORPORATE)
        ot$AUDIT <- as.factor(ot$AUDIT)
        
        ot <- as.data.frame(ot)
        otnum <- which(grepl("Index", names(ot)))
        #number here need to be updated for new update
        # ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)] <-
        #   format(ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)],
        #          big.mark = ",", scientific = FALSE)
        
        ot[, (otnum + 1):length(ot)] <-
          format(
            ot[, (otnum + 1):length(ot)],
            nsmall = 2,
            big.mark = ",",
            scientific = FALSE
          )
        
        ot$Measure[which(ot$Measure == "RENMINBI")] <- "RMB"
        
        if (input$sub_index_c == "ms") {
          tmp_sub_index <- "MS%"
        } else if (input$sub_index_c == "gr") {
          tmp_sub_index <- "GR%"
        } else if (input$sub_index_c == "mc") {
          tmp_sub_index <- "MS+/- %"
        } else {
          tmp_sub_index <- input$sub_index_c
        }
        
        if (as.integer(input$top) > 0) {
          plot_data <- ot %>%
            filter(
              Measure == input$sub_measure_c,
              Index == toupper(tmp_sub_index),
              AUDIT %in% gsub(" .*$", "", input$sub_region_c),
              # as.character(CORPORATE) %in% c("Company", "Overall"),
              # (as.character(PRODUCT) == as.character(AUDIT) |
              as.character(PRODUCT) %in% input$sub_top_c
              # )
            ) %>%
            # toplist_c()[1:(1 + as.integer(input$sub_top_c))])) %>%
            gather(
              Date,
              Value,-AUDIT,-MARKET.DESC,-MANUF.TYPE.DESC,
              -TC.I.SHORT.DESC,-TC.I.DESC,-TC.II.SHORT.DESC,
              -TC.II.DESC,-TC.III.SHORT.DESC,-TC.II.DESC,
              -TC.III.SHORT.DESC,-TC.III.DESC,
              -CORPORATE,-CORP.CN,-PRD.CODE,-PRODUCT,
              -PRD.CN,-COMPS.ABBR,-COMPS.DESC,-COMPS.CN,
              -Measure,-Index
            ) %>%
            mutate(
              Date = as.Date(ymd(paste(
                Date, "01", "."
              ))),
              Value = as.numeric(gsub(",", "", Value)),
              Date1 = substr(as.character(Date), 1, 7)
            )
        } else {
          plot_data <- ot %>%
            filter(
              Measure == input$sub_measure_c,
              Index == toupper(tmp_sub_index),
              # AUDIT %in% gsub(" .*$", "", input$sub_region_c),
              # as.character(CORPORATE) %in% c("Company", "Overall"),
              # (
              as.character(PRODUCT) == as.character(AUDIT)
              # |
              # as.character(PRODUCT) %in% input$sub_top_c
              # )
            ) %>%
            # toplist_c()[1:(1 + as.integer(input$sub_top_c))])) %>%
            gather(
              Date,
              Value,-AUDIT,-MARKET.DESC,-MANUF.TYPE.DESC,
              -TC.I.SHORT.DESC,-TC.I.DESC,-TC.II.SHORT.DESC,
              -TC.II.DESC,-TC.III.SHORT.DESC,-TC.II.DESC,
              -TC.III.SHORT.DESC,-TC.III.DESC,
              -CORPORATE,-CORP.CN,-PRD.CODE,-PRODUCT,
              -PRD.CN,-COMPS.ABBR,-COMPS.DESC,-COMPS.CN,
              -Measure,-Index
            ) %>%
            mutate(
              Date = as.Date(ymd(paste(
                Date, "01", "."
              ))),
              Value = as.numeric(gsub(",", "", Value)),
              Date1 = substr(as.character(Date), 1, 7)
            )
        }
        
        tickval <- unlist(distinct(plot_data, Date1))
        
        if ((as.integer(input$top) > 0 && input$bp_p_c) ||
            as.numeric(input$top) == 0) {
          if (input$label_c) {
            if (input$sub_index_c %in% c("abs")) {
              plot_ly(
                plot_data,
                x = ~ Date,
                y = ~ Value / 1000000,
                color = ~ PRODUCT,
                colors = "Set1",
                hoverinfo = "x+y+name",
                mode = "text",
                text = ~ round(Value / 1000000, 1)
              ) %>%
                add_lines(name = ~ PRODUCT) %>%
                add_text(
                  textfont = list(size = 10),
                  textposition = "top",
                  hoverinfo = "none",
                  showlegend = FALSE
                ) %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value(Million)",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
              
            } else {
              plot_ly(
                plot_data,
                x = ~ Date,
                y = ~ Value,
                color = ~ PRODUCT,
                colors = "Set1",
                hoverinfo = "x+y+name",
                mode = "text",
                text = ~ round(Value, 1)
              ) %>%
                add_lines(name = ~ PRODUCT) %>%
                add_text(
                  textfont = list(size = 10),
                  textposition = "top",
                  hoverinfo = "none",
                  showlegend = FALSE
                ) %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
            }
            
          } else {
            if (input$sub_index_c %in% c("abs")) {
              plot_ly(
                plot_data,
                x = ~ Date,
                y = ~ Value / 1000000,
                color = ~ PRODUCT,
                colors = "Set1",
                hoverinfo = "x+y+name",
                mode = "text",
                text = ~ round(Value / 1000000, 1)
              ) %>%
                add_lines(name = ~ PRODUCT) %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value(Million)",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
              
            } else {
              plot_ly(
                plot_data,
                x = ~ Date,
                y = ~ Value,
                color = ~ PRODUCT,
                colors = "Set1",
                hoverinfo = "x+y+name",
                mode = "text",
                text = ~ round(Value, 1)
              ) %>%
                add_lines(name = ~ PRODUCT) %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
              
            }
          }
        } else {
          if (input$label_c) {
            if (input$sub_index_c %in% c("abs")) {
              plot_ly(
                plot_data,
                x = ~ Date,
                y = ~ Value / 1000000,
                color = ~ AUDIT,
                colors = "Set1",
                hoverinfo = "x+y+name",
                mode = "text",
                text = ~ round(Value / 1000000, 1)
              ) %>%
                add_lines(name = ~ AUDIT) %>%
                add_text(
                  textfont = list(size = 10),
                  textposition = "top",
                  hoverinfo = "none",
                  showlegend = FALSE
                ) %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value(Million)",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
              
            } else {
              plot_ly(
                plot_data,
                x = ~ Date,
                y = ~ Value,
                color = ~ AUDIT,
                colors = "Set1",
                hoverinfo = "x+y+name",
                mode = "text",
                text = ~ round(Value, 1)
              ) %>%
                add_lines(name = ~ AUDIT) %>%
                add_text(
                  textfont = list(size = 10),
                  textposition = "top",
                  hoverinfo = "none",
                  showlegend = FALSE
                ) %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
            }
          } else {
            if (input$sub_index_c %in% c("abs")) {
              plot_ly(
                plot_data,
                x = ~ Date,
                y = ~ Value / 1000000,
                color = ~ AUDIT,
                colors = "Set1",
                hoverinfo = "x+y+name",
                mode = "text",
                text = ~ round(Value / 1000000, 1)
              ) %>%
                add_lines(name = ~ AUDIT) %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value(Million)",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
            } else {
              plot_ly(
                plot_data,
                x = ~ Date,
                y = ~ Value,
                color = ~ AUDIT,
                colors = "Set1",
                hoverinfo = "x+y+name",
                mode = "text",
                text = ~ round(Value, 1)
              ) %>%
                add_lines(name = ~ AUDIT) %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
            }
          }
        }
      })
    })
    output$chart_c <- renderPlotly(corporation_plot())
  })
  
  output$downloadPlot_c <- downloadHandler(
    filename = function() {
      paste("Corporation_Visulization_", input$period, "_", input$window, "year", '.png', sep = '')
    },
    content = function(file) {
      export(corporation_plot(), file = file)
    }
  )
  molecule_plot <- NULL
  observeEvent(input$goButton_m, {
    molecule_plot <<- reactive({
      input$goButton_m
      isolate({
        if (is.null(ot()))
          return(NULL)
        ot <- ot()$result3_m
        ot <- filter(
          ot,
          (COMPS.DESC != "Overall") |
            (CORPORATE.DESC == "Overall" &
               COMPS.DESC == "Overall")
        )
        
        if ("ALL" %in% input$province) {
          summary <-
            summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())), ]
        } else {
          summary <-
            summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)), ]
        }
        top_brand <- graph1m(
          summary,
          # cate=input$category,
          # subcate=input$subcategory,
          value = "RENMINBI",
          period = input$period,
          kpi = "abs",
          window = 1,
          level = "molecule",
          top = as.integer(input$top3)
        ) %>%
          select(AUDIT.DESC, PRODUCT.DESC, COMPS.DESC)
        
        ot$AUDIT.DESC <- gsub(" .*$", "", ot$AUDIT.DESC)
        
        ot$PRODUCT.DESC <- as.character(ot$PRODUCT.DESC)
        ot$COMPS.DESC <- as.character(ot$COMPS.DESC)
        
        ot$PRODUCT.DESC[which(ot$COMPS.DESC == "Overall")] <-
          ot$AUDIT.DESC[which(ot$COMPS.DESC == "Overall")]
        KK <- which(ot$PRODUCT.DESC == "Overall")
        
        ot$PRODUCT.DESC[KK] <- ot$COMPS.DESC[KK]
        ot$COMPS.DESC[KK] <- "Molecule"
        
        names(ot)[names(ot) == 'AUDIT.DESC'] <- 'AUDIT'
        names(ot)[names(ot) == 'CORPORATE.DESC'] <- 'CORPORATE'
        names(ot)[names(ot) == 'PRODUCT.DESC'] <- 'PRODUCT'
        
        ot$PRODUCT <- as.factor(ot$PRODUCT)
        ot$CORPORATE <- as.factor(ot$CORPORATE)
        ot$AUDIT <- as.factor(ot$AUDIT)
        
        ot <- as.data.frame(ot)
        otnum <- which(grepl("Index", names(ot)))
        #number here need to be updated for new update
        # ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)] <-
        #   format(ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)], big.mark = ",", scientific = FALSE)
        ot[, (otnum + 1):length(ot)] <-
          format(ot[, (otnum + 1):length(ot)], nsamll = 2, big.mark = ",", scientific = FALSE)

        ot$Measure[which(ot$Measure == "RENMINBI")] <- "RMB"
        
        if (input$sub_index_m == "ms") {
          tmp_sub_index <- "MS%"
        } else if (input$sub_index_m == "gr") {
          tmp_sub_index <- "GR%"
        } else if (input$sub_index_m == "mc") {
          tmp_sub_index <- "MS+/- %"
        } else {
          tmp_sub_index <- input$sub_index_m
        }
        
        if (as.integer(input$top) > 0) {
          plot_data <- ot %>%
            filter(
              Measure == input$sub_measure_m,
              Index == toupper(tmp_sub_index),
              AUDIT %in% gsub(" .*$", "", input$sub_region_m),
              # COMPS.DESC %in% c("Molecule", "Overall"),
              # (as.character(PRODUCT) == as.character(AUDIT) |
              as.character(PRODUCT) %in% input$sub_top_m
              # )
            ) %>%
            # toplist_m()[1:(1 + as.integer(input$sub_top_m))])) %>% #
            gather(
              Date,
              Value,-AUDIT,-MARKET.DESC,-MANUF.TYPE.DESC,
              -TC.I.SHORT.DESC,-TC.I.DESC,-TC.II.SHORT.DESC,
              -TC.II.DESC,-TC.III.SHORT.DESC,-TC.II.DESC,
              -TC.III.SHORT.DESC,-TC.III.DESC,
              -CORPORATE,-CORP.CN,-PRD.CODE,-PRODUCT,
              -PRD.CN,-COMPS.ABBR,-COMPS.DESC,-COMPS.CN,
              -Measure,-Index
            ) %>%
            mutate(
              Date = as.Date(ymd(paste(
                Date, "01", "."
              ))),
              Value = as.numeric(gsub(",", "", Value)),
              Date1 = substr(as.character(Date), 1, 7)
            )
        } else {
          plot_data <- ot %>%
            filter(
              Measure == input$sub_measure_m,
              Index == toupper(tmp_sub_index),
              # AUDIT %in% gsub(" .*$", "", input$sub_region_m),
              # COMPS.DESC %in% c("Molecule", "Overall"),
              # (
              as.character(PRODUCT) == as.character(AUDIT)
              # |
              # as.character(PRODUCT) %in% input$sub_top_m
              # )
            ) %>%
            # toplist_m()[1:(1 + as.integer(input$sub_top_m))])) %>% #
            gather(
              Date,
              Value,-AUDIT,-MARKET.DESC,-MANUF.TYPE.DESC,
              -TC.I.SHORT.DESC,-TC.I.DESC,-TC.II.SHORT.DESC,
              -TC.II.DESC,-TC.III.SHORT.DESC,-TC.II.DESC,
              -TC.III.SHORT.DESC,-TC.III.DESC,
              -CORPORATE,-CORP.CN,-PRD.CODE,-PRODUCT,
              -PRD.CN,-COMPS.ABBR,-COMPS.DESC,-COMPS.CN,
              -Measure,-Index
            ) %>%
            mutate(
              Date = as.Date(ymd(paste(
                Date, "01", "."
              ))),
              Value = as.numeric(gsub(",", "", Value)),
              Date1 = substr(as.character(Date), 1, 7)
            )
        }
        tickval <- unlist(distinct(plot_data, Date1))
        
        
        # DT::datatable(ot)
        
        
        
        if ((as.integer(input$top) > 0 && input$bp_p_m) ||
            as.numeric(input$top) == 0) {
          if (input$label_m) {
            if (input$sub_index_m %in% c("abs")) {
              plot_ly(
                plot_data,
                x = ~ Date,
                y = ~ Value / 1000000,
                color = ~ PRODUCT,
                colors = "Set1",
                hoverinfo = "x+y+name",
                mode = "text",
                text = ~ round(Value / 1000000, 1)
              ) %>%
                add_lines(name = ~ PRODUCT) %>%
                add_text(
                  textfont = list(size = 10),
                  textposition = "top",
                  hoverinfo = "none",
                  showlegend = FALSE
                ) %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value(Million)",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
              
            } else {
              plot_ly(
                plot_data,
                x = ~ Date,
                y = ~ Value,
                color = ~ PRODUCT,
                colors = "Set1",
                hoverinfo = "x+y+name",
                mode = "text",
                text = ~ round(Value, 1)
              ) %>%
                add_lines(name = ~ PRODUCT) %>%
                add_text(
                  textfont = list(size = 10),
                  textposition = "top",
                  hoverinfo = "none",
                  showlegend = FALSE
                ) %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
            }
            
          } else {
            if (input$sub_index_m %in% c("abs")) {
              plot_ly(
                plot_data,
                x = ~ Date,
                y = ~ Value / 1000000,
                color = ~ PRODUCT,
                colors = "Set1",
                hoverinfo = "x+y+name",
                mode = "text",
                text = ~ round(Value / 1000000, 1)
              ) %>%
                add_lines(name = ~ PRODUCT) %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value(Million)",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
              
            } else {
              plot_ly(
                plot_data,
                x = ~ Date,
                y = ~ Value,
                color = ~ PRODUCT,
                colors = "Set1",
                hoverinfo = "x+y+name",
                mode = "text",
                text = ~ round(Value, 1)
              ) %>%
                add_lines(name = ~ PRODUCT) %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
              
            }
          }
        } else {
          if (input$label_m) {
            if (input$sub_index_m %in% c("abs")) {
              plot_ly(
                plot_data,
                x = ~ Date,
                y = ~ Value / 1000000,
                color = ~ AUDIT,
                colors = "Set1",
                hoverinfo = "x+y+name",
                mode = "text",
                text = ~ round(Value / 1000000, 1)
              ) %>%
                add_lines(name = ~ AUDIT) %>%
                add_text(
                  textfont = list(size = 10),
                  textposition = "top",
                  hoverinfo = "none",
                  showlegend = FALSE
                ) %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value(Million)",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
              
            } else {
              plot_ly(
                plot_data,
                x = ~ Date,
                y = ~ Value,
                color = ~ AUDIT,
                colors = "Set1",
                hoverinfo = "x+y+name",
                mode = "text",
                text = ~ round(Value, 1)
              ) %>%
                add_lines(name = ~ AUDIT) %>%
                add_text(
                  textfont = list(size = 10),
                  textposition = "top",
                  hoverinfo = "none",
                  showlegend = FALSE
                ) %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
            }
            
          } else {
            if (input$sub_index_m %in% c("abs")) {
              plot_ly(
                plot_data,
                x = ~ Date,
                y = ~ Value / 1000000,
                color = ~ AUDIT,
                colors = "Set1",
                hoverinfo = "x+y+name",
                mode = "text",
                text = ~ round(Value / 1000000, 1)
              ) %>%
                add_lines(name = ~ AUDIT) %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value(Million)",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
              
            } else {
              plot_ly(
                plot_data,
                x = ~ Date,
                y = ~ Value,
                color = ~ AUDIT,
                colors = "Set1",
                hoverinfo = "x+y+name",
                mode = "text",
                text = ~ round(Value, 1)
              ) %>%
                add_lines(name = ~ AUDIT) %>%
                config(
                  displaylogo = FALSE,
                  collaborate = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'zoom2d',
                    'pan2d',
                    'select2d',
                    'lasso2d',
                    'toggleSpikelines'
                  )
                ) %>%
                layout(
                  xaxis = list(
                    zeroline = FALSE,
                    title = "",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks",
                    tickvals = tickval,
                    tickformat = "%y%m",
                    type = "date"
                  ),
                  yaxis = list(
                    zeroline = FALSE,
                    title = "Value",
                    showline = TRUE,
                    showgrid = FALSE,
                    mirror = "ticks"
                  )
                )
              
            }
          }
        }
        
        
      })
    })
    output$chart_m <- renderPlotly(molecule_plot())
    output$downloadPlot_m <- downloadHandler(
      filename = function() {
        paste("Molecule_Visulization_", input$period, "_", input$window, "year", '.png', sep = '')
      },
      content = function(file) {
        export(molecule_plot(), file = file)
      }
    )
  })
  
  ############################# download plot data #############################
  
  plot_data_p <- reactive({
    if (is.null(ot()))
      return(NULL)
    
    ot <- ot()$result3
    ot <-
      filter(
        ot,
        (CORPORATE.DESC != "Overall" & COMPS.DESC != "Overall") |
          (CORPORATE.DESC == "Overall" &
             COMPS.DESC == "Overall")
      )
    ot$AUDIT.DESC <- gsub(" .*$", "", ot$AUDIT.DESC)
    
    ot$PRODUCT.DESC <- as.character(ot$PRODUCT.DESC)
    ot$CORPORATE.DESC <- as.character(ot$CORPORATE.DESC)
    
    ot$PRODUCT.DESC[which(ot$CORPORATE.DESC == "Overall")] <-
      ot$AUDIT.DESC[which(ot$CORPORATE.DESC == "Overall")]
    KK <- which(ot$PRODUCT.DESC == "Overall")
    
    ot$PRODUCT.DESC[KK] <- ot$CORPORATE.DESC[KK]
    ot$CORPORATE.DESC[KK] <- "Company"
    
    names(ot)[names(ot) == 'AUDIT.DESC'] <- 'AUDIT'
    names(ot)[names(ot) == 'CORPORATE.DESC'] <- 'CORPORATE'
    names(ot)[names(ot) == 'PRODUCT.DESC'] <- 'PRODUCT'
    
    ot$PRODUCT <- as.factor(ot$PRODUCT)
    ot$CORPORATE <- as.factor(ot$CORPORATE)
    ot$AUDIT <- as.factor(ot$AUDIT)
    
    ot <- as.data.frame(ot)
    otnum <- which(grepl("Index", names(ot)))
    #number here need to be updated for new update
    ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)] <-
      format(ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)],
             big.mark = ",", scientific = FALSE)
    ot$Measure[which(ot$Measure == "RENMINBI")] <- "RMB"
    
    if (input$sub_index == "ms") {
      tmp_sub_index <- "MS%"
    } else if (input$sub_index == "gr") {
      tmp_sub_index <- "GR%"
    } else if (input$sub_index == "mc") {
      tmp_sub_index <- "MS+/- %"
    } else {
      tmp_sub_index <- input$sub_index
    }
    
    if (as.integer(input$top) > 0) {
      plot_data <- ot %>%
        filter(
          Measure == input$sub_measure,
          Index == toupper(tmp_sub_index),
          AUDIT == gsub(" .*$", "", input$sub_region),
          # COMPS.DESC %in% c("Molecule", "Overall"),
          (
            as.character(PRODUCT) == as.character(AUDIT) |
              as.character(PRODUCT) %in% input$sub_top
          )
        ) %>%
        # toplist()[1:(1 + as.integer(input$sub_top))])) %>%
        gather(
          Date,
          Value,-AUDIT,-MARKET.DESC,-MANUF.TYPE.DESC,
          -TC.I.SHORT.DESC,-TC.I.DESC,-TC.II.SHORT.DESC,
          -TC.II.DESC,-TC.III.SHORT.DESC,-TC.II.DESC,
          -TC.III.SHORT.DESC,-TC.III.DESC,
          -CORPORATE,-CORP.CN,-PRD.CODE,-PRODUCT,
          -PRD.CN,-COMPS.ABBR,-COMPS.DESC,-COMPS.CN,
          -Measure,-Index
        ) %>%
        mutate(Date = as.Date(ymd(paste(
          Date, "01", "."
        ))),
        Value = as.numeric(gsub(",", "", Value))) %>%
        spread(Date, Value)
    } else {
      plot_data <- ot %>%
        filter(
          Measure == input$sub_measure,
          Index == toupper(tmp_sub_index),
          # AUDIT == gsub(" .*$", "", input$sub_region),
          # COMPS.DESC %in% c("Molecule", "Overall"),
          (
            as.character(PRODUCT) == as.character(AUDIT) |
              as.character(PRODUCT) %in% input$sub_top
          )
        ) %>%
        # toplist()[1:(1 + as.integer(input$sub_top))])) %>%
        gather(
          Date,
          Value,-AUDIT,-MARKET.DESC,-MANUF.TYPE.DESC,
          -TC.I.SHORT.DESC,-TC.I.DESC,-TC.II.SHORT.DESC,
          -TC.II.DESC,-TC.III.SHORT.DESC,-TC.II.DESC,
          -TC.III.SHORT.DESC,-TC.III.DESC,
          -CORPORATE,-CORP.CN,-PRD.CODE,-PRODUCT,
          -PRD.CN,-COMPS.ABBR,-COMPS.DESC,-COMPS.CN,
          -Measure,-Index
        ) %>%
        mutate(Date = as.Date(ymd(paste(
          Date, "01", "."
        ))),
        Value = as.numeric(gsub(",", "", Value))) %>%
        spread(Date, Value)
    }
    plot_data
  })
  
  plot_data_c <- reactive({
    if (is.null(ot()))
      return(NULL)
    
    ot <- ot()$result3_c
    ot <- filter(
      ot,
      (CORPORATE.DESC != "Overall") |
        (CORPORATE.DESC == "Overall" &
           COMPS.DESC == "Overall")
    )
    
    if ("ALL" %in% input$province) {
      summary <- summary()[which(summary()$AUDIT.DESC %in%
                                   c("China", input$region,  province())), ]
    } else {
      summary <- summary()[which(summary()$AUDIT.DESC %in%
                                   c("China", input$region, input$province)), ]
    }
    
    
    top_brand <- graph1m(
      summary,
      # cate = input$category,
      # subcate = input$subcategory,
      value = "RENMINBI",
      period = input$period,
      kpi = "abs",
      window = 1,
      level = "corporation",
      top = as.integer(input$top2)
    ) %>%
      select(AUDIT.DESC, PRODUCT.DESC, CORPORATE.DESC)
    
    ot <- ot %>% inner_join(top_brand)
    
    ot$AUDIT.DESC <- gsub(" .*$", "", ot$AUDIT.DESC)
    
    
    ot$PRODUCT.DESC <- as.character(ot$PRODUCT.DESC)
    ot$CORPORATE.DESC <- as.character(ot$CORPORATE.DESC)
    
    ot$PRODUCT.DESC[which(ot$CORPORATE.DESC == "Overall")] <-
      ot$AUDIT.DESC[which(ot$CORPORATE.DESC == "Overall")]
    KK <- which(ot$PRODUCT.DESC == "Overall")
    
    ot$PRODUCT.DESC[KK] <- ot$CORPORATE.DESC[KK]
    ot$CORPORATE.DESC[KK] <- "Company"
    
    names(ot)[names(ot) == 'AUDIT.DESC'] <- 'AUDIT'
    names(ot)[names(ot) == 'CORPORATE.DESC'] <- 'CORPORATE'
    names(ot)[names(ot) == 'PRODUCT.DESC'] <- 'PRODUCT'
    
    ot$PRODUCT <- as.factor(ot$PRODUCT)
    ot$CORPORATE <- as.factor(ot$CORPORATE)
    ot$AUDIT <- as.factor(ot$AUDIT)
    
    ot <- as.data.frame(ot)
    otnum <- which(grepl("Index", names(ot)))
    #number here need to be updated for new update
    ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)] <-
      format(ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)],
             big.mark = ",", scientific = FALSE)
    ot$Measure[which(ot$Measure == "RENMINBI")] <- "RMB"
    
    if (input$sub_index_c == "ms") {
      tmp_sub_index <- "MS%"
    } else if (input$sub_index_c == "gr") {
      tmp_sub_index <- "GR%"
    } else if (input$sub_index_c == "mc") {
      tmp_sub_index <- "MS+/- %"
    } else {
      tmp_sub_index <- input$sub_index_c
    }
    
    if (as.integer(input$top) > 0) {
      plot_data <- ot %>%
        filter(
          Measure == input$sub_measure_c,
          Index == toupper(tmp_sub_index),
          AUDIT == gsub(" .*$", "", input$sub_region_c),
          as.character(CORPORATE) %in% c("Company", "Overall"),
          (
            as.character(PRODUCT) == as.character(AUDIT) |
              as.character(PRODUCT) %in% input$sub_top_c
          )
        ) %>%
        # toplist_c()[1:(1 + as.integer(input$sub_top_c))])) %>%
        gather(
          Date,
          Value,-AUDIT,-MARKET.DESC,-MANUF.TYPE.DESC,
          -TC.I.SHORT.DESC,-TC.I.DESC,-TC.II.SHORT.DESC,
          -TC.II.DESC,-TC.III.SHORT.DESC,-TC.II.DESC,
          -TC.III.SHORT.DESC,-TC.III.DESC,
          -CORPORATE,-CORP.CN,-PRD.CODE,-PRODUCT,
          -PRD.CN,-COMPS.ABBR,-COMPS.DESC,-COMPS.CN,
          -Measure,-Index
        ) %>%
        mutate(Date = as.Date(ymd(paste(
          Date, "01", "."
        ))),
        Value = as.numeric(gsub(",", "", Value))) %>%
        spread(Date, Value)
    } else {
      plot_data <- ot %>%
        filter(
          Measure == input$sub_measure_c,
          Index == toupper(tmp_sub_index),
          # AUDIT == gsub(" .*$", "", input$sub_region_c),
          as.character(CORPORATE) %in% c("Company", "Overall"),
          (
            as.character(PRODUCT) == as.character(AUDIT) |
              as.character(PRODUCT) %in% input$sub_top_c
          )
        ) %>%
        # toplist_c()[1:(1 + as.integer(input$sub_top_c))])) %>%
        gather(
          Date,
          Value,-AUDIT,-MARKET.DESC,-MANUF.TYPE.DESC,
          -TC.I.SHORT.DESC,-TC.I.DESC,-TC.II.SHORT.DESC,
          -TC.II.DESC,-TC.III.SHORT.DESC,-TC.II.DESC,
          -TC.III.SHORT.DESC,-TC.III.DESC,
          -CORPORATE,-CORP.CN,-PRD.CODE,-PRODUCT,
          -PRD.CN,-COMPS.ABBR,-COMPS.DESC,-COMPS.CN,
          -Measure,-Index
        ) %>%
        mutate(Date = as.Date(ymd(paste(
          Date, "01", "."
        ))),
        Value = as.numeric(gsub(",", "", Value))) %>%
        spread(Date, Value)
    }
    
    plot_data
  })
  
  plot_data_m <- reactive({
    if (is.null(ot()))
      return(NULL)
    
    ot <- ot()$result3_m
    ot <- filter(
      ot,
      (COMPS.DESC != "Overall") |
        (CORPORATE.DESC == "Overall" &
           COMPS.DESC == "Overall")
    )
    
    if ("ALL" %in% input$province) {
      summary <-
        summary()[which(summary()$AUDIT.DESC %in% c("China", input$region,  province())), ]
    } else {
      summary <-
        summary()[which(summary()$AUDIT.DESC %in% c("China", input$region, input$province)), ]
    }
    
    top_brand <- graph1m(
      summary,
      # cate = input$category,
      # subcate = input$subcategory,
      value = "RENMINBI",
      period = input$period,
      kpi = "abs",
      window = 1,
      level = "molecule",
      top = as.integer(input$top3)
    ) %>%
      select(AUDIT.DESC, PRODUCT.DESC, COMPS.DESC)
    
    ot$AUDIT.DESC <- gsub(" .*$", "", ot$AUDIT.DESC)
    
    ot$PRODUCT.DESC <- as.character(ot$PRODUCT.DESC)
    ot$COMPS.DESC <- as.character(ot$COMPS.DESC)
    
    ot$PRODUCT.DESC[which(ot$COMPS.DESC == "Overall")] <-
      ot$AUDIT.DESC[which(ot$COMPS.DESC == "Overall")]
    KK <- which(ot$PRODUCT.DESC == "Overall")
    
    ot$PRODUCT.DESC[KK] <- ot$COMPS.DESC[KK]
    ot$COMPS.DESC[KK] <- "Molecule"
    
    names(ot)[names(ot) == 'AUDIT.DESC'] <- 'AUDIT'
    names(ot)[names(ot) == 'CORPORATE.DESC'] <- 'CORPORATE'
    names(ot)[names(ot) == 'PRODUCT.DESC'] <- 'PRODUCT'
    
    ot$PRODUCT <- as.factor(ot$PRODUCT)
    ot$CORPORATE <- as.factor(ot$CORPORATE)
    ot$AUDIT <- as.factor(ot$AUDIT)
    
    ot <- as.data.frame(ot)
    otnum <- which(grepl("Index", names(ot)))
    #number here need to be updated for new update
    # ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)] <-
    #   format(ot[which(ot$Index == "ABS"), (otnum + 1):length(ot)], big.mark = ",", scientific = FALSE)
    
    ot[, (otnum + 1):length(ot)] <-
      format(ot[, (otnum + 1):length(ot)], nsmall = 2,  big.mark = ",", scientific = FALSE)
    
    
    ot$Measure[which(ot$Measure == "RENMINBI")] <- "RMB"
    
    if (input$sub_index_m == "ms") {
      tmp_sub_index <- "MS%"
    } else if (input$sub_index_m == "gr") {
      tmp_sub_index <- "GR%"
    } else if (input$sub_index_m == "mc") {
      tmp_sub_index <- "MS+/- %"
    } else {
      tmp_sub_index <- input$sub_index_m
    }
    
    if (as.integer(input$top) > 0) {
      plot_data <- ot %>%
        filter(
          Measure == input$sub_measure_m,
          Index == toupper(tmp_sub_index),
          AUDIT == gsub(" .*$", "", input$sub_region_m),
          COMPS.DESC %in% c("Molecule", "Overall"),
          (
            as.character(PRODUCT) == as.character(AUDIT) |
              as.character(PRODUCT) %in% input$sub_top_m
          )
        ) %>%
        # toplist_m()[1:(1 + as.integer(input$sub_top_m))])) %>% #
        gather(
          Date,
          Value,-AUDIT,-MARKET.DESC,-MANUF.TYPE.DESC,
          -TC.I.SHORT.DESC,-TC.I.DESC,-TC.II.SHORT.DESC,
          -TC.II.DESC,-TC.III.SHORT.DESC,-TC.II.DESC,
          -TC.III.SHORT.DESC,-TC.III.DESC,
          -CORPORATE,-CORP.CN,-PRD.CODE,-PRODUCT,
          -PRD.CN,-COMPS.ABBR,-COMPS.DESC,-COMPS.CN,
          -Measure,-Index
        ) %>%
        mutate(Date = as.Date(ymd(paste(
          Date, "01", "."
        ))),
        Value = as.numeric(gsub(",", "", Value))) %>%
        spread(Date, Value)
    } else {
      plot_data <- ot %>%
        filter(
          Measure == input$sub_measure_m,
          Index == toupper(tmp_sub_index),
          # AUDIT == gsub(" .*$", "", input$sub_region_m),
          COMPS.DESC %in% c("Molecule", "Overall"),
          (
            as.character(PRODUCT) == as.character(AUDIT) |
              as.character(PRODUCT) %in% input$sub_top_m
          )
        ) %>%
        # toplist_m()[1:(1 + as.integer(input$sub_top_m))])) %>% #
        gather(
          Date,
          Value,-AUDIT,-MARKET.DESC,-MANUF.TYPE.DESC,
          -TC.I.SHORT.DESC,-TC.I.DESC,-TC.II.SHORT.DESC,
          -TC.II.DESC,-TC.III.SHORT.DESC,-TC.II.DESC,
          -TC.III.SHORT.DESC,-TC.III.DESC,
          -CORPORATE,-CORP.CN,-PRD.CODE,-PRODUCT,
          -PRD.CN,-COMPS.ABBR,-COMPS.DESC,-COMPS.CN,
          -Measure,-Index
        ) %>%
        mutate(Date = as.Date(ymd(paste(
          Date, "01", "."
        ))),
        Value = as.numeric(gsub(",", "", Value))) %>%
        spread(Date, Value)
    }
    
    plot_data
    
  })
  
  output$downloadData_p <- downloadHandler(
    filename = function() {
      paste("plot_data_by_product_", input$period, "_", input$window, "year", '.xlsx', sep = '')
    },
    content = function(file) {
      write.xlsx(plot_data_p(), file, overwrite = TRUE)
    }
  )
  
  output$downloadData_c <- downloadHandler(
    filename = function() {
      paste("plot_data_by_corporation_", input$period, "_", input$window, "year", '.xlsx', sep = '')
    },
    content = function(file) {
      write.xlsx(plot_data_c(), file, overwrite = TRUE)
    }
  )
  
  output$downloadData_m <- downloadHandler(
    filename = function() {
      paste("plot_data_by_molecule_",  input$period, "_", input$window, "year", '.xlsx', sep = '')
    },
    content = function(file) {
      write.xlsx(plot_data_m(), file, overwrite = TRUE)
    }
  )
}
