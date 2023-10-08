library(shiny)
library(shinyTree)
library(dplyr)
library(purrr)

#lists of techniques for the selection boxes, shinyTree requires a named list where the names are the labels
#the values don't get used so numbers are placeholders here
thoracic_soft_tissue <- list("Thoracic Myofascial Release" = 1,
                             "Unilateral Prone Pressure" = 2,
                             "Unilateral Prone Pressure - Catwalk Variation" = 3,
                             "Bilateral Prone Pressure" = 4,
                             "Bilateral Thumb Pressure" = 5,
                             "Thoracic Lateral Recumbent" = 6)

thoracic_muscle_energy <-  list("T1-T4E or N" = 1,
                                "T1-T6F" = 2,
                                "T5-T12N" = 3,
                                "T5-T12E" = 4)

rib_muscle_energy <- list("Rib 1-2 Inhaled Post Isometric Relaxation" = 1,
                          "Rib 1-2 Inhaled Respiratory Assist" = 2,
                          "Rib 3-6 Inhaled" = 3,
                          "Rib 7-10 Inhaled" = 4,
                          "Rib 11-12 Inhaled" = 5,
                          "Rib 1-2 Exhaled" = 6,
                          "Rib 3-5 Exhaled" = 7,
                          "Rib 6-8 Exhaled" = 8,
                          "Rib 9-10 Exhaled" = 9,
                          "Rib 11-12 Exhaled" = 10)

#A version of sample() that doesn't misbehave when the input is a single integer
sample_fixed <- function(x) {
  if(length(x) == 1) {
    out <- x
  }
  else{
    out <- sample(x, 1)
  }
}
##Functions for making random diagnoses/techniques

#Thoracic soft tissue functions
generate_t_mfr <- function() {
  dir_indir <- sample(c("direct", "indirect"), 1)
  barrier_ease <- sample(c("barrier", "ease"), 1)
  v_dir <- sample(c("superior", "inferior"), 1)
  h_dir <- sample(c("left", "right"), 1)
  r_dir <- sample(c("clockwise", "counterclockwise"), 1)
  
  paste("Perform", dir_indir, "myofacial release on a patient whose", barrier_ease, "is", v_dir, "to the", h_dir, "and", r_dir)
}

#Thoracic muscle energy functions
generate_t_me_en_1_4 <- function() {
  type <- sample(c(1,2), 1)
  if(type == 1) {
    top <- sample(c(1,2,3), 1)
    bottom <- sample_fixed(seq(top + 1, 4, 1))
    rot <- sample(c("R", "L"), 1)
    sideb <- if(rot == "R") {
      "L"
    }
    else {
      "R"
    }
    out <- paste0("T", top, "-T", bottom, "N", "R", rot, "S", sideb) 
  }
  else {
    vert <- sample(seq_len(4), 1)
    side <- sample(c("R", "L"), 1)
    out <- paste0("T", vert, "E", "R", side, "S", side)
  }
  out
}

generate_t_me_f_1_6 <- function() {
  vert <- sample(seq_len(6), 1)
  side <- sample(c("R", "L"), 1)
  out <- paste0("T", vert, "F", "R", side, "S", side)
  out
}

generate_t_me_e_5_12 <- function() {
  vert <- sample(seq(5, 12, 1), 1)
  side <- sample(c("R", "L"), 1)
  out <- paste0("T", vert, "E", "R", side, "S", side)
} 

generate_t_me_n_5_12 <- function() {
  top <- sample(seq(5,11,1), 1)
  bottom <- sample_fixed(seq(top + 1, 12, 1))
  rot <- sample(c("R", "L"), 1)
  sideb <- if(rot == "R") {
    "L"
  }
  else {
    "R"
  }
  out <- paste0("T", top, "-T", bottom, "N", "R", rot, "S", sideb) 
}

#Rib muscle energy functions
generate_r_me_i_ra_1_2 <- function() {
  rib <- sample(c("1", "2", "1-2"), 1)
  side <- sample(c("Left", "Right"), 1)
  if(rib == "1-2") {
    out <- paste(side, "ribs", rib, "inhaled", "using respiratory assist")
  }
  else {
    out <- paste(side, "rib", rib, "inhaled", "using respiratory assist")
  }
}

generate_r_me_i_pir_1_2 <- function() {
  rib <- sample(c("1", "2", "1-2"), 1)
  side <- sample(c("Left", "Right"), 1)
  if(rib == "1-2") {
    out <- paste(side, "ribs", rib, "inhaled", "using post isometric relaxation")
  }
  else {
    out <- paste(side, "rib", rib, "inhaled", "using post isometric relaxation")
  }
}
generate_r_me_i_3_6 <- function() {
  side <- sample(c("Left", "Right"), 1)
  top <- sample(seq_len(6), 1)
  bottom <- sample_fixed(seq(top, 6, 1))
  if(top == bottom) {
    out <- paste(side, "rib", top, "inhaled")
  }
  else {
    out <- paste0(side, " ", "ribs ", top, "-", bottom,  " inhaled")
  }
  out
}

generate_r_me_i_7_10 <- function() {
  side <- sample(c("Left", "Right"), 1)
  top <- sample(seq_len(10), 1)
  if(top >= 7) {
    bottom <- sample_fixed(seq(top, 10, 1))
  }
  else {
    bottom <- sample(seq(7, 10, 1), 1)
  }
  if(top == bottom) {
    out <- paste(side, "rib", top, "inhaled")
  }
  else {
    out <- paste0(side, " ", "ribs ", top, "-", bottom,  " inhaled")
  }
  out
}

generate_r_me_i_11_12 <- function() {
  side <- sample(c("Left", "Right"), 1)
  top <- sample(seq_len(12), 1)
  if(top >= 11) {
    bottom <- sample_fixed(seq(top, 12, 1))
  }
  else {
    bottom <- sample(seq(11, 12, 1), 1)
  }
  if(top == bottom) {
    out <- paste(side, "rib", top, "inhaled")
  }
  else {
    out <- paste0(side, " ", "ribs ", top, "-", bottom,  " inhaled")
  }
  out
}

generate_r_me_e_1_2 <- function() {
  side <- sample(c("Left", "Right"), 1)
  top <- sample(c(1, 2), 1)
  bottom <- sample(seq(top, 12), 1)
  if(top == bottom) {
    out <- paste(side, "rib", top, "exhaled")
  }
  else {
    out <- paste0(side, " ", "ribs ", top, "-", bottom,  " exhaled")
  }
  out
}

generate_r_me_e_3_5 <- function() {
  side <- sample(c("Left", "Right"), 1)
  top <- sample(c(3, 4, 5), 1)
  bottom <- sample(seq(top, 12), 1)
  if(top == bottom) {
    out <- paste(side, "rib", top, "exhaled")
  }
  else {
    out <- paste0(side, " ", "ribs ", top, "-", bottom,  " exhaled")
  }
  out
}

generate_r_me_e_6_8 <- function() {
  side <- sample(c("Left", "Right"), 1)
  top <- sample(c(6, 7, 8), 1)
  bottom <- sample(seq(top, 12), 1)
  if(top == bottom) {
    out <- paste(side, "rib", top, "exhaled")
  }
  else {
    out <- paste0(side, " ", "ribs ", top, "-", bottom,  " exhaled")
  }
  out
}

generate_r_me_e_9_10 <- function() {
  side <- sample(c("Left", "Right"), 1)
  top <- sample(c(9,10), 1)
  bottom <- sample(seq(top, 12), 1)
  if(top == bottom) {
    out <- paste(side, "rib", top, "exhaled")
  }
  else {
    out <- paste0(side, " ", "ribs ", top, "-", bottom,  " exhaled")
  }
  out
}

generate_r_me_e_11_12 <- function() {
  rib <- sample(c("11", "12", "11-12"), 1)
  side <- sample(c("Left", "Right"), 1)
  if(rib == "11-12") {
    out <- paste(side, "ribs", rib, "exhaled")
  }
  else {
    out <- paste(side, "rib", rib, "exhaled")
  }
}

#Selection functions that give a technique from a vector of techniques in a single category selected from the tree
generate_t_soft_tissue <- function(techs) {
  tech <- sample(techs, 1)
  if(tech == "Thoracic Myofascial Release") {
    out <- generate_t_mfr()
  }
  else if(tech %in% c("Bilateral Prone Pressure", "Bilateral Thumb Pressure")) {
    out <- paste("Perform", tech)
  }
  else {
    side <- sample(c("left", "right"), 1)
    out <- paste("Perform", tech, "on the patient's", side, "side")
  }
  out
}

generate_t_me <- function(techs) {
  tech <- sample(techs, 1)
  diagnosis <- case_when(tech == "T1-T4E or N" ~ generate_t_me_en_1_4(),
                         tech == "T1-T6F" ~ generate_t_me_f_1_6(),
                         tech == "T5-T12N" ~ generate_t_me_n_5_12(),
                         tech == "T5-T12E" ~ generate_t_me_e_5_12())
  paste("Perform thoracic muscle energy on a patient with the following diagnosis:", diagnosis)
}

generate_r_me <- function(techs) {
  tech <- sample(techs, 1)
  diagnosis <- case_when(tech == "Rib 1-2 Inhaled Post Isometric Relaxation" ~ generate_r_me_i_pir_1_2(),
                         tech == "Rib 1-2 Inhaled Respiratory Assist" ~ generate_r_me_i_ra_1_2(),
                         tech == "Rib 3-6 Inhaled" ~ generate_r_me_i_3_6(),
                         tech == "Rib 7-10 Inhaled" ~ generate_r_me_i_7_10(),
                         tech == "Rib 11-12 Inhaled" ~ generate_r_me_i_11_12(),
                         tech == "Rib 1-2 Exhaled" ~ generate_r_me_e_1_2(),
                         tech == "Rib 3-5 Exhaled" ~ generate_r_me_e_3_5(),
                         tech == "Rib 6-8 Exhaled" ~ generate_r_me_e_6_8(),
                         tech == "Rib 9-10 Exhaled" ~ generate_r_me_e_9_10(),
                         tech == "Rib 11-12 Exhaled" ~ generate_r_me_e_11_12())
  paste("Perform rib muscle energy on a patient with the following diagnosis:", diagnosis)
}

generate_t_hvla <- function(techs) {
  tech <- sample(techs,1)
  diagnosis <- case_when(tech == "Supine HVLA Thoracic", generate_t_s_hvla)
}

#Function that gives a technique from a tree category and vector of selected techniques in that category
generate_technique <- function(category, techs) {
  case_when(category == "Thoracic Soft Tissue" ~ generate_t_soft_tissue(techs),
            category == "Thoracic Muscle Energy" ~ generate_t_me(techs),
            category == "Rib Muscle Energy" ~ generate_r_me(techs))
}

ui <- fluidPage(
  
  titlePanel("OMM Practical Practicer"),
  
  # Sidebar with checkboxes for techniques to use, a selector for number of techniques, and a button to randomize
  sidebarLayout(
    sidebarPanel(width = 5,
                 numericInput("n_techs", "Number of Techniques", 2, min = 1),
                 tags$h5("Select techniques from the list below"),
                 shinyTree("tree", TRUE),
                 HTML("<br>"),
                 actionButton("randomizer","New Techniques Please!"),
                 actionButton("browser", "browser")
    ),
    
    #Main panel with techniques to perform
    mainPanel(width = 7,
              uiOutput("diagnoses_text")
    )
  )
)


server <- function(input, output) {
  observeEvent(input$browser, {
    browser()
  })
  
  techs <- reactive({
    get_selected(input$tree, format = "names") |>
      discard(~is_empty(attr(.x, "ancestry")))
  })
  
  categories <- reactive({
    map(techs(), attr, "ancestry") |>
      unique() |>
      unlist()
  })
  
  diagnoses <- eventReactive(input$randomizer, {
    if(length(categories()) >= input$n_techs) {
      category_vec <- map(techs(), attr, "ancestry") |>
        unlist()
      tech_vec <- map(categories(), ~which(category_vec == .x)) |>
        map(~unlist(map(.x, ~pluck(techs(), .x)))) |>
        setNames(categories()) |>
        sample(input$n_techs)
      cats <- names(tech_vec)
      map2_chr(tech_vec, cats, ~generate_technique(.y, .x))
    }
    else {
      "You have chosen a number of techniques greater than the number of chosen technique categories. Please reduce the number of techniques or select more technique categories."
    }
  })
  
  output$diagnoses_text <- renderUI({
    HTML(pmap_chr(as.list(diagnoses()), paste, sep = "<br/><br/>"))
  })
  
  output$tree <- renderTree(list("Thoracic Soft Tissue" = thoracic_soft_tissue,
                                 "Thoracic Muscle Energy" = thoracic_muscle_energy,
                                 "Rib Muscle Energy" = rib_muscle_energy))
}
# Run the application 
shinyApp(ui = ui, server = server)
