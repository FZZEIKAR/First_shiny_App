#----------------------------------------------------------------import packages
library(shiny)
library(dplyr)
library(data.table)
library(plotly)
library(echarts4r)
library(reticulate)
library(sp)
library(raster)
library(tensorflow)
library(keras)


#-------------------------------------------------------------------------colors
colors_courses_categories=c("#F5FCC2","#E0ED87","#CDDE57","#B3C732","#94A813")
colors_User_desactivated= c("#99D8C9","#2CA25F")

#-----------------------------------------------------------------load our model
model<- load_model_tf("C:/Users/Fatima-Zahra/Documents/App1")

#-----------------------------------------------------------sourcing python code
source_python("prepare.py")


#-----------------------------------------------------------------Preparing data
BD=read.csv("user_course_activity_report.csv")
head(BD)
is.na(BD)
df = subset(BD, select = -c(ID.externe,Groupes.d.utilisateurs) )
colnames(df)
is.na(df)
df[is.na(df)]<-0
df=data.table(df)
setnames(df, "CatÃ.gorie.de.cours", "Catégorie.de.cours")
setnames(df, "Titre.du.cours", "Titre.du.cours")
setnames(df, "X..marquÃ..comme.terminÃ.", "Pourcentage.marqué.comme.terminé")
setnames(df,"Minutes.de.vidÃ.o.consommÃ.es","Minutes.consommées")

setnames(df,"L.utilisateur.est.dÃ.sactivÃ.","Utilisateur.désactivé")
setnames(df,"AssignÃ.","Assigné")
#-----------------------------------------------------------------------------UI
ui <- navbarPage(strong("Bienvenu sur votre application"),
  theme = bslib::bs_theme(bootswatch = "darkly"),
    tabPanel("Description d'une base de données", 
      sidebarLayout(
        sidebarPanel(
          selectInput("Select_user",
                  "Sélectionner un utilisateur:",
                  choices = c(unique(df$Nom.de.l.utilisateur)),
                  multiple = FALSE),
          titlePanel(h5(strong("L'utilisateur sélectionné est désactivé ?"),
                        style='color: black;background-color: #A7C7E7; text-align: center;')),
          textOutput("User_desactivated")
        ),
        
        mainPanel(
          verticalLayout(
          h5(strong("Le panier de formations :"),
             style='color: black;background-color: #A7C7E7; text-align: center;'),
          br(),
          br(),
          tableOutput("User_courses_table"),
          downloadButton("download_user_courses","Télécharger la table de cet utilisateur"),
          h5(strong("Le pourcentage marqué comme terminé selon chaque formation :"),
             style='color: black;background-color: #A7C7E7; text-align: center;'),
          br(),
          br(),
          plotlyOutput("Percent_achieved")))),
      br(),
      br(),
      h5(strong("Répartition des utilisateurs sur les formations suivies et sur les catégories de formations:"),style='color: black;background-color: #A7C7E7; text-align: center;'),
      br(),
      fluidRow(column(6,plotlyOutput("pie_courses")),
        column(6,plotlyOutput("pie_categories"))),
      br(),
      br(),
      fluidRow(column(6,
        h5(strong("Pourcentage des utilisateurs désactivés :"),
           style='color: black;background-color: #A7C7E7; text-align: center;'),
        br(),
        echarts4rOutput("pie_user_desactivated")),
        column(6,h5(strong("Pourcentage des utilisateurs assignés :"),
                    style='color: black;background-color: #A7C7E7; text-align: center;'),
              br(),
             echarts4rOutput("pie_user_assigned"))),
      h5(strong("Les dix premiers meilleurs cours en termes de minutes consommées :"),
         style='color: black;background-color: #A7C7E7; text-align: center;'),
      br(),
      tableOutput("Top_courses")
    ),
    tabPanel("Prédiction des chiffres avec CNN",
             titlePanel("Choisir l'image à prédire"),
             
             sidebarLayout(
               sidebarPanel(
                 fileInput("image_path", label = "Choisir une image")
               ),
               mainPanel(
                 h5(strong("Le chiffre prédit par le modèle :"),
                    style='color: black;background-color: #A7C7E7; text-align: center;'),
                 textOutput("prediction"),
                 br(),
                 br(),
                 h5(strong("L'image choisie par l'utilisateur :"),
                    style='color: black;background-color: #A7C7E7; text-align: center;'),
                 plotOutput("image")
               )
             )
             )
)


#-------------------------------------------------------------------------Server
server <- function(input, output, session) {
  
  
  User_selected<-reactive(input$Select_user)
  
  User_data<-reactive(df[df$Nom.de.l.utilisateur==User_selected(),c("Titre.du.cours","Catégorie.de.cours")])
  
  output$User_courses_table<-renderTable({User_data()}
    ,bordered = TRUE)
  output$download_user_courses<-downloadHandler(filename = function(){paste(User_selected(),"data", ".csv", sep = "")}, 
                                                content = function(fname){
                                                  write.csv(User_data(), fname)
                                                })
  
  output$User_desactivated<-renderText(paste(df[df$Nom.de.l.utilisateur==User_selected(),"Utilisateur.désactivé"][1,1]))
  
  output$Percent_achieved<-renderPlotly({data.frame(df[df$Nom.de.l.utilisateur==User_selected(),])%>%plot_ly(x = ~Titre.du.cours,
                                                                                                             y = ~Pourcentage.marqué.comme.terminé,
                                                                                                             type = 'bar', text = text,
                                                                                                            marker = list(color = 'rgb(158,202,225)',
                                                                                                            line = list(color = 'rgb(9,48,107)',
                                                                                                            width = 1)))%>%
                                                                                                            layout(yaxis=list(title="Pourcentage marqué comme terminé"),
                                                                                                            xaxis = list(showticklabels = FALSE,
                                                                                                            title='Titre du cours'))%>%
                                                                                                            layout(plot_bgcolor='rgb(254, 247, 234)')%>%
                                                                                                            layout(paper_bgcolor='rgb(254, 247, 234)')
                                        })
  output$pie_courses<-renderPlotly({df_courses_groups<-group_by(df,Titre.du.cours)
                        df_courses_groups<-df_courses_groups%>%summarise(n = n())
                        df_courses_groups<-data.table(df_courses_groups,key="n")
                        df_courses_groups[order(-rank(n), Titre.du.cours)[5:nrow(df_courses_groups)],Titre.du.cours:="Other.courses"]
                        df_courses_groups<-df_courses_groups[, list(n=sum(n),Pcnt=sum(n)/df_courses_groups[,sum(n)]),by="Titre.du.cours"]%>%
                        data.frame()%>% 
                        plot_ly(labels=~Titre.du.cours,values=~Pcnt,sort=FALSE,marker=list(colors=colors_courses_categories,line=list(color="black",width=1)))%>%add_pie(hole=0.6)%>%layout(legend=list(orientation = 'h',title=list(text='<b> Formations suivies : </b>'),font = list(
                          family = "sans-serif",
                          size = 12,
                          color = "#000"),
                          bgcolor = "#E2E2E2",
                          bordercolor = "#FFFFFF",
                          borderwidth = 2))%>%layout(plot_bgcolor='transparent')%>%layout(paper_bgcolor='transparent')
                                  })
  output$pie_categories<-renderPlotly({df_categories_groups<-group_by(df,Catégorie.de.cours)
                        df_categories_groups<-df_categories_groups%>%summarise(n = n())
                        df_categories_groups<-data.table(df_categories_groups,key="n")
                        df_categories_groups[order(-rank(n), Catégorie.de.cours)[5:nrow(df_categories_groups)],Catégorie.de.cours:="Autres.catégories"]
                        df_categories_groups<-df_categories_groups[, list(n=sum(n),Pcnt=sum(n)/df_categories_groups[,sum(n)]),by="Catégorie.de.cours"]%>%
                        data.frame()%>% 
                        plot_ly(labels=~Catégorie.de.cours,values=~Pcnt,sort=FALSE,marker=list(colors=colors_courses_categories,line=list(color="black",width=1)))%>%add_pie(hole=0.6)%>%layout(legend=list(orientation = 'h',title=list(text='<b> Catégories de formations : </b>'),font = list(
                          family = "sans-serif",
                          size = 12,
                          color = "#000"),
                          bgcolor = "#E2E2E2",
                          bordercolor = "#FFFFFF",
                          borderwidth = 2))%>%layout(plot_bgcolor='transparent')%>%layout(paper_bgcolor='transparent')
                                      })
  output$pie_user_desactivated<-renderEcharts4r({pie_desactivated <- count(df, Utilisateur.désactivé) %>% 
                                                  e_charts(x = Utilisateur.désactivé) %>% 
                                                  e_pie(n, legend = FALSE, name = "Utilisateur désactivé ?",radius = c("50%", "70%")) %>% 
                                                  e_tooltip() %>% 
                                                  e_title("Utilisateur désactivé ?")%>%
                                                  e_labels(show = TRUE,
                                                          formatter = "{d}%",
                                                          position = "inside") %>%
                                                  e_color(colors_User_desactivated)%>%
                                                  e_theme("chalk")
                                                pie_desactivated
                                                })
  output$pie_user_assigned<-renderEcharts4r({pie_assigned <- count(df, Assigné) %>% 
                                            e_charts(x = Assigné) %>% 
                                            e_pie(n, legend = FALSE, name = "Utilisateur assigné ?",radius = c("50%", "70%")) %>% 
                                            e_tooltip() %>% 
                                            e_title("Utilisateur assigné ?")%>%
                                            e_labels(show = TRUE,
                                                    formatter = "{d}%",
                                                    position = "inside")%>%
                                            e_color(colors_User_desactivated)%>%
                                            e_theme("chalk")
                                          pie_assigned
                                        })
  output$Top_courses<-renderTable({df_top_courses<-data.frame(arrange(aggregate(Minutes.consommées ~ Titre.du.cours, df, sum),
                                           desc(Minutes.consommées)))
                        df_top_courses[1:10,]},
                        bordered = TRUE)
  
  path<-reactive(input$image_path)
  
  image <- reactive({
    req(path()$datapath,cancelOutput = TRUE)
    jpeg::readJPEG(path()$datapath)
  })
  
  output$prediction <- renderText(
    {req(path()$datapath,cancelOutput = TRUE)
      print(path()$datapath)
      img<-load_image(path()$datapath)
      paste0( predict_classes(model, img))
    }
  )
  
  output$image <- renderPlot({
    req(image(),cancelOutput = TRUE)
    plot(as.raster(image()))
  })
  
}

shinyApp(ui, server)
