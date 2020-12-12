library("rstudioapi")                                 # Load rstudioapi package
setwd(dirname(getActiveDocumentContext()$path))       # Set working directory to source file location

install.packages("tidyverse")
library("tidyverse")

install.packages("sqldf")
library(sqldf)


get_p<-function(model_id, app_id,app_version) {

  # ---------- Ucitavanje podataka na osnovu ulaznih parametara: app_id, app_version, model_id ----------

  #ucitavanje model_details
  get_current_model<-function(model_id){

    #ukoliko model ima vise verzija, da se uzme poslednja
    req<-str_replace("select * from MODEL_DETAILS md inner join MODELS m on m.id=md.model_id where
                   m.id=@@model_id and md.MODEL_VERSION = (select max(MODEL_VERSION) from MODEL_DETAILS where
                   model_id=@@model)","@@model",model_id)

    current_model<-sqldf(req)
    return(current_model)

  }

  #ucitavanje poslednjeg unosa u kreditnu aplikaciju
  get_current_app <- function(app_id, app_version) {

    #sa forme se prosledjuje app_id i app_version. Ucitavamo poslednje unete vrednosti za kred. aplikaciju i spajamo sa tabelom Credit_Bureau
    req<-str_replace("select sum(cb.debt) as debt,a.sex,a.family,a.children,a.rent, sum(cb.overdue) as overdue
              from AppHistory a
              left join CreditBureau cb on a.APP_ID=cb.application_id
              where a.APP_ID=@@APP_ID and a.VERSION=@@APP_VERSION
              group by a.APP_ID", "@@APP_ID", app_id)

    req<-str_replace(req,"@@APP_VERSION", app_version)

    current_app<-sqldf(req)
    return(current_app)
  }


  # ----------------- ZA TEST (rucno napravljene tabele, umesto poziva ka bazi) ------------------
  current_model<-tribble(

    ~model_id,  ~model_version,  ~coef,      ~col_name,  ~col_classification,~col_type,  ~string_match, ~start_range, ~end_range,
    1,          1             ,-5.1626802,  "COEF"     , "intercept "       , ""    ,     ""           , 0,             0,
    1,          1             , 0.0007927,  "debt"     , "debt"       	     , "+"   ,     ""           , 0,             0,
    1,          1             , 0.3996028,  "sex"      , "sexmale"          ,"match",    "Male"        , 0,             0,
    1,          1             , 0.8707857,  "family"   , "marriedTRUE"      ,"match",    "MARRIED"     , 0,             0,
    1,          1             , 0.3482291,  "children" , "children"         , "+"   ,     ""           , 0,             0,
    1,          1             , 1.4557413,  "rent"     , "rentTRUE"         ,"match",    "Y"           , 0,             0,
    1,          1             , 0.4927653,  "overdue"  , "overdue_0"        ,"range",     ""           , 0,             1,
    1,          1             , 0.9504462,  "overdue"  , "overdue_25"       ,"range",     ""           , 2,             25,
    1,          1             , 1.0687459,  "overdue"  , "overdue_inf"      ,"range",     ""           , 26,           10^10

  )


  current_app<-tribble(
    ~debt,  ~sex,   ~family,   ~children, ~rent, ~overdue,
    8000,  "Female", "MARRIED", -4       , "N"  , 1
  )

  # ----------------- KRAJ UCITAVANJA PODATAKA -----------------


  #jedinstvene vrednosti kolona koje ucestvuju u modelu
  distinct_col_name<-unique(filter(current_model, col_name!="COEF")$col_name)

  #prazna tabela u koju cemo upisivati rezultat for petlje
  model_calc <- filter(current_model, model_version == "-")

  j<-1

  for (i in distinct_col_name) {

    #uzmi redove gde je col_name = ime kolone iz petlje
    df<-filter(current_model, col_name == i)

    #dodaj novu kolonu koja ima oblik koji zahteva data scientist
    df<-mutate(df, final_col = 0)

    #prvobitna ideja je bila da vrednostima iz current_app pristupim pomocu pokazivaca - current_app@i (gde je i ime kolone), medjutim R
    #ne podrzava pokazivace, pa sam morala da koristim redni broj kolone - current_app[j]

    x<-current_app[j] #vrednost iz aplikacije za kolonu iz petlje


    #tip podatka definisan u model_details: range, match, +,-,...
    curr_col_type<-unique(df$col_type)

    if(curr_col_type=='range'){
      x<-as.double(x)
      df<-mutate(df, final_col=as.double(ifelse(start_range <= x & x<=end_range,  1,0)))

    } else if (curr_col_type=='match') {
      x<-as.character(x)
      df<-mutate(df, final_col=as.double(ifelse(length(grep(x,string_match))==1,  1,0)))

    } else if (curr_col_type=='+') {
      x<-as.double(x)
      df<-mutate(df, final_col=as.double(ifelse( !is.na(x) & !is.null(x),  abs(x),0)))

    } else if (curr_col_type=='-') {
      x<-as.double(x)
      df<-mutate(df, final_col=as.double(ifelse( !is.na(x) & !is.null(x),  -abs(x),0)))

    }
    model_calc<-bind_rows(model_calc,df)
    j<-j+1

  }

  #probability
  a<-(filter(current_model, col_name == 'COEF'))$coef #uzmima se intercept

  #mnoze se koeficijenti sa transformisanom kolonom (u skladi sa data science potrebama)
  model_calc$coef_col <- model_calc$coef*model_calc$final_col

  score<-a+sum(model_calc$coef_col)
  p<-1/(1+exp(-score))

  # VRACA SE VEROVATNOCA DEFAULT-A
  ###########
  return(p) #
  ###########
}

################## KRAJ ################################



