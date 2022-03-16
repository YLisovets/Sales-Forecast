library(DBI)
library(odbc)

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "SERVER2",
                 Database = "work",
                 UID = "sa",
                 PWD = "server232", # rstudioapi::askForPassword("Database password")
)

# Read sales
sales_new <- dbGetQuery(con,'
      select t1."_Period" as date, sum(t1."_Fld17446" + t1."_Fld17448") as value,
             t6."_Description" as cat_id, t3."_Description" as subdiv_id, t4."_Description" as proj_id
      from "_AccumRg17435" as t1
      join "_Reference120" as t2
      on t1."_Fld17436RRef" = t2."_IDRRef" and
         t1."_Active" = 1 and
         t1."_Period" > {ts \'4021-05-31 23:59:59\'}
      join "_Reference122" as t5
      on t2."_Fld2045RRef" = t5."_IDRRef"
      join "_Reference19598" as t6
      on t5."_Fld19599RRef" = t6."_IDRRef"
      join "_Reference135" as t3
      on t1."_Fld17441RRef" = t3."_IDRRef"
      join "_Reference143" as t4
      on t1."_Fld17442RRef" = t4."_IDRRef"
      group by t1."_Period", t6."_Description", t3."_Description", t4."_Description"
      ')

# Read Subdivision reference

ref_subdiv <- dbGetQuery(con,'
    select t1."_Description" as subdiv_id, t2."_Description" as subdiv_parent
    from "_Reference135" as t1
    join "_Reference135" as t2
    on t1."_ParentIDRRef" = t2."_IDRRef" and
    t1."_Marked" = 0
    ')

dbDisconnect(con)
