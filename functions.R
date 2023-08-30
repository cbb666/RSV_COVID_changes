# Calculating new agegroups----
New_AGEGR012 <- function(each) { 
  res.df_t <- data.frame(SID=NA,start_dat=NA,end_dat=NA,duration_in_months=NA,date_middle=NA,
                           AGEGR=NA,DenoRaw=NA,ALRI_total=NA,ALRI_tested=NA,ALRI_RSV=NA,
                           sALRI_total=NA,sALRI_tested=NA,sALRI_RSV=NA,vsALRI_total=NA,vsALRI_tested=NA,vsALRI_RSV=NA,
                           ALRIDeaths_total=NA,ALRIDeaths_tested=NA,ALRIDeaths_RSV=NA )
 
  res.df <- each[ 
    with(each, 
         SID %in% intersect( intersect( intersect(SID[AGEGR == "0-<3m" ], 
                                                  SID[AGEGR == "3-<6m" ]),  
                                        SID[AGEGR == "6-<9m" ]),  
                             SID[AGEGR == "9-<12m" ])  
         & AGEGR %in% c("0-<3m", "3-<6m","6-<9m", "9-<12m" )), 
    c("SID","start_dat","end_dat","duration_in_months","date_middle",
      "AGEGR","DenoRaw","ALRI_total","ALRI_tested","ALRI_RSV",
      "sALRI_total","sALRI_tested","sALRI_RSV","vsALRI_total","vsALRI_tested","vsALRI_RSV",
      "ALRIDeaths_total","ALRIDeaths_tested","ALRIDeaths_RSV")] 

  r1 <- res.df_t %>% mutate(AGEGR = "0-<6m",end_dat=res.df$start_dat[1],duration_in_months=res.df$duration_in_months[1],date_middle=res.df$date_middle[1],
                            ALRI_RSV = sum(head(res.df,2)$ALRI_RSV),sALRI_RSV = sum(head(res.df,2)$sALRI_RSV),
                            vsALRI_RSV = sum(head(res.df,2)$vsALRI_RSV),ALRIDeaths_RSV = sum(head(res.df,2)$ALRIDeaths_RSV),
                            ALRI_total = sum(head(res.df,2)$ALRI_total), ALRI_tested=sum(head(res.df,2)$ALRI_tested),
                            sALRI_total=sum(head(res.df,2)$sALRI_total),  sALRI_tested=sum(head(res.df,2)$sALRI_tested),
                            vsALRI_total=sum(head(res.df,2)$vsALRI_total), vsALRI_tested=sum(head(res.df,2)$vsALRI_tested),
                            ALRIDeaths_total=sum(head(res.df,2)$ALRIDeaths_total), ALRIDeaths_tested=sum(head(res.df,2)$ALRIDeaths_tested),
                            DenoRaw=sum(head(res.df,2)$DenoRaw))
  r2 <- res.df_t %>% mutate(AGEGR = "6-<12m",end_dat=res.df$start_dat[1],duration_in_months=res.df$duration_in_months[1],date_middle=res.df$date_middle[1],
                            ALRI_RSV = sum(tail(res.df,2)$ALRI_RSV),sALRI_RSV = sum(tail(res.df,2)$sALRI_RSV),
                            vsALRI_RSV = sum(tail(res.df,2)$vsALRI_RSV),ALRIDeaths_RSV = sum(tail(res.df,2)$ALRIDeaths_RSV),
                            ALRI_total = sum(tail(res.df,2)$ALRI_total), ALRI_tested=sum(tail(res.df,2)$ALRI_tested),
                            sALRI_total=sum(tail(res.df,2)$sALRI_total),  sALRI_tested=sum(tail(res.df,2)$sALRI_tested),
                            vsALRI_total=sum(tail(res.df,2)$vsALRI_total), vsALRI_tested=sum(tail(res.df,2)$vsALRI_tested),
                            ALRIDeaths_total=sum(tail(res.df,2)$ALRIDeaths_total), ALRIDeaths_tested=sum(tail(res.df,2)$ALRIDeaths_tested),
                            DenoRaw=sum(tail(res.df,2)$DenoRaw))
  r3 <- res.df_t %>% mutate(AGEGR = "0-<12m",end_dat=res.df$start_dat[1],duration_in_months=res.df$duration_in_months[1],date_middle=res.df$date_middle[1],
                            ALRI_RSV = sum(res.df$ALRI_RSV),sALRI_RSV = sum(res.df$sALRI_RSV),
                            vsALRI_RSV = sum(res.df$vsALRI_RSV),ALRIDeaths_RSV = sum(res.df$ALRIDeaths_RSV),
                            ALRI_total = sum(res.df$ALRI_total), ALRI_tested=sum(res.df$ALRI_tested),
                            sALRI_total=sum(res.df$sALRI_total),  sALRI_tested=sum(res.df$sALRI_tested),
                            vsALRI_total=sum(res.df$vsALRI_total), vsALRI_tested=sum(res.df$vsALRI_tested),
                            ALRIDeaths_total=sum(res.df$ALRIDeaths_total), ALRIDeaths_tested=sum(res.df$ALRIDeaths_tested),
                            DenoRaw=sum(res.df$DenoRaw)) 
  res.df_t <- rbind(res.df,r1,r2,r3)
  res.df_t$SID[is.na(res.df_t$SID)] <- res.df_t$SID[1]
  res.df_t$start_dat[is.na(res.df_t$start_dat)] <- res.df_t$start_dat[1]
  return(res.df_t)  
}


New_AGEGR1224 <- function(each) {  
  res.df_t <- data.frame(SID=NA,start_dat=NA,end_dat=NA,duration_in_months=NA,date_middle=NA,
                         AGEGR=NA,DenoRaw=NA,ALRI_total=NA,ALRI_tested=NA,ALRI_RSV=NA,
                         sALRI_total=NA,sALRI_tested=NA,sALRI_RSV=NA,vsALRI_total=NA,vsALRI_tested=NA,vsALRI_RSV=NA,
                         ALRIDeaths_total=NA,ALRIDeaths_tested=NA,ALRIDeaths_RSV=NA )
  res.df <- each[
    with(each, 
         SID %in% intersect( SID[AGEGR == "12-<18m" ],  
                             SID[AGEGR == "18-<24m" ]) 
         & AGEGR %in% c("12-<18m","18-<24m" )), 
    c("SID","start_dat","end_dat","duration_in_months","date_middle",
      "AGEGR","DenoRaw","ALRI_total","ALRI_tested","ALRI_RSV",
      "sALRI_total","sALRI_tested","sALRI_RSV","vsALRI_total","vsALRI_tested","vsALRI_RSV",
      "ALRIDeaths_total","ALRIDeaths_tested","ALRIDeaths_RSV")] 
  
  r1 <- res.df_t %>% mutate(AGEGR = "12-<24m",end_dat=res.df$start_dat[1],duration_in_months=res.df$duration_in_months[1],date_middle=res.df$date_middle[1],
                            ALRI_RSV = sum(res.df$ALRI_RSV),sALRI_RSV = sum(res.df$sALRI_RSV),
                            vsALRI_RSV = sum(res.df$vsALRI_RSV),ALRIDeaths_RSV = sum(res.df$ALRIDeaths_RSV),
                            ALRI_total = sum(res.df$ALRI_total), ALRI_tested=sum(res.df$ALRI_tested),
                            sALRI_total=sum(res.df$sALRI_total),  sALRI_tested=sum(res.df$sALRI_tested),
                            vsALRI_total=sum(res.df$vsALRI_total), vsALRI_tested=sum(res.df$vsALRI_tested),
                            ALRIDeaths_total=sum(res.df$ALRIDeaths_total), ALRIDeaths_tested=sum(res.df$ALRIDeaths_tested),
                            DenoRaw=sum(res.df$DenoRaw))
  
  res.df_t <- rbind(res.df,r1)
  res.df_t$SID[is.na(res.df_t$SID)] <- res.df_t$SID[1]
  res.df_t$start_dat[is.na(res.df_t$start_dat)] <- res.df_t$start_dat[1]
  return(res.df_t)  
}


New_AGEGR1260 <- function(each) {  
  res.df_t <- data.frame(SID=NA,start_dat=NA,end_dat=NA,duration_in_months=NA,date_middle=NA,
                         AGEGR=NA,DenoRaw=NA,ALRI_total=NA,ALRI_tested=NA,ALRI_RSV=NA,
                         sALRI_total=NA,sALRI_tested=NA,sALRI_RSV=NA,vsALRI_total=NA,vsALRI_tested=NA,vsALRI_RSV=NA,
                         ALRIDeaths_total=NA,ALRIDeaths_tested=NA,ALRIDeaths_RSV=NA )
  res.df <- each[
    with(each, 
         SID %in% intersect( intersect(SID[AGEGR == "12-<18m" ], 
                                       SID[AGEGR == "18-<24m" ]), 
                             SID[AGEGR == "24-<60m" ] )             
         & AGEGR %in% c("12-<18m","18-<24m","24-<60m")),  
    c("SID","start_dat","end_dat","duration_in_months","date_middle",
      "AGEGR","DenoRaw","ALRI_total","ALRI_tested","ALRI_RSV",
      "sALRI_total","sALRI_tested","sALRI_RSV","vsALRI_total","vsALRI_tested","vsALRI_RSV",
      "ALRIDeaths_total","ALRIDeaths_tested","ALRIDeaths_RSV")]  
  
  r1 <- res.df_t %>% mutate(AGEGR = "12-<60m",end_dat=res.df$start_dat[1],duration_in_months=res.df$duration_in_months[1],date_middle=res.df$date_middle[1],
                            ALRI_RSV = sum(res.df$ALRI_RSV),sALRI_RSV = sum(res.df$sALRI_RSV),
                            vsALRI_RSV = sum(res.df$vsALRI_RSV),ALRIDeaths_RSV = sum(res.df$ALRIDeaths_RSV),
                            ALRI_total = sum(res.df$ALRI_total), ALRI_tested=sum(res.df$ALRI_tested),
                            sALRI_total=sum(res.df$sALRI_total),  sALRI_tested=sum(res.df$sALRI_tested),
                            vsALRI_total=sum(res.df$vsALRI_total), vsALRI_tested=sum(res.df$vsALRI_tested),
                            ALRIDeaths_total=sum(res.df$ALRIDeaths_total), ALRIDeaths_tested=sum(res.df$ALRIDeaths_tested),
                            DenoRaw=sum(res.df$DenoRaw) )
  
  res.df_t <- rbind(res.df,r1)
  res.df_t$SID[is.na(res.df_t$SID)] <- res.df_t$SID[1]
  res.df_t$start_dat[is.na(res.df_t$start_dat)] <- res.df_t$start_dat[1]
  return(res.df_t)   
}


# Moving average----
getSIDAGEGR_MOV <- function(eachSIDAGEGR) {
  res <- apply(period_matrix, 1, FUN = getdata_cond, dataset = eachSIDAGEGR)  
  # messages(res$SID)
  res <- do.call(rbind,
                 as.list(res)) 
  return(res)
}

getdata_cond <- function(period_12, dataset = eachSIDAGEGR) {  
  if( nrow( dataset[dataset$start_dat %in% period_12,] )<12 ){
    return(NULL)
  }else{
     message(period_12[12])
     df <- dataset[dataset$start_dat %in% period_12,] %>% 
        dplyr::summarise( SID=SID, start_dat=start_dat, end_dat= start_dat+11/12, date_middle=start_dat+11/12/2,
                          AGEGR=AGEGR, ALRI_total_MOV=sum(ALRI_total),ALRI_tested_MOV=sum(ALRI_tested),ALRI_RSV_MOV=sum(ALRI_RSV),
                          sALRI_RSV_MOV=sum(sALRI_RSV), sALRI_total_MOV=sum(sALRI_total),sALRI_tested_MOV=sum(sALRI_tested),
                          vsALRI_total_MOV=sum(vsALRI_total),vsALRI_tested_MOV=sum(vsALRI_tested),vsALRI_RSV_MOV=sum(vsALRI_RSV),
                          ALRIDeaths_total_MOV=sum(ALRIDeaths_total),ALRIDeaths_tested_MOV=sum(ALRIDeaths_tested),ALRIDeaths_RSV_MOV=sum(ALRIDeaths_RSV),
                          Deno_MOV=sum(DenoRaw)/12)  %>% .[1,]
    return(df)
  }
}

# not non-consecutive months
genAverage_rate.prop1 <- function(dataset){
  res <- dataset %>% group_by(SID,AGEGR,Year) %>% dplyr::summarise(
      ALRI_total_MOV=sum(ALRI_total),ALRI_tested_MOV=sum(ALRI_tested),ALRI_RSV_MOV=sum(ALRI_RSV),
      sALRI_RSV_MOV=sum(sALRI_RSV), sALRI_total_MOV=sum(sALRI_total),sALRI_tested_MOV=sum(sALRI_tested),
      vsALRI_total_MOV=sum(vsALRI_total),vsALRI_tested_MOV=sum(vsALRI_tested),vsALRI_RSV_MOV=sum(vsALRI_RSV),
      ALRIDeaths_total_MOV=sum(ALRIDeaths_total),ALRIDeaths_tested_MOV=sum(ALRIDeaths_tested),ALRIDeaths_RSV_MOV=sum(ALRIDeaths_RSV),
      Deno_MOV=mean(DenoRaw)) %>% as.data.frame() %>% 
    mutate(ALRI_RSV_Deno_MOV=Deno_MOV,sALRI_RSV_Deno_MOV=Deno_MOV,vsALRI_RSV_Deno_MOV=Deno_MOV)
  return(res)
}

# meta analysis----
# Rates----
# only one rate
genINC <- function(case, deno) {
  est <- log(case/deno)
  se <- sqrt(1/case- 1/deno)
  return(c(est = est, se = se))
}

# Meta Rate
genMetaRateEach <- function(df, prefix, varToKeep = NULL, rate.adjust = 1000) { # prefix="ALRI_RSV"
  message(df$Income[1],df$Dev[1],df$Year[1], df$AGEGR[1])
  df <- df[ !(is.na(df[[paste(prefix, "_Deno_MOV", sep = "")]]) | is.na(df[[paste(prefix, "_MOV", sep = "")]])),] 
  df[paste(prefix, "_Deno_MOV", sep = "")] <- round(df[paste(prefix, "_Deno_MOV", sep = "")],0) 
  df[paste(prefix, "_MOV", sep = "")] <- round(df[paste(prefix, "_MOV", sep = "")],0) 
  
  if(is.null(df)){
    res.df <- data.frame(
      est = NA,
      se = NA,
      n.all = 0,
      I2 = NA,
      IR.est = NA,
      IR.lci = NA,
      IR.uci = NA
    )
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }  
  
  if(nrow(df)==0){  # for the sake of
    res.df <- data.frame(
      est = NA,
      se = NA,
      n.all = 0, 
      I2 = NA,
      IR.est = NA,
      IR.lci = NA,
      IR.uci = NA
    )
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }
  
  if(nrow(df) ==1) {
    # df <-  data.frame(ALRI_RSV_MOV=c(0),ALRI_RSV_Deno_MOV=c(200)) 
    temp <- genINC(df[[paste(prefix, "_MOV", sep = "")]], 
                   df[[paste(prefix, "_Deno_MOV", sep = "")]])
    res.df <- data.frame(
      est = temp[1],
      se = temp[2],
      n.all = nrow(df),
      I2 = NA
    )
  } else {
    fit <- rma.glmm(measure = "IRLN", data = df, #
                    xi = get(paste(prefix, "_MOV", sep = "")),
                    ti = get(paste(prefix, "_Deno_MOV", sep = ""))
                    ) 
    res.df <- data.frame(
      est = as.numeric(fit$b),
      se = fit$se,
      n.all = nrow(df)
    )
    res.df$I2 <- formatC(fit$I2, digits=1, format="f")
  }
  res.df$IR.est <- exp(res.df$est) * rate.adjust
  res.df$IR.lci <- exp(res.df$est - 1.96*res.df$se)* rate.adjust 
  res.df$IR.uci <- exp(res.df$est + 1.96*res.df$se)* rate.adjust
  res.df <- cbind( df[1,varToKeep], res.df ) 
  return(res.df) 
}

genMC <- function(df, id, input.mean, input.se, transFUN, n, output.name = "value") {
set.seed(6920)
res <- by( df[c(id, input.mean, input.se)],
         df[id],
         function(x, n) 
           return( data.frame(
             id = x[[id]],
             index = 1:n,
             value = transFUN(rnorm(n = n,mean = x[[input.mean]], sd = x[[input.se]]))
           )),
         n = n) 
res <- do.call(rbind, res)
row.names(res) <- NULL
names(res) <- c(id, "index", output.name)
return(res)
}


genRateGlobal <- function(df, n.level,genMC = FALSE ){
  df <- df[df$Group != "Global",]  
  if(sum(!is.na(df$est))<n.level) {
    return(df)
  }else{
    rate <- genMC(df =df, id = "Group", input.mean = "est", input.se = "se", n = N.MC,transFUN = exp) 
    rate <- left_join(rate, unique( df[c("Group", "Pop")] ) )
    rate <- rate %>% group_by(index) %>%
      dplyr::summarise(N = round(sum(value * Pop),3)) 
    new.df <- df[1,]
    new.df$AGEGR <- df$AGEGR[1]
    new.df$Group <- "Global"  
    new.df$est <- NA
    new.df$se <- NA
    new.df$n.all <- sum(df$n.all)
    new.df$N.est <- round(median(rate$N),3) 
    new.df$N.lci <- round(quantile(rate$N, 0.025),3) 
    new.df$N.uci <- round(quantile(rate$N, 0.975),3)  
    new.df$Pop <- sum(df$Pop)
    new.df$IR.est <- with(new.df, N.est / Pop*1000)
    new.df$IR.lci <- with(new.df, N.lci / Pop*1000)
    new.df$IR.uci <- with(new.df, N.uci / Pop*1000)
    if(genMC) { 
      rate$rate <- rate$N / sum(df$Pop)*1000
      return(rate)
    }
    return( rbind(df, new.df) )
  }
}

genDevFig_MOVresults <- function(Each,choice=NULL,outcome=NULL) {   
  if( choice=="finerAge" ){
  Each$AGEGR <- factor(Each$AGEGR, levels = c("0-<3m","3-<6m","6-<9m", "9-<12m", "12-<24m"))  #"24-<60m"
  res <-  ggplot(data = Each, aes(x =  as.Date(end_dat))) + 
    geom_line(aes(y = IR.est,colour = Dev)) + #,size=0.3,na.rm=T,show.legend = FALSE  colour = "blue4",
    geom_point(aes(y = IR.est,colour = Dev)) +  # ,size=0.3,na.rm=Tshow.legend = FALSE
    geom_line(aes(y = IR.est,colour = Dev))  +
    # geom_ribbon(aes (ymin = IR.lci , ymax = IR.uci,colour = Dev,fill = Dev), alpha = 0.2, size=0.3,na.rm=T) + #,,show.legend = FALSEcolor="blue4", fill = "blue4",
    # geom_hline(yintercept = mean(Rates_Dev_pre,na.rm=T)) +
    geom_line(aes(y = Rates_Dev_pre,colour = Dev),linetype=2) +#,size=0.17,na.rm=T
    facet_wrap(~AGEGR, ncol = 2 , scales = 'free_y') +  
    scale_y_continuous(name =  paste("RSV -",outcome,"hospitalisation rate per 1000", sep = " ")) + # "RSV-ALRI hospitalisation rate \nper 1000",limits = c(-20,115)
    scale_x_date(
      name = "Date (month ending)",
      date_breaks = "3 month",  date_labels =  "%b \n%Y",limits =NULL,
      expand = expansion(mult = c(0.05, 0.05))) +
    scale_colour_lancet(name="Development status",labels = c("Developing Countries","Industrialised Countries")) +  # 
    scale_fill_lancet(name="Development status",labels = c("Developing Countries","Industrialised Countries")) +
    # labs(colour="development status")+
    theme_bw()  +
    theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5))+  # angle = 90,
    # ggtitle(Each$Dev[1])+
    theme(plot.title.position = "plot", # axis.title = element_blank(),
          legend.title = element_text(face = "bold"),
          legend.position="bottom",legend.box = "horizontal")+
    guides(shape = guide_legend(override.aes = list(size = 5)))
  res.df <- ggsave(
    res,
    filename = paste("Figures1/",Each$Index1[1],"/",Each$Index[1], "Dev", "_finer.tiff", sep = " "), width = 10, height = 6 ) 
    } else{
    Each$AGEGR <- factor(Each$AGEGR, levels = c("0-<6m","6-<12m","12-<60m", "0-<60m" ))
    res <-  ggplot(data = Each, aes(x =  as.Date(end_dat))) + 
      geom_line(aes(y = IR.est,colour = Dev)) + #,size=0.3,na.rm=Tshow.legend = FALSE  colour = "blue4",
      geom_point(aes(y = IR.est,colour = Dev)) +  # ,size=0.3,na.rm=Tshow.legend = FALSE
      # geom_ribbon(aes (ymin = IR.lci , ymax = IR.uci,colour = Dev,fill = Dev), alpha = 0.2, size=0.3,na.rm=T) + #,,show.legend = FALSEcolor="blue4", fill = "blue4",
      # geom_hline(yintercept = mean(Rates_Dev_pre,na.rm=T)) +
      geom_line(aes(y = Rates_Dev_pre,colour = Dev),linetype=2) + #,size=0.17,na.rm=T
      facet_wrap(~AGEGR, ncol = 2 , scales = 'free_y') +  
      scale_y_continuous(name =  paste("RSV -",outcome,"hospitalisation rate per 1000", sep = " ")) + # "RSV-ALRI hospitalisation rate \nper 1000",limits = c(-20,115)
      scale_x_date(
        name = "Date (month ending)",
        date_breaks = "3 month",  date_labels =  "%b \n%Y",limits =NULL,
        expand = expansion(mult = c(0.05, 0.05))) +
      scale_colour_lancet(name="Development status",labels = c("Developing Countries","Industrialised Countries")) +
      scale_fill_lancet(name="Development status",labels = c("Developing Countries","Industrialised Countries")) +
      theme_bw()  +
      theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5))+  # angle = 90,
      # ggtitle(Each$Dev[1])+
      theme(plot.title.position = "plot", # axis.title = element_blank(),
            legend.title = element_text(face = "bold"),
            legend.position="bottom",legend.box = "horizontal")+
      guides(shape = guide_legend(override.aes = list(size = 5)))
  res.df <-  ggsave(
    res,
    filename = paste("Figures1/",Each$Index1[1],"/",Each$Index[1], "Dev", "_broad.tiff", sep = " "), width = 10, height = 6 ) # 8
   }  
    return(res)
}


genIncomeFig_MOVresults <- function(Each,choice=NULL,outcome=NULL) {  
  if( choice=="finerAge" ){
    Each$AGEGR <- factor(Each$AGEGR, levels = c("0-<3m","3-<6m","6-<9m", "9-<12m", "12-<24m"))
    Each$Income <- factor(Each$Income,levels = c("Lower-middle-income Countries","Upper-middle-income Countries","High-income Countries"))
    res <-  ggplot(data = Each, aes(x =  as.Date(end_dat))) + 
      geom_line(aes(y = IR.est,colour = Income)) + #,show.legend = FALSE  colour = "blue4",
      geom_point(aes(y = IR.est,colour = Income)) +  # show.legend = FALSE
      # geom_point(aes(y = median_googMov*-2,colour = Income),size=0.4,position = position_dodge2(width = 0.5)) +  #   quantile( hos_rate_ALRI_Income.meta$median_googMov,na.rm = T)
      # geom_errorbar(aes(ymin = p25*-2, ymax = p75*-2,colour = Income), width = 0.5, position = position_dodge2(width = 0.5),size=0.2) +
      # geom_line(aes(y = I2,colour = Income)) +
      # geom_line(aes(y = I2,colour = Income)) +
      # geom_ribbon(aes (ymin = IR.lci , ymax = IR.uci,colour = Income,fill = Income), alpha = 0.2, size=0.3,na.rm=T) + #,,show.legend = FALSEcolor="blue4", fill = "blue4",
      # geom_hline(yintercept = mean(Rates_Dev_pre,na.rm=T)) + 
      geom_line(aes(y = firstpoint,colour = Income),linetype=2) +
      facet_wrap(~AGEGR, ncol = 2 , scales = 'free_y') +  
      scale_y_continuous(name =  paste("RSV -",outcome,"hospitalisation rate per 1000", sep = " ")  ) +
                        #  limits = c(0,60), expand = c(0, 0),
                #       ,   sec.axis = sec_axis(~./2*(-1),
                  #                                             name = "retail and recreation change"
                                                            #   , breaks = seq(0,-30,-5))
                 #       )) + # "RSV-ALRI hospitalisation rate \nper 1000",limits = c(-20,115)
      scale_x_date(
        name = "Date (month ending)",
        date_breaks = "3 month",  date_labels =  "%b \n%Y",limits =NULL,
        expand = expansion(mult = c(0.05, 0.05))) +
      scale_colour_lancet() +
      scale_fill_lancet() +
      theme_bw()  +
      theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5))+  # angle = 90,
      # ggtitle(Each$Dev[1])+
      theme(plot.title.position = "plot", # axis.title = element_blank(),
            legend.title = element_text(face = "bold"),
            legend.position="bottom",legend.box = "horizontal")+
      guides(shape = guide_legend(override.aes = list(size = 5)))
  } else{
    Each$AGEGR <- factor(Each$AGEGR, levels =  c("0-<6m","6-<12m","12-<60m", "0-<60m" ))
    Each$Income <- factor(Each$Income,levels = c("Lower-middle-income Countries","Upper-middle-income Countries","High-income Countries"))
    res <-  ggplot(data = Each, aes(x =  as.Date(end_dat))) + 
      geom_line(aes(y = IR.est,colour = Income)) + #,show.legend = FALSE  colour = "blue4",
      geom_point(aes(y = IR.est,colour = Income)) +  # show.legend = FALSE
      # geom_ribbon(aes (ymin = IR.lci , ymax = IR.uci,colour = Income,fill = Income), alpha = 0.2, size=0.3,na.rm=T) + #,,show.legend = FALSEcolor="blue4", fill = "blue4",
      # geom_hline(yintercept = mean(Rates_Dev_pre,na.rm=T)) +
      # geom_point(aes(y = median_googMov*-2,colour = Income),size=0.4,position = position_dodge2(width = 0.5)) +  #   quantile( hos_rate_ALRI_Income.meta$median_googMov,na.rm = T)
     # geom_errorbar(aes(ymin = p25*-2, ymax = p75*-2,colour = Income), width = 0.5, position = position_dodge2(width = 0.5),size=0.2) +
      geom_line(aes(y = firstpoint,colour = Income),linetype=2) +
      facet_wrap(~AGEGR, ncol = 2 , scales = 'free_y') +  
      scale_y_continuous(name =  paste("RSV -",outcome,"hospitalisation rate per 1000", sep = " ")) +
                     #   , sec.axis = sec_axis(~./2*(-1),
                      #                       name = "retail and recreation change")) + # "RSV-ALRI hospitalisation rate \nper 1000",limits = c(-20,115)
      scale_x_date(
        name = "Date (month ending)",
        date_breaks = "3 month",  date_labels =  "%b \n%Y",limits =NULL,
        expand = expansion(mult = c(0.05, 0.05))) +
      scale_colour_lancet() +
      scale_fill_lancet() +
      theme_bw()  +
      theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5))+  # angle = 90,
      # ggtitle(Each$Dev[1])+
      theme(plot.title.position = "plot", # axis.title = element_blank(),
            legend.title = element_text(face = "bold"),
            legend.position="bottom",legend.box = "horizontal")+
      guides(shape = guide_legend(override.aes = list(size = 5)))
  }  
  return(res)
}

gen1224Fig_MOVresults <- function(Each,choice=NULL,outcome=NULL) {   
  if( choice=="Dev" ){
    Each$AGEGR <- factor(Each$AGEGR, levels = c("12-<18m", "18-<24m"))  #"24-<60m"
    res <-  ggplot(data = Each, aes(x =  as.Date(end_dat))) + 
      geom_line(aes(y = IR.est,colour = get(choice))) + #,size=0.3,na.rm=T,show.legend = FALSE  colour = "blue4",
      geom_point(aes(y = IR.est,colour = get(choice))) +  # ,size=0.3,na.rm=Tshow.legend = FALSE
      geom_line(aes(y = IR.est,colour = get(choice)))  +
      # geom_ribbon(aes (ymin = IR.lci , ymax = IR.uci,colour = Dev,fill = Dev), alpha = 0.2, size=0.3,na.rm=T) + #,,show.legend = FALSEcolor="blue4", fill = "blue4",
      # geom_hline(yintercept = mean(Rates_Dev_pre,na.rm=T)) +
      geom_line(aes(y = Rates_Dev_pre,colour = get(choice)),linetype=2) +#,size=0.17,na.rm=T
      facet_wrap(~AGEGR, ncol = 2 , scales = 'free_y') +  
      scale_y_continuous(name =  paste("RSV -",outcome,"hospitalisation rate per 1000", sep = " ")) + # "RSV-ALRI hospitalisation rate \nper 1000",limits = c(-20,115)
      scale_x_date(
        name = "Date (month ending)",
        date_breaks = "3 month",  date_labels =  "%b \n%Y",limits =NULL,
        expand = expansion(mult = c(0.05, 0.05))) +
      scale_colour_lancet(name="Development status",labels = c("Developing Countries","Industrialised Countries")) + 
      scale_fill_lancet(name="Development status",labels = c("Developing Countries","Industrialised Countries")) +
      # labs(colour="development status")+
      theme_bw()  +
      theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5))+  # angle = 90,
      # ggtitle(Each$Dev[1])+
      theme(plot.title.position = "plot", # axis.title = element_blank(),
            legend.title = element_text(face = "bold"),
            legend.position="bottom",legend.box = "horizontal")+
      guides(shape = guide_legend(override.aes = list(size = 5)))
    res.df <- ggsave(
      res,
      # filename = paste("Figures1/",Each$Index1[1],"/",Each$Index[1], Each$Dev[1], "_finer.tiff", sep = " "), width = 10, height = 6 ) # paste("Hello", 1:2, sep = "-", collapse = ",")
      filename = paste("Figures1/",Each$Index1[1],"/",Each$Index[1], "Dev", "_1224.tiff", sep = " "), width = 10, height = 6 ) 
  }else{
    Each$AGEGR <- factor(Each$AGEGR, levels =  c("12-<18m", "18-<24m"))
    Each$Income <- factor(Each$Income,levels = c("Lower-middle-income Countries","Upper-middle-income Countries","High-income Countries"))
    res <-  ggplot(data = Each, aes(x =  as.Date(end_dat))) + 
      geom_line(aes(y = IR.est,colour = Income)) + #,show.legend = FALSE  colour = "blue4",
      geom_point(aes(y = IR.est,colour = Income)) +  # show.legend = FALSE
      # geom_ribbon(aes (ymin = IR.lci , ymax = IR.uci,colour = Income,fill = Income), alpha = 0.2, size=0.3,na.rm=T) + #,,show.legend = FALSEcolor="blue4", fill = "blue4",
      # geom_hline(yintercept = mean(Rates_Dev_pre,na.rm=T)) +
      geom_line(aes(y = Rates_Income_pre,colour = Income),linetype=2) +
      facet_wrap(~AGEGR, ncol = 2 , scales = 'free_y') +  
      scale_y_continuous(name =  paste("RSV -",outcome,"hospitalisation rate per 1000", sep = " ")) + # "RSV-ALRI hospitalisation rate \nper 1000",limits = c(-20,115)
      scale_x_date(
        name = "Date (month ending)",
        date_breaks = "3 month",  date_labels =  "%b \n%Y",limits =NULL,
        expand = expansion(mult = c(0.05, 0.05))) +
      scale_colour_lancet() +
      scale_fill_lancet() +
      theme_bw()  +
      theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5))+  # angle = 90,
      # ggtitle(Each$Dev[1])+
      theme(plot.title.position = "plot", # axis.title = element_blank(),
            legend.title = element_text(face = "bold"),
            legend.position="bottom",legend.box = "horizontal")+
      guides(shape = guide_legend(override.aes = list(size = 5)))
    res.df <-  ggsave(
      res,
      filename = paste("Figures1/",Each$Index1[1],"/",Each$Index[1], "Income", "_1224.tiff", sep = " "), width = 10, height = 6 ) # 8
  }  
  return(res)
}

# Proportions----
genMetapropEach <- function(df, case.text, deno.text,varToKeep = NULL, prop.adjust = 100) {
  message(df$Dev[1],df$Income[1],df$AGEGR[1],df$Year[1])
  if(is.null(df)){
    res.df <- data.frame(
      est = NA,
      se = NA,
      n.all = 0,
      I2 = NA,
      prop.est = NA,
      prop.lci = NA,
      prop.uci = NA
    )
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }
  
  if( sum(df$ALRIDeaths_RSV_MOV==0)==nrow(df) ){ 
    res.df <- data.frame(
      est = 0,
      se = NA,
      n.all = nrow(df),
      I2 = NA,
      prop.est = 0,
      prop.lci = NA,
      prop.uci = NA
    )
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }
  
  df <- df[!(is.na(df[case.text]) | is.na(df[deno.text])),]
  df <- df[df[deno.text]!=0,] 
  df[deno.text] <- round(df[deno.text],0)
  df[case.text] <- round(df[case.text],0)
  
  if(nrow(df)==0){
    res.df <- data.frame(
      est = NA,
      se = NA,
      n.all = 0,
      I2 = NA,
      prop.est = NA,
      prop.lci = NA,
      prop.uci = NA
    )
    res.df <- cbind(df[1,varToKeep], res.df)
    return(res.df)
  }
  
  if(nrow(df) ==1) {
    fit <- rma.glmm(measure = "PLO", data = df, 
                    xi = get(case.text),
                    ni = get(deno.text),
                    model="UM.FS" # default
                    ) 
    res.df <- data.frame(
      est = as.numeric(fit$b),
      se = fit$se,
      n.all = nrow(df),
      I2 = NA
    )
  }else{
    fit <- rma.glmm(measure = "PLO", data = df, #logit transformed proportions (i.e., log odds)verbose=TRUE, 
                    xi = get(case.text),
                    ni = get(deno.text),
                    model="UM.RS"
                    ) 
    res.df <- data.frame(
      est = as.numeric(fit$b),
      se = fit$se,
      n.all = nrow(df)
    )
    res.df$I2 <- formatC(fit$I2, digits=1, format="f")
  }
  res.df$prop.est <- transf.ilogit(res.df$est) * prop.adjust
  res.df$prop.lci <- transf.ilogit(res.df$est - 1.96*res.df$se)* prop.adjust
  res.df$prop.uci <- transf.ilogit(res.df$est + 1.96*res.df$se)* prop.adjust
  res.df <- cbind(df[1,varToKeep], res.df)
  return(res.df)
}

# Severity----
# genyi_vi <-  function(Each, prefix1 = NULL, prefix2 = NULL,compare_year=NULL,compare_severity=NULL,year=NULL) {  
#   message(paste(Each$SID[1], Each$AGEGR[1]))
#   if(nrow(Each)<2){
#     return(NULL)
#   }else{
#    if(prefix1=="sALRI_RSV_MOV"){ 
#     Each <- Each %>% pivot_wider(names_from = c(Year), values_from = c(sALRI_RSV_MOV,ALRI_RSV_MOV)) 
#     res <- escalc(measure = "OR", 
#                   ai = get(paste(prefix1, year, sep = "_")),ci = get(paste(prefix1, "_2019", sep = "")), 
#                   n1i = get(paste(prefix2, year, sep = "_")), n2i = get(paste(prefix2, "_2019", sep = "")),  
#                   data = Each)
#   }else{
#     Each <- Each %>% pivot_wider(names_from = c(Year), values_from = c(vsALRI_RSV_MOV,ALRI_RSV_MOV))
#     res <- escalc(measure = "OR", 
#                   ai = get(paste(prefix1, year, sep = "_")),ci = get(paste(prefix1, "_2019", sep = "")),
#                   n1i = get(paste(prefix2, year, sep = "_")), n2i = get(paste(prefix2, "_2019", sep = "")), 
#                   data = Each)
#   }
#     res <- res[,-c(4:7)]
#     res <- cbind(res,compare_year,compare_severity)
#   return(res) 
#   }
# }
# 
# genfit_prop <- function(Each,varToKeep=NULL){
#   message(paste(Each$Income[1], Each$AGEGR[1],Each$compare[1]))
#   Each <- Each[!is.na(Each$yi),]
#   if(nrow(Each)<1){
#     return(NULL)
#   } else {
#     fit <- rma(yi, vi, 
#                data = Each[!is.na(Each$yi),], 
#                measure = "OR"
#                )  # Default is "REML". 
#     res <- data.frame(
#       est = as.numeric(fit$b),
#       se = fit$se,
#       n.all = nrow(Each)
#     )
#     res$I2 <- formatC(fit$I2, digits=1, format="f")
#     res <- cbind(Each[1,varToKeep],res)
#     res$OR.est <- exp(res$est)
#     res$OR.lci <- exp(res$est - 1.96*res$se)
#     res$OR.uci <- exp(res$est + 1.96*res$se)
#   }
#   return(res)
# }

genOR_prep <-  function(Each,prefix=NULL) {  
  message(paste(Each$SID[1], Each$AGEGR[1]))
  if(nrow(Each)!=2){
    return(NULL)
  }else{
    if(prefix=="sALRI_RSV_MOV"){  
      Each <- Each[order(Each$Year),]
      res <- Each %>% pivot_wider(names_from = c(Year), values_from = c(sALRI_RSV_MOV,ALRI_RSV_MOV)) 
      res$compare_year <- paste(Each$Year[1],Each$Year[2],sep="vs")
      res$compare_severity <- "sALRI"
      names(res)[4] <- "s.vs_2019"
      names(res)[5] <- "s.vs_202_"
      names(res)[6] <- "all_2019"
      names(res)[7] <- "all_202_"
    }else{
      Each <- Each[order(Each$Year),]
      res <- Each %>% pivot_wider(names_from = c(Year), values_from = c(vsALRI_RSV_MOV,ALRI_RSV_MOV)) # get(prefix1),get(prefix2)
      res$compare_year <- paste(Each$Year[1],Each$Year[2],sep="vs")
      res$compare_severity <- "vsALRI"
      names(res)[4] <- "s.vs_2019"
      names(res)[5] <- "s.vs_202_"
      names(res)[6] <- "all_2019"
      names(res)[7] <- "all_202_"
    }
    return(res) 
  }
}


genfit_prop_glmm <- function(Each,varToKeep=NULL){
  message(paste(Each$Income[1], Each$AGEGR[1],Each$compare_year[1],Each$compare_severity[1]))
  Each <- Each[complete.cases(Each),]
  Each <- Each[Each$all_2019!=0 &  Each$all_202_!=0,]
  if(nrow(Each)<2){
    return(NULL)
  } else {
    fit <- rma.glmm(ai = get("s.vs_202_"), bi=get("s.vs_2019"),ci=get("all_202_")-get("s.vs_202_"),di=get("all_2019")-get("s.vs_2019"),
                    data = Each,
                    measure = "OR")  # Default is "REML". # fix(Each)
    res <- data.frame(
      est = as.numeric(fit$b),
      se = fit$se,
      n.all = nrow(Each)
    )
    res$I2 <- formatC(fit$I2, digits=1, format="f")
    res <- cbind(Each[1,varToKeep],res)
    res$OR.est <- exp(res$est)
    res$OR.lci <- exp(res$est - 1.96*res$se)
    res$OR.uci <- exp(res$est + 1.96*res$se)
  }
  return(res)
}


gen_prop_OR_forfig <- function(dataset,outcome=NULL){ 
   dataset <- dataset[dataset$AGEGR %in% mob_ageGroup_total2,] 
   dataset$AGEGR <- factor(dataset$AGEGR, levels = mob_ageGroup_total2)
dataset$Income <- factor(dataset$Income,levels = c("Lower-middle-income Countries","Upper-middle-income Countries","High-income Countries"))

  if( outcome=="sALRI" ){
res <- ggplot(data = dataset,
       aes(x = compare_year, y = OR.est, colour = Income))+
  geom_hline(yintercept = 1, linetype = "dashed")+
  geom_point(position = position_dodge2(width = 0.5))+
  geom_errorbar(aes(ymin = OR.lci, ymax = OR.uci), width = 0.5, position = position_dodge2(width = 0.5)) +
  geom_text(y = log(min(dataset$OR.lci)/1.2), aes(label = n.all),
            position = position_dodge2(width = 0.5)) +
  scale_y_continuous(name = "OR",trans = "log",
                     breaks = c(0.1,0.3,1,3,9),
                     limits = c(min(dataset$OR.lci,na.rm=T)/1.4,
                                max(dataset$OR.uci,na.rm=T)*1.1))+
  scale_x_discrete(name = NULL,
  labels = c("2019vs2020"="2020", "2019vs2021"="2021", "2019vs2022.3"="Apr 2021 to \nMar 2022")) +
  facet_wrap(~AGEGR, ncol = 2 ) +   
  ggtitle(label="A. Requiring supplemental oxygen")+ 
  scale_colour_lancet()+
  theme_bw()+
  theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5),
        legend.position="bottom",legend.box = "horizontal",
        legend.title = element_text(face = "bold"),
        text = element_text(size = 12))  
  guides(shape = guide_legend(override.aes = list(size = 5)))
res <- ggsave(
  res,
  filename = paste("Figures1/Severity/",outcome,"_prop_OR_ChangeUS.pdf", sep = ""), width = 8, height = 6 ) 
  } else if(outcome=="vsALRI") {
    res <- ggplot(data = dataset,
                  aes(x = compare_year, y = OR.est, colour = Income))+
      geom_hline(yintercept = 1, linetype = "dashed")+
      geom_point(position = position_dodge2(width = 0.5))+
      geom_errorbar(aes(ymin = OR.lci, ymax = OR.uci), width = 0.5, position = position_dodge2(width = 0.5)) +
      geom_text(y = log(min(dataset$OR.lci)/1.2), aes(label = n.all),
                position = position_dodge2(width = 0.5)) +
      scale_y_continuous(name = "OR",trans = "log",
                         breaks = c(0.1,0.3,1,3,9),
                         limits = c(min(dataset$OR.lci,na.rm=T)/1.4,
                                    max(dataset$OR.uci,na.rm=T)*1.1))+
      scale_x_discrete(name = NULL,
                       labels = c("2019vs2020"="2020", "2019vs2021"="2021", "2019vs2022.3"="Apr 2021 to \nMar 2022")) +
      facet_wrap(~AGEGR, ncol = 2 ) +   
      ggtitle(label="B. Requiring mechanical ventilation or ICU admission")+  
      scale_colour_lancet()+
      theme_bw()+
      theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5),
            legend.position="bottom",legend.box = "horizontal",
            legend.title = element_text(face = "bold"),
            text = element_text(size = 12))  
    guides(shape = guide_legend(override.aes = list(size = 5)))
    res <- ggsave(
      res,
      filename = paste("Figures1//Severity/",outcome,"_prop_OR_ChangeUS.pdf", sep = ""), width = 8, height = 6 ) 
   }else{
    res <- ggplot(data = dataset,
                  aes(x = compare_year, y = OR.est, colour = Income))+
      geom_hline(yintercept = 1, linetype = "dashed")+
      geom_point(position = position_dodge2(width = 0.5))+
      geom_errorbar(aes(ymin = OR.lci, ymax = OR.uci), width = 0.5, position = position_dodge2(width = 0.5)) +
      geom_text(y = log(min(dataset$OR.lci)/1.2), aes(label = n.all),
                position = position_dodge2(width = 0.5)) +
      scale_y_continuous(name = "OR",trans = "log",
                         breaks = c(0.1,0.3,1,3,9),
                         limits = c(min(dataset$OR.lci,na.rm=T)/1.4,
                                    max(dataset$OR.uci,na.rm=T)*1.1))+
      scale_x_discrete(name = NULL, 
                       labels = c("2019vs2020"="2020", "2019vs2021"="2021", "2019vs2022.3"="Apr 2021 to \nMar 2022")) +
      facet_wrap(~AGEGR, ncol = 3 ) + 
      scale_colour_lancet()+
      theme_bw()+
      theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5),
            legend.position="bottom",legend.box = "horizontal",
            legend.title = element_text(face = "bold"),
            text = element_text(size = 12))  
    guides(shape = guide_legend(override.aes = list(size = 5)))
    res <- ggsave(
      res,
      filename = paste("Figures1/Severity/",outcome,"_prop_OR_0to24m_ChangeUS.pdf", sep = ""), width = 8, height = 6 ) 
  }
}

# Ageshift----
genyi_vi_ageshift <- function(Each, prefix = NULL, year=NULL) { 
  message(paste(Each$SID[1], Each$AGEGR[1]))
  Rows <- Each %>% group_by(AGEGR) %>% dplyr::summarise(nrow=n())
   if( nrow(Rows[Rows$nrow<2,]) >= 1 | nrow(Each)<4 | sum(Rows$AGEGR %in% c("0-<3m"))==0){   
     return(NULL)
   } else {
  Each <- Each %>% pivot_wider(names_from = c(Year,AGEGR), values_from = c(ALRI_RSV_MOV) ) #revised
      res_ALRI_3to6 <- escalc(measure = "OR",   
                              ai = get(paste(year,"3-<6m", sep = "_")),ci = get(paste("2019", "3-<6m",sep = "_")),  
                              bi = get(paste(year,"0-<3m", sep = "_")), di = get(paste("2019","0-<3m", sep = "_")),  
                              data = Each)
      res_ALRI_3to6 <- res_ALRI_3to6[c( "SID","Income","yi","vi")] 
      aa <- data.frame(compare_year=paste(year,"vs2019",sep=""),compare_severity=prefix,compare_age="3-<6m")
      res_ALRI_3to6 <- cbind(res_ALRI_3to6,aa)
      
      res_ALRI_6to9 <- escalc(measure = "OR",  
                              ai = get(paste(year,"6-<9m", sep = "_")),ci = get(paste("2019", "6-<9m",sep = "_")),  
                              bi = get(paste(year,"0-<3m", sep = "_")), di = get(paste("2019","0-<3m", sep = "_")), 
                              data = Each)
      res_ALRI_6to9 <- res_ALRI_6to9[c( "SID","Income","yi","vi")]
      bb <- data.frame(compare_year=paste(year,"vs2019",sep=""),compare_severity=prefix,compare_age="6-<9m")
      res_ALRI_6to9 <- cbind(res_ALRI_6to9,bb)
      
      res_ALRI_9to12 <- escalc(measure = "OR",  
                               ai = get(paste(year,"9-<12m", sep = "_")),ci = get(paste("2019", "9-<12m",sep = "_")),
                               bi = get(paste(year,"0-<3m", sep = "_")), di = get(paste("2019","0-<3m", sep = "_")), 
                               data = Each)
      res_ALRI_9to12 <- res_ALRI_9to12[c( "SID","Income","yi","vi")]
      cc <- data.frame(compare_year=paste(year,"vs2019",sep=""),compare_severity=prefix,compare_age="9-<12m")
      res_ALRI_9to12 <- cbind(res_ALRI_9to12,cc)
      
      res_ALRI_12to24 <- escalc(measure = "OR",   
                                ai = get(paste(year,"12-<24m", sep = "_")),ci = get(paste("2019", "12-<24m",sep = "_")), 
                                bi = get(paste(year,"0-<3m", sep = "_")), di = get(paste("2019","0-<3m", sep = "_")), 
                                data = Each)
      res_ALRI_12to24 <- res_ALRI_12to24[c( "SID","Income","yi","vi")]
      cc <- data.frame(compare_year=paste(year,"vs2019",sep=""),compare_severity=prefix,compare_age="12-<24m")
      res_ALRI_12to24 <- cbind(res_ALRI_12to24,cc)
      
      res_ALRI_12to18 <- escalc(measure = "OR", 
                                ai = get(paste(year,"12-<18m", sep = "_")),ci = get(paste("2019", "12-<18m",sep = "_")),  
                                bi = get(paste(year,"0-<3m", sep = "_")), di = get(paste("2019","0-<3m", sep = "_")), 
                                data = Each)
      res_ALRI_12to18 <- res_ALRI_12to18[c( "SID","Income","yi","vi")]
      cc <- data.frame(compare_year=paste(year,"vs2019",sep=""),compare_severity=prefix,compare_age="12-<18m")
      res_ALRI_12to18 <- cbind(res_ALRI_12to18,cc)
      
      res_ALRI_18to24 <- escalc(measure = "OR",  
                                ai = get(paste(year,"18-<24m", sep = "_")),ci = get(paste("2019", "18-<24m",sep = "_")), 
                                bi = get(paste(year,"0-<3m", sep = "_")), di = get(paste("2019","0-<3m", sep = "_")),  
                                data = Each)
      res_ALRI_18to24 <- res_ALRI_18to24[c( "SID","Income","yi","vi")]
      cc <- data.frame(compare_year=paste(year,"vs2019",sep=""),compare_severity=prefix,compare_age="18-<24m")
      res_ALRI_18to24 <- cbind(res_ALRI_18to24,cc)
      
      res_all <- rbind(res_ALRI_3to6,res_ALRI_6to9,res_ALRI_9to12,res_ALRI_12to24,res_ALRI_12to18,res_ALRI_18to24)
      return(res_all)
   }
}  
    
  
genfit_ageshift <-  function(Each,varToKeep=NULL){  #no revised
  message(paste(Each$Income[1], Each$AGEGR[1],Each$compare[1]))
  Each <- Each[!is.na(Each$yi),]
  if(nrow(Each)<1){
    return(NULL)
  } else {
   fit <- rma(yi, vi, 
              data = Each[!is.na(Each$yi),], 
              measure = "OR",
              method="FE")  # Default is "REML". 
    res <- data.frame(
      est = as.numeric(fit$b),
      se = fit$se,
      n.all = nrow(Each)
    )
    res$I2 <- formatC(fit$I2, digits=1, format="f")
    res <- cbind(Each[1,varToKeep],res)
    res$OR.est <- exp(res$est)
    res$OR.lci <- exp(res$est - 1.96*res$se)
    res$OR.uci <- exp(res$est + 1.96*res$se)
  }
  return(res)
}
 
    
gen_ageshift_OR_forfig <- function(dataset,choice,outcome=NULL){
  if(choice=="main"){
     dataset$compare_age <- factor(dataset$compare_age, levels = c("3-<6m","6-<9m","9-<12m","12-<24m"))
    dataset$Income <- factor(dataset$Income,levels = c("Lower-middle-income Countries","Upper-middle-income Countries","High-income Countries"))
    
    res <- ggplot(data = dataset,
                  aes(x = compare_year, y = OR.est, colour = Income))+
      geom_hline(yintercept = 1, linetype = "dashed")+
      geom_point(position = position_dodge2(width = 0.5)) +
      geom_errorbar(aes(ymin = OR.lci, ymax = OR.uci), width = 0.5, position = position_dodge2(width = 0.5)) +
      geom_text(y = log(min(dataset$OR.lci)/1.2), aes(label = n.all),
                position = position_dodge2(width = 0.5)) +
      scale_y_continuous(name = "OR",trans = "log",
                         breaks = c(0.13,0.25,0.5,1,2,4,8),
                         limits = c(min(dataset$OR.lci,na.rm=T)/1.3,
                                    max(dataset$OR.uci,na.rm=T)*1.3))+
      scale_x_discrete(name = NULL,
       labels = c("2020vs2019"="2020", "2021vs2019"="2021", "2022.3vs2019"="Apr 2021 to \nMar 2022")) +
      facet_wrap(~compare_age, ncol = 2 ) + 
      ggtitle(label="A. RSV-associated ALRI hospitalisations")+
      # ggtitle(label=paste("B. RSV-associated ALRI hospitalisations requiring mechanical ventilation or ICU admission"))+  # Age distribution of RSV-associated
      # ggtitle(label=paste("B. RSV-associated ALRI hospitalisations requiring supplemental oxygen"))+
      scale_colour_lancet()+
      theme_bw()+
      theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5),
            legend.position="bottom",legend.box = "horizontal",
            legend.title = element_text(face = "bold"),
            text = element_text(size = 12))  
    guides(shape = guide_legend(override.aes = list(size = 5)))
    res <- ggsave(
      res,
      filename = paste("Figures1/Ageshift/",outcome,"_ageshift_OR.pdf", sep = ""), width = 10, height = 6 ) 
  }
  if(choice=="others"){
    dataset$compare_age <- factor(dataset$compare_age, levels = c("12-<18m","18-<24m"))
    dataset$Income <- factor(dataset$Income,levels = c("Lower-middle-income Countries","Upper-middle-income Countries","High-income Countries"))
    
    res <- ggplot(data = dataset,
                  aes(x = compare_year, y = OR.est, colour = Income))+
      geom_hline(yintercept = 1, linetype = "dashed")+
      geom_point(position = position_dodge2(width = 0.5)) +
      geom_errorbar(aes(ymin = OR.lci, ymax = OR.uci), width = 0.5, position = position_dodge2(width = 0.5)) +
      geom_text(y = log(min(dataset$OR.lci)/1.2), aes(label = n.all),
                position = position_dodge2(width = 0.5)) +
      scale_y_continuous(name = "OR",trans = "log",
                         breaks = c(0.25,0.5,1,2,4,8,16),
                         limits = c(min(dataset$OR.lci,na.rm=T)/1.3,
                                    max(dataset$OR.uci,na.rm=T)*1.3))+
      scale_x_discrete(name = NULL,
                       labels = c("2020vs2019"="2020", "2021vs2019"="2021", "2022.3vs2019"="Apr 2021 to \nMar 2022")) +
      facet_wrap(~compare_age, ncol = 2 ) + 
      ggtitle(label="A. RSV-associated ALRI hospitalisations")+
      # ggtitle(label=paste("B. RSV-associated ALRI hospitalisations requiring mechanical ventilation or ICU admission"))+  # Age distribution of RSV-associated
      # ggtitle(label=paste("B. RSV-associated ALRI hospitalisations requiring supplemental oxygen"))+
      scale_colour_lancet()+
      theme_bw()+
      theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5),
            legend.position="bottom",legend.box = "horizontal",
            legend.title = element_text(face = "bold"),
            text = element_text(size = 12))  
    guides(shape = guide_legend(override.aes = list(size = 5)))
    res <- ggsave(
      res,
      filename = paste("Figures1/Ageshift/",outcome,"_ageshift_OR.1224.pdf", sep = ""), width = 10, height = 6 ) 
  } 
}  
  
  
# others----
#appendix: monthly hospitalisations----
geneachGR <- function(eachGR){
  res <-  eachGR %>% arrange(start_dat) %>% mutate(ALRI_RSV_rollmean = rollmean(ALRI_RSV, k =3, fill = NA),
                                                   Deno_RSV_rollmean = rollmean(DenoRaw, k =3, fill = NA)) 
  return(res)
}


genMonthlyPositiveNum_forfig <- function(Each){
  message(Each$SID[1],Each$Country[1])  
  Each$AGEGR <- factor(Each$AGEGR, levels = c("0-<3m","3-<6m","6-<9m", "9-<12m", "12-<24m","24-<60m","0-<60m")) 
  Each$site <- paste(Each$Country,Each$Location01,sep=", ")
  res <-   ggplot(data = Each, aes(x = as.Date(end_dat))) + 
    geom_line(aes(y = MonthlyPositiveNum_Mov,colour = AGEGR)) +
    geom_tile(aes(y = -1*max(Each$MonthlyPositiveNum_Mov,na.rm = T)/18, 
                  fill = GoogleMob_rollmean),color="gray50",height=max(Each$MonthlyPositiveNum_Mov,na.rm = T)/25) +   
    scale_y_continuous(name = NULL,limits = c(-1*max(Each$MonthlyPositiveNum_Mov,na.rm = T)/9,max(Each$MonthlyPositiveNum_Mov,na.rm = T)+1)) + 
    scale_x_date(
      name = NULL,
      date_breaks = "3 month",  date_labels =  "%b \n%Y",limits =c(as.Date("2019-01-01"), as.Date("2022-05-01")),
      expand = expansion(mult = c(0.05, 0.05))) +
    scale_fill_gradient2(name = "Retail & recreation change",low = "deepskyblue3", mid = "white", high = "red3")  +
    scale_colour_lancet() +
    ggtitle(Each$site[1])+
    theme_bw()  +
    theme(axis.text.x = element_text(vjust = 0.5, hjust=0.5))+ 
    theme(plot.title.position = "plot",
          legend.title = element_text(face = "bold"),
          legend.position="bottom",legend.box = "horizontal")+
    guides(shape = guide_legend(override.aes = list(size = 5)))
  ggsave(
    res,
    filename = paste("Figures1/MonthlyPositiveNum/", Each$SID[1], Each$site[1],".tiff", sep = " "), width = 10, height = 6 )
  return(res)
}  

gen_Country_MonthlyGoogMov <- function(Each){
  message(Each$location[1])
  res <- Each %>% arrange(year_month) %>% mutate(GoogleMob_rollmean = rollmean(mean_recreation, k =3, fill = NA)) 
  return(res)
}

# appendix: association of 12-month rate and MPIs/Mobility data----
get_impute <- function(Each) {  
  df <- data.frame(location=Each$location[1],year_month=as.yearmon(head(as.vector(period_range),14)),mean_SI=0)
  return(df)
}

getLoc_MOV <- function(eachSIDAGEGR) {
  res <- apply(period_matrix, 1, FUN = getdata_cond, dataset = eachSIDAGEGR)  

  res <- do.call(rbind, 
                 as.list(res))
  return(res)
}

getdata_cond <- function(period_12, dataset = eachSIDAGEGR) {  
  if( nrow( dataset[dataset$year_month %in% period_12,] )<12 ){
    return(NULL)
  }else{
    df <- dataset[dataset$year_month %in% period_12,] %>% 
      dplyr::summarise(location=dataset$location[1], AGEGR=dataset$AGEGR[1],
                       end_dat=tail(year_month,1),mean_12SI=mean(mean_SI))
    return(df)
  }
} 

get_impute1 <- function(Each) {  
  df <- data.frame(location=Each$location[1],year_month=as.yearmon(head(as.vector(period_range),13)),mean_recreation=0) 
  return(df)
}


getLoc_MOV1 <- function(eachSIDAGEGR) {
  res <- apply(period_matrix, 1, FUN = getdata_cond1, dataset = eachSIDAGEGR)  
  res <- do.call(rbind, 
                 as.list(res))
  return(res)
}

getdata_cond1 <- function(period_12, dataset = eachSIDAGEGR) {  
  if( nrow( dataset[dataset$year_month %in% period_12,] )<12 ){
    return(NULL)
  }else{
    df <- dataset[dataset$year_month %in% period_12,] %>% 
      dplyr::summarise(location=dataset$location[1], AGEGR=dataset$AGEGR[1],
                       end_dat=tail(year_month,1),mean_12recreation=mean(mean_recreation))  
    return(df)
  }
} 

get_crosscor <- function(Each) {
  df <- ccf(x=Each$mean_12SI, y=Each$rate_12m_moving, plot = F,lag.max = 11)
  res <- data.frame(SID=Each$SID[1],Country=Each$Country[1],Income=Each$Income[1],AGEGR=Each$AGEGR[1],
                    lag = df$lag, max_correlation = df$acf)  
  res <- subset(res,res$lag>=0 & res$max_correlation>0)  
  res <- res[which.max(abs(res$max_correlation)),]  
  return(res)
}

get_crosscor1 <- function(Each) {
  df <- ccf(x=Each$mean_12recreation, y=Each$rate_12m_moving, plot = F,lag.max = 11)
  res <- data.frame(SID=Each$SID[1],Country=Each$Country[1],Income=Each$Income[1],AGEGR=Each$AGEGR[1],
                    lag = df$lag, max_correlation = df$acf) 
  res <- subset(res,res$lag>=0 & res$max_correlation>0)   
  res <- res[which.max(abs(res$max_correlation)),] 
  return(res)
}


genGoogleMob_rates_bycountry <- function(Each,index=NULL) {
  if(index=="US"){
    temp <-  ggplot(data = Each, aes(x =  as.Date(end_dat))) + 
      geom_line(aes(y = rate_12m_moving,group=SID),colour = "blue4", show.legend = FALSE) +
      scale_y_continuous(name = NULL, limits = c(-15,250)  ) +
      geom_tile(aes(y = -8, height=4,
                    fill = mean_12recreation),color="gray50")
  }else{
    temp <-  ggplot(data = Each, aes(x =  as.Date(end_dat))) + 
      geom_line(aes(y = rate_12m_moving,group=SID),colour = "blue4", show.legend = FALSE) +
      scale_y_continuous(name = NULL, limits = c(-5,60)  ) +
      geom_tile(aes(y = -3,
                    fill = mean_12recreation),color="gray50") 
  }
  temp <- temp + 
    scale_fill_gradient2(name = "Retail & recreation change",low = "deepskyblue3", mid = "white", high = "red3")  +
    scale_x_date(
      name = "Date (month ending)",
      date_breaks = "3 month",  date_labels =  "%b \n%Y",limits =NULL,
      expand = expansion(mult = c(0.05, 0.05))) +
    theme_bw() +
    ggtitle(Each$Country[1])+
    theme(plot.title.position = "plot",
          legend.position=c(.8,.8),     
          legend.title = element_text(face = "bold")) +
    guides(shape = guide_legend(override.aes = list(size = 5)))
  ggsave(
    temp,
    filename = paste("Figures1/Country/", Each$Country[1], ".tiff", sep = ""), width = 8, height = 6
  )
  return(temp)
} 
