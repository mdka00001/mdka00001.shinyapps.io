#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

require(shiny)
require(data.table)
require(mixtox)
require(ggplot2)
require(gridExtra)
require(shinyscreenshot)
require(bayestestR)
require(BAS)
require(markdown)
require(knitr)
require(readr)
require(stringr)

extract_data <- function(data) {
  df = readLines(data)
  compound_name <- str_split(grep("^compound_name: (.+)", df, value = TRUE)," ", simplify=TRUE)[,2]
  mw <- str_split(grep("^mw: (.+)", df, value = TRUE)," ", simplify=TRUE)[,2]
  
  all_text <- paste(df, collapse = "\n")
  pattern <- "-(\\r?\\n([\\s\\S]*?)\\r?\\n)-"
  
  
  matches <- regmatches(all_text, gregexpr(pattern, all_text, perl = TRUE))
  
  #print(matches)
  extracted_lines <- lapply(matches, function(x) trimws(gsub("(?<=\\d)-", "", x, perl=TRUE)))
  #print(extracted_lines)
  
  csv_files <- list()
  
  for (i in extracted_lines[[1]]){
    lines <- unlist(strsplit(i, "\n"))
    
    
    lines <- lines[lines != "-" & lines != ""]
    
    
    header_line <- lines[1]
    data_lines <- lines[-1]
    csv_text <- paste(header_line, paste(data_lines, collapse = "\n"), sep = "\n")
    
    df_s = data.table::fread(csv_text)
    
    
    df_s = data.frame(df_s)
    rownames(df_s) <- c(1:nrow(df_s))
    
    
    csv_files = c(csv_files, list(df_s))
    
  }
  
  output_list <- list(output1 = csv_files, output2 = compound_name, output3 = mw)
  return(output_list)
}  


makePlot = function(compound_input, func, EC, ml){
  input_list = extract_data(compound_input)
  Alpha = NULL
  Beta = NULL
  Gamma = NULL
  Compound = NULL
  model = NULL
  
  for(i in 1:length(input_list$output1)){
    #print(i)
    if(!is.null(input_list$output1[[i]])==TRUE){
      mat = input_list$output1[[i]]
      #print(mat)
      conc = NULL
      resp = NULL
      tune_fit = NULL
      if (func == "Logit" | func == "Weibull"){
        conc = append(conc, mat[,1]/(1e+6))
      }
      else{
        conc = append(conc, mat[,1])
        
      }
      
      
      resp = append(resp, mat[,2:ncol(mat)])
      #print(resp)
      tune_fit = tuneFit(conc, rowMeans(as.data.frame(resp)), func)
      res_curve = curveFit(conc, rowMeans(as.data.frame(resp)), func, c(tune_fit$sta["fit_1","Alpha"], tune_fit$sta["fit_1","Beta"]))
      
      Alpha = append(Alpha, tune_fit$sta["fit_1", "Alpha"])
      Beta = append(Beta, tune_fit$sta["fit_1", "Beta"])
      Gamma = append(Gamma, 0)
      Compound = append(Compound, gsub(".csv","",input_list[[2]][i]))
      model = append(model, func)
    }
  }
  
  param = data.frame(alpha=Alpha, beta=Beta, gamma=Gamma, compound=Compound)
  rownames(param) = param$compound
  param = param[,-4]
  
  iaA = iaPred(model, t(t(param)), mixType = "eecr", effv=EC)
  caA = caPred(model, t(t(param)), mixType = "eecr", effv=EC)
  
  
  
  
  EC_list <- list(3,5,10,15,20,25,30,35,40,45,50,52,55,60,65,70,75,80,85,90,95)
  EC_list <- lapply(EC_list, function(x) x / 100)
  
  ca_val <- NULL
  for (i in EC_list){
    caA_tmp <- caPred(model, t(t(param)), mixType = "eecr", i)
    df_new_ca <- caA_tmp$pct/t(ECx(model, t(t(param)),i))
    ca_val <- c(ca_val,rowSums(df_new_ca)**(-1))
  }
  
  ca_val <- data.frame(ca_val)
  
  new_col_name <- paste0("Concentration (µmol)")
  colnames(ca_val) <- c(new_col_name)
  
  
  
  ecs <- NULL
  MW = strsplit(input_list[[3]], split=",")
  
  for(i in MW){
    ecs <- (((ECx(model, t(t(param)),effv=as.numeric(EC[[1]][1])))*as.numeric(i))*1e+6)/(10**3)
  }
  ecs_mg = (ecs*as.numeric(ml[[1]][1]))/10**3
  ecs_wf = t(t(ecs)/(colSums(ecs)))
  
  
  ca_ia_results = data.frame(inhibition_percentage=iaA$e*100, ca_pred = log10(caA$ca)[1,], ia_pred = log10(iaA$ia)[1,])
  
  ca_ia_results_ml = data.frame(inhibition_percentage=iaA$e*100, ca_pred = (caA$ca*(1e+6))[1,], ia_pred = (iaA$ia*(1e+6))[1,])
  
  rownames(ca_ia_results) <- c(1:nrow(ca_ia_results))
  rownames(ca_ia_results_ml) <- c(1:nrow(ca_ia_results_ml))
  
  # par(mfrow = c(2, 1))
  # plot(log10(iaA$ia), iaA$e*100, col="red", lwd=2.5, lty=2,xlab="log(c) mol/L", ylab ="Inhibition %")
  # lines(log10(iaA$ia), iaA$e*100, col="red", lwd=2.5, lty=2)
  # #legend("topleft", legend=paste0("EC",EC*100), cex=1.4, bty="n")
  # title(main="Independent Action (IA)")
  # 
  # plot(log10(caA$ca), caA$e*100, col="blue", lwd=2.5, lty=2,xlab="log(c) mol/L", ylab ="Inhibition %")
  # lines(log10(caA$ca), caA$e*100, col="blue", lwd=2.5, lty=2)
  # #legend("topleft", legend=paste0("EC",EC*100), cex=1.4, bty="n")
  # title(main="Concentration addition (CA)")
  
  plt <- ggplot(ca_ia_results, aes(y = inhibition_percentage)) + 
    geom_point(aes(x = ca_pred), color = "blue", size = 3, shape = 16) +
    geom_point(aes(x = ia_pred), color = "red", size = 3, shape = 16) +
    geom_smooth(aes(x = ca_pred, y = inhibition_percentage), color = "blue", method = "loess", se = FALSE) + 
    geom_smooth(aes(x = ia_pred, y = inhibition_percentage), color = "red", method = "loess", se = FALSE) +  
    labs(x = "Value", y = "Inhibition (%)") +
    scale_shape_manual(values = c(16, 17, 18)) +
    ggtitle("Prediction of Inhibition") +
    theme_minimal() +
    theme(
      panel.grid.major = element_blank(),  
      panel.grid.minor = element_blank(), 
      panel.border = element_rect(color = "black", fill = NA, size = 1) 
    )
  
  res = list("plot" = plt,"mg"=as.data.frame(ecs_mg), "wf"=as.data.frame(ecs_wf), "conc" = ca_val*(1e+6), 
             "table" = ca_ia_results, "table_ml" = as.data.frame(ca_ia_results_ml), "iaa" = iaA, "caa" = caA)
  
  # par(mfrow = c(2, 1))
  # plot(iaA$ia*(1e+6), iaA$e*100, col="red", lwd=2.5, lty=2,xlab="log(c) mol/L", ylab ="Inhibition %")
  # lines(iaA$ia*(1e+6), iaA$e*100, col="red", lwd=2.5, lty=2)
  # legend("topleft", legend=paste0("EC",EC*100), cex=1.4, bty="n")
  # title(main="Concentration addition (CA)")
  # 
  # plot(caA$ca*(1e+6), caA$e*100, col="blue", lwd=2.5, lty=2,xlab="log(c) mol/L", ylab ="Inhibition %")
  # lines(caA$ca*(1e+6), caA$e*100, col="blue", lwd=2.5, lty=2)
  # legend("topleft", legend=paste0("EC",EC*100), cex=1.4, bty="n")
  # title(main="Indepndent action (IA)")
  
  return(res)
}

makePlot_miture = function(mixture, func, iaA, caA, EC){
  conc = NULL
  resp = NULL
  mat = read.csv(mixture, check.names=FALSE)
  conc = append(conc, mat[,1]/(1e+6))
  resp = append(resp, mat[,2:ncol(mat)])
  tune_fit = NULL
  tune_fit = tuneFit(conc, rowMeans(as.data.frame(resp)), func)
  res_curve = curveFit(conc, rowMeans(as.data.frame(resp)), func, c(tune_fit$sta["fit_1","Alpha"], tune_fit$sta["fit_1","Beta"]))
  plot(log10(conc), rowMeans(as.data.frame(resp))*100, pch = 20, ylim =c(-10,110),
       xlab="log(c) mol/L", ylab ="Inhibition %", cex=1.8, cex.lab=1.8,
       cex.axis=1.8)
  lines(log10(conc), res_curve$crcInfo[,2]*100, col=1, lwd=2)
  lines(log10(conc), res_curve$crcInfo[,6]*100, col="green", lwd=1.5, lty =3)
  lines(log10(conc), res_curve$crcInfo[,7]*100, col="green", lwd=1.5, lty =3)
  lines(log10(iaA$ia), iaA$e*100, col="red", lwd=2.5, lty=2)
  lines(log10(caA$ca), caA$e*100, col="blue", lwd=2.5, lty=2)
  legend("topleft", legend=paste0("EC",EC*100), cex=1.4, bty="n")
  title(main="Mixture curve")
  
}

makePlot2 = function(file){
  input = read.csv(file, check.names=FALSE)
  c_list = names(input)
  for(i in 1:(ncol(input)-1)){
    for(j in (i+1):ncol(input)){
      model = lm(get(c_list[j])~get(c_list[i]), data=input)
      alpha = model$coefficients[1]
      beta = model$coefficients[2]
      new_x = seq(min(input[[i]]),max(input[[i]]), length.out=100)
      y_hat = alpha + beta * new_x
      ymean = data.frame(predict(model, newdata=data.frame(assign(c_list[i], new_x)), interval="confidence", level=0.95))
      ypred = data.frame(predict(model, newdata=data.frame(assign(c_list[i], new_x)), interval="prediction", level=0.95))
      output = data.frame(x=new_x, y_hat=y_hat, ymean_lwr=ymean$lwr, ymean_upr=ymean$upr, ypred_lwr=ypred$lwr, ypred_upr=ypred$upr)
      res = ggplot(data=input, aes(x=get(c_list[i]), y=get(c_list[j]))) + geom_point(color="blue") + 
        geom_line(data=output, aes(x=new_x, y=y_hat, color="first"), lty=1) +
        geom_line(data=output, aes(x=new_x, y=ymean_lwr, lty="second")) +
        geom_line(data=output, aes(x=new_x, y=ymean_upr, lty="second")) +
        geom_line(data=output, aes(x=new_x, y=ypred_upr, lty="third")) +
        geom_line(data=output, aes(x=new_x, y=ypred_lwr, lty="third")) + 
        scale_colour_manual(values=c("orange"), labels="Posterior mean", name="") + 
        scale_linetype_manual(values=c(2, 3), labels=c("95% CI for mean", "95% CI for predictions"), name="") + 
        theme_bw() + 
        theme(legend.position=c(0.01,0.99), legend.justification=c(0.01,0.99)) +
        labs(x=c_list[i], y=c_list[j], caption=paste0("P-value: ",round(summary(model)$coefficients[,4][[2]],10),", R2: ",round(summary(model)$adj.r.squared,3),", Probability: ",round(mean(y_hat),2),"%"))
      print(res)
      model = lm(get(c_list[i])~get(c_list[j]), data=input)
      alpha = model$coefficients[1]
      beta = model$coefficients[2]
      new_x = seq(min(input[[j]]),max(input[[j]]), length.out=100)
      y_hat = alpha + beta * new_x
      ymean = data.frame(predict(model, newdata=data.frame(assign(c_list[j], new_x)), interval="confidence", level=0.95))
      ypred = data.frame(predict(model, newdata=data.frame(assign(c_list[j], new_x)), interval="prediction", level=0.95))
      output = data.frame(x=new_x, y_hat=y_hat, ymean_lwr=ymean$lwr, ymean_upr=ymean$upr, ypred_lwr=ypred$lwr, ypred_upr=ypred$upr)
      res = ggplot(data=input, aes(x=get(c_list[j]), y=get(c_list[i]))) + geom_point(color="blue") + 
        geom_line(data=output, aes(x=new_x, y=y_hat, color="first"), lty=1) +
        geom_line(data=output, aes(x=new_x, y=ymean_lwr, lty="second")) +
        geom_line(data=output, aes(x=new_x, y=ymean_upr, lty="second")) +
        geom_line(data=output, aes(x=new_x, y=ypred_upr, lty="third")) +
        geom_line(data=output, aes(x=new_x, y=ypred_lwr, lty="third")) + 
        scale_colour_manual(values=c("orange"), labels="Posterior mean", name="") + 
        scale_linetype_manual(values=c(2, 3), labels=c("95% CI for mean", "95% CI for predictions"), name="") + 
        theme_bw() + 
        theme(legend.position=c(0.01,0.99), legend.justification=c(0.01,0.99)) +
        labs(x=c_list[j], y=c_list[i], caption=paste0("P-value: ",round(summary(model)$coefficients[,4][[2]],10),", R2: ",round(summary(model)$adj.r.squared,3),", Probability: ",round(mean(y_hat),2),"%"))
      print(res)
    }
  }
}

shinyServer(function(input, output){

  

  # Enable downloading of the pre-uploaded file
  output$downloadText <- downloadHandler(
    filename = "sample_input.txt" , 
    content = function(file_) {
      file.copy("sample_input.txt", file_)  # Copy the file to the temporary download location
    }
  )
  
  iaa_input = eventReactive(input$action_button,{
    makePlot(input$input1$datapath, input$func, input$EC, input$ml)$iaa
  })
  
  caa_input = eventReactive(input$action_button,{
    makePlot(input$input1$datapath, input$func, input$EC, input$ml)$caa
  })
  
  observeEvent(input$action_button,{
  output$plot = renderPlot({
    makePlot(input$input1$datapath, input$func, input$EC, input$ml)$plot
  })
  output$auth_output <- renderPrint({
    h4(paste0("Legend:","\n",
                 "Blue: Concentration Addition (CA) & Red: Independent Action (IA)"))})
  output$table1 = renderTable({
    makePlot(input$input1$datapath, input$func, input$EC, input$ml)$mg
  },digits=5,rownames=TRUE)
  
  output$table2 = renderTable({
    makePlot(input$input1$datapath, input$func, input$EC, input$ml)$wf
  },digits=5,rownames=TRUE)
  output$table3 = renderTable({
    makePlot(input$input1$datapath, input$func, input$EC, input$ml)$table_ml
  },digits=2,rownames=TRUE)
  output$out_vol <- renderUI({
    paste0("mg of each compound in mixture (mg/",input$ml, "ml)")
  })
  output$out_vol_2 <- renderUI({
    paste0("Weight fraction of each compound in the mixture")
  })
  output$out_vol_3 <- renderUI({
    paste0("Total concentration of mixture predicted by IA and CA model")
  })
  
  }
  
  
  
  )
  
  observeEvent(input$action_button_mixture,{
    output$plot_mixture = renderPlot({
      makePlot_miture(input$input2$datapath, input$func, iaa_input(), caa_input(), input$EC)
    })
    
    output$auth_output_mixture <- renderPrint({
      h5(paste0("Legend:","\n",
                "Experimental, predicted Concentration addition (CA), and Independent action (IA) concentration response curves of mixtures","\n",
                "designed by equal effect concentration ratio. Dot: observed; Black line: fitted CRC, Grey dotted lines: 95 percent PI;","\n",
                "Blue dashed line: IA prediction; Red dashed line: CA.\n"))})
  })
  
  output$download_button <- downloadHandler(
    filename = "results.zip",
    content = function(file) {
      write.table(makePlot(input$input1$datapath, input$func, input$EC, input$ml)$table_ml, 
                  file="ca_ia_pred.csv", sep=",")
      write.table(makePlot(input$input1$datapath, input$func, input$EC, input$ml)$table, 
                  file="ca_ia_pred_log.csv", sep=",")
      write.table(makePlot(input$input1$datapath, input$func, input$EC, input$ml)$mg, 
                  file="EC_per_mg.txt")
      write.table(makePlot(input$input1$datapath, input$func, input$EC, input$ml)$wf, 
                  file="weight_fraction.txt")
      
      png(filename="plot.png")
      plot(makePlot(input$input1$datapath, input$func, input$EC, input$ml)$plot)
      dev.off()
      zip(zipfile = "results", files = c("ca_ia_pred.csv", "ca_ia_pred_log.csv", "EC_per_mg.txt", "weight_fraction.txt", "plot.png"))
      file.copy("results.zip", file)
      # write.table(paste(text,collapse=", "), file,col.names=FALSE)
    }
  )
  
  observeEvent(input$pdf, {
    screenshot(filename=="result")
    
  })
  
  output$bay_pdf = downloadHandler(
    filename = function() {"result.pdf"},
    content = function(file) {
      pdf(file, onefile=TRUE)
      makePlot2(input$bay_input1$datapath)
      
      
      dev.off()
    }
  )
})
