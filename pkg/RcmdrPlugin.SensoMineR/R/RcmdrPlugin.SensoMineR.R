#! Ensemble des interfaces graphiques associees au package SensoMineR

################################################################################
################################# FONCTION DEFMACRO ############################
#           utilisee indirectement pour la construction des GUI                #
################################################################################
# the Rcmdr if it is not already loaded
.packageName <- c("RcmdrPlugin.SensoMineR")

.First.lib <- function(libname, pkgname){
    if (!interactive()) return()
    Rcmdr <- options()$Rcmdr
    plugins <- Rcmdr$plugins
    if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
        Rcmdr$plugins <- c(plugins, pkgname)
        options(Rcmdr=Rcmdr)
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
        }
    }
    
#! Ensemble des interfaces graphiques associees au package SensoMineR

################################################################################
################################# FONCTION DEFMACRO ############################
#           utilisée indirectement pour la construction des GUI                #
################################################################################
  defmacro <- function(..., expr){
    expr <- substitute(expr)
    len <- length(expr)
    expr[3:(len+1)] <- expr[2:len]
    ## delete "macro" variables starting in ..
    expr[[2]] <- quote(on.exit(remove(list=objects(pattern="^\\.\\.", all.names=TRUE))))
    a <- substitute(list(...))[-1]
    ## process the argument list
    nn <- names(a)
    if (is.null(nn)) nn <- rep("", length(a))
    for (i in seq(length=length(a))){
        if (nn[i] == "") {
            nn[i] <- paste(a[[i]])
            msg <- paste(a[[i]], gettext("not supplied", domain="R-Rcmdr"))
            a[[i]] <- substitute(stop(foo), list(foo = msg))
            }
        }
    names(a) <- nn
    a <- as.list(a)
    ff <- eval(substitute(
        function(){
            tmp <- substitute(body)
            eval(tmp, parent.frame())
            },
        list(body = expr)))
    ## add the argument list
    formals(ff) <- a
    ## create a fake source attribute
    mm <- match.call()
    mm$expr <- NULL
    mm[[1]] <- as.name("macro")
    expr[[2]] <- NULL # get "local" variable removal out of source
    attr(ff, "source") <- c(deparse(mm), deparse(expr))
    ## return the macro
    ff
    }

#############################FIN FONCTION DEFMACRO #############################


##########################################################################################################################################################
######################################################            FONCTION BOXPROD          ##############################################################

SensBoxprod<-function(){
initializeDialog(title=gettextRcmdr("Boxprod"))
require(SensoMineR)
donnee<-get(.activeDataSet)
nomdonnee<-.activeDataSet


######## FRAMES --------------------------------------------------------------------------------------------------------------------
titleFrame <- tkframe(top)
    tkgrid(tklabel(titleFrame,text=paste("Variables                                          "),fg="blue"),tklabel(titleFrame,text="                       Variable of interest",fg="blue"),columnspan=2,sticky="nw")

listFrame <- tkframe(top,borderwidth=2)                                                                                                          
    listdesc<-tklistbox(listFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(listFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
    tkselection.set(listdesc,0)
    listfact<-tklistbox(listFrame,selectmode="single",yscrollcommand=function(...) tkset(scrfact,...))
    scrfact <- tkscrollbar(listFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
    vars<-colnames(donnee)
    vars.fact = NULL
    vars.desc = NULL
    for (i in (1:ncol(donnee))){
      if (is.numeric(donnee[,i])){
        tkinsert(listdesc,"end",vars[i])
        vars.desc = c(vars.desc,vars[i])
      }
      else {
        vars.fact = c(vars.fact,vars[i])
        tkinsert(listfact,"end",vars[i])
      }
    }

    tkgrid(listdesc, scr,tklabel(listFrame,text="                                       "),listfact,scrfact,sticky = "nw")
    tkgrid.configure(scr, sticky = "wns")
    tkgrid.configure(listdesc,sticky = "ew")
    tkgrid.configure(scrfact,sticky = "wns")
    tkgrid.configure(listfact,sticky = "ew")

optionsFrame <- tkframe(top,borderwidth=3,relief="ridge")
    numr.val<-tclVar("2")
    numr.lab<-tklabel(optionsFrame,text="Number of boxplots per row : ")
    numr.ent<-tkentry(optionsFrame,width=4,textvariable=numr.val)
    numc.val<-tclVar("2")
    numc.lab<-tklabel(optionsFrame,text="Number of boxplots per column : ")
    numc.ent<-tkentry(optionsFrame,width=4,textvariable=numc.val)
    tkgrid(numr.lab,numr.ent,sticky="w")
    tkgrid(numc.lab,numc.ent,sticky="w")

######## Fonction liée au bouton OK -----------------------------------------------------------------------------------------------------

App<-function(){
    variable <- vars.desc[as.numeric(tkcurselection(listdesc))+1]
    col.val <- as.numeric(tkcurselection(listfact))+1
    nbitemdesc<-c(tclvalue(tkcurselection(listdesc)))
    nbitemdesc<-unlist(strsplit(nbitemdesc,"\\ "))
    nbitemfact<-c(tclvalue(tkcurselection(listfact)))
    nbitemfact<-unlist(strsplit(nbitemfact,"\\ "))
    if (length(nbitemdesc)==0) tkmessageBox(message="You must select al least 1 descriptor",icon="warning",type="ok")
    if (length(nbitemfact)==0) tkmessageBox(message="You must select one factor",icon="warning",type="ok")
    if ((length(nbitemfact)!=0)&(length(nbitemdesc)!=0)) {
      command3=paste('boxprod(',nomdonnee,'[,c("', paste(vars.fact, collapse='", "'), '", "', paste(variable, collapse='", "'), '")],firstvar=',length(vars.fact)+1,',col.p=',col.val,',numr=',as.numeric(tclvalue(numr.val)),',numc=',as.numeric(tclvalue(numc.val)),')',sep='')
      doItAndPrint(command3)
    }
}

onOK<-function(){
    App()        
    tkdestroy(top)
    top<-NULL
}

######## Positionnement des widgets sur la fenêtre principale ----------------------------------------------------------------------------
App.but<-tkbutton(top,text="Submit",command=App,fg="blue",width=13,borderwidth=3)
OKCancelHelp(helpSubject="boxprod")
tkgrid(titleFrame)
tkgrid(listFrame,sticky="w",columnspan=3)
tkgrid(tklabel(top,text=""))
tkgrid(optionsFrame)
tkgrid(tklabel(top,text=""))
tkgrid(App.but,sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/boxprod.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
dialogSuffix(rows=5, columns=1)
top<-NULL
.activeDataSet<-NULL
buttonsFrame<-NULL
}

#########################################################           FIN FONCTION BOXPROD         ######################################################
#######################################################################################################################################################


############################# histprod

Senshist<-function(){
require(tcltk)        ### Appel aux packages qui vont être utilisés
require(SensoMineR)
top<-tktoplevel(borderwidth=10)     ### Création de la fenêtre principale
tkwm.title(top,"Histprod")        ### Donne un titre à la fenêtre principale

####### Frames ------------------------------------------------------------------------------------------------------------------------
listframe <- tkframe(top)
    listdesc<-tklistbox(listframe,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))  
    scr <- tkscrollbar(listframe,repeatinterval=5,command=function(...)tkyview(listdesc,...)) 
    nomdonnee<-.activeDataSet  
    donnee<-get(nomdonnee)     
    vars<-colnames(donnee)
    vars.fact = NULL
    vars.desc = NULL
    for (i in (1:ncol(donnee))){
      if (is.numeric(donnee[,i])){
        tkinsert(listdesc,"end",vars[i])
        vars.desc = c(vars.desc,vars[i])
      }
    }
    tkgrid(listdesc, scr, sticky = "nw")     ###################
    tkgrid.configure(scr, sticky = "wns")      ###################
    tkgrid.configure(listdesc, sticky = "ew") 

optionsframe <- tkframe(top,borderwidth=2,relief="ridge")
    col<-tclVar("2")       ### Initialisation de la variable tcl "col" dont la valeur par défaut vaut 2
    nbcol<-tkentry(optionsframe,width=4,textvariable=col)    ### Nouvelle boîte de texte
    row<-tclVar("2")
    nbrow<-tkentry(optionsframe,width=4,textvariable=row)
    adj.val<-tclVar("1")
    adj<-tkentry(optionsframe,width=4,textvariable=adj.val)
    nbcol.lab<-tklabel(optionsframe,text="Number of histograms per column")    ### Nouveau Label
    nbrow.lab<-tklabel(optionsframe,text="Number of histograms per row")
    adj.lab<-tklabel(optionsframe,text="Scale for density")
    tkgrid(nbcol.lab,nbcol, sticky="w")        ###################
    tkgrid(nbrow.lab,nbrow, sticky="w")      ###################
    tkgrid(adj.lab,adj,sticky="w")

App<-function(){
  ### On appelle App la fonction qui regroupera les trois lignes de code qui suivent (exécution de la fonction histprod notamment)
  variable <- vars.desc[as.numeric(tkcurselection(listdesc))+1]
  command2=paste('histprod(',nomdonnee,'[,c("', paste(variable, collapse='", "'), '")],firstvar=1,numc=',as.numeric(tclvalue(col)),',numr=',as.numeric(tclvalue(row)),',adjust=',as.numeric(tclvalue(adj.val)),')',sep='')    
  doItAndPrint(command2)
}

onOK<-function(){
App()
tkdestroy(top)  
}

App.but <- tkbutton(top,borderwidth=3,width=12,text="Submit",command=App, fg="blue")
OKCancelHelp(helpSubject="histprod")  
tkgrid(tklabel(top, text = "Descriptors (pick two or more)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(listframe, optionsframe,sticky="w")
tkgrid(tklabel(top,text=""))
tkgrid(App.but,sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/histprod.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)  ### A l'ouverture de la fenêtre top, celle-ci deviendra la fenêtre active par défaut.
}

######           FIN FONCTION HISTPROD         #######


############################################################################################################################################
##############################################        FONCTION AVERAGETABLE         ########################################################
SensAverage<-function(){
require(tcltk)
require(SensoMineR)
top<-tktoplevel(borderwidth=10)
tkwm.title(top,"Averagetable")
plus.funct <- function(){
    tkinsert(model,"end","+")}
fois.funct <- function(){
    tkinsert(model,"end","*")}
interact.funct <- function(){
    tkinsert(model,"end",":")}
OpFont <- tkfont.create(family="courier", size=11)

###### FRAMES --------------------------------------------------------------------------------------------------------------------

modelFrame <- tkframe(top,borderwidth=3)
    factFrame<-tkframe(modelFrame)
        listfact<-tklistbox(factFrame,selectmode="extended",height=5,yscrollcommand=function(...) tkset(scrfact,...))
        scrfact <- tkscrollbar(factFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
        tkgrid(listfact,scrfact, sticky = "nw")
        tkgrid.configure(scrfact,sticky = "wns")
        tkgrid.configure(listfact,sticky = "w")
    opFrame <- tkframe(modelFrame,borderwidth=2)
        formul.lab<-tklabel(opFrame,text="Model formula : ")
        plus.but <- tkbutton(opFrame,text="+",width=2,command=plus.funct,font=OpFont)
        fois.but <- tkbutton(opFrame,text="*",width=2,command=fois.funct,font=OpFont)
        interact.but <- tkbutton(opFrame,text=":",width=2,command=interact.funct,font=OpFont)
        tkgrid(formul.lab,plus.but,fois.but,interact.but, sticky="sw")
    formulFrame <- tkframe(modelFrame,borderwidth=2)
        formul<-tclVar("")
        model <- tkentry(formulFrame,width=45,textvariable=formul)
        modelscr <- tkscrollbar(formulFrame,repeatinterval=5,orient="horizontal",command=function(...)tkxview(model,...))
        tkconfigure(model, xscrollcommand=function(...) tkset(modelscr, ...))
        tkgrid(model,sticky="w")
        tkgrid(modelscr,sticky="w")
        tkgrid.configure(modelscr,sticky="we")
        tkgrid(tklabel(formulFrame,text=""))
        tkgrid(tklabel(formulFrame,text=""))

descopframe<-tkframe(top)
    optionsFrame <- tkframe(descopframe,relief="ridge",borderwidth=2)
        methodFrame <- tkframe(optionsFrame)
            mean.rb <- tkradiobutton(methodFrame)
            coef.rb <- tkradiobutton(methodFrame)
            rbValue <- tclVar("coeff")
            tkconfigure(mean.rb,variable=rbValue,value="mean")
            tkconfigure(coef.rb,variable=rbValue,value="coeff")
            tkgrid(tklabel(methodFrame,text="Method : "),tklabel(methodFrame,text="Mean"),mean.rb)
            tkgrid(tklabel(methodFrame,text=""),tklabel(methodFrame,text="Coefficient"),coef.rb)
        resFrame <- tkframe(optionsFrame)
            resu.val<-tclVar("results")
            resu<-tkentry(resFrame,width=10,textvariable=resu.val)
            resu.lab<-tklabel(resFrame,text="Keep the result in : ")
            tkgrid(resu.lab,resu)

    descFrame <- tkframe(descopframe)
        listdesc<-tklistbox(descFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
        scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
        donnee<-get(.activeDataSet)
   nomdonnee<-.activeDataSet
        vars<-colnames(donnee)
        nbfac=0
        vars.fact = NULL
        vars.desc = NULL
        for (i in (1:ncol(donnee))){
            if (is.numeric(donnee[,i])){
              tkinsert(listdesc,"end",vars[i])
              vars.desc = c(vars.desc,vars[i])
            }
            else {
                vars.fact = c(vars.fact,vars[i])
                tkinsert(listfact,"end",vars[i])
            }
        }
        tkgrid(listdesc, scr,sticky = "nw")
        tkgrid.configure(scr, sticky = "wns")
        tkgrid.configure(listdesc,sticky = "ew")
        tkselection.set(listdesc,0)


#### Fonction pour l'effet d'un double-clic sur un item de 'listfact' ---------------------------------------------------------------

doubleclick<-function(){
    tkfocus(model)
    formul <- vars.fact[as.numeric(tkcurselection(listfact))+1]
    tkinsert(model,"end",formul)
}
tkbind(listfact,"<Double-ButtonPress-1>",doubleclick)


#### Appel de la fonction AVERAGETABLE --------------------------------------------------------------------------------------------------

App<-function(){
    modele<-tclvalue(formul)
    variable<-vars.desc[as.numeric(tkcurselection(listdesc))+1]
    method.val <- as.character(tclvalue(rbValue))
    resultat<-tclvalue(resu.val)
    if (modele=="") tkmessageBox(message="You must fill in the grid 'Model formula'",icon="warning",type="ok")
    command3=paste(resultat,'=averagetable(',nomdonnee,'[,c("', paste(vars.fact, collapse='", "'), '", "', paste(variable, collapse='", "'), '")],firstvar=',length(vars.fact)+1,',formul=~',modele,',method="',method.val,'")',sep='')
    justDoIt(command3)
    logger(command3)
}

onOK<-function(){
  App()
  tkdestroy(top)
}



#### Positionnement sur la fenêtre 'top' ----------------------------------------------------------------------------------------------
app.but<-tkbutton(top,text="Submit",command=App,fg="blue",width=13,borderwidth=3)
OKCancelHelp(helpSubject="averagetable")
tkgrid(tklabel(top, text = "Factors (double-click to formula)", fg = "blue"),  sticky = "w")
tkgrid(modelFrame,sticky="w")
tkgrid(factFrame,sticky="w")
tkgrid(opFrame,sticky="w")
tkgrid(formulFrame,sticky="w")
tkgrid(tklabel(top,text = "Variables (pick one or more)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(descopframe,sticky="w")
tkgrid(descFrame,tklabel(top,text="   "),optionsFrame,sticky="w")
tkgrid.configure(optionsFrame,sticky="s")
tkgrid(tklabel(top,text=""))
tkgrid(methodFrame,sticky="w")
tkgrid(resFrame,sticky="w")
tkgrid(tklabel(top,text=""))
tkgrid(app.but,sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/averagetable.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}

#########################################################       FIN FONCTION AVERAGETABLE         #####################################################
#######################################################################################################################################################




##################################################################################################################################################
#####################################################             FONCTION ARDI            #########################################################


SensArdi <- function(){
require(tcltk)
require(SensoMineR)
top<-tktoplevel(borderwidth=10)
tkwm.title(top,"Ardi")

######## FRAMES -----------------------------------------------------------------------------------------------------------------------

factFrame <- tkframe(top)
    listfact<-tklistbox(factFrame,selectmode="extended",yscrollcommand=function(...) tkset(scrfact,...))
    scrfact <- tkscrollbar(factFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
    tkgrid(listfact, scrfact,sticky = "nw")
    tkgrid.configure(scrfact, sticky = "wns")
    tkgrid.configure(listfact,sticky = "ew")


interestFrame <- tkframe(top)
    col.lab<-tklabel(interestFrame,text="First factor of interest : 'Products'",fg="darkgreen")
    col.val<-tclVar("")
    colj <- tkentry(interestFrame,width=15,textvariable=col.val)
    col2.lab<-tklabel(interestFrame,text="Second factor of interest : 'Panelists'",fg="darkgreen")
    col2.val<-tclVar("")
    col2j <- tkentry(interestFrame,width=15,textvariable=col2.val)
    tkgrid(col.lab, colj,sticky = "nw")
    tkgrid(col2.lab, col2j,sticky = "nw")



descopFrame<-tkframe(top)
descFrame <- tkframe(descopFrame)
    listdesc<-tklistbox(descFrame,selectmode="extended",yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
    donnee<-get(.activeDataSet)
 nomdonnee<-.activeDataSet
    vars<-colnames(donnee)
    nbfac=0
    for (i in (1:ncol(donnee))){
        if (is.numeric(donnee[,i]))
            tkinsert(listdesc,"end",vars[i])
        if (is.factor(donnee[,i])){
            tkinsert(listfact,"end",vars[i])
            nbfac=nbfac+1
        }
    }
    tkgrid(listdesc, scr,sticky = "nw")
    tkgrid.configure(scr, sticky = "wns")
    tkgrid.configure(listdesc,sticky = "ew")

optionsFrame<-tkframe(descopFrame,relief="ridge",borderwidth=2)
    ndiv.val<-tclVar("10")
    ndiv.ent<-tkentry(optionsFrame,width=3,textvariable=ndiv.val)
    ndiv.lab<-tklabel(optionsFrame,text="Number of divergences to print : ")
    center.check <- tkcheckbutton(optionsFrame)
    centerValue <- tclVar("1")
    tkconfigure(center.check,variable=centerValue)
    center.lab<-tklabel(optionsFrame,text="Center the data by panelists ")
    scale.check <- tkcheckbutton(optionsFrame)
    scaleValue <- tclVar("0")
    tkconfigure(scale.check,variable=scaleValue)
    scale.lab<-tklabel(optionsFrame,text="Scale the data by panelists ")
    resu.val<-tclVar("results")
    resu<-tkentry(optionsFrame,width=10,textvariable=resu.val)
    resu.lab<-tklabel(optionsFrame,text="Keep the data in : ")
    tkgrid(ndiv.lab,ndiv.ent,sticky="w")
    tkgrid(center.lab,center.check,sticky="w")
    tkgrid(scale.lab,scale.check,sticky="w")
    tkgrid(resu.lab,resu,sticky="w")


#### Fonction liée au bouton 'Sorties' --------------------------------------------------------------------------------------------------
env<-environment()
Gtab<-FALSE
Gpandiv<-FALSE
Gprodiv<-FALSE
Gcmb<-FALSE

#cptopt<-0
onSortie<-function(){
    sortiestop<-tktoplevel()
    tkwm.title(sortiestop,"Outputs")        
    n.div<-as.numeric(tclvalue(ndiv.val))

    tab.check <- tkcheckbutton(sortiestop)
    if (Gtab) tabValue <- tclVar("1")
    else tabValue <- tclVar("0")
    tab.lab<-tklabel(sortiestop,text="The centered and scaled data by panelists")
    tkconfigure(tab.check,variable=tabValue)

    pandiv.check <- tkcheckbutton(sortiestop)
    if (Gpandiv) pandivValue <- tclVar("1")
    else pandivValue <- tclVar("0")
    pandiv.lab<-tklabel(sortiestop,text=paste("The", n.div ,"highest divergences between panelists"))
    tkconfigure(pandiv.check,variable=pandivValue)
    
    prodiv.check <- tkcheckbutton(sortiestop)
    if(Gprodiv) prodivValue <- tclVar("1")
    else prodivValue <- tclVar("0")
    prodiv.lab<-tklabel(sortiestop,text=paste("The", n.div ,"highest divergences between products"))
    tkconfigure(prodiv.check,variable=prodivValue)
    
    cmb.check <- tkcheckbutton(sortiestop)
    if(Gcmb) cmbValue <- tclVar("1")
    cmbValue <- tclVar("0")
    cmb.lab<-tklabel(sortiestop,text=paste("The", n.div ,"highest divergences between panelists and products"))
    tkconfigure(cmb.check,variable=cmbValue)
    
    onOKsortie<-function(){
        if(tclvalue(tabValue)=="1") assign("Gtab", TRUE, envir=env)
        else assign("Gtab", FALSE, envir=env)
        if(tclvalue(pandivValue)=="1") assign("Gpandiv", TRUE, envir=env)
        else assign("Gpandiv", FALSE, envir=env)
        if(tclvalue(prodivValue)=="1") assign("Gprodiv", TRUE, envir=env)
        else assign("Gprodiv", FALSE, envir=env)
        if(tclvalue(cmbValue)=="1") assign("Gcmb", TRUE, envir=env)
        else assign("Gcmb", FALSE, envir=env)
                
        tkdestroy(sortiestop)
        tkfocus(top)
    }

    tkgrid(tklabel(sortiestop,text="Options for printing the results",fg="red"))
    tkgrid(tab.lab,tab.check,sticky="w")
    tkgrid(pandiv.lab,pandiv.check,sticky="w")
    tkgrid(prodiv.lab,prodiv.check,sticky="w")
    tkgrid(cmb.lab,cmb.check,sticky="w")
    tkgrid(tkbutton(sortiestop,text="OK",width=5,command=onOKsortie))
    tkfocus(sortiestop)
}

#####Fonction liée à un doubleclick dans la liste des facteurs --------------------------------------------------------------------------------------------
doubleclick<-function(){
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    if(col.nam==""){
        col.val <- vars[as.numeric(tkcurselection(listfact))+1]
        tkinsert(colj,"end",col.val)}
    else{
        if(col2.nam==""){
            col2.val <- vars[as.numeric(tkcurselection(listfact))+1]
            tkinsert(col2j,"end",col2.val)}
    }
}


####### Fonction liée au bouton Appliquer ---------------------------------------------------------------------------------------------------------------
App<-function(){
    n.div<-as.numeric(tclvalue(ndiv.val))
    centerVal<-as.numeric(tclvalue(centerValue))
    scaleVal<-as.numeric(tclvalue(scaleValue))
    nbitemlist<-c(tclvalue(tkcurselection(listdesc)))
    nbitem<-unlist(strsplit(nbitemlist,"\\ "))
    variable<-as.numeric(tkcurselection(listdesc))+(nbfac+1)
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    for (i in 1:ncol(donnee)){
        if (vars[i]==col.nam) col.pos<-i
        if (vars[i]==col2.nam) col2.pos<-i
    }
    resultat<-tclvalue(resu.val)
    if (col.nam=="")
        tkmessageBox(message="You must select a factor for 'Products'",icon="warning",type="ok")
    else{
        if (col2.nam=="")
            tkmessageBox(message="You must select a factor for 'Panelists'",icon="warning",type="ok")
        else{
            if (length(nbitem)<2)
                tkmessageBox(message="You must select at least 2 descriptors in the list",icon="warning",type="ok")
            else{
            
                    descri=paste(variable,collapse=",")
            command1=paste('descriptors=c(',descri,')',sep='')
doItAndPrint(command1)

#je lui demande d'afficher l' indice de la dernière variable quanlitative : le facteur      

command2=paste('factors=c(1:',nbfac,')',sep='')
doItAndPrint(command2)

command3=paste(resultat,'=ardi(',nomdonnee,'[,c(factors,descriptors)],col.p=',col.pos,',col.j=',col2.pos,',firstvar=',nbfac+1,',nbval=',n.div,',center=',centerVal,',scale=',scaleVal,')',sep='')
justDoIt(command3)
logger(command3)
#doItAndPrint(command3)

                    if (Gtab==1){
               doItAndPrint(paste(resultat,'$tab',sep='')) 
            }
                    if (Gpandiv==1){
               doItAndPrint(paste(resultat,'$panelist',sep='')) 
            }
                    if (Gprodiv==1){
               doItAndPrint(paste(resultat,'$product',sep='')) 
            }
                    if (Gcmb==1){
               doItAndPrint(paste(resultat,'$combination',sep='')) 
            }
            }
        }
    }
}


#####Fonction liée au bouton OK, appel de Ardi et destruction de la fenêtre  ------------------------------------------------------------------------------
onOK<-function(){
    n.div<-as.numeric(tclvalue(ndiv.val))
    centerVal<-as.numeric(tclvalue(centerValue))
    scaleVal<-as.numeric(tclvalue(scaleValue))
    nbitemlist<-c(tclvalue(tkcurselection(listdesc)))
    nbitem <- unlist(strsplit(nbitemlist,"\\ "))
    variable<-as.numeric(tkcurselection(listdesc))+(nbfac+1)
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    for (i in 1:ncol(donnee)){
        if (vars[i]==col.nam) col.pos<-i
        if (vars[i]==col2.nam) col2.pos<-i
    }
    resultat<-tclvalue(resu.val)
    if (col.nam=="")
        tkmessageBox(message="'Product'",icon="warning",type="ok")
    else{
        if (col2.nam=="")
            tkmessageBox(message="You must select a variable for 'Panelists'",icon="warning",type="ok")
        else{
            if (length(nbitem)<2)
                tkmessageBox(message="You must select at least 2 variables in the list",icon="warning",type="ok")
            else{
                            descri=paste(variable,collapse=",")
            command1=paste('descriptors=c(',descri,')',sep='')
doItAndPrint(command1)

#je lui demande d'afficher l' indice de la dernière variable quanlitative : le facteur      

command2=paste('factors=c(1:',nbfac,')',sep='')
doItAndPrint(command2)

command3=paste(resultat,'=ardi(',nomdonnee,'[,c(factors,descriptors)],col.p=',col.pos,',col.j=',col2.pos,',firstvar=',nbfac+1,',nbval=',n.div,',center=',centerVal,',scale=',scaleVal,')',sep='')
justDoIt(command3)
logger(command3)
#doItAndPrint(command3)

                ardires<-get(resultat)

                    if (Gtab==1){
               doItAndPrint(paste('Data centered and scaled',sep=''))            
            }
                    if (Gpandiv==1){
               doItAndPrint(paste(resultat,'$panelist',sep=''))                                       
            }
                    if (Gprodiv==1){
               doItAndPrint(paste(resultat,'$product',sep='')) 
            }
                    if (Gcmb==1){
               doItAndPrint(paste(resultat,'$combination',sep='')) 
            }

                tkdestroy(top)
            }
        }
    }
}





#####Positionnement des widgets sur top -------------------------------------------------------------------------------------------------------------------
sorties.but<-tkbutton(optionsFrame,text="Outputs",command=onSortie,width=13,default="active",fg="darkred",borderwidth=3)
app.but<- tkbutton(top,text="Submit", command=App, fg="blue", width=13,borderwidth=3)
OKCancelHelp(helpSubject="ardi")
tkgrid(tklabel(top, text = "Factors (double-click to factors of interest)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(factFrame,sticky="w")
tkgrid(interestFrame,sticky="w")
tkgrid(tklabel(top,text=""))
tkgrid(tklabel(top,text=""))
tkgrid(tklabel(top, text = "Descriptors (pick two or more)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(descopFrame, sticky="w")
tkgrid(descFrame,tklabel(top,text="   "),optionsFrame,sticky="w")
tkgrid(sorties.but,sticky="e")
tkgrid.configure(optionsFrame,sticky="sw")
tkgrid(tklabel(top,text=""))
tkgrid(app.but, sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/ardi.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkbind(listfact,"<Double-ButtonPress-1>",doubleclick)
tkfocus(top)
}

#####################################################           FIN FONCTION ARDI          #########################################################
##################################################################################################################################################


###########################################################################################################################################################
###################################################################          FONCTION   DECAT              ################################################

SensDecat<-function(){
    require(tcltk)
    require(SensoMineR)
    top<-tktoplevel(borderwidth=5)
    tkwm.title(top,"Decat")
    OpFont <- tkfont.create(family="courier", size=10)

    factFrame<-tkframe(top)
    listfact<-tklistbox(factFrame,selectmode="extended",height=7,yscrollcommand=function(...) tkset(scrfact,...))
    scrfact <- tkscrollbar(factFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
    tkgrid(listfact,scrfact,tklabel(factFrame,text="   "), sticky = "nw")
    tkgrid.configure(scrfact,sticky = "wns")
    tkgrid.configure(listfact,sticky = "w")

  #! fonction pour la gestion des options graphiques
  couleur.a.changer<-defmacro(label, firstLabel, expr=
  {
    optionsFrame2 <- tkframe(optionsFrame,borderwidth=3)
    .PlotLabel<-tclVar(paste(firstLabel, "", sep=" "))
    env<-environment()
    Rcol.low<-Rcol.low.tmp<-"mistyrose"
    Rcol.up<-Rcol.up.tmp<-"lightblue"
    Rgraph <- TRUE

    OnPlot<-function()
    {
      PlotWin<-tktoplevel()
      tkwm.title(PlotWin,gettextRcmdr("Displayed options"))
      tkwm.geometry(PlotWin, "-100+50")

      #création de la fonction onOKsub
      onOKsub<-function()
      {
        assign("Rcol.low", Rcol.low.tmp, envir=env)
        assign("Rcol.up", Rcol.up.tmp, envir=env)
        if(tclvalue(cbValue)=="1") assign("Rgraph", TRUE, envir=env)
        else assign("Rgraph", FALSE, envir=env)
        tkdestroy(PlotWin)
      }

      RcolFrame<-tkframe(PlotWin, borderwidth=2)
      Rcol.low.value <- Rcol.low
      canvas.low <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.low.value)
      ChangeColor.low <- function()
      {
        Rcol.low.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.low.value,title="Select color"))
        if (nchar(Rcol.low.value)>0){
          tkconfigure(canvas.low,bg=Rcol.low.value)
          assign("Rcol.low.tmp", Rcol.low.value, envir=env)
        }
      }
      ChangeColor.low.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.low)
      tkgrid(tklabel(RcolFrame, text=gettextRcmdr("Color for low values")),canvas.low,ChangeColor.low.button, sticky="w")

      Rcol.up.value<-Rcol.up
      canvas.up <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.up.value)
      ChangeColor.up <- function()
      {
        Rcol.up.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.up.value,title="Select color"))
        if (nchar(Rcol.up.value)>0){
          tkconfigure(canvas.up,bg=Rcol.up.value)
          assign("Rcol.up.tmp", Rcol.up.value, envir=env)
        }
      }
      ChangeColor.up.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.up)
      tkgrid(tklabel(RcolFrame, text=gettextRcmdr("Color for high values")),canvas.up,ChangeColor.up.button, sticky="w")

      graph.check <- tkcheckbutton(PlotWin)
      if (Rgraph) cbValue <- tclVar("1")
      else cbValue <- tclVar("0")
      tkconfigure(graph.check,variable=cbValue)
      graph.lab<-tklabel(PlotWin,text="Add the associated graphic? ")

      #mise en page des différents frames de PlotIndFrame
      tkgrid(RcolFrame)
      tkgrid(tklabel(PlotWin, text=" "))
      tkgrid(graph.lab,graph.check,sticky="w")

      subOKCancelHelp(PlotWin, helpSubject=NULL)
      tkgrid(subButtonsFrame, sticky="ew")
    }
    Plot.but<-tkbutton(optionsFrame2, textvariable=.PlotLabel, command=OnPlot, borderwidth=3, width=13)
    tkgrid(Plot.but, sticky="w")
  })

######################### Bouton formul ------------------------------------------------------------------------------------------------------------------------
onFormul<-function(){
    topformul<-tktoplevel(borderwidth=10)
    tkfocus(topformul)
    tkwm.title(topformul,"Global model formula")
    plus.funct <- function(){tkinsert(model.par,"end","+")}
    interact.funct <- function(){tkinsert(model.par,"end",":")}
    
    modelFrame <- tkframe(topformul,borderwidth=3)
    opFrame <- tkframe(modelFrame,borderwidth=2)
    formul.lab.par<-tklabel(opFrame,text="Model Formula :      ")
    plus.but <- tkbutton(opFrame,text="+",width=3,command=plus.funct)
    interact.but <- tkbutton(opFrame,text=":",width=3,command=interact.funct)
    tkgrid(formul.lab.par,plus.but,interact.but, sticky="s")
    formulFrame <- tkframe(modelFrame,borderwidth=2)
    formul.par<-tclVar(tclvalue(formul))
    model.par <- tkentry(formulFrame,width=45,textvariable=formul.par)
    tkgrid(model.par,sticky="sw")
    
    factFrame<-tkframe(modelFrame)
        listfact<-tklistbox(factFrame,selectmode="single",height=4,width=20)
        for (i in (1:ncol(donnee))){
            if (is.factor(donnee[,i])) tkinsert(listfact,"end",vars[i])
        }
        scrfact <- tkscrollbar(factFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
        tkgrid(listfact,scrfact, sticky = "nw")
        tkgrid.configure(scrfact,sticky = "wns")
        tkgrid.configure(listfact,sticky = "w")    
    
    onCreate<-function(){
        tclvalue(formul)<-""
        tkinsert(model,"end",tclvalue(formul.par))
        tkdestroy(topformul)
        tkfocus(top)
    }
    
    create.but<-tkbutton(topformul,text="OK",width=9,command=onCreate)
    
    doubleclick<-function(){
        formul.par <- vars.fact[as.numeric(tkcurselection(listfact))+1]
        tkinsert(model.par,"end",formul.par)
    }
    
    tkgrid(tklabel(topformul, text = "Factors (double-click to formula)", fg = "blue"), columnspan = 3, sticky = "w")
    tkgrid(modelFrame)
    tkgrid(factFrame)
    tkgrid(opFrame)
    tkgrid(formulFrame)
    tkgrid(create.but)
    tkbind(listfact,"<Double-ButtonPress-1>",doubleclick)
}


######################### FRAMES ------------------------------------------------------------------------------------------------------------------------

    interestFrame <- tkframe(top)
    col.lab<-tklabel(interestFrame,text="Factor 'Product' : ",fg="darkgreen")
    col.val<-tclVar("")
    colp <- tkentry(interestFrame,width=15,textvariable=col.val)   
    col2.lab<-tklabel(interestFrame,text="Factor 'Panelist' : ",fg="darkgreen")
    col2.val<-tclVar("")
    colj <- tkentry(interestFrame,width=15,textvariable=col2.val)
    tkgrid(col.lab, colp,sticky = "w")
    tkgrid(col2.lab, colj,sticky = "w")

    modelFrame <- tkframe(top)
    formul.lab<-tklabel(modelFrame,text="Model Formula :      ")
    tkgrid(formul.lab,sticky="sw")
    formul<-tclVar("")
    model <- tkentry(modelFrame,width=45,textvariable=formul)
    modelscr <- tkscrollbar(modelFrame,repeatinterval=5,orient="horizontal",command=function(...)tkxview(model,...))
    tkconfigure(model, xscrollcommand=function(...) tkset(modelscr, ...))
    
    cree.but<-tkbutton(top,text="Complete model",command=onFormul)
    tkgrid(model,cree.but,sticky="w")
    tkgrid(modelscr,sticky="w")    

    optionsFrame <- tkframe(top,relief="ridge",borderwidth=2)
    random.check <- tkcheckbutton(optionsFrame)
    rnd.bool <- tclVar("1")
    tkconfigure(random.check,variable=rnd.bool)
    random.lab<-tklabel(optionsFrame,text="Random effect for 'Panelist'")    
    seuil.val<-tclVar("0.05")
    seuil<-tkentry(optionsFrame,width=5,textvariable=seuil.val)
    seuil.lab<-tklabel(optionsFrame,text="The significance level for the AOV model: ")
    resu.val<-tclVar("results")
    resu<-tkentry(optionsFrame,width=10,textvariable=resu.val)
    resu.lab<-tklabel(optionsFrame,text="Keep the results in: ")
    tkgrid(random.lab,random.check, sticky="w")
    tkgrid(seuil.lab,seuil,sticky="w")
    tkgrid(resu.lab,resu,sticky="w")
    couleur.a.changer(label=gettextRcmdr("Graphical options"), firstLabel=gettextRcmdr("Graphical options"))

    descFrame <- tkframe(top)
    listdesc<-tklistbox(descFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
    donnee<-get(.activeDataSet)
    nomdonnee=.activeDataSet
    vars<-colnames(donnee)
    vars.fact = NULL
    vars.desc = NULL
    for (i in (1:ncol(donnee))){
      if (is.numeric(donnee[,i])){
        tkinsert(listdesc,"end",vars[i])
        vars.desc = c(vars.desc,vars[i])
      }
      else {
        vars.fact = c(vars.fact,vars[i])
        tkinsert(listfact,"end",vars[i])
      }
    }
    tkselection.set(listdesc,0)
    tkgrid(listdesc, scr,sticky = "nw")
    tkgrid.configure(scr, sticky = "wns")
    tkgrid.configure(listdesc,sticky = "ew")
    tkselection.set(listdesc,0)


#################### Fonctions liée au bouton 'Sorties' --------------------------------------------------------------------------------------------------
    env<-environment()
    Gtabf<-FALSE
    Gtabt<-FALSE
    Gcoef<-FALSE
    Gadj<-TRUE
    Gresf<-TRUE
    Grest<-TRUE

onSortie<-function(){
    sortiestop<-tktoplevel(borderwidth=10)
    tkwm.title(sortiestop,"Outputs")

    onOKsortie<-function(){
        if(tclvalue(tabfValue)=="1") assign("Gtabf", TRUE, envir=env)
        else assign("Gtabf", FALSE, envir=env)
        if(tclvalue(tabtValue)=="1") assign("Gtabt", TRUE, envir=env)
        else assign("Gtabt", FALSE, envir=env)
        if(tclvalue(coefValue)=="1") assign("Gcoef", TRUE, envir=env)
        else assign("Gcoef", FALSE, envir=env)
        if(tclvalue(adjValue)=="1") assign("Gadj", TRUE, envir=env)
        else assign("Gadj", FALSE, envir=env)
        if(tclvalue(resfValue)=="1") assign("Gresf", TRUE, envir=env)
        else assign("Gresf", FALSE, envir=env)
        if(tclvalue(restValue)=="1") assign("Grest", TRUE, envir=env)
        else assign("Grest", FALSE, envir=env)
        tkdestroy(sortiestop)
    }

    tabf.check <- tkcheckbutton(sortiestop)
    if (Gtabf) tabfValue <- tclVar("1")
    else tabfValue <- tclVar("0")
    tkconfigure(tabf.check,variable=tabfValue)
    tabf.lab<-tklabel(sortiestop,text="V-test and P-value of the F-test")

    tabt.check <- tkcheckbutton(sortiestop)
    if (Gtabt) tabtValue <- tclVar("1")
    else tabtValue <- tclVar("0")
    tkconfigure(tabt.check,variable=tabtValue)
    tabt.lab<-tklabel(sortiestop,text="Vtest for a given product and a given descriptor")

    coef.check <- tkcheckbutton(sortiestop)
    if (Gcoef) coefValue <- tclVar("1")
    else coefValue <- tclVar("0")
    tkconfigure(coef.check,variable=coefValue)
    coef.lab<-tklabel(sortiestop,text="Coefficient resulting from the AOV model")

    adj.check <- tkcheckbutton(sortiestop)
    if (Gadj) adjValue <- tclVar("1")
    else adjValue <- tclVar("0")
    tkconfigure(adj.check,variable=adjValue)
    adj.lab<-tklabel(sortiestop,text="Adjusted mean")

    resf.check <- tkcheckbutton(sortiestop)
    if (Gresf) resfValue <- tclVar("1")
    else resfValue <- tclVar("0")
    tkconfigure(resf.check,variable=resfValue)
    resf.lab<-tklabel(sortiestop,text="V-test and P-value of the F-tests sorted in ascending order")

    rest.check <- tkcheckbutton(sortiestop)
    if (Grest) restValue <- tclVar("1")
    else restValue <- tclVar("0")
    tkconfigure(rest.check,variable=restValue)
    rest.lab<-tklabel(sortiestop,text="Coefficients, P-value and V-test for the significant descriptors")

    tkgrid(tklabel(sortiestop,text="Options to print the results",fg="red"))
    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(resf.lab,resf.check,sticky="w")
    tkgrid(rest.lab,rest.check,sticky="w")
    tkgrid(adj.lab,adj.check,sticky="w")
    tkgrid(tabf.lab,tabf.check,sticky="w")
    tkgrid(tabt.lab,tabt.check,sticky="w")
    tkgrid(coef.lab,coef.check,sticky="w")
    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(tkbutton(sortiestop,text="OK",width=9,command=onOKsortie))
    tkfocus(sortiestop)
}

###### Fonction double-click------------------------------------------------------------------------------------------------------------

doubleclick2<-function(){
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    if(col.nam==""){
      col.val <- vars.fact[as.numeric(tkcurselection(listfact))+1]
      tkinsert(colp,"end",col.val)
      tkinsert(model,"end",col.val)
    }
    else{
      if(col2.nam==""){    
        col2.val <- vars.fact[as.numeric(tkcurselection(listfact))+1]
        tkinsert(colj,"end",col2.val)
        tkinsert(model,"end",c('+',col2.val))
      }
    }
}

###### Fonction principale qui lance DECAT sans fermer la fenêtre------------------------------------------------------------------------------------------------------------
App<-function(){
    nbitemlist<-c(tclvalue(tkcurselection(listdesc)))
    nbitem<-unlist(strsplit(nbitemlist,"\\ "))
    modele<-tclvalue(formul)
    rnd.val <- as.numeric(tclvalue(rnd.bool))
    variable<-vars.desc[as.numeric(tkcurselection(listdesc))+1]
    resultat<-tclvalue(resu.val)
    done = 0
    if (modele=="") tkmessageBox(message="You must fill in the grid 'Model formula'",icon="warning",type="ok")
    else{
      done = 1
      if (length(nbitem)>1) command3=paste(resultat,'=decat(',nomdonnee,'[,c("', paste(vars.fact, collapse='", "'), '", "', paste(variable, collapse='", "'), '")],firstvar=',length(vars.fact)+1,',formul=~',modele,',proba=',as.numeric(tclvalue(seuil.val)),',graph=',Rgraph,',col.lower="',Rcol.low,'",col.upper="',Rcol.up,'",random=',rnd.val,')',sep='')
      else  command3=paste(resultat,'=decat(',nomdonnee,'[,c("', paste(vars.fact, collapse='", "'), '", "', paste(vars.desc, collapse='", "'), '")],firstvar=',length(vars.fact)+1,',formul=~',modele,',proba=',as.numeric(tclvalue(seuil.val)),',graph=',Rgraph,',col.lower="',Rcol.low,'",col.upper="',Rcol.up,'",random=',rnd.val,')',sep='')
      justDoIt(command3)
      logger(command3)                     
      decatRes<-get(resultat)    
            
      if (Gresf){
        doItAndPrint(paste(resultat,'$resF',sep='')) 
      }
      if (Grest){
        doItAndPrint(paste(resultat,'$resT',sep='')) 
      }
      if (Gadj){
        doItAndPrint(paste(resultat,'$adjmean',sep='')) 
      }
      if (Gtabf){
        doItAndPrint(paste(resultat,'$tabF',sep='')) 
      }
      if (Gtabt){          
        doItAndPrint(paste(resultat,'$tabT',sep=''))  
      }
      if (Gcoef){
        doItAndPrint(paste(resultat,'$coeff',sep='')) 
      }              
    }
    return(done)
}


###### Fonction principale qui lance DECAT et ferme la fenêtre------------------------------------------------------------------------------------------------------------
onOK <- function(){
  done = App()
  if (done >0) tkdestroy(top)
}


##### Positionnement des widgets et frames sur la fenêtre 'top' ------------------------------------------------------------------------------------------
App.but <- tkbutton(top,borderwidth=3,width=12,text="Submit",command=App,fg="blue")
sorties.but<-tkbutton(optionsFrame,text="Outputs",command=onSortie,width=13,default="active",fg="darkred")

OKCancelHelp(helpSubject="decat")
tkgrid(tklabel(top, text = "Choose the product and panelist variables (double-click) :", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(factFrame,interestFrame,sticky="w")
tkgrid(tklabel(top,text=""))
tkgrid(modelFrame,columnspan = 2, sticky="w")
tkgrid(tklabel(top,text=""))
tkgrid(tklabel(top, text = "Select descriptors (by default, all descriptors are selected)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(descFrame,optionsFrame,sticky="w")
tkgrid.configure(optionsFrame,column=1, columnspan=2)
tkgrid(optionsFrame2,sorties.but,sticky="e")
tkgrid(tklabel(top,text=""))
tkbind(listfact,"<Double-ButtonPress-1>",doubleclick2)
tkbind(model,"<Return>",onOK)
tkgrid(App.but,sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/decat.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}

######################################################           FIN FONCTION DECAT         #############################################################
#########################################################################################################################################################


############################################################################################################################################
##############################################        FONCTION GRAPHINTER         ##########################################################
SensGraphinter <- function(){
require(tcltk)
require(SensoMineR)
top<-tktoplevel(borderwidth=10)
tkwm.title(top,"Interaction plot")
donnee<-get(.activeDataSet)
nomdonnee<-.activeDataSet

######## FRAMES -----------------------------------------------------------------------------------------------------------------------

factFrame <- tkframe(top)
    listfact<-tklistbox(factFrame,selectmode="extended",yscrollcommand=function(...) tkset(scrfact,...))
    scrfact <- tkscrollbar(factFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
    tkgrid(listfact, scrfact,sticky = "nw")
    tkgrid.configure(scrfact, sticky = "wns")
    tkgrid.configure(listfact,sticky = "ew")

interestFrame <- tkframe(top)
    col.lab<-tklabel(interestFrame,text="Factor 1 : ",fg="darkgreen")
    col.val<-tclVar("")
    colj <- tkentry(interestFrame,width=15,textvariable=col.val)
    col2.lab<-tklabel(interestFrame,text="Factor 2 : ",fg="darkgreen")
    col2.val<-tclVar("")
    col2j <- tkentry(interestFrame,width=15,textvariable=col2.val)
    
    tkgrid(col.lab, colj,sticky = "nw")
    tkgrid(col2.lab, col2j,sticky = "nw")

descopframe<-tkframe(top)
   descFrame <- tkframe(descopframe)
   listdesc<-tklistbox(descFrame,selectmode="extended",yscrollcommand=function(...) tkset(scr,...))
   scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
   vars<-colnames(donnee)
   vars.fact = NULL
   vars.desc = NULL
   for (i in (1:ncol(donnee))){
     if (is.numeric(donnee[,i])){
       tkinsert(listdesc,"end",vars[i])
       vars.desc = c(vars.desc,vars[i])
     }
     else {
       vars.fact = c(vars.fact,vars[i])
       tkinsert(listfact,"end",vars[i])
     }
   }
   tkgrid(listdesc, scr,sticky = "nw")
   tkgrid.configure(scr, sticky = "wns")
   tkgrid.configure(listdesc,sticky = "ew")

optionsFrame<-tkframe(descopframe,borderwidth=2,relief="ridge")
   nrow.val<-tclVar("2")
   nrow.ent<-tkentry(optionsFrame,width=3,textvariable=nrow.val)
   nrow.lab<-tklabel(optionsFrame,text="Number of graphs per row : ")
   ncol.val<-tclVar("2")
   ncol.ent<-tkentry(optionsFrame,width=3,textvariable=ncol.val)
   ncol.lab<-tklabel(optionsFrame,text="Number of graphs per column : ")
   resu.val<-tclVar("results")
   resu<-tkentry(optionsFrame,width=10,textvariable=resu.val)
   resu.lab<-tklabel(optionsFrame,text="Keep the result in : ")
   tkgrid(nrow.lab,nrow.ent,sticky="w")
   tkgrid(ncol.lab,ncol.ent,sticky="w")
   tkgrid(resu.lab,resu,sticky="w")


####### Fonction attachée aux boutons OK, appel de graphinter -------------------------------------------------------------------------------------
onOK<-function(){
    App()
    tkdestroy(top)
}

App<-function(){
    n.row<-as.numeric(tclvalue(nrow.val))
    n.col<-as.numeric(tclvalue(ncol.val))
    nbitemlist<-c(tclvalue(tkcurselection(listdesc)))
    nbitem<-unlist(strsplit(nbitemlist,"\\ "))
    variable <- vars.desc[as.numeric(tkcurselection(listdesc))+1]
    resultat<-tclvalue(resu.val)
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    for (i in 1:length(vars.fact)){
        if (vars.fact[i]==col.nam) col.pos<-i
        if (vars.fact[i]==col2.nam) col2.pos<-i
    }
    if (col.nam=="") tkmessageBox(message="No variable selected for the first factor of interest",icon="warning",type="ok")
    if (col2.nam=="") tkmessageBox(message="No variable selected for the second factor of interest",icon="warning",type="ok")
    if ((col.nam!="")&(col2.nam!="")){
      if (length(nbitem)>0) command3=paste(resultat,'=graphinter(',nomdonnee,'[,c("',paste(vars.fact, collapse='", "'), '", "', paste(variable, collapse='", "'), '")],col.p=',col.pos,',col.j=',col2.pos,',firstvar=',length(vars.fact)+1,',numr=',n.row,',numc=',n.col,')',sep='')
      else command3=paste(resultat,'=graphinter(',nomdonnee,'[,c("',paste(vars.fact, collapse='", "'), '", "', paste(vars.desc, collapse='", "'), '")],col.p=',col.pos,',col.j=',col2.pos,',firstvar=',length(vars.fact)+1,',numr=',n.row,',numc=',n.col,')',sep='')
      justDoIt(command3)
      logger(command3)

      justDoIt(paste('dev.new()'))

      if (length(nbitem)>0) command4=paste(resultat,'=interact(',nomdonnee,'[,c("',paste(vars.fact, collapse='", "'), '", "', paste(variable, collapse='", "'), '")],col.p=',col.pos,',col.j=',col2.pos,',firstvar=',length(vars.fact)+1,')',sep='')
      else command4=paste(resultat,'=interact(',nomdonnee,'[,c("',paste(vars.fact, collapse='", "'), '", "', paste(vars.desc, collapse='", "'), '")],col.p=',col.pos,',col.j=',col2.pos,',firstvar=',length(vars.fact)+1,')',sep='')
      justDoIt(command4)
      logger(command4)
    }
}


doubleclick<-function(){
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    if(col.nam==""){
      col.val <- vars.fact[as.numeric(tkcurselection(listfact))+1]
      tkinsert(colj,"end",col.val)}
    else{
      if(col2.nam==""){
        col2.val <- vars.fact[as.numeric(tkcurselection(listfact))+1]
        tkinsert(col2j,"end",col2.val)
      }
    }
}
tkbind(listfact,"<Double-ButtonPress-1>",doubleclick)


app.but<-tkbutton(top,text="Submit",command=App,fg="blue",width=13,borderwidth=3)
OKCancelHelp(helpSubject="graphinter")
tkgrid(tklabel(top, text = "Factors (double-click to select factors of interest)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(factFrame,interestFrame,sticky="w")
tkgrid.configure(interestFrame, column=1)
tkgrid(tklabel(top,text=""))
tkgrid(tklabel(top, text = "Select descriptors (by default, all descriptors are selected)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(descopframe,columnspan = 2, sticky="w")
tkgrid(descFrame,tklabel(top,text="   "),optionsFrame,sticky="w")
tkgrid.configure(optionsFrame,sticky="es",column=2)
tkgrid(tklabel(top,text=""))
tkgrid(app.but,sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/graphinter.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}

#########################################################       FIN FONCTION GRAPHINTER        ########################################################
#######################################################################################################################################################


#################################################################################################################################################
###################################################       FONCTION PANELIPERF (ET PANELPERF)  #####################################################

SensPaneliperf<-function(){
require(tcltk)
require(SensoMineR)
top<-tktoplevel(borderwidth=10)
tkwm.title(top,"Panelists' performance")

####### Fonction  intermédiaire (bouton Créer Formule) --------------------------------------------------------------------------
onFormul<-function(){
    topformul<-tktoplevel(borderwidth=10)
    tkfocus(topformul)
    tkwm.title(topformul,"Global model formula")
    plus.funct <- function(){tkinsert(model.par,"end","+")}
    interact.funct <- function(){tkinsert(model.par,"end",":")}
    
    modelFrame <- tkframe(topformul,borderwidth=3)
    opFrame <- tkframe(modelFrame,borderwidth=2)
    formul.lab.par<-tklabel(opFrame,text="Model Formula :      ")
    plus.but <- tkbutton(opFrame,text="+",width=3,command=plus.funct)
    interact.but <- tkbutton(opFrame,text=":",width=3,command=interact.funct)
    tkgrid(formul.lab.par,plus.but,interact.but, sticky="s")
    formulFrame <- tkframe(modelFrame,borderwidth=2)
    formul.par<-tclVar(tclvalue(formul))
    model.par <- tkentry(formulFrame,width=45,textvariable=formul.par)
    tkgrid(model.par,sticky="sw")
    
    factFrame<-tkframe(modelFrame)
        listfact<-tklistbox(factFrame,selectmode="single",height=4,width=20)
        for (i in (1:ncol(donnee))){
            if (is.factor(donnee[,i])) tkinsert(listfact,"end",vars[i])
        }
        scrfact <- tkscrollbar(factFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
        tkgrid(listfact,scrfact, sticky = "nw")
        tkgrid.configure(scrfact,sticky = "wns")
        tkgrid.configure(listfact,sticky = "w")    
    
    onCreate<-function(){
        tclvalue(formul)<-""
        tkinsert(model,"end",tclvalue(formul.par))
        tkdestroy(topformul)
        tkfocus(top)
    }
    
    create.but<-tkbutton(topformul,text="OK",width=9,command=onCreate)
    
    doubleclick<-function(){
        formul.par <- vars.fact[as.numeric(tkcurselection(listfact))+1]
        tkinsert(model.par,"end",formul.par)
    }
    
    tkgrid(tklabel(topformul, text = "Factors (double-click to formula)", fg = "blue"), columnspan = 3, sticky = "w")
    tkgrid(modelFrame)
    tkgrid(factFrame)
    tkgrid(opFrame)
    tkgrid(formulFrame)
    tkgrid(create.but)
    tkbind(listfact,"<Double-ButtonPress-1>",doubleclick)
}


####### Fonction  intermédiaire 2 (bouton Créer Formule) --------------------------------------------------------------------------
onFormul2<-function(){
    topformul<-tktoplevel(borderwidth=10)
    tkfocus(topformul)
    tkwm.title(topformul,"Model formula for each panelist")
    plus.funct <- function(){
    tkinsert(model.par,"end","+")}
    fois.funct <- function(){
    tkinsert(model.par,"end","*")}
    interact.funct <- function(){
    tkinsert(model.par,"end",":")}
    OpFont <- tkfont.create(family="courier", size=11)
    plus.but <- tkbutton(topformul,text="+",width=2,command=plus.funct,font=OpFont)
    fois.but <- tkbutton(topformul,text="*",width=2,command=fois.funct,font=OpFont)
    interact.but <- tkbutton(topformul,text=":",width=2,command=interact.funct,font=OpFont)
    
    modelFrame <- tkframe(topformul,borderwidth=3)
    opFrame <- tkframe(modelFrame,borderwidth=2)
    formul.lab.par<-tklabel(opFrame,text="Model formula :      ")
    plus.but <- tkbutton(opFrame,text="+",width=3,command=plus.funct)
    fois.but <- tkbutton(opFrame,text=":",width=3,command=interact.funct)
    tkgrid(formul.lab.par,plus.but,fois.but, sticky="sw")
    formulFrame <- tkframe(modelFrame,borderwidth=2)
    formul.par<-tclVar(tclvalue(formulj))
    model.par <- tkentry(formulFrame,width=45,textvariable=formul.par)
    modelscr.par <- tkscrollbar(formulFrame,repeatinterval=5,orient="horizontal",command=function(...)tkxview(model,...))
    tkconfigure(model.par, xscrollcommand=function(...) tkset(modelscr.par, ...))
    tkgrid(model.par,sticky="sw")
    tkgrid(modelscr.par,sticky="sw")
    tkgrid.configure(modelscr.par,sticky="we")
        
    factFrame<-tkframe(modelFrame)
        listfact<-tklistbox(factFrame,selectmode="single",height=4,width=20)
        for (i in (1:ncol(donnee))){
            if (is.factor(donnee[,i])) tkinsert(listfact,"end",vars[i])
        }
        scrfact <- tkscrollbar(factFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
        tkgrid(listfact,scrfact, sticky = "nw")
        tkgrid.configure(scrfact,sticky = "wns")
        tkgrid.configure(listfact,sticky = "w")    

    onCreate<-function(){
      tclvalue(formulj)<-""
      tkinsert(modelj,"end",tclvalue(formul.par))
      tkdestroy(topformul)
      tkfocus(top)
    }

    create.but<-tkbutton(topformul,text="OK",width=9,command=onCreate)
    
    doubleclick<-function(){
        formul.par <- vars.fact[as.numeric(tkcurselection(listfact))+1]
        tkinsert(model.par,"end",formul.par)
    }
    
    tkgrid(tklabel(topformul, text = "Factors (double-click to formula)", fg = "blue"), columnspan = 3, sticky = "w")
    tkgrid(listfact,sticky = "nw")
    tkgrid(modelFrame)
    tkgrid(factFrame)
    tkgrid(opFrame)
    tkgrid(formulFrame)
    tkgrid(create.but)
    tkbind(listfact,"<Double-ButtonPress-1>",doubleclick)
}



#################### Fonctions liées au bouton 'Sorties' --------------------------------------------------------------------------------------------------

env<-environment()
Gprob<-TRUE
Gvtest<-FALSE
Gres<-FALSE
Gr2<-FALSE
Gsign<-FALSE
Gagr<-TRUE
Gcomp<-FALSE
Gpval<-TRUE
Gvari<-FALSE
Gres2<-FALSE
    
onSortie<-function(){
    sortiestop<-tktoplevel(borderwidth=10)
    tkwm.title(sortiestop,"Outputs")
    
    
    onOKsortie<-function(){
        if(tclvalue(probValue)=="1") assign("Gprob", TRUE, envir=env)
        else assign("Gprob", FALSE, envir=env)
        if(tclvalue(agrValue)=="1") assign("Gagr", TRUE, envir=env)
        else assign("Gagr", FALSE, envir=env)
        if(tclvalue(vtestValue)=="1") assign("Gvtest", TRUE, envir=env)
        else assign("Gvtest", FALSE, envir=env)
        if(tclvalue(resValue)=="1") assign("Gres", TRUE, envir=env)
        else assign("Gres", FALSE, envir=env)
        if(tclvalue(r2Value)=="1") assign("Gr2", TRUE, envir=env)
        else assign("Gr2", FALSE, envir=env)
        if(tclvalue(signValue)=="1") assign("Gsign", TRUE, envir=env)
        else assign("Gsign", FALSE, envir=env)
        if(tclvalue(pvalValue)=="1") assign("Gpval", TRUE, envir=env)
        else assign("Gpval", FALSE, envir=env)
        if(tclvalue(compValue)=="1") assign("Gcomp", TRUE, envir=env)
        else assign("Gcomp", FALSE, envir=env)
        if(tclvalue(variValue)=="1") assign("Gvari", TRUE, envir=env)
        else assign("Gvari", FALSE, envir=env)
        if(tclvalue(res2Value)=="1") assign("Gres2", TRUE, envir=env)
        else assign("Gres2", FALSE, envir=env)
        tkdestroy(sortiestop)
    }

    pval.check <- tkcheckbutton(sortiestop)
    if (Gpval) pvalValue <- tclVar("1")
    else pvalValue <- tclVar("0")
    tkconfigure(pval.check,variable=pvalValue)
    pval.lab<-tklabel(sortiestop,text="P-values_ associated with the F-test in the AOV model")

    comp.check <- tkcheckbutton(sortiestop)
    if (Gcomp) compValue <- tclVar("1")
    else compValue <- tclVar("0")
    tkconfigure(comp.check,variable=compValue)
    comp.lab<-tklabel(sortiestop,text="V-test associated with the AOV model per panelist")

    vari.check <- tkcheckbutton(sortiestop)
    if (Gvari) variValue <- tclVar("1")
    else variValue <- tclVar("0")
    tkconfigure(vari.check,variable=variValue)
    vari.lab<-tklabel(sortiestop,text="variability due to the effects introduced in the AOV model")

    res2.check <- tkcheckbutton(sortiestop)
    if (Gres2) res2Value <- tclVar("1")
    else res2Value <- tclVar("0")
    tkconfigure(res2.check,variable=res2Value)
    res2.lab<-tklabel(sortiestop,text="Residual terms for the AOV model")
    
    prob.check <- tkcheckbutton(sortiestop)
    if (Gprob) probValue <- tclVar("1")
    else probValue <- tclVar("0")
    prob.lab<-tklabel(sortiestop,text="P-values associated with the AOV model")
    tkconfigure(prob.check,variable=probValue)  
    
    agr.check <- tkcheckbutton(sortiestop)
    if (Gagr) agrValue <- tclVar("1")
    else agrValue <- tclVar("0")
    tkconfigure(agr.check,variable=agrValue)
    agr.lab<-tklabel(sortiestop,text="Correlation coefficients between the product coefficients (panel/panelist)")   
    
    vtest.check <- tkcheckbutton(sortiestop)
    if (Gvtest) vtestValue <- tclVar("1")
    else vtestValue <- tclVar("0")
    tkconfigure(vtest.check,variable=vtestValue)
    vtest.lab<-tklabel(sortiestop,text="V-test associated with the AOV model")        

    res.check <- tkcheckbutton(sortiestop)
    if (Gres) resValue <- tclVar("1")
    else resValue <- tclVar("0")
    tkconfigure(res.check,variable=resValue)
    res.lab<-tklabel(sortiestop,text="Residual terms associated with the AOV model")

    r2.check <- tkcheckbutton(sortiestop)
    if (Gr2) r2Value <- tclVar("1")
    else r2Value <- tclVar("0")
    tkconfigure(r2.check,variable=r2Value)
    r2.lab<-tklabel(sortiestop,text="R2 of the AOV model")

    sign.check <- tkcheckbutton(sortiestop)
    if (Gsign) signValue <- tclVar("1")
    else signValue <- tclVar("0")
    tkconfigure(sign.check,variable=signValue)
    sign.lab<-tklabel(sortiestop,text="Significant descriptors (per panelist)")    

    synth.val <- as.numeric(tclvalue(synth.bool))
    ind.val <- as.numeric(tclvalue(ind.bool))
    if(ind.val==1){
      tkgrid(tklabel(sortiestop,text="Options to print the results for individual performance",fg="red"))
      tkgrid(prob.lab,prob.check,sticky="w")
      tkgrid(agr.lab,agr.check,sticky="w")
      tkgrid(vtest.lab,vtest.check,sticky="w")
      tkgrid(res.lab,res.check,sticky="w")
      tkgrid(r2.lab,r2.check,sticky="w")
      tkgrid(sign.lab,sign.check,sticky="w")   
    }
    if(synth.val==1){
        tkgrid(tklabel(sortiestop,text=""))
        tkgrid(tklabel(sortiestop,text="Options to print the results for panel performance",fg="red"))
        tkgrid(pval.lab,pval.check,sticky="w")
        tkgrid(comp.lab,comp.check,sticky="w")
        tkgrid(vari.lab,vari.check,sticky="w")
        tkgrid(res2.lab,res2.check,sticky="w")
    }

    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(tkbutton(sortiestop,text="OK",width=9,command=onOKsortie))
    tkfocus(sortiestop)
}

####### FRAMES -------------------------------------------------------------------------------------------------------------------------------
    factFrame <- tkframe(top)
    listfact<-tklistbox(factFrame,selectmode="single",height=7,width=20,yscrollcommand=function(...) tkset(scrfact,...))
    scrfact <- tkscrollbar(factFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
    tkgrid(listfact, scrfact,sticky = "nw")
    tkgrid.configure(scrfact, sticky = "wns")
    tkgrid.configure(listfact,sticky = "ew")

    panelFrame <- tkframe(top)
    col.lab<-tklabel(panelFrame,text="The product variable :",fg="darkgreen")
    col.val<-tclVar("")
    colp <- tkentry(panelFrame,width=15,textvariable=col.val)
    col2.lab<-tklabel(panelFrame,text="The panelist variable :",fg="darkgreen")
    col2.val<-tclVar("")
    colj <- tkentry(panelFrame,width=15,textvariable=col2.val)
    tkgrid(col.lab,colp,sticky="w")
    tkgrid(col2.lab,colj,sticky="w")

    modelsFrame <- tkframe(top)
    formul.lab<-tklabel(modelsFrame,text="AOV model used for the panel:",fg="blue")
    formul<-tclVar("")
    model <- tkentry(modelsFrame,width=45,textvariable=formul)
    modelscr <- tkscrollbar(modelsFrame,repeatinterval=5,orient="horizontal",command=function(...)tkxview(model,...))
    tkconfigure(model, xscrollcommand=function(...) tkset(modelscr, ...))
    
    formulj.lab<-tklabel(modelsFrame,text="AOV model used for each panelist: ",fg="blue")
    formulj<-tclVar("")
    modelj <- tkentry(modelsFrame,width=45,textvariable=formulj)
    cree.but<-tkbutton(top,text="Complete model",command=onFormul)
    cree2.but<-tkbutton(top,text="Complete model",command=onFormul2)
    tkgrid(formul.lab, sticky="w")
    tkgrid(model,cree.but,sticky="w")
    tkgrid(modelscr,sticky="w")
    tkgrid(formulj.lab,sticky="w")
    tkgrid(modelj,cree2.but,sticky="w")
    tkgrid(tklabel(modelsFrame,text=""))
    tkgrid.configure(modelscr,sticky="we")

    descopFrame<-tkframe(top)
    descFrame <- tkframe(descopFrame)
    listdesc<-tklistbox(descFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
    donnee<-get(.activeDataSet)
    nomdonnee<-.activeDataSet
    vars<-colnames(donnee)
    vars.fact = NULL
    vars.desc = NULL
    for (i in (1:ncol(donnee))){
      if (is.numeric(donnee[,i])){
        tkinsert(listdesc,"end",vars[i])
        vars.desc = c(vars.desc,vars[i])
      }
      else {
        vars.fact = c(vars.fact,vars[i])
        tkinsert(listfact,"end",vars[i])
      }
    }
    tkgrid(listdesc, scr,sticky = "nw")
    tkgrid.configure(scr, sticky = "wns")
    tkgrid.configure(listdesc,sticky = "ew")

    optionsFrame <- tkframe(descopFrame,borderwidth=2,relief="ridge")
    random.check <- tkcheckbutton(top)
    rnd.bool <- tclVar("1")
    tkconfigure(random.check,variable=rnd.bool)
    random.lab<-tklabel(optionsFrame,text="Random effect for 'Panelist'")
#    graph.check <- tkcheckbutton(top)
#    graph.bool <- tclVar("0")
#    tkconfigure(graph.check,variable=graph.bool)
#    graph.lab<-tklabel(optionsFrame,text="Print graph associated:")
    ind.check <- tkcheckbutton(top)
    ind.bool <- tclVar("1")
    tkconfigure(ind.check,variable=ind.bool)
    ind.lab<-tklabel(optionsFrame,text="Results for the individual performance:")
    synth.check <- tkcheckbutton(top)
    synth.bool <- tclVar("1")
    tkconfigure(synth.check,variable=synth.bool)
    synth.lab<-tklabel(optionsFrame,text="Results for the panel performance:")
    resu.val<-tclVar("results")
    resu<-tkentry(optionsFrame,width=10,textvariable=resu.val)
    resu.lab<-tklabel(optionsFrame,text="Keep the results in : ")
    tkgrid(random.lab,random.check,sticky="w")
    tkgrid(synth.lab,synth.check,sticky="w")
    tkgrid(ind.lab,ind.check,sticky="w")
#    tkgrid(graph.lab,graph.check,sticky="w")
    tkgrid(resu.lab,resu,sticky="w")


####### Fonction liée au bouton Appliquer, appel de la fonction Paneliperf -------------------------------------------------------------------------------
App<-function(){
    nbitemlist<-c(tclvalue(tkcurselection(listdesc)))
    nbitem<-unlist(strsplit(nbitemlist,"\\ "))
    modelej<-tclvalue(formulj)
    modele<-tclvalue(formul)
    col.nam<-tclvalue(col2.val)
    for (i in 1:length(vars.fact)){
        if (vars.fact[i]==col.nam) col.pos<- i
    }
    variable <- vars.desc[as.numeric(tkcurselection(listdesc))+1]
    rnd.val <- as.numeric(tclvalue(rnd.bool))
#    graph.val <- as.numeric(tclvalue(graph.bool))
    ind.val <- as.numeric(tclvalue(ind.bool))
    synth.val <- as.numeric(tclvalue(synth.bool))
    resultat<-tclvalue(resu.val)
    if (modele=="") tkmessageBox(message="You must fill in the grid 'Model formula'",icon="warning",type="ok")
    else{
##      if (ind.val==0) {
##        if (length(nbitem)>1) command3=paste(resultat,'=panelperf(',nomdonnee,'[,c("', paste(vars.fact, collapse='", "'), '", "', paste(variable, collapse='", "'), '")],firstvar=',length(vars.fact)+1,',formul=~"',modele,'",random=',rnd.val,')',sep='')
##        else command3=paste(resultat,'=panelperf(',nomdonnee,'[,c("', paste(vars.fact, collapse='", "'), '", "', paste(vars.desc, collapse='", "'), '")],firstvar=',length(vars.fact)+1,',formul="~',modele,'",random=',rnd.val,')',sep='')
##        justDoIt(command3)
##        logger(command3)
##      }
##      else {
        if (col.nam=="") tkmessageBox(message="No variable selected for the panelists",icon="warning",type="ok")
        if (modelej=="") tkmessageBox(message="You must fill in the grid 'AOV model per panelist'",icon="warning",type="ok")
        if (col.nam%in%unlist(strsplit(modelej,split="\\+"))) tkmessageBox(message="The 'panelist' factor should not be present in the 'AOV model per panelist'",icon="warning",type="ok")
        done = 0
        if ((col.nam!="")&(modelej!="")&(!(col.nam%in%unlist(strsplit(modelej,split="\\+"))))){
          done = 1  
          if (length(nbitem)>1) command3=paste(resultat,'=paneliperf(',nomdonnee,'[,c("', paste(vars.fact, collapse='", "'), '", "', paste(variable, collapse='", "'), '")],firstvar=',length(vars.fact)+1,',formul=~"',modele,'",formul.j=~"',modelej,'",random=',rnd.val,',col.j=',col.pos,',synthesis=',synth.val,')',sep='')
          else command3=paste(resultat,'=paneliperf(',nomdonnee,'[,c("', paste(vars.fact, collapse='", "'), '", "', paste(vars.desc, collapse='", "'), '")],firstvar=',length(vars.fact)+1,',formul="~',modele,'",formul.j=~"',modelej,'",random=',rnd.val,',col.j=',col.pos,',synthesis=',synth.val,')',sep='')
          justDoIt(command3)
          logger(command3)
          
          if (ind.val==1){      
          if (Gprob==1){
            doItAndPrint(paste(resultat,'$prob.ind',sep='')) 
            command5=paste('resprob<-magicsort(',resultat,'$prob.ind, method = "median")',sep='')
            justDoIt(command5)
            logger(command5)
            command6=paste('coltable(resprob, level.lower = 0.05, level.upper = 1,main.title = "P-value of the F-test (by panelist)")',sep='')
            doItAndPrint(command6)              
          }
          if (Gvtest==1){
            doItAndPrint(paste(resultat,'$vtest.ind',sep='')) 
          }
          if (Gres==1){
            doItAndPrint(paste(resultat,'$res.ind',sep='')) 
          }
          if (Gr2==1){
            doItAndPrint(paste(resultat,'$r2.ind',sep='')) 
            command7=paste('resr2<-magicsort(',resultat,'$r2.ind, method = "median", ascending = FALSE)',sep='')
            justDoIt(command7)
            logger(command7)
            command8=paste('coltable(resr2, level.lower = 0.00, level.upper = 0.85,main.title = "Adjusted R-square (by panelist)")',sep='')
            doItAndPrint(command8)    
          }
          if (Gsign==1){
            doItAndPrint(paste(resultat,'$signif.ind',sep='')) 
          }
          if (Gagr==1){
            doItAndPrint(paste(resultat,'$agree.ind',sep='')) 
            command9=paste('resagree<-magicsort(',resultat,'$agree, method = "median", ascending = FALSE)',sep='')
            justDoIt(command9)
            logger(command9)
            command10=paste('coltable(resagree, level.lower = 0.00, level.upper = 0.85,main.title = "Agreement between panelists")',sep='')
            doItAndPrint(command10)                       
          }
      }
#      if ((synth.val==1)|(ind.val==0)){
          if (synth.val==1){
            if (Gpval==1){
              doItAndPrint(paste(resultat,'$p.value',sep='')) 
              commanda=paste('coltable(magicsort(',resultat,'$p.value, sort.mat = ',resultat,'$p.value[,1], bycol = FALSE,method = "median"),main.title = "Panel performance (sorted by product P-value)")',sep='')
              doItAndPrint(commanda)
            }
            if (Gcomp==1){
              doItAndPrint(paste(resultat,'$complete',sep='')) 
            }
            if (Gvari==1){
              doItAndPrint(paste(resultat,'$variability',sep='')) 
            }
            if (Gres2==1){
              doItAndPrint(paste(resultat,'$res',sep='')) 
            }
          }
        }
    }
    return(done)
  }


####### Fonction liée au bouton OK, appel de la fonction Paneliperf -------------------------------------------------------------------------------
onOK <- function(){
  done = App()
  if (done >0) tkdestroy(top)
}


########### Effet d'un double-clic dans la liste des facteurs -----------------------------------------------------------------------------------------
doubleclick<-function(){
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    if(col.nam==""){
      col.val <- vars.fact[as.numeric(tkcurselection(listfact))+1]
      tkinsert(colp,"end",col.val)
      tkinsert(model,"end",col.val)
      tkinsert(modelj,"end",col.val)
    }
    else{
      if(col2.nam==""){    
        col2.val <- vars.fact[as.numeric(tkcurselection(listfact))+1]
        tkinsert(colj,"end",col2.val)
        tkinsert(model,"end",c('+',col2.val))
      }
    }
}

sorties<-tkbutton(optionsFrame,text="Outputs",borderwidth=3,width=12,fg="darkred",command=onSortie)
appel<-tkbutton(top,text="Submit",borderwidth=3,width=12,fg="blue",command=App)
OKCancelHelp(helpSubject="paneliperf")
tkgrid(tklabel(top, text = "Choose the product and panelist variables (double-click) :", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(factFrame,panelFrame, sticky="w")
tkgrid(tklabel(top,text=""))
tkgrid(modelsFrame,columnspan = 2, sticky="w")
tkgrid(tklabel(top,text=""))
tkgrid(tklabel(top, text = "Select descriptors (by default, all descriptors are selected)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(descopFrame,columnspan = 2, sticky="w")
tkgrid(sorties,sticky="e")
tkgrid(descFrame,tklabel(descopFrame,text="    "),optionsFrame,sticky="w")
tkgrid.configure(optionsFrame,sticky="s")
tkbind(model,"<Return>",onOK)
tkbind(listfact,"<Double-ButtonPress-1>",doubleclick)
tkgrid(tklabel(top,text=""))
tkgrid(appel, sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/paneliperf.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}

#########################################################       FIN FONCTION PANELIPERF        ########################################################
#######################################################################################################################################################


#################################################################################################################################################
###################################################       FONCTION PANELPERF  #####################################################

SensPanelperf<-function(){
require(tcltk)
require(SensoMineR)
top<-tktoplevel(borderwidth=10)
tkwm.title(top,"Panel performance")

####### Fonction  intermédiaire (bouton Créer Formule) --------------------------------------------------------------------------
onFormul<-function(){
    topformul<-tktoplevel(borderwidth=10)
    tkfocus(topformul)
    tkwm.title(topformul,"Global model formula")
    plus.funct <- function(){tkinsert(model.par,"end","+")}
    interact.funct <- function(){tkinsert(model.par,"end",":")}
    
    modelFrame <- tkframe(topformul,borderwidth=3)
    opFrame <- tkframe(modelFrame,borderwidth=2)
    formul.lab.par<-tklabel(opFrame,text="Model Formula :      ")
    plus.but <- tkbutton(opFrame,text="+",width=3,command=plus.funct)
    interact.but <- tkbutton(opFrame,text=":",width=3,command=interact.funct)
    tkgrid(formul.lab.par,plus.but,interact.but, sticky="s")
    formulFrame <- tkframe(modelFrame,borderwidth=2)
    formul.par<-tclVar(tclvalue(formul))
    model.par <- tkentry(formulFrame,width=45,textvariable=formul.par)
    tkgrid(model.par,sticky="sw")
    
    factFrame<-tkframe(modelFrame)
        listfact<-tklistbox(factFrame,selectmode="single",height=5,width=20)
        for (i in (1:ncol(donnee))){
            if (is.factor(donnee[,i])) tkinsert(listfact,"end",vars[i])
        }
        scrfact <- tkscrollbar(factFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
        tkgrid(listfact,scrfact, sticky = "nw")
        tkgrid.configure(scrfact,sticky = "wns")
        tkgrid.configure(listfact,sticky = "w")    
    
    onCreate<-function(){
        tclvalue(formul)<-""
        tkinsert(model,"end",tclvalue(formul.par))
        tkdestroy(topformul)
        tkfocus(top)
    }
    
    create.but<-tkbutton(topformul,text="OK",width=9,command=onCreate)
    
    doubleclick<-function(){
        formul.par <- vars.fact[as.numeric(tkcurselection(listfact))+1]
        tkinsert(model.par,"end",formul.par)
    }
    
    tkgrid(tklabel(topformul, text = "Factors (double-click to formula)", fg = "blue"), columnspan = 3, sticky = "w")
    tkgrid(listfact,sticky = "nw")
    tkgrid(modelFrame)
    tkgrid(factFrame)
    tkgrid(opFrame)
    tkgrid(formulFrame)
    tkgrid(create.but)
    tkbind(listfact,"<Double-ButtonPress-1>",doubleclick)
}

#################### Fonctions liées au bouton 'Sorties' --------------------------------------------------------------------------------------------------

env<-environment()
#Gcomp<-FALSE
Gpval<-TRUE
Gvari<-FALSE
Gres2<-FALSE
Gr22<-FALSE
    
onSortie<-function(){
    sortiestop<-tktoplevel(borderwidth=10)
    tkwm.title(sortiestop,"Outputs")
    
    
    onOKsortie<-function(){
        if(tclvalue(pvalValue)=="1") assign("Gpval", TRUE, envir=env)
        else assign("Gpval", FALSE, envir=env)
        #if(tclvalue(compValue)=="1") assign("Gcomp", TRUE, envir=env)
        #else assign("Gcomp", FALSE, envir=env)
        if(tclvalue(variValue)=="1") assign("Gvari", TRUE, envir=env)
        else assign("Gvari", FALSE, envir=env)
        if(tclvalue(res2Value)=="1") assign("Gres2", TRUE, envir=env)
        else assign("Gres2", FALSE, envir=env)
        if(tclvalue(r22Value)=="1") assign("Gr22", TRUE, envir=env)
        else assign("Gr22", FALSE, envir=env)        
        tkdestroy(sortiestop)
    }

    pval.check <- tkcheckbutton(sortiestop)
    if (Gpval) pvalValue <- tclVar("1")
    else pvalValue <- tclVar("0")
    tkconfigure(pval.check,variable=pvalValue)
    pval.lab<-tklabel(sortiestop,text="P-values_ associated with the F-test in the AOV model")

    #comp.check <- tkcheckbutton(sortiestop)
    #if (Gcomp) compValue <- tclVar("1")
    #else compValue <- tclVar("0")
    #tkconfigure(comp.check,variable=compValue)
    #comp.lab<-tklabel(sortiestop,text="V-test associated with the AOV model per panelist")

    vari.check <- tkcheckbutton(sortiestop)
    if (Gvari) variValue <- tclVar("1")
    else variValue <- tclVar("0")
    tkconfigure(vari.check,variable=variValue)
    vari.lab<-tklabel(sortiestop,text="Variability due to the effects introduced in the AOV model")

    res2.check <- tkcheckbutton(sortiestop)
    if (Gres2) res2Value <- tclVar("1")
    else res2Value <- tclVar("0")
    tkconfigure(res2.check,variable=res2Value)
    res2.lab<-tklabel(sortiestop,text="Residual terms for the AOV model")
    
    r22.check <- tkcheckbutton(sortiestop)
    if (Gr22) r22Value <- tclVar("1")
    else r22Value <- tclVar("0")
    tkconfigure(r22.check,variable=r22Value)
    r22.lab<-tklabel(sortiestop,text="R-squared for the AOV model")    

    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(tklabel(sortiestop,text="Options to print the results for panel performance",fg="red"))
    tkgrid(pval.lab,pval.check,sticky="w")
    #tkgrid(comp.lab,comp.check,sticky="w")
    tkgrid(vari.lab,vari.check,sticky="w")
    tkgrid(res2.lab,res2.check,sticky="w")
    tkgrid(r22.lab,r22.check,sticky="w")

    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(tkbutton(sortiestop,text="OK",width=9,command=onOKsortie))
    tkfocus(sortiestop)
}

####### FRAMES -------------------------------------------------------------------------------------------------------------------------------
    factFrame <- tkframe(top)
    listfact<-tklistbox(factFrame,selectmode="single",height=7,width=20,yscrollcommand=function(...) tkset(scrfact,...))
    scrfact <- tkscrollbar(factFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
    tkgrid(listfact, scrfact,tklabel(factFrame,text="   "),sticky = "nw")
    tkgrid.configure(scrfact, sticky = "wns")
    tkgrid.configure(listfact,sticky = "ew")
    
    interestFrame <- tkframe(top)
    col.lab<-tklabel(interestFrame,text="Factor 'Product' : ",fg="darkgreen")
    col.val<-tclVar("")
    colp <- tkentry(interestFrame,width=15,textvariable=col.val)  
    col2.lab<-tklabel(interestFrame,text="Factor 'Panelist' : ",fg="darkgreen")
    col2.val<-tclVar("")
    colj <- tkentry(interestFrame,width=15,textvariable=col2.val)     
    tkgrid(col.lab, colp, sticky = "w")
    tkgrid(col2.lab,colj,sticky="w")    

    modelsFrame <- tkframe(top)
    formul.lab<-tklabel(modelsFrame,text="AOV model used for the panel:",fg="blue")
    tkgrid(formul.lab,sticky="sw")
    formul<-tclVar("")
    model <- tkentry(modelsFrame,width=45,textvariable=formul)
    modelscr <- tkscrollbar(modelsFrame,repeatinterval=5,orient="horizontal",command=function(...)tkxview(model,...))
    tkconfigure(model, xscrollcommand=function(...) tkset(modelscr, ...))
    
    cree.but<-tkbutton(top,text="Complete model",command=onFormul)
    tkgrid(formul.lab, sticky="w")
    tkgrid(model,cree.but,sticky="w")
    tkgrid(modelscr,sticky="w")
#    tkgrid(tklabel(modelsFrame,text=""))
#    tkgrid.configure(modelscr,sticky="we")

    descFrame <- tkframe(top)
    listdesc<-tklistbox(descFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
    donnee<-get(.activeDataSet)
    nomdonnee<-.activeDataSet
    vars<-colnames(donnee)
    vars.fact = NULL
    vars.desc = NULL
    for (i in (1:ncol(donnee))){
      if (is.numeric(donnee[,i])){
        tkinsert(listdesc,"end",vars[i])
        vars.desc = c(vars.desc,vars[i])
      }
      else {
        vars.fact = c(vars.fact,vars[i])
        tkinsert(listfact,"end",vars[i])
      }
    }
    tkselection.set(listdesc,0)
    tkgrid(listdesc, scr,sticky = "nw")
    tkgrid.configure(scr, sticky = "wns")
    tkgrid.configure(listdesc,sticky = "ew")
    tkselection.set(listdesc,0)

    optionsFrame <- tkframe(top,relief="ridge",borderwidth=2)
    random.check <- tkcheckbutton(optionsFrame)
    rnd.bool <- tclVar("1")
    tkconfigure(random.check,variable=rnd.bool)
    random.lab<-tklabel(optionsFrame,text="Random effect for 'Panelist'")
    graph.check <- tkcheckbutton(optionsFrame)
    graph.bool <- tclVar("0")
    tkconfigure(graph.check,variable=graph.bool)
    graph.lab<-tklabel(optionsFrame,text="Print graph associated:")
    resu.val<-tclVar("results")
    resu<-tkentry(optionsFrame,width=10,textvariable=resu.val)
    resu.lab<-tklabel(optionsFrame,text="Keep the results in : ")
    tkgrid(random.lab,random.check,sticky="w")
    tkgrid(graph.lab,graph.check,sticky="w")
    tkgrid(resu.lab,resu,sticky="w")

########### Effet d'un double-clic dans la liste des facteurs -----------------------------------------------------------------------------------------

    doubleclick2<-function(){
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    if(col.nam==""){
      col.val <- vars.fact[as.numeric(tkcurselection(listfact))+1]
      tkinsert(colp,"end",col.val)
      tkinsert(model,"end",col.val)
    }
    else{
      if(col2.nam==""){    
        col2.val <- vars.fact[as.numeric(tkcurselection(listfact))+1]
        tkinsert(colj,"end",col2.val)
        tkinsert(model,"end",c('+',col2.val))
      }
    }
}
####### Fonction liée au bouton Appliquer, appel de la fonction Panelperf -------------------------------------------------------------------------------
App<-function(){
    nbitemlist<-c(tclvalue(tkcurselection(listdesc)))
    nbitem<-unlist(strsplit(nbitemlist,"\\ "))
    modele<-tclvalue(formul)
    variable <- vars.desc[as.numeric(tkcurselection(listdesc))+1]
    rnd.val <- as.numeric(tclvalue(rnd.bool))
    graph.val <- as.numeric(tclvalue(graph.bool))
    resultat<-tclvalue(resu.val)
    done = 0
    if (modele=="") tkmessageBox(message="You must fill in the grid 'Model formula'",icon="warning",type="ok")
    else{
      done = 1
      if (length(nbitem)>1) command3=paste(resultat,'=panelperf(',nomdonnee,'[,c("', paste(vars.fact, collapse='", "'), '", "', paste(variable, collapse='", "'), '")],firstvar=',length(vars.fact)+1,',formul=~"',modele,'",random=',rnd.val,')',sep='')
      else command3=paste(resultat,'=panelperf(',nomdonnee,'[,c("', paste(vars.fact, collapse='", "'), '", "', paste(vars.desc, collapse='", "'), '")],firstvar=',length(vars.fact)+1,',formul="~',modele,'",random=',rnd.val,')',sep='')
      justDoIt(command3)
      logger(command3)
      
      if (Gpval==1){
        doItAndPrint(paste(resultat,'$p.value',sep='')) 
        if (graph.val==1){
        commanda=paste('coltable(magicsort(',resultat,'$p.value, sort.mat = ',resultat,'$p.value[,1], bycol = FALSE,method = "median"),main.title = "Panel performance (sorted by product P-value)")',sep='')
        doItAndPrint(commanda)
        }
      }
      #if (Gcomp==1){
      #  commandb=paste('-qnorm(',resultat,'$p.value/2)',sep='')
      #  doItAndPrint(commandb)      
      #  doItAndPrint(paste(resultat,'$complete',sep='')) 
      #}
      if (Gvari==1){
        doItAndPrint(paste(resultat,'$variability',sep='')) 
      }
      if (Gres2==1){
        doItAndPrint(paste(resultat,'$res',sep='')) 
      }
      if (Gr22==1){
        doItAndPrint(paste(resultat,'$r2',sep='')) 
      }      
    }
    return(done)
  }


####### Fonction liée au bouton OK, appel de la fonction Panelperf -------------------------------------------------------------------------------
onOK <- function(){
  done = App()
  if (done >0) tkdestroy(top)
}

sorties<-tkbutton(optionsFrame,text="Outputs",borderwidth=3,width=12,fg="darkred",command=onSortie)
appel<-tkbutton(top,text="Submit",borderwidth=3,width=12,fg="blue",command=App)
OKCancelHelp(helpSubject="panelperf")
tkgrid(tklabel(top, text = "Choose the product and panelist variables (double-click) :", fg = "blue"), columnspan = 2, sticky = "w") 
tkgrid(factFrame,interestFrame,sticky="w")
tkgrid(tklabel(top,text=""))
tkgrid(modelsFrame,columnspan = 2,sticky="w")
tkgrid(tklabel(top,text=""))
tkgrid(tklabel(top, text = "Select descriptors (by default, all descriptors are selected)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(descFrame,optionsFrame,sticky="w")
tkgrid.configure(optionsFrame,column=1, columnspan=2)
tkgrid(tklabel(top,text=""))
tkbind(model,"<Return>",onOK)
tkbind(listfact,"<Double-ButtonPress-1>",doubleclick2)
tkgrid(appel, sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/panelperf.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}
 
########       FIN FONCTION PANELPERF         #######


#################################################################################################################################################
###################################################       FONCTION  SCALEBYPANELIST #####################################################

SensScaleby<-function(){
require(tcltk)
require(SensoMineR)
top<-tktoplevel(borderwidth=10)
tkwm.title(top,"Scale by panelist")
donnee<-get(.activeDataSet)
nomdonnee<-.activeDataSet

######## FRAMES -----------------------------------------------------------------------------------------------------------------------

factFrame <- tkframe(top)
    listfact<-tklistbox(factFrame,selectmode="extended",yscrollcommand=function(...) tkset(scrfact,...))
    scrfact <- tkscrollbar(factFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
    tkgrid(listfact, scrfact,sticky = "nw")
    tkgrid.configure(scrfact, sticky = "wns")
    tkgrid.configure(listfact,sticky = "ew")

interestFrame <- tkframe(top)
    col.lab<-tklabel(interestFrame,text="'Product' factor: ",fg="darkgreen")
    col.val<-tclVar("")
    colj <- tkentry(interestFrame,width=15,textvariable=col.val)
    col2.lab<-tklabel(interestFrame,text="'Panelist' factor: ",fg="darkgreen")
    col2.val<-tclVar("")
    col2j <- tkentry(interestFrame,width=15,textvariable=col2.val)
    
    tkgrid(col.lab, colj,sticky = "nw")
    tkgrid(col2.lab, col2j,sticky = "nw")

descopframe<-tkframe(top)
   descFrame <- tkframe(descopframe)
   listdesc<-tklistbox(descFrame,selectmode="extended",yscrollcommand=function(...) tkset(scr,...))
   scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
   vars<-colnames(donnee)
   vars.fact = NULL
   vars.desc = NULL
   for (i in (1:ncol(donnee))){
     if (is.numeric(donnee[,i])){
       tkinsert(listdesc,"end",vars[i])
       vars.desc = c(vars.desc,vars[i])
     }
     else {
       vars.fact = c(vars.fact,vars[i])
       tkinsert(listfact,"end",vars[i])
     }
   }
   tkgrid(listdesc, scr,sticky = "nw")
   tkgrid.configure(scr, sticky = "wns")
   tkgrid.configure(listdesc,sticky = "ew")

optionsFrame<-tkframe(descopframe,borderwidth=2,relief="ridge")
    center.check <- tkcheckbutton(top)
    center.bool <- tclVar("1")
    tkconfigure(center.check,variable=center.bool)
    center.lab<-tklabel(optionsFrame,text="Center data by 'Panelist': ")
    scale.check <- tkcheckbutton(top)
    scale.bool <- tclVar("0")
    tkconfigure(scale.check,variable=scale.bool)
    scale.lab<-tklabel(optionsFrame,text="Scale data by 'Panelist': ")
    methodFrame <- tkframe(optionsFrame)
       mean.rb <- tkradiobutton(methodFrame)
       coef.rb <- tkradiobutton(methodFrame)
       rbValue <- tclVar("coeff")
       tkconfigure(mean.rb,variable=rbValue,value="mean")
       tkconfigure(coef.rb,variable=rbValue,value="coeff")
       tkgrid(tklabel(methodFrame,text="Method : "),tklabel(methodFrame,text="Mean"),mean.rb)
       tkgrid(tklabel(methodFrame,text=""),tklabel(methodFrame,text="Coefficient"),coef.rb)
   resu.val<-tclVar("results")
   resu<-tkentry(optionsFrame,width=10,textvariable=resu.val)
   resu.lab<-tklabel(optionsFrame,text="Keep the result in : ")
   tkgrid(center.lab,center.check,sticky="w")
   tkgrid(scale.lab,scale.check,sticky="w")
   tkgrid(resu.lab,resu,sticky="w")


####### Fonction attachée aux boutons OK, appel de scalebypanelist -------------------------------------------------------------------------------------
onOK<-function(){
    App()
    tkdestroy(top)
}

App<-function(){
    centre <- as.numeric(tclvalue(center.bool))
    reduit <- as.numeric(tclvalue(scale.bool))
    method.val <- as.character(tclvalue(rbValue))
    nbitemlist<-c(tclvalue(tkcurselection(listdesc)))
    nbitem<-unlist(strsplit(nbitemlist,"\\ "))
    variable <- vars.desc[as.numeric(tkcurselection(listdesc))+1]
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    for (i in 1:length(vars.fact)){
        if (vars.fact[i]==col.nam) col.pos<-i
        if (vars.fact[i]==col2.nam) col2.pos<-i
    }
    resultat<-tclvalue(resu.val)
    if (col.nam=="") tkmessageBox(message="No variable selected for the first factor of interest",icon="warning",type="ok")
    if (col2.nam=="") tkmessageBox(message="No variable selected for the second factor of interest",icon="warning",type="ok")
    if ((col.nam!="")&(col2.nam!="")){
      if (length(nbitem)>1) command3=paste(resultat,'=scalebypanelist(',nomdonnee,'[,c("',paste(vars.fact, collapse='", "'), '", "', paste(variable, collapse='", "'), '")],col.p=',col.pos,',col.j=',col2.pos,',firstvar=',length(vars.fact)+1,',center=',centre,',scale=',reduit,',method="',method.val,'")',sep='')
      else command3=paste(resultat,'=scalebypanelist(',nomdonnee,'[,c("',paste(vars.fact, collapse='", "'), '", "', paste(vars.desc, collapse='", "'), '")],col.p=',col.pos,',col.j=',col2.pos,',firstvar=',length(vars.fact)+1,',center=',centre,',scale=',reduit,',method="',method.val,'")',sep='')
      justDoIt(command3)
      logger(command3)
    }
}


doubleclick<-function(){
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    if(col.nam==""){
      col.val <- vars.fact[as.numeric(tkcurselection(listfact))+1]
      tkinsert(colj,"end",col.val)}
    else{
      if(col2.nam==""){
        col2.val <- vars.fact[as.numeric(tkcurselection(listfact))+1]
        tkinsert(col2j,"end",col2.val)
      }
    }
}
tkbind(listfact,"<Double-ButtonPress-1>",doubleclick)


app.but<-tkbutton(top,text="Submit",command=App,fg="blue",width=13,borderwidth=3)
OKCancelHelp(helpSubject="scalebypanelist")
tkgrid(tklabel(top, text = "Factors (double-click to select factors of interest)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(factFrame,interestFrame,sticky="w")
tkgrid(tklabel(top,text=""))
tkgrid(tklabel(top, text = "Select descriptors (by default, all descriptors are selected)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(descopframe,sticky="w",columnspan=2)
tkgrid(descFrame,tklabel(top,text="   "),optionsFrame,sticky="w")
tkgrid.configure(optionsFrame,sticky="es")
tkgrid(tklabel(top,text=""))
tkgrid(app.but,sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/scalebypanelist.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}
###################################################################          Fin FONCTION   SCALEBYPANELIST              ################################################
###########################################################################################################################################################


###########################################################################################################################################################
###################################################################          FONCTION   PANELLIPSE              ################################################

SensPanellipse<-function(){
    require(tcltk)
    require(SensoMineR)
    top<-tktoplevel(borderwidth=5)
    tkwm.title(top,"Multidimensional sensory profile: panellipse")
    donnee<-get(.activeDataSet)
    nomdonnee<-.activeDataSet

######## FRAMES -----------------------------------------------------------------------------------------------------------------------

factFrame <- tkframe(top)
    listfact<-tklistbox(factFrame,selectmode="extended",yscrollcommand=function(...) tkset(scrfact,...))
    scrfact <- tkscrollbar(factFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
    tkgrid(listfact, scrfact,sticky = "nw")
    tkgrid.configure(scrfact, sticky = "wns")
    tkgrid.configure(listfact,sticky = "ew")

interestFrame <- tkframe(top)
    col.lab<-tklabel(interestFrame,text="'Product' factor: ",fg="darkgreen")
    col.val<-tclVar("")
    colj <- tkentry(interestFrame,width=15,textvariable=col.val)
    col2.lab<-tklabel(interestFrame,text="'Panelist' factor: ",fg="darkgreen")
    col2.val<-tclVar("")
    col2j <- tkentry(interestFrame,width=15,textvariable=col2.val)
    
    tkgrid(col.lab, colj,sticky = "nw")
    tkgrid(col2.lab, col2j,sticky = "nw")

descopframe<-tkframe(top)
   descFrame <- tkframe(descopframe)
   listdesc<-tklistbox(descFrame,selectmode="extended",yscrollcommand=function(...) tkset(scr,...))
   scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
   vars<-colnames(donnee)
   vars.fact = NULL
   vars.desc = NULL
   for (i in (1:ncol(donnee))){
     if (is.numeric(donnee[,i])){
       tkinsert(listdesc,"end",vars[i])
       vars.desc = c(vars.desc,vars[i])
     }
     else {
       vars.fact = c(vars.fact,vars[i])
       tkinsert(listfact,"end",vars[i])
     }
   }
   tkgrid(listdesc, scr,sticky = "nw")
   tkgrid.configure(scr, sticky = "wns")
   tkgrid.configure(listdesc,sticky = "ew")


  #! fonction pour la gestion des options graphiques
  couleur.a.changer<-defmacro(label, firstLabel, expr=
  {
    optionsFrame2 <- tkframe(optionsFrame,borderwidth=3)
    .PlotLabel<-tclVar(paste(firstLabel, "", sep=" "))
    env<-environment()
    Rgraph <- TRUE
    Rcenterpan <- TRUE
    Rscalepan <- FALSE
    Rnamepan <- FALSE    
    Rchoix <- "NULL"
    Rsimul <- 500
    Raxe <- "1,2"
    
    OnPlot<-function()
    {
      PlotWin<-tktoplevel()
      tkwm.title(PlotWin,gettextRcmdr("Options"))
      tkwm.geometry(PlotWin, "-100+50")

      #création de la fonction onOKsub
      onOKsub<-function()
      {
        if(tclvalue(cbValue)=="1") assign("Rgraph", TRUE, envir=env)
        else assign("Rgraph", FALSE, envir=env)
        if(tclvalue(centerpan.bool)=="1") assign("Rcenterpan", TRUE, envir=env)
        else assign("Rcenterpan", FALSE, envir=env)
        if(tclvalue(scalepan.bool)=="1") assign("Rscalepan", TRUE, envir=env)
        else assign("Rscalepan", FALSE, envir=env)
        if(tclvalue(namepan.bool)=="1") assign("Rnamepan", TRUE, envir=env)
        else assign("Rnamepan", FALSE, envir=env)
        assign("Rsimul",tclvalue(simul.val),envir=env)
        assign("Raxe",tclvalue(axe.val),envir=env)
        assign("Rchoix",tclvalue(choix.val),envir=env)
        tkdestroy(PlotWin)
      }

      graph.check <- tkcheckbutton(PlotWin)
      if (Rgraph) cbValue <- tclVar("1")
      else cbValue <- tclVar("0")
      tkconfigure(graph.check,variable=cbValue)
      graph.lab<-tklabel(PlotWin,text="Plot the graph with the variability of the variables")

      centerpan.check <- tkcheckbutton(PlotWin)
      if (Rcenterpan) centerpan.bool <- tclVar("1")
      else centerpan.bool <- tclVar("0")
      tkconfigure(centerpan.check,variable=centerpan.bool)
      centerpan.lab<-tklabel(PlotWin,text="Center data by panelist")
        
      scalepan.check <- tkcheckbutton(PlotWin)
      if (Rscalepan) scalepan.bool <- tclVar("1")
      else scalepan.bool <- tclVar("0")
      tkconfigure(scalepan.check,variable=scalepan.bool)
      scalepan.lab<-tklabel(PlotWin,text="Scale data by panelist")
      
      namepan.check <- tkcheckbutton(PlotWin)
      if (Rnamepan) namepan.bool <- tclVar("1")
      else namepan.bool <- tclVar("0")
      tkconfigure(namepan.check,variable=namepan.bool)
      namepan.lab<-tklabel(PlotWin,text="Name of each panelist is displayed on the 'plotpanelist' graph ")

      axe.lab<-tklabel(PlotWin,text="Dimension to plot")
      axe.val<-tclVar(Raxe)
      axe.ent <- tkentry(PlotWin,width=6,textvariable=axe.val)
      
      simul.lab<-tklabel(PlotWin,text="Number of simulations used to compute the ellipses")
      simul.val<-tclVar(Rsimul)
      simul.ent <- tkentry(PlotWin,width=15,textvariable=simul.val)
      
      choix.lab<-tklabel(PlotWin,text="Number of panelists forming a virtual panel (if NULL, the number in the true panel) ")
      choix.val<-tclVar(Rchoix)
      choix.ent <- tkentry(PlotWin,width=15,textvariable=choix.val)

      #mise en page des différents frames de PlotIndFrame
      tkgrid(tklabel(PlotWin, text=" "))
      tkgrid(axe.lab,axe.ent,sticky="w")
      tkgrid(simul.lab,simul.ent,sticky="w")
      tkgrid(choix.lab,choix.ent,sticky="w")
      tkgrid(centerpan.lab,centerpan.check,sticky="w")
      tkgrid(scalepan.lab,scalepan.check,sticky="w")
      tkgrid(namepan.lab,namepan.check,sticky="w")
      tkgrid(graph.lab,graph.check,sticky="w")

      subOKCancelHelp(PlotWin, helpSubject=NULL)
      tkgrid(subButtonsFrame, sticky="ew")
    }
    Plot.but<-tkbutton(optionsFrame2, textvariable=.PlotLabel, command=OnPlot, borderwidth=3, width=13)
    tkgrid(Plot.but, sticky="e")
  })

optionsFrame <- tkframe(top,relief="ridge",borderwidth=2)
    scale.check <- tkcheckbutton(top)
    scale.bool <- tclVar("1")
    tkconfigure(scale.check,variable=scale.bool)
    scale.lab<-tklabel(optionsFrame,text="Scale unit the variables:")

    searchlevel.val<-tclVar("0.20")
    searchlevel<-tkentry(optionsFrame,width=5,textvariable=searchlevel.val)
    searchlevel.lab<-tklabel(optionsFrame,text="Significant level to select descriptors: ")
    seuil.val<-tclVar("0.05")
    seuil<-tkentry(optionsFrame,width=5,textvariable=seuil.val)
    seuil.lab<-tklabel(optionsFrame,text="Significant level for the ellipses: ")
    resu.val<-tclVar("results")
    resu<-tkentry(optionsFrame,width=10,textvariable=resu.val)
    resu.lab<-tklabel(optionsFrame,text="Keep the results in: ")
    tkgrid(scale.lab,scale.check,sticky="w")
    tkgrid(searchlevel.lab,searchlevel,sticky="w")
    tkgrid(seuil.lab,seuil,sticky="w")
    tkgrid(resu.lab,resu,sticky="w")
    couleur.a.changer(label=gettextRcmdr("Options"), firstLabel=gettextRcmdr("Options"))

descFrame <- tkframe(top)
    listdesc<-tklistbox(descFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
    vars<-colnames(donnee)
    vars.desc = NULL
    for (i in (1:ncol(donnee))){
      if (is.numeric(donnee[,i])){
        tkinsert(listdesc,"end",vars[i])
        vars.desc = c(vars.desc,vars[i])
      }
    }
    tkselection.set(listdesc,0)
    tkgrid(listdesc, scr,sticky = "nw")
    tkgrid.configure(scr, sticky = "wns")
    tkgrid.configure(listdesc,sticky = "ew")
    tkselection.set(listdesc,0)


#################### Fonctions liée au bouton 'Sorties' --------------------------------------------------------------------------------------------------
    env<-environment()
    Geig<-TRUE
    Gcoord<-FALSE
    Gcoef<-FALSE
    Ghotel<-TRUE

onSortie<-function(){
    sortiestop<-tktoplevel(borderwidth=10)
    tkwm.title(sortiestop,"Outputs")

    onOKsortie<-function(){
        if(tclvalue(eigValue)=="1") assign("Geig", TRUE, envir=env)
        else assign("Geig", FALSE, envir=env)
        if(tclvalue(coordValue)=="1") assign("Gcoord", TRUE, envir=env)
        else assign("Gcoord", FALSE, envir=env)
        if(tclvalue(coefValue)=="1") assign("Gcoef", TRUE, envir=env)
        else assign("Gcoef", FALSE, envir=env)
        if(tclvalue(hotelValue)=="1") assign("Ghotel", TRUE, envir=env)
        else assign("Ghotel", FALSE, envir=env)
        tkdestroy(sortiestop)
    }

    eig.check <- tkcheckbutton(sortiestop)
    if (Geig) eigValue <- tclVar("1")
    else eigValue <- tclVar("0")
    tkconfigure(eig.check,variable=eigValue)
    eig.lab<-tklabel(sortiestop,text="Eigenvalues")

    coord.check <- tkcheckbutton(sortiestop)
    if (Gcoord) coordValue <- tclVar("1")
    else coordValue <- tclVar("0")
    tkconfigure(coord.check,variable=coordValue)
    coord.lab<-tklabel(sortiestop,text="Coordinates of the products")

    coef.check <- tkcheckbutton(sortiestop)
    if (Gcoef) coefValue <- tclVar("1")
    else coefValue <- tclVar("0")
    tkconfigure(coef.check,variable=coefValue)
    coef.lab<-tklabel(sortiestop,text="Matrix with the correlation coefficient and confidence level")

    hotel.check <- tkcheckbutton(sortiestop)
    if (Ghotel) hotelValue <- tclVar("1")
    else hotelValue <- tclVar("0")
    tkconfigure(hotel.check,variable=hotelValue)
    hotel.lab<-tklabel(sortiestop,text="Hotelling tests between products")

    tkgrid(tklabel(sortiestop,text="Options to print the results",fg="red"))
    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(eig.lab,eig.check,sticky="w")
    tkgrid(coord.lab,coord.check,sticky="w")
    tkgrid(coef.lab,coef.check,sticky="w")
    tkgrid(hotel.lab,hotel.check,sticky="w")
    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(tkbutton(sortiestop,text="OK",width=9,command=onOKsortie))
    tkfocus(sortiestop)
}

doubleclick<-function(){
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    if(col.nam==""){
      col.val <- vars.fact[as.numeric(tkcurselection(listfact))+1]
      tkinsert(colj,"end",col.val)}
    else{
      if(col2.nam==""){
        col2.val <- vars.fact[as.numeric(tkcurselection(listfact))+1]
        tkinsert(col2j,"end",col2.val)
      }
    }
}
tkbind(listfact,"<Double-ButtonPress-1>",doubleclick)

###### Fonction principale qui lance PANELLIPSE sans fermer la fenêtre------------------------------------------------------------------------------------------------------------
App<-function(){
    nbitemlist<-c(tclvalue(tkcurselection(listdesc)))
    nbitem<-unlist(strsplit(nbitemlist,"\\ "))
    variable<-vars.desc[as.numeric(tkcurselection(listdesc))+1]
    resultat<-tclvalue(resu.val)
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    for (i in 1:length(vars.fact)){
        if (vars.fact[i]==col.nam) col.pos<-i
        if (vars.fact[i]==col2.nam) col2.pos<-i
    }
    if (col.nam=="") tkmessageBox(message="No variable selected for the first factor of interest",icon="warning",type="ok")
    if (col2.nam=="") tkmessageBox(message="No variable selected for the second factor of interest",icon="warning",type="ok")
    done = 0
    if ((col.nam!="")&(col2.nam!="")){
      done = 1
      if (length(nbitem)>1) command3=paste(resultat,'=panellipse(',nomdonnee,'[,c("', paste(vars.fact, collapse='", "'), '", "', paste(variable, collapse='", "'), '")],col.p=',col.pos,',col.j=',col2.pos,',firstvar=',length(vars.fact)+1,',alpha=',as.numeric(tclvalue(seuil.val)),',coord = c(',Raxe,'), nbsimul =',Rsimul,',nbchoix =',Rchoix,',level.search.desc=',as.numeric(tclvalue(searchlevel.val)),',scale.unit=',tclvalue(scale.bool),',variability.variable =',Rgraph, ',centerbypanelist =',Rcenterpan, ',scalebypanelist=',Rscalepan, ',name.panelist=', Rnamepan , ')',sep='')
      else  command3=paste(resultat,'=panellipse(',nomdonnee,'[,c("', paste(vars.fact, collapse='", "'), '", "', paste(vars.desc, collapse='", "'), '")],col.p=',col.pos,',col.j=',col2.pos,',firstvar=',length(vars.fact)+1,',alpha=',as.numeric(tclvalue(seuil.val)),',coord = c(',Raxe,'), nbsimul =',Rsimul,',nbchoix =',Rchoix,',level.search.desc=',as.numeric(tclvalue(searchlevel.val)),',scale.unit=',tclvalue(scale.bool),',variability.variable =',Rgraph, ',centerbypanelist =',Rcenterpan, ',scalebypanelist=',Rscalepan, ',name.panelist=', Rnamepan , ')',sep='')
      justDoIt(command3)
      logger(command3)                     
      panellipseRes<-get(resultat)    
            
      if (Geig){
        doItAndPrint(paste(resultat,'$eig',sep='')) 
        commande1 <- paste('barplot(',resultat,'$eig[,1], main="Eigenvalues", xlab="Dimension", ylab="Eigenvalues",names.arg=1:nrow(',resultat,'$eig))',sep='')
        doItAndPrint(paste('dev.new()'))
        doItAndPrint(commande1) 
      }
      if (Gcoord){
        doItAndPrint(paste(resultat,'$coordinates',sep='')) 
      }
      if (Gcoef){          
        doItAndPrint(paste(resultat,'$correl',sep=''))  
      }
      if (Ghotel){
        doItAndPrint(paste(resultat,'$hotelling',sep='')) 
        doItAndPrint(paste('coltable(',resultat,'$hotelling, main.title = "P-values for the Hotelling T2 tests")',sep=""))
      }              
    }
    return(done)
}


###### Fonction principale qui lance PANELLIPSE et ferme la fenêtre------------------------------------------------------------------------------------------------------------
onOK <- function(){
  done = App()
  if (done >0) tkdestroy(top)
}


##### Positionnement des widgets et frames sur la fenêtre 'top' ------------------------------------------------------------------------------------------
App.but <- tkbutton(top,borderwidth=3,width=12,text="Submit",command=App,fg="blue")
sorties.but<-tkbutton(optionsFrame,text="Outputs",command=onSortie,width=13,default="active",fg="darkred")

OKCancelHelp(helpSubject="panellipse")
tkgrid(tklabel(top, text = "Factors (double-click to select factors of interest)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(factFrame,interestFrame,sticky="w")
tkgrid(tklabel(top,text=""))
tkgrid(tklabel(top, text = "Select descriptors (by default, all descriptors are selected)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(descFrame,optionsFrame,sticky="w")
tkgrid.configure(optionsFrame,column=1, columnspan=2)
tkgrid(optionsFrame2,sorties.but,sticky="e")
tkgrid(tklabel(top,text=""))
tkbind(listfact,"<Double-ButtonPress-1>",doubleclick)
tkgrid(App.but,sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/panellipse.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}

######################################################           FIN FONCTION PANELLIPSE         #############################################################
#########################################################################################################################################################


###########################################################################################################################################################
###################################################################          FONCTION   PANELLIPSE.SESSION              ################################################

SensPanellipse.Sess<-function(){
    require(tcltk)
    require(SensoMineR)
    top<-tktoplevel(borderwidth=5)
    tkwm.title(top,"Repetability of the multidimensional sensory profile")
    donnee<-get(.activeDataSet)
    nomdonnee<-.activeDataSet

######## FRAMES -----------------------------------------------------------------------------------------------------------------------

factFrame <- tkframe(top)
    listfact<-tklistbox(factFrame,selectmode="extended",yscrollcommand=function(...) tkset(scrfact,...))
    scrfact <- tkscrollbar(factFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
    tkgrid(listfact, scrfact,sticky = "nw")
    tkgrid.configure(scrfact, sticky = "wns")
    tkgrid.configure(listfact,sticky = "ew")

interestFrame <- tkframe(top)
    col.lab<-tklabel(interestFrame,text="'Product' factor: ",fg="darkgreen")
    col.val<-tclVar("")
    colj <- tkentry(interestFrame,width=15,textvariable=col.val)
    col2.lab<-tklabel(interestFrame,text="'Panelist' factor: ",fg="darkgreen")
    col2.val<-tclVar("")
    col2j <- tkentry(interestFrame,width=15,textvariable=col2.val)
    col3.lab<-tklabel(interestFrame,text="'Session' factor: ",fg="darkgreen")
    col3.val<-tclVar("")
    col3j <- tkentry(interestFrame,width=15,textvariable=col3.val)

    tkgrid(col.lab, colj,sticky = "nw")
    tkgrid(col2.lab, col2j,sticky = "nw")
    tkgrid(col3.lab, col3j,sticky = "nw")

descopframe<-tkframe(top)
   descFrame <- tkframe(descopframe)
   listdesc<-tklistbox(descFrame,selectmode="extended",yscrollcommand=function(...) tkset(scr,...))
   scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
   vars<-colnames(donnee)
   vars.fact = NULL
   vars.desc = NULL
   for (i in (1:ncol(donnee))){
     if (is.numeric(donnee[,i])){
       tkinsert(listdesc,"end",vars[i])
       vars.desc = c(vars.desc,vars[i])
     }
     else {
       vars.fact = c(vars.fact,vars[i])
       tkinsert(listfact,"end",vars[i])
     }
   }
   tkgrid(listdesc, scr,sticky = "nw")
   tkgrid.configure(scr, sticky = "wns")
   tkgrid.configure(listdesc,sticky = "ew")


  #! fonction pour la gestion des options graphiques
  couleur.a.changer<-defmacro(label, firstLabel, expr=
  {
    optionsFrame2 <- tkframe(optionsFrame,borderwidth=3)
    .PlotLabel<-tclVar(paste(firstLabel, "", sep=" "))
    env<-environment()
    Rgraph <- TRUE
    Rcenterpan <- TRUE
    Rscalepan <- FALSE
    Rnamepan <- FALSE
    Rchoix <- "NULL"
    Rsimul <- 500
    Raxe <- "1,2"

    OnPlot<-function()
    {
      PlotWin<-tktoplevel()
      tkwm.title(PlotWin,gettextRcmdr("Options"))
      tkwm.geometry(PlotWin, "-100+50")

      #création de la fonction onOKsub
      onOKsub<-function()
      {
        if(tclvalue(cbValue)=="1") assign("Rgraph", TRUE, envir=env)
        else assign("Rgraph", FALSE, envir=env)
        if(tclvalue(centerpan.bool)=="1") assign("Rcenterpan", TRUE, envir=env)
        else assign("Rcenterpan", FALSE, envir=env)
        if(tclvalue(scalepan.bool)=="1") assign("Rscalepan", TRUE, envir=env)
        else assign("Rscalepan", FALSE, envir=env)
        if(tclvalue(namepan.bool)=="1") assign("Rnamepan", TRUE, envir=env)
        else assign("Rnamepan", FALSE, envir=env)
        assign("Rsimul",tclvalue(simul.val),envir=env)
        assign("Raxe",tclvalue(axe.val),envir=env)
        assign("Rchoix",tclvalue(choix.val),envir=env)
        tkdestroy(PlotWin)
      }

      graph.check <- tkcheckbutton(PlotWin)
      if (Rgraph) cbValue <- tclVar("1")
      else cbValue <- tclVar("0")
      tkconfigure(graph.check,variable=cbValue)
      graph.lab<-tklabel(PlotWin,text="Plot the graph with the variability of the variables")

      centerpan.check <- tkcheckbutton(PlotWin)
      if (Rcenterpan) centerpan.bool <- tclVar("1")
      else centerpan.bool <- tclVar("0")
      tkconfigure(centerpan.check,variable=centerpan.bool)
      centerpan.lab<-tklabel(PlotWin,text="Center data by panelist")

      scalepan.check <- tkcheckbutton(PlotWin)
      if (Rscalepan) scalepan.bool <- tclVar("1")
      else scalepan.bool <- tclVar("0")
      tkconfigure(scalepan.check,variable=scalepan.bool)
      scalepan.lab<-tklabel(PlotWin,text="Scale data by panelist")

      namepan.check <- tkcheckbutton(PlotWin)
      if (Rnamepan) namepan.bool <- tclVar("1")
      else namepan.bool <- tclVar("0")
      tkconfigure(namepan.check,variable=namepan.bool)
      namepan.lab<-tklabel(PlotWin,text="Name of each panelist is displayed on the 'plotpanelist' graph ")

      axe.lab<-tklabel(PlotWin,text="Dimension to plot")
      axe.val<-tclVar(Raxe)
      axe.ent <- tkentry(PlotWin,width=6,textvariable=axe.val)

      simul.lab<-tklabel(PlotWin,text="Number of simulations used to compute the ellipses")
      simul.val<-tclVar(Rsimul)
      simul.ent <- tkentry(PlotWin,width=15,textvariable=simul.val)

      choix.lab<-tklabel(PlotWin,text="Number of panelists forming a virtual panel (if NULL, the number in the true panel) ")
      choix.val<-tclVar(Rchoix)
      choix.ent <- tkentry(PlotWin,width=15,textvariable=choix.val)

      #mise en page des différents frames de PlotIndFrame
      tkgrid(tklabel(PlotWin, text=" "))
      tkgrid(axe.lab,axe.ent,sticky="w")
      tkgrid(simul.lab,simul.ent,sticky="w")
      tkgrid(choix.lab,choix.ent,sticky="w")
      tkgrid(centerpan.lab,centerpan.check,sticky="w")
      tkgrid(scalepan.lab,scalepan.check,sticky="w")
      tkgrid(namepan.lab,namepan.check,sticky="w")
      tkgrid(graph.lab,graph.check,sticky="w")

      subOKCancelHelp(PlotWin, helpSubject=NULL)
      tkgrid(subButtonsFrame, sticky="ew")
    }
    Plot.but<-tkbutton(optionsFrame2, textvariable=.PlotLabel, command=OnPlot, borderwidth=3, width=13)
    tkgrid(Plot.but, sticky="e")
  })

optionsFrame <- tkframe(top,relief="ridge",borderwidth=2)
    scale.check <- tkcheckbutton(top)
    scale.bool <- tclVar("1")
    tkconfigure(scale.check,variable=scale.bool)
    scale.lab<-tklabel(optionsFrame,text="Scale unit the variables:")

    searchlevel.val<-tclVar("0.20")
    searchlevel<-tkentry(optionsFrame,width=5,textvariable=searchlevel.val)
    searchlevel.lab<-tklabel(optionsFrame,text="Significant level to select descriptors: ")
    seuil.val<-tclVar("0.05")
    seuil<-tkentry(optionsFrame,width=5,textvariable=seuil.val)
    seuil.lab<-tklabel(optionsFrame,text="Significant level for the ellipses: ")
    resu.val<-tclVar("results")
    resu<-tkentry(optionsFrame,width=10,textvariable=resu.val)
    resu.lab<-tklabel(optionsFrame,text="Keep the results in: ")
    tkgrid(scale.lab,scale.check,sticky="w")
    tkgrid(searchlevel.lab,searchlevel,sticky="w")
    tkgrid(seuil.lab,seuil,sticky="w")
    tkgrid(resu.lab,resu,sticky="w")
    couleur.a.changer(label=gettextRcmdr("Options"), firstLabel=gettextRcmdr("Options"))

descFrame <- tkframe(top)
    listdesc<-tklistbox(descFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
    vars<-colnames(donnee)
    vars.desc = NULL
    for (i in (1:ncol(donnee))){
      if (is.numeric(donnee[,i])){
        tkinsert(listdesc,"end",vars[i])
        vars.desc = c(vars.desc,vars[i])
      }
    }
    tkselection.set(listdesc,0)
    tkgrid(listdesc, scr,sticky = "nw")
    tkgrid.configure(scr, sticky = "wns")
    tkgrid.configure(listdesc,sticky = "ew")
    tkselection.set(listdesc,0)


#################### Fonctions liée au bouton 'Sorties' --------------------------------------------------------------------------------------------------
    env<-environment()
    Geig<-FALSE
    Gcoord<-FALSE
    Gcoef<-FALSE
    Ghotel<-TRUE

onSortie<-function(){
    sortiestop<-tktoplevel(borderwidth=10)
    tkwm.title(sortiestop,"Outputs")

    onOKsortie<-function(){
        if(tclvalue(eigValue)=="1") assign("Geig", TRUE, envir=env)
        else assign("Geig", FALSE, envir=env)
        if(tclvalue(coordValue)=="1") assign("Gcoord", TRUE, envir=env)
        else assign("Gcoord", FALSE, envir=env)
        if(tclvalue(coefValue)=="1") assign("Gcoef", TRUE, envir=env)
        else assign("Gcoef", FALSE, envir=env)
        if(tclvalue(hotelValue)=="1") assign("Ghotel", TRUE, envir=env)
        else assign("Ghotel", FALSE, envir=env)
        tkdestroy(sortiestop)
    }

    eig.check <- tkcheckbutton(sortiestop)
    if (Geig) eigValue <- tclVar("1")
    else eigValue <- tclVar("0")
    tkconfigure(eig.check,variable=eigValue)
    eig.lab<-tklabel(sortiestop,text="Eigenvalues")

    coord.check <- tkcheckbutton(sortiestop)
    if (Gcoord) coordValue <- tclVar("1")
    else coordValue <- tclVar("0")
    tkconfigure(coord.check,variable=coordValue)
    coord.lab<-tklabel(sortiestop,text="Coordinates of the products")

    coef.check <- tkcheckbutton(sortiestop)
    if (Gcoef) coefValue <- tclVar("1")
    else coefValue <- tclVar("0")
    tkconfigure(coef.check,variable=coefValue)
    coef.lab<-tklabel(sortiestop,text="Matrix with the correlation coefficient and confidence level")

    hotel.check <- tkcheckbutton(sortiestop)
    if (Ghotel) hotelValue <- tclVar("1")
    else hotelValue <- tclVar("0")
    tkconfigure(hotel.check,variable=hotelValue)
    hotel.lab<-tklabel(sortiestop,text="Hotelling tests between products")

    tkgrid(tklabel(sortiestop,text="Options to print the results",fg="red"))
    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(eig.lab,eig.check,sticky="w")
    tkgrid(coord.lab,coord.check,sticky="w")
    tkgrid(coef.lab,coef.check,sticky="w")
    tkgrid(hotel.lab,hotel.check,sticky="w")
    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(tkbutton(sortiestop,text="OK",width=9,command=onOKsortie))
    tkfocus(sortiestop)
}

doubleclick<-function(){
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    col3.nam<-tclvalue(col3.val)
    if(col.nam==""){
      col.val <- vars.fact[as.numeric(tkcurselection(listfact))+1]
      tkinsert(colj,"end",col.val)
    }
    else{
      if(col2.nam==""){
        col2.val <- vars.fact[as.numeric(tkcurselection(listfact))+1]
        tkinsert(col2j,"end",col2.val)
      }
      else{
        if(col3.nam==""){
          col3.val <- vars.fact[as.numeric(tkcurselection(listfact))+1]
          tkinsert(col3j,"end",col3.val)
        }
      }
    }
}
tkbind(listfact,"<Double-ButtonPress-1>",doubleclick)

###### Fonction principale qui lance PANELLIPSE.SESSION sans fermer la fenêtre------------------------------------------------------------------------------------------------------------
App<-function(){
    nbitemlist<-c(tclvalue(tkcurselection(listdesc)))
    nbitem<-unlist(strsplit(nbitemlist,"\\ "))
    variable<-vars.desc[as.numeric(tkcurselection(listdesc))+1]
    resultat<-tclvalue(resu.val)
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    col3.nam<-tclvalue(col3.val)
    for (i in 1:length(vars.fact)){
        if (vars.fact[i]==col.nam) col.pos<-i
        if (vars.fact[i]==col2.nam) col2.pos<-i
        if (vars.fact[i]==col3.nam) col3.pos<-i
    }
    if (col.nam=="") tkmessageBox(message="No variable selected for the 'product' factor",icon="warning",type="ok")
    if (col2.nam=="") tkmessageBox(message="No variable selected for the 'panelist' factor",icon="warning",type="ok")
    if (col3.nam=="") tkmessageBox(message="No variable selected for the 'session' factor",icon="warning",type="ok")
    done = 0
    if ((col.nam!="")&(col2.nam!="")&(col3.nam!="")){
      done = 1
      if (length(nbitem)>1) command3=paste(resultat,'=panellipse.session(',nomdonnee,'[,c("', paste(vars.fact, collapse='", "'), '", "', paste(variable, collapse='", "'), '")],col.p=',col.pos,',col.j=',col2.pos,',col.s=',col3.pos,',firstvar=',length(vars.fact)+1,',alpha=',as.numeric(tclvalue(seuil.val)),',coord = c(',Raxe,'), nbsimul =',Rsimul,',nbchoix =',Rchoix,',level.search.desc=',as.numeric(tclvalue(searchlevel.val)),',scale.unit=',tclvalue(scale.bool),',variability.variable =',Rgraph, ',centerbypanelist =',Rcenterpan, ',scalebypanelist=',Rscalepan, ',name.panelist=', Rnamepan , ')',sep='')
      else  command3=paste(resultat,'=panellipse.session(',nomdonnee,'[,c("', paste(vars.fact, collapse='", "'), '", "', paste(vars.desc, collapse='", "'), '")],col.p=',col.pos,',col.j=',col2.pos,',col.s=',col3.pos,',firstvar=',length(vars.fact)+1,',alpha=',as.numeric(tclvalue(seuil.val)),',coord = c(',Raxe,'), nbsimul =',Rsimul,',nbchoix =',Rchoix,',level.search.desc=',as.numeric(tclvalue(searchlevel.val)),',scale.unit=',tclvalue(scale.bool),',variability.variable =',Rgraph, ',centerbypanelist =',Rcenterpan, ',scalebypanelist=',Rscalepan, ',name.panelist=', Rnamepan , ')',sep='')
      justDoIt(command3)
      logger(command3)

      if (Geig){
        doItAndPrint(paste(resultat,'$eig',sep=''))
        commande1 <- paste('barplot(',resultat,'$eig[,1], main="Eigenvalues", xlab="Dimension", ylab="Eigenvalues",names.arg=1:nrow(',resultat,'$eig))',sep='')
        doItAndPrint(paste('dev.new()'))
        doItAndPrint(commande1)
      }
      if (Gcoord){
        doItAndPrint(paste(resultat,'$coordinates',sep=''))
      }
      if (Gcoef){
        doItAndPrint(paste(resultat,'$correl',sep=''))
      }
      if (Ghotel){
        doItAndPrint(paste(resultat,'$hotelling$byproduct',sep=''))
        doItAndPrint(paste(resultat,'$hotelling$bysession',sep=''))
      }
    }
    return(done)
}


###### Fonction principale qui lance PANELLIPSE.SESSION et ferme la fenêtre------------------------------------------------------------------------------------------------------------
onOK <- function(){
  done = App()
  if (done >0) tkdestroy(top)
}


##### Positionnement des widgets et frames sur la fenêtre 'top' ------------------------------------------------------------------------------------------
App.but <- tkbutton(top,borderwidth=3,width=12,text="Submit",command=App,fg="blue")
sorties.but<-tkbutton(optionsFrame,text="Outputs",command=onSortie,width=13,default="active",fg="darkred")

OKCancelHelp(helpSubject="panellipse.session")
tkgrid(tklabel(top, text = "Factors (double-click to select factors of interest)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(factFrame,interestFrame,sticky="w")
tkgrid(tklabel(top,text=""))
tkgrid(tklabel(top, text = "Select descriptors (by default, all descriptors are selected)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(descFrame,optionsFrame,sticky="w")
tkgrid(sorties.but,sticky="e")
tkgrid(tklabel(top,text=""))
tkbind(listfact,"<Double-ButtonPress-1>",doubleclick)
tkgrid(App.but,sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/panellipse.session.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}

######################################################           FIN FONCTION PANELLIPSE.SESSION         #############################################################
#########################################################################################################################################################


###########################################################################################################################################################
###################################################################          FONCTION   TRIANGLE.DESIGN              ################################################

SensTriangdesign<-function(){
    require(tcltk)
    require(SensoMineR)
    top<-tktoplevel(borderwidth=5)
    tkwm.title(top,"Construct design for triangle test")

    prod.lab<-tklabel(top,text="Number of products to compare")
    prod.val<-tclVar("")
    prod.ent <- tkentry(top,width=15,textvariable=prod.val)

    assess.lab<-tklabel(top,text="Number of panelists")
    assess.val<-tclVar("")
    assess.ent <- tkentry(top,width=15,textvariable=assess.val)

    prodby.lab<-tklabel(top,text="Number of products compared by assessor")
    prodby.val<-tclVar("")
    prodby.ent <- tkentry(top,width=15,textvariable=prodby.val)

    resu.val<-tclVar("design")
    resu<-tkentry(top,width=10,textvariable=resu.val)
    resu.lab<-tklabel(top,text="Keep the results in : ")

App<-function(){
    done = 0
    nb.prod<-as.numeric(tclvalue(prod.val))
    nb.assess<-as.numeric(tclvalue(assess.val))
    nb.prodby<-as.numeric(tclvalue(prodby.val))
    if (is.na(nb.prod)) tkmessageBox(message="The number of products must be an integer",icon="warning",type="ok")
    if (is.na(nb.assess)) tkmessageBox(message="The number of panelists must be an integer",icon="warning",type="ok")
    if (is.na(nb.prodby)) tkmessageBox(message="The number of products compared by assessor must be an integer",icon="warning",type="ok")
    if (!is.na(nb.prod)&!is.na(nb.assess)&!is.na(nb.prodby)){
      done = 1
      resultat <- tclvalue(resu.val)
      command3 = paste(resultat,'=triangle.design (nbprod=',nb.prod,' , nbpanelist=',nb.assess,', bypanelist =',nb.prodby,')',sep='')
      doItAndPrint(command3)
    }
    return(done)
}

onOK <- function(){
  done = App()
  if (done >0) tkdestroy(top)
}

App.but <- tkbutton(top,borderwidth=3,width=12,text="Submit",command=App,fg="blue")
OKCancelHelp(helpSubject="triangle.design")
tkgrid(tklabel(top,text=""))
tkgrid(prod.lab,prod.ent,sticky="w")
tkgrid(assess.lab,assess.ent,sticky="w")
tkgrid(prodby.lab,prodby.ent,sticky="w")
tkgrid(resu.lab,resu,sticky="w")
tkgrid(App.but,sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/triangle.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}

########       FIN FONCTION triangle.design         #######


###########################################################################################################################################
##############################################        FONCTION TRIANGLE.TEST         ##########################################################
SensTriang <- function(){
require(tcltk)
require(SensoMineR)
top<-tktoplevel(borderwidth=10)
tkwm.title(top,"Analyse triangle test")
donnee<-get(.activeDataSet)
nomdonnee<-.activeDataSet

######## FRAMES -----------------------------------------------------------------------------------------------------------------------

factFrame <- tkframe(top)
    listvar<-tklistbox(factFrame,selectmode="extended",yscrollcommand=function(...) tkset(scrfact,...))
    scrfact <- tkscrollbar(factFrame,repeatinterval=5,command=function(...)tkyview(listvar,...))
    tkgrid(listvar, scrfact,sticky = "nw")
    tkgrid.configure(scrfact, sticky = "wns")
    tkgrid.configure(listvar,sticky = "ew")

interestFrame <- tkframe(top)
    col.lab<-tklabel(interestFrame,text="Answer variable: ",fg="darkgreen")
    col.val<-tclVar("")
    colj <- tkentry(interestFrame,width=15,textvariable=col.val)
    col2.lab<-tklabel(interestFrame,text="Preference variable: ",fg="darkgreen")
    col2.val<-tclVar("")
    col2j <- tkentry(interestFrame,width=15,textvariable=col2.val)

    tkgrid(col.lab, colj,sticky = "nw")
    tkgrid(col2.lab, col2j,sticky = "nw")

descopframe<-tkframe(top)
   descFrame <- tkframe(descopframe)
   listvar<-tklistbox(descFrame,selectmode="extended",yscrollcommand=function(...) tkset(scr,...))
   scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listvar,...))
   vars<-colnames(donnee)
   for (i in (1:ncol(donnee))){
       tkinsert(listvar,"end",vars[i])
   }
   tkgrid(listvar, scr,sticky = "nw")
   tkgrid.configure(scr, sticky = "wns")
   tkgrid.configure(listvar,sticky = "ew")

optionsFrame<-tkframe(top,borderwidth=2,relief="ridge")
    maxML.check <- tkcheckbutton(top)
    maxML.bool <- tclVar("0")
    tkconfigure(maxML.check,variable=maxML.bool)
    maxML.lab<-tklabel(optionsFrame,text="Percentage of panelists who really perceive the difference between the products (estimated by ML)")
    pvalue.check <- tkcheckbutton(top)
    pvalue.bool <- tclVar("1")
    tkconfigure(pvalue.check,variable=pvalue.bool)
    pvalue.lab<-tklabel(optionsFrame,text="P-values of the Triangle tests for each pair of products")
    nbident.check <- tkcheckbutton(top)
    nbident.bool <- tclVar("0")
    tkconfigure(nbident.check,variable=nbident.bool)
    nbident.lab<-tklabel(optionsFrame,text="number of panelists who indicate the odd product for each pair of products")
    nbrecognition.check <- tkcheckbutton(top)
    nbrecognition.bool <- tclVar("1")
    tkconfigure(nbrecognition.check,variable=nbrecognition.bool)
    nbrecognition.lab<-tklabel(optionsFrame,text="Number of panelists who really perceive the difference between the products")
    minimum.check <- tkcheckbutton(top)
    minimum.bool <- tclVar("0")
    tkconfigure(minimum.check,variable=minimum.bool)
    minimum.lab<-tklabel(optionsFrame,text="Minimum of panelists who should detect the odd product to can say that panelists perceive the difference between the products")
    confusion.check <- tkcheckbutton(top)
    confusion.bool <- tclVar("1")
    tkconfigure(confusion.check,variable=confusion.bool)
    confusion.lab<-tklabel(optionsFrame,text="Percentage of panelists who do not perceived the difference between two product")
    resu.val<-tclVar("results")
    resu<-tkentry(optionsFrame,width=10,textvariable=resu.val)
    resu.lab<-tklabel(optionsFrame,text="Keep the result in : ")

    tkgrid(pvalue.lab,pvalue.check,sticky="w")
    tkgrid(maxML.lab,maxML.check,sticky="w")
    tkgrid(nbident.lab,nbident.check,sticky="w")
    tkgrid(nbrecognition.lab,nbrecognition.check,sticky="w")
    tkgrid(confusion.lab,confusion.check,sticky="w")
    tkgrid(minimum.lab,minimum.check,sticky="w")
    tkgrid(resu.lab,resu,sticky="w")

    
App<-function(){
    nbitemlist<-c(tclvalue(tkcurselection(listvar)))
    nbitem<-unlist(strsplit(nbitemlist,"\\ "))
    maxML.val <- as.numeric(tclvalue(maxML.bool))
    pvalue.val <- as.numeric(tclvalue(pvalue.bool))
    nbrecognition.val <- as.numeric(tclvalue(nbrecognition.bool))
    nbident.val <- as.numeric(tclvalue(nbident.bool))
    confusion.val <- as.numeric(tclvalue(confusion.bool))
    minimum.val <- as.numeric(tclvalue(minimum.bool))
    resultat<-tclvalue(resu.val)
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    for (i in 1:length(vars)){
        if (vars[i]==col.nam) col.pos<-i
        if (col2.nam!=""){
          if (vars[i]==col2.nam) col2.pos<-i
        }
    }
    done = 0
    if (col2.nam=="") col2.nam="NULL"
    if (col.nam=="") tkmessageBox(message="No variable selected for the answer",icon="warning",type="ok")
    else {
      done = 1
      if (col2.nam=="NULL") command3 <- paste(resultat,' = triangle.test (',nomdonnee,'[,-',col.pos,'],',nomdonnee,'[,',col.pos,'])',sep='')
      else command3 <- paste(resultat,' = triangle.test (',nomdonnee,'[,-c(',col.pos,',',col2.pos,')],',nomdonnee,'[,',col.pos,'],',nomdonnee,'[,',col2.pos,'])',sep="")
      justDoIt(command3)
      logger(command3)
      if (pvalue.val) doItAndPrint(paste(resultat,'$p.value',sep=''))
      if (maxML.val) doItAndPrint(paste(resultat,'$maxML',sep=''))      
      if (nbrecognition.val) doItAndPrint(paste(resultat,'$nb.recognition',sep=''))      
      if (minimum.val) doItAndPrint(paste(resultat,'$minimum',sep=''))      
      if (nbident.val) doItAndPrint(paste(resultat,'$nb.ident',sep=''))      
      if (confusion.val) doItAndPrint(paste(resultat,'$confusion',sep=''))      
    }
    return(done)
}

####### Fonction attachée aux boutons OK, appel de triangle.test -------------------------------------------------------------------------------------
onOK<-function(){
  done = App()
  if (done >0) tkdestroy(top)
}

doubleclick<-function(){
    col.nam<-tclvalue(col.val)
    col2.nam<-tclvalue(col2.val)
    if(col.nam==""){
      col.val <- vars[as.numeric(tkcurselection(listvar))+1]
      tkinsert(colj,"end",col.val)}
    else{
      if(col2.nam==""){
        col2.val <- vars[as.numeric(tkcurselection(listvar))+1]
        tkinsert(col2j,"end",col2.val)
      }
    }
}
tkbind(listvar,"<Double-ButtonPress-1>",doubleclick)
 

app.but<-tkbutton(top,text="Submit",command=App,fg="blue",width=13,borderwidth=3)
OKCancelHelp(helpSubject="triangle.test")
tkgrid(tklabel(top, text = "Factors (double-click to select column of interest)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(descopframe,interestFrame,sticky="w")
tkgrid(descFrame,columnspan=2)
tkgrid(tklabel(top,text=""))
tkgrid(optionsFrame,sticky="w",columnspan=3)
tkbind(listvar,"<Double-ButtonPress-1>",doubleclick)
tkgrid(tklabel(top,text=""))
tkgrid(app.but,sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/triangle.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}

#########################################################       FIN FONCTION triangle.test        ########################################################
#######################################################################################################################################################

###########################################################################################################################################################
###################################################################          FONCTION   CARTO              ################################################
SensCarto<-function(){
    require(tcltk)
    require(SensoMineR)
    top<-tktoplevel(borderwidth=5)
    tkwm.title(top,"Preference mapping")
    donnee<-get(.activeDataSet)
    nomdonnee<-.activeDataSet

######## FRAMES -----------------------------------------------------------------------------------------------------------------------

descopframe<-tkframe(top)
   descFrame <- tkframe(descopframe)
   listdesc<-tklistbox(descFrame,selectmode="extended",yscrollcommand=function(...) tkset(scr,...))
   scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
   vars<-colnames(donnee)
   vars.desc = NULL
   for (i in (1:ncol(donnee))){
     if (is.numeric(donnee[,i])){
       tkinsert(listdesc,"end",vars[i])
       vars.desc = c(vars.desc,vars[i])
     }
   }
   tkgrid(listdesc, scr,sticky = "nw")
   tkgrid.configure(scr, sticky = "wns")
   tkgrid.configure(listdesc,sticky = "ew")


  #! fonction pour la gestion des options graphiques
  couleur.a.changer<-defmacro(label, firstLabel, expr=
  {
    optionsFrame2 <- tkframe(optionsFrame,borderwidth=3)
    .PlotLabel<-tclVar(paste(firstLabel, "", sep=" "))
    env<-environment()
    Rmethod <- 1
    Rnamepan <- FALSE
    Rlevel <- 0
    Rresol <- 200
    Raxe <- "1,2"

    OnPlot<-function()
    {
      PlotWin<-tktoplevel()
      tkwm.title(PlotWin,gettextRcmdr("Options"))
      tkwm.geometry(PlotWin, "-100+50")

      #création de la fonction onOKsub
      onOKsub<-function()
      {
        if(tclvalue(method)==1) assign("Rmethod", 1, envir=env)
        if(tclvalue(method)==2) assign("Rmethod", 2, envir=env)
        if(tclvalue(method)==3) assign("Rmethod", 3, envir=env)
        if(tclvalue(method)==4) assign("Rmethod", 4, envir=env)
        if(tclvalue(namepan.bool)=="1") assign("Rnamepan", TRUE, envir=env)
        else assign("Rnamepan", FALSE, envir=env)
        assign("Rresol",tclvalue(resol.val),envir=env)
        assign("Raxe",tclvalue(axe.val),envir=env)
        assign("Rlevel",tclvalue(level.val),envir=env)
        tkdestroy(PlotWin)
      }

      meth1 <- tkradiobutton(PlotWin)
      meth2 <- tkradiobutton(PlotWin)
      meth3 <- tkradiobutton(PlotWin)
      meth4 <- tkradiobutton(PlotWin)
      if (Rmethod==1) method <- tclVar("1")
      if (Rmethod==2) method <- tclVar("2")
      if (Rmethod==3) method <- tclVar("3")
      if (Rmethod==4) method <- tclVar("4")
      tkconfigure(meth1,variable=method,value="1")
      tkconfigure(meth2,variable=method,value="2")
      tkconfigure(meth3,variable=method,value="3")
      tkconfigure(meth4,variable=method,value="4")
      tkgrid(tklabel(PlotWin,text="Choose the method"))
      tkgrid(tklabel(PlotWin,text=""))
      tkgrid(tklabel(PlotWin,text="Quadratic model"),meth1)
      tkgrid(tklabel(PlotWin,text="Vectorial model"),meth2)
      tkgrid(tklabel(PlotWin,text="Circular model"),meth3)
      tkgrid(tklabel(PlotWin,text="Elliptical model"),meth4)
      tkgrid(tklabel(PlotWin,text=""))

      namepan.check <- tkcheckbutton(PlotWin)
      if (Rnamepan) namepan.bool <- tclVar("1")
      else namepan.bool <- tclVar("0")
      tkconfigure(namepan.check,variable=namepan.bool)
      namepan.lab<-tklabel(PlotWin,text="Name of each panelist is displayed on the graph ")

      axe.lab<-tklabel(PlotWin,text="Dimension to plot")
      axe.val<-tclVar(Raxe)
      axe.ent <- tkentry(PlotWin,width=6,textvariable=axe.val)

      resol.lab<-tklabel(PlotWin,text="Resolution of the graph")
      resol.val<-tclVar(Rresol)
      resol.ent <- tkentry(PlotWin,width=5,textvariable=resol.val)

      level.lab<-tklabel(PlotWin,text="Number of standard deviations used to consider that product is prefered ")  
      level.val<-tclVar(Rlevel)
      level.ent <- tkentry(PlotWin,width=5,textvariable=level.val)

      #mise en page des différents frames de PlotIndFrame
      tkgrid(tklabel(PlotWin, text=" "))
      tkgrid(axe.lab,axe.ent,sticky="w")
      tkgrid(resol.lab,resol.ent,sticky="w")
      tkgrid(level.lab,level.ent,sticky="w")
      tkgrid(namepan.lab,namepan.check,sticky="w")

      subOKCancelHelp(PlotWin, helpSubject=NULL)
      tkgrid(subButtonsFrame, sticky="ew")
    }
    Plot.but<-tkbutton(optionsFrame2, textvariable=.PlotLabel, command=OnPlot, borderwidth=3)
    tkgrid(Plot.but, sticky="w")
  })

optionsFrame <- tkframe(top,relief="ridge",borderwidth=2)
    scale.check <- tkcheckbutton(top)
    scale.bool <- tclVar("1")
    tkconfigure(scale.check,variable=scale.bool)
    scale.lab<-tklabel(optionsFrame,text="Scale the descriptors for the PCA")
    doPCA.check <- tkcheckbutton(top)
    doPCA.bool <- tclVar("1")
    tkconfigure(doPCA.check,variable=doPCA.bool)
    doPCA.lab<-tklabel(optionsFrame,text="Make a PCA (if false, the two first variables are the dimensions of the map) ")
    keepPCA.check <- tkcheckbutton(top)
    keepPCA.bool <- tclVar("0")
    tkconfigure(keepPCA.check,variable=keepPCA.bool)
    keepPCA.lab<-tklabel(optionsFrame,text="Keep PCA results in the object res.pca")
    nbclusters.val<-tclVar("0")
    nbclusters<-tkentry(optionsFrame,width=5,textvariable=nbclusters.val)
    nbclusters.lab<-tklabel(optionsFrame,text="Number of clusters (if 0, the number of clusters is calculated) ")
    resu.val<-tclVar("results")
    resu<-tkentry(optionsFrame,width=10,textvariable=resu.val)
    resu.lab<-tklabel(optionsFrame,text="Keep the results in: ")
    tkgrid(doPCA.lab,doPCA.check,sticky="w")
    tkgrid(scale.lab,scale.check,sticky="w")
    tkgrid(keepPCA.lab,keepPCA.check,sticky="w")
    tkgrid(nbclusters.lab,nbclusters,sticky="w")
    tkgrid(resu.lab,resu,sticky="w")
    couleur.a.changer(label=gettextRcmdr("Options"), firstLabel=gettextRcmdr("Options"))
      
descFrame <- tkframe(top)
    listdesc<-tklistbox(descFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
    vars<-colnames(donnee)
    vars.desc = NULL
    for (i in (1:ncol(donnee))){
      if (is.numeric(donnee[,i])){
        tkinsert(listdesc,"end",vars[i])
        vars.desc = c(vars.desc,vars[i])
      }
    }
    tkselection.set(listdesc,0)
    tkgrid(listdesc, scr,sticky = "nw")
    tkgrid.configure(scr, sticky = "wns")
    tkgrid.configure(listdesc,sticky = "ew")
    tkselection.set(listdesc,0)


#################### Fonctions liée au bouton 'Sorties' --------------------------------------------------------------------------------------------------
    env<-environment()
    Gclusters<-TRUE
    Gprodclust<-FALSE

onSortie<-function(){
    sortiestop<-tktoplevel(borderwidth=10)
    tkwm.title(sortiestop,"Outputs")

    onOKsortie<-function(){
        if(tclvalue(clustersValue)=="1") assign("Gclusters", TRUE, envir=env)
        else assign("Gclusters", FALSE, envir=env)
        if(tclvalue(prodclustValue)=="1") assign("Gprodclust", TRUE, envir=env)
        else assign("Gprodclust", FALSE, envir=env)
        tkdestroy(sortiestop)
    }

    clusters.check <- tkcheckbutton(sortiestop)
    if (Gclusters) clustersValue <- tclVar("1")
    else clustersValue <- tclVar("0")
    tkconfigure(clusters.check,variable=clustersValue)
    clusters.lab<-tklabel(sortiestop,text="Give the cluster of each panelist")

    prodclust.check <- tkcheckbutton(sortiestop)
    if (Gprodclust) prodclustValue <- tclVar("1")
    else prodclustValue <- tclVar("0")
    tkconfigure(prodclust.check,variable=prodclustValue)
    prodclust.lab<-tklabel(sortiestop,text="Description of each cluster")

    tkgrid(tklabel(sortiestop,text="Options to print the results",fg="red"))
    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(clusters.lab,clusters.check,sticky="w")
    tkgrid(prodclust.lab,prodclust.check,sticky="w")
    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(tkbutton(sortiestop,text="OK",width=9,command=onOKsortie))
    tkfocus(sortiestop)
}

###### Fonction principale qui lance CARTO sans fermer la fenêtre------------------------------------------------------------------------------------------------------------
App<-function(){
    nbitemlist<-c(tclvalue(tkcurselection(listdesc)))
    nbitem<-unlist(strsplit(nbitemlist,"\\ "))
    variable<-vars.desc[as.numeric(tkcurselection(listdesc))+1]
    hedo.var <- vars.desc[-(as.numeric(tkcurselection(listdesc))+1)]
    scale.val <- as.numeric(tclvalue(scale.bool))
    doPCA.val <- as.numeric(tclvalue(doPCA.bool))
    keepPCA.val <- as.numeric(tclvalue(keepPCA.bool))
    resultat<-tclvalue(resu.val)
    done = 0
      done = 1
      if (doPCA.val==1){
        command1 = paste('res.pca = PCA(',nomdonnee,'[,c("', paste(variable, collapse='", "'), '")], axes = c(',Raxe,'), graph = FALSE, scale.unit=',scale.val, ')',sep='')
        justDoIt(command1)
        logger(command1)
        command3=paste(resultat,'=carto(res.pca$ind$coord[,c(',Raxe,')],',nomdonnee,'[,c("', paste(hedo.var, collapse='", "'), '")], regmod=',Rmethod,',coord = c(',Raxe,'), resolution =',Rresol,',level =',Rlevel,',nb.clusters=',as.numeric(tclvalue(nbclusters.val)),',label.j=', as.numeric(Rnamepan) , ')',sep='')
        justDoIt(command3)
        logger(command3)
        if (keepPCA.val!=1) doItAndPrint(paste('rm(res.pca)'))
      }
      else {
        command3=paste(resultat,'=carto(',nomdonnee,'[,1:2],',nomdonnee,'[,-c(1:2)], regmod=',Rmethod,',coord = c(',Raxe,'), resolution =',Rresol,',level =',Rlevel,',nb.clusters=',as.numeric(tclvalue(nbclusters.val)),',label.j=', as.numeric(Rnamepan) , ')',sep='')
        justDoIt(command3)
        logger(command3)    
      }
      cartoRes<-get(resultat)

      if (Gclusters){
        doItAndPrint(paste(resultat,'$clusters',sep=''))
      }
      if (Gprodclust){
        doItAndPrint(paste(resultat,'$prod.clusters',sep=''))
      }
    return(done)
}


###### Fonction principale qui lance CARTO et ferme la fenêtre------------------------------------------------------------------------------------------------------------
onOK <- function(){
  done = App()
  if (done >0) tkdestroy(top)
}


##### Positionnement des widgets et frames sur la fenêtre 'top' ------------------------------------------------------------------------------------------
App.but <- tkbutton(top,borderwidth=3,width=12,text="Submit",command=App,fg="blue")
sorties.but<-tkbutton(optionsFrame,text="Outputs",command=onSortie,width=13,default="active",fg="darkred")

OKCancelHelp(helpSubject="carto")
tkgrid(tklabel(top,text=""))
tkgrid(tklabel(top, text = "Select descriptors (by default, all descriptors are selected)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(descFrame,optionsFrame,sticky="w")
tkgrid.configure(optionsFrame,column=1, columnspan=2)
tkgrid(optionsFrame2,sorties.but,sticky="e")
tkgrid(App.but,sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/carto.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}

######################################################           FIN FONCTION CARTO         #############################################################
#########################################################################################################################################################

###########################################################################################################################################################
###################################################################          FONCTION   CPA              ################################################
SensCPA<-function(){
    require(tcltk)
    require(SensoMineR)
    top<-tktoplevel(borderwidth=5)
    tkwm.title(top,"Consumer Preference Analysis")
    donnee<-get(.activeDataSet)
    nomdonnee<-.activeDataSet

######## FRAMES -----------------------------------------------------------------------------------------------------------------------

descopframe<-tkframe(top)
   descFrame <- tkframe(descopframe)
   listdesc<-tklistbox(descFrame,selectmode="extended",yscrollcommand=function(...) tkset(scr,...))
   scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
   vars<-colnames(donnee)
   vars.desc = NULL
   for (i in (1:ncol(donnee))){
     if (is.numeric(donnee[,i])){
       tkinsert(listdesc,"end",vars[i])
       vars.desc = c(vars.desc,vars[i])
     }
   }
   tkgrid(listdesc, scr,sticky = "nw")
   tkgrid.configure(scr, sticky = "wns")
   tkgrid.configure(listdesc,sticky = "ew")


  #! fonction pour la gestion des options graphiques
  couleur.a.changer<-defmacro(label, firstLabel, expr=
  {
    optionsFrame2 <- tkframe(optionsFrame,borderwidth=3)
    .PlotLabel<-tclVar(paste(firstLabel, "", sep=" "))
    env<-environment()
    Rmethod <- 1
    Rnamepan <- TRUE
    Rscale <- TRUE
    Rcenter <- TRUE
    Raxe <- "1,2"

    OnPlot<-function()
    {
      PlotWin<-tktoplevel()
      tkwm.title(PlotWin,gettextRcmdr("Options"))
      tkwm.geometry(PlotWin, "-100+50")

      #création de la fonction onOKsub
      onOKsub<-function()
      {
        if(tclvalue(namepan.bool)=="1") assign("Rnamepan", TRUE, envir=env)
        else assign("Rnamepan", FALSE, envir=env)
        if(tclvalue(scale.bool)=="1") assign("Rscale", TRUE, envir=env)
        else assign("Rscale", FALSE, envir=env)
        if(tclvalue(center.bool)=="1") assign("Rcenter", TRUE, envir=env)
        else assign("Rcenter", FALSE, envir=env)
        assign("Raxe",tclvalue(axe.val),envir=env)
        tkdestroy(PlotWin)
      }

      namepan.check <- tkcheckbutton(PlotWin)
      if (Rnamepan) namepan.bool <- tclVar("1")
      else namepan.bool <- tclVar("0")
      tkconfigure(namepan.check,variable=namepan.bool)
      namepan.lab<-tklabel(PlotWin,text="Name of each panelist is displayed on the graph ")

      axe.lab<-tklabel(PlotWin,text="Dimension to plot")
      axe.val<-tclVar(Raxe)
      axe.ent <- tkentry(PlotWin,width=6,textvariable=axe.val)

      scale.check <- tkcheckbutton(PlotWin)
      if (Rscale) scale.bool <- tclVar("1")
      else scale.bool <- tclVar("0")
      tkconfigure(scale.check,variable=scale.bool)
      scale.lab<-tklabel(PlotWin,text="Scale data by panelist")

      center.check <- tkcheckbutton(PlotWin)
      if (Rcenter) center.bool <- tclVar("1")
      else center.bool <- tclVar("0")
      tkconfigure(center.check,variable=center.bool)
      center.lab<-tklabel(PlotWin,text="Center data by panelist")

      #mise en page des différents frames de PlotIndFrame
      tkgrid(tklabel(PlotWin, text=" "))
      tkgrid(axe.lab,axe.ent,sticky="w")
      tkgrid(scale.lab,scale.check,sticky="w")
      tkgrid(center.lab,center.check,sticky="w")
      tkgrid(namepan.lab,namepan.check,sticky="w")

      subOKCancelHelp(PlotWin, helpSubject=NULL)
      tkgrid(subButtonsFrame, sticky="ew")
    }
    Plot.but<-tkbutton(optionsFrame2, textvariable=.PlotLabel, command=OnPlot, borderwidth=3)
    tkgrid(Plot.but, sticky="w")
  })

optionsFrame <- tkframe(top,relief="ridge",borderwidth=2)
    scaleu.check <- tkcheckbutton(top)
    scaleu.bool <- tclVar("1")
    tkconfigure(scaleu.check,variable=scaleu.bool)
    scaleu.lab<-tklabel(optionsFrame,text="Scale unit the descriptors for the PCA")
    nbclusters.val<-tclVar("0")
    nbclusters<-tkentry(optionsFrame,width=5,textvariable=nbclusters.val)
    nbclusters.lab<-tklabel(optionsFrame,text="Number of clusters (if 0, the number of clusters is calculated) ")
    resu.val<-tclVar("results")
    resu<-tkentry(optionsFrame,width=10,textvariable=resu.val)
    resu.lab<-tklabel(optionsFrame,text="Keep the results in: ")
    tkgrid(scaleu.lab,scaleu.check,sticky="w")
    tkgrid(nbclusters.lab,nbclusters,sticky="w")
    tkgrid(resu.lab,resu,sticky="w")
    couleur.a.changer(label=gettextRcmdr("Options"), firstLabel=gettextRcmdr("Options"))
      
descFrame <- tkframe(top)
    listdesc<-tklistbox(descFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
    vars<-colnames(donnee)
    vars.desc = NULL
    for (i in (1:ncol(donnee))){
      if (is.numeric(donnee[,i])){
        tkinsert(listdesc,"end",vars[i])
        vars.desc = c(vars.desc,vars[i])
      }
    }
    tkselection.set(listdesc,0)
    tkgrid(listdesc, scr,sticky = "nw")
    tkgrid.configure(scr, sticky = "wns")
    tkgrid.configure(listdesc,sticky = "ew")
    tkselection.set(listdesc,0)


#################### Fonctions liée au bouton 'Sorties' --------------------------------------------------------------------------------------------------
    env<-environment()
    Gclusters<-FALSE
    Gprodclust<-FALSE
    Gdescluster<-TRUE
    Gresult<-FALSE

onSortie<-function(){
    sortiestop<-tktoplevel(borderwidth=10)
    tkwm.title(sortiestop,"Outputs")

    onOKsortie<-function(){
        if(tclvalue(clustersValue)=="1") assign("Gclusters", TRUE, envir=env)
        else assign("Gclusters", FALSE, envir=env)
        if(tclvalue(prodclustValue)=="1") assign("Gprodclust", TRUE, envir=env)
        else assign("Gprodclust", FALSE, envir=env)
        if(tclvalue(resultValue)=="1") assign("Gresult", TRUE, envir=env)
        else assign("Gresult", FALSE, envir=env)
        if(tclvalue(desclusterValue)=="1") assign("Gdescluster", TRUE, envir=env)
        else assign("Gdescluster", FALSE, envir=env)
        tkdestroy(sortiestop)
    }

    clusters.check <- tkcheckbutton(sortiestop)
    if (Gclusters) clustersValue <- tclVar("1")
    else clustersValue <- tclVar("0")
    tkconfigure(clusters.check,variable=clustersValue)
    clusters.lab<-tklabel(sortiestop,text="Give the cluster of each panelist")

    prodclust.check <- tkcheckbutton(sortiestop)
    if (Gprodclust) prodclustValue <- tclVar("1")
    else prodclustValue <- tclVar("0")
    tkconfigure(prodclust.check,variable=prodclustValue)
    prodclust.lab<-tklabel(sortiestop,text="Description of each cluster by the product")

    descluster.check <- tkcheckbutton(sortiestop)
    if (Gdescluster) desclusterValue <- tclVar("1")
    else desclusterValue <- tclVar("0")
    tkconfigure(descluster.check,variable=desclusterValue)
    descluster.lab<-tklabel(sortiestop,text="Description of each cluster by the descriptors")

    result.check <- tkcheckbutton(sortiestop)
    if (Gresult) resultValue <- tclVar("1")
    else resultValue <- tclVar("0")
    tkconfigure(result.check,variable=resultValue)
    result.lab<-tklabel(sortiestop,text="Coordinates of the panelists, the clusters, the archetypes")

    tkgrid(tklabel(sortiestop,text="Options to print the results",fg="red"))
    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(clusters.lab,clusters.check,sticky="w")
    tkgrid(prodclust.lab,prodclust.check,sticky="w")
    tkgrid(descluster.lab,descluster.check,sticky="w")
    tkgrid(result.lab,result.check,sticky="w")
    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(tkbutton(sortiestop,text="OK",width=9,command=onOKsortie))
    tkfocus(sortiestop)
}

###### Fonction principale qui lance CPA sans fermer la fenêtre------------------------------------------------------------------------------------------------------------
App<-function(){
    nbitemlist<-c(tclvalue(tkcurselection(listdesc)))
    nbitem<-unlist(strsplit(nbitemlist,"\\ "))
    variable<-vars.desc[as.numeric(tkcurselection(listdesc))+1]
    hedo.var <- vars.desc[-(as.numeric(tkcurselection(listdesc))+1)]
    scaleu.val <- as.numeric(tclvalue(scaleu.bool))
    resultat<-tclvalue(resu.val)
    done = 0
      done = 1
      command3=paste(resultat,'=cpa(',nomdonnee,'[,c("', paste(variable, collapse='", "'), '")],',nomdonnee,'[,c("', paste(hedo.var, collapse='", "'), '")],coord = c(',Raxe,') ,scale.unit=', scaleu.val ,',nb.clusters=',as.numeric(tclvalue(nbclusters.val)), ',center=', Rcenter , ',scale=', Rscale , ')',sep='')
      justDoIt(command3)
      logger(command3)
      cpaRes<-get(resultat)

      if (Gclusters){
        doItAndPrint(paste(resultat,'$clusters',sep=''))
      }
      if (Gprodclust){
        doItAndPrint(paste(resultat,'$prod.clusters',sep=''))
      }
      if (Gdescluster){
        doItAndPrint(paste(resultat,'$desc.clusters',sep=''))
      }
      if (Gresult){
        doItAndPrint(paste(resultat,'$result',sep=''))
      }
    return(done)
}


###### Fonction principale qui lance CPA et ferme la fenêtre------------------------------------------------------------------------------------------------------------
onOK <- function(){
  done = App()
  if (done >0) tkdestroy(top)
}


##### Positionnement des widgets et frames sur la fenêtre 'top' ------------------------------------------------------------------------------------------
App.but <- tkbutton(top,borderwidth=3,width=12,text="Submit",command=App,fg="blue")
sorties.but<-tkbutton(optionsFrame,text="Outputs",command=onSortie,width=13,default="active",fg="darkred")

OKCancelHelp(helpSubject="cpa")
tkgrid(tklabel(top,text=""))
tkgrid(tklabel(top, text = "Select descriptors (by default, all descriptors are selected)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(descFrame,optionsFrame,sticky="w")
tkgrid.configure(optionsFrame,column=1, columnspan=2)
tkgrid(optionsFrame2,sorties.but,sticky="e")
tkgrid(App.but,sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/cpa.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}

######################################################           FIN FONCTION CPA         #############################################################
#########################################################################################################################################################

##########################################################################################################################################################
######################################################            FONCTION NAPPEPLOT          ##############################################################

SensDrawnapp<-function(){
require(SensoMineR)
donnee<-get(.activeDataSet)
nomdonnee<-.activeDataSet
top<-tktoplevel(borderwidth=5)
tkwm.title(top,"Draw tableclothe")


######## FRAMES --------------------------------------------------------------------------------------------------------------------
titleFrame <- tkframe(top)
    tkgrid(tklabel(titleFrame,text=paste("Select the coordinates of all the panelists"),fg="blue"),columnspan=2,sticky="nw")

listFrame <- tkframe(top,borderwidth=2)                                                                                                          
    listdesc<-tklistbox(listFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(listFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
    tkselection.set(listdesc,0)
    vars<-colnames(donnee)
    vars.desc = NULL
    for (i in (1:ncol(donnee))){
      if (is.numeric(donnee[,i])){
        tkinsert(listdesc,"end",vars[i])
        vars.desc = c(vars.desc,vars[i])
      }
    }

    tkgrid(listdesc, scr,tklabel(listFrame,text="                                       "),sticky = "nw")
    tkgrid.configure(scr, sticky = "wns")
    tkgrid.configure(listdesc,sticky = "ew")

optionsFrame <- tkframe(top,borderwidth=3,relief="ridge")
    numr.val<-tclVar("2")
    numr.lab<-tklabel(optionsFrame,text="Number of tableclothe per row : ")
    numr.ent<-tkentry(optionsFrame,width=4,textvariable=numr.val)
    numc.val<-tclVar("2")
    numc.lab<-tklabel(optionsFrame,text="Number of tableclothe per column : ")
    numc.ent<-tkentry(optionsFrame,width=4,textvariable=numc.val)
    lim.val<-tclVar("60,40")
    lim.lab<-tklabel(optionsFrame,text="Size of the tablecothes")
    lim.ent<-tkentry(optionsFrame,width=8,textvariable=lim.val)
    tkgrid(lim.lab,lim.ent,sticky="w")
    tkgrid(numr.lab,numr.ent,sticky="w")
    tkgrid(numc.lab,numc.ent,sticky="w")

######## Fonction liée au bouton OK -----------------------------------------------------------------------------------------------------

App<-function(){
    variable <- vars.desc[as.numeric(tkcurselection(listdesc))+1]
    nbitemdesc<-c(tclvalue(tkcurselection(listdesc)))
    nbitemdesc<-unlist(strsplit(nbitemdesc,"\\ "))
    done = 0
    if ((length(nbitemdesc)<1)){
      if ((length(vars.desc)%%2)!=0) tkmessageBox(message="The number of variables selected must be a multiple of two",icon="warning",type="ok")
      else {
        done = 1
        command3=paste('nappeplot(',nomdonnee,', lim=c(',tclvalue(lim.val),'), numr=',as.numeric(tclvalue(numr.val)),',numc=',as.numeric(tclvalue(numc.val)),')',sep='')
        justDoIt(command3)
        logger(command3)
      }
    }
    else{
      if ((length(nbitemdesc)%%2)!=0) tkmessageBox(message="The number of variables selected must be a multiple of two",icon="warning",type="ok")
      else{
        done = 1
        command3=paste('nappeplot(',nomdonnee,'[,c("', paste(variable, collapse='", "'), '")], lim=c(',tclvalue(lim.val),'), numr=',as.numeric(tclvalue(numr.val)),',numc=',as.numeric(tclvalue(numc.val)),')',sep='')
        justDoIt(command3)
        logger(command3)
      }
    }
    return(done)
}

onOK <- function(){
  done = App()
  if (done >0) tkdestroy(top)
}

######## Positionnement des widgets sur la fenêtre principale ----------------------------------------------------------------------------
App.but<-tkbutton(top,text="Submit",command=App,fg="blue",width=13,borderwidth=3)
OKCancelHelp(helpSubject="nappeplot")
tkgrid(titleFrame, columnspan=2,sticky="w")
tkgrid(listFrame,optionsFrame,sticky="w")
tkgrid.configure(optionsFrame,columnspan=2)
tkgrid(tklabel(top,text=""))
tkgrid(App.but,sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/nappeplot.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}

#########################################################           FIN FONCTION NAPPEPLOT         ######################################################
#######################################################################################################################################################

#####################################Fonction Sorting Task######################
SensFAST<-function()
{
  require(tcltk)
  require(FactoMineR)
  require(SensoMineR)


#    Création des fonctions pour les options via nouvelle fenêtre graphique

##! fonction pour le choix des variables qualitatives supplémentaires
#  Fillu.funct<-defmacro(label, firstLabel, expr=
#  {
#    env<-environment()
#    variablefact<-NULL
#    .FilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
#    .factors<-Factors()
#    OnFillu<-function()
#    {
#      if(length(.factors)==0) errorCondition(recall=NULL, message=gettextRcmdr("No Factor available"))
#
#      FilluWin<-tktoplevel()
#      tkwm.title(FilluWin,gettextRcmdr("Choice of supplementary factors"))
#      #création de la fonction FOK.funct
#      FOK.funct<-function()
#      {
#        fact.select<-listfact.nom[as.numeric(tkcurselection(listfact))+1]
#        if(length(fact.select)==0)
#        {
#          assign("variablefact", NULL, envir=env)
#          tclvalue(.FilluLabel)<-paste(firstLabel, "", sep=" ")
#          tkconfigure(Fillu.but, fg="black")
#          tkdestroy(FilluWin)
#          return()
#        }
#        assign("variablefact", fact.select, envir=env)
#        tclvalue(.FilluLabel)<-paste(label, "", sep=" ")
#        tkconfigure(Fillu.but, fg="blue")
#        tkdestroy(FilluWin)
#      }
#
#      # création et mise en page de la fenetre Fillu
#      listfact<-tklistbox(FilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrfact,...)) # Liste vide
#      scrfact <-tkscrollbar(FilluWin,repeatinterval=5,command=function(...)tkyview(listfact,...))
#      listfact.nom<-NULL
#      indice<-0
#      for (i in (1:ncol(donnee))) {
#          if (is.factor(donnee[,i])) {
#            tkinsert(listfact,"end",vars[i]) # On renseigne la liste
#            listfact.nom<-c(listfact.nom,vars[i])
#            if(vars[i] %in% variablefact) tkselection.set(listfact, indice)
#            indice<-indice+1
#          }
#      }
#
#      FOK.but<-tkbutton(FilluWin, text="OK", width=16,command=FOK.funct)
#
#      tkgrid(tklabel(FilluWin, text=""))
#      tkgrid(tklabel(FilluWin, text = gettextRcmdr("Select supplementary factor(s)"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")
#      tkgrid(listfact, scrfact, sticky = "nw")
#      tkgrid.configure(scrfact, sticky = "ens", columnspan=1)
#      tkgrid.configure(listfact, sticky = "ew", column=1, columnspan=1)
#      tkgrid(tklabel(FilluWin, text=""))
#      tkgrid(FOK.but, column=1,columnspan=1, sticky="ew")
#      tkgrid(tklabel(FilluWin, text=""))
#      tkgrid.columnconfigure(FilluWin,0, minsize=25)
#      tkgrid.columnconfigure(FilluWin,2, minsize=25)
#  }
#
#   FilluFrame<-tkframe(IlluFrame)
#   Fillu.but<-tkbutton(FilluFrame, textvariable=.FilluLabel, command=OnFillu, borderwidth=3)
#   tkgrid(Fillu.but, sticky="ew")
#  })
#
#  #! fonction pour le choix des variables quantitatives supplémentaires
#  Dillu.funct<-defmacro(label, firstLabel, expr=
#  {
#    env<-environment()
#    variableillu<-NULL
#    .DilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
#    OnDillu<-function()
#    {
#      DilluWin<-tktoplevel()
#      tkwm.title(DilluWin,gettextRcmdr("Select supplementary variables"))
#      #création de la fonction DOK.funct
#      DOK.funct<-function()
#      {
#        vsup.select<-listvar.nom[as.numeric(tkcurselection(listvar))+1]
#        if(length(vsup.select)==0)
#        {
#          assign("variableillu", NULL, envir=env)
#          tclvalue(.DilluLabel)<-paste(firstLabel, "", sep=" ")
#          tkconfigure(Dillu.but, fg="black")
#          tkdestroy(DilluWin)
#          return()
#        }
#        assign("variableillu", vsup.select, envir=env)
#        tclvalue(.DilluLabel)<-paste(label, "", sep=" ")
#        tkconfigure(Dillu.but, fg="blue")
#        tkdestroy(DilluWin)
#      }
#
#      # création et mise en page de la fenetre Dillu
#      listvar<-tklistbox(DilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrvar,...)) # Liste vide
#      scrvar <-tkscrollbar(DilluWin,repeatinterval=5,command=function(...)tkyview(listvar,...))
#      listvar.nom<-NULL
#      indice<-0
#      for (i in (1:ncol(donnee))) {
#          if (is.numeric(donnee[,i])) {
#            tkinsert(listvar,"end",vars[i]) # On renseigne la liste
#            listvar.nom<-c(listvar.nom,vars[i])
#            if(vars[i] %in% variableillu) tkselection.set(listvar, indice)
#            indice<-indice+1
#          }
#      }
#
#      DOK.but<-tkbutton(DilluWin, text="OK", width=16,command=DOK.funct)
#
#      tkgrid(tklabel(DilluWin, text=""))
#      tkgrid(tklabel(DilluWin, text = gettextRcmdr("Select supplementary variable(s)"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")
#      tkgrid(listvar, scrvar, sticky = "nw")
#      tkgrid.configure(scrvar, sticky = "ens", columnspan=1)
#      tkgrid.configure(listvar, sticky = "ew", column=1, columnspan=1)
#      tkgrid(tklabel(DilluWin, text=""))
#      tkgrid(DOK.but, column=1,columnspan=1, sticky="ew")
#      tkgrid(tklabel(DilluWin, text=""))
#      tkgrid.columnconfigure(DilluWin,0, minsize=25)
#      tkgrid.columnconfigure(DilluWin,2, minsize=25)
#  }
#
#   DilluFrame<-tkframe(IlluFrame)
#   if(length(listNumeric())==0){
#     Dillu.but<-tkbutton(DilluFrame, text=gettextRcmdr("No quantitative variable available"), borderwidth=3)
#     tkconfigure(Dillu.but, fg="grey")
#   }
#   else Dillu.but<-tkbutton(DilluFrame, textvariable=.DilluLabel, command=OnDillu, borderwidth=3)
#   tkgrid(Dillu.but, sticky="ew")
#  })
#
#
#  #! fonction pour le choix des individus supplémentaires
#  Iillu.funct<-defmacro(label, firstLabel, expr=
#  {
#    env<-environment()
#    individuillu<-NULL
#    .IilluLabel<-tclVar(paste(firstLabel, "", sep=" "))
#    OnIillu<-function()
#    {
#      IilluWin<-tktoplevel()
#      tkwm.title(IilluWin,gettextRcmdr("Select supplementary individuals"))
#      #création de la fonction IOK.funct
#      IOK.funct<-function()
#      {
#        ind.select<-rows[as.numeric(tkcurselection(listind))+1]
#        if(length(ind.select)==0) {
#          assign("individuillu", NULL, envir=env)
#          tclvalue(.IilluLabel)<-paste(firstLabel, "", sep=" ")
#          tkconfigure(Iillu.but, fg="black")
#          tkdestroy(IilluWin)
#          return()
#        }
#        assign("individuillu", ind.select, envir=env)
#        tclvalue(.IilluLabel)<-paste(label, "", sep=" ")
#        tkconfigure(Iillu.but, fg="blue")
#        tkdestroy(IilluWin)
#      }
#
#      # création et mise en page de la fenetre Fillu
#      listind<-tklistbox(IilluWin,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scrind,...)) # Liste vide
#      scrind <-tkscrollbar(IilluWin,repeatinterval=5,command=function(...)tkyview(listind,...))
#      indice<-0
#      for (i in (1:nrow(donnee))) {
#          tkinsert(listind,"end",rows[i]) # On renseigne la liste
#          if(rows[i] %in% individuillu) tkselection.set(listind,indice)
#          indice<-indice+1
#        }
#
#      IOK.but<-tkbutton(IilluWin, text="OK", width=16,command=IOK.funct)
#
#      tkgrid(tklabel(IilluWin, text=""))
#        tkgrid(tklabel(IilluWin, text = gettextRcmdr("Select supplementary individual(s)"), fg = "blue"), column=1, columnspan = 1, sticky = "ew")
#        tkgrid(listind, scrind, sticky = "nw")
#        tkgrid.configure(scrind, sticky = "ens", columnspan=1)
#        tkgrid.configure(listind, sticky = "ew", column=1, columnspan=1)
#        tkgrid(tklabel(IilluWin, text=""))
#        tkgrid(IOK.but, column=1,columnspan=1, sticky="ew")
#        tkgrid(tklabel(IilluWin, text=""))
#        tkgrid.columnconfigure(IilluWin,0, minsize=25)
#      tkgrid.columnconfigure(IilluWin,2, minsize=25)
#  }
#
#   IilluFrame<-tkframe(IlluFrame)
#   Iillu.but<-tkbutton(IilluFrame, textvariable=.IilluLabel, command=OnIillu, borderwidth=3)
#   tkgrid(Iillu.but, sticky="ew")
#  })
#
#
#
  #! fonction pour la gestion des options graphiques
  PLOT.MCA<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.graph<-0
    #déclaration des variables
    Rchoix<-TRUE
    RTitle<-NULL
    Rlabel<-c("ind",# "ind.sup", "quali.sup",
    "var"#,"quanti.sup"
    )
    Rinvis<- ""
    Rcol.ind<-Rcol.ind.tmp<-"black"
    #Rcol.ind.sup<-Rcol.ind.sup.tmp<-"blue"
    Rcol.quali<-Rcol.quali.tmp<-"magenta"
   # Rcol.qualisup<-Rcol.qualisup.tmp<-"red"
    RXlimInd<-NULL
    RYlimInd<-NULL

    Wchoix=TRUE
    WTitle<-NULL
    #Wlabel<-c("quanti.sup")
    Wlim.cos<-0.1
    #Wcol.quanti.sup<-Wcol.quanti.sup.tmp<-"blue"
    Wcol.var<-Wcol.var.tmp<-"black"
    WXlimVar<-NULL
    WYlimVar<-NULL

    Gchoix<-TRUE
    GTitle<-NULL
    GAxeGrpe<-c(1,2)
    Glabel<-TRUE

    Echoix<-TRUE
    ETitle<-NULL
    EAxeGrpe<-c(1,2)

    .PlotLabel<-tclVar(paste(firstLabel, "", sep=" "))

    OnPlot<-function()
    {
      PlotWin<-tktoplevel()
      tkwm.title(PlotWin, gettextRcmdr("Select graphical options"))
      tkwm.geometry(PlotWin, "-100+50")
      PlotWin2<-tkframe(PlotWin)

      #création de la fonction onOKsub
      onOKsub<-function()
      {
        assign("compteur.graph", compteur.graph+1, envir=env)
        if(compteur.graph>0) tclvalue(.PlotLabel)<-paste(label, "", sep=" ")
        tkconfigure(Plot.but, fg="blue")

        # gestion des entrées de la partie graphique des ellipses
        if(tclvalue(ell.check.value)==1) assign("Echoix", TRUE, envir=env)
        else assign("Echoix", FALSE, envir=env)

        if(Echoix) {
          if (tclvalue(ETitre)==" ") assign("ETitle", NULL, envir=env)
          assign("ETitle", tclvalue(ETitre), envir=env)

          #assign("GAxeGrpe", c(as.numeric(tclvalue(AxeGrpe1)), as.numeric(tclvalue(AxeGrpe2))), envir=env)

         # label.tmp.ell<-tclvalue(label.ell.checkValue)
#          if(label.tmp.ell==1) assign("Elabel", TRUE, envir=env)
#          else assign("Elabel", FALSE, envir=env)
        }

        # gestion des entrées de la partie graphique des Groupes
        if(tclvalue(grpe.check.value)==1) assign("Gchoix", TRUE, envir=env)
        else assign("Gchoix", FALSE, envir=env)

        if(Gchoix) {
          if (tclvalue(GTitre)==" ") assign("GTitle", NULL, envir=env)
          assign("GTitle", tclvalue(GTitre), envir=env)

          #assign("GAxeGrpe", c(as.numeric(tclvalue(AxeGrpe1)), as.numeric(tclvalue(AxeGrpe2))), envir=env)

          label.tmp.grpe<-tclvalue(label.grpe.checkValue)
          if(label.tmp.grpe==1) assign("Glabel", TRUE, envir=env)
          else assign("Glabel", FALSE, envir=env)
        }

        # gestion des entrées de la partie graphique des individus
        if(tclvalue(ind.check.value)==1) assign("Rchoix", TRUE, envir=env)
        else assign("Rchoix", FALSE, envir=env)

        if(Rchoix)
        {
          if (tclvalue(Titre)==" ") assign("RTitle", NULL, envir=env)
          assign("RTitle", tclvalue(Titre), envir=env)

          label.tmp.ind<-tclvalue(label.ind.checkValue)
          #label.tmp.ind.sup<-tclvalue(label.ind.sup.checkValue)
#          label.tmp.quali.sup<-tclvalue(label.quali.sup.checkValue)
          label.tmp.var<-tclvalue(label.var.checkValue)
          #label.tmp.quanti.sup<-tclvalue(label.quanti.sup.checkValue)
          assign("Rlabel", NULL, envir=env)
          if(label.tmp.ind==1) assign("Rlabel", c(Rlabel, "ind"), envir=env)
         # if(label.tmp.ind.sup==1) assign("Rlabel", c(Rlabel, "ind.sup"), envir=env)
#          if(label.tmp.quali.sup==1) assign("Rlabel", c(Rlabel, "quali.sup"), envir=env)
          if(label.tmp.var==1) assign("Rlabel", c(Rlabel, "var"), envir=env)
          #if(label.tmp.quanti.sup==1) assign("Rlabel", c(Rlabel, "quanti.sup"), envir=env)

          invis.tmp.ind<-tclvalue(invis.ind.checkValue)
          #invis.tmp.ind.sup<-tclvalue(invis.ind.sup.checkValue)
#          invis.tmp.quali.sup<-tclvalue(invis.quali.sup.checkValue)
          invis.tmp.var<-tclvalue(invis.var.checkValue)
          assign("Rinvis", NULL, envir=env)
          if(invis.tmp.ind==0) assign("Rinvis", c(Rinvis, "ind"), envir=env)
          #if(invis.tmp.ind.sup==0) assign("Rinvis", c(Rinvis, "ind.sup"), envir=env)
#          if(invis.tmp.quali.sup==0) assign("Rinvis", c(Rinvis, "quali.sup"), envir=env)
          if(invis.tmp.var==0) assign("Rinvis", c(Rinvis, "var"), envir=env)


          assign("Rcol.ind", Rcol.ind.tmp, envir=env)
          #assign("Rcol.ind.sup", Rcol.ind.sup.tmp, envir=env)
          assign("Rcol.quali", Rcol.quali.tmp, envir=env)
          #assign("Rcol.qualisup", Rcol.qualisup.tmp, envir=env)
          assign("Wcol.var", Wcol.var.tmp, envir=env)

          if(tclvalue(XlimIndMin)=="" | tclvalue(XlimIndMax)=="") assign("RXlimInd", NULL, envir=env)
          else assign("RXlimInd", c(as.numeric(tclvalue(XlimIndMin)), as.numeric(tclvalue(XlimIndMax))), envir=env)
          if(tclvalue(YlimIndMin)=="" | tclvalue(YlimIndMax)=="") assign("RYlimInd", NULL, envir=env)
          else assign("RYlimInd", c(as.numeric(tclvalue(YlimIndMin)), as.numeric(tclvalue(YlimIndMax))), envir=env)
        }

        if(tclvalue(var.check.value)==1) assign("Wchoix", TRUE, envir=env)
        else assign("Wchoix", FALSE, envir=env)

        if(Wchoix)
        {
          if (tclvalue(WTitre)==" ") assign("WTitle", NULL, envir=env)
          assign("WTitle", tclvalue(WTitre), envir=env)
          assign("Wlim.cos", tclvalue(WlimCosValue), envir=env)
          #label.tmp.quanti.sup<-tclvalue(label.quanti.sup.checkValue)
#          assign("Wlabel", NULL, envir=env)
          #if(label.tmp.quanti.sup==1) assign("Wlabel", c(Wlabel, "quanti.sup"), envir=env)
          #assign("Wcol.quanti.sup", Wcol.quanti.sup.tmp, envir=env)

        }

        tkdestroy(PlotWin)
      }

      ##########################
            # construction de la partie graphique des ellipses
      PlotEllFrame<-tkframe(PlotWin2, borderwidth=5, relief="groove")

      EchoixFrame<-tkframe(PlotEllFrame,borderwidth=2)
      ell.check<-tkcheckbutton(EchoixFrame)
      if(Echoix) ell.check.value<-tclVar("1")
      else ell.check.value<-tclVar("0")
      tkconfigure(ell.check, variable=ell.check.value)
      tkgrid(tklabel(EchoixFrame, text=gettextRcmdr("Ellipses for products"), font=font2),ell.check)
      tkgrid(tklabel(EchoixFrame, text="  "))

      ETitleFrame<-tkframe(PlotEllFrame,borderwidth=2)
      if (is.null(ETitle)) ETitre <- tclVar(" ")
      else ETitre<-tclVar(ETitle)
      ETitre.entry <-tkentry(ETitleFrame,width="40",textvariable=ETitre)
      tkgrid(tklabel(ETitleFrame,text=gettextRcmdr("Title of the graph")),ETitre.entry)

      #ElabelFrame<-tkframe(PlotEllFrame,borderwidth=2)
#      label.ell.check<-tkcheckbutton(ElabelFrame)
#      if (Elabel) label.ell.checkValue<-tclVar("1")
#      else label.ell.checkValue<-tclVar("0")
#      tkconfigure(label.ell.check, variable=label.ell.checkValue)
#      tkgrid(tklabel(ElabelFrame, text=gettextRcmdr("Draw labels for the products")),label.ell.check)
#

      #mise en page des différents frames de PlotEllFrame
      tkgrid(EchoixFrame)
      #tkgrid(ETitleFrame)
  #    tkgrid(ElabelFrame)
      tkgrid(tklabel(PlotEllFrame, text=" "))


      # construction de la partie graphique des Groupes
      PlotGrpeFrame<-tkframe(PlotWin2, borderwidth=5, relief="groove")

      GchoixFrame<-tkframe(PlotGrpeFrame,borderwidth=2)
      grpe.check<-tkcheckbutton(GchoixFrame)
      if(Gchoix) grpe.check.value<-tclVar("1")
      else grpe.check.value<-tclVar("0")
      tkconfigure(grpe.check, variable=grpe.check.value)
      tkgrid(tklabel(GchoixFrame, text=gettextRcmdr("Graph of subjects"), font=font2),grpe.check)
      tkgrid(tklabel(GchoixFrame, text="  "))

      GTitleFrame<-tkframe(PlotGrpeFrame,borderwidth=2)
      if (is.null(GTitle)) GTitre <- tclVar(" ")
      else GTitre<-tclVar(GTitle)
      GTitre.entry <-tkentry(GTitleFrame,width="40",textvariable=GTitre)
      tkgrid(tklabel(GTitleFrame,text=gettextRcmdr("Title of the graph")),GTitre.entry)

      GlabelFrame<-tkframe(PlotGrpeFrame,borderwidth=2)
      label.grpe.check<-tkcheckbutton(GlabelFrame)
      if (Glabel) label.grpe.checkValue<-tclVar("1")
      else label.grpe.checkValue<-tclVar("0")
      tkconfigure(label.grpe.check, variable=label.grpe.checkValue)
      tkgrid(tklabel(GlabelFrame, text=gettextRcmdr("Draw labels for the groups")),label.grpe.check)


      #mise en page des différents frames de PlotGrpeFrame
      tkgrid(GchoixFrame)
      tkgrid(GTitleFrame)
      tkgrid(GlabelFrame)
      tkgrid(tklabel(PlotGrpeFrame, text=" "))



    # construction de la partie graphique des individus
    PlotIndFrame<-tkframe(PlotWin, borderwidth=5, relief="groove")

    RchoixFrame<-tkframe(PlotIndFrame,borderwidth=2)
    ind.check<-tkcheckbutton(RchoixFrame)
    if(Rchoix) ind.check.value<-tclVar("1")
    else ind.check.value<-tclVar("0")
    tkconfigure(ind.check, variable=ind.check.value)
    tkgrid(tklabel(RchoixFrame, text=gettextRcmdr("Graphical output"), font=font2),ind.check)
    tkgrid(tklabel(RchoixFrame, text="  "))

    RTitleFrame<-tkframe(PlotIndFrame,borderwidth=2)
    if (is.null(RTitle)) Titre <- tclVar(" ")
    else Titre<-tclVar(RTitle)
    Titre.entry <-tkentry(RTitleFrame,width="40",textvariable=Titre)
    tkgrid(tklabel(RTitleFrame,text=gettextRcmdr("Title of the graph")),Titre.entry)

    RlabelFrame<-tkframe(PlotIndFrame,borderwidth=2)
    tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("  ")),tklabel(RlabelFrame,text=gettextRcmdr("Plot")),tklabel(RlabelFrame, text=gettextRcmdr("Label")))
    label.ind.check<-tkcheckbutton(RlabelFrame)
    if ("ind" %in% Rlabel) label.ind.checkValue<-tclVar("1")
    else label.ind.checkValue<-tclVar("0")
    invis.ind.check<-tkcheckbutton(RlabelFrame)
    if ("ind" %in% Rinvis) invis.ind.checkValue<-tclVar("0")
    else invis.ind.checkValue<-tclVar("1")
    tkconfigure(label.ind.check, variable=label.ind.checkValue)
    tkconfigure(invis.ind.check, variable=invis.ind.checkValue)
    tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Products")), invis.ind.check,label.ind.check)
    #label.ind.sup.check<-tkcheckbutton(RlabelFrame)
    #if ("ind.sup" %in% Rlabel) label.ind.sup.checkValue<-tclVar("1")
#    else label.ind.sup.checkValue<-tclVar("0")
#    invis.ind.sup.check<-tkcheckbutton(RlabelFrame)
#    if ("ind.sup" %in% Rinvis) invis.ind.sup.checkValue<-tclVar("0")
#    else invis.ind.sup.checkValue<-tclVar("1")
#    tkconfigure(label.ind.sup.check, variable=label.ind.sup.checkValue)
#    tkconfigure(invis.ind.sup.check, variable=invis.ind.sup.checkValue)
    #if(!is.null(individuillu)) tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Supplementary individuals")),invis.ind.sup.check,label.ind.sup.check)
    label.var.check<-tkcheckbutton(RlabelFrame)
    if ("var" %in% Rlabel) label.var.checkValue<-tclVar("1")
    else label.var.checkValue<-tclVar("0")
    invis.var.check<-tkcheckbutton(RlabelFrame)
    if ("var" %in% Rinvis) invis.var.checkValue<-tclVar("0")
    else invis.var.checkValue<-tclVar("1")
    tkconfigure(label.var.check, variable=label.var.checkValue)
    tkconfigure(invis.var.check, variable=invis.var.checkValue)
    tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Labels")),invis.var.check, label.var.check)
    #label.quali.sup.check<-tkcheckbutton(RlabelFrame)
#    if ("quali.sup" %in% Rlabel) label.quali.sup.checkValue<-tclVar("1")
#    else label.quali.sup.checkValue<-tclVar("0")
#    invis.quali.sup.check<-tkcheckbutton(RlabelFrame)
#    if ("quali.sup" %in% Rinvis) invis.quali.sup.checkValue<-tclVar("0")
#    else invis.quali.sup.checkValue<-tclVar("1")
#    tkconfigure(label.quali.sup.check, variable=label.quali.sup.checkValue)
#    tkconfigure(invis.quali.sup.check, variable=invis.quali.sup.checkValue)
#    if(!is.null(variablefact)) tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Supplementary factors")),invis.quali.sup.check, label.quali.sup.check)
##    label.quanti.sup.check<-tkcheckbutton(RlabelFrame)
##    if ("quanti.sup" %in% Rlabel) label.quanti.sup.checkValue<-tclVar("1")
##    else label.quanti.sup.checkValue<-tclVar("0")
##    invis.quanti.sup.check<-tkcheckbutton(RlabelFrame)
##    if ("quanti.sup" %in% Rinvis) invis.quanti.sup.checkValue<-tclVar("0")
##    else invis.quanti.sup.checkValue<-tclVar("1")
##    tkconfigure(label.quanti.sup.check, variable=label.quanti.sup.checkValue)
##    tkconfigure(invis.quanti.sup.check, variable=invis.quanti.sup.checkValue)
##    tkgrid(tklabel(RlabelFrame, text=gettextRcmdr("Quantitative supplementary variables")),invis.quanti.sup.check, label.quanti.sup.check)

    RcolFrame<-tkframe(PlotIndFrame,borderwidth=2)
    Rcol.ind.value <- Rcol.ind
    canvas.ind <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.ind.value)
    ChangeColor.ind <- function()
    {
      Rcol.ind.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.ind.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Rcol.ind.value)>0)
      {
        tkconfigure(canvas.ind,bg=Rcol.ind.value)
        assign("Rcol.ind.tmp", Rcol.ind.value, envir=env)
      }
    }
    ChangeColor.ind.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.ind)
    tkgrid(tklabel(RcolFrame, text=gettextRcmdr("Color for products")),canvas.ind,ChangeColor.ind.button)

    #Rcol.ind.sup.value<-Rcol.ind.sup
#    canvas.ind.sup <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.ind.sup.value)
    #ChangeColor.ind.sup <- function()
#    {
#      Rcol.ind.sup.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.ind.sup.value,title=gettextRcmdr("Choose a color")))
#      if (nchar(Rcol.ind.sup.value)>0)
#      {
#        tkconfigure(canvas.ind.sup,bg=Rcol.ind.sup.value)
#        assign("Rcol.ind.sup.tmp", Rcol.ind.sup.value, envir=env)
#      }
#    }
#    ChangeColor.ind.sup.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.ind.sup)
    #if(!is.null(individuillu)) tkgrid(tklabel(RcolFrame, text=gettextRcmdr("color for supplementary individuals")),canvas.ind.sup,ChangeColor.ind.sup.button)

    Rcol.quali.value<- Rcol.quali
    canvas.quali <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.quali.value)
    ChangeColor.quali <- function()
    {
      Rcol.quali.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.quali.value,title=gettextRcmdr("Choose a color")))
      if (nchar(Rcol.quali.value)>0)
      {
        tkconfigure(canvas.quali,bg=Rcol.quali.value)
        assign("Rcol.quali.tmp", Rcol.quali.value, envir=env)
      }
    }
    ChangeColor.quali.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.quali)
    tkgrid(tklabel(RcolFrame, text=gettextRcmdr("Color for labels")),canvas.quali,ChangeColor.quali.button)

    #Rcol.qualisup.value<- Rcol.qualisup
#    canvas.qualisup <- tkcanvas(RcolFrame,width="80",height="25",bg=Rcol.qualisup.value)
#    ChangeColor.qualisup <- function()
#    {
#      Rcol.qualisup.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Rcol.qualisup.value,title=gettextRcmdr("Choose a color")))
#      if (nchar(Rcol.qualisup.value)>0)
#      {
#        tkconfigure(canvas.qualisup,bg=Rcol.qualisup.value)
#        assign("Rcol.qualisup.tmp", Rcol.qualisup.value, envir=env)
#      }
#    }
#    ChangeColor.qualisup.button <- tkbutton(RcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.qualisup)
#    if(!is.null(variablefact)) tkgrid(tklabel(RcolFrame, text=gettextRcmdr("Color for supplementary factors")),canvas.qualisup,ChangeColor.qualisup.button)


    RlimFrame<-tkframe(PlotIndFrame,borderwidth=2)
    if(is.null(RXlimInd)) XlimIndMin<-tclVar("")
    else XlimIndMin<-tclVar(paste(RXlimInd[1]))
    XlimIndMin.entry <-tkentry(RlimFrame,width="5",textvariable=XlimIndMin)
    if (is.null(RXlimInd)) XlimIndMax<- tclVar("")
    else XlimIndMax<-tclVar(paste(RXlimInd[1]))
      XlimIndMax.entry <-tkentry(RlimFrame,width="5",textvariable=XlimIndMax)
      tkgrid(tklabel(RlimFrame,text=gettextRcmdr("x limits of the graph:")),XlimIndMin.entry, XlimIndMax.entry)
    if(is.null(RYlimInd)) YlimIndMin<- tclVar("")
    else YlimIndMin<-tclVar(paste(RYlimInd[1]))
    YlimIndMin.entry <-tkentry(RlimFrame,width="5",textvariable=YlimIndMin)
    if (is.null(RYlimInd)) YlimIndMax<- tclVar("")
    else YlimIndMax<-tclVar(paste(RYlimInd[2]))
    YlimIndMax.entry <-tkentry(RlimFrame,width="5",textvariable=YlimIndMax)
    tkgrid(tklabel(RlimFrame,text=gettextRcmdr("y limits of the graph:")),YlimIndMin.entry,YlimIndMax.entry)

    #mise en page des différents frames de PlotIndFrame
    #tkgrid(tklabel(PlotIndFrame, text=gettextRcmdr("Individuals graph"), font=font2))
    #tkgrid(tklabel(PlotIndFrame, text=" "))
    tkgrid(RchoixFrame)
    tkgrid(RTitleFrame)
    tkgrid(tklabel(PlotIndFrame, text=" "))
    tkgrid(RlabelFrame)
    tkgrid(tklabel(PlotIndFrame, text=" "))
    tkgrid(RcolFrame)
    tkgrid(tklabel(PlotIndFrame, text=" "))
    tkgrid(RlimFrame)
    tkgrid(tklabel(PlotIndFrame, text=" "))

    PlotVarFrame<-tkframe(PlotWin, borderwidth=5, relief="groove")
    WchoixFrame<-tkframe(PlotVarFrame,borderwidth=2)
    var.check<-tkcheckbutton(WchoixFrame)
    if(Wchoix) var.check.value<-tclVar("1")
    else var.check.value<-tclVar("0")
    tkconfigure(var.check, variable=var.check.value)
    tkgrid(tklabel(WchoixFrame, text=gettextRcmdr("Plot subjects graph"), font=font2),var.check)
    tkgrid(tklabel(WchoixFrame, text="  "))

    WTitleFrame<-tkframe(PlotVarFrame,borderwidth=2)
    if (is.null(WTitle)) WTitre <- tclVar(" ")
    else WTitre<-tclVar(WTitle)
    WTitre.entry <-tkentry(WTitleFrame,width="40",textvariable=WTitre)
    tkgrid(tklabel(WTitleFrame,text=gettextRcmdr("Title of the graph")),WTitre.entry)

    WcosFrame<-tkframe(PlotVarFrame,borderwidth=2)
    WlimCosValue<-tclVar(paste(Wlim.cos))
    WlimCos.entry<-tkentry(WcosFrame, width=5, textvariable=WlimCosValue)
    tkgrid(tklabel(WcosFrame,text=gettextRcmdr("Draw variables with a cos² >:")),WlimCos.entry)

   WlabelFrame<-tkframe(PlotVarFrame,borderwidth=2)
    #label.quanti.sup.check<-tkcheckbutton(WlabelFrame)
#    if ("quanti.sup" %in% Wlabel) label.quanti.sup.checkValue<-tclVar("1")
#    else label.quanti.sup.checkValue<-tclVar("0")
#    tkconfigure(label.quanti.sup.check, variable=label.quanti.sup.checkValue)
#    tkgrid(tklabel(WlabelFrame, text=gettextRcmdr("Labels for the supplementary quantitative variables")),label.quanti.sup.check)

    WcolFrame<-tkframe(PlotVarFrame,borderwidth=2)
    #Wcol.quanti.sup.value<-Wcol.quanti.sup
#    canvas.quanti.sup <- tkcanvas(WcolFrame,width="80",height="25",bg=Wcol.quanti.sup.value)
#    ChangeColor.quanti.sup <- function()
#    {
#      Wcol.quanti.sup.value<-tclvalue(tcl("tk_chooseColor",initialcolor=Wcol.quanti.sup.value,title=gettextRcmdr("Choose a color")))
#      if (nchar(Wcol.quanti.sup.value)>0) {
#        tkconfigure(canvas.quanti.sup,bg=Wcol.quanti.sup.value)
#        assign("Wcol.quanti.sup.tmp", Wcol.quanti.sup.value, envir=env)
#      }
#    }
    #ChangeColor.quanti.sup.button <- tkbutton(WcolFrame,text=gettextRcmdr("Change Color"),command=ChangeColor.quanti.sup)
#    tkgrid(tklabel(WcolFrame, text=gettextRcmdr("Color for supplementary variables")),canvas.quanti.sup,ChangeColor.quanti.sup.button)
#
    #mise en page des différents frames de PlotVarFrame
    tkgrid(WchoixFrame)
    tkgrid(WTitleFrame)
    tkgrid(WcosFrame)
    tkgrid(WlabelFrame)
    tkgrid(tklabel(PlotVarFrame, text=" "))
    tkgrid(WcolFrame)
    tkgrid(tklabel(PlotVarFrame, text=" "))


    # construction de la partie graphique des variables

    subOKCancelHelp(PlotWin, "plot.fast")
    #if (length(variableillu)>0) {
#      tkgrid(PlotIndFrame,PlotVarFrame)
#      tkgrid.configure(PlotVarFrame, sticky="n")
#    }
    #else
    tkgrid(PlotGrpeFrame)
    tkgrid(PlotEllFrame)
    tkgrid(PlotIndFrame,PlotWin2, sticky="ns")
    tkgrid(subButtonsFrame, sticky="ew", columnspan=2)

    }

    PlotFrame<-tkframe(IlluFrame)
    Plot.but<-tkbutton(PlotFrame, textvariable=.PlotLabel, command=OnPlot, borderwidth=3)
    tkgrid(Plot.but, sticky="ew")
  })


  #! fonction pour la réinitialisation des paramètre
  Reinitializ.funct<-function()
  {
    tkdestroy(top)
        SensFAST()
  }


  #! fonction pour le choix des éléments de sortie
  Sortie.funct<-defmacro(label, firstLabel, expr=
  {
    env<-environment()
    compteur.sortie<-0
    #déclaration des variables
    Rpropre<-FALSE
    RFichier <- ""
        Rvariable<-FALSE
        Rindividu<-FALSE
        Rindsup<-FALSE
        Rgroupe<-FALSE
        Rcoocc<-FALSE
        Rcramer<-FALSE
        Rtext<-FALSE
        Rreord<-FALSE
        #Rquantisup<-FALSE
#        Rqualisup<-FALSE
        #Rdescdim<-FALSE

    .SortieLabel<-tclVar(paste(firstLabel, "", sep=" "))

    OnSortie<-function()
    {
      SortieWin<-tktoplevel()
      tkwm.title(SortieWin,"Displayed outputs")

      #création de la fonction onOKsub
      onOK.sortie<-function()
      {
        assign("compteur.sortie", compteur.sortie+1, envir=env)
        if(compteur.sortie>0) tclvalue(.SortieLabel)<-paste(label, "", sep=" ")
        tkconfigure(Sortie.but, fg="blue")

        if(tclvalue(eigValue)=="1") assign("Rpropre", TRUE, envir=env)
        else assign("Rpropre", FALSE, envir=env)

        if(tclvalue(varValue)=="1") assign("Rvariable", TRUE, envir=env)
        else assign("Rvariable", FALSE, envir=env)

        if(tclvalue(indValue)=="1") assign("Rindividu", TRUE, envir=env)
        else assign("Rindividu", FALSE, envir=env)

        if(tclvalue(grpValue)=="1") assign("Rgroupe", TRUE, envir=env)
        else assign("Rgroupe", FALSE, envir=env)

        if(tclvalue(cooccValue)=="1") assign("Rcoocc", TRUE, envir=env)
        else assign("Rcoocc", FALSE, envir=env)

        if(tclvalue(cramerValue)=="1") assign("Rcramer", TRUE, envir=env)
        else assign("Rcramer", FALSE, envir=env)

        if(tclvalue(textValue)=="1") assign("Rtext", TRUE, envir=env)
        else assign("Rtext", FALSE, envir=env)

        if(tclvalue(reordValue)=="1") assign("Rreord", TRUE, envir=env)
        else assign("Rreord", FALSE, envir=env)

        #if(tclvalue(ind.sup.Value)=="1") assign("Rindsup", TRUE, envir=env)
#        else assign("Rindsup", FALSE, envir=env)
#
#        if(tclvalue(quanti.sup.Value)=="1") assign("Rquantisup", TRUE, envir=env)
#        else assign("Rquantisup", FALSE, envir=env)
#
#        if(tclvalue(quali.sup.Value)=="1") assign("Rqualisup", TRUE, envir=env)
#        else assign("Rqualisup", FALSE, envir=env)

        #if(tclvalue(descdimValue)=="1") assign("Rdescdim", TRUE, envir=env)
#        else assign("Rdescdim", FALSE, envir=env)

        if (tclvalue(Fichier)=="") assign("RFichier", NULL, envir=env)
        assign("RFichier", tclvalue(Fichier), envir=env)

        tkdestroy(SortieWin)

      }

      eig.lab <-tklabel(SortieWin, text=gettextRcmdr("Eigenvalues"))
        eig.check <- tkcheckbutton(SortieWin)
        if(Rpropre) eigValue <- tclVar("1")
        else eigValue <- tclVar("0")
        tkconfigure(eig.check,variable=eigValue)

      var.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for labels"))
        var.check <- tkcheckbutton(SortieWin)
        if(Rvariable) varValue <- tclVar("1")
        else varValue <- tclVar("0")
        tkconfigure(var.check,variable=varValue)

      ind.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for products"))
        ind.check <- tkcheckbutton(SortieWin)
        if(Rindividu) indValue <- tclVar("1")
        else indValue <- tclVar("0")
        tkconfigure(ind.check,variable=indValue)

        grp.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for consumers"))
        grp.check <- tkcheckbutton(SortieWin)
        if(Rgroupe) grpValue <- tclVar("1")
        else grpValue <- tclVar("0")
        tkconfigure(grp.check,variable=grpValue)

        coocc.lab<-tklabel(SortieWin,text=gettextRcmdr("Cooccurence matrix"))
        coocc.check <- tkcheckbutton(SortieWin)
        if(Rcoocc) cooccValue <- tclVar("1")
        else cooccValue <- tclVar("0")
        tkconfigure(coocc.check,variable=cooccValue)

        cramer.lab<-tklabel(SortieWin,text=gettextRcmdr("Cramer matrix"))
        cramer.check <- tkcheckbutton(SortieWin)
        if(Rcramer) cramerValue <- tclVar("1")
        else cramerValue <- tclVar("0")
        tkconfigure(cramer.check,variable=cramerValue)

        text.lab<-tklabel(SortieWin,text=gettextRcmdr("Textual results"))
        text.check <- tkcheckbutton(SortieWin)
        if(Rtext) textValue <- tclVar("1")
        else textValue <- tclVar("0")
        tkconfigure(text.check,variable=textValue)

         reord.lab<-tklabel(SortieWin,text=gettextRcmdr("Reordered data"))
        reord.check <- tkcheckbutton(SortieWin)
        if(Rreord) reordValue <- tclVar("1")
        else reordValue <- tclVar("0")
        tkconfigure (reord.check,variable=reordValue)

      #ind.sup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for supplementary individuals"))
#        ind.sup.check <- tkcheckbutton(SortieWin)
#        if(Rindsup) ind.sup.Value <- tclVar("1")
#        else ind.sup.Value <- tclVar("0")
#        tkconfigure(ind.sup.check,variable=ind.sup.Value)


     # quanti.sup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for supplementary variables"))
#        quanti.sup.check <- tkcheckbutton(SortieWin)
#        if(Rquantisup) quanti.sup.Value <- tclVar("1")
#        else quanti.sup.Value <- tclVar("0")
#        tkconfigure(quanti.sup.check,variable=quanti.sup.Value)
#
#      quali.sup.lab<-tklabel(SortieWin,text=gettextRcmdr("Results for supplementary factors"))
#        quali.sup.check <- tkcheckbutton(SortieWin)
#      if(Rqualisup) quali.sup.Value <- tclVar("1")
#        else quali.sup.Value <- tclVar("0")
#        tkconfigure(quali.sup.check,variable=quali.sup.Value)
#
        #descdim.lab<-tklabel(SortieWin, text=gettextRcmdr("Description of the dimensions"))
#      descdim.check<-tkcheckbutton(SortieWin)
#      if(Rdescdim) descdimValue<-tclVar("1")
#      else descdimValue<-tclVar("0")
#      tkconfigure(descdim.check,variable=descdimValue)

      RFichierFrame<-tkframe(SortieWin,borderwidth=2)
      if (is.null(RFichier)) Fichier <- tclVar("")
      else Fichier<-tclVar(RFichier)
      Fichier.entry <-tkentry(RFichierFrame,width="40",textvariable=Fichier)
      tkgrid(tklabel(RFichierFrame,text=gettextRcmdr("Print results on a 'csv' file")),Fichier.entry)

      SortieOK.but<-tkbutton(SortieWin,text="OK",width=16,command=onOK.sortie)

        tkgrid(tklabel(SortieWin, text = gettextRcmdr("Select output options"), fg ="blue"),  columnspan = 2, sticky = "w")
        tkgrid(tklabel(SortieWin, text = " "))
        tkgrid(eig.lab,eig.check,sticky="w")
        tkgrid(var.lab,var.check,sticky="w")
        tkgrid(ind.lab,ind.check,sticky="w")
        tkgrid(grp.lab,grp.check,sticky="w")
        tkgrid(coocc.lab,coocc.check,sticky="w")
        tkgrid(cramer.lab,cramer.check,sticky="w")
        tkgrid(text.lab,text.check,sticky="w")
        tkgrid(reord.lab,reord.check,sticky="w")
     #   if (!is.null(individuillu)) tkgrid(ind.sup.lab,ind.sup.check,sticky="w")
     #   if (!is.null(variableillu)) tkgrid(quanti.sup.lab,quanti.sup.check,sticky="w")
#        if (!is.null(variablefact)) tkgrid(quali.sup.lab,quali.sup.check,sticky="w")
        #tkgrid(descdim.lab,descdim.check,sticky="w")

        tkgrid(tklabel(SortieWin, text = " "))
        tkgrid(RFichierFrame)
        tkgrid(SortieOK.but)
   }

    SortieFrame<-tkframe(IlluFrame)
    Sortie.but<-tkbutton(SortieFrame, textvariable=.SortieLabel, command=OnSortie, borderwidth=3)
    tkgrid(Sortie.but, sticky="ew")
  })


  #! fonction associer au bouton OK, execute et détruit l'interface graphique
onOK <- function(){
  done = OnAppliquer()
  tkdestroy(top)
}

  #! fonction associer au bouton Appliquer, execute sans détruire la fenêtre top
  OnAppliquer<-function()
  {
    # liste de toutes les variables interne créées      (** mise en forme incomplète)
      # sur la fenetre principale
#         listdesc         **
#         resu.val         **
#         ncp.val          **
      # dans les boutons des fenêtres illustratives
#         variablefact     **
#         variableillu     **
#         individuillu     **
      # dans le bouton Plot MCA
#         Rchoix
#         RTitle
#         Rlabel
#         Rcol.ind
#         Rcol.ind.sup
#         Rcol.quali
#         Rhabillage       **
#         RXlimInd
#         RYlimInd
#         Wcol.var
      # dans le bouton affichage sortie
#         Rpropre
#             Rvariable
#           Rindividu
#           Rindsup
#           Rquantisup
#           Rqualisup


    # récupération des paramètres de la fenêtre principale
    nom.res<-tclvalue(resu.val)
    if (length(ls(pat=nom.res))>0) justDoIt(paste('remove (',nom.res,')'))
    if(length(as.numeric(tkcurselection(listdesc)))<2) varActives<-listdesc.nom
    else varActives<-listdesc.nom[as.numeric(tkcurselection(listdesc))+1]
#    varActives <- varActives[!(varActives%in%variablefact)]
    ncp<-as.numeric(tclvalue(ncp.val))
    Axe<-c(as.numeric(tclvalue(Axe1)), as.numeric(tclvalue(Axe2)))
    alpha<-as.numeric(tclvalue(alpha.val))
    mot<-as.numeric(tclvalue(mot.val))
    separ<-as.character(tclvalue(separ.val))
    B<-as.numeric(tclvalue(B.val))

    # gestion du tableau de données pour l'ACM
#    if(!is.null(individuillu)) {
#      ind.actif<-rows[-which(rows %in% individuillu)]
#      if(!is.null(variableillu)) {
#        if(!is.null(variablefact)) commande.data<-paste(activeDataSet(),'.', 'SortingTask', '<-', activeDataSet(), '[c("', paste(ind.actif, collapse='", "'), '", "', paste(individuillu, collapse='", "'), '") ,c("', paste(varActives, collapse='", "'), '", "', paste(variableillu, collapse='", "'), '", "', paste(variablefact, collapse='", "'), '")]', sep="")
#        else commande.data<-paste(activeDataSet(),'.', 'SortingTask', '<-', activeDataSet(), '[c("', paste(ind.actif, collapse='", "'), '", "', paste(individuillu, collapse='", "'), '") ,c("', paste(varActives, collapse='", "'), '", "', paste(variableillu, collapse='", "'), '")]', sep="")
#      }
#      else {
#        if(!is.null(variablefact)) commande.data<-paste(activeDataSet(),'.', 'MSortingTask', '<-', activeDataSet(), '[c("', paste(ind.actif, collapse='", "'), '", "', paste(individuillu, collapse='", "'), '") ,c("', paste(varActives, collapse='", "'), '", "', paste(variablefact, collapse='", "'), '")]', sep="")
#        else commande.data<-paste(activeDataSet(),'.', 'SortingTask', '<-', activeDataSet(), '[c("', paste(ind.actif, collapse='", "'), '", "', paste(individuillu, collapse='", "'), '") ,c("', paste(varActives, collapse='", "'), '")]', sep="")
#      }
#    }
#    else {
      #if(!is.null(variableillu)) {
#        if(!is.null(variablefact)) commande.data<-paste(activeDataSet(),'.', 'MCA', '<-', activeDataSet(), '[, c("', paste(varActives, collapse='", "'), '", "', paste(variableillu, collapse='", "'), '", "', paste(variablefact, collapse='", "'), '")]', sep="")
#        else commande.data<-paste(activeDataSet(),'.', 'MCA', '<-', activeDataSet(), '[, c("', paste(varActives, collapse='", "'), '", "', paste(variableillu, collapse='", "'), '")]', sep="")
#      }
#      else {
#        if(!is.null(variablefact)) commande.data<-paste(activeDataSet(),'.', 'MCA', '<-', activeDataSet(), '[, c("', paste(varActives, collapse='", "'), '", "', paste(variablefact, collapse='", "'), '")]', sep="")
#        else
        commande.data<-paste(activeDataSet(),'.', 'fast', '<-', activeDataSet(), '[, c("', paste(varActives, collapse='", "'), '")]', sep="")
#      }
#    }
    justDoIt(commande.data)
    logger(commande.data)
    donnee.depart<-activeDataSet()
    activeDataSet(paste(activeDataSet(),'.', 'fast', sep=""))

    # gestion de la commande réalisant la fonction Sorting Task
      commande.FAST<-paste(nom.res, '<-fast(', activeDataSet(), ', ncp=', ncp,', word.min=', mot,', sep.words="', separ,'", alpha=', alpha,', B=',B,', graph = FALSE)', sep="")

    justDoIt(commande.FAST)
    logger(commande.FAST)

    # gestion des graphiques

    if (length(ls(pat=nom.res))>0) {if (get(nom.res)$eig[1,2]==100) doItAndPrint(paste('"No graph can be plot: data are unidimensional"'))}
    if((Rchoix)&(length(ls(pat=nom.res))>0)){
    if (get(nom.res)$eig[1,2]!=100) {
      commande.plotInd<-paste('plot.fast(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), col.ind="', Rcol.ind, '", label=c("', paste(Rlabel, collapse='", "'), '"), invisible=c("', paste(Rinvis, collapse='", "'), '")',
   #, col.ind="', Rcol.ind, '", col.ind.sup="', Rcol.ind.sup, '", col.quali.sup="', Rcol.quali, '", label=c("', paste(Rlabel, collapse='", "'), '"), invisible=c("quanti.sup","', paste(Rinvis, collapse='", "'), '")',
       sep="")
      if (!is.null(RTitle)) {
        if (RTitle !="") commande.plotInd <- paste(commande.plotInd,', title="', RTitle,'"', sep="")
      }
      if (!is.null(RXlimInd)) commande.plotInd <- paste(commande.plotInd,', xlim=c(', paste(RXlimInd, collapse=", "), ')', sep="")
      if (!is.null(RYlimInd)) commande.plotInd <- paste(commande.plotInd,', ylim=c(', paste(RYlimInd, collapse=", "), ')', sep="")
      commande.plotInd <- paste(commande.plotInd,')', sep="")
      justDoIt(commande.plotInd)
      logger(commande.plotInd)
    }}

    if((Gchoix)&(length(ls(pat=nom.res))>0)){
      if (get(nom.res)$eig[1,2]!=100) {
        commande.plotG<-paste('plot.fast(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '), choix="group", lab.grpe=', Glabel, sep="")
        if (is.null(GTitle)) commande.plotG <- paste(commande.plotG,')', sep="")
        else {
         if (GTitle =="") commande.plotG <- paste(commande.plotG,')', sep="")
          else commande.plotG <- paste(commande.plotG,', title="', GTitle,'")', sep="")
        }
        justDoIt(commande.plotG)
        logger(commande.plotG)
      }}

      if((Echoix)&(length(ls(pat=nom.res))>0)){
      if (get(nom.res)$eig[1,2]!=100) {
        commande.plotE<-paste('plotellipse(', nom.res,'$call$sim , coord=c(', paste(Axe, collapse=", "), '), alpha=', alpha,',eig=signif(',nom.res,'$eig,4))', sep="")
#        if (is.null(ETitle)) commande.plotE <- paste(commande.plotE,')', sep="")
#        else {
#          if (ETitle ==" ") commande.plotE <- paste(commande.plotE,')', sep="")
#          else commande.plotE <- paste(commande.plotE,', title="', ETitle,'")', sep="")
#        }
        justDoIt(paste('dev.new()'))
        logger(paste('dev.new()'))
        justDoIt(commande.plotE)
        logger(commande.plotE)
      }}

    #if((Wchoix)&(length(ls(pat=nom.res))>0)#&(length(variableillu)>0)
#    ){
#    if (get(nom.res)$eig[1,2]!=100) {
#      commande.plotInd<-paste('plot.SortingTask(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '),choix="group"',
#      # invisible=c("ind","ind.sup","var","quali.sup"), col.quanti.sup="',Wcol.quanti.sup,'"',
#      sep="")
#      if (!is.null(WTitle)) {
#        if (WTitle !=" ") commande.plotInd <- paste(commande.plotInd,', title="', WTitle,'")', sep="")
#      }
#      #if ("quanti.sup"%in%Rlabel) commande.plotInd <- paste(commande.plotInd, ',label=c("quanti.sup")',sep='')
#      commande.plotInd <- paste(commande.plotInd,')', sep="")
#      justDoIt(commande.plotInd)
#      logger(commande.plotInd)
#    }}

    # gestion de l'édition de certains resultats
    if (RFichier==""){
      if(Rpropre) doItAndPrint(paste( nom.res, '$eig', sep=""))
      if(Rvariable) doItAndPrint(paste( nom.res, '$var', sep=""))
      if(Rindividu) doItAndPrint(paste( nom.res, '$ind', sep=""))
      if(Rgroupe) doItAndPrint(paste( nom.res, '$group', sep=""))
    #  if(Rindsup & !is.null(individuillu)) doItAndPrint(paste( nom.res, '$ind.sup', sep=""))
     # if(Rquantisup & !is.null(variableillu)) doItAndPrint(paste( nom.res, '$quanti.sup', sep=""))
      #if(Rqualisup & !is.null(variablefact)) doItAndPrint(paste( nom.res, '$quali.sup', sep=""))
      #if(Rdescdim) doItAndPrint(paste('dimdesc(', nom.res, ', axes=c(', paste(Axe, collapse=", "), '))', sep=""))
    if(Rcoocc) doItAndPrint(paste( nom.res, '$cooccur', sep=""))
    if(Rcramer) doItAndPrint(paste( nom.res, '$cramer', sep=""))
    if(Rtext) doItAndPrint(paste( nom.res, '$textual', sep=""))
    if(Rreord) doItAndPrint(paste( nom.res, '$reord', sep=""))
    }
    else {
      Fich = RFichier
      if (substr(Fich,1,1)!='"') Fich = paste('"',Fich,sep='')
      if (substr(Fich,nchar(Fich),nchar(Fich))!='"') Fich = paste(Fich,'"',sep='')
      append = FALSE
      if(Rpropre){
        doItAndPrint(paste('write.infile(', nom.res, '$eig, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rvariable){
        doItAndPrint(paste('write.infile(', nom.res, '$var, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      if(Rindividu){
        doItAndPrint(paste('write.infile(', nom.res, '$ind, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
      #if(Rindsup){
#        doItAndPrint(paste('write.infile(', nom.res, '$ind.sup, file =',Fich,',append=',append,')', sep=""))
#        append = TRUE
#      }
#      if(Rquantisup){
#        doItAndPrint(paste('write.infile(', nom.res, '$quanti.sup, file =',Fich,',append=',append,')', sep=""))
#        append = TRUE
#      }
#      if(Rqualisup){
#        doItAndPrint(paste('write.infile(', nom.res, '$quali.sup, file =',Fich,',append=',append,')', sep=""))
#        append = TRUE
#      }
      #if(Rdescdim) doItAndPrint(paste('write.infile(dimdesc(', nom.res, ', axes=c(', paste(Axe, collapse=", "), ')), file =',Fich,',append=',append,')', sep=""))
     if(Rgroupe){
        doItAndPrint(paste('write.infile(', nom.res, '$group, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
       if(Rcoocc){
        doItAndPrint(paste('write.infile(', nom.res, '$cooccur, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
       if(Rcramer){
        doItAndPrint(paste('write.infile(', nom.res, '$cramer, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
       if(Rtext){
        doItAndPrint(paste('write.infile(', nom.res, '$textual, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }
       if(Rreord){
        doItAndPrint(paste('write.infile(', nom.res, '$reord, file =',Fich,',append=',append,')', sep=""))
        append = TRUE
      }

    }

    # Re-chargement du tableau de départ et supression du tableau temporaire
    activeDataSet(donnee.depart)
    justDoIt(paste('remove(',activeDataSet(),'.fast)',sep=""))
    logger(paste('remove(',activeDataSet(),'.fast)',sep=""))
    # destuction de la fenêtre Top
    #closeDialog(top)
  }


################################################################################
#                   Création de la fenêtre top                                 #
################################################################################
  top<-tktoplevel(borderwidth=10)
  tkwm.title(top, gettextRcmdr("fast"))
  tkwm.geometry(top, "-100+50")

  # définition des polices
  font2<-tkfont.create(family="times",size=12,weight="bold")
  fontheading<-tkfont.create(family="times",size=18,weight="bold")

  # récupération du jeu de données actif
  donnee<-get(.activeDataSet)
  vars<-colnames(donnee)
  rows<-rownames(donnee)

  # création de la liste pour le choix des variables actives
  listdesc<-tklistbox(top,selectmode="extended",exportselection="FALSE",yscrollcommand=function(...)tkset(scr,...))
  scr <-tkscrollbar(top,repeatinterval=5,command=function(...)tkyview(listdesc,...))
  listdesc.nom<-NULL
  for (i in (1:ncol(donnee))) {
      if (is.factor(donnee[,i])) {
          tkinsert(listdesc,"end",vars[i])
          listdesc.nom<-c(listdesc.nom, vars[i])
      }
  }

  # création de tous les boutons d'options dans IlluFrame
  IlluFrame<- tkframe(top, borderwidth=2)
  Reinitializ.but<-tkbutton(IlluFrame, text=gettextRcmdr("Restart"),width=18,command=Reinitializ.funct, borderwidth=3)
       # mise en page de IlluFrame

  #Fillu.funct(label=gettextRcmdr("Select supplementary factors"), firstLabel=gettextRcmdr("Select supplementary factors"))
#  Dillu.funct(label=gettextRcmdr("Select supplementary quantitative variables"), firstLabel=gettextRcmdr("Select supplementary quantitative variables"))
#  Iillu.funct(label=gettextRcmdr("Select supplementary individuals"), firstLabel=gettextRcmdr("Select supplementary individuals"))
 PLOT.MCA(label=gettextRcmdr("Graphical options"), firstLabel=gettextRcmdr("Graphical options"))
  Sortie.funct(label=gettextRcmdr("Outputs"), firstLabel=gettextRcmdr("Outputs"))
  #tkgrid(FilluFrame, DilluFrame, IilluFrame, columnspan=7)
  tkgrid(tklabel(IlluFrame, text=""))
  tkgrid(PlotFrame, SortieFrame, Reinitializ.but, columnspan=7)
  #tkgrid.configure(FilluFrame, column=1, columnspan=1)
  tkgrid.configure(PlotFrame, column=1, columnspan=1)
  #tkgrid.configure(DilluFrame, column=3, columnspan=1)
  tkgrid.configure(SortieFrame, column=3, columnspan=1)
  #tkgrid.configure(IilluFrame, column=5, columnspan=1)
  tkgrid.configure(Reinitializ.but, column=5, columnspan=1)
  tkgrid.columnconfigure(IlluFrame,0, minsize=25)
  tkgrid.columnconfigure(IlluFrame,7, minsize=25)
  tkgrid.columnconfigure(IlluFrame,2, minsize=40)
  tkgrid.columnconfigure(IlluFrame,4, minsize=40)

  # création des options dans OptionFrame
  OptionFrame<-tkframe(top, borderwidth=2, relief="groove")
  resu.lab<-tklabel(OptionFrame,text=gettextRcmdr("Name of the result object: "))
  resu.val<-tclVar("res")
  resu<-tkentry(OptionFrame,width=10,textvariable=resu.val)
  ncp.lab<-tklabel(OptionFrame,text=gettextRcmdr("Number of dimensions: "))
  ncp.val<-tclVar("5")
  ncp<-tkentry(OptionFrame,width=5,textvariable=ncp.val)
  Axe.label<-tklabel(OptionFrame,text=gettextRcmdr("Graphical output: select the dimensions:"))
  Axe1<-tclVar("1")
  Axe2<-tclVar("2")
  Axe1.entry <-tkentry(OptionFrame,width="5",textvariable=Axe1)
  Axe2.entry <-tkentry(OptionFrame,width="5",textvariable=Axe2)
  mot.lab<-tklabel(OptionFrame,text=gettextRcmdr("Number of words (minimum): "))
  mot.val<-tclVar("2")
  mot<-tkentry(OptionFrame,width=5,textvariable=mot.val)
  separ.lab<-tklabel(OptionFrame,text=gettextRcmdr("Words separator:"))
  separ.val<-tclVar(";")
  separ<-tkentry(OptionFrame,width=5,textvariable=separ.val)
  alpha.lab<-tklabel(OptionFrame,text=gettextRcmdr("Alpha: "))
  alpha.val<-tclVar("0.05")
  alpha<-tkentry(OptionFrame,width=5,textvariable=alpha.val)
  B.lab<-tklabel(OptionFrame,text=gettextRcmdr("Number of simulation: "))
  B.val<-tclVar("200")
  B<-tkentry(OptionFrame,width=5,textvariable=B.val)
    # mise en page de OptionFrame
  tkgrid(tklabel(OptionFrame,text=gettextRcmdr("Main options"), fg = "darkred"), columnspan=8, sticky="we")
  tkgrid(tklabel(OptionFrame,text="")) # Ligne de blanc
  tkgrid(ncp.lab, ncp)
  tkgrid(Axe.label,Axe1.entry , Axe2.entry, sticky="w")
  tkgrid(mot.lab, mot)
  tkgrid(separ.lab, separ)
  tkgrid(alpha.lab, alpha)
  tkgrid(B.lab, B)
  tkgrid(resu.lab, resu)
  tkgrid(tklabel(OptionFrame,text="")) # Ligne de blanc
  tkgrid.configure(ncp.lab, resu.lab, Axe.label,mot.lab,separ.lab,alpha.lab,B.lab, column=1, columnspan=4, sticky="w")
  tkgrid.configure(ncp,mot,separ,alpha,B, resu, column=6, columnspan=2, sticky="e")
  tkgrid.configure(Axe1.entry, column=6, columnspan=1, sticky="w")
  tkgrid.configure(Axe2.entry, column=7, columnspan=1, sticky="e")
  tkgrid.columnconfigure(OptionFrame,0, minsize=25)
  tkgrid.columnconfigure(OptionFrame,5, minsize=40)
  tkgrid.columnconfigure(OptionFrame,8, minsize=25)

  appliquer.but<-tkbutton(top, text=gettextRcmdr("Apply"),width=12,command=OnAppliquer, borderwidth=3, fg="#690f96")
  OKCancelHelp(helpSubject="fast")

  #TOP
  tkgrid(tklabel(top, text=gettextRcmdr("Factorial Approach for Sorting Task data (FAST)"),font=fontheading),columnspan=3)
  tkgrid(tklabel(top,text=""))
  tkgrid(tklabel(top, text = gettextRcmdr("Select active subjects (by default all the subjects are active)"),fg = "darkred"), column=1,columnspan=2, sticky = "w")
  tkgrid(listdesc, scr, sticky = "nw")
  tkgrid.configure(scr, sticky = "ens",column=2)
  tkgrid.configure(listdesc, sticky = "ew", column=1, columnspan=2)
  tkgrid(tklabel(top,text="")) # Ligne de blanc
  tkgrid(IlluFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text="")) # Ligne de blanc
  tkgrid(OptionFrame, column=1, columnspan=1)
  tkgrid(tklabel(top,text="")) # Ligne de blanc
  tkgrid(appliquer.but, column=1, columnspan=1)
  tkgrid(tklabel(top,text="")) # Ligne de blanc
  tkgrid(buttonsFrame, column=1, columnspan=1, sticky="ew" )
  tkgrid(tklabel(top,text="")) # Ligne de blanc

}

#############################FIN FONCTION Sorting Task #############################

##########################################################################################################################################################
######################################################            FONCTION INDSCAL          ##############################################################

SensIndscal<-function(){
require(SensoMineR)
donnee<-get(.activeDataSet)
nomdonnee<-.activeDataSet
top<-tktoplevel(borderwidth=5)
tkwm.title(top,"Indscal model")


#################### Fonctions liées au bouton 'Sorties' --------------------------------------------------------------------------------------------------

    env<-environment()
    Gpoints<-FALSE
    Gweight<-FALSE
    Gsubvar<-FALSE
    Gstrain<-FALSE

onSortie<-function(){
    sortiestop<-tktoplevel(borderwidth=10)
    tkwm.title(sortiestop,"Outputs")

    onOKsortie<-function(){
        if(tclvalue(pointsValue)=="1") assign("Gpoints", TRUE, envir=env)
        else assign("Gpoints", FALSE, envir=env)
        if(tclvalue(weightValue)=="1") assign("Gweight", TRUE, envir=env)
        else assign("Gweight", FALSE, envir=env)
        if(tclvalue(subvarValue)=="1") assign("Gsubvar", TRUE, envir=env)
        else assign("Gsubvar", FALSE, envir=env)
        if(tclvalue(strainValue)=="1") assign("Gstrain", TRUE, envir=env)
        else assign("Gstrain", FALSE, envir=env)
        tkdestroy(sortiestop)
    }

    points.check <- tkcheckbutton(sortiestop)
    if (Gpoints) pointsValue <- tclVar("1")
    else pointsValue <- tclVar("0")
    tkconfigure(points.check,variable=pointsValue)
    points.lab<-tklabel(sortiestop,text="Print the coordinates of the stimuli (individuals)")

    weight.check <- tkcheckbutton(sortiestop)
    if (Gweight) weightValue <- tclVar("1")
    else weightValue <- tclVar("0")
    tkconfigure(weight.check,variable=weightValue)
    weight.lab<-tklabel(sortiestop,text="Print the subject (weight) coordinates")

    subvar.check <- tkcheckbutton(sortiestop)
    if (Gsubvar) subvarValue <- tclVar("1")
    else subvarValue <- tclVar("0")
    tkconfigure(subvar.check,variable=subvarValue)
    subvar.lab<-tklabel(sortiestop,text="Print the strain between each configuration and the stimuli configuration")

    strain.check <- tkcheckbutton(sortiestop)
    if (Gstrain) strainValue <- tclVar("1")
    else strainValue <- tclVar("0")
    tkconfigure(strain.check,variable=strainValue)
    strain.lab<-tklabel(sortiestop,text="Print the strain criterion")

    tkgrid(tklabel(sortiestop,text="Options to print the results",fg="red"))
    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(strain.lab,strain.check,sticky="w")
    tkgrid(points.lab,points.check,sticky="w")
    tkgrid(weight.lab,weight.check,sticky="w")
    tkgrid(subvar.lab,subvar.check,sticky="w")
    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(tkbutton(sortiestop,text="OK",width=9,command=onOKsortie))
    tkfocus(sortiestop)
}

####### FRAMES -------------------------------------------------------------------------------------------------------------------------------
    descopFrame<-tkframe(top)
    descFrame <- tkframe(descopFrame)
    listdesc<-tklistbox(descFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
    donnee<-get(.activeDataSet)
    nomdonnee<-.activeDataSet
    vars<-colnames(donnee)
    vars.desc = NULL
    for (i in (1:ncol(donnee))){
      if (is.numeric(donnee[,i])){
        tkinsert(listdesc,"end",vars[i])
        vars.desc = c(vars.desc,vars[i])
      }
    }
    tkgrid(listdesc, scr,sticky = "nw")
    tkgrid.configure(scr, sticky = "wns")
    tkgrid.configure(listdesc,sticky = "ew")

    optionsFrame <- tkframe(descopFrame,borderwidth=2,relief="ridge")
    crit.val<-tclVar("1e-05")
    crit.lab<-tklabel(optionsFrame,text="Convergence criterion: ")
    crit.ent<-tkentry(optionsFrame,width=6,textvariable=crit.val)
    maxit.val<-tclVar("200")
    maxit.lab<-tklabel(optionsFrame,text="Maximum number of iteration of the algorithm: ")
    maxit.ent<-tkentry(optionsFrame,width=6,textvariable=maxit.val)
    coord.val<-tclVar("1,2")
    coord.lab<-tklabel(optionsFrame,text="Dimension to plot: ")
    coord.ent<-tkentry(optionsFrame,width=6,textvariable=coord.val)
    resu.val<-tclVar("results")
    resu<-tkentry(optionsFrame,width=10,textvariable=resu.val)
    resu.lab<-tklabel(optionsFrame,text="Keep the results in: ")
    tkgrid(coord.lab,coord.ent,sticky="w")
    tkgrid(crit.lab,crit.ent,sticky="w")
    tkgrid(maxit.lab,maxit.ent,sticky="w")
    tkgrid(resu.lab,resu,sticky="w")


####### Fonction liée au bouton Appliquer, appel de la fonction Panelperf -------------------------------------------------------------------------------
App<-function(){
    variable <- vars.desc[as.numeric(tkcurselection(listdesc))+1]
    nbitemdesc<-c(tclvalue(tkcurselection(listdesc)))
    nbitemdesc<-unlist(strsplit(nbitemdesc,"\\ "))
    descriptor.var <- vars.desc[-(as.numeric(tkcurselection(listdesc))+1)]
    resultat<-tclvalue(resu.val)
    done = 0
    if ((length(nbitemdesc)<1)){
      if ((length(vars.desc)%%2)!=0) tkmessageBox(message="The number of variables selected must be a multiple of two",icon="warning",type="ok")
      else {
        done = 1
        command3=paste(resultat,' = indscal(',nomdonnee,', coord=c(',tclvalue(coord.val),'), eps=',as.numeric(tclvalue(crit.val)),',maxit=',as.numeric(tclvalue(maxit.val)),')',sep='')
      }
    }
    else{
      if ((length(nbitemdesc)%%2)!=0) tkmessageBox(message="The number of variables selected must be a multiple of two",icon="warning",type="ok")
      else{
        done = 1
        command3=paste(resultat,' = indscal(',nomdonnee,'[,c("', paste(variable, collapse='", "'), '")],',nomdonnee,'[,c("', paste(descriptor.var, collapse='", "'), '")], coord=c(',tclvalue(coord.val),'), eps=',as.numeric(tclvalue(crit.val)),',maxit=',as.numeric(tclvalue(maxit.val)),')',sep='')
      }
    }
    if (done ==1){
      justDoIt(command3)
      logger(command3)
      if (length(nbitemdesc)>0)  {
        command4 = paste('prefpls(cbind(',resultat,'$points, ',nomdonnee,'[,c("', paste(descriptor.var, collapse='", "'), '")]))',sep='')
        justDoIt(command4)
        logger(command4)
      }
      if (Gpoints){
        doItAndPrint(paste(resultat,'$points',sep=''))
      }
      if (Gweight){
        doItAndPrint(paste(resultat,'$W',sep=''))
      }
      if (Gsubvar){
        doItAndPrint(paste(resultat,'$subvar',sep=''))
      }
      if (Gstrain){
        doItAndPrint(paste(resultat,'$r2',sep=''))
      }
    }
    return(done)
}

onOK <- function(){
  done = App()
  if (done >0) tkdestroy(top)
}

######## Positionnement des widgets sur la fenêtre principale ----------------------------------------------------------------------------
sorties<-tkbutton(optionsFrame,text="Outputs",borderwidth=3,width=12,fg="darkred",command=onSortie)
appel<-tkbutton(top,text="Submit",borderwidth=3,width=12,fg="blue",command=App)
OKCancelHelp(helpSubject="indscal")
tkgrid(tklabel(top,text=""))
tkgrid(tklabel(top, text = "Select descriptors (by default, all descriptors are selected)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(descopFrame,sticky="w",columnspan=3)
tkgrid(sorties,sticky="e")
tkgrid(descFrame,tklabel(descopFrame,text="    "),optionsFrame,sticky="w")
tkgrid.configure(optionsFrame,sticky="s",columnspan=2)
tkgrid(appel, sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/indscal.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}

#########################################################           FIN FONCTION INDSCAL         ######################################################
#######################################################################################################################################################

##########################################################################################################################################################
######################################################            FONCTION PMFA          ##############################################################

SensPmfa<-function(){
require(SensoMineR)
donnee<-get(.activeDataSet)
nomdonnee<-.activeDataSet
top<-tktoplevel(borderwidth=5)
tkwm.title(top,"Procrustes Multiple Factor Analysis")


#################### Fonctions liées au bouton 'Sorties' --------------------------------------------------------------------------------------------------

    env<-environment()
    Gnappeind<-FALSE
    Glim<-"60,40"
    GRV<-FALSE
    Gdilat<-TRUE

onSortie<-function(){
    sortiestop<-tktoplevel(borderwidth=10)
    tkwm.title(sortiestop,"Options")

    onOKsortie<-function(){
        if(tclvalue(nappeindValue)=="1") assign("Gnappeind", TRUE, envir=env)
        else assign("Gnappeind", FALSE, envir=env)
        assign("Glim",tclvalue(lim.val),envir=env)
        if(tclvalue(RVValue)=="1") assign("GRV", TRUE, envir=env)
        else assign("GRV", FALSE, envir=env)
        if(tclvalue(dilatValue)=="1") assign("Gdilat", TRUE, envir=env)
        else assign("Gdilat", FALSE, envir=env)
        tkdestroy(sortiestop)
    }

    nappeind.check <- tkcheckbutton(sortiestop)
    if (Gnappeind) nappeindValue <- tclVar("1")
    else nappeindValue <- tclVar("0")
    tkconfigure(nappeind.check,variable=nappeindValue)
    nappeind.lab<-tklabel(sortiestop,text="Plot the individual tableclothe")

    lim.lab<-tklabel(sortiestop,text="Size of the tableclothe")
    lim.val<-tclVar(Glim)
    lim.ent <- tkentry(sortiestop,width=6,textvariable=lim.val)

    RV.check <- tkcheckbutton(sortiestop)
    if (GRV) RVValue <- tclVar("1")
    else RVValue <- tclVar("0")
    tkconfigure(RV.check,variable=RVValue)
    RV.lab<-tklabel(sortiestop,text="Print the RV coefficient between each configuration and the mean configuration")

    dilat.check <- tkcheckbutton(sortiestop)
    if (Gdilat) dilatValue <- tclVar("1")
    else dilatValue <- tclVar("0")
    tkconfigure(dilat.check,variable=dilatValue)
    dilat.lab<-tklabel(sortiestop,text="Dilate the individual configuration on the mean configuration")

    tkgrid(tklabel(sortiestop,text="Options",fg="red"))
    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(RV.lab,RV.check,sticky="w")
    tkgrid(dilat.lab,dilat.check,sticky="w")
    tkgrid(nappeind.lab,nappeind.check,sticky="w")
    tkgrid(lim.lab,lim.ent,sticky="w")
    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(tkbutton(sortiestop,text="OK",width=9,command=onOKsortie))
    tkfocus(sortiestop)
}

####### FRAMES -------------------------------------------------------------------------------------------------------------------------------
    descopFrame<-tkframe(top)
    descFrame <- tkframe(descopFrame)
    listdesc<-tklistbox(descFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
    donnee<-get(.activeDataSet)
    nomdonnee<-.activeDataSet
    vars<-colnames(donnee)
    vars.desc = NULL
    for (i in (1:ncol(donnee))){
      if (is.numeric(donnee[,i])){
        tkinsert(listdesc,"end",vars[i])
        vars.desc = c(vars.desc,vars[i])
      }
    }
    tkgrid(listdesc, scr,sticky = "nw")
    tkgrid.configure(scr, sticky = "wns")
    tkgrid.configure(listdesc,sticky = "ew")

    optionsFrame <- tkframe(descopFrame,borderwidth=2,relief="ridge")
    graphmfa.check <- tkcheckbutton(top)
    graphmfa.bool <- tclVar("1")
    tkconfigure(graphmfa.check,variable=graphmfa.bool)
    graphmfa.lab<-tklabel(optionsFrame,text="Plot the MFA graphs for the mean configuration")
    graphind.check <- tkcheckbutton(top)
    graphind.bool <- tclVar("1")
    tkconfigure(graphind.check,variable=graphind.bool)
    graphind.lab<-tklabel(optionsFrame,text="Plot the individual configuration with the mean configuration")
    coord.val<-tclVar("1,2")
    coord.lab<-tklabel(optionsFrame,text="Dimension to plot: ")
    coord.ent<-tkentry(optionsFrame,width=6,textvariable=coord.val)
    resu.val<-tclVar("results")
    resu<-tkentry(optionsFrame,width=10,textvariable=resu.val)
    resu.lab<-tklabel(optionsFrame,text="Keep the results in: ")
    tkgrid(coord.lab,coord.ent,sticky="w")
    tkgrid(graphmfa.lab,graphmfa.check,sticky="w")
    tkgrid(graphind.lab,graphind.check,sticky="w")
    tkgrid(resu.lab,resu,sticky="w")


####### Fonction liée au bouton Appliquer, appel de la fonction PMFA -------------------------------------------------------------------------------
App<-function(){
    variable <- vars.desc[as.numeric(tkcurselection(listdesc))+1]
    nbitemdesc<-c(tclvalue(tkcurselection(listdesc)))
    nbitemdesc<-unlist(strsplit(nbitemdesc,"\\ "))
    descriptor.var <- vars.desc[-(as.numeric(tkcurselection(listdesc))+1)]
    resultat<-tclvalue(resu.val)
    done = 0
    if ((length(nbitemdesc)<1)){
      if ((length(vars.desc)%%2)!=0) tkmessageBox(message="The number of variables selected must be a multiple of two",icon="warning",type="ok")
      else {
        done = 1
        if (Gnappeind) command1 = paste('nappeplot(',nomdonnee,')',sep='')
        command3=paste(resultat,' = pmfa(',nomdonnee,', coord=c(',tclvalue(coord.val),'), lim = c(',Glim,'), graph.mfa=',tclvalue(graphmfa.bool),',graph.ind=',tclvalue(graphind.bool),',dilat=',Gdilat,')',sep='')
      }
    }
    else{
      if ((length(nbitemdesc)%%2)!=0) tkmessageBox(message="The number of variables selected must be a multiple of two",icon="warning",type="ok")
      else{
        done = 1
        if (Gnappeind) command1 = paste('nappeplot(',nomdonnee,'[,c("', paste(variable, collapse='", "'), '")])',sep='')
        if (length(vars.desc)==length(nbitemdesc)) command3=paste(resultat,' = pmfa(',nomdonnee,'[,c("', paste(variable, collapse='", "'), '")], coord=c(',tclvalue(coord.val),'), lim = c(',Glim,'), graph.mfa=',tclvalue(graphmfa.bool),',graph.ind=',tclvalue(graphind.bool),',dilat=',Gdilat,')',sep='')
        else command3=paste(resultat,' = pmfa(',nomdonnee,'[,c("', paste(variable, collapse='", "'), '")],',nomdonnee,'[,c("', paste(descriptor.var, collapse='", "'), '")], coord=c(',tclvalue(coord.val),'), lim = c(',Glim,'), graph.mfa=',tclvalue(graphmfa.bool),',graph.ind=',tclvalue(graphind.bool),',dilat=',Gdilat,')',sep='')
      }
    }
    if (done ==1){
      if (Gnappeind){
        justDoIt(command1)
        logger(command1)
      }
      justDoIt(command3)
      logger(command3)
      if (GRV){
        doItAndPrint(paste(resultat))
      }
    }
    return(done)
}

onOK <- function(){
  done = App()
  if (done >0) tkdestroy(top)
}

######## Positionnement des widgets sur la fenêtre principale ----------------------------------------------------------------------------
sorties<-tkbutton(optionsFrame,text="Options",borderwidth=3,width=12,fg="darkred",command=onSortie)
appel<-tkbutton(top,text="Submit",borderwidth=3,width=12,fg="blue",command=App)
OKCancelHelp(helpSubject="pmfa")
tkgrid(tklabel(top,text=""))
tkgrid(tklabel(top, text = "Select descriptors (by default, all descriptors are selected)", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(descopFrame,sticky="w",columnspan=3)
tkgrid(sorties,sticky="e")
tkgrid(descFrame,tklabel(descopFrame,text="    "),optionsFrame,sticky="w")
tkgrid.configure(optionsFrame,sticky="s")
tkgrid(appel, sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/pmfa.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}

#########################################################           FIN FONCTION PMFA         ######################################################
#######################################################################################################################################################

#################################################################################################################################################
###################################################       FONCTION optimaldesign  #####################################################

SensOptDesign<-function(){
require(tcltk)
require(SensoMineR)
top<-tktoplevel(borderwidth=10)
tkwm.title(top,"Optimal Design")

#################### Fonctions liées au bouton 'Sorties' --------------------------------------------------------------------------------------------------

env<-environment()
GnbPanelistMin <- ""
Gweight <- 0.5
GnbDesignProd <- 10
GnbDesignOrdre <- 50
Ggraine <- Sys.time()
GmatEssImp <- "NA"
    
onSortie<-function(){
    sortiestop<-tktoplevel(borderwidth=10)
    tkwm.title(sortiestop,"Outputs")
        
    onOKsortie<-function(){
        assign("GnbPanelistMin",tclvalue(nbPanelistMin.val),envir=env)
        assign("GnbDesignProd",tclvalue(nbDesignProd.val),envir=env)
        assign("Gweight",tclvalue(weight.val),envir=env)
        assign("GnbDesignOrdre",tclvalue(nbDesignOrdre.val),envir=env)
        tkdestroy(sortiestop)
    }

      nbPanelistMin.lab<-tklabel(sortiestop,text="Minimum number of panelists (by default the number  of panelists)")
      nbPanelistMin.val<-tclVar(GnbPanelistMin)
      nbPanelistMin.ent <- tkentry(sortiestop,width=5,textvariable=nbPanelistMin.val)

      weight.lab<-tklabel(sortiestop,text="Weight")
      weight.val<-tclVar(Gweight)
      weight.ent <- tkentry(sortiestop,width=5,textvariable=weight.val)

      nbDesignProd.lab<-tklabel(sortiestop,text="Number of iteration of the algorithm to affect the products to the panelists")
      nbDesignProd.val<-tclVar(GnbDesignProd)
      nbDesignProd.ent <- tkentry(sortiestop,width=5,textvariable=nbDesignProd.val)

      nbDesignOrdre.lab<-tklabel(sortiestop,text="Number of iteration of the algorithm to affect the rank to the products")
      nbDesignOrdre.val<-tclVar(GnbDesignOrdre)
      nbDesignOrdre.ent <- tkentry(sortiestop,width=5,textvariable=nbDesignOrdre.val)

    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(tklabel(sortiestop,text="Options to print the results for panel performance",fg="red"))
    tkgrid(nbPanelistMin.lab,nbPanelistMin.ent,sticky="w")
    tkgrid(weight.lab,weight.ent,sticky="w")
    tkgrid(nbDesignProd.lab,nbDesignProd.ent,sticky="w")
    tkgrid(nbDesignOrdre.lab,nbDesignOrdre.ent,sticky="w")

    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(tkbutton(sortiestop,text="OK",width=9,command=onOKsortie))
    tkfocus(sortiestop)
}

####### FRAMES -------------------------------------------------------------------------------------------------------------------------------
    optionsFrame <- tkframe(top,borderwidth=3,relief="ridge")
    nbPanelist.val<-tclVar("")
    nbPanelist.lab<-tklabel(optionsFrame,text="Maximum number of panelists: ")
    nbPanelist.ent<-tkentry(optionsFrame,width=6,textvariable=nbPanelist.val)
    nbProd.val<-tclVar("")
    nbProd.lab<-tklabel(optionsFrame,text="Number of products: ")
    nbProd.ent<-tkentry(optionsFrame,width=6,textvariable=nbProd.val)
    nbProdByPanelist.val<-tclVar("")
    nbProdByPanelist.lab<-tklabel(optionsFrame,text="Number of products by panelist: ")
    nbProdByPanelist.ent<-tkentry(optionsFrame,width=6,textvariable=nbProdByPanelist.val)
    ordre.check <- tkcheckbutton(top)
    ordre.bool <- tclVar("1")
    tkconfigure(ordre.check,variable=ordre.bool)
    ordre.lab<-tklabel(optionsFrame,text="Give the rank of the product")
    resu.val<-tclVar("results")
    resu<-tkentry(optionsFrame,width=10,textvariable=resu.val)
    resu.lab<-tklabel(optionsFrame,text="Keep the results in : ")
    tkgrid(nbProd.lab,nbProd.ent,sticky="w")
    tkgrid(nbPanelist.lab,nbPanelist.ent,sticky="w")
    tkgrid(nbProdByPanelist.lab,nbProdByPanelist.ent,sticky="w")
    tkgrid(ordre.lab,ordre.check,sticky="w")
    tkgrid(resu.lab,resu,sticky="w")


####### Fonction liée au bouton Appliquer, appel de la fonction Panelperf -------------------------------------------------------------------------------
App<-function(){
    ordre.val <- as.numeric(tclvalue(ordre.bool))
    resultat<-tclvalue(resu.val)
    done = 0
    if (tclvalue(nbProd.val)=="") tkmessageBox(message="You must give the number of products",icon="warning",type="ok")
    if (tclvalue(nbPanelist.val)=="") tkmessageBox(message="You must give the number of panelists",icon="warning",type="ok")
    if (tclvalue(nbProdByPanelist.val)=="") tkmessageBox(message="You must give the number of products by panelist",icon="warning",type="ok")
    if ((as.numeric(tclvalue(nbProd.val))<as.numeric(tclvalue(nbProdByPanelist.val)))==TRUE)     
    tkmessageBox(message="The number of products by panelist must be less than the total number of products",icon="warning",type="ok")
    if ((tclvalue(nbProd.val)!="")&(tclvalue(nbPanelist.val)!="")&(tclvalue(nbProdByPanelist.val)!="")&((as.numeric(tclvalue(nbProd.val))<=as.numeric(tclvalue(nbProdByPanelist.val)))==FALSE)){
      done = 1
      commanda=paste(resultat,' = optimaldesign(nbPanelist=',as.numeric(tclvalue(nbPanelist.val)), ', nbProd=',as.numeric(tclvalue(nbProd.val)), ', nbProdByPanelist=',as.numeric(tclvalue(nbProdByPanelist.val)),sep='')
      if (GnbPanelistMin != ""){
        if (GnbPanelistMin != nbPanelist.val) commanda = paste(commanda,',nbPanelistMin=',GnbPanelistMin,sep='')
      }
      if (ordre.val==0) commanda = paste(commanda,',ordre=FALSE',sep='')
      if (Gweight!=0.5) commanda = paste(commanda,',weight=',Gweight,sep='')
      if (GnbDesignProd!=10) commanda = paste(commanda,',nbDesignProd=',GnbDesignProd,sep='')
      if (GnbDesignOrdre!=50) commanda = paste(commanda,',nbDesignOrdre=',GnbDesignOrdre,sep='')
      if (GmatEssImp!="NA") commanda = paste(commanda,',matEssImp=',GmatEssImp,sep='')
      commanda = paste(commanda,')',sep='')
      doItAndPrint(commanda)
    }
    return(done)
  }


####### Fonction liée au bouton OK, appel de la fonction Paneliperf -------------------------------------------------------------------------------
onOK <- function(){
  done = App()
  if (done >0) tkdestroy(top)
}

sorties<-tkbutton(optionsFrame,text="Options",borderwidth=3,width=12,fg="darkred",command=onSortie)
appel<-tkbutton(top,text="Submit",borderwidth=3,width=12,fg="blue",command=App)
OKCancelHelp(helpSubject="optimaldesign")
tkgrid(sorties,sticky="e")
tkgrid(optionsFrame,sticky="w")
tkgrid(tklabel(top,text=""))
tkgrid(appel, sticky="w")
tkgrid(tklabel(top,text=""))
#didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/optimaldesign.htm"))
#tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
#tkgrid.configure(didact, column=2)
tkfocus(top)
}

########       FIN FONCTION optimaldesign         #######

###########################################################################################################################################################
###################################################################          FONCTION   PANELMATCH           ################################################

SensPanelmatch<-function(){
    require(tcltk)
    require(SensoMineR)
    top<-tktoplevel(borderwidth=5)
    tkwm.title(top,"Comparison of panels")
    vars<-listDataSets()

######## FRAMES -----------------------------------------------------------------------------------------------------------------------

factFrame <- tkframe(top)
    listfact<-tklistbox(factFrame,selectmode="extended",yscrollcommand=function(...) tkset(scrfact,...))
    scrfact <- tkscrollbar(factFrame,repeatinterval=5,command=function(...)tkyview(listfact,...))
    tkgrid(listfact, scrfact,sticky = "nw")
    tkgrid.configure(scrfact, sticky = "wns")
    tkgrid.configure(listfact,sticky = "ew")

interestFrame <- tkframe(top)
    col.lab<-tklabel(interestFrame,text="'Product' factor: ",fg="darkgreen")
    col.val<-tclVar("")
    colj <- tkentry(interestFrame,width=15,textvariable=col.val)
    col2.lab<-tklabel(interestFrame,text="'Panelist' factor: ",fg="darkgreen")
    col2.val<-tclVar("")
    col2j <- tkentry(interestFrame,width=15,textvariable=col2.val)
    tkgrid(col.lab, colj,sticky = "nw")
    tkgrid(col2.lab, col2j,sticky = "nw")

descopframe<-tkframe(top)
   descFrame <- tkframe(descopframe)
   listdesc<-tklistbox(descFrame,selectmode="extended",yscrollcommand=function(...) tkset(scr,...))
   scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
   for (i in (1:length(vars))) tkinsert(listdesc,"end",vars[i])
   tkgrid(listdesc, scr,sticky = "nw")
   tkgrid.configure(scr, sticky = "wns")
   tkgrid.configure(listdesc,sticky = "ew")


  #! fonction pour la gestion des options graphiques
  couleur.a.changer<-defmacro(label, firstLabel, expr=
  {
    optionsFrame2 <- tkframe(optionsFrame,borderwidth=3)
    .PlotLabel<-tclVar(paste(firstLabel, "", sep=" "))
    env<-environment()
##    Rgraph <- TRUE
    Rcenterpan <- TRUE
    Rscalepan <- FALSE
    Rnamepan <- FALSE
    Rchoix <- "NULL"
    Rsimul <- 500
    Raxe <- "1,2"

    OnPlot<-function()
    {
      PlotWin<-tktoplevel()
      tkwm.title(PlotWin,gettextRcmdr("Options"))
      tkwm.geometry(PlotWin, "-100+50")

      #création de la fonction onOKsub
      onOKsub<-function()
      {
##        if(tclvalue(cbValue)=="1") assign("Rgraph", TRUE, envir=env)
##        else assign("Rgraph", FALSE, envir=env)
        if(tclvalue(centerpan.bool)=="1") assign("Rcenterpan", TRUE, envir=env)
        else assign("Rcenterpan", FALSE, envir=env)
        if(tclvalue(scalepan.bool)=="1") assign("Rscalepan", TRUE, envir=env)
        else assign("Rscalepan", FALSE, envir=env)
        if(tclvalue(namepan.bool)=="1") assign("Rnamepan", TRUE, envir=env)
        else assign("Rnamepan", FALSE, envir=env)
        assign("Rsimul",tclvalue(simul.val),envir=env)
        assign("Raxe",tclvalue(axe.val),envir=env)
        assign("Rchoix",tclvalue(choix.val),envir=env)
        tkdestroy(PlotWin)
      }

##      graph.check <- tkcheckbutton(PlotWin)
##      if (Rgraph) cbValue <- tclVar("1")
##      else cbValue <- tclVar("0")
##      tkconfigure(graph.check,variable=cbValue)
##      graph.lab<-tklabel(PlotWin,text="Plot the graph with the variability of the variables")

      centerpan.check <- tkcheckbutton(PlotWin)
      if (Rcenterpan) centerpan.bool <- tclVar("1")
      else centerpan.bool <- tclVar("0")
      tkconfigure(centerpan.check,variable=centerpan.bool)
      centerpan.lab<-tklabel(PlotWin,text="Center data by panelist")

      scalepan.check <- tkcheckbutton(PlotWin)
      if (Rscalepan) scalepan.bool <- tclVar("1")
      else scalepan.bool <- tclVar("0")
      tkconfigure(scalepan.check,variable=scalepan.bool)
      scalepan.lab<-tklabel(PlotWin,text="Scale data by panelist")

      namepan.check <- tkcheckbutton(PlotWin)
      if (Rnamepan) namepan.bool <- tclVar("1")
      else namepan.bool <- tclVar("0")
      tkconfigure(namepan.check,variable=namepan.bool)
      namepan.lab<-tklabel(PlotWin,text="Name of each panelist is displayed on the 'plotpanelist' graph ")

      axe.lab<-tklabel(PlotWin,text="Dimension to plot")
      axe.val<-tclVar(Raxe)
      axe.ent <- tkentry(PlotWin,width=6,textvariable=axe.val)

      simul.lab<-tklabel(PlotWin,text="Number of simulations used to compute the ellipses")
      simul.val<-tclVar(Rsimul)
      simul.ent <- tkentry(PlotWin,width=15,textvariable=simul.val)

      choix.lab<-tklabel(PlotWin,text="Number of panelists forming a virtual panel (if NULL, the number in the true panel) ")
      choix.val<-tclVar(Rchoix)
      choix.ent <- tkentry(PlotWin,width=15,textvariable=choix.val)

      #mise en page des différents frames de PlotIndFrame
      tkgrid(tklabel(PlotWin, text=" "))
      tkgrid(axe.lab,axe.ent,sticky="w")
      tkgrid(simul.lab,simul.ent,sticky="w")
      tkgrid(choix.lab,choix.ent,sticky="w")
      tkgrid(centerpan.lab,centerpan.check,sticky="w")
      tkgrid(scalepan.lab,scalepan.check,sticky="w")
      tkgrid(namepan.lab,namepan.check,sticky="w")
##      tkgrid(graph.lab,graph.check,sticky="w")

      subOKCancelHelp(PlotWin, helpSubject=NULL)
      tkgrid(subButtonsFrame, sticky="ew")
    }
    Plot.but<-tkbutton(optionsFrame2, textvariable=.PlotLabel, command=OnPlot, borderwidth=3, width=13)
    tkgrid(Plot.but, sticky="w")
  })

optionsFrame <- tkframe(top,relief="ridge",borderwidth=2)
    scale.check <- tkcheckbutton(top)
    scale.bool <- tclVar("1")
    tkconfigure(scale.check,variable=scale.bool)
    scale.lab<-tklabel(optionsFrame,text="Scale unit the variables:")

    prod.val<-tclVar("Product")
    prod<-tkentry(optionsFrame,width=15,textvariable=prod.val)
    prod.lab<-tklabel(optionsFrame,text="Name of the 'Product' factor: ")
    panelist.val<-tclVar("Panelist")
    panelist<-tkentry(optionsFrame,width=15,textvariable=panelist.val)
    panelist.lab<-tklabel(optionsFrame,text="Name of the 'Panelist' factor: ")

##    searchlevel.val<-tclVar("0.20")
##    searchlevel<-tkentry(optionsFrame,width=5,textvariable=searchlevel.val)
##    searchlevel.lab<-tklabel(optionsFrame,text="Significant level to select descriptors: ")
    seuil.val<-tclVar("0.05")
    seuil<-tkentry(optionsFrame,width=5,textvariable=seuil.val)
    seuil.lab<-tklabel(optionsFrame,text="Significant level for the ellipses: ")
    resu.val<-tclVar("results")
    resu<-tkentry(optionsFrame,width=10,textvariable=resu.val)
    resu.lab<-tklabel(optionsFrame,text="Keep the results in: ")
    tkgrid(prod.lab,prod,sticky="w")
    tkgrid(panelist.lab,panelist,sticky="w")
    tkgrid(scale.lab,scale.check,sticky="w")
##    tkgrid(searchlevel.lab,searchlevel,sticky="w")
    tkgrid(seuil.lab,seuil,sticky="w")
    tkgrid(resu.lab,resu,sticky="w")
    couleur.a.changer(label=gettextRcmdr("Options"), firstLabel=gettextRcmdr("Options"))

descFrame <- tkframe(top)
    listdesc<-tklistbox(descFrame,selectmode="extended",exportselection=FALSE,yscrollcommand=function(...) tkset(scr,...))
    scr <- tkscrollbar(descFrame,repeatinterval=5,command=function(...)tkyview(listdesc,...))
    vars.desc = NULL
    for (i in (1:length(vars))){
        tkinsert(listdesc,"end",vars[i])
        vars.desc = c(vars.desc,vars[i])
    }
    tkselection.set(listdesc,0)
    tkgrid(listdesc, scr,sticky = "nw")
    tkgrid.configure(scr, sticky = "wns")
    tkgrid.configure(listdesc,sticky = "ew")
    tkselection.set(listdesc,0)


#################### Fonctions liée au bouton 'Sorties' --------------------------------------------------------------------------------------------------
    env<-environment()
    Geig<-FALSE
    Gcoord<-FALSE
    Gcoef<-FALSE
    Ghotel<-TRUE

onSortie<-function(){
    sortiestop<-tktoplevel(borderwidth=10)
    tkwm.title(sortiestop,"Outputs")

    onOKsortie<-function(){
        if(tclvalue(eigValue)=="1") assign("Geig", TRUE, envir=env)
        else assign("Geig", FALSE, envir=env)
        if(tclvalue(coordValue)=="1") assign("Gcoord", TRUE, envir=env)
        else assign("Gcoord", FALSE, envir=env)
        if(tclvalue(coefValue)=="1") assign("Gcoef", TRUE, envir=env)
        else assign("Gcoef", FALSE, envir=env)
        if(tclvalue(hotelValue)=="1") assign("Ghotel", TRUE, envir=env)
        else assign("Ghotel", FALSE, envir=env)
        tkdestroy(sortiestop)
    }

    eig.check <- tkcheckbutton(sortiestop)
    if (Geig) eigValue <- tclVar("1")
    else eigValue <- tclVar("0")
    tkconfigure(eig.check,variable=eigValue)
    eig.lab<-tklabel(sortiestop,text="Eigenvalues")

    coord.check <- tkcheckbutton(sortiestop)
    if (Gcoord) coordValue <- tclVar("1")
    else coordValue <- tclVar("0")
    tkconfigure(coord.check,variable=coordValue)
    coord.lab<-tklabel(sortiestop,text="Coordinates of the products")

    coef.check <- tkcheckbutton(sortiestop)
    if (Gcoef) coefValue <- tclVar("1")
    else coefValue <- tclVar("0")
    tkconfigure(coef.check,variable=coefValue)
    coef.lab<-tklabel(sortiestop,text="Matrix with the correlation coefficients and confidence level")

    hotel.check <- tkcheckbutton(sortiestop)
    if (Ghotel) hotelValue <- tclVar("1")
    else hotelValue <- tclVar("0")
    tkconfigure(hotel.check,variable=hotelValue)
    hotel.lab<-tklabel(sortiestop,text="Hotelling tests between products")

    tkgrid(tklabel(sortiestop,text="Options to print the results",fg="red"))
    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(eig.lab,eig.check,sticky="w")
    tkgrid(coord.lab,coord.check,sticky="w")
    tkgrid(coef.lab,coef.check,sticky="w")
    tkgrid(hotel.lab,hotel.check,sticky="w")
    tkgrid(tklabel(sortiestop,text=""))
    tkgrid(tkbutton(sortiestop,text="OK",width=9,command=onOKsortie))
    tkfocus(sortiestop)
}


###### Fonction principale qui lance PANELMATCH sans fermer la fenêtre------------------------------------------------------------------------------------------------------------
App<-function(){
    nbitemlist<-c(tclvalue(tkcurselection(listdesc)))
    nbitem<-unlist(strsplit(nbitemlist,"\\ "))
    variable<-vars.desc[as.numeric(tkcurselection(listdesc))+1]
    resultat<-tclvalue(resu.val)
    done = 0
    if (tclvalue(prod.val)=="") tkmessageBox(message="No variable selected for the first factor of interest",icon="warning",type="ok")
    if (tclvalue(panelist.val)=="") tkmessageBox(message="No variable selected for the second factor of interest",icon="warning",type="ok")
    if ((tclvalue(prod.val)!="")&(tclvalue(panelist.val)!="")){
      for (j in 1:length(variable)){
        nom.donnee = variable[j]
        donnee = get(nom.donnee)
        truc = NULL
        if (!any(i <- grep(tclvalue(prod.val) ,colnames(donnee)))) tkmessageBox(message="The variable Product is unknown",icon="warning",type="ok")
        else col.pos = grep(tclvalue(prod.val) ,colnames(donnee))
        if (!any(i <- grep(tclvalue(panelist.val) ,colnames(donnee)))) tkmessageBox(message="The variable Panelist is unknown",icon="warning",type="ok")
        else col2.pos = grep(tclvalue(panelist.val) ,colnames(donnee))
        for (k in 1:ncol(donnee)){
          if (is.numeric(donnee[,k])) truc = c(truc,k)
        }
        done = 1
        commande.data<-paste(nom.donnee,'.aux <-', nom.donnee,'[,c(',col.pos,',',col2.pos,',',paste(truc,collapse=','),')]',sep='')
        justDoIt(commande.data)
        logger(commande.data)
        if (j==1) aaa = c(variable[j], "=",variable[j],".aux",sep="")
        else aaa = c(aaa, ",", variable[j], "=",variable[j],".aux",sep="")
      }
      commande <- paste(resultat,'<-panelmatch(list(',paste(aaa,collapse=""), '), col.p=1, col.j=2, firstvar=3, alpha=',as.numeric(tclvalue(seuil.val)),',coord = c(',Raxe,'), nbsimul =',Rsimul,',nbchoix =',Rchoix,',scale.unit=',tclvalue(scale.bool),',centerbypanelist =',Rcenterpan, ',scalebypanelist=',Rscalepan, ',name.panelist=', Rnamepan , ')',sep='')
      justDoIt(commande)
      logger(commande)
      for (j in 1:length(variable)) doItAndPrint(paste('remove(',variable[j],'.aux)',sep=""))

      if (Geig){
        doItAndPrint(paste(resultat,'$eig',sep=''))
        commande1 <- paste('barplot(',resultat,'$eig[,1], main="Eigenvalues", xlab="Dimension", ylab="Eigenvalues",names.arg=1:nrow(',resultat,'$eig))',sep='')
        doItAndPrint(paste('dev.new()'))
        justDoIt(commande1)
        logger(commande1)
      }
      if (Gcoord){
        doItAndPrint(paste(resultat,'$coordinates',sep=''))
      }
      if (Gcoef){
        doItAndPrint(paste(resultat,'$correl',sep=''))
      }
      if (Ghotel){
        doItAndPrint(paste(resultat,'$hotelling$byproduct',sep=''))
        doItAndPrint(paste(resultat,'$hotelling$bypanel',sep=''))
      }
    }
    return(done)
}


###### Fonction principale qui lance PANELMATCH et ferme la fenêtre------------------------------------------------------------------------------------------------------------
onOK <- function(){
  done = App()
  if (done >0) tkdestroy(top)
}


##### Positionnement des widgets et frames sur la fenêtre 'top' ------------------------------------------------------------------------------------------
App.but <- tkbutton(top,borderwidth=3,width=12,text="Submit",command=App,fg="blue")
sorties.but<-tkbutton(optionsFrame,text="Outputs",command=onSortie,width=13,default="active",fg="darkred")

OKCancelHelp(helpSubject="panelmatch")
tkgrid(tklabel(top, text = "Select panels", fg = "blue"), columnspan = 2, sticky = "w")
tkgrid(descFrame,optionsFrame,sticky="w")
tkgrid.configure(optionsFrame,column=1, columnspan=2)
tkgrid(optionsFrame2,sorties.but,sticky="e")
tkgrid(tklabel(top,text=""))
tkgrid(App.but,sticky="w")
tkgrid(tklabel(top,text=""))
didact<-tkbutton(top,text="Didacticiel",borderwidth=3,width=12,fg="blue",command=function()browseURL("http://sensominer.free.fr/panelmatch.htm"))
tkgrid(buttonsFrame, didact)
tkgrid.configure(buttonsFrame, columnspan=2)
tkgrid.configure(didact, column=2)
tkfocus(top)
}

######################################################           FIN FONCTION PANELMATCH         #############################################################
#########################################################################################################################################################

