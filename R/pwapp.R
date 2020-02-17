
#' app to retrieve genes corresponding to one of a selection of pathways named in Gene Ontology
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel helpText selectInput mainPanel runGadget runApp actionButton stopApp
#' @import GO.db
#' @import org.Hs.eg.db
#' @importFrom DT renderDataTable dataTableOutput
#' @import miniUI
#' @import AnnotationDbi
#' @note Uses org.Hs.eg.db and GO.db to obtain pathway names and gene mappings.
#' @export
genes_from_pathway = function() {
ui = fluidPage(
 sidebarLayout(
  sidebarPanel(
   helpText("Select a GO category"),
   selectInput("pws", "categories", choices=edxAdvBioc::gopw_selected$TERM),
   helpText("To retrieve selected pathway:"),
   actionButton("stopBtn", "stop/return")
   ),
  mainPanel(
   DT::dataTableOutput("genes")
   )
  )
 )
 server = function(input, output) {
  gettab = reactive({
   categ = input$pws
   categ_tag = AnnotationDbi::mapIds(GO.db, keys=categ, keytype="TERM", column="GOID")[[1]]
   ans = AnnotationDbi::select(org.Hs.eg.db, keys=categ_tag, keytype="GO", columns=c("GENENAME", "SYMBOL"))
   ans
   })
  output$genes = DT::renderDataTable({
   #categ = input$pws
   #categ_tag = AnnotationDbi::mapIds(GO.db, keys=categ, keytype="TERM", column="GOID")[[1]]
   #ans = AnnotationDbi::select(org.Hs.eg.db, keys=categ_tag, keytype="GO", columns=c("GENENAME", "SYMBOL"))
   #ans
   gettab()
   })
  observeEvent(input$stopBtn, {
       ans = gettab()
       stopApp(returnValue=ans)
       })  
  }
runApp(list(ui=ui, server=server))
}
  
   
