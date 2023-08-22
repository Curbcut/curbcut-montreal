screenshot_UI <- function(id) {
  tagList(
    tags$head(tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/html2canvas/0.5.0-beta4/html2canvas.js")),
    actionButton(shiny::NS("capture"), "Capture Screenshot"),
    downloadLink(shiny::NS("download"), "Download JPG"),
    tags$script("
      function captureScreenshot(elements) {
        html2canvas(document.getElementById(elements[0])).then(function(canvas1) {
          html2canvas(document.getElementById(elements[1])).then(function(canvas2) {
            var finalCanvas = document.createElement('canvas');
            finalCanvas.width = canvas1.width + canvas2.width;
            finalCanvas.height = canvas1.height;

            var context = finalCanvas.getContext('2d');
            context.drawImage(canvas1, 0, 0);
            context.drawImage(canvas2, canvas1.width, 0);

            var link = document.getElementById('", shiny::NS(id, "download"), "');
            link.href = finalCanvas.toDataURL('image/jpeg');
            link.download = 'screenshot.jpg';
          });
        });
      }
    ")
  )
}

screenshot_server <- function(id, elements_capture) {
  shiny::moduleServer(id, function(input, output, session) {
    observeEvent(input$capture, {
      session$sendCustomMessage(type = 'captureScreenshot', message = elements_capture)
    })
  })
}
