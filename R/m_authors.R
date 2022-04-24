### AUTHORS MODULE #####################################################

authors_UI <- function(id) {
  tagList(
    susPage(class="sus-page-authors", footer = susFooter(), susPageSection(
      h2(paste0("Authors")),
      susAuthor("Socrates", "Philosopher", "https://cdn.britannica.com/69/75569-050-7AB67C4B/herm-Socrates-half-original-Greek-Capitoline-Museums.jpg",
                susAuthorBio("Socrates was a Greek philosopher from Athens who is credited as the founder of Western philosophy and among the first moral philosophers of the ethical tradition of thought. An enigmatic figure, Socrates authored no texts and is known mainly through the posthumous accounts of classical writers, particularly his students Plato and Xenophon."),
                susAuthorLink("Wikipedia", href="https://en.wikipedia.org/wiki/Socrates",
                              icon=tags$img(src="https://iconarchive.com/download/i54074/danleech/simple/wikipedia.ico")),
                susAuthorLink("Encyclopedia Britannica", href="https://www.britannica.com/biography/Socrates",
                              icon=tags$img(src="https://cdn.britannica.com/mendel-resources/3-63/images/EBLogo.jpg?v=3.63.12")),
                susAuthorLink("Stanford Encyclopedia of Philosophy", href="https://plato.stanford.edu/entries/socrates/")
                ),
      susAuthor("Plato", "Philosopher", "https://upload.wikimedia.org/wikipedia/commons/thumb/8/88/Plato_Silanion_Musei_Capitolini_MC1377.jpg/440px-Plato_Silanion_Musei_Capitolini_MC1377.jpg",
                susAuthorBio("Plato was a Greek philosopher born in Athens during the Classical period in Ancient Greece. He founded the Platonist school of thought and the Academy, the first institution of higher learning in the Western world."),
                susAuthorLink("Wikipedia", href="https://en.wikipedia.org/wiki/Plato",
                              icon=tags$img(src="https://iconarchive.com/download/i54074/danleech/simple/wikipedia.ico")),
                susAuthorLink("Encyclopedia Britannica", href="https://www.britannica.com/biography/Plato",
                              icon=tags$img(src="https://cdn.britannica.com/mendel-resources/3-63/images/EBLogo.jpg?v=3.63.12"))
                )
      )))
}


authors_server <- function(id) {
  moduleServer(id, function(input, output, session) {})
  }
