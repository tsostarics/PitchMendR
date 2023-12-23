windowResizeUI <- function(id) {
  ns <- NS(id)
  # These next two let us listen for a window resize event and then have
  # the server send custom messages to resize the jqui_resizable div
  tagList(
    tags$head(
      tags$script(HTML(
        paste0('
          var resizeTimer;
          $(window).resize(function() {
            clearTimeout(resizeTimer);
            resizeTimer = setTimeout(function() {
              var w = $(window).width();
              var h = $(window).height();
              var obj = {width: w, height: h};
              Shiny.onInputChange("windowChange", obj);
            }, 300);
          });
        ')))
    ),
    tags$head(
      tags$script(
        HTML("
      Shiny.addCustomMessageHandler('updateJquiWrapper', function(message) {
        $('.jqui-wrapper').css('width', message.width);
      $('.jqui-wrapper').css('height', message.height);
      });
    ")
      )
    )
  )

}

windowResizeServer <- function(id, windowChange) {
  moduleServer(id, function(input, output, session) {
    # When the user resizes the window, the size of the resizable plot might be
    # too big and force them to scroll to the bottom left to make it small
    # enough for the new window size. So, this listens for a window resize event
    # then automatically resizes the plot to be 60% of the window dimensions.
    # It's not perfect but it's much better than the plot being too big.
    observeEvent(windowChange(),{
      # message("window resize")
      new_height = paste0(round((windowChange()$height) * 0.6, 0), "px")
      new_width = paste0(round((windowChange()$width) * 0.6, 0), "px")

      session$sendCustomMessage(type = 'updateJquiWrapper', message = list(width = new_width,
                                                                           height = new_height))
    })
  })
}
