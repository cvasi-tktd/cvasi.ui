
#' deactivateMouseWheel
#'
#' Deactivates the changing of the numeric input field with the mouse wheel
#'
#' @return js code to deactivate the mouse wheel
#' @noRd
deactivateMouseWheel <- function(){
  # Deactivate mousewheel
  tags$script(htmltools::HTML("
      $(function(){
        // Disable Mouse scrolling
        $('input[type=number]').on('wheel',function(e){ $(this).blur();});

        // Disable keyboard scrolling
        $('input[type=number]').on('keydown',function(e) {
          var key = e.charCode || e.keyCode;
          // Disable Up and Down Arrows on Keyboard
          if(key == 38 || key == 40 ) {
            e.preventDefault();
          } else {
            return;
          }
        });
      });
      "))
}
