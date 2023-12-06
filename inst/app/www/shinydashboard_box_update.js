/*
Shiny dashboard boxes do not evaluate the content if expanded since shiny still
believes that the inputs are hidden and because hidden inputs are not updated 
you observe this behavior. Only if the box itself is expanded the content is 
updated but not if the encapsulating box is expanded.
The solution is the following javascript code.
source: https://stackoverflow.com/questions/68080033/shinydashboard-box-collapse/68133165#68133165
*/
$(() => $('body').on('shown.bs.collapse', '.box', function(evt) { 
      setTimeout(function(){
         $(evt.target).find('.shiny-bound-output').trigger('shown.bs.collapse');
      }, 800);
   }));