$(document).ready(function(){
  $('#condicional_rubro_elegido').on('show', function(event){
    $(this).css('opacity', 0).animate({opacity: 1}, {duration: 1000});
  });
});