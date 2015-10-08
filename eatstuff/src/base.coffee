disable = (element) ->
  unless $(element).hasClass("disabled")
    $(element).addClass("disabled")

jQuery ->
  $("button.oneuse").click ->
    disable this

