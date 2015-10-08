# Add a checkbox for an eating class
addCheckbox = (name) ->
  elem = $("
    <div class='item'>
      <div class='ui checkbox'>
        <input name='#{name}' type='checkbox'>
        <label>#{name}</label>
      </div>
    </div>
    ")
  $("div#eatingClasses").append($(elem))

# Add all the foods to the left column. For initial setup only
addFood = (name) ->
  elem = $("<div class='item'>#{name}</div>")
  $("div#allowed").append($(elem))

# Run this as soon as the page is loaded
jQuery ->

  # Add disable callback to relevant buttons
  $("button.oneuse").click ->
    disable this

  # List the different classes that a person can be part of and add a checkbox
  # for each one
  eatingClasses = [ "Vegan", "Vegetarian", "Muslim", "Jewish",
                    "Lactose intolerant" ]
  addCheckbox(ec) for ec in eatingClasses.sort()

  # List the different foods for adding them easily
  # TODO sort them in categories to check for status
  foods = [ "Pork", "Fish", "Ham", "Milk", "Cheese" ]
  addFood(f) for f in foods.sort()

