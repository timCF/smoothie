# smoothie

Functional Smooothies Inc. is releasing a new smoothie machine that will make the best icy fruit beverages of all time. According to Wikipedia:

```
A smoothie (occasionally spelled smoothee or smoothy) is a thick, cold beverage made from pureed raw fruit blended with ice cream or frozen yogurt along with other ingredients such as crushed ice, fruit juice, sweeteners, dairy products, nuts, seeds, etc.
```

In order to sell the machine to smoothie vendors all over the world, Functional Smooothies needs to ensure that the machine takes all dietary preferences and allergies into account. They have hired you to write the software for the machine.

The software has a menu of standard smoothie options, including the ingredients for each drink. When a customer places their order, they supply a list of zero or more dietary restrictions that must be honoured. Your software will print out a list of the ingredients that the smoothie operator needs to put into the machine.

The menu options, along with the ingredients needed for each are as follows.

- Classic: strawberry, banana, pineapple, mango, peach, honey, ice, yogurt
- Forest Berry: strawberry, raspberry, blueberry, honey, ice, yogurt
- Freezie: blackberry, blueberry, black currant, grape juice, frozen yogurt
- Greenie: green apple, kiwi, lime, avocado, spinach, ice, apple juice
- Vegan Delite: strawberry, passion fruit, pineapple, mango, peach, ice, soy milk
- Just Desserts: banana, ice cream, chocolate, peanut, cherry

You should write a function called `ingredients`, which takes as input a string containing item from the menu and optionally one or more ingredients to omit from the smoothie, separated by commas. The function should return a string listing the ingredients that the operator needs to put in. To make it more convenient for the operator, the ingredients should be listed in alphabetical order and separated by commas in the string returned from the function.

For example, if a customer orders a Classic but is allergic to strawberry, your function will be called with the argument `"Classic,-strawberry"` should return the string `"banana,honey,ice,mango,peach,pineapple,yogurt"`.

You may assume that input to your function is always valid.
