-- Unfortunately I wasn't able to complete exercises 7 and 8. However, this is how I would go about finishing them.
-- Exercise 7:
-- In order to implement the exercise, the luhn algorithm must be implemented.
-- This can then be used to calculate the check digit, if this equals the original check digit the number is valid.
-- This can be done by doubling every other digit from right to left and calculating the sum as described in the wiki-page:
-- https://en.wikipedia.org/wiki/Luhn_algorithm#Description
-- In order to check from what company a card is, we can use the descriptions found at wikipedia:
-- https://en.wikipedia.org/wiki/Payment_card_number
-- Here we can see that American Express uses 34 and 37 for the first two numbers, Master uses 51–55 and Visa uses 4.
-- Along with this the cards are 15, 16 and 13 or 16 digits in length respectively.
-- We can use this to check if a card is from a certain company alongside with the made luhn algorithm checker.
-- A test of correctness can be made by using quickcheck along with known valid and invalid card number to very if everything works as intended.
-- Time spend: 15 minutes (for the explanation)
-- Exercise 8:
-- This Exercise should be made by following the conventions of the functions on the canvas page and then implementing what each boy says.
-- By implementing each case, you create a situation where you can "plug-in" each name and the eventual perpetrator comes out at the end.
-- The lists of the guilty and honest are made as described above, by feeding each boy into every statement. The honest needs to have a check
-- that takes in to account the person found guilty and what persons accused that boy.
-- Time spend: 20 minutes (for the explanation)
