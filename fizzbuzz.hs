module FizzBuzz where

fizzbuzz = zipWith (++) fizz buzz
  where fizz = "" : "" : "Fizz" : fizz
        buzz = "" : "" : "" : "" : "Buzz" : buzz