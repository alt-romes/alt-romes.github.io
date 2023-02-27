---
author: Rodrigo Mesquita
title: Assignment 7
---

# Stocks

For this assignment, you will build a simple stock market watch-list app.

This app has three defining features:

- Inspect a SYMBOL
- Create and edit a watchlist
- Inspect the whole watchlist

## Example usage:

Display the current price, and today's change
```
> show IBM
IBM:    141,18$    +0.73%
```

Add a symbol to your watchlist
```
> watch IBM
IBM was added to your watchlist

> watch AMZN
AMZN was added to your watchlist
```

Display your watchlist (for bonus points try to align the values, but at first
just use tabs or spaces)
```
> watchlist
IBM:       $141,18    +0.73%
AMZN:    $2 447,00    -2.52%
```

Remove symbol from watchlist
```
> unwatch IBM
IBM was removed from your watchlist
```

## Details:

The watchlist should persist across uses! This means when you run the program
the watchlist should be exactly as you left it when you closed the program.

To this effect the watchlist should be stored as JSON in a normal file. When the
user adds a symbol the file is edited or created, and when the program starts,
the watchlist is loaded from the file.

The information about the symbols should be fetched with HTTP requests using
`http-conduit` through a URL similar to the following (you might want to change
query parameters):

```
https://www.alphavantage.co/query?function=GLOBAL_QUOTE&symbol=IBM&apikey=demo
```

Note the different components of the URL you'll need to take into consideration
when constructing the http request

Remember to create a cabal project and add both `aeson` and `http-conduit` as
dependencies, and remember to keep your project under version control using
`git`. You shall additionally create repository on GitHub so that I can better
comment on your progress.

For this project I'm giving you full initial freedom, and again will give you
hints to guide your progress when needed.

Initially it should be a text-based app, but if you'd like later on you can use
a library like `brick` to make it a TUI.

