
# Movies Tracker

For this assignment, you will build a simple movie platform where any user can
register and add movies to their watched list.

You should write two programs for this assignment.

The first program is the movie platform server.  When run, the program will
launch a server that will keep running until explicitly killed.  This server
accepts HTTP requests to a handful of endpoints and replies accordingly to the
user making the request.

The server will write in a JSON database the registered users and their list of
watched movies. When the server is started it should load the existing JSON
database. (You can discuss the design of this JSON file with me)

The server should listen on port 7143 and react to the following HTTP requests:

* `POST /register --data {user:"rodrigo", password:"1234"}`
    When a user sends a POST request to the /register path, with a user and a
    password, the server should add a new user to the JSON database and reply
    with a unique token (can be a string) that the user can use to manipulate
    their movies.  If the user already exists, we check the password matches the
    registered one and return their unique token. If the user exists but the
    password doesn't match, return error code 403. Example reply
    `yrpghst3reczxplg`

* `POST /movie --data {token:"yrpghst3reczxplg", movie:{name: "In The Mood For Love", rating:9}}`
    When a user sends a POST request to the /movie path, with a token and a
    movie, the server should add the movie to the user corresponding to the
    token. If the user doesn't exist, return the 403 error code, otherwise
    return the code 200

* `GET /movies?token=yrpghst3reczxplg&query="Mood"`
    When a user sends a GET request to hte /movie path, with a query param for
    the token and a query param for the query, the server should search in the
    list of movies of the user corresponding to the token.
    If the user doesn't exist, return the 403 error code, otherwise
    return the list of movies whose names include the query (returning all
    movies if the movie query doesn't exist)

The second program should be a client for this server. That is, it's a program
that, when run, will send HTTP requests to the server according to the user
need. You can implement this either as a library (and run it with ghci), or as a
REPL (though I'm not sure it would be worth the trouble)

This program should define at least these three functions
```hs
-- | Get the server token by registering/logging in with the given username and password
register :: String -> String -> Token

-- | Add a movie to your user list in the server, given the user token, the name
-- of the movie and the rating
addMovie :: Token -> String -> Int -> IO ()

-- | Search for movies given a user token and a query string
findMovies :: Token -> String -> IO [Movie]
```

There might be some code you want to share between the client and the server,
using an independent module.

For extra complexity you can save the hash of the password in the JSON file
rather than password itself.

## Example usage:

This example assumes the server is already running, and you've loaded the client
module in GHCi

```hs
> tok <- register "Rodrigo" "1234"
> addMovie tok "In The Mood For Love" 9
> addMovie tok "Punch-Drunk Love" 7
> addMovie tok "The Matrix" 10
> findMovies tok "Love"
  -- [Movie "In The Mood For Love" 9, Movie "Punch-Drunk Love" 7]
> tok2 <- register "Liam" "5678"
> findMovies tok2 "Love"
  -- []
```
## Details:

When defining a server application with `Servant`, remember there are
essentially three steps:

* Model the API as a type
* Describe/implement a `Server` for this API/Type
* Start the server by 1. transforming it to a WAI application 2. using the
    `Warp.run` function

Remember to create a cabal project and add both `servant`, `servant-server`,
`warp`, `aeson` and `http-conduit` as dependencies, and remember to keep your
project under version control using `git`. You can additionally create a
repository on GitHub so that I can better comment on your progress. If you
eventually see the need to share code between the client and the server, you
might want to run `cabal init --interactive` and edit the cabal file to have 2
applications and 1 library under the same package -- but do consult me on this,
if you do want to do so.

For this project you have more freedom, but as usual I will give you hints to
guide your progress when needed.

