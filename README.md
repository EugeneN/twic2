# README #

[![Join the chat at https://gitter.im/twic2/Lobby](https://badges.gitter.im/twic2/Lobby.svg)](https://gitter.im/twic2/Lobby?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Twic2 is an experiment in teaching Haskell and architecture by writing new frontend for Twic1.


# Step by step instructions

## First time only

1. Install Stack

   Follow instructions here: https://docs.haskellstack.org/en/stable/install_and_upgrade/

2. Install node-7.4.0

   hint: you can use `nvm` (https://github.com/creationix/nvm)


3. Clone
   ```
   $ git clone https://github.com/EugeneN/twic2.git
   $ cd twic2
   ```

4. Setup
   ```
   $ make setup
   ```
   note: this takes a *long* time

5. Configure

   Follow instructions here: https://github.com/EugeneN/twic/wiki/Quick-start#how-to-configure-and-run-twic

## Every time

6. Build
   ```
   $ make frontend
   $ make backend
   ```

7. Run

   ```
   $ make run
   ```

   Default browser will open with twic UI. In case you happen to use poorly configured desktop environment open http://localhost:3000 manually.

 ---

 # Next steps

 The goal is to write new UI for twic. For this we must discuss the architecture for the frontend (UI) application, implementation details and visual design/UX. More details on original twic design are here: https://github.com/EugeneN/twic/wiki/About

 The architecture will not be Elm/React-like one obviously. In general this should be a standalone single page application with support for
 offline work. The app source should be served by the backend, and once initialized in a browser it should connect to the backend streaming API using websockets.

 The app will consist of several independent components - a global feed, individual tweet feed, new tweet, retweet, reply components, search, notifications, private messages.

 These business tasks influence implementation details - namely, implementation must provide a way to work with standalone isolated incapsulated composable components. There are few candidate libraries for this task, which provide different levels of abstractions.

 Visual design and UX should be similar to twic1 one's, with the difference that it should not use right click as the main and only input method.


---

# Notes

- happy bug https://github.com/commercialhaskell/stack/issues/1258#issuecomment-229167556

  Workaround:

  ```
  cd frontend
  GHCPATH=`cd ../backend && stack exec which ghc`
  PATH=$PATH:`dirname $GHCPATH` stack build
  ```
