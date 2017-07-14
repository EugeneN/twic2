# README #

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
 note: this takes a long time

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
