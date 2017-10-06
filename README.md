# Dissertation
PhD Thesis: Supporting Password Coping Strategies with Persuasive Design

CodeName ***spacoswipers*** 

## Commands

In `~/.bash_profile` add `export PATH=$PATH:/Users/Tobi/Dissertation/bin` (or whatever path is used).

Then you get:
1. `dpush` <br /> Creates an automatic commit and pushes to all repositories registered inside the shell-script. 

2. `dpull` <br /> Fetches all repos, pulls from origin and master branch from other repos. 

3. `dsyncdb` <br /> Uses `rsync` to copy the source files to DropBox. 

## Dependencies
1. `onchange` to watch files -- `npm install -g onchange`
Then you can `onchange '/Users/Tobi/Dissertation/**/*.tex' -- dsyncdb`

## Structure
`/raw` contains the raw content, without styling. 