# Dissertation
PhD Thesis: Supporting Users in Password Authentication with Persuasive Design

CodeName ***susipawipedes*** 

## Commands

In `~/.bash_profile` add `export PATH=$PATH:/Users/Tobi/Dissertation/bin` (or whatever path is used).

Then you get:
1. `dpush` <br /> Creates an automatic commit and pushes to all repositories registered inside the shell-script. 

2. `dpull` <br /> Fetches all repos, pulls from origin and master branch from other repos. 

3. `dsyncdb` <br /> Uses `rsync` to copy the source files to DropBox. 

4. `watchtex` <br /> Internally calls `dbsyncdb` and moves watching in the background. Beware: Once shell is closed, the watching stops! (no service is started, it's only a child process of bash)

## Dependencies
1. `onchange` to watch files -- `npm install -g onchange`
Then you can `onchange '/Users/Tobi/Dissertation/**/*.tex' -- dsyncdb`

## Structure
`/raw` contains the raw content, without styling. 