# Work Tips

Things I did to write the thesis

## Philosophy
- created a "distraction free" browser profile. I switch to that to avoid checking incoming emails and other guilty pleasures.

## Back-Up
- Backup Stragey: I'm going all-in on back-ups. Philosophy: Work locally, when you "save" a file, the first stage of the back-up cascade begins by synchronizing the project to dropbox. When you compile the document, the second stage gets active and commits the changes to the git repository and pushes it to at least two different remote repositories. Use the files in `/bin` and add them to your compilation chain. They do just that and automatically commit all changes and push them to multiple repos every time you compile your doc. 

## Mendeley / Reference Management
- use BibTeX synchronization in Mendeley / Zotero -- whenever you make a change there, it will automatically update the .bib file so your citations are up-to-date. 
- for the literature review, I used a form that helped me structure all important aspects of the content of a paper. --> https://docs.google.com/spreadsheets/d/1jYQIVkXmHW445mKgK5tJQEH1x-gwSfE5dE9B68f-1UM/edit
- If you enter a DOI in Mendeley and click the small magnifying glass, it will try to retrieve all the information it can get automatically. 
- finding untagged papers: a -tag:Methodology -tag:"Coping Strategies" -tag:Qualitative -tag:Quantitative -tag:"Password Selection Support" -tag:Alternatives -tag:Persuasion -tag:Psychology -tag:Personality -tag:Foundational -tag:Observation -tag:Technical -tag:Phishing -tag:Warnings -tag:SSO -tag:"Password Manager" -tag:Mobile -tag:"Graphical Authentication" -tag:Memorability -tag:"Password Meter" -tag:Thesis -tag:Report -tag:HCI -tag:Critical -tag:Emoji -tag:Statistics -tag:"Mental Models" -tag:Policies
- "`missing \endcsname inserted`" could mean there's an Umlaut in one of your Citation Keys (\cite{Baß2017})

## Editor
- Use TexStudio instead of Texmaker. It's the shizzle.
- In TexStudio, hitting the "Esc" key closes the PDF viewer by default. This was driving me nuts, and I removed the keyboard shortcut in "View" -> "Close Something" (set to "\<none\>")
- The structure tree view in TexStudio breaks by default if you use the `\input` macro (e.g. for tables). 