### MPS_Project

#### WSJ folder contains all the scraping script
#### LoughranMcDonald_MasterDictionary_2014 is the financial dictionary
#### Python notebook "Filter_Doc.ipnb" contains script for cleaning the scraped file
#### "Score.csv" current output from Filter_Doc.ipnb (to do = extract more feature)

### Current logic in Filter_Doc.ipnb
For each news header: <br />
  remove stopword <br />
  extract all the word that's in Mcdonald dictionary <br />
  score = word1_score + word2_score +... (look up from the dictionary) <br />
  clean up date 
