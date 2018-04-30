### For Client.
The main script is in **Multiclass_Model.R**
GBM from H2O is used for multi-classification and NB is used as a base line comparison.
Prophet model (Fully Bayesian Time Serie Model developed by Facebook) is used for second stage prediction.

### MPS_Project
1) Research on related papers. <br />
https://docs.google.com/spreadsheets/d/15BCDaNXiiUL2QYLtYsj7KyzDzPyyZgpd53_zFzzA-E0/edit#gid=0 <br />

2) Scraping Data <br />
- **WSJ folder** contains all the scraping scripts for WSJ. <br />
- **LoughranMcDonald_MasterDictionary_2014** is the financial dictionary. <br />
- **Python notebook Filter_Doc.ipnb"** contains the script for cleaning data. <br />
- **"Score.csv"** current output from Filter_Doc.ipnb (to do = extract more feature). <br />
- **"Twitter+API+Scraping.ipynb"** containts script for pulling data from twitter.
- **"train.csv"** contains the final data ready for modelling. Note that this is only for 2012 (In the process of extracting more).
- **"sectors.csv"** is the raw data from client (last 10 years financial index by sector)
- **"Feature_Extraction.ipynb"** the script for joining all the data.

3) Tutorial on the tool we use to scrape the data. <br />
https://docs.google.com/document/d/1MFxfclc5q66OU8cB2NSayste8ZyRmRUHeD_Fo16PAJU/edit <br />

4) To do list <br />
- Fit the model using sentiment score and compare the result to the model without this feature. <br />
- Feature to use: Trend/Seasonality/Sentiment Score <br />

### Current logic in Filter_Doc.ipnb
For each news header: <br />
- remove stopword (The Macdonald dictionary contains stopword) <br />
- extract all the word that match the words in Mcdonald's dictionary <br />
- score = word1_score + word2_score +... (look up from the dictionary) <br />
- clean up date <br />
