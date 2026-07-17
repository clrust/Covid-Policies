# Created by: CR
# Date: 4/27/26
# Script to run Burnham's Political Debate Model v. 1.1 on data from all states
# Where rescraped data is available, this incorporates the rescraped data without filtering

from transformers import pipeline
import pandas as pd
import os
import numpy as np

### Global Constants; can be changed ###
LABELS = ["economic relief", "reopening", "jobs", "housing", "vaccines",
          "testing", "positive cases", "healthcare professionals", 
          "healthcare infrastructure", "other", "research", "food"]
MULTI_LABEL = True
CHARACTER_NUMBER = 100 #number of characers to slice from each press release
WORKING_DIRECTORY = "/Users/connorrust/Library/CloudStorage/Box-Box/Covid Policies/"
INPUT_DATA = "Data/05_combine_all_states.csv"
OUTPUT_PATH = "Analysis/Testing/Results/06_burnham_all_states.csv"
#####################################
os.chdir(WORKING_DIRECTORY)
data = pd.read_csv(INPUT_DATA)

# defining normalization function
def normalize(matrix, axis=-1):
    """ Takes a numpy 2D array of topic probabilities and normalizes them. 
    (This applies L1 not L2 normalization)
    Args: 
        matrix(numpy array)
        axis(int): axis to normalize across, 1: rows; 0: columns

    Returns:
        2D array with rows/columns normalized

    """
    return matrix / np.sum(matrix, axis=axis, keepdims = True)

# extracting text from data
text = data.pop("Text").str.slice(0,CHARACTER_NUMBER)
lst = text.to_list()

hypothesis_template = "This text is about {}"
classes_verbalized = LABELS

zeroshot_classifier = pipeline("zero-shot-classification", 
                               model="mlburnham/Political_DEBATE_DeBERTa_large_v1.1", 
                               device = "mps")  # change the model identifier here

output = zeroshot_classifier(lst, classes_verbalized, 
                             hypothesis_template=hypothesis_template, 
                             multi_label=MULTI_LABEL)

clean_output = []

for dct in output:
    nd = {}
    nd["sequence"] = dct["sequence"]
    for idx, label in enumerate(dct["labels"]):
        nd[label] = dct["scores"][idx]
    clean_output.append(nd)

df = pd.DataFrame(clean_output)

# mask to identify the columns with probabilities
prob_cols = df.columns.difference(['sequence'])
# creating a copy to normalize
df2 = df.copy(deep=False) 
# Normalizing each row
df2[prob_cols] = normalize(df[prob_cols].values, axis=1)
# combining probabilities with the original data
output =pd.concat([data, df2], axis=1)

# writing to csv
output.to_csv(OUTPUT_PATH)


