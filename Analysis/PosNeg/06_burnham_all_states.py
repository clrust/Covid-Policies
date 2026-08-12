# Created by: CR
# Date: 7/26/26
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
MULTI_LABEL = False
CHARACTER_NUMBER = 100 #number of characters to slice from each press release
WORKING_DIRECTORY = "/Users/connorrust/Library/CloudStorage/Box-Box/Covid Policies/"
INPUT_DATA = "Data/05_combine_all_states.csv"
OUTPUT_PATH = "Analysis/Testing/Results/06_burnham_posneg_all_states_sample.csv"
#####################################
os.chdir(WORKING_DIRECTORY)
# data = pd.read_csv(INPUT_DATA)
data = pd.read_csv(INPUT_DATA).sample(frac=0.1, random_state=42)

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

ent_template = "This text is about {}"
dis_template = "This text is not about {}"

classes_verbalized = LABELS

zeroshot_classifier = pipeline("zero-shot-classification", 
                               model="mlburnham/Political_DEBATE_DeBERTa_large_v1.1", 
                               device = "mps")  # change the model identifier here

entailment = zeroshot_classifier(lst, classes_verbalized, 
                             hypothesis_template=ent_template, 
                             multi_label=MULTI_LABEL)

disentailment = zeroshot_classifier(lst, classes_verbalized, 
                             hypothesis_template=dis_template, 
                             multi_label=MULTI_LABEL)

clean_output = []

for edct, ddct in zip(entailment, disentailment):
    nd = {}
    nd["sequence"] = edct["sequence"]

    if edct["sequence"] != ddct["sequence"]:
        raise ValueError("entailment and disentailment sequences do not match")
    
    for idx, label in enumerate(edct["labels"]):
        nd[label + "_ent"] = edct["scores"][idx]
    for idx, label in enumerate(ddct["labels"]):
        nd[label + "_dis"] = ddct["scores"][idx]

    clean_output.append(nd)

df = pd.DataFrame(clean_output)

#---------For normalizing when multi label is set to True
if MULTI_LABEL:
# # separating out only positive and negative scores into separate dfs
    ent_df = df.filter(like = "ent")
    dis_df = df.filter(like = "dis")
    
    # copies to normalize
    ent_df2 = ent_df.copy(deep=False) 
    dis_df2 = dis_df.copy(deep=False) 

    # L1 Normalizing each row
    ent_df2.loc[:,ent_df2.columns.str.contains("ent")] = normalize(ent_df.values, axis=1)
    dis_df2.loc[:,dis_df2.columns.str.contains("dis")] = normalize(dis_df.values, axis=1)

    # combining probabilities with the original data
    output = pd.concat([data, ent_df2, dis_df2], axis=1)
    # writing to csv
    output.to_csv(OUTPUT_PATH)

elif MULTI_LABEL == False:
    df.to_csv(OUTPUT_PATH)



