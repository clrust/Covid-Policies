# hoping to test out implementation with multiple classes here
from transformers import pipeline
import pandas as pd
import os
from scipy.special import softmax

os.chdir("/Users/connorrust/Library/CloudStorage/Box-Box/Covid Policies/Data")
data = pd.read_csv("MN_Health_short.csv")

# extracting text from data
text = data.pop("Text").str.slice(0,100)
lst = text.to_list()

hypothesis_template = "This text is about {}"
classes_verbalized = ["economic relief", "reopening", "jobs", "housing", "vaccines","testing", "positive cases", 
                      "healthcare professionals", "healthcare infrastructure", "other", "research", "food"]

zeroshot_classifier = pipeline("zero-shot-classification", 
                               model="mlburnham/Political_DEBATE_DeBERTa_large_v1.1", 
                               device = "mps")  # change the model identifier here

output = zeroshot_classifier(lst, classes_verbalized, hypothesis_template=hypothesis_template, multi_label=True)

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
# creating a copy to apply softmax to
df2 = df.copy(deep=False) 
# Applying softmax to the copy
df2[prob_cols] = softmax(df[prob_cols].values, axis=1)

# combining probabilities with the original data
output =pd.concat([data, df2], axis=1)

# writing to csv
output.to_csv('/Users/connorrust/Library/CloudStorage/Box-Box/Covid Policies/Analysis/Testing/Results/03_burnham_test.csv')