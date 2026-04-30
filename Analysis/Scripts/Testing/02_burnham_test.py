from transformers import pipeline
import pandas as pd
import os


os.chdir("/Users/connorrust/Library/CloudStorage/Box-Box/Covid Policies")
data = pd.read_csv("Data/All_no_na.csv")

text = data["Text"].str.slice(0,50)
lst = text.to_list()

hypothesis_template = "This text is about {}."
classes_verbalized = ["covid", "health", "other"]

zeroshot_classifier = pipeline("zero-shot-classification", model="mlburnham/Political_DEBATE_large_v1.0", device = "mps")  # change the model identifier here
output = zeroshot_classifier(lst, classes_verbalized, hypothesis_template=hypothesis_template, multi_label=False)

clean_output = []

for dct in output:
    nd = {}
    nd["sequence"] = dct["sequence"]
    for idx, label in enumerate(dct["labels"]):
        nd[label] = dct["scores"][idx]
    clean_output.append(nd)

df = pd.DataFrame(clean_output)

df.to_csv('Analysis/Testing/Results/all_test.csv')