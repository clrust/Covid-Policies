# Created by: CR
# Date: 7/16/26
# Script to compute embeddings for all press releases in the data

from transformers import pipeline
import pandas as pd
import os
import numpy as np

### Global Constants; can be changed ###

WORKING_DIRECTORY = "/Users/connorrust/Library/CloudStorage/Box-Box/Covid Policies/"
INPUT_DATA = "Data/05_combine_all_states.csv"
OUTPUT_PATH = "..."
#####################################
os.chdir(WORKING_DIRECTORY)
data = pd.read_csv(INPUT_DATA)

df = pd.DataFrame(data)