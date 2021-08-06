#!/usr/bin/env python3

from fastai.vision.all import *
from matplotlib import pyplot
import numpy as np
import pandas as pd
np.set_printoptions(threshold=sys.maxsize)

#---------------------------------------------------------------------------------
# run configs
#---------------------------------------------------------------------------------

#import barzooka_v2 or barzooka_v2_no_flow or barzooka_v2_flowyesno
import barzooka_v2_flowyesno as barzooka_v2
tmp_folder_run = './tmp/' 
 
#---------------------------------------------------------------------------------
# calculate charite PDF validation
#---------------------------------------------------------------------------------

b = barzooka_v2.Barzooka('/fast/users/riedeln_c/work/charite_dashboard/barzooka_v2_flow.pkl')
b.predict_from_folder('/fast/users/riedeln_c/work/charite_dashboard/PDFs_new/',
        './results/dashboard_PDFs_new.csv', tmp_folder=tmp_folder_run)
