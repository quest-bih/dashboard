#!/usr/bin/env python3

#first, need to install barzooka package from https://github.com/quest-bih/barzooka
import barzooka

#---------------------------------------------------------------------------------
# run configs
#---------------------------------------------------------------------------------

#tmp folder for img extraction from PDFs
tmp_folder_run = './tmp/' 
 
#might need to adjust PDF folder path according to PDF location
pdf_folder = '/fast/users/riedeln_c/work/charite_dashboard/PDFs/' 

#---------------------------------------------------------------------------------
# calculate charite PDF validation
#---------------------------------------------------------------------------------

b = barzooka.Barzooka()

b.predict_from_folder(pdf_folder,
        './results/Barzooka.csv', tmp_folder=tmp_folder_run)
