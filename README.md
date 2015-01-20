# moon_phase

**2.A.1_root_angle.R**

**mixed_model.R**

**gravity.Rdata**

These are the files relevant to the analysis I could find in Jinliang's old files. There were two folders marked "root angle" and "lunar phase". The lunar phase folder was empty.

**moonphase_mlm.R**

**rsquaredglmm.R**

**final_all.csv**

These are my files for the new data. **final_all.csv** contains the formatted data. Only two camera setups were used in the new experiments, and each setup was used with a single genotype. Basing the model on the slides Jinliang sent me and considering that genotype and setup are completely confounded in the new data set, the model is

angle = lunar day + genotype + replicate(genotype)

where lunar day is a fixed effect and genotype and replicate are random effects.