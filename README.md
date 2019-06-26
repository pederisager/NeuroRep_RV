
# NeuroRep Replication Project
### Component 1: Selecting target study based on replication value.

This repository contains materials and data related to the first component of the NeuroRep project. The goal of this project component is to select a target study for replication based partly on quantitative estimates of the replication value of a large dataset of relevant studies within the social neuroscience literature. A subgoal is to empirically validate the replication value quantification procedure against the intuitive judgement of a sample of researchers within the field. 

## Project description 

  Not every published finding turns out to be a robust and replicable effect, and it is therefore important to differentiate robust findings from chance findings, preferably before the latter have an unwarranted impact on scientific theories. However, given that the published record of findings in psychology is vast and largely unreplicated, the number of findings that merit replication likely exceeds what can be addressed with the resources we have available. Researchers thus have to choose which among many findings would be the most valuable to replicate. In order to make this process more efficient, we propose a general study selection procedure that is based on quantifying the replication value of individual findings. 
  
  We assume that replication value can be seen as a ratio between how impactful and how well-corroborated a finding has become, and we propose concrete formulas to capture this ratio that are based on quantitative and obtainable information. In order to validate our novel approach to study selection in replication research, we will conduct two studies to explore the association between formula-derived replication value and other indicators of replication value. In the first study, we will apply our study selection approach to derive a choice for an actual replication attempt in social neuroscience research. One thousand empirical studies will be assessed using multiple formula operationalizations (goal A). We will compare the replication value estimates of different formulas in a subset of the thousand in order to explore which indicators are essential for inclusion in the formula.  We will also evaluate the face validity of our study selection approach by comparing formula-based recommendations to our own expertise judgement (goal B). 
  
  In the second study we will, for a stratified subset of the dataset of 1000 studies, compare formula estimates of replication value to rank orderings provided by researchers in the field of social neuroscience (goal C). If formula estimates and expert assessments both partially capture the "true" replication value of a finding, then rank order results from both approaches should correlate. The goal of these studies is thus to examine the validity of the Replication Value study selection approach. The poster will present the theoretical outline of the Replication Value, our planned methods for these studies and the overall project goals.


### Goal A - Curate
Large dataset (dataset A) that researchers can use when deciding to browse options for replication. Minimally, this contains year, citation count (extract 2, and see if we can find a way to do google as well), sample size, altmetric score (extract, perhaps see if we can extract again at the end or project to update this).

### Goal B - Compare
Smaller dataset (dataset B) that is a (random?) sample of A, where we code more information, giving us an opportunity to compare the A-dataset formula with the B-dataset formula outcomes.

### Goal C - Validate
An even smaller dataset (dataset C) of about 10 papers that are drawn from top, middle and low range RV of [A or B]. Giving us an opportunity to compare this ranking to how researchers in the field of social neuroscience would rank these ten papers after reading their abstracts (with optionally some additional minimal info to ensure all ten abstracts contain the same information).

### Goal D - Replicate
After providing validation of the replication value procedure as per goal A, B and C, we will select one study to replicate from the pool of 1000. The intent of this final goal is partially to provide "use-case validity" for the replication value approach, and partially to ensure that the validating research team has a vested interest in critically assessing the quality of the validation research.  


## Repository structure

The structure of this repository is based on the Psych-DS data specification (version 1.0.1). For details on the specification, see: https://docs.google.com/document/d/1u8o5jnWk0Iqp_J06PTu5NjBfVsdoPbBhstht6W0fFp0/edit?usp=sharing (draft in progress)

Because there is no validator app for the specification yet, we cannot at this time guarantee full compliance with the spec. Because the spec itself is under development, the structure of this repository may change as the spec gets updated. If so, we will log the current relevant version number of the spec in this document. 


## Related documents and records located elsewhere

Google Drive directory containing various project-related documents: https://drive.google.com/drive/folders/1ejlPhI0h6l5Akk7lo_z187IWEqyCfZdc?usp=sharing (link grants read access)