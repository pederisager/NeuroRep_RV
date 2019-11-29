#####################
Content descriptions.
#####################

- source_data: 
	This folder contains meta-data for the individual DOIs included as well as miscelaneous data files related to the curation process.  

- dataset_A_coded: all records of dataset A with sample size coded. Includes excluded records and reason for exclusion as well. 
log:
2019-11-26 (merging data from dataset_A_coded_remainder_of_assigned_papers.tsv)
Decisions made'(by Peder):
- I am removing obviously erroneous "yes" responses from exclusion_flagged, probably left over from the previous round of coding.
- When the new coder has added a new coder_comment, I will remove the old resolver and resolver_comment value, add myself (PI) as resolver and address the new coder_comment. Otherwise I will leave the old resolver and resolver_comment value.
- Any non-coded sample size left I will code myself. For these I will substitute my identifier (PI) with the coder identifier. Later, we will have to make sure that two students recode my data so that each data have 2 student coders and 1 senior coder for the inter-rater reliability checks.
- Any row with a coder_comment not addressed by AV I will resolve myself. For these I will substitute my identifier (PI) with the AV resolver identifier. As long as the coder comment concerns whether the article is "social" or whether the study population is feasible, I will follow ATs principle and resolve they should be included.
- 1 exception made regarding previous point. I decided to exclude a study where the study population were non-human (cats - 10.1016/j.jneumeth.2013.12.012).


- dataset_B_main-finding_coded: all records of dataset B with main finding information coded.