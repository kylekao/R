# Overview
1. Background and Technical Setup
2. Part Worth Calculations
3. Market Share Estimation
4. Data Interpretation
 
- 1. Background  and Technical Setup
 
You are the marketing manager for Acme Espresso Machines (fictitious), a small manufacturer of premium coffee maker machines. You are in a market dominated by several large manufacturers, including Breville, DeLonghi, Gaggia, and Rancilio. To improve Acme’s market share, you plan to analyze the market with several objectives in mind:
 
-Identify specific market segments within the greater market
-Identify specific features desired by different market segments
-Select marketing messages that will resonate with market segments
-Estimate market share performance for different types of machines
 
You plan to conduct a conjoint analysis to address the objectives. You have conducted market research and found that buyers rank capacity, speed, and price as the most important attributes when making coffee maker machine purchases. Based on research on the market and competition, you have declared two levels each of the three decision criteria variables:
 
-Speed: S1: “Fast” (machine capable of brewing coffee in under one minute)
-Speed: S2: “Slow” (machine requires 1 minute or more to brew coffee)
-Capacity: C1: “Single-Cup” (machine makes 1 cup of espresso at a time)
-Capacity: C2: “Multi-Cup” (machines makes multiple cups at a time)
-Price: P1: “Budget” (machine price under $300)
-Price: P2: “Premium” (machine price $300 or more)
 
The three attributes, at two levels each, result in a possible solution set of eight different types of machines. The different versions are called Cards. The table below shows the eight cards: The data in the table is also shown in the data set called profiles.

|Card| Bundle Speed | Capacity | Price |
|:----:|:----:|:----:|:----:|
| 1 | 1 | 1 | 1 |
| 2 | 1 | 1 | 2 |
| 3 | 1 | 2 | 2 |
| 5 | 2 | 1 | 1 |
| 6 | 2 | 1 | 2 |
| 7 | 2 | 2 | 1 |
| 8 | 2 | 2 | 2 |
 
Data set: Eight cards to represent two levels each of three attributes
 
Using a five point Likert scale, you have asked for market feedback on the preference of the eight different variants, or cards, of the machine. The preference scores vary from 1, representing a poor machine, to 5, representing an outstanding machine. Note the format of the data table shown below. Note that the preferences data set includes preference scores from multiple evaluators.

| Profile1|Profile2|Profile3|Profile4|Profile5|Profile6|Profile7|Profile8| 
|:----:|:----:|:----:|:----:|:----:|:----:|:----:|:----:|
| 5|4|5|4|3|1|3|2|
|4|5|5|4|3|3|2|1|
|4|4|4|4|4|2|3|2|
|5|4|5|5|2|1|4|3|
|5|4|5|3|3|1|2|1|
|4|4|5|4|2|1|2|3|
|5|4|5|4|3|2|3|2|
|4|4|5|4|2|1|4|2|
|5|4|5|3|1|2|4|2|
|4|5|5|4|3|2|3|3|
 
 
Data set: Preference scores for the eight cards
 
- 2.	Part Worth Calculations
Using the conjoint functionality in R, please compute the part worths for the three attribute variables.
 
- 3.	Market Share Estimation
Using the conjoint functionality in R, please estimate the market share for two cases. The first case is where buyers will only purchase one machine, and thus seek to maximize their utility with the one machine. The second case is where buyers will purchase multiple machines, such as for different areas in the house, which will indicate that a probabilistic method for market share estimation should be used.
 
- 4.	Data Interpretation
### Findings
### Conclusion
### Recommendations
### Reference
