# Scotland Independence Referendum Analysis 

This is an excerpt of the data analysis portion of my senior thesis (completed April 2020) that examines the motivations behind the Scottish independence referendum and how it shaped the vote on Scotland's constitutional status in 1979, 1997, and 2014. With the prospect of a second Scottish independence referendum in the [news](https://www.bbc.com/news/uk-scotland-scotland-politics-48026430) and after studying abroad in Scotland for the semester, this was a topic that really interested me.

For my data analysis, I pulled data from the 1997 Scottish Referendum Study (after Scottish citizens voted to create the Scottish Parliament with powers devolved from the Parliament of the United Kingdom, restoring power in the legislative body for the first time since 1707) and Wave 3 of the British Election Study Internet Panel Survey (after the 2014 independence referendum vote). 

After cleaning the data for logistic regression analysis, I built a set of prediction models that showed how Scottish voters would vote on these separate issues. The code referenced in this repository shows the data analysis for just the 2014 independence referendum survey. These models were built by regressing a binary variable (whether they voted in favor of Scottish independence) on a set of independent variables (demographics, national identity likert scales, measures of support for various political parties/figures, economic perspective variables).

In my analysis, I correctly predicted 88.8% of the votes for/against Scottish independence using my model that measured demographics, national identity scale, and political variables (model 3). From here, a field organizer on a political campaign for a possible second independence referendum can cut a segment of the population that may be a persuadable voter for/against independence to contact and persuade to their side.

If you're interested, the excerpt from my thesis paper itself can be accessed [here](https://docs.google.com/document/d/1ccC1L3tHLcaGzhkHwy83AmdIu8RPJnG2uArb3x3AJOo/edit#heading=h.26in1rg).
