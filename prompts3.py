# new prompts from Wed 12.03.
#

accuracy_eval_prompt = """Decide if the following summary is consistent with the corresponding article.
You find both below this instruction. Note that consistency means all information in the summary is supported by the article. Respond with ANSWER:1 if the article and the summary are consistent or ANSWER:0 if any information in the summary is not supported by the article. If the texts do not mention the coalition break down, respond with WRONG_CONTENT. The only output you give should be one of the three options."""

article_eval_prompt = """Analyze the sentiment in the given text toward each of these entities: {Olaf Scholz, SPD, Robert Habeck, die Gruenen, Christian Lindner, FDP, the coalition breakdown}. Focus on the author's opinion and not on direct quotes.

Assign a sentiment score for each entity:
-1 = Negative, 0 = Neutral, 1 = Positive
If an entity is not mentioned at all, mark it as NONE.
Consider tone, language, and context when determining sentiment. Do not judge direct quotes.
Examples for Sentiment Scoring:

Negative (-1): [The government of Germany was blown apart.]
Neutral (0): [Scholz has argued for easing spending rules to address urgent national and international challenges.]
Positive (1): [Habeck geht damit auf Finanzminister Christian Lindner zu.] 

Use these benchmarks as guidance: Your response needs to be of the following format, where you replace x with the number or NONE. It is very important that you stick to this format and do not write anything else: Olaf Scholz:x, SPD:x, Robert Habeck:x, die Gruenen:x, Christian Lindner:x, FDP:x, the coalition breakdown:x"""

summary_eval_prompt = """Analyze the sentiment in the given text toward each of these entities: {Olaf Scholz, SPD, Robert Habeck, die Gruenen, Christian Lindner, FDP, the coalition breakdown}. Focus on the author's opinion and not on direct quotes.

Assign a sentiment score for each entity:
-1 = Negative, 0 = Neutral, 1 = Positive
If an entity is not mentioned at all, mark it as NONE.
Consider tone, language, and context when determining sentiment. Do not judge direct quotes.
Examples for Sentiment Scoring:

Negative (-1): [The government of Germany was blown apart.]
Neutral (0): [Scholz has argued for easing spending rules to address urgent national and international challenges.]
Positive (1): [Habeck geht damit auf Finanzminister Christian Lindner zu.] 

Use these benchmarks as guidance: Your response needs to be of the following format, where you replace x with the number or NONE. It is very important that you stick to this format and do not write anything else: Olaf Scholz:x, SPD:x, Robert Habeck:x, die Gruenen:x, Christian Lindner:x, FDP:x, the coalition breakdown:x"""