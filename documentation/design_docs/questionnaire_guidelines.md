# Questionnaire Conventions
This document outlines the conventions for creating questions to be asked in the questionnaire as part of the DoseFinderHub project. The purpose of the questionnaire is to gain insight into the needs of the user and their prospective dose finding trial in order to streamline the comparisons drawn between trial designs and the recommendations returned to the user by our system. Adhering to these guidleines will ensure that questions presented to users are clear and concise, simple to answer, and consistently named for subsequent use in the system. 

## Question Style
It is critical that the questions presented to users are clear, unambiguous, and easy to understand. When considering which questions should be asked to users, one should consider:
- Is the question relevant to the needs of the user and their trial?
- Would the answer to the question help to determine suitability of differnet trial designs?
- Is the question user friendly and does it use any language that might need an explanation?
- Is the question impartial?
- Is the 'question type' the most appropriate? 

N.B. The 'question type' determines what type of answer the user can give in the Shiny app. Examples include 'text' (open-ended answer), 'numeric' (numeric answers only), 'select' (drop down box with prespecified answers), and 'slider' (an interactive scale to select a numeric value), etc.

## Answer Style
Asking a great question means nothing to us without a great answer, so we must carefully consider the answer options offered to the user to ensure we extract the most accurate information possible. When considering which answers should be offered to the user, one should consider:
- Are the answers presented an appropriate response to the question asked?
- Is the range of answers too limited or narrow? Are any options/values missing?
- Is the range of answers too broad or ambiguous? Are there answers that overlap eachother?
- Is the user able to answer in the most appropriate way? Would a different 'question type' be more suitable?

## Storing Questions and Answers in the database
Once the questions and corresponding answers have been decided, the next step is to store them in the questionnaire databse. The questionnaire database is called 'q_database' and can be found at app/modules/questionnaire/q_database. It's a csv file and follows a set structure, described below. 

For each question please describe:
- **q_number**: the question number, ie question number 1 means the first question asked to the user. If rearranging numbers, ensure to re-label every question so there are no duplicated or missing numbers.
- **q_type**: the 'question type' as referred to above. This will determine the type of answer offered to the user.
- **q_variable**: the question variable, ie the name of the questio in the UI. This should be decriptive but very concise and adhere to the coding guidelines, eg drug_type.
- **q_text**: the question text is what will be shown to the user, eg 'What type of drug are you testing?'. Make sure this is a full sentence and is grammatically correct.
- **a_1**: the option displayed as the first answer, eg 'Chemotherapy'.
- **a_2**: the option displayed as the second answer, eg 'Immunotherapy'.
- **a_3 - a_10**: further options for the user to select as an asnwer. Try to order the answers in a coherent way when applicable, like if the answers range over a scale from 'Stongly agree' to 'Strongly Disagree'. If the question type is a slider, use a_1 to record the minimum value, a_2 the maximum value, and a_3 to specify the size of each step on the scale.
- Where no answers are prespecified, a1 - a10 can be left blank. If there are fewer than 10 prespecified answers, enter answers in the first available answer options and leave the remaining answer options blank (dont skip answer options).

Here is an example of what q_database should look like:

| q_number | q_type | q_variable | q_text | a_1 | a_2 | a_3 
| ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | ----------- | 
| 1 | radioButtons | drug_type | What type of drug are you using? | Chemotherapy | Immunotherapy | 