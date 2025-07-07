# Prompt: Transform Legal Text into Logical English (LE)


## Objective
Convert the provided legal text into Logical English (LE) for clarity and precision.


## Step by Step

1. Split the paragraph into sentences.
2. For each sentence do the following relevant information:
- Subject: Entity (vehicle, person, etc.) that performs the action described in the conclusion.
- Conclusion: The action that is present in the main sentence.
- Deontic Modality: Optional. The importance of the action indicated in the conclusion. Either, should or must.
- Conditions: Things that have to be true, in order to the conclusion to be applicable.
3. Present the JSON with the extracted elements for each sentence.
4. For each JSON object with conclusion and conditions, *identify the templates*.

- **Conclusion**:
* Predicate name is the deontic modality if it exists. If not assume "can".
* Arguments are the subject, action and specifications (if they exist). A specification can be a location, a time, a characteristic.

* You should explicitly choose *one* of the following templates to write the conclusion.

```le
templates:
*a subject* must *an action*.
*a subject* must *an action* at *a location*.
*a subject* must not *an action*.
*a subject* must not *an action* at *a location*.
*a subject* can *an action*.
*a subject* can *an action* at *a location*.
```

* Only create a new template, if and only if, the existing templates *do not fit*.
* present the conclusion together with the corresponding template. For example:
- "you must stop behind the line at a junction": *a subject* must *an action* at *a location*.

- **Conditions**
* Find the subject, the verb, and the specification.
* Use the verb as the predicate name.
* The arguments are the subject and specifications.
* You should explicitly choose *one* of the following templates to write the conditions.

```le
templates:

*a thing* has *a property/object*
*a thing* is *a property/object*
*a thing* exists in *a thing*
```

* Only create a new template, if and only if, the existing templates *do not fit*.
* For each condition, present it together with the corresponding template. For example:
- "The junction has stop sign": *a thing* has *a property*.
* The output for this step should be like:
- **Conditions:**
- "Condition 1": `Associated template`
- "Condition 2": `Another associated template`
- ...
- **Conclusion:**
- "Conclusion": `Associated template`


5. *Build the rules* using the identified templates.
- Connect the rules and conditions using the templates and logical connections from Logical English.
- If the subject is "you", replace the agent in the templates with the constant ego.

## Few Shot examples

### Example 1:

**Text:** You MUST stop behind the line at a junction with a 'Stop' sign and a solid white line across the road. Wait for a safe gap in the traffic before you move off.

**Step 1: Sentences**

1. You MUST stop behind the line at a junction with a 'Stop' sign and a solid white line across the road.
2. Wait for a safe gap in the traffic before you move off.

**Step 2: For each phrase, extract the relevant information**

1. You MUST stop behind the line at a junction with a 'Stop' sign and a solid white line across the road.

- Subject: "ego"
- Conclusion: "stop behind the line at a junction"
- Deontic modality: "must"
- Conditions:
* "the junction has a 'Stop' sign"
* "the junction has a solid white line across the road"

2. Wait for a safe gap in the traffic before you move off.

- Subject: "ego"
- Conclusion: "move off"
- Deontic modality: None
- Conditions:
* "there is a safe gap in the traffic"


**Step 3: Present the JSON**

```json
[
{
"subject":"ego",
"conclusion":"stop behind the line at a junction",
"deontic_modality":"must",
"conditions":[
"the junction has a 'Stop' sign",
"the junction has a solid white line across the road"
]
},
{
"subject":"ego",
"conclusion":"move off",
"deontic_modality":null,
"conditions":[
"there is a safe gap in the traffic"
]
}
]
```

**Step 4: Identifying the templates**

1. You MUST stop behind the line at a junction with a 'Stop' sign and a solid white line across the road.

**Conclusion**
- "You MUST stop behind the line at a junction": `*an agent* must *an action* at *a location*`
**Conditions:**
- "with a 'Stop' sign": `*a thing* has *a thing*`
- "a solid white line across the road": `*a thing* has *a thing*`


2. Wait for a safe gap in the traffic before you move off.

**Conclusion**
- "you move off": `*an agent* can *an action*`
**Conditions**
- "there is a safe gap in the traffic": `*a thing* exists in *a location*`

**Step 5: Build the rules**

1. You MUST stop behind the line at a junction with a 'Stop' sign and a solid white line across the road.

```le
the knowledge base example includes:
ego must stop behind the line at a junction
if the junction has stop sign
and the junction has solid white line across the road.
```

2. Wait for a safe gap in the traffic before you move off.

```
the knowledge base example includes:
ego can move off
if safe gap exists in traffic.
```

## Logical English Instructions:


#### Basic Form

- **Structure:** `conclusion if conditions`
- **Example:** `a person P is a British citizen if P is born in the United Kingdom after commencement and ...`

#### Templates

- **Purpose:** Declare a fixed predicate with variable arguments.
- **Structure:** Use simple or compound verbs and noun phrases.
- **Example:** `a person P is a British citizen if ...`

#### Variables

- **Introduction:** Use indefinite articles (a/an) to introduce variables.
- **Example:** `a person P`
- **Reference:** Use definite articles (the) to refer back to variables.
- **Example:** `the person P`

#### Logical Constructs

- **AND:** Used to combine multiple conditions that must all be true.
- **Example:** `condition1 and condition2`
- **OR:** Used to combine conditions where at least one must be true.
- **Example:** `condition1 or condition2`
- **NOT:** Used to negate a condition.
- **Example:** `not condition`

---

### Example Transformations

#### British Nationality Act 1981

**Natural Language:**
"A person born in the UK after commencement shall be a British citizen if at the time of birth his father or mother is a British citizen or settled in the UK."

**LE Form:**
```le
the templates are:
*a person* acquires british citizenship on *a date*,
*a person* is born in the uk on *a date*,
*a date* is after commencement,
*a person* is the mother of *a person*,
*a person* is the father of *a person*,
*a person* is a british citizen on *a date*,
*a person* is settled in the uk on *a date*.

the knowledge base citizenship includes:
a person acquires british citizenship on a date
if the person is born in the uk on the date
and the date is after commencement
and an other person is the mother of the person
or the other person is the father of the person
and the other person is a british citizen on the date
or the other person is settled in the uk on the date.
```

#### ISDA Master Agreement
**Natural Language:**
"If at any time an Event of Default with respect to a party (the 'Defaulting Party') has occurred and is then continuing, the other party (the 'Non-defaulting Party') may, by not more than 20 days notice to the Defaulting Party specifying the relevant Event of Default, designate a day not earlier than the day such notice is effective as an Early Termination Date in respect of all outstanding Transactions."

**LE Form:**
```le
the templates are:

it is permitted that *an eventuality*,
*a party* designates that *an eventuality*,
*an event* occurs at *a time*.
*an event* of Default occurs with respect to *a party* at *a time*,
*an event* is continuing at *a time*,
*a party* gives notice to *a party* at *a time* that *a message*,
*a date* is on or before *another date*,
*a time* and *a time* are at most *a number* days apart, the Schedule specifies that *a specification*,
Automatic Early Termination applies to *a party* for *an event* of Default.

the knowledge base eventuality includes:

it is permitted that a party designates
that Early Termination in respect of all outstanding Transactions occurs at a time T3

if an Event of Default occurs with respect to another party at a time T1
and the Event is continuing at a time T2
and the party gives notice to the other party at T2
that the Event occurs at T1
and T2 is on or before T3
and T3 and T2 are at most 20 days apart
and it is not the case that
the Schedule specifies that Automatic Early Termination applies to the other party for the Event of Default.
```

#### Loan Agreement
**Natural Language:**
"5. The Borrower will be in default under this agreement upon the occurrence of any of the
following events or conditions, provided they shall remain uncured within a period of two days
after notice is given to Borrower by Lender of their occurrence (such an uncured event an “Event
of Default”):
(a) Borrower shall fail to make timely payment of any amount due to Lender hereunder;
(b) Any of the representation or warranties of Borrower under this agreement shall prove
untrue;
(c) Borrower shall fail to perform any of its covenants under this agreement;
(d) Borrower shall file for bankruptcy or insolvency under any applicable federal or state law.
A default will be cured by the Borrower (i) remedying the potential event of default and (ii)
giving effective notice of such remedy to the Lender. In the event of multiple events of default,
the first to occur shall take precedence for the purposes of specifying outcomes under this
agreement.
6. Acceleration on Default
Upon the occurrence of an Event of Default all outstanding payments under this agreement will
become immediately due and payable, including both principal and interest amounts, without
further notice, presentment, or demand to the Borrower."

**LE Form:**
```le
the borrower fails on a date to fulfil an obligation if
the obligation is
that the borrower pays an amount to the lender on the date
and it is not the case that
the borrower pays the amount to the lender on the date.

the borrower defaults on a date D3 if
the borrower must fulfil an obligation
and the borrower fails on a date D0 to fulfil the obligation
and the lender gives notice to the borrower on a date D1
that the borrower fails on D0 to fulfil the obligation
and D3 is 2 days after D1
and it is not the case that
the borrower cures on a date D2 the failure of the obligation
and D2 is on or before D3.

the borrower cures on a date D the failure of an obligation
if the obligation is
that the borrower pays an amount to the lender on a date DO
and the borrower pays the amount to the lender
on a date D1
and the borrower gives notice to the lender on a date D2
that the borrower pays the amount to the lender on D1
and D is the latest of D1 and D2.
```


## Final Remarks

You task is to transform natural language text into Logical English.
To do so, follow the Cheatsheet guide on how to write Logical English, to fulfill the following steps:

1. Break the input into sentences.
2. For each sentence do the following relevant information:
3. Present the JSON with the extracted elements for each sentence.
4. For each JSON object with conclusion and conditions, identify the templates.
5. Build the rules using the templates.