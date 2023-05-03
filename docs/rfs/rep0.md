# REP 0 – Definition of a REP and Structural Guidelines

**Authors:** The Ribbit Team  
**Status:** Ongoing  
**Type:** Information  
**Target**: REP  
**Created:** 23/04/2023  
**Updated:** 23/04/2023

## What is a REP

REP stands for *Ribbit Enhancement Proposal*, and, as the name suggests, they are 
used to propose a change to the Ribbit project. 

The subject of the proposal is called its **target** and can be any one of:

- **Ribbit Compiler**
- **Standard RVM feature**
- **Standard Library**
- **\<Other\>**: If the proposal doesn't fit in any of the aforementioned targets, you can 
specify your own target.


The nature of the change is called the **type** of the REP and can be any one of:

- **New Feature**: The REP is describing the addition of a new feature in its target.
- **Change**: The REP is describing a change to its target.
- **Removal**: The REP is describing the removal of a feature from its target.
- **Information**: The REP is describing a piece of information about its target.


## What is this first REP about? 

This REP establishes the structural guidelines to follow when writing a valid REP.

## How to write a REP

In general, a REP should follow the following structure:

1. **Abstract**  
   The "abstract" section should be a *short* paragraph describing what the REP is about without
   going into the nitty-gritty of the proposal. This part should allow the reader to
   have a general idea of what is proposed in the REP.

2. **Context and Motivation**  
   The "context and motivation" section should describe how things are done 
   at the moment of the proposal and explain what is wrong with the status quo.
   This section could be very long or very short, depending on the scope of the 
   REP and the subtlety of the problem. Please note that this section is **NOT**
   meant to explain how the REP solves the issue, it only describes the problem
   to solve and why it is important to do so. 

   This section is useful, as it highlights a problem that needs fixing, sets up the
   context for the rest of the proposal, and helps to start a discussion regarding the
   issue, even if the REP is rejected in its current form. 

3. **Non-goals**  
   The "non-goals" section should describe what the REP is *NOT* trying to achieve.
   It can be long or short, depending on the complexity of the problem and
   the number of ways one could understand the proposal.

   This section helps clarify any ambiguity regarding the intended scope of the
   proposal by clearly stating what this REP should not be used for. This is useful
   as it portrays a more accurate representation of the REP, which will lead to
   more constructive discussions down the line.

4. **Presentation**
   The "presentation" section is the meat of the REP, as it is here that the content
   of the proposal is presented in full details. Concrete examples are encouraged in this section
   to illustrate the "real-world" implication of the proposal.

5. **Limitations and Future Work**  
   The "limitations and future work" section should first explain the limits of
   this REP in relation to the problem that it solves. For example, if there are
   some edge cases that are not covered / are not solved by the proposal, they
   should be written here with an explanation of why they can be ignored by the
   proposal. This first part should also talk about the changes that would need
   to be made to the existing code base, should the REP be approved.

   The second part should contain a small paragraph about what could be done
   in the future to extend this proposal, or solve its current limitations. 

6. **Discussion**  
   The "discussion" section should contain a brief list, in the form of a
   Q&A, of the relevant questions (and their answers) that were asked while this
   proposal was in the process of being accepted.

7. **References**
   The "references" section should contain a list of references to the sources
   used to write this REP.
   Those references can be other REPs, websites, books, videos, etc. 
