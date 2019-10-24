## AD616 Course helper Package
This is a helper package for the AD616 course in Enterprise Risk Analytics at BU. 
The primary purpose of this package is to allow you to build decision trees and do sensitivity analysis and stochastic optimization on the decision trees. It also helps you visualize the Trees and the Expected Values, calculate PayOffs and Utilities with built-in functions.

## Installing and loading the Package
You can install the package using the `devtools` package in R. 
`devtools::install_github("WXCode/AD616")`
and then load the package with the `library` command
`library (AD616)`

## Usage
The package allows you to build a decision Tree with the following options
- Root Node
    - The Root node is by default a decision node.
- Decision Nodes
    - Decision nodes are typically represented by rectangles and have an yes or no branch
- Chance Nodes
    - Chance nodes represent probabilistic outcomes and always have a cost associated with performing the experiment
- Terminal Nodes
    - Terminal Nodes are outcomes which will have payoffs associated with them

##### Creating a new Tree
Use `new` method to create and initialize the new tree
    `new_tree <- Tree$new('newTree')`
##### Adding a chance node
To add a chance node, use the tree that was created and define the parent node where the Chance node needs to connect to. A chance is an experiment and there is always a cost attached to it. 
    `new_tree$addChance(name='chance_1',parent='new_tree',route='Yes',cost=20)`
##### Adding a terminal node
To add a terminal node, use the tree that was created and define the parent node where the terminal node needs to connect to. A terminal is an experiment and there is always a payoff attached to it. If the parent node is a chance event then add the probability from the chance event that leads to the terminal node
    `new_tree$addTerminal(name='term_1',parent='chance_1',prob,payoff)`



## Acknowledgements
We make use of the `data.tree` package heavily in creating the graphical models. We also use stats package and R6 package for creating the utility functions
