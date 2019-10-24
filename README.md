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


