# HTN_Planner: Shop2

This project aims to implement an Hierarchical Task Network (HTN) planning based on [Shop2](https://github.com/cl-axon/shop2.git) using input as HDDL modelling language.  


## Installation
Please download the SLIME and Steel Bank Common Lisp compiler locally and install the program through git command. 

## Usage
Our team uses SLIME for Common Lisp Development on the ground of Steel Bank Common Lisp compiler.
As a suggestion, to run the algorithm, please load the htn.lisp file first on SLIME with the following code snippet.  
```lisp
(load "./your/path/to/file/htn.lisp")
```
After loading the file, you are ready to run our program with the following command.
```lisp
(shop2-operator)
```
The program will ask for domain.hddl and problem.hddl as follows. Please enter your domain and problem file in a form of HDDL structure. 
```lisp
Enter domain filepath"domain_file.hddl"
Enter problem filepath"problem_file.hddl"
```
Then, the program will show the possible plan, when it does not drop into local solution.

## Limitations
+ This project implements the input with simplified HDDL from the resources of Datasets named as Transport. If the user calls inputs from other enlarged resources may cause nonoptimal solution. 
+ Please follows the formation of HDDL modeling language with respect to simplified version of inputs. Furthermore, the restriction on the complexity of modeling language. This project limits itself merely on totally ordered or unordered subtasks. Also the logic operator (AND, NOT) within effects and precondition of one action or task can be implemented through this project. However, the logical operator of OR in the tasks, preconditions or effects can not be reinforced in this project. 
+ Through variables-binding, this project does support merely on the same hierarchical layer of parsing type not on the subtasks' layer or on the superset of current-task' layer. 

## Resources
*[SBCL](http://www.sbcl.org/) <br/>
*[SLIME](https://slime.common-lisp.dev/#:~:text=SLIME%20is%20a%20Emacs%20mode%20for%20Common%20Lisp,invitation%20to%20learn%20more%20about%20what%27s%20going%20on.) <br/>
*[Datasets of HDDL](https://github.com/panda-planner-dev/ipc2020-domains/tree/master/partial-order/Transport)<br/>

## Teams
Alisa Veronique Münsterberg and
Yi-Shan Lee 
under the guidance of the department of Smart Environments of the Otto-Friedrich-Universität Bamberg.