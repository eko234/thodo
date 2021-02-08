# thodo

Data types, parser and actions to manipulate minimalistic todo lists like
org mode, it is made with kakoune text editor in mind but can be easily 
integrated with other tools

## format
the format of an entry goes like this:
[date-it-was-done|empty] any kind of text <<due-date>>|nothing  

## examples
### task without due date
[] do the dishes 
### task with due date
[] pack things up for the gig <2020-12-12> 
### task already done
[2020-03-15] send money to grandma
