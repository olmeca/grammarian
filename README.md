# grammarian
A wrapper for Nim's PEG library, that supports extensions to Nim's PEG DSL, 
that simplify handling of complex PEG's.
### Rationale
As an avid user of the PEG library I have attempted matching / parsing ever more complex textual structures. 
I have learned that Nim's PEG can handle the complexity without ever breaking a sweat. Yet I noted that some
characteristics of the implementation do in effect impose some limitation on my proficiency developing and
using patterns. Furthermore, as a software developer I am accustomed to avoiding repeating myself, supported
by the language I use. Given a PEG describing a complex textual structure, e.g. a computer language or DSL,
if you want to use the PEG to extract specific substructures you have to adapt it. You have to extract from it
only the parts that describe the substructure of interest and the mark the parts to be extracted. This has to be
repeated for every substructure you want to address. In the end you have multiple PEG rulesets that only differ
in small parts. If you then discover a bug in your original PEG then you may have to fix it in all the 
substructure PEG's.

### PEG (Parsing Expression Grammar) 101
At the very low level, PEG's built-in matchers are quite similar to those offered by _Regex_. But whilst Regex
requires you to express the pattern only in terms of the built-in matchers, PEG also allows higher level
abstract patterns, which _describe the structure_ and _identify the parts_, but do **not** 
_describe the details of the parts_. It's like stating that an email address consists of in sequence a user name, an '@'
and a domain name. The structure (a sequence) is specified, one of the parts (the '@') is specified and two parts
have been identified, but not specified (user name and domain name). In PEG you identify a part by giving it a
unique name (of your choice). Parts identified in one PEG pattern can be specified in another pattern. 
Here is where pattern names come into play. In a PEG every pattern is named. I use the term _Rule_. 
A rule has a name and a pattern. For every subpart name mentioned in a pattern the PEG should also include
a rule with that name, of which the pattern describes the details of the subpart. Example:
```
EmailAddress <- UserName '@' DomainName
UserName <- [a-z]+
DomainName <- [a-z]+ ('.' [a-z]+)*
```
A rule starts with a name followed by a left arrow (<-) and a _pattern_. 
In a PEG there is one _root_ rule which may refer to other rules, forming a DAG. In this example the
first rule _EmailAddress_ is the root rule.
### PEG Inconveniences
1. Nim's PEG implementation requires all the rules in a PEG to form one DAG. It throws an error if any rule is not referred to 
by other rules (this may be a characteristic of every PEG implementation, I only know Nim's).
2. A PEG can only be used to match the pattern described by the whole DAG (i.e. the pattern described by root rule) 
   The above example PEG contains sufficient information for matching only domain names (not as part of an email address),
   but there is no way use only a part of a PEG for matching.
### Grammarian
Grammarian offers a couple of features that help eliminate the duplication and mitigate the rigidity of the
plain PEG library. Some of these features are based on extensions to the PEG DSL. Grammarian will interpret
these extensions and will produce standard PEG DSL pattern rules.
### Feature 1: more flexible PEG composition / decomposition
Grammarian offers a store for PEG rules, called a *Grammar*, from where you can easily retrieve a consistent PEG 
ruleset for use. The intended use is for storing the grammar of some complex textual structure (a grammar), or 
even storage of multiple grammars. When retrieving a rule set from the store you _specify a root rule name_. This
results in a rule set containing this root rule and all the rules referred directly or indirectly (a DAG).
For example, the following set of rules describe two textual data types: email addresses and URL's. These types
have some similarity in their composition. E.g. both contain a domain name. 
```
Url <- Protocol '://' Host '.' Domain '/' Path
EmailAddress <- AccountName '@' DomainName
AccountName <- Chars
DomainName <- Chars '.' Chars
Protocol <- 'https' / 'http'
Host <- Chars '.' Host / Chars
Path <- PathItem '/' Path / PathItem
Chars <- [a-z]+
PathItem <- [a-z-]+
```
The ability to define both types in one rule set would help eliminate duplication, in this example, of the
domain description. Using Grammarian you can use the above combined definition and create PEG objects
for any contained rule, by name:
```
# Assuming the string myRuleSet is the above rule set string
let grammar = newGrammar(myRuleSet)
let emailAddressPeg = grammar.matcher("EmailAddress")
# emailAddressPeg would be a PEG object based on this subset of rules:
EmailAddress <- AccountName '@' DomainName
AccountName <- Chars
DomainName <- Chars '.' Chars
Chars <- [a-z]+
```
Though called a *store* here, a *Grammar* is just an in memory object. 
### Feature 2: Rule (set) variants
Imagine a PEG rule set describing SQL statements. Most of the rules would apply to all mainstream DBMS's. But
there are always some minor differences in syntax between the DBMS's. With the plain PEG library you would have
to use multiple rulesets, even though the content of the rulesets would be for 95% identical. Grammarian 
addresses this by introducing _rule variants_. A rule variant is a rule that is tagged with a variant name.
There may be multiple rules with the same name, but different variant tags. For example the ANSI SQL datatype
_FLOAT4_ is called _FLOAT_ in MySQL and _REAL_ in PostgreSQL. This can be captured using Grammarian, with the
following PEG rules:
```
Float4 <- 'FLOAT' / 'float4'
Float4:MySql <- 'FLOAT' / 'float'
Float4:Postres <- 'REAL' / 'real'
```
The tag is appended to the rule name, with a colon as separator.
When requesting a rule set from the store you can specify a variant. When looking up a rule, Grammarian will
first search for a rule with the given name and the given variant tag. If not found then it will search for
a rule only by name. This means you only need to create rule variants for the corner cases.
### Feature 3: Capture targets
Most of the time I use PEGs for extracting information from textual data. For Nim's PEG I need to mark the
items to be extracted by tagging their names in the rule set. This means that for every use case I need to
revise the tags in the rule set, or create multiple copies of the same rule set to support both use cases.
Grammarian addresses this issue by letting you store the rule set(s) untagged in the store. When requesting
a ruleset from the store you can specify a list of rule names. All rule names except the root rule's are
also none-terminals in some other rule. All non-terminals whose names were specified in the list will be
marked for extraction in the rule set retrieved from the store.
As an example, imagine a (somwhat simplified) PEG describing web page URL's:
```
Url <- Protocol '://' Host '/' Path '?' Parameters
Protocol <- 'https' / 'http'
Host <- Chars '.' Host / Chars
Path <- PathItem '/' Path / PathItem
Parameters <- KeyValue '&' Parameters / KeyValue
KeyValue <- Key '=' Value
Chars <- [a-z]+
PathItem <- [a-z-]+
Key <- [a-zA-Z0-9_]+
Value <- [a-zA-Z0-9_+-]+ / ''
```
This can be used for multiple extraction use case: extracting host names, paths, parameters, to name a few.
But for every case you would have to use an adapted version of this PEG. For example, to extract host names
you would have to adapt the first rule like so:
```
Url <- Protocol '://' {Host} '/' Path '?' Parameters
```
But to extract the individual parameters, you would instead have to adapt this rule:
```
Parameters <- {KeyValue} '&' Parameters / {KeyValue}
```
Using Grammarian you would create a Grammar object using the above URL description PEG and then ask it for
a PEG, passing as parameters the desired capture items:
```
let grammar = newGrammar(pegString)
let paramsPeg = grammar.extractorPeg(["KeyValue"])
let hostPeg = grammar.extractorPeg(["Host"])
# Marking multiple items for capture:
let protocolAndHostPeg = grammar.extractorPeg(["Protocol", "Host"])

```
This mechanism only works for non-terminals! Keep this in mind when defining your PEG rules. Any terminal
expression can be put into a separate rule and then you can specify the rule name for capture.
### Feature 4: Parameterized rules
PEG is all about structures and substructures. I have encountered a number of cases where the same higher
level structure is used but with different substructures. In pure PEG you have to repeat yourself with 
respect to the high level structure if you need to cover both substructure cases. This may sound quite 
abstract, so here's a stupidly simple example structure: a list. A textual representation of a list of
items always consists of the items themselves, plus some textual separator, like a linebreak or a comma.
The list item could be a telephone number, an amount, a bank account number, a name, etc. So the PEG
describing the item and the separator are specific to the case, but the PEG for the list structure is
invariant. A parameterized rule describes the high level structure (here: the list) in terms of
pseudo-non-terminals that represent the substructures. The pseudo-non-terminals are however not
references to other rules. They are parameters to the rule. When another rule references a parameterized 
rule, the reference must include the real values to be used instead of the parameters. The values in the
reference are called *arguments*.
A parameterized rule is specified as follows:
```
List<Item, Separator> <- Item Separator List<Item, Separator> / Item
```
Then you could reference this rule in another rule, to describe a comma separated list of numbers:
```
InClause <- 'IN' \s* '(' List<Number, Comma> ')' \s*
```
When you retrieve a rule set from the store that would involve these rules, what you get would be:
```
List_Number_Comma <- Number Comma List_Number_Comma / Number
InClause <- 'IN' \s* '(' List_Number_Comma ')' \s*
```
Here the rule is *applied* to the arguments which yields a *resolved* rule. The name of the resolved rule
is derived from the original rule's name and the arguments. As in this case the arguments 'Number' and
'Comma' are non-terminals, i.e. pure names, they can be directly incorporated into the resolved rule's name.
This gives a resolved rule name that is unique for the combination of rule and arguments and is also 
meaningful.
Another variation is with arguments that are not themselves non-terminal. Imagine instead using terminals:
```
InClause <- 'IN' \s* '(' List<\d+, ','> ')' \s*
```
Combined with the same List rule, this expression would result in the following:
```
List_p2258791698_p3582574359 <- Number Comma List_p2258791698_p3582574359 / Number
InClause <- 'IN' \s* '(' List_p2258791698_p3582574359 ')' \s*
```
Now the resolved rule's name contains meaningless numbers instead of meaningful names. The reason is that
terminal expressions like the above "\d+" cannot be part of a rule name. A rule name must consist of
letters, digits and underscores. But we must resolve to a rule name that is unique to the combination
of the original rule's name and the arguments. This is solved by using hashes of the argument values.
The numbers in the resolved rule name are these hashes. Note that you never need to deal directly with
these generated names. The exist only in an intermediate step towards creating the PEG you retrieve from
the Grammar object.

### Feature 5: Named values extractor
The above explained that a Grammar will provide you a standard PEG, which you can then use for matching
like you would do with any non-grammar generated PEG. Nim's matching mechanism will return a fixed array containing
the matched values.
For convenience, the Grammar can also provide you an Extractor object. An Extractor consists of a PEG and a list
of non-terminals to be captured. An _extract_ function, when passed an Extractor and an input string,
will return a hash table, containing the captured items as entries. The keys are the non-terminal names and
the values are the captured strings from the input provided. An example:
```
let grammar = newGrammar(myPegString)
let extractor = grammar.newPatternExtractor(myRootNonTerminal, myNonTerminalsToCapture)
let foundItems = extractor.extract(myInputString)
```
I have found this to often be more convenient than retrieving the PEG from the Grammar, performing the match
and then extracting the items one by one from the match results array. 

### How to use Grammarian
1- Just add Grammarian as a dependency to your project's _.nimble_ file:
```
requires "grammarian >= 0.2.0"
```
2- Add an import statement to your source file:
```
import grammarian
```
3- Call grammarian functions: 
1. Create a Grammar object, providing a string that represents a PEG rule set. This
rule set may also contain the extension expressions described under the Feature sections.
2. Retrieve a standard PEG from the Grammar, by calling either the function _matcher()_
   (for a PEG without any captures) or _extractorPeg()_ for a PEG with captures.
3. Use the obtained PEG for your matching purposes.
```
let grammar = newGrammar(myPegRuleSet)
let myRootNonTerminal = "EmailAddress"

# Using a PEG to determine whether a string matches or not
let matcherPeg = grammar.matcher(myRootNonTerminal)
let success = "admin@example.com".match(matcherPeg)

# Create a PEG to extract user name and domain name
let myCaptureTargets = @["UserName", "DomainName"]
let capturingPeg = newExtractorPeg(grammar, myRootNonTerminal, myCaptureTargets)
# Use this PEG to match and capture
if "admin@example.com" =~ capturingPeg:
   let userName = matches[0]
   let domainName = matches[1]
else: discard
```
Alternatively you can use the convenience Extractor:
```
# Using an Extractor object
let extractor = newPatternExtractor(grammar, myRootNonTerminal, myCaptureTargets)
let foundItems = extractor.extract("admin@example.com")
let userName = foundItems["UserName"]
let domainName = foundItems["DomainName"]
```
Examples with variants will be added soon.
