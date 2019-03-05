use "regex.sml";
use "ndfa.sml";

val SIGMA = (explode "ab");
val ndfa = parse SIGMA "(aa|bb)*((ab|ba)(aa|bb)*(ab|ba)(aa|bb)*)*";
val dfa = to_dfa ndfa;
print (Bool.toString (in_language (explode "abab") ndfa));
print "\n";
print (Bool.toString (in_language (explode "ababaa") ndfa));
print "\n";
print (Bool.toString (in_language (explode "ababbba") ndfa));
print "\n";
use "dfa.sml";
print (Bool.toString (in_language (explode "abab") dfa));
print "\n";
print (Bool.toString (in_language (explode "ababaa") dfa));
print "\n";
print (Bool.toString (in_language (explode "ababbba") dfa));
print "\n";

