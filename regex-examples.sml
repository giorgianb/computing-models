use "regex.sml";
use "ndfa.sml";

val SIGMA = (explode "ab");
val (ndfa, _) = parse SIGMA 
  (explode "((aa)|(bb))*(((ab)|(ba))((aa)|(bb))*((ab)|(ba))((aa)|(bb))*)*");

print (Bool.toString (in_language (explode "abab") ndfa));
print "\n";
print (Bool.toString (in_language (explode "ababaa") ndfa));
print "\n";
print (Bool.toString (in_language (explode "ababbba") ndfa));
print "\n";
