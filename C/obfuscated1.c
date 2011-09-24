/* Graden's explanation
 * takes input from stdin, identifies sequences of 4+ or 8+ chars seperated by anything that is not a charactor
 * for sequences len(s) < 4, prints s
 * for seqences 4 <= len(s) < 8 prints poot with each char of case inputted
 * for sequences len(s) <= 8 prints pootpoot with each char of the case inputted
 * all charactors that are not a letter are printer unaltered
 * the char '<' put anywhere causes the program to print all subsequent input unaltered
 * the char '>' re-enable functionality on subsequent input after '<' char has disabled it
 */
#include <stdio.h>
#define PO(o,t)\
(((o>64)&&(o<91))?(((t>96)&&(t<123))?(t-32):(t)):(((t>64)&&(t<91))?(t+32):(t)))  

      void main() {                                       char *poo= "poot",
      *Poo="pootpoot"      ,O[9];int      o,t,T,p;(t=p   =0)||(*O='\0');while
      ((o=       getc(   stdin   ))!=(   EOF))if  ((p==   0)&& (((o>64 )&& (
      o<91       )) ||   ((o>     96 )   &&(o<     123)        ))) (
      t!=8       )&&(O   [t]=     o)&&   (O[++     t] =        '\0')
      ;else {if (t>7)    {for     (T =   0 ; T     <=7;        T++ )
      printf("%c",       PO(*(   O+T),   *(Poo+   T)));       printf
      ("%c",              o);}else if     (t>3){for (T        =0;T<=
      3;T++)                                                  printf
      ("%c",                                                  PO(*(O
      +T),*(                                                  poo+T)

) ) ; printf( "%c" , o ) ; } else  printf ( "%s%c" , O , o )  ; ( t =  0 ) || (
* O =  '\0' ) ; ( o == 60 ) && ( ++p ) ; ( o == 62 ) && (p!=0) && ( --p ) ; } }
