/* This programs decrypts the text of the obfuscated.c program. */

#include <stdio.h>
#include <string.h>

int main (int argc, char * argv)
{
    char *string = strdup("@n'+,#'/*{}w+/w#cdnr/+,{}r/*de}+,/*{*+,/w{%+,/w#q#n+,/#{l,+,/n{n+,/+#n+,/#\
            ;#q#n+,/+k#;*+,/'r :'d*'3,}{w+K w'K:'+}e#';dq#'l\
            q#'+d'K#!/+k#;q#'r}eKK#}w'r}eKK{nl]'/#;#q#n'){)#}w'){){nl]'/+#n';d}rw' i;# \
            ){nl]!/n{n#'; r{#w'r nc{nl]'/#{l,+'K {rw' iK{;[{nl]'/w#q#n'wk nw' \
            iwk{KK{nl]!/w{%'l##w#' i; :{nl]'/*{q#'ld;r'}{nlwb!/*de}'c \
            ;;{nl'-{}rw]'/+,}##'*}#nc,',#nw]'/+kd'+e}+;#'rdq#w! nr'/ ') }+}{rl#'{n' ')# \
            }'+}##(!!/");
    char *trans = strdup("!ek;dc i@bK'(q)-[w]*%n+r3#l,{}:\nuwloca-O;m .vpbks,fxntdCeghiry");
    int length = strlen(string);
    int length2 = strlen(trans);
    int i;
    for (i = 0; i < length; i++) {
        int j = 0;
        if (string[i] == '/') {
            string[i] = '\n';
        } else {
            while (string[i] != trans[j] && (j+32) < length2) {
                j++;
            }
            string[i] = trans[j+31];
        }
    }
    printf("%s", string);
    return 0;
}
