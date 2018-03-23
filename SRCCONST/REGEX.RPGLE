000100110829     D* regcomp() cflags
000200110829     D REG_BASIC       C                   X'0000'                              Basic RE rules (BRE)
000300110829     D REG_EXTENDED    C                   X'0001'                              Extended RE rules
000400110829     D*                                                                         (ERE)
000500110829     D REG_ICASE       C                   X'0002'                              Ignore case in match
000600110829     D REG_NEWLINE     C                   X'0004'                              Convert
000700110829     D*                                                                         <backslash><n> to
000800110829     D*                                                                         <newline>
000900110829     D REG_NOSUB       C                   X'0008'                              regexec() not report
001000110829     D*                                                                         subexpressions
001100110829     D REG_ALT_NL      C                   X'0010'                              POSIX: Use IFS
001200110829     D*                                                                         newline instead of
001300110829     D*                                                                         database newline
001400110829     D*                                                                         UTF32: Use database
001500110829     D*                                                                         newline instead of
001600110829     D*                                                                         IFS newline.
001700110829     D* regexec() eflags
001800110829     D REG_NOTBOL      C                   X'0100'                              First character not
001900110829     D*                                                                         start of line
002000110829     D REG_NOTEOL      C                   X'0200'                              Last character not
002100110829     D*                                                                         end of line
002200110829     D* Regular Expression error codes
002300110829     D REG_NOMATCH     C                   1                                    RE pattern not found
002400110829     D REG_BADPAT      C                   2                                    Invalid Regular
002500110829     D*                                                                         Expression
002600110829     D REG_ECOLLATE    C                   3                                    Invalid collating
002700110829     D*                                                                         element
002800110829     D REG_ECTYPE      C                   4                                    Invalid character
002900110829     D*                                                                         class
003000110829     D REG_EESCAPE     C                   5                                    Last character is \
003100110829     D REG_ESUBREG     C                   6                                    Invalid number in
003200110829     D*                                                                         \digit
003300110829     D REG_EBRACK      C                   7                                    imbalance
003400110829     D REG_EPAREN      C                   8                                    \( \) or ()
003500110829     D*                                                                         imbalance
003600110829     D REG_EBRACE      C                   9                                    \{ \} or { }
003700110829     D*                                                                         imbalance
003800110829     D REG_BADBR       C                   10                                   Invalid \{ \} range
003900110829     D*                                                                         exp
004000110829     D REG_ERANGE      C                   11                                   Invalid range exp
004100110829     D*                                                                         endpoint
004200110829     D REG_ESPACE      C                   12                                   Out of memory
004300110829     D REG_BADRPT      C                   13                                   ?*+ not preceded by
004400110829     D*                                                                         valid RE
004500110829     D REG_ECHAR       C                   14                                   invalid multibyte
004600110829     D*                                                                         character
004700110829     D REG_EBOL        C                   15                                   (shift 6 caret or
004800110829     D*                                                                         not) anchor and not
004900110829     D*                                                                         BOL
005000110829     D REG_EEOL        C                   16                                   $ anchor and not EOL
005100110829     D REG_ECOMP       C                   17                                   Unknown error in
005200110829     D*                                                                         regcomp() call
005300110829     D REG_EEXEC       C                   18                                   Unknown error in
005400110829     D*                                                                         regexec() call
005500110829     D REG_LAST        C                   18                                   Needs to always be
005600110829     D*                                                                         the greatest
005700110829     D*                                                                         regerror uses it to
005800110829     D*                                                                         check for valid
005900110829     D*                                                                         number
