000100110829      /IF NOT DEFINED(regex_t)
000200110829     D regex_t         DS                  ALIGN
000300110829     D  re_nsub                      10I 0
000400110829     D  re_comp                        *
000500110829     D  re_cflags                    10I 0
000600110829     D  re_erroff                    10I 0
000700110829     D  re_len                       10I 0
000800110829     D  re_ucoll                     10I 0 DIM(2)
000900110829     D  re_lsub                        *   DIM(9)
001000110829     D  re_esub                        *   DIM(9)
001100110829     D  re_map                      256A
001200110829     D  re_shift                      5I 0
001300110829     D  re_dbcs                       5I 0
001400110829      /ENDIF
001500110829
001600110829      /IF NOT DEFINED(regmatch_t)
001700110829     D regmatch_t      DS                  OCCURS(2) ALIGN
001800110829     D  rm_so                        10I 0
001900110829     D  rm_ss                         5I 0
002000110829     D  rm_eo                        10I 0
002100110829     D  rm_es                         5I 0
002200110829      /ENDIF
002300110829
002400110829     D RegularExpression_Compile...
002500110829     D                 PR            10I 0 EXTPROC('regcomp')
002600110829     D  preg                           *   VALUE
002700110829     D  pattern                        *   VALUE
002800110829     D  cflags                       10I 0 VALUE
002900110829
003000110829     D RegularExpression_Execute...
003100110829     D                 PR            10I 0 EXTPROC('regexec')
003200110829     D  preg                           *   VALUE
003300110829     D  string                         *   VALUE
003400110829     D  nmatch                       10U 0 VALUE
003500110829     D  pmatch                         *   VALUE
003600110829     D  eflags                       10I 0 VALUE
003700110829
003800110829     D RegularExpression_ReturnErrorMessage...
003900110829     D                 PR            10U 0 EXTPROC('regerror')
004000110829     D  errcode                      10I 0 VALUE
004100110829     D  preg                           *   VALUE
004200110829     D  errbuf                         *   VALUE
004300110829     D  errbuf_size                  10I 0 VALUE
004400110829
004500110829     D RegularExpression_FreeMemory...
004600110829     D                 PR                  EXTPROC('regfree')
004700110829     D  preg                           *   VALUE
