000100020731     Ftitasque  O    E             DISK    usropn
000200020731
000300020612     D DS£ECO        E DS
000400990908
000500020731     C*
000600020731     C* Innanzitutto spengo l'indicatore che condiziona la scrittura di TITASQUE
000700020731     C                   setoff                                       55
000800020731     C*
000900020731     C                   if        D£GLIB = 'GAITRAGRU'
001000020731     C                   seton                                        55
001100020731     C                   endif
001200020731     C*
001300020731     C                   if        not %open(titasque)
001400020731     C                   call      'TRECD3C'
001500020731     C                   parm                    D£GLIB
001600020731     C                   open      titasque
001700020731     C                   endif
001800020731     C
001900020614     C                   clear                   titasq00
002000020617     C                   eval      queflg = *blanks
002100020617     C*
002200020617     C* Testo il file della immagine passata: se trattasi di TITAS10F o TNCSB00F
002300020617     C* imposto diversamente il flag x nn sottoporli ad elaborazoine C/E.
002400020617     C                   if         D£GLIB = 'GAITRAGRPS'
002500020617     C                   eval      queflg = '1'
002600020617     C                   endif
002700020619     C***                if         D£GLIB = 'GAITRAGRU' AND
002800020619     C***                           D£GFIL = 'TITAS10F'
002900020618     C***                eval      queflg = '3'
003000020619     C***                endif
003100020619     C***                if         D£GLIB = 'GAITRAGRU' AND
003200020619     C***                           D£GFIL = 'TNCSB00F'
003300020618     C***                eval      queflg = '3'
003400020619     C***                endif
003500020617     C                   if         D£GLIB = 'EDPFG' AND
003600020617     C                              D£GFIL = 'TESTRG0F'
003700020617     C                   eval      queflg = '1'
003800020731     C                   seton                                        55
003900020617     C                   endif
004000020617     C*
004100020612     C                   eval      quedta = DS£ECO
004200020731     C   55              write     titasq00
004300000724     C*
004400020731     C                   if        D£GTJB = 'I'
004500020731     C                   seton                                        lr
004600020731     C                   else
004700020612     C                   seton                                        rt
004800020731     C                   endif
004900000613     C***
005000990920
005100990910
005200000613     C     *inzsr        BEGSR
005300990910     C*
005400990910     C     *entry        plist
005500020612     C                   parm                    DS£ECO
005600000613     C*
005700000613     C                   ENDSR
005800000613     C***
