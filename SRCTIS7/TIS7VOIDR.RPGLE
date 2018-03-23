000100131108      /TITLE Upload via Internet
000200100308     H DFTACTGRP(*NO) BNDDIR('TRUL':'UBRTVNETA') ACTGRP('BARTVAS')
000300000313     F*
000400990910     Ftivin00r  uF   E             DISK    usropn
000500000313     D*
000600010330     D*----------------------------------------------------
000700010330     D* DICHIARAZIOINE VARIABILI DI WRK
000800010330     D*----------------------------------------------------
000900150706     D tivlrds       e ds                  extname(tivlr00f)
001000010330     D esito           s              1
001100010330     D prmlit          s             10
001200010330     D prmfir          s             10
001300010330     D wrkesito        s                   like(esito)
001400080702
001500010330
001600100308     C*
001700990921     C                   reset                   esito
001800990921     C                   reset                   wrkesito
001900010601     C*
002000131108     C                   exsr      rwfile
002100100308     C*
002200010601     C                   seton                                        lr
002300010601
002400150706
002500010601
002600131108     C     RWFILE        BEGSR
002700150706     C*
002800150706     C                   setoff                                       31
002900131108     C*
003000131108     C* Inizializzo subito ad OK l'esito della transazione
003100131108     C                   eval      wrkesito = '0'
003200010330     C*
003300010330     C                   if        not %open(tivin00r)
003400010330     C                   open      tivin00r
003500010330     C                   endif
003600010330     C*
003700030822     C                   do        *HIVAL
003800010330     C*
003900030822     C                   read      tivin00r                               70
004000040510     C*
004100150706     C                   if        vindta > *blanks
004200010330     C*
004300010601     C                   if        *in70 = *off and
004400010330     C                             (vinflg = *blanks
004500010330     C                              or vinflg = '0'
004600010330     C                              or vinflg = '2')
004700010330     C*
004800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
004900010711     C                   if        vinflg = *blanks or vinflg = '0'
005000010711     C                   clear                   vinmsg
005100010711     C                   endif
005200010604     C*
005300131108     C                   if        *in31 = *off
005400010604     C                   eval      vinflg = '1'
005500010604     C                   else
005600010604     C                   eval      vinflg = '2'
005700010604     C                   endif
005800010601     C*
005900010604     C                   endif
006000010604     C*
006100010330     C                   else
006200010330     C                   eval      vinflg = '1'
006300010330     C                   endif
006400010601     C*
006500010601     C  N70              update    tivin000
006600010330     C*
006700150126     C  N70              enddo
006800010330     C*
006900010330     C                   if        %open(tivin00r)
007000010330     C                   close     tivin00r
007100010330     C                   endif
007200131108     C*
007300150126     C* controllo se ci sono stati warning o errori
007400150126     C                   eval      wrkesito = '0'
007500010330     C*
007600010330     C                   ENDSR
007700010330     C***
007800050421
007900050421
008000050421     C     *pssr         BEGSR
008100050421     C*
008200050421     C                   eval      wrkesito = '2'
008300050421     C*
008400050421     C                   ENDSR     '*CANCL'
008500050421     C***
008600010330
008700010330
008800000613     C     *inzsr        BEGSR
008900990910     C*
009000990910     C     *entry        plist
009100990920     C                   parm                    tivlrds
009200990921     C                   parm      wrkesito      esito
009300000724     C                   parm                    prmlit
009400000710     C                   parm                    prmfir
009500131108     C*
009600131108     C                   ENDSR
009700000613     C***
