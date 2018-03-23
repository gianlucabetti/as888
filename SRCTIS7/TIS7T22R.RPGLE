000100140818      /TITLE Upload via Internet: traduzione in TIIDC00F.
000200130108     H DFTACTGRP(*no) ACTGRP(*caller)
000300990915
000400140818     Ftivin00r  uF   E             DISK    usropn
000500140818     FTIIDC00F  O    E             DISK    commit
000600990915
000700080908
000800080908
000900000724     D*------------
001000051017     D kpjba         e ds                  inz
001100990915     D psds           sds
001200990915     D  procname         *PROC
001300990921     D tivlrds       e ds                  extname(tivlr00f)
001400140818     D TIIDCDS       e ds                  extname(TIIDC00F) inz
001500990915     D esito           s              1
001600000724     D prmlit          s             10
001700000710     D prmfir          s             10
001800990921     D wrkesito        s                   like(esito)
001900990915
002000990921     C                   reset                   esito
002100990921     C                   reset                   wrkesito
002200010531     C*
002300010531     C                   exsr      opeini
002400140818     C                   exsr      rwIDC
002500010531     C*
002600010531     C                   seton                                        lr
002700010531
002800010531
002900010531
003000990915
003100910830     C*--------------------------------------------------------
003200140818     C* RWIDC - SCRIVE TIIDC00F
003300910830     C*--------------------------------------------------------
003400140818     C     RWIDC         BEGSR
003500990915     C*
003600990915     C                   if        not %open(tivin00r)
003700990915     C                   open      tivin00r
003800990915     C                   endif
003900990915     C*
004000010601     C                   MOVE      *ZERO         低TROK
004100010601     C                   MOVE      *ZERO         低TRNO
004200921023     C                   DO        *HIVAL
004300010618     C*
004400990915     C                   READ      tivin00r                               70
004500030305     C*
004600030305     C* Dopo ogni lettura azzero gli indicatori di errore
004700030305     C                   SETOFF                                       31
004800010618     C*
004900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
005000010618     C                   if        vinflg = '1'
005100010618     C                   eval      flgOk = '1'
005200010618     C                   endif
005300010618     C*
005400000905     C                   if        vindta > *blanks
005500990915     C*
005600010601     C                   if        *in70 = *off and
005700990915     C                             (vinflg = *blanks
005800010711     C                              or vinflg = '0'
005900010711     C                              or vinflg = '2')
006000990915     C*
006100140818     C                   clear                   TIIDC000
006200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
006300010711     C                   if        vinflg = *blanks or vinflg = '0'
006400010711     C                   clear                   vinmsg
006500010711     C                   endif
006600010711     C*
006700140818     C                   eval      TIIDCDS = vindta
006800010601     C*
006900010601     C  N31              ADD       1             低TROK            7 0
007000010601     C   31              ADD       1             低TRNO            7 0
007100140818     C  N31              WRITE     TIIDC000
007200010601     C*
007300010601     C                   if        *in31 = *off
007400010601     C                   eval      vinflg = '1'
007500010601     C                   else
007600010601     C                   eval      vinflg = '2'
007700010601     C                   endif
007800010601     C                   endif
007900010604     C*
008000000905     C                   else
008100000905     C                   eval      vinflg = '1'
008200990915     C                   endif
008300000905     C*
008400000905     C  N70              update    tivin000
008500991022     C*
008600991022     C  N70              ENDdo
008700990915     C*
008800010531     C*
008900010531     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
009000010531     C                   if        cntNonEl = *zeros or
009100010531     C                             flgMulti = '0'
009200010601     C* Se non ci sono record con errori ...
009300010601     C                   if        呃trno = 0 and
009400010601     C                             flgStato <> '2'
009500010601     C* ... restituisco esito OK.
009600010601     C                   eval      wrkesito = '0'
009700010601     C                   else
009800010601     C                   if        呃trok > 0
009900010601     C                   eval      wrkesito = '1'
010000010601     C                   else
010100010615     C                   if        flgOk = '0'
010200010615     C                   eval      wrkesito = '2'
010300010615     C                   else
010400010615     C                   eval      wrkesito = '6'
010500010615     C                   endif
010600010601     C                   endif
010700010601     C                   endif
010800010531     C                   else
010900010531     C                   eval      wrkesito = '9'
011000010531     C                   endif
011100990915     C*
011200990915     C                   if        %open(tivin00r)
011300990915     C                   close     tivin00r
011400990915     C                   endif
011500990921     C*
011600140818     C* Se tutto Ok sino a qui => sancisco il commit
011700051108     C                   COMMIT
011800010531     C*
011900010531     C                   ENDSR
012000010531     C***
012100010531
012200010531
012300010531
012400010531      /TITLE Invio dei dati al punto operativo.
012500010531     C     opeini        BEGSR
012600010531     C*
012700010531     C* Inizializzo flag e contatori operativi
012800010531     C                   movel     '0'           flgGiro           1
012900010531     C                   movel     '0'           flgMulti          1
013000010531     C                   movel     '1'           flgStato          1
013100010615     C                   movel     '0'           flgOk             1
013200010531     C                   z-add     *zeros        cntNonEl         10 0
013300010531     C                   z-add     *zeros        depfil            3 0
013400010531     C                   z-add     *zeros        invfil            3 0
013500010531     C*
013600010531     C                   ENDSR
013700010531     C***
013800051108
013900051108
014000051108
014100051108      /TITLE Gestione errore generico
014200141007 xxx C***  *pssr         BEGSR
014300051108     C*
014400051108     C* Se errore => emetto operazione d ROLLBACK
014500141007     C***                ROLBK
014600051108     C*
014700141007     C***                ENDSR     '*CANCL'
014800080908     C***
014900010531
015000080908
015100990915
015200000613     C     *inzsr        BEGSR
015300990915     C*
015400990915     C     *entry        plist
015500990921     C                   parm                    tivlrds
015600990921     C                   parm      wrkesito      esito
015700000724     C                   parm                    prmlit
015800000710     C                   parm                    prmfir
015900000613     C*
016000130108     C                   z-add     *zeros        datcor            8 0
016100130108     C                   eval      datcor = %dec(%date() : *ISO)
016200130108     C*
016300000613     C                   ENDSR
016400000613     C***
