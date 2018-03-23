000100091022      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
000200081217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400091013     FTITV1S41L uF   E           k DISK
000500990908
000600000801     D*----------------------------------------------------
000700000801     D* DICHIARAZIOINE VARIABILI DI WRK
000800000801     D*----------------------------------------------------
000900990920     D tivlrds       e ds                  extname(tivlr00f)
001000990910     D esito           s              1
001100000724     D prmlit          s             10
001200000710     D prmfir          s             10
001300990921     D wrkesito        s                   like(esito)
001400010201
001500081217
001600091014     C*
001700990921     C                   reset                   esito
001800990921     C                   reset                   wrkesito
001900000613     C*
002000091110     C                   EXSR      DEFCAM
002100091013     C                   EXSR      ELAFILE
002200000616     C*
002300000801     C
002400010201     C                   seton                                        LR
002500990908
002600000801
002700910830     C*--------------------------------------------------------
002800091013     C* ELAFILE  LEGGE TITV1S40F ED EFFETTUA CONSIDERAZIONI
002900910830     C*--------------------------------------------------------
003000091014     C     ELAFILE       BEGSR
003100990910     C*
003200091014     C                   clear                   低TROK            5 0
003300091014     C                   clear                   低TRNO            5 0
003400091013     C*
003500091022     C                   EVAL      TV1S4KEY  = '02101177_BOLLE'
003600990910     C*
003700091013     C     TV1S4KEY      setll     TITV1S41L
003800091013     C                   if        %found(TITV1S41L)
003900091013     C     TV1S4KEY      reade     TITV1S41L
004000091013     C                   dow       not %eof(TITV1S41L)
004100091123     C*
004200091123     C                   setoff                                       51
004300091123     C*
004400091123     C                   select
004500091123     C                   when      TV1S4VAB = 'X' and
004600091123     C                             TV1S4VAT = 'X'
004700091123     C                   seton                                        51
004800091123     C                   when      TV1S4VAB = 'X'  and
004900091123     C                             vlrtip   = 'VB' and
005000091123     C                             *IN50    = *on
005100091123     C                   seton                                        51
005200091123     C                   when      TV1S4VAT = 'X'  and
005300091123     C                             vlrtip   = 'VT' and
005400091123     C                             *IN50    = *on
005500091123     C                   seton                                        51
005600091123     C                   endsl
005700091123     C*
005800091123     C                   if        *in51 = *on
005900091123     C                   exsr      impvab                                       => carico  VAB
006000091014     C*
006100091014     C* Aggiorno il record...
006200091014     C                   if        errore <> *zeros
006300091014     C                   eval      TV1S4VAB = '*'
006400091014     C                   eval      TV1S4VAT = '*'
006500091014     C                   endif
006600091014     C*
006700091014     C                   update    TITV1S400
006800000905     C*
006900050628     C                   endif
007000000905     C*
007100091013     C     TV1S4KEY      reade     TITV1S41L
007200091013     C*
007300091013     C                   ENDdo
007400010202     C*
007500010202     C                   endif
007600091014     C*
007700990910     C* Se non ci sono record con errori ...
007800000710     C                   if        呃trno = 0
007900990910     C* ... restituisco esito OK.
008000990921     C                   eval      wrkesito = '0'
008100990910     C                   else
008200091014     C                   if        呃trok > 0
008300990921     C                   eval      wrkesito = '1'
008400000710     C                   else
008500000710     C                   eval      wrkesito = '2'
008600990910     C                   endif
008700000710     C                   endif
008800990920     C*
008900910830     C                   ENDSR
009000000613     C***
009100091013
009200091013     C*----------------------------------------------------*
009300091013     C*  INIZIALIZZAZIOINE VARIABILI DI WRK
009400091013     C*----------------------------------------------------*
009500091013     C     INZVAR        BEGSR
009600091013     C*
009700091013     C*
009800091013     C                   ENDSR
009900091013
010000000801     C*----------------------------------------------------*
010100000801     C*  IMPOSTAZIONE CAMPI COSTANTI
010200000801     C*----------------------------------------------------*
010300000801     C     DEFCAM        BEGSR
010400091123     C*
010500091123     C                   SETOFF                                       50
010600000801     C*
010700020619     C* Imposto i valori di default...
010800020619     C* ... e poi verifico se sono stati passati come parametri
010900091110     C                   IF        %subst(vlrppt:21) > *blanks
011000091123     C                   IF        %subst(vlrppt:21:1) = 'X'
011100091123     C                   SETON                                        50
011200091123     C                   ENDIF
011300020619     C                   ENDIF
011400000801     C*
011500000801     C                   ENDSR
011600091013
011700000801     C*----------------------------------------------------*
011800091022     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
011900000801     C*----------------------------------------------------*
012000091013     C     IMPVAB        BEGSR
012100010305     C*
012200000801     C                   Z-ADD     *zeros        errore            1 0
012300091022     C*
012400091022     C* Aggiorno il numero spedizione con il riferimenti mittente numerico...
012500091110     C                   move      vabRMN        vabNSP
012600091110     C                   move      vabRMN        vatNSP
012700091022     C*
012800091022     C* Modifico il codice trattamento merce in modo da prevedere anche il packing list
012900091202     C                   eval      vabCTM = '2A'
013000010202     C*
013100000801     C* Ebbene...
013200010201     C                   IF        errore <> *zeros
013300000801     C                   ADD       1             低TRNO
013400000801     C                   ELSE
013500091014     C                   ADD       1             低TROK
013600000801     C                   ENDIF
013700000801     C*
013800000801     C                   ENDSR
013900090331
014000090331
014100090331
014200070411     C     *pssr         BEGSR
014300070411     C*
014400070411     C                   eval      wrkesito = '2'
014500070411     C*
014600070411     C                   seton                                        LR
014700070411     C*
014800070411     C                   ENDSR     '*CANCL'
014900070411     C***
015000070411
015100090331
015200990910
015300000613     C     *inzsr        BEGSR
015400990910     C*
015500990910     C     *entry        plist
015600990920     C                   parm                    tivlrds
015700990921     C                   parm      wrkesito      esito
015800000724     C                   parm                    prmlit
015900000710     C                   parm                    prmfir
016000000830     C*
016100000613     C                   ENDSR
016200000613     C***
