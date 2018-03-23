000100091022      /TITLE Upload via Internet: traduzione in EDIVATWR
000200091006     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500091012     FTITV1S41L uF A E           k DISK    rename(TITV1S400:TITV1S401)
000600091012     F                                     prefix(VAB_)
000700091012     FTITV1S42L uF A E           k DISK    rename(TITV1S400:TITV1S402)
000800091012     F                                     prefix(VAT_)
000900990908
001000000801     D*----------------------------------------------------
001100000801     D* DICHIARAZIOINE VARIABILI DI WRK
001200000801     D*----------------------------------------------------
001300990910     D psds           sds
001400990910     D  procname         *PROC
001500990920     D tivlrds       e ds                  extname(tivlr00f)
001600990910     D esito           s              1
001700000724     D prmlit          s             10
001800000710     D prmfir          s             10
001900990921     D wrkesito        s                   like(esito)
002000091022     D edivat_IN     e ds                  extname(edivatwr)
002100091022     D edivat_O_VAT  e ds                  extname(edivatwr) prefix(VAT_)
002200091022     D edivat_O_VAB  e ds                  extname(edivatwr) prefix(VAB_)
002300091012     D wTV1S4KEY       s                   like(VAT_TV1S4KEY)
002400010201
002500091022
002600091022     D*
002700091022     D fnvataar        ds                  inz
002800091022     D  a_vatatb               1      1
002900091022     D  a_vatccm               3      9
003000091022     D  a_vatlnp              11     13
003100091022     D  a_vataas              15     18
003200091022     D  a_vatnrs              20     21
003300091022     D  a_vatnsp              23     29
003400091022     D  a_vattrc              30     30
003500091022     D  a_vatnot              31     65
003600091006
003700010201
003800990921     C                   reset                   esito
003900990921     C                   reset                   wrkesito
004000000613     C*
004100091012     C                   EXSR      RWFILE                                       LETT/SCR. VAT
004200000616     C*
004300010201     C                   seton                                        LR
004400990908
004500000801
004600910830     C*--------------------------------------------------------
004700091022     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVATWR
004800910830     C*--------------------------------------------------------
004900040526     C     RWFILE        BEGSR
005000990910     C*
005100990914     C                   if        not %open(tivin00r)
005200990908     C                   open      tivin00r
005300990914     C                   endif
005400091012     C*
005500091012     C                   clear                   §CTROKVT          5 0
005600091012     C                   clear                   §CTRMO            5 0
005700091012     C                   clear                   §CTRNO            5 0
005800990910     C*
005900921023     C                   DO        *HIVAL
006000990913     C*
006100990915     C                   READ      tivin00r                               70
006200050627     C                   if        vindta > *blanks
006300000801     C*
006400000801     C                   if        *in70 = *off
006500000801     C                             and
006600000801     C                             (vinflg = *blanks
006700000801     C                              or vinflg = '0'
006800000801     C                              or vinflg = '2')
006900000801     C*
007000000801     C                   clear                   vinmsg
007100000801     C                   eval      vinflg = '1'
007200010305     C*
007300091022     C                   clear                   edivat_in
007400091012     C                   exsr      impvat                                       => carico VAT
007500091012     C                   exsr      wrivat                                       => scarico VAT
007600000905     C*
007700000905     C                   else
007800000905     C                   eval      vinflg = '1'
007900050628     C                   endif
008000000905     C                   endif
008100000905     C*
008200000905     C  N70              update    tivin000
008300000905     C*
008400991022     C  N70              ENDdo
008500990910
008600091012
008700990910     C* Se non ci sono record con errori ...
008800000710     C                   if        §ctrno = 0
008900990910     C* ... restituisco esito OK.
009000990921     C                   eval      wrkesito = '0'
009100990910     C                   else
009200091012     C                   if        §ctrokvt > 0
009300990921     C                   eval      wrkesito = '1'
009400000710     C                   else
009500000710     C                   eval      wrkesito = '2'
009600990910     C                   endif
009700000710     C                   endif
009800990910     C*
009900990914     C                   if        %open(tivin00r)
010000990908     C                   close     tivin00r
010100990914     C                   endif
010200990920     C*
010300910830     C                   ENDSR
010400000613     C***
010500010305
010600010305     C*----------------------------------------------------*
010700091012     C*  SCARICAMENTO BUFFER RECORDS VAT
010800010305     C*----------------------------------------------------*
010900091012     C     WRIVAT        BEGSR
011000091012     C*
011100091022     C                   EVAL      wTV1S4KEY  = '02101177_BOLLE'
011200091012     C*
011300091022     C* Verifico se già esistono i record EDIVAB correlati
011400091014     C     KEY1s4_1_P    CHAIN     TITV1S41L
011500091012     C*
011600091012     C                   IF        %found(TITV1S41L)
011700091012     C                   IF        VAB_TV1S4VAT = *blanks
011800091012     C                   DELETE    TITV1S401
011900091022     C                   EVAL      edivat_o_vab = edivat_in
012000091012     C                   EVAL      VAB_TV1S4KEY  = wTV1S4KEY
012100091012     C                   EVAL      VAB_TV1S4VAT  = 'X'
012200091012     C                   WRITE     TITV1S401
012300091013     C                   ELSE
012400091013     C                   UNLOCK    TITV1S41L
012500091012     C                   ENDIF
012600091012     C                   ENDIF
012700091012     C*
012800091012     C     KEY1s4_2      CHAIN     TITV1S42L
012900091012     C*
013000091012     C                   IF        %found(TITV1S42L)
013100091022     C                   EVAL      edivat_o_vat = edivat_in
013200091012     C                   EVAL      VAT_TV1S4KEY  = wTV1S4KEY
013300091012     C                   EVAL      VAT_TV1S4VAT  = 'X'
013400091012     C                   UPDATE    TITV1S402
013500091012     C*
013600091012     C                   ELSE
013700091012     C*
013800091022     C                   EVAL      edivat_o_vat = edivat_in
013900091012     C                   EVAL      VAT_TV1S4KEY  = wTV1S4KEY
014000091012     C                   EVAL      VAT_TV1S4VAT  = 'X'
014100091012     C                   WRITE     TITV1S402
014200091012     C                   ENDIF
014300010305     C*
014400010305     C                   ENDSR
014500990920
014600000801     C*----------------------------------------------------*
014700091022     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAT)
014800000801     C*----------------------------------------------------*
014900091012     C     IMPVAT        BEGSR
015000091022     C*
015100091022     C                   eval      fnvataar = vindta
015200091022     C*
015300091022     C                   SETOFF                                       303132
015400091022     C* PRIMA DI MUOVERE I CAMPI => TESTO CHE I NUMERICI SIANO TALI
015500091022     C                   TESTN                   a_vatccm             30
015600091022     C  N30              TESTN                   a_vatccm               30
015700091022     C  N30              TESTN                   a_vatccm                 30
015800091022     C                   MOVE      a_vatccm      W001A             1
015900091022     C     *IN30         IFEQ      *OFF
016000091022     C     W001A         ORLT      '0'
016100091022     C                   SETON                                        31
016200091022     C                   Z-ADD     *zeros        VATCCM
016300091022     C                   EVAL      vinmsg = 'VATCCM'
016400091022     C                   ELSE
016500091022     C                   MOVE      a_vatccm      VATCCM
016600091022     C     VATCCM        IFLE      *ZEROS
016700091022     C                   SETON                                        31
016800091022     C                   Z-ADD     *zeros        VATCCM
016900091022     C                   EVAL      vinmsg = 'VATCCM'
017000091022     C                   ENDIF
017100091022     C                   ENDIF
017200091022     C*
017300091022     C                   TESTN                   a_vatlnp             30
017400091022     C  N30              TESTN                   a_vatlnp               30
017500091022     C  N30              TESTN                   a_vatlnp                 30
017600091022     C                   MOVE      a_vatlnp      W001A             1
017700091022     C     *IN30         IFEQ      *OFF
017800091022     C     W001A         ORLT      '0'
017900091022     C                   SETON                                        31
018000091022     C                   Z-ADD     *zeros        VATLNP
018100091022     C                   EVAL      vinmsg = %trimr(vinmsg)
018200091022     C                             + ' ' + 'VATLNP'
018300091022     C                   ELSE
018400091022     C                   MOVE      a_vatlnp      VATLNP
018500091022     C     VATLNP        IFLE      *ZEROS
018600091022     C                   SETON                                        31
018700091022     C                   Z-ADD     *zeros        VATLNP
018800091022     C                   EVAL      vinmsg = %trimr(vinmsg)
018900091022     C                             + ' ' + 'VATLNP'
019000091022     C                   ENDIF
019100091022     C                   ENDIF
019200091022     C*
019300091022     C                   TESTN                   a_vataas             30
019400091022     C  N30              TESTN                   a_vataas               30
019500091022     C  N30              TESTN                   a_vataas                 30
019600091022     C                   MOVE      a_vataas      W001A             1
019700091022     C     *IN30         IFEQ      *OFF
019800091022     C     W001A         ORLT      '0'
019900091022     C                   SETON                                        31
020000091022     C                   MOVEL(P)  datcor        VATAAS
020100091022     C                   EVAL      vinmsg = %trimr(vinmsg)
020200091022     C                             + ' ' + 'VATAAS'
020300091022     C                   ELSE
020400091022     C                   MOVE      a_vataas      VATAAS
020500091022     C                   ENDIF
020600091022     C*
020700091022     C                   TESTN                   a_vatnrs             30
020800091022     C  N30              TESTN                   a_vatnrs               30
020900091022     C  N30              TESTN                   a_vatnrs                 30
021000091022     C                   MOVE      a_vatnrs      W001A             1
021100091022     C     *IN30         IFEQ      *OFF
021200091022     C     W001A         ORLT      '0'
021300091022     C     W001A         ANDNE     *BLANK
021400091022     C                   SETON                                        31
021500091022     C                   Z-ADD     *zeros        VATNRS
021600091022     C                   EVAL      vinmsg = %trimr(vinmsg)
021700091022     C                             + ' ' + 'VATNRS'
021800091022     C                   ELSE
021900091022     C                   MOVE      a_vatnrs      VATNRS
022000091022     C                   ENDIF
022100091022     C*
022200091022     C                   TESTN                   a_vatnsp             30
022300091022     C  N30              TESTN                   a_vatnsp               30
022400091022     C  N30              TESTN                   a_vatnsp                 30
022500091022     C                   MOVE      a_vatnsp      W001A             1
022600091022     C     *IN30         IFEQ      *OFF
022700091022     C     W001A         ORLT      '0'
022800091022     C                   SETON                                        31
022900091022     C                   Z-ADD     *zeros        VATNSP
023000091022     C                   EVAL      vinmsg = %trimr(vinmsg)
023100091022     C                             + ' ' + 'VATNSP'
023200091022     C                   ELSE
023300091022     C                   MOVE      a_vatnsp      VATNSP
023400091022     C                   ENDIF
023500091022     C*
023600091022     C*
023700091022     C* ALFABETICI
023800091022     C                   MOVE      a_vatatb      VATATB
023900091022     C                   MOVE      a_vattrc      VATTRC
024000091022     C                   EVAL      VATNOT = %trim(a_vatnot)
024100091022     C*
024200091022     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAT)
024300110204     C                   EVAL      VATCMR = %char(vatCCM)
024400091022     C                   EVAL      VATCNT = 1
024500091022     C*
024600091022     C* Determino la filiale d gestione...
024700091022     C                   if        vlrpoi = 999
024800091022     C                   MOVE(P)   vatLNP        VATFGS
024900091022     C                   else
025000091022     C                   MOVE(P)   vlrpoi        VATFGS
025100091022     C                   endif
025200091022     C*
025300000801     C* Ebbene...
025400091022     C   32              ADD       1             §CTRMO
025500091022     C                   IF        *IN31 = *on
025600091012     C                   ADD       1             §CTRNO
025700000801     C                   EVAL      vinflg = '2'
025800091012     C                   ELSE
025900091012     C                   ADD       1             §CTROKVT
026000091012     C                   ENDIF
026100000801     C*
026200000801     C                   ENDSR
026300070411
026400091012
026500091012
026600070411     C     *pssr         BEGSR
026700070411     C*
026800070411     C                   if        %open(tivin00r)
026900070411     C                   close     tivin00r
027000070411     C                   endif
027100070411     C*
027200070411     C                   eval      wrkesito = '2'
027300070411     C*
027400070411     C                   seton                                        LR
027500070411     C*
027600070411     C                   ENDSR     '*CANCL'
027700070411     C***
027800070411
027900990910
028000000613     C     *inzsr        BEGSR
028100990910     C*
028200990910     C     *entry        plist
028300990920     C                   parm                    tivlrds
028400990921     C                   parm      wrkesito      esito
028500000724     C                   parm                    prmlit
028600000710     C                   parm                    prmfir
028700091012     C*
028800091012     C* Definizione chiavi
028900091014     C* TITV1S41L - Parziale
029000091014     C     KEY1s4_1_P    klist
029100091012     C                   kfld                    wTV1S4KEY
029200091012     C                   kfld                    VATAAS
029300091012     C                   kfld                    VATLNP
029400091012     C                   kfld                    VATNRS
029500091012     C                   kfld                    VATNSP
029600091012     C* TITV1S42L - Completa
029700091012     C     KEY1s4_2      klist
029800091012     C                   kfld                    wTV1S4KEY
029900091012     C                   kfld                    VATAAS
030000091012     C                   kfld                    VATLNP
030100091012     C                   kfld                    VATNRS
030200091012     C                   kfld                    VATNSP
030300091012     C                   kfld                    VATTRC
030400091012     C                   kfld                    VATNOT
030500091022     C*
030600091022     C* CALCOLA LA DATA CORRENTE
030700091022     C                   time                    wn14             14 0
030800091022     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
030900091022     C                   movel     wn14          oracor            6 0          *ORA
031000091110     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
031100091110     C                   eval      datcor = %dec(%date() : *ISO)
031200000830     C*
031300000613     C                   ENDSR
031400000613     C***
