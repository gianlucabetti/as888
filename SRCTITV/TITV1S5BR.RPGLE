000100091022      /TITLE Upload via Internet: traduzione in EDIVABWR/EDIVATWR.
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
001500091022     D fnvabaar      e ds
001600990920     D tivlrds       e ds                  extname(tivlr00f)
001700990910     D esito           s              1
001800000724     D prmlit          s             10
001900000710     D prmfir          s             10
002000990921     D wrkesito        s                   like(esito)
002100091012     D rrnum           s              6  0 INZ(*zeros)
002200091022     D edivab_IN     e ds                  extname(edivab0f)
002300091022     D edivab_O_VAT  e ds                  extname(edivab0f) prefix(VAT_)
002400091022     D edivab_O_VAB  e ds                  extname(edivab0f) prefix(VAB_)
002500091012     D wTV1S4KEY       s                   like(VAB_TV1S4KEY)
002600091022
002700091022
002800091022     D TxtInOut        S           2048
002900091022     D ElencoChar      S            256
003000091022     D TipoElenco      S              1
003100091022     D CharSost        S              1
003200091022     D UpperCase       S              1
003300091022     D ChkNull         S              1
003400091022     D CharNull        S              1
003500091022     D EsitoBon        S              1
003600091022
003700091022
003800091022     D*------------------
003900091022     D* LINKING A DEFINIZIONI ESTERNE
004000091022     D*------------------
004100091022     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
004200091022     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
004300091022
004400091006
004500091022     D*------------------
004600091022     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
004700091022     D*------------------
004800091022     D* 10+3
004900091022     D                 DS
005000091022     D  WS15                   1      1
005100091022     D  WI15                   2     11
005200091022     D  WV15                  12     12
005300091022     D  WD15                  13     15
005400091022     D  WASC15                 1     15
005500091022     D*  6+1
005600091022     D                 DS
005700091022     D  WS09                   1      1
005800091022     D  WI09                   2      7
005900091022     D  WV09                   8      8
006000091022     D  WD09                   9      9
006100091022     D  WASC09                 1      9
006200091022     D*  2+3
006300091022     D                 DS
006400091022     D  WS07                   1      1
006500091022     D  WI07                   2      3
006600091022     D  WV07                   4      4
006700091022     D  WD07                   5      7
006800091022     D  WASC07                 1      7
006900010201
007000091022
007100091022
007200000913     C                   reset                   rrnum
007300990921     C                   reset                   esito
007400990921     C                   reset                   wrkesito
007500091110     C*
007600040526     C                   EXSR      RWFILE                                       LETT/SCR. VAB
007700000616     C*
007800010201     C                   seton                                        LR
007900990908
008000000801
008100910830     C*--------------------------------------------------------
008200091022     C* RWFILE   LEGGE tivin00r E SCRIVE EDIVABWR e EDIVATWR   *
008300910830     C*--------------------------------------------------------
008400040526     C     RWFILE        BEGSR
008500990910     C*
008600990914     C                   if        not %open(tivin00r)
008700990908     C                   open      tivin00r
008800990914     C                   endif
008900091012     C*
009000091012     C                   clear                   §CTROKVB          5 0
009100091012     C                   clear                   §CTROKVT          5 0
009200091012     C                   clear                   §CTRMO            5 0
009300091012     C                   clear                   §CTRNO            5 0
009400990910     C*
009500921023     C                   DO        *HIVAL
009600990913     C*
009700990915     C                   READ      tivin00r                               70
009800050627     C                   if        vindta > *blanks
009900000613     C                   add       1             rrnum
010000000801     C*
010100000801     C                   if        *in70 = *off
010200000801     C                             and
010300000801     C                             (vinflg = *blanks
010400000801     C                              or vinflg = '0'
010500000801     C                              or vinflg = '2')
010600000801     C*
010700000801     C                   clear                   vinmsg
010800000801     C                   eval      vinflg = '1'
010900010305     C*
011000091022     C                   clear                   edivab_in
011100050628     C                   exsr      impvab                                       => carico VAB
011200060725     C                   exsr      wrivab                                       => scarico VAB
011300000905     C*
011400000905     C                   else
011500000905     C                   eval      vinflg = '1'
011600050628     C                   endif
011700000905     C                   endif
011800000905     C*
011900000905     C  N70              update    tivin000
012000000905     C*
012100991022     C  N70              ENDdo
012200990910
012300091012
012400990910     C* Se non ci sono record con errori ...
012500000710     C                   if        §ctrno = 0
012600990910     C* ... restituisco esito OK.
012700990921     C                   eval      wrkesito = '0'
012800990910     C                   else
012900010201     C                   if        §ctrokvb > 0
013000990921     C                   eval      wrkesito = '1'
013100000710     C                   else
013200000710     C                   eval      wrkesito = '2'
013300990910     C                   endif
013400000710     C                   endif
013500990910     C*
013600990914     C                   if        %open(tivin00r)
013700990908     C                   close     tivin00r
013800990914     C                   endif
013900990920     C*
014000910830     C                   ENDSR
014100000613     C***
014200091110
014300091110     C*----------------------------------------------------*
014400091110     C*  IMPOSTAZIONE CAMPI COSTANTI
014500091110     C*----------------------------------------------------*
014600091110     C     DEFCAM        BEGSR
014700091110     C*
014800091110     C* Imposto i valori di default...
014900091110     C
015000091110     C
015100091110     C
015200091110     C* ... e poi verifico se sono stati passati come parametri
015300091110     C                   IF        vlrppt > *blanks
015400091110     C                   IF        %subst(vlrppt:8:3) <> *blanks
015500091110     C                   EVAL      PiStr=%trim(%subst(vlrppt:8:3))
015600091110     C                   EXSR      CHKNUM
015700091110     C                   IF        PiInt=*on
015800091110     C                   Z-ADD     PiVal         VABLNP
015900091110     C                   ENDIF
016000091110     C                   ENDIF
016100091110     C                   IF        %subst(vlrppt:11:3) <> *blanks
016200091110     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
016300091110     C                   EXSR      CHKNUM
016400091110     C                   IF        PiInt=*on
016500091110     C                   Z-ADD     PiVal         VABCTR
016600091110     C                   ENDIF
016700091110     C                   ENDIF
016800091110     C                   IF        %subst(vlrppt:14:7) = '*SOLOVB'
016900091110     C                   SETON                                        50
017000091110     C                   ELSE
017100091110     C                   SETOFF                                       50
017200091110     C                   ENDIF
017300091110     C                   ENDIF
017400091110     C*
017500091110     C                   ENDSR
017600010305
017700010305     C*----------------------------------------------------*
017800091012     C*  SCARICAMENTO BUFFER RECORDS VAB
017900010305     C*----------------------------------------------------*
018000020305     C     WRIVAB        BEGSR
018100091012     C*
018200091022     C                   EVAL      wTV1S4KEY  = '02101177_BOLLE'
018300091012     C*
018400091022     C* Verifico se già esistono i record EDIVAT correlati
018500091014     C     KEY1s4_P      SETLL     TITV1S42L
018600091012     C*
018700091012     C                   IF        %equal(TITV1S42L)
018800091014     C     KEY1s4_P      READE     TITV1S42L
018900091012     C                   DOW       not %eof(TITV1S42L)
019000091022     C                   EVAL      edivab_o_vat = edivab_in
019100091012     C                   EVAL      VAT_TV1S4KEY  = wTV1S4KEY
019200091012     C                   EVAL      VAT_TV1S4VAB  = 'X'
019300091110     C   50              EVAL      VAT_TV1S4VAT  = 'X'
019400091012     C                   UPDATE    TITV1S402
019500091014     C     KEY1s4_P      READE     TITV1S42L
019600091012     C                   ENDDO
019700091012     C*
019800091012     C                   ELSE
019900091012     C*
020000091014     C     KEY1s4_P      CHAIN     TITV1S41L
020100091012     C*
020200091012     C                   IF        %found(TITV1S41L)
020300091022     C                   EVAL      edivab_o_vab = edivab_in
020400091012     C                   EVAL      VAB_TV1S4KEY  = wTV1S4KEY
020500091012     C                   EVAL      VAB_TV1S4VAB  = 'X'
020600091110     C   50              EVAL      VAB_TV1S4VAT  = 'X'
020700091012     C                   UPDATE    TITV1S401
020800091012     C*
020900091012     C                   ELSE
021000091012     C*
021100091022     C                   EVAL      edivab_o_vab = edivab_in
021200091012     C                   EVAL      VAB_TV1S4KEY  = wTV1S4KEY
021300091012     C                   EVAL      VAB_TV1S4VAB  = 'X'
021400091110     C   50              EVAL      VAB_TV1S4VAT  = 'X'
021500091012     C                   WRITE     TITV1S401
021600091012     C                   ENDIF
021700091012     C*
021800091012     C                   ENDIF
021900010305     C*
022000010305     C                   ENDSR
022100990920
022200000801     C*----------------------------------------------------*
022300091022     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X EDIVAB)
022400000801     C*----------------------------------------------------*
022500040823     C     IMPVAB        BEGSR
022600091110     C*
022700091110     C                   EXSR      DEFCAM
022800091022     C*
022900091022     C                   eval      fnvabaar = vindta
023000091022     C*
023100091022     C                   testb     x'3F'         vhbatb                   02
023200091022     C                   if        not *in02
023300091022     C*
023400091022     C                   SETOFF                                       303132
023500091022     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
023600091022     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
023700091022     C*    OPPURE TUTTI BLANKS
023800091022     C*
023900091022     C                   TESTN                   VHBLNP               30
024000091022     C  N30              TESTN                   VHBLNP                 30
024100091022     C  N30              TESTN                   VHBLNP                   30
024200091022     C                   MOVE      VHBLNP        W001A             1
024300091022     C     *IN30         IFEQ      *OFF
024400091022     C     W001A         ORLT      '0'
024500091022     C                   SETON                                        31
024600091022     C                   eval      vinmsg = %trimr(vinmsg)
024700091022     C                             + ' ' + 'VHBLNP'
024800091022     C                   ELSE
024900091022     C                   MOVE      VHBLNP        VABLNP
025000091022     C     VABLNP        IFLE      *ZEROS
025100091022     C                   SETON                                        31
025200091022     C                   eval      vinmsg = %trimr(vinmsg)
025300091022     C                             + ' ' + 'VHBLNP'
025400091022     C                   ENDIF
025500091022     C                   ENDIF
025600091022     C*
025700091022     C                   TESTN                   VHBCCM               30
025800091022     C  N30              TESTN                   VHBCCM                 30
025900091022     C  N30              TESTN                   VHBCCM                   30
026000091022     C                   MOVE      VHBCCM        W001A             1
026100091022     C     *IN30         IFEQ      *OFF
026200091022     C     W001A         ORLT      '0'
026300091022     C                   SETON                                        31
026400091022     C                   eval      vinmsg = %trimr(vinmsg)
026500091022     C                             + ' ' + 'VHBCCM'
026600091022     C                   ELSE
026700091022     C                   MOVE      VHBCCM        VABCCM
026800091022     C     VABCCM        IFLE      *ZEROS
026900091022     C                   SETON                                        31
027000091022     C                   eval      vinmsg = %trimr(vinmsg)
027100091022     C                             + ' ' + 'VHBCCM'
027200091022     C                   ENDIF
027300091022     C                   ENDIF
027400091022     C*
027500091022     C                   TESTN                   VHBAAS               30
027600091022     C  N30              TESTN                   VHBAAS                 30
027700091022     C  N30              TESTN                   VHBAAS                   30
027800091022     C                   MOVE      VHBAAS        W001A             1
027900091022     C     *IN30         IFEQ      *OFF
028000091022     C     W001A         ORLT      '0'
028100091022     C     W001A         ANDNE     *BLANK
028200091022     C                   SETON                                        32
028300091022     C                   MOVEL     datcor        VABAAS
028400091022     C                   eval      vinmsg = %trimr(vinmsg)
028500091022     C                             + ' ' + 'VHBAAS'
028600091022     C                   ELSE
028700091022     C                   MOVE      VHBAAS        VABAAS
028800091022     C                   ENDIF
028900091022     C*
029000091022     C                   TESTN                   VHBMGS               30
029100091022     C  N30              TESTN                   VHBMGS                 30
029200091022     C  N30              TESTN                   VHBMGS                   30
029300091022     C                   MOVE      VHBMGS        W001A             1
029400091022     C     *IN30         IFEQ      *OFF
029500091022     C     W001A         ORLT      '0'
029600091022     C     W001A         ANDNE     *BLANK
029700091022     C                   SETON                                        32
029800091022     C                   MOVE      '0101'        VABMGS
029900091022     C                   eval      vinmsg = %trimr(vinmsg)
030000091022     C                             + ' ' + 'VHBMGS'
030100091022     C                   ELSE
030200091022     C                   MOVE      VHBMGS        VABMGS
030300091022     C                   ENDIF
030400091022     C*
030500091022     C                   TESTN                   VHBNRS               30
030600091022     C  N30              TESTN                   VHBNRS                 30
030700091022     C  N30              TESTN                   VHBNRS                   30
030800091022     C                   MOVE      VHBNRS        W001A             1
030900091022     C     *IN30         IFEQ      *OFF
031000091022     C     W001A         ORLT      '0'
031100091022     C     W001A         ANDNE     *BLANK
031200091022     C                   SETON                                        32
031300091022     C                   MOVE      *ZERO         VABNRS
031400091022     C                   eval      vinmsg = %trimr(vinmsg)
031500091022     C                             + ' ' + 'VHBNRS'
031600091022     C                   ELSE
031700091022     C                   MOVE      VHBNRS        VABNRS
031800091022     C                   ENDIF
031900091022     C*
032000091022     C                   TESTN                   VHBNSP               30
032100091022     C  N30              TESTN                   VHBNSP                 30
032200091022     C  N30              TESTN                   VHBNSP                   30
032300091022     C                   MOVE      VHBNSP        W001A             1
032400091022     C     *IN30         IFEQ      *OFF
032500091022     C     W001A         ORLT      '0'
032600091022     C     W001A         ANDNE     *BLANK
032700091022     C                   SETON                                        32
032800091022     C                   MOVE      *ZERO         VABNSP
032900091022     C                   eval      vinmsg = %trimr(vinmsg)
033000091022     C                             + ' ' + 'VHBNSP'
033100091022     C                   ELSE
033200091022     C                   MOVE      VHBNSP        VABNSP
033300091022     C                   ENDIF
033400091022     C*
033500091022     C                   TESTN                   VHBLNA               30
033600091022     C  N30              TESTN                   VHBLNA                 30
033700091022     C  N30              TESTN                   VHBLNA                   30
033800091022     C                   MOVE      VHBLNA        W001A             1
033900091022     C     *IN30         IFEQ      *OFF
034000091022     C     W001A         ORLT      '0'
034100091022     C     W001A         ANDNE     *BLANK
034200091022     C                   SETON                                        32
034300091022     C                   MOVE      *ZERO         VABLNA
034400091022     C                   eval      vinmsg = %trimr(vinmsg)
034500091022     C                             + ' ' + 'VHBLNA'
034600091022     C                   ELSE
034700091022     C                   MOVE      VHBLNA        VABLNA
034800091022     C                   ENDIF
034900091022     C*
035000091022     C                   TESTN                   VHBCTR               30
035100091022     C  N30              TESTN                   VHBCTR                 30
035200091022     C  N30              TESTN                   VHBCTR                   30
035300091022     C                   MOVE      VHBCTR        W001A             1
035400091022     C     *IN30         IFEQ      *OFF
035500091022     C     W001A         ORLT      '0'
035600091022     C     W001A         ANDNE     *BLANK
035700091022     C                   SETON                                        32
035800091022     C                   MOVE      *ZERO         VABCTR
035900091022     C                   eval      vinmsg = %trimr(vinmsg)
036000091022     C                             + ' ' + 'VHBCTR'
036100091022     C                   ELSE
036200091022     C                   MOVE      VHBCTR        VABCTR
036300091022     C                   ENDIF
036400091022     C*
036500091022     C                   MOVE      *ZERO         VABIAS
036600091022     C                   MOVE      VHBIAS        WASC15
036700091022     C                   TESTN                   WI15                 30
036800091022     C  N30              TESTN                   WI15                   30
036900091022     C  N30              TESTN                   WI15                     30
037000091022     C                   MOVE      WI15          W001A             1
037100091022     C     *IN30         IFEQ      *OFF
037200091022     C     W001A         ORLT      '0'
037300091022     C     W001A         ANDNE     *BLANK
037400091022     C                   SETON                                        32
037500091022     C                   MOVE      *ZERO         VABIAS
037600091022     C                   eval      vinmsg = %trimr(vinmsg)
037700091022     C                             + ' ' + 'VHBIAS'
037800091022     C                   ELSE
037900091022     C                   MOVEL     WI15          VABIAS
038000091022     C                   TESTN                   WD15                 30
038100091022     C  N30              TESTN                   WD15                   30
038200091022     C  N30              TESTN                   WD15                     30
038300091022     C                   MOVE      WD15          W001A             1
038400091022     C     *IN30         IFEQ      *OFF
038500091022     C     W001A         ORLT      '0'
038600091022     C     W001A         ANDNE     *BLANK
038700091022     C                   SETON                                        32
038800091022     C                   MOVE      *ZERO         VABIAS
038900091022     C                   eval      vinmsg = %trimr(vinmsg)
039000091022     C                             + ' ' + 'VHBIAS'
039100091022     C                   ELSE
039200091022     C                   MOVE      WD15          VABIAS
039300091022     C                   ENDIF
039400091022     C                   ENDIF
039500091022     C*
039600091022     C                   TESTN                   VHBNCL               30
039700091022     C  N30              TESTN                   VHBNCL                 30
039800091022     C  N30              TESTN                   VHBNCL                   30
039900091022     C                   MOVE      VHBNCL        W001A             1
040000091022     C     *IN30         IFEQ      *OFF
040100091022     C     W001A         ORLT      '0'
040200091022     C                   SETON                                        32
040300091022     C                   MOVE      *ZERO         VABNCL
040400091022     C                   eval      vinmsg = %trimr(vinmsg)
040500091022     C                             + ' ' + 'VHBNCL'
040600091022     C                   ELSE
040700091022     C                   MOVE      VHBNCL        VABNCL
040800091022     C                   ENDIF
040900091022     C*
041000091022     C                   MOVE      *ZERO         VABPKB
041100091022     C                   MOVE      VHBPKB        WASC09
041200091022     C                   IF        WI09 = *blanks
041300091022     C                   EVAL      WI09 = *zeros
041400091022     C                   ENDIF
041500091022     C                   TESTN                   WI09                 30
041600091022     C  N30              TESTN                   WI09                   30
041700091022     C  N30              TESTN                   WI09                     30
041800091022     C                   MOVE      WI09          W001A             1
041900091022     C     *IN30         IFEQ      *OFF
042000091022     C     W001A         ORLT      '0'
042100091022     C                   SETON                                        32
042200091022     C                   MOVE      *ZERO         VABPKB
042300091022     C                   eval      vinmsg = %trimr(vinmsg)
042400091022     C                             + ' ' + 'VHBPKB'
042500091022     C                   ELSE
042600091022     C                   MOVEL     WI09          VABPKB
042700091022     C                   IF        WD09 = *blanks
042800091022     C                   EVAL      WD09 = *zeros
042900091022     C                   ENDIF
043000091022     C                   TESTN                   WD09                 30
043100091022     C  N30              TESTN                   WD09                     30
043200091022     C                   MOVE      WD09          W001A             1
043300091022     C     *IN30         IFEQ      *OFF
043400091022     C     W001A         ORLT      '0'
043500091022     C     W001A         ANDNE     *BLANK
043600091022     C                   SETON                                        32
043700091022     C                   MOVE      *ZERO         VABPKB
043800091022     C                   eval      vinmsg = %trimr(vinmsg)
043900091022     C                             + ' ' + 'VHBPKB'
044000091022     C                   ELSE
044100091022     C                   MOVE      WD09          VABPKB
044200091022     C                   ENDIF
044300091022     C                   ENDIF
044400091022     C*
044500091022     C                   MOVE      *ZERO         VABVLB
044600091022     C                   MOVE      VHBVLB        WASC07
044700091022     C                   IF        WI07 = *blanks
044800091022     C                   EVAL      WI07 = *zeros
044900091022     C                   ENDIF
045000091022     C                   TESTN                   WI07                 30
045100091022     C  N30              TESTN                   WI07                   30
045200091022     C  N30              TESTN                   WI07                     30
045300091022     C                   MOVE      WI07          W001A             1
045400091022     C     *IN30         IFEQ      *OFF
045500091022     C     W001A         ORLT      '0'
045600091022     C     W001A         ANDNE     *BLANK
045700091022     C                   SETON                                        32
045800091022     C                   MOVE      *ZERO         VABVLB
045900091022     C                   eval      vinmsg = %trimr(vinmsg)
046000091022     C                             + ' ' + 'VHBVLB'
046100091022     C                   ELSE
046200091022     C                   MOVEL     WI07          VABVLB
046300091022     C                   IF        WD07 = *blanks
046400091022     C                   EVAL      WD07 = *zeros
046500091022     C                   ENDIF
046600091022     C                   TESTN                   WD07                 30
046700091022     C  N30              TESTN                   WD07                   30
046800091022     C  N30              TESTN                   WD07                     30
046900091022     C                   MOVE      WD07          W001A             1
047000091022     C     *IN30         IFEQ      *OFF
047100091022     C     W001A         ORLT      '0'
047200091022     C     W001A         ANDNE     *BLANK
047300091022     C                   SETON                                        32
047400091022     C                   MOVE      *ZERO         VABVLB
047500091022     C                   eval      vinmsg = %trimr(vinmsg)
047600091022     C                             + ' ' + 'VHBVLB'
047700091022     C                   ELSE
047800091022     C                   MOVE      WD07          VABVLB
047900091022     C                   ENDIF
048000091022     C                   ENDIF
048100091022     C*
048200091022     C                   MOVE      *ZERO         VABQFT
048300091022     C                   MOVE      VHBQFT        WASC15
048400091022     C                   IF        WI15 = *blanks
048500091022     C                   EVAL      WI15 = *zeros
048600091022     C                   ENDIF
048700091022     C                   TESTN                   WI15                 30
048800091022     C  N30              TESTN                   WI15                   30
048900091022     C  N30              TESTN                   WI15                     30
049000091022     C                   MOVE      WI15          W001A             1
049100091022     C     *IN30         IFEQ      *OFF
049200091022     C     W001A         ORLT      '0'
049300091022     C     W001A         ANDNE     *BLANK
049400091022     C                   SETON                                        32
049500091022     C                   MOVE      *ZERO         VABQFT
049600091022     C                   eval      vinmsg = %trimr(vinmsg)
049700091022     C                             + ' ' + 'VHBQFT'
049800091022     C                   ELSE
049900091022     C                   MOVEL     WI15          VABQFT
050000091022     C                   IF        WD15 = *blanks
050100091022     C                   EVAL      WD15 = *zeros
050200091022     C                   ENDIF
050300091022     C                   TESTN                   WD15                 30
050400091022     C  N30              TESTN                   WD15                   30
050500091022     C  N30              TESTN                   WD15                     30
050600091022     C                   MOVE      WD15          W001A             1
050700091022     C     *IN30         IFEQ      *OFF
050800091022     C     W001A         ORLT      '0'
050900091022     C     W001A         ANDNE     *BLANK
051000091022     C                   SETON                                        32
051100091022     C                   MOVE      *ZERO         VABQFT
051200091022     C                   eval      vinmsg = %trimr(vinmsg)
051300091022     C                             + ' ' + 'VHBQFT'
051400091022     C                   ELSE
051500091022     C                   MOVE      WD15          VABQFT
051600091022     C                   ENDIF
051700091022     C                   ENDIF
051800091022     C*
051900091022     C                   MOVE      *ZERO         VABCAS
052000091022     C                   MOVEL(P)  VHBCAS        PiStr
052100091022     C                   EXSR      CHKNUM
052200091022     C                   IF        PiNum=*on
052300091022     C                   Z-ADD     PiVal         VABCAS
052400091022     C                   ELSE
052500091022     C                   SETON                                        32
052600091022     C                   Z-ADD     *zeros        VABCAS
052700091022     C                   EVAL      vinmsg = %trimr(vinmsg)
052800091022     C                             + ' ' + 'VHBCAS'
052900091022     C                   ENDIF
053000091022     C*
053100091022     C                   TESTN                   VHBRMN               30
053200091022     C  N30              TESTN                   VHBRMN                 30
053300091022     C  N30              TESTN                   VHBRMN                   30
053400091022     C                   MOVE      VHBRMN        W001A             1
053500091022     C     *IN30         IFEQ      *OFF
053600091022     C     W001A         ORLT      '0'
053700091022     C                   SETON                                        32
053800091022     C                   MOVE      *ZERO         VABRMN
053900091022     C                   eval      vinmsg = %trimr(vinmsg)
054000091022     C                             + ' ' + 'VHBRMN'
054100091022     C                   ELSE
054200091022     C                   MOVE      VHBRMN        VABRMN
054300091022     C                   ENDIF
054400091022     C*
054500091022     C                   TESTN                   VHBNCD               30
054600091022     C  N30              TESTN                   VHBNCD                 30
054700091022     C  N30              TESTN                   VHBNCD                   30
054800091022     C                   MOVE      VHBNCD        W001A             1
054900091022     C     *IN30         IFEQ      *OFF
055000091022     C     W001A         ORLT      '0'
055100091022     C     W001A         ANDNE     *BLANK
055200091022     C                   SETON                                        32
055300091022     C                   MOVE      *ZERO         VABNCD
055400091022     C                   eval      vinmsg = %trimr(vinmsg)
055500091022     C                             + ' ' + 'VHBNCD'
055600091022     C                   ELSE
055700091022     C                   MOVE      VHBNCD        VABNCD
055800091022     C                   ENDIF
055900091022     C*
056000091022     C                   TESTN                   VHBNCA               30
056100091022     C  N30              TESTN                   VHBNCA                 30
056200091022     C  N30              TESTN                   VHBNCA                   30
056300091022     C                   MOVE      VHBNCA        W001A             1
056400091022     C     *IN30         IFEQ      *OFF
056500091022     C     W001A         ORLT      '0'
056600091022     C     W001A         ANDNE     *BLANK
056700091022     C                   SETON                                        32
056800091022     C                   MOVE      *ZERO         VABNCA
056900091022     C                   eval      vinmsg = %trimr(vinmsg)
057000091022     C                             + ' ' + 'VHBNCA'
057100091022     C                   ELSE
057200091022     C                   MOVE      VHBNCA        VABNCA
057300091022     C                   ENDIF
057400091022     C*
057500091022     C                   TESTN                   VHBZNC               30
057600091022     C  N30              TESTN                   VHBZNC                 30
057700091022     C  N30              TESTN                   VHBZNC                   30
057800091022     C                   MOVE      VHBZNC        W001A             1
057900091022     C     *IN30         IFEQ      *OFF
058000091022     C     W001A         ORLT      '0'
058100091022     C     W001A         ANDNE     *BLANK
058200091022     C                   SETON                                        32
058300091022     C                   MOVE      *ZERO         VABZNC
058400091022     C                   eval      vinmsg = %trimr(vinmsg)
058500091022     C                             + ' ' + 'VHBZNC'
058600091022     C                   ELSE
058700091022     C                   MOVE      VHBZNC        VABZNC
058800091022     C                   ENDIF
058900091022     C*
059000091022     C                   TESTN                   VHBDCR               30
059100091022     C  N30              TESTN                   VHBDCR                 30
059200091022     C  N30              TESTN                   VHBDCR                   30
059300091022     C                   MOVE      VHBDCR        W001A             1
059400091022     C     *IN30         IFEQ      *OFF
059500091022     C     W001A         ORLT      '0'
059600091022     C     W001A         ANDNE     *BLANK
059700091022     C                   SETON                                        32
059800091022     C                   MOVE      *ZERO         VABDCR
059900091022     C                   eval      vinmsg = %trimr(vinmsg)
060000091022     C                             + ' ' + 'VHBDCR'
060100091022     C                   ELSE
060200091022     C                   MOVE      VHBDCR        VABDCR
060300091022     C                   ENDIF
060400091022     C*
060500091022     C                   TESTN                   VHBHCR               30
060600091022     C  N30              TESTN                   VHBHCR                 30
060700091022     C  N30              TESTN                   VHBHCR                   30
060800091022     C                   MOVE      VHBHCR        W001A             1
060900091022     C     *IN30         IFEQ      *OFF
061000091022     C     W001A         ORLT      '0'
061100091022     C     W001A         ANDNE     *BLANK
061200091022     C                   SETON                                        32
061300091022     C                   MOVE      *ZERO         VABHCR
061400091022     C                   eval      vinmsg = %trimr(vinmsg)
061500091022     C                             + ' ' + 'VHBHCR'
061600091022     C                   ELSE
061700091022     C                   MOVE      VHBHCR        VABHCR
061800091022     C                   ENDIF
061900091022     C*
062000091022     C                   MOVE      *ZERO         VABVMD
062100091022     C                   MOVE      VHBVMD        WASC15
062200091022     C                   IF        WI15 = *blanks
062300091022     C                   EVAL      WI15 = *zeros
062400091022     C                   ENDIF
062500091022     C                   TESTN                   WI15                 30
062600091022     C  N30              TESTN                   WI15                   30
062700091022     C  N30              TESTN                   WI15                     30
062800091022     C                   MOVE      WI15          W001A             1
062900091022     C     *IN30         IFEQ      *OFF
063000091022     C     W001A         ORLT      '0'
063100091022     C     W001A         ANDNE     *BLANK
063200091022     C                   SETON                                        32
063300091022     C                   MOVE      *ZERO         VABVMD
063400091022     C                   eval      vinmsg = %trimr(vinmsg)
063500091022     C                             + ' ' + 'VHBVMD'
063600091022     C                   ELSE
063700091022     C                   MOVEL     WI15          VABVMD
063800091022     C                   IF        WD15 = *blanks
063900091022     C                   EVAL      WD15 = *zeros
064000091022     C                   ENDIF
064100091022     C                   TESTN                   WD15                 30
064200091022     C  N30              TESTN                   WD15                   30
064300091022     C  N30              TESTN                   WD15                     30
064400091022     C                   MOVE      WD15          W001A             1
064500091022     C     *IN30         IFEQ      *OFF
064600091022     C     W001A         ORLT      '0'
064700091022     C     W001A         ANDNE     *BLANK
064800091022     C                   SETON                                        32
064900091022     C                   MOVE      *ZERO         VABVMD
065000091022     C                   eval      vinmsg = %trimr(vinmsg)
065100091022     C                             + ' ' + 'VHBVMD'
065200091022     C                   ELSE
065300091022     C                   MOVE      WD15          VABVMD
065400091022     C                   ENDIF
065500091022     C                   ENDIF
065600091022     C*
065700091022     C                   TESTN                   VHBANT               30
065800091022     C  N30              TESTN                   VHBANT                 30
065900091022     C  N30              TESTN                   VHBANT                   30
066000091022     C                   MOVE      VHBANT        W001A             1
066100091022     C     *IN30         IFEQ      *OFF
066200091022     C     W001A         ORLT      '0'
066300091022     C     W001A         ANDNE     *BLANK
066400091022     C                   SETON                                        32
066500091022     C                   MOVE      *ZERO         VABANT
066600091022     C                   eval      vinmsg = %trimr(vinmsg)
066700091022     C                             + ' ' + 'VHBANT'
066800091022     C                   ELSE
066900091022     C                   MOVE      VHBANT        VABANT
067000091022     C                   ENDIF
067100091022     C*
067200091022     C* CAMPI ALFABETICI:
067300091022     C                   MOVE      VHBATB        VABATB
067400091022     C                   MOVE      VHBCBO        VABCBO
067500091022     C                   MOVE      VHBRSD        VABRSD
067600091022     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
067700091022     C     '@':'A'       XLATE     VABRSD        VABRSD
067800091022     C* ==
067900091022     C                   MOVE      VHBRD2        VABRD2
068000091022     C                   MOVE      VHBIND        VABIND
068100091022     C                   MOVE      VHBCAD        VABCAD
068200091022     C                   MOVE      VHBLOD        VABLOD
068300091022     C                   MOVE      VHBPRD        VABPRD
068400091022     C                   MOVE      VHBNZD        VABNZD
068500091022     C                   MOVE      VHBGC1        VABGC1
068600091022     C                   MOVE      VHBGC2        VABGC2
068700091022     C                   MOVE      VHBTSP        VABTSP
068800091022     C                   MOVE      VHBVAS        VABVAS
068900091022     C                   MOVE      VHBNAS        VABNAS
069000091022     C                   MOVE      VHBTIC        VABTIC
069100091022     C                   MOVE      VHBVCA        VABVCA
069200091022     C                   MOVE      VHBGCA        VABGCA
069300091022     C                   MOVE      VHBRMA        VABRMA
069400091022     C                   MOVE      VHBXCO        VABXCO
069500091022     C                   MOVE      VHBNOT        VABNOT
069600091022     C                   MOVE      VHBNT2        VABNT2
069700091022     C                   MOVE      VHBCTM        VABCTM
069800091022     C                   MOVE      VHBFFD        VABFFD
069900091022     C                   MOVE      VHBTCR        VABTCR
070000091022     C                   MOVE      VHBCTS        VABCTS
070100091022     C                   MOVE      VHBFTM        VABFTM
070200091022     C                   MOVE      VHBVAD        VABVAD
070300091022     C                   MOVE      VHBGMA        VABGMA
070400091022     C                   MOVE      VHBGGA        VABGGA
070500091022     C                   MOVE      VHBGVA        VABGVA
070600091022     C                   MOVE      VHBTC1        VABTC1
070700091022     C                   MOVE      VHBTC2        VABTC2
070800091022     C                   MOVE      VHBSCL        VABSCL
070900091022     C                   MOVE      VHBRMO        VABRMO
071000091022     C                   MOVE      VHBCMO        VABCMO
071100091022     C                   MOVE      VHBNMO        VABNMO
071200091022     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
071300091022     C* NELLA NAZIONE MITTENTE ORIGINALE
071400091022     C                   IF        %trim(VHBNMO) < 'A'
071500091022     C                   MOVE      *BLANKS       VABNMO
071600091022     C                   ENDIF
071700091022     C*
071800091022     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
071900091022     C                   IF        %trim(VABNZD) = 'I'   OR
072000091022     C                             %trim(VABNZD) = 'IT'  OR
072100091022     C                             %trim(VABNZD) = 'ITA'
072200091022     C                   EVAL      VABNZD = *blanks
072300091022     C                   ENDIF
072400091022     C*
072500091022     C* "bonifico" i dati d input da eventuali caratteri indesiderati
072600091022     C                   exsr      exeBon
072700091022     C*
072800091022     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
072900110204     C                   EVAL      VABCMR = %char(vabCCM)
073000091022     C                   EVAL      VABDCM = DATCOR
073100091022     C                   EVAL      VABDTS = DATCOR
073200091022     C                   EVAL      VABHMS = ORACOR
073300091022     C                   EVAL      VABCNT = 1
073400091022     C*
073500091022     C* Determino la filiale d gestione...
073600091022     C                   if        vlrpoi = 999
073700091022     C                   MOVE(P)   vabLNP        VABFGS
073800091022     C                   else
073900091022     C                   MOVE(P)   vlrpoi        VABFGS
074000091022     C                   endif
074100091022     C*
074200091022     C                   endif
074300010202     C*
074400000801     C* Ebbene...
074500091022     C   32              ADD       1             §CTRMO
074600091022     C                   IF        *IN31 = *on
074700091012     C                   ADD       1             §CTRNO
074800000801     C                   EVAL      vinflg = '2'
074900091012     C                   ELSE
075000010201     C                   ADD       1             §CTROKVB
075100091012     C                   ENDIF
075200000801     C*
075300000801     C                   ENDSR
075400000801     C*----------------------------------------------------*
075500091022     C*  CONTROLLO NUMERICITA' CAMPI
075600000801     C*----------------------------------------------------*
075700091006     C     CHKNUM        BEGSR
075800091006     C*
075900091006     C                   IF        PiDecChr = *blanks
076000091006     C                   EVAL      PiDecChr = ','
076100091006     C                   ENDIF
076200091006     C*
076300091006     C                   callp(e)  UBISNUM_Check(PiStr
076400091006     C                                          :PiDecChr
076500091006     C                                          :PiVal
076600091006     C                                          :PiNum
076700091006     C                                          :PiInt)
076800091006     C*
076900091006     C                   IF        %error
077000091006     C                   EVAL      PiInt=*off
077100091006     C                   ENDIF
077200000801     C*
077300000801     C                   ENDSR
077400000801     C***
077500070411
077600091012
077700091012
077800070411     C     *pssr         BEGSR
077900070411     C*
078000070411     C                   if        %open(tivin00r)
078100070411     C                   close     tivin00r
078200070411     C                   endif
078300070411     C*
078400070411     C                   eval      wrkesito = '2'
078500070411     C*
078600070411     C                   seton                                        LR
078700070411     C*
078800070411     C                   ENDSR     '*CANCL'
078900070411     C***
079000091022
079100091022
079200091022
079300091022      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
079400091022     C     exeBon        BEGSR
079500091022     C*
079600091022     C* Imposto i parametri x il *pgm d "bonifica"
079700091022     C                   EVAL      ElencoChar = *blanks
079800091022     C                   EVAL      TipoElenco = *blanks
079900091022     C                   EVAL      CharSost   = *blanks
080000091022     C                   EVAL      UpperCase  = '1'
080100091022     C                   EVAL      ChkNull    = '1'
080200091022     C                   EVAL      CharNull   = *blanks
080300091022     C*
080400091022     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
080500091022     C                   EVAL      TxtInOut   = VABRSD
080600091022     C                   EVAL      EsitoBon   = *blanks
080700091022     C*
080800091022     C                   CALL      'XCHKCHAR'
080900091022     C                   PARM                    TxtInOut
081000091022     C                   PARM                    ElencoChar
081100091022     C                   PARM                    TipoElenco
081200091022     C                   PARM                    CharSost
081300091022     C                   PARM                    UpperCase
081400091022     C                   PARM                    ChkNull
081500091022     C                   PARM                    CharNull
081600091022     C                   PARM                    EsitoBon
081700091022     C*
081800091022     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
081900091022     C                   IF        EsitoBon = '1'
082000091022     C                   EVAL      VABRSD = TxtInOut
082100091022     C                   ENDIF
082200091022     C*
082300091022     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
082400091022     C                   EVAL      TxtInOut   = VABIND
082500091022     C                   EVAL      EsitoBon   = *blanks
082600091022     C*
082700091022     C                   CALL      'XCHKCHAR'
082800091022     C                   PARM                    TxtInOut
082900091022     C                   PARM                    ElencoChar
083000091022     C                   PARM                    TipoElenco
083100091022     C                   PARM                    CharSost
083200091022     C                   PARM                    UpperCase
083300091022     C                   PARM                    ChkNull
083400091022     C                   PARM                    CharNull
083500091022     C                   PARM                    EsitoBon
083600091022     C*
083700091022     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
083800091022     C                   IF        EsitoBon = '1'
083900091022     C                   EVAL      VABIND = TxtInOut
084000091022     C                   ENDIF
084100091022     C*
084200091022     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
084300091022     C                   EVAL      TxtInOut   = VABLOD
084400091022     C                   EVAL      EsitoBon   = *blanks
084500091022     C*
084600091022     C                   CALL      'XCHKCHAR'
084700091022     C                   PARM                    TxtInOut
084800091022     C                   PARM                    ElencoChar
084900091022     C                   PARM                    TipoElenco
085000091022     C                   PARM                    CharSost
085100091022     C                   PARM                    UpperCase
085200091022     C                   PARM                    ChkNull
085300091022     C                   PARM                    CharNull
085400091022     C                   PARM                    EsitoBon
085500091022     C*
085600091022     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
085700091022     C                   IF        EsitoBon = '1'
085800091022     C                   EVAL      VABLOD = TxtInOut
085900091022     C                   ENDIF
086000091022     C*
086100091022     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
086200091022     C                   EVAL      TxtInOut   = VABNOT
086300091022     C                   EVAL      EsitoBon   = *blanks
086400091022     C                   EVAL      UpperCase  = *blanks
086500091022     C*
086600091022     C                   CALL      'XCHKCHAR'
086700091022     C                   PARM                    TxtInOut
086800091022     C                   PARM                    ElencoChar
086900091022     C                   PARM                    TipoElenco
087000091022     C                   PARM                    CharSost
087100091022     C                   PARM                    UpperCase
087200091022     C                   PARM                    ChkNull
087300091022     C                   PARM                    CharNull
087400091022     C                   PARM                    EsitoBon
087500091022     C*
087600091022     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
087700091022     C                   IF        EsitoBon = '1'
087800091022     C                   EVAL      VABNOT = TxtInOut
087900091022     C                   ENDIF
088000091022     C*
088100091022     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
088200091022     C                   EVAL      TxtInOut   = VABNT2
088300091022     C                   EVAL      EsitoBon   = *blanks
088400091022     C                   EVAL      UpperCase  = *blanks
088500091022     C*
088600091022     C                   CALL      'XCHKCHAR'
088700091022     C                   PARM                    TxtInOut
088800091022     C                   PARM                    ElencoChar
088900091022     C                   PARM                    TipoElenco
089000091022     C                   PARM                    CharSost
089100091022     C                   PARM                    UpperCase
089200091022     C                   PARM                    ChkNull
089300091022     C                   PARM                    CharNull
089400091022     C                   PARM                    EsitoBon
089500091022     C*
089600091022     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
089700091022     C                   IF        EsitoBon = '1'
089800091022     C                   EVAL      VABNT2 = TxtInOut
089900091022     C                   ENDIF
090000091022     C*
090100091022     C                   ENDSR
090200070411
090300091022
090400990910
090500000613     C     *inzsr        BEGSR
090600990910     C*
090700990910     C     *entry        plist
090800990920     C                   parm                    tivlrds
090900990921     C                   parm      wrkesito      esito
091000000724     C                   parm                    prmlit
091100000710     C                   parm                    prmfir
091200091012     C*
091300091012     C* Definizione chiavi
091400091014     C* TITV1S4*L - Parziale
091500091014     C     KEY1s4_P      klist
091600091012     C                   kfld                    wTV1S4KEY
091700091012     C                   kfld                    VABAAS
091800091012     C                   kfld                    VABLNP
091900091012     C                   kfld                    VABNRS
092000091012     C                   kfld                    VABNSP
092100000613     C*
092200000830     C* CALCOLA LA DATA CORRENTE
092300000830     C                   time                    wn14             14 0
092400000830     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
092500091022     C                   movel     wn14          oracor            6 0          *ORA
092600091110     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
092700091110     C                   eval      datcor = %dec(%date() : *ISO)
092800000830     C*
092900000613     C                   ENDSR
093000000613     C***
