000100020913      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200130410      *
000300130410      * PARTICOLARITA':
000400130410     C* questo traduttore è la copia di TIS7T5R1 (standard per VAB) con la pulizia di VABNOT e NT2
000500130410     C* perché le note che vengono valorizzate dal cliente per errore
000600130410      *
000700121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000800990908
000900990910     Ftivin00r  uF   E             DISK    usropn
001000020913     FFIVABwwr  O    E             DISK    usropn
001100990908
001200000512     D*------------
001300990920     D dscmz         e ds                  inz
001400990910     D psds           sds
001500990910     D  procname         *PROC
001600941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001700941229     D* 10+3
001800941229     D                 DS
001900941229     D  WS15                   1      1
002000941229     D  WI15                   2     11
002100941229     D  WV15                  12     12
002200941229     D  WD15                  13     15
002300941229     D  WASC15                 1     15
002400941229     D*  6+1
002500941229     D                 DS
002600941229     D  WS09                   1      1
002700941229     D  WI09                   2      7
002800941229     D  WV09                   8      8
002900941229     D  WD09                   9      9
003000941229     D  WASC09                 1      9
003100941229     D*  2+3
003200941229     D                 DS
003300941229     D  WS07                   1      1
003400941229     D  WI07                   2      3
003500941229     D  WV07                   4      4
003600941229     D  WD07                   5      7
003700941229     D  WASC07                 1      7
003800990920     D tivlrds       e ds                  extname(tivlr00f)
003900041025     D tisi95ds      e ds
004000990910     D fnvabaar      e ds                  inz
004100990910     D esito           s              1
004200000724     D prmlit          s             10
004300000710     D prmfir          s             10
004400990921     D wrkesito        s                   like(esito)
004500000613     D rrnum           s              6  0 INZ(*zeros)
004600060410     D wVHBCAS         s                   LIKE(VHBCAS) INZ(*blanks)
004700010606     D*------------------
004800010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
004900010606     D*------------------
005000010606     D WLBDA8          DS                  INZ
005100010606     D  G08DAT                 1      8  0
005200010606     D  G08INV                 9     16  0
005300010606     D  G08ERR                17     17
005400010606     D  G08TGI                18     22  0
005500010606     D*
005600040407     D TxtInOut        S           2048
005700040407     D ElencoChar      S            256
005800040407     D TipoElenco      S              1
005900040407     D CharSost        S              1
006000040407     D UpperCase       S              1
006100040407     D ChkNull         S              1
006200040407     D CharNull        S              1
006300040407     D EsitoBon        S              1
006400990908
006500000913     C                   reset                   rrnum
006600990921     C                   reset                   esito
006700990921     C                   reset                   wrkesito
006800010530     C*
006900010530     C                   exsr      opeini
007000010530     C                   exsr      rwvab
007100041025     C*
007200041025     C* Esegue lancio TISI95R solo x chiusura
007300041025     C                   CLEAR                   TISI95DS
007400041025     C                   EVAL      I95TLA = 'C'
007500041025     C                   CALL      'TISI95R'
007600041025     C                   PARM                    TISI95DS
007700010530     C*
007800010530     C                   seton                                        lr
007900010530
008000010530
008100010530
008200010530
008300010530     C*--------------------------------------------------------
008400010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008500010530     C*--------------------------------------------------------
008600010530     C     PREELA        BEGSR
008700000724     C*
008800000724     C* SE OCCORRE SPEDIRE IN FILIALE
008900010530     C                   if        invfil <> *zeros and
009000010530     C                             flgGiro = '0'
009100010530     C*
009200010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009300010530     C                   eval      flgGiro = '1'
009400010530     C*
009500010530     C                   endif
009600010530     C*
009700010530     C                   ENDSR
009800010530     C***
009900010530
010000010530
010100010530
010200010530
010300010530
010400010530
010500010530     C*--------------------------------------------------------
010600010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010700010530     C*--------------------------------------------------------
010800010530     C     ENDELA        BEGSR
010900010530     C*
011000010530     C*
011100010530     C                   ENDSR
011200010530     C***
011300990908
011400010530
011500010530
011600010530
011700910830     C*--------------------------------------------------------
011800020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
011900910830     C*--------------------------------------------------------
012000941229     C     RWVAB         BEGSR
012100990910     C*
012200990914     C                   if        not %open(tivin00r)
012300990908     C                   open      tivin00r
012400990914     C                   endif
012500020913     C                   if        not %open(fivabwwr)
012600020913     C                   open      fivabwwr
012700990914     C                   endif
012800990910     C*
012900990910     C                   clear                   §CTROK
013000990910     C                   clear                   §CTRMO
013100990910     C                   clear                   §CTRNO
013200990910     C*
013300921023     C                   DO        *HIVAL
013400990913     C*
013500990915     C                   READ      tivin00r                               70
013600010618     C*
013700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
013800010618     C                   if        vinflg = '1'
013900010618     C                   eval      flgOk = '1'
014000010618     C                   endif
014100010705     C*
014200010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
014300010705     C                   if        %subst(vindta:1:1) = '='
014400010705     C                   eval      %subst(vindta:1:1) = *blanks
014500010705     C                   endif
014600010705     C*
014700051017     C                   if        vindta  > *blanks
014800000613     C                   add       1             rrnum
014900010601     C* ...elimino eventuali segni
015000010601     C     '+':' '       xlate     vindta        vindta
015100990913     C*
015200010601     C                   if        *in70 = *off and
015300990913     C                             (vinflg = *blanks
015400010711     C                              or vinflg = '0'
015500010711     C                              or vinflg = '2')
015600990913     C*
015700020913     C                   clear                   FIVAB000
015800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
015900010711     C                   if        vinflg = *blanks or vinflg = '0'
016000010711     C                   clear                   vinmsg
016100010711     C                   endif
016200010711     C*
016300990913     C                   eval      fnvabaar = vindta
016400991022     C*
016500991022     C                   testb     x'3F'         vhbatb                   02
016600991022     C                   if        not *in02
016700991022     C*
016800971021     C                   SETOFF                                       303132
016900971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
017000971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
017100971023     C*    OPPURE TUTTI BLANKS
017200010530     C*
017300010530     C                   TESTN                   VHBLNP               30
017400010530     C  N30              TESTN                   VHBLNP                 30
017500010530     C  N30              TESTN                   VHBLNP                   30
017600010530     C                   MOVE      VHBLNP        W001A             1
017700010530     C     *IN30         IFEQ      *OFF
017800010530     C     W001A         ORLT      '0'
017900010530     C                   SETON                                        31
018000010530     C                   eval      vinmsg = %trimr(vinmsg)
018100010530     C                             + ' ' + 'VHBLNP'
018200010530     C                   ELSE
018300010530     C                   MOVE      VHBLNP        VABLNP
018400010530     C     VABLNP        IFLE      *ZEROS
018500010530     C                   SETON                                        31
018600010530     C                   eval      vinmsg = %trimr(vinmsg)
018700010530     C                             + ' ' + 'VHBLNP'
018800010530     C                   ENDIF
018900010530     C                   ENDIF
019000010530     C*
019100010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019200010530     C                   eval      depfil = VABLNP
019300010530     C                   exsr      repfil
019400010601     C                   if        depfil = invfil
019500021014     C                   if        vlrpoi = 999
019600021010     C                   MOVE(P)   invfil        VABFGS
019700021014     C                   else
019800021014     C                   MOVE(P)   vlrpoi        VABFGS
019900021014     C                   endif
020000010530     C*
020100010530     C                   exsr      PREELA
020200010530     C*
020300971023     C                   TESTN                   VHBCCM               30
020400971023     C  N30              TESTN                   VHBCCM                 30
020500971023     C  N30              TESTN                   VHBCCM                   30
020600971021     C                   MOVE      VHBCCM        W001A             1
020700971021     C     *IN30         IFEQ      *OFF
020800971021     C     W001A         ORLT      '0'
020900971021     C                   SETON                                        31
021000010604     C                   eval      vinmsg = %trimr(vinmsg)
021100010604     C                             + ' ' + 'VHBCCM'
021200971021     C                   ELSE
021300971021     C                   MOVE      VHBCCM        VABCCM
021400971024     C     VABCCM        IFLE      *ZEROS
021500971024     C                   SETON                                        31
021600010604     C                   eval      vinmsg = %trimr(vinmsg)
021700010604     C                             + ' ' + 'VHBCCM'
021800971024     C                   ENDIF
021900971021     C                   ENDIF
022000971021     C*
022100971023     C                   TESTN                   VHBAAS               30
022200971023     C  N30              TESTN                   VHBAAS                 30
022300971023     C  N30              TESTN                   VHBAAS                   30
022400971021     C                   MOVE      VHBAAS        W001A             1
022500971021     C     *IN30         IFEQ      *OFF
022600971021     C     W001A         ORLT      '0'
022700971023     C     W001A         ANDNE     *BLANK
022800971021     C                   SETON                                        32
022900010606     C                   MOVEL     datcor        VABAAS
023000010604     C                   eval      vinmsg = %trimr(vinmsg)
023100010604     C                             + ' ' + 'VHBAAS'
023200971021     C                   ELSE
023300971021     C                   MOVE      VHBAAS        VABAAS
023400971021     C                   ENDIF
023500971021     C*
023600971023     C                   TESTN                   VHBMGS               30
023700971023     C  N30              TESTN                   VHBMGS                 30
023800971023     C  N30              TESTN                   VHBMGS                   30
023900971021     C                   MOVE      VHBMGS        W001A             1
024000971021     C     *IN30         IFEQ      *OFF
024100971021     C     W001A         ORLT      '0'
024200971023     C     W001A         ANDNE     *BLANK
024300971021     C                   SETON                                        32
024400010601     C                   MOVE      '0101'        VABMGS
024500010604     C                   eval      vinmsg = %trimr(vinmsg)
024600010604     C                             + ' ' + 'VHBMGS'
024700971021     C                   ELSE
024800971021     C                   MOVE      VHBMGS        VABMGS
024900971021     C                   ENDIF
025000971021     C*
025100971023     C                   TESTN                   VHBNRS               30
025200971023     C  N30              TESTN                   VHBNRS                 30
025300971023     C  N30              TESTN                   VHBNRS                   30
025400971021     C                   MOVE      VHBNRS        W001A             1
025500971021     C     *IN30         IFEQ      *OFF
025600971021     C     W001A         ORLT      '0'
025700971023     C     W001A         ANDNE     *BLANK
025800971021     C                   SETON                                        32
025900971021     C                   MOVE      *ZERO         VABNRS
026000010604     C                   eval      vinmsg = %trimr(vinmsg)
026100010604     C                             + ' ' + 'VHBNRS'
026200971021     C                   ELSE
026300971021     C                   MOVE      VHBNRS        VABNRS
026400971021     C                   ENDIF
026500971021     C*
026600971023     C                   TESTN                   VHBNSP               30
026700971023     C  N30              TESTN                   VHBNSP                 30
026800971023     C  N30              TESTN                   VHBNSP                   30
026900971021     C                   MOVE      VHBNSP        W001A             1
027000971021     C     *IN30         IFEQ      *OFF
027100971021     C     W001A         ORLT      '0'
027200971023     C     W001A         ANDNE     *BLANK
027300971021     C                   SETON                                        32
027400971021     C                   MOVE      *ZERO         VABNSP
027500010604     C                   eval      vinmsg = %trimr(vinmsg)
027600010604     C                             + ' ' + 'VHBNSP'
027700971021     C                   ELSE
027800971021     C                   MOVE      VHBNSP        VABNSP
027900971021     C                   ENDIF
028000971021     C*
028100971023     C                   TESTN                   VHBLNA               30
028200971023     C  N30              TESTN                   VHBLNA                 30
028300971023     C  N30              TESTN                   VHBLNA                   30
028400971021     C                   MOVE      VHBLNA        W001A             1
028500971021     C     *IN30         IFEQ      *OFF
028600971021     C     W001A         ORLT      '0'
028700971023     C     W001A         ANDNE     *BLANK
028800971021     C                   SETON                                        32
028900971021     C                   MOVE      *ZERO         VABLNA
029000010604     C                   eval      vinmsg = %trimr(vinmsg)
029100010604     C                             + ' ' + 'VHBLNA'
029200971021     C                   ELSE
029300971021     C                   MOVE      VHBLNA        VABLNA
029400971021     C                   ENDIF
029500971021     C*
029600971023     C                   TESTN                   VHBCTR               30
029700971023     C  N30              TESTN                   VHBCTR                 30
029800971023     C  N30              TESTN                   VHBCTR                   30
029900971021     C                   MOVE      VHBCTR        W001A             1
030000971021     C     *IN30         IFEQ      *OFF
030100971021     C     W001A         ORLT      '0'
030200971023     C     W001A         ANDNE     *BLANK
030300971021     C                   SETON                                        32
030400971021     C                   MOVE      *ZERO         VABCTR
030500010604     C                   eval      vinmsg = %trimr(vinmsg)
030600010604     C                             + ' ' + 'VHBCTR'
030700971021     C                   ELSE
030800971021     C                   MOVE      VHBCTR        VABCTR
030900971021     C                   ENDIF
031000941229     C*
031100971021     C                   MOVE      *ZERO         VABIAS
031200941229     C                   MOVE      VHBIAS        WASC15
031300971023     C                   TESTN                   WI15                 30
031400971023     C  N30              TESTN                   WI15                   30
031500971023     C  N30              TESTN                   WI15                     30
031600971021     C                   MOVE      WI15          W001A             1
031700971021     C     *IN30         IFEQ      *OFF
031800971021     C     W001A         ORLT      '0'
031900971023     C     W001A         ANDNE     *BLANK
032000971021     C                   SETON                                        32
032100971021     C                   MOVE      *ZERO         VABIAS
032200010604     C                   eval      vinmsg = %trimr(vinmsg)
032300010604     C                             + ' ' + 'VHBIAS'
032400971021     C                   ELSE
032500971021     C                   MOVEL     WI15          VABIAS
032600971023     C                   TESTN                   WD15                 30
032700971023     C  N30              TESTN                   WD15                   30
032800971023     C  N30              TESTN                   WD15                     30
032900971021     C                   MOVE      WD15          W001A             1
033000971021     C     *IN30         IFEQ      *OFF
033100971021     C     W001A         ORLT      '0'
033200971023     C     W001A         ANDNE     *BLANK
033300971021     C                   SETON                                        32
033400971021     C                   MOVE      *ZERO         VABIAS
033500010604     C                   eval      vinmsg = %trimr(vinmsg)
033600010604     C                             + ' ' + 'VHBIAS'
033700971021     C                   ELSE
033800971021     C                   MOVE      WD15          VABIAS
033900971021     C                   ENDIF
034000050118     C                   ENDIF
034100941229     C*
034200971023     C                   TESTN                   VHBNCL               30
034300971023     C  N30              TESTN                   VHBNCL                 30
034400971023     C  N30              TESTN                   VHBNCL                   30
034500971021     C                   MOVE      VHBNCL        W001A             1
034600971021     C     *IN30         IFEQ      *OFF
034700971021     C     W001A         ORLT      '0'
034800971021     C                   SETON                                        32
034900971021     C                   MOVE      *ZERO         VABNCL
035000010604     C                   eval      vinmsg = %trimr(vinmsg)
035100010604     C                             + ' ' + 'VHBNCL'
035200971021     C                   ELSE
035300971021     C                   MOVE      VHBNCL        VABNCL
035400971021     C                   ENDIF
035500941229     C*
035600971021     C                   MOVE      *ZERO         VABPKB
035700971021     C                   MOVE      VHBPKB        WASC09
035800010719     C                   IF        WI09 = *blanks
035900010719     C                   EVAL      WI09 = *zeros
036000010719     C                   ENDIF
036100971023     C                   TESTN                   WI09                 30
036200971023     C  N30              TESTN                   WI09                   30
036300971023     C  N30              TESTN                   WI09                     30
036400971021     C                   MOVE      WI09          W001A             1
036500971021     C     *IN30         IFEQ      *OFF
036600971021     C     W001A         ORLT      '0'
036700971021     C                   SETON                                        32
036800971021     C                   MOVE      *ZERO         VABPKB
036900010604     C                   eval      vinmsg = %trimr(vinmsg)
037000010604     C                             + ' ' + 'VHBPKB'
037100971021     C                   ELSE
037200971021     C                   MOVEL     WI09          VABPKB
037300010719     C                   IF        WD09 = *blanks
037400010719     C                   EVAL      WD09 = *zeros
037500010719     C                   ENDIF
037600971023     C                   TESTN                   WD09                 30
037700971023     C  N30              TESTN                   WD09                     30
037800971021     C                   MOVE      WD09          W001A             1
037900971021     C     *IN30         IFEQ      *OFF
038000971021     C     W001A         ORLT      '0'
038100971023     C     W001A         ANDNE     *BLANK
038200971021     C                   SETON                                        32
038300971021     C                   MOVE      *ZERO         VABPKB
038400010604     C                   eval      vinmsg = %trimr(vinmsg)
038500010604     C                             + ' ' + 'VHBPKB'
038600971021     C                   ELSE
038700971021     C                   MOVE      WD09          VABPKB
038800971021     C                   ENDIF
038900050118     C                   ENDIF
039000971021     C*
039100041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
039200041001     C                   IF        *IN51 = *OFF
039300971021     C                   MOVE      *ZERO         VABVLB
039400971021     C                   MOVE      VHBVLB        WASC07
039500010719     C                   IF        WI07 = *blanks
039600010719     C                   EVAL      WI07 = *zeros
039700010719     C                   ENDIF
039800971023     C                   TESTN                   WI07                 30
039900971023     C  N30              TESTN                   WI07                   30
040000971023     C  N30              TESTN                   WI07                     30
040100971021     C                   MOVE      WI07          W001A             1
040200971021     C     *IN30         IFEQ      *OFF
040300971021     C     W001A         ORLT      '0'
040400971023     C     W001A         ANDNE     *BLANK
040500971021     C                   SETON                                        32
040600971021     C                   MOVE      *ZERO         VABVLB
040700010604     C                   eval      vinmsg = %trimr(vinmsg)
040800010604     C                             + ' ' + 'VHBVLB'
040900971021     C                   ELSE
041000971021     C                   MOVEL     WI07          VABVLB
041100010719     C                   IF        WD07 = *blanks
041200010719     C                   EVAL      WD07 = *zeros
041300010719     C                   ENDIF
041400971023     C                   TESTN                   WD07                 30
041500971023     C  N30              TESTN                   WD07                   30
041600971023     C  N30              TESTN                   WD07                     30
041700971021     C                   MOVE      WD07          W001A             1
041800971021     C     *IN30         IFEQ      *OFF
041900971021     C     W001A         ORLT      '0'
042000971023     C     W001A         ANDNE     *BLANK
042100971021     C                   SETON                                        32
042200971021     C                   MOVE      *ZERO         VABVLB
042300010604     C                   eval      vinmsg = %trimr(vinmsg)
042400010604     C                             + ' ' + 'VHBVLB'
042500971021     C                   ELSE
042600971021     C                   MOVE      WD07          VABVLB
042700971021     C                   ENDIF
042800050118     C                   ENDIF
042900041001     C                   ELSE
043000041001     C* ...altrimenti forzo azzeramento del volume
043100041001     C                   Z-ADD     *zeros        VABVLB
043200041001     C                   ENDIF
043300941229     C*
043400971021     C                   MOVE      *ZERO         VABQFT
043500971021     C                   MOVE      VHBQFT        WASC15
043600010719     C                   IF        WI15 = *blanks
043700010719     C                   EVAL      WI15 = *zeros
043800010719     C                   ENDIF
043900971023     C                   TESTN                   WI15                 30
044000971023     C  N30              TESTN                   WI15                   30
044100971023     C  N30              TESTN                   WI15                     30
044200971021     C                   MOVE      WI15          W001A             1
044300971021     C     *IN30         IFEQ      *OFF
044400971021     C     W001A         ORLT      '0'
044500971023     C     W001A         ANDNE     *BLANK
044600971021     C                   SETON                                        32
044700971021     C                   MOVE      *ZERO         VABQFT
044800010604     C                   eval      vinmsg = %trimr(vinmsg)
044900010604     C                             + ' ' + 'VHBQFT'
045000971021     C                   ELSE
045100971021     C                   MOVEL     WI15          VABQFT
045200010719     C                   IF        WD15 = *blanks
045300010719     C                   EVAL      WD15 = *zeros
045400010719     C                   ENDIF
045500971023     C                   TESTN                   WD15                 30
045600971023     C  N30              TESTN                   WD15                   30
045700971023     C  N30              TESTN                   WD15                     30
045800971021     C                   MOVE      WD15          W001A             1
045900971021     C     *IN30         IFEQ      *OFF
046000971021     C     W001A         ORLT      '0'
046100971023     C     W001A         ANDNE     *BLANK
046200971021     C                   SETON                                        32
046300971021     C                   MOVE      *ZERO         VABQFT
046400010604     C                   eval      vinmsg = %trimr(vinmsg)
046500010604     C                             + ' ' + 'VHBQFT'
046600971021     C                   ELSE
046700971021     C                   MOVE      WD15          VABQFT
046800971021     C                   ENDIF
046900050118     C                   ENDIF
047000941229     C*
047100971021     C                   MOVE      *ZERO         VABCAS
047200060410     C* Se presente separatore decimale alla giusta posizione => forzo sempre la virgola
047300060410     C                   EVAL      wVHBCAS = VHBCAS
047400060410     C                   IF        %subst(wVHBCAS:12:1) = '.'
047500060410     C                   EVAL      %subst(wVHBCAS:12:1) = ','
047600060410     C                   ENDIF
047700060410     C*
047800060410     C                   MOVEL(P)  wVHBCAS       PiStr
047900060410     C                   EXSR      CHKNUM
048000060410     C                   IF        PiNum=*on
048100060410     C                   Z-ADD     PiVal         VABCAS
048200060410     C                   ELSE
048300060410     C                   SETON                                        32
048400060410     C                   Z-ADD     *zeros        VABCAS
048500060410     C                   EVAL      vinmsg = %trimr(vinmsg)
048600060410     C                             + ' ' + 'VHBCAS'
048700060410     C                   ENDIF
048800971021     C*
048900971023     C                   TESTN                   VHBRMN               30
049000971023     C  N30              TESTN                   VHBRMN                 30
049100971023     C  N30              TESTN                   VHBRMN                   30
049200971021     C                   MOVE      VHBRMN        W001A             1
049300971021     C     *IN30         IFEQ      *OFF
049400971021     C     W001A         ORLT      '0'
049500971021     C                   SETON                                        32
049600971021     C                   MOVE      *ZERO         VABRMN
049700010604     C                   eval      vinmsg = %trimr(vinmsg)
049800010604     C                             + ' ' + 'VHBRMN'
049900971021     C                   ELSE
050000971021     C                   MOVE      VHBRMN        VABRMN
050100971021     C                   ENDIF
050200971021     C*
050300971023     C                   TESTN                   VHBNCD               30
050400971023     C  N30              TESTN                   VHBNCD                 30
050500971023     C  N30              TESTN                   VHBNCD                   30
050600971021     C                   MOVE      VHBNCD        W001A             1
050700971021     C     *IN30         IFEQ      *OFF
050800971021     C     W001A         ORLT      '0'
050900971023     C     W001A         ANDNE     *BLANK
051000971021     C                   SETON                                        32
051100971021     C                   MOVE      *ZERO         VABNCD
051200010604     C                   eval      vinmsg = %trimr(vinmsg)
051300010604     C                             + ' ' + 'VHBNCD'
051400971021     C                   ELSE
051500971021     C                   MOVE      VHBNCD        VABNCD
051600971021     C                   ENDIF
051700971021     C*
051800971023     C                   TESTN                   VHBNCA               30
051900971023     C  N30              TESTN                   VHBNCA                 30
052000971023     C  N30              TESTN                   VHBNCA                   30
052100971021     C                   MOVE      VHBNCA        W001A             1
052200971021     C     *IN30         IFEQ      *OFF
052300971021     C     W001A         ORLT      '0'
052400971023     C     W001A         ANDNE     *BLANK
052500971021     C                   SETON                                        32
052600971021     C                   MOVE      *ZERO         VABNCA
052700010604     C                   eval      vinmsg = %trimr(vinmsg)
052800010604     C                             + ' ' + 'VHBNCA'
052900971021     C                   ELSE
053000971021     C                   MOVE      VHBNCA        VABNCA
053100971021     C                   ENDIF
053200971021     C*
053300971023     C                   TESTN                   VHBZNC               30
053400971023     C  N30              TESTN                   VHBZNC                 30
053500971023     C  N30              TESTN                   VHBZNC                   30
053600971021     C                   MOVE      VHBZNC        W001A             1
053700971021     C     *IN30         IFEQ      *OFF
053800971021     C     W001A         ORLT      '0'
053900971023     C     W001A         ANDNE     *BLANK
054000971021     C                   SETON                                        32
054100971021     C                   MOVE      *ZERO         VABZNC
054200010604     C                   eval      vinmsg = %trimr(vinmsg)
054300010604     C                             + ' ' + 'VHBZNC'
054400971021     C                   ELSE
054500971021     C                   MOVE      VHBZNC        VABZNC
054600971021     C                   ENDIF
054700971021     C*
054800971023     C                   TESTN                   VHBDCR               30
054900971023     C  N30              TESTN                   VHBDCR                 30
055000971023     C  N30              TESTN                   VHBDCR                   30
055100971021     C                   MOVE      VHBDCR        W001A             1
055200971021     C     *IN30         IFEQ      *OFF
055300971021     C     W001A         ORLT      '0'
055400971023     C     W001A         ANDNE     *BLANK
055500971021     C                   SETON                                        32
055600971021     C                   MOVE      *ZERO         VABDCR
055700010604     C                   eval      vinmsg = %trimr(vinmsg)
055800010604     C                             + ' ' + 'VHBDCR'
055900971021     C                   ELSE
056000971021     C                   MOVE      VHBDCR        VABDCR
056100971021     C                   ENDIF
056200971021     C*
056300971023     C                   TESTN                   VHBHCR               30
056400971023     C  N30              TESTN                   VHBHCR                 30
056500971023     C  N30              TESTN                   VHBHCR                   30
056600971021     C                   MOVE      VHBHCR        W001A             1
056700971021     C     *IN30         IFEQ      *OFF
056800971021     C     W001A         ORLT      '0'
056900971023     C     W001A         ANDNE     *BLANK
057000971021     C                   SETON                                        32
057100971021     C                   MOVE      *ZERO         VABHCR
057200010604     C                   eval      vinmsg = %trimr(vinmsg)
057300010604     C                             + ' ' + 'VHBHCR'
057400971021     C                   ELSE
057500971021     C                   MOVE      VHBHCR        VABHCR
057600971021     C                   ENDIF
057700941229     C*
057800971021     C                   MOVE      *ZERO         VABVMD
057900971021     C                   MOVE      VHBVMD        WASC15
058000010719     C                   IF        WI15 = *blanks
058100010719     C                   EVAL      WI15 = *zeros
058200010719     C                   ENDIF
058300971023     C                   TESTN                   WI15                 30
058400971023     C  N30              TESTN                   WI15                   30
058500971023     C  N30              TESTN                   WI15                     30
058600971021     C                   MOVE      WI15          W001A             1
058700971021     C     *IN30         IFEQ      *OFF
058800971021     C     W001A         ORLT      '0'
058900971023     C     W001A         ANDNE     *BLANK
059000971021     C                   SETON                                        32
059100971021     C                   MOVE      *ZERO         VABVMD
059200010604     C                   eval      vinmsg = %trimr(vinmsg)
059300010604     C                             + ' ' + 'VHBVMD'
059400971021     C                   ELSE
059500971021     C                   MOVEL     WI15          VABVMD
059600010719     C                   IF        WD15 = *blanks
059700010719     C                   EVAL      WD15 = *zeros
059800010719     C                   ENDIF
059900971023     C                   TESTN                   WD15                 30
060000971023     C  N30              TESTN                   WD15                   30
060100971023     C  N30              TESTN                   WD15                     30
060200971021     C                   MOVE      WD15          W001A             1
060300971021     C     *IN30         IFEQ      *OFF
060400971021     C     W001A         ORLT      '0'
060500971023     C     W001A         ANDNE     *BLANK
060600971021     C                   SETON                                        32
060700971021     C                   MOVE      *ZERO         VABVMD
060800010604     C                   eval      vinmsg = %trimr(vinmsg)
060900010604     C                             + ' ' + 'VHBVMD'
061000971021     C                   ELSE
061100971021     C                   MOVE      WD15          VABVMD
061200971021     C                   ENDIF
061300050118     C                   ENDIF
061400971021     C*
061500971023     C                   TESTN                   VHBANT               30
061600971023     C  N30              TESTN                   VHBANT                 30
061700971023     C  N30              TESTN                   VHBANT                   30
061800971021     C                   MOVE      VHBANT        W001A             1
061900971021     C     *IN30         IFEQ      *OFF
062000971021     C     W001A         ORLT      '0'
062100971023     C     W001A         ANDNE     *BLANK
062200971021     C                   SETON                                        32
062300971021     C                   MOVE      *ZERO         VABANT
062400010604     C                   eval      vinmsg = %trimr(vinmsg)
062500010604     C                             + ' ' + 'VHBANT'
062600971021     C                   ELSE
062700971021     C                   MOVE      VHBANT        VABANT
062800971021     C                   ENDIF
062900971021     C*
063000971021     C* CAMPI ALFABETICI:
063100971021     C                   MOVE      VHBATB        VABATB
063200971021     C                   MOVE      VHBCBO        VABCBO
063300971021     C                   MOVE      VHBRSD        VABRSD
063400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
063500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
063600020117     C* ==
063700971021     C                   MOVE      VHBRD2        VABRD2
063800971021     C                   MOVE      VHBIND        VABIND
063900971021     C                   MOVE      VHBCAD        VABCAD
064000971021     C                   MOVE      VHBLOD        VABLOD
064100971021     C                   MOVE      VHBPRD        VABPRD
064200971021     C                   MOVE      VHBNZD        VABNZD
064300971021     C                   MOVE      VHBGC1        VABGC1
064400971021     C                   MOVE      VHBGC2        VABGC2
064500971021     C                   MOVE      VHBTSP        VABTSP
064600971021     C                   MOVE      VHBVAS        VABVAS
064700971021     C                   MOVE      VHBNAS        VABNAS
064800971021     C                   MOVE      VHBTIC        VABTIC
064900971021     C                   MOVE      VHBVCA        VABVCA
065000971021     C                   MOVE      VHBGCA        VABGCA
065100971021     C                   MOVE      VHBRMA        VABRMA
065200971021     C                   MOVE      VHBXCO        VABXCO
065300971021     C                   MOVE      VHBNOT        VABNOT
065400971021     C                   MOVE      VHBNT2        VABNT2
065500971021     C                   MOVE      VHBCTM        VABCTM
065600971021     C                   MOVE      VHBFFD        VABFFD
065700971021     C                   MOVE      VHBTCR        VABTCR
065800971021     C                   MOVE      VHBCTS        VABCTS
065900971021     C                   MOVE      VHBFTM        VABFTM
066000971021     C                   MOVE      VHBVAD        VABVAD
066100971021     C                   MOVE      VHBGMA        VABGMA
066200971021     C                   MOVE      VHBGGA        VABGGA
066300971021     C                   MOVE      VHBGVA        VABGVA
066400971021     C                   MOVE      VHBTC1        VABTC1
066500971021     C                   MOVE      VHBTC2        VABTC2
066600971021     C                   MOVE      VHBSCL        VABSCL
066700971021     C                   MOVE      VHBRMO        VABRMO
066800971021     C                   MOVE      VHBCMO        VABCMO
066900971021     C                   MOVE      VHBNMO        VABNMO
067000971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
067100060329     C* NELLA NAZIONE MITTENTE ORIGINALE
067200060331     C                   IF        %trim(VHBNMO) < 'A'
067300060329     C                   MOVE      *BLANKS       VABNMO
067400060329     C                   ENDIF
067500050418     C*
067600050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
067700050418     C                   IF        %trim(VABNZD) = 'I'   OR
067800050418     C                             %trim(VABNZD) = 'IT'  OR
067900050418     C                             %trim(VABNZD) = 'ITA'
068000050418     C                   EVAL      VABNZD = *blanks
068100050418     C                   ENDIF
068200041025     C*
068300041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
068400041025     C                   IF        *IN52 = *ON
068500041025     C* Reperisco la provincia dal CAP e dalla località
068600041025     C                   IF        VABCAD <> *blanks AND
068700050907     C                             VABPRD  = *blanks AND
068800050907     C                             VABNZD  = *blanks
068900041025     C                   CLEAR                   TISI95DS
069000041025     C                   EVAL      I95TCN = '3'
069100041025     C                   Z-ADD     datcor        I95DAT
069200041025     C                   EVAL      I95CAP = VABCAD
069300041025     C                   EVAL      I95LOC = VABLOD
069400041025     C                   CALL      'TISI95R'
069500041025     C                   PARM                    TISI95DS
069600041025     C                   EVAL      VABPRD = O95PRV
069700041025     C                   ENDIF
069800041025     C                   ENDIF
069900040630     C*
070000040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
070100040630     C                   exsr      exeBon
070200100419     C*
070300100419     C* Forzatura su campi DIVISE
070400100419     C                   IF        VABVAS = *blanks
070500100419     C                   EVAL      VABVAS = 'EUR'
070600100419     C                   ENDIF
070700100419     C                   IF        VABVCA = *blanks
070800100419     C                   EVAL      VABVCA = 'EUR'
070900100419     C                   ENDIF
071000100419     C                   IF        VABVAD = *blanks
071100100419     C                   EVAL      VABVAD = 'EUR'
071200100419     C                   ENDIF
071300111102     C*
071400111102     C* Forzature da parametri d anagrafica
071500111102     C                   if        %subst(vlrppt:4:2) <> *blanks
071600111102     C                   eval      VABTIC = %subst(vlrppt:4:2)
071700111102     C                   endif
071800930409     C*
071900010604     C  N31              ADD       1             §CTROK            7 0
072000010604     C   32              ADD       1             §CTRMO            7 0
072100010604     C   31              ADD       1             §CTRNO            7 0
072200130410     C*
072300130410     C* svuoto le note che vengono valorizzate dal cliente per errore
072400130410     C                   IF        *in31 = *off
072500130410     C                   CLEAR                   VABNOT
072600130410     C                   CLEAR                   VABNT2
072700130410     C                   ENDIF
072800130410     C*
072900020913     C  N31              WRITE     FIVAB000
073000990910     C*
073100010601     C                   if        *in31 = *off and
073200010601     C                             *in32 = *off
073300990910     C                   eval      vinflg = '1'
073400990910     C                   else
073500990910     C                   eval      vinflg = '2'
073600990910     C                   endif
073700991022     C                   endif
073800990910     C                   endif
073900010601     C*
074000010601     C                   endif
074100000905     C*
074200000905     C                   else
074300000905     C                   eval      vinflg = '1'
074400000905     C                   endif
074500000905     C*
074600000905     C  N70              update    tivin000
074700991022     C*
074800991022     C  N70              ENDdo
074900010530     C
075000010530     C*
075100010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
075200010530     C                   if        cntNonEl = *zeros or
075300010530     C                             flgMulti = '0'
075400990910     C* Se non ci sono record con errori ...
075500010530     C                   if        §ctrno = 0 and
075600010601     C                             §ctrmo = 0 and
075700010530     C                             flgStato <> '2'
075800990910     C* ... restituisco esito OK.
075900990921     C                   eval      wrkesito = '0'
076000990910     C                   else
076100010601     C                   if        §ctrok > 0
076200990921     C                   eval      wrkesito = '1'
076300000710     C                   else
076400010615     C                   if        flgOk = '0'
076500010615     C                   eval      wrkesito = '2'
076600010615     C                   else
076700010615     C                   eval      wrkesito = '6'
076800010615     C                   endif
076900990910     C                   endif
077000000710     C                   endif
077100010530     C                   else
077200010530     C                   eval      wrkesito = '9'
077300010530     C                   endif
077400990910     C*
077500990914     C                   if        %open(tivin00r)
077600990908     C                   close     tivin00r
077700990914     C                   endif
077800020913     C                   if        %open(fivabwwr)
077900020913     C                   close     fivabwwr
078000990914     C                   endif
078100010530     C*
078200010530     C                   if        vlrpoi <> 999
078300010530     C                   eval      invfil = vlrpoi
078400010530     C                   endif
078500990910     C*
078600990920     C                   if        §ctrok > 0
078700010530     C                             and invfil > *zeros
078800000613     C                   exsr      invio
078900990920     C                   endif
079000010612     C*
079100010612     C                   if        flgGiro = '1'
079200010612     C                   exsr      endela
079300010612     C                   endif
079400990920     C*
079500910830     C                   ENDSR
079600000613     C***
079700990920
079800010530
079900010530
080000010530
080100010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
080200010530     C     repfil        BEGSR
080300010530     C*
080400010530     C                   if        invfil = *zeros and
080500010530     C                             depfil > *zeros and
080600010613     C                             (vinflg = *blanks or
080700010613     C                              vinflg = *zeros)
080800010530     C
080900010530     C                   eval      invfil = depfil
081000010530     C                   endif
081100010530     C*
081200010530     C                   if        depfil <> invfil and
081300010530     C                             invfil > *zeros
081400010530     C                   eval      flgMulti = '1'
081500010530     C                   if        vinflg = *blanks
081600010530     C                   add       1             cntNonEl
081700010530     C                   endif
081800010530     C                   endif
081900010530     C*
082000010530     C                   if        vinflg = '2'
082100010530     C                   eval      flgStato = '2'
082200010530     C                   endif
082300010530     C*
082400010530     C                   ENDSR
082500010530     C***
082600010530
082700010530
082800010530
082900010530
083000990920      /TITLE Invio dei dati al punto operativo.
083100000613     C     invio         BEGSR
083200990920     C*
083300990920     C                   reset                   dscmz
083400010530     C                   move      invfil        cmzdst
083500990920     C                   eval      cmzfld = vlrfou
083600990920     C                   eval      cmzmbd = vlrhdl
083700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
083800000710     C                   if        prmfir = *blanks
083900020913     C                   eval      cmzfla = 'FIVAB00F'
084000020913     C                   eval      cmzmba = 'FIVAB00F'
084100000710     C                   else
084200000710     C                   eval      cmzfla = prmfir
084300000710     C                   eval      cmzmba = prmfir
084400000710     C                   endif
084500990920     C                   eval      cmznrr = *zeros
084600990920     C                   move      §ctrok        cmznrr
084700021003     C                   eval      cmzlba = vlrfl1
084800990920     C                   call(e)   'TIS711C'
084900990920     C                   parm                    dscmz
085000990921     C                   parm      *blanks       esito
085100990920     C                   if        %error
085200990920     C                             or cmzerr = '1'
085300990921     C                             or esito  = '1'
085400000710     C                   eval      wrkesito = '3'
085500990920     C                   endif
085600990920     C*
085700000613     C                   ENDSR
085800000613     C***
085900010530
086000010530
086100010530
086200010530
086300010530      /TITLE Invio dei dati al punto operativo.
086400010530     C     opeini        BEGSR
086500010530     C*
086600010530     C* Inizializzo flag e contatori operativi
086700010530     C                   movel     '0'           flgGiro           1
086800010530     C                   movel     '0'           flgMulti          1
086900010530     C                   movel     '1'           flgStato          1
087000010615     C                   movel     '0'           flgOk             1
087100010530     C                   z-add     *zeros        cntNonEl         10 0
087200010530     C                   z-add     *zeros        depfil            3 0
087300010530     C                   z-add     *zeros        invfil            3 0
087400030407     C*
087500030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
087600030407     C                   if        %subst(vlrppt:1:1) = 'S'
087700030407     C                   seton                                        50
087800030407     C                   else
087900030407     C                   setoff                                       50
088000030407     C                   endif
088100041001     C*
088200041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
088300041001     C                   if        %subst(vlrppt:2:1) = 'S'
088400041001     C                   seton                                        51
088500041001     C                   else
088600041001     C                   setoff                                       51
088700041001     C                   endif
088800041025     C*
088900041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
089000041025     C* (naturalmente solo se nn già presente)
089100041025     C                   if        %subst(vlrppt:3:1) = 'S'
089200041025     C                   seton                                        52
089300041025     C                   else
089400041025     C                   setoff                                       52
089500041025     C                   endif
089600010530     C*
089700010530     C                   ENDSR
089800010530     C***
089900040407
090000040407
090100040407
090200040407
090300040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
090400040407     C     exeBon        BEGSR
090500040407     C*
090600040407     C* Imposto i parametri x il *pgm d "bonifica"
090700040407     C                   EVAL      ElencoChar = *blanks
090800040407     C                   EVAL      TipoElenco = *blanks
090900040407     C                   EVAL      CharSost   = *blanks
091000040629     C                   EVAL      UpperCase  = '1'
091100040407     C                   EVAL      ChkNull    = '1'
091200040407     C                   EVAL      CharNull   = *blanks
091300040629     C*
091400040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
091500040629     C                   EVAL      TxtInOut   = VABRSD
091600040629     C                   EVAL      EsitoBon   = *blanks
091700040407     C*
091800040407     C                   CALL      'XCHKCHAR'
091900040407     C                   PARM                    TxtInOut
092000040407     C                   PARM                    ElencoChar
092100040407     C                   PARM                    TipoElenco
092200040407     C                   PARM                    CharSost
092300040407     C                   PARM                    UpperCase
092400040407     C                   PARM                    ChkNull
092500040407     C                   PARM                    CharNull
092600040407     C                   PARM                    EsitoBon
092700040407     C*
092800040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
092900040407     C                   IF        EsitoBon = '1'
093000040629     C                   EVAL      VABRSD = TxtInOut
093100040407     C                   ENDIF
093200040629     C*
093300040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
093400040629     C                   EVAL      TxtInOut   = VABIND
093500040629     C                   EVAL      EsitoBon   = *blanks
093600040629     C*
093700040629     C                   CALL      'XCHKCHAR'
093800040629     C                   PARM                    TxtInOut
093900040629     C                   PARM                    ElencoChar
094000040629     C                   PARM                    TipoElenco
094100040629     C                   PARM                    CharSost
094200040629     C                   PARM                    UpperCase
094300040629     C                   PARM                    ChkNull
094400040629     C                   PARM                    CharNull
094500040629     C                   PARM                    EsitoBon
094600040629     C*
094700040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
094800040629     C                   IF        EsitoBon = '1'
094900040629     C                   EVAL      VABIND = TxtInOut
095000040629     C                   ENDIF
095100040629     C*
095200040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
095300040629     C                   EVAL      TxtInOut   = VABLOD
095400040629     C                   EVAL      EsitoBon   = *blanks
095500040629     C*
095600040629     C                   CALL      'XCHKCHAR'
095700040629     C                   PARM                    TxtInOut
095800040629     C                   PARM                    ElencoChar
095900040629     C                   PARM                    TipoElenco
096000040629     C                   PARM                    CharSost
096100040629     C                   PARM                    UpperCase
096200040629     C                   PARM                    ChkNull
096300040629     C                   PARM                    CharNull
096400040629     C                   PARM                    EsitoBon
096500040629     C*
096600040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
096700040629     C                   IF        EsitoBon = '1'
096800040629     C                   EVAL      VABLOD = TxtInOut
096900040629     C                   ENDIF
097000040629     C*
097100040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
097200040629     C                   EVAL      TxtInOut   = VABNOT
097300040629     C                   EVAL      EsitoBon   = *blanks
097400040630     C                   EVAL      UpperCase  = *blanks
097500040629     C*
097600040629     C                   CALL      'XCHKCHAR'
097700040629     C                   PARM                    TxtInOut
097800040629     C                   PARM                    ElencoChar
097900040629     C                   PARM                    TipoElenco
098000040629     C                   PARM                    CharSost
098100040629     C                   PARM                    UpperCase
098200040629     C                   PARM                    ChkNull
098300040629     C                   PARM                    CharNull
098400040629     C                   PARM                    EsitoBon
098500040629     C*
098600040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
098700040629     C                   IF        EsitoBon = '1'
098800040629     C                   EVAL      VABNOT = TxtInOut
098900040629     C                   ENDIF
099000040629     C*
099100040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
099200040629     C                   EVAL      TxtInOut   = VABNT2
099300040629     C                   EVAL      EsitoBon   = *blanks
099400040630     C                   EVAL      UpperCase  = *blanks
099500040629     C*
099600040629     C                   CALL      'XCHKCHAR'
099700040629     C                   PARM                    TxtInOut
099800040629     C                   PARM                    ElencoChar
099900040629     C                   PARM                    TipoElenco
100000040629     C                   PARM                    CharSost
100100040629     C                   PARM                    UpperCase
100200040629     C                   PARM                    ChkNull
100300040629     C                   PARM                    CharNull
100400040629     C                   PARM                    EsitoBon
100500040629     C*
100600040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
100700040629     C                   IF        EsitoBon = '1'
100800040629     C                   EVAL      VABNT2 = TxtInOut
100900040629     C                   ENDIF
101000040407     C*
101100040407     C                   ENDSR
101200010530
101300060410
101400060410     C*----------------------------------------------------*
101500060410     C*  CONTROLLO NUMERICITA' CAMPI
101600060410     C*----------------------------------------------------*
101700060410     C     CHKNUM        BEGSR
101800060410     C*
101900060410     C                   IF        PiDecChr = *blanks
102000060410     C                   EVAL      PiDecChr = ','
102100060410     C                   ENDIF
102200060410     C*
102300060410     C                   CALL(e)   'ISNUMERIC'
102400060410     C                   PARM                    PiStr            30
102500060410     C                   PARM                    PiDecChr          1
102600060410     C                   PARM      *ZEROS        PiVal            30 9
102700060410     C                   PARM      '0'           PiInt             1
102800060410     C                   PARM      '0'           PiNum             1
102900060410     C                   IF        %error
103000060410     C                   EVAL      PiNum=*off
103100060410     C                   ENDIF
103200060410     C*
103300060410     C                   ENDSR
103400060410     C***
103500060410
103600990910
103700000613     C     *inzsr        BEGSR
103800990910     C*
103900990910     C     *entry        plist
104000990920     C                   parm                    tivlrds
104100990921     C                   parm      wrkesito      esito
104200000724     C                   parm                    prmlit
104300000710     C                   parm                    prmfir
104400010606     C*
104500010606     C* CALCOLA LA DATA CORRENTE
104600010606     C                   time                    wn14             14 0
104700010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
104800010606     C                   z-add     wn8           g08dat
104900010606     C                   z-add     *zeros        g08inv
105000010606     C                   movel     '0'           g08err
105100010606     C                   call      'XSRDA8'
105200010606     C                   parm                    wlbda8
105300010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
105400000613     C*
105500000613     C                   ENDSR
