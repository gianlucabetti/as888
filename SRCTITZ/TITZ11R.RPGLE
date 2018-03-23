000100020913      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300120209
000400990910     Ftivin00r  uF   E             DISK    usropn
000500020913     FFIVABwwr  O    E             DISK    usropn
000600990908
000700000512     D*------------
000800990920     D dscmz         e ds                  inz
000900990910     D psds           sds
001000990910     D  procname         *PROC
001100941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001200941229     D* 10+3
001300941229     D                 DS
001400941229     D  WS15                   1      1
001500941229     D  WI15                   2     11
001600941229     D  WV15                  12     12
001700941229     D  WD15                  13     15
001800941229     D  WASC15                 1     15
001900941229     D*  6+1
002000941229     D                 DS
002100941229     D  WS09                   1      1
002200941229     D  WI09                   2      7
002300941229     D  WV09                   8      8
002400941229     D  WD09                   9      9
002500941229     D  WASC09                 1      9
002600941229     D*  2+3
002700941229     D                 DS
002800941229     D  WS07                   1      1
002900941229     D  WI07                   2      3
003000941229     D  WV07                   4      4
003100941229     D  WD07                   5      7
003200941229     D  WASC07                 1      7
003300990920     D tivlrds       e ds                  extname(tivlr00f)
003400041025     D tisi95ds      e ds
003500990910     D fnvabaar      e ds                  inz
003600990910     D esito           s              1
003700000724     D prmlit          s             10
003800000710     D prmfir          s             10
003900990921     D wrkesito        s                   like(esito)
004000000613     D rrnum           s              6  0 INZ(*zeros)
004100060410     D wVHBCAS         s                   LIKE(VHBCAS) INZ(*blanks)
004200010606     D*------------------
004300010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
004400010606     D*------------------
004500010606     D WLBDA8          DS                  INZ
004600010606     D  G08DAT                 1      8  0
004700010606     D  G08INV                 9     16  0
004800010606     D  G08ERR                17     17
004900010606     D  G08TGI                18     22  0
005000010606     D*
005100040407     D TxtInOut        S           2048
005200040407     D ElencoChar      S            256
005300040407     D TipoElenco      S              1
005400040407     D CharSost        S              1
005500040407     D UpperCase       S              1
005600040407     D ChkNull         S              1
005700040407     D CharNull        S              1
005800040407     D EsitoBon        S              1
005900990908
006000000913     C                   reset                   rrnum
006100990921     C                   reset                   esito
006200990921     C                   reset                   wrkesito
006300010530     C*
006400010530     C                   exsr      opeini
006500010530     C                   exsr      rwvab
006600041025     C*
006700041025     C* Esegue lancio TISI95R solo x chiusura
006800041025     C                   CLEAR                   TISI95DS
006900041025     C                   EVAL      I95TLA = 'C'
007000041025     C                   CALL      'TISI95R'
007100041025     C                   PARM                    TISI95DS
007200010530     C*
007300010530     C                   seton                                        lr
007400010530
007500010530
007600010530
007700010530
007800010530     C*--------------------------------------------------------
007900010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008000010530     C*--------------------------------------------------------
008100010530     C     PREELA        BEGSR
008200000724     C*
008300000724     C* SE OCCORRE SPEDIRE IN FILIALE
008400010530     C                   if        invfil <> *zeros and
008500010530     C                             flgGiro = '0'
008600010530     C*
008700010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008800010530     C                   eval      flgGiro = '1'
008900010530     C*
009000010530     C                   endif
009100010530     C*
009200010530     C                   ENDSR
009300010530     C***
009400010530
009500010530
009600010530
009700010530
009800010530
009900010530
010000010530     C*--------------------------------------------------------
010100010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010200010530     C*--------------------------------------------------------
010300010530     C     ENDELA        BEGSR
010400010530     C*
010500010530     C*
010600010530     C                   ENDSR
010700010530
010800010530
010900910830     C*--------------------------------------------------------
011000020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
011100910830     C*--------------------------------------------------------
011200941229     C     RWVAB         BEGSR
011300990910     C*
011400990914     C                   if        not %open(tivin00r)
011500990908     C                   open      tivin00r
011600990914     C                   endif
011700020913     C                   if        not %open(fivabwwr)
011800020913     C                   open      fivabwwr
011900990914     C                   endif
012000990910     C*
012100990910     C                   clear                   §CTROK
012200990910     C                   clear                   §CTRMO
012300990910     C                   clear                   §CTRNO
012400990910     C*
012500921023     C                   DO        *HIVAL
012600990913     C*
012700990915     C                   READ      tivin00r                               70
012800010618     C*
012900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
013000010618     C                   if        vinflg = '1'
013100010618     C                   eval      flgOk = '1'
013200010618     C                   endif
013300010705     C*
013400010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
013500010705     C                   if        %subst(vindta:1:1) = '='
013600010705     C                   eval      %subst(vindta:1:1) = *blanks
013700010705     C                   endif
013800010705     C*
013900051017     C                   if        vindta  > *blanks
014000000613     C                   add       1             rrnum
014100010601     C* ...elimino eventuali segni
014200010601     C     '+':' '       xlate     vindta        vindta
014300990913     C*
014400010601     C                   if        *in70 = *off and
014500990913     C                             (vinflg = *blanks
014600010711     C                              or vinflg = '0'
014700010711     C                              or vinflg = '2')
014800990913     C*
014900020913     C                   clear                   FIVAB000
015000010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
015100010711     C                   if        vinflg = *blanks or vinflg = '0'
015200010711     C                   clear                   vinmsg
015300010711     C                   endif
015400010711     C*
015500990913     C                   eval      fnvabaar = vindta
015600991022     C*
015700991022     C                   testb     x'3F'         vhbatb                   02
015800991022     C                   if        not *in02
015900991022     C*
016000971021     C                   SETOFF                                       303132
016100971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
016200971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
016300971023     C*    OPPURE TUTTI BLANKS
016400010530     C*
016500010530     C                   TESTN                   VHBLNP               30
016600010530     C  N30              TESTN                   VHBLNP                 30
016700010530     C  N30              TESTN                   VHBLNP                   30
016800010530     C                   MOVE      VHBLNP        W001A             1
016900010530     C     *IN30         IFEQ      *OFF
017000010530     C     W001A         ORLT      '0'
017100010530     C                   SETON                                        31
017200010530     C                   eval      vinmsg = %trimr(vinmsg)
017300010530     C                             + ' ' + 'VHBLNP'
017400010530     C                   ELSE
017500010530     C                   MOVE      VHBLNP        VABLNP
017600010530     C     VABLNP        IFLE      *ZEROS
017700010530     C                   SETON                                        31
017800010530     C                   eval      vinmsg = %trimr(vinmsg)
017900010530     C                             + ' ' + 'VHBLNP'
018000010530     C                   ENDIF
018100010530     C                   ENDIF
018200010530     C*
018300010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018400010530     C                   eval      depfil = VABLNP
018500010530     C                   exsr      repfil
018600010601     C                   if        depfil = invfil
018700021014     C                   if        vlrpoi = 999
018800021010     C                   MOVE(P)   invfil        VABFGS
018900021014     C                   else
019000021014     C                   MOVE(P)   vlrpoi        VABFGS
019100021014     C                   endif
019200010530     C*
019300010530     C                   exsr      PREELA
019400010530     C*
019500971023     C                   TESTN                   VHBCCM               30
019600971023     C  N30              TESTN                   VHBCCM                 30
019700971023     C  N30              TESTN                   VHBCCM                   30
019800971021     C                   MOVE      VHBCCM        W001A             1
019900971021     C     *IN30         IFEQ      *OFF
020000971021     C     W001A         ORLT      '0'
020100971021     C                   SETON                                        31
020200010604     C                   eval      vinmsg = %trimr(vinmsg)
020300010604     C                             + ' ' + 'VHBCCM'
020400971021     C                   ELSE
020500971021     C                   MOVE      VHBCCM        VABCCM
020600971024     C     VABCCM        IFLE      *ZEROS
020700971024     C                   SETON                                        31
020800010604     C                   eval      vinmsg = %trimr(vinmsg)
020900010604     C                             + ' ' + 'VHBCCM'
021000971024     C                   ENDIF
021100971021     C                   ENDIF
021200971021     C*
021300971023     C                   TESTN                   VHBAAS               30
021400971023     C  N30              TESTN                   VHBAAS                 30
021500971023     C  N30              TESTN                   VHBAAS                   30
021600971021     C                   MOVE      VHBAAS        W001A             1
021700971021     C     *IN30         IFEQ      *OFF
021800971021     C     W001A         ORLT      '0'
021900971023     C     W001A         ANDNE     *BLANK
022000971021     C                   SETON                                        32
022100010606     C                   MOVEL     datcor        VABAAS
022200010604     C                   eval      vinmsg = %trimr(vinmsg)
022300010604     C                             + ' ' + 'VHBAAS'
022400971021     C                   ELSE
022500971021     C                   MOVE      VHBAAS        VABAAS
022600971021     C                   ENDIF
022700971021     C*
022800971023     C                   TESTN                   VHBMGS               30
022900971023     C  N30              TESTN                   VHBMGS                 30
023000971023     C  N30              TESTN                   VHBMGS                   30
023100971021     C                   MOVE      VHBMGS        W001A             1
023200971021     C     *IN30         IFEQ      *OFF
023300971021     C     W001A         ORLT      '0'
023400971023     C     W001A         ANDNE     *BLANK
023500971021     C                   SETON                                        32
023600010601     C                   MOVE      '0101'        VABMGS
023700010604     C                   eval      vinmsg = %trimr(vinmsg)
023800010604     C                             + ' ' + 'VHBMGS'
023900971021     C                   ELSE
024000971021     C                   MOVE      VHBMGS        VABMGS
024100971021     C                   ENDIF
024200971021     C*
024300971023     C                   TESTN                   VHBNRS               30
024400971023     C  N30              TESTN                   VHBNRS                 30
024500971023     C  N30              TESTN                   VHBNRS                   30
024600971021     C                   MOVE      VHBNRS        W001A             1
024700971021     C     *IN30         IFEQ      *OFF
024800971021     C     W001A         ORLT      '0'
024900971023     C     W001A         ANDNE     *BLANK
025000971021     C                   SETON                                        32
025100971021     C                   MOVE      *ZERO         VABNRS
025200010604     C                   eval      vinmsg = %trimr(vinmsg)
025300010604     C                             + ' ' + 'VHBNRS'
025400971021     C                   ELSE
025500971021     C                   MOVE      VHBNRS        VABNRS
025600971021     C                   ENDIF
025700971021     C*
025800971023     C                   TESTN                   VHBNSP               30
025900971023     C  N30              TESTN                   VHBNSP                 30
026000971023     C  N30              TESTN                   VHBNSP                   30
026100971021     C                   MOVE      VHBNSP        W001A             1
026200971021     C     *IN30         IFEQ      *OFF
026300971021     C     W001A         ORLT      '0'
026400971023     C     W001A         ANDNE     *BLANK
026500971021     C                   SETON                                        32
026600971021     C                   MOVE      *ZERO         VABNSP
026700010604     C                   eval      vinmsg = %trimr(vinmsg)
026800010604     C                             + ' ' + 'VHBNSP'
026900971021     C                   ELSE
027000971021     C                   MOVE      VHBNSP        VABNSP
027100971021     C                   ENDIF
027200971021     C*
027300971023     C                   TESTN                   VHBLNA               30
027400971023     C  N30              TESTN                   VHBLNA                 30
027500971023     C  N30              TESTN                   VHBLNA                   30
027600971021     C                   MOVE      VHBLNA        W001A             1
027700971021     C     *IN30         IFEQ      *OFF
027800971021     C     W001A         ORLT      '0'
027900971023     C     W001A         ANDNE     *BLANK
028000971021     C                   SETON                                        32
028100971021     C                   MOVE      *ZERO         VABLNA
028200010604     C                   eval      vinmsg = %trimr(vinmsg)
028300010604     C                             + ' ' + 'VHBLNA'
028400971021     C                   ELSE
028500971021     C                   MOVE      VHBLNA        VABLNA
028600971021     C                   ENDIF
028700971021     C*
028800971023     C                   TESTN                   VHBCTR               30
028900971023     C  N30              TESTN                   VHBCTR                 30
029000971023     C  N30              TESTN                   VHBCTR                   30
029100971021     C                   MOVE      VHBCTR        W001A             1
029200971021     C     *IN30         IFEQ      *OFF
029300971021     C     W001A         ORLT      '0'
029400971023     C     W001A         ANDNE     *BLANK
029500971021     C                   SETON                                        32
029600971021     C                   MOVE      *ZERO         VABCTR
029700010604     C                   eval      vinmsg = %trimr(vinmsg)
029800010604     C                             + ' ' + 'VHBCTR'
029900971021     C                   ELSE
030000971021     C                   MOVE      VHBCTR        VABCTR
030100971021     C                   ENDIF
030200941229     C*
030300971021     C                   MOVE      *ZERO         VABIAS
030400941229     C                   MOVE      VHBIAS        WASC15
030500971023     C                   TESTN                   WI15                 30
030600971023     C  N30              TESTN                   WI15                   30
030700971023     C  N30              TESTN                   WI15                     30
030800971021     C                   MOVE      WI15          W001A             1
030900971021     C     *IN30         IFEQ      *OFF
031000971021     C     W001A         ORLT      '0'
031100971023     C     W001A         ANDNE     *BLANK
031200971021     C                   SETON                                        32
031300971021     C                   MOVE      *ZERO         VABIAS
031400010604     C                   eval      vinmsg = %trimr(vinmsg)
031500010604     C                             + ' ' + 'VHBIAS'
031600971021     C                   ELSE
031700971021     C                   MOVEL     WI15          VABIAS
031800971023     C                   TESTN                   WD15                 30
031900971023     C  N30              TESTN                   WD15                   30
032000971023     C  N30              TESTN                   WD15                     30
032100971021     C                   MOVE      WD15          W001A             1
032200971021     C     *IN30         IFEQ      *OFF
032300971021     C     W001A         ORLT      '0'
032400971023     C     W001A         ANDNE     *BLANK
032500971021     C                   SETON                                        32
032600971021     C                   MOVE      *ZERO         VABIAS
032700010604     C                   eval      vinmsg = %trimr(vinmsg)
032800010604     C                             + ' ' + 'VHBIAS'
032900971021     C                   ELSE
033000971021     C                   MOVE      WD15          VABIAS
033100971021     C                   ENDIF
033200050118     C                   ENDIF
033300941229     C*
033400971023     C                   TESTN                   VHBNCL               30
033500971023     C  N30              TESTN                   VHBNCL                 30
033600971023     C  N30              TESTN                   VHBNCL                   30
033700971021     C                   MOVE      VHBNCL        W001A             1
033800971021     C     *IN30         IFEQ      *OFF
033900971021     C     W001A         ORLT      '0'
034000971021     C                   SETON                                        32
034100971021     C                   MOVE      *ZERO         VABNCL
034200010604     C                   eval      vinmsg = %trimr(vinmsg)
034300010604     C                             + ' ' + 'VHBNCL'
034400971021     C                   ELSE
034500971021     C                   MOVE      VHBNCL        VABNCL
034600971021     C                   ENDIF
034700941229     C*
034800971021     C                   MOVE      *ZERO         VABPKB
034900971021     C                   MOVE      VHBPKB        WASC09
035000010719     C                   IF        WI09 = *blanks
035100010719     C                   EVAL      WI09 = *zeros
035200010719     C                   ENDIF
035300971023     C                   TESTN                   WI09                 30
035400971023     C  N30              TESTN                   WI09                   30
035500971023     C  N30              TESTN                   WI09                     30
035600971021     C                   MOVE      WI09          W001A             1
035700971021     C     *IN30         IFEQ      *OFF
035800971021     C     W001A         ORLT      '0'
035900971021     C                   SETON                                        32
036000971021     C                   MOVE      *ZERO         VABPKB
036100010604     C                   eval      vinmsg = %trimr(vinmsg)
036200010604     C                             + ' ' + 'VHBPKB'
036300971021     C                   ELSE
036400971021     C                   MOVEL     WI09          VABPKB
036500010719     C                   IF        WD09 = *blanks
036600010719     C                   EVAL      WD09 = *zeros
036700010719     C                   ENDIF
036800971023     C                   TESTN                   WD09                 30
036900971023     C  N30              TESTN                   WD09                     30
037000971021     C                   MOVE      WD09          W001A             1
037100971021     C     *IN30         IFEQ      *OFF
037200971021     C     W001A         ORLT      '0'
037300971023     C     W001A         ANDNE     *BLANK
037400971021     C                   SETON                                        32
037500971021     C                   MOVE      *ZERO         VABPKB
037600010604     C                   eval      vinmsg = %trimr(vinmsg)
037700010604     C                             + ' ' + 'VHBPKB'
037800971021     C                   ELSE
037900971021     C                   MOVE      WD09          VABPKB
038000971021     C                   ENDIF
038100050118     C                   ENDIF
038200971021     C*
038300041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
038400041001     C                   IF        *IN51 = *OFF
038500971021     C                   MOVE      *ZERO         VABVLB
038600971021     C                   MOVE      VHBVLB        WASC07
038700010719     C                   IF        WI07 = *blanks
038800010719     C                   EVAL      WI07 = *zeros
038900010719     C                   ENDIF
039000971023     C                   TESTN                   WI07                 30
039100971023     C  N30              TESTN                   WI07                   30
039200971023     C  N30              TESTN                   WI07                     30
039300971021     C                   MOVE      WI07          W001A             1
039400971021     C     *IN30         IFEQ      *OFF
039500971021     C     W001A         ORLT      '0'
039600971023     C     W001A         ANDNE     *BLANK
039700971021     C                   SETON                                        32
039800971021     C                   MOVE      *ZERO         VABVLB
039900010604     C                   eval      vinmsg = %trimr(vinmsg)
040000010604     C                             + ' ' + 'VHBVLB'
040100971021     C                   ELSE
040200971021     C                   MOVEL     WI07          VABVLB
040300010719     C                   IF        WD07 = *blanks
040400010719     C                   EVAL      WD07 = *zeros
040500010719     C                   ENDIF
040600971023     C                   TESTN                   WD07                 30
040700971023     C  N30              TESTN                   WD07                   30
040800971023     C  N30              TESTN                   WD07                     30
040900971021     C                   MOVE      WD07          W001A             1
041000971021     C     *IN30         IFEQ      *OFF
041100971021     C     W001A         ORLT      '0'
041200971023     C     W001A         ANDNE     *BLANK
041300971021     C                   SETON                                        32
041400971021     C                   MOVE      *ZERO         VABVLB
041500010604     C                   eval      vinmsg = %trimr(vinmsg)
041600010604     C                             + ' ' + 'VHBVLB'
041700971021     C                   ELSE
041800971021     C                   MOVE      WD07          VABVLB
041900971021     C                   ENDIF
042000050118     C                   ENDIF
042100041001     C                   ELSE
042200041001     C* ...altrimenti forzo azzeramento del volume
042300041001     C                   Z-ADD     *zeros        VABVLB
042400041001     C                   ENDIF
042500941229     C*
042600971021     C                   MOVE      *ZERO         VABQFT
042700971021     C                   MOVE      VHBQFT        WASC15
042800010719     C                   IF        WI15 = *blanks
042900010719     C                   EVAL      WI15 = *zeros
043000010719     C                   ENDIF
043100971023     C                   TESTN                   WI15                 30
043200971023     C  N30              TESTN                   WI15                   30
043300971023     C  N30              TESTN                   WI15                     30
043400971021     C                   MOVE      WI15          W001A             1
043500971021     C     *IN30         IFEQ      *OFF
043600971021     C     W001A         ORLT      '0'
043700971023     C     W001A         ANDNE     *BLANK
043800971021     C                   SETON                                        32
043900971021     C                   MOVE      *ZERO         VABQFT
044000010604     C                   eval      vinmsg = %trimr(vinmsg)
044100010604     C                             + ' ' + 'VHBQFT'
044200971021     C                   ELSE
044300971021     C                   MOVEL     WI15          VABQFT
044400010719     C                   IF        WD15 = *blanks
044500010719     C                   EVAL      WD15 = *zeros
044600010719     C                   ENDIF
044700971023     C                   TESTN                   WD15                 30
044800971023     C  N30              TESTN                   WD15                   30
044900971023     C  N30              TESTN                   WD15                     30
045000971021     C                   MOVE      WD15          W001A             1
045100971021     C     *IN30         IFEQ      *OFF
045200971021     C     W001A         ORLT      '0'
045300971023     C     W001A         ANDNE     *BLANK
045400971021     C                   SETON                                        32
045500971021     C                   MOVE      *ZERO         VABQFT
045600010604     C                   eval      vinmsg = %trimr(vinmsg)
045700010604     C                             + ' ' + 'VHBQFT'
045800971021     C                   ELSE
045900971021     C                   MOVE      WD15          VABQFT
046000971021     C                   ENDIF
046100050118     C                   ENDIF
046200941229     C*
046300971021     C                   MOVE      *ZERO         VABCAS
046400060410     C* Se presente separatore decimale alla giusta posizione => forzo sempre la virgola
046500060410     C                   EVAL      wVHBCAS = VHBCAS
046600060410     C                   IF        %subst(wVHBCAS:12:1) = '.'
046700060410     C                   EVAL      %subst(wVHBCAS:12:1) = ','
046800060410     C                   ENDIF
046900060410     C*
047000060410     C                   MOVEL(P)  wVHBCAS       PiStr
047100060410     C                   EXSR      CHKNUM
047200060410     C                   IF        PiNum=*on
047300060410     C                   Z-ADD     PiVal         VABCAS
047400060410     C                   ELSE
047500060410     C                   SETON                                        32
047600060410     C                   Z-ADD     *zeros        VABCAS
047700060410     C                   EVAL      vinmsg = %trimr(vinmsg)
047800060410     C                             + ' ' + 'VHBCAS'
047900060410     C                   ENDIF
048000971021     C*
048100971023     C                   TESTN                   VHBRMN               30
048200971023     C  N30              TESTN                   VHBRMN                 30
048300971023     C  N30              TESTN                   VHBRMN                   30
048400971021     C                   MOVE      VHBRMN        W001A             1
048500971021     C     *IN30         IFEQ      *OFF
048600971021     C     W001A         ORLT      '0'
048700971021     C                   SETON                                        32
048800971021     C                   MOVE      *ZERO         VABRMN
048900010604     C                   eval      vinmsg = %trimr(vinmsg)
049000010604     C                             + ' ' + 'VHBRMN'
049100971021     C                   ELSE
049200971021     C                   MOVE      VHBRMN        VABRMN
049300971021     C                   ENDIF
049400971021     C*
049500971023     C                   TESTN                   VHBNCD               30
049600971023     C  N30              TESTN                   VHBNCD                 30
049700971023     C  N30              TESTN                   VHBNCD                   30
049800971021     C                   MOVE      VHBNCD        W001A             1
049900971021     C     *IN30         IFEQ      *OFF
050000971021     C     W001A         ORLT      '0'
050100971023     C     W001A         ANDNE     *BLANK
050200971021     C                   SETON                                        32
050300971021     C                   MOVE      *ZERO         VABNCD
050400010604     C                   eval      vinmsg = %trimr(vinmsg)
050500010604     C                             + ' ' + 'VHBNCD'
050600971021     C                   ELSE
050700971021     C                   MOVE      VHBNCD        VABNCD
050800971021     C                   ENDIF
050900971021     C*
051000971023     C                   TESTN                   VHBNCA               30
051100971023     C  N30              TESTN                   VHBNCA                 30
051200971023     C  N30              TESTN                   VHBNCA                   30
051300971021     C                   MOVE      VHBNCA        W001A             1
051400971021     C     *IN30         IFEQ      *OFF
051500971021     C     W001A         ORLT      '0'
051600971023     C     W001A         ANDNE     *BLANK
051700971021     C                   SETON                                        32
051800971021     C                   MOVE      *ZERO         VABNCA
051900010604     C                   eval      vinmsg = %trimr(vinmsg)
052000010604     C                             + ' ' + 'VHBNCA'
052100971021     C                   ELSE
052200971021     C                   MOVE      VHBNCA        VABNCA
052300971021     C                   ENDIF
052400971021     C*
052500971023     C                   TESTN                   VHBZNC               30
052600971023     C  N30              TESTN                   VHBZNC                 30
052700971023     C  N30              TESTN                   VHBZNC                   30
052800971021     C                   MOVE      VHBZNC        W001A             1
052900971021     C     *IN30         IFEQ      *OFF
053000971021     C     W001A         ORLT      '0'
053100971023     C     W001A         ANDNE     *BLANK
053200971021     C                   SETON                                        32
053300971021     C                   MOVE      *ZERO         VABZNC
053400010604     C                   eval      vinmsg = %trimr(vinmsg)
053500010604     C                             + ' ' + 'VHBZNC'
053600971021     C                   ELSE
053700971021     C                   MOVE      VHBZNC        VABZNC
053800971021     C                   ENDIF
053900971021     C*
054000971023     C                   TESTN                   VHBDCR               30
054100971023     C  N30              TESTN                   VHBDCR                 30
054200971023     C  N30              TESTN                   VHBDCR                   30
054300971021     C                   MOVE      VHBDCR        W001A             1
054400971021     C     *IN30         IFEQ      *OFF
054500971021     C     W001A         ORLT      '0'
054600971023     C     W001A         ANDNE     *BLANK
054700971021     C                   SETON                                        32
054800971021     C                   MOVE      *ZERO         VABDCR
054900010604     C                   eval      vinmsg = %trimr(vinmsg)
055000010604     C                             + ' ' + 'VHBDCR'
055100971021     C                   ELSE
055200971021     C                   MOVE      VHBDCR        VABDCR
055300971021     C                   ENDIF
055400971021     C*
055500971023     C                   TESTN                   VHBHCR               30
055600971023     C  N30              TESTN                   VHBHCR                 30
055700971023     C  N30              TESTN                   VHBHCR                   30
055800971021     C                   MOVE      VHBHCR        W001A             1
055900971021     C     *IN30         IFEQ      *OFF
056000971021     C     W001A         ORLT      '0'
056100971023     C     W001A         ANDNE     *BLANK
056200971021     C                   SETON                                        32
056300971021     C                   MOVE      *ZERO         VABHCR
056400010604     C                   eval      vinmsg = %trimr(vinmsg)
056500010604     C                             + ' ' + 'VHBHCR'
056600971021     C                   ELSE
056700971021     C                   MOVE      VHBHCR        VABHCR
056800971021     C                   ENDIF
056900941229     C*
057000971021     C                   MOVE      *ZERO         VABVMD
057100971021     C                   MOVE      VHBVMD        WASC15
057200010719     C                   IF        WI15 = *blanks
057300010719     C                   EVAL      WI15 = *zeros
057400010719     C                   ENDIF
057500971023     C                   TESTN                   WI15                 30
057600971023     C  N30              TESTN                   WI15                   30
057700971023     C  N30              TESTN                   WI15                     30
057800971021     C                   MOVE      WI15          W001A             1
057900971021     C     *IN30         IFEQ      *OFF
058000971021     C     W001A         ORLT      '0'
058100971023     C     W001A         ANDNE     *BLANK
058200971021     C                   SETON                                        32
058300971021     C                   MOVE      *ZERO         VABVMD
058400010604     C                   eval      vinmsg = %trimr(vinmsg)
058500010604     C                             + ' ' + 'VHBVMD'
058600971021     C                   ELSE
058700971021     C                   MOVEL     WI15          VABVMD
058800010719     C                   IF        WD15 = *blanks
058900010719     C                   EVAL      WD15 = *zeros
059000010719     C                   ENDIF
059100971023     C                   TESTN                   WD15                 30
059200971023     C  N30              TESTN                   WD15                   30
059300971023     C  N30              TESTN                   WD15                     30
059400971021     C                   MOVE      WD15          W001A             1
059500971021     C     *IN30         IFEQ      *OFF
059600971021     C     W001A         ORLT      '0'
059700971023     C     W001A         ANDNE     *BLANK
059800971021     C                   SETON                                        32
059900971021     C                   MOVE      *ZERO         VABVMD
060000010604     C                   eval      vinmsg = %trimr(vinmsg)
060100010604     C                             + ' ' + 'VHBVMD'
060200971021     C                   ELSE
060300971021     C                   MOVE      WD15          VABVMD
060400971021     C                   ENDIF
060500050118     C                   ENDIF
060600971021     C*
060700971023     C                   TESTN                   VHBANT               30
060800971023     C  N30              TESTN                   VHBANT                 30
060900971023     C  N30              TESTN                   VHBANT                   30
061000971021     C                   MOVE      VHBANT        W001A             1
061100971021     C     *IN30         IFEQ      *OFF
061200971021     C     W001A         ORLT      '0'
061300971023     C     W001A         ANDNE     *BLANK
061400971021     C                   SETON                                        32
061500971021     C                   MOVE      *ZERO         VABANT
061600010604     C                   eval      vinmsg = %trimr(vinmsg)
061700010604     C                             + ' ' + 'VHBANT'
061800971021     C                   ELSE
061900971021     C                   MOVE      VHBANT        VABANT
062000971021     C                   ENDIF
062100971021     C*
062200971021     C* CAMPI ALFABETICI:
062300971021     C                   MOVE      VHBATB        VABATB
062400971021     C                   MOVE      VHBCBO        VABCBO
062500971021     C                   MOVE      VHBRSD        VABRSD
062600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
062700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
062800020117     C* ==
062900971021     C                   MOVE      VHBRD2        VABRD2
063000971021     C                   MOVE      VHBIND        VABIND
063100971021     C                   MOVE      VHBCAD        VABCAD
063200971021     C                   MOVE      VHBLOD        VABLOD
063300971021     C                   MOVE      VHBPRD        VABPRD
063400971021     C                   MOVE      VHBNZD        VABNZD
063500971021     C                   MOVE      VHBGC1        VABGC1
063600971021     C                   MOVE      VHBGC2        VABGC2
063700971021     C                   MOVE      VHBTSP        VABTSP
063800971021     C                   MOVE      VHBVAS        VABVAS
063900971021     C                   MOVE      VHBNAS        VABNAS
064000971021     C                   MOVE      VHBTIC        VABTIC
064100971021     C                   MOVE      VHBVCA        VABVCA
064200971021     C                   MOVE      VHBGCA        VABGCA
064300971021     C                   MOVE      VHBRMA        VABRMA
064400971021     C                   MOVE      VHBXCO        VABXCO
064500971021     C                   MOVE      VHBNOT        VABNOT
064600971021     C                   MOVE      VHBNT2        VABNT2
064700971021     C                   MOVE      VHBCTM        VABCTM
064800971021     C                   MOVE      VHBFFD        VABFFD
064900971021     C                   MOVE      VHBTCR        VABTCR
065000971021     C                   MOVE      VHBCTS        VABCTS
065100971021     C                   MOVE      VHBFTM        VABFTM
065200971021     C                   MOVE      VHBVAD        VABVAD
065300971021     C                   MOVE      VHBGMA        VABGMA
065400971021     C                   MOVE      VHBGGA        VABGGA
065500971021     C                   MOVE      VHBGVA        VABGVA
065600971021     C                   MOVE      VHBTC1        VABTC1
065700971021     C                   MOVE      VHBTC2        VABTC2
065800971021     C                   MOVE      VHBSCL        VABSCL
065900971021     C                   MOVE      VHBRMO        VABRMO
066000971021     C                   MOVE      VHBCMO        VABCMO
066100971021     C                   MOVE      VHBNMO        VABNMO
066200971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
066300060329     C* NELLA NAZIONE MITTENTE ORIGINALE
066400060331     C                   IF        %trim(VHBNMO) < 'A'
066500060329     C                   MOVE      *BLANKS       VABNMO
066600060329     C                   ENDIF
066700050418     C*
066800050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
066900050418     C                   IF        %trim(VABNZD) = 'I'   OR
067000050418     C                             %trim(VABNZD) = 'IT'  OR
067100050418     C                             %trim(VABNZD) = 'ITA'
067200050418     C                   EVAL      VABNZD = *blanks
067300050418     C                   ENDIF
067400041025     C*
067500041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
067600041025     C                   IF        *IN52 = *ON
067700041025     C* Reperisco la provincia dal CAP e dalla località
067800041025     C                   IF        VABCAD <> *blanks AND
067900050907     C                             VABPRD  = *blanks AND
068000050907     C                             VABNZD  = *blanks
068100041025     C                   CLEAR                   TISI95DS
068200041025     C                   EVAL      I95TCN = '3'
068300041025     C                   Z-ADD     datcor        I95DAT
068400041025     C                   EVAL      I95CAP = VABCAD
068500041025     C                   EVAL      I95LOC = VABLOD
068600041025     C                   CALL      'TISI95R'
068700041025     C                   PARM                    TISI95DS
068800041025     C                   EVAL      VABPRD = O95PRV
068900041025     C                   ENDIF
069000041025     C                   ENDIF
069100040630     C*
069200040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
069300040630     C                   exsr      exeBon
069400100419     C*
069500100419     C* Forzatura su campi DIVISE
069600100419     C                   IF        VABVAS = *blanks
069700100419     C                   EVAL      VABVAS = 'EUR'
069800100419     C                   ENDIF
069900100419     C                   IF        VABVCA = *blanks
070000100419     C                   EVAL      VABVCA = 'EUR'
070100100419     C                   ENDIF
070200100419     C                   IF        VABVAD = *blanks
070300100419     C                   EVAL      VABVAD = 'EUR'
070400100419     C                   ENDIF
070500111102     C*
070600111102     C* Forzature da parametri d anagrafica
070700111102     C                   if        %subst(vlrppt:4:2) <> *blanks
070800111102     C                   eval      VABTIC = %subst(vlrppt:4:2)
070900111102     C                   endif
071000120130     C*
071100120130     C* Forzature CODICE BOLLA
071200121217     C***                eval      VABCBO = '1'
071300930409     C*
071400010604     C  N31              ADD       1             §CTROK            7 0
071500010604     C   32              ADD       1             §CTRMO            7 0
071600010604     C   31              ADD       1             §CTRNO            7 0
071700020913     C  N31              WRITE     FIVAB000
071800990910     C*
071900010601     C                   if        *in31 = *off and
072000010601     C                             *in32 = *off
072100990910     C                   eval      vinflg = '1'
072200990910     C                   else
072300990910     C                   eval      vinflg = '2'
072400990910     C                   endif
072500991022     C                   endif
072600990910     C                   endif
072700010601     C*
072800010601     C                   endif
072900000905     C*
073000000905     C                   else
073100000905     C                   eval      vinflg = '1'
073200000905     C                   endif
073300000905     C*
073400000905     C  N70              update    tivin000
073500991022     C*
073600991022     C  N70              ENDdo
073700010530     C
073800010530     C*
073900010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
074000010530     C                   if        cntNonEl = *zeros or
074100010530     C                             flgMulti = '0'
074200990910     C* Se non ci sono record con errori ...
074300010530     C                   if        §ctrno = 0 and
074400010601     C                             §ctrmo = 0 and
074500010530     C                             flgStato <> '2'
074600990910     C* ... restituisco esito OK.
074700990921     C                   eval      wrkesito = '0'
074800990910     C                   else
074900010601     C                   if        §ctrok > 0
075000990921     C                   eval      wrkesito = '1'
075100000710     C                   else
075200010615     C                   if        flgOk = '0'
075300010615     C                   eval      wrkesito = '2'
075400010615     C                   else
075500010615     C                   eval      wrkesito = '6'
075600010615     C                   endif
075700990910     C                   endif
075800000710     C                   endif
075900010530     C                   else
076000010530     C                   eval      wrkesito = '9'
076100010530     C                   endif
076200990910     C*
076300990914     C                   if        %open(tivin00r)
076400990908     C                   close     tivin00r
076500990914     C                   endif
076600020913     C                   if        %open(fivabwwr)
076700020913     C                   close     fivabwwr
076800990914     C                   endif
076900010530     C*
077000010530     C                   if        vlrpoi <> 999
077100010530     C                   eval      invfil = vlrpoi
077200010530     C                   endif
077300990910     C*
077400990920     C                   if        §ctrok > 0
077500010530     C                             and invfil > *zeros
077600000613     C                   exsr      invio
077700990920     C                   endif
077800010612     C*
077900010612     C                   if        flgGiro = '1'
078000010612     C                   exsr      endela
078100010612     C                   endif
078200990920     C*
078300910830     C                   ENDSR
078400000613     C***
078500990920
078600010530
078700010530
078800010530
078900010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
079000010530     C     repfil        BEGSR
079100010530     C*
079200010530     C                   if        invfil = *zeros and
079300010530     C                             depfil > *zeros and
079400010613     C                             (vinflg = *blanks or
079500010613     C                              vinflg = *zeros)
079600010530     C
079700010530     C                   eval      invfil = depfil
079800010530     C                   endif
079900010530     C*
080000010530     C                   if        depfil <> invfil and
080100010530     C                             invfil > *zeros
080200010530     C                   eval      flgMulti = '1'
080300010530     C                   if        vinflg = *blanks
080400010530     C                   add       1             cntNonEl
080500010530     C                   endif
080600010530     C                   endif
080700010530     C*
080800010530     C                   if        vinflg = '2'
080900010530     C                   eval      flgStato = '2'
081000010530     C                   endif
081100010530     C*
081200010530     C                   ENDSR
081300010530     C***
081400010530
081500010530
081600010530
081700010530
081800990920      /TITLE Invio dei dati al punto operativo.
081900000613     C     invio         BEGSR
082000990920     C*
082100990920     C                   reset                   dscmz
082200010530     C                   move      invfil        cmzdst
082300990920     C                   eval      cmzfld = vlrfou
082400990920     C                   eval      cmzmbd = vlrhdl
082500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
082600000710     C                   if        prmfir = *blanks
082700020913     C                   eval      cmzfla = 'FIVAB00F'
082800020913     C                   eval      cmzmba = 'FIVAB00F'
082900000710     C                   else
083000000710     C                   eval      cmzfla = prmfir
083100000710     C                   eval      cmzmba = prmfir
083200000710     C                   endif
083300990920     C                   eval      cmznrr = *zeros
083400990920     C                   move      §ctrok        cmznrr
083500021003     C                   eval      cmzlba = vlrfl1
083600990920     C                   call(e)   'TIS711C'
083700990920     C                   parm                    dscmz
083800990921     C                   parm      *blanks       esito
083900990920     C                   if        %error
084000990920     C                             or cmzerr = '1'
084100990921     C                             or esito  = '1'
084200000710     C                   eval      wrkesito = '3'
084300990920     C                   endif
084400990920     C*
084500000613     C                   ENDSR
084600000613     C***
084700010530
084800010530
084900010530
085000010530
085100010530      /TITLE Invio dei dati al punto operativo.
085200010530     C     opeini        BEGSR
085300010530     C*
085400010530     C* Inizializzo flag e contatori operativi
085500010530     C                   movel     '0'           flgGiro           1
085600010530     C                   movel     '0'           flgMulti          1
085700010530     C                   movel     '1'           flgStato          1
085800010615     C                   movel     '0'           flgOk             1
085900010530     C                   z-add     *zeros        cntNonEl         10 0
086000010530     C                   z-add     *zeros        depfil            3 0
086100010530     C                   z-add     *zeros        invfil            3 0
086200041001     C*
086300041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
086400041001     C                   if        %subst(vlrppt:2:1) = 'S'
086500041001     C                   seton                                        51
086600041001     C                   else
086700041001     C                   setoff                                       51
086800041001     C                   endif
086900041025     C*
087000041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
087100041025     C* (naturalmente solo se nn già presente)
087200041025     C                   if        %subst(vlrppt:3:1) = 'S'
087300041025     C                   seton                                        52
087400041025     C                   else
087500041025     C                   setoff                                       52
087600041025     C                   endif
087700010530     C*
087800010530     C                   ENDSR
087900010530     C***
088000040407
088100040407
088200040407
088300040407
088400040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
088500040407     C     exeBon        BEGSR
088600040407     C*
088700040407     C* Imposto i parametri x il *pgm d "bonifica"
088800040407     C                   EVAL      ElencoChar = *blanks
088900040407     C                   EVAL      TipoElenco = *blanks
089000040407     C                   EVAL      CharSost   = *blanks
089100040629     C                   EVAL      UpperCase  = '1'
089200040407     C                   EVAL      ChkNull    = '1'
089300040407     C                   EVAL      CharNull   = *blanks
089400040629     C*
089500040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
089600040629     C                   EVAL      TxtInOut   = VABRSD
089700040629     C                   EVAL      EsitoBon   = *blanks
089800040407     C*
089900040407     C                   CALL      'XCHKCHAR'
090000040407     C                   PARM                    TxtInOut
090100040407     C                   PARM                    ElencoChar
090200040407     C                   PARM                    TipoElenco
090300040407     C                   PARM                    CharSost
090400040407     C                   PARM                    UpperCase
090500040407     C                   PARM                    ChkNull
090600040407     C                   PARM                    CharNull
090700040407     C                   PARM                    EsitoBon
090800040407     C*
090900040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
091000040407     C                   IF        EsitoBon = '1'
091100040629     C                   EVAL      VABRSD = TxtInOut
091200040407     C                   ENDIF
091300040629     C*
091400040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
091500040629     C                   EVAL      TxtInOut   = VABIND
091600040629     C                   EVAL      EsitoBon   = *blanks
091700040629     C*
091800040629     C                   CALL      'XCHKCHAR'
091900040629     C                   PARM                    TxtInOut
092000040629     C                   PARM                    ElencoChar
092100040629     C                   PARM                    TipoElenco
092200040629     C                   PARM                    CharSost
092300040629     C                   PARM                    UpperCase
092400040629     C                   PARM                    ChkNull
092500040629     C                   PARM                    CharNull
092600040629     C                   PARM                    EsitoBon
092700040629     C*
092800040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
092900040629     C                   IF        EsitoBon = '1'
093000040629     C                   EVAL      VABIND = TxtInOut
093100040629     C                   ENDIF
093200040629     C*
093300040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
093400040629     C                   EVAL      TxtInOut   = VABLOD
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
094900040629     C                   EVAL      VABLOD = TxtInOut
095000040629     C                   ENDIF
095100040629     C*
095200040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
095300040629     C                   EVAL      TxtInOut   = VABNOT
095400040629     C                   EVAL      EsitoBon   = *blanks
095500040630     C                   EVAL      UpperCase  = *blanks
095600040629     C*
095700040629     C                   CALL      'XCHKCHAR'
095800040629     C                   PARM                    TxtInOut
095900040629     C                   PARM                    ElencoChar
096000040629     C                   PARM                    TipoElenco
096100040629     C                   PARM                    CharSost
096200040629     C                   PARM                    UpperCase
096300040629     C                   PARM                    ChkNull
096400040629     C                   PARM                    CharNull
096500040629     C                   PARM                    EsitoBon
096600040629     C*
096700040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
096800040629     C                   IF        EsitoBon = '1'
096900040629     C                   EVAL      VABNOT = TxtInOut
097000040629     C                   ENDIF
097100040629     C*
097200040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
097300040629     C                   EVAL      TxtInOut   = VABNT2
097400040629     C                   EVAL      EsitoBon   = *blanks
097500040630     C                   EVAL      UpperCase  = *blanks
097600040629     C*
097700040629     C                   CALL      'XCHKCHAR'
097800040629     C                   PARM                    TxtInOut
097900040629     C                   PARM                    ElencoChar
098000040629     C                   PARM                    TipoElenco
098100040629     C                   PARM                    CharSost
098200040629     C                   PARM                    UpperCase
098300040629     C                   PARM                    ChkNull
098400040629     C                   PARM                    CharNull
098500040629     C                   PARM                    EsitoBon
098600040629     C*
098700040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
098800040629     C                   IF        EsitoBon = '1'
098900040629     C                   EVAL      VABNT2 = TxtInOut
099000040629     C                   ENDIF
099100040407     C*
099200040407     C                   ENDSR
099300010530
099400060410
099500060410     C*----------------------------------------------------*
099600060410     C*  CONTROLLO NUMERICITA' CAMPI
099700060410     C*----------------------------------------------------*
099800060410     C     CHKNUM        BEGSR
099900060410     C*
100000060410     C                   IF        PiDecChr = *blanks
100100060410     C                   EVAL      PiDecChr = ','
100200060410     C                   ENDIF
100300060410     C*
100400060410     C                   CALL(e)   'ISNUMERIC'
100500060410     C                   PARM                    PiStr            30
100600060410     C                   PARM                    PiDecChr          1
100700060410     C                   PARM      *ZEROS        PiVal            30 9
100800060410     C                   PARM      '0'           PiInt             1
100900060410     C                   PARM      '0'           PiNum             1
101000060410     C                   IF        %error
101100060410     C                   EVAL      PiNum=*off
101200060410     C                   ENDIF
101300060410     C*
101400060410     C                   ENDSR
101500060410     C***
101600060410
101700990910
101800000613     C     *inzsr        BEGSR
101900990910     C*
102000990910     C     *entry        plist
102100990920     C                   parm                    tivlrds
102200990921     C                   parm      wrkesito      esito
102300000724     C                   parm                    prmlit
102400000710     C                   parm                    prmfir
102500010606     C*
102600010606     C* CALCOLA LA DATA CORRENTE
102700010606     C                   time                    wn14             14 0
102800010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
102900010606     C                   z-add     wn8           g08dat
103000010606     C                   z-add     *zeros        g08inv
103100010606     C                   movel     '0'           g08err
103200010606     C                   call      'XSRDA8'
103300010606     C                   parm                    wlbda8
103400010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
103500000613     C*
103600000613     C                   ENDSR
103700000613     C***
103800990908
