000100110126      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500110126     FEDIVABwr  O    E             DISK    usropn
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
004200110126     D wNomeFile       s             30    INZ(*blanks)
004300010606     D*------------------
004400010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
004500010606     D*------------------
004600010606     D WLBDA8          DS                  INZ
004700010606     D  G08DAT                 1      8  0
004800010606     D  G08INV                 9     16  0
004900010606     D  G08ERR                17     17
005000010606     D  G08TGI                18     22  0
005100010606     D*
005200040407     D TxtInOut        S           2048
005300040407     D ElencoChar      S            256
005400040407     D TipoElenco      S              1
005500040407     D CharSost        S              1
005600040407     D UpperCase       S              1
005700040407     D ChkNull         S              1
005800040407     D CharNull        S              1
005900040407     D EsitoBon        S              1
006000990908
006100000913     C                   reset                   rrnum
006200990921     C                   reset                   esito
006300990921     C                   reset                   wrkesito
006400010530     C*
006500010530     C                   exsr      opeini
006600010530     C                   exsr      rwvab
006700041025     C*
006800041025     C* Esegue lancio TISI95R solo x chiusura
006900041025     C                   CLEAR                   TISI95DS
007000041025     C                   EVAL      I95TLA = 'C'
007100041025     C                   CALL      'TISI95R'
007200041025     C                   PARM                    TISI95DS
007300010530     C*
007400010530     C                   seton                                        lr
007500010530
007600010530
007700010530
007800010530
007900010530     C*--------------------------------------------------------
008000010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008100010530     C*--------------------------------------------------------
008200010530     C     PREELA        BEGSR
008300000724     C*
008400000724     C* SE OCCORRE SPEDIRE IN FILIALE
008500010530     C                   if        invfil <> *zeros and
008600010530     C                             flgGiro = '0'
008700010530     C*
008800010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008900010530     C                   eval      flgGiro = '1'
009000010530     C*
009100010530     C                   endif
009200010530     C*
009300010530     C                   ENDSR
009400010530     C***
009500010530
009600010530
009700010530
009800010530
009900010530
010000010530
010100010530     C*--------------------------------------------------------
010200010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010300010530     C*--------------------------------------------------------
010400010530     C     ENDELA        BEGSR
010500010530     C*
010600010530     C                   ENDSR
010700010530     C***
010800990908
010900010530
011000010530
011100010530
011200910830     C*--------------------------------------------------------
011300110126     C* RWVAB   LEGGE tivin00r E SCRIVE EDIVABWF              *
011400910830     C*--------------------------------------------------------
011500941229     C     RWVAB         BEGSR
011600990910     C*
011700990914     C                   if        not %open(tivin00r)
011800990908     C                   open      tivin00r
011900990914     C                   endif
012000110126     C                   if        not %open(edivabwr)
012100110126     C                   open      edivabwr
012200990914     C                   endif
012300990910     C*
012400990910     C                   clear                   §CTROK
012500990910     C                   clear                   §CTRMO
012600990910     C                   clear                   §CTRNO
012700990910     C*
012800921023     C                   DO        *HIVAL
012900990913     C*
013000990915     C                   READ      tivin00r                               70
013100010618     C*
013200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
013300010618     C                   if        vinflg = '1'
013400010618     C                   eval      flgOk = '1'
013500010618     C                   endif
013600010705     C*
013700010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
013800010705     C                   if        %subst(vindta:1:1) = '='
013900010705     C                   eval      %subst(vindta:1:1) = *blanks
014000010705     C                   endif
014100010705     C*
014200051017     C                   if        vindta  > *blanks
014300000613     C                   add       1             rrnum
014400010601     C* ...elimino eventuali segni
014500010601     C     '+':' '       xlate     vindta        vindta
014600990913     C*
014700010601     C                   if        *in70 = *off and
014800990913     C                             (vinflg = *blanks
014900010711     C                              or vinflg = '0'
015000010711     C                              or vinflg = '2')
015100990913     C*
015200110126     C                   clear                   EDIVAB00
015300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
015400010711     C                   if        vinflg = *blanks or vinflg = '0'
015500010711     C                   clear                   vinmsg
015600010711     C                   endif
015700010711     C*
015800990913     C                   eval      fnvabaar = vindta
015900991022     C*
016000991022     C                   testb     x'3F'         vhbatb                   02
016100991022     C                   if        not *in02
016200991022     C*
016300971021     C                   SETOFF                                       303132
016400971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
016500971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
016600971023     C*    OPPURE TUTTI BLANKS
016700010530     C*
016800010530     C                   TESTN                   VHBLNP               30
016900010530     C  N30              TESTN                   VHBLNP                 30
017000010530     C  N30              TESTN                   VHBLNP                   30
017100010530     C                   MOVE      VHBLNP        W001A             1
017200010530     C     *IN30         IFEQ      *OFF
017300010530     C     W001A         ORLT      '0'
017400010530     C                   SETON                                        31
017500010530     C                   eval      vinmsg = %trimr(vinmsg)
017600010530     C                             + ' ' + 'VHBLNP'
017700010530     C                   ELSE
017800010530     C                   MOVE      VHBLNP        VABLNP
017900010530     C     VABLNP        IFLE      *ZEROS
018000010530     C                   SETON                                        31
018100010530     C                   eval      vinmsg = %trimr(vinmsg)
018200010530     C                             + ' ' + 'VHBLNP'
018300010530     C                   ENDIF
018400010530     C                   ENDIF
018500070115     C*
018600070115     C                   TESTN                   VHBCCM               30
018700070115     C  N30              TESTN                   VHBCCM                 30
018800070115     C  N30              TESTN                   VHBCCM                   30
018900070115     C                   MOVE      VHBCCM        W001A             1
019000070115     C     *IN30         IFEQ      *OFF
019100070115     C     W001A         ORLT      '0'
019200070115     C                   SETON                                        31
019300070115     C                   eval      vinmsg = %trimr(vinmsg)
019400070115     C                             + ' ' + 'VHBCCM'
019500070115     C                   ELSE
019600070115     C                   MOVE      VHBCCM        VABCCM
019700070115     C     VABCCM        IFLE      *ZEROS
019800070115     C                   SETON                                        31
019900070115     C                   eval      vinmsg = %trimr(vinmsg)
020000070115     C                             + ' ' + 'VHBCCM'
020100070115     C                   ENDIF
020200070115     C                   ENDIF
020300070115     C*
020400070115     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020500070115     C                   eval      depfil = VABLNP
020600070115     C*
020700070115     C* Effettuo forzatura ESPRINET x invio a terminal d competenza rispetto al CCM indicato dal
020800070115     C* cliente
020900070507     C                   if        VABCCM = 0893943 OR
021000070507     C                             VABCCM = 0894169
021100070115     C                   eval      depfil = 050
021200070115     C                   eval      invfil = 050
021300070115     C                   eval      vabfgs = 050
021400070115     C                   endif
021500070507     C                   if        VABCCM = 0893942 OR
021600070507     C                             VABCCM = 0894168
021700070115     C                   eval      depfil = 005
021800070115     C                   eval      invfil = 005
021900070115     C                   eval      vabfgs = 005
022000070115     C                   endif
022100010530     C*
022200010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
022300010530     C                   exsr      repfil
022400010601     C                   if        depfil = invfil
022500021014     C                   if        vlrpoi = 999
022600021010     C                   MOVE(P)   invfil        VABFGS
022700021014     C                   else
022800021014     C                   MOVE(P)   vlrpoi        VABFGS
022900021014     C                   endif
023000010530     C*
023100010530     C                   exsr      PREELA
023200971021     C*
023300971023     C                   TESTN                   VHBAAS               30
023400971023     C  N30              TESTN                   VHBAAS                 30
023500971023     C  N30              TESTN                   VHBAAS                   30
023600971021     C                   MOVE      VHBAAS        W001A             1
023700971021     C     *IN30         IFEQ      *OFF
023800971021     C     W001A         ORLT      '0'
023900971023     C     W001A         ANDNE     *BLANK
024000971021     C                   SETON                                        32
024100010606     C                   MOVEL     datcor        VABAAS
024200010604     C                   eval      vinmsg = %trimr(vinmsg)
024300010604     C                             + ' ' + 'VHBAAS'
024400971021     C                   ELSE
024500971021     C                   MOVE      VHBAAS        VABAAS
024600971021     C                   ENDIF
024700971021     C*
024800971023     C                   TESTN                   VHBMGS               30
024900971023     C  N30              TESTN                   VHBMGS                 30
025000971023     C  N30              TESTN                   VHBMGS                   30
025100971021     C                   MOVE      VHBMGS        W001A             1
025200971021     C     *IN30         IFEQ      *OFF
025300971021     C     W001A         ORLT      '0'
025400971023     C     W001A         ANDNE     *BLANK
025500971021     C                   SETON                                        32
025600010601     C                   MOVE      '0101'        VABMGS
025700010604     C                   eval      vinmsg = %trimr(vinmsg)
025800010604     C                             + ' ' + 'VHBMGS'
025900971021     C                   ELSE
026000971021     C                   MOVE      VHBMGS        VABMGS
026100971021     C                   ENDIF
026200971021     C*
026300971023     C                   TESTN                   VHBNRS               30
026400971023     C  N30              TESTN                   VHBNRS                 30
026500971023     C  N30              TESTN                   VHBNRS                   30
026600971021     C                   MOVE      VHBNRS        W001A             1
026700971021     C     *IN30         IFEQ      *OFF
026800971021     C     W001A         ORLT      '0'
026900971023     C     W001A         ANDNE     *BLANK
027000971021     C                   SETON                                        32
027100971021     C                   MOVE      *ZERO         VABNRS
027200010604     C                   eval      vinmsg = %trimr(vinmsg)
027300010604     C                             + ' ' + 'VHBNRS'
027400971021     C                   ELSE
027500971021     C                   MOVE      VHBNRS        VABNRS
027600971021     C                   ENDIF
027700971021     C*
027800971023     C                   TESTN                   VHBNSP               30
027900971023     C  N30              TESTN                   VHBNSP                 30
028000971023     C  N30              TESTN                   VHBNSP                   30
028100971021     C                   MOVE      VHBNSP        W001A             1
028200971021     C     *IN30         IFEQ      *OFF
028300971021     C     W001A         ORLT      '0'
028400971023     C     W001A         ANDNE     *BLANK
028500971021     C                   SETON                                        32
028600971021     C                   MOVE      *ZERO         VABNSP
028700010604     C                   eval      vinmsg = %trimr(vinmsg)
028800010604     C                             + ' ' + 'VHBNSP'
028900971021     C                   ELSE
029000971021     C                   MOVE      VHBNSP        VABNSP
029100971021     C                   ENDIF
029200971021     C*
029300971023     C                   TESTN                   VHBLNA               30
029400971023     C  N30              TESTN                   VHBLNA                 30
029500971023     C  N30              TESTN                   VHBLNA                   30
029600971021     C                   MOVE      VHBLNA        W001A             1
029700971021     C     *IN30         IFEQ      *OFF
029800971021     C     W001A         ORLT      '0'
029900971023     C     W001A         ANDNE     *BLANK
030000971021     C                   SETON                                        32
030100971021     C                   MOVE      *ZERO         VABLNA
030200010604     C                   eval      vinmsg = %trimr(vinmsg)
030300010604     C                             + ' ' + 'VHBLNA'
030400971021     C                   ELSE
030500971021     C                   MOVE      VHBLNA        VABLNA
030600971021     C                   ENDIF
030700971021     C*
030800971023     C                   TESTN                   VHBCTR               30
030900971023     C  N30              TESTN                   VHBCTR                 30
031000971023     C  N30              TESTN                   VHBCTR                   30
031100971021     C                   MOVE      VHBCTR        W001A             1
031200971021     C     *IN30         IFEQ      *OFF
031300971021     C     W001A         ORLT      '0'
031400971023     C     W001A         ANDNE     *BLANK
031500971021     C                   SETON                                        32
031600971021     C                   MOVE      *ZERO         VABCTR
031700010604     C                   eval      vinmsg = %trimr(vinmsg)
031800010604     C                             + ' ' + 'VHBCTR'
031900971021     C                   ELSE
032000971021     C                   MOVE      VHBCTR        VABCTR
032100971021     C                   ENDIF
032200941229     C*
032300971021     C                   MOVE      *ZERO         VABIAS
032400941229     C                   MOVE      VHBIAS        WASC15
032500971023     C                   TESTN                   WI15                 30
032600971023     C  N30              TESTN                   WI15                   30
032700971023     C  N30              TESTN                   WI15                     30
032800971021     C                   MOVE      WI15          W001A             1
032900971021     C     *IN30         IFEQ      *OFF
033000971021     C     W001A         ORLT      '0'
033100971023     C     W001A         ANDNE     *BLANK
033200971021     C                   SETON                                        32
033300971021     C                   MOVE      *ZERO         VABIAS
033400010604     C                   eval      vinmsg = %trimr(vinmsg)
033500010604     C                             + ' ' + 'VHBIAS'
033600971021     C                   ELSE
033700971021     C                   MOVEL     WI15          VABIAS
033800971023     C                   TESTN                   WD15                 30
033900971023     C  N30              TESTN                   WD15                   30
034000971023     C  N30              TESTN                   WD15                     30
034100971021     C                   MOVE      WD15          W001A             1
034200971021     C     *IN30         IFEQ      *OFF
034300971021     C     W001A         ORLT      '0'
034400971023     C     W001A         ANDNE     *BLANK
034500971021     C                   SETON                                        32
034600971021     C                   MOVE      *ZERO         VABIAS
034700010604     C                   eval      vinmsg = %trimr(vinmsg)
034800010604     C                             + ' ' + 'VHBIAS'
034900971021     C                   ELSE
035000971021     C                   MOVE      WD15          VABIAS
035100971021     C                   ENDIF
035200050118     C                   ENDIF
035300941229     C*
035400971023     C                   TESTN                   VHBNCL               30
035500971023     C  N30              TESTN                   VHBNCL                 30
035600971023     C  N30              TESTN                   VHBNCL                   30
035700971021     C                   MOVE      VHBNCL        W001A             1
035800971021     C     *IN30         IFEQ      *OFF
035900971021     C     W001A         ORLT      '0'
036000971021     C                   SETON                                        32
036100971021     C                   MOVE      *ZERO         VABNCL
036200010604     C                   eval      vinmsg = %trimr(vinmsg)
036300010604     C                             + ' ' + 'VHBNCL'
036400971021     C                   ELSE
036500971021     C                   MOVE      VHBNCL        VABNCL
036600971021     C                   ENDIF
036700941229     C*
036800971021     C                   MOVE      *ZERO         VABPKB
036900971021     C                   MOVE      VHBPKB        WASC09
037000010719     C                   IF        WI09 = *blanks
037100010719     C                   EVAL      WI09 = *zeros
037200010719     C                   ENDIF
037300971023     C                   TESTN                   WI09                 30
037400971023     C  N30              TESTN                   WI09                   30
037500971023     C  N30              TESTN                   WI09                     30
037600971021     C                   MOVE      WI09          W001A             1
037700971021     C     *IN30         IFEQ      *OFF
037800971021     C     W001A         ORLT      '0'
037900971021     C                   SETON                                        32
038000971021     C                   MOVE      *ZERO         VABPKB
038100010604     C                   eval      vinmsg = %trimr(vinmsg)
038200010604     C                             + ' ' + 'VHBPKB'
038300971021     C                   ELSE
038400971021     C                   MOVEL     WI09          VABPKB
038500010719     C                   IF        WD09 = *blanks
038600010719     C                   EVAL      WD09 = *zeros
038700010719     C                   ENDIF
038800971023     C                   TESTN                   WD09                 30
038900971023     C  N30              TESTN                   WD09                     30
039000971021     C                   MOVE      WD09          W001A             1
039100971021     C     *IN30         IFEQ      *OFF
039200971021     C     W001A         ORLT      '0'
039300971023     C     W001A         ANDNE     *BLANK
039400971021     C                   SETON                                        32
039500971021     C                   MOVE      *ZERO         VABPKB
039600010604     C                   eval      vinmsg = %trimr(vinmsg)
039700010604     C                             + ' ' + 'VHBPKB'
039800971021     C                   ELSE
039900971021     C                   MOVE      WD09          VABPKB
040000971021     C                   ENDIF
040100050118     C                   ENDIF
040200971021     C*
040300041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
040400041001     C                   IF        *IN51 = *OFF
040500971021     C                   MOVE      *ZERO         VABVLB
040600971021     C                   MOVE      VHBVLB        WASC07
040700010719     C                   IF        WI07 = *blanks
040800010719     C                   EVAL      WI07 = *zeros
040900010719     C                   ENDIF
041000971023     C                   TESTN                   WI07                 30
041100971023     C  N30              TESTN                   WI07                   30
041200971023     C  N30              TESTN                   WI07                     30
041300971021     C                   MOVE      WI07          W001A             1
041400971021     C     *IN30         IFEQ      *OFF
041500971021     C     W001A         ORLT      '0'
041600971023     C     W001A         ANDNE     *BLANK
041700971021     C                   SETON                                        32
041800971021     C                   MOVE      *ZERO         VABVLB
041900010604     C                   eval      vinmsg = %trimr(vinmsg)
042000010604     C                             + ' ' + 'VHBVLB'
042100971021     C                   ELSE
042200971021     C                   MOVEL     WI07          VABVLB
042300010719     C                   IF        WD07 = *blanks
042400010719     C                   EVAL      WD07 = *zeros
042500010719     C                   ENDIF
042600971023     C                   TESTN                   WD07                 30
042700971023     C  N30              TESTN                   WD07                   30
042800971023     C  N30              TESTN                   WD07                     30
042900971021     C                   MOVE      WD07          W001A             1
043000971021     C     *IN30         IFEQ      *OFF
043100971021     C     W001A         ORLT      '0'
043200971023     C     W001A         ANDNE     *BLANK
043300971021     C                   SETON                                        32
043400971021     C                   MOVE      *ZERO         VABVLB
043500010604     C                   eval      vinmsg = %trimr(vinmsg)
043600010604     C                             + ' ' + 'VHBVLB'
043700971021     C                   ELSE
043800971021     C                   MOVE      WD07          VABVLB
043900971021     C                   ENDIF
044000050118     C                   ENDIF
044100041001     C                   ELSE
044200041001     C* ...altrimenti forzo azzeramento del volume
044300041001     C                   Z-ADD     *zeros        VABVLB
044400041001     C                   ENDIF
044500941229     C*
044600971021     C                   MOVE      *ZERO         VABQFT
044700971021     C                   MOVE      VHBQFT        WASC15
044800010719     C                   IF        WI15 = *blanks
044900010719     C                   EVAL      WI15 = *zeros
045000010719     C                   ENDIF
045100971023     C                   TESTN                   WI15                 30
045200971023     C  N30              TESTN                   WI15                   30
045300971023     C  N30              TESTN                   WI15                     30
045400971021     C                   MOVE      WI15          W001A             1
045500971021     C     *IN30         IFEQ      *OFF
045600971021     C     W001A         ORLT      '0'
045700971023     C     W001A         ANDNE     *BLANK
045800971021     C                   SETON                                        32
045900971021     C                   MOVE      *ZERO         VABQFT
046000010604     C                   eval      vinmsg = %trimr(vinmsg)
046100010604     C                             + ' ' + 'VHBQFT'
046200971021     C                   ELSE
046300971021     C                   MOVEL     WI15          VABQFT
046400010719     C                   IF        WD15 = *blanks
046500010719     C                   EVAL      WD15 = *zeros
046600010719     C                   ENDIF
046700971023     C                   TESTN                   WD15                 30
046800971023     C  N30              TESTN                   WD15                   30
046900971023     C  N30              TESTN                   WD15                     30
047000971021     C                   MOVE      WD15          W001A             1
047100971021     C     *IN30         IFEQ      *OFF
047200971021     C     W001A         ORLT      '0'
047300971023     C     W001A         ANDNE     *BLANK
047400971021     C                   SETON                                        32
047500971021     C                   MOVE      *ZERO         VABQFT
047600010604     C                   eval      vinmsg = %trimr(vinmsg)
047700010604     C                             + ' ' + 'VHBQFT'
047800971021     C                   ELSE
047900971021     C                   MOVE      WD15          VABQFT
048000971021     C                   ENDIF
048100050118     C                   ENDIF
048200941229     C*
048300971021     C                   MOVE      *ZERO         VABCAS
048400060410     C* Se presente separatore decimale alla giusta posizione => forzo sempre la virgola
048500060410     C                   EVAL      wVHBCAS = VHBCAS
048600060410     C                   IF        %subst(wVHBCAS:12:1) = '.'
048700060410     C                   EVAL      %subst(wVHBCAS:12:1) = ','
048800060410     C                   ENDIF
048900060410     C*
049000060410     C                   MOVEL(P)  wVHBCAS       PiStr
049100060410     C                   EXSR      CHKNUM
049200060410     C                   IF        PiNum=*on
049300060410     C                   Z-ADD     PiVal         VABCAS
049400060410     C                   ELSE
049500060410     C                   SETON                                        32
049600060410     C                   Z-ADD     *zeros        VABCAS
049700060410     C                   EVAL      vinmsg = %trimr(vinmsg)
049800060410     C                             + ' ' + 'VHBCAS'
049900060410     C                   ENDIF
050000971021     C*
050100971023     C                   TESTN                   VHBRMN               30
050200971023     C  N30              TESTN                   VHBRMN                 30
050300971023     C  N30              TESTN                   VHBRMN                   30
050400971021     C                   MOVE      VHBRMN        W001A             1
050500971021     C     *IN30         IFEQ      *OFF
050600971021     C     W001A         ORLT      '0'
050700971021     C                   SETON                                        32
050800971021     C                   MOVE      *ZERO         VABRMN
050900010604     C                   eval      vinmsg = %trimr(vinmsg)
051000010604     C                             + ' ' + 'VHBRMN'
051100971021     C                   ELSE
051200971021     C                   MOVE      VHBRMN        VABRMN
051300971021     C                   ENDIF
051400971021     C*
051500971023     C                   TESTN                   VHBNCD               30
051600971023     C  N30              TESTN                   VHBNCD                 30
051700971023     C  N30              TESTN                   VHBNCD                   30
051800971021     C                   MOVE      VHBNCD        W001A             1
051900971021     C     *IN30         IFEQ      *OFF
052000971021     C     W001A         ORLT      '0'
052100971023     C     W001A         ANDNE     *BLANK
052200971021     C                   SETON                                        32
052300971021     C                   MOVE      *ZERO         VABNCD
052400010604     C                   eval      vinmsg = %trimr(vinmsg)
052500010604     C                             + ' ' + 'VHBNCD'
052600971021     C                   ELSE
052700971021     C                   MOVE      VHBNCD        VABNCD
052800971021     C                   ENDIF
052900971021     C*
053000971023     C                   TESTN                   VHBNCA               30
053100971023     C  N30              TESTN                   VHBNCA                 30
053200971023     C  N30              TESTN                   VHBNCA                   30
053300971021     C                   MOVE      VHBNCA        W001A             1
053400971021     C     *IN30         IFEQ      *OFF
053500971021     C     W001A         ORLT      '0'
053600971023     C     W001A         ANDNE     *BLANK
053700971021     C                   SETON                                        32
053800971021     C                   MOVE      *ZERO         VABNCA
053900010604     C                   eval      vinmsg = %trimr(vinmsg)
054000010604     C                             + ' ' + 'VHBNCA'
054100971021     C                   ELSE
054200971021     C                   MOVE      VHBNCA        VABNCA
054300971021     C                   ENDIF
054400971021     C*
054500971023     C                   TESTN                   VHBZNC               30
054600971023     C  N30              TESTN                   VHBZNC                 30
054700971023     C  N30              TESTN                   VHBZNC                   30
054800971021     C                   MOVE      VHBZNC        W001A             1
054900971021     C     *IN30         IFEQ      *OFF
055000971021     C     W001A         ORLT      '0'
055100971023     C     W001A         ANDNE     *BLANK
055200971021     C                   SETON                                        32
055300971021     C                   MOVE      *ZERO         VABZNC
055400010604     C                   eval      vinmsg = %trimr(vinmsg)
055500010604     C                             + ' ' + 'VHBZNC'
055600971021     C                   ELSE
055700971021     C                   MOVE      VHBZNC        VABZNC
055800971021     C                   ENDIF
055900971021     C*
056000971023     C                   TESTN                   VHBDCR               30
056100971023     C  N30              TESTN                   VHBDCR                 30
056200971023     C  N30              TESTN                   VHBDCR                   30
056300971021     C                   MOVE      VHBDCR        W001A             1
056400971021     C     *IN30         IFEQ      *OFF
056500971021     C     W001A         ORLT      '0'
056600971023     C     W001A         ANDNE     *BLANK
056700971021     C                   SETON                                        32
056800971021     C                   MOVE      *ZERO         VABDCR
056900010604     C                   eval      vinmsg = %trimr(vinmsg)
057000010604     C                             + ' ' + 'VHBDCR'
057100971021     C                   ELSE
057200971021     C                   MOVE      VHBDCR        VABDCR
057300971021     C                   ENDIF
057400971021     C*
057500971023     C                   TESTN                   VHBHCR               30
057600971023     C  N30              TESTN                   VHBHCR                 30
057700971023     C  N30              TESTN                   VHBHCR                   30
057800971021     C                   MOVE      VHBHCR        W001A             1
057900971021     C     *IN30         IFEQ      *OFF
058000971021     C     W001A         ORLT      '0'
058100971023     C     W001A         ANDNE     *BLANK
058200971021     C                   SETON                                        32
058300971021     C                   MOVE      *ZERO         VABHCR
058400010604     C                   eval      vinmsg = %trimr(vinmsg)
058500010604     C                             + ' ' + 'VHBHCR'
058600971021     C                   ELSE
058700971021     C                   MOVE      VHBHCR        VABHCR
058800971021     C                   ENDIF
058900941229     C*
059000971021     C                   MOVE      *ZERO         VABVMD
059100971021     C                   MOVE      VHBVMD        WASC15
059200010719     C                   IF        WI15 = *blanks
059300010719     C                   EVAL      WI15 = *zeros
059400010719     C                   ENDIF
059500971023     C                   TESTN                   WI15                 30
059600971023     C  N30              TESTN                   WI15                   30
059700971023     C  N30              TESTN                   WI15                     30
059800971021     C                   MOVE      WI15          W001A             1
059900971021     C     *IN30         IFEQ      *OFF
060000971021     C     W001A         ORLT      '0'
060100971023     C     W001A         ANDNE     *BLANK
060200971021     C                   SETON                                        32
060300971021     C                   MOVE      *ZERO         VABVMD
060400010604     C                   eval      vinmsg = %trimr(vinmsg)
060500010604     C                             + ' ' + 'VHBVMD'
060600971021     C                   ELSE
060700971021     C                   MOVEL     WI15          VABVMD
060800010719     C                   IF        WD15 = *blanks
060900010719     C                   EVAL      WD15 = *zeros
061000010719     C                   ENDIF
061100971023     C                   TESTN                   WD15                 30
061200971023     C  N30              TESTN                   WD15                   30
061300971023     C  N30              TESTN                   WD15                     30
061400971021     C                   MOVE      WD15          W001A             1
061500971021     C     *IN30         IFEQ      *OFF
061600971021     C     W001A         ORLT      '0'
061700971023     C     W001A         ANDNE     *BLANK
061800971021     C                   SETON                                        32
061900971021     C                   MOVE      *ZERO         VABVMD
062000010604     C                   eval      vinmsg = %trimr(vinmsg)
062100010604     C                             + ' ' + 'VHBVMD'
062200971021     C                   ELSE
062300971021     C                   MOVE      WD15          VABVMD
062400971021     C                   ENDIF
062500050118     C                   ENDIF
062600971021     C*
062700971023     C                   TESTN                   VHBANT               30
062800971023     C  N30              TESTN                   VHBANT                 30
062900971023     C  N30              TESTN                   VHBANT                   30
063000971021     C                   MOVE      VHBANT        W001A             1
063100971021     C     *IN30         IFEQ      *OFF
063200971021     C     W001A         ORLT      '0'
063300971023     C     W001A         ANDNE     *BLANK
063400971021     C                   SETON                                        32
063500971021     C                   MOVE      *ZERO         VABANT
063600010604     C                   eval      vinmsg = %trimr(vinmsg)
063700010604     C                             + ' ' + 'VHBANT'
063800971021     C                   ELSE
063900971021     C                   MOVE      VHBANT        VABANT
064000971021     C                   ENDIF
064100971021     C*
064200971021     C* CAMPI ALFABETICI:
064300971021     C                   MOVE      VHBATB        VABATB
064400971021     C                   MOVE      VHBCBO        VABCBO
064500971021     C                   MOVE      VHBRSD        VABRSD
064600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
064700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
064800020117     C* ==
064900971021     C                   MOVE      VHBRD2        VABRD2
065000971021     C                   MOVE      VHBIND        VABIND
065100971021     C                   MOVE      VHBCAD        VABCAD
065200971021     C                   MOVE      VHBLOD        VABLOD
065300971021     C                   MOVE      VHBPRD        VABPRD
065400971021     C                   MOVE      VHBNZD        VABNZD
065500971021     C                   MOVE      VHBGC1        VABGC1
065600971021     C                   MOVE      VHBGC2        VABGC2
065700971021     C                   MOVE      VHBTSP        VABTSP
065800971021     C                   MOVE      VHBVAS        VABVAS
065900971021     C                   MOVE      VHBNAS        VABNAS
066000971021     C                   MOVE      VHBTIC        VABTIC
066100971021     C                   MOVE      VHBVCA        VABVCA
066200971021     C                   MOVE      VHBGCA        VABGCA
066300971021     C                   MOVE      VHBRMA        VABRMA
066400971021     C                   MOVE      VHBXCO        VABXCO
066500971021     C                   MOVE      VHBNOT        VABNOT
066600971021     C                   MOVE      VHBNT2        VABNT2
066700971021     C                   MOVE      VHBCTM        VABCTM
066800971021     C                   MOVE      VHBFFD        VABFFD
066900971021     C                   MOVE      VHBTCR        VABTCR
067000971021     C                   MOVE      VHBCTS        VABCTS
067100971021     C                   MOVE      VHBFTM        VABFTM
067200971021     C                   MOVE      VHBVAD        VABVAD
067300971021     C                   MOVE      VHBGMA        VABGMA
067400971021     C                   MOVE      VHBGGA        VABGGA
067500971021     C                   MOVE      VHBGVA        VABGVA
067600971021     C                   MOVE      VHBTC1        VABTC1
067700971021     C                   MOVE      VHBTC2        VABTC2
067800971021     C                   MOVE      VHBSCL        VABSCL
067900971021     C                   MOVE      VHBRMO        VABRMO
068000971021     C                   MOVE      VHBCMO        VABCMO
068100971021     C                   MOVE      VHBNMO        VABNMO
068200971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
068300060329     C* NELLA NAZIONE MITTENTE ORIGINALE
068400060331     C                   IF        %trim(VHBNMO) < 'A'
068500060329     C                   MOVE      *BLANKS       VABNMO
068600060329     C                   ENDIF
068700050418     C*
068800050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
068900050418     C                   IF        %trim(VABNZD) = 'I'   OR
069000050418     C                             %trim(VABNZD) = 'IT'  OR
069100050418     C                             %trim(VABNZD) = 'ITA'
069200050418     C                   EVAL      VABNZD = *blanks
069300050418     C                   ENDIF
069400041025     C*
069500041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
069600041025     C                   IF        *IN52 = *ON
069700041025     C* Reperisco la provincia dal CAP e dalla località
069800041025     C                   IF        VABCAD <> *blanks AND
069900050907     C                             VABPRD  = *blanks AND
070000050907     C                             VABNZD  = *blanks
070100041025     C                   CLEAR                   TISI95DS
070200041025     C                   EVAL      I95TCN = '3'
070300041025     C                   Z-ADD     datcor        I95DAT
070400041025     C                   EVAL      I95CAP = VABCAD
070500041025     C                   EVAL      I95LOC = VABLOD
070600041025     C                   CALL      'TISI95R'
070700041025     C                   PARM                    TISI95DS
070800041025     C                   EVAL      VABPRD = O95PRV
070900041025     C                   ENDIF
071000041025     C                   ENDIF
071100040630     C*
071200040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
071300040630     C                   exsr      exeBon
071400101027     C*
071500101027     C* forzature richieste
071600101027     C                   if        VABTC1 = 'F' or VABTC2 = 'F'
071700101027     C                   eval      VABNOT = 'CONSEGNA CON SPONDA IDRAULICA'
071800101027     C                   endif
071900110207     C*
072000120515     C***                if        VABCCM = 0896002
072100120515     C***                eval      VABCTM = '2D'
072200120515     C***                endif
072300930409     C*
072400010604     C  N31              ADD       1             §CTROK            7 0
072500010604     C   32              ADD       1             §CTRMO            7 0
072600010604     C   31              ADD       1             §CTRNO            7 0
072700110126     C*
072800110126     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
072900110126     C                   IF        not *in31
073000110126     C                   EVAL      VABCMR = %subst(wNomeFile:8:8)
073100110126     C                   EVAL      VABDCM = DATCOR
073200110126     C                   EVAL      VABDTS = DATCOR
073300110126     C                   EVAL      VABHMS = ORACOR
073400110126     C                   EVAL      VABCNT = 1
073500110126     C                   ENDIF
073600110126     C*
073700110126     C  N31              WRITE     EDIVAB00
073800990910     C*
073900010601     C                   if        *in31 = *off and
074000010601     C                             *in32 = *off
074100990910     C                   eval      vinflg = '1'
074200990910     C                   else
074300990910     C                   eval      vinflg = '2'
074400990910     C                   endif
074500991022     C                   endif
074600990910     C                   endif
074700010601     C*
074800010601     C                   endif
074900000905     C*
075000000905     C                   else
075100000905     C                   eval      vinflg = '1'
075200000905     C                   endif
075300000905     C*
075400000905     C  N70              update    tivin000
075500991022     C*
075600991022     C  N70              ENDdo
075700010530     C
075800010530     C*
075900010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
076000010530     C                   if        cntNonEl = *zeros or
076100010530     C                             flgMulti = '0'
076200990910     C* Se non ci sono record con errori ...
076300010530     C                   if        §ctrno = 0 and
076400010601     C                             §ctrmo = 0 and
076500010530     C                             flgStato <> '2'
076600990910     C* ... restituisco esito OK.
076700990921     C                   eval      wrkesito = '0'
076800990910     C                   else
076900010601     C                   if        §ctrok > 0
077000990921     C                   eval      wrkesito = '1'
077100000710     C                   else
077200010615     C                   if        flgOk = '0'
077300010615     C                   eval      wrkesito = '2'
077400010615     C                   else
077500010615     C                   eval      wrkesito = '6'
077600010615     C                   endif
077700990910     C                   endif
077800000710     C                   endif
077900010530     C                   else
078000010530     C                   eval      wrkesito = '9'
078100010530     C                   endif
078200990910     C*
078300990914     C                   if        %open(tivin00r)
078400990908     C                   close     tivin00r
078500990914     C                   endif
078600110126     C                   if        %open(edivabwr)
078700110126     C                   close     edivabwr
078800990914     C                   endif
078900010530     C*
079000010530     C                   if        vlrpoi <> 999
079100010530     C                   eval      invfil = vlrpoi
079200010530     C                   endif
079300990910     C*
079400990920     C                   if        §ctrok > 0
079500010530     C                             and invfil > *zeros
079600000613     C                   exsr      invio
079700990920     C                   endif
079800010612     C*
079900010612     C                   if        flgGiro = '1'
080000010612     C                   exsr      endela
080100010612     C                   endif
080200990920     C*
080300910830     C                   ENDSR
080400000613     C***
080500990920
080600010530
080700010530
080800010530
080900010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
081000010530     C     repfil        BEGSR
081100010530     C*
081200010530     C                   if        invfil = *zeros and
081300010530     C                             depfil > *zeros and
081400010613     C                             (vinflg = *blanks or
081500010613     C                              vinflg = *zeros)
081600010530     C
081700010530     C                   eval      invfil = depfil
081800010530     C                   endif
081900010530     C*
082000010530     C                   if        depfil <> invfil and
082100010530     C                             invfil > *zeros
082200010530     C                   eval      flgMulti = '1'
082300010530     C                   if        vinflg = *blanks
082400010530     C                   add       1             cntNonEl
082500010530     C                   endif
082600010530     C                   endif
082700010530     C*
082800010530     C                   if        vinflg = '2'
082900010530     C                   eval      flgStato = '2'
083000010530     C                   endif
083100010530     C*
083200010530     C                   ENDSR
083300010530     C***
083400010530
083500010530
083600010530
083700010530
083800990920      /TITLE Invio dei dati al punto operativo.
083900000613     C     invio         BEGSR
084000990920     C*
084100990920     C                   reset                   dscmz
084200010530     C                   move      invfil        cmzdst
084300990920     C                   eval      cmzfld = vlrfou
084400990920     C                   eval      cmzmbd = vlrhdl
084500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
084600000710     C                   if        prmfir = *blanks
084700110126     C                   eval      cmzfla = 'EDIVAB0F'
084800110126     C                   eval      cmzmba = 'EDIVAB0F'
084900000710     C                   else
085000000710     C                   eval      cmzfla = prmfir
085100000710     C                   eval      cmzmba = prmfir
085200000710     C                   endif
085300990920     C                   eval      cmznrr = *zeros
085400990920     C                   move      §ctrok        cmznrr
085500021003     C                   eval      cmzlba = vlrfl1
085600990920     C                   call(e)   'TIS711C'
085700990920     C                   parm                    dscmz
085800990921     C                   parm      *blanks       esito
085900990920     C                   if        %error
086000990920     C                             or cmzerr = '1'
086100990921     C                             or esito  = '1'
086200000710     C                   eval      wrkesito = '3'
086300990920     C                   endif
086400990920     C*
086500000613     C                   ENDSR
086600000613     C***
086700010530
086800010530
086900010530
087000010530
087100010530      /TITLE Invio dei dati al punto operativo.
087200010530     C     opeini        BEGSR
087300010530     C*
087400010530     C* Inizializzo flag e contatori operativi
087500010530     C                   movel     '0'           flgGiro           1
087600010530     C                   movel     '0'           flgMulti          1
087700010530     C                   movel     '1'           flgStato          1
087800010615     C                   movel     '0'           flgOk             1
087900010530     C                   z-add     *zeros        cntNonEl         10 0
088000010530     C                   z-add     *zeros        depfil            3 0
088100010530     C                   z-add     *zeros        invfil            3 0
088200030407     C*
088300030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
088400030407     C                   if        %subst(vlrppt:1:1) = 'S'
088500030407     C                   seton                                        50
088600030407     C                   else
088700030407     C                   setoff                                       50
088800030407     C                   endif
088900041001     C*
089000041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
089100041001     C                   if        %subst(vlrppt:2:1) = 'S'
089200041001     C                   seton                                        51
089300041001     C                   else
089400041001     C                   setoff                                       51
089500041001     C                   endif
089600041025     C*
089700041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
089800041025     C* (naturalmente solo se nn già presente)
089900041025     C                   if        %subst(vlrppt:3:1) = 'S'
090000041025     C                   seton                                        52
090100041025     C                   else
090200041025     C                   setoff                                       52
090300041025     C                   endif
090400010530     C*
090500010530     C                   ENDSR
090600010530     C***
090700040407
090800040407
090900040407
091000040407
091100040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
091200040407     C     exeBon        BEGSR
091300040407     C*
091400040407     C* Imposto i parametri x il *pgm d "bonifica"
091500040407     C                   EVAL      ElencoChar = *blanks
091600040407     C                   EVAL      TipoElenco = *blanks
091700040407     C                   EVAL      CharSost   = *blanks
091800040629     C                   EVAL      UpperCase  = '1'
091900040407     C                   EVAL      ChkNull    = '1'
092000040407     C                   EVAL      CharNull   = *blanks
092100040629     C*
092200040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
092300040629     C                   EVAL      TxtInOut   = VABRSD
092400040629     C                   EVAL      EsitoBon   = *blanks
092500040407     C*
092600040407     C                   CALL      'XCHKCHAR'
092700040407     C                   PARM                    TxtInOut
092800040407     C                   PARM                    ElencoChar
092900040407     C                   PARM                    TipoElenco
093000040407     C                   PARM                    CharSost
093100040407     C                   PARM                    UpperCase
093200040407     C                   PARM                    ChkNull
093300040407     C                   PARM                    CharNull
093400040407     C                   PARM                    EsitoBon
093500040407     C*
093600040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
093700040407     C                   IF        EsitoBon = '1'
093800040629     C                   EVAL      VABRSD = TxtInOut
093900040407     C                   ENDIF
094000040629     C*
094100040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
094200040629     C                   EVAL      TxtInOut   = VABIND
094300040629     C                   EVAL      EsitoBon   = *blanks
094400040629     C*
094500040629     C                   CALL      'XCHKCHAR'
094600040629     C                   PARM                    TxtInOut
094700040629     C                   PARM                    ElencoChar
094800040629     C                   PARM                    TipoElenco
094900040629     C                   PARM                    CharSost
095000040629     C                   PARM                    UpperCase
095100040629     C                   PARM                    ChkNull
095200040629     C                   PARM                    CharNull
095300040629     C                   PARM                    EsitoBon
095400040629     C*
095500040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
095600040629     C                   IF        EsitoBon = '1'
095700040629     C                   EVAL      VABIND = TxtInOut
095800040629     C                   ENDIF
095900040629     C*
096000040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
096100040629     C                   EVAL      TxtInOut   = VABLOD
096200040629     C                   EVAL      EsitoBon   = *blanks
096300040629     C*
096400040629     C                   CALL      'XCHKCHAR'
096500040629     C                   PARM                    TxtInOut
096600040629     C                   PARM                    ElencoChar
096700040629     C                   PARM                    TipoElenco
096800040629     C                   PARM                    CharSost
096900040629     C                   PARM                    UpperCase
097000040629     C                   PARM                    ChkNull
097100040629     C                   PARM                    CharNull
097200040629     C                   PARM                    EsitoBon
097300040629     C*
097400040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
097500040629     C                   IF        EsitoBon = '1'
097600040629     C                   EVAL      VABLOD = TxtInOut
097700040629     C                   ENDIF
097800040629     C*
097900040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
098000040629     C                   EVAL      TxtInOut   = VABNOT
098100040629     C                   EVAL      EsitoBon   = *blanks
098200040630     C                   EVAL      UpperCase  = *blanks
098300040629     C*
098400040629     C                   CALL      'XCHKCHAR'
098500040629     C                   PARM                    TxtInOut
098600040629     C                   PARM                    ElencoChar
098700040629     C                   PARM                    TipoElenco
098800040629     C                   PARM                    CharSost
098900040629     C                   PARM                    UpperCase
099000040629     C                   PARM                    ChkNull
099100040629     C                   PARM                    CharNull
099200040629     C                   PARM                    EsitoBon
099300040629     C*
099400040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
099500040629     C                   IF        EsitoBon = '1'
099600040629     C                   EVAL      VABNOT = TxtInOut
099700040629     C                   ENDIF
099800040629     C*
099900040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
100000040629     C                   EVAL      TxtInOut   = VABNT2
100100040629     C                   EVAL      EsitoBon   = *blanks
100200040630     C                   EVAL      UpperCase  = *blanks
100300040629     C*
100400040629     C                   CALL      'XCHKCHAR'
100500040629     C                   PARM                    TxtInOut
100600040629     C                   PARM                    ElencoChar
100700040629     C                   PARM                    TipoElenco
100800040629     C                   PARM                    CharSost
100900040629     C                   PARM                    UpperCase
101000040629     C                   PARM                    ChkNull
101100040629     C                   PARM                    CharNull
101200040629     C                   PARM                    EsitoBon
101300040629     C*
101400040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
101500040629     C                   IF        EsitoBon = '1'
101600040629     C                   EVAL      VABNT2 = TxtInOut
101700040629     C                   ENDIF
101800040407     C*
101900040407     C                   ENDSR
102000010530
102100060410
102200060410     C*----------------------------------------------------*
102300060410     C*  CONTROLLO NUMERICITA' CAMPI
102400060410     C*----------------------------------------------------*
102500060410     C     CHKNUM        BEGSR
102600060410     C*
102700060410     C                   IF        PiDecChr = *blanks
102800060410     C                   EVAL      PiDecChr = ','
102900060410     C                   ENDIF
103000060410     C*
103100060410     C                   CALL(e)   'ISNUMERIC'
103200060410     C                   PARM                    PiStr            30
103300060410     C                   PARM                    PiDecChr          1
103400060410     C                   PARM      *ZEROS        PiVal            30 9
103500060410     C                   PARM      '0'           PiInt             1
103600060410     C                   PARM      '0'           PiNum             1
103700060410     C                   IF        %error
103800060410     C                   EVAL      PiNum=*off
103900060410     C                   ENDIF
104000060410     C*
104100060410     C                   ENDSR
104200060410     C***
104300060410
104400990910
104500000613     C     *inzsr        BEGSR
104600990910     C*
104700990910     C     *entry        plist
104800990920     C                   parm                    tivlrds
104900990921     C                   parm      wrkesito      esito
105000000724     C                   parm                    prmlit
105100000710     C                   parm                    prmfir
105200110126     C*
105300110126     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
105400110126     C                   move(p)   vlrMSG        wNomeFile
105500010606     C*
105600010606     C* CALCOLA LA DATA CORRENTE
105700010606     C                   time                    wn14             14 0
105800010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
105900110126     C                   movel     wn14          oracor            6 0          *ORA
106000010606     C                   z-add     wn8           g08dat
106100010606     C                   z-add     *zeros        g08inv
106200010606     C                   movel     '0'           g08err
106300010606     C                   call      'XSRDA8'
106400010606     C                   parm                    wlbda8
106500010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
106600000613     C*
106700000613     C                   ENDSR
106800000613     C***
