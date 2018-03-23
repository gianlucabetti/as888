000100100719      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200100719     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500020913     FFIVABwwr  O    E             DISK    usropn
000600050209     FFIVATwwr  O    E             DISK    usropn
000700990908
000800000512     D*------------
000900990920     D dscmz         e ds                  inz
001000990910     D psds           sds
001100990910     D  procname         *PROC
001200941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001300941229     D* 10+3
001400941229     D                 DS
001500941229     D  WS15                   1      1
001600941229     D  WI15                   2     11
001700941229     D  WV15                  12     12
001800941229     D  WD15                  13     15
001900941229     D  WASC15                 1     15
002000941229     D*  6+1
002100941229     D                 DS
002200941229     D  WS09                   1      1
002300941229     D  WI09                   2      7
002400941229     D  WV09                   8      8
002500941229     D  WD09                   9      9
002600941229     D  WASC09                 1      9
002700941229     D*  2+3
002800941229     D                 DS
002900941229     D  WS07                   1      1
003000941229     D  WI07                   2      3
003100941229     D  WV07                   4      4
003200941229     D  WD07                   5      7
003300941229     D  WASC07                 1      7
003400990920     D tivlrds       e ds                  extname(tivlr00f)
003500990910     D fnvabaar      e ds                  inz
003600100719     D tisi95ds      e ds                  inz
003700990910     D esito           s              1
003800000724     D prmlit          s             10
003900000710     D prmfir          s             10
004000990921     D wrkesito        s                   like(esito)
004100000613     D rrnum           s              6  0 INZ(*zeros)
004200050209     D parccm          s              8    INZ(*blanks)
004300050209     D parmbr          s             10    INZ(*blanks)
004400050209     D paropz          s              1    INZ(*blanks)
004500050209     D chkcall         s              1    INZ(*blanks)
004600050209     D*------------------
004700050209     D* DS REPERIMENTO NUMERATORE
004800050209     D*------------------
004900050303     D trul33ds      e ds                  inz
005000050209     D*------------------
005100050209     D* DS ARCHITETTURA
005200050209     D*------------------
005300050209     D kpjba         e ds                  inz
005400050209     D*------------------
005500100719
005600100719
005700100719     D*------------------
005800100719     D* VARIABILI X *PGM UTILITA XCHKCHAR
005900100719     D*------------------
006000100719     D TxtInOut        S           2048
006100100719     D ElencoChar      S            256
006200100719     D TipoElenco      S              1
006300100719     D CharSost        S              1
006400100719     D UpperCase       S              1
006500100719     D ChkNull         S              1
006600100719     D CharNull        S              1
006700100719     D EsitoBon        S              1
006800100719
006900100719
007000100719     D*------------------
007100100719     D* LINKING A DEFINIZIONI ESTERNE
007200100719     D*------------------
007300100719     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007400100719     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007500100719
007600100719
007700050209
007800050209     C* /MAIN
007900000913     C                   reset                   rrnum
008000990921     C                   reset                   esito
008100990921     C                   reset                   wrkesito
008200010530     C*
008300010530     C                   exsr      opeini
008400010530     C                   exsr      rwvab
008500050209     C*
008600050209     C* Effettuo la chiamata al CLLE preposto
008700050209     C                   call(e)   'TITVVTC'
008800050209     C                   parm                    parccm
008900050209     C                   parm                    parmbr
009000050209     C                   parm      '2'           paropz
009100010530     C*
009200010530     C                   seton                                        lr
009300010530
009400010530
009500010530
009600010530
009700010530     C*--------------------------------------------------------
009800010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009900010530     C*--------------------------------------------------------
010000010530     C     PREELA        BEGSR
010100000724     C*
010200000724     C* SE OCCORRE SPEDIRE IN FILIALE
010300010530     C                   if        invfil <> *zeros and
010400010530     C                             flgGiro = '0'
010500010530     C*
010600010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010700010530     C                   eval      flgGiro = '1'
010800010530     C*
010900010530     C                   endif
011000010530     C*
011100010530     C                   ENDSR
011200010530     C***
011300010530
011400010530
011500010530
011600010530
011700010530
011800010530
011900010530     C*--------------------------------------------------------
012000010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
012100010530     C*--------------------------------------------------------
012200010530     C     ENDELA        BEGSR
012300010530     C*
012400010530     C                   ENDSR
012500010530     C***
012600990908
012700010530
012800010530
012900010530
013000910830     C*--------------------------------------------------------
013100020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
013200910830     C*--------------------------------------------------------
013300941229     C     RWVAB         BEGSR
013400990910     C*
013500990914     C                   if        not %open(tivin00r)
013600990908     C                   open      tivin00r
013700990914     C                   endif
013800020913     C                   if        not %open(fivabwwr)
013900020913     C                   open      fivabwwr
014000990914     C                   endif
014100050209     C*
014200050209     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
014300050209     C                   exsr      prevat
014400050209     C*
014500050209     C                   if        chkcall = '0'
014600050209     C*
014700050209     C                   if        not %open(fivatwwr)
014800050209     C                   open      fivatwwr
014900050209     C                   endif
015000050209     C*
015100990910     C                   clear                   §CTROK
015200990910     C                   clear                   §CTRMO
015300990910     C                   clear                   §CTRNO
015400990910     C*
015500921023     C                   DO        *HIVAL
015600990913     C*
015700990915     C                   READ      tivin00r                               70
015800010618     C*
015900010618     C* Dopo ogni lettura verifico se ci sono stati record OK
016000010618     C                   if        vinflg = '1'
016100010618     C                   eval      flgOk = '1'
016200010618     C                   endif
016300010705     C*
016400010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
016500010705     C                   if        %subst(vindta:1:1) = '='
016600010705     C                   eval      %subst(vindta:1:1) = *blanks
016700010705     C                   endif
016800010705     C*
016900000905     C                   if        vindta > *blanks
017000000613     C                   add       1             rrnum
017100010601     C* ...elimino eventuali segni
017200010601     C     '+':' '       xlate     vindta        vindta
017300990913     C*
017400010601     C                   if        *in70 = *off and
017500990913     C                             (vinflg = *blanks
017600010711     C                              or vinflg = '0'
017700010711     C                              or vinflg = '2')
017800990913     C*
017900020913     C                   clear                   FIVAB000
018000050209     C                   clear                   FIVAT000
018100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
018200010711     C                   if        vinflg = *blanks or vinflg = '0'
018300010711     C                   clear                   vinmsg
018400010711     C                   endif
018500010711     C*
018600990913     C                   eval      fnvabaar = vindta
018700991022     C*
018800991022     C                   testb     x'3F'         vhbatb                   02
018900991022     C                   if        not *in02
019000991022     C*
019100971021     C                   SETOFF                                       303132
019200971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
019300971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
019400971023     C*    OPPURE TUTTI BLANKS
019500010530     C*
019600010530     C                   TESTN                   VHBLNP               30
019700010530     C  N30              TESTN                   VHBLNP                 30
019800010530     C  N30              TESTN                   VHBLNP                   30
019900010530     C                   MOVE      VHBLNP        W001A             1
020000010530     C     *IN30         IFEQ      *OFF
020100010530     C     W001A         ORLT      '0'
020200010530     C                   SETON                                        31
020300010530     C                   eval      vinmsg = %trimr(vinmsg)
020400010530     C                             + ' ' + 'VHBLNP'
020500010530     C                   ELSE
020600010530     C                   MOVE      VHBLNP        VABLNP
020700010530     C     VABLNP        IFLE      *ZEROS
020800010530     C                   SETON                                        31
020900010530     C                   eval      vinmsg = %trimr(vinmsg)
021000010530     C                             + ' ' + 'VHBLNP'
021100010530     C                   ENDIF
021200010530     C                   ENDIF
021300010530     C*
021400010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
021500010530     C                   eval      depfil = VABLNP
021600010530     C                   exsr      repfil
021700010601     C                   if        depfil = invfil
021800021014     C                   if        vlrpoi = 999
021900021010     C                   MOVE(P)   invfil        VABFGS
022000021014     C                   else
022100021014     C                   MOVE(P)   vlrpoi        VABFGS
022200021014     C                   endif
022300010530     C*
022400010530     C                   exsr      PREELA
022500010530     C*
022600971023     C                   TESTN                   VHBCCM               30
022700971023     C  N30              TESTN                   VHBCCM                 30
022800971023     C  N30              TESTN                   VHBCCM                   30
022900971021     C                   MOVE      VHBCCM        W001A             1
023000971021     C     *IN30         IFEQ      *OFF
023100971021     C     W001A         ORLT      '0'
023200971021     C                   SETON                                        31
023300010604     C                   eval      vinmsg = %trimr(vinmsg)
023400010604     C                             + ' ' + 'VHBCCM'
023500971021     C                   ELSE
023600971021     C                   MOVE      VHBCCM        VABCCM
023700971024     C     VABCCM        IFLE      *ZEROS
023800971024     C                   SETON                                        31
023900010604     C                   eval      vinmsg = %trimr(vinmsg)
024000010604     C                             + ' ' + 'VHBCCM'
024100971024     C                   ENDIF
024200971021     C                   ENDIF
024300971021     C*
024400971023     C                   TESTN                   VHBAAS               30
024500971023     C  N30              TESTN                   VHBAAS                 30
024600971023     C  N30              TESTN                   VHBAAS                   30
024700971021     C                   MOVE      VHBAAS        W001A             1
024800971021     C     *IN30         IFEQ      *OFF
024900971021     C     W001A         ORLT      '0'
025000971023     C     W001A         ANDNE     *BLANK
025100971021     C                   SETON                                        32
025200010606     C                   MOVEL     datcor        VABAAS
025300010604     C                   eval      vinmsg = %trimr(vinmsg)
025400010604     C                             + ' ' + 'VHBAAS'
025500971021     C                   ELSE
025600971021     C                   MOVE      VHBAAS        VABAAS
025700971021     C                   ENDIF
025800971021     C*
025900971023     C                   TESTN                   VHBMGS               30
026000971023     C  N30              TESTN                   VHBMGS                 30
026100971023     C  N30              TESTN                   VHBMGS                   30
026200971021     C                   MOVE      VHBMGS        W001A             1
026300971021     C     *IN30         IFEQ      *OFF
026400971021     C     W001A         ORLT      '0'
026500971023     C     W001A         ANDNE     *BLANK
026600971021     C                   SETON                                        32
026700010601     C                   MOVE      '0101'        VABMGS
026800010604     C                   eval      vinmsg = %trimr(vinmsg)
026900010604     C                             + ' ' + 'VHBMGS'
027000971021     C                   ELSE
027100971021     C                   MOVE      VHBMGS        VABMGS
027200971021     C                   ENDIF
027300971021     C*
027400971023     C                   TESTN                   VHBNRS               30
027500971023     C  N30              TESTN                   VHBNRS                 30
027600971023     C  N30              TESTN                   VHBNRS                   30
027700971021     C                   MOVE      VHBNRS        W001A             1
027800971021     C     *IN30         IFEQ      *OFF
027900971021     C     W001A         ORLT      '0'
028000971023     C     W001A         ANDNE     *BLANK
028100971021     C                   SETON                                        32
028200971021     C                   MOVE      *ZERO         VABNRS
028300010604     C                   eval      vinmsg = %trimr(vinmsg)
028400010604     C                             + ' ' + 'VHBNRS'
028500971021     C                   ELSE
028600971021     C                   MOVE      VHBNRS        VABNRS
028700971021     C                   ENDIF
028800971021     C*
028900971023     C                   TESTN                   VHBNSP               30
029000971023     C  N30              TESTN                   VHBNSP                 30
029100971023     C  N30              TESTN                   VHBNSP                   30
029200971021     C                   MOVE      VHBNSP        W001A             1
029300971021     C     *IN30         IFEQ      *OFF
029400971021     C     W001A         ORLT      '0'
029500971023     C     W001A         ANDNE     *BLANK
029600971021     C                   SETON                                        32
029700971021     C                   MOVE      *ZERO         VABNSP
029800050303     C                   MOVE      *ZERO         VATNSP
029900010604     C                   eval      vinmsg = %trimr(vinmsg)
030000010604     C                             + ' ' + 'VHBNSP'
030100971021     C                   ELSE
030200971021     C                   MOVE      VHBNSP        VABNSP
030300050303     C                   MOVE      VHBNSP        VATNSP
030400971021     C                   ENDIF
030500971021     C*
030600971023     C                   TESTN                   VHBLNA               30
030700971023     C  N30              TESTN                   VHBLNA                 30
030800971023     C  N30              TESTN                   VHBLNA                   30
030900971021     C                   MOVE      VHBLNA        W001A             1
031000971021     C     *IN30         IFEQ      *OFF
031100971021     C     W001A         ORLT      '0'
031200971023     C     W001A         ANDNE     *BLANK
031300971021     C                   SETON                                        32
031400971021     C                   MOVE      *ZERO         VABLNA
031500010604     C                   eval      vinmsg = %trimr(vinmsg)
031600010604     C                             + ' ' + 'VHBLNA'
031700971021     C                   ELSE
031800971021     C                   MOVE      VHBLNA        VABLNA
031900971021     C                   ENDIF
032000971021     C*
032100971023     C                   TESTN                   VHBCTR               30
032200971023     C  N30              TESTN                   VHBCTR                 30
032300971023     C  N30              TESTN                   VHBCTR                   30
032400971021     C                   MOVE      VHBCTR        W001A             1
032500971021     C     *IN30         IFEQ      *OFF
032600971021     C     W001A         ORLT      '0'
032700971023     C     W001A         ANDNE     *BLANK
032800971021     C                   SETON                                        32
032900971021     C                   MOVE      *ZERO         VABCTR
033000010604     C                   eval      vinmsg = %trimr(vinmsg)
033100010604     C                             + ' ' + 'VHBCTR'
033200971021     C                   ELSE
033300971021     C                   MOVE      VHBCTR        VABCTR
033400971021     C                   ENDIF
033500941229     C*
033600971021     C                   MOVE      *ZERO         VABIAS
033700941229     C                   MOVE      VHBIAS        WASC15
033800971023     C                   TESTN                   WI15                 30
033900971023     C  N30              TESTN                   WI15                   30
034000971023     C  N30              TESTN                   WI15                     30
034100971021     C                   MOVE      WI15          W001A             1
034200971021     C     *IN30         IFEQ      *OFF
034300971021     C     W001A         ORLT      '0'
034400971023     C     W001A         ANDNE     *BLANK
034500971021     C                   SETON                                        32
034600971021     C                   MOVE      *ZERO         VABIAS
034700010604     C                   eval      vinmsg = %trimr(vinmsg)
034800010604     C                             + ' ' + 'VHBIAS'
034900971021     C                   ELSE
035000971021     C                   MOVEL     WI15          VABIAS
035100971023     C                   TESTN                   WD15                 30
035200971023     C  N30              TESTN                   WD15                   30
035300971023     C  N30              TESTN                   WD15                     30
035400971021     C                   MOVE      WD15          W001A             1
035500971021     C     *IN30         IFEQ      *OFF
035600971021     C     W001A         ORLT      '0'
035700971023     C     W001A         ANDNE     *BLANK
035800971021     C                   SETON                                        32
035900971021     C                   MOVE      *ZERO         VABIAS
036000010604     C                   eval      vinmsg = %trimr(vinmsg)
036100010604     C                             + ' ' + 'VHBIAS'
036200971021     C                   ELSE
036300971021     C                   MOVE      WD15          VABIAS
036400971021     C                   ENDIF
036500050118     C                   ENDIF
036600941229     C*
036700971023     C                   TESTN                   VHBNCL               30
036800971023     C  N30              TESTN                   VHBNCL                 30
036900971023     C  N30              TESTN                   VHBNCL                   30
037000971021     C                   MOVE      VHBNCL        W001A             1
037100971021     C     *IN30         IFEQ      *OFF
037200971021     C     W001A         ORLT      '0'
037300971021     C                   SETON                                        32
037400971021     C                   MOVE      *ZERO         VABNCL
037500010604     C                   eval      vinmsg = %trimr(vinmsg)
037600010604     C                             + ' ' + 'VHBNCL'
037700971021     C                   ELSE
037800971021     C                   MOVE      VHBNCL        VABNCL
037900971021     C                   ENDIF
038000941229     C*
038100971021     C                   MOVE      *ZERO         VABPKB
038200971021     C                   MOVE      VHBPKB        WASC09
038300010719     C                   IF        WI09 = *blanks
038400010719     C                   EVAL      WI09 = *zeros
038500010719     C                   ENDIF
038600971023     C                   TESTN                   WI09                 30
038700971023     C  N30              TESTN                   WI09                   30
038800971023     C  N30              TESTN                   WI09                     30
038900971021     C                   MOVE      WI09          W001A             1
039000971021     C     *IN30         IFEQ      *OFF
039100971021     C     W001A         ORLT      '0'
039200971021     C                   SETON                                        32
039300971021     C                   MOVE      *ZERO         VABPKB
039400010604     C                   eval      vinmsg = %trimr(vinmsg)
039500010604     C                             + ' ' + 'VHBPKB'
039600971021     C                   ELSE
039700971021     C                   MOVEL     WI09          VABPKB
039800010719     C                   IF        WD09 = *blanks
039900010719     C                   EVAL      WD09 = *zeros
040000010719     C                   ENDIF
040100971023     C                   TESTN                   WD09                 30
040200971023     C  N30              TESTN                   WD09                     30
040300971021     C                   MOVE      WD09          W001A             1
040400971021     C     *IN30         IFEQ      *OFF
040500971021     C     W001A         ORLT      '0'
040600971023     C     W001A         ANDNE     *BLANK
040700971021     C                   SETON                                        32
040800971021     C                   MOVE      *ZERO         VABPKB
040900010604     C                   eval      vinmsg = %trimr(vinmsg)
041000010604     C                             + ' ' + 'VHBPKB'
041100971021     C                   ELSE
041200971021     C                   MOVE      WD09          VABPKB
041300971021     C                   ENDIF
041400050118     C                   ENDIF
041500971021     C*
041600041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
041700041001     C                   IF        *IN51 = *OFF
041800971021     C                   MOVE      *ZERO         VABVLB
041900971021     C                   MOVE      VHBVLB        WASC07
042000010719     C                   IF        WI07 = *blanks
042100010719     C                   EVAL      WI07 = *zeros
042200010719     C                   ENDIF
042300971023     C                   TESTN                   WI07                 30
042400971023     C  N30              TESTN                   WI07                   30
042500971023     C  N30              TESTN                   WI07                     30
042600971021     C                   MOVE      WI07          W001A             1
042700971021     C     *IN30         IFEQ      *OFF
042800971021     C     W001A         ORLT      '0'
042900971023     C     W001A         ANDNE     *BLANK
043000971021     C                   SETON                                        32
043100971021     C                   MOVE      *ZERO         VABVLB
043200010604     C                   eval      vinmsg = %trimr(vinmsg)
043300010604     C                             + ' ' + 'VHBVLB'
043400971021     C                   ELSE
043500971021     C                   MOVEL     WI07          VABVLB
043600010719     C                   IF        WD07 = *blanks
043700010719     C                   EVAL      WD07 = *zeros
043800010719     C                   ENDIF
043900971023     C                   TESTN                   WD07                 30
044000971023     C  N30              TESTN                   WD07                   30
044100971023     C  N30              TESTN                   WD07                     30
044200971021     C                   MOVE      WD07          W001A             1
044300971021     C     *IN30         IFEQ      *OFF
044400971021     C     W001A         ORLT      '0'
044500971023     C     W001A         ANDNE     *BLANK
044600971021     C                   SETON                                        32
044700971021     C                   MOVE      *ZERO         VABVLB
044800010604     C                   eval      vinmsg = %trimr(vinmsg)
044900010604     C                             + ' ' + 'VHBVLB'
045000971021     C                   ELSE
045100971021     C                   MOVE      WD07          VABVLB
045200971021     C                   ENDIF
045300050118     C                   ENDIF
045400041001     C                   ELSE
045500041001     C* ...altrimenti forzo azzeramento del volume
045600041001     C                   Z-ADD     *zeros        VABVLB
045700041001     C                   ENDIF
045800941229     C*
045900971021     C                   MOVE      *ZERO         VABQFT
046000971021     C                   MOVE      VHBQFT        WASC15
046100010719     C                   IF        WI15 = *blanks
046200010719     C                   EVAL      WI15 = *zeros
046300010719     C                   ENDIF
046400971023     C                   TESTN                   WI15                 30
046500971023     C  N30              TESTN                   WI15                   30
046600971023     C  N30              TESTN                   WI15                     30
046700971021     C                   MOVE      WI15          W001A             1
046800971021     C     *IN30         IFEQ      *OFF
046900971021     C     W001A         ORLT      '0'
047000971023     C     W001A         ANDNE     *BLANK
047100971021     C                   SETON                                        32
047200971021     C                   MOVE      *ZERO         VABQFT
047300010604     C                   eval      vinmsg = %trimr(vinmsg)
047400010604     C                             + ' ' + 'VHBQFT'
047500971021     C                   ELSE
047600971021     C                   MOVEL     WI15          VABQFT
047700010719     C                   IF        WD15 = *blanks
047800010719     C                   EVAL      WD15 = *zeros
047900010719     C                   ENDIF
048000971023     C                   TESTN                   WD15                 30
048100971023     C  N30              TESTN                   WD15                   30
048200971023     C  N30              TESTN                   WD15                     30
048300971021     C                   MOVE      WD15          W001A             1
048400971021     C     *IN30         IFEQ      *OFF
048500971021     C     W001A         ORLT      '0'
048600971023     C     W001A         ANDNE     *BLANK
048700971021     C                   SETON                                        32
048800971021     C                   MOVE      *ZERO         VABQFT
048900010604     C                   eval      vinmsg = %trimr(vinmsg)
049000010604     C                             + ' ' + 'VHBQFT'
049100971021     C                   ELSE
049200971021     C                   MOVE      WD15          VABQFT
049300971021     C                   ENDIF
049400050118     C                   ENDIF
049500941229     C*
049600971021     C                   MOVE      *ZERO         VABCAS
049700971021     C                   MOVE      VHBCAS        WASC15
049800010719     C                   IF        WI15 = *blanks
049900010719     C                   EVAL      WI15 = *zeros
050000010719     C                   ENDIF
050100971023     C                   TESTN                   WI15                 30
050200971023     C  N30              TESTN                   WI15                   30
050300971023     C  N30              TESTN                   WI15                     30
050400971021     C                   MOVE      WI15          W001A             1
050500971021     C     *IN30         IFEQ      *OFF
050600971021     C     W001A         ORLT      '0'
050700971023     C     W001A         ANDNE     *BLANK
050800971021     C                   SETON                                        32
050900971021     C                   MOVE      *ZERO         VABCAS
051000010604     C                   eval      vinmsg = %trimr(vinmsg)
051100010604     C                             + ' ' + 'VHBCAS'
051200971021     C                   ELSE
051300971021     C                   MOVEL     WI15          VABCAS
051400010719     C                   IF        WD15 = *blanks
051500010719     C                   EVAL      WD15 = *zeros
051600010719     C                   ENDIF
051700971023     C                   TESTN                   WD15                 30
051800971023     C  N30              TESTN                   WD15                   30
051900971023     C  N30              TESTN                   WD15                     30
052000971021     C                   MOVE      WD15          W001A             1
052100971021     C     *IN30         IFEQ      *OFF
052200971021     C     W001A         ORLT      '0'
052300971023     C     W001A         ANDNE     *BLANK
052400971021     C                   SETON                                        32
052500971021     C                   MOVE      *ZERO         VABCAS
052600010604     C                   eval      vinmsg = %trimr(vinmsg)
052700010604     C                             + ' ' + 'VHBCAS'
052800971021     C                   ELSE
052900971021     C                   MOVE      WD15          VABCAS
053000971021     C                   ENDIF
053100040827     C                   ENDIF
053200971021     C*
053300971023     C                   TESTN                   VHBRMN               30
053400971023     C  N30              TESTN                   VHBRMN                 30
053500971023     C  N30              TESTN                   VHBRMN                   30
053600971021     C                   MOVE      VHBRMN        W001A             1
053700971021     C     *IN30         IFEQ      *OFF
053800971021     C     W001A         ORLT      '0'
053900971021     C                   SETON                                        32
054000971021     C                   MOVE      *ZERO         VABRMN
054100010604     C                   eval      vinmsg = %trimr(vinmsg)
054200010604     C                             + ' ' + 'VHBRMN'
054300971021     C                   ELSE
054400971021     C                   MOVE      VHBRMN        VABRMN
054500971021     C                   ENDIF
054600971021     C*
054700971023     C                   TESTN                   VHBNCD               30
054800971023     C  N30              TESTN                   VHBNCD                 30
054900971023     C  N30              TESTN                   VHBNCD                   30
055000971021     C                   MOVE      VHBNCD        W001A             1
055100971021     C     *IN30         IFEQ      *OFF
055200971021     C     W001A         ORLT      '0'
055300971023     C     W001A         ANDNE     *BLANK
055400971021     C                   SETON                                        32
055500971021     C                   MOVE      *ZERO         VABNCD
055600010604     C                   eval      vinmsg = %trimr(vinmsg)
055700010604     C                             + ' ' + 'VHBNCD'
055800971021     C                   ELSE
055900971021     C                   MOVE      VHBNCD        VABNCD
056000971021     C                   ENDIF
056100971021     C*
056200971023     C                   TESTN                   VHBNCA               30
056300971023     C  N30              TESTN                   VHBNCA                 30
056400971023     C  N30              TESTN                   VHBNCA                   30
056500971021     C                   MOVE      VHBNCA        W001A             1
056600971021     C     *IN30         IFEQ      *OFF
056700971021     C     W001A         ORLT      '0'
056800971023     C     W001A         ANDNE     *BLANK
056900971021     C                   SETON                                        32
057000971021     C                   MOVE      *ZERO         VABNCA
057100010604     C                   eval      vinmsg = %trimr(vinmsg)
057200010604     C                             + ' ' + 'VHBNCA'
057300971021     C                   ELSE
057400971021     C                   MOVE      VHBNCA        VABNCA
057500971021     C                   ENDIF
057600971021     C*
057700971023     C                   TESTN                   VHBZNC               30
057800971023     C  N30              TESTN                   VHBZNC                 30
057900971023     C  N30              TESTN                   VHBZNC                   30
058000971021     C                   MOVE      VHBZNC        W001A             1
058100971021     C     *IN30         IFEQ      *OFF
058200971021     C     W001A         ORLT      '0'
058300971023     C     W001A         ANDNE     *BLANK
058400971021     C                   SETON                                        32
058500971021     C                   MOVE      *ZERO         VABZNC
058600010604     C                   eval      vinmsg = %trimr(vinmsg)
058700010604     C                             + ' ' + 'VHBZNC'
058800971021     C                   ELSE
058900971021     C                   MOVE      VHBZNC        VABZNC
059000971021     C                   ENDIF
059100971021     C*
059200971023     C                   TESTN                   VHBDCR               30
059300971023     C  N30              TESTN                   VHBDCR                 30
059400971023     C  N30              TESTN                   VHBDCR                   30
059500971021     C                   MOVE      VHBDCR        W001A             1
059600971021     C     *IN30         IFEQ      *OFF
059700971021     C     W001A         ORLT      '0'
059800971023     C     W001A         ANDNE     *BLANK
059900971021     C                   SETON                                        32
060000971021     C                   MOVE      *ZERO         VABDCR
060100010604     C                   eval      vinmsg = %trimr(vinmsg)
060200010604     C                             + ' ' + 'VHBDCR'
060300971021     C                   ELSE
060400971021     C                   MOVE      VHBDCR        VABDCR
060500971021     C                   ENDIF
060600971021     C*
060700971023     C                   TESTN                   VHBHCR               30
060800971023     C  N30              TESTN                   VHBHCR                 30
060900971023     C  N30              TESTN                   VHBHCR                   30
061000971021     C                   MOVE      VHBHCR        W001A             1
061100971021     C     *IN30         IFEQ      *OFF
061200971021     C     W001A         ORLT      '0'
061300971023     C     W001A         ANDNE     *BLANK
061400971021     C                   SETON                                        32
061500971021     C                   MOVE      *ZERO         VABHCR
061600010604     C                   eval      vinmsg = %trimr(vinmsg)
061700010604     C                             + ' ' + 'VHBHCR'
061800971021     C                   ELSE
061900971021     C                   MOVE      VHBHCR        VABHCR
062000971021     C                   ENDIF
062100941229     C*
062200971021     C                   MOVE      *ZERO         VABVMD
062300971021     C                   MOVE      VHBVMD        WASC15
062400010719     C                   IF        WI15 = *blanks
062500010719     C                   EVAL      WI15 = *zeros
062600010719     C                   ENDIF
062700971023     C                   TESTN                   WI15                 30
062800971023     C  N30              TESTN                   WI15                   30
062900971023     C  N30              TESTN                   WI15                     30
063000971021     C                   MOVE      WI15          W001A             1
063100971021     C     *IN30         IFEQ      *OFF
063200971021     C     W001A         ORLT      '0'
063300971023     C     W001A         ANDNE     *BLANK
063400971021     C                   SETON                                        32
063500971021     C                   MOVE      *ZERO         VABVMD
063600010604     C                   eval      vinmsg = %trimr(vinmsg)
063700010604     C                             + ' ' + 'VHBVMD'
063800971021     C                   ELSE
063900971021     C                   MOVEL     WI15          VABVMD
064000010719     C                   IF        WD15 = *blanks
064100010719     C                   EVAL      WD15 = *zeros
064200010719     C                   ENDIF
064300971023     C                   TESTN                   WD15                 30
064400971023     C  N30              TESTN                   WD15                   30
064500971023     C  N30              TESTN                   WD15                     30
064600971021     C                   MOVE      WD15          W001A             1
064700971021     C     *IN30         IFEQ      *OFF
064800971021     C     W001A         ORLT      '0'
064900971023     C     W001A         ANDNE     *BLANK
065000971021     C                   SETON                                        32
065100971021     C                   MOVE      *ZERO         VABVMD
065200010604     C                   eval      vinmsg = %trimr(vinmsg)
065300010604     C                             + ' ' + 'VHBVMD'
065400971021     C                   ELSE
065500971021     C                   MOVE      WD15          VABVMD
065600971021     C                   ENDIF
065700050118     C                   ENDIF
065800971021     C*
065900971023     C                   TESTN                   VHBANT               30
066000971023     C  N30              TESTN                   VHBANT                 30
066100971023     C  N30              TESTN                   VHBANT                   30
066200971021     C                   MOVE      VHBANT        W001A             1
066300971021     C     *IN30         IFEQ      *OFF
066400971021     C     W001A         ORLT      '0'
066500971023     C     W001A         ANDNE     *BLANK
066600971021     C                   SETON                                        32
066700971021     C                   MOVE      *ZERO         VABANT
066800010604     C                   eval      vinmsg = %trimr(vinmsg)
066900010604     C                             + ' ' + 'VHBANT'
067000971021     C                   ELSE
067100971021     C                   MOVE      VHBANT        VABANT
067200971021     C                   ENDIF
067300971021     C*
067400971021     C* CAMPI ALFABETICI:
067500170830     C***                MOVE      VHBATB        VABATB
067600971021     C                   MOVE      VHBCBO        VABCBO
067700971021     C                   MOVE      VHBRSD        VABRSD
067800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
067900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
068000020117     C* ==
068100971021     C                   MOVE      VHBRD2        VABRD2
068200971021     C                   MOVE      VHBIND        VABIND
068300971021     C                   MOVE      VHBCAD        VABCAD
068400971021     C                   MOVE      VHBLOD        VABLOD
068500971021     C                   MOVE      VHBPRD        VABPRD
068600971021     C                   MOVE      VHBNZD        VABNZD
068700971021     C                   MOVE      VHBGC1        VABGC1
068800971021     C                   MOVE      VHBGC2        VABGC2
068900971021     C                   MOVE      VHBTSP        VABTSP
069000971021     C                   MOVE      VHBVAS        VABVAS
069100971021     C                   MOVE      VHBNAS        VABNAS
069200971021     C                   MOVE      VHBTIC        VABTIC
069300971021     C                   MOVE      VHBVCA        VABVCA
069400971021     C                   MOVE      VHBGCA        VABGCA
069500971021     C                   MOVE      VHBRMA        VABRMA
069600971021     C                   MOVE      VHBXCO        VABXCO
069700971021     C                   MOVE      VHBNOT        VABNOT
069800971021     C                   MOVE      VHBNT2        VABNT2
069900100719     C  N53              MOVE      VHBCTM        VABCTM
070000971021     C                   MOVE      VHBFFD        VABFFD
070100971021     C                   MOVE      VHBTCR        VABTCR
070200971021     C                   MOVE      VHBCTS        VABCTS
070300971021     C                   MOVE      VHBFTM        VABFTM
070400971021     C                   MOVE      VHBVAD        VABVAD
070500971021     C                   MOVE      VHBGMA        VABGMA
070600971021     C                   MOVE      VHBGGA        VABGGA
070700971021     C                   MOVE      VHBGVA        VABGVA
070800971021     C                   MOVE      VHBTC1        VABTC1
070900971021     C                   MOVE      VHBTC2        VABTC2
071000971021     C                   MOVE      VHBSCL        VABSCL
071100971021     C                   MOVE      VHBRMO        VABRMO
071200971021     C                   MOVE      VHBCMO        VABCMO
071300971021     C                   MOVE      VHBNMO        VABNMO
071400971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
071500971021     C* NELLA NAZIONE ORIGINALE
071600971021     C                   MOVE      VHBNMO        WA1D              1
071700971021     C                   MOVEL     VHBNMO        WA1S              1
071800971021     C     WA1D          IFNE      *BLANK
071900971021     C     WA1S          ANDEQ     *BLANK
072000971021     C                   MOVE      *BLANK        VABNMO
072100971021     C                   END
072200050209     C*
072300050303     C* NSP => Se nn valorizzata numero spedizione => stacco numeratore da AZNUM
072400050303     C                   if        VABNSP = *zeros
072500050209     C                   clear                   TRUL33DS
072600050209     C                   eval      I33OPE = *zeros
072700050209     C                   eval      I33CNU = 302
072800050209     C                   eval      I33NUM = 1
072900050209     C                   movel     TRUL33DS      KPJBU
073000050209     C                   call      'TRUL33R'
073100050209     C                   parm                    KPJBA
073200050209     C                   movel     KPJBU         TRUL33DS
073300050209     C                   if        O33ERR = *zeros
073400050209     C                   z-add     O33NRF        VABNSP
073500050209     C                   z-add     O33NRF        VATNSP
073600050209     C                   else
073700050209     C                   SETON                                        31
073800050209     C                   EVAL      vinmsg = %trimr(vinmsg)
073900050209     C                             + ' ' + 'VABNSP VATNSP'
074000050209     C                   endif
074100050303     C                   endif
074200100719     C*
074300100719     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
074400100719     C* NELLA NAZIONE MITTENTE ORIGINALE
074500100719     C                   IF        %trim(VHBNMO) < 'A'
074600100719     C                   MOVE      *BLANKS       VABNMO
074700100719     C                   ENDIF
074800100719     C*
074900100719     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
075000100719     C                   IF        %trim(VABNZD) = 'I'   OR
075100100719     C                             %trim(VABNZD) = 'IT'  OR
075200100719     C                             %trim(VABNZD) = 'ITA'
075300100719     C                   EVAL      VABNZD = *blanks
075400100719     C                   ENDIF
075500100719     C*
075600100719     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
075700100719     C                   IF        *IN52 = *ON
075800100719     C* Reperisco la provincia dal CAP e dalla località
075900100719     C                   IF        VABCAD <> *blanks AND
076000100719     C                             VABPRD  = *blanks AND
076100100719     C                             VABNZD  = *blanks
076200100719     C                   CLEAR                   TISI95DS
076300100719     C                   EVAL      I95TCN = '3'
076400100719     C                   Z-ADD     datcor        I95DAT
076500100719     C                   EVAL      I95CAP = VABCAD
076600100719     C                   EVAL      I95LOC = VABLOD
076700100719     C                   CALL      'TISI95R'
076800100719     C                   PARM                    TISI95DS
076900100719     C                   EVAL      VABPRD = O95PRV
077000100719     C                   ENDIF
077100100719     C                   ENDIF
077200100719     C*
077300100719     C* "bonifico" i dati d input da eventuali caratteri indesiderati
077400100719     C                   exsr      exeBon
077500100719     C*
077600100719     C* Forzatura su campi DIVISE
077700100719     C                   IF        VABVAS = *blanks
077800100719     C                   EVAL      VABVAS = 'EUR'
077900100719     C                   ENDIF
078000100719     C                   IF        VABVCA = *blanks
078100100719     C                   EVAL      VABVCA = 'EUR'
078200100719     C                   ENDIF
078300100719     C                   IF        VABVAD = *blanks
078400100719     C                   EVAL      VABVAD = 'EUR'
078500100719     C                   ENDIF
078600930409     C*
078700010604     C  N31              ADD       1             §CTROK            7 0
078800010604     C   32              ADD       1             §CTRMO            7 0
078900010604     C   31              ADD       1             §CTRNO            7 0
079000020913     C  N31              WRITE     FIVAB000
079100100719     C  N31
079200100719     CAN 55              EXSR      WRIVAT
079300990910     C*
079400010601     C                   if        *in31 = *off and
079500010601     C                             *in32 = *off
079600990910     C                   eval      vinflg = '1'
079700990910     C                   else
079800990910     C                   eval      vinflg = '2'
079900990910     C                   endif
080000991022     C                   endif
080100990910     C                   endif
080200010601     C*
080300010601     C                   endif
080400000905     C*
080500000905     C                   else
080600000905     C                   eval      vinflg = '1'
080700000905     C                   endif
080800000905     C*
080900000905     C  N70              update    tivin000
081000991022     C*
081100991022     C  N70              ENDdo
081200010530     C
081300010530     C*
081400010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
081500010530     C                   if        cntNonEl = *zeros or
081600010530     C                             flgMulti = '0'
081700990910     C* Se non ci sono record con errori ...
081800010530     C                   if        §ctrno = 0 and
081900010601     C                             §ctrmo = 0 and
082000010530     C                             flgStato <> '2'
082100990910     C* ... restituisco esito OK.
082200990921     C                   eval      wrkesito = '0'
082300990910     C                   else
082400010601     C                   if        §ctrok > 0
082500990921     C                   eval      wrkesito = '1'
082600000710     C                   else
082700010615     C                   if        flgOk = '0'
082800010615     C                   eval      wrkesito = '2'
082900010615     C                   else
083000010615     C                   eval      wrkesito = '6'
083100010615     C                   endif
083200990910     C                   endif
083300000710     C                   endif
083400010530     C                   else
083500010530     C                   eval      wrkesito = '9'
083600010530     C                   endif
083700050209     C*
083800050209     C                   else
083900050209     C                   eval      wrkesito = '2'
084000050209     C                   endif
084100990910     C*
084200990914     C                   if        %open(tivin00r)
084300990908     C                   close     tivin00r
084400990914     C                   endif
084500020913     C                   if        %open(fivabwwr)
084600020913     C                   close     fivabwwr
084700990914     C                   endif
084800050209     C                   if        %open(fivatwwr)
084900050209     C                   close     fivatwwr
085000050209     C                   endif
085100010530     C*
085200010530     C                   if        vlrpoi <> 999
085300010530     C                   eval      invfil = vlrpoi
085400010530     C                   endif
085500990910     C*
085600990920     C                   if        §ctrok > 0
085700010530     C                             and invfil > *zeros
085800000613     C                   exsr      invio
085900990920     C                   endif
086000010612     C*
086100010612     C                   if        flgGiro = '1'
086200010612     C                   exsr      endela
086300010612     C                   endif
086400990920     C*
086500910830     C                   ENDSR
086600000613     C***
086700050209
086800050209
086900050209
087000050209     C*----------------------------------------------------*
087100050209     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
087200050209     C*----------------------------------------------------*
087300050209     C     PREVAT        BEGSR
087400050209     C*
087500050209     C* Compongo il nome del membro da dare al FIVATWWR
087600050209     C                   eval      parmbr = vlrhdl
087700050209     C                   movel     'M'           parmbr
087800050209     C                   eval      parccm = %subst(vlrKSC:2:7)
087900050209     C                   eval      paropz = '1'
088000050209     C* Effettuo la chiamata al CLLE preposto
088100050209     C                   call(e)   'TITVVTC'
088200050209     C                   parm                    parccm
088300050209     C                   parm                    parmbr
088400050209     C                   parm                    paropz
088500050209     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
088600050209     C                   if        %error
088700050209     C                   movel     '1'           chkcall
088800050209     C                   else
088900050209     C                   movel     '0'           chkcall
089000050209     C                   endif
089100050209     C*
089200050209     C                   ENDSR
089300100719
089400100719
089500100719
089600100719     C*----------------------------------------------------*
089700100719     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
089800100719     C*----------------------------------------------------*
089900100719     C     WRIVAT        BEGSR
090000100719     C*
090100100719     C* Valorizzo il buffer del file dettagli barcode colli
090200100719     C                   EVAL      VATFGS = VABFGS
090300100719     C                   EVAL      VATAAS = VABAAS
090400100719     C                   EVAL      VATLNP = VABLNP
090500100719     C                   EVAL      VATNSP = VABNSP
090600100719     C                   EVAL      VATCCM = VABCCM
090700100719     C                   EVAL      VATTRC='E'
090800100719     C*
090900100719     C* Compongo tanti "chi sono" quanti sono i colli della spedizione sulla base del RMN
091000100719     C                   z-add     10            wIdxRMN           9 0
091100100719     C                   z-add     10            wIdxRMNsav        9 0
091200100719     C     2             do        wMultRMN
091300100719     C                   eval      wIdxRMN = wIdxRMN * 10
091400100719     C                   enddo
091500100719     C*
091600100719     C                   eval      wIdxRMNsav = wIdxRMN
091700100719     C                   movel     *blanks       wVATNOT_E        35
091800100719     C*
091900100719     C                   if        %subst(vlrppt:8:1) = 'N'
092000100719     C                   eval      wVATNOT_E = %trim(%editc(VABRMN:'Z'))
092100100719     C                   else
092200100719     C                   eval      wVATNOT_E = %trim(VABRMA)
092300100719     C                   endif
092400100719     C*
092500111019     C                   if        wMultRMN < 1
092600111019     C                   eval      VATNOT = %trim(wVATNOT_E)
092700111019     C                   if        VATNOT <> *blanks
092800111019     C                   write     FIVAT000
092900111019     C                   endif
093000111019     C*
093100111019     C                   else
093200100719     C     1             do        VABNCL        wColloNum         9 0
093300100719     C                   eval      wIdxRMN = wIdxRMNsav + wColloNum
093400100719     C                   eval      VATNOT = %trim(wVATNOT_E) +
093500100719     C                                      %subst(%trim(%char(wIdxRMN)):2)
093600100719     C                   if        VATNOT <> *blanks
093700100719     C                   write     FIVAT000
093800100719     C                   endif
093900100719     C                   enddo
094000111019     C                   endif
094100100719     C*
094200100719     C                   ENDSR
094300010530
094400010530
094500050209
094600010530
094700010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
094800010530     C     repfil        BEGSR
094900010530     C*
095000010530     C                   if        invfil = *zeros and
095100010530     C                             depfil > *zeros and
095200010613     C                             (vinflg = *blanks or
095300010613     C                              vinflg = *zeros)
095400010530     C
095500010530     C                   eval      invfil = depfil
095600010530     C                   endif
095700010530     C*
095800010530     C                   if        depfil <> invfil and
095900010530     C                             invfil > *zeros
096000010530     C                   eval      flgMulti = '1'
096100010530     C                   if        vinflg = *blanks
096200010530     C                   add       1             cntNonEl
096300010530     C                   endif
096400010530     C                   endif
096500010530     C*
096600010530     C                   if        vinflg = '2'
096700010530     C                   eval      flgStato = '2'
096800010530     C                   endif
096900010530     C*
097000010530     C                   ENDSR
097100010530     C***
097200050209
097300050209
097400050209
097500050209
097600050209      /TITLE Invio dei dati al punto operativo.
097700050209     C     invio         BEGSR
097800050209     C*
097900050209     C* 1° invio FIVAT
098000050209     C                   reset                   dscmz
098100050209     C                   move      vlrpoi        cmzdst
098200050209     C                   eval      cmzfld = 'FIVATWWR'
098300050209     C                   eval      cmzmbd = vlrhdl
098400050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
098500050209     C***                if        prmfir = *blanks
098600050209     C                   eval      cmzfla = 'FIVAT00F'
098700050209     C                   eval      cmzmba = 'FIVAT00F'
098800050209     C***                else
098900050209     C***                eval      cmzfla = prmfir
099000050209     C***                eval      cmzmba = prmfir
099100050209     C***                endif
099200050209     C                   eval      cmznrr = *zeros
099300050209     C                   move      §ctrok        cmznrr
099400050209     C                   eval      cmzlba = vlrfl1
099500050209     C                   call(e)   'TIS711C'
099600050209     C                   parm                    dscmz
099700050209     C                   parm      *blanks       esito
099800050209     C                   if        %error
099900050209     C                             or cmzerr = '1'
100000050209     C                             or esito  = '1'
100100050209     C                   eval      wrkesito = '3'
100200050209     C                   else
100300050209     C*
100400050209     C* 2° invio FIVAB
100500050209     C                   reset                   dscmz
100600050209     C                   move      vlrpoi        cmzdst
100700050209     C                   eval      cmzfld = vlrfou
100800050209     C                   eval      cmzmbd = vlrhdl
100900050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
101000050209     C***                if        prmfir = *blanks
101100050209     C                   eval      cmzfla = 'FIVAB00F'
101200050209     C                   eval      cmzmba = 'FIVAB00F'
101300050209     C***                else
101400050209     C***                eval      cmzfla = prmfir
101500050209     C***                eval      cmzmba = prmfir
101600050209     C***                endif
101700050209     C                   eval      cmznrr = *zeros
101800050209     C                   move      §ctrok        cmznrr
101900050209     C                   eval      cmzlba = vlrfl1
102000050209     C                   call(e)   'TIS711C'
102100050209     C                   parm                    dscmz
102200050209     C                   parm      *blanks       esito
102300050209     C                   if        %error
102400050209     C                             or cmzerr = '1'
102500050209     C                             or esito  = '1'
102600050209     C                   eval      wrkesito = '3'
102700050209     C                   endif
102800050209     C                   endif
102900050209     C*
103000050209     C                   ENDSR
103100050209     C***
103200010530
103300010530
103400010530
103500010530
103600010530      /TITLE Invio dei dati al punto operativo.
103700010530     C     opeini        BEGSR
103800010530     C*
103900010530     C* Inizializzo flag e contatori operativi
104000010530     C                   movel     '0'           flgGiro           1
104100010530     C                   movel     '0'           flgMulti          1
104200010530     C                   movel     '1'           flgStato          1
104300010615     C                   movel     '0'           flgOk             1
104400010530     C                   z-add     *zeros        cntNonEl         10 0
104500010530     C                   z-add     *zeros        depfil            3 0
104600010530     C                   z-add     *zeros        invfil            3 0
104700100719     C*
104800100719     C                   z-add     *zeros        wMultRMN          1 0
104900030407     C*
105000050209     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
105100030407     C                   if        %subst(vlrppt:1:1) = 'S'
105200030407     C                   seton                                        50
105300030407     C                   else
105400030407     C                   setoff                                       50
105500030407     C                   endif
105600041001     C*
105700050209     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
105800041001     C                   if        %subst(vlrppt:2:1) = 'S'
105900041001     C                   seton                                        51
106000041001     C                   else
106100041001     C                   setoff                                       51
106200041001     C                   endif
106300100719     C*
106400100719     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
106500100719     C* (naturalmente solo se nn già presente)
106600100719     C                   if        %subst(vlrppt:3:1) = 'S'
106700100719     C                   seton                                        52
106800100719     C                   else
106900100719     C                   setoff                                       52
107000100719     C                   endif
107100100719     C*
107200100719     C* Altri parametri...
107300100719     C                   setoff                                       53
107400100719     C                   if        %subst(vlrppt:4:2) <> *blanks
107500100719     C                   eval      vabctm = %subst(vlrppt:4:2)
107600100719     C                   seton                                        53
107700100719     C                   endif
107800010530     C*
107900100719     C                   setoff                                       54
108000100719     C                   if        %subst(vlrppt:6:1) <> *blanks
108100100719     C                   eval      PiDecChr = %subst(vlrppt:6:1)
108200100719     C                   seton                                        54
108300100719     C                   endif
108400100719     C*
108500100719     C                   setoff                                       55
108600100719     C                   if        %subst(vlrppt:7:1) <> *blanks
108700100719     C                   eval      PiStr=%trim(%subst(vlrppt:7:1))
108800100719     C                   exsr      CHKNUM
108900100719     C                   if        PiInt=*on
109000100719     C                   z-add     PiVal         wMultRMN
109100100719     C                   seton                                        55
109200100719     C                   endif
109300100719     C                   endif
109400100719     C*
109500100719     C                   if        *in55 = *on        and
109600100719     C                             (%subst(vlrppt:8:1) = 'N' or
109700100719     C                              %subst(vlrppt:8:1) = 'A')
109800100719     C                   else
109900100719     C                   setoff                                       55
110000100719     C                   endif
110100100719     C*
110200010530     C                   ENDSR
110300010530     C***
110400100719
110500100719
110600100719
110700100719
110800100719      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
110900100719     C     exeBon        BEGSR
111000100719     C*
111100100719     C* Imposto i parametri x il *pgm d "bonifica"
111200100719     C                   EVAL      ElencoChar = *blanks
111300100719     C                   EVAL      TipoElenco = *blanks
111400100719     C                   EVAL      CharSost   = *blanks
111500100719     C                   EVAL      UpperCase  = '1'
111600100719     C                   EVAL      ChkNull    = '1'
111700100719     C                   EVAL      CharNull   = *blanks
111800100719     C*
111900100719     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
112000100719     C                   EVAL      TxtInOut   = VABRSD
112100100719     C                   EVAL      EsitoBon   = *blanks
112200100719     C*
112300100719     C                   CALL      'XCHKCHAR'
112400100719     C                   PARM                    TxtInOut
112500100719     C                   PARM                    ElencoChar
112600100719     C                   PARM                    TipoElenco
112700100719     C                   PARM                    CharSost
112800100719     C                   PARM                    UpperCase
112900100719     C                   PARM                    ChkNull
113000100719     C                   PARM                    CharNull
113100100719     C                   PARM                    EsitoBon
113200100719     C*
113300100719     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
113400100719     C                   IF        EsitoBon = '1'
113500100719     C                   EVAL      VABRSD = TxtInOut
113600100719     C                   ENDIF
113700100719     C*
113800100719     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
113900100719     C                   EVAL      TxtInOut   = VABIND
114000100719     C                   EVAL      EsitoBon   = *blanks
114100100719     C*
114200100719     C                   CALL      'XCHKCHAR'
114300100719     C                   PARM                    TxtInOut
114400100719     C                   PARM                    ElencoChar
114500100719     C                   PARM                    TipoElenco
114600100719     C                   PARM                    CharSost
114700100719     C                   PARM                    UpperCase
114800100719     C                   PARM                    ChkNull
114900100719     C                   PARM                    CharNull
115000100719     C                   PARM                    EsitoBon
115100100719     C*
115200100719     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
115300100719     C                   IF        EsitoBon = '1'
115400100719     C                   EVAL      VABIND = TxtInOut
115500100719     C                   ENDIF
115600100719     C*
115700100719     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
115800100719     C                   EVAL      TxtInOut   = VABLOD
115900100719     C                   EVAL      EsitoBon   = *blanks
116000100719     C*
116100100719     C                   CALL      'XCHKCHAR'
116200100719     C                   PARM                    TxtInOut
116300100719     C                   PARM                    ElencoChar
116400100719     C                   PARM                    TipoElenco
116500100719     C                   PARM                    CharSost
116600100719     C                   PARM                    UpperCase
116700100719     C                   PARM                    ChkNull
116800100719     C                   PARM                    CharNull
116900100719     C                   PARM                    EsitoBon
117000100719     C*
117100100719     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
117200100719     C                   IF        EsitoBon = '1'
117300100719     C                   EVAL      VABLOD = TxtInOut
117400100719     C                   ENDIF
117500100719     C*
117600100719     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
117700100719     C                   EVAL      TxtInOut   = VABNOT
117800100719     C                   EVAL      EsitoBon   = *blanks
117900100719     C                   EVAL      UpperCase  = *blanks
118000100719     C*
118100100719     C                   CALL      'XCHKCHAR'
118200100719     C                   PARM                    TxtInOut
118300100719     C                   PARM                    ElencoChar
118400100719     C                   PARM                    TipoElenco
118500100719     C                   PARM                    CharSost
118600100719     C                   PARM                    UpperCase
118700100719     C                   PARM                    ChkNull
118800100719     C                   PARM                    CharNull
118900100719     C                   PARM                    EsitoBon
119000100719     C*
119100100719     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
119200100719     C                   IF        EsitoBon = '1'
119300100719     C                   EVAL      VABNOT = TxtInOut
119400100719     C                   ENDIF
119500100719     C*
119600100719     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
119700100719     C                   EVAL      TxtInOut   = VABNT2
119800100719     C                   EVAL      EsitoBon   = *blanks
119900100719     C                   EVAL      UpperCase  = *blanks
120000100719     C*
120100100719     C                   CALL      'XCHKCHAR'
120200100719     C                   PARM                    TxtInOut
120300100719     C                   PARM                    ElencoChar
120400100719     C                   PARM                    TipoElenco
120500100719     C                   PARM                    CharSost
120600100719     C                   PARM                    UpperCase
120700100719     C                   PARM                    ChkNull
120800100719     C                   PARM                    CharNull
120900100719     C                   PARM                    EsitoBon
121000100719     C*
121100100719     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
121200100719     C                   IF        EsitoBon = '1'
121300100719     C                   EVAL      VABNT2 = TxtInOut
121400100719     C                   ENDIF
121500100719     C*
121600100719     C                   ENDSR
121700100719
121800100719
121900100719
122000100719     C*----------------------------------------------------*
122100100719     C*  CONTROLLO NUMERICITA' CAMPI
122200100719     C*----------------------------------------------------*
122300100719     C     CHKNUM        BEGSR
122400100719     C*
122500100719     C                   IF        PiDecChr = *blanks
122600100719     C  N54              EVAL      PiDecChr = '.'
122700100719     C                   ENDIF
122800100719     C*
122900100719     C                   callp(e)  UBISNUM_Check(PiStr
123000100719     C                                          :PiDecChr
123100100719     C                                          :PiVal
123200100719     C                                          :PiNum
123300100719     C                                          :PiInt)
123400100719     C*
123500100719     C                   IF        %error
123600100719     C                   EVAL      PiInt=*off
123700100719     C                   ENDIF
123800100719     C*
123900100719     C                   ENDSR
124000100719     C***
124100100719
124200010530
124300990910
124400000613     C     *inzsr        BEGSR
124500990910     C*
124600990910     C     *entry        plist
124700990920     C                   parm                    tivlrds
124800990921     C                   parm      wrkesito      esito
124900000724     C                   parm                    prmlit
125000000710     C                   parm                    prmfir
125100010606     C*
125200010606     C* CALCOLA LA DATA CORRENTE
125300100719     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
125400100719     C                   eval      datcor = %dec(%date() : *ISO)
125500000613     C*
125600000613     C                   ENDSR
125700000613     C***
