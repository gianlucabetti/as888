000100070213      /TITLE Upload via Internet: traduzione in FIVABWWR. (SEM EXPRESS - Cod. 0052973)
000200990908     H dftactgrp(*yes)
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
003600990910     D esito           s              1
003700000724     D prmlit          s             10
003800000710     D prmfir          s             10
003900990921     D wrkesito        s                   like(esito)
004000000613     D rrnum           s              6  0 INZ(*zeros)
004100050209     D parccm          s              8    INZ(*blanks)
004200050209     D parmbr          s             10    INZ(*blanks)
004300050209     D paropz          s              1    INZ(*blanks)
004400050209     D chkcall         s              1    INZ(*blanks)
004500010606     D*------------------
004600010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
004700010606     D*------------------
004800010606     D WLBDA8          DS                  INZ
004900010606     D  G08DAT                 1      8  0
005000010606     D  G08INV                 9     16  0
005100010606     D  G08ERR                17     17
005200010606     D  G08TGI                18     22  0
005300050209     D*------------------
005400050209     D* DS REPERIMENTO NUMERATORE
005500050209     D*------------------
005600050209     D trul33ds      e ds                  inz
005700050209     D*------------------
005800050209     D* DS ARCHITETTURA
005900050209     D*------------------
006000050209     D kpjba         e ds                  inz
006100050209     D*------------------
006200990908
006300050209
006400050209
006500050209     C* /MAIN
006600000913     C                   reset                   rrnum
006700990921     C                   reset                   esito
006800990921     C                   reset                   wrkesito
006900010530     C*
007000010530     C                   exsr      opeini
007100010530     C                   exsr      rwvab
007200050209     C*
007300050209     C* Effettuo la chiamata al CLLE preposto
007400050209     C                   call(e)   'TITVVTC'
007500050209     C                   parm                    parccm
007600050209     C                   parm                    parmbr
007700050209     C                   parm      '2'           paropz
007800010530     C*
007900010530     C                   seton                                        lr
008000010530
008100010530
008200010530
008300010530
008400010530     C*--------------------------------------------------------
008500010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008600010530     C*--------------------------------------------------------
008700010530     C     PREELA        BEGSR
008800000724     C*
008900000724     C* SE OCCORRE SPEDIRE IN FILIALE
009000010530     C                   if        invfil <> *zeros and
009100010530     C                             flgGiro = '0'
009200010530     C*
009300010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009400010530     C                   eval      flgGiro = '1'
009500010530     C*
009600010530     C                   endif
009700010530     C*
009800010530     C                   ENDSR
009900010530     C***
010000010530
010100010530
010200010530
010300010530
010400010530
010500010530
010600010530     C*--------------------------------------------------------
010700010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010800010530     C*--------------------------------------------------------
010900010530     C     ENDELA        BEGSR
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
012800050209     C*
012900050209     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
013000050209     C                   exsr      prevat
013100050209     C*
013200050209     C                   if        chkcall = '0'
013300050209     C*
013400050209     C                   if        not %open(fivatwwr)
013500050209     C                   open      fivatwwr
013600050209     C                   endif
013700050209     C*
013800990910     C                   clear                   §CTROK
013900990910     C                   clear                   §CTRMO
014000990910     C                   clear                   §CTRNO
014100990910     C*
014200921023     C                   DO        *HIVAL
014300990913     C*
014400990915     C                   READ      tivin00r                               70
014500010618     C*
014600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
014700010618     C                   if        vinflg = '1'
014800010618     C                   eval      flgOk = '1'
014900010618     C                   endif
015000010705     C*
015100010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
015200010705     C                   if        %subst(vindta:1:1) = '='
015300010705     C                   eval      %subst(vindta:1:1) = *blanks
015400010705     C                   endif
015500010705     C*
015600000905     C                   if        vindta > *blanks
015700000613     C                   add       1             rrnum
015800010601     C* ...elimino eventuali segni
015900010601     C     '+':' '       xlate     vindta        vindta
016000990913     C*
016100010601     C                   if        *in70 = *off and
016200990913     C                             (vinflg = *blanks
016300010711     C                              or vinflg = '0'
016400010711     C                              or vinflg = '2')
016500990913     C*
016600020913     C                   clear                   FIVAB000
016700050209     C                   clear                   FIVAT000
016800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
016900010711     C                   if        vinflg = *blanks or vinflg = '0'
017000010711     C                   clear                   vinmsg
017100010711     C                   endif
017200010711     C*
017300990913     C                   eval      fnvabaar = vindta
017400991022     C*
017500991022     C                   testb     x'3F'         vhbatb                   02
017600991022     C                   if        not *in02
017700991022     C*
017800971021     C                   SETOFF                                       303132
017900971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
018000971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
018100971023     C*    OPPURE TUTTI BLANKS
018200010530     C*
018300010530     C                   TESTN                   VHBLNP               30
018400010530     C  N30              TESTN                   VHBLNP                 30
018500010530     C  N30              TESTN                   VHBLNP                   30
018600010530     C                   MOVE      VHBLNP        W001A             1
018700010530     C     *IN30         IFEQ      *OFF
018800010530     C     W001A         ORLT      '0'
018900010530     C                   SETON                                        31
019000010530     C                   eval      vinmsg = %trimr(vinmsg)
019100010530     C                             + ' ' + 'VHBLNP'
019200010530     C                   ELSE
019300010530     C                   MOVE      VHBLNP        VABLNP
019400010530     C     VABLNP        IFLE      *ZEROS
019500010530     C                   SETON                                        31
019600010530     C                   eval      vinmsg = %trimr(vinmsg)
019700010530     C                             + ' ' + 'VHBLNP'
019800010530     C                   ENDIF
019900010530     C                   ENDIF
020000010530     C*
020100010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020200010530     C                   eval      depfil = VABLNP
020300010530     C                   exsr      repfil
020400010601     C                   if        depfil = invfil
020500021014     C                   if        vlrpoi = 999
020600021010     C                   MOVE(P)   invfil        VABFGS
020700021014     C                   else
020800021014     C                   MOVE(P)   vlrpoi        VABFGS
020900021014     C                   endif
021000010530     C*
021100010530     C                   exsr      PREELA
021200010530     C*
021300971023     C                   TESTN                   VHBCCM               30
021400971023     C  N30              TESTN                   VHBCCM                 30
021500971023     C  N30              TESTN                   VHBCCM                   30
021600971021     C                   MOVE      VHBCCM        W001A             1
021700971021     C     *IN30         IFEQ      *OFF
021800971021     C     W001A         ORLT      '0'
021900971021     C                   SETON                                        31
022000010604     C                   eval      vinmsg = %trimr(vinmsg)
022100010604     C                             + ' ' + 'VHBCCM'
022200971021     C                   ELSE
022300971021     C                   MOVE      VHBCCM        VABCCM
022400971024     C     VABCCM        IFLE      *ZEROS
022500971024     C                   SETON                                        31
022600010604     C                   eval      vinmsg = %trimr(vinmsg)
022700010604     C                             + ' ' + 'VHBCCM'
022800971024     C                   ENDIF
022900971021     C                   ENDIF
023000971021     C*
023100971023     C                   TESTN                   VHBAAS               30
023200971023     C  N30              TESTN                   VHBAAS                 30
023300971023     C  N30              TESTN                   VHBAAS                   30
023400971021     C                   MOVE      VHBAAS        W001A             1
023500971021     C     *IN30         IFEQ      *OFF
023600971021     C     W001A         ORLT      '0'
023700971023     C     W001A         ANDNE     *BLANK
023800971021     C                   SETON                                        32
023900010606     C                   MOVEL     datcor        VABAAS
024000010604     C                   eval      vinmsg = %trimr(vinmsg)
024100010604     C                             + ' ' + 'VHBAAS'
024200971021     C                   ELSE
024300971021     C                   MOVE      VHBAAS        VABAAS
024400971021     C                   ENDIF
024500971021     C*
024600971023     C                   TESTN                   VHBMGS               30
024700971023     C  N30              TESTN                   VHBMGS                 30
024800971023     C  N30              TESTN                   VHBMGS                   30
024900971021     C                   MOVE      VHBMGS        W001A             1
025000971021     C     *IN30         IFEQ      *OFF
025100971021     C     W001A         ORLT      '0'
025200971023     C     W001A         ANDNE     *BLANK
025300971021     C                   SETON                                        32
025400010601     C                   MOVE      '0101'        VABMGS
025500010604     C                   eval      vinmsg = %trimr(vinmsg)
025600010604     C                             + ' ' + 'VHBMGS'
025700971021     C                   ELSE
025800971021     C                   MOVE      VHBMGS        VABMGS
025900971021     C                   ENDIF
026000971021     C*
026100971023     C                   TESTN                   VHBNRS               30
026200971023     C  N30              TESTN                   VHBNRS                 30
026300971023     C  N30              TESTN                   VHBNRS                   30
026400971021     C                   MOVE      VHBNRS        W001A             1
026500971021     C     *IN30         IFEQ      *OFF
026600971021     C     W001A         ORLT      '0'
026700971023     C     W001A         ANDNE     *BLANK
026800971021     C                   SETON                                        32
026900971021     C                   MOVE      *ZERO         VABNRS
027000010604     C                   eval      vinmsg = %trimr(vinmsg)
027100010604     C                             + ' ' + 'VHBNRS'
027200971021     C                   ELSE
027300971021     C                   MOVE      VHBNRS        VABNRS
027400971021     C                   ENDIF
027500971021     C*
027600971023     C                   TESTN                   VHBNSP               30
027700971023     C  N30              TESTN                   VHBNSP                 30
027800971023     C  N30              TESTN                   VHBNSP                   30
027900971021     C                   MOVE      VHBNSP        W001A             1
028000971021     C     *IN30         IFEQ      *OFF
028100971021     C     W001A         ORLT      '0'
028200971023     C     W001A         ANDNE     *BLANK
028300971021     C                   SETON                                        32
028400971021     C                   MOVE      *ZERO         VABNSP
028500010604     C                   eval      vinmsg = %trimr(vinmsg)
028600010604     C                             + ' ' + 'VHBNSP'
028700971021     C                   ELSE
028800971021     C                   MOVE      VHBNSP        VABNSP
028900971021     C                   ENDIF
029000971021     C*
029100971023     C                   TESTN                   VHBLNA               30
029200971023     C  N30              TESTN                   VHBLNA                 30
029300971023     C  N30              TESTN                   VHBLNA                   30
029400971021     C                   MOVE      VHBLNA        W001A             1
029500971021     C     *IN30         IFEQ      *OFF
029600971021     C     W001A         ORLT      '0'
029700971023     C     W001A         ANDNE     *BLANK
029800971021     C                   SETON                                        32
029900971021     C                   MOVE      *ZERO         VABLNA
030000010604     C                   eval      vinmsg = %trimr(vinmsg)
030100010604     C                             + ' ' + 'VHBLNA'
030200971021     C                   ELSE
030300971021     C                   MOVE      VHBLNA        VABLNA
030400971021     C                   ENDIF
030500971021     C*
030600971023     C                   TESTN                   VHBCTR               30
030700971023     C  N30              TESTN                   VHBCTR                 30
030800971023     C  N30              TESTN                   VHBCTR                   30
030900971021     C                   MOVE      VHBCTR        W001A             1
031000971021     C     *IN30         IFEQ      *OFF
031100971021     C     W001A         ORLT      '0'
031200971023     C     W001A         ANDNE     *BLANK
031300971021     C                   SETON                                        32
031400971021     C                   MOVE      *ZERO         VABCTR
031500010604     C                   eval      vinmsg = %trimr(vinmsg)
031600010604     C                             + ' ' + 'VHBCTR'
031700971021     C                   ELSE
031800971021     C                   MOVE      VHBCTR        VABCTR
031900971021     C                   ENDIF
032000941229     C*
032100971021     C                   MOVE      *ZERO         VABIAS
032200941229     C                   MOVE      VHBIAS        WASC15
032300971023     C                   TESTN                   WI15                 30
032400971023     C  N30              TESTN                   WI15                   30
032500971023     C  N30              TESTN                   WI15                     30
032600971021     C                   MOVE      WI15          W001A             1
032700971021     C     *IN30         IFEQ      *OFF
032800971021     C     W001A         ORLT      '0'
032900971023     C     W001A         ANDNE     *BLANK
033000971021     C                   SETON                                        32
033100971021     C                   MOVE      *ZERO         VABIAS
033200010604     C                   eval      vinmsg = %trimr(vinmsg)
033300010604     C                             + ' ' + 'VHBIAS'
033400971021     C                   ELSE
033500971021     C                   MOVEL     WI15          VABIAS
033600971023     C                   TESTN                   WD15                 30
033700971023     C  N30              TESTN                   WD15                   30
033800971023     C  N30              TESTN                   WD15                     30
033900971021     C                   MOVE      WD15          W001A             1
034000971021     C     *IN30         IFEQ      *OFF
034100971021     C     W001A         ORLT      '0'
034200971023     C     W001A         ANDNE     *BLANK
034300971021     C                   SETON                                        32
034400971021     C                   MOVE      *ZERO         VABIAS
034500010604     C                   eval      vinmsg = %trimr(vinmsg)
034600010604     C                             + ' ' + 'VHBIAS'
034700971021     C                   ELSE
034800971021     C                   MOVE      WD15          VABIAS
034900971021     C                   ENDIF
035000050118     C                   ENDIF
035100941229     C*
035200971023     C                   TESTN                   VHBNCL               30
035300971023     C  N30              TESTN                   VHBNCL                 30
035400971023     C  N30              TESTN                   VHBNCL                   30
035500971021     C                   MOVE      VHBNCL        W001A             1
035600971021     C     *IN30         IFEQ      *OFF
035700971021     C     W001A         ORLT      '0'
035800971021     C                   SETON                                        32
035900971021     C                   MOVE      *ZERO         VABNCL
036000010604     C                   eval      vinmsg = %trimr(vinmsg)
036100010604     C                             + ' ' + 'VHBNCL'
036200971021     C                   ELSE
036300971021     C                   MOVE      VHBNCL        VABNCL
036400971021     C                   ENDIF
036500941229     C*
036600971021     C                   MOVE      *ZERO         VABPKB
036700971021     C                   MOVE      VHBPKB        WASC09
036800010719     C                   IF        WI09 = *blanks
036900010719     C                   EVAL      WI09 = *zeros
037000010719     C                   ENDIF
037100971023     C                   TESTN                   WI09                 30
037200971023     C  N30              TESTN                   WI09                   30
037300971023     C  N30              TESTN                   WI09                     30
037400971021     C                   MOVE      WI09          W001A             1
037500971021     C     *IN30         IFEQ      *OFF
037600971021     C     W001A         ORLT      '0'
037700971021     C                   SETON                                        32
037800971021     C                   MOVE      *ZERO         VABPKB
037900010604     C                   eval      vinmsg = %trimr(vinmsg)
038000010604     C                             + ' ' + 'VHBPKB'
038100971021     C                   ELSE
038200971021     C                   MOVEL     WI09          VABPKB
038300010719     C                   IF        WD09 = *blanks
038400010719     C                   EVAL      WD09 = *zeros
038500010719     C                   ENDIF
038600971023     C                   TESTN                   WD09                 30
038700971023     C  N30              TESTN                   WD09                     30
038800971021     C                   MOVE      WD09          W001A             1
038900971021     C     *IN30         IFEQ      *OFF
039000971021     C     W001A         ORLT      '0'
039100971023     C     W001A         ANDNE     *BLANK
039200971021     C                   SETON                                        32
039300971021     C                   MOVE      *ZERO         VABPKB
039400010604     C                   eval      vinmsg = %trimr(vinmsg)
039500010604     C                             + ' ' + 'VHBPKB'
039600971021     C                   ELSE
039700971021     C                   MOVE      WD09          VABPKB
039800971021     C                   ENDIF
039900050118     C                   ENDIF
040000971021     C*
040100041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
040200041001     C                   IF        *IN51 = *OFF
040300971021     C                   MOVE      *ZERO         VABVLB
040400971021     C                   MOVE      VHBVLB        WASC07
040500010719     C                   IF        WI07 = *blanks
040600010719     C                   EVAL      WI07 = *zeros
040700010719     C                   ENDIF
040800971023     C                   TESTN                   WI07                 30
040900971023     C  N30              TESTN                   WI07                   30
041000971023     C  N30              TESTN                   WI07                     30
041100971021     C                   MOVE      WI07          W001A             1
041200971021     C     *IN30         IFEQ      *OFF
041300971021     C     W001A         ORLT      '0'
041400971023     C     W001A         ANDNE     *BLANK
041500971021     C                   SETON                                        32
041600971021     C                   MOVE      *ZERO         VABVLB
041700010604     C                   eval      vinmsg = %trimr(vinmsg)
041800010604     C                             + ' ' + 'VHBVLB'
041900971021     C                   ELSE
042000971021     C                   MOVEL     WI07          VABVLB
042100010719     C                   IF        WD07 = *blanks
042200010719     C                   EVAL      WD07 = *zeros
042300010719     C                   ENDIF
042400971023     C                   TESTN                   WD07                 30
042500971023     C  N30              TESTN                   WD07                   30
042600971023     C  N30              TESTN                   WD07                     30
042700971021     C                   MOVE      WD07          W001A             1
042800971021     C     *IN30         IFEQ      *OFF
042900971021     C     W001A         ORLT      '0'
043000971023     C     W001A         ANDNE     *BLANK
043100971021     C                   SETON                                        32
043200971021     C                   MOVE      *ZERO         VABVLB
043300010604     C                   eval      vinmsg = %trimr(vinmsg)
043400010604     C                             + ' ' + 'VHBVLB'
043500971021     C                   ELSE
043600971021     C                   MOVE      WD07          VABVLB
043700971021     C                   ENDIF
043800050118     C                   ENDIF
043900041001     C                   ELSE
044000041001     C* ...altrimenti forzo azzeramento del volume
044100041001     C                   Z-ADD     *zeros        VABVLB
044200041001     C                   ENDIF
044300941229     C*
044400971021     C                   MOVE      *ZERO         VABQFT
044500971021     C                   MOVE      VHBQFT        WASC15
044600010719     C                   IF        WI15 = *blanks
044700010719     C                   EVAL      WI15 = *zeros
044800010719     C                   ENDIF
044900971023     C                   TESTN                   WI15                 30
045000971023     C  N30              TESTN                   WI15                   30
045100971023     C  N30              TESTN                   WI15                     30
045200971021     C                   MOVE      WI15          W001A             1
045300971021     C     *IN30         IFEQ      *OFF
045400971021     C     W001A         ORLT      '0'
045500971023     C     W001A         ANDNE     *BLANK
045600971021     C                   SETON                                        32
045700971021     C                   MOVE      *ZERO         VABQFT
045800010604     C                   eval      vinmsg = %trimr(vinmsg)
045900010604     C                             + ' ' + 'VHBQFT'
046000971021     C                   ELSE
046100971021     C                   MOVEL     WI15          VABQFT
046200010719     C                   IF        WD15 = *blanks
046300010719     C                   EVAL      WD15 = *zeros
046400010719     C                   ENDIF
046500971023     C                   TESTN                   WD15                 30
046600971023     C  N30              TESTN                   WD15                   30
046700971023     C  N30              TESTN                   WD15                     30
046800971021     C                   MOVE      WD15          W001A             1
046900971021     C     *IN30         IFEQ      *OFF
047000971021     C     W001A         ORLT      '0'
047100971023     C     W001A         ANDNE     *BLANK
047200971021     C                   SETON                                        32
047300971021     C                   MOVE      *ZERO         VABQFT
047400010604     C                   eval      vinmsg = %trimr(vinmsg)
047500010604     C                             + ' ' + 'VHBQFT'
047600971021     C                   ELSE
047700971021     C                   MOVE      WD15          VABQFT
047800971021     C                   ENDIF
047900050118     C                   ENDIF
048000941229     C*
048100971021     C                   MOVE      *ZERO         VABCAS
048200971021     C                   MOVE      VHBCAS        WASC15
048300010719     C                   IF        WI15 = *blanks
048400010719     C                   EVAL      WI15 = *zeros
048500010719     C                   ENDIF
048600971023     C                   TESTN                   WI15                 30
048700971023     C  N30              TESTN                   WI15                   30
048800971023     C  N30              TESTN                   WI15                     30
048900971021     C                   MOVE      WI15          W001A             1
049000971021     C     *IN30         IFEQ      *OFF
049100971021     C     W001A         ORLT      '0'
049200971023     C     W001A         ANDNE     *BLANK
049300971021     C                   SETON                                        32
049400971021     C                   MOVE      *ZERO         VABCAS
049500010604     C                   eval      vinmsg = %trimr(vinmsg)
049600010604     C                             + ' ' + 'VHBCAS'
049700971021     C                   ELSE
049800971021     C                   MOVEL     WI15          VABCAS
049900010719     C                   IF        WD15 = *blanks
050000010719     C                   EVAL      WD15 = *zeros
050100010719     C                   ENDIF
050200971023     C                   TESTN                   WD15                 30
050300971023     C  N30              TESTN                   WD15                   30
050400971023     C  N30              TESTN                   WD15                     30
050500971021     C                   MOVE      WD15          W001A             1
050600971021     C     *IN30         IFEQ      *OFF
050700971021     C     W001A         ORLT      '0'
050800971023     C     W001A         ANDNE     *BLANK
050900971021     C                   SETON                                        32
051000971021     C                   MOVE      *ZERO         VABCAS
051100010604     C                   eval      vinmsg = %trimr(vinmsg)
051200010604     C                             + ' ' + 'VHBCAS'
051300971021     C                   ELSE
051400971021     C                   MOVE      WD15          VABCAS
051500971021     C                   ENDIF
051600040827     C                   ENDIF
051700971021     C*
051800971023     C                   TESTN                   VHBRMN               30
051900971023     C  N30              TESTN                   VHBRMN                 30
052000971023     C  N30              TESTN                   VHBRMN                   30
052100971021     C                   MOVE      VHBRMN        W001A             1
052200971021     C     *IN30         IFEQ      *OFF
052300971021     C     W001A         ORLT      '0'
052400971021     C                   SETON                                        32
052500971021     C                   MOVE      *ZERO         VABRMN
052600010604     C                   eval      vinmsg = %trimr(vinmsg)
052700010604     C                             + ' ' + 'VHBRMN'
052800971021     C                   ELSE
052900971021     C                   MOVE      VHBRMN        VABRMN
053000971021     C                   ENDIF
053100971021     C*
053200971023     C                   TESTN                   VHBNCD               30
053300971023     C  N30              TESTN                   VHBNCD                 30
053400971023     C  N30              TESTN                   VHBNCD                   30
053500971021     C                   MOVE      VHBNCD        W001A             1
053600971021     C     *IN30         IFEQ      *OFF
053700971021     C     W001A         ORLT      '0'
053800971023     C     W001A         ANDNE     *BLANK
053900971021     C                   SETON                                        32
054000971021     C                   MOVE      *ZERO         VABNCD
054100010604     C                   eval      vinmsg = %trimr(vinmsg)
054200010604     C                             + ' ' + 'VHBNCD'
054300971021     C                   ELSE
054400971021     C                   MOVE      VHBNCD        VABNCD
054500971021     C                   ENDIF
054600971021     C*
054700971023     C                   TESTN                   VHBNCA               30
054800971023     C  N30              TESTN                   VHBNCA                 30
054900971023     C  N30              TESTN                   VHBNCA                   30
055000971021     C                   MOVE      VHBNCA        W001A             1
055100971021     C     *IN30         IFEQ      *OFF
055200971021     C     W001A         ORLT      '0'
055300971023     C     W001A         ANDNE     *BLANK
055400971021     C                   SETON                                        32
055500971021     C                   MOVE      *ZERO         VABNCA
055600010604     C                   eval      vinmsg = %trimr(vinmsg)
055700010604     C                             + ' ' + 'VHBNCA'
055800971021     C                   ELSE
055900971021     C                   MOVE      VHBNCA        VABNCA
056000971021     C                   ENDIF
056100971021     C*
056200971023     C                   TESTN                   VHBZNC               30
056300971023     C  N30              TESTN                   VHBZNC                 30
056400971023     C  N30              TESTN                   VHBZNC                   30
056500971021     C                   MOVE      VHBZNC        W001A             1
056600971021     C     *IN30         IFEQ      *OFF
056700971021     C     W001A         ORLT      '0'
056800971023     C     W001A         ANDNE     *BLANK
056900971021     C                   SETON                                        32
057000971021     C                   MOVE      *ZERO         VABZNC
057100010604     C                   eval      vinmsg = %trimr(vinmsg)
057200010604     C                             + ' ' + 'VHBZNC'
057300971021     C                   ELSE
057400971021     C                   MOVE      VHBZNC        VABZNC
057500971021     C                   ENDIF
057600971021     C*
057700971023     C                   TESTN                   VHBDCR               30
057800971023     C  N30              TESTN                   VHBDCR                 30
057900971023     C  N30              TESTN                   VHBDCR                   30
058000971021     C                   MOVE      VHBDCR        W001A             1
058100971021     C     *IN30         IFEQ      *OFF
058200971021     C     W001A         ORLT      '0'
058300971023     C     W001A         ANDNE     *BLANK
058400971021     C                   SETON                                        32
058500971021     C                   MOVE      *ZERO         VABDCR
058600010604     C                   eval      vinmsg = %trimr(vinmsg)
058700010604     C                             + ' ' + 'VHBDCR'
058800971021     C                   ELSE
058900971021     C                   MOVE      VHBDCR        VABDCR
059000971021     C                   ENDIF
059100971021     C*
059200971023     C                   TESTN                   VHBHCR               30
059300971023     C  N30              TESTN                   VHBHCR                 30
059400971023     C  N30              TESTN                   VHBHCR                   30
059500971021     C                   MOVE      VHBHCR        W001A             1
059600971021     C     *IN30         IFEQ      *OFF
059700971021     C     W001A         ORLT      '0'
059800971023     C     W001A         ANDNE     *BLANK
059900971021     C                   SETON                                        32
060000971021     C                   MOVE      *ZERO         VABHCR
060100010604     C                   eval      vinmsg = %trimr(vinmsg)
060200010604     C                             + ' ' + 'VHBHCR'
060300971021     C                   ELSE
060400971021     C                   MOVE      VHBHCR        VABHCR
060500971021     C                   ENDIF
060600941229     C*
060700971021     C                   MOVE      *ZERO         VABVMD
060800971021     C                   MOVE      VHBVMD        WASC15
060900010719     C                   IF        WI15 = *blanks
061000010719     C                   EVAL      WI15 = *zeros
061100010719     C                   ENDIF
061200971023     C                   TESTN                   WI15                 30
061300971023     C  N30              TESTN                   WI15                   30
061400971023     C  N30              TESTN                   WI15                     30
061500971021     C                   MOVE      WI15          W001A             1
061600971021     C     *IN30         IFEQ      *OFF
061700971021     C     W001A         ORLT      '0'
061800971023     C     W001A         ANDNE     *BLANK
061900971021     C                   SETON                                        32
062000971021     C                   MOVE      *ZERO         VABVMD
062100010604     C                   eval      vinmsg = %trimr(vinmsg)
062200010604     C                             + ' ' + 'VHBVMD'
062300971021     C                   ELSE
062400971021     C                   MOVEL     WI15          VABVMD
062500010719     C                   IF        WD15 = *blanks
062600010719     C                   EVAL      WD15 = *zeros
062700010719     C                   ENDIF
062800971023     C                   TESTN                   WD15                 30
062900971023     C  N30              TESTN                   WD15                   30
063000971023     C  N30              TESTN                   WD15                     30
063100971021     C                   MOVE      WD15          W001A             1
063200971021     C     *IN30         IFEQ      *OFF
063300971021     C     W001A         ORLT      '0'
063400971023     C     W001A         ANDNE     *BLANK
063500971021     C                   SETON                                        32
063600971021     C                   MOVE      *ZERO         VABVMD
063700010604     C                   eval      vinmsg = %trimr(vinmsg)
063800010604     C                             + ' ' + 'VHBVMD'
063900971021     C                   ELSE
064000971021     C                   MOVE      WD15          VABVMD
064100971021     C                   ENDIF
064200050118     C                   ENDIF
064300971021     C*
064400971023     C                   TESTN                   VHBANT               30
064500971023     C  N30              TESTN                   VHBANT                 30
064600971023     C  N30              TESTN                   VHBANT                   30
064700971021     C                   MOVE      VHBANT        W001A             1
064800971021     C     *IN30         IFEQ      *OFF
064900971021     C     W001A         ORLT      '0'
065000971023     C     W001A         ANDNE     *BLANK
065100971021     C                   SETON                                        32
065200971021     C                   MOVE      *ZERO         VABANT
065300010604     C                   eval      vinmsg = %trimr(vinmsg)
065400010604     C                             + ' ' + 'VHBANT'
065500971021     C                   ELSE
065600971021     C                   MOVE      VHBANT        VABANT
065700971021     C                   ENDIF
065800971021     C*
065900971021     C* CAMPI ALFABETICI:
066000971021     C                   MOVE      VHBATB        VABATB
066100971021     C                   MOVE      VHBCBO        VABCBO
066200971021     C                   MOVE      VHBRSD        VABRSD
066300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
066400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
066500020117     C* ==
066600971021     C                   MOVE      VHBRD2        VABRD2
066700971021     C                   MOVE      VHBIND        VABIND
066800971021     C                   MOVE      VHBCAD        VABCAD
066900971021     C                   MOVE      VHBLOD        VABLOD
067000971021     C                   MOVE      VHBPRD        VABPRD
067100971021     C                   MOVE      VHBNZD        VABNZD
067200971021     C                   MOVE      VHBGC1        VABGC1
067300971021     C                   MOVE      VHBGC2        VABGC2
067400971021     C                   MOVE      VHBTSP        VABTSP
067500971021     C                   MOVE      VHBVAS        VABVAS
067600971021     C                   MOVE      VHBNAS        VABNAS
067700971021     C                   MOVE      VHBTIC        VABTIC
067800971021     C                   MOVE      VHBVCA        VABVCA
067900971021     C                   MOVE      VHBGCA        VABGCA
068000971021     C                   MOVE      VHBRMA        VABRMA
068100971021     C                   MOVE      VHBXCO        VABXCO
068200971021     C                   MOVE      VHBNOT        VABNOT
068300971021     C                   MOVE      VHBNT2        VABNT2
068400971021     C                   MOVE      VHBCTM        VABCTM
068500971021     C                   MOVE      VHBFFD        VABFFD
068600971021     C                   MOVE      VHBTCR        VABTCR
068700971021     C                   MOVE      VHBCTS        VABCTS
068800971021     C                   MOVE      VHBFTM        VABFTM
068900971021     C                   MOVE      VHBVAD        VABVAD
069000971021     C                   MOVE      VHBGMA        VABGMA
069100971021     C                   MOVE      VHBGGA        VABGGA
069200971021     C                   MOVE      VHBGVA        VABGVA
069300971021     C                   MOVE      VHBTC1        VABTC1
069400971021     C                   MOVE      VHBTC2        VABTC2
069500971021     C                   MOVE      VHBSCL        VABSCL
069600971021     C                   MOVE      VHBRMO        VABRMO
069700971021     C                   MOVE      VHBCMO        VABCMO
069800971021     C                   MOVE      VHBNMO        VABNMO
069900971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
070000971021     C* NELLA NAZIONE ORIGINALE
070100971021     C                   MOVE      VHBNMO        WA1D              1
070200971021     C                   MOVEL     VHBNMO        WA1S              1
070300971021     C     WA1D          IFNE      *BLANK
070400971021     C     WA1S          ANDEQ     *BLANK
070500971021     C                   MOVE      *BLANK        VABNMO
070600971021     C                   END
070700050209     C*
070800050209     C* NSP => Stacco un numeratore da AZNUM x la valorizzazione del numero spedizione
070900170801     C***                clear                   TRUL33DS
071000170801     C***                eval      I33OPE = *zeros
071100170801     C***                eval      I33CNU = 302
071200170801     C***                eval      I33NUM = 1
071300170801     C***                movel     TRUL33DS      KPJBU
071400170801     C***                call      'TRUL33R'
071500170801     C***                parm                    KPJBA
071600170801     C***                movel     KPJBU         TRUL33DS
071700170801     C***                if        O33ERR = *zeros
071800170801     C***                z-add     O33NRF        VABNSP
071900170801     C***                z-add     O33NRF        VATNSP
072000170801     C***                else
072100170801     C***                SETON                                        31
072200170801     C***                EVAL      vinmsg = %trimr(vinmsg)
072300170801     C***                          + ' ' + 'VABNSP VATNSP'
072400170801     C***                endif
072500050209     C*
072600050209     C* Forzo il codice trattamento merce a 7Q
072700050209     C                   EVAL      VABCTM = '7Q'
072800071105     C*
072900071105     C* Forzatura sul codice tariffa indicato dal cliente: se CTR=300 (DPD) CTM='7R'
073000071105     C                   IF        VABCTR = 300
073100071105     C                   EVAL      VABCTM = '7R'
073200071105     C                   ENDIF
073300930409     C*
073400010604     C  N31              ADD       1             §CTROK            7 0
073500010604     C   32              ADD       1             §CTRMO            7 0
073600010604     C   31              ADD       1             §CTRNO            7 0
073700020913     C  N31              WRITE     FIVAB000
073800050209     C  N31              EXSR      WRIVAT
073900990910     C*
074000010601     C                   if        *in31 = *off and
074100010601     C                             *in32 = *off
074200990910     C                   eval      vinflg = '1'
074300990910     C                   else
074400990910     C                   eval      vinflg = '2'
074500990910     C                   endif
074600991022     C                   endif
074700990910     C                   endif
074800010601     C*
074900010601     C                   endif
075000000905     C*
075100000905     C                   else
075200000905     C                   eval      vinflg = '1'
075300000905     C                   endif
075400000905     C*
075500000905     C  N70              update    tivin000
075600991022     C*
075700991022     C  N70              ENDdo
075800010530     C
075900010530     C*
076000010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
076100010530     C                   if        cntNonEl = *zeros or
076200010530     C                             flgMulti = '0'
076300990910     C* Se non ci sono record con errori ...
076400010530     C                   if        §ctrno = 0 and
076500010601     C                             §ctrmo = 0 and
076600010530     C                             flgStato <> '2'
076700990910     C* ... restituisco esito OK.
076800990921     C                   eval      wrkesito = '0'
076900990910     C                   else
077000010601     C                   if        §ctrok > 0
077100990921     C                   eval      wrkesito = '1'
077200000710     C                   else
077300010615     C                   if        flgOk = '0'
077400010615     C                   eval      wrkesito = '2'
077500010615     C                   else
077600010615     C                   eval      wrkesito = '6'
077700010615     C                   endif
077800990910     C                   endif
077900000710     C                   endif
078000010530     C                   else
078100010530     C                   eval      wrkesito = '9'
078200010530     C                   endif
078300050209     C*
078400050209     C                   else
078500050209     C                   eval      wrkesito = '2'
078600050209     C                   endif
078700990910     C*
078800990914     C                   if        %open(tivin00r)
078900990908     C                   close     tivin00r
079000990914     C                   endif
079100020913     C                   if        %open(fivabwwr)
079200020913     C                   close     fivabwwr
079300990914     C                   endif
079400050209     C                   if        %open(fivatwwr)
079500050209     C                   close     fivatwwr
079600050209     C                   endif
079700010530     C*
079800010530     C                   if        vlrpoi <> 999
079900010530     C                   eval      invfil = vlrpoi
080000010530     C                   endif
080100990910     C*
080200990920     C                   if        §ctrok > 0
080300010530     C                             and invfil > *zeros
080400000613     C                   exsr      invio
080500990920     C                   endif
080600010612     C*
080700010612     C                   if        flgGiro = '1'
080800010612     C                   exsr      endela
080900010612     C                   endif
081000990920     C*
081100910830     C                   ENDSR
081200000613     C***
081300050209
081400050209
081500050209
081600050209     C*----------------------------------------------------*
081700050209     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
081800050209     C*----------------------------------------------------*
081900050209     C     PREVAT        BEGSR
082000050209     C*
082100050209     C* Compongo il nome del membro da dare al FIVATWWR
082200050209     C                   eval      parmbr = vlrhdl
082300050209     C                   movel     'M'           parmbr
082400050209     C                   eval      parccm = %subst(vlrKSC:2:7)
082500050209     C                   eval      paropz = '1'
082600050209     C* Effettuo la chiamata al CLLE preposto
082700050209     C                   call(e)   'TITVVTC'
082800050209     C                   parm                    parccm
082900050209     C                   parm                    parmbr
083000050209     C                   parm                    paropz
083100050209     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
083200050209     C                   if        %error
083300050209     C                   movel     '1'           chkcall
083400050209     C                   else
083500050209     C                   movel     '0'           chkcall
083600050209     C                   endif
083700050209     C*
083800050209     C                   ENDSR
083900050209
084000050209
084100990920
084200050209     C*----------------------------------------------------*
084300050209     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
084400050209     C*----------------------------------------------------*
084500050209     C     WRIVAT        BEGSR
084600050209     C*
084700050209     C* Valorizzo il buffer del file dettagli barcode colli
084800050209     C                   EVAL      VATFGS = VABFGS
084900050209     C                   EVAL      VATAAS = VABAAS
085000050209     C                   EVAL      VATLNP = VABLNP
085100050209     C                   EVAL      VATNSP = VABNSP
085200050209     C                   EVAL      VATCCM = VABCCM
085300070209     C                   EVAL      VATTRC='E'
085400070209     C                   Z-ADD     1             wCollo            3 0
085500070209     C                   DOW       wCollo <= VABNCL
085600070209     C                   MOVE(P)   wCollo        wColloAlf         3
085700070209     C                   EVAL      VATNOT = %subst(%trim(VABRMA):1:12)+wColloAlf
085800070209     C                   WRITE     FIVAT000
085900070209     C                   ADD       1             wCollo
086000070209     C                   ENDDO
086100050209     C*
086200050209     C                   ENDSR
086300010530
086400010530
086500050209
086600010530
086700010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
086800010530     C     repfil        BEGSR
086900010530     C*
087000010530     C                   if        invfil = *zeros and
087100010530     C                             depfil > *zeros and
087200010613     C                             (vinflg = *blanks or
087300010613     C                              vinflg = *zeros)
087400010530     C
087500010530     C                   eval      invfil = depfil
087600010530     C                   endif
087700010530     C*
087800010530     C                   if        depfil <> invfil and
087900010530     C                             invfil > *zeros
088000010530     C                   eval      flgMulti = '1'
088100010530     C                   if        vinflg = *blanks
088200010530     C                   add       1             cntNonEl
088300010530     C                   endif
088400010530     C                   endif
088500010530     C*
088600010530     C                   if        vinflg = '2'
088700010530     C                   eval      flgStato = '2'
088800010530     C                   endif
088900010530     C*
089000010530     C                   ENDSR
089100010530     C***
089200050209
089300050209
089400050209
089500050209
089600050209      /TITLE Invio dei dati al punto operativo.
089700050209     C     invio         BEGSR
089800050209     C*
089900050209     C* 1° invio FIVAT
090000050209     C                   reset                   dscmz
090100050209     C                   move      vlrpoi        cmzdst
090200050209     C                   eval      cmzfld = 'FIVATWWR'
090300050209     C                   eval      cmzmbd = vlrhdl
090400050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
090500050209     C***                if        prmfir = *blanks
090600050209     C                   eval      cmzfla = 'FIVAT00F'
090700050209     C                   eval      cmzmba = 'FIVAT00F'
090800050209     C***                else
090900050209     C***                eval      cmzfla = prmfir
091000050209     C***                eval      cmzmba = prmfir
091100050209     C***                endif
091200050209     C                   eval      cmznrr = *zeros
091300050209     C                   move      §ctrok        cmznrr
091400050209     C                   eval      cmzlba = vlrfl1
091500050209     C                   call(e)   'TIS711C'
091600050209     C                   parm                    dscmz
091700050209     C                   parm      *blanks       esito
091800050209     C                   if        %error
091900050209     C                             or cmzerr = '1'
092000050209     C                             or esito  = '1'
092100050209     C                   eval      wrkesito = '3'
092200050209     C                   else
092300050209     C*
092400050209     C* 2° invio FIVAB
092500050209     C                   reset                   dscmz
092600050209     C                   move      vlrpoi        cmzdst
092700050209     C                   eval      cmzfld = vlrfou
092800050209     C                   eval      cmzmbd = vlrhdl
092900050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
093000050209     C***                if        prmfir = *blanks
093100050209     C                   eval      cmzfla = 'FIVAB00F'
093200050209     C                   eval      cmzmba = 'FIVAB00F'
093300050209     C***                else
093400050209     C***                eval      cmzfla = prmfir
093500050209     C***                eval      cmzmba = prmfir
093600050209     C***                endif
093700050209     C                   eval      cmznrr = *zeros
093800050209     C                   move      §ctrok        cmznrr
093900050209     C                   eval      cmzlba = vlrfl1
094000050209     C                   call(e)   'TIS711C'
094100050209     C                   parm                    dscmz
094200050209     C                   parm      *blanks       esito
094300050209     C                   if        %error
094400050209     C                             or cmzerr = '1'
094500050209     C                             or esito  = '1'
094600050209     C                   eval      wrkesito = '3'
094700050209     C                   endif
094800050209     C                   endif
094900050209     C*
095000050209     C                   ENDSR
095100050209     C***
095200010530
095300010530
095400010530
095500010530
095600010530      /TITLE Invio dei dati al punto operativo.
095700010530     C     opeini        BEGSR
095800010530     C*
095900010530     C* Inizializzo flag e contatori operativi
096000010530     C                   movel     '0'           flgGiro           1
096100010530     C                   movel     '0'           flgMulti          1
096200010530     C                   movel     '1'           flgStato          1
096300010615     C                   movel     '0'           flgOk             1
096400010530     C                   z-add     *zeros        cntNonEl         10 0
096500010530     C                   z-add     *zeros        depfil            3 0
096600010530     C                   z-add     *zeros        invfil            3 0
096700030407     C*
096800050209     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
096900030407     C                   if        %subst(vlrppt:1:1) = 'S'
097000030407     C                   seton                                        50
097100030407     C                   else
097200030407     C                   setoff                                       50
097300030407     C                   endif
097400041001     C*
097500050209     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
097600041001     C                   if        %subst(vlrppt:2:1) = 'S'
097700041001     C                   seton                                        51
097800041001     C                   else
097900041001     C                   setoff                                       51
098000041001     C                   endif
098100010530     C*
098200010530     C                   ENDSR
098300010530     C***
098400010530
098500010530
098600070411
098700070411     C     *pssr         BEGSR
098800070411     C*
098900070411     C                   if        %open(tivin00r)
099000070411     C                   close     tivin00r
099100070411     C                   endif
099200070411     C                   if        %open(fivabwwr)
099300070411     C                   close     fivabwwr
099400070411     C                   endif
099500070411     C                   if        %open(fivatwwr)
099600070411     C                   close     fivatwwr
099700070411     C                   endif
099800070411     C*
099900070411     C* Effettuo la chiamata al CLLE preposto
100000070411     C                   call(e)   'TITVVTC'
100100070411     C                   parm                    parccm
100200070411     C                   parm                    parmbr
100300070411     C                   parm      '2'           paropz
100400070411     C*
100500070411     C                   eval      wrkesito = '2'
100600070411     C*
100700070411     C                   seton                                        LR
100800070411     C*
100900070411     C                   ENDSR     '*CANCL'
101000070411     C***
101100070411
101200010530
101300990910
101400000613     C     *inzsr        BEGSR
101500990910     C*
101600990910     C     *entry        plist
101700990920     C                   parm                    tivlrds
101800990921     C                   parm      wrkesito      esito
101900000724     C                   parm                    prmlit
102000000710     C                   parm                    prmfir
102100010606     C*
102200010606     C* CALCOLA LA DATA CORRENTE
102300010606     C                   time                    wn14             14 0
102400010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
102500010606     C                   z-add     wn8           g08dat
102600010606     C                   z-add     *zeros        g08inv
102700010606     C                   movel     '0'           g08err
102800010606     C                   call      'XSRDA8'
102900010606     C                   parm                    wlbda8
103000010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
103100000613     C*
103200000613     C                   ENDSR
103300000613     C***
