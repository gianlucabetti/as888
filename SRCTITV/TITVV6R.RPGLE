000100050722      /TITLE Upload via Internet: traduzione in FIVAB/FIVAT.
000200990908     H dftactgrp(*yes)
000300990908
000400990910     Ftivin00r  uF   E             DISK    usropn
000500050722     FFIVATwwr  O    E             DISK    usropn
000600020913     FFIVABwwr  O    E             DISK    usropn
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
003500041025     D tisi95ds      e ds
003600990910     D fnvabaar      e ds                  inz
003700990910     D esito           s              1
003800000724     D prmlit          s             10
003900000710     D prmfir          s             10
004000990921     D wrkesito        s                   like(esito)
004100000613     D rrnum           s              6  0 INZ(*zeros)
004200050722     D parccm          s              8    INZ(*blanks)
004300050722     D parmbr          s             10    INZ(*blanks)
004400050722     D paropz          s              1    INZ(*blanks)
004500050722     D chkcall         s              1    INZ(*blanks)
004600010606     D*------------------
004700010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
004800010606     D*------------------
004900010606     D WLBDA8          DS                  INZ
005000010606     D  G08DAT                 1      8  0
005100010606     D  G08INV                 9     16  0
005200010606     D  G08ERR                17     17
005300010606     D  G08TGI                18     22  0
005400010606     D*
005500040407     D TxtInOut        S           2048
005600040407     D ElencoChar      S            256
005700040407     D TipoElenco      S              1
005800040407     D CharSost        S              1
005900040407     D UpperCase       S              1
006000040407     D ChkNull         S              1
006100040407     D CharNull        S              1
006200040407     D EsitoBon        S              1
006300990908
006400000913     C                   reset                   rrnum
006500990921     C                   reset                   esito
006600990921     C                   reset                   wrkesito
006700010530     C*
006800010530     C                   exsr      opeini
006900010530     C                   exsr      rwvab
007000041025     C*
007100041025     C* Esegue lancio TISI95R solo x chiusura
007200041025     C                   CLEAR                   TISI95DS
007300041025     C                   EVAL      I95TLA = 'C'
007400041025     C                   CALL      'TISI95R'
007500041025     C                   PARM                    TISI95DS
007600010530     C*
007700010530     C                   seton                                        lr
007800010530
007900010530
008000010530
008100010530
008200010530     C*--------------------------------------------------------
008300010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008400010530     C*--------------------------------------------------------
008500010530     C     PREELA        BEGSR
008600000724     C*
008700000724     C* SE OCCORRE SPEDIRE IN FILIALE
008800010530     C                   if        invfil <> *zeros and
008900010530     C                             flgGiro = '0'
009000010530     C*
009100010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009200010530     C                   eval      flgGiro = '1'
009300010530     C*
009400010530     C                   endif
009500010530     C*
009600010530     C                   ENDSR
009700010530     C***
009800010530
009900010530
010000010530
010100010530
010200010530
010300010530
010400010530     C*--------------------------------------------------------
010500010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
010600010530     C*--------------------------------------------------------
010700010530     C     ENDELA        BEGSR
010800050722     C*
010900050722     C* Effettuo la chiamata al CLLE preposto
011000050722     C                   call(e)   'TITVVTC'
011100050722     C                   parm                    parccm
011200050722     C                   parm                    parmbr
011300050722     C                   parm      '2'           paropz
011400010530     C*
011500010530     C                   ENDSR
011600010530     C***
011700990908
011800010530
011900010530
012000010530
012100910830     C*--------------------------------------------------------
012200020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
012300910830     C*--------------------------------------------------------
012400941229     C     RWVAB         BEGSR
012500990910     C*
012600990914     C                   if        not %open(tivin00r)
012700990908     C                   open      tivin00r
012800990914     C                   endif
012900020913     C                   if        not %open(fivabwwr)
013000020913     C                   open      fivabwwr
013100990914     C                   endif
013200050722     C*
013300050722     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
013400050722     C                   exsr      prevat
013500050722     C*
013600050722     C                   if        chkcall = '0'
013700050722     C*
013800050722     C                   if        not %open(fivatwwr)
013900050722     C                   open      fivatwwr
014000050722     C                   endif
014100990910     C*
014200050722     C                   clear                   §CTROKVB          7 0
014300050722     C                   clear                   §CTROKVT          7 0
014400050722     C                   clear                   §CTRMO            7 0
014500050722     C                   clear                   §CTRNO            7 0
014600990910     C*
014700921023     C                   DO        *HIVAL
014800990913     C*
014900990915     C                   READ      tivin00r                               70
015000010618     C*
015100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
015200010618     C                   if        vinflg = '1'
015300010618     C                   eval      flgOk = '1'
015400010618     C                   endif
015500010705     C*
015600010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
015700010705     C                   if        %subst(vindta:1:1) = '='
015800010705     C                   eval      %subst(vindta:1:1) = *blanks
015900010705     C                   endif
016000010705     C*
016100000905     C                   if        vindta > *blanks
016200000613     C                   add       1             rrnum
016300010601     C* ...elimino eventuali segni
016400010601     C     '+':' '       xlate     vindta        vindta
016500990913     C*
016600010601     C                   if        *in70 = *off and
016700990913     C                             (vinflg = *blanks
016800010711     C                              or vinflg = '0'
016900010711     C                              or vinflg = '2')
017000990913     C*
017100020913     C                   clear                   FIVAB000
017200050722     C                   clear                   FIVAT000
017300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
017400010711     C                   if        vinflg = *blanks or vinflg = '0'
017500010711     C                   clear                   vinmsg
017600010711     C                   endif
017700010711     C*
017800990913     C                   eval      fnvabaar = vindta
017900991022     C*
018000991022     C                   testb     x'3F'         vhbatb                   02
018100991022     C                   if        not *in02
018200991022     C*
018300971021     C                   SETOFF                                       303132
018400971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
018500971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
018600971023     C*    OPPURE TUTTI BLANKS
018700010530     C*
018800010530     C                   TESTN                   VHBLNP               30
018900010530     C  N30              TESTN                   VHBLNP                 30
019000010530     C  N30              TESTN                   VHBLNP                   30
019100010530     C                   MOVE      VHBLNP        W001A             1
019200010530     C     *IN30         IFEQ      *OFF
019300010530     C     W001A         ORLT      '0'
019400010530     C                   SETON                                        31
019500010530     C                   eval      vinmsg = %trimr(vinmsg)
019600010530     C                             + ' ' + 'VHBLNP'
019700010530     C                   ELSE
019800010530     C                   MOVE      VHBLNP        VABLNP
019900010530     C     VABLNP        IFLE      *ZEROS
020000010530     C                   SETON                                        31
020100010530     C                   eval      vinmsg = %trimr(vinmsg)
020200010530     C                             + ' ' + 'VHBLNP'
020300010530     C                   ENDIF
020400010530     C                   ENDIF
020500010530     C*
020600010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
020700010530     C                   eval      depfil = VABLNP
020800010530     C                   exsr      repfil
020900010601     C                   if        depfil = invfil
021000021014     C                   if        vlrpoi = 999
021100021010     C                   MOVE(P)   invfil        VABFGS
021200050722     C                   move(P)   invfil        VATFGS
021300021014     C                   else
021400021014     C                   MOVE(P)   vlrpoi        VABFGS
021500050722     C                   move(P)   vlrpoi        VATFGS
021600021014     C                   endif
021700010530     C*
021800010530     C                   exsr      PREELA
021900010530     C*
022000971023     C                   TESTN                   VHBCCM               30
022100971023     C  N30              TESTN                   VHBCCM                 30
022200971023     C  N30              TESTN                   VHBCCM                   30
022300971021     C                   MOVE      VHBCCM        W001A             1
022400971021     C     *IN30         IFEQ      *OFF
022500971021     C     W001A         ORLT      '0'
022600971021     C                   SETON                                        31
022700010604     C                   eval      vinmsg = %trimr(vinmsg)
022800010604     C                             + ' ' + 'VHBCCM'
022900971021     C                   ELSE
023000971021     C                   MOVE      VHBCCM        VABCCM
023100971024     C     VABCCM        IFLE      *ZEROS
023200971024     C                   SETON                                        31
023300010604     C                   eval      vinmsg = %trimr(vinmsg)
023400010604     C                             + ' ' + 'VHBCCM'
023500971024     C                   ENDIF
023600971021     C                   ENDIF
023700971021     C*
023800971023     C                   TESTN                   VHBAAS               30
023900971023     C  N30              TESTN                   VHBAAS                 30
024000971023     C  N30              TESTN                   VHBAAS                   30
024100971021     C                   MOVE      VHBAAS        W001A             1
024200971021     C     *IN30         IFEQ      *OFF
024300971021     C     W001A         ORLT      '0'
024400971023     C     W001A         ANDNE     *BLANK
024500971021     C                   SETON                                        32
024600010606     C                   MOVEL     datcor        VABAAS
024700010604     C                   eval      vinmsg = %trimr(vinmsg)
024800010604     C                             + ' ' + 'VHBAAS'
024900971021     C                   ELSE
025000971021     C                   MOVE      VHBAAS        VABAAS
025100971021     C                   ENDIF
025200971021     C*
025300971023     C                   TESTN                   VHBMGS               30
025400971023     C  N30              TESTN                   VHBMGS                 30
025500971023     C  N30              TESTN                   VHBMGS                   30
025600971021     C                   MOVE      VHBMGS        W001A             1
025700971021     C     *IN30         IFEQ      *OFF
025800971021     C     W001A         ORLT      '0'
025900971023     C     W001A         ANDNE     *BLANK
026000971021     C                   SETON                                        32
026100010601     C                   MOVE      '0101'        VABMGS
026200010604     C                   eval      vinmsg = %trimr(vinmsg)
026300010604     C                             + ' ' + 'VHBMGS'
026400971021     C                   ELSE
026500971021     C                   MOVE      VHBMGS        VABMGS
026600971021     C                   ENDIF
026700971021     C*
026800971023     C                   TESTN                   VHBNRS               30
026900971023     C  N30              TESTN                   VHBNRS                 30
027000971023     C  N30              TESTN                   VHBNRS                   30
027100971021     C                   MOVE      VHBNRS        W001A             1
027200971021     C     *IN30         IFEQ      *OFF
027300971021     C     W001A         ORLT      '0'
027400971023     C     W001A         ANDNE     *BLANK
027500971021     C                   SETON                                        32
027600971021     C                   MOVE      *ZERO         VABNRS
027700010604     C                   eval      vinmsg = %trimr(vinmsg)
027800010604     C                             + ' ' + 'VHBNRS'
027900971021     C                   ELSE
028000971021     C                   MOVE      VHBNRS        VABNRS
028100971021     C                   ENDIF
028200971021     C*
028300971023     C                   TESTN                   VHBNSP               30
028400971023     C  N30              TESTN                   VHBNSP                 30
028500971023     C  N30              TESTN                   VHBNSP                   30
028600971021     C                   MOVE      VHBNSP        W001A             1
028700971021     C     *IN30         IFEQ      *OFF
028800971021     C     W001A         ORLT      '0'
028900971023     C     W001A         ANDNE     *BLANK
029000971021     C                   SETON                                        32
029100971021     C                   MOVE      *ZERO         VABNSP
029200010604     C                   eval      vinmsg = %trimr(vinmsg)
029300010604     C                             + ' ' + 'VHBNSP'
029400971021     C                   ELSE
029500971021     C                   MOVE      VHBNSP        VABNSP
029600971021     C                   ENDIF
029700971021     C*
029800971023     C                   TESTN                   VHBLNA               30
029900971023     C  N30              TESTN                   VHBLNA                 30
030000971023     C  N30              TESTN                   VHBLNA                   30
030100971021     C                   MOVE      VHBLNA        W001A             1
030200971021     C     *IN30         IFEQ      *OFF
030300971021     C     W001A         ORLT      '0'
030400971023     C     W001A         ANDNE     *BLANK
030500971021     C                   SETON                                        32
030600971021     C                   MOVE      *ZERO         VABLNA
030700010604     C                   eval      vinmsg = %trimr(vinmsg)
030800010604     C                             + ' ' + 'VHBLNA'
030900971021     C                   ELSE
031000971021     C                   MOVE      VHBLNA        VABLNA
031100971021     C                   ENDIF
031200971021     C*
031300971023     C                   TESTN                   VHBCTR               30
031400971023     C  N30              TESTN                   VHBCTR                 30
031500971023     C  N30              TESTN                   VHBCTR                   30
031600971021     C                   MOVE      VHBCTR        W001A             1
031700971021     C     *IN30         IFEQ      *OFF
031800971021     C     W001A         ORLT      '0'
031900971023     C     W001A         ANDNE     *BLANK
032000971021     C                   SETON                                        32
032100971021     C                   MOVE      *ZERO         VABCTR
032200010604     C                   eval      vinmsg = %trimr(vinmsg)
032300010604     C                             + ' ' + 'VHBCTR'
032400971021     C                   ELSE
032500971021     C                   MOVE      VHBCTR        VABCTR
032600971021     C                   ENDIF
032700941229     C*
032800971021     C                   MOVE      *ZERO         VABIAS
032900941229     C                   MOVE      VHBIAS        WASC15
033000971023     C                   TESTN                   WI15                 30
033100971023     C  N30              TESTN                   WI15                   30
033200971023     C  N30              TESTN                   WI15                     30
033300971021     C                   MOVE      WI15          W001A             1
033400971021     C     *IN30         IFEQ      *OFF
033500971021     C     W001A         ORLT      '0'
033600971023     C     W001A         ANDNE     *BLANK
033700971021     C                   SETON                                        32
033800971021     C                   MOVE      *ZERO         VABIAS
033900010604     C                   eval      vinmsg = %trimr(vinmsg)
034000010604     C                             + ' ' + 'VHBIAS'
034100971021     C                   ELSE
034200971021     C                   MOVEL     WI15          VABIAS
034300971023     C                   TESTN                   WD15                 30
034400971023     C  N30              TESTN                   WD15                   30
034500971023     C  N30              TESTN                   WD15                     30
034600971021     C                   MOVE      WD15          W001A             1
034700971021     C     *IN30         IFEQ      *OFF
034800971021     C     W001A         ORLT      '0'
034900971023     C     W001A         ANDNE     *BLANK
035000971021     C                   SETON                                        32
035100971021     C                   MOVE      *ZERO         VABIAS
035200010604     C                   eval      vinmsg = %trimr(vinmsg)
035300010604     C                             + ' ' + 'VHBIAS'
035400971021     C                   ELSE
035500971021     C                   MOVE      WD15          VABIAS
035600971021     C                   ENDIF
035700050118     C                   ENDIF
035800941229     C*
035900971023     C                   TESTN                   VHBNCL               30
036000971023     C  N30              TESTN                   VHBNCL                 30
036100971023     C  N30              TESTN                   VHBNCL                   30
036200971021     C                   MOVE      VHBNCL        W001A             1
036300971021     C     *IN30         IFEQ      *OFF
036400971021     C     W001A         ORLT      '0'
036500971021     C                   SETON                                        32
036600971021     C                   MOVE      *ZERO         VABNCL
036700010604     C                   eval      vinmsg = %trimr(vinmsg)
036800010604     C                             + ' ' + 'VHBNCL'
036900971021     C                   ELSE
037000971021     C                   MOVE      VHBNCL        VABNCL
037100971021     C                   ENDIF
037200941229     C*
037300971021     C                   MOVE      *ZERO         VABPKB
037400971021     C                   MOVE      VHBPKB        WASC09
037500010719     C                   IF        WI09 = *blanks
037600010719     C                   EVAL      WI09 = *zeros
037700010719     C                   ENDIF
037800971023     C                   TESTN                   WI09                 30
037900971023     C  N30              TESTN                   WI09                   30
038000971023     C  N30              TESTN                   WI09                     30
038100971021     C                   MOVE      WI09          W001A             1
038200971021     C     *IN30         IFEQ      *OFF
038300971021     C     W001A         ORLT      '0'
038400971021     C                   SETON                                        32
038500971021     C                   MOVE      *ZERO         VABPKB
038600010604     C                   eval      vinmsg = %trimr(vinmsg)
038700010604     C                             + ' ' + 'VHBPKB'
038800971021     C                   ELSE
038900971021     C                   MOVEL     WI09          VABPKB
039000010719     C                   IF        WD09 = *blanks
039100010719     C                   EVAL      WD09 = *zeros
039200010719     C                   ENDIF
039300971023     C                   TESTN                   WD09                 30
039400971023     C  N30              TESTN                   WD09                     30
039500971021     C                   MOVE      WD09          W001A             1
039600971021     C     *IN30         IFEQ      *OFF
039700971021     C     W001A         ORLT      '0'
039800971023     C     W001A         ANDNE     *BLANK
039900971021     C                   SETON                                        32
040000971021     C                   MOVE      *ZERO         VABPKB
040100010604     C                   eval      vinmsg = %trimr(vinmsg)
040200010604     C                             + ' ' + 'VHBPKB'
040300971021     C                   ELSE
040400971021     C                   MOVE      WD09          VABPKB
040500971021     C                   ENDIF
040600050118     C                   ENDIF
040700971021     C*
040800041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
040900041001     C                   IF        *IN51 = *OFF
041000971021     C                   MOVE      *ZERO         VABVLB
041100971021     C                   MOVE      VHBVLB        WASC07
041200010719     C                   IF        WI07 = *blanks
041300010719     C                   EVAL      WI07 = *zeros
041400010719     C                   ENDIF
041500971023     C                   TESTN                   WI07                 30
041600971023     C  N30              TESTN                   WI07                   30
041700971023     C  N30              TESTN                   WI07                     30
041800971021     C                   MOVE      WI07          W001A             1
041900971021     C     *IN30         IFEQ      *OFF
042000971021     C     W001A         ORLT      '0'
042100971023     C     W001A         ANDNE     *BLANK
042200971021     C                   SETON                                        32
042300971021     C                   MOVE      *ZERO         VABVLB
042400010604     C                   eval      vinmsg = %trimr(vinmsg)
042500010604     C                             + ' ' + 'VHBVLB'
042600971021     C                   ELSE
042700971021     C                   MOVEL     WI07          VABVLB
042800010719     C                   IF        WD07 = *blanks
042900010719     C                   EVAL      WD07 = *zeros
043000010719     C                   ENDIF
043100971023     C                   TESTN                   WD07                 30
043200971023     C  N30              TESTN                   WD07                   30
043300971023     C  N30              TESTN                   WD07                     30
043400971021     C                   MOVE      WD07          W001A             1
043500971021     C     *IN30         IFEQ      *OFF
043600971021     C     W001A         ORLT      '0'
043700971023     C     W001A         ANDNE     *BLANK
043800971021     C                   SETON                                        32
043900971021     C                   MOVE      *ZERO         VABVLB
044000010604     C                   eval      vinmsg = %trimr(vinmsg)
044100010604     C                             + ' ' + 'VHBVLB'
044200971021     C                   ELSE
044300971021     C                   MOVE      WD07          VABVLB
044400971021     C                   ENDIF
044500050118     C                   ENDIF
044600041001     C                   ELSE
044700041001     C* ...altrimenti forzo azzeramento del volume
044800041001     C                   Z-ADD     *zeros        VABVLB
044900041001     C                   ENDIF
045000941229     C*
045100971021     C                   MOVE      *ZERO         VABQFT
045200971021     C                   MOVE      VHBQFT        WASC15
045300010719     C                   IF        WI15 = *blanks
045400010719     C                   EVAL      WI15 = *zeros
045500010719     C                   ENDIF
045600971023     C                   TESTN                   WI15                 30
045700971023     C  N30              TESTN                   WI15                   30
045800971023     C  N30              TESTN                   WI15                     30
045900971021     C                   MOVE      WI15          W001A             1
046000971021     C     *IN30         IFEQ      *OFF
046100971021     C     W001A         ORLT      '0'
046200971023     C     W001A         ANDNE     *BLANK
046300971021     C                   SETON                                        32
046400971021     C                   MOVE      *ZERO         VABQFT
046500010604     C                   eval      vinmsg = %trimr(vinmsg)
046600010604     C                             + ' ' + 'VHBQFT'
046700971021     C                   ELSE
046800971021     C                   MOVEL     WI15          VABQFT
046900010719     C                   IF        WD15 = *blanks
047000010719     C                   EVAL      WD15 = *zeros
047100010719     C                   ENDIF
047200971023     C                   TESTN                   WD15                 30
047300971023     C  N30              TESTN                   WD15                   30
047400971023     C  N30              TESTN                   WD15                     30
047500971021     C                   MOVE      WD15          W001A             1
047600971021     C     *IN30         IFEQ      *OFF
047700971021     C     W001A         ORLT      '0'
047800971023     C     W001A         ANDNE     *BLANK
047900971021     C                   SETON                                        32
048000971021     C                   MOVE      *ZERO         VABQFT
048100010604     C                   eval      vinmsg = %trimr(vinmsg)
048200010604     C                             + ' ' + 'VHBQFT'
048300971021     C                   ELSE
048400971021     C                   MOVE      WD15          VABQFT
048500971021     C                   ENDIF
048600050118     C                   ENDIF
048700941229     C*
048800971021     C                   MOVE      *ZERO         VABCAS
048900971021     C                   MOVE      VHBCAS        WASC15
049000010719     C                   IF        WI15 = *blanks
049100010719     C                   EVAL      WI15 = *zeros
049200010719     C                   ENDIF
049300971023     C                   TESTN                   WI15                 30
049400971023     C  N30              TESTN                   WI15                   30
049500971023     C  N30              TESTN                   WI15                     30
049600971021     C                   MOVE      WI15          W001A             1
049700971021     C     *IN30         IFEQ      *OFF
049800971021     C     W001A         ORLT      '0'
049900971023     C     W001A         ANDNE     *BLANK
050000971021     C                   SETON                                        32
050100971021     C                   MOVE      *ZERO         VABCAS
050200010604     C                   eval      vinmsg = %trimr(vinmsg)
050300010604     C                             + ' ' + 'VHBCAS'
050400971021     C                   ELSE
050500971021     C                   MOVEL     WI15          VABCAS
050600010719     C                   IF        WD15 = *blanks
050700010719     C                   EVAL      WD15 = *zeros
050800010719     C                   ENDIF
050900971023     C                   TESTN                   WD15                 30
051000971023     C  N30              TESTN                   WD15                   30
051100971023     C  N30              TESTN                   WD15                     30
051200971021     C                   MOVE      WD15          W001A             1
051300971021     C     *IN30         IFEQ      *OFF
051400971021     C     W001A         ORLT      '0'
051500971023     C     W001A         ANDNE     *BLANK
051600971021     C                   SETON                                        32
051700971021     C                   MOVE      *ZERO         VABCAS
051800010604     C                   eval      vinmsg = %trimr(vinmsg)
051900010604     C                             + ' ' + 'VHBCAS'
052000971021     C                   ELSE
052100971021     C                   MOVE      WD15          VABCAS
052200971021     C                   ENDIF
052300040827     C                   ENDIF
052400971021     C*
052500971023     C                   TESTN                   VHBRMN               30
052600971023     C  N30              TESTN                   VHBRMN                 30
052700971023     C  N30              TESTN                   VHBRMN                   30
052800971021     C                   MOVE      VHBRMN        W001A             1
052900971021     C     *IN30         IFEQ      *OFF
053000971021     C     W001A         ORLT      '0'
053100971021     C                   SETON                                        32
053200971021     C                   MOVE      *ZERO         VABRMN
053300010604     C                   eval      vinmsg = %trimr(vinmsg)
053400010604     C                             + ' ' + 'VHBRMN'
053500971021     C                   ELSE
053600971021     C                   MOVE      VHBRMN        VABRMN
053700971021     C                   ENDIF
053800971021     C*
053900971023     C                   TESTN                   VHBNCD               30
054000971023     C  N30              TESTN                   VHBNCD                 30
054100971023     C  N30              TESTN                   VHBNCD                   30
054200971021     C                   MOVE      VHBNCD        W001A             1
054300971021     C     *IN30         IFEQ      *OFF
054400971021     C     W001A         ORLT      '0'
054500971023     C     W001A         ANDNE     *BLANK
054600971021     C                   SETON                                        32
054700971021     C                   MOVE      *ZERO         VABNCD
054800010604     C                   eval      vinmsg = %trimr(vinmsg)
054900010604     C                             + ' ' + 'VHBNCD'
055000971021     C                   ELSE
055100971021     C                   MOVE      VHBNCD        VABNCD
055200971021     C                   ENDIF
055300971021     C*
055400971023     C                   TESTN                   VHBNCA               30
055500971023     C  N30              TESTN                   VHBNCA                 30
055600971023     C  N30              TESTN                   VHBNCA                   30
055700971021     C                   MOVE      VHBNCA        W001A             1
055800971021     C     *IN30         IFEQ      *OFF
055900971021     C     W001A         ORLT      '0'
056000971023     C     W001A         ANDNE     *BLANK
056100971021     C                   SETON                                        32
056200971021     C                   MOVE      *ZERO         VABNCA
056300010604     C                   eval      vinmsg = %trimr(vinmsg)
056400010604     C                             + ' ' + 'VHBNCA'
056500971021     C                   ELSE
056600971021     C                   MOVE      VHBNCA        VABNCA
056700971021     C                   ENDIF
056800971021     C*
056900971023     C                   TESTN                   VHBZNC               30
057000971023     C  N30              TESTN                   VHBZNC                 30
057100971023     C  N30              TESTN                   VHBZNC                   30
057200971021     C                   MOVE      VHBZNC        W001A             1
057300971021     C     *IN30         IFEQ      *OFF
057400971021     C     W001A         ORLT      '0'
057500971023     C     W001A         ANDNE     *BLANK
057600971021     C                   SETON                                        32
057700971021     C                   MOVE      *ZERO         VABZNC
057800010604     C                   eval      vinmsg = %trimr(vinmsg)
057900010604     C                             + ' ' + 'VHBZNC'
058000971021     C                   ELSE
058100971021     C                   MOVE      VHBZNC        VABZNC
058200971021     C                   ENDIF
058300971021     C*
058400971023     C                   TESTN                   VHBDCR               30
058500971023     C  N30              TESTN                   VHBDCR                 30
058600971023     C  N30              TESTN                   VHBDCR                   30
058700971021     C                   MOVE      VHBDCR        W001A             1
058800971021     C     *IN30         IFEQ      *OFF
058900971021     C     W001A         ORLT      '0'
059000971023     C     W001A         ANDNE     *BLANK
059100971021     C                   SETON                                        32
059200971021     C                   MOVE      *ZERO         VABDCR
059300010604     C                   eval      vinmsg = %trimr(vinmsg)
059400010604     C                             + ' ' + 'VHBDCR'
059500971021     C                   ELSE
059600971021     C                   MOVE      VHBDCR        VABDCR
059700971021     C                   ENDIF
059800971021     C*
059900971023     C                   TESTN                   VHBHCR               30
060000971023     C  N30              TESTN                   VHBHCR                 30
060100971023     C  N30              TESTN                   VHBHCR                   30
060200971021     C                   MOVE      VHBHCR        W001A             1
060300971021     C     *IN30         IFEQ      *OFF
060400971021     C     W001A         ORLT      '0'
060500971023     C     W001A         ANDNE     *BLANK
060600971021     C                   SETON                                        32
060700971021     C                   MOVE      *ZERO         VABHCR
060800010604     C                   eval      vinmsg = %trimr(vinmsg)
060900010604     C                             + ' ' + 'VHBHCR'
061000971021     C                   ELSE
061100971021     C                   MOVE      VHBHCR        VABHCR
061200971021     C                   ENDIF
061300941229     C*
061400971021     C                   MOVE      *ZERO         VABVMD
061500971021     C                   MOVE      VHBVMD        WASC15
061600010719     C                   IF        WI15 = *blanks
061700010719     C                   EVAL      WI15 = *zeros
061800010719     C                   ENDIF
061900971023     C                   TESTN                   WI15                 30
062000971023     C  N30              TESTN                   WI15                   30
062100971023     C  N30              TESTN                   WI15                     30
062200971021     C                   MOVE      WI15          W001A             1
062300971021     C     *IN30         IFEQ      *OFF
062400971021     C     W001A         ORLT      '0'
062500971023     C     W001A         ANDNE     *BLANK
062600971021     C                   SETON                                        32
062700971021     C                   MOVE      *ZERO         VABVMD
062800010604     C                   eval      vinmsg = %trimr(vinmsg)
062900010604     C                             + ' ' + 'VHBVMD'
063000971021     C                   ELSE
063100971021     C                   MOVEL     WI15          VABVMD
063200010719     C                   IF        WD15 = *blanks
063300010719     C                   EVAL      WD15 = *zeros
063400010719     C                   ENDIF
063500971023     C                   TESTN                   WD15                 30
063600971023     C  N30              TESTN                   WD15                   30
063700971023     C  N30              TESTN                   WD15                     30
063800971021     C                   MOVE      WD15          W001A             1
063900971021     C     *IN30         IFEQ      *OFF
064000971021     C     W001A         ORLT      '0'
064100971023     C     W001A         ANDNE     *BLANK
064200971021     C                   SETON                                        32
064300971021     C                   MOVE      *ZERO         VABVMD
064400010604     C                   eval      vinmsg = %trimr(vinmsg)
064500010604     C                             + ' ' + 'VHBVMD'
064600971021     C                   ELSE
064700971021     C                   MOVE      WD15          VABVMD
064800971021     C                   ENDIF
064900050118     C                   ENDIF
065000971021     C*
065100971021     C* CAMPI ALFABETICI:
065200971021     C                   MOVE      VHBATB        VABATB
065300971021     C                   MOVE      VHBCBO        VABCBO
065400971021     C                   MOVE      VHBRSD        VABRSD
065500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
065600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
065700020117     C* ==
065800070604     C                   MOVE      VHBRD2        VABRD2
065900971021     C                   MOVE      VHBIND        VABIND
066000971021     C                   MOVE      VHBCAD        VABCAD
066100971021     C                   MOVE      VHBLOD        VABLOD
066200971021     C                   MOVE      VHBPRD        VABPRD
066300971021     C                   MOVE      VHBNZD        VABNZD
066400971021     C                   MOVE      VHBGC1        VABGC1
066500971021     C                   MOVE      VHBGC2        VABGC2
066600971021     C                   MOVE      VHBTSP        VABTSP
066700971021     C                   MOVE      VHBVAS        VABVAS
066800971021     C                   MOVE      VHBNAS        VABNAS
066900971021     C                   MOVE      VHBTIC        VABTIC
067000971021     C                   MOVE      VHBVCA        VABVCA
067100971021     C                   MOVE      VHBGCA        VABGCA
067200971021     C                   MOVE      VHBRMA        VABRMA
067300971021     C                   MOVE      VHBXCO        VABXCO
067400971021     C                   MOVE      VHBNOT        VABNOT
067500971021     C                   MOVE      VHBNT2        VABNT2
067600971021     C                   MOVE      VHBCTM        VABCTM
067700971021     C                   MOVE      VHBFFD        VABFFD
067800971021     C                   MOVE      VHBTCR        VABTCR
067900971021     C                   MOVE      VHBCTS        VABCTS
068000971021     C                   MOVE      VHBFTM        VABFTM
068100971021     C                   MOVE      VHBVAD        VABVAD
068200971021     C                   MOVE      VHBGMA        VABGMA
068300971021     C                   MOVE      VHBGGA        VABGGA
068400971021     C                   MOVE      VHBGVA        VABGVA
068500971021     C                   MOVE      VHBTC1        VABTC1
068600971021     C                   MOVE      VHBTC2        VABTC2
068700971021     C                   MOVE      VHBSCL        VABSCL
068800971021     C                   MOVE      VHBRMO        VABRMO
068900971021     C                   MOVE      VHBCMO        VABCMO
069000971021     C                   MOVE      VHBNMO        VABNMO
069100971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
069200971021     C* NELLA NAZIONE ORIGINALE
069300971021     C                   MOVE      VHBNMO        WA1D              1
069400971021     C                   MOVEL     VHBNMO        WA1S              1
069500971021     C     WA1D          IFNE      *BLANK
069600971021     C     WA1S          ANDEQ     *BLANK
069700971021     C                   MOVE      *BLANK        VABNMO
069800971021     C                   END
069900050418     C*
070000050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
070100050418     C                   IF        %trim(VABNZD) = 'I'   OR
070200050418     C                             %trim(VABNZD) = 'IT'  OR
070300050418     C                             %trim(VABNZD) = 'ITA'
070400050418     C                   EVAL      VABNZD = *blanks
070500050418     C                   ENDIF
070600041025     C*
070700041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
070800041025     C                   IF        *IN52 = *ON
070900041025     C* Reperisco la provincia dal CAP e dalla località
071000041025     C                   IF        VABCAD <> *blanks AND
071100041025     C                             VABPRD  = *blanks
071200041025     C                   CLEAR                   TISI95DS
071300041025     C                   EVAL      I95TCN = '3'
071400041025     C                   Z-ADD     datcor        I95DAT
071500041025     C                   EVAL      I95CAP = VABCAD
071600041025     C                   EVAL      I95LOC = VABLOD
071700041025     C                   CALL      'TISI95R'
071800041025     C                   PARM                    TISI95DS
071900041025     C                   EVAL      VABPRD = O95PRV
072000041025     C                   ENDIF
072100041025     C                   ENDIF
072200040630     C*
072300040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
072400040630     C                   exsr      exeBon
072500930409     C*
072600050722     C  N31              ADD       1             §CTROKVB
072700050722     C   32              ADD       1             §CTRMO
072800050722     C   31              ADD       1             §CTRNO
072900020913     C  N31              WRITE     FIVAB000
073000050722     C  N31              exsr      wrivat
073100990910     C*
073200010601     C                   if        *in31 = *off and
073300010601     C                             *in32 = *off
073400990910     C                   eval      vinflg = '1'
073500990910     C                   else
073600990910     C                   eval      vinflg = '2'
073700990910     C                   endif
073800991022     C                   endif
073900990910     C                   endif
074000010601     C*
074100010601     C                   endif
074200000905     C*
074300000905     C                   else
074400000905     C                   eval      vinflg = '1'
074500000905     C                   endif
074600000905     C*
074700000905     C  N70              update    tivin000
074800991022     C*
074900991022     C  N70              ENDdo
075000050722     C*
075100050722     C                   endif                                                  (endif - chkcall)
075200010530     C*
075300010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
075400010530     C                   if        cntNonEl = *zeros or
075500010530     C                             flgMulti = '0'
075600990910     C* Se non ci sono record con errori ...
075700010530     C                   if        §ctrno = 0 and
075800010601     C                             §ctrmo = 0 and
075900010530     C                             flgStato <> '2'
076000990910     C* ... restituisco esito OK.
076100990921     C                   eval      wrkesito = '0'
076200990910     C                   else
076300050722     C                   if        §ctrokvb > 0
076400990921     C                   eval      wrkesito = '1'
076500000710     C                   else
076600010615     C                   if        flgOk = '0'
076700010615     C                   eval      wrkesito = '2'
076800010615     C                   else
076900010615     C                   eval      wrkesito = '6'
077000010615     C                   endif
077100990910     C                   endif
077200000710     C                   endif
077300010530     C                   else
077400010530     C                   eval      wrkesito = '9'
077500010530     C                   endif
077600990910     C*
077700990914     C                   if        %open(tivin00r)
077800990908     C                   close     tivin00r
077900990914     C                   endif
078000020913     C                   if        %open(fivabwwr)
078100020913     C                   close     fivabwwr
078200990914     C                   endif
078300050722     C                   if        %open(fivatwwr)
078400050722     C                   close     fivatwwr
078500050722     C                   endif
078600010530     C*
078700010530     C                   if        vlrpoi <> 999
078800010530     C                   eval      invfil = vlrpoi
078900010530     C                   endif
079000990910     C*
079100050722     C                   if        §ctrokvb > 0
079200010530     C                             and invfil > *zeros
079300000613     C                   exsr      invio
079400990920     C                   endif
079500010612     C*
079600010612     C                   if        flgGiro = '1'
079700010612     C                   exsr      endela
079800010612     C                   endif
079900990920     C*
080000910830     C                   ENDSR
080100000613     C***
080200050722
080300050722
080400050722
080500050722     C*----------------------------------------------------*
080600050722     C*  SCARICAMENTO BUFFER RECORDS VAT
080700050722     C*----------------------------------------------------*
080800050722     C     WRIVAT        BEGSR
080900050722     C*
081000050722     C* Attribuisco la stessa chiave della testata bolle
081100050722     C                   EVAL      VATAAS = VABAAS
081200050722     C                   EVAL      VATLNP = VABLNP
081300050722     C                   EVAL      VATNRS = VABNRS
081400050722     C                   EVAL      VATNSP = VABNSP
081500050722     C                   EVAL      VATCCM = VABCCM
081600061023     C*
081700061023     C* Imposto il tipo record sul file VAT a 'A' x indicare il referente consegna
081800061023     C                   EVAL      VATTRC = 'A'
081900061023     C                   EVAL      VATNOT = '.'
082000061023     C*
082100061023     C* Scarico il buffer del FNVAT
082200061023     C                   WRITE     FIVAT000
082300061023     C                   ADD       1             §CTROKVT
082400050722     C*
082500050722     C* Imposto il tipo record sul file VAT a 'B' x indicare il telefono destinatario
082600050722     C                   EVAL      VATTRC = 'B'
082700061023     C                   EVAL      VATNOT = %trim(VABNOT)
082800050722     C*
082900050722     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
083000061023     C                   IF        VATNOT = *blanks
083100061023     C                   EVAL      VATNOT = '.'
083200050722     C                   ENDIF
083300061023     C*
083400061023     C* Scarico il buffer del FNVAT
083500061023     C                   WRITE     FIVAT000
083600061023     C                   ADD       1             §CTROKVT
083700050722     C*
083800050722     C                   ENDSR
083900050722
084000050722
084100050722
084200050722     C*----------------------------------------------------*
084300050722     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
084400050722     C*----------------------------------------------------*
084500050722     C     PREVAT        BEGSR
084600050722     C*
084700050722     C* Compongo il nome del membro da dare al FIVATWWR
084800050722     C                   eval      parmbr = vlrhdl
084900050722     C                   movel     'M'           parmbr
085000050722     C                   eval      parccm = vlrksc
085100050722     C                   eval      paropz = '1'
085200050722     C* Effettuo la chiamata al CLLE preposto
085300050722     C                   call(e)   'TITVVTC'
085400050722     C                   parm                    parccm
085500050722     C                   parm                    parmbr
085600050722     C                   parm                    paropz
085700050722     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
085800050722     C                   if        %error
085900050722     C                   movel     '1'           chkcall
086000050722     C                   else
086100050722     C                   movel     '0'           chkcall
086200050722     C                   endif
086300050722     C*
086400050722     C                   ENDSR
086500990920
086600010530
086700010530
086800010530
086900010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
087000010530     C     repfil        BEGSR
087100010530     C*
087200010530     C                   if        invfil = *zeros and
087300010530     C                             depfil > *zeros and
087400010613     C                             (vinflg = *blanks or
087500010613     C                              vinflg = *zeros)
087600010530     C
087700010530     C                   eval      invfil = depfil
087800010530     C                   endif
087900010530     C*
088000010530     C                   if        depfil <> invfil and
088100010530     C                             invfil > *zeros
088200010530     C                   eval      flgMulti = '1'
088300010530     C                   if        vinflg = *blanks
088400010530     C                   add       1             cntNonEl
088500010530     C                   endif
088600010530     C                   endif
088700010530     C*
088800010530     C                   if        vinflg = '2'
088900010530     C                   eval      flgStato = '2'
089000010530     C                   endif
089100010530     C*
089200010530     C                   ENDSR
089300010530     C***
089400010530
089500010530
089600010530
089700010530
089800990920      /TITLE Invio dei dati al punto operativo.
089900000613     C     invio         BEGSR
090000050722     C*
090100050722     C* 1° invio FIVAT
090200050722     C                   reset                   dscmz
090300050722     C                   move      vlrpoi        cmzdst
090400050722     C                   eval      cmzfld = 'FIVATWWR'
090500050722     C                   eval      cmzmbd = vlrhdl
090600050722     C                   eval      %subst(cmzmbd:1:1) = 'M'
090700050722     C***                if        prmfir = *blanks
090800050722     C                   eval      cmzfla = 'FIVAT00F'
090900050722     C                   eval      cmzmba = 'FIVAT00F'
091000050722     C***                else
091100050722     C***                eval      cmzfla = prmfir
091200050722     C***                eval      cmzmba = prmfir
091300050722     C***                endif
091400050722     C                   eval      cmznrr = *zeros
091500050722     C                   move      §ctrokvt      cmznrr
091600050722     C                   eval      cmzlba = vlrfl1
091700050722     C                   call(e)   'TIS711C'
091800050722     C                   parm                    dscmz
091900050722     C                   parm      *blanks       esito
092000050722     C                   if        %error
092100050722     C                             or cmzerr = '1'
092200050722     C                             or esito  = '1'
092300050722     C                   eval      wrkesito = '3'
092400050722     C                   else
092500050722     C*
092600050722     C* 2° invio FIVAB
092700050722     C                   reset                   dscmz
092800050722     C                   move      vlrpoi        cmzdst
092900050722     C                   eval      cmzfld = vlrfou
093000050722     C                   eval      cmzmbd = vlrhdl
093100050722     C                   eval      %subst(cmzmbd:1:1) = 'M'
093200050722     C***                if        prmfir = *blanks
093300050722     C                   eval      cmzfla = 'FIVAB00F'
093400050722     C                   eval      cmzmba = 'FIVAB00F'
093500050722     C***                else
093600050722     C***                eval      cmzfla = prmfir
093700050722     C***                eval      cmzmba = prmfir
093800050722     C***                endif
093900050722     C                   eval      cmznrr = *zeros
094000050722     C                   move      §ctrokvb      cmznrr
094100050722     C                   eval      cmzlba = vlrfl1
094200050722     C                   call(e)   'TIS711C'
094300050722     C                   parm                    dscmz
094400050722     C                   parm      *blanks       esito
094500050722     C                   if        %error
094600050722     C                             or cmzerr = '1'
094700050722     C                             or esito  = '1'
094800050722     C                   eval      wrkesito = '3'
094900050722     C                   endif
095000050722     C                   endif
095100990920     C*
095200000613     C                   ENDSR
095300000613     C***
095400010530
095500010530
095600010530
095700010530
095800010530      /TITLE Invio dei dati al punto operativo.
095900010530     C     opeini        BEGSR
096000010530     C*
096100010530     C* Inizializzo flag e contatori operativi
096200010530     C                   movel     '0'           flgGiro           1
096300010530     C                   movel     '0'           flgMulti          1
096400010530     C                   movel     '1'           flgStato          1
096500010615     C                   movel     '0'           flgOk             1
096600010530     C                   z-add     *zeros        cntNonEl         10 0
096700010530     C                   z-add     *zeros        depfil            3 0
096800010530     C                   z-add     *zeros        invfil            3 0
096900030407     C*
097000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
097100030407     C                   if        %subst(vlrppt:1:1) = 'S'
097200030407     C                   seton                                        50
097300030407     C                   else
097400030407     C                   setoff                                       50
097500030407     C                   endif
097600041001     C*
097700041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
097800041001     C                   if        %subst(vlrppt:2:1) = 'S'
097900041001     C                   seton                                        51
098000041001     C                   else
098100041001     C                   setoff                                       51
098200041001     C                   endif
098300041025     C*
098400041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
098500041025     C* (naturalmente solo se nn già presente)
098600041025     C                   if        %subst(vlrppt:3:1) = 'S'
098700041025     C                   seton                                        52
098800041025     C                   else
098900041025     C                   setoff                                       52
099000041025     C                   endif
099100010530     C*
099200010530     C                   ENDSR
099300010530     C***
099400040407
099500040407
099600040407
099700040407
099800040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
099900040407     C     exeBon        BEGSR
100000040407     C*
100100040407     C* Imposto i parametri x il *pgm d "bonifica"
100200040407     C                   EVAL      ElencoChar = *blanks
100300040407     C                   EVAL      TipoElenco = *blanks
100400040407     C                   EVAL      CharSost   = *blanks
100500040629     C                   EVAL      UpperCase  = '1'
100600040407     C                   EVAL      ChkNull    = '1'
100700040407     C                   EVAL      CharNull   = *blanks
100800040629     C*
100900040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
101000040629     C                   EVAL      TxtInOut   = VABRSD
101100040629     C                   EVAL      EsitoBon   = *blanks
101200040407     C*
101300040407     C                   CALL      'XCHKCHAR'
101400040407     C                   PARM                    TxtInOut
101500040407     C                   PARM                    ElencoChar
101600040407     C                   PARM                    TipoElenco
101700040407     C                   PARM                    CharSost
101800040407     C                   PARM                    UpperCase
101900040407     C                   PARM                    ChkNull
102000040407     C                   PARM                    CharNull
102100040407     C                   PARM                    EsitoBon
102200040407     C*
102300040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
102400040407     C                   IF        EsitoBon = '1'
102500040629     C                   EVAL      VABRSD = TxtInOut
102600040407     C                   ENDIF
102700040629     C*
102800040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
102900040629     C                   EVAL      TxtInOut   = VABIND
103000040629     C                   EVAL      EsitoBon   = *blanks
103100040629     C*
103200040629     C                   CALL      'XCHKCHAR'
103300040629     C                   PARM                    TxtInOut
103400040629     C                   PARM                    ElencoChar
103500040629     C                   PARM                    TipoElenco
103600040629     C                   PARM                    CharSost
103700040629     C                   PARM                    UpperCase
103800040629     C                   PARM                    ChkNull
103900040629     C                   PARM                    CharNull
104000040629     C                   PARM                    EsitoBon
104100040629     C*
104200040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
104300040629     C                   IF        EsitoBon = '1'
104400040629     C                   EVAL      VABIND = TxtInOut
104500040629     C                   ENDIF
104600040629     C*
104700040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
104800040629     C                   EVAL      TxtInOut   = VABLOD
104900040629     C                   EVAL      EsitoBon   = *blanks
105000040629     C*
105100040629     C                   CALL      'XCHKCHAR'
105200040629     C                   PARM                    TxtInOut
105300040629     C                   PARM                    ElencoChar
105400040629     C                   PARM                    TipoElenco
105500040629     C                   PARM                    CharSost
105600040629     C                   PARM                    UpperCase
105700040629     C                   PARM                    ChkNull
105800040629     C                   PARM                    CharNull
105900040629     C                   PARM                    EsitoBon
106000040629     C*
106100040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
106200040629     C                   IF        EsitoBon = '1'
106300040629     C                   EVAL      VABLOD = TxtInOut
106400040629     C                   ENDIF
106500040629     C*
106600040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
106700040629     C                   EVAL      TxtInOut   = VABNOT
106800040629     C                   EVAL      EsitoBon   = *blanks
106900040630     C                   EVAL      UpperCase  = *blanks
107000040629     C*
107100040629     C                   CALL      'XCHKCHAR'
107200040629     C                   PARM                    TxtInOut
107300040629     C                   PARM                    ElencoChar
107400040629     C                   PARM                    TipoElenco
107500040629     C                   PARM                    CharSost
107600040629     C                   PARM                    UpperCase
107700040629     C                   PARM                    ChkNull
107800040629     C                   PARM                    CharNull
107900040629     C                   PARM                    EsitoBon
108000040629     C*
108100040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
108200040629     C                   IF        EsitoBon = '1'
108300040629     C                   EVAL      VABNOT = TxtInOut
108400040629     C                   ENDIF
108500040629     C*
108600040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
108700040629     C                   EVAL      TxtInOut   = VABNT2
108800040629     C                   EVAL      EsitoBon   = *blanks
108900040630     C                   EVAL      UpperCase  = *blanks
109000040629     C*
109100040629     C                   CALL      'XCHKCHAR'
109200040629     C                   PARM                    TxtInOut
109300040629     C                   PARM                    ElencoChar
109400040629     C                   PARM                    TipoElenco
109500040629     C                   PARM                    CharSost
109600040629     C                   PARM                    UpperCase
109700040629     C                   PARM                    ChkNull
109800040629     C                   PARM                    CharNull
109900040629     C                   PARM                    EsitoBon
110000040629     C*
110100040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
110200040629     C                   IF        EsitoBon = '1'
110300040629     C                   EVAL      VABNT2 = TxtInOut
110400040629     C                   ENDIF
110500040407     C*
110600040407     C                   ENDSR
110700010530
110800070411
110900070411     C     *pssr         BEGSR
111000070411     C*
111100070411     C                   if        %open(tivin00r)
111200070411     C                   close     tivin00r
111300070411     C                   endif
111400070411     C                   if        %open(fivabwwr)
111500070411     C                   close     fivabwwr
111600070411     C                   endif
111700070411     C                   if        %open(fivatwwr)
111800070411     C                   close     fivatwwr
111900070411     C                   endif
112000070411     C*
112100070411     C* Effettuo la chiamata al CLLE preposto
112200070411     C                   call(e)   'TITVVTC'
112300070411     C                   parm                    parccm
112400070411     C                   parm                    parmbr
112500070411     C                   parm      '2'           paropz
112600070411     C*
112700070411     C                   eval      wrkesito = '2'
112800070411     C*
112900070411     C                   seton                                        LR
113000070411     C*
113100070411     C                   ENDSR     '*CANCL'
113200070411     C***
113300070411
113400010530
113500010530
113600990910
113700000613     C     *inzsr        BEGSR
113800990910     C*
113900990910     C     *entry        plist
114000990920     C                   parm                    tivlrds
114100990921     C                   parm      wrkesito      esito
114200000724     C                   parm                    prmlit
114300000710     C                   parm                    prmfir
114400010606     C*
114500010606     C* CALCOLA LA DATA CORRENTE
114600010606     C                   time                    wn14             14 0
114700010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
114800010606     C                   z-add     wn8           g08dat
114900010606     C                   z-add     *zeros        g08inv
115000010606     C                   movel     '0'           g08err
115100010606     C                   call      'XSRDA8'
115200010606     C                   parm                    wlbda8
115300010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
115400000613     C*
115500000613     C                   ENDSR
115600000613     C***
