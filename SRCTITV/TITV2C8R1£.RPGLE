000100110413      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200120612     H DFTACTGRP(*NO) BNDDIR('UBBNDDIR') ACTGRP('BARTVAS')
000300120612     H DECEDIT('0,') DATEDIT(*DMY.)
000400990908
000500990910     Ftivin00r  uF   E             DISK    usropn
000600110413     FEDIVABwr  O    E             DISK    usropn
000700120612     FTIVGD00F  O    E             DISK
000800990908
000900000512     D*------------
001000990920     D dscmz         e ds                  inz
001100990910     D psds           sds
001200990910     D  procname         *PROC
001300941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
001400941229     D* 10+3
001500941229     D                 DS
001600941229     D  WS15                   1      1
001700941229     D  WI15                   2     11
001800941229     D  WV15                  12     12
001900941229     D  WD15                  13     15
002000941229     D  WASC15                 1     15
002100941229     D*  6+1
002200941229     D                 DS
002300941229     D  WS09                   1      1
002400941229     D  WI09                   2      7
002500941229     D  WV09                   8      8
002600941229     D  WD09                   9      9
002700941229     D  WASC09                 1      9
002800941229     D*  2+3
002900941229     D                 DS
003000941229     D  WS07                   1      1
003100941229     D  WI07                   2      3
003200941229     D  WV07                   4      4
003300941229     D  WD07                   5      7
003400941229     D  WASC07                 1      7
003500990920     D tivlrds       e ds                  extname(tivlr00f)
003600041025     D tisi95ds      e ds
003700990910     D fnvabaar      e ds                  inz
003800120612     D fnvab00t      e ds                  inz
003900990910     D esito           s              1
004000000724     D prmlit          s             10
004100000710     D prmfir          s             10
004200990921     D wrkesito        s                   like(esito)
004300000613     D rrnum           s              6  0 INZ(*zeros)
004400060410     D wVHBCAS         s                   LIKE(VHBCAS) INZ(*blanks)
004500010606     D*
004600040407     D TxtInOut        S           2048
004700040407     D ElencoChar      S            256
004800040407     D TipoElenco      S              1
004900040407     D CharSost        S              1
005000040407     D UpperCase       S              1
005100040407     D ChkNull         S              1
005200040407     D CharNull        S              1
005300040407     D EsitoBon        S              1
005400990908
005500000913     C                   reset                   rrnum
005600990921     C                   reset                   esito
005700990921     C                   reset                   wrkesito
005800010530     C*
005900010530     C                   exsr      opeini
006000010530     C                   exsr      rwvab
006100041025     C*
006200041025     C* Esegue lancio TISI95R solo x chiusura
006300041025     C                   CLEAR                   TISI95DS
006400041025     C                   EVAL      I95TLA = 'C'
006500041025     C                   CALL      'TISI95R'
006600041025     C                   PARM                    TISI95DS
006700010530     C*
006800010530     C                   seton                                        lr
006900010530
007000010530
007100010530
007200010530
007300010530     C*--------------------------------------------------------
007400010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007500010530     C*--------------------------------------------------------
007600010530     C     PREELA        BEGSR
007700000724     C*
007800000724     C* SE OCCORRE SPEDIRE IN FILIALE
007900010530     C                   if        invfil <> *zeros and
008000010530     C                             flgGiro = '0'
008100010530     C*
008200010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008300010530     C                   eval      flgGiro = '1'
008400010530     C*
008500010530     C                   endif
008600010530     C*
008700010530     C                   ENDSR
008800010530     C***
008900010530
009000010530
009100010530
009200010530
009300010530
009400010530
009500010530     C*--------------------------------------------------------
009600010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009700010530     C*--------------------------------------------------------
009800010530     C     ENDELA        BEGSR
009900010530     C*
010000010530     C                   ENDSR
010100010530     C***
010200990908
010300010530
010400010530
010500010530
010600910830     C*--------------------------------------------------------
010700110413     C* RWVAB   LEGGE tivin00r E SCRIVE EDIVABWF              *
010800910830     C*--------------------------------------------------------
010900941229     C     RWVAB         BEGSR
011000990910     C*
011100990914     C                   if        not %open(tivin00r)
011200990908     C                   open      tivin00r
011300990914     C                   endif
011400110413     C                   if        not %open(edivabwr)
011500110413     C                   open      edivabwr
011600990914     C                   endif
011700990910     C*
011800990910     C                   clear                   §CTROK
011900990910     C                   clear                   §CTRMO
012000990910     C                   clear                   §CTRNO
012100990910     C*
012200921023     C                   DO        *HIVAL
012300990913     C*
012400990915     C                   READ      tivin00r                               70
012500010618     C*
012600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012700010618     C                   if        vinflg = '1'
012800010618     C                   eval      flgOk = '1'
012900010618     C                   endif
013000010705     C*
013100010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
013200010705     C                   if        %subst(vindta:1:1) = '='
013300010705     C                   eval      %subst(vindta:1:1) = *blanks
013400010705     C                   endif
013500010705     C*
013600051017     C                   if        vindta  > *blanks
013700000613     C                   add       1             rrnum
013800010601     C* ...elimino eventuali segni
013900010601     C     '+':' '       xlate     vindta        vindta
014000990913     C*
014100010601     C                   if        *in70 = *off and
014200990913     C                             (vinflg = *blanks
014300010711     C                              or vinflg = '0'
014400010711     C                              or vinflg = '2')
014500990913     C*
014600110413     C                   clear                   EDIVAB00
014700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014800010711     C                   if        vinflg = *blanks or vinflg = '0'
014900010711     C                   clear                   vinmsg
015000010711     C                   endif
015100010711     C*
015200990913     C                   eval      fnvabaar = vindta
015300991022     C*
015400991022     C                   testb     x'3F'         vhbatb                   02
015500991022     C                   if        not *in02
015600991022     C*
015700971021     C                   SETOFF                                       303132
015800971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
015900971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
016000971023     C*    OPPURE TUTTI BLANKS
016100010530     C*
016200010530     C                   TESTN                   VHBLNP               30
016300010530     C  N30              TESTN                   VHBLNP                 30
016400010530     C  N30              TESTN                   VHBLNP                   30
016500010530     C                   MOVE      VHBLNP        W001A             1
016600010530     C     *IN30         IFEQ      *OFF
016700010530     C     W001A         ORLT      '0'
016800010530     C                   SETON                                        31
016900010530     C                   eval      vinmsg = %trimr(vinmsg)
017000010530     C                             + ' ' + 'VHBLNP'
017100010530     C                   ELSE
017200010530     C                   MOVE      VHBLNP        VABLNP
017300010530     C     VABLNP        IFLE      *ZEROS
017400010530     C                   SETON                                        31
017500010530     C                   eval      vinmsg = %trimr(vinmsg)
017600010530     C                             + ' ' + 'VHBLNP'
017700010530     C                   ENDIF
017800010530     C                   ENDIF
017900010530     C*
018000010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018100010530     C                   eval      depfil = VABLNP
018200010530     C                   exsr      repfil
018300010601     C                   if        depfil = invfil
018400021014     C                   if        vlrpoi = 999
018500021010     C                   MOVE(P)   invfil        VABFGS
018600021014     C                   else
018700021014     C                   MOVE(P)   vlrpoi        VABFGS
018800021014     C                   endif
018900010530     C*
019000010530     C                   exsr      PREELA
019100010530     C*
019200971023     C                   TESTN                   VHBCCM               30
019300971023     C  N30              TESTN                   VHBCCM                 30
019400971023     C  N30              TESTN                   VHBCCM                   30
019500971021     C                   MOVE      VHBCCM        W001A             1
019600971021     C     *IN30         IFEQ      *OFF
019700971021     C     W001A         ORLT      '0'
019800971021     C                   SETON                                        31
019900010604     C                   eval      vinmsg = %trimr(vinmsg)
020000010604     C                             + ' ' + 'VHBCCM'
020100971021     C                   ELSE
020200971021     C                   MOVE      VHBCCM        VABCCM
020300971024     C     VABCCM        IFLE      *ZEROS
020400971024     C                   SETON                                        31
020500010604     C                   eval      vinmsg = %trimr(vinmsg)
020600010604     C                             + ' ' + 'VHBCCM'
020700971024     C                   ENDIF
020800971021     C                   ENDIF
020900971021     C*
021000971023     C                   TESTN                   VHBAAS               30
021100971023     C  N30              TESTN                   VHBAAS                 30
021200971023     C  N30              TESTN                   VHBAAS                   30
021300971021     C                   MOVE      VHBAAS        W001A             1
021400971021     C     *IN30         IFEQ      *OFF
021500971021     C     W001A         ORLT      '0'
021600971023     C     W001A         ANDNE     *BLANK
021700971021     C                   SETON                                        32
021800010606     C                   MOVEL     datcor        VABAAS
021900010604     C                   eval      vinmsg = %trimr(vinmsg)
022000010604     C                             + ' ' + 'VHBAAS'
022100971021     C                   ELSE
022200971021     C                   MOVE      VHBAAS        VABAAS
022300971021     C                   ENDIF
022400971021     C*
022500971023     C                   TESTN                   VHBMGS               30
022600971023     C  N30              TESTN                   VHBMGS                 30
022700971023     C  N30              TESTN                   VHBMGS                   30
022800971021     C                   MOVE      VHBMGS        W001A             1
022900971021     C     *IN30         IFEQ      *OFF
023000971021     C     W001A         ORLT      '0'
023100971023     C     W001A         ANDNE     *BLANK
023200971021     C                   SETON                                        32
023300010601     C                   MOVE      '0101'        VABMGS
023400010604     C                   eval      vinmsg = %trimr(vinmsg)
023500010604     C                             + ' ' + 'VHBMGS'
023600971021     C                   ELSE
023700971021     C                   MOVE      VHBMGS        VABMGS
023800971021     C                   ENDIF
023900971021     C*
024000971023     C                   TESTN                   VHBNRS               30
024100971023     C  N30              TESTN                   VHBNRS                 30
024200971023     C  N30              TESTN                   VHBNRS                   30
024300971021     C                   MOVE      VHBNRS        W001A             1
024400971021     C     *IN30         IFEQ      *OFF
024500971021     C     W001A         ORLT      '0'
024600971023     C     W001A         ANDNE     *BLANK
024700971021     C                   SETON                                        32
024800971021     C                   MOVE      *ZERO         VABNRS
024900010604     C                   eval      vinmsg = %trimr(vinmsg)
025000010604     C                             + ' ' + 'VHBNRS'
025100971021     C                   ELSE
025200971021     C                   MOVE      VHBNRS        VABNRS
025300971021     C                   ENDIF
025400971021     C*
025500971023     C                   TESTN                   VHBNSP               30
025600971023     C  N30              TESTN                   VHBNSP                 30
025700971023     C  N30              TESTN                   VHBNSP                   30
025800971021     C                   MOVE      VHBNSP        W001A             1
025900971021     C     *IN30         IFEQ      *OFF
026000971021     C     W001A         ORLT      '0'
026100971023     C     W001A         ANDNE     *BLANK
026200971021     C                   SETON                                        32
026300971021     C                   MOVE      *ZERO         VABNSP
026400010604     C                   eval      vinmsg = %trimr(vinmsg)
026500010604     C                             + ' ' + 'VHBNSP'
026600971021     C                   ELSE
026700971021     C                   MOVE      VHBNSP        VABNSP
026800971021     C                   ENDIF
026900971021     C*
027000971023     C                   TESTN                   VHBLNA               30
027100971023     C  N30              TESTN                   VHBLNA                 30
027200971023     C  N30              TESTN                   VHBLNA                   30
027300971021     C                   MOVE      VHBLNA        W001A             1
027400971021     C     *IN30         IFEQ      *OFF
027500971021     C     W001A         ORLT      '0'
027600971023     C     W001A         ANDNE     *BLANK
027700971021     C                   SETON                                        32
027800971021     C                   MOVE      *ZERO         VABLNA
027900010604     C                   eval      vinmsg = %trimr(vinmsg)
028000010604     C                             + ' ' + 'VHBLNA'
028100971021     C                   ELSE
028200971021     C                   MOVE      VHBLNA        VABLNA
028300971021     C                   ENDIF
028400971021     C*
028500971023     C                   TESTN                   VHBCTR               30
028600971023     C  N30              TESTN                   VHBCTR                 30
028700971023     C  N30              TESTN                   VHBCTR                   30
028800971021     C                   MOVE      VHBCTR        W001A             1
028900971021     C     *IN30         IFEQ      *OFF
029000971021     C     W001A         ORLT      '0'
029100971023     C     W001A         ANDNE     *BLANK
029200971021     C                   SETON                                        32
029300971021     C                   MOVE      *ZERO         VABCTR
029400010604     C                   eval      vinmsg = %trimr(vinmsg)
029500010604     C                             + ' ' + 'VHBCTR'
029600971021     C                   ELSE
029700971021     C                   MOVE      VHBCTR        VABCTR
029800971021     C                   ENDIF
029900941229     C*
030000971021     C                   MOVE      *ZERO         VABIAS
030100941229     C                   MOVE      VHBIAS        WASC15
030200971023     C                   TESTN                   WI15                 30
030300971023     C  N30              TESTN                   WI15                   30
030400971023     C  N30              TESTN                   WI15                     30
030500971021     C                   MOVE      WI15          W001A             1
030600971021     C     *IN30         IFEQ      *OFF
030700971021     C     W001A         ORLT      '0'
030800971023     C     W001A         ANDNE     *BLANK
030900971021     C                   SETON                                        32
031000971021     C                   MOVE      *ZERO         VABIAS
031100010604     C                   eval      vinmsg = %trimr(vinmsg)
031200010604     C                             + ' ' + 'VHBIAS'
031300971021     C                   ELSE
031400971021     C                   MOVEL     WI15          VABIAS
031500971023     C                   TESTN                   WD15                 30
031600971023     C  N30              TESTN                   WD15                   30
031700971023     C  N30              TESTN                   WD15                     30
031800971021     C                   MOVE      WD15          W001A             1
031900971021     C     *IN30         IFEQ      *OFF
032000971021     C     W001A         ORLT      '0'
032100971023     C     W001A         ANDNE     *BLANK
032200971021     C                   SETON                                        32
032300971021     C                   MOVE      *ZERO         VABIAS
032400010604     C                   eval      vinmsg = %trimr(vinmsg)
032500010604     C                             + ' ' + 'VHBIAS'
032600971021     C                   ELSE
032700971021     C                   MOVE      WD15          VABIAS
032800971021     C                   ENDIF
032900050118     C                   ENDIF
033000941229     C*
033100971023     C                   TESTN                   VHBNCL               30
033200971023     C  N30              TESTN                   VHBNCL                 30
033300971023     C  N30              TESTN                   VHBNCL                   30
033400971021     C                   MOVE      VHBNCL        W001A             1
033500971021     C     *IN30         IFEQ      *OFF
033600971021     C     W001A         ORLT      '0'
033700971021     C                   SETON                                        32
033800971021     C                   MOVE      *ZERO         VABNCL
033900010604     C                   eval      vinmsg = %trimr(vinmsg)
034000010604     C                             + ' ' + 'VHBNCL'
034100971021     C                   ELSE
034200971021     C                   MOVE      VHBNCL        VABNCL
034300971021     C                   ENDIF
034400941229     C*
034500971021     C                   MOVE      *ZERO         VABPKB
034600971021     C                   MOVE      VHBPKB        WASC09
034700010719     C                   IF        WI09 = *blanks
034800010719     C                   EVAL      WI09 = *zeros
034900010719     C                   ENDIF
035000971023     C                   TESTN                   WI09                 30
035100971023     C  N30              TESTN                   WI09                   30
035200971023     C  N30              TESTN                   WI09                     30
035300971021     C                   MOVE      WI09          W001A             1
035400971021     C     *IN30         IFEQ      *OFF
035500971021     C     W001A         ORLT      '0'
035600971021     C                   SETON                                        32
035700971021     C                   MOVE      *ZERO         VABPKB
035800010604     C                   eval      vinmsg = %trimr(vinmsg)
035900010604     C                             + ' ' + 'VHBPKB'
036000971021     C                   ELSE
036100971021     C                   MOVEL     WI09          VABPKB
036200010719     C                   IF        WD09 = *blanks
036300010719     C                   EVAL      WD09 = *zeros
036400010719     C                   ENDIF
036500971023     C                   TESTN                   WD09                 30
036600971023     C  N30              TESTN                   WD09                     30
036700971021     C                   MOVE      WD09          W001A             1
036800971021     C     *IN30         IFEQ      *OFF
036900971021     C     W001A         ORLT      '0'
037000971023     C     W001A         ANDNE     *BLANK
037100971021     C                   SETON                                        32
037200971021     C                   MOVE      *ZERO         VABPKB
037300010604     C                   eval      vinmsg = %trimr(vinmsg)
037400010604     C                             + ' ' + 'VHBPKB'
037500971021     C                   ELSE
037600971021     C                   MOVE      WD09          VABPKB
037700971021     C                   ENDIF
037800050118     C                   ENDIF
037900971021     C*
038000041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
038100041001     C                   IF        *IN51 = *OFF
038200971021     C                   MOVE      *ZERO         VABVLB
038300971021     C                   MOVE      VHBVLB        WASC07
038400010719     C                   IF        WI07 = *blanks
038500010719     C                   EVAL      WI07 = *zeros
038600010719     C                   ENDIF
038700971023     C                   TESTN                   WI07                 30
038800971023     C  N30              TESTN                   WI07                   30
038900971023     C  N30              TESTN                   WI07                     30
039000971021     C                   MOVE      WI07          W001A             1
039100971021     C     *IN30         IFEQ      *OFF
039200971021     C     W001A         ORLT      '0'
039300971023     C     W001A         ANDNE     *BLANK
039400971021     C                   SETON                                        32
039500971021     C                   MOVE      *ZERO         VABVLB
039600010604     C                   eval      vinmsg = %trimr(vinmsg)
039700010604     C                             + ' ' + 'VHBVLB'
039800971021     C                   ELSE
039900971021     C                   MOVEL     WI07          VABVLB
040000010719     C                   IF        WD07 = *blanks
040100010719     C                   EVAL      WD07 = *zeros
040200010719     C                   ENDIF
040300971023     C                   TESTN                   WD07                 30
040400971023     C  N30              TESTN                   WD07                   30
040500971023     C  N30              TESTN                   WD07                     30
040600971021     C                   MOVE      WD07          W001A             1
040700971021     C     *IN30         IFEQ      *OFF
040800971021     C     W001A         ORLT      '0'
040900971023     C     W001A         ANDNE     *BLANK
041000971021     C                   SETON                                        32
041100971021     C                   MOVE      *ZERO         VABVLB
041200010604     C                   eval      vinmsg = %trimr(vinmsg)
041300010604     C                             + ' ' + 'VHBVLB'
041400971021     C                   ELSE
041500971021     C                   MOVE      WD07          VABVLB
041600971021     C                   ENDIF
041700050118     C                   ENDIF
041800041001     C                   ELSE
041900041001     C* ...altrimenti forzo azzeramento del volume
042000041001     C                   Z-ADD     *zeros        VABVLB
042100041001     C                   ENDIF
042200941229     C*
042300971021     C                   MOVE      *ZERO         VABQFT
042400971021     C                   MOVE      VHBQFT        WASC15
042500010719     C                   IF        WI15 = *blanks
042600010719     C                   EVAL      WI15 = *zeros
042700010719     C                   ENDIF
042800971023     C                   TESTN                   WI15                 30
042900971023     C  N30              TESTN                   WI15                   30
043000971023     C  N30              TESTN                   WI15                     30
043100971021     C                   MOVE      WI15          W001A             1
043200971021     C     *IN30         IFEQ      *OFF
043300971021     C     W001A         ORLT      '0'
043400971023     C     W001A         ANDNE     *BLANK
043500971021     C                   SETON                                        32
043600971021     C                   MOVE      *ZERO         VABQFT
043700010604     C                   eval      vinmsg = %trimr(vinmsg)
043800010604     C                             + ' ' + 'VHBQFT'
043900971021     C                   ELSE
044000971021     C                   MOVEL     WI15          VABQFT
044100010719     C                   IF        WD15 = *blanks
044200010719     C                   EVAL      WD15 = *zeros
044300010719     C                   ENDIF
044400971023     C                   TESTN                   WD15                 30
044500971023     C  N30              TESTN                   WD15                   30
044600971023     C  N30              TESTN                   WD15                     30
044700971021     C                   MOVE      WD15          W001A             1
044800971021     C     *IN30         IFEQ      *OFF
044900971021     C     W001A         ORLT      '0'
045000971023     C     W001A         ANDNE     *BLANK
045100971021     C                   SETON                                        32
045200971021     C                   MOVE      *ZERO         VABQFT
045300010604     C                   eval      vinmsg = %trimr(vinmsg)
045400010604     C                             + ' ' + 'VHBQFT'
045500971021     C                   ELSE
045600971021     C                   MOVE      WD15          VABQFT
045700971021     C                   ENDIF
045800050118     C                   ENDIF
045900941229     C*
046000971021     C                   MOVE      *ZERO         VABCAS
046100060410     C* Se presente separatore decimale alla giusta posizione => forzo sempre la virgola
046200060410     C                   EVAL      wVHBCAS = VHBCAS
046300060410     C                   IF        %subst(wVHBCAS:12:1) = '.'
046400060410     C                   EVAL      %subst(wVHBCAS:12:1) = ','
046500060410     C                   ENDIF
046600060410     C*
046700060410     C                   MOVEL(P)  wVHBCAS       PiStr
046800060410     C                   EXSR      CHKNUM
046900060410     C                   IF        PiNum=*on
047000060410     C                   Z-ADD     PiVal         VABCAS
047100060410     C                   ELSE
047200060410     C                   SETON                                        32
047300060410     C                   Z-ADD     *zeros        VABCAS
047400060410     C                   EVAL      vinmsg = %trimr(vinmsg)
047500060410     C                             + ' ' + 'VHBCAS'
047600060410     C                   ENDIF
047700971021     C*
047800971023     C                   TESTN                   VHBRMN               30
047900971023     C  N30              TESTN                   VHBRMN                 30
048000971023     C  N30              TESTN                   VHBRMN                   30
048100971021     C                   MOVE      VHBRMN        W001A             1
048200971021     C     *IN30         IFEQ      *OFF
048300971021     C     W001A         ORLT      '0'
048400971021     C                   SETON                                        32
048500971021     C                   MOVE      *ZERO         VABRMN
048600010604     C                   eval      vinmsg = %trimr(vinmsg)
048700010604     C                             + ' ' + 'VHBRMN'
048800971021     C                   ELSE
048900971021     C                   MOVE      VHBRMN        VABRMN
049000971021     C                   ENDIF
049100971021     C*
049200971023     C                   TESTN                   VHBNCD               30
049300971023     C  N30              TESTN                   VHBNCD                 30
049400971023     C  N30              TESTN                   VHBNCD                   30
049500971021     C                   MOVE      VHBNCD        W001A             1
049600971021     C     *IN30         IFEQ      *OFF
049700971021     C     W001A         ORLT      '0'
049800971023     C     W001A         ANDNE     *BLANK
049900971021     C                   SETON                                        32
050000971021     C                   MOVE      *ZERO         VABNCD
050100010604     C                   eval      vinmsg = %trimr(vinmsg)
050200010604     C                             + ' ' + 'VHBNCD'
050300971021     C                   ELSE
050400971021     C                   MOVE      VHBNCD        VABNCD
050500971021     C                   ENDIF
050600971021     C*
050700971023     C                   TESTN                   VHBNCA               30
050800971023     C  N30              TESTN                   VHBNCA                 30
050900971023     C  N30              TESTN                   VHBNCA                   30
051000971021     C                   MOVE      VHBNCA        W001A             1
051100971021     C     *IN30         IFEQ      *OFF
051200971021     C     W001A         ORLT      '0'
051300971023     C     W001A         ANDNE     *BLANK
051400971021     C                   SETON                                        32
051500971021     C                   MOVE      *ZERO         VABNCA
051600010604     C                   eval      vinmsg = %trimr(vinmsg)
051700010604     C                             + ' ' + 'VHBNCA'
051800971021     C                   ELSE
051900971021     C                   MOVE      VHBNCA        VABNCA
052000971021     C                   ENDIF
052100971021     C*
052200971023     C                   TESTN                   VHBZNC               30
052300971023     C  N30              TESTN                   VHBZNC                 30
052400971023     C  N30              TESTN                   VHBZNC                   30
052500971021     C                   MOVE      VHBZNC        W001A             1
052600971021     C     *IN30         IFEQ      *OFF
052700971021     C     W001A         ORLT      '0'
052800971023     C     W001A         ANDNE     *BLANK
052900971021     C                   SETON                                        32
053000971021     C                   MOVE      *ZERO         VABZNC
053100010604     C                   eval      vinmsg = %trimr(vinmsg)
053200010604     C                             + ' ' + 'VHBZNC'
053300971021     C                   ELSE
053400971021     C                   MOVE      VHBZNC        VABZNC
053500971021     C                   ENDIF
053600971021     C*
053700971023     C                   TESTN                   VHBDCR               30
053800971023     C  N30              TESTN                   VHBDCR                 30
053900971023     C  N30              TESTN                   VHBDCR                   30
054000971021     C                   MOVE      VHBDCR        W001A             1
054100971021     C     *IN30         IFEQ      *OFF
054200971021     C     W001A         ORLT      '0'
054300971023     C     W001A         ANDNE     *BLANK
054400971021     C                   SETON                                        32
054500971021     C                   MOVE      *ZERO         VABDCR
054600010604     C                   eval      vinmsg = %trimr(vinmsg)
054700010604     C                             + ' ' + 'VHBDCR'
054800971021     C                   ELSE
054900971021     C                   MOVE      VHBDCR        VABDCR
055000971021     C                   ENDIF
055100971021     C*
055200971023     C                   TESTN                   VHBHCR               30
055300971023     C  N30              TESTN                   VHBHCR                 30
055400971023     C  N30              TESTN                   VHBHCR                   30
055500971021     C                   MOVE      VHBHCR        W001A             1
055600971021     C     *IN30         IFEQ      *OFF
055700971021     C     W001A         ORLT      '0'
055800971023     C     W001A         ANDNE     *BLANK
055900971021     C                   SETON                                        32
056000971021     C                   MOVE      *ZERO         VABHCR
056100010604     C                   eval      vinmsg = %trimr(vinmsg)
056200010604     C                             + ' ' + 'VHBHCR'
056300971021     C                   ELSE
056400971021     C                   MOVE      VHBHCR        VABHCR
056500971021     C                   ENDIF
056600941229     C*
056700971021     C                   MOVE      *ZERO         VABVMD
056800971021     C                   MOVE      VHBVMD        WASC15
056900010719     C                   IF        WI15 = *blanks
057000010719     C                   EVAL      WI15 = *zeros
057100010719     C                   ENDIF
057200971023     C                   TESTN                   WI15                 30
057300971023     C  N30              TESTN                   WI15                   30
057400971023     C  N30              TESTN                   WI15                     30
057500971021     C                   MOVE      WI15          W001A             1
057600971021     C     *IN30         IFEQ      *OFF
057700971021     C     W001A         ORLT      '0'
057800971023     C     W001A         ANDNE     *BLANK
057900971021     C                   SETON                                        32
058000971021     C                   MOVE      *ZERO         VABVMD
058100010604     C                   eval      vinmsg = %trimr(vinmsg)
058200010604     C                             + ' ' + 'VHBVMD'
058300971021     C                   ELSE
058400971021     C                   MOVEL     WI15          VABVMD
058500010719     C                   IF        WD15 = *blanks
058600010719     C                   EVAL      WD15 = *zeros
058700010719     C                   ENDIF
058800971023     C                   TESTN                   WD15                 30
058900971023     C  N30              TESTN                   WD15                   30
059000971023     C  N30              TESTN                   WD15                     30
059100971021     C                   MOVE      WD15          W001A             1
059200971021     C     *IN30         IFEQ      *OFF
059300971021     C     W001A         ORLT      '0'
059400971023     C     W001A         ANDNE     *BLANK
059500971021     C                   SETON                                        32
059600971021     C                   MOVE      *ZERO         VABVMD
059700010604     C                   eval      vinmsg = %trimr(vinmsg)
059800010604     C                             + ' ' + 'VHBVMD'
059900971021     C                   ELSE
060000971021     C                   MOVE      WD15          VABVMD
060100971021     C                   ENDIF
060200050118     C                   ENDIF
060300971021     C*
060400971023     C                   TESTN                   VHBANT               30
060500971023     C  N30              TESTN                   VHBANT                 30
060600971023     C  N30              TESTN                   VHBANT                   30
060700971021     C                   MOVE      VHBANT        W001A             1
060800971021     C     *IN30         IFEQ      *OFF
060900971021     C     W001A         ORLT      '0'
061000971023     C     W001A         ANDNE     *BLANK
061100971021     C                   SETON                                        32
061200971021     C                   MOVE      *ZERO         VABANT
061300010604     C                   eval      vinmsg = %trimr(vinmsg)
061400010604     C                             + ' ' + 'VHBANT'
061500971021     C                   ELSE
061600971021     C                   MOVE      VHBANT        VABANT
061700971021     C                   ENDIF
061800971021     C*
061900971021     C* CAMPI ALFABETICI:
062000971021     C                   MOVE      VHBATB        VABATB
062100971021     C                   MOVE      VHBCBO        VABCBO
062200971021     C                   MOVE      VHBRSD        VABRSD
062300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
062400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
062500020117     C* ==
062600971021     C                   MOVE      VHBRD2        VABRD2
062700971021     C                   MOVE      VHBIND        VABIND
062800971021     C                   MOVE      VHBCAD        VABCAD
062900971021     C                   MOVE      VHBLOD        VABLOD
063000971021     C                   MOVE      VHBPRD        VABPRD
063100971021     C                   MOVE      VHBNZD        VABNZD
063200971021     C                   MOVE      VHBGC1        VABGC1
063300971021     C                   MOVE      VHBGC2        VABGC2
063400971021     C                   MOVE      VHBTSP        VABTSP
063500971021     C                   MOVE      VHBVAS        VABVAS
063600971021     C                   MOVE      VHBNAS        VABNAS
063700971021     C                   MOVE      VHBTIC        VABTIC
063800971021     C                   MOVE      VHBVCA        VABVCA
063900971021     C                   MOVE      VHBGCA        VABGCA
064000971021     C                   MOVE      VHBRMA        VABRMA
064100971021     C                   MOVE      VHBXCO        VABXCO
064200971021     C                   MOVE      VHBNOT        VABNOT
064300971021     C                   MOVE      VHBNT2        VABNT2
064400971021     C                   MOVE      VHBCTM        VABCTM
064500971021     C                   MOVE      VHBFFD        VABFFD
064600971021     C                   MOVE      VHBTCR        VABTCR
064700971021     C                   MOVE      VHBCTS        VABCTS
064800971021     C                   MOVE      VHBFTM        VABFTM
064900971021     C                   MOVE      VHBVAD        VABVAD
065000971021     C                   MOVE      VHBGMA        VABGMA
065100971021     C                   MOVE      VHBGGA        VABGGA
065200971021     C                   MOVE      VHBGVA        VABGVA
065300971021     C                   MOVE      VHBTC1        VABTC1
065400971021     C                   MOVE      VHBTC2        VABTC2
065500971021     C                   MOVE      VHBSCL        VABSCL
065600971021     C                   MOVE      VHBRMO        VABRMO
065700971021     C                   MOVE      VHBCMO        VABCMO
065800971021     C                   MOVE      VHBNMO        VABNMO
065900971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
066000060329     C* NELLA NAZIONE MITTENTE ORIGINALE
066100060331     C                   IF        %trim(VHBNMO) < 'A'
066200060329     C                   MOVE      *BLANKS       VABNMO
066300060329     C                   ENDIF
066400050418     C*
066500050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
066600050418     C                   IF        %trim(VABNZD) = 'I'   OR
066700050418     C                             %trim(VABNZD) = 'IT'  OR
066800050418     C                             %trim(VABNZD) = 'ITA'
066900050418     C                   EVAL      VABNZD = *blanks
067000050418     C                   ENDIF
067100041025     C*
067200041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
067300041025     C                   IF        *IN52 = *ON
067400041025     C* Reperisco la provincia dal CAP e dalla località
067500041025     C                   IF        VABCAD <> *blanks AND
067600050907     C                             VABPRD  = *blanks AND
067700050907     C                             VABNZD  = *blanks
067800041025     C                   CLEAR                   TISI95DS
067900041025     C                   EVAL      I95TCN = '3'
068000041025     C                   Z-ADD     datcor        I95DAT
068100041025     C                   EVAL      I95CAP = VABCAD
068200041025     C                   EVAL      I95LOC = VABLOD
068300041025     C                   CALL      'TISI95R'
068400041025     C                   PARM                    TISI95DS
068500041025     C                   EVAL      VABPRD = O95PRV
068600041025     C                   ENDIF
068700041025     C                   ENDIF
068800040630     C*
068900040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
069000040630     C                   exsr      exeBon
069100100419     C*
069200100419     C* Forzatura su campi DIVISE
069300100419     C                   IF        VABVAS = *blanks
069400100419     C                   EVAL      VABVAS = 'EUR'
069500100419     C                   ENDIF
069600100419     C                   IF        VABVCA = *blanks
069700100419     C                   EVAL      VABVCA = 'EUR'
069800100419     C                   ENDIF
069900100419     C                   IF        VABVAD = *blanks
070000100419     C                   EVAL      VABVAD = 'EUR'
070100100419     C                   ENDIF
070200110413     C*
070300110413     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
070400110413     C                   EVAL      VABCMR = 'MANIFEST: ' + VABNAS
070500110413     C                   EVAL      VABDCM = datcor
070600110413     C                   EVAL      VABDTS = datcor
070700110413     C                   EVAL      VABHMS = oracor
070800110413     C                   EVAL      VABCNT = 1
070900110224     C*
071000110224     C* Forzature varie
071100110224     C                   EVAL      VABVLB = *zeros
071200110224     C                   SELECT
071300110224     C                   WHEN      VABCCM = 0895728
071400120307     C                   EVAL      VABRMO = 'Vente Privee c/o BRT 089'
071500110224     C                   WHEN      VABCCM = 0494274
071600120307     C                   EVAL      VABRMO = 'Vente Privee c/o BRT 049'
071700110224     C                   ENDSL
071800930409     C*
071900010604     C  N31              ADD       1             §CTROK            7 0
072000010604     C   32              ADD       1             §CTRMO            7 0
072100010604     C   31              ADD       1             §CTRNO            7 0
072200110413     C  N31              WRITE     EDIVAB00
072300120612     C                   exsr      wrivgd
072400990910     C*
072500010601     C                   if        *in31 = *off and
072600010601     C                             *in32 = *off
072700990910     C                   eval      vinflg = '1'
072800990910     C                   else
072900990910     C                   eval      vinflg = '2'
073000990910     C                   endif
073100991022     C                   endif
073200990910     C                   endif
073300010601     C*
073400010601     C                   endif
073500000905     C*
073600000905     C                   else
073700000905     C                   eval      vinflg = '1'
073800000905     C                   endif
073900000905     C*
074000000905     C  N70              update    tivin000
074100991022     C*
074200991022     C  N70              ENDdo
074300010530     C
074400010530     C*
074500010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
074600010530     C                   if        cntNonEl = *zeros or
074700010530     C                             flgMulti = '0'
074800990910     C* Se non ci sono record con errori ...
074900010530     C                   if        §ctrno = 0 and
075000010601     C                             §ctrmo = 0 and
075100010530     C                             flgStato <> '2'
075200990910     C* ... restituisco esito OK.
075300990921     C                   eval      wrkesito = '0'
075400990910     C                   else
075500010601     C                   if        §ctrok > 0
075600990921     C                   eval      wrkesito = '1'
075700000710     C                   else
075800010615     C                   if        flgOk = '0'
075900010615     C                   eval      wrkesito = '2'
076000010615     C                   else
076100010615     C                   eval      wrkesito = '6'
076200010615     C                   endif
076300990910     C                   endif
076400000710     C                   endif
076500010530     C                   else
076600010530     C                   eval      wrkesito = '9'
076700010530     C                   endif
076800990910     C*
076900990914     C                   if        %open(tivin00r)
077000990908     C                   close     tivin00r
077100990914     C                   endif
077200110413     C                   if        %open(edivabwr)
077300110413     C                   close     edivabwr
077400990914     C                   endif
077500010530     C*
077600010530     C                   if        vlrpoi <> 999
077700010530     C                   eval      invfil = vlrpoi
077800010530     C                   endif
077900990910     C*
078000990920     C                   if        §ctrok > 0
078100010530     C                             and invfil > *zeros
078200000613     C                   exsr      invio
078300990920     C                   endif
078400010612     C*
078500010612     C                   if        flgGiro = '1'
078600010612     C                   exsr      endela
078700010612     C                   endif
078800990920     C*
078900910830     C                   ENDSR
079000000613     C***
079100990920
079200010530
079300010530
079400010530
079500010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
079600010530     C     repfil        BEGSR
079700010530     C*
079800010530     C                   if        invfil = *zeros and
079900010530     C                             depfil > *zeros and
080000010613     C                             (vinflg = *blanks or
080100010613     C                              vinflg = *zeros)
080200010530     C
080300010530     C                   eval      invfil = depfil
080400010530     C                   endif
080500010530     C*
080600010530     C                   if        depfil <> invfil and
080700010530     C                             invfil > *zeros
080800010530     C                   eval      flgMulti = '1'
080900010530     C                   if        vinflg = *blanks
081000010530     C                   add       1             cntNonEl
081100010530     C                   endif
081200010530     C                   endif
081300010530     C*
081400010530     C                   if        vinflg = '2'
081500010530     C                   eval      flgStato = '2'
081600010530     C                   endif
081700010530     C*
081800010530     C                   ENDSR
081900010530     C***
082000010530
082100010530
082200010530
082300010530
082400990920      /TITLE Invio dei dati al punto operativo.
082500000613     C     invio         BEGSR
082600990920     C*
082700990920     C                   reset                   dscmz
082800010530     C                   move      invfil        cmzdst
082900990920     C                   eval      cmzfld = vlrfou
083000990920     C                   eval      cmzmbd = vlrhdl
083100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
083200000710     C                   if        prmfir = *blanks
083300110413     C                   eval      cmzfla = 'EDIVAB0F'
083400110413     C                   eval      cmzmba = 'EDIVAB0F'
083500000710     C                   else
083600000710     C                   eval      cmzfla = prmfir
083700000710     C                   eval      cmzmba = prmfir
083800000710     C                   endif
083900990920     C                   eval      cmznrr = *zeros
084000990920     C                   move      §ctrok        cmznrr
084100021003     C                   eval      cmzlba = vlrfl1
084200990920     C                   call(e)   'TIS711C'
084300990920     C                   parm                    dscmz
084400990921     C                   parm      *blanks       esito
084500990920     C                   if        %error
084600990920     C                             or cmzerr = '1'
084700990921     C                             or esito  = '1'
084800000710     C                   eval      wrkesito = '3'
084900990920     C                   endif
085000990920     C*
085100000613     C                   ENDSR
085200000613     C***
085300010530
085400010530
085500010530
085600010530
085700010530      /TITLE Invio dei dati al punto operativo.
085800010530     C     opeini        BEGSR
085900010530     C*
086000010530     C* Inizializzo flag e contatori operativi
086100010530     C                   movel     '0'           flgGiro           1
086200010530     C                   movel     '0'           flgMulti          1
086300010530     C                   movel     '1'           flgStato          1
086400010615     C                   movel     '0'           flgOk             1
086500010530     C                   z-add     *zeros        cntNonEl         10 0
086600010530     C                   z-add     *zeros        depfil            3 0
086700010530     C                   z-add     *zeros        invfil            3 0
086800030407     C*
086900030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
087000030407     C                   if        %subst(vlrppt:1:1) = 'S'
087100030407     C                   seton                                        50
087200030407     C                   else
087300030407     C                   setoff                                       50
087400030407     C                   endif
087500041001     C*
087600041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
087700041001     C                   if        %subst(vlrppt:2:1) = 'S'
087800041001     C                   seton                                        51
087900041001     C                   else
088000041001     C                   setoff                                       51
088100041001     C                   endif
088200041025     C*
088300120611     C* Verifico se sui parametri del traduttore è richiesto il reperimento provincia
088400041025     C* (naturalmente solo se nn già presente)
088500041025     C                   if        %subst(vlrppt:3:1) = 'S'
088600041025     C                   seton                                        52
088700041025     C                   else
088800041025     C                   setoff                                       52
088900041025     C                   endif
089000120611     C*
089100120611     C* Verifico se sui parametri del traduttore è richiesto il feedback dati input
089200120611     C                   if        %subst(vlrppt:4:1) = 'S'
089300120611     C                   seton                                        53
089400120611     C                   else
089500120611     C                   setoff                                       53
089600120611     C                   endif
089700010530     C*
089800010530     C                   ENDSR
089900010530     C***
090000040407
090100040407
090200040407
090300040407
090400040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
090500040407     C     exeBon        BEGSR
090600040407     C*
090700040407     C* Imposto i parametri x il *pgm d "bonifica"
090800040407     C                   EVAL      ElencoChar = *blanks
090900040407     C                   EVAL      TipoElenco = *blanks
091000040407     C                   EVAL      CharSost   = *blanks
091100040629     C                   EVAL      UpperCase  = '1'
091200040407     C                   EVAL      ChkNull    = '1'
091300040407     C                   EVAL      CharNull   = *blanks
091400040629     C*
091500040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
091600040629     C                   EVAL      TxtInOut   = VABRSD
091700040629     C                   EVAL      EsitoBon   = *blanks
091800040407     C*
091900040407     C                   CALL      'XCHKCHAR'
092000040407     C                   PARM                    TxtInOut
092100040407     C                   PARM                    ElencoChar
092200040407     C                   PARM                    TipoElenco
092300040407     C                   PARM                    CharSost
092400040407     C                   PARM                    UpperCase
092500040407     C                   PARM                    ChkNull
092600040407     C                   PARM                    CharNull
092700040407     C                   PARM                    EsitoBon
092800040407     C*
092900040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
093000040407     C                   IF        EsitoBon = '1'
093100040629     C                   EVAL      VABRSD = TxtInOut
093200040407     C                   ENDIF
093300040629     C*
093400040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
093500040629     C                   EVAL      TxtInOut   = VABIND
093600040629     C                   EVAL      EsitoBon   = *blanks
093700040629     C*
093800040629     C                   CALL      'XCHKCHAR'
093900040629     C                   PARM                    TxtInOut
094000040629     C                   PARM                    ElencoChar
094100040629     C                   PARM                    TipoElenco
094200040629     C                   PARM                    CharSost
094300040629     C                   PARM                    UpperCase
094400040629     C                   PARM                    ChkNull
094500040629     C                   PARM                    CharNull
094600040629     C                   PARM                    EsitoBon
094700040629     C*
094800040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
094900040629     C                   IF        EsitoBon = '1'
095000040629     C                   EVAL      VABIND = TxtInOut
095100040629     C                   ENDIF
095200040629     C*
095300040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
095400040629     C                   EVAL      TxtInOut   = VABLOD
095500040629     C                   EVAL      EsitoBon   = *blanks
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
096900040629     C                   EVAL      VABLOD = TxtInOut
097000040629     C                   ENDIF
097100040629     C*
097200040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
097300040629     C                   EVAL      TxtInOut   = VABNOT
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
098900040629     C                   EVAL      VABNOT = TxtInOut
099000040629     C                   ENDIF
099100040629     C*
099200040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
099300040629     C                   EVAL      TxtInOut   = VABNT2
099400040629     C                   EVAL      EsitoBon   = *blanks
099500040630     C                   EVAL      UpperCase  = *blanks
099600040629     C*
099700040629     C                   CALL      'XCHKCHAR'
099800040629     C                   PARM                    TxtInOut
099900040629     C                   PARM                    ElencoChar
100000040629     C                   PARM                    TipoElenco
100100040629     C                   PARM                    CharSost
100200040629     C                   PARM                    UpperCase
100300040629     C                   PARM                    ChkNull
100400040629     C                   PARM                    CharNull
100500040629     C                   PARM                    EsitoBon
100600040629     C*
100700040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
100800040629     C                   IF        EsitoBon = '1'
100900040629     C                   EVAL      VABNT2 = TxtInOut
101000040629     C                   ENDIF
101100040407     C*
101200040407     C                   ENDSR
101300010530
101400060410
101500060410     C*----------------------------------------------------*
101600060410     C*  CONTROLLO NUMERICITA' CAMPI
101700060410     C*----------------------------------------------------*
101800060410     C     CHKNUM        BEGSR
101900060410     C*
102000060410     C                   IF        PiDecChr = *blanks
102100060410     C                   EVAL      PiDecChr = ','
102200060410     C                   ENDIF
102300060410     C*
102400060410     C                   CALL(e)   'ISNUMERIC'
102500060410     C                   PARM                    PiStr            30
102600060410     C                   PARM                    PiDecChr          1
102700060410     C                   PARM      *ZEROS        PiVal            30 9
102800060410     C                   PARM      '0'           PiInt             1
102900060410     C                   PARM      '0'           PiNum             1
103000060410     C                   IF        %error
103100060410     C                   EVAL      PiNum=*off
103200060410     C                   ENDIF
103300060410     C*
103400060410     C                   ENDSR
103500060410     C***
103600120612
103700120612
103800120612
103900120612      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
104000120612     C     wriVGD        BEGSR
104100120612     C*
104200120612     C                   clear                   tivgd000
104300120612     C                   eval      vgdDTA = fnvab00t
104400120612     C                   eval      vgdTIP = 'EB'
104500120612     C                   eval      vgdKSU = vlrKSC
104600120612     C                   eval      vgdTSC = 'WW'
104700120612     C                   eval      vgdDAT = datcor
104800120612     C                   eval      vgdPGM = 'TITV2C8R1'
104900120612     C                   write     tivgd000
105000120612     C*
105100120612     C                   ENDSR
105200120612     C*------------------------------------------------------------------------*
105300120612
105400060410
105500990910
105600000613     C     *inzsr        BEGSR
105700990910     C*
105800990910     C     *entry        plist
105900990920     C                   parm                    tivlrds
106000990921     C                   parm      wrkesito      esito
106100000724     C                   parm                    prmlit
106200000710     C                   parm                    prmfir
106300110413     C*
106400110413     C* CALCOLA LA DATA CORRENTE
106500110413     C                   time                    wn14             14 0
106600110413     C                   movel     wn14          oracor            6 0          *ORA
106700110413     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
106800110413     C                   eval      datcor = %dec(%date() : *ISO)
106900000613     C*
107000000613     C                   ENDSR
107100000613     C***
