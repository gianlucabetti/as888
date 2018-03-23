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
004500170914     D wNomeFile       s             30    INZ(*blanks)
004600010606     D*
004700040407     D TxtInOut        S           2048
004800040407     D ElencoChar      S            256
004900040407     D TipoElenco      S              1
005000040407     D CharSost        S              1
005100040407     D UpperCase       S              1
005200040407     D ChkNull         S              1
005300040407     D CharNull        S              1
005400040407     D EsitoBon        S              1
005500990908
005600000913     C                   reset                   rrnum
005700990921     C                   reset                   esito
005800990921     C                   reset                   wrkesito
005900010530     C*
006000010530     C                   exsr      opeini
006100010530     C                   exsr      rwvab
006200041025     C*
006300041025     C* Esegue lancio TISI95R solo x chiusura
006400041025     C                   CLEAR                   TISI95DS
006500041025     C                   EVAL      I95TLA = 'C'
006600041025     C                   CALL      'TISI95R'
006700041025     C                   PARM                    TISI95DS
006800010530     C*
006900010530     C                   seton                                        lr
007000010530
007100010530
007200010530
007300010530
007400010530     C*--------------------------------------------------------
007500010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
007600010530     C*--------------------------------------------------------
007700010530     C     PREELA        BEGSR
007800000724     C*
007900000724     C* SE OCCORRE SPEDIRE IN FILIALE
008000010530     C                   if        invfil <> *zeros and
008100010530     C                             flgGiro = '0'
008200010530     C*
008300010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
008400010530     C                   eval      flgGiro = '1'
008500010530     C*
008600010530     C                   endif
008700010530     C*
008800010530     C                   ENDSR
008900010530     C***
009000010530
009100010530
009200010530
009300010530
009400010530
009500010530
009600010530     C*--------------------------------------------------------
009700010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
009800010530     C*--------------------------------------------------------
009900010530     C     ENDELA        BEGSR
010000010530     C*
010100010530     C                   ENDSR
010200010530     C***
010300990908
010400010530
010500010530
010600010530
010700910830     C*--------------------------------------------------------
010800110413     C* RWVAB   LEGGE tivin00r E SCRIVE EDIVABWF              *
010900910830     C*--------------------------------------------------------
011000941229     C     RWVAB         BEGSR
011100990910     C*
011200990914     C                   if        not %open(tivin00r)
011300990908     C                   open      tivin00r
011400990914     C                   endif
011500110413     C                   if        not %open(edivabwr)
011600110413     C                   open      edivabwr
011700990914     C                   endif
011800990910     C*
011900990910     C                   clear                   §CTROK
012000990910     C                   clear                   §CTRMO
012100990910     C                   clear                   §CTRNO
012200990910     C*
012300921023     C                   DO        *HIVAL
012400990913     C*
012500990915     C                   READ      tivin00r                               70
012600010618     C*
012700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
012800010618     C                   if        vinflg = '1'
012900010618     C                   eval      flgOk = '1'
013000010618     C                   endif
013100010705     C*
013200010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
013300010705     C                   if        %subst(vindta:1:1) = '='
013400010705     C                   eval      %subst(vindta:1:1) = *blanks
013500010705     C                   endif
013600010705     C*
013700051017     C                   if        vindta  > *blanks
013800000613     C                   add       1             rrnum
013900010601     C* ...elimino eventuali segni
014000010601     C     '+':' '       xlate     vindta        vindta
014100990913     C*
014200010601     C                   if        *in70 = *off and
014300990913     C                             (vinflg = *blanks
014400010711     C                              or vinflg = '0'
014500010711     C                              or vinflg = '2')
014600990913     C*
014700110413     C                   clear                   EDIVAB00
014800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
014900010711     C                   if        vinflg = *blanks or vinflg = '0'
015000010711     C                   clear                   vinmsg
015100010711     C                   endif
015200010711     C*
015300990913     C                   eval      fnvabaar = vindta
015400991022     C*
015500991022     C                   testb     x'3F'         vhbatb                   02
015600991022     C                   if        not *in02
015700991022     C*
015800971021     C                   SETOFF                                       303132
015900971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
016000971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
016100971023     C*    OPPURE TUTTI BLANKS
016200010530     C*
016300010530     C                   TESTN                   VHBLNP               30
016400010530     C  N30              TESTN                   VHBLNP                 30
016500010530     C  N30              TESTN                   VHBLNP                   30
016600010530     C                   MOVE      VHBLNP        W001A             1
016700010530     C     *IN30         IFEQ      *OFF
016800010530     C     W001A         ORLT      '0'
016900010530     C                   SETON                                        31
017000010530     C                   eval      vinmsg = %trimr(vinmsg)
017100010530     C                             + ' ' + 'VHBLNP'
017200010530     C                   ELSE
017300010530     C                   MOVE      VHBLNP        VABLNP
017400010530     C     VABLNP        IFLE      *ZEROS
017500010530     C                   SETON                                        31
017600010530     C                   eval      vinmsg = %trimr(vinmsg)
017700010530     C                             + ' ' + 'VHBLNP'
017800010530     C                   ENDIF
017900010530     C                   ENDIF
018000010530     C*
018100010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018200010530     C                   eval      depfil = VABLNP
018300010530     C                   exsr      repfil
018400010601     C                   if        depfil = invfil
018500021014     C                   if        vlrpoi = 999
018600021010     C                   MOVE(P)   invfil        VABFGS
018700021014     C                   else
018800021014     C                   MOVE(P)   vlrpoi        VABFGS
018900021014     C                   endif
019000010530     C*
019100010530     C                   exsr      PREELA
019200010530     C*
019300971023     C                   TESTN                   VHBCCM               30
019400971023     C  N30              TESTN                   VHBCCM                 30
019500971023     C  N30              TESTN                   VHBCCM                   30
019600971021     C                   MOVE      VHBCCM        W001A             1
019700971021     C     *IN30         IFEQ      *OFF
019800971021     C     W001A         ORLT      '0'
019900971021     C                   SETON                                        31
020000010604     C                   eval      vinmsg = %trimr(vinmsg)
020100010604     C                             + ' ' + 'VHBCCM'
020200971021     C                   ELSE
020300971021     C                   MOVE      VHBCCM        VABCCM
020400971024     C     VABCCM        IFLE      *ZEROS
020500971024     C                   SETON                                        31
020600010604     C                   eval      vinmsg = %trimr(vinmsg)
020700010604     C                             + ' ' + 'VHBCCM'
020800971024     C                   ENDIF
020900971021     C                   ENDIF
021000971021     C*
021100971023     C                   TESTN                   VHBAAS               30
021200971023     C  N30              TESTN                   VHBAAS                 30
021300971023     C  N30              TESTN                   VHBAAS                   30
021400971021     C                   MOVE      VHBAAS        W001A             1
021500971021     C     *IN30         IFEQ      *OFF
021600971021     C     W001A         ORLT      '0'
021700971023     C     W001A         ANDNE     *BLANK
021800971021     C                   SETON                                        32
021900010606     C                   MOVEL     datcor        VABAAS
022000010604     C                   eval      vinmsg = %trimr(vinmsg)
022100010604     C                             + ' ' + 'VHBAAS'
022200971021     C                   ELSE
022300971021     C                   MOVE      VHBAAS        VABAAS
022400971021     C                   ENDIF
022500971021     C*
022600971023     C                   TESTN                   VHBMGS               30
022700971023     C  N30              TESTN                   VHBMGS                 30
022800971023     C  N30              TESTN                   VHBMGS                   30
022900971021     C                   MOVE      VHBMGS        W001A             1
023000971021     C     *IN30         IFEQ      *OFF
023100971021     C     W001A         ORLT      '0'
023200971023     C     W001A         ANDNE     *BLANK
023300971021     C                   SETON                                        32
023400010601     C                   MOVE      '0101'        VABMGS
023500010604     C                   eval      vinmsg = %trimr(vinmsg)
023600010604     C                             + ' ' + 'VHBMGS'
023700971021     C                   ELSE
023800971021     C                   MOVE      VHBMGS        VABMGS
023900971021     C                   ENDIF
024000971021     C*
024100971023     C                   TESTN                   VHBNRS               30
024200971023     C  N30              TESTN                   VHBNRS                 30
024300971023     C  N30              TESTN                   VHBNRS                   30
024400971021     C                   MOVE      VHBNRS        W001A             1
024500971021     C     *IN30         IFEQ      *OFF
024600971021     C     W001A         ORLT      '0'
024700971023     C     W001A         ANDNE     *BLANK
024800971021     C                   SETON                                        32
024900971021     C                   MOVE      *ZERO         VABNRS
025000010604     C                   eval      vinmsg = %trimr(vinmsg)
025100010604     C                             + ' ' + 'VHBNRS'
025200971021     C                   ELSE
025300971021     C                   MOVE      VHBNRS        VABNRS
025400971021     C                   ENDIF
025500971021     C*
025600971023     C                   TESTN                   VHBNSP               30
025700971023     C  N30              TESTN                   VHBNSP                 30
025800971023     C  N30              TESTN                   VHBNSP                   30
025900971021     C                   MOVE      VHBNSP        W001A             1
026000971021     C     *IN30         IFEQ      *OFF
026100971021     C     W001A         ORLT      '0'
026200971023     C     W001A         ANDNE     *BLANK
026300971021     C                   SETON                                        32
026400971021     C                   MOVE      *ZERO         VABNSP
026500010604     C                   eval      vinmsg = %trimr(vinmsg)
026600010604     C                             + ' ' + 'VHBNSP'
026700971021     C                   ELSE
026800971021     C                   MOVE      VHBNSP        VABNSP
026900971021     C                   ENDIF
027000971021     C*
027100971023     C                   TESTN                   VHBLNA               30
027200971023     C  N30              TESTN                   VHBLNA                 30
027300971023     C  N30              TESTN                   VHBLNA                   30
027400971021     C                   MOVE      VHBLNA        W001A             1
027500971021     C     *IN30         IFEQ      *OFF
027600971021     C     W001A         ORLT      '0'
027700971023     C     W001A         ANDNE     *BLANK
027800971021     C                   SETON                                        32
027900971021     C                   MOVE      *ZERO         VABLNA
028000010604     C                   eval      vinmsg = %trimr(vinmsg)
028100010604     C                             + ' ' + 'VHBLNA'
028200971021     C                   ELSE
028300971021     C                   MOVE      VHBLNA        VABLNA
028400971021     C                   ENDIF
028500971021     C*
028600971023     C                   TESTN                   VHBCTR               30
028700971023     C  N30              TESTN                   VHBCTR                 30
028800971023     C  N30              TESTN                   VHBCTR                   30
028900971021     C                   MOVE      VHBCTR        W001A             1
029000971021     C     *IN30         IFEQ      *OFF
029100971021     C     W001A         ORLT      '0'
029200971023     C     W001A         ANDNE     *BLANK
029300971021     C                   SETON                                        32
029400971021     C                   MOVE      *ZERO         VABCTR
029500010604     C                   eval      vinmsg = %trimr(vinmsg)
029600010604     C                             + ' ' + 'VHBCTR'
029700971021     C                   ELSE
029800971021     C                   MOVE      VHBCTR        VABCTR
029900971021     C                   ENDIF
030000941229     C*
030100971021     C                   MOVE      *ZERO         VABIAS
030200941229     C                   MOVE      VHBIAS        WASC15
030300971023     C                   TESTN                   WI15                 30
030400971023     C  N30              TESTN                   WI15                   30
030500971023     C  N30              TESTN                   WI15                     30
030600971021     C                   MOVE      WI15          W001A             1
030700971021     C     *IN30         IFEQ      *OFF
030800971021     C     W001A         ORLT      '0'
030900971023     C     W001A         ANDNE     *BLANK
031000971021     C                   SETON                                        32
031100971021     C                   MOVE      *ZERO         VABIAS
031200010604     C                   eval      vinmsg = %trimr(vinmsg)
031300010604     C                             + ' ' + 'VHBIAS'
031400971021     C                   ELSE
031500971021     C                   MOVEL     WI15          VABIAS
031600971023     C                   TESTN                   WD15                 30
031700971023     C  N30              TESTN                   WD15                   30
031800971023     C  N30              TESTN                   WD15                     30
031900971021     C                   MOVE      WD15          W001A             1
032000971021     C     *IN30         IFEQ      *OFF
032100971021     C     W001A         ORLT      '0'
032200971023     C     W001A         ANDNE     *BLANK
032300971021     C                   SETON                                        32
032400971021     C                   MOVE      *ZERO         VABIAS
032500010604     C                   eval      vinmsg = %trimr(vinmsg)
032600010604     C                             + ' ' + 'VHBIAS'
032700971021     C                   ELSE
032800971021     C                   MOVE      WD15          VABIAS
032900971021     C                   ENDIF
033000050118     C                   ENDIF
033100941229     C*
033200971023     C                   TESTN                   VHBNCL               30
033300971023     C  N30              TESTN                   VHBNCL                 30
033400971023     C  N30              TESTN                   VHBNCL                   30
033500971021     C                   MOVE      VHBNCL        W001A             1
033600971021     C     *IN30         IFEQ      *OFF
033700971021     C     W001A         ORLT      '0'
033800971021     C                   SETON                                        32
033900971021     C                   MOVE      *ZERO         VABNCL
034000010604     C                   eval      vinmsg = %trimr(vinmsg)
034100010604     C                             + ' ' + 'VHBNCL'
034200971021     C                   ELSE
034300971021     C                   MOVE      VHBNCL        VABNCL
034400971021     C                   ENDIF
034500941229     C*
034600971021     C                   MOVE      *ZERO         VABPKB
034700971021     C                   MOVE      VHBPKB        WASC09
034800010719     C                   IF        WI09 = *blanks
034900010719     C                   EVAL      WI09 = *zeros
035000010719     C                   ENDIF
035100971023     C                   TESTN                   WI09                 30
035200971023     C  N30              TESTN                   WI09                   30
035300971023     C  N30              TESTN                   WI09                     30
035400971021     C                   MOVE      WI09          W001A             1
035500971021     C     *IN30         IFEQ      *OFF
035600971021     C     W001A         ORLT      '0'
035700971021     C                   SETON                                        32
035800971021     C                   MOVE      *ZERO         VABPKB
035900010604     C                   eval      vinmsg = %trimr(vinmsg)
036000010604     C                             + ' ' + 'VHBPKB'
036100971021     C                   ELSE
036200971021     C                   MOVEL     WI09          VABPKB
036300010719     C                   IF        WD09 = *blanks
036400010719     C                   EVAL      WD09 = *zeros
036500010719     C                   ENDIF
036600971023     C                   TESTN                   WD09                 30
036700971023     C  N30              TESTN                   WD09                     30
036800971021     C                   MOVE      WD09          W001A             1
036900971021     C     *IN30         IFEQ      *OFF
037000971021     C     W001A         ORLT      '0'
037100971023     C     W001A         ANDNE     *BLANK
037200971021     C                   SETON                                        32
037300971021     C                   MOVE      *ZERO         VABPKB
037400010604     C                   eval      vinmsg = %trimr(vinmsg)
037500010604     C                             + ' ' + 'VHBPKB'
037600971021     C                   ELSE
037700971021     C                   MOVE      WD09          VABPKB
037800971021     C                   ENDIF
037900050118     C                   ENDIF
038000971021     C*
038100041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
038200041001     C                   IF        *IN51 = *OFF
038300971021     C                   MOVE      *ZERO         VABVLB
038400971021     C                   MOVE      VHBVLB        WASC07
038500010719     C                   IF        WI07 = *blanks
038600010719     C                   EVAL      WI07 = *zeros
038700010719     C                   ENDIF
038800971023     C                   TESTN                   WI07                 30
038900971023     C  N30              TESTN                   WI07                   30
039000971023     C  N30              TESTN                   WI07                     30
039100971021     C                   MOVE      WI07          W001A             1
039200971021     C     *IN30         IFEQ      *OFF
039300971021     C     W001A         ORLT      '0'
039400971023     C     W001A         ANDNE     *BLANK
039500971021     C                   SETON                                        32
039600971021     C                   MOVE      *ZERO         VABVLB
039700010604     C                   eval      vinmsg = %trimr(vinmsg)
039800010604     C                             + ' ' + 'VHBVLB'
039900971021     C                   ELSE
040000971021     C                   MOVEL     WI07          VABVLB
040100010719     C                   IF        WD07 = *blanks
040200010719     C                   EVAL      WD07 = *zeros
040300010719     C                   ENDIF
040400971023     C                   TESTN                   WD07                 30
040500971023     C  N30              TESTN                   WD07                   30
040600971023     C  N30              TESTN                   WD07                     30
040700971021     C                   MOVE      WD07          W001A             1
040800971021     C     *IN30         IFEQ      *OFF
040900971021     C     W001A         ORLT      '0'
041000971023     C     W001A         ANDNE     *BLANK
041100971021     C                   SETON                                        32
041200971021     C                   MOVE      *ZERO         VABVLB
041300010604     C                   eval      vinmsg = %trimr(vinmsg)
041400010604     C                             + ' ' + 'VHBVLB'
041500971021     C                   ELSE
041600971021     C                   MOVE      WD07          VABVLB
041700971021     C                   ENDIF
041800050118     C                   ENDIF
041900041001     C                   ELSE
042000041001     C* ...altrimenti forzo azzeramento del volume
042100041001     C                   Z-ADD     *zeros        VABVLB
042200041001     C                   ENDIF
042300941229     C*
042400971021     C                   MOVE      *ZERO         VABQFT
042500971021     C                   MOVE      VHBQFT        WASC15
042600010719     C                   IF        WI15 = *blanks
042700010719     C                   EVAL      WI15 = *zeros
042800010719     C                   ENDIF
042900971023     C                   TESTN                   WI15                 30
043000971023     C  N30              TESTN                   WI15                   30
043100971023     C  N30              TESTN                   WI15                     30
043200971021     C                   MOVE      WI15          W001A             1
043300971021     C     *IN30         IFEQ      *OFF
043400971021     C     W001A         ORLT      '0'
043500971023     C     W001A         ANDNE     *BLANK
043600971021     C                   SETON                                        32
043700971021     C                   MOVE      *ZERO         VABQFT
043800010604     C                   eval      vinmsg = %trimr(vinmsg)
043900010604     C                             + ' ' + 'VHBQFT'
044000971021     C                   ELSE
044100971021     C                   MOVEL     WI15          VABQFT
044200010719     C                   IF        WD15 = *blanks
044300010719     C                   EVAL      WD15 = *zeros
044400010719     C                   ENDIF
044500971023     C                   TESTN                   WD15                 30
044600971023     C  N30              TESTN                   WD15                   30
044700971023     C  N30              TESTN                   WD15                     30
044800971021     C                   MOVE      WD15          W001A             1
044900971021     C     *IN30         IFEQ      *OFF
045000971021     C     W001A         ORLT      '0'
045100971023     C     W001A         ANDNE     *BLANK
045200971021     C                   SETON                                        32
045300971021     C                   MOVE      *ZERO         VABQFT
045400010604     C                   eval      vinmsg = %trimr(vinmsg)
045500010604     C                             + ' ' + 'VHBQFT'
045600971021     C                   ELSE
045700971021     C                   MOVE      WD15          VABQFT
045800971021     C                   ENDIF
045900050118     C                   ENDIF
046000941229     C*
046100971021     C                   MOVE      *ZERO         VABCAS
046200060410     C* Se presente separatore decimale alla giusta posizione => forzo sempre la virgola
046300060410     C                   EVAL      wVHBCAS = VHBCAS
046400060410     C                   IF        %subst(wVHBCAS:12:1) = '.'
046500060410     C                   EVAL      %subst(wVHBCAS:12:1) = ','
046600060410     C                   ENDIF
046700060410     C*
046800060410     C                   MOVEL(P)  wVHBCAS       PiStr
046900060410     C                   EXSR      CHKNUM
047000060410     C                   IF        PiNum=*on
047100060410     C                   Z-ADD     PiVal         VABCAS
047200060410     C                   ELSE
047300060410     C                   SETON                                        32
047400060410     C                   Z-ADD     *zeros        VABCAS
047500060410     C                   EVAL      vinmsg = %trimr(vinmsg)
047600060410     C                             + ' ' + 'VHBCAS'
047700060410     C                   ENDIF
047800971021     C*
047900971023     C                   TESTN                   VHBRMN               30
048000971023     C  N30              TESTN                   VHBRMN                 30
048100971023     C  N30              TESTN                   VHBRMN                   30
048200971021     C                   MOVE      VHBRMN        W001A             1
048300971021     C     *IN30         IFEQ      *OFF
048400971021     C     W001A         ORLT      '0'
048500971021     C                   SETON                                        32
048600971021     C                   MOVE      *ZERO         VABRMN
048700010604     C                   eval      vinmsg = %trimr(vinmsg)
048800010604     C                             + ' ' + 'VHBRMN'
048900971021     C                   ELSE
049000971021     C                   MOVE      VHBRMN        VABRMN
049100971021     C                   ENDIF
049200971021     C*
049300971023     C                   TESTN                   VHBNCD               30
049400971023     C  N30              TESTN                   VHBNCD                 30
049500971023     C  N30              TESTN                   VHBNCD                   30
049600971021     C                   MOVE      VHBNCD        W001A             1
049700971021     C     *IN30         IFEQ      *OFF
049800971021     C     W001A         ORLT      '0'
049900971023     C     W001A         ANDNE     *BLANK
050000971021     C                   SETON                                        32
050100971021     C                   MOVE      *ZERO         VABNCD
050200010604     C                   eval      vinmsg = %trimr(vinmsg)
050300010604     C                             + ' ' + 'VHBNCD'
050400971021     C                   ELSE
050500971021     C                   MOVE      VHBNCD        VABNCD
050600971021     C                   ENDIF
050700971021     C*
050800971023     C                   TESTN                   VHBNCA               30
050900971023     C  N30              TESTN                   VHBNCA                 30
051000971023     C  N30              TESTN                   VHBNCA                   30
051100971021     C                   MOVE      VHBNCA        W001A             1
051200971021     C     *IN30         IFEQ      *OFF
051300971021     C     W001A         ORLT      '0'
051400971023     C     W001A         ANDNE     *BLANK
051500971021     C                   SETON                                        32
051600971021     C                   MOVE      *ZERO         VABNCA
051700010604     C                   eval      vinmsg = %trimr(vinmsg)
051800010604     C                             + ' ' + 'VHBNCA'
051900971021     C                   ELSE
052000971021     C                   MOVE      VHBNCA        VABNCA
052100971021     C                   ENDIF
052200971021     C*
052300971023     C                   TESTN                   VHBZNC               30
052400971023     C  N30              TESTN                   VHBZNC                 30
052500971023     C  N30              TESTN                   VHBZNC                   30
052600971021     C                   MOVE      VHBZNC        W001A             1
052700971021     C     *IN30         IFEQ      *OFF
052800971021     C     W001A         ORLT      '0'
052900971023     C     W001A         ANDNE     *BLANK
053000971021     C                   SETON                                        32
053100971021     C                   MOVE      *ZERO         VABZNC
053200010604     C                   eval      vinmsg = %trimr(vinmsg)
053300010604     C                             + ' ' + 'VHBZNC'
053400971021     C                   ELSE
053500971021     C                   MOVE      VHBZNC        VABZNC
053600971021     C                   ENDIF
053700971021     C*
053800971023     C                   TESTN                   VHBDCR               30
053900971023     C  N30              TESTN                   VHBDCR                 30
054000971023     C  N30              TESTN                   VHBDCR                   30
054100971021     C                   MOVE      VHBDCR        W001A             1
054200971021     C     *IN30         IFEQ      *OFF
054300971021     C     W001A         ORLT      '0'
054400971023     C     W001A         ANDNE     *BLANK
054500971021     C                   SETON                                        32
054600971021     C                   MOVE      *ZERO         VABDCR
054700010604     C                   eval      vinmsg = %trimr(vinmsg)
054800010604     C                             + ' ' + 'VHBDCR'
054900971021     C                   ELSE
055000971021     C                   MOVE      VHBDCR        VABDCR
055100971021     C                   ENDIF
055200971021     C*
055300971023     C                   TESTN                   VHBHCR               30
055400971023     C  N30              TESTN                   VHBHCR                 30
055500971023     C  N30              TESTN                   VHBHCR                   30
055600971021     C                   MOVE      VHBHCR        W001A             1
055700971021     C     *IN30         IFEQ      *OFF
055800971021     C     W001A         ORLT      '0'
055900971023     C     W001A         ANDNE     *BLANK
056000971021     C                   SETON                                        32
056100971021     C                   MOVE      *ZERO         VABHCR
056200010604     C                   eval      vinmsg = %trimr(vinmsg)
056300010604     C                             + ' ' + 'VHBHCR'
056400971021     C                   ELSE
056500971021     C                   MOVE      VHBHCR        VABHCR
056600971021     C                   ENDIF
056700941229     C*
056800971021     C                   MOVE      *ZERO         VABVMD
056900971021     C                   MOVE      VHBVMD        WASC15
057000010719     C                   IF        WI15 = *blanks
057100010719     C                   EVAL      WI15 = *zeros
057200010719     C                   ENDIF
057300971023     C                   TESTN                   WI15                 30
057400971023     C  N30              TESTN                   WI15                   30
057500971023     C  N30              TESTN                   WI15                     30
057600971021     C                   MOVE      WI15          W001A             1
057700971021     C     *IN30         IFEQ      *OFF
057800971021     C     W001A         ORLT      '0'
057900971023     C     W001A         ANDNE     *BLANK
058000971021     C                   SETON                                        32
058100971021     C                   MOVE      *ZERO         VABVMD
058200010604     C                   eval      vinmsg = %trimr(vinmsg)
058300010604     C                             + ' ' + 'VHBVMD'
058400971021     C                   ELSE
058500971021     C                   MOVEL     WI15          VABVMD
058600010719     C                   IF        WD15 = *blanks
058700010719     C                   EVAL      WD15 = *zeros
058800010719     C                   ENDIF
058900971023     C                   TESTN                   WD15                 30
059000971023     C  N30              TESTN                   WD15                   30
059100971023     C  N30              TESTN                   WD15                     30
059200971021     C                   MOVE      WD15          W001A             1
059300971021     C     *IN30         IFEQ      *OFF
059400971021     C     W001A         ORLT      '0'
059500971023     C     W001A         ANDNE     *BLANK
059600971021     C                   SETON                                        32
059700971021     C                   MOVE      *ZERO         VABVMD
059800010604     C                   eval      vinmsg = %trimr(vinmsg)
059900010604     C                             + ' ' + 'VHBVMD'
060000971021     C                   ELSE
060100971021     C                   MOVE      WD15          VABVMD
060200971021     C                   ENDIF
060300050118     C                   ENDIF
060400971021     C*
060500971023     C                   TESTN                   VHBANT               30
060600971023     C  N30              TESTN                   VHBANT                 30
060700971023     C  N30              TESTN                   VHBANT                   30
060800971021     C                   MOVE      VHBANT        W001A             1
060900971021     C     *IN30         IFEQ      *OFF
061000971021     C     W001A         ORLT      '0'
061100971023     C     W001A         ANDNE     *BLANK
061200971021     C                   SETON                                        32
061300971021     C                   MOVE      *ZERO         VABANT
061400010604     C                   eval      vinmsg = %trimr(vinmsg)
061500010604     C                             + ' ' + 'VHBANT'
061600971021     C                   ELSE
061700971021     C                   MOVE      VHBANT        VABANT
061800971021     C                   ENDIF
061900971021     C*
062000971021     C* CAMPI ALFABETICI:
062100971021     C                   MOVE      VHBATB        VABATB
062200971021     C                   MOVE      VHBCBO        VABCBO
062300971021     C                   MOVE      VHBRSD        VABRSD
062400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
062500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
062600020117     C* ==
062700971021     C                   MOVE      VHBRD2        VABRD2
062800971021     C                   MOVE      VHBIND        VABIND
062900971021     C                   MOVE      VHBCAD        VABCAD
063000971021     C                   MOVE      VHBLOD        VABLOD
063100971021     C                   MOVE      VHBPRD        VABPRD
063200971021     C                   MOVE      VHBNZD        VABNZD
063300971021     C                   MOVE      VHBGC1        VABGC1
063400971021     C                   MOVE      VHBGC2        VABGC2
063500971021     C                   MOVE      VHBTSP        VABTSP
063600971021     C                   MOVE      VHBVAS        VABVAS
063700971021     C                   MOVE      VHBNAS        VABNAS
063800971021     C                   MOVE      VHBTIC        VABTIC
063900971021     C                   MOVE      VHBVCA        VABVCA
064000971021     C                   MOVE      VHBGCA        VABGCA
064100971021     C                   MOVE      VHBRMA        VABRMA
064200971021     C                   MOVE      VHBXCO        VABXCO
064300971021     C                   MOVE      VHBNOT        VABNOT
064400971021     C                   MOVE      VHBNT2        VABNT2
064500971021     C                   MOVE      VHBCTM        VABCTM
064600971021     C                   MOVE      VHBFFD        VABFFD
064700971021     C                   MOVE      VHBTCR        VABTCR
064800971021     C                   MOVE      VHBCTS        VABCTS
064900971021     C                   MOVE      VHBFTM        VABFTM
065000971021     C                   MOVE      VHBVAD        VABVAD
065100971021     C                   MOVE      VHBGMA        VABGMA
065200971021     C                   MOVE      VHBGGA        VABGGA
065300971021     C                   MOVE      VHBGVA        VABGVA
065400971021     C                   MOVE      VHBTC1        VABTC1
065500971021     C                   MOVE      VHBTC2        VABTC2
065600971021     C                   MOVE      VHBSCL        VABSCL
065700971021     C                   MOVE      VHBRMO        VABRMO
065800971021     C                   MOVE      VHBCMO        VABCMO
065900971021     C                   MOVE      VHBNMO        VABNMO
066000971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
066100060329     C* NELLA NAZIONE MITTENTE ORIGINALE
066200060331     C                   IF        %trim(VHBNMO) < 'A'
066300060329     C                   MOVE      *BLANKS       VABNMO
066400060329     C                   ENDIF
066500050418     C*
066600050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
066700050418     C                   IF        %trim(VABNZD) = 'I'   OR
066800050418     C                             %trim(VABNZD) = 'IT'  OR
066900050418     C                             %trim(VABNZD) = 'ITA'
067000050418     C                   EVAL      VABNZD = *blanks
067100050418     C                   ENDIF
067200041025     C*
067300041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
067400041025     C                   IF        *IN52 = *ON
067500041025     C* Reperisco la provincia dal CAP e dalla località
067600041025     C                   IF        VABCAD <> *blanks AND
067700050907     C                             VABPRD  = *blanks AND
067800050907     C                             VABNZD  = *blanks
067900041025     C                   CLEAR                   TISI95DS
068000041025     C                   EVAL      I95TCN = '3'
068100041025     C                   Z-ADD     datcor        I95DAT
068200041025     C                   EVAL      I95CAP = VABCAD
068300041025     C                   EVAL      I95LOC = VABLOD
068400041025     C                   CALL      'TISI95R'
068500041025     C                   PARM                    TISI95DS
068600041025     C                   EVAL      VABPRD = O95PRV
068700041025     C                   ENDIF
068800041025     C                   ENDIF
068900040630     C*
069000040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
069100040630     C                   exsr      exeBon
069200100419     C*
069300100419     C* Forzatura su campi DIVISE
069400100419     C                   IF        VABVAS = *blanks
069500100419     C                   EVAL      VABVAS = 'EUR'
069600100419     C                   ENDIF
069700100419     C                   IF        VABVCA = *blanks
069800100419     C                   EVAL      VABVCA = 'EUR'
069900100419     C                   ENDIF
070000100419     C                   IF        VABVAD = *blanks
070100100419     C                   EVAL      VABVAD = 'EUR'
070200100419     C                   ENDIF
070300110413     C*
070400110413     C* VALORIZZO CAMPI RELATIVI AL "CMR" (EDIVAB)
070500170914     C                   EVAL      VABCMR = 'MANIFEST: '+%subst(wNomeFile:22)
070600110413     C                   EVAL      VABDCM = datcor
070700110413     C                   EVAL      VABDTS = datcor
070800110413     C                   EVAL      VABHMS = oracor
070900110413     C                   EVAL      VABCNT = 1
071000110224     C*
071100110224     C* Forzature varie
071200110224     C                   EVAL      VABVLB = *zeros
071300110224     C                   SELECT
071400110224     C                   WHEN      VABCCM = 0895728
071500120307     C                   EVAL      VABRMO = 'Vente Privee c/o BRT 089'
071600110224     C                   WHEN      VABCCM = 0494274
071700120307     C                   EVAL      VABRMO = 'Vente Privee c/o BRT 049'
071800110224     C                   ENDSL
071900930409     C*
072000010604     C  N31              ADD       1             §CTROK            7 0
072100010604     C   32              ADD       1             §CTRMO            7 0
072200010604     C   31              ADD       1             §CTRNO            7 0
072300110413     C  N31              WRITE     EDIVAB00
072400120612     C                   exsr      wrivgd
072500990910     C*
072600010601     C                   if        *in31 = *off and
072700010601     C                             *in32 = *off
072800990910     C                   eval      vinflg = '1'
072900990910     C                   else
073000990910     C                   eval      vinflg = '2'
073100990910     C                   endif
073200991022     C                   endif
073300990910     C                   endif
073400010601     C*
073500010601     C                   endif
073600000905     C*
073700000905     C                   else
073800000905     C                   eval      vinflg = '1'
073900000905     C                   endif
074000000905     C*
074100000905     C  N70              update    tivin000
074200991022     C*
074300991022     C  N70              ENDdo
074400010530     C
074500010530     C*
074600010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
074700010530     C                   if        cntNonEl = *zeros or
074800010530     C                             flgMulti = '0'
074900990910     C* Se non ci sono record con errori ...
075000010530     C                   if        §ctrno = 0 and
075100010601     C                             §ctrmo = 0 and
075200010530     C                             flgStato <> '2'
075300990910     C* ... restituisco esito OK.
075400990921     C                   eval      wrkesito = '0'
075500990910     C                   else
075600010601     C                   if        §ctrok > 0
075700990921     C                   eval      wrkesito = '1'
075800000710     C                   else
075900010615     C                   if        flgOk = '0'
076000010615     C                   eval      wrkesito = '2'
076100010615     C                   else
076200010615     C                   eval      wrkesito = '6'
076300010615     C                   endif
076400990910     C                   endif
076500000710     C                   endif
076600010530     C                   else
076700010530     C                   eval      wrkesito = '9'
076800010530     C                   endif
076900990910     C*
077000990914     C                   if        %open(tivin00r)
077100990908     C                   close     tivin00r
077200990914     C                   endif
077300110413     C                   if        %open(edivabwr)
077400110413     C                   close     edivabwr
077500990914     C                   endif
077600010530     C*
077700010530     C                   if        vlrpoi <> 999
077800010530     C                   eval      invfil = vlrpoi
077900010530     C                   endif
078000990910     C*
078100990920     C                   if        §ctrok > 0
078200010530     C                             and invfil > *zeros
078300000613     C                   exsr      invio
078400990920     C                   endif
078500010612     C*
078600010612     C                   if        flgGiro = '1'
078700010612     C                   exsr      endela
078800010612     C                   endif
078900990920     C*
079000910830     C                   ENDSR
079100000613     C***
079200990920
079300010530
079400010530
079500010530
079600010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
079700010530     C     repfil        BEGSR
079800010530     C*
079900010530     C                   if        invfil = *zeros and
080000010530     C                             depfil > *zeros and
080100010613     C                             (vinflg = *blanks or
080200010613     C                              vinflg = *zeros)
080300010530     C
080400010530     C                   eval      invfil = depfil
080500010530     C                   endif
080600010530     C*
080700010530     C                   if        depfil <> invfil and
080800010530     C                             invfil > *zeros
080900010530     C                   eval      flgMulti = '1'
081000010530     C                   if        vinflg = *blanks
081100010530     C                   add       1             cntNonEl
081200010530     C                   endif
081300010530     C                   endif
081400010530     C*
081500010530     C                   if        vinflg = '2'
081600010530     C                   eval      flgStato = '2'
081700010530     C                   endif
081800010530     C*
081900010530     C                   ENDSR
082000010530     C***
082100010530
082200010530
082300010530
082400010530
082500990920      /TITLE Invio dei dati al punto operativo.
082600000613     C     invio         BEGSR
082700990920     C*
082800990920     C                   reset                   dscmz
082900010530     C                   move      invfil        cmzdst
083000990920     C                   eval      cmzfld = vlrfou
083100990920     C                   eval      cmzmbd = vlrhdl
083200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
083300000710     C                   if        prmfir = *blanks
083400110413     C                   eval      cmzfla = 'EDIVAB0F'
083500110413     C                   eval      cmzmba = 'EDIVAB0F'
083600000710     C                   else
083700000710     C                   eval      cmzfla = prmfir
083800000710     C                   eval      cmzmba = prmfir
083900000710     C                   endif
084000990920     C                   eval      cmznrr = *zeros
084100990920     C                   move      §ctrok        cmznrr
084200021003     C                   eval      cmzlba = vlrfl1
084300990920     C                   call(e)   'TIS711C'
084400990920     C                   parm                    dscmz
084500990921     C                   parm      *blanks       esito
084600990920     C                   if        %error
084700990920     C                             or cmzerr = '1'
084800990921     C                             or esito  = '1'
084900000710     C                   eval      wrkesito = '3'
085000990920     C                   endif
085100990920     C*
085200000613     C                   ENDSR
085300000613     C***
085400010530
085500010530
085600010530
085700010530
085800010530      /TITLE Invio dei dati al punto operativo.
085900010530     C     opeini        BEGSR
086000010530     C*
086100010530     C* Inizializzo flag e contatori operativi
086200010530     C                   movel     '0'           flgGiro           1
086300010530     C                   movel     '0'           flgMulti          1
086400010530     C                   movel     '1'           flgStato          1
086500010615     C                   movel     '0'           flgOk             1
086600010530     C                   z-add     *zeros        cntNonEl         10 0
086700010530     C                   z-add     *zeros        depfil            3 0
086800010530     C                   z-add     *zeros        invfil            3 0
086900030407     C*
087000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
087100030407     C                   if        %subst(vlrppt:1:1) = 'S'
087200030407     C                   seton                                        50
087300030407     C                   else
087400030407     C                   setoff                                       50
087500030407     C                   endif
087600041001     C*
087700041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
087800041001     C                   if        %subst(vlrppt:2:1) = 'S'
087900041001     C                   seton                                        51
088000041001     C                   else
088100041001     C                   setoff                                       51
088200041001     C                   endif
088300041025     C*
088400120611     C* Verifico se sui parametri del traduttore è richiesto il reperimento provincia
088500041025     C* (naturalmente solo se nn già presente)
088600041025     C                   if        %subst(vlrppt:3:1) = 'S'
088700041025     C                   seton                                        52
088800041025     C                   else
088900041025     C                   setoff                                       52
089000041025     C                   endif
089100120611     C*
089200120611     C* Verifico se sui parametri del traduttore è richiesto il feedback dati input
089300120611     C                   if        %subst(vlrppt:4:1) = 'S'
089400120611     C                   seton                                        53
089500120611     C                   else
089600120611     C                   setoff                                       53
089700120611     C                   endif
089800010530     C*
089900010530     C                   ENDSR
090000010530     C***
090100040407
090200040407
090300040407
090400040407
090500040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
090600040407     C     exeBon        BEGSR
090700040407     C*
090800040407     C* Imposto i parametri x il *pgm d "bonifica"
090900040407     C                   EVAL      ElencoChar = *blanks
091000040407     C                   EVAL      TipoElenco = *blanks
091100040407     C                   EVAL      CharSost   = *blanks
091200040629     C                   EVAL      UpperCase  = '1'
091300040407     C                   EVAL      ChkNull    = '1'
091400040407     C                   EVAL      CharNull   = *blanks
091500040629     C*
091600040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
091700040629     C                   EVAL      TxtInOut   = VABRSD
091800040629     C                   EVAL      EsitoBon   = *blanks
091900040407     C*
092000040407     C                   CALL      'XCHKCHAR'
092100040407     C                   PARM                    TxtInOut
092200040407     C                   PARM                    ElencoChar
092300040407     C                   PARM                    TipoElenco
092400040407     C                   PARM                    CharSost
092500040407     C                   PARM                    UpperCase
092600040407     C                   PARM                    ChkNull
092700040407     C                   PARM                    CharNull
092800040407     C                   PARM                    EsitoBon
092900040407     C*
093000040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
093100040407     C                   IF        EsitoBon = '1'
093200040629     C                   EVAL      VABRSD = TxtInOut
093300040407     C                   ENDIF
093400040629     C*
093500040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
093600040629     C                   EVAL      TxtInOut   = VABIND
093700040629     C                   EVAL      EsitoBon   = *blanks
093800040629     C*
093900040629     C                   CALL      'XCHKCHAR'
094000040629     C                   PARM                    TxtInOut
094100040629     C                   PARM                    ElencoChar
094200040629     C                   PARM                    TipoElenco
094300040629     C                   PARM                    CharSost
094400040629     C                   PARM                    UpperCase
094500040629     C                   PARM                    ChkNull
094600040629     C                   PARM                    CharNull
094700040629     C                   PARM                    EsitoBon
094800040629     C*
094900040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
095000040629     C                   IF        EsitoBon = '1'
095100040629     C                   EVAL      VABIND = TxtInOut
095200040629     C                   ENDIF
095300040629     C*
095400040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
095500040629     C                   EVAL      TxtInOut   = VABLOD
095600040629     C                   EVAL      EsitoBon   = *blanks
095700040629     C*
095800040629     C                   CALL      'XCHKCHAR'
095900040629     C                   PARM                    TxtInOut
096000040629     C                   PARM                    ElencoChar
096100040629     C                   PARM                    TipoElenco
096200040629     C                   PARM                    CharSost
096300040629     C                   PARM                    UpperCase
096400040629     C                   PARM                    ChkNull
096500040629     C                   PARM                    CharNull
096600040629     C                   PARM                    EsitoBon
096700040629     C*
096800040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
096900040629     C                   IF        EsitoBon = '1'
097000040629     C                   EVAL      VABLOD = TxtInOut
097100040629     C                   ENDIF
097200040629     C*
097300040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
097400040629     C                   EVAL      TxtInOut   = VABNOT
097500040629     C                   EVAL      EsitoBon   = *blanks
097600040630     C                   EVAL      UpperCase  = *blanks
097700040629     C*
097800040629     C                   CALL      'XCHKCHAR'
097900040629     C                   PARM                    TxtInOut
098000040629     C                   PARM                    ElencoChar
098100040629     C                   PARM                    TipoElenco
098200040629     C                   PARM                    CharSost
098300040629     C                   PARM                    UpperCase
098400040629     C                   PARM                    ChkNull
098500040629     C                   PARM                    CharNull
098600040629     C                   PARM                    EsitoBon
098700040629     C*
098800040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
098900040629     C                   IF        EsitoBon = '1'
099000040629     C                   EVAL      VABNOT = TxtInOut
099100040629     C                   ENDIF
099200040629     C*
099300040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
099400040629     C                   EVAL      TxtInOut   = VABNT2
099500040629     C                   EVAL      EsitoBon   = *blanks
099600040630     C                   EVAL      UpperCase  = *blanks
099700040629     C*
099800040629     C                   CALL      'XCHKCHAR'
099900040629     C                   PARM                    TxtInOut
100000040629     C                   PARM                    ElencoChar
100100040629     C                   PARM                    TipoElenco
100200040629     C                   PARM                    CharSost
100300040629     C                   PARM                    UpperCase
100400040629     C                   PARM                    ChkNull
100500040629     C                   PARM                    CharNull
100600040629     C                   PARM                    EsitoBon
100700040629     C*
100800040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
100900040629     C                   IF        EsitoBon = '1'
101000040629     C                   EVAL      VABNT2 = TxtInOut
101100040629     C                   ENDIF
101200040407     C*
101300040407     C                   ENDSR
101400010530
101500060410
101600060410     C*----------------------------------------------------*
101700060410     C*  CONTROLLO NUMERICITA' CAMPI
101800060410     C*----------------------------------------------------*
101900060410     C     CHKNUM        BEGSR
102000060410     C*
102100060410     C                   IF        PiDecChr = *blanks
102200060410     C                   EVAL      PiDecChr = ','
102300060410     C                   ENDIF
102400060410     C*
102500060410     C                   CALL(e)   'ISNUMERIC'
102600060410     C                   PARM                    PiStr            30
102700060410     C                   PARM                    PiDecChr          1
102800060410     C                   PARM      *ZEROS        PiVal            30 9
102900060410     C                   PARM      '0'           PiInt             1
103000060410     C                   PARM      '0'           PiNum             1
103100060410     C                   IF        %error
103200060410     C                   EVAL      PiNum=*off
103300060410     C                   ENDIF
103400060410     C*
103500060410     C                   ENDSR
103600060410     C***
103700120612
103800120612
103900120612
104000120612      /TITLE Scrittura record FNVAP00F in file TIVGD00F (file VAS generico download)
104100120612     C     wriVGD        BEGSR
104200120612     C*
104300120612     C                   clear                   tivgd000
104400120612     C                   eval      vgdDTA = fnvab00t
104500120612     C                   eval      vgdTIP = 'EB'
104600120612     C                   eval      vgdKSU = vlrKSC
104700120612     C                   eval      vgdTSC = 'WW'
104800120612     C                   eval      vgdDAT = datcor
104900120612     C                   eval      vgdPGM = 'TITV2C8R1'
105000120612     C                   write     tivgd000
105100120612     C*
105200120612     C                   ENDSR
105300120612     C*------------------------------------------------------------------------*
105400120612
105500060410
105600990910
105700000613     C     *inzsr        BEGSR
105800990910     C*
105900990910     C     *entry        plist
106000990920     C                   parm                    tivlrds
106100990921     C                   parm      wrkesito      esito
106200000724     C                   parm                    prmlit
106300000710     C                   parm                    prmfir
106400170914     C*
106500170914     C* VALORIZZO IL VALORE DEL CMR PER IL FILE CORRENTE
106600170914     C                   MOVE(P)   vlrMSG        wNomeFile
106700110413     C*
106800110413     C* CALCOLA LA DATA CORRENTE
106900110413     C                   time                    wn14             14 0
107000110413     C                   movel     wn14          oracor            6 0          *ORA
107100110413     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
107200110413     C                   eval      datcor = %dec(%date() : *ISO)
107300000613     C*
107400000613     C                   ENDSR
107500000613     C***
