000100020913      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200121217     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP(*caller)
000300990908
000400170512     Ftntbe01l  if   E           K DISK
000500990910     Ftivin00r  uF   E             DISK    usropn
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
004200060410     D wVHBCAS         s                   LIKE(VHBCAS) INZ(*blanks)
004300100301     D*------------------
004400100301     D* DS FIVAB DA EASYWEB
004500100301     D*------------------
004600100301     D ds_easyweb      ds
004700100301     D   ds_vab                            like(fnvabaar)
004800100301     D   vhbfgs                            like(vhblnp)
004900010606     D*
005000040407     D TxtInOut        S           2048
005100040407     D ElencoChar      S            256
005200040407     D TipoElenco      S              1
005300040407     D CharSost        S              1
005400040407     D UpperCase       S              1
005500040407     D ChkNull         S              1
005600040407     D CharNull        S              1
005700040407     D EsitoBon        S              1
005800990908
005900000913     C                   reset                   rrnum
006000990921     C                   reset                   esito
006100990921     C                   reset                   wrkesito
006200010530     C*
006300010530     C                   exsr      opeini
006400010530     C                   exsr      rwvab
006500041025     C*
006600041025     C* Esegue lancio TISI95R solo x chiusura
006700041025     C                   CLEAR                   TISI95DS
006800041025     C                   EVAL      I95TLA = 'C'
006900041025     C                   CALL      'TISI95R'
007000041025     C                   PARM                    TISI95DS
007100010530     C*
007200010530     C                   seton                                        lr
007300010530
007400010530
007500010530
007600910830     C*--------------------------------------------------------
007700020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
007800910830     C*--------------------------------------------------------
007900941229     C     RWVAB         BEGSR
008000990910     C*
008100990914     C                   if        not %open(tivin00r)
008200990908     C                   open      tivin00r
008300990914     C                   endif
008400020913     C                   if        not %open(fivabwwr)
008500020913     C                   open      fivabwwr
008600990914     C                   endif
008700990910     C*
008800990910     C                   clear                   §CTROK
008900990910     C                   clear                   §CTRMO
009000990910     C                   clear                   §CTRNO
009100990910     C*
009200921023     C                   DO        *HIVAL
009300990913     C*
009400990915     C                   READ      tivin00r                               70
009500010618     C*
009600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
009700010618     C                   if        vinflg = '1'
009800010618     C                   eval      flgOk = '1'
009900010618     C                   endif
010000010705     C*
010100010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
010200010705     C                   if        %subst(vindta:1:1) = '='
010300010705     C                   eval      %subst(vindta:1:1) = *blanks
010400010705     C                   endif
010500010705     C*
010600051017     C                   if        vindta  > *blanks
010700000613     C                   add       1             rrnum
010800010601     C* ...elimino eventuali segni
010900010601     C     '+':' '       xlate     vindta        vindta
011000990913     C*
011100010601     C                   if        *in70 = *off and
011200990913     C                             (vinflg = *blanks
011300010711     C                              or vinflg = '0'
011400010711     C                              or vinflg = '2')
011500990913     C*
011600020913     C                   clear                   FIVAB000
011700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
011800010711     C                   if        vinflg = *blanks or vinflg = '0'
011900010711     C                   clear                   vinmsg
012000010711     C                   endif
012100010711     C*
012200100301     C                   eval      ds_easyweb = vindta
012300100301     C                   eval      fnvabaar   = ds_vab
012400991022     C*
012500991022     C                   testb     x'3F'         vhbatb                   02
012600991022     C                   if        not *in02
012700991022     C*
012800971021     C                   SETOFF                                       303132
012900971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
013000971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
013100971023     C*    OPPURE TUTTI BLANKS
013200010530     C*
013300010530     C                   TESTN                   VHBLNP               30
013400010530     C  N30              TESTN                   VHBLNP                 30
013500010530     C  N30              TESTN                   VHBLNP                   30
013600010530     C                   MOVE      VHBLNP        W001A             1
013700010530     C     *IN30         IFEQ      *OFF
013800010530     C     W001A         ORLT      '0'
013900010530     C                   SETON                                        31
014000010530     C                   eval      vinmsg = %trimr(vinmsg)
014100010530     C                             + ' ' + 'VHBLNP'
014200010530     C                   ELSE
014300010530     C                   MOVE      VHBLNP        VABLNP
014400010530     C     VABLNP        IFLE      *ZEROS
014500010530     C                   SETON                                        31
014600010530     C                   eval      vinmsg = %trimr(vinmsg)
014700010530     C                             + ' ' + 'VHBLNP'
014800010530     C                   ENDIF
014900010530     C                   ENDIF
015000100301     C*
015100100301     C                   TESTN                   VHBFGS               30
015200100301     C  N30              TESTN                   VHBFGS                 30
015300100301     C  N30              TESTN                   VHBFGS                   30
015400100301     C                   MOVE      VHBFGS        W001A             1
015500100301     C     *IN30         IFEQ      *OFF
015600100301     C     W001A         ORLT      '0'
015700100301     C                   SETON                                        31
015800100301     C                   eval      vinmsg = %trimr(vinmsg)
015900100301     C                             + ' ' + 'VHBFGS'
016000100301     C                   ELSE
016100100301     C                   MOVE      VHBFGS        VABFGS
016200100301     C     VABFGS        IFLE      *ZEROS
016300100301     C                   SETON                                        31
016400100301     C                   eval      vinmsg = %trimr(vinmsg)
016500100301     C                             + ' ' + 'VHBFGS'
016600100301     C                   ENDIF
016700100301     C                   ENDIF
016800010530     C*
016900971023     C                   TESTN                   VHBCCM               30
017000971023     C  N30              TESTN                   VHBCCM                 30
017100971023     C  N30              TESTN                   VHBCCM                   30
017200971021     C                   MOVE      VHBCCM        W001A             1
017300971021     C     *IN30         IFEQ      *OFF
017400971021     C     W001A         ORLT      '0'
017500971021     C                   SETON                                        31
017600010604     C                   eval      vinmsg = %trimr(vinmsg)
017700010604     C                             + ' ' + 'VHBCCM'
017800971021     C                   ELSE
017900971021     C                   MOVE      VHBCCM        VABCCM
018000971024     C     VABCCM        IFLE      *ZEROS
018100971024     C                   SETON                                        31
018200010604     C                   eval      vinmsg = %trimr(vinmsg)
018300010604     C                             + ' ' + 'VHBCCM'
018400971024     C                   ENDIF
018500971021     C                   ENDIF
018600170504     C*
018700170504     C* Effettuo considerazioni x elaborazioni "multi-filiale"
018800170504     C                   if        vlrpoi = 999
018900170817     C                   if        VABFGS = 999
019000170504     C                   movel     VABCCM        VABFGS
019100170504     C                   endif
019200170504     C                   eval      invfil = VABFGS
019300170504     C                   else
019400170504     C                   eval      VABFGS = vlrpoi
019500170504     C                   eval      invfil = vlrpoi
019600170504     C                   endif
019700971021     C*
019800971023     C                   TESTN                   VHBAAS               30
019900971023     C  N30              TESTN                   VHBAAS                 30
020000971023     C  N30              TESTN                   VHBAAS                   30
020100971021     C                   MOVE      VHBAAS        W001A             1
020200971021     C     *IN30         IFEQ      *OFF
020300971021     C     W001A         ORLT      '0'
020400971023     C     W001A         ANDNE     *BLANK
020500971021     C                   SETON                                        32
020600010606     C                   MOVEL     datcor        VABAAS
020700010604     C                   eval      vinmsg = %trimr(vinmsg)
020800010604     C                             + ' ' + 'VHBAAS'
020900971021     C                   ELSE
021000971021     C                   MOVE      VHBAAS        VABAAS
021100971021     C                   ENDIF
021200971021     C*
021300971023     C                   TESTN                   VHBMGS               30
021400971023     C  N30              TESTN                   VHBMGS                 30
021500971023     C  N30              TESTN                   VHBMGS                   30
021600971021     C                   MOVE      VHBMGS        W001A             1
021700971021     C     *IN30         IFEQ      *OFF
021800971021     C     W001A         ORLT      '0'
021900971023     C     W001A         ANDNE     *BLANK
022000971021     C                   SETON                                        32
022100010601     C                   MOVE      '0101'        VABMGS
022200010604     C                   eval      vinmsg = %trimr(vinmsg)
022300010604     C                             + ' ' + 'VHBMGS'
022400971021     C                   ELSE
022500971021     C                   MOVE      VHBMGS        VABMGS
022600971021     C                   ENDIF
022700971021     C*
022800971023     C                   TESTN                   VHBNRS               30
022900971023     C  N30              TESTN                   VHBNRS                 30
023000971023     C  N30              TESTN                   VHBNRS                   30
023100971021     C                   MOVE      VHBNRS        W001A             1
023200971021     C     *IN30         IFEQ      *OFF
023300971021     C     W001A         ORLT      '0'
023400971023     C     W001A         ANDNE     *BLANK
023500971021     C                   SETON                                        32
023600971021     C                   MOVE      *ZERO         VABNRS
023700010604     C                   eval      vinmsg = %trimr(vinmsg)
023800010604     C                             + ' ' + 'VHBNRS'
023900971021     C                   ELSE
024000971021     C                   MOVE      VHBNRS        VABNRS
024100971021     C                   ENDIF
024200971021     C*
024300971023     C                   TESTN                   VHBNSP               30
024400971023     C  N30              TESTN                   VHBNSP                 30
024500971023     C  N30              TESTN                   VHBNSP                   30
024600971021     C                   MOVE      VHBNSP        W001A             1
024700971021     C     *IN30         IFEQ      *OFF
024800971021     C     W001A         ORLT      '0'
024900971023     C     W001A         ANDNE     *BLANK
025000971021     C                   SETON                                        32
025100971021     C                   MOVE      *ZERO         VABNSP
025200010604     C                   eval      vinmsg = %trimr(vinmsg)
025300010604     C                             + ' ' + 'VHBNSP'
025400971021     C                   ELSE
025500971021     C                   MOVE      VHBNSP        VABNSP
025600971021     C                   ENDIF
025700971021     C*
025800971023     C                   TESTN                   VHBLNA               30
025900971023     C  N30              TESTN                   VHBLNA                 30
026000971023     C  N30              TESTN                   VHBLNA                   30
026100971021     C                   MOVE      VHBLNA        W001A             1
026200971021     C     *IN30         IFEQ      *OFF
026300971021     C     W001A         ORLT      '0'
026400971023     C     W001A         ANDNE     *BLANK
026500971021     C                   SETON                                        32
026600971021     C                   MOVE      *ZERO         VABLNA
026700010604     C                   eval      vinmsg = %trimr(vinmsg)
026800010604     C                             + ' ' + 'VHBLNA'
026900971021     C                   ELSE
027000971021     C                   MOVE      VHBLNA        VABLNA
027100971021     C                   ENDIF
027200971021     C*
027300971023     C                   TESTN                   VHBCTR               30
027400971023     C  N30              TESTN                   VHBCTR                 30
027500971023     C  N30              TESTN                   VHBCTR                   30
027600971021     C                   MOVE      VHBCTR        W001A             1
027700971021     C     *IN30         IFEQ      *OFF
027800971021     C     W001A         ORLT      '0'
027900971023     C     W001A         ANDNE     *BLANK
028000971021     C                   SETON                                        32
028100971021     C                   MOVE      *ZERO         VABCTR
028200010604     C                   eval      vinmsg = %trimr(vinmsg)
028300010604     C                             + ' ' + 'VHBCTR'
028400971021     C                   ELSE
028500971021     C                   MOVE      VHBCTR        VABCTR
028600971021     C                   ENDIF
028700941229     C*
028800971021     C                   MOVE      *ZERO         VABIAS
028900941229     C                   MOVE      VHBIAS        WASC15
029000971023     C                   TESTN                   WI15                 30
029100971023     C  N30              TESTN                   WI15                   30
029200971023     C  N30              TESTN                   WI15                     30
029300971021     C                   MOVE      WI15          W001A             1
029400971021     C     *IN30         IFEQ      *OFF
029500971021     C     W001A         ORLT      '0'
029600971023     C     W001A         ANDNE     *BLANK
029700971021     C                   SETON                                        32
029800971021     C                   MOVE      *ZERO         VABIAS
029900010604     C                   eval      vinmsg = %trimr(vinmsg)
030000010604     C                             + ' ' + 'VHBIAS'
030100971021     C                   ELSE
030200971021     C                   MOVEL     WI15          VABIAS
030300971023     C                   TESTN                   WD15                 30
030400971023     C  N30              TESTN                   WD15                   30
030500971023     C  N30              TESTN                   WD15                     30
030600971021     C                   MOVE      WD15          W001A             1
030700971021     C     *IN30         IFEQ      *OFF
030800971021     C     W001A         ORLT      '0'
030900971023     C     W001A         ANDNE     *BLANK
031000971021     C                   SETON                                        32
031100971021     C                   MOVE      *ZERO         VABIAS
031200010604     C                   eval      vinmsg = %trimr(vinmsg)
031300010604     C                             + ' ' + 'VHBIAS'
031400971021     C                   ELSE
031500971021     C                   MOVE      WD15          VABIAS
031600971021     C                   ENDIF
031700050118     C                   ENDIF
031800941229     C*
031900971023     C                   TESTN                   VHBNCL               30
032000971023     C  N30              TESTN                   VHBNCL                 30
032100971023     C  N30              TESTN                   VHBNCL                   30
032200971021     C                   MOVE      VHBNCL        W001A             1
032300971021     C     *IN30         IFEQ      *OFF
032400971021     C     W001A         ORLT      '0'
032500971021     C                   SETON                                        32
032600971021     C                   MOVE      *ZERO         VABNCL
032700010604     C                   eval      vinmsg = %trimr(vinmsg)
032800010604     C                             + ' ' + 'VHBNCL'
032900971021     C                   ELSE
033000971021     C                   MOVE      VHBNCL        VABNCL
033100971021     C                   ENDIF
033200941229     C*
033300971021     C                   MOVE      *ZERO         VABPKB
033400971021     C                   MOVE      VHBPKB        WASC09
033500010719     C                   IF        WI09 = *blanks
033600010719     C                   EVAL      WI09 = *zeros
033700010719     C                   ENDIF
033800971023     C                   TESTN                   WI09                 30
033900971023     C  N30              TESTN                   WI09                   30
034000971023     C  N30              TESTN                   WI09                     30
034100971021     C                   MOVE      WI09          W001A             1
034200971021     C     *IN30         IFEQ      *OFF
034300971021     C     W001A         ORLT      '0'
034400971021     C                   SETON                                        32
034500971021     C                   MOVE      *ZERO         VABPKB
034600010604     C                   eval      vinmsg = %trimr(vinmsg)
034700010604     C                             + ' ' + 'VHBPKB'
034800971021     C                   ELSE
034900971021     C                   MOVEL     WI09          VABPKB
035000010719     C                   IF        WD09 = *blanks
035100010719     C                   EVAL      WD09 = *zeros
035200010719     C                   ENDIF
035300971023     C                   TESTN                   WD09                 30
035400971023     C  N30              TESTN                   WD09                     30
035500971021     C                   MOVE      WD09          W001A             1
035600971021     C     *IN30         IFEQ      *OFF
035700971021     C     W001A         ORLT      '0'
035800971023     C     W001A         ANDNE     *BLANK
035900971021     C                   SETON                                        32
036000971021     C                   MOVE      *ZERO         VABPKB
036100010604     C                   eval      vinmsg = %trimr(vinmsg)
036200010604     C                             + ' ' + 'VHBPKB'
036300971021     C                   ELSE
036400971021     C                   MOVE      WD09          VABPKB
036500971021     C                   ENDIF
036600050118     C                   ENDIF
036700971021     C*
036800041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
036900041001     C                   IF        *IN51 = *OFF
037000971021     C                   MOVE      *ZERO         VABVLB
037100971021     C                   MOVE      VHBVLB        WASC07
037200010719     C                   IF        WI07 = *blanks
037300010719     C                   EVAL      WI07 = *zeros
037400010719     C                   ENDIF
037500971023     C                   TESTN                   WI07                 30
037600971023     C  N30              TESTN                   WI07                   30
037700971023     C  N30              TESTN                   WI07                     30
037800971021     C                   MOVE      WI07          W001A             1
037900971021     C     *IN30         IFEQ      *OFF
038000971021     C     W001A         ORLT      '0'
038100971023     C     W001A         ANDNE     *BLANK
038200971021     C                   SETON                                        32
038300971021     C                   MOVE      *ZERO         VABVLB
038400010604     C                   eval      vinmsg = %trimr(vinmsg)
038500010604     C                             + ' ' + 'VHBVLB'
038600971021     C                   ELSE
038700971021     C                   MOVEL     WI07          VABVLB
038800010719     C                   IF        WD07 = *blanks
038900010719     C                   EVAL      WD07 = *zeros
039000010719     C                   ENDIF
039100971023     C                   TESTN                   WD07                 30
039200971023     C  N30              TESTN                   WD07                   30
039300971023     C  N30              TESTN                   WD07                     30
039400971021     C                   MOVE      WD07          W001A             1
039500971021     C     *IN30         IFEQ      *OFF
039600971021     C     W001A         ORLT      '0'
039700971023     C     W001A         ANDNE     *BLANK
039800971021     C                   SETON                                        32
039900971021     C                   MOVE      *ZERO         VABVLB
040000010604     C                   eval      vinmsg = %trimr(vinmsg)
040100010604     C                             + ' ' + 'VHBVLB'
040200971021     C                   ELSE
040300971021     C                   MOVE      WD07          VABVLB
040400971021     C                   ENDIF
040500050118     C                   ENDIF
040600041001     C                   ELSE
040700041001     C* ...altrimenti forzo azzeramento del volume
040800041001     C                   Z-ADD     *zeros        VABVLB
040900041001     C                   ENDIF
041000941229     C*
041100971021     C                   MOVE      *ZERO         VABQFT
041200971021     C                   MOVE      VHBQFT        WASC15
041300010719     C                   IF        WI15 = *blanks
041400010719     C                   EVAL      WI15 = *zeros
041500010719     C                   ENDIF
041600971023     C                   TESTN                   WI15                 30
041700971023     C  N30              TESTN                   WI15                   30
041800971023     C  N30              TESTN                   WI15                     30
041900971021     C                   MOVE      WI15          W001A             1
042000971021     C     *IN30         IFEQ      *OFF
042100971021     C     W001A         ORLT      '0'
042200971023     C     W001A         ANDNE     *BLANK
042300971021     C                   SETON                                        32
042400971021     C                   MOVE      *ZERO         VABQFT
042500010604     C                   eval      vinmsg = %trimr(vinmsg)
042600010604     C                             + ' ' + 'VHBQFT'
042700971021     C                   ELSE
042800971021     C                   MOVEL     WI15          VABQFT
042900010719     C                   IF        WD15 = *blanks
043000010719     C                   EVAL      WD15 = *zeros
043100010719     C                   ENDIF
043200971023     C                   TESTN                   WD15                 30
043300971023     C  N30              TESTN                   WD15                   30
043400971023     C  N30              TESTN                   WD15                     30
043500971021     C                   MOVE      WD15          W001A             1
043600971021     C     *IN30         IFEQ      *OFF
043700971021     C     W001A         ORLT      '0'
043800971023     C     W001A         ANDNE     *BLANK
043900971021     C                   SETON                                        32
044000971021     C                   MOVE      *ZERO         VABQFT
044100010604     C                   eval      vinmsg = %trimr(vinmsg)
044200010604     C                             + ' ' + 'VHBQFT'
044300971021     C                   ELSE
044400971021     C                   MOVE      WD15          VABQFT
044500971021     C                   ENDIF
044600050118     C                   ENDIF
044700941229     C*
044800971021     C                   MOVE      *ZERO         VABCAS
044900060410     C* Se presente separatore decimale alla giusta posizione => forzo sempre la virgola
045000060410     C                   EVAL      wVHBCAS = VHBCAS
045100060410     C                   IF        %subst(wVHBCAS:12:1) = '.'
045200060410     C                   EVAL      %subst(wVHBCAS:12:1) = ','
045300060410     C                   ENDIF
045400060410     C*
045500060410     C                   MOVEL(P)  wVHBCAS       PiStr
045600060410     C                   EXSR      CHKNUM
045700060410     C                   IF        PiNum=*on
045800060410     C                   Z-ADD     PiVal         VABCAS
045900060410     C                   ELSE
046000060410     C                   SETON                                        32
046100060410     C                   Z-ADD     *zeros        VABCAS
046200060410     C                   EVAL      vinmsg = %trimr(vinmsg)
046300060410     C                             + ' ' + 'VHBCAS'
046400060410     C                   ENDIF
046500971021     C*
046600971023     C                   TESTN                   VHBRMN               30
046700971023     C  N30              TESTN                   VHBRMN                 30
046800971023     C  N30              TESTN                   VHBRMN                   30
046900971021     C                   MOVE      VHBRMN        W001A             1
047000971021     C     *IN30         IFEQ      *OFF
047100971021     C     W001A         ORLT      '0'
047200971021     C                   SETON                                        32
047300971021     C                   MOVE      *ZERO         VABRMN
047400010604     C                   eval      vinmsg = %trimr(vinmsg)
047500010604     C                             + ' ' + 'VHBRMN'
047600971021     C                   ELSE
047700971021     C                   MOVE      VHBRMN        VABRMN
047800971021     C                   ENDIF
047900971021     C*
048000971023     C                   TESTN                   VHBNCD               30
048100971023     C  N30              TESTN                   VHBNCD                 30
048200971023     C  N30              TESTN                   VHBNCD                   30
048300971021     C                   MOVE      VHBNCD        W001A             1
048400971021     C     *IN30         IFEQ      *OFF
048500971021     C     W001A         ORLT      '0'
048600971023     C     W001A         ANDNE     *BLANK
048700971021     C                   SETON                                        32
048800971021     C                   MOVE      *ZERO         VABNCD
048900010604     C                   eval      vinmsg = %trimr(vinmsg)
049000010604     C                             + ' ' + 'VHBNCD'
049100971021     C                   ELSE
049200971021     C                   MOVE      VHBNCD        VABNCD
049300971021     C                   ENDIF
049400971021     C*
049500971023     C                   TESTN                   VHBNCA               30
049600971023     C  N30              TESTN                   VHBNCA                 30
049700971023     C  N30              TESTN                   VHBNCA                   30
049800971021     C                   MOVE      VHBNCA        W001A             1
049900971021     C     *IN30         IFEQ      *OFF
050000971021     C     W001A         ORLT      '0'
050100971023     C     W001A         ANDNE     *BLANK
050200971021     C                   SETON                                        32
050300971021     C                   MOVE      *ZERO         VABNCA
050400010604     C                   eval      vinmsg = %trimr(vinmsg)
050500010604     C                             + ' ' + 'VHBNCA'
050600971021     C                   ELSE
050700971021     C                   MOVE      VHBNCA        VABNCA
050800971021     C                   ENDIF
050900971021     C*
051000971023     C                   TESTN                   VHBZNC               30
051100971023     C  N30              TESTN                   VHBZNC                 30
051200971023     C  N30              TESTN                   VHBZNC                   30
051300971021     C                   MOVE      VHBZNC        W001A             1
051400971021     C     *IN30         IFEQ      *OFF
051500971021     C     W001A         ORLT      '0'
051600971023     C     W001A         ANDNE     *BLANK
051700971021     C                   SETON                                        32
051800971021     C                   MOVE      *ZERO         VABZNC
051900010604     C                   eval      vinmsg = %trimr(vinmsg)
052000010604     C                             + ' ' + 'VHBZNC'
052100971021     C                   ELSE
052200971021     C                   MOVE      VHBZNC        VABZNC
052300971021     C                   ENDIF
052400971021     C*
052500971023     C                   TESTN                   VHBDCR               30
052600971023     C  N30              TESTN                   VHBDCR                 30
052700971023     C  N30              TESTN                   VHBDCR                   30
052800971021     C                   MOVE      VHBDCR        W001A             1
052900971021     C     *IN30         IFEQ      *OFF
053000971021     C     W001A         ORLT      '0'
053100971023     C     W001A         ANDNE     *BLANK
053200971021     C                   SETON                                        32
053300971021     C                   MOVE      *ZERO         VABDCR
053400010604     C                   eval      vinmsg = %trimr(vinmsg)
053500010604     C                             + ' ' + 'VHBDCR'
053600971021     C                   ELSE
053700971021     C                   MOVE      VHBDCR        VABDCR
053800971021     C                   ENDIF
053900971021     C*
054000971023     C                   TESTN                   VHBHCR               30
054100971023     C  N30              TESTN                   VHBHCR                 30
054200971023     C  N30              TESTN                   VHBHCR                   30
054300971021     C                   MOVE      VHBHCR        W001A             1
054400971021     C     *IN30         IFEQ      *OFF
054500971021     C     W001A         ORLT      '0'
054600971023     C     W001A         ANDNE     *BLANK
054700971021     C                   SETON                                        32
054800971021     C                   MOVE      *ZERO         VABHCR
054900010604     C                   eval      vinmsg = %trimr(vinmsg)
055000010604     C                             + ' ' + 'VHBHCR'
055100971021     C                   ELSE
055200971021     C                   MOVE      VHBHCR        VABHCR
055300971021     C                   ENDIF
055400941229     C*
055500971021     C                   MOVE      *ZERO         VABVMD
055600971021     C                   MOVE      VHBVMD        WASC15
055700010719     C                   IF        WI15 = *blanks
055800010719     C                   EVAL      WI15 = *zeros
055900010719     C                   ENDIF
056000971023     C                   TESTN                   WI15                 30
056100971023     C  N30              TESTN                   WI15                   30
056200971023     C  N30              TESTN                   WI15                     30
056300971021     C                   MOVE      WI15          W001A             1
056400971021     C     *IN30         IFEQ      *OFF
056500971021     C     W001A         ORLT      '0'
056600971023     C     W001A         ANDNE     *BLANK
056700971021     C                   SETON                                        32
056800971021     C                   MOVE      *ZERO         VABVMD
056900010604     C                   eval      vinmsg = %trimr(vinmsg)
057000010604     C                             + ' ' + 'VHBVMD'
057100971021     C                   ELSE
057200971021     C                   MOVEL     WI15          VABVMD
057300010719     C                   IF        WD15 = *blanks
057400010719     C                   EVAL      WD15 = *zeros
057500010719     C                   ENDIF
057600971023     C                   TESTN                   WD15                 30
057700971023     C  N30              TESTN                   WD15                   30
057800971023     C  N30              TESTN                   WD15                     30
057900971021     C                   MOVE      WD15          W001A             1
058000971021     C     *IN30         IFEQ      *OFF
058100971021     C     W001A         ORLT      '0'
058200971023     C     W001A         ANDNE     *BLANK
058300971021     C                   SETON                                        32
058400971021     C                   MOVE      *ZERO         VABVMD
058500010604     C                   eval      vinmsg = %trimr(vinmsg)
058600010604     C                             + ' ' + 'VHBVMD'
058700971021     C                   ELSE
058800971021     C                   MOVE      WD15          VABVMD
058900971021     C                   ENDIF
059000050118     C                   ENDIF
059100971021     C*
059200971023     C                   TESTN                   VHBANT               30
059300971023     C  N30              TESTN                   VHBANT                 30
059400971023     C  N30              TESTN                   VHBANT                   30
059500971021     C                   MOVE      VHBANT        W001A             1
059600971021     C     *IN30         IFEQ      *OFF
059700971021     C     W001A         ORLT      '0'
059800971023     C     W001A         ANDNE     *BLANK
059900971021     C                   SETON                                        32
060000971021     C                   MOVE      *ZERO         VABANT
060100010604     C                   eval      vinmsg = %trimr(vinmsg)
060200010604     C                             + ' ' + 'VHBANT'
060300971021     C                   ELSE
060400971021     C                   MOVE      VHBANT        VABANT
060500971021     C                   ENDIF
060600971021     C*
060700971021     C* CAMPI ALFABETICI:
060800170831     C***                MOVE      VHBATB        VABATB
060900971021     C                   MOVE      VHBCBO        VABCBO
061000971021     C                   MOVE      VHBRSD        VABRSD
061100020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
061200020117     C     '@':'A'       XLATE     VABRSD        VABRSD
061300020117     C* ==
061400971021     C                   MOVE      VHBRD2        VABRD2
061500971021     C                   MOVE      VHBIND        VABIND
061600971021     C                   MOVE      VHBCAD        VABCAD
061700971021     C                   MOVE      VHBLOD        VABLOD
061800971021     C                   MOVE      VHBPRD        VABPRD
061900971021     C                   MOVE      VHBNZD        VABNZD
062000971021     C                   MOVE      VHBGC1        VABGC1
062100971021     C                   MOVE      VHBGC2        VABGC2
062200971021     C                   MOVE      VHBTSP        VABTSP
062300971021     C                   MOVE      VHBVAS        VABVAS
062400971021     C                   MOVE      VHBNAS        VABNAS
062500971021     C                   MOVE      VHBTIC        VABTIC
062600971021     C                   MOVE      VHBVCA        VABVCA
062700971021     C                   MOVE      VHBGCA        VABGCA
062800971021     C                   MOVE      VHBRMA        VABRMA
062900971021     C                   MOVE      VHBXCO        VABXCO
063000971021     C                   MOVE      VHBNOT        VABNOT
063100971021     C                   MOVE      VHBNT2        VABNT2
063200971021     C                   MOVE      VHBCTM        VABCTM
063300971021     C                   MOVE      VHBFFD        VABFFD
063400971021     C                   MOVE      VHBTCR        VABTCR
063500971021     C                   MOVE      VHBCTS        VABCTS
063600971021     C                   MOVE      VHBFTM        VABFTM
063700971021     C                   MOVE      VHBVAD        VABVAD
063800971021     C                   MOVE      VHBGMA        VABGMA
063900971021     C                   MOVE      VHBGGA        VABGGA
064000971021     C                   MOVE      VHBGVA        VABGVA
064100971021     C                   MOVE      VHBTC1        VABTC1
064200971021     C                   MOVE      VHBTC2        VABTC2
064300971021     C                   MOVE      VHBSCL        VABSCL
064400971021     C                   MOVE      VHBRMO        VABRMO
064500971021     C                   MOVE      VHBCMO        VABCMO
064600971021     C                   MOVE      VHBNMO        VABNMO
064700971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
064800060329     C* NELLA NAZIONE MITTENTE ORIGINALE
064900060331     C                   IF        %trim(VHBNMO) < 'A'
065000060329     C                   MOVE      *BLANKS       VABNMO
065100060329     C                   ENDIF
065200050418     C*
065300050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
065400050418     C                   IF        %trim(VABNZD) = 'I'   OR
065500050418     C                             %trim(VABNZD) = 'IT'  OR
065600050418     C                             %trim(VABNZD) = 'ITA'
065700050418     C                   EVAL      VABNZD = *blanks
065800050418     C                   ENDIF
065900041025     C*
066000041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
066100041025     C                   IF        *IN52 = *ON
066200041025     C* Reperisco la provincia dal CAP e dalla località
066300041025     C                   IF        VABCAD <> *blanks AND
066400050907     C                             VABPRD  = *blanks AND
066500050907     C                             VABNZD  = *blanks
066600041025     C                   CLEAR                   TISI95DS
066700041025     C                   EVAL      I95TCN = '3'
066800041025     C                   Z-ADD     datcor        I95DAT
066900041025     C                   EVAL      I95CAP = VABCAD
067000041025     C                   EVAL      I95LOC = VABLOD
067100041025     C                   CALL      'TISI95R'
067200041025     C                   PARM                    TISI95DS
067300041025     C                   EVAL      VABPRD = O95PRV
067400041025     C                   ENDIF
067500041025     C                   ENDIF
067600040630     C*
067700040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
067800040630     C                   exsr      exeBon
067900100419     C*
068000100419     C* Forzatura su campi DIVISE
068100100419     C                   IF        VABVAS = *blanks
068200100419     C                   EVAL      VABVAS = 'EUR'
068300100419     C                   ENDIF
068400100419     C                   IF        VABVCA = *blanks
068500100419     C                   EVAL      VABVCA = 'EUR'
068600100419     C                   ENDIF
068700100419     C                   IF        VABVAD = *blanks
068800100419     C                   EVAL      VABVAD = 'EUR'
068900100419     C                   ENDIF
069000930409     C*
069100010604     C  N31              ADD       1             §CTROK            7 0
069200010604     C   32              ADD       1             §CTRMO            7 0
069300010604     C   31              ADD       1             §CTRNO            7 0
069400110418     C*
069500110418     C* Verifico se sui parametri del traduttore è richiesta l'esclusione del 1020005
069600121115     C                   IF        %subst(vlrppt:4:1) = 'S' AND
069700121115     C                             (VABCCM = 1020005 OR VABCCM = 1020004)
069800110418     C                   ELSE
069900020913     C  N31              WRITE     FIVAB000
070000110418     C                   ENDIF
070100990910     C*
070200010601     C                   if        *in31 = *off and
070300010601     C                             *in32 = *off
070400990910     C                   eval      vinflg = '1'
070500990910     C                   else
070600990910     C                   eval      vinflg = '2'
070700990910     C                   endif
070800991022     C                   endif
070900990910     C                   endif
071000000905     C*
071100000905     C                   else
071200000905     C                   eval      vinflg = '1'
071300000905     C                   endif
071400000905     C*
071500000905     C  N70              update    tivin000
071600991022     C*
071700991022     C  N70              ENDdo
071800010530     C
071900990910     C* Se non ci sono record con errori ...
072000010530     C                   if        §ctrno = 0 and
072100170504     C                             §ctrmo = 0
072200990910     C* ... restituisco esito OK.
072300990921     C                   eval      wrkesito = '0'
072400990910     C                   else
072500010601     C                   if        §ctrok > 0
072600990921     C                   eval      wrkesito = '1'
072700000710     C                   else
072800010615     C                   if        flgOk = '0'
072900010615     C                   eval      wrkesito = '2'
073000010615     C                   else
073100010615     C                   eval      wrkesito = '6'
073200010615     C                   endif
073300990910     C                   endif
073400000710     C                   endif
073500990910     C*
073600990914     C                   if        %open(tivin00r)
073700990908     C                   close     tivin00r
073800990914     C                   endif
073900020913     C                   if        %open(fivabwwr)
074000020913     C                   close     fivabwwr
074100990914     C                   endif
074200990910     C*
074300990920     C                   if        §ctrok > 0
074400010530     C                             and invfil > *zeros
074500000613     C                   exsr      invio
074600990920     C                   endif
074700990920     C*
074800910830     C                   ENDSR
074900000613     C***
075000010530
075100010530
075200010530
075300990920      /TITLE Invio dei dati al punto operativo.
075400000613     C     invio         BEGSR
075500990920     C*
075600990920     C                   reset                   dscmz
075700010530     C                   move      invfil        cmzdst
075800990920     C                   eval      cmzfld = vlrfou
075900990920     C                   eval      cmzmbd = vlrhdl
076000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
076100000710     C                   if        prmfir = *blanks
076200020913     C                   eval      cmzfla = 'FIVAB00F'
076300020913     C                   eval      cmzmba = 'FIVAB00F'
076400000710     C                   else
076500000710     C                   eval      cmzfla = prmfir
076600000710     C                   eval      cmzmba = prmfir
076700000710     C                   endif
076800990920     C                   eval      cmznrr = *zeros
076900990920     C                   move      §ctrok        cmznrr
077000021003     C                   eval      cmzlba = vlrfl1
077100990920     C                   call(e)   'TIS711C'
077200990920     C                   parm                    dscmz
077300990921     C                   parm      *blanks       esito
077400990920     C                   if        %error
077500990920     C                             or cmzerr = '1'
077600990921     C                             or esito  = '1'
077700000710     C                   eval      wrkesito = '3'
077800990920     C                   endif
077900990920     C*
078000000613     C                   ENDSR
078100000613     C***
078200010530
078300010530
078400010530
078500010530
078600010530      /TITLE Invio dei dati al punto operativo.
078700010530     C     opeini        BEGSR
078800010530     C*
078900010530     C* Inizializzo flag e contatori operativi
079000010615     C                   movel     '0'           flgOk             1
079100010530     C                   z-add     *zeros        invfil            3 0
079200030407     C*
079300030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
079400030407     C                   if        %subst(vlrppt:1:1) = 'S'
079500030407     C                   seton                                        50
079600030407     C                   else
079700030407     C                   setoff                                       50
079800030407     C                   endif
079900041001     C*
080000041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
080100041001     C                   if        %subst(vlrppt:2:1) = 'S'
080200041001     C                   seton                                        51
080300041001     C                   else
080400041001     C                   setoff                                       51
080500041001     C                   endif
080600041025     C*
080700041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
080800041025     C* (naturalmente solo se nn già presente)
080900041025     C                   if        %subst(vlrppt:3:1) = 'S'
081000041025     C                   seton                                        52
081100041025     C                   else
081200041025     C                   setoff                                       52
081300041025     C                   endif
081400010530     C*
081500010530     C                   ENDSR
081600010530     C***
081700040407
081800040407
081900040407
082000040407
082100040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
082200040407     C     exeBon        BEGSR
082300040407     C*
082400040407     C* Imposto i parametri x il *pgm d "bonifica"
082500040407     C                   EVAL      ElencoChar = *blanks
082600040407     C                   EVAL      TipoElenco = *blanks
082700040407     C                   EVAL      CharSost   = *blanks
082800040629     C                   EVAL      UpperCase  = '1'
082900040407     C                   EVAL      ChkNull    = '1'
083000040407     C                   EVAL      CharNull   = *blanks
083100040629     C*
083200040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
083300040629     C                   EVAL      TxtInOut   = VABRSD
083400040629     C                   EVAL      EsitoBon   = *blanks
083500040407     C*
083600040407     C                   CALL      'XCHKCHAR'
083700040407     C                   PARM                    TxtInOut
083800040407     C                   PARM                    ElencoChar
083900040407     C                   PARM                    TipoElenco
084000040407     C                   PARM                    CharSost
084100040407     C                   PARM                    UpperCase
084200040407     C                   PARM                    ChkNull
084300040407     C                   PARM                    CharNull
084400040407     C                   PARM                    EsitoBon
084500040407     C*
084600040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
084700040407     C                   IF        EsitoBon = '1'
084800040629     C                   EVAL      VABRSD = TxtInOut
084900040407     C                   ENDIF
085000040629     C*
085100040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
085200040629     C                   EVAL      TxtInOut   = VABIND
085300040629     C                   EVAL      EsitoBon   = *blanks
085400040629     C*
085500040629     C                   CALL      'XCHKCHAR'
085600040629     C                   PARM                    TxtInOut
085700040629     C                   PARM                    ElencoChar
085800040629     C                   PARM                    TipoElenco
085900040629     C                   PARM                    CharSost
086000040629     C                   PARM                    UpperCase
086100040629     C                   PARM                    ChkNull
086200040629     C                   PARM                    CharNull
086300040629     C                   PARM                    EsitoBon
086400040629     C*
086500040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
086600040629     C                   IF        EsitoBon = '1'
086700040629     C                   EVAL      VABIND = TxtInOut
086800040629     C                   ENDIF
086900040629     C*
087000040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
087100040629     C                   EVAL      TxtInOut   = VABLOD
087200040629     C                   EVAL      EsitoBon   = *blanks
087300040629     C*
087400040629     C                   CALL      'XCHKCHAR'
087500040629     C                   PARM                    TxtInOut
087600040629     C                   PARM                    ElencoChar
087700040629     C                   PARM                    TipoElenco
087800040629     C                   PARM                    CharSost
087900040629     C                   PARM                    UpperCase
088000040629     C                   PARM                    ChkNull
088100040629     C                   PARM                    CharNull
088200040629     C                   PARM                    EsitoBon
088300040629     C*
088400040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
088500040629     C                   IF        EsitoBon = '1'
088600040629     C                   EVAL      VABLOD = TxtInOut
088700040629     C                   ENDIF
088800040629     C*
088900040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
089000040629     C                   EVAL      TxtInOut   = VABNOT
089100040629     C                   EVAL      EsitoBon   = *blanks
089200040630     C                   EVAL      UpperCase  = *blanks
089300040629     C*
089400040629     C                   CALL      'XCHKCHAR'
089500040629     C                   PARM                    TxtInOut
089600040629     C                   PARM                    ElencoChar
089700040629     C                   PARM                    TipoElenco
089800040629     C                   PARM                    CharSost
089900040629     C                   PARM                    UpperCase
090000040629     C                   PARM                    ChkNull
090100040629     C                   PARM                    CharNull
090200040629     C                   PARM                    EsitoBon
090300040629     C*
090400040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
090500040629     C                   IF        EsitoBon = '1'
090600040629     C                   EVAL      VABNOT = TxtInOut
090700040629     C                   ENDIF
090800040629     C*
090900040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
091000040629     C                   EVAL      TxtInOut   = VABNT2
091100040629     C                   EVAL      EsitoBon   = *blanks
091200040630     C                   EVAL      UpperCase  = *blanks
091300040629     C*
091400040629     C                   CALL      'XCHKCHAR'
091500040629     C                   PARM                    TxtInOut
091600040629     C                   PARM                    ElencoChar
091700040629     C                   PARM                    TipoElenco
091800040629     C                   PARM                    CharSost
091900040629     C                   PARM                    UpperCase
092000040629     C                   PARM                    ChkNull
092100040629     C                   PARM                    CharNull
092200040629     C                   PARM                    EsitoBon
092300040629     C*
092400040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
092500040629     C                   IF        EsitoBon = '1'
092600040629     C                   EVAL      VABNT2 = TxtInOut
092700040629     C                   ENDIF
092800040407     C*
092900040407     C                   ENDSR
093000010530
093100060410
093200060410     C*----------------------------------------------------*
093300060410     C*  CONTROLLO NUMERICITA' CAMPI
093400060410     C*----------------------------------------------------*
093500060410     C     CHKNUM        BEGSR
093600060410     C*
093700060410     C                   IF        PiDecChr = *blanks
093800060410     C                   EVAL      PiDecChr = ','
093900060410     C                   ENDIF
094000060410     C*
094100060410     C                   CALL(e)   'ISNUMERIC'
094200060410     C                   PARM                    PiStr            30
094300060410     C                   PARM                    PiDecChr          1
094400060410     C                   PARM      *ZEROS        PiVal            30 9
094500060410     C                   PARM      '0'           PiInt             1
094600060410     C                   PARM      '0'           PiNum             1
094700060410     C                   IF        %error
094800060410     C                   EVAL      PiNum=*off
094900060410     C                   ENDIF
095000130116     C*
095100060410     C                   ENDSR
095200060410     C***
095300170512
095400060410
095500990910
095600000613     C     *inzsr        BEGSR
095700990910     C*
095800990910     C     *entry        plist
095900990920     C                   parm                    tivlrds
096000990921     C                   parm      wrkesito      esito
096100000724     C                   parm                    prmlit
096200000710     C                   parm                    prmfir
096300170512     C*
096400170512     C* Definizione chiavi di lettura
096500170512     C* TNTBE01L - parziale
096600170512     C     KEYtbe01_P    klist
096700170512     C                   kfld                    tbeCOD                         *UTENTE
096800170512     C                   kfld                    tbeKE1                         *CODICE
096900010606     C*
097000010606     C* CALCOLA LA DATA CORRENTE
097100100301     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
097200100301     C                   eval      datcor = %dec(%date() : *ISO)
097300000613     C*
097400000613     C                   ENDSR
097500000613     C***
