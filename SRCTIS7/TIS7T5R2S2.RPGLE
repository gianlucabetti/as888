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
005800170512
005900170512     D DVPC          e ds                  inz
006000170512     D skVPC999        s              7  0 inz dim(200)
006100170512     D jVPC            s              3  0 inz
006200990908
006300000913     C                   reset                   rrnum
006400990921     C                   reset                   esito
006500990921     C                   reset                   wrkesito
006600010530     C*
006700010530     C                   exsr      opeini
006800010530     C                   exsr      rwvab
006900041025     C*
007000041025     C* Esegue lancio TISI95R solo x chiusura
007100041025     C                   CLEAR                   TISI95DS
007200041025     C                   EVAL      I95TLA = 'C'
007300041025     C                   CALL      'TISI95R'
007400041025     C                   PARM                    TISI95DS
007500010530     C*
007600010530     C                   seton                                        lr
007700010530
007800010530
007900010530
008000910830     C*--------------------------------------------------------
008100020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
008200910830     C*--------------------------------------------------------
008300941229     C     RWVAB         BEGSR
008400990910     C*
008500990914     C                   if        not %open(tivin00r)
008600990908     C                   open      tivin00r
008700990914     C                   endif
008800020913     C                   if        not %open(fivabwwr)
008900020913     C                   open      fivabwwr
009000990914     C                   endif
009100990910     C*
009200990910     C                   clear                   §CTROK
009300990910     C                   clear                   §CTRMO
009400990910     C                   clear                   §CTRNO
009500990910     C*
009600921023     C                   DO        *HIVAL
009700990913     C*
009800990915     C                   READ      tivin00r                               70
009900010618     C*
010000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
010100010618     C                   if        vinflg = '1'
010200010618     C                   eval      flgOk = '1'
010300010618     C                   endif
010400010705     C*
010500010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
010600010705     C                   if        %subst(vindta:1:1) = '='
010700010705     C                   eval      %subst(vindta:1:1) = *blanks
010800010705     C                   endif
010900010705     C*
011000051017     C                   if        vindta  > *blanks
011100000613     C                   add       1             rrnum
011200010601     C* ...elimino eventuali segni
011300010601     C     '+':' '       xlate     vindta        vindta
011400990913     C*
011500010601     C                   if        *in70 = *off and
011600990913     C                             (vinflg = *blanks
011700010711     C                              or vinflg = '0'
011800010711     C                              or vinflg = '2')
011900990913     C*
012000020913     C                   clear                   FIVAB000
012100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
012200010711     C                   if        vinflg = *blanks or vinflg = '0'
012300010711     C                   clear                   vinmsg
012400010711     C                   endif
012500010711     C*
012600100301     C                   eval      ds_easyweb = vindta
012700100301     C                   eval      fnvabaar   = ds_vab
012800991022     C*
012900991022     C                   testb     x'3F'         vhbatb                   02
013000991022     C                   if        not *in02
013100991022     C*
013200971021     C                   SETOFF                                       303132
013300971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
013400971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
013500971023     C*    OPPURE TUTTI BLANKS
013600010530     C*
013700010530     C                   TESTN                   VHBLNP               30
013800010530     C  N30              TESTN                   VHBLNP                 30
013900010530     C  N30              TESTN                   VHBLNP                   30
014000010530     C                   MOVE      VHBLNP        W001A             1
014100010530     C     *IN30         IFEQ      *OFF
014200010530     C     W001A         ORLT      '0'
014300010530     C                   SETON                                        31
014400010530     C                   eval      vinmsg = %trimr(vinmsg)
014500010530     C                             + ' ' + 'VHBLNP'
014600010530     C                   ELSE
014700010530     C                   MOVE      VHBLNP        VABLNP
014800010530     C     VABLNP        IFLE      *ZEROS
014900010530     C                   SETON                                        31
015000010530     C                   eval      vinmsg = %trimr(vinmsg)
015100010530     C                             + ' ' + 'VHBLNP'
015200010530     C                   ENDIF
015300010530     C                   ENDIF
015400100301     C*
015500100301     C                   TESTN                   VHBFGS               30
015600100301     C  N30              TESTN                   VHBFGS                 30
015700100301     C  N30              TESTN                   VHBFGS                   30
015800100301     C                   MOVE      VHBFGS        W001A             1
015900100301     C     *IN30         IFEQ      *OFF
016000100301     C     W001A         ORLT      '0'
016100100301     C                   SETON                                        31
016200100301     C                   eval      vinmsg = %trimr(vinmsg)
016300100301     C                             + ' ' + 'VHBFGS'
016400100301     C                   ELSE
016500100301     C                   MOVE      VHBFGS        VABFGS
016600100301     C     VABFGS        IFLE      *ZEROS
016700100301     C                   SETON                                        31
016800100301     C                   eval      vinmsg = %trimr(vinmsg)
016900100301     C                             + ' ' + 'VHBFGS'
017000100301     C                   ENDIF
017100100301     C                   ENDIF
017200010530     C*
017300971023     C                   TESTN                   VHBCCM               30
017400971023     C  N30              TESTN                   VHBCCM                 30
017500971023     C  N30              TESTN                   VHBCCM                   30
017600971021     C                   MOVE      VHBCCM        W001A             1
017700971021     C     *IN30         IFEQ      *OFF
017800971021     C     W001A         ORLT      '0'
017900971021     C                   SETON                                        31
018000010604     C                   eval      vinmsg = %trimr(vinmsg)
018100010604     C                             + ' ' + 'VHBCCM'
018200971021     C                   ELSE
018300971021     C                   MOVE      VHBCCM        VABCCM
018400971024     C     VABCCM        IFLE      *ZEROS
018500971024     C                   SETON                                        31
018600010604     C                   eval      vinmsg = %trimr(vinmsg)
018700010604     C                             + ' ' + 'VHBCCM'
018800971024     C                   ENDIF
018900971021     C                   ENDIF
019000170504     C*
019100170504     C* Effettuo considerazioni x elaborazioni "multi-filiale"
019200170504     C                   if        vlrpoi = 999
019300170512     C                   exsr      chkVPC999
019400170512     C                   if        VABFGS = 999 or *IN55
019500170504     C                   movel     VABCCM        VABFGS
019600170504     C                   endif
019700170504     C                   eval      invfil = VABFGS
019800170504     C                   else
019900170504     C                   eval      VABFGS = vlrpoi
020000170504     C                   eval      invfil = vlrpoi
020100170504     C                   endif
020200971021     C*
020300971023     C                   TESTN                   VHBAAS               30
020400971023     C  N30              TESTN                   VHBAAS                 30
020500971023     C  N30              TESTN                   VHBAAS                   30
020600971021     C                   MOVE      VHBAAS        W001A             1
020700971021     C     *IN30         IFEQ      *OFF
020800971021     C     W001A         ORLT      '0'
020900971023     C     W001A         ANDNE     *BLANK
021000971021     C                   SETON                                        32
021100010606     C                   MOVEL     datcor        VABAAS
021200010604     C                   eval      vinmsg = %trimr(vinmsg)
021300010604     C                             + ' ' + 'VHBAAS'
021400971021     C                   ELSE
021500971021     C                   MOVE      VHBAAS        VABAAS
021600971021     C                   ENDIF
021700971021     C*
021800971023     C                   TESTN                   VHBMGS               30
021900971023     C  N30              TESTN                   VHBMGS                 30
022000971023     C  N30              TESTN                   VHBMGS                   30
022100971021     C                   MOVE      VHBMGS        W001A             1
022200971021     C     *IN30         IFEQ      *OFF
022300971021     C     W001A         ORLT      '0'
022400971023     C     W001A         ANDNE     *BLANK
022500971021     C                   SETON                                        32
022600010601     C                   MOVE      '0101'        VABMGS
022700010604     C                   eval      vinmsg = %trimr(vinmsg)
022800010604     C                             + ' ' + 'VHBMGS'
022900971021     C                   ELSE
023000971021     C                   MOVE      VHBMGS        VABMGS
023100971021     C                   ENDIF
023200971021     C*
023300971023     C                   TESTN                   VHBNRS               30
023400971023     C  N30              TESTN                   VHBNRS                 30
023500971023     C  N30              TESTN                   VHBNRS                   30
023600971021     C                   MOVE      VHBNRS        W001A             1
023700971021     C     *IN30         IFEQ      *OFF
023800971021     C     W001A         ORLT      '0'
023900971023     C     W001A         ANDNE     *BLANK
024000971021     C                   SETON                                        32
024100971021     C                   MOVE      *ZERO         VABNRS
024200010604     C                   eval      vinmsg = %trimr(vinmsg)
024300010604     C                             + ' ' + 'VHBNRS'
024400971021     C                   ELSE
024500971021     C                   MOVE      VHBNRS        VABNRS
024600971021     C                   ENDIF
024700971021     C*
024800971023     C                   TESTN                   VHBNSP               30
024900971023     C  N30              TESTN                   VHBNSP                 30
025000971023     C  N30              TESTN                   VHBNSP                   30
025100971021     C                   MOVE      VHBNSP        W001A             1
025200971021     C     *IN30         IFEQ      *OFF
025300971021     C     W001A         ORLT      '0'
025400971023     C     W001A         ANDNE     *BLANK
025500971021     C                   SETON                                        32
025600971021     C                   MOVE      *ZERO         VABNSP
025700010604     C                   eval      vinmsg = %trimr(vinmsg)
025800010604     C                             + ' ' + 'VHBNSP'
025900971021     C                   ELSE
026000971021     C                   MOVE      VHBNSP        VABNSP
026100971021     C                   ENDIF
026200971021     C*
026300971023     C                   TESTN                   VHBLNA               30
026400971023     C  N30              TESTN                   VHBLNA                 30
026500971023     C  N30              TESTN                   VHBLNA                   30
026600971021     C                   MOVE      VHBLNA        W001A             1
026700971021     C     *IN30         IFEQ      *OFF
026800971021     C     W001A         ORLT      '0'
026900971023     C     W001A         ANDNE     *BLANK
027000971021     C                   SETON                                        32
027100971021     C                   MOVE      *ZERO         VABLNA
027200010604     C                   eval      vinmsg = %trimr(vinmsg)
027300010604     C                             + ' ' + 'VHBLNA'
027400971021     C                   ELSE
027500971021     C                   MOVE      VHBLNA        VABLNA
027600971021     C                   ENDIF
027700971021     C*
027800971023     C                   TESTN                   VHBCTR               30
027900971023     C  N30              TESTN                   VHBCTR                 30
028000971023     C  N30              TESTN                   VHBCTR                   30
028100971021     C                   MOVE      VHBCTR        W001A             1
028200971021     C     *IN30         IFEQ      *OFF
028300971021     C     W001A         ORLT      '0'
028400971023     C     W001A         ANDNE     *BLANK
028500971021     C                   SETON                                        32
028600971021     C                   MOVE      *ZERO         VABCTR
028700010604     C                   eval      vinmsg = %trimr(vinmsg)
028800010604     C                             + ' ' + 'VHBCTR'
028900971021     C                   ELSE
029000971021     C                   MOVE      VHBCTR        VABCTR
029100971021     C                   ENDIF
029200941229     C*
029300971021     C                   MOVE      *ZERO         VABIAS
029400941229     C                   MOVE      VHBIAS        WASC15
029500971023     C                   TESTN                   WI15                 30
029600971023     C  N30              TESTN                   WI15                   30
029700971023     C  N30              TESTN                   WI15                     30
029800971021     C                   MOVE      WI15          W001A             1
029900971021     C     *IN30         IFEQ      *OFF
030000971021     C     W001A         ORLT      '0'
030100971023     C     W001A         ANDNE     *BLANK
030200971021     C                   SETON                                        32
030300971021     C                   MOVE      *ZERO         VABIAS
030400010604     C                   eval      vinmsg = %trimr(vinmsg)
030500010604     C                             + ' ' + 'VHBIAS'
030600971021     C                   ELSE
030700971021     C                   MOVEL     WI15          VABIAS
030800971023     C                   TESTN                   WD15                 30
030900971023     C  N30              TESTN                   WD15                   30
031000971023     C  N30              TESTN                   WD15                     30
031100971021     C                   MOVE      WD15          W001A             1
031200971021     C     *IN30         IFEQ      *OFF
031300971021     C     W001A         ORLT      '0'
031400971023     C     W001A         ANDNE     *BLANK
031500971021     C                   SETON                                        32
031600971021     C                   MOVE      *ZERO         VABIAS
031700010604     C                   eval      vinmsg = %trimr(vinmsg)
031800010604     C                             + ' ' + 'VHBIAS'
031900971021     C                   ELSE
032000971021     C                   MOVE      WD15          VABIAS
032100971021     C                   ENDIF
032200050118     C                   ENDIF
032300941229     C*
032400971023     C                   TESTN                   VHBNCL               30
032500971023     C  N30              TESTN                   VHBNCL                 30
032600971023     C  N30              TESTN                   VHBNCL                   30
032700971021     C                   MOVE      VHBNCL        W001A             1
032800971021     C     *IN30         IFEQ      *OFF
032900971021     C     W001A         ORLT      '0'
033000971021     C                   SETON                                        32
033100971021     C                   MOVE      *ZERO         VABNCL
033200010604     C                   eval      vinmsg = %trimr(vinmsg)
033300010604     C                             + ' ' + 'VHBNCL'
033400971021     C                   ELSE
033500971021     C                   MOVE      VHBNCL        VABNCL
033600971021     C                   ENDIF
033700941229     C*
033800971021     C                   MOVE      *ZERO         VABPKB
033900971021     C                   MOVE      VHBPKB        WASC09
034000010719     C                   IF        WI09 = *blanks
034100010719     C                   EVAL      WI09 = *zeros
034200010719     C                   ENDIF
034300971023     C                   TESTN                   WI09                 30
034400971023     C  N30              TESTN                   WI09                   30
034500971023     C  N30              TESTN                   WI09                     30
034600971021     C                   MOVE      WI09          W001A             1
034700971021     C     *IN30         IFEQ      *OFF
034800971021     C     W001A         ORLT      '0'
034900971021     C                   SETON                                        32
035000971021     C                   MOVE      *ZERO         VABPKB
035100010604     C                   eval      vinmsg = %trimr(vinmsg)
035200010604     C                             + ' ' + 'VHBPKB'
035300971021     C                   ELSE
035400971021     C                   MOVEL     WI09          VABPKB
035500010719     C                   IF        WD09 = *blanks
035600010719     C                   EVAL      WD09 = *zeros
035700010719     C                   ENDIF
035800971023     C                   TESTN                   WD09                 30
035900971023     C  N30              TESTN                   WD09                     30
036000971021     C                   MOVE      WD09          W001A             1
036100971021     C     *IN30         IFEQ      *OFF
036200971021     C     W001A         ORLT      '0'
036300971023     C     W001A         ANDNE     *BLANK
036400971021     C                   SETON                                        32
036500971021     C                   MOVE      *ZERO         VABPKB
036600010604     C                   eval      vinmsg = %trimr(vinmsg)
036700010604     C                             + ' ' + 'VHBPKB'
036800971021     C                   ELSE
036900971021     C                   MOVE      WD09          VABPKB
037000971021     C                   ENDIF
037100050118     C                   ENDIF
037200971021     C*
037300041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
037400041001     C                   IF        *IN51 = *OFF
037500971021     C                   MOVE      *ZERO         VABVLB
037600971021     C                   MOVE      VHBVLB        WASC07
037700010719     C                   IF        WI07 = *blanks
037800010719     C                   EVAL      WI07 = *zeros
037900010719     C                   ENDIF
038000971023     C                   TESTN                   WI07                 30
038100971023     C  N30              TESTN                   WI07                   30
038200971023     C  N30              TESTN                   WI07                     30
038300971021     C                   MOVE      WI07          W001A             1
038400971021     C     *IN30         IFEQ      *OFF
038500971021     C     W001A         ORLT      '0'
038600971023     C     W001A         ANDNE     *BLANK
038700971021     C                   SETON                                        32
038800971021     C                   MOVE      *ZERO         VABVLB
038900010604     C                   eval      vinmsg = %trimr(vinmsg)
039000010604     C                             + ' ' + 'VHBVLB'
039100971021     C                   ELSE
039200971021     C                   MOVEL     WI07          VABVLB
039300010719     C                   IF        WD07 = *blanks
039400010719     C                   EVAL      WD07 = *zeros
039500010719     C                   ENDIF
039600971023     C                   TESTN                   WD07                 30
039700971023     C  N30              TESTN                   WD07                   30
039800971023     C  N30              TESTN                   WD07                     30
039900971021     C                   MOVE      WD07          W001A             1
040000971021     C     *IN30         IFEQ      *OFF
040100971021     C     W001A         ORLT      '0'
040200971023     C     W001A         ANDNE     *BLANK
040300971021     C                   SETON                                        32
040400971021     C                   MOVE      *ZERO         VABVLB
040500010604     C                   eval      vinmsg = %trimr(vinmsg)
040600010604     C                             + ' ' + 'VHBVLB'
040700971021     C                   ELSE
040800971021     C                   MOVE      WD07          VABVLB
040900971021     C                   ENDIF
041000050118     C                   ENDIF
041100041001     C                   ELSE
041200041001     C* ...altrimenti forzo azzeramento del volume
041300041001     C                   Z-ADD     *zeros        VABVLB
041400041001     C                   ENDIF
041500941229     C*
041600971021     C                   MOVE      *ZERO         VABQFT
041700971021     C                   MOVE      VHBQFT        WASC15
041800010719     C                   IF        WI15 = *blanks
041900010719     C                   EVAL      WI15 = *zeros
042000010719     C                   ENDIF
042100971023     C                   TESTN                   WI15                 30
042200971023     C  N30              TESTN                   WI15                   30
042300971023     C  N30              TESTN                   WI15                     30
042400971021     C                   MOVE      WI15          W001A             1
042500971021     C     *IN30         IFEQ      *OFF
042600971021     C     W001A         ORLT      '0'
042700971023     C     W001A         ANDNE     *BLANK
042800971021     C                   SETON                                        32
042900971021     C                   MOVE      *ZERO         VABQFT
043000010604     C                   eval      vinmsg = %trimr(vinmsg)
043100010604     C                             + ' ' + 'VHBQFT'
043200971021     C                   ELSE
043300971021     C                   MOVEL     WI15          VABQFT
043400010719     C                   IF        WD15 = *blanks
043500010719     C                   EVAL      WD15 = *zeros
043600010719     C                   ENDIF
043700971023     C                   TESTN                   WD15                 30
043800971023     C  N30              TESTN                   WD15                   30
043900971023     C  N30              TESTN                   WD15                     30
044000971021     C                   MOVE      WD15          W001A             1
044100971021     C     *IN30         IFEQ      *OFF
044200971021     C     W001A         ORLT      '0'
044300971023     C     W001A         ANDNE     *BLANK
044400971021     C                   SETON                                        32
044500971021     C                   MOVE      *ZERO         VABQFT
044600010604     C                   eval      vinmsg = %trimr(vinmsg)
044700010604     C                             + ' ' + 'VHBQFT'
044800971021     C                   ELSE
044900971021     C                   MOVE      WD15          VABQFT
045000971021     C                   ENDIF
045100050118     C                   ENDIF
045200941229     C*
045300971021     C                   MOVE      *ZERO         VABCAS
045400060410     C* Se presente separatore decimale alla giusta posizione => forzo sempre la virgola
045500060410     C                   EVAL      wVHBCAS = VHBCAS
045600060410     C                   IF        %subst(wVHBCAS:12:1) = '.'
045700060410     C                   EVAL      %subst(wVHBCAS:12:1) = ','
045800060410     C                   ENDIF
045900060410     C*
046000060410     C                   MOVEL(P)  wVHBCAS       PiStr
046100060410     C                   EXSR      CHKNUM
046200060410     C                   IF        PiNum=*on
046300060410     C                   Z-ADD     PiVal         VABCAS
046400060410     C                   ELSE
046500060410     C                   SETON                                        32
046600060410     C                   Z-ADD     *zeros        VABCAS
046700060410     C                   EVAL      vinmsg = %trimr(vinmsg)
046800060410     C                             + ' ' + 'VHBCAS'
046900060410     C                   ENDIF
047000971021     C*
047100971023     C                   TESTN                   VHBRMN               30
047200971023     C  N30              TESTN                   VHBRMN                 30
047300971023     C  N30              TESTN                   VHBRMN                   30
047400971021     C                   MOVE      VHBRMN        W001A             1
047500971021     C     *IN30         IFEQ      *OFF
047600971021     C     W001A         ORLT      '0'
047700971021     C                   SETON                                        32
047800971021     C                   MOVE      *ZERO         VABRMN
047900010604     C                   eval      vinmsg = %trimr(vinmsg)
048000010604     C                             + ' ' + 'VHBRMN'
048100971021     C                   ELSE
048200971021     C                   MOVE      VHBRMN        VABRMN
048300971021     C                   ENDIF
048400971021     C*
048500971023     C                   TESTN                   VHBNCD               30
048600971023     C  N30              TESTN                   VHBNCD                 30
048700971023     C  N30              TESTN                   VHBNCD                   30
048800971021     C                   MOVE      VHBNCD        W001A             1
048900971021     C     *IN30         IFEQ      *OFF
049000971021     C     W001A         ORLT      '0'
049100971023     C     W001A         ANDNE     *BLANK
049200971021     C                   SETON                                        32
049300971021     C                   MOVE      *ZERO         VABNCD
049400010604     C                   eval      vinmsg = %trimr(vinmsg)
049500010604     C                             + ' ' + 'VHBNCD'
049600971021     C                   ELSE
049700971021     C                   MOVE      VHBNCD        VABNCD
049800971021     C                   ENDIF
049900971021     C*
050000971023     C                   TESTN                   VHBNCA               30
050100971023     C  N30              TESTN                   VHBNCA                 30
050200971023     C  N30              TESTN                   VHBNCA                   30
050300971021     C                   MOVE      VHBNCA        W001A             1
050400971021     C     *IN30         IFEQ      *OFF
050500971021     C     W001A         ORLT      '0'
050600971023     C     W001A         ANDNE     *BLANK
050700971021     C                   SETON                                        32
050800971021     C                   MOVE      *ZERO         VABNCA
050900010604     C                   eval      vinmsg = %trimr(vinmsg)
051000010604     C                             + ' ' + 'VHBNCA'
051100971021     C                   ELSE
051200971021     C                   MOVE      VHBNCA        VABNCA
051300971021     C                   ENDIF
051400971021     C*
051500971023     C                   TESTN                   VHBZNC               30
051600971023     C  N30              TESTN                   VHBZNC                 30
051700971023     C  N30              TESTN                   VHBZNC                   30
051800971021     C                   MOVE      VHBZNC        W001A             1
051900971021     C     *IN30         IFEQ      *OFF
052000971021     C     W001A         ORLT      '0'
052100971023     C     W001A         ANDNE     *BLANK
052200971021     C                   SETON                                        32
052300971021     C                   MOVE      *ZERO         VABZNC
052400010604     C                   eval      vinmsg = %trimr(vinmsg)
052500010604     C                             + ' ' + 'VHBZNC'
052600971021     C                   ELSE
052700971021     C                   MOVE      VHBZNC        VABZNC
052800971021     C                   ENDIF
052900971021     C*
053000971023     C                   TESTN                   VHBDCR               30
053100971023     C  N30              TESTN                   VHBDCR                 30
053200971023     C  N30              TESTN                   VHBDCR                   30
053300971021     C                   MOVE      VHBDCR        W001A             1
053400971021     C     *IN30         IFEQ      *OFF
053500971021     C     W001A         ORLT      '0'
053600971023     C     W001A         ANDNE     *BLANK
053700971021     C                   SETON                                        32
053800971021     C                   MOVE      *ZERO         VABDCR
053900010604     C                   eval      vinmsg = %trimr(vinmsg)
054000010604     C                             + ' ' + 'VHBDCR'
054100971021     C                   ELSE
054200971021     C                   MOVE      VHBDCR        VABDCR
054300971021     C                   ENDIF
054400971021     C*
054500971023     C                   TESTN                   VHBHCR               30
054600971023     C  N30              TESTN                   VHBHCR                 30
054700971023     C  N30              TESTN                   VHBHCR                   30
054800971021     C                   MOVE      VHBHCR        W001A             1
054900971021     C     *IN30         IFEQ      *OFF
055000971021     C     W001A         ORLT      '0'
055100971023     C     W001A         ANDNE     *BLANK
055200971021     C                   SETON                                        32
055300971021     C                   MOVE      *ZERO         VABHCR
055400010604     C                   eval      vinmsg = %trimr(vinmsg)
055500010604     C                             + ' ' + 'VHBHCR'
055600971021     C                   ELSE
055700971021     C                   MOVE      VHBHCR        VABHCR
055800971021     C                   ENDIF
055900941229     C*
056000971021     C                   MOVE      *ZERO         VABVMD
056100971021     C                   MOVE      VHBVMD        WASC15
056200010719     C                   IF        WI15 = *blanks
056300010719     C                   EVAL      WI15 = *zeros
056400010719     C                   ENDIF
056500971023     C                   TESTN                   WI15                 30
056600971023     C  N30              TESTN                   WI15                   30
056700971023     C  N30              TESTN                   WI15                     30
056800971021     C                   MOVE      WI15          W001A             1
056900971021     C     *IN30         IFEQ      *OFF
057000971021     C     W001A         ORLT      '0'
057100971023     C     W001A         ANDNE     *BLANK
057200971021     C                   SETON                                        32
057300971021     C                   MOVE      *ZERO         VABVMD
057400010604     C                   eval      vinmsg = %trimr(vinmsg)
057500010604     C                             + ' ' + 'VHBVMD'
057600971021     C                   ELSE
057700971021     C                   MOVEL     WI15          VABVMD
057800010719     C                   IF        WD15 = *blanks
057900010719     C                   EVAL      WD15 = *zeros
058000010719     C                   ENDIF
058100971023     C                   TESTN                   WD15                 30
058200971023     C  N30              TESTN                   WD15                   30
058300971023     C  N30              TESTN                   WD15                     30
058400971021     C                   MOVE      WD15          W001A             1
058500971021     C     *IN30         IFEQ      *OFF
058600971021     C     W001A         ORLT      '0'
058700971023     C     W001A         ANDNE     *BLANK
058800971021     C                   SETON                                        32
058900971021     C                   MOVE      *ZERO         VABVMD
059000010604     C                   eval      vinmsg = %trimr(vinmsg)
059100010604     C                             + ' ' + 'VHBVMD'
059200971021     C                   ELSE
059300971021     C                   MOVE      WD15          VABVMD
059400971021     C                   ENDIF
059500050118     C                   ENDIF
059600971021     C*
059700971023     C                   TESTN                   VHBANT               30
059800971023     C  N30              TESTN                   VHBANT                 30
059900971023     C  N30              TESTN                   VHBANT                   30
060000971021     C                   MOVE      VHBANT        W001A             1
060100971021     C     *IN30         IFEQ      *OFF
060200971021     C     W001A         ORLT      '0'
060300971023     C     W001A         ANDNE     *BLANK
060400971021     C                   SETON                                        32
060500971021     C                   MOVE      *ZERO         VABANT
060600010604     C                   eval      vinmsg = %trimr(vinmsg)
060700010604     C                             + ' ' + 'VHBANT'
060800971021     C                   ELSE
060900971021     C                   MOVE      VHBANT        VABANT
061000971021     C                   ENDIF
061100971021     C*
061200971021     C* CAMPI ALFABETICI:
061300971021     C                   MOVE      VHBATB        VABATB
061400971021     C                   MOVE      VHBCBO        VABCBO
061500971021     C                   MOVE      VHBRSD        VABRSD
061600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
061700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
061800020117     C* ==
061900971021     C                   MOVE      VHBRD2        VABRD2
062000971021     C                   MOVE      VHBIND        VABIND
062100971021     C                   MOVE      VHBCAD        VABCAD
062200971021     C                   MOVE      VHBLOD        VABLOD
062300971021     C                   MOVE      VHBPRD        VABPRD
062400971021     C                   MOVE      VHBNZD        VABNZD
062500971021     C                   MOVE      VHBGC1        VABGC1
062600971021     C                   MOVE      VHBGC2        VABGC2
062700971021     C                   MOVE      VHBTSP        VABTSP
062800971021     C                   MOVE      VHBVAS        VABVAS
062900971021     C                   MOVE      VHBNAS        VABNAS
063000971021     C                   MOVE      VHBTIC        VABTIC
063100971021     C                   MOVE      VHBVCA        VABVCA
063200971021     C                   MOVE      VHBGCA        VABGCA
063300971021     C                   MOVE      VHBRMA        VABRMA
063400971021     C                   MOVE      VHBXCO        VABXCO
063500971021     C                   MOVE      VHBNOT        VABNOT
063600971021     C                   MOVE      VHBNT2        VABNT2
063700971021     C                   MOVE      VHBCTM        VABCTM
063800971021     C                   MOVE      VHBFFD        VABFFD
063900971021     C                   MOVE      VHBTCR        VABTCR
064000971021     C                   MOVE      VHBCTS        VABCTS
064100971021     C                   MOVE      VHBFTM        VABFTM
064200971021     C                   MOVE      VHBVAD        VABVAD
064300971021     C                   MOVE      VHBGMA        VABGMA
064400971021     C                   MOVE      VHBGGA        VABGGA
064500971021     C                   MOVE      VHBGVA        VABGVA
064600971021     C                   MOVE      VHBTC1        VABTC1
064700971021     C                   MOVE      VHBTC2        VABTC2
064800971021     C                   MOVE      VHBSCL        VABSCL
064900971021     C                   MOVE      VHBRMO        VABRMO
065000971021     C                   MOVE      VHBCMO        VABCMO
065100971021     C                   MOVE      VHBNMO        VABNMO
065200971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
065300060329     C* NELLA NAZIONE MITTENTE ORIGINALE
065400060331     C                   IF        %trim(VHBNMO) < 'A'
065500060329     C                   MOVE      *BLANKS       VABNMO
065600060329     C                   ENDIF
065700050418     C*
065800050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
065900050418     C                   IF        %trim(VABNZD) = 'I'   OR
066000050418     C                             %trim(VABNZD) = 'IT'  OR
066100050418     C                             %trim(VABNZD) = 'ITA'
066200050418     C                   EVAL      VABNZD = *blanks
066300050418     C                   ENDIF
066400041025     C*
066500041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
066600041025     C                   IF        *IN52 = *ON
066700041025     C* Reperisco la provincia dal CAP e dalla località
066800041025     C                   IF        VABCAD <> *blanks AND
066900050907     C                             VABPRD  = *blanks AND
067000050907     C                             VABNZD  = *blanks
067100041025     C                   CLEAR                   TISI95DS
067200041025     C                   EVAL      I95TCN = '3'
067300041025     C                   Z-ADD     datcor        I95DAT
067400041025     C                   EVAL      I95CAP = VABCAD
067500041025     C                   EVAL      I95LOC = VABLOD
067600041025     C                   CALL      'TISI95R'
067700041025     C                   PARM                    TISI95DS
067800041025     C                   EVAL      VABPRD = O95PRV
067900041025     C                   ENDIF
068000041025     C                   ENDIF
068100040630     C*
068200040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
068300040630     C                   exsr      exeBon
068400100419     C*
068500100419     C* Forzatura su campi DIVISE
068600100419     C                   IF        VABVAS = *blanks
068700100419     C                   EVAL      VABVAS = 'EUR'
068800100419     C                   ENDIF
068900100419     C                   IF        VABVCA = *blanks
069000100419     C                   EVAL      VABVCA = 'EUR'
069100100419     C                   ENDIF
069200100419     C                   IF        VABVAD = *blanks
069300100419     C                   EVAL      VABVAD = 'EUR'
069400100419     C                   ENDIF
069500930409     C*
069600010604     C  N31              ADD       1             §CTROK            7 0
069700010604     C   32              ADD       1             §CTRMO            7 0
069800010604     C   31              ADD       1             §CTRNO            7 0
069900110418     C*
070000110418     C* Verifico se sui parametri del traduttore è richiesta l'esclusione del 1020005
070100121115     C                   IF        %subst(vlrppt:4:1) = 'S' AND
070200121115     C                             (VABCCM = 1020005 OR VABCCM = 1020004)
070300110418     C                   ELSE
070400020913     C  N31              WRITE     FIVAB000
070500110418     C                   ENDIF
070600990910     C*
070700010601     C                   if        *in31 = *off and
070800010601     C                             *in32 = *off
070900990910     C                   eval      vinflg = '1'
071000990910     C                   else
071100990910     C                   eval      vinflg = '2'
071200990910     C                   endif
071300991022     C                   endif
071400990910     C                   endif
071500000905     C*
071600000905     C                   else
071700000905     C                   eval      vinflg = '1'
071800000905     C                   endif
071900000905     C*
072000000905     C  N70              update    tivin000
072100991022     C*
072200991022     C  N70              ENDdo
072300010530     C
072400990910     C* Se non ci sono record con errori ...
072500010530     C                   if        §ctrno = 0 and
072600170504     C                             §ctrmo = 0
072700990910     C* ... restituisco esito OK.
072800990921     C                   eval      wrkesito = '0'
072900990910     C                   else
073000010601     C                   if        §ctrok > 0
073100990921     C                   eval      wrkesito = '1'
073200000710     C                   else
073300010615     C                   if        flgOk = '0'
073400010615     C                   eval      wrkesito = '2'
073500010615     C                   else
073600010615     C                   eval      wrkesito = '6'
073700010615     C                   endif
073800990910     C                   endif
073900000710     C                   endif
074000990910     C*
074100990914     C                   if        %open(tivin00r)
074200990908     C                   close     tivin00r
074300990914     C                   endif
074400020913     C                   if        %open(fivabwwr)
074500020913     C                   close     fivabwwr
074600990914     C                   endif
074700990910     C*
074800990920     C                   if        §ctrok > 0
074900010530     C                             and invfil > *zeros
075000000613     C                   exsr      invio
075100990920     C                   endif
075200990920     C*
075300910830     C                   ENDSR
075400000613     C***
075500010530
075600010530
075700010530
075800990920      /TITLE Invio dei dati al punto operativo.
075900000613     C     invio         BEGSR
076000990920     C*
076100990920     C                   reset                   dscmz
076200010530     C                   move      invfil        cmzdst
076300990920     C                   eval      cmzfld = vlrfou
076400990920     C                   eval      cmzmbd = vlrhdl
076500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
076600000710     C                   if        prmfir = *blanks
076700020913     C                   eval      cmzfla = 'FIVAB00F'
076800020913     C                   eval      cmzmba = 'FIVAB00F'
076900000710     C                   else
077000000710     C                   eval      cmzfla = prmfir
077100000710     C                   eval      cmzmba = prmfir
077200000710     C                   endif
077300990920     C                   eval      cmznrr = *zeros
077400990920     C                   move      §ctrok        cmznrr
077500021003     C                   eval      cmzlba = vlrfl1
077600990920     C                   call(e)   'TIS711C'
077700990920     C                   parm                    dscmz
077800990921     C                   parm      *blanks       esito
077900990920     C                   if        %error
078000990920     C                             or cmzerr = '1'
078100990921     C                             or esito  = '1'
078200000710     C                   eval      wrkesito = '3'
078300990920     C                   endif
078400990920     C*
078500000613     C                   ENDSR
078600000613     C***
078700010530
078800010530
078900010530
079000010530
079100010530      /TITLE Invio dei dati al punto operativo.
079200010530     C     opeini        BEGSR
079300010530     C*
079400010530     C* Inizializzo flag e contatori operativi
079500010615     C                   movel     '0'           flgOk             1
079600010530     C                   z-add     *zeros        invfil            3 0
079700030407     C*
079800030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
079900030407     C                   if        %subst(vlrppt:1:1) = 'S'
080000030407     C                   seton                                        50
080100030407     C                   else
080200030407     C                   setoff                                       50
080300030407     C                   endif
080400041001     C*
080500041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
080600041001     C                   if        %subst(vlrppt:2:1) = 'S'
080700041001     C                   seton                                        51
080800041001     C                   else
080900041001     C                   setoff                                       51
081000041001     C                   endif
081100041025     C*
081200041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
081300041025     C* (naturalmente solo se nn già presente)
081400041025     C                   if        %subst(vlrppt:3:1) = 'S'
081500041025     C                   seton                                        52
081600041025     C                   else
081700041025     C                   setoff                                       52
081800041025     C                   endif
081900010530     C*
082000010530     C                   ENDSR
082100010530     C***
082200040407
082300040407
082400040407
082500040407
082600040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
082700040407     C     exeBon        BEGSR
082800040407     C*
082900040407     C* Imposto i parametri x il *pgm d "bonifica"
083000040407     C                   EVAL      ElencoChar = *blanks
083100040407     C                   EVAL      TipoElenco = *blanks
083200040407     C                   EVAL      CharSost   = *blanks
083300040629     C                   EVAL      UpperCase  = '1'
083400040407     C                   EVAL      ChkNull    = '1'
083500040407     C                   EVAL      CharNull   = *blanks
083600040629     C*
083700040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
083800040629     C                   EVAL      TxtInOut   = VABRSD
083900040629     C                   EVAL      EsitoBon   = *blanks
084000040407     C*
084100040407     C                   CALL      'XCHKCHAR'
084200040407     C                   PARM                    TxtInOut
084300040407     C                   PARM                    ElencoChar
084400040407     C                   PARM                    TipoElenco
084500040407     C                   PARM                    CharSost
084600040407     C                   PARM                    UpperCase
084700040407     C                   PARM                    ChkNull
084800040407     C                   PARM                    CharNull
084900040407     C                   PARM                    EsitoBon
085000040407     C*
085100040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
085200040407     C                   IF        EsitoBon = '1'
085300040629     C                   EVAL      VABRSD = TxtInOut
085400040407     C                   ENDIF
085500040629     C*
085600040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
085700040629     C                   EVAL      TxtInOut   = VABIND
085800040629     C                   EVAL      EsitoBon   = *blanks
085900040629     C*
086000040629     C                   CALL      'XCHKCHAR'
086100040629     C                   PARM                    TxtInOut
086200040629     C                   PARM                    ElencoChar
086300040629     C                   PARM                    TipoElenco
086400040629     C                   PARM                    CharSost
086500040629     C                   PARM                    UpperCase
086600040629     C                   PARM                    ChkNull
086700040629     C                   PARM                    CharNull
086800040629     C                   PARM                    EsitoBon
086900040629     C*
087000040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
087100040629     C                   IF        EsitoBon = '1'
087200040629     C                   EVAL      VABIND = TxtInOut
087300040629     C                   ENDIF
087400040629     C*
087500040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
087600040629     C                   EVAL      TxtInOut   = VABLOD
087700040629     C                   EVAL      EsitoBon   = *blanks
087800040629     C*
087900040629     C                   CALL      'XCHKCHAR'
088000040629     C                   PARM                    TxtInOut
088100040629     C                   PARM                    ElencoChar
088200040629     C                   PARM                    TipoElenco
088300040629     C                   PARM                    CharSost
088400040629     C                   PARM                    UpperCase
088500040629     C                   PARM                    ChkNull
088600040629     C                   PARM                    CharNull
088700040629     C                   PARM                    EsitoBon
088800040629     C*
088900040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
089000040629     C                   IF        EsitoBon = '1'
089100040629     C                   EVAL      VABLOD = TxtInOut
089200040629     C                   ENDIF
089300040629     C*
089400040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
089500040629     C                   EVAL      TxtInOut   = VABNOT
089600040629     C                   EVAL      EsitoBon   = *blanks
089700040630     C                   EVAL      UpperCase  = *blanks
089800040629     C*
089900040629     C                   CALL      'XCHKCHAR'
090000040629     C                   PARM                    TxtInOut
090100040629     C                   PARM                    ElencoChar
090200040629     C                   PARM                    TipoElenco
090300040629     C                   PARM                    CharSost
090400040629     C                   PARM                    UpperCase
090500040629     C                   PARM                    ChkNull
090600040629     C                   PARM                    CharNull
090700040629     C                   PARM                    EsitoBon
090800040629     C*
090900040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
091000040629     C                   IF        EsitoBon = '1'
091100040629     C                   EVAL      VABNOT = TxtInOut
091200040629     C                   ENDIF
091300040629     C*
091400040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
091500040629     C                   EVAL      TxtInOut   = VABNT2
091600040629     C                   EVAL      EsitoBon   = *blanks
091700040630     C                   EVAL      UpperCase  = *blanks
091800040629     C*
091900040629     C                   CALL      'XCHKCHAR'
092000040629     C                   PARM                    TxtInOut
092100040629     C                   PARM                    ElencoChar
092200040629     C                   PARM                    TipoElenco
092300040629     C                   PARM                    CharSost
092400040629     C                   PARM                    UpperCase
092500040629     C                   PARM                    ChkNull
092600040629     C                   PARM                    CharNull
092700040629     C                   PARM                    EsitoBon
092800040629     C*
092900040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
093000040629     C                   IF        EsitoBon = '1'
093100040629     C                   EVAL      VABNT2 = TxtInOut
093200040629     C                   ENDIF
093300040407     C*
093400040407     C                   ENDSR
093500010530
093600060410
093700060410     C*----------------------------------------------------*
093800060410     C*  CONTROLLO NUMERICITA' CAMPI
093900060410     C*----------------------------------------------------*
094000060410     C     CHKNUM        BEGSR
094100060410     C*
094200060410     C                   IF        PiDecChr = *blanks
094300060410     C                   EVAL      PiDecChr = ','
094400060410     C                   ENDIF
094500060410     C*
094600060410     C                   CALL(e)   'ISNUMERIC'
094700060410     C                   PARM                    PiStr            30
094800060410     C                   PARM                    PiDecChr          1
094900060410     C                   PARM      *ZEROS        PiVal            30 9
095000060410     C                   PARM      '0'           PiInt             1
095100060410     C                   PARM      '0'           PiNum             1
095200060410     C                   IF        %error
095300060410     C                   EVAL      PiNum=*off
095400060410     C                   ENDIF
095500130116     C*
095600060410     C                   ENDSR
095700060410     C***
095800170512
095900170512
096000170512     C     chkVPC999     BEGSR
096100170512     C*
096200170512     C* Verifico se codice cliente già presente in schiera codici abilitati "ESW999"
096300170512     C     vabCCM        LOOKUP    skVPC999                               55
096400170512     C                   IF        %found
096500170512     C                   SETON                                        55
096600170512     C                   ELSE
096700170512     C                   SETOFF                                       55
096800170512     C*
096900170512     C* Cerco in tabella VPC se c'è la personalizzazione
097000170512     C                   movel(p)  'VPC'         tbeCOD
097100170512     C                   movel(p)  vabCCM        tbeKE1
097200170512     C     KEYtbe01_P    chain     tntbe01l
097300170512     C                   if        %found(tntbe01l)
097400170512     C                   movel(P)  tbeUNI        DVPC
097500170512     C                   if        §VPCESW999 = 'S'
097600170512     C                   add       1             jVPC
097700170512     C                   eval      skVPC999(jVPC) = vabCCM
097800170512     C                   SETON                                        55
097900170512     C                   endif
098000170512     C                   endif
098100170512     C                   ENDIF
098200170512     C*
098300170512     C                   ENDSR
098400170512     C*------------------------------------------------------------------------*
098500170512
098600060410
098700990910
098800000613     C     *inzsr        BEGSR
098900990910     C*
099000990910     C     *entry        plist
099100990920     C                   parm                    tivlrds
099200990921     C                   parm      wrkesito      esito
099300000724     C                   parm                    prmlit
099400000710     C                   parm                    prmfir
099500170512     C*
099600170512     C* Definizione chiavi di lettura
099700170512     C* TNTBE01L - parziale
099800170512     C     KEYtbe01_P    klist
099900170512     C                   kfld                    tbeCOD                         *UTENTE
100000170512     C                   kfld                    tbeKE1                         *CODICE
100100010606     C*
100200010606     C* CALCOLA LA DATA CORRENTE
100300100301     C                   z-add     *zeros        datcor            8 0          *DATA CORRENTE AA/M/
100400100301     C                   eval      datcor = %dec(%date() : *ISO)
100500000613     C*
100600000613     C                   ENDSR
100700000613     C***
