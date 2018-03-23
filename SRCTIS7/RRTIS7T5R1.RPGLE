000100020913      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600020913     FFIVABwwr  O    E             DISK    usropn
000700010601     Ftis7t5p   O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900010601     Ftis7t5ps  O    f  198        PRINTER usropn
001000000621     F                                     oflind(*inob)
001100990908
001200000512     D*------------
001300000512     D* COMANDI
001400000512     D*------------
001500011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001600000512     D*------------
001700990920     D dscmz         e ds                  inz
001800990910     D psds           sds
001900990910     D  procname         *PROC
002000941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002100941229     D* 10+3
002200941229     D                 DS
002300941229     D  WS15                   1      1
002400941229     D  WI15                   2     11
002500941229     D  WV15                  12     12
002600941229     D  WD15                  13     15
002700941229     D  WASC15                 1     15
002800941229     D*  6+1
002900941229     D                 DS
003000941229     D  WS09                   1      1
003100941229     D  WI09                   2      7
003200941229     D  WV09                   8      8
003300941229     D  WD09                   9      9
003400941229     D  WASC09                 1      9
003500941229     D*  2+3
003600941229     D                 DS
003700941229     D  WS07                   1      1
003800941229     D  WI07                   2      3
003900941229     D  WV07                   4      4
004000941229     D  WD07                   5      7
004100941229     D  WASC07                 1      7
004200990920     D tivlrds       e ds                  extname(tivlr00f)
004300990910     D fnvabaar      e ds                  inz
004400990910     D esito           s              1
004500000724     D prmlit          s             10
004600000710     D prmfir          s             10
004700990921     D wrkesito        s                   like(esito)
004800990915     D wrkdata         s               d
004900990915     D wrkora          s               t
005000000613     D rrnum           s              6  0 INZ(*zeros)
005100000621     D recko           s            150    INZ(*blanks)
005200011123     D depcmd          s            150    INZ(*blanks)
005300010606     D*------------------
005400010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
005500010606     D*------------------
005600010606     D WLBDA8          DS                  INZ
005700010606     D  G08DAT                 1      8  0
005800010606     D  G08INV                 9     16  0
005900010606     D  G08ERR                17     17
006000010606     D  G08TGI                18     22  0
006100010606     D*
006200040407     D TxtInOut        S           2048
006300040407     D ElencoChar      S            256
006400040407     D TipoElenco      S              1
006500040407     D CharSost        S              1
006600040407     D UpperCase       S              1
006700040407     D ChkNull         S              1
006800040407     D CharNull        S              1
006900040407     D EsitoBon        S              1
007000990908
007100990915     C                   time                    wrkdata
007200990915     C                   time                    wrkora
007300000913     C                   reset                   rrnum
007400990921     C                   reset                   esito
007500990921     C                   reset                   wrkesito
007600010530     C*
007700010530     C                   exsr      opeini
007800010530     C                   exsr      rwvab
007900010530     C*
008000010530     C                   seton                                        lr
008100010530
008200010530
008300010530
008400010530
008500010530     C*--------------------------------------------------------
008600010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
008700010530     C*--------------------------------------------------------
008800010530     C     PREELA        BEGSR
008900000724     C*
009000000724     C* SE OCCORRE SPEDIRE IN FILIALE
009100010530     C                   if        invfil <> *zeros and
009200010530     C                             flgGiro = '0'
009300010530     C*
009400010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
009500010530     C                   eval      flgGiro = '1'
009600000724     C*
009700000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
009800000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
009900010530     C     invfil        chain     azorg01l
010000000724     C                   if        %found
010100000616     C                   movel(p)  CMD(1)        depcmd
010200020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010300000616     C*
010400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
010500011123     C                   Z-ADD     150           LENGH            15 5
010600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
010700000616     C                   PARM                    depcmd
010800000616     C                   PARM                    LENGH
010900000724     C*
011000000724     C                   endif
011100000616     C*
011200000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011300000616     C                   movel(p)  CMD(2)        depcmd
011400000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
011500000616     C*
011600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
011700011123     C                   Z-ADD     150           LENGH            15 5
011800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011900000616     C                   PARM                    depcmd
012000000616     C                   PARM                    LENGH
012100000616     C*
012200010601     C                   if        not %open(tis7t5ps)
012300010601     C                   open      tis7t5ps
012400030407     C   50              except    testdett
012500000613     C                   endif
012600010530     C*
012700010530     C                   endif
012800010530     C*
012900010530     C                   ENDSR
013000010530     C***
013100010530
013200010530
013300010530
013400010530
013500010530
013600010530
013700010530     C*--------------------------------------------------------
013800010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
013900010530     C*--------------------------------------------------------
014000010530     C     ENDELA        BEGSR
014100010530     C*
014200010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014300000613     C*
014400010601     C                   if        %open(tis7t5ps)
014500030407     C   50              except    findett
014600010601     C                   close     tis7t5ps
014700000613     C                   endif
014800000616     C*
014900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015000011123     C                   Z-ADD     150           LENGH            15 5
015100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015200000616     C                   PARM                    CMD(3)
015300000616     C                   PARM                    LENGH
015400010530     C*
015500010530     C                   ENDSR
015600010530     C***
015700000613
015800010530
015900010530
016000000613
016100000613     C*--------------------------------------------------------
016200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016300000613     C*--------------------------------------------------------
016400000621     C     STPR          BEGSR
016500000613     C*
016600010601     C                   if        not %open(tis7t5p)
016700010601     C                   open      tis7t5p
016800990915     C                   endif
016900990915     C*
017000030407     C   50              except    riepilogo
017100990915     C*
017200010601     C                   if        %open(tis7t5p)
017300010601     C                   close     tis7t5p
017400990914     C                   endif
017500990910     C*
017600000613     C                   ENDSR
017700000613     C***
017800990908
017900010530
018000010530
018100010530
018200910830     C*--------------------------------------------------------
018300020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
018400910830     C*--------------------------------------------------------
018500941229     C     RWVAB         BEGSR
018600990910     C*
018700990914     C                   if        not %open(tivin00r)
018800990908     C                   open      tivin00r
018900990914     C                   endif
019000020913     C                   if        not %open(fivabwwr)
019100020913     C                   open      fivabwwr
019200990914     C                   endif
019300990910     C*
019400990910     C                   clear                   §CTROK
019500990910     C                   clear                   §CTRMO
019600990910     C                   clear                   §CTRNO
019700990910     C*
019800921023     C                   DO        *HIVAL
019900990913     C*
020000990915     C                   READ      tivin00r                               70
020100010618     C*
020200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
020300010618     C                   if        vinflg = '1'
020400010618     C                   eval      flgOk = '1'
020500010618     C                   endif
020600010705     C*
020700010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
020800010705     C                   if        %subst(vindta:1:1) = '='
020900010705     C                   eval      %subst(vindta:1:1) = *blanks
021000010705     C                   endif
021100010705     C*
021200000905     C                   if        vindta > *blanks
021300000613     C                   add       1             rrnum
021400010601     C* ...elimino eventuali segni
021500010601     C     '+':' '       xlate     vindta        vindta
021600990913     C*
021700010601     C                   if        *in70 = *off and
021800990913     C                             (vinflg = *blanks
021900010711     C                              or vinflg = '0'
022000010711     C                              or vinflg = '2')
022100990913     C*
022200020913     C                   clear                   FIVAB000
022300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
022400010711     C                   if        vinflg = *blanks or vinflg = '0'
022500010711     C                   clear                   vinmsg
022600010711     C                   endif
022700010711     C*
022800990913     C                   eval      fnvabaar = vindta
022900040407     C*
023000040407     C* "bonifico" i dati d input da eventuali caratteri indesiderati
023100040407     C                   exsr      exeBon
023200991022     C*
023300991022     C                   testb     x'3F'         vhbatb                   02
023400991022     C                   if        not *in02
023500991022     C*
023600971021     C                   SETOFF                                       303132
023700971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
023800971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
023900971023     C*    OPPURE TUTTI BLANKS
024000010530     C*
024100010530     C                   TESTN                   VHBLNP               30
024200010530     C  N30              TESTN                   VHBLNP                 30
024300010530     C  N30              TESTN                   VHBLNP                   30
024400010530     C                   MOVE      VHBLNP        W001A             1
024500010530     C     *IN30         IFEQ      *OFF
024600010530     C     W001A         ORLT      '0'
024700010530     C                   SETON                                        31
024800010530     C                   eval      vinmsg = %trimr(vinmsg)
024900010530     C                             + ' ' + 'VHBLNP'
025000010530     C                   ELSE
025100010530     C                   MOVE      VHBLNP        VABLNP
025200010530     C     VABLNP        IFLE      *ZEROS
025300010530     C                   SETON                                        31
025400010530     C                   eval      vinmsg = %trimr(vinmsg)
025500010530     C                             + ' ' + 'VHBLNP'
025600010530     C                   ENDIF
025700010530     C                   ENDIF
025800010530     C*
025900010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026000010530     C                   eval      depfil = VABLNP
026100010530     C                   exsr      repfil
026200010601     C                   if        depfil = invfil
026300021014     C                   if        vlrpoi = 999
026400021010     C                   MOVE(P)   invfil        VABFGS
026500021014     C                   else
026600021014     C                   MOVE(P)   vlrpoi        VABFGS
026700021014     C                   endif
026800010530     C*
026900010530     C                   exsr      PREELA
027000010530     C*
027100971023     C                   TESTN                   VHBCCM               30
027200971023     C  N30              TESTN                   VHBCCM                 30
027300971023     C  N30              TESTN                   VHBCCM                   30
027400971021     C                   MOVE      VHBCCM        W001A             1
027500971021     C     *IN30         IFEQ      *OFF
027600971021     C     W001A         ORLT      '0'
027700971021     C                   SETON                                        31
027800010604     C                   eval      vinmsg = %trimr(vinmsg)
027900010604     C                             + ' ' + 'VHBCCM'
028000971021     C                   ELSE
028100971021     C                   MOVE      VHBCCM        VABCCM
028200971024     C     VABCCM        IFLE      *ZEROS
028300971024     C                   SETON                                        31
028400010604     C                   eval      vinmsg = %trimr(vinmsg)
028500010604     C                             + ' ' + 'VHBCCM'
028600971024     C                   ENDIF
028700971021     C                   ENDIF
028800971021     C*
028900971023     C                   TESTN                   VHBAAS               30
029000971023     C  N30              TESTN                   VHBAAS                 30
029100971023     C  N30              TESTN                   VHBAAS                   30
029200971021     C                   MOVE      VHBAAS        W001A             1
029300971021     C     *IN30         IFEQ      *OFF
029400971021     C     W001A         ORLT      '0'
029500971023     C     W001A         ANDNE     *BLANK
029600971021     C                   SETON                                        32
029700010606     C                   MOVEL     datcor        VABAAS
029800010604     C                   eval      vinmsg = %trimr(vinmsg)
029900010604     C                             + ' ' + 'VHBAAS'
030000971021     C                   ELSE
030100971021     C                   MOVE      VHBAAS        VABAAS
030200971021     C                   ENDIF
030300971021     C*
030400971023     C                   TESTN                   VHBMGS               30
030500971023     C  N30              TESTN                   VHBMGS                 30
030600971023     C  N30              TESTN                   VHBMGS                   30
030700971021     C                   MOVE      VHBMGS        W001A             1
030800971021     C     *IN30         IFEQ      *OFF
030900971021     C     W001A         ORLT      '0'
031000971023     C     W001A         ANDNE     *BLANK
031100971021     C                   SETON                                        32
031200010601     C                   MOVE      '0101'        VABMGS
031300010604     C                   eval      vinmsg = %trimr(vinmsg)
031400010604     C                             + ' ' + 'VHBMGS'
031500971021     C                   ELSE
031600971021     C                   MOVE      VHBMGS        VABMGS
031700971021     C                   ENDIF
031800971021     C*
031900971023     C                   TESTN                   VHBNRS               30
032000971023     C  N30              TESTN                   VHBNRS                 30
032100971023     C  N30              TESTN                   VHBNRS                   30
032200971021     C                   MOVE      VHBNRS        W001A             1
032300971021     C     *IN30         IFEQ      *OFF
032400971021     C     W001A         ORLT      '0'
032500971023     C     W001A         ANDNE     *BLANK
032600971021     C                   SETON                                        32
032700971021     C                   MOVE      *ZERO         VABNRS
032800010604     C                   eval      vinmsg = %trimr(vinmsg)
032900010604     C                             + ' ' + 'VHBNRS'
033000971021     C                   ELSE
033100971021     C                   MOVE      VHBNRS        VABNRS
033200971021     C                   ENDIF
033300971021     C*
033400971023     C                   TESTN                   VHBNSP               30
033500971023     C  N30              TESTN                   VHBNSP                 30
033600971023     C  N30              TESTN                   VHBNSP                   30
033700971021     C                   MOVE      VHBNSP        W001A             1
033800971021     C     *IN30         IFEQ      *OFF
033900971021     C     W001A         ORLT      '0'
034000971023     C     W001A         ANDNE     *BLANK
034100971021     C                   SETON                                        32
034200971021     C                   MOVE      *ZERO         VABNSP
034300010604     C                   eval      vinmsg = %trimr(vinmsg)
034400010604     C                             + ' ' + 'VHBNSP'
034500971021     C                   ELSE
034600971021     C                   MOVE      VHBNSP        VABNSP
034700971021     C                   ENDIF
034800971021     C*
034900971023     C                   TESTN                   VHBLNA               30
035000971023     C  N30              TESTN                   VHBLNA                 30
035100971023     C  N30              TESTN                   VHBLNA                   30
035200971021     C                   MOVE      VHBLNA        W001A             1
035300971021     C     *IN30         IFEQ      *OFF
035400971021     C     W001A         ORLT      '0'
035500971023     C     W001A         ANDNE     *BLANK
035600971021     C                   SETON                                        32
035700971021     C                   MOVE      *ZERO         VABLNA
035800010604     C                   eval      vinmsg = %trimr(vinmsg)
035900010604     C                             + ' ' + 'VHBLNA'
036000971021     C                   ELSE
036100971021     C                   MOVE      VHBLNA        VABLNA
036200971021     C                   ENDIF
036300971021     C*
036400971023     C                   TESTN                   VHBCTR               30
036500971023     C  N30              TESTN                   VHBCTR                 30
036600971023     C  N30              TESTN                   VHBCTR                   30
036700971021     C                   MOVE      VHBCTR        W001A             1
036800971021     C     *IN30         IFEQ      *OFF
036900971021     C     W001A         ORLT      '0'
037000971023     C     W001A         ANDNE     *BLANK
037100971021     C                   SETON                                        32
037200971021     C                   MOVE      *ZERO         VABCTR
037300010604     C                   eval      vinmsg = %trimr(vinmsg)
037400010604     C                             + ' ' + 'VHBCTR'
037500971021     C                   ELSE
037600971021     C                   MOVE      VHBCTR        VABCTR
037700971021     C                   ENDIF
037800941229     C*
037900971021     C                   MOVE      *ZERO         VABIAS
038000941229     C                   MOVE      VHBIAS        WASC15
038100971023     C                   TESTN                   WI15                 30
038200971023     C  N30              TESTN                   WI15                   30
038300971023     C  N30              TESTN                   WI15                     30
038400971021     C                   MOVE      WI15          W001A             1
038500971021     C     *IN30         IFEQ      *OFF
038600971021     C     W001A         ORLT      '0'
038700971023     C     W001A         ANDNE     *BLANK
038800971021     C                   SETON                                        32
038900971021     C                   MOVE      *ZERO         VABIAS
039000010604     C                   eval      vinmsg = %trimr(vinmsg)
039100010604     C                             + ' ' + 'VHBIAS'
039200971021     C                   ELSE
039300971021     C                   MOVEL     WI15          VABIAS
039400971021     C                   ENDIF
039500971023     C                   TESTN                   WD15                 30
039600971023     C  N30              TESTN                   WD15                   30
039700971023     C  N30              TESTN                   WD15                     30
039800971021     C                   MOVE      WD15          W001A             1
039900971021     C     *IN30         IFEQ      *OFF
040000971021     C     W001A         ORLT      '0'
040100971023     C     W001A         ANDNE     *BLANK
040200971021     C                   SETON                                        32
040300971021     C                   MOVE      *ZERO         VABIAS
040400010604     C                   eval      vinmsg = %trimr(vinmsg)
040500010604     C                             + ' ' + 'VHBIAS'
040600971021     C                   ELSE
040700971021     C                   MOVE      WD15          VABIAS
040800971021     C                   ENDIF
040900941229     C*
041000971023     C                   TESTN                   VHBNCL               30
041100971023     C  N30              TESTN                   VHBNCL                 30
041200971023     C  N30              TESTN                   VHBNCL                   30
041300971021     C                   MOVE      VHBNCL        W001A             1
041400971021     C     *IN30         IFEQ      *OFF
041500971021     C     W001A         ORLT      '0'
041600971021     C                   SETON                                        32
041700971021     C                   MOVE      *ZERO         VABNCL
041800010604     C                   eval      vinmsg = %trimr(vinmsg)
041900010604     C                             + ' ' + 'VHBNCL'
042000971021     C                   ELSE
042100971021     C                   MOVE      VHBNCL        VABNCL
042200971021     C                   ENDIF
042300941229     C*
042400971021     C                   MOVE      *ZERO         VABPKB
042500971021     C                   MOVE      VHBPKB        WASC09
042600010719     C                   IF        WI09 = *blanks
042700010719     C                   EVAL      WI09 = *zeros
042800010719     C                   ENDIF
042900971023     C                   TESTN                   WI09                 30
043000971023     C  N30              TESTN                   WI09                   30
043100971023     C  N30              TESTN                   WI09                     30
043200971021     C                   MOVE      WI09          W001A             1
043300971021     C     *IN30         IFEQ      *OFF
043400971021     C     W001A         ORLT      '0'
043500971021     C                   SETON                                        32
043600971021     C                   MOVE      *ZERO         VABPKB
043700010604     C                   eval      vinmsg = %trimr(vinmsg)
043800010604     C                             + ' ' + 'VHBPKB'
043900971021     C                   ELSE
044000971021     C                   MOVEL     WI09          VABPKB
044100971021     C                   ENDIF
044200010719     C                   IF        WD09 = *blanks
044300010719     C                   EVAL      WD09 = *zeros
044400010719     C                   ENDIF
044500971023     C                   TESTN                   WD09                 30
044600971023     C  N30              TESTN                   WD09                     30
044700971021     C                   MOVE      WD09          W001A             1
044800971021     C     *IN30         IFEQ      *OFF
044900971021     C     W001A         ORLT      '0'
045000971023     C     W001A         ANDNE     *BLANK
045100971021     C                   SETON                                        32
045200971021     C                   MOVE      *ZERO         VABPKB
045300010604     C                   eval      vinmsg = %trimr(vinmsg)
045400010604     C                             + ' ' + 'VHBPKB'
045500971021     C                   ELSE
045600971021     C                   MOVE      WD09          VABPKB
045700971021     C                   ENDIF
045800971021     C*
045900971021     C                   MOVE      *ZERO         VABVLB
046000971021     C                   MOVE      VHBVLB        WASC07
046100010719     C                   IF        WI07 = *blanks
046200010719     C                   EVAL      WI07 = *zeros
046300010719     C                   ENDIF
046400971023     C                   TESTN                   WI07                 30
046500971023     C  N30              TESTN                   WI07                   30
046600971023     C  N30              TESTN                   WI07                     30
046700971021     C                   MOVE      WI07          W001A             1
046800971021     C     *IN30         IFEQ      *OFF
046900971021     C     W001A         ORLT      '0'
047000971023     C     W001A         ANDNE     *BLANK
047100971021     C                   SETON                                        32
047200971021     C                   MOVE      *ZERO         VABVLB
047300010604     C                   eval      vinmsg = %trimr(vinmsg)
047400010604     C                             + ' ' + 'VHBVLB'
047500971021     C                   ELSE
047600971021     C                   MOVEL     WI07          VABVLB
047700971021     C                   ENDIF
047800010719     C                   IF        WD07 = *blanks
047900010719     C                   EVAL      WD07 = *zeros
048000010719     C                   ENDIF
048100971023     C                   TESTN                   WD07                 30
048200971023     C  N30              TESTN                   WD07                   30
048300971023     C  N30              TESTN                   WD07                     30
048400971021     C                   MOVE      WD07          W001A             1
048500971021     C     *IN30         IFEQ      *OFF
048600971021     C     W001A         ORLT      '0'
048700971023     C     W001A         ANDNE     *BLANK
048800971021     C                   SETON                                        32
048900971021     C                   MOVE      *ZERO         VABVLB
049000010604     C                   eval      vinmsg = %trimr(vinmsg)
049100010604     C                             + ' ' + 'VHBVLB'
049200971021     C                   ELSE
049300971021     C                   MOVE      WD07          VABVLB
049400971021     C                   ENDIF
049500941229     C*
049600971021     C                   MOVE      *ZERO         VABQFT
049700971021     C                   MOVE      VHBQFT        WASC15
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
050900971021     C                   MOVE      *ZERO         VABQFT
051000010604     C                   eval      vinmsg = %trimr(vinmsg)
051100010604     C                             + ' ' + 'VHBQFT'
051200971021     C                   ELSE
051300971021     C                   MOVEL     WI15          VABQFT
051400971021     C                   ENDIF
051500010719     C                   IF        WD15 = *blanks
051600010719     C                   EVAL      WD15 = *zeros
051700010719     C                   ENDIF
051800971023     C                   TESTN                   WD15                 30
051900971023     C  N30              TESTN                   WD15                   30
052000971023     C  N30              TESTN                   WD15                     30
052100971021     C                   MOVE      WD15          W001A             1
052200971021     C     *IN30         IFEQ      *OFF
052300971021     C     W001A         ORLT      '0'
052400971023     C     W001A         ANDNE     *BLANK
052500971021     C                   SETON                                        32
052600971021     C                   MOVE      *ZERO         VABQFT
052700010604     C                   eval      vinmsg = %trimr(vinmsg)
052800010604     C                             + ' ' + 'VHBQFT'
052900971021     C                   ELSE
053000971021     C                   MOVE      WD15          VABQFT
053100971021     C                   ENDIF
053200941229     C*
053300971021     C                   MOVE      *ZERO         VABCAS
053400971021     C                   MOVE      VHBCAS        WASC15
053500010719     C                   IF        WI15 = *blanks
053600010719     C                   EVAL      WI15 = *zeros
053700010719     C                   ENDIF
053800971023     C                   TESTN                   WI15                 30
053900971023     C  N30              TESTN                   WI15                   30
054000971023     C  N30              TESTN                   WI15                     30
054100971021     C                   MOVE      WI15          W001A             1
054200971021     C     *IN30         IFEQ      *OFF
054300971021     C     W001A         ORLT      '0'
054400971023     C     W001A         ANDNE     *BLANK
054500971021     C                   SETON                                        32
054600971021     C                   MOVE      *ZERO         VABCAS
054700010604     C                   eval      vinmsg = %trimr(vinmsg)
054800010604     C                             + ' ' + 'VHBCAS'
054900971021     C                   ELSE
055000971021     C                   MOVEL     WI15          VABCAS
055100971021     C                   ENDIF
055200010719     C                   IF        WD15 = *blanks
055300010719     C                   EVAL      WD15 = *zeros
055400010719     C                   ENDIF
055500971023     C                   TESTN                   WD15                 30
055600971023     C  N30              TESTN                   WD15                   30
055700971023     C  N30              TESTN                   WD15                     30
055800971021     C                   MOVE      WD15          W001A             1
055900971021     C     *IN30         IFEQ      *OFF
056000971021     C     W001A         ORLT      '0'
056100971023     C     W001A         ANDNE     *BLANK
056200971021     C                   SETON                                        32
056300971021     C                   MOVE      *ZERO         VABCAS
056400010604     C                   eval      vinmsg = %trimr(vinmsg)
056500010604     C                             + ' ' + 'VHBCAS'
056600971021     C                   ELSE
056700971021     C                   MOVE      WD15          VABCAS
056800971021     C                   ENDIF
056900971021     C*
057000971023     C                   TESTN                   VHBRMN               30
057100971023     C  N30              TESTN                   VHBRMN                 30
057200971023     C  N30              TESTN                   VHBRMN                   30
057300971021     C                   MOVE      VHBRMN        W001A             1
057400971021     C     *IN30         IFEQ      *OFF
057500971021     C     W001A         ORLT      '0'
057600971021     C                   SETON                                        32
057700971021     C                   MOVE      *ZERO         VABRMN
057800010604     C                   eval      vinmsg = %trimr(vinmsg)
057900010604     C                             + ' ' + 'VHBRMN'
058000971021     C                   ELSE
058100971021     C                   MOVE      VHBRMN        VABRMN
058200971021     C                   ENDIF
058300971021     C*
058400971023     C                   TESTN                   VHBNCD               30
058500971023     C  N30              TESTN                   VHBNCD                 30
058600971023     C  N30              TESTN                   VHBNCD                   30
058700971021     C                   MOVE      VHBNCD        W001A             1
058800971021     C     *IN30         IFEQ      *OFF
058900971021     C     W001A         ORLT      '0'
059000971023     C     W001A         ANDNE     *BLANK
059100971021     C                   SETON                                        32
059200971021     C                   MOVE      *ZERO         VABNCD
059300010604     C                   eval      vinmsg = %trimr(vinmsg)
059400010604     C                             + ' ' + 'VHBNCD'
059500971021     C                   ELSE
059600971021     C                   MOVE      VHBNCD        VABNCD
059700971021     C                   ENDIF
059800971021     C*
059900971023     C                   TESTN                   VHBNCA               30
060000971023     C  N30              TESTN                   VHBNCA                 30
060100971023     C  N30              TESTN                   VHBNCA                   30
060200971021     C                   MOVE      VHBNCA        W001A             1
060300971021     C     *IN30         IFEQ      *OFF
060400971021     C     W001A         ORLT      '0'
060500971023     C     W001A         ANDNE     *BLANK
060600971021     C                   SETON                                        32
060700971021     C                   MOVE      *ZERO         VABNCA
060800010604     C                   eval      vinmsg = %trimr(vinmsg)
060900010604     C                             + ' ' + 'VHBNCA'
061000971021     C                   ELSE
061100971021     C                   MOVE      VHBNCA        VABNCA
061200971021     C                   ENDIF
061300971021     C*
061400971023     C                   TESTN                   VHBZNC               30
061500971023     C  N30              TESTN                   VHBZNC                 30
061600971023     C  N30              TESTN                   VHBZNC                   30
061700971021     C                   MOVE      VHBZNC        W001A             1
061800971021     C     *IN30         IFEQ      *OFF
061900971021     C     W001A         ORLT      '0'
062000971023     C     W001A         ANDNE     *BLANK
062100971021     C                   SETON                                        32
062200971021     C                   MOVE      *ZERO         VABZNC
062300010604     C                   eval      vinmsg = %trimr(vinmsg)
062400010604     C                             + ' ' + 'VHBZNC'
062500971021     C                   ELSE
062600971021     C                   MOVE      VHBZNC        VABZNC
062700971021     C                   ENDIF
062800971021     C*
062900971023     C                   TESTN                   VHBDCR               30
063000971023     C  N30              TESTN                   VHBDCR                 30
063100971023     C  N30              TESTN                   VHBDCR                   30
063200971021     C                   MOVE      VHBDCR        W001A             1
063300971021     C     *IN30         IFEQ      *OFF
063400971021     C     W001A         ORLT      '0'
063500971023     C     W001A         ANDNE     *BLANK
063600971021     C                   SETON                                        32
063700971021     C                   MOVE      *ZERO         VABDCR
063800010604     C                   eval      vinmsg = %trimr(vinmsg)
063900010604     C                             + ' ' + 'VHBDCR'
064000971021     C                   ELSE
064100971021     C                   MOVE      VHBDCR        VABDCR
064200971021     C                   ENDIF
064300971021     C*
064400971023     C                   TESTN                   VHBHCR               30
064500971023     C  N30              TESTN                   VHBHCR                 30
064600971023     C  N30              TESTN                   VHBHCR                   30
064700971021     C                   MOVE      VHBHCR        W001A             1
064800971021     C     *IN30         IFEQ      *OFF
064900971021     C     W001A         ORLT      '0'
065000971023     C     W001A         ANDNE     *BLANK
065100971021     C                   SETON                                        32
065200971021     C                   MOVE      *ZERO         VABHCR
065300010604     C                   eval      vinmsg = %trimr(vinmsg)
065400010604     C                             + ' ' + 'VHBHCR'
065500971021     C                   ELSE
065600971021     C                   MOVE      VHBHCR        VABHCR
065700971021     C                   ENDIF
065800941229     C*
065900971021     C                   MOVE      *ZERO         VABVMD
066000971021     C                   MOVE      VHBVMD        WASC15
066100010719     C                   IF        WI15 = *blanks
066200010719     C                   EVAL      WI15 = *zeros
066300010719     C                   ENDIF
066400971023     C                   TESTN                   WI15                 30
066500971023     C  N30              TESTN                   WI15                   30
066600971023     C  N30              TESTN                   WI15                     30
066700971021     C                   MOVE      WI15          W001A             1
066800971021     C     *IN30         IFEQ      *OFF
066900971021     C     W001A         ORLT      '0'
067000971023     C     W001A         ANDNE     *BLANK
067100971021     C                   SETON                                        32
067200971021     C                   MOVE      *ZERO         VABVMD
067300010604     C                   eval      vinmsg = %trimr(vinmsg)
067400010604     C                             + ' ' + 'VHBVMD'
067500971021     C                   ELSE
067600971021     C                   MOVEL     WI15          VABVMD
067700971021     C                   ENDIF
067800010719     C                   IF        WD15 = *blanks
067900010719     C                   EVAL      WD15 = *zeros
068000010719     C                   ENDIF
068100971023     C                   TESTN                   WD15                 30
068200971023     C  N30              TESTN                   WD15                   30
068300971023     C  N30              TESTN                   WD15                     30
068400971021     C                   MOVE      WD15          W001A             1
068500971021     C     *IN30         IFEQ      *OFF
068600971021     C     W001A         ORLT      '0'
068700971023     C     W001A         ANDNE     *BLANK
068800971021     C                   SETON                                        32
068900971021     C                   MOVE      *ZERO         VABVMD
069000010604     C                   eval      vinmsg = %trimr(vinmsg)
069100010604     C                             + ' ' + 'VHBVMD'
069200971021     C                   ELSE
069300971021     C                   MOVE      WD15          VABVMD
069400971021     C                   ENDIF
069500971021     C*
069600971023     C                   TESTN                   VHBANT               30
069700971023     C  N30              TESTN                   VHBANT                 30
069800971023     C  N30              TESTN                   VHBANT                   30
069900971021     C                   MOVE      VHBANT        W001A             1
070000971021     C     *IN30         IFEQ      *OFF
070100971021     C     W001A         ORLT      '0'
070200971023     C     W001A         ANDNE     *BLANK
070300971021     C                   SETON                                        32
070400971021     C                   MOVE      *ZERO         VABANT
070500010604     C                   eval      vinmsg = %trimr(vinmsg)
070600010604     C                             + ' ' + 'VHBANT'
070700971021     C                   ELSE
070800971021     C                   MOVE      VHBANT        VABANT
070900971021     C                   ENDIF
071000971021     C*
071100971021     C* CAMPI ALFABETICI:
071200971021     C                   MOVE      VHBATB        VABATB
071300971021     C                   MOVE      VHBCBO        VABCBO
071400971021     C                   MOVE      VHBRSD        VABRSD
071500020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
071600020117     C     '@':'A'       XLATE     VABRSD        VABRSD
071700020117     C* ==
071800971021     C                   MOVE      VHBRD2        VABRD2
071900971021     C                   MOVE      VHBIND        VABIND
072000971021     C                   MOVE      VHBCAD        VABCAD
072100971021     C                   MOVE      VHBLOD        VABLOD
072200971021     C                   MOVE      VHBPRD        VABPRD
072300971021     C                   MOVE      VHBNZD        VABNZD
072400971021     C                   MOVE      VHBGC1        VABGC1
072500971021     C                   MOVE      VHBGC2        VABGC2
072600971021     C                   MOVE      VHBTSP        VABTSP
072700971021     C                   MOVE      VHBVAS        VABVAS
072800971021     C                   MOVE      VHBNAS        VABNAS
072900971021     C                   MOVE      VHBTIC        VABTIC
073000971021     C                   MOVE      VHBVCA        VABVCA
073100971021     C                   MOVE      VHBGCA        VABGCA
073200971021     C                   MOVE      VHBRMA        VABRMA
073300971021     C                   MOVE      VHBXCO        VABXCO
073400971021     C                   MOVE      VHBNOT        VABNOT
073500971021     C                   MOVE      VHBNT2        VABNT2
073600971021     C                   MOVE      VHBCTM        VABCTM
073700971021     C                   MOVE      VHBFFD        VABFFD
073800971021     C                   MOVE      VHBTCR        VABTCR
073900971021     C                   MOVE      VHBCTS        VABCTS
074000971021     C                   MOVE      VHBFTM        VABFTM
074100971021     C                   MOVE      VHBVAD        VABVAD
074200971021     C                   MOVE      VHBGMA        VABGMA
074300971021     C                   MOVE      VHBGGA        VABGGA
074400971021     C                   MOVE      VHBGVA        VABGVA
074500971021     C                   MOVE      VHBTC1        VABTC1
074600971021     C                   MOVE      VHBTC2        VABTC2
074700971021     C                   MOVE      VHBSCL        VABSCL
074800971021     C                   MOVE      VHBRMO        VABRMO
074900971021     C                   MOVE      VHBCMO        VABCMO
075000971021     C                   MOVE      VHBNMO        VABNMO
075100971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
075200971021     C* NELLA NAZIONE ORIGINALE
075300971021     C                   MOVE      VHBNMO        WA1D              1
075400971021     C                   MOVEL     VHBNMO        WA1S              1
075500971021     C     WA1D          IFNE      *BLANK
075600971021     C     WA1S          ANDEQ     *BLANK
075700971021     C                   MOVE      *BLANK        VABNMO
075800971021     C                   END
075900930409     C*
076000010604     C  N31              ADD       1             §CTROK            7 0
076100010604     C   32              ADD       1             §CTRMO            7 0
076200010604     C   31              ADD       1             §CTRNO            7 0
076300020913     C  N31              WRITE     FIVAB000
076400990910     C*
076500010601     C                   if        *in31 = *off and
076600010601     C                             *in32 = *off
076700990910     C                   eval      vinflg = '1'
076800990910     C                   else
076900000621     C                   eval      recko = vindta
077000010612     C                   if        %open(tis7t5ps)
077100030407     C   50              except    rigadett
077200010612     C                   endif
077300990910     C                   eval      vinflg = '2'
077400990910     C                   endif
077500991022     C                   endif
077600990910     C                   endif
077700010601     C*
077800010601     C                   endif
077900000905     C*
078000000905     C                   else
078100000905     C                   eval      vinflg = '1'
078200000905     C                   endif
078300000905     C*
078400000905     C  N70              update    tivin000
078500991022     C*
078600991022     C  N70              ENDdo
078700010530     C
078800010530     C*
078900010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
079000010530     C                   if        cntNonEl = *zeros or
079100010530     C                             flgMulti = '0'
079200990910     C* Se non ci sono record con errori ...
079300010530     C                   if        §ctrno = 0 and
079400010601     C                             §ctrmo = 0 and
079500010530     C                             flgStato <> '2'
079600990910     C* ... restituisco esito OK.
079700990921     C                   eval      wrkesito = '0'
079800990910     C                   else
079900010601     C                   if        §ctrok > 0
080000990921     C                   eval      wrkesito = '1'
080100000710     C                   else
080200010615     C                   if        flgOk = '0'
080300010615     C                   eval      wrkesito = '2'
080400010615     C                   else
080500010615     C                   eval      wrkesito = '6'
080600010615     C                   endif
080700990910     C                   endif
080800000710     C                   endif
080900010530     C                   else
081000010530     C                   eval      wrkesito = '9'
081100010530     C                   endif
081200990910     C*
081300990914     C                   if        %open(tivin00r)
081400990908     C                   close     tivin00r
081500990914     C                   endif
081600020913     C                   if        %open(fivabwwr)
081700020913     C                   close     fivabwwr
081800990914     C                   endif
081900010530     C*
082000010530     C                   if        vlrpoi <> 999
082100010530     C                   eval      invfil = vlrpoi
082200010530     C                   endif
082300990910     C*
082400990920     C                   if        §ctrok > 0
082500010530     C                             and invfil > *zeros
082600000613     C                   exsr      invio
082700990920     C                   endif
082800010612     C*
082900010612     C                   if        flgGiro = '1'
083000010612     C                   exsr      endela
083100010612     C                   endif
083200990920     C*
083300910830     C                   ENDSR
083400000613     C***
083500990920
083600010530
083700010530
083800010530
083900010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
084000010530     C     repfil        BEGSR
084100010530     C*
084200010530     C                   if        invfil = *zeros and
084300010530     C                             depfil > *zeros and
084400010613     C                             (vinflg = *blanks or
084500010613     C                              vinflg = *zeros)
084600010530     C
084700010530     C                   eval      invfil = depfil
084800010530     C                   endif
084900010530     C*
085000010530     C                   if        depfil <> invfil and
085100010530     C                             invfil > *zeros
085200010530     C                   eval      flgMulti = '1'
085300010530     C                   if        vinflg = *blanks
085400010530     C                   add       1             cntNonEl
085500010530     C                   endif
085600010530     C                   endif
085700010530     C*
085800010530     C                   if        vinflg = '2'
085900010530     C                   eval      flgStato = '2'
086000010530     C                   endif
086100010530     C*
086200010530     C                   ENDSR
086300010530     C***
086400010530
086500010530
086600010530
086700010530
086800990920      /TITLE Invio dei dati al punto operativo.
086900000613     C     invio         BEGSR
087000990920     C*
087100990920     C                   reset                   dscmz
087200010530     C                   move      invfil        cmzdst
087300990920     C                   eval      cmzfld = vlrfou
087400990920     C                   eval      cmzmbd = vlrhdl
087500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
087600000710     C                   if        prmfir = *blanks
087700020913     C                   eval      cmzfla = 'FIVAB00F'
087800020913     C                   eval      cmzmba = 'FIVAB00F'
087900000710     C                   else
088000000710     C                   eval      cmzfla = prmfir
088100000710     C                   eval      cmzmba = prmfir
088200000710     C                   endif
088300990920     C                   eval      cmznrr = *zeros
088400990920     C                   move      §ctrok        cmznrr
088500021003     C                   eval      cmzlba = vlrfl1
088600990920     C                   call(e)   'TIS711C'
088700990920     C                   parm                    dscmz
088800990921     C                   parm      *blanks       esito
088900990920     C                   if        %error
089000990920     C                             or cmzerr = '1'
089100990921     C                             or esito  = '1'
089200000710     C                   eval      wrkesito = '3'
089300990920     C                   endif
089400990920     C*
089500000613     C                   ENDSR
089600000613     C***
089700010530
089800010530
089900010530
090000010530
090100010530      /TITLE Invio dei dati al punto operativo.
090200010530     C     opeini        BEGSR
090300010530     C*
090400010530     C* Inizializzo flag e contatori operativi
090500010530     C                   movel     '0'           flgGiro           1
090600010530     C                   movel     '0'           flgMulti          1
090700010530     C                   movel     '1'           flgStato          1
090800010615     C                   movel     '0'           flgOk             1
090900010530     C                   z-add     *zeros        cntNonEl         10 0
091000010530     C                   z-add     *zeros        depfil            3 0
091100010530     C                   z-add     *zeros        invfil            3 0
091200030407     C*
091300030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
091400030407     C                   if        %subst(vlrppt:1:1) = 'S'
091500030407     C                   seton                                        50
091600030407     C                   else
091700030407     C                   setoff                                       50
091800030407     C                   endif
091900010530     C*
092000010530     C                   ENDSR
092100010530     C***
092200040407
092300040407
092400040407
092500040407
092600040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
092700040407     C     exeBon        BEGSR
092800040407     C*
092900040407     C* Imposto i parametri x il *pgm d "bonifica"
093000040407     C                   EVAL      TxtInOut   = fnvabaar
093100040407     C                   EVAL      ElencoChar = *blanks
093200040407     C                   EVAL      TipoElenco = *blanks
093300040407     C                   EVAL      CharSost   = *blanks
093400040407     C                   EVAL      UpperCase  = '1'
093500040407     C                   EVAL      ChkNull    = '1'
093600040407     C                   EVAL      CharNull   = *blanks
093700040407     C                   EVAL      EsitoBon   = *blanks
093800040407     C*
093900040407     C* Effettuo la chiamata al *pgm d "bonifica"
094000040407     C                   CALL      'XCHKCHAR'
094100040407     C                   PARM                    TxtInOut
094200040407     C                   PARM                    ElencoChar
094300040407     C                   PARM                    TipoElenco
094400040407     C                   PARM                    CharSost
094500040407     C                   PARM                    UpperCase
094600040407     C                   PARM                    ChkNull
094700040407     C                   PARM                    CharNull
094800040407     C                   PARM                    EsitoBon
094900040407     C*
095000040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
095100040407     C                   IF        EsitoBon = '1'
095200040407     C                   EVAL      fnvabaar = TxtInOut
095300040407     C                   ENDIF
095400040407     C*
095500040407     C                   ENDSR
095600010530
095700010530
095800010530
095900990910
096000000613     C     *inzsr        BEGSR
096100990910     C*
096200990910     C     *entry        plist
096300990920     C                   parm                    tivlrds
096400990921     C                   parm      wrkesito      esito
096500000724     C                   parm                    prmlit
096600000710     C                   parm                    prmfir
096700010606     C*
096800010606     C* CALCOLA LA DATA CORRENTE
096900010606     C                   time                    wn14             14 0
097000010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
097100010606     C                   z-add     wn8           g08dat
097200010606     C                   z-add     *zeros        g08inv
097300010606     C                   movel     '0'           g08err
097400010606     C                   call      'XSRDA8'
097500010606     C                   parm                    wlbda8
097600010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
097700000613     C*
097800000613     C                   ENDSR
097900000613     C***
098000990908
098100010601     Otis7t5p   E            riepilogo         2
098200990915     O                                              'Upload via Internet'
098300990915     O                                           +1 'Traduzione TIVIN00R -'
098400020913     O                                           +1 'FIVABWWR'
098500990915     O                                           +1 'Testate bolle'
098600990915     O          E            riepilogo   2
098700990915     O                       wrkdata
098800990915     O                       wrkora              +1
098900990915     O                       procname            +1
099000990915     O          E            riepilogo   2
099100990915     O                                              'Cliente..................:'
099200990915     O                       VABCCM        z     +1
099300990915     O          E            riepilogo   2
099400990920     O                                              'Riferimento Strategi.....:'
099500990920     O                       vlrhdl              +1
099600990915     O          E            riepilogo   2
099700990915     O                                              'Giusti...................:'
099800971022     O                       §CTROK        2   +  1
099900990915     O          E            riepilogo   2
100000990915     O                                              'Sbagliati e corretti.....:'
100100971022     O                       §CTRMO        2   +  1
100200990915     O          E            riepilogo   2
100300990915     O                                              'Sbagliati e scartati.....:'
100400971022     O                       §CTRNO        2   +  1
100500000613
100600010601     Otis7t5ps  E            testdett          2
100700000613     O                                              'Upload via Internet'
100800000613     O                                           +1 'Traduzione TIVIN00R -'
100900020913     O                                           +1 'FIVABWWR'
101000000613     O                                           +1 'Testate bolle'
101100000616     O          E            testdett    3
101200000613     O                                           +2 'N° rec'
101300000613     O                                           +3 'Anteprima contenuto'
101400000616     O          E            rigadett    2
101500000613     O                       rrnum               +2
101600000621     O                       recko               +3
101700000616     O          E            findett     2
101800000613     O                       wrkdata
101900000613     O                       wrkora              +1
102000000613     O                       procname            +1
102100000616     O          E            findett     2
102200000613     O                                              'Cliente..................:'
102300000613     O                       VABCCM        z     +1
102400000616     O          E            findett     2
102500000613     O                                              'Riferimento Strategi.....:'
102600000613     O                       vlrhdl              +1
102700000616     O          E            findett     2
102800000613     O                                              'Giusti...................:'
102900000613     O                       §CTROK        2   +  1
103000000616     O          E            findett     2
103100000613     O                                              'Sbagliati e corretti.....:'
103200000613     O                       §CTRMO        2   +  1
103300000616     O          E            findett     2
103400000613     O                                              'Sbagliati e scartati.....:'
103500000613     O                       §CTRNO        2   +  1
103600000512** CMD - COMANDI CL
103700011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T5P)  FORMTYPE(RICCLI) OUTQ(
103800011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T5PS) FORMTYPE(RICCLI) OUTQ(
103900010601DLTOVR FILE(TIS7T5P TIS7T5PS) LVL(*)
104000000512
104100000512
