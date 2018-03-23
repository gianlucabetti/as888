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
022900991022     C*
023000991022     C                   testb     x'3F'         vhbatb                   02
023100991022     C                   if        not *in02
023200991022     C*
023300971021     C                   SETOFF                                       303132
023400971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
023500971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
023600971023     C*    OPPURE TUTTI BLANKS
023700010530     C*
023800010530     C                   TESTN                   VHBLNP               30
023900010530     C  N30              TESTN                   VHBLNP                 30
024000010530     C  N30              TESTN                   VHBLNP                   30
024100010530     C                   MOVE      VHBLNP        W001A             1
024200010530     C     *IN30         IFEQ      *OFF
024300010530     C     W001A         ORLT      '0'
024400010530     C                   SETON                                        31
024500010530     C                   eval      vinmsg = %trimr(vinmsg)
024600010530     C                             + ' ' + 'VHBLNP'
024700010530     C                   ELSE
024800010530     C                   MOVE      VHBLNP        VABLNP
024900010530     C     VABLNP        IFLE      *ZEROS
025000010530     C                   SETON                                        31
025100010530     C                   eval      vinmsg = %trimr(vinmsg)
025200010530     C                             + ' ' + 'VHBLNP'
025300010530     C                   ENDIF
025400010530     C                   ENDIF
025500010530     C*
025600010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
025700010530     C                   eval      depfil = VABLNP
025800010530     C                   exsr      repfil
025900010601     C                   if        depfil = invfil
026000021014     C                   if        vlrpoi = 999
026100021010     C                   MOVE(P)   invfil        VABFGS
026200021014     C                   else
026300021014     C                   MOVE(P)   vlrpoi        VABFGS
026400021014     C                   endif
026500010530     C*
026600010530     C                   exsr      PREELA
026700010530     C*
026800971023     C                   TESTN                   VHBCCM               30
026900971023     C  N30              TESTN                   VHBCCM                 30
027000971023     C  N30              TESTN                   VHBCCM                   30
027100971021     C                   MOVE      VHBCCM        W001A             1
027200971021     C     *IN30         IFEQ      *OFF
027300971021     C     W001A         ORLT      '0'
027400971021     C                   SETON                                        31
027500010604     C                   eval      vinmsg = %trimr(vinmsg)
027600010604     C                             + ' ' + 'VHBCCM'
027700971021     C                   ELSE
027800971021     C                   MOVE      VHBCCM        VABCCM
027900971024     C     VABCCM        IFLE      *ZEROS
028000971024     C                   SETON                                        31
028100010604     C                   eval      vinmsg = %trimr(vinmsg)
028200010604     C                             + ' ' + 'VHBCCM'
028300971024     C                   ENDIF
028400971021     C                   ENDIF
028500971021     C*
028600971023     C                   TESTN                   VHBAAS               30
028700971023     C  N30              TESTN                   VHBAAS                 30
028800971023     C  N30              TESTN                   VHBAAS                   30
028900971021     C                   MOVE      VHBAAS        W001A             1
029000971021     C     *IN30         IFEQ      *OFF
029100971021     C     W001A         ORLT      '0'
029200971023     C     W001A         ANDNE     *BLANK
029300971021     C                   SETON                                        32
029400010606     C                   MOVEL     datcor        VABAAS
029500010604     C                   eval      vinmsg = %trimr(vinmsg)
029600010604     C                             + ' ' + 'VHBAAS'
029700971021     C                   ELSE
029800971021     C                   MOVE      VHBAAS        VABAAS
029900971021     C                   ENDIF
030000971021     C*
030100971023     C                   TESTN                   VHBMGS               30
030200971023     C  N30              TESTN                   VHBMGS                 30
030300971023     C  N30              TESTN                   VHBMGS                   30
030400971021     C                   MOVE      VHBMGS        W001A             1
030500971021     C     *IN30         IFEQ      *OFF
030600971021     C     W001A         ORLT      '0'
030700971023     C     W001A         ANDNE     *BLANK
030800971021     C                   SETON                                        32
030900010601     C                   MOVE      '0101'        VABMGS
031000010604     C                   eval      vinmsg = %trimr(vinmsg)
031100010604     C                             + ' ' + 'VHBMGS'
031200971021     C                   ELSE
031300971021     C                   MOVE      VHBMGS        VABMGS
031400971021     C                   ENDIF
031500971021     C*
031600971023     C                   TESTN                   VHBNRS               30
031700971023     C  N30              TESTN                   VHBNRS                 30
031800971023     C  N30              TESTN                   VHBNRS                   30
031900971021     C                   MOVE      VHBNRS        W001A             1
032000971021     C     *IN30         IFEQ      *OFF
032100971021     C     W001A         ORLT      '0'
032200971023     C     W001A         ANDNE     *BLANK
032300971021     C                   SETON                                        32
032400971021     C                   MOVE      *ZERO         VABNRS
032500010604     C                   eval      vinmsg = %trimr(vinmsg)
032600010604     C                             + ' ' + 'VHBNRS'
032700971021     C                   ELSE
032800971021     C                   MOVE      VHBNRS        VABNRS
032900971021     C                   ENDIF
033000971021     C*
033100971023     C                   TESTN                   VHBNSP               30
033200971023     C  N30              TESTN                   VHBNSP                 30
033300971023     C  N30              TESTN                   VHBNSP                   30
033400971021     C                   MOVE      VHBNSP        W001A             1
033500971021     C     *IN30         IFEQ      *OFF
033600971021     C     W001A         ORLT      '0'
033700971023     C     W001A         ANDNE     *BLANK
033800971021     C                   SETON                                        32
033900971021     C                   MOVE      *ZERO         VABNSP
034000010604     C                   eval      vinmsg = %trimr(vinmsg)
034100010604     C                             + ' ' + 'VHBNSP'
034200971021     C                   ELSE
034300971021     C                   MOVE      VHBNSP        VABNSP
034400971021     C                   ENDIF
034500971021     C*
034600971023     C                   TESTN                   VHBLNA               30
034700971023     C  N30              TESTN                   VHBLNA                 30
034800971023     C  N30              TESTN                   VHBLNA                   30
034900971021     C                   MOVE      VHBLNA        W001A             1
035000971021     C     *IN30         IFEQ      *OFF
035100971021     C     W001A         ORLT      '0'
035200971023     C     W001A         ANDNE     *BLANK
035300971021     C                   SETON                                        32
035400971021     C                   MOVE      *ZERO         VABLNA
035500010604     C                   eval      vinmsg = %trimr(vinmsg)
035600010604     C                             + ' ' + 'VHBLNA'
035700971021     C                   ELSE
035800971021     C                   MOVE      VHBLNA        VABLNA
035900971021     C                   ENDIF
036000971021     C*
036100971023     C                   TESTN                   VHBCTR               30
036200971023     C  N30              TESTN                   VHBCTR                 30
036300971023     C  N30              TESTN                   VHBCTR                   30
036400971021     C                   MOVE      VHBCTR        W001A             1
036500971021     C     *IN30         IFEQ      *OFF
036600971021     C     W001A         ORLT      '0'
036700971023     C     W001A         ANDNE     *BLANK
036800971021     C                   SETON                                        32
036900971021     C                   MOVE      *ZERO         VABCTR
037000010604     C                   eval      vinmsg = %trimr(vinmsg)
037100010604     C                             + ' ' + 'VHBCTR'
037200971021     C                   ELSE
037300971021     C                   MOVE      VHBCTR        VABCTR
037400971021     C                   ENDIF
037500941229     C*
037600971021     C                   MOVE      *ZERO         VABIAS
037700941229     C                   MOVE      VHBIAS        WASC15
037800971023     C                   TESTN                   WI15                 30
037900971023     C  N30              TESTN                   WI15                   30
038000971023     C  N30              TESTN                   WI15                     30
038100971021     C                   MOVE      WI15          W001A             1
038200971021     C     *IN30         IFEQ      *OFF
038300971021     C     W001A         ORLT      '0'
038400971023     C     W001A         ANDNE     *BLANK
038500971021     C                   SETON                                        32
038600971021     C                   MOVE      *ZERO         VABIAS
038700010604     C                   eval      vinmsg = %trimr(vinmsg)
038800010604     C                             + ' ' + 'VHBIAS'
038900971021     C                   ELSE
039000971021     C                   MOVEL     WI15          VABIAS
039100971021     C                   ENDIF
039200971023     C                   TESTN                   WD15                 30
039300971023     C  N30              TESTN                   WD15                   30
039400971023     C  N30              TESTN                   WD15                     30
039500971021     C                   MOVE      WD15          W001A             1
039600971021     C     *IN30         IFEQ      *OFF
039700971021     C     W001A         ORLT      '0'
039800971023     C     W001A         ANDNE     *BLANK
039900971021     C                   SETON                                        32
040000971021     C                   MOVE      *ZERO         VABIAS
040100010604     C                   eval      vinmsg = %trimr(vinmsg)
040200010604     C                             + ' ' + 'VHBIAS'
040300971021     C                   ELSE
040400971021     C                   MOVE      WD15          VABIAS
040500971021     C                   ENDIF
040600941229     C*
040700971023     C                   TESTN                   VHBNCL               30
040800971023     C  N30              TESTN                   VHBNCL                 30
040900971023     C  N30              TESTN                   VHBNCL                   30
041000971021     C                   MOVE      VHBNCL        W001A             1
041100971021     C     *IN30         IFEQ      *OFF
041200971021     C     W001A         ORLT      '0'
041300971021     C                   SETON                                        32
041400971021     C                   MOVE      *ZERO         VABNCL
041500010604     C                   eval      vinmsg = %trimr(vinmsg)
041600010604     C                             + ' ' + 'VHBNCL'
041700971021     C                   ELSE
041800971021     C                   MOVE      VHBNCL        VABNCL
041900971021     C                   ENDIF
042000941229     C*
042100040706     C*** PKB
042200040706     C                   EVAL      PiStr=%trim(%subst(vinDTA:243:9))
042300040706     C                   EXSR      CHKNUM
042400040706     C                   IF        PiNum=*on
042500040706     C                   Z-ADD(H)  PiVal         VABPKB
042600040706     C                   ELSE
042700040706     C                   SETON                                        32
042800040706     C                   EVAL      VABPKB = *zeros
042900040706     C                   EVAL      vinmsg = %trimr(vinmsg)
043000040706     C                             + ' ' + 'VABRMN'
043100040706     C                   ENDIF
043200971021     C*
043300971021     C                   MOVE      *ZERO         VABVLB
043400971021     C                   MOVE      VHBVLB        WASC07
043500010719     C                   IF        WI07 = *blanks
043600010719     C                   EVAL      WI07 = *zeros
043700010719     C                   ENDIF
043800971023     C                   TESTN                   WI07                 30
043900971023     C  N30              TESTN                   WI07                   30
044000971023     C  N30              TESTN                   WI07                     30
044100971021     C                   MOVE      WI07          W001A             1
044200971021     C     *IN30         IFEQ      *OFF
044300971021     C     W001A         ORLT      '0'
044400971023     C     W001A         ANDNE     *BLANK
044500971021     C                   SETON                                        32
044600971021     C                   MOVE      *ZERO         VABVLB
044700010604     C                   eval      vinmsg = %trimr(vinmsg)
044800010604     C                             + ' ' + 'VHBVLB'
044900971021     C                   ELSE
045000971021     C                   MOVEL     WI07          VABVLB
045100971021     C                   ENDIF
045200010719     C                   IF        WD07 = *blanks
045300010719     C                   EVAL      WD07 = *zeros
045400010719     C                   ENDIF
045500971023     C                   TESTN                   WD07                 30
045600971023     C  N30              TESTN                   WD07                   30
045700971023     C  N30              TESTN                   WD07                     30
045800971021     C                   MOVE      WD07          W001A             1
045900971021     C     *IN30         IFEQ      *OFF
046000971021     C     W001A         ORLT      '0'
046100971023     C     W001A         ANDNE     *BLANK
046200971021     C                   SETON                                        32
046300971021     C                   MOVE      *ZERO         VABVLB
046400010604     C                   eval      vinmsg = %trimr(vinmsg)
046500010604     C                             + ' ' + 'VHBVLB'
046600971021     C                   ELSE
046700971021     C                   MOVE      WD07          VABVLB
046800971021     C                   ENDIF
046900941229     C*
047000971021     C                   MOVE      *ZERO         VABQFT
047100971021     C                   MOVE      VHBQFT        WASC15
047200010719     C                   IF        WI15 = *blanks
047300010719     C                   EVAL      WI15 = *zeros
047400010719     C                   ENDIF
047500971023     C                   TESTN                   WI15                 30
047600971023     C  N30              TESTN                   WI15                   30
047700971023     C  N30              TESTN                   WI15                     30
047800971021     C                   MOVE      WI15          W001A             1
047900971021     C     *IN30         IFEQ      *OFF
048000971021     C     W001A         ORLT      '0'
048100971023     C     W001A         ANDNE     *BLANK
048200971021     C                   SETON                                        32
048300971021     C                   MOVE      *ZERO         VABQFT
048400010604     C                   eval      vinmsg = %trimr(vinmsg)
048500010604     C                             + ' ' + 'VHBQFT'
048600971021     C                   ELSE
048700971021     C                   MOVEL     WI15          VABQFT
048800971021     C                   ENDIF
048900010719     C                   IF        WD15 = *blanks
049000010719     C                   EVAL      WD15 = *zeros
049100010719     C                   ENDIF
049200971023     C                   TESTN                   WD15                 30
049300971023     C  N30              TESTN                   WD15                   30
049400971023     C  N30              TESTN                   WD15                     30
049500971021     C                   MOVE      WD15          W001A             1
049600971021     C     *IN30         IFEQ      *OFF
049700971021     C     W001A         ORLT      '0'
049800971023     C     W001A         ANDNE     *BLANK
049900971021     C                   SETON                                        32
050000971021     C                   MOVE      *ZERO         VABQFT
050100010604     C                   eval      vinmsg = %trimr(vinmsg)
050200010604     C                             + ' ' + 'VHBQFT'
050300971021     C                   ELSE
050400971021     C                   MOVE      WD15          VABQFT
050500971021     C                   ENDIF
050600941229     C*
050700971021     C                   MOVE      *ZERO         VABCAS
050800971021     C                   MOVE      VHBCAS        WASC15
050900010719     C                   IF        WI15 = *blanks
051000010719     C                   EVAL      WI15 = *zeros
051100010719     C                   ENDIF
051200971023     C                   TESTN                   WI15                 30
051300971023     C  N30              TESTN                   WI15                   30
051400971023     C  N30              TESTN                   WI15                     30
051500971021     C                   MOVE      WI15          W001A             1
051600971021     C     *IN30         IFEQ      *OFF
051700971021     C     W001A         ORLT      '0'
051800971023     C     W001A         ANDNE     *BLANK
051900971021     C                   SETON                                        32
052000971021     C                   MOVE      *ZERO         VABCAS
052100010604     C                   eval      vinmsg = %trimr(vinmsg)
052200010604     C                             + ' ' + 'VHBCAS'
052300971021     C                   ELSE
052400971021     C                   MOVEL     WI15          VABCAS
052500971021     C                   ENDIF
052600010719     C                   IF        WD15 = *blanks
052700010719     C                   EVAL      WD15 = *zeros
052800010719     C                   ENDIF
052900971023     C                   TESTN                   WD15                 30
053000971023     C  N30              TESTN                   WD15                   30
053100971023     C  N30              TESTN                   WD15                     30
053200971021     C                   MOVE      WD15          W001A             1
053300971021     C     *IN30         IFEQ      *OFF
053400971021     C     W001A         ORLT      '0'
053500971023     C     W001A         ANDNE     *BLANK
053600971021     C                   SETON                                        32
053700971021     C                   MOVE      *ZERO         VABCAS
053800010604     C                   eval      vinmsg = %trimr(vinmsg)
053900010604     C                             + ' ' + 'VHBCAS'
054000971021     C                   ELSE
054100971021     C                   MOVE      WD15          VABCAS
054200971021     C                   ENDIF
054300971021     C*
054400971023     C                   TESTN                   VHBRMN               30
054500971023     C  N30              TESTN                   VHBRMN                 30
054600971023     C  N30              TESTN                   VHBRMN                   30
054700971021     C                   MOVE      VHBRMN        W001A             1
054800971021     C     *IN30         IFEQ      *OFF
054900971021     C     W001A         ORLT      '0'
055000971021     C                   SETON                                        32
055100971021     C                   MOVE      *ZERO         VABRMN
055200010604     C                   eval      vinmsg = %trimr(vinmsg)
055300010604     C                             + ' ' + 'VHBRMN'
055400971021     C                   ELSE
055500971021     C                   MOVE      VHBRMN        VABRMN
055600971021     C                   ENDIF
055700971021     C*
055800971023     C                   TESTN                   VHBNCD               30
055900971023     C  N30              TESTN                   VHBNCD                 30
056000971023     C  N30              TESTN                   VHBNCD                   30
056100971021     C                   MOVE      VHBNCD        W001A             1
056200971021     C     *IN30         IFEQ      *OFF
056300971021     C     W001A         ORLT      '0'
056400971023     C     W001A         ANDNE     *BLANK
056500971021     C                   SETON                                        32
056600971021     C                   MOVE      *ZERO         VABNCD
056700010604     C                   eval      vinmsg = %trimr(vinmsg)
056800010604     C                             + ' ' + 'VHBNCD'
056900971021     C                   ELSE
057000971021     C                   MOVE      VHBNCD        VABNCD
057100971021     C                   ENDIF
057200971021     C*
057300971023     C                   TESTN                   VHBNCA               30
057400971023     C  N30              TESTN                   VHBNCA                 30
057500971023     C  N30              TESTN                   VHBNCA                   30
057600971021     C                   MOVE      VHBNCA        W001A             1
057700971021     C     *IN30         IFEQ      *OFF
057800971021     C     W001A         ORLT      '0'
057900971023     C     W001A         ANDNE     *BLANK
058000971021     C                   SETON                                        32
058100971021     C                   MOVE      *ZERO         VABNCA
058200010604     C                   eval      vinmsg = %trimr(vinmsg)
058300010604     C                             + ' ' + 'VHBNCA'
058400971021     C                   ELSE
058500971021     C                   MOVE      VHBNCA        VABNCA
058600971021     C                   ENDIF
058700971021     C*
058800971023     C                   TESTN                   VHBZNC               30
058900971023     C  N30              TESTN                   VHBZNC                 30
059000971023     C  N30              TESTN                   VHBZNC                   30
059100971021     C                   MOVE      VHBZNC        W001A             1
059200971021     C     *IN30         IFEQ      *OFF
059300971021     C     W001A         ORLT      '0'
059400971023     C     W001A         ANDNE     *BLANK
059500971021     C                   SETON                                        32
059600971021     C                   MOVE      *ZERO         VABZNC
059700010604     C                   eval      vinmsg = %trimr(vinmsg)
059800010604     C                             + ' ' + 'VHBZNC'
059900971021     C                   ELSE
060000971021     C                   MOVE      VHBZNC        VABZNC
060100971021     C                   ENDIF
060200971021     C*
060300971023     C                   TESTN                   VHBDCR               30
060400971023     C  N30              TESTN                   VHBDCR                 30
060500971023     C  N30              TESTN                   VHBDCR                   30
060600971021     C                   MOVE      VHBDCR        W001A             1
060700971021     C     *IN30         IFEQ      *OFF
060800971021     C     W001A         ORLT      '0'
060900971023     C     W001A         ANDNE     *BLANK
061000971021     C                   SETON                                        32
061100971021     C                   MOVE      *ZERO         VABDCR
061200010604     C                   eval      vinmsg = %trimr(vinmsg)
061300010604     C                             + ' ' + 'VHBDCR'
061400971021     C                   ELSE
061500971021     C                   MOVE      VHBDCR        VABDCR
061600971021     C                   ENDIF
061700971021     C*
061800971023     C                   TESTN                   VHBHCR               30
061900971023     C  N30              TESTN                   VHBHCR                 30
062000971023     C  N30              TESTN                   VHBHCR                   30
062100971021     C                   MOVE      VHBHCR        W001A             1
062200971021     C     *IN30         IFEQ      *OFF
062300971021     C     W001A         ORLT      '0'
062400971023     C     W001A         ANDNE     *BLANK
062500971021     C                   SETON                                        32
062600971021     C                   MOVE      *ZERO         VABHCR
062700010604     C                   eval      vinmsg = %trimr(vinmsg)
062800010604     C                             + ' ' + 'VHBHCR'
062900971021     C                   ELSE
063000971021     C                   MOVE      VHBHCR        VABHCR
063100971021     C                   ENDIF
063200941229     C*
063300971021     C                   MOVE      *ZERO         VABVMD
063400971021     C                   MOVE      VHBVMD        WASC15
063500010719     C                   IF        WI15 = *blanks
063600010719     C                   EVAL      WI15 = *zeros
063700010719     C                   ENDIF
063800971023     C                   TESTN                   WI15                 30
063900971023     C  N30              TESTN                   WI15                   30
064000971023     C  N30              TESTN                   WI15                     30
064100971021     C                   MOVE      WI15          W001A             1
064200971021     C     *IN30         IFEQ      *OFF
064300971021     C     W001A         ORLT      '0'
064400971023     C     W001A         ANDNE     *BLANK
064500971021     C                   SETON                                        32
064600971021     C                   MOVE      *ZERO         VABVMD
064700010604     C                   eval      vinmsg = %trimr(vinmsg)
064800010604     C                             + ' ' + 'VHBVMD'
064900971021     C                   ELSE
065000971021     C                   MOVEL     WI15          VABVMD
065100971021     C                   ENDIF
065200010719     C                   IF        WD15 = *blanks
065300010719     C                   EVAL      WD15 = *zeros
065400010719     C                   ENDIF
065500971023     C                   TESTN                   WD15                 30
065600971023     C  N30              TESTN                   WD15                   30
065700971023     C  N30              TESTN                   WD15                     30
065800971021     C                   MOVE      WD15          W001A             1
065900971021     C     *IN30         IFEQ      *OFF
066000971021     C     W001A         ORLT      '0'
066100971023     C     W001A         ANDNE     *BLANK
066200971021     C                   SETON                                        32
066300971021     C                   MOVE      *ZERO         VABVMD
066400010604     C                   eval      vinmsg = %trimr(vinmsg)
066500010604     C                             + ' ' + 'VHBVMD'
066600971021     C                   ELSE
066700971021     C                   MOVE      WD15          VABVMD
066800971021     C                   ENDIF
066900971021     C*
067000971023     C                   TESTN                   VHBANT               30
067100971023     C  N30              TESTN                   VHBANT                 30
067200971023     C  N30              TESTN                   VHBANT                   30
067300971021     C                   MOVE      VHBANT        W001A             1
067400971021     C     *IN30         IFEQ      *OFF
067500971021     C     W001A         ORLT      '0'
067600971023     C     W001A         ANDNE     *BLANK
067700971021     C                   SETON                                        32
067800971021     C                   MOVE      *ZERO         VABANT
067900010604     C                   eval      vinmsg = %trimr(vinmsg)
068000010604     C                             + ' ' + 'VHBANT'
068100971021     C                   ELSE
068200971021     C                   MOVE      VHBANT        VABANT
068300971021     C                   ENDIF
068400971021     C*
068500971021     C* CAMPI ALFABETICI:
068600971021     C                   MOVE      VHBATB        VABATB
068700971021     C                   MOVE      VHBCBO        VABCBO
068800971021     C                   MOVE      VHBRSD        VABRSD
068900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
069000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
069100020117     C* ==
069200971021     C                   MOVE      VHBRD2        VABRD2
069300971021     C                   MOVE      VHBIND        VABIND
069400971021     C                   MOVE      VHBCAD        VABCAD
069500971021     C                   MOVE      VHBLOD        VABLOD
069600971021     C                   MOVE      VHBPRD        VABPRD
069700971021     C                   MOVE      VHBNZD        VABNZD
069800971021     C                   MOVE      VHBGC1        VABGC1
069900971021     C                   MOVE      VHBGC2        VABGC2
070000971021     C                   MOVE      VHBTSP        VABTSP
070100971021     C                   MOVE      VHBVAS        VABVAS
070200971021     C                   MOVE      VHBNAS        VABNAS
070300971021     C                   MOVE      VHBTIC        VABTIC
070400971021     C                   MOVE      VHBVCA        VABVCA
070500971021     C                   MOVE      VHBGCA        VABGCA
070600971021     C                   MOVE      VHBRMA        VABRMA
070700971021     C                   MOVE      VHBXCO        VABXCO
070800971021     C                   MOVE      VHBNOT        VABNOT
070900971021     C                   MOVE      VHBNT2        VABNT2
071000971021     C                   MOVE      VHBCTM        VABCTM
071100971021     C                   MOVE      VHBFFD        VABFFD
071200971021     C                   MOVE      VHBTCR        VABTCR
071300971021     C                   MOVE      VHBCTS        VABCTS
071400971021     C                   MOVE      VHBFTM        VABFTM
071500971021     C                   MOVE      VHBVAD        VABVAD
071600971021     C                   MOVE      VHBGMA        VABGMA
071700971021     C                   MOVE      VHBGGA        VABGGA
071800971021     C                   MOVE      VHBGVA        VABGVA
071900971021     C                   MOVE      VHBTC1        VABTC1
072000971021     C                   MOVE      VHBTC2        VABTC2
072100971021     C                   MOVE      VHBSCL        VABSCL
072200971021     C                   MOVE      VHBRMO        VABRMO
072300971021     C                   MOVE      VHBCMO        VABCMO
072400971021     C                   MOVE      VHBNMO        VABNMO
072500971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
072600971021     C* NELLA NAZIONE ORIGINALE
072700971021     C                   MOVE      VHBNMO        WA1D              1
072800971021     C                   MOVEL     VHBNMO        WA1S              1
072900971021     C     WA1D          IFNE      *BLANK
073000971021     C     WA1S          ANDEQ     *BLANK
073100971021     C                   MOVE      *BLANK        VABNMO
073200971021     C                   END
073300040630     C*
073400040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
073500040630     C                   exsr      exeBon
073600930409     C*
073700010604     C  N31              ADD       1             §CTROK            7 0
073800010604     C   32              ADD       1             §CTRMO            7 0
073900010604     C   31              ADD       1             §CTRNO            7 0
074000020913     C  N31              WRITE     FIVAB000
074100990910     C*
074200010601     C                   if        *in31 = *off and
074300010601     C                             *in32 = *off
074400990910     C                   eval      vinflg = '1'
074500990910     C                   else
074600000621     C                   eval      recko = vindta
074700010612     C                   if        %open(tis7t5ps)
074800030407     C   50              except    rigadett
074900010612     C                   endif
075000990910     C                   eval      vinflg = '2'
075100990910     C                   endif
075200991022     C                   endif
075300990910     C                   endif
075400010601     C*
075500010601     C                   endif
075600000905     C*
075700000905     C                   else
075800000905     C                   eval      vinflg = '1'
075900000905     C                   endif
076000000905     C*
076100000905     C  N70              update    tivin000
076200991022     C*
076300991022     C  N70              ENDdo
076400010530     C
076500010530     C*
076600010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
076700010530     C                   if        cntNonEl = *zeros or
076800010530     C                             flgMulti = '0'
076900990910     C* Se non ci sono record con errori ...
077000010530     C                   if        §ctrno = 0 and
077100010601     C                             §ctrmo = 0 and
077200010530     C                             flgStato <> '2'
077300990910     C* ... restituisco esito OK.
077400990921     C                   eval      wrkesito = '0'
077500990910     C                   else
077600010601     C                   if        §ctrok > 0
077700990921     C                   eval      wrkesito = '1'
077800000710     C                   else
077900010615     C                   if        flgOk = '0'
078000010615     C                   eval      wrkesito = '2'
078100010615     C                   else
078200010615     C                   eval      wrkesito = '6'
078300010615     C                   endif
078400990910     C                   endif
078500000710     C                   endif
078600010530     C                   else
078700010530     C                   eval      wrkesito = '9'
078800010530     C                   endif
078900990910     C*
079000990914     C                   if        %open(tivin00r)
079100990908     C                   close     tivin00r
079200990914     C                   endif
079300020913     C                   if        %open(fivabwwr)
079400020913     C                   close     fivabwwr
079500990914     C                   endif
079600010530     C*
079700010530     C                   if        vlrpoi <> 999
079800010530     C                   eval      invfil = vlrpoi
079900010530     C                   endif
080000990910     C*
080100990920     C                   if        §ctrok > 0
080200010530     C                             and invfil > *zeros
080300000613     C                   exsr      invio
080400990920     C                   endif
080500010612     C*
080600010612     C                   if        flgGiro = '1'
080700010612     C                   exsr      endela
080800010612     C                   endif
080900990920     C*
081000910830     C                   ENDSR
081100000613     C***
081200990920
081300010530
081400010530
081500010530
081600010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
081700010530     C     repfil        BEGSR
081800010530     C*
081900010530     C                   if        invfil = *zeros and
082000010530     C                             depfil > *zeros and
082100010613     C                             (vinflg = *blanks or
082200010613     C                              vinflg = *zeros)
082300010530     C
082400010530     C                   eval      invfil = depfil
082500010530     C                   endif
082600010530     C*
082700010530     C                   if        depfil <> invfil and
082800010530     C                             invfil > *zeros
082900010530     C                   eval      flgMulti = '1'
083000010530     C                   if        vinflg = *blanks
083100010530     C                   add       1             cntNonEl
083200010530     C                   endif
083300010530     C                   endif
083400010530     C*
083500010530     C                   if        vinflg = '2'
083600010530     C                   eval      flgStato = '2'
083700010530     C                   endif
083800010530     C*
083900010530     C                   ENDSR
084000010530     C***
084100010530
084200010530
084300010530
084400010530
084500990920      /TITLE Invio dei dati al punto operativo.
084600000613     C     invio         BEGSR
084700990920     C*
084800990920     C                   reset                   dscmz
084900010530     C                   move      invfil        cmzdst
085000990920     C                   eval      cmzfld = vlrfou
085100990920     C                   eval      cmzmbd = vlrhdl
085200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
085300000710     C                   if        prmfir = *blanks
085400020913     C                   eval      cmzfla = 'FIVAB00F'
085500020913     C                   eval      cmzmba = 'FIVAB00F'
085600000710     C                   else
085700000710     C                   eval      cmzfla = prmfir
085800000710     C                   eval      cmzmba = prmfir
085900000710     C                   endif
086000990920     C                   eval      cmznrr = *zeros
086100990920     C                   move      §ctrok        cmznrr
086200021003     C                   eval      cmzlba = vlrfl1
086300990920     C                   call(e)   'TIS711C'
086400990920     C                   parm                    dscmz
086500990921     C                   parm      *blanks       esito
086600990920     C                   if        %error
086700990920     C                             or cmzerr = '1'
086800990921     C                             or esito  = '1'
086900000710     C                   eval      wrkesito = '3'
087000990920     C                   endif
087100990920     C*
087200000613     C                   ENDSR
087300000613     C***
087400010530
087500010530
087600010530
087700010530
087800010530      /TITLE Invio dei dati al punto operativo.
087900010530     C     opeini        BEGSR
088000010530     C*
088100010530     C* Inizializzo flag e contatori operativi
088200010530     C                   movel     '0'           flgGiro           1
088300010530     C                   movel     '0'           flgMulti          1
088400010530     C                   movel     '1'           flgStato          1
088500010615     C                   movel     '0'           flgOk             1
088600010530     C                   z-add     *zeros        cntNonEl         10 0
088700010530     C                   z-add     *zeros        depfil            3 0
088800010530     C                   z-add     *zeros        invfil            3 0
088900030407     C*
089000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
089100030407     C                   if        %subst(vlrppt:1:1) = 'S'
089200030407     C                   seton                                        50
089300030407     C                   else
089400030407     C                   setoff                                       50
089500030407     C                   endif
089600010530     C*
089700010530     C                   ENDSR
089800010530     C***
089900040407
090000040407
090100040407
090200040407
090300040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
090400040407     C     exeBon        BEGSR
090500040407     C*
090600040407     C* Imposto i parametri x il *pgm d "bonifica"
090700040407     C                   EVAL      ElencoChar = *blanks
090800040407     C                   EVAL      TipoElenco = *blanks
090900040407     C                   EVAL      CharSost   = *blanks
091000040629     C                   EVAL      UpperCase  = '1'
091100040407     C                   EVAL      ChkNull    = '1'
091200040407     C                   EVAL      CharNull   = *blanks
091300040629     C*
091400040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
091500040629     C                   EVAL      TxtInOut   = VABRSD
091600040629     C                   EVAL      EsitoBon   = *blanks
091700040407     C*
091800040407     C                   CALL      'XCHKCHAR'
091900040407     C                   PARM                    TxtInOut
092000040407     C                   PARM                    ElencoChar
092100040407     C                   PARM                    TipoElenco
092200040407     C                   PARM                    CharSost
092300040407     C                   PARM                    UpperCase
092400040407     C                   PARM                    ChkNull
092500040407     C                   PARM                    CharNull
092600040407     C                   PARM                    EsitoBon
092700040407     C*
092800040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
092900040407     C                   IF        EsitoBon = '1'
093000040629     C                   EVAL      VABRSD = TxtInOut
093100040407     C                   ENDIF
093200040629     C*
093300040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
093400040629     C                   EVAL      TxtInOut   = VABIND
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
094900040629     C                   EVAL      VABIND = TxtInOut
095000040629     C                   ENDIF
095100040629     C*
095200040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
095300040629     C                   EVAL      TxtInOut   = VABLOD
095400040629     C                   EVAL      EsitoBon   = *blanks
095500040629     C*
095600040629     C                   CALL      'XCHKCHAR'
095700040629     C                   PARM                    TxtInOut
095800040629     C                   PARM                    ElencoChar
095900040629     C                   PARM                    TipoElenco
096000040629     C                   PARM                    CharSost
096100040629     C                   PARM                    UpperCase
096200040629     C                   PARM                    ChkNull
096300040629     C                   PARM                    CharNull
096400040629     C                   PARM                    EsitoBon
096500040629     C*
096600040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
096700040629     C                   IF        EsitoBon = '1'
096800040629     C                   EVAL      VABLOD = TxtInOut
096900040629     C                   ENDIF
097000040629     C*
097100040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
097200040629     C                   EVAL      TxtInOut   = VABNOT
097300040629     C                   EVAL      EsitoBon   = *blanks
097400040630     C                   EVAL      UpperCase  = *blanks
097500040629     C*
097600040629     C                   CALL      'XCHKCHAR'
097700040629     C                   PARM                    TxtInOut
097800040629     C                   PARM                    ElencoChar
097900040629     C                   PARM                    TipoElenco
098000040629     C                   PARM                    CharSost
098100040629     C                   PARM                    UpperCase
098200040629     C                   PARM                    ChkNull
098300040629     C                   PARM                    CharNull
098400040629     C                   PARM                    EsitoBon
098500040629     C*
098600040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
098700040629     C                   IF        EsitoBon = '1'
098800040629     C                   EVAL      VABNOT = TxtInOut
098900040629     C                   ENDIF
099000040629     C*
099100040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
099200040629     C                   EVAL      TxtInOut   = VABNT2
099300040629     C                   EVAL      EsitoBon   = *blanks
099400040630     C                   EVAL      UpperCase  = *blanks
099500040629     C*
099600040629     C                   CALL      'XCHKCHAR'
099700040629     C                   PARM                    TxtInOut
099800040629     C                   PARM                    ElencoChar
099900040629     C                   PARM                    TipoElenco
100000040629     C                   PARM                    CharSost
100100040629     C                   PARM                    UpperCase
100200040629     C                   PARM                    ChkNull
100300040629     C                   PARM                    CharNull
100400040629     C                   PARM                    EsitoBon
100500040629     C*
100600040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
100700040629     C                   IF        EsitoBon = '1'
100800040629     C                   EVAL      VABNT2 = TxtInOut
100900040629     C                   ENDIF
101000040407     C*
101100040407     C                   ENDSR
101200040706
101300040706
101400040706
101500040706     C*----------------------------------------------------*
101600040706     C*  CONTROLLO NUMERICITA' CAMPI
101700040706     C*----------------------------------------------------*
101800040706     C     CHKNUM        BEGSR
101900040706     C*
102000040706     C                   IF        PiDecChr = *blanks
102100040706     C                   EVAL      PiDecChr = ','
102200040706     C                   ENDIF
102300040706     C*
102400040706     C                   CALL(e)   'ISNUMERIC'
102500040706     C                   PARM                    PiStr            30
102600040706     C                   PARM                    PiDecChr          1
102700040706     C                   PARM      *ZEROS        PiVal            30 9
102800040706     C                   PARM      '0'           PiInt             1
102900040706     C                   PARM      '0'           PiNum             1
103000040706     C                   IF        %error
103100040706     C                   EVAL      PiNum=*off
103200040706     C                   ENDIF
103300040706     C*
103400040706     C                   ENDSR
103500040706     C***
103600010530
103700010530
103800010530
103900990910
104000000613     C     *inzsr        BEGSR
104100990910     C*
104200990910     C     *entry        plist
104300990920     C                   parm                    tivlrds
104400990921     C                   parm      wrkesito      esito
104500000724     C                   parm                    prmlit
104600000710     C                   parm                    prmfir
104700010606     C*
104800010606     C* CALCOLA LA DATA CORRENTE
104900010606     C                   time                    wn14             14 0
105000010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
105100010606     C                   z-add     wn8           g08dat
105200010606     C                   z-add     *zeros        g08inv
105300010606     C                   movel     '0'           g08err
105400010606     C                   call      'XSRDA8'
105500010606     C                   parm                    wlbda8
105600010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
105700000613     C*
105800000613     C                   ENDSR
105900000613     C***
106000990908
106100010601     Otis7t5p   E            riepilogo         2
106200990915     O                                              'Upload via Internet'
106300990915     O                                           +1 'Traduzione TIVIN00R -'
106400020913     O                                           +1 'FIVABWWR'
106500990915     O                                           +1 'Testate bolle'
106600990915     O          E            riepilogo   2
106700990915     O                       wrkdata
106800990915     O                       wrkora              +1
106900990915     O                       procname            +1
107000990915     O          E            riepilogo   2
107100990915     O                                              'Cliente..................:'
107200990915     O                       VABCCM        z     +1
107300990915     O          E            riepilogo   2
107400990920     O                                              'Riferimento Strategi.....:'
107500990920     O                       vlrhdl              +1
107600990915     O          E            riepilogo   2
107700990915     O                                              'Giusti...................:'
107800971022     O                       §CTROK        2   +  1
107900990915     O          E            riepilogo   2
108000990915     O                                              'Sbagliati e corretti.....:'
108100971022     O                       §CTRMO        2   +  1
108200990915     O          E            riepilogo   2
108300990915     O                                              'Sbagliati e scartati.....:'
108400971022     O                       §CTRNO        2   +  1
108500000613
108600010601     Otis7t5ps  E            testdett          2
108700000613     O                                              'Upload via Internet'
108800000613     O                                           +1 'Traduzione TIVIN00R -'
108900020913     O                                           +1 'FIVABWWR'
109000000613     O                                           +1 'Testate bolle'
109100000616     O          E            testdett    3
109200000613     O                                           +2 'N° rec'
109300000613     O                                           +3 'Anteprima contenuto'
109400000616     O          E            rigadett    2
109500000613     O                       rrnum               +2
109600000621     O                       recko               +3
109700000616     O          E            findett     2
109800000613     O                       wrkdata
109900000613     O                       wrkora              +1
110000000613     O                       procname            +1
110100000616     O          E            findett     2
110200000613     O                                              'Cliente..................:'
110300000613     O                       VABCCM        z     +1
110400000616     O          E            findett     2
110500000613     O                                              'Riferimento Strategi.....:'
110600000613     O                       vlrhdl              +1
110700000616     O          E            findett     2
110800000613     O                                              'Giusti...................:'
110900000613     O                       §CTROK        2   +  1
111000000616     O          E            findett     2
111100000613     O                                              'Sbagliati e corretti.....:'
111200000613     O                       §CTRMO        2   +  1
111300000616     O          E            findett     2
111400000613     O                                              'Sbagliati e scartati.....:'
111500000613     O                       §CTRNO        2   +  1
111600000512** CMD - COMANDI CL
111700011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T5P)  FORMTYPE(RICCLI) OUTQ(
111800011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T5PS) FORMTYPE(RICCLI) OUTQ(
111900010601DLTOVR FILE(TIS7T5P TIS7T5PS) LVL(*)
112000000512
112100000512
