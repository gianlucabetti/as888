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
004300041025     D tisi95ds      e ds
004400990910     D fnvabaar      e ds                  inz
004500990910     D esito           s              1
004600000724     D prmlit          s             10
004700000710     D prmfir          s             10
004800990921     D wrkesito        s                   like(esito)
004900990915     D wrkdata         s               d
005000990915     D wrkora          s               t
005100000613     D rrnum           s              6  0 INZ(*zeros)
005200000621     D recko           s            150    INZ(*blanks)
005300011123     D depcmd          s            150    INZ(*blanks)
005400060410     D wVHBCAS         s                   LIKE(VHBCAS) INZ(*blanks)
005500010606     D*------------------
005600010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
005700010606     D*------------------
005800010606     D WLBDA8          DS                  INZ
005900010606     D  G08DAT                 1      8  0
006000010606     D  G08INV                 9     16  0
006100010606     D  G08ERR                17     17
006200010606     D  G08TGI                18     22  0
006300010606     D*
006400040407     D TxtInOut        S           2048
006500040407     D ElencoChar      S            256
006600040407     D TipoElenco      S              1
006700040407     D CharSost        S              1
006800040407     D UpperCase       S              1
006900040407     D ChkNull         S              1
007000040407     D CharNull        S              1
007100040407     D EsitoBon        S              1
007200990908
007300990915     C                   time                    wrkdata
007400990915     C                   time                    wrkora
007500000913     C                   reset                   rrnum
007600990921     C                   reset                   esito
007700990921     C                   reset                   wrkesito
007800010530     C*
007900010530     C                   exsr      opeini
008000010530     C                   exsr      rwvab
008100041025     C*
008200041025     C* Esegue lancio TISI95R solo x chiusura
008300041025     C                   CLEAR                   TISI95DS
008400041025     C                   EVAL      I95TLA = 'C'
008500041025     C                   CALL      'TISI95R'
008600041025     C                   PARM                    TISI95DS
008700010530     C*
008800010530     C                   seton                                        lr
008900010530
009000010530
009100010530
009200010530
009300010530     C*--------------------------------------------------------
009400010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009500010530     C*--------------------------------------------------------
009600010530     C     PREELA        BEGSR
009700000724     C*
009800000724     C* SE OCCORRE SPEDIRE IN FILIALE
009900010530     C                   if        invfil <> *zeros and
010000010530     C                             flgGiro = '0'
010100010530     C*
010200010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010300010530     C                   eval      flgGiro = '1'
010400000724     C*
010500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010700010530     C     invfil        chain     azorg01l
010800000724     C                   if        %found
010900000616     C                   movel(p)  CMD(1)        depcmd
011000020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011100000616     C*
011200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011300011123     C                   Z-ADD     150           LENGH            15 5
011400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011500000616     C                   PARM                    depcmd
011600000616     C                   PARM                    LENGH
011700000724     C*
011800000724     C                   endif
011900000616     C*
012000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012100000616     C                   movel(p)  CMD(2)        depcmd
012200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012300000616     C*
012400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012500011123     C                   Z-ADD     150           LENGH            15 5
012600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012700000616     C                   PARM                    depcmd
012800000616     C                   PARM                    LENGH
012900000616     C*
013000010601     C                   if        not %open(tis7t5ps)
013100010601     C                   open      tis7t5ps
013200030407     C   50              except    testdett
013300000613     C                   endif
013400010530     C*
013500010530     C                   endif
013600010530     C*
013700010530     C                   ENDSR
013800010530     C***
013900010530
014000010530
014100010530
014200010530
014300010530
014400010530
014500010530     C*--------------------------------------------------------
014600010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014700010530     C*--------------------------------------------------------
014800010530     C     ENDELA        BEGSR
014900010530     C*
015000010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015100000613     C*
015200010601     C                   if        %open(tis7t5ps)
015300030407     C   50              except    findett
015400010601     C                   close     tis7t5ps
015500000613     C                   endif
015600000616     C*
015700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015800011123     C                   Z-ADD     150           LENGH            15 5
015900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
016000000616     C                   PARM                    CMD(3)
016100000616     C                   PARM                    LENGH
016200010530     C*
016300010530     C                   ENDSR
016400010530     C***
016500000613
016600010530
016700010530
016800000613
016900000613     C*--------------------------------------------------------
017000000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
017100000613     C*--------------------------------------------------------
017200000621     C     STPR          BEGSR
017300000613     C*
017400010601     C                   if        not %open(tis7t5p)
017500010601     C                   open      tis7t5p
017600990915     C                   endif
017700990915     C*
017800030407     C   50              except    riepilogo
017900990915     C*
018000010601     C                   if        %open(tis7t5p)
018100010601     C                   close     tis7t5p
018200990914     C                   endif
018300990910     C*
018400000613     C                   ENDSR
018500000613     C***
018600990908
018700010530
018800010530
018900010530
019000910830     C*--------------------------------------------------------
019100020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
019200910830     C*--------------------------------------------------------
019300941229     C     RWVAB         BEGSR
019400990910     C*
019500990914     C                   if        not %open(tivin00r)
019600990908     C                   open      tivin00r
019700990914     C                   endif
019800020913     C                   if        not %open(fivabwwr)
019900020913     C                   open      fivabwwr
020000990914     C                   endif
020100990910     C*
020200990910     C                   clear                   §CTROK
020300990910     C                   clear                   §CTRMO
020400990910     C                   clear                   §CTRNO
020500990910     C*
020600921023     C                   DO        *HIVAL
020700990913     C*
020800990915     C                   READ      tivin00r                               70
020900010618     C*
021000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021100010618     C                   if        vinflg = '1'
021200010618     C                   eval      flgOk = '1'
021300010618     C                   endif
021400010705     C*
021500010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
021600010705     C                   if        %subst(vindta:1:1) = '='
021700010705     C                   eval      %subst(vindta:1:1) = *blanks
021800010705     C                   endif
021900010705     C*
022000051017     C                   if        vindta  > *blanks
022100000613     C                   add       1             rrnum
022200010601     C* ...elimino eventuali segni
022300010601     C     '+':' '       xlate     vindta        vindta
022400990913     C*
022500010601     C                   if        *in70 = *off and
022600990913     C                             (vinflg = *blanks
022700010711     C                              or vinflg = '0'
022800010711     C                              or vinflg = '2')
022900990913     C*
023000020913     C                   clear                   FIVAB000
023100010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023200010711     C                   if        vinflg = *blanks or vinflg = '0'
023300010711     C                   clear                   vinmsg
023400010711     C                   endif
023500010711     C*
023600990913     C                   eval      fnvabaar = vindta
023700991022     C*
023800991022     C                   testb     x'3F'         vhbatb                   02
023900991022     C                   if        not *in02
024000991022     C*
024100971021     C                   SETOFF                                       303132
024200971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
024300971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
024400971023     C*    OPPURE TUTTI BLANKS
024500010530     C*
024600010530     C                   TESTN                   VHBLNP               30
024700010530     C  N30              TESTN                   VHBLNP                 30
024800010530     C  N30              TESTN                   VHBLNP                   30
024900010530     C                   MOVE      VHBLNP        W001A             1
025000010530     C     *IN30         IFEQ      *OFF
025100010530     C     W001A         ORLT      '0'
025200010530     C                   SETON                                        31
025300010530     C                   eval      vinmsg = %trimr(vinmsg)
025400010530     C                             + ' ' + 'VHBLNP'
025500010530     C                   ELSE
025600010530     C                   MOVE      VHBLNP        VABLNP
025700010530     C     VABLNP        IFLE      *ZEROS
025800010530     C                   SETON                                        31
025900010530     C                   eval      vinmsg = %trimr(vinmsg)
026000010530     C                             + ' ' + 'VHBLNP'
026100010530     C                   ENDIF
026200010530     C                   ENDIF
026300010530     C*
026400010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026500010530     C                   eval      depfil = VABLNP
026600010530     C                   exsr      repfil
026700010601     C                   if        depfil = invfil
026800021014     C                   if        vlrpoi = 999
026900021010     C                   MOVE(P)   invfil        VABFGS
027000021014     C                   else
027100021014     C                   MOVE(P)   vlrpoi        VABFGS
027200021014     C                   endif
027300010530     C*
027400010530     C                   exsr      PREELA
027500010530     C*
027600971023     C                   TESTN                   VHBCCM               30
027700971023     C  N30              TESTN                   VHBCCM                 30
027800971023     C  N30              TESTN                   VHBCCM                   30
027900971021     C                   MOVE      VHBCCM        W001A             1
028000971021     C     *IN30         IFEQ      *OFF
028100971021     C     W001A         ORLT      '0'
028200971021     C                   SETON                                        31
028300010604     C                   eval      vinmsg = %trimr(vinmsg)
028400010604     C                             + ' ' + 'VHBCCM'
028500971021     C                   ELSE
028600971021     C                   MOVE      VHBCCM        VABCCM
028700971024     C     VABCCM        IFLE      *ZEROS
028800971024     C                   SETON                                        31
028900010604     C                   eval      vinmsg = %trimr(vinmsg)
029000010604     C                             + ' ' + 'VHBCCM'
029100971024     C                   ENDIF
029200971021     C                   ENDIF
029300971021     C*
029400971023     C                   TESTN                   VHBAAS               30
029500971023     C  N30              TESTN                   VHBAAS                 30
029600971023     C  N30              TESTN                   VHBAAS                   30
029700971021     C                   MOVE      VHBAAS        W001A             1
029800971021     C     *IN30         IFEQ      *OFF
029900971021     C     W001A         ORLT      '0'
030000971023     C     W001A         ANDNE     *BLANK
030100971021     C                   SETON                                        32
030200010606     C                   MOVEL     datcor        VABAAS
030300010604     C                   eval      vinmsg = %trimr(vinmsg)
030400010604     C                             + ' ' + 'VHBAAS'
030500971021     C                   ELSE
030600971021     C                   MOVE      VHBAAS        VABAAS
030700971021     C                   ENDIF
030800971021     C*
030900971023     C                   TESTN                   VHBMGS               30
031000971023     C  N30              TESTN                   VHBMGS                 30
031100971023     C  N30              TESTN                   VHBMGS                   30
031200971021     C                   MOVE      VHBMGS        W001A             1
031300971021     C     *IN30         IFEQ      *OFF
031400971021     C     W001A         ORLT      '0'
031500971023     C     W001A         ANDNE     *BLANK
031600971021     C                   SETON                                        32
031700010601     C                   MOVE      '0101'        VABMGS
031800010604     C                   eval      vinmsg = %trimr(vinmsg)
031900010604     C                             + ' ' + 'VHBMGS'
032000971021     C                   ELSE
032100971021     C                   MOVE      VHBMGS        VABMGS
032200971021     C                   ENDIF
032300971021     C*
032400971023     C                   TESTN                   VHBNRS               30
032500971023     C  N30              TESTN                   VHBNRS                 30
032600971023     C  N30              TESTN                   VHBNRS                   30
032700971021     C                   MOVE      VHBNRS        W001A             1
032800971021     C     *IN30         IFEQ      *OFF
032900971021     C     W001A         ORLT      '0'
033000971023     C     W001A         ANDNE     *BLANK
033100971021     C                   SETON                                        32
033200971021     C                   MOVE      *ZERO         VABNRS
033300010604     C                   eval      vinmsg = %trimr(vinmsg)
033400010604     C                             + ' ' + 'VHBNRS'
033500971021     C                   ELSE
033600971021     C                   MOVE      VHBNRS        VABNRS
033700971021     C                   ENDIF
033800971021     C*
033900971023     C                   TESTN                   VHBNSP               30
034000971023     C  N30              TESTN                   VHBNSP                 30
034100971023     C  N30              TESTN                   VHBNSP                   30
034200971021     C                   MOVE      VHBNSP        W001A             1
034300971021     C     *IN30         IFEQ      *OFF
034400971021     C     W001A         ORLT      '0'
034500971023     C     W001A         ANDNE     *BLANK
034600971021     C                   SETON                                        32
034700971021     C                   MOVE      *ZERO         VABNSP
034800010604     C                   eval      vinmsg = %trimr(vinmsg)
034900010604     C                             + ' ' + 'VHBNSP'
035000971021     C                   ELSE
035100971021     C                   MOVE      VHBNSP        VABNSP
035200971021     C                   ENDIF
035300971021     C*
035400971023     C                   TESTN                   VHBLNA               30
035500971023     C  N30              TESTN                   VHBLNA                 30
035600971023     C  N30              TESTN                   VHBLNA                   30
035700971021     C                   MOVE      VHBLNA        W001A             1
035800971021     C     *IN30         IFEQ      *OFF
035900971021     C     W001A         ORLT      '0'
036000971023     C     W001A         ANDNE     *BLANK
036100971021     C                   SETON                                        32
036200971021     C                   MOVE      *ZERO         VABLNA
036300010604     C                   eval      vinmsg = %trimr(vinmsg)
036400010604     C                             + ' ' + 'VHBLNA'
036500971021     C                   ELSE
036600971021     C                   MOVE      VHBLNA        VABLNA
036700971021     C                   ENDIF
036800971021     C*
036900971023     C                   TESTN                   VHBCTR               30
037000971023     C  N30              TESTN                   VHBCTR                 30
037100971023     C  N30              TESTN                   VHBCTR                   30
037200971021     C                   MOVE      VHBCTR        W001A             1
037300971021     C     *IN30         IFEQ      *OFF
037400971021     C     W001A         ORLT      '0'
037500971023     C     W001A         ANDNE     *BLANK
037600971021     C                   SETON                                        32
037700971021     C                   MOVE      *ZERO         VABCTR
037800010604     C                   eval      vinmsg = %trimr(vinmsg)
037900010604     C                             + ' ' + 'VHBCTR'
038000971021     C                   ELSE
038100971021     C                   MOVE      VHBCTR        VABCTR
038200971021     C                   ENDIF
038300941229     C*
038400971021     C                   MOVE      *ZERO         VABIAS
038500941229     C                   MOVE      VHBIAS        WASC15
038600971023     C                   TESTN                   WI15                 30
038700971023     C  N30              TESTN                   WI15                   30
038800971023     C  N30              TESTN                   WI15                     30
038900971021     C                   MOVE      WI15          W001A             1
039000971021     C     *IN30         IFEQ      *OFF
039100971021     C     W001A         ORLT      '0'
039200971023     C     W001A         ANDNE     *BLANK
039300971021     C                   SETON                                        32
039400971021     C                   MOVE      *ZERO         VABIAS
039500010604     C                   eval      vinmsg = %trimr(vinmsg)
039600010604     C                             + ' ' + 'VHBIAS'
039700971021     C                   ELSE
039800971021     C                   MOVEL     WI15          VABIAS
039900971023     C                   TESTN                   WD15                 30
040000971023     C  N30              TESTN                   WD15                   30
040100971023     C  N30              TESTN                   WD15                     30
040200971021     C                   MOVE      WD15          W001A             1
040300971021     C     *IN30         IFEQ      *OFF
040400971021     C     W001A         ORLT      '0'
040500971023     C     W001A         ANDNE     *BLANK
040600971021     C                   SETON                                        32
040700971021     C                   MOVE      *ZERO         VABIAS
040800010604     C                   eval      vinmsg = %trimr(vinmsg)
040900010604     C                             + ' ' + 'VHBIAS'
041000971021     C                   ELSE
041100971021     C                   MOVE      WD15          VABIAS
041200971021     C                   ENDIF
041300050118     C                   ENDIF
041400941229     C*
041500971023     C                   TESTN                   VHBNCL               30
041600971023     C  N30              TESTN                   VHBNCL                 30
041700971023     C  N30              TESTN                   VHBNCL                   30
041800971021     C                   MOVE      VHBNCL        W001A             1
041900971021     C     *IN30         IFEQ      *OFF
042000971021     C     W001A         ORLT      '0'
042100971021     C                   SETON                                        32
042200971021     C                   MOVE      *ZERO         VABNCL
042300010604     C                   eval      vinmsg = %trimr(vinmsg)
042400010604     C                             + ' ' + 'VHBNCL'
042500971021     C                   ELSE
042600971021     C                   MOVE      VHBNCL        VABNCL
042700971021     C                   ENDIF
042800941229     C*
042900971021     C                   MOVE      *ZERO         VABPKB
043000971021     C                   MOVE      VHBPKB        WASC09
043100010719     C                   IF        WI09 = *blanks
043200010719     C                   EVAL      WI09 = *zeros
043300010719     C                   ENDIF
043400971023     C                   TESTN                   WI09                 30
043500971023     C  N30              TESTN                   WI09                   30
043600971023     C  N30              TESTN                   WI09                     30
043700971021     C                   MOVE      WI09          W001A             1
043800971021     C     *IN30         IFEQ      *OFF
043900971021     C     W001A         ORLT      '0'
044000971021     C                   SETON                                        32
044100971021     C                   MOVE      *ZERO         VABPKB
044200010604     C                   eval      vinmsg = %trimr(vinmsg)
044300010604     C                             + ' ' + 'VHBPKB'
044400971021     C                   ELSE
044500971021     C                   MOVEL     WI09          VABPKB
044600010719     C                   IF        WD09 = *blanks
044700010719     C                   EVAL      WD09 = *zeros
044800010719     C                   ENDIF
044900971023     C                   TESTN                   WD09                 30
045000971023     C  N30              TESTN                   WD09                     30
045100971021     C                   MOVE      WD09          W001A             1
045200971021     C     *IN30         IFEQ      *OFF
045300971021     C     W001A         ORLT      '0'
045400971023     C     W001A         ANDNE     *BLANK
045500971021     C                   SETON                                        32
045600971021     C                   MOVE      *ZERO         VABPKB
045700010604     C                   eval      vinmsg = %trimr(vinmsg)
045800010604     C                             + ' ' + 'VHBPKB'
045900971021     C                   ELSE
046000971021     C                   MOVE      WD09          VABPKB
046100971021     C                   ENDIF
046200050118     C                   ENDIF
046300971021     C*
046400041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
046500041001     C                   IF        *IN51 = *OFF
046600971021     C                   MOVE      *ZERO         VABVLB
046700971021     C                   MOVE      VHBVLB        WASC07
046800010719     C                   IF        WI07 = *blanks
046900010719     C                   EVAL      WI07 = *zeros
047000010719     C                   ENDIF
047100971023     C                   TESTN                   WI07                 30
047200971023     C  N30              TESTN                   WI07                   30
047300971023     C  N30              TESTN                   WI07                     30
047400971021     C                   MOVE      WI07          W001A             1
047500971021     C     *IN30         IFEQ      *OFF
047600971021     C     W001A         ORLT      '0'
047700971023     C     W001A         ANDNE     *BLANK
047800971021     C                   SETON                                        32
047900971021     C                   MOVE      *ZERO         VABVLB
048000010604     C                   eval      vinmsg = %trimr(vinmsg)
048100010604     C                             + ' ' + 'VHBVLB'
048200971021     C                   ELSE
048300971021     C                   MOVEL     WI07          VABVLB
048400010719     C                   IF        WD07 = *blanks
048500010719     C                   EVAL      WD07 = *zeros
048600010719     C                   ENDIF
048700971023     C                   TESTN                   WD07                 30
048800971023     C  N30              TESTN                   WD07                   30
048900971023     C  N30              TESTN                   WD07                     30
049000971021     C                   MOVE      WD07          W001A             1
049100971021     C     *IN30         IFEQ      *OFF
049200971021     C     W001A         ORLT      '0'
049300971023     C     W001A         ANDNE     *BLANK
049400971021     C                   SETON                                        32
049500971021     C                   MOVE      *ZERO         VABVLB
049600010604     C                   eval      vinmsg = %trimr(vinmsg)
049700010604     C                             + ' ' + 'VHBVLB'
049800971021     C                   ELSE
049900971021     C                   MOVE      WD07          VABVLB
050000971021     C                   ENDIF
050100050118     C                   ENDIF
050200041001     C                   ELSE
050300041001     C* ...altrimenti forzo azzeramento del volume
050400041001     C                   Z-ADD     *zeros        VABVLB
050500041001     C                   ENDIF
050600941229     C*
050700971021     C                   MOVE      *ZERO         VABQFT
050800971021     C                   MOVE      VHBQFT        WASC15
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
052000971021     C                   MOVE      *ZERO         VABQFT
052100010604     C                   eval      vinmsg = %trimr(vinmsg)
052200010604     C                             + ' ' + 'VHBQFT'
052300971021     C                   ELSE
052400971021     C                   MOVEL     WI15          VABQFT
052500010719     C                   IF        WD15 = *blanks
052600010719     C                   EVAL      WD15 = *zeros
052700010719     C                   ENDIF
052800971023     C                   TESTN                   WD15                 30
052900971023     C  N30              TESTN                   WD15                   30
053000971023     C  N30              TESTN                   WD15                     30
053100971021     C                   MOVE      WD15          W001A             1
053200971021     C     *IN30         IFEQ      *OFF
053300971021     C     W001A         ORLT      '0'
053400971023     C     W001A         ANDNE     *BLANK
053500971021     C                   SETON                                        32
053600971021     C                   MOVE      *ZERO         VABQFT
053700010604     C                   eval      vinmsg = %trimr(vinmsg)
053800010604     C                             + ' ' + 'VHBQFT'
053900971021     C                   ELSE
054000971021     C                   MOVE      WD15          VABQFT
054100971021     C                   ENDIF
054200050118     C                   ENDIF
054300941229     C*
054400971021     C                   MOVE      *ZERO         VABCAS
054500060410     C* Se presente separatore decimale alla giusta posizione => forzo sempre la virgola
054600060410     C                   EVAL      wVHBCAS = VHBCAS
054700060410     C                   IF        %subst(wVHBCAS:12:1) = '.'
054800060410     C                   EVAL      %subst(wVHBCAS:12:1) = ','
054900060410     C                   ENDIF
055000060410     C*
055100060410     C                   MOVEL(P)  wVHBCAS       PiStr
055200060410     C                   EXSR      CHKNUM
055300060410     C                   IF        PiNum=*on
055400060410     C                   Z-ADD     PiVal         VABCAS
055500060410     C                   ELSE
055600060410     C                   SETON                                        32
055700060410     C                   Z-ADD     *zeros        VABCAS
055800060410     C                   EVAL      vinmsg = %trimr(vinmsg)
055900060410     C                             + ' ' + 'VHBCAS'
056000060410     C                   ENDIF
056100971021     C*
056200971023     C                   TESTN                   VHBRMN               30
056300971023     C  N30              TESTN                   VHBRMN                 30
056400971023     C  N30              TESTN                   VHBRMN                   30
056500971021     C                   MOVE      VHBRMN        W001A             1
056600971021     C     *IN30         IFEQ      *OFF
056700971021     C     W001A         ORLT      '0'
056800971021     C                   SETON                                        32
056900971021     C                   MOVE      *ZERO         VABRMN
057000010604     C                   eval      vinmsg = %trimr(vinmsg)
057100010604     C                             + ' ' + 'VHBRMN'
057200971021     C                   ELSE
057300971021     C                   MOVE      VHBRMN        VABRMN
057400971021     C                   ENDIF
057500971021     C*
057600971023     C                   TESTN                   VHBNCD               30
057700971023     C  N30              TESTN                   VHBNCD                 30
057800971023     C  N30              TESTN                   VHBNCD                   30
057900971021     C                   MOVE      VHBNCD        W001A             1
058000971021     C     *IN30         IFEQ      *OFF
058100971021     C     W001A         ORLT      '0'
058200971023     C     W001A         ANDNE     *BLANK
058300971021     C                   SETON                                        32
058400971021     C                   MOVE      *ZERO         VABNCD
058500010604     C                   eval      vinmsg = %trimr(vinmsg)
058600010604     C                             + ' ' + 'VHBNCD'
058700971021     C                   ELSE
058800971021     C                   MOVE      VHBNCD        VABNCD
058900971021     C                   ENDIF
059000971021     C*
059100971023     C                   TESTN                   VHBNCA               30
059200971023     C  N30              TESTN                   VHBNCA                 30
059300971023     C  N30              TESTN                   VHBNCA                   30
059400971021     C                   MOVE      VHBNCA        W001A             1
059500971021     C     *IN30         IFEQ      *OFF
059600971021     C     W001A         ORLT      '0'
059700971023     C     W001A         ANDNE     *BLANK
059800971021     C                   SETON                                        32
059900971021     C                   MOVE      *ZERO         VABNCA
060000010604     C                   eval      vinmsg = %trimr(vinmsg)
060100010604     C                             + ' ' + 'VHBNCA'
060200971021     C                   ELSE
060300971021     C                   MOVE      VHBNCA        VABNCA
060400971021     C                   ENDIF
060500971021     C*
060600971023     C                   TESTN                   VHBZNC               30
060700971023     C  N30              TESTN                   VHBZNC                 30
060800971023     C  N30              TESTN                   VHBZNC                   30
060900971021     C                   MOVE      VHBZNC        W001A             1
061000971021     C     *IN30         IFEQ      *OFF
061100971021     C     W001A         ORLT      '0'
061200971023     C     W001A         ANDNE     *BLANK
061300971021     C                   SETON                                        32
061400971021     C                   MOVE      *ZERO         VABZNC
061500010604     C                   eval      vinmsg = %trimr(vinmsg)
061600010604     C                             + ' ' + 'VHBZNC'
061700971021     C                   ELSE
061800971021     C                   MOVE      VHBZNC        VABZNC
061900971021     C                   ENDIF
062000971021     C*
062100971023     C                   TESTN                   VHBDCR               30
062200971023     C  N30              TESTN                   VHBDCR                 30
062300971023     C  N30              TESTN                   VHBDCR                   30
062400971021     C                   MOVE      VHBDCR        W001A             1
062500971021     C     *IN30         IFEQ      *OFF
062600971021     C     W001A         ORLT      '0'
062700971023     C     W001A         ANDNE     *BLANK
062800971021     C                   SETON                                        32
062900971021     C                   MOVE      *ZERO         VABDCR
063000010604     C                   eval      vinmsg = %trimr(vinmsg)
063100010604     C                             + ' ' + 'VHBDCR'
063200971021     C                   ELSE
063300971021     C                   MOVE      VHBDCR        VABDCR
063400971021     C                   ENDIF
063500971021     C*
063600971023     C                   TESTN                   VHBHCR               30
063700971023     C  N30              TESTN                   VHBHCR                 30
063800971023     C  N30              TESTN                   VHBHCR                   30
063900971021     C                   MOVE      VHBHCR        W001A             1
064000971021     C     *IN30         IFEQ      *OFF
064100971021     C     W001A         ORLT      '0'
064200971023     C     W001A         ANDNE     *BLANK
064300971021     C                   SETON                                        32
064400971021     C                   MOVE      *ZERO         VABHCR
064500010604     C                   eval      vinmsg = %trimr(vinmsg)
064600010604     C                             + ' ' + 'VHBHCR'
064700971021     C                   ELSE
064800971021     C                   MOVE      VHBHCR        VABHCR
064900971021     C                   ENDIF
065000941229     C*
065100971021     C                   MOVE      *ZERO         VABVMD
065200971021     C                   MOVE      VHBVMD        WASC15
065300010719     C                   IF        WI15 = *blanks
065400010719     C                   EVAL      WI15 = *zeros
065500010719     C                   ENDIF
065600971023     C                   TESTN                   WI15                 30
065700971023     C  N30              TESTN                   WI15                   30
065800971023     C  N30              TESTN                   WI15                     30
065900971021     C                   MOVE      WI15          W001A             1
066000971021     C     *IN30         IFEQ      *OFF
066100971021     C     W001A         ORLT      '0'
066200971023     C     W001A         ANDNE     *BLANK
066300971021     C                   SETON                                        32
066400971021     C                   MOVE      *ZERO         VABVMD
066500010604     C                   eval      vinmsg = %trimr(vinmsg)
066600010604     C                             + ' ' + 'VHBVMD'
066700971021     C                   ELSE
066800971021     C                   MOVEL     WI15          VABVMD
066900010719     C                   IF        WD15 = *blanks
067000010719     C                   EVAL      WD15 = *zeros
067100010719     C                   ENDIF
067200971023     C                   TESTN                   WD15                 30
067300971023     C  N30              TESTN                   WD15                   30
067400971023     C  N30              TESTN                   WD15                     30
067500971021     C                   MOVE      WD15          W001A             1
067600971021     C     *IN30         IFEQ      *OFF
067700971021     C     W001A         ORLT      '0'
067800971023     C     W001A         ANDNE     *BLANK
067900971021     C                   SETON                                        32
068000971021     C                   MOVE      *ZERO         VABVMD
068100010604     C                   eval      vinmsg = %trimr(vinmsg)
068200010604     C                             + ' ' + 'VHBVMD'
068300971021     C                   ELSE
068400971021     C                   MOVE      WD15          VABVMD
068500971021     C                   ENDIF
068600050118     C                   ENDIF
068700971021     C*
068800971023     C                   TESTN                   VHBANT               30
068900971023     C  N30              TESTN                   VHBANT                 30
069000971023     C  N30              TESTN                   VHBANT                   30
069100971021     C                   MOVE      VHBANT        W001A             1
069200971021     C     *IN30         IFEQ      *OFF
069300971021     C     W001A         ORLT      '0'
069400971023     C     W001A         ANDNE     *BLANK
069500971021     C                   SETON                                        32
069600971021     C                   MOVE      *ZERO         VABANT
069700010604     C                   eval      vinmsg = %trimr(vinmsg)
069800010604     C                             + ' ' + 'VHBANT'
069900971021     C                   ELSE
070000971021     C                   MOVE      VHBANT        VABANT
070100971021     C                   ENDIF
070200971021     C*
070300971021     C* CAMPI ALFABETICI:
070400971021     C                   MOVE      VHBATB        VABATB
070500971021     C                   MOVE      VHBCBO        VABCBO
070600971021     C                   MOVE      VHBRSD        VABRSD
070700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
070800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
070900020117     C* ==
071000971021     C                   MOVE      VHBRD2        VABRD2
071100971021     C                   MOVE      VHBIND        VABIND
071200971021     C                   MOVE      VHBCAD        VABCAD
071300971021     C                   MOVE      VHBLOD        VABLOD
071400971021     C                   MOVE      VHBPRD        VABPRD
071500971021     C                   MOVE      VHBNZD        VABNZD
071600971021     C                   MOVE      VHBGC1        VABGC1
071700971021     C                   MOVE      VHBGC2        VABGC2
071800971021     C                   MOVE      VHBTSP        VABTSP
071900971021     C                   MOVE      VHBVAS        VABVAS
072000971021     C                   MOVE      VHBNAS        VABNAS
072100971021     C                   MOVE      VHBTIC        VABTIC
072200971021     C                   MOVE      VHBVCA        VABVCA
072300971021     C                   MOVE      VHBGCA        VABGCA
072400971021     C                   MOVE      VHBRMA        VABRMA
072500971021     C                   MOVE      VHBXCO        VABXCO
072600971021     C                   MOVE      VHBNOT        VABNOT
072700971021     C                   MOVE      VHBNT2        VABNT2
072800971021     C                   MOVE      VHBCTM        VABCTM
072900971021     C                   MOVE      VHBFFD        VABFFD
073000971021     C                   MOVE      VHBTCR        VABTCR
073100971021     C                   MOVE      VHBCTS        VABCTS
073200971021     C                   MOVE      VHBFTM        VABFTM
073300971021     C                   MOVE      VHBVAD        VABVAD
073400971021     C                   MOVE      VHBGMA        VABGMA
073500971021     C                   MOVE      VHBGGA        VABGGA
073600971021     C                   MOVE      VHBGVA        VABGVA
073700971021     C                   MOVE      VHBTC1        VABTC1
073800971021     C                   MOVE      VHBTC2        VABTC2
073900971021     C                   MOVE      VHBSCL        VABSCL
074000971021     C                   MOVE      VHBRMO        VABRMO
074100971021     C                   MOVE      VHBCMO        VABCMO
074200971021     C                   MOVE      VHBNMO        VABNMO
074300971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
074400060329     C* NELLA NAZIONE MITTENTE ORIGINALE
074500060331     C                   IF        %trim(VHBNMO) < 'A'
074600060329     C                   MOVE      *BLANKS       VABNMO
074700060329     C                   ENDIF
074800050418     C*
074900050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
075000050418     C                   IF        %trim(VABNZD) = 'I'   OR
075100050418     C                             %trim(VABNZD) = 'IT'  OR
075200050418     C                             %trim(VABNZD) = 'ITA'
075300050418     C                   EVAL      VABNZD = *blanks
075400050418     C                   ENDIF
075500041025     C*
075600041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
075700041025     C                   IF        *IN52 = *ON
075800041025     C* Reperisco la provincia dal CAP e dalla località
075900041025     C                   IF        VABCAD <> *blanks AND
076000050907     C                             VABPRD  = *blanks AND
076100050907     C                             VABNZD  = *blanks
076200041025     C                   CLEAR                   TISI95DS
076300041025     C                   EVAL      I95TCN = '3'
076400041025     C                   Z-ADD     datcor        I95DAT
076500041025     C                   EVAL      I95CAP = VABCAD
076600041025     C                   EVAL      I95LOC = VABLOD
076700041025     C                   CALL      'TISI95R'
076800041025     C                   PARM                    TISI95DS
076900041025     C                   EVAL      VABPRD = O95PRV
077000041025     C                   ENDIF
077100041025     C                   ENDIF
077200040630     C*
077300040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
077400040630     C                   exsr      exeBon
077500090119     C*
077600090119     C* Forzatura momentanea x anomalia peso da cliente
077700090119     C                   if        vabNCL >   1 and
077800090119     C                             vabPKB >= 21
077900090119     C                   eval      vabPKB = vabPKB - 19
078000090119     C                   endif
078100930409     C*
078200010604     C  N31              ADD       1             §CTROK            7 0
078300010604     C   32              ADD       1             §CTRMO            7 0
078400010604     C   31              ADD       1             §CTRNO            7 0
078500020913     C  N31              WRITE     FIVAB000
078600990910     C*
078700010601     C                   if        *in31 = *off and
078800010601     C                             *in32 = *off
078900990910     C                   eval      vinflg = '1'
079000990910     C                   else
079100000621     C                   eval      recko = vindta
079200010612     C                   if        %open(tis7t5ps)
079300030407     C   50              except    rigadett
079400010612     C                   endif
079500990910     C                   eval      vinflg = '2'
079600990910     C                   endif
079700991022     C                   endif
079800990910     C                   endif
079900010601     C*
080000010601     C                   endif
080100000905     C*
080200000905     C                   else
080300000905     C                   eval      vinflg = '1'
080400000905     C                   endif
080500000905     C*
080600000905     C  N70              update    tivin000
080700991022     C*
080800991022     C  N70              ENDdo
080900010530     C
081000010530     C*
081100010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
081200010530     C                   if        cntNonEl = *zeros or
081300010530     C                             flgMulti = '0'
081400990910     C* Se non ci sono record con errori ...
081500010530     C                   if        §ctrno = 0 and
081600010601     C                             §ctrmo = 0 and
081700010530     C                             flgStato <> '2'
081800990910     C* ... restituisco esito OK.
081900990921     C                   eval      wrkesito = '0'
082000990910     C                   else
082100010601     C                   if        §ctrok > 0
082200990921     C                   eval      wrkesito = '1'
082300000710     C                   else
082400010615     C                   if        flgOk = '0'
082500010615     C                   eval      wrkesito = '2'
082600010615     C                   else
082700010615     C                   eval      wrkesito = '6'
082800010615     C                   endif
082900990910     C                   endif
083000000710     C                   endif
083100010530     C                   else
083200010530     C                   eval      wrkesito = '9'
083300010530     C                   endif
083400990910     C*
083500990914     C                   if        %open(tivin00r)
083600990908     C                   close     tivin00r
083700990914     C                   endif
083800020913     C                   if        %open(fivabwwr)
083900020913     C                   close     fivabwwr
084000990914     C                   endif
084100010530     C*
084200010530     C                   if        vlrpoi <> 999
084300010530     C                   eval      invfil = vlrpoi
084400010530     C                   endif
084500990910     C*
084600990920     C                   if        §ctrok > 0
084700010530     C                             and invfil > *zeros
084800000613     C                   exsr      invio
084900990920     C                   endif
085000010612     C*
085100010612     C                   if        flgGiro = '1'
085200010612     C                   exsr      endela
085300010612     C                   endif
085400990920     C*
085500910830     C                   ENDSR
085600000613     C***
085700990920
085800010530
085900010530
086000010530
086100010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
086200010530     C     repfil        BEGSR
086300010530     C*
086400010530     C                   if        invfil = *zeros and
086500010530     C                             depfil > *zeros and
086600010613     C                             (vinflg = *blanks or
086700010613     C                              vinflg = *zeros)
086800010530     C
086900010530     C                   eval      invfil = depfil
087000010530     C                   endif
087100010530     C*
087200010530     C                   if        depfil <> invfil and
087300010530     C                             invfil > *zeros
087400010530     C                   eval      flgMulti = '1'
087500010530     C                   if        vinflg = *blanks
087600010530     C                   add       1             cntNonEl
087700010530     C                   endif
087800010530     C                   endif
087900010530     C*
088000010530     C                   if        vinflg = '2'
088100010530     C                   eval      flgStato = '2'
088200010530     C                   endif
088300010530     C*
088400010530     C                   ENDSR
088500010530     C***
088600010530
088700010530
088800010530
088900010530
089000990920      /TITLE Invio dei dati al punto operativo.
089100000613     C     invio         BEGSR
089200990920     C*
089300990920     C                   reset                   dscmz
089400010530     C                   move      invfil        cmzdst
089500990920     C                   eval      cmzfld = vlrfou
089600990920     C                   eval      cmzmbd = vlrhdl
089700990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
089800000710     C                   if        prmfir = *blanks
089900020913     C                   eval      cmzfla = 'FIVAB00F'
090000020913     C                   eval      cmzmba = 'FIVAB00F'
090100000710     C                   else
090200000710     C                   eval      cmzfla = prmfir
090300000710     C                   eval      cmzmba = prmfir
090400000710     C                   endif
090500990920     C                   eval      cmznrr = *zeros
090600990920     C                   move      §ctrok        cmznrr
090700021003     C                   eval      cmzlba = vlrfl1
090800990920     C                   call(e)   'TIS711C'
090900990920     C                   parm                    dscmz
091000990921     C                   parm      *blanks       esito
091100990920     C                   if        %error
091200990920     C                             or cmzerr = '1'
091300990921     C                             or esito  = '1'
091400000710     C                   eval      wrkesito = '3'
091500990920     C                   endif
091600990920     C*
091700000613     C                   ENDSR
091800000613     C***
091900010530
092000010530
092100010530
092200010530
092300010530      /TITLE Invio dei dati al punto operativo.
092400010530     C     opeini        BEGSR
092500010530     C*
092600010530     C* Inizializzo flag e contatori operativi
092700010530     C                   movel     '0'           flgGiro           1
092800010530     C                   movel     '0'           flgMulti          1
092900010530     C                   movel     '1'           flgStato          1
093000010615     C                   movel     '0'           flgOk             1
093100010530     C                   z-add     *zeros        cntNonEl         10 0
093200010530     C                   z-add     *zeros        depfil            3 0
093300010530     C                   z-add     *zeros        invfil            3 0
093400030407     C*
093500030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
093600030407     C                   if        %subst(vlrppt:1:1) = 'S'
093700030407     C                   seton                                        50
093800030407     C                   else
093900030407     C                   setoff                                       50
094000030407     C                   endif
094100041001     C*
094200041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
094300041001     C                   if        %subst(vlrppt:2:1) = 'S'
094400041001     C                   seton                                        51
094500041001     C                   else
094600041001     C                   setoff                                       51
094700041001     C                   endif
094800041025     C*
094900041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
095000041025     C* (naturalmente solo se nn già presente)
095100041025     C                   if        %subst(vlrppt:3:1) = 'S'
095200041025     C                   seton                                        52
095300041025     C                   else
095400041025     C                   setoff                                       52
095500041025     C                   endif
095600010530     C*
095700010530     C                   ENDSR
095800010530     C***
095900040407
096000040407
096100040407
096200040407
096300040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
096400040407     C     exeBon        BEGSR
096500040407     C*
096600040407     C* Imposto i parametri x il *pgm d "bonifica"
096700040407     C                   EVAL      ElencoChar = *blanks
096800040407     C                   EVAL      TipoElenco = *blanks
096900040407     C                   EVAL      CharSost   = *blanks
097000040629     C                   EVAL      UpperCase  = '1'
097100040407     C                   EVAL      ChkNull    = '1'
097200040407     C                   EVAL      CharNull   = *blanks
097300040629     C*
097400040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
097500040629     C                   EVAL      TxtInOut   = VABRSD
097600040629     C                   EVAL      EsitoBon   = *blanks
097700040407     C*
097800040407     C                   CALL      'XCHKCHAR'
097900040407     C                   PARM                    TxtInOut
098000040407     C                   PARM                    ElencoChar
098100040407     C                   PARM                    TipoElenco
098200040407     C                   PARM                    CharSost
098300040407     C                   PARM                    UpperCase
098400040407     C                   PARM                    ChkNull
098500040407     C                   PARM                    CharNull
098600040407     C                   PARM                    EsitoBon
098700040407     C*
098800040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
098900040407     C                   IF        EsitoBon = '1'
099000040629     C                   EVAL      VABRSD = TxtInOut
099100040407     C                   ENDIF
099200040629     C*
099300040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
099400040629     C                   EVAL      TxtInOut   = VABIND
099500040629     C                   EVAL      EsitoBon   = *blanks
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
100900040629     C                   EVAL      VABIND = TxtInOut
101000040629     C                   ENDIF
101100040629     C*
101200040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
101300040629     C                   EVAL      TxtInOut   = VABLOD
101400040629     C                   EVAL      EsitoBon   = *blanks
101500040629     C*
101600040629     C                   CALL      'XCHKCHAR'
101700040629     C                   PARM                    TxtInOut
101800040629     C                   PARM                    ElencoChar
101900040629     C                   PARM                    TipoElenco
102000040629     C                   PARM                    CharSost
102100040629     C                   PARM                    UpperCase
102200040629     C                   PARM                    ChkNull
102300040629     C                   PARM                    CharNull
102400040629     C                   PARM                    EsitoBon
102500040629     C*
102600040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
102700040629     C                   IF        EsitoBon = '1'
102800040629     C                   EVAL      VABLOD = TxtInOut
102900040629     C                   ENDIF
103000040629     C*
103100040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
103200040629     C                   EVAL      TxtInOut   = VABNOT
103300040629     C                   EVAL      EsitoBon   = *blanks
103400040630     C                   EVAL      UpperCase  = *blanks
103500040629     C*
103600040629     C                   CALL      'XCHKCHAR'
103700040629     C                   PARM                    TxtInOut
103800040629     C                   PARM                    ElencoChar
103900040629     C                   PARM                    TipoElenco
104000040629     C                   PARM                    CharSost
104100040629     C                   PARM                    UpperCase
104200040629     C                   PARM                    ChkNull
104300040629     C                   PARM                    CharNull
104400040629     C                   PARM                    EsitoBon
104500040629     C*
104600040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
104700040629     C                   IF        EsitoBon = '1'
104800040629     C                   EVAL      VABNOT = TxtInOut
104900040629     C                   ENDIF
105000040629     C*
105100040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
105200040629     C                   EVAL      TxtInOut   = VABNT2
105300040629     C                   EVAL      EsitoBon   = *blanks
105400040630     C                   EVAL      UpperCase  = *blanks
105500040629     C*
105600040629     C                   CALL      'XCHKCHAR'
105700040629     C                   PARM                    TxtInOut
105800040629     C                   PARM                    ElencoChar
105900040629     C                   PARM                    TipoElenco
106000040629     C                   PARM                    CharSost
106100040629     C                   PARM                    UpperCase
106200040629     C                   PARM                    ChkNull
106300040629     C                   PARM                    CharNull
106400040629     C                   PARM                    EsitoBon
106500040629     C*
106600040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
106700040629     C                   IF        EsitoBon = '1'
106800040629     C                   EVAL      VABNT2 = TxtInOut
106900040629     C                   ENDIF
107000040407     C*
107100040407     C                   ENDSR
107200010530
107300060410
107400060410     C*----------------------------------------------------*
107500060410     C*  CONTROLLO NUMERICITA' CAMPI
107600060410     C*----------------------------------------------------*
107700060410     C     CHKNUM        BEGSR
107800060410     C*
107900060410     C                   IF        PiDecChr = *blanks
108000060410     C                   EVAL      PiDecChr = ','
108100060410     C                   ENDIF
108200060410     C*
108300060410     C                   CALL(e)   'ISNUMERIC'
108400060410     C                   PARM                    PiStr            30
108500060410     C                   PARM                    PiDecChr          1
108600060410     C                   PARM      *ZEROS        PiVal            30 9
108700060410     C                   PARM      '0'           PiInt             1
108800060410     C                   PARM      '0'           PiNum             1
108900060410     C                   IF        %error
109000060410     C                   EVAL      PiNum=*off
109100060410     C                   ENDIF
109200060410     C*
109300060410     C                   ENDSR
109400060410     C***
109500060410
109600990910
109700000613     C     *inzsr        BEGSR
109800990910     C*
109900990910     C     *entry        plist
110000990920     C                   parm                    tivlrds
110100990921     C                   parm      wrkesito      esito
110200000724     C                   parm                    prmlit
110300000710     C                   parm                    prmfir
110400010606     C*
110500010606     C* CALCOLA LA DATA CORRENTE
110600010606     C                   time                    wn14             14 0
110700010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
110800010606     C                   z-add     wn8           g08dat
110900010606     C                   z-add     *zeros        g08inv
111000010606     C                   movel     '0'           g08err
111100010606     C                   call      'XSRDA8'
111200010606     C                   parm                    wlbda8
111300010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
111400000613     C*
111500000613     C                   ENDSR
111600000613     C***
111700990908
111800010601     Otis7t5p   E            riepilogo         2
111900990915     O                                              'Upload via Internet'
112000990915     O                                           +1 'Traduzione TIVIN00R -'
112100020913     O                                           +1 'FIVABWWR'
112200990915     O                                           +1 'Testate bolle'
112300990915     O          E            riepilogo   2
112400990915     O                       wrkdata
112500990915     O                       wrkora              +1
112600990915     O                       procname            +1
112700990915     O          E            riepilogo   2
112800990915     O                                              'Cliente..................:'
112900990915     O                       VABCCM        z     +1
113000990915     O          E            riepilogo   2
113100990920     O                                              'Riferimento Strategi.....:'
113200990920     O                       vlrhdl              +1
113300990915     O          E            riepilogo   2
113400990915     O                                              'Giusti...................:'
113500971022     O                       §CTROK        2   +  1
113600990915     O          E            riepilogo   2
113700990915     O                                              'Sbagliati e corretti.....:'
113800971022     O                       §CTRMO        2   +  1
113900990915     O          E            riepilogo   2
114000990915     O                                              'Sbagliati e scartati.....:'
114100971022     O                       §CTRNO        2   +  1
114200000613
114300010601     Otis7t5ps  E            testdett          2
114400000613     O                                              'Upload via Internet'
114500000613     O                                           +1 'Traduzione TIVIN00R -'
114600020913     O                                           +1 'FIVABWWR'
114700000613     O                                           +1 'Testate bolle'
114800000616     O          E            testdett    3
114900000613     O                                           +2 'N° rec'
115000000613     O                                           +3 'Anteprima contenuto'
115100000616     O          E            rigadett    2
115200000613     O                       rrnum               +2
115300000621     O                       recko               +3
115400000616     O          E            findett     2
115500000613     O                       wrkdata
115600000613     O                       wrkora              +1
115700000613     O                       procname            +1
115800000616     O          E            findett     2
115900000613     O                                              'Cliente..................:'
116000000613     O                       VABCCM        z     +1
116100000616     O          E            findett     2
116200000613     O                                              'Riferimento Strategi.....:'
116300000613     O                       vlrhdl              +1
116400000616     O          E            findett     2
116500000613     O                                              'Giusti...................:'
116600000613     O                       §CTROK        2   +  1
116700000616     O          E            findett     2
116800000613     O                                              'Sbagliati e corretti.....:'
116900000613     O                       §CTRMO        2   +  1
117000000616     O          E            findett     2
117100000613     O                                              'Sbagliati e scartati.....:'
117200000613     O                       §CTRNO        2   +  1
117300000512** CMD - COMANDI CL
117400011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T5P)  FORMTYPE(RICCLI) OUTQ(
117500011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T5PS) FORMTYPE(RICCLI) OUTQ(
117600010601DLTOVR FILE(TIS7T5P TIS7T5PS) LVL(*)
117700000512
117800000512
