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
077500100419     C*
077600100419     C* Forzatura su campi DIVISE
077700100419     C                   IF        VABVAS = *blanks
077800100419     C                   EVAL      VABVAS = 'EUR'
077900100419     C                   ENDIF
078000100419     C                   IF        VABVCA = *blanks
078100100419     C                   EVAL      VABVCA = 'EUR'
078200100419     C                   ENDIF
078300100419     C                   IF        VABVAD = *blanks
078400100419     C                   EVAL      VABVAD = 'EUR'
078500100419     C                   ENDIF
078600110429     C*
078700110429     C* Forzature x diversa gestione traffici ILLY
078800110429     C*
078900110429     C* CCM
079000110429     C                   MOVEL     VABNSP        wCLI              1
079100110429     C                   IF        wCLI = '7'
079200110429     C                   EVAL      VABCCM = 1162074
079300110429     C                   ENDIF
079400110429     C* TIC
079500110502     C                   IF        VABCCM = 1162074 AND
079600110502     C                             (VABCBO = '4' OR VABCBO = '6') AND
079700110502     C                             VABTIC = *blanks
079800110429     C                   EVAL      VABTIC = 'SC'
079900110429     C                   ENDIF
080000111125     C                   IF        VABCCM = 1161162 AND
080100111125     C                             VABCAS >= 500 AND VABCAS <= 1000 AND
080200111125     C                             VABTIC = *blanks
080300111125     C                   EVAL      VABTIC = 'SC'
080400111125     C                   ENDIF
080500930409     C*
080600010604     C  N31              ADD       1             §CTROK            7 0
080700010604     C   32              ADD       1             §CTRMO            7 0
080800010604     C   31              ADD       1             §CTRNO            7 0
080900020913     C  N31              WRITE     FIVAB000
081000990910     C*
081100010601     C                   if        *in31 = *off and
081200010601     C                             *in32 = *off
081300990910     C                   eval      vinflg = '1'
081400990910     C                   else
081500000621     C                   eval      recko = vindta
081600010612     C                   if        %open(tis7t5ps)
081700030407     C   50              except    rigadett
081800010612     C                   endif
081900990910     C                   eval      vinflg = '2'
082000990910     C                   endif
082100991022     C                   endif
082200990910     C                   endif
082300010601     C*
082400010601     C                   endif
082500000905     C*
082600000905     C                   else
082700000905     C                   eval      vinflg = '1'
082800000905     C                   endif
082900000905     C*
083000000905     C  N70              update    tivin000
083100991022     C*
083200991022     C  N70              ENDdo
083300010530     C
083400010530     C*
083500010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
083600010530     C                   if        cntNonEl = *zeros or
083700010530     C                             flgMulti = '0'
083800990910     C* Se non ci sono record con errori ...
083900010530     C                   if        §ctrno = 0 and
084000010601     C                             §ctrmo = 0 and
084100010530     C                             flgStato <> '2'
084200990910     C* ... restituisco esito OK.
084300990921     C                   eval      wrkesito = '0'
084400990910     C                   else
084500010601     C                   if        §ctrok > 0
084600990921     C                   eval      wrkesito = '1'
084700000710     C                   else
084800010615     C                   if        flgOk = '0'
084900010615     C                   eval      wrkesito = '2'
085000010615     C                   else
085100010615     C                   eval      wrkesito = '6'
085200010615     C                   endif
085300990910     C                   endif
085400000710     C                   endif
085500010530     C                   else
085600010530     C                   eval      wrkesito = '9'
085700010530     C                   endif
085800990910     C*
085900990914     C                   if        %open(tivin00r)
086000990908     C                   close     tivin00r
086100990914     C                   endif
086200020913     C                   if        %open(fivabwwr)
086300020913     C                   close     fivabwwr
086400990914     C                   endif
086500010530     C*
086600010530     C                   if        vlrpoi <> 999
086700010530     C                   eval      invfil = vlrpoi
086800010530     C                   endif
086900990910     C*
087000990920     C                   if        §ctrok > 0
087100010530     C                             and invfil > *zeros
087200000613     C                   exsr      invio
087300990920     C                   endif
087400010612     C*
087500010612     C                   if        flgGiro = '1'
087600010612     C                   exsr      endela
087700010612     C                   endif
087800990920     C*
087900910830     C                   ENDSR
088000000613     C***
088100990920
088200010530
088300010530
088400010530
088500010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
088600010530     C     repfil        BEGSR
088700010530     C*
088800010530     C                   if        invfil = *zeros and
088900010530     C                             depfil > *zeros and
089000010613     C                             (vinflg = *blanks or
089100010613     C                              vinflg = *zeros)
089200010530     C
089300010530     C                   eval      invfil = depfil
089400010530     C                   endif
089500010530     C*
089600010530     C                   if        depfil <> invfil and
089700010530     C                             invfil > *zeros
089800010530     C                   eval      flgMulti = '1'
089900010530     C                   if        vinflg = *blanks
090000010530     C                   add       1             cntNonEl
090100010530     C                   endif
090200010530     C                   endif
090300010530     C*
090400010530     C                   if        vinflg = '2'
090500010530     C                   eval      flgStato = '2'
090600010530     C                   endif
090700010530     C*
090800010530     C                   ENDSR
090900010530     C***
091000010530
091100010530
091200010530
091300010530
091400990920      /TITLE Invio dei dati al punto operativo.
091500000613     C     invio         BEGSR
091600990920     C*
091700990920     C                   reset                   dscmz
091800010530     C                   move      invfil        cmzdst
091900990920     C                   eval      cmzfld = vlrfou
092000990920     C                   eval      cmzmbd = vlrhdl
092100990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
092200000710     C                   if        prmfir = *blanks
092300020913     C                   eval      cmzfla = 'FIVAB00F'
092400020913     C                   eval      cmzmba = 'FIVAB00F'
092500000710     C                   else
092600000710     C                   eval      cmzfla = prmfir
092700000710     C                   eval      cmzmba = prmfir
092800000710     C                   endif
092900990920     C                   eval      cmznrr = *zeros
093000990920     C                   move      §ctrok        cmznrr
093100021003     C                   eval      cmzlba = vlrfl1
093200990920     C                   call(e)   'TIS711C'
093300990920     C                   parm                    dscmz
093400990921     C                   parm      *blanks       esito
093500990920     C                   if        %error
093600990920     C                             or cmzerr = '1'
093700990921     C                             or esito  = '1'
093800000710     C                   eval      wrkesito = '3'
093900990920     C                   endif
094000990920     C*
094100000613     C                   ENDSR
094200000613     C***
094300010530
094400010530
094500010530
094600010530
094700010530      /TITLE Invio dei dati al punto operativo.
094800010530     C     opeini        BEGSR
094900010530     C*
095000010530     C* Inizializzo flag e contatori operativi
095100010530     C                   movel     '0'           flgGiro           1
095200010530     C                   movel     '0'           flgMulti          1
095300010530     C                   movel     '1'           flgStato          1
095400010615     C                   movel     '0'           flgOk             1
095500010530     C                   z-add     *zeros        cntNonEl         10 0
095600010530     C                   z-add     *zeros        depfil            3 0
095700010530     C                   z-add     *zeros        invfil            3 0
095800030407     C*
095900030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
096000030407     C                   if        %subst(vlrppt:1:1) = 'S'
096100030407     C                   seton                                        50
096200030407     C                   else
096300030407     C                   setoff                                       50
096400030407     C                   endif
096500041001     C*
096600041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
096700041001     C                   if        %subst(vlrppt:2:1) = 'S'
096800041001     C                   seton                                        51
096900041001     C                   else
097000041001     C                   setoff                                       51
097100041001     C                   endif
097200041025     C*
097300041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
097400041025     C* (naturalmente solo se nn già presente)
097500041025     C                   if        %subst(vlrppt:3:1) = 'S'
097600041025     C                   seton                                        52
097700041025     C                   else
097800041025     C                   setoff                                       52
097900041025     C                   endif
098000010530     C*
098100010530     C                   ENDSR
098200010530     C***
098300040407
098400040407
098500040407
098600040407
098700040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
098800040407     C     exeBon        BEGSR
098900040407     C*
099000040407     C* Imposto i parametri x il *pgm d "bonifica"
099100040407     C                   EVAL      ElencoChar = *blanks
099200040407     C                   EVAL      TipoElenco = *blanks
099300040407     C                   EVAL      CharSost   = *blanks
099400040629     C                   EVAL      UpperCase  = '1'
099500040407     C                   EVAL      ChkNull    = '1'
099600040407     C                   EVAL      CharNull   = *blanks
099700040629     C*
099800040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
099900040629     C                   EVAL      TxtInOut   = VABRSD
100000040629     C                   EVAL      EsitoBon   = *blanks
100100040407     C*
100200040407     C                   CALL      'XCHKCHAR'
100300040407     C                   PARM                    TxtInOut
100400040407     C                   PARM                    ElencoChar
100500040407     C                   PARM                    TipoElenco
100600040407     C                   PARM                    CharSost
100700040407     C                   PARM                    UpperCase
100800040407     C                   PARM                    ChkNull
100900040407     C                   PARM                    CharNull
101000040407     C                   PARM                    EsitoBon
101100040407     C*
101200040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
101300040407     C                   IF        EsitoBon = '1'
101400040629     C                   EVAL      VABRSD = TxtInOut
101500040407     C                   ENDIF
101600040629     C*
101700040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
101800040629     C                   EVAL      TxtInOut   = VABIND
101900040629     C                   EVAL      EsitoBon   = *blanks
102000040629     C*
102100040629     C                   CALL      'XCHKCHAR'
102200040629     C                   PARM                    TxtInOut
102300040629     C                   PARM                    ElencoChar
102400040629     C                   PARM                    TipoElenco
102500040629     C                   PARM                    CharSost
102600040629     C                   PARM                    UpperCase
102700040629     C                   PARM                    ChkNull
102800040629     C                   PARM                    CharNull
102900040629     C                   PARM                    EsitoBon
103000040629     C*
103100040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
103200040629     C                   IF        EsitoBon = '1'
103300040629     C                   EVAL      VABIND = TxtInOut
103400040629     C                   ENDIF
103500040629     C*
103600040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
103700040629     C                   EVAL      TxtInOut   = VABLOD
103800040629     C                   EVAL      EsitoBon   = *blanks
103900040629     C*
104000040629     C                   CALL      'XCHKCHAR'
104100040629     C                   PARM                    TxtInOut
104200040629     C                   PARM                    ElencoChar
104300040629     C                   PARM                    TipoElenco
104400040629     C                   PARM                    CharSost
104500040629     C                   PARM                    UpperCase
104600040629     C                   PARM                    ChkNull
104700040629     C                   PARM                    CharNull
104800040629     C                   PARM                    EsitoBon
104900040629     C*
105000040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
105100040629     C                   IF        EsitoBon = '1'
105200040629     C                   EVAL      VABLOD = TxtInOut
105300040629     C                   ENDIF
105400040629     C*
105500040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
105600040629     C                   EVAL      TxtInOut   = VABNOT
105700040629     C                   EVAL      EsitoBon   = *blanks
105800040630     C                   EVAL      UpperCase  = *blanks
105900040629     C*
106000040629     C                   CALL      'XCHKCHAR'
106100040629     C                   PARM                    TxtInOut
106200040629     C                   PARM                    ElencoChar
106300040629     C                   PARM                    TipoElenco
106400040629     C                   PARM                    CharSost
106500040629     C                   PARM                    UpperCase
106600040629     C                   PARM                    ChkNull
106700040629     C                   PARM                    CharNull
106800040629     C                   PARM                    EsitoBon
106900040629     C*
107000040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
107100040629     C                   IF        EsitoBon = '1'
107200040629     C                   EVAL      VABNOT = TxtInOut
107300040629     C                   ENDIF
107400040629     C*
107500040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
107600040629     C                   EVAL      TxtInOut   = VABNT2
107700040629     C                   EVAL      EsitoBon   = *blanks
107800040630     C                   EVAL      UpperCase  = *blanks
107900040629     C*
108000040629     C                   CALL      'XCHKCHAR'
108100040629     C                   PARM                    TxtInOut
108200040629     C                   PARM                    ElencoChar
108300040629     C                   PARM                    TipoElenco
108400040629     C                   PARM                    CharSost
108500040629     C                   PARM                    UpperCase
108600040629     C                   PARM                    ChkNull
108700040629     C                   PARM                    CharNull
108800040629     C                   PARM                    EsitoBon
108900040629     C*
109000040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
109100040629     C                   IF        EsitoBon = '1'
109200040629     C                   EVAL      VABNT2 = TxtInOut
109300040629     C                   ENDIF
109400040407     C*
109500040407     C                   ENDSR
109600010530
109700060410
109800060410     C*----------------------------------------------------*
109900060410     C*  CONTROLLO NUMERICITA' CAMPI
110000060410     C*----------------------------------------------------*
110100060410     C     CHKNUM        BEGSR
110200060410     C*
110300060410     C                   IF        PiDecChr = *blanks
110400060410     C                   EVAL      PiDecChr = ','
110500060410     C                   ENDIF
110600060410     C*
110700060410     C                   CALL(e)   'ISNUMERIC'
110800060410     C                   PARM                    PiStr            30
110900060410     C                   PARM                    PiDecChr          1
111000060410     C                   PARM      *ZEROS        PiVal            30 9
111100060410     C                   PARM      '0'           PiInt             1
111200060410     C                   PARM      '0'           PiNum             1
111300060410     C                   IF        %error
111400060410     C                   EVAL      PiNum=*off
111500060410     C                   ENDIF
111600060410     C*
111700060410     C                   ENDSR
111800060410     C***
111900060410
112000990910
112100000613     C     *inzsr        BEGSR
112200990910     C*
112300990910     C     *entry        plist
112400990920     C                   parm                    tivlrds
112500990921     C                   parm      wrkesito      esito
112600000724     C                   parm                    prmlit
112700000710     C                   parm                    prmfir
112800010606     C*
112900010606     C* CALCOLA LA DATA CORRENTE
113000010606     C                   time                    wn14             14 0
113100010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
113200010606     C                   z-add     wn8           g08dat
113300010606     C                   z-add     *zeros        g08inv
113400010606     C                   movel     '0'           g08err
113500010606     C                   call      'XSRDA8'
113600010606     C                   parm                    wlbda8
113700010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
113800000613     C*
113900000613     C                   ENDSR
114000000613     C***
114100990908
114200010601     Otis7t5p   E            riepilogo         2
114300990915     O                                              'Upload via Internet'
114400990915     O                                           +1 'Traduzione TIVIN00R -'
114500020913     O                                           +1 'FIVABWWR'
114600990915     O                                           +1 'Testate bolle'
114700990915     O          E            riepilogo   2
114800990915     O                       wrkdata
114900990915     O                       wrkora              +1
115000990915     O                       procname            +1
115100990915     O          E            riepilogo   2
115200990915     O                                              'Cliente..................:'
115300990915     O                       VABCCM        z     +1
115400990915     O          E            riepilogo   2
115500990920     O                                              'Riferimento Strategi.....:'
115600990920     O                       vlrhdl              +1
115700990915     O          E            riepilogo   2
115800990915     O                                              'Giusti...................:'
115900971022     O                       §CTROK        2   +  1
116000990915     O          E            riepilogo   2
116100990915     O                                              'Sbagliati e corretti.....:'
116200971022     O                       §CTRMO        2   +  1
116300990915     O          E            riepilogo   2
116400990915     O                                              'Sbagliati e scartati.....:'
116500971022     O                       §CTRNO        2   +  1
116600000613
116700010601     Otis7t5ps  E            testdett          2
116800000613     O                                              'Upload via Internet'
116900000613     O                                           +1 'Traduzione TIVIN00R -'
117000020913     O                                           +1 'FIVABWWR'
117100000613     O                                           +1 'Testate bolle'
117200000616     O          E            testdett    3
117300000613     O                                           +2 'N° rec'
117400000613     O                                           +3 'Anteprima contenuto'
117500000616     O          E            rigadett    2
117600000613     O                       rrnum               +2
117700000621     O                       recko               +3
117800000616     O          E            findett     2
117900000613     O                       wrkdata
118000000613     O                       wrkora              +1
118100000613     O                       procname            +1
118200000616     O          E            findett     2
118300000613     O                                              'Cliente..................:'
118400000613     O                       VABCCM        z     +1
118500000616     O          E            findett     2
118600000613     O                                              'Riferimento Strategi.....:'
118700000613     O                       vlrhdl              +1
118800000616     O          E            findett     2
118900000613     O                                              'Giusti...................:'
119000000613     O                       §CTROK        2   +  1
119100000616     O          E            findett     2
119200000613     O                                              'Sbagliati e corretti.....:'
119300000613     O                       §CTRMO        2   +  1
119400000616     O          E            findett     2
119500000613     O                                              'Sbagliati e scartati.....:'
119600000613     O                       §CTRNO        2   +  1
119700000512** CMD - COMANDI CL
119800011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T5P)  FORMTYPE(RICCLI) OUTQ(
119900011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T5PS) FORMTYPE(RICCLI) OUTQ(
120000010601DLTOVR FILE(TIS7T5P TIS7T5PS) LVL(*)
120100000512
120200000512
