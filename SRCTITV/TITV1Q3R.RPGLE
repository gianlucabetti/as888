000100020913      /TITLE Upload via Internet: traduzione in FIVABWWR.
000200090611     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
007200090611
007300090611     D*------------------
007400090611     D* LINKING A DEFINIZIONI ESTERNE
007500090611     D*------------------
007600090611     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
007700090611     D/COPY GAITRASRC/SRCPROTOPI,UBISNUM
007800990908
007900990915     C                   time                    wrkdata
008000990915     C                   time                    wrkora
008100000913     C                   reset                   rrnum
008200990921     C                   reset                   esito
008300990921     C                   reset                   wrkesito
008400010530     C*
008500010530     C                   exsr      opeini
008600010530     C                   exsr      rwvab
008700041025     C*
008800041025     C* Esegue lancio TISI95R solo x chiusura
008900041025     C                   CLEAR                   TISI95DS
009000041025     C                   EVAL      I95TLA = 'C'
009100041025     C                   CALL      'TISI95R'
009200041025     C                   PARM                    TISI95DS
009300010530     C*
009400010530     C                   seton                                        lr
009500010530
009600010530
009700010530
009800010530
009900010530     C*--------------------------------------------------------
010000010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010100010530     C*--------------------------------------------------------
010200010530     C     PREELA        BEGSR
010300000724     C*
010400000724     C* SE OCCORRE SPEDIRE IN FILIALE
010500010530     C                   if        invfil <> *zeros and
010600010530     C                             flgGiro = '0'
010700010530     C*
010800010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010900010530     C                   eval      flgGiro = '1'
011000000724     C*
011100000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011200000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011300010530     C     invfil        chain     azorg01l
011400000724     C                   if        %found
011500000616     C                   movel(p)  CMD(1)        depcmd
011600020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011700000616     C*
011800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011900011123     C                   Z-ADD     150           LENGH            15 5
012000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012100000616     C                   PARM                    depcmd
012200000616     C                   PARM                    LENGH
012300000724     C*
012400000724     C                   endif
012500000616     C*
012600000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012700000616     C                   movel(p)  CMD(2)        depcmd
012800000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012900000616     C*
013000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013100011123     C                   Z-ADD     150           LENGH            15 5
013200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013300000616     C                   PARM                    depcmd
013400000616     C                   PARM                    LENGH
013500000616     C*
013600010601     C                   if        not %open(tis7t5ps)
013700010601     C                   open      tis7t5ps
013800030407     C   50              except    testdett
013900000613     C                   endif
014000010530     C*
014100010530     C                   endif
014200010530     C*
014300010530     C                   ENDSR
014400010530     C***
014500010530
014600010530
014700010530
014800010530
014900010530
015000010530
015100010530     C*--------------------------------------------------------
015200010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015300010530     C*--------------------------------------------------------
015400010530     C     ENDELA        BEGSR
015500010530     C*
015600010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015700000613     C*
015800010601     C                   if        %open(tis7t5ps)
015900030407     C   50              except    findett
016000010601     C                   close     tis7t5ps
016100000613     C                   endif
016200000616     C*
016300000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016400011123     C                   Z-ADD     150           LENGH            15 5
016500010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
016600000616     C                   PARM                    CMD(3)
016700000616     C                   PARM                    LENGH
016800010530     C*
016900010530     C                   ENDSR
017000010530     C***
017100000613
017200010530
017300010530
017400000613
017500000613     C*--------------------------------------------------------
017600000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
017700000613     C*--------------------------------------------------------
017800000621     C     STPR          BEGSR
017900000613     C*
018000010601     C                   if        not %open(tis7t5p)
018100010601     C                   open      tis7t5p
018200990915     C                   endif
018300990915     C*
018400030407     C   50              except    riepilogo
018500990915     C*
018600010601     C                   if        %open(tis7t5p)
018700010601     C                   close     tis7t5p
018800990914     C                   endif
018900990910     C*
019000000613     C                   ENDSR
019100000613     C***
019200990908
019300010530
019400010530
019500010530
019600910830     C*--------------------------------------------------------
019700020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
019800910830     C*--------------------------------------------------------
019900941229     C     RWVAB         BEGSR
020000990910     C*
020100990914     C                   if        not %open(tivin00r)
020200990908     C                   open      tivin00r
020300990914     C                   endif
020400020913     C                   if        not %open(fivabwwr)
020500020913     C                   open      fivabwwr
020600990914     C                   endif
020700990910     C*
020800990910     C                   clear                   §CTROK
020900990910     C                   clear                   §CTRMO
021000990910     C                   clear                   §CTRNO
021100990910     C*
021200921023     C                   DO        *HIVAL
021300990913     C*
021400990915     C                   READ      tivin00r                               70
021500010618     C*
021600010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021700010618     C                   if        vinflg = '1'
021800010618     C                   eval      flgOk = '1'
021900010618     C                   endif
022000010705     C*
022100010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
022200010705     C                   if        %subst(vindta:1:1) = '='
022300010705     C                   eval      %subst(vindta:1:1) = *blanks
022400010705     C                   endif
022500010705     C*
022600051017     C                   if        vindta  > *blanks
022700000613     C                   add       1             rrnum
022800010601     C* ...elimino eventuali segni
022900010601     C     '+':' '       xlate     vindta        vindta
023000990913     C*
023100010601     C                   if        *in70 = *off and
023200990913     C                             (vinflg = *blanks
023300010711     C                              or vinflg = '0'
023400010711     C                              or vinflg = '2')
023500990913     C*
023600020913     C                   clear                   FIVAB000
023700010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023800010711     C                   if        vinflg = *blanks or vinflg = '0'
023900010711     C                   clear                   vinmsg
024000010711     C                   endif
024100010711     C*
024200990913     C                   eval      fnvabaar = vindta
024300991022     C*
024400991022     C                   testb     x'3F'         vhbatb                   02
024500991022     C                   if        not *in02
024600991022     C*
024700971021     C                   SETOFF                                       303132
024800971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
024900971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
025000971023     C*    OPPURE TUTTI BLANKS
025100010530     C*
025200010530     C                   TESTN                   VHBLNP               30
025300010530     C  N30              TESTN                   VHBLNP                 30
025400010530     C  N30              TESTN                   VHBLNP                   30
025500010530     C                   MOVE      VHBLNP        W001A             1
025600010530     C     *IN30         IFEQ      *OFF
025700010530     C     W001A         ORLT      '0'
025800010530     C                   SETON                                        31
025900010530     C                   eval      vinmsg = %trimr(vinmsg)
026000010530     C                             + ' ' + 'VHBLNP'
026100010530     C                   ELSE
026200010530     C                   MOVE      VHBLNP        VABLNP
026300010530     C     VABLNP        IFLE      *ZEROS
026400010530     C                   SETON                                        31
026500010530     C                   eval      vinmsg = %trimr(vinmsg)
026600010530     C                             + ' ' + 'VHBLNP'
026700010530     C                   ENDIF
026800010530     C                   ENDIF
026900010530     C*
027000010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
027100010530     C                   eval      depfil = VABLNP
027200010530     C                   exsr      repfil
027300010601     C                   if        depfil = invfil
027400021014     C                   if        vlrpoi = 999
027500021010     C                   MOVE(P)   invfil        VABFGS
027600021014     C                   else
027700021014     C                   MOVE(P)   vlrpoi        VABFGS
027800021014     C                   endif
027900010530     C*
028000010530     C                   exsr      PREELA
028100010530     C*
028200971023     C                   TESTN                   VHBCCM               30
028300971023     C  N30              TESTN                   VHBCCM                 30
028400971023     C  N30              TESTN                   VHBCCM                   30
028500971021     C                   MOVE      VHBCCM        W001A             1
028600971021     C     *IN30         IFEQ      *OFF
028700971021     C     W001A         ORLT      '0'
028800971021     C                   SETON                                        31
028900010604     C                   eval      vinmsg = %trimr(vinmsg)
029000010604     C                             + ' ' + 'VHBCCM'
029100971021     C                   ELSE
029200971021     C                   MOVE      VHBCCM        VABCCM
029300971024     C     VABCCM        IFLE      *ZEROS
029400971024     C                   SETON                                        31
029500010604     C                   eval      vinmsg = %trimr(vinmsg)
029600010604     C                             + ' ' + 'VHBCCM'
029700971024     C                   ENDIF
029800971021     C                   ENDIF
029900971021     C*
030000971023     C                   TESTN                   VHBAAS               30
030100971023     C  N30              TESTN                   VHBAAS                 30
030200971023     C  N30              TESTN                   VHBAAS                   30
030300971021     C                   MOVE      VHBAAS        W001A             1
030400971021     C     *IN30         IFEQ      *OFF
030500971021     C     W001A         ORLT      '0'
030600971023     C     W001A         ANDNE     *BLANK
030700971021     C                   SETON                                        32
030800010606     C                   MOVEL     datcor        VABAAS
030900010604     C                   eval      vinmsg = %trimr(vinmsg)
031000010604     C                             + ' ' + 'VHBAAS'
031100971021     C                   ELSE
031200971021     C                   MOVE      VHBAAS        VABAAS
031300971021     C                   ENDIF
031400971021     C*
031500971023     C                   TESTN                   VHBMGS               30
031600971023     C  N30              TESTN                   VHBMGS                 30
031700971023     C  N30              TESTN                   VHBMGS                   30
031800971021     C                   MOVE      VHBMGS        W001A             1
031900971021     C     *IN30         IFEQ      *OFF
032000971021     C     W001A         ORLT      '0'
032100971023     C     W001A         ANDNE     *BLANK
032200971021     C                   SETON                                        32
032300010601     C                   MOVE      '0101'        VABMGS
032400010604     C                   eval      vinmsg = %trimr(vinmsg)
032500010604     C                             + ' ' + 'VHBMGS'
032600971021     C                   ELSE
032700971021     C                   MOVE      VHBMGS        VABMGS
032800971021     C                   ENDIF
032900971021     C*
033000971023     C                   TESTN                   VHBNRS               30
033100971023     C  N30              TESTN                   VHBNRS                 30
033200971023     C  N30              TESTN                   VHBNRS                   30
033300971021     C                   MOVE      VHBNRS        W001A             1
033400971021     C     *IN30         IFEQ      *OFF
033500971021     C     W001A         ORLT      '0'
033600971023     C     W001A         ANDNE     *BLANK
033700971021     C                   SETON                                        32
033800971021     C                   MOVE      *ZERO         VABNRS
033900010604     C                   eval      vinmsg = %trimr(vinmsg)
034000010604     C                             + ' ' + 'VHBNRS'
034100971021     C                   ELSE
034200971021     C                   MOVE      VHBNRS        VABNRS
034300971021     C                   ENDIF
034400971021     C*
034500971023     C                   TESTN                   VHBNSP               30
034600971023     C  N30              TESTN                   VHBNSP                 30
034700971023     C  N30              TESTN                   VHBNSP                   30
034800971021     C                   MOVE      VHBNSP        W001A             1
034900971021     C     *IN30         IFEQ      *OFF
035000971021     C     W001A         ORLT      '0'
035100971023     C     W001A         ANDNE     *BLANK
035200971021     C                   SETON                                        32
035300971021     C                   MOVE      *ZERO         VABNSP
035400010604     C                   eval      vinmsg = %trimr(vinmsg)
035500010604     C                             + ' ' + 'VHBNSP'
035600971021     C                   ELSE
035700971021     C                   MOVE      VHBNSP        VABNSP
035800971021     C                   ENDIF
035900971021     C*
036000971023     C                   TESTN                   VHBLNA               30
036100971023     C  N30              TESTN                   VHBLNA                 30
036200971023     C  N30              TESTN                   VHBLNA                   30
036300971021     C                   MOVE      VHBLNA        W001A             1
036400971021     C     *IN30         IFEQ      *OFF
036500971021     C     W001A         ORLT      '0'
036600971023     C     W001A         ANDNE     *BLANK
036700971021     C                   SETON                                        32
036800971021     C                   MOVE      *ZERO         VABLNA
036900010604     C                   eval      vinmsg = %trimr(vinmsg)
037000010604     C                             + ' ' + 'VHBLNA'
037100971021     C                   ELSE
037200971021     C                   MOVE      VHBLNA        VABLNA
037300971021     C                   ENDIF
037400971021     C*
037500971023     C                   TESTN                   VHBCTR               30
037600971023     C  N30              TESTN                   VHBCTR                 30
037700971023     C  N30              TESTN                   VHBCTR                   30
037800971021     C                   MOVE      VHBCTR        W001A             1
037900971021     C     *IN30         IFEQ      *OFF
038000971021     C     W001A         ORLT      '0'
038100971023     C     W001A         ANDNE     *BLANK
038200971021     C                   SETON                                        32
038300971021     C                   MOVE      *ZERO         VABCTR
038400010604     C                   eval      vinmsg = %trimr(vinmsg)
038500010604     C                             + ' ' + 'VHBCTR'
038600971021     C                   ELSE
038700971021     C                   MOVE      VHBCTR        VABCTR
038800971021     C                   ENDIF
038900941229     C*
039000971021     C                   MOVE      *ZERO         VABIAS
039100941229     C                   MOVE      VHBIAS        WASC15
039200971023     C                   TESTN                   WI15                 30
039300971023     C  N30              TESTN                   WI15                   30
039400971023     C  N30              TESTN                   WI15                     30
039500971021     C                   MOVE      WI15          W001A             1
039600971021     C     *IN30         IFEQ      *OFF
039700971021     C     W001A         ORLT      '0'
039800971023     C     W001A         ANDNE     *BLANK
039900971021     C                   SETON                                        32
040000971021     C                   MOVE      *ZERO         VABIAS
040100010604     C                   eval      vinmsg = %trimr(vinmsg)
040200010604     C                             + ' ' + 'VHBIAS'
040300971021     C                   ELSE
040400971021     C                   MOVEL     WI15          VABIAS
040500971023     C                   TESTN                   WD15                 30
040600971023     C  N30              TESTN                   WD15                   30
040700971023     C  N30              TESTN                   WD15                     30
040800971021     C                   MOVE      WD15          W001A             1
040900971021     C     *IN30         IFEQ      *OFF
041000971021     C     W001A         ORLT      '0'
041100971023     C     W001A         ANDNE     *BLANK
041200971021     C                   SETON                                        32
041300971021     C                   MOVE      *ZERO         VABIAS
041400010604     C                   eval      vinmsg = %trimr(vinmsg)
041500010604     C                             + ' ' + 'VHBIAS'
041600971021     C                   ELSE
041700971021     C                   MOVE      WD15          VABIAS
041800971021     C                   ENDIF
041900050118     C                   ENDIF
042000941229     C*
042100971023     C                   TESTN                   VHBNCL               30
042200971023     C  N30              TESTN                   VHBNCL                 30
042300971023     C  N30              TESTN                   VHBNCL                   30
042400971021     C                   MOVE      VHBNCL        W001A             1
042500971021     C     *IN30         IFEQ      *OFF
042600971021     C     W001A         ORLT      '0'
042700971021     C                   SETON                                        32
042800971021     C                   MOVE      *ZERO         VABNCL
042900010604     C                   eval      vinmsg = %trimr(vinmsg)
043000010604     C                             + ' ' + 'VHBNCL'
043100971021     C                   ELSE
043200971021     C                   MOVE      VHBNCL        VABNCL
043300971021     C                   ENDIF
043400941229     C*
043500971021     C                   MOVE      *ZERO         VABPKB
043600971021     C                   MOVE      VHBPKB        WASC09
043700010719     C                   IF        WI09 = *blanks
043800010719     C                   EVAL      WI09 = *zeros
043900010719     C                   ENDIF
044000971023     C                   TESTN                   WI09                 30
044100971023     C  N30              TESTN                   WI09                   30
044200971023     C  N30              TESTN                   WI09                     30
044300971021     C                   MOVE      WI09          W001A             1
044400971021     C     *IN30         IFEQ      *OFF
044500971021     C     W001A         ORLT      '0'
044600971021     C                   SETON                                        32
044700971021     C                   MOVE      *ZERO         VABPKB
044800010604     C                   eval      vinmsg = %trimr(vinmsg)
044900010604     C                             + ' ' + 'VHBPKB'
045000971021     C                   ELSE
045100971021     C                   MOVEL     WI09          VABPKB
045200010719     C                   IF        WD09 = *blanks
045300010719     C                   EVAL      WD09 = *zeros
045400010719     C                   ENDIF
045500971023     C                   TESTN                   WD09                 30
045600971023     C  N30              TESTN                   WD09                     30
045700971021     C                   MOVE      WD09          W001A             1
045800971021     C     *IN30         IFEQ      *OFF
045900971021     C     W001A         ORLT      '0'
046000971023     C     W001A         ANDNE     *BLANK
046100971021     C                   SETON                                        32
046200971021     C                   MOVE      *ZERO         VABPKB
046300010604     C                   eval      vinmsg = %trimr(vinmsg)
046400010604     C                             + ' ' + 'VHBPKB'
046500971021     C                   ELSE
046600971021     C                   MOVE      WD09          VABPKB
046700971021     C                   ENDIF
046800050118     C                   ENDIF
046900971021     C*
047000041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
047100041001     C                   IF        *IN51 = *OFF
047200971021     C                   MOVE      *ZERO         VABVLB
047300971021     C                   MOVE      VHBVLB        WASC07
047400010719     C                   IF        WI07 = *blanks
047500010719     C                   EVAL      WI07 = *zeros
047600010719     C                   ENDIF
047700971023     C                   TESTN                   WI07                 30
047800971023     C  N30              TESTN                   WI07                   30
047900971023     C  N30              TESTN                   WI07                     30
048000971021     C                   MOVE      WI07          W001A             1
048100971021     C     *IN30         IFEQ      *OFF
048200971021     C     W001A         ORLT      '0'
048300971023     C     W001A         ANDNE     *BLANK
048400971021     C                   SETON                                        32
048500971021     C                   MOVE      *ZERO         VABVLB
048600010604     C                   eval      vinmsg = %trimr(vinmsg)
048700010604     C                             + ' ' + 'VHBVLB'
048800971021     C                   ELSE
048900971021     C                   MOVEL     WI07          VABVLB
049000010719     C                   IF        WD07 = *blanks
049100010719     C                   EVAL      WD07 = *zeros
049200010719     C                   ENDIF
049300971023     C                   TESTN                   WD07                 30
049400971023     C  N30              TESTN                   WD07                   30
049500971023     C  N30              TESTN                   WD07                     30
049600971021     C                   MOVE      WD07          W001A             1
049700971021     C     *IN30         IFEQ      *OFF
049800971021     C     W001A         ORLT      '0'
049900971023     C     W001A         ANDNE     *BLANK
050000971021     C                   SETON                                        32
050100971021     C                   MOVE      *ZERO         VABVLB
050200010604     C                   eval      vinmsg = %trimr(vinmsg)
050300010604     C                             + ' ' + 'VHBVLB'
050400971021     C                   ELSE
050500971021     C                   MOVE      WD07          VABVLB
050600971021     C                   ENDIF
050700050118     C                   ENDIF
050800041001     C                   ELSE
050900041001     C* ...altrimenti forzo azzeramento del volume
051000041001     C                   Z-ADD     *zeros        VABVLB
051100041001     C                   ENDIF
051200941229     C*
051300971021     C                   MOVE      *ZERO         VABQFT
051400971021     C                   MOVE      VHBQFT        WASC15
051500010719     C                   IF        WI15 = *blanks
051600010719     C                   EVAL      WI15 = *zeros
051700010719     C                   ENDIF
051800971023     C                   TESTN                   WI15                 30
051900971023     C  N30              TESTN                   WI15                   30
052000971023     C  N30              TESTN                   WI15                     30
052100971021     C                   MOVE      WI15          W001A             1
052200971021     C     *IN30         IFEQ      *OFF
052300971021     C     W001A         ORLT      '0'
052400971023     C     W001A         ANDNE     *BLANK
052500971021     C                   SETON                                        32
052600971021     C                   MOVE      *ZERO         VABQFT
052700010604     C                   eval      vinmsg = %trimr(vinmsg)
052800010604     C                             + ' ' + 'VHBQFT'
052900971021     C                   ELSE
053000971021     C                   MOVEL     WI15          VABQFT
053100010719     C                   IF        WD15 = *blanks
053200010719     C                   EVAL      WD15 = *zeros
053300010719     C                   ENDIF
053400971023     C                   TESTN                   WD15                 30
053500971023     C  N30              TESTN                   WD15                   30
053600971023     C  N30              TESTN                   WD15                     30
053700971021     C                   MOVE      WD15          W001A             1
053800971021     C     *IN30         IFEQ      *OFF
053900971021     C     W001A         ORLT      '0'
054000971023     C     W001A         ANDNE     *BLANK
054100971021     C                   SETON                                        32
054200971021     C                   MOVE      *ZERO         VABQFT
054300010604     C                   eval      vinmsg = %trimr(vinmsg)
054400010604     C                             + ' ' + 'VHBQFT'
054500971021     C                   ELSE
054600971021     C                   MOVE      WD15          VABQFT
054700971021     C                   ENDIF
054800050118     C                   ENDIF
054900941229     C*
055000971021     C                   MOVE      *ZERO         VABCAS
055100060410     C* Se presente separatore decimale alla giusta posizione => forzo sempre la virgola
055200060410     C                   EVAL      wVHBCAS = VHBCAS
055300060410     C                   IF        %subst(wVHBCAS:12:1) = '.'
055400060410     C                   EVAL      %subst(wVHBCAS:12:1) = ','
055500060410     C                   ENDIF
055600060410     C*
055700060410     C                   MOVEL(P)  wVHBCAS       PiStr
055800060410     C                   EXSR      CHKNUM
055900060410     C                   IF        PiNum=*on
056000060410     C                   Z-ADD     PiVal         VABCAS
056100060410     C                   ELSE
056200060410     C                   SETON                                        32
056300060410     C                   Z-ADD     *zeros        VABCAS
056400060410     C                   EVAL      vinmsg = %trimr(vinmsg)
056500060410     C                             + ' ' + 'VHBCAS'
056600060410     C                   ENDIF
056700971021     C*
056800971023     C                   TESTN                   VHBRMN               30
056900971023     C  N30              TESTN                   VHBRMN                 30
057000971023     C  N30              TESTN                   VHBRMN                   30
057100971021     C                   MOVE      VHBRMN        W001A             1
057200971021     C     *IN30         IFEQ      *OFF
057300971021     C     W001A         ORLT      '0'
057400971021     C                   SETON                                        32
057500971021     C                   MOVE      *ZERO         VABRMN
057600010604     C                   eval      vinmsg = %trimr(vinmsg)
057700010604     C                             + ' ' + 'VHBRMN'
057800971021     C                   ELSE
057900971021     C                   MOVE      VHBRMN        VABRMN
058000971021     C                   ENDIF
058100971021     C*
058200971023     C                   TESTN                   VHBNCD               30
058300971023     C  N30              TESTN                   VHBNCD                 30
058400971023     C  N30              TESTN                   VHBNCD                   30
058500971021     C                   MOVE      VHBNCD        W001A             1
058600971021     C     *IN30         IFEQ      *OFF
058700971021     C     W001A         ORLT      '0'
058800971023     C     W001A         ANDNE     *BLANK
058900971021     C                   SETON                                        32
059000971021     C                   MOVE      *ZERO         VABNCD
059100010604     C                   eval      vinmsg = %trimr(vinmsg)
059200010604     C                             + ' ' + 'VHBNCD'
059300971021     C                   ELSE
059400971021     C                   MOVE      VHBNCD        VABNCD
059500971021     C                   ENDIF
059600971021     C*
059700971023     C                   TESTN                   VHBNCA               30
059800971023     C  N30              TESTN                   VHBNCA                 30
059900971023     C  N30              TESTN                   VHBNCA                   30
060000971021     C                   MOVE      VHBNCA        W001A             1
060100971021     C     *IN30         IFEQ      *OFF
060200971021     C     W001A         ORLT      '0'
060300971023     C     W001A         ANDNE     *BLANK
060400971021     C                   SETON                                        32
060500971021     C                   MOVE      *ZERO         VABNCA
060600010604     C                   eval      vinmsg = %trimr(vinmsg)
060700010604     C                             + ' ' + 'VHBNCA'
060800971021     C                   ELSE
060900971021     C                   MOVE      VHBNCA        VABNCA
061000971021     C                   ENDIF
061100971021     C*
061200971023     C                   TESTN                   VHBZNC               30
061300971023     C  N30              TESTN                   VHBZNC                 30
061400971023     C  N30              TESTN                   VHBZNC                   30
061500971021     C                   MOVE      VHBZNC        W001A             1
061600971021     C     *IN30         IFEQ      *OFF
061700971021     C     W001A         ORLT      '0'
061800971023     C     W001A         ANDNE     *BLANK
061900971021     C                   SETON                                        32
062000971021     C                   MOVE      *ZERO         VABZNC
062100010604     C                   eval      vinmsg = %trimr(vinmsg)
062200010604     C                             + ' ' + 'VHBZNC'
062300971021     C                   ELSE
062400971021     C                   MOVE      VHBZNC        VABZNC
062500971021     C                   ENDIF
062600971021     C*
062700971023     C                   TESTN                   VHBDCR               30
062800971023     C  N30              TESTN                   VHBDCR                 30
062900971023     C  N30              TESTN                   VHBDCR                   30
063000971021     C                   MOVE      VHBDCR        W001A             1
063100971021     C     *IN30         IFEQ      *OFF
063200971021     C     W001A         ORLT      '0'
063300971023     C     W001A         ANDNE     *BLANK
063400971021     C                   SETON                                        32
063500971021     C                   MOVE      *ZERO         VABDCR
063600010604     C                   eval      vinmsg = %trimr(vinmsg)
063700010604     C                             + ' ' + 'VHBDCR'
063800971021     C                   ELSE
063900971021     C                   MOVE      VHBDCR        VABDCR
064000971021     C                   ENDIF
064100971021     C*
064200971023     C                   TESTN                   VHBHCR               30
064300971023     C  N30              TESTN                   VHBHCR                 30
064400971023     C  N30              TESTN                   VHBHCR                   30
064500971021     C                   MOVE      VHBHCR        W001A             1
064600971021     C     *IN30         IFEQ      *OFF
064700971021     C     W001A         ORLT      '0'
064800971023     C     W001A         ANDNE     *BLANK
064900971021     C                   SETON                                        32
065000971021     C                   MOVE      *ZERO         VABHCR
065100010604     C                   eval      vinmsg = %trimr(vinmsg)
065200010604     C                             + ' ' + 'VHBHCR'
065300971021     C                   ELSE
065400971021     C                   MOVE      VHBHCR        VABHCR
065500971021     C                   ENDIF
065600941229     C*
065700971021     C                   MOVE      *ZERO         VABVMD
065800971021     C                   MOVE      VHBVMD        WASC15
065900010719     C                   IF        WI15 = *blanks
066000010719     C                   EVAL      WI15 = *zeros
066100010719     C                   ENDIF
066200971023     C                   TESTN                   WI15                 30
066300971023     C  N30              TESTN                   WI15                   30
066400971023     C  N30              TESTN                   WI15                     30
066500971021     C                   MOVE      WI15          W001A             1
066600971021     C     *IN30         IFEQ      *OFF
066700971021     C     W001A         ORLT      '0'
066800971023     C     W001A         ANDNE     *BLANK
066900971021     C                   SETON                                        32
067000971021     C                   MOVE      *ZERO         VABVMD
067100010604     C                   eval      vinmsg = %trimr(vinmsg)
067200010604     C                             + ' ' + 'VHBVMD'
067300971021     C                   ELSE
067400971021     C                   MOVEL     WI15          VABVMD
067500010719     C                   IF        WD15 = *blanks
067600010719     C                   EVAL      WD15 = *zeros
067700010719     C                   ENDIF
067800971023     C                   TESTN                   WD15                 30
067900971023     C  N30              TESTN                   WD15                   30
068000971023     C  N30              TESTN                   WD15                     30
068100971021     C                   MOVE      WD15          W001A             1
068200971021     C     *IN30         IFEQ      *OFF
068300971021     C     W001A         ORLT      '0'
068400971023     C     W001A         ANDNE     *BLANK
068500971021     C                   SETON                                        32
068600971021     C                   MOVE      *ZERO         VABVMD
068700010604     C                   eval      vinmsg = %trimr(vinmsg)
068800010604     C                             + ' ' + 'VHBVMD'
068900971021     C                   ELSE
069000971021     C                   MOVE      WD15          VABVMD
069100971021     C                   ENDIF
069200050118     C                   ENDIF
069300971021     C*
069400971023     C                   TESTN                   VHBANT               30
069500971023     C  N30              TESTN                   VHBANT                 30
069600971023     C  N30              TESTN                   VHBANT                   30
069700971021     C                   MOVE      VHBANT        W001A             1
069800971021     C     *IN30         IFEQ      *OFF
069900971021     C     W001A         ORLT      '0'
070000971023     C     W001A         ANDNE     *BLANK
070100971021     C                   SETON                                        32
070200971021     C                   MOVE      *ZERO         VABANT
070300010604     C                   eval      vinmsg = %trimr(vinmsg)
070400010604     C                             + ' ' + 'VHBANT'
070500971021     C                   ELSE
070600971021     C                   MOVE      VHBANT        VABANT
070700971021     C                   ENDIF
070800971021     C*
070900971021     C* CAMPI ALFABETICI:
071000971021     C                   MOVE      VHBATB        VABATB
071100971021     C                   MOVE      VHBCBO        VABCBO
071200971021     C                   MOVE      VHBRSD        VABRSD
071300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
071400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
071500020117     C* ==
071600971021     C                   MOVE      VHBRD2        VABRD2
071700971021     C                   MOVE      VHBIND        VABIND
071800971021     C                   MOVE      VHBCAD        VABCAD
071900971021     C                   MOVE      VHBLOD        VABLOD
072000971021     C                   MOVE      VHBPRD        VABPRD
072100971021     C                   MOVE      VHBNZD        VABNZD
072200971021     C                   MOVE      VHBGC1        VABGC1
072300971021     C                   MOVE      VHBGC2        VABGC2
072400971021     C                   MOVE      VHBTSP        VABTSP
072500971021     C                   MOVE      VHBVAS        VABVAS
072600971021     C                   MOVE      VHBNAS        VABNAS
072700971021     C                   MOVE      VHBTIC        VABTIC
072800971021     C                   MOVE      VHBVCA        VABVCA
072900971021     C                   MOVE      VHBGCA        VABGCA
073000971021     C                   MOVE      VHBRMA        VABRMA
073100971021     C                   MOVE      VHBXCO        VABXCO
073200971021     C                   MOVE      VHBNOT        VABNOT
073300971021     C                   MOVE      VHBNT2        VABNT2
073400971021     C                   MOVE      VHBCTM        VABCTM
073500971021     C                   MOVE      VHBFFD        VABFFD
073600971021     C                   MOVE      VHBTCR        VABTCR
073700971021     C                   MOVE      VHBCTS        VABCTS
073800971021     C                   MOVE      VHBFTM        VABFTM
073900971021     C                   MOVE      VHBVAD        VABVAD
074000971021     C                   MOVE      VHBGMA        VABGMA
074100971021     C                   MOVE      VHBGGA        VABGGA
074200971021     C                   MOVE      VHBGVA        VABGVA
074300971021     C                   MOVE      VHBTC1        VABTC1
074400971021     C                   MOVE      VHBTC2        VABTC2
074500971021     C                   MOVE      VHBSCL        VABSCL
074600971021     C                   MOVE      VHBRMO        VABRMO
074700971021     C                   MOVE      VHBCMO        VABCMO
074800971021     C                   MOVE      VHBNMO        VABNMO
074900971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
075000060329     C* NELLA NAZIONE MITTENTE ORIGINALE
075100060331     C                   IF        %trim(VHBNMO) < 'A'
075200060329     C                   MOVE      *BLANKS       VABNMO
075300060329     C                   ENDIF
075400050418     C*
075500050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
075600050418     C                   IF        %trim(VABNZD) = 'I'   OR
075700050418     C                             %trim(VABNZD) = 'IT'  OR
075800050418     C                             %trim(VABNZD) = 'ITA'
075900050418     C                   EVAL      VABNZD = *blanks
076000050418     C                   ENDIF
076100041025     C*
076200041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
076300041025     C                   IF        *IN52 = *ON
076400041025     C* Reperisco la provincia dal CAP e dalla località
076500041025     C                   IF        VABCAD <> *blanks AND
076600050907     C                             VABPRD  = *blanks AND
076700050907     C                             VABNZD  = *blanks
076800041025     C                   CLEAR                   TISI95DS
076900041025     C                   EVAL      I95TCN = '3'
077000041025     C                   Z-ADD     datcor        I95DAT
077100041025     C                   EVAL      I95CAP = VABCAD
077200041025     C                   EVAL      I95LOC = VABLOD
077300041025     C                   CALL      'TISI95R'
077400041025     C                   PARM                    TISI95DS
077500041025     C                   EVAL      VABPRD = O95PRV
077600041025     C                   ENDIF
077700041025     C                   ENDIF
077800040630     C*
077900040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
078000040630     C                   exsr      exeBon
078100090611     C*
078200090611     C* Forzatura parametri anagrafica
078300090611     C                   IF        %subst(vlrppt:4:7) <> *blanks
078400090611     C                   EVAL      PiStr=%trim(%subst(vlrppt:4:7))
078500090611     C                   EXSR      CHKNUM
078600090611     C                   IF        PiInt=*on
078700090611     C                   Z-ADD     PiVal         VABCCM
078800090611     C                   ENDIF
078900090611     C                   ENDIF
079000090611     C*
079100090611     C                   IF        %subst(vlrppt:11:3) <> *blanks
079200090611     C                   EVAL      PiStr=%trim(%subst(vlrppt:11:3))
079300090611     C                   EXSR      CHKNUM
079400090611     C                   IF        PiInt=*on
079500090611     C                   Z-ADD     PiVal         VABLNP
079600090611     C                   ENDIF
079700090611     C                   ENDIF
079800090611     C*
079900090611     C                   IF        %subst(vlrppt:14:3) <> *blanks
080000090611     C                   EVAL      PiStr=%trim(%subst(vlrppt:14:3))
080100090611     C                   EXSR      CHKNUM
080200090611     C                   IF        PiInt=*on
080300090611     C                   Z-ADD     PiVal         VABCTR
080400090611     C                   ENDIF
080500090611     C                   ENDIF
080600930409     C*
080700010604     C  N31              ADD       1             §CTROK            7 0
080800010604     C   32              ADD       1             §CTRMO            7 0
080900010604     C   31              ADD       1             §CTRNO            7 0
081000020913     C  N31              WRITE     FIVAB000
081100990910     C*
081200010601     C                   if        *in31 = *off and
081300010601     C                             *in32 = *off
081400990910     C                   eval      vinflg = '1'
081500990910     C                   else
081600000621     C                   eval      recko = vindta
081700010612     C                   if        %open(tis7t5ps)
081800030407     C   50              except    rigadett
081900010612     C                   endif
082000990910     C                   eval      vinflg = '2'
082100990910     C                   endif
082200991022     C                   endif
082300990910     C                   endif
082400010601     C*
082500010601     C                   endif
082600000905     C*
082700000905     C                   else
082800000905     C                   eval      vinflg = '1'
082900000905     C                   endif
083000000905     C*
083100000905     C  N70              update    tivin000
083200991022     C*
083300991022     C  N70              ENDdo
083400010530     C
083500010530     C*
083600010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
083700010530     C                   if        cntNonEl = *zeros or
083800010530     C                             flgMulti = '0'
083900990910     C* Se non ci sono record con errori ...
084000010530     C                   if        §ctrno = 0 and
084100010601     C                             §ctrmo = 0 and
084200010530     C                             flgStato <> '2'
084300990910     C* ... restituisco esito OK.
084400990921     C                   eval      wrkesito = '0'
084500990910     C                   else
084600010601     C                   if        §ctrok > 0
084700990921     C                   eval      wrkesito = '1'
084800000710     C                   else
084900010615     C                   if        flgOk = '0'
085000010615     C                   eval      wrkesito = '2'
085100010615     C                   else
085200010615     C                   eval      wrkesito = '6'
085300010615     C                   endif
085400990910     C                   endif
085500000710     C                   endif
085600010530     C                   else
085700010530     C                   eval      wrkesito = '9'
085800010530     C                   endif
085900990910     C*
086000990914     C                   if        %open(tivin00r)
086100990908     C                   close     tivin00r
086200990914     C                   endif
086300020913     C                   if        %open(fivabwwr)
086400020913     C                   close     fivabwwr
086500990914     C                   endif
086600010530     C*
086700010530     C                   if        vlrpoi <> 999
086800010530     C                   eval      invfil = vlrpoi
086900010530     C                   endif
087000990910     C*
087100990920     C                   if        §ctrok > 0
087200010530     C                             and invfil > *zeros
087300000613     C                   exsr      invio
087400990920     C                   endif
087500010612     C*
087600010612     C                   if        flgGiro = '1'
087700010612     C                   exsr      endela
087800010612     C                   endif
087900990920     C*
088000910830     C                   ENDSR
088100000613     C***
088200990920
088300010530
088400010530
088500010530
088600010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
088700010530     C     repfil        BEGSR
088800010530     C*
088900010530     C                   if        invfil = *zeros and
089000010530     C                             depfil > *zeros and
089100010613     C                             (vinflg = *blanks or
089200010613     C                              vinflg = *zeros)
089300010530     C
089400010530     C                   eval      invfil = depfil
089500010530     C                   endif
089600010530     C*
089700010530     C                   if        depfil <> invfil and
089800010530     C                             invfil > *zeros
089900010530     C                   eval      flgMulti = '1'
090000010530     C                   if        vinflg = *blanks
090100010530     C                   add       1             cntNonEl
090200010530     C                   endif
090300010530     C                   endif
090400010530     C*
090500010530     C                   if        vinflg = '2'
090600010530     C                   eval      flgStato = '2'
090700010530     C                   endif
090800010530     C*
090900010530     C                   ENDSR
091000010530     C***
091100010530
091200010530
091300010530
091400010530
091500990920      /TITLE Invio dei dati al punto operativo.
091600000613     C     invio         BEGSR
091700990920     C*
091800990920     C                   reset                   dscmz
091900010530     C                   move      invfil        cmzdst
092000990920     C                   eval      cmzfld = vlrfou
092100990920     C                   eval      cmzmbd = vlrhdl
092200990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
092300000710     C                   if        prmfir = *blanks
092400020913     C                   eval      cmzfla = 'FIVAB00F'
092500020913     C                   eval      cmzmba = 'FIVAB00F'
092600000710     C                   else
092700000710     C                   eval      cmzfla = prmfir
092800000710     C                   eval      cmzmba = prmfir
092900000710     C                   endif
093000990920     C                   eval      cmznrr = *zeros
093100990920     C                   move      §ctrok        cmznrr
093200021003     C                   eval      cmzlba = vlrfl1
093300990920     C                   call(e)   'TIS711C'
093400990920     C                   parm                    dscmz
093500990921     C                   parm      *blanks       esito
093600990920     C                   if        %error
093700990920     C                             or cmzerr = '1'
093800990921     C                             or esito  = '1'
093900000710     C                   eval      wrkesito = '3'
094000990920     C                   endif
094100990920     C*
094200000613     C                   ENDSR
094300000613     C***
094400010530
094500010530
094600010530
094700010530
094800010530      /TITLE Invio dei dati al punto operativo.
094900010530     C     opeini        BEGSR
095000010530     C*
095100010530     C* Inizializzo flag e contatori operativi
095200010530     C                   movel     '0'           flgGiro           1
095300010530     C                   movel     '0'           flgMulti          1
095400010530     C                   movel     '1'           flgStato          1
095500010615     C                   movel     '0'           flgOk             1
095600010530     C                   z-add     *zeros        cntNonEl         10 0
095700010530     C                   z-add     *zeros        depfil            3 0
095800010530     C                   z-add     *zeros        invfil            3 0
095900030407     C*
096000030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
096100030407     C                   if        %subst(vlrppt:1:1) = 'S'
096200030407     C                   seton                                        50
096300030407     C                   else
096400030407     C                   setoff                                       50
096500030407     C                   endif
096600041001     C*
096700041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
096800041001     C                   if        %subst(vlrppt:2:1) = 'S'
096900041001     C                   seton                                        51
097000041001     C                   else
097100041001     C                   setoff                                       51
097200041001     C                   endif
097300041025     C*
097400041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
097500041025     C* (naturalmente solo se nn già presente)
097600041025     C                   if        %subst(vlrppt:3:1) = 'S'
097700041025     C                   seton                                        52
097800041025     C                   else
097900041025     C                   setoff                                       52
098000041025     C                   endif
098100010530     C*
098200010530     C                   ENDSR
098300010530     C***
098400040407
098500040407
098600040407
098700040407
098800040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
098900040407     C     exeBon        BEGSR
099000040407     C*
099100040407     C* Imposto i parametri x il *pgm d "bonifica"
099200040407     C                   EVAL      ElencoChar = *blanks
099300040407     C                   EVAL      TipoElenco = *blanks
099400040407     C                   EVAL      CharSost   = *blanks
099500040629     C                   EVAL      UpperCase  = '1'
099600040407     C                   EVAL      ChkNull    = '1'
099700040407     C                   EVAL      CharNull   = *blanks
099800040629     C*
099900040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
100000040629     C                   EVAL      TxtInOut   = VABRSD
100100040629     C                   EVAL      EsitoBon   = *blanks
100200040407     C*
100300040407     C                   CALL      'XCHKCHAR'
100400040407     C                   PARM                    TxtInOut
100500040407     C                   PARM                    ElencoChar
100600040407     C                   PARM                    TipoElenco
100700040407     C                   PARM                    CharSost
100800040407     C                   PARM                    UpperCase
100900040407     C                   PARM                    ChkNull
101000040407     C                   PARM                    CharNull
101100040407     C                   PARM                    EsitoBon
101200040407     C*
101300040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
101400040407     C                   IF        EsitoBon = '1'
101500040629     C                   EVAL      VABRSD = TxtInOut
101600040407     C                   ENDIF
101700040629     C*
101800040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
101900040629     C                   EVAL      TxtInOut   = VABIND
102000040629     C                   EVAL      EsitoBon   = *blanks
102100040629     C*
102200040629     C                   CALL      'XCHKCHAR'
102300040629     C                   PARM                    TxtInOut
102400040629     C                   PARM                    ElencoChar
102500040629     C                   PARM                    TipoElenco
102600040629     C                   PARM                    CharSost
102700040629     C                   PARM                    UpperCase
102800040629     C                   PARM                    ChkNull
102900040629     C                   PARM                    CharNull
103000040629     C                   PARM                    EsitoBon
103100040629     C*
103200040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
103300040629     C                   IF        EsitoBon = '1'
103400040629     C                   EVAL      VABIND = TxtInOut
103500040629     C                   ENDIF
103600040629     C*
103700040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
103800040629     C                   EVAL      TxtInOut   = VABLOD
103900040629     C                   EVAL      EsitoBon   = *blanks
104000040629     C*
104100040629     C                   CALL      'XCHKCHAR'
104200040629     C                   PARM                    TxtInOut
104300040629     C                   PARM                    ElencoChar
104400040629     C                   PARM                    TipoElenco
104500040629     C                   PARM                    CharSost
104600040629     C                   PARM                    UpperCase
104700040629     C                   PARM                    ChkNull
104800040629     C                   PARM                    CharNull
104900040629     C                   PARM                    EsitoBon
105000040629     C*
105100040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
105200040629     C                   IF        EsitoBon = '1'
105300040629     C                   EVAL      VABLOD = TxtInOut
105400040629     C                   ENDIF
105500040629     C*
105600040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
105700040629     C                   EVAL      TxtInOut   = VABNOT
105800040629     C                   EVAL      EsitoBon   = *blanks
105900040630     C                   EVAL      UpperCase  = *blanks
106000040629     C*
106100040629     C                   CALL      'XCHKCHAR'
106200040629     C                   PARM                    TxtInOut
106300040629     C                   PARM                    ElencoChar
106400040629     C                   PARM                    TipoElenco
106500040629     C                   PARM                    CharSost
106600040629     C                   PARM                    UpperCase
106700040629     C                   PARM                    ChkNull
106800040629     C                   PARM                    CharNull
106900040629     C                   PARM                    EsitoBon
107000040629     C*
107100040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
107200040629     C                   IF        EsitoBon = '1'
107300040629     C                   EVAL      VABNOT = TxtInOut
107400040629     C                   ENDIF
107500040629     C*
107600040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
107700040629     C                   EVAL      TxtInOut   = VABNT2
107800040629     C                   EVAL      EsitoBon   = *blanks
107900040630     C                   EVAL      UpperCase  = *blanks
108000040629     C*
108100040629     C                   CALL      'XCHKCHAR'
108200040629     C                   PARM                    TxtInOut
108300040629     C                   PARM                    ElencoChar
108400040629     C                   PARM                    TipoElenco
108500040629     C                   PARM                    CharSost
108600040629     C                   PARM                    UpperCase
108700040629     C                   PARM                    ChkNull
108800040629     C                   PARM                    CharNull
108900040629     C                   PARM                    EsitoBon
109000040629     C*
109100040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
109200040629     C                   IF        EsitoBon = '1'
109300040629     C                   EVAL      VABNT2 = TxtInOut
109400040629     C                   ENDIF
109500040407     C*
109600040407     C                   ENDSR
109700010530
109800090611
109900090611
110000090611     C*----------------------------------------------------*
110100090611     C*  CONTROLLO NUMERICITA' CAMPI
110200090611     C*----------------------------------------------------*
110300090611     C     CHKNUM        BEGSR
110400090611     C*
110500090611     C                   IF        PiDecChr = *blanks
110600090611     C                   EVAL      PiDecChr = ','
110700090611     C                   ENDIF
110800090611     C*
110900090611     C                   callp(e)  UBISNUM_Check(PiStr
111000090611     C                                          :PiDecChr
111100090611     C                                          :PiVal
111200090611     C                                          :PiNum
111300090611     C                                          :PiInt)
111400090611     C*
111500090611     C                   IF        %error
111600090611     C                   EVAL      PiInt=*off
111700090611     C                   ENDIF
111800090611     C*
111900090611     C                   ENDSR
112000090611     C***
112100090611
112200090611
112300060410
112400990910
112500000613     C     *inzsr        BEGSR
112600990910     C*
112700990910     C     *entry        plist
112800990920     C                   parm                    tivlrds
112900990921     C                   parm      wrkesito      esito
113000000724     C                   parm                    prmlit
113100000710     C                   parm                    prmfir
113200010606     C*
113300010606     C* CALCOLA LA DATA CORRENTE
113400010606     C                   time                    wn14             14 0
113500010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
113600010606     C                   z-add     wn8           g08dat
113700010606     C                   z-add     *zeros        g08inv
113800010606     C                   movel     '0'           g08err
113900010606     C                   call      'XSRDA8'
114000010606     C                   parm                    wlbda8
114100010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
114200000613     C*
114300000613     C                   ENDSR
114400000613     C***
114500990908
114600010601     Otis7t5p   E            riepilogo         2
114700990915     O                                              'Upload via Internet'
114800990915     O                                           +1 'Traduzione TIVIN00R -'
114900020913     O                                           +1 'FIVABWWR'
115000990915     O                                           +1 'Testate bolle'
115100990915     O          E            riepilogo   2
115200990915     O                       wrkdata
115300990915     O                       wrkora              +1
115400990915     O                       procname            +1
115500990915     O          E            riepilogo   2
115600990915     O                                              'Cliente..................:'
115700990915     O                       VABCCM        z     +1
115800990915     O          E            riepilogo   2
115900990920     O                                              'Riferimento Strategi.....:'
116000990920     O                       vlrhdl              +1
116100990915     O          E            riepilogo   2
116200990915     O                                              'Giusti...................:'
116300971022     O                       §CTROK        2   +  1
116400990915     O          E            riepilogo   2
116500990915     O                                              'Sbagliati e corretti.....:'
116600971022     O                       §CTRMO        2   +  1
116700990915     O          E            riepilogo   2
116800990915     O                                              'Sbagliati e scartati.....:'
116900971022     O                       §CTRNO        2   +  1
117000000613
117100010601     Otis7t5ps  E            testdett          2
117200000613     O                                              'Upload via Internet'
117300000613     O                                           +1 'Traduzione TIVIN00R -'
117400020913     O                                           +1 'FIVABWWR'
117500000613     O                                           +1 'Testate bolle'
117600000616     O          E            testdett    3
117700000613     O                                           +2 'N° rec'
117800000613     O                                           +3 'Anteprima contenuto'
117900000616     O          E            rigadett    2
118000000613     O                       rrnum               +2
118100000621     O                       recko               +3
118200000616     O          E            findett     2
118300000613     O                       wrkdata
118400000613     O                       wrkora              +1
118500000613     O                       procname            +1
118600000616     O          E            findett     2
118700000613     O                                              'Cliente..................:'
118800000613     O                       VABCCM        z     +1
118900000616     O          E            findett     2
119000000613     O                                              'Riferimento Strategi.....:'
119100000613     O                       vlrhdl              +1
119200000616     O          E            findett     2
119300000613     O                                              'Giusti...................:'
119400000613     O                       §CTROK        2   +  1
119500000616     O          E            findett     2
119600000613     O                                              'Sbagliati e corretti.....:'
119700000613     O                       §CTRMO        2   +  1
119800000616     O          E            findett     2
119900000613     O                                              'Sbagliati e scartati.....:'
120000000613     O                       §CTRNO        2   +  1
120100000512** CMD - COMANDI CL
120200011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T5P)  FORMTYPE(RICCLI) OUTQ(
120300011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T5PS) FORMTYPE(RICCLI) OUTQ(
120400010601DLTOVR FILE(TIS7T5P TIS7T5PS) LVL(*)
120500000512
120600000512
