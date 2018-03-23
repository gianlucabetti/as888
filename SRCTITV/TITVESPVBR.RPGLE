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
026300070115     C*
026400070115     C                   TESTN                   VHBCCM               30
026500070115     C  N30              TESTN                   VHBCCM                 30
026600070115     C  N30              TESTN                   VHBCCM                   30
026700070115     C                   MOVE      VHBCCM        W001A             1
026800070115     C     *IN30         IFEQ      *OFF
026900070115     C     W001A         ORLT      '0'
027000070115     C                   SETON                                        31
027100070115     C                   eval      vinmsg = %trimr(vinmsg)
027200070115     C                             + ' ' + 'VHBCCM'
027300070115     C                   ELSE
027400070115     C                   MOVE      VHBCCM        VABCCM
027500070115     C     VABCCM        IFLE      *ZEROS
027600070115     C                   SETON                                        31
027700070115     C                   eval      vinmsg = %trimr(vinmsg)
027800070115     C                             + ' ' + 'VHBCCM'
027900070115     C                   ENDIF
028000070115     C                   ENDIF
028100070115     C*
028200070115     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028300070115     C                   eval      depfil = VABLNP
028400070115     C*
028500070115     C* Effettuo forzatura ESPRINET x invio a terminal d competenza rispetto al CCM indicato dal
028600070115     C* cliente
028700070507     C                   if        VABCCM = 0893943 OR
028800070507     C                             VABCCM = 0894169
028900070115     C                   eval      depfil = 050
029000070115     C                   eval      invfil = 050
029100070115     C                   eval      vabfgs = 050
029200070115     C                   endif
029300070507     C                   if        VABCCM = 0893942 OR
029400070507     C                             VABCCM = 0894168
029500070115     C                   eval      depfil = 005
029600070115     C                   eval      invfil = 005
029700070115     C                   eval      vabfgs = 005
029800070115     C                   endif
029900010530     C*
030000010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
030100010530     C                   exsr      repfil
030200010601     C                   if        depfil = invfil
030300021014     C                   if        vlrpoi = 999
030400021010     C                   MOVE(P)   invfil        VABFGS
030500021014     C                   else
030600021014     C                   MOVE(P)   vlrpoi        VABFGS
030700021014     C                   endif
030800010530     C*
030900010530     C                   exsr      PREELA
031000971021     C*
031100971023     C                   TESTN                   VHBAAS               30
031200971023     C  N30              TESTN                   VHBAAS                 30
031300971023     C  N30              TESTN                   VHBAAS                   30
031400971021     C                   MOVE      VHBAAS        W001A             1
031500971021     C     *IN30         IFEQ      *OFF
031600971021     C     W001A         ORLT      '0'
031700971023     C     W001A         ANDNE     *BLANK
031800971021     C                   SETON                                        32
031900010606     C                   MOVEL     datcor        VABAAS
032000010604     C                   eval      vinmsg = %trimr(vinmsg)
032100010604     C                             + ' ' + 'VHBAAS'
032200971021     C                   ELSE
032300971021     C                   MOVE      VHBAAS        VABAAS
032400971021     C                   ENDIF
032500971021     C*
032600971023     C                   TESTN                   VHBMGS               30
032700971023     C  N30              TESTN                   VHBMGS                 30
032800971023     C  N30              TESTN                   VHBMGS                   30
032900971021     C                   MOVE      VHBMGS        W001A             1
033000971021     C     *IN30         IFEQ      *OFF
033100971021     C     W001A         ORLT      '0'
033200971023     C     W001A         ANDNE     *BLANK
033300971021     C                   SETON                                        32
033400010601     C                   MOVE      '0101'        VABMGS
033500010604     C                   eval      vinmsg = %trimr(vinmsg)
033600010604     C                             + ' ' + 'VHBMGS'
033700971021     C                   ELSE
033800971021     C                   MOVE      VHBMGS        VABMGS
033900971021     C                   ENDIF
034000971021     C*
034100971023     C                   TESTN                   VHBNRS               30
034200971023     C  N30              TESTN                   VHBNRS                 30
034300971023     C  N30              TESTN                   VHBNRS                   30
034400971021     C                   MOVE      VHBNRS        W001A             1
034500971021     C     *IN30         IFEQ      *OFF
034600971021     C     W001A         ORLT      '0'
034700971023     C     W001A         ANDNE     *BLANK
034800971021     C                   SETON                                        32
034900971021     C                   MOVE      *ZERO         VABNRS
035000010604     C                   eval      vinmsg = %trimr(vinmsg)
035100010604     C                             + ' ' + 'VHBNRS'
035200971021     C                   ELSE
035300971021     C                   MOVE      VHBNRS        VABNRS
035400971021     C                   ENDIF
035500971021     C*
035600971023     C                   TESTN                   VHBNSP               30
035700971023     C  N30              TESTN                   VHBNSP                 30
035800971023     C  N30              TESTN                   VHBNSP                   30
035900971021     C                   MOVE      VHBNSP        W001A             1
036000971021     C     *IN30         IFEQ      *OFF
036100971021     C     W001A         ORLT      '0'
036200971023     C     W001A         ANDNE     *BLANK
036300971021     C                   SETON                                        32
036400971021     C                   MOVE      *ZERO         VABNSP
036500010604     C                   eval      vinmsg = %trimr(vinmsg)
036600010604     C                             + ' ' + 'VHBNSP'
036700971021     C                   ELSE
036800971021     C                   MOVE      VHBNSP        VABNSP
036900971021     C                   ENDIF
037000971021     C*
037100971023     C                   TESTN                   VHBLNA               30
037200971023     C  N30              TESTN                   VHBLNA                 30
037300971023     C  N30              TESTN                   VHBLNA                   30
037400971021     C                   MOVE      VHBLNA        W001A             1
037500971021     C     *IN30         IFEQ      *OFF
037600971021     C     W001A         ORLT      '0'
037700971023     C     W001A         ANDNE     *BLANK
037800971021     C                   SETON                                        32
037900971021     C                   MOVE      *ZERO         VABLNA
038000010604     C                   eval      vinmsg = %trimr(vinmsg)
038100010604     C                             + ' ' + 'VHBLNA'
038200971021     C                   ELSE
038300971021     C                   MOVE      VHBLNA        VABLNA
038400971021     C                   ENDIF
038500971021     C*
038600971023     C                   TESTN                   VHBCTR               30
038700971023     C  N30              TESTN                   VHBCTR                 30
038800971023     C  N30              TESTN                   VHBCTR                   30
038900971021     C                   MOVE      VHBCTR        W001A             1
039000971021     C     *IN30         IFEQ      *OFF
039100971021     C     W001A         ORLT      '0'
039200971023     C     W001A         ANDNE     *BLANK
039300971021     C                   SETON                                        32
039400971021     C                   MOVE      *ZERO         VABCTR
039500010604     C                   eval      vinmsg = %trimr(vinmsg)
039600010604     C                             + ' ' + 'VHBCTR'
039700971021     C                   ELSE
039800971021     C                   MOVE      VHBCTR        VABCTR
039900971021     C                   ENDIF
040000941229     C*
040100971021     C                   MOVE      *ZERO         VABIAS
040200941229     C                   MOVE      VHBIAS        WASC15
040300971023     C                   TESTN                   WI15                 30
040400971023     C  N30              TESTN                   WI15                   30
040500971023     C  N30              TESTN                   WI15                     30
040600971021     C                   MOVE      WI15          W001A             1
040700971021     C     *IN30         IFEQ      *OFF
040800971021     C     W001A         ORLT      '0'
040900971023     C     W001A         ANDNE     *BLANK
041000971021     C                   SETON                                        32
041100971021     C                   MOVE      *ZERO         VABIAS
041200010604     C                   eval      vinmsg = %trimr(vinmsg)
041300010604     C                             + ' ' + 'VHBIAS'
041400971021     C                   ELSE
041500971021     C                   MOVEL     WI15          VABIAS
041600971023     C                   TESTN                   WD15                 30
041700971023     C  N30              TESTN                   WD15                   30
041800971023     C  N30              TESTN                   WD15                     30
041900971021     C                   MOVE      WD15          W001A             1
042000971021     C     *IN30         IFEQ      *OFF
042100971021     C     W001A         ORLT      '0'
042200971023     C     W001A         ANDNE     *BLANK
042300971021     C                   SETON                                        32
042400971021     C                   MOVE      *ZERO         VABIAS
042500010604     C                   eval      vinmsg = %trimr(vinmsg)
042600010604     C                             + ' ' + 'VHBIAS'
042700971021     C                   ELSE
042800971021     C                   MOVE      WD15          VABIAS
042900971021     C                   ENDIF
043000050118     C                   ENDIF
043100941229     C*
043200971023     C                   TESTN                   VHBNCL               30
043300971023     C  N30              TESTN                   VHBNCL                 30
043400971023     C  N30              TESTN                   VHBNCL                   30
043500971021     C                   MOVE      VHBNCL        W001A             1
043600971021     C     *IN30         IFEQ      *OFF
043700971021     C     W001A         ORLT      '0'
043800971021     C                   SETON                                        32
043900971021     C                   MOVE      *ZERO         VABNCL
044000010604     C                   eval      vinmsg = %trimr(vinmsg)
044100010604     C                             + ' ' + 'VHBNCL'
044200971021     C                   ELSE
044300971021     C                   MOVE      VHBNCL        VABNCL
044400971021     C                   ENDIF
044500941229     C*
044600971021     C                   MOVE      *ZERO         VABPKB
044700971021     C                   MOVE      VHBPKB        WASC09
044800010719     C                   IF        WI09 = *blanks
044900010719     C                   EVAL      WI09 = *zeros
045000010719     C                   ENDIF
045100971023     C                   TESTN                   WI09                 30
045200971023     C  N30              TESTN                   WI09                   30
045300971023     C  N30              TESTN                   WI09                     30
045400971021     C                   MOVE      WI09          W001A             1
045500971021     C     *IN30         IFEQ      *OFF
045600971021     C     W001A         ORLT      '0'
045700971021     C                   SETON                                        32
045800971021     C                   MOVE      *ZERO         VABPKB
045900010604     C                   eval      vinmsg = %trimr(vinmsg)
046000010604     C                             + ' ' + 'VHBPKB'
046100971021     C                   ELSE
046200971021     C                   MOVEL     WI09          VABPKB
046300010719     C                   IF        WD09 = *blanks
046400010719     C                   EVAL      WD09 = *zeros
046500010719     C                   ENDIF
046600971023     C                   TESTN                   WD09                 30
046700971023     C  N30              TESTN                   WD09                     30
046800971021     C                   MOVE      WD09          W001A             1
046900971021     C     *IN30         IFEQ      *OFF
047000971021     C     W001A         ORLT      '0'
047100971023     C     W001A         ANDNE     *BLANK
047200971021     C                   SETON                                        32
047300971021     C                   MOVE      *ZERO         VABPKB
047400010604     C                   eval      vinmsg = %trimr(vinmsg)
047500010604     C                             + ' ' + 'VHBPKB'
047600971021     C                   ELSE
047700971021     C                   MOVE      WD09          VABPKB
047800971021     C                   ENDIF
047900050118     C                   ENDIF
048000971021     C*
048100041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
048200041001     C                   IF        *IN51 = *OFF
048300971021     C                   MOVE      *ZERO         VABVLB
048400971021     C                   MOVE      VHBVLB        WASC07
048500010719     C                   IF        WI07 = *blanks
048600010719     C                   EVAL      WI07 = *zeros
048700010719     C                   ENDIF
048800971023     C                   TESTN                   WI07                 30
048900971023     C  N30              TESTN                   WI07                   30
049000971023     C  N30              TESTN                   WI07                     30
049100971021     C                   MOVE      WI07          W001A             1
049200971021     C     *IN30         IFEQ      *OFF
049300971021     C     W001A         ORLT      '0'
049400971023     C     W001A         ANDNE     *BLANK
049500971021     C                   SETON                                        32
049600971021     C                   MOVE      *ZERO         VABVLB
049700010604     C                   eval      vinmsg = %trimr(vinmsg)
049800010604     C                             + ' ' + 'VHBVLB'
049900971021     C                   ELSE
050000971021     C                   MOVEL     WI07          VABVLB
050100010719     C                   IF        WD07 = *blanks
050200010719     C                   EVAL      WD07 = *zeros
050300010719     C                   ENDIF
050400971023     C                   TESTN                   WD07                 30
050500971023     C  N30              TESTN                   WD07                   30
050600971023     C  N30              TESTN                   WD07                     30
050700971021     C                   MOVE      WD07          W001A             1
050800971021     C     *IN30         IFEQ      *OFF
050900971021     C     W001A         ORLT      '0'
051000971023     C     W001A         ANDNE     *BLANK
051100971021     C                   SETON                                        32
051200971021     C                   MOVE      *ZERO         VABVLB
051300010604     C                   eval      vinmsg = %trimr(vinmsg)
051400010604     C                             + ' ' + 'VHBVLB'
051500971021     C                   ELSE
051600971021     C                   MOVE      WD07          VABVLB
051700971021     C                   ENDIF
051800050118     C                   ENDIF
051900041001     C                   ELSE
052000041001     C* ...altrimenti forzo azzeramento del volume
052100041001     C                   Z-ADD     *zeros        VABVLB
052200041001     C                   ENDIF
052300941229     C*
052400971021     C                   MOVE      *ZERO         VABQFT
052500971021     C                   MOVE      VHBQFT        WASC15
052600010719     C                   IF        WI15 = *blanks
052700010719     C                   EVAL      WI15 = *zeros
052800010719     C                   ENDIF
052900971023     C                   TESTN                   WI15                 30
053000971023     C  N30              TESTN                   WI15                   30
053100971023     C  N30              TESTN                   WI15                     30
053200971021     C                   MOVE      WI15          W001A             1
053300971021     C     *IN30         IFEQ      *OFF
053400971021     C     W001A         ORLT      '0'
053500971023     C     W001A         ANDNE     *BLANK
053600971021     C                   SETON                                        32
053700971021     C                   MOVE      *ZERO         VABQFT
053800010604     C                   eval      vinmsg = %trimr(vinmsg)
053900010604     C                             + ' ' + 'VHBQFT'
054000971021     C                   ELSE
054100971021     C                   MOVEL     WI15          VABQFT
054200010719     C                   IF        WD15 = *blanks
054300010719     C                   EVAL      WD15 = *zeros
054400010719     C                   ENDIF
054500971023     C                   TESTN                   WD15                 30
054600971023     C  N30              TESTN                   WD15                   30
054700971023     C  N30              TESTN                   WD15                     30
054800971021     C                   MOVE      WD15          W001A             1
054900971021     C     *IN30         IFEQ      *OFF
055000971021     C     W001A         ORLT      '0'
055100971023     C     W001A         ANDNE     *BLANK
055200971021     C                   SETON                                        32
055300971021     C                   MOVE      *ZERO         VABQFT
055400010604     C                   eval      vinmsg = %trimr(vinmsg)
055500010604     C                             + ' ' + 'VHBQFT'
055600971021     C                   ELSE
055700971021     C                   MOVE      WD15          VABQFT
055800971021     C                   ENDIF
055900050118     C                   ENDIF
056000941229     C*
056100971021     C                   MOVE      *ZERO         VABCAS
056200060410     C* Se presente separatore decimale alla giusta posizione => forzo sempre la virgola
056300060410     C                   EVAL      wVHBCAS = VHBCAS
056400060410     C                   IF        %subst(wVHBCAS:12:1) = '.'
056500060410     C                   EVAL      %subst(wVHBCAS:12:1) = ','
056600060410     C                   ENDIF
056700060410     C*
056800060410     C                   MOVEL(P)  wVHBCAS       PiStr
056900060410     C                   EXSR      CHKNUM
057000060410     C                   IF        PiNum=*on
057100060410     C                   Z-ADD     PiVal         VABCAS
057200060410     C                   ELSE
057300060410     C                   SETON                                        32
057400060410     C                   Z-ADD     *zeros        VABCAS
057500060410     C                   EVAL      vinmsg = %trimr(vinmsg)
057600060410     C                             + ' ' + 'VHBCAS'
057700060410     C                   ENDIF
057800971021     C*
057900971023     C                   TESTN                   VHBRMN               30
058000971023     C  N30              TESTN                   VHBRMN                 30
058100971023     C  N30              TESTN                   VHBRMN                   30
058200971021     C                   MOVE      VHBRMN        W001A             1
058300971021     C     *IN30         IFEQ      *OFF
058400971021     C     W001A         ORLT      '0'
058500971021     C                   SETON                                        32
058600971021     C                   MOVE      *ZERO         VABRMN
058700010604     C                   eval      vinmsg = %trimr(vinmsg)
058800010604     C                             + ' ' + 'VHBRMN'
058900971021     C                   ELSE
059000971021     C                   MOVE      VHBRMN        VABRMN
059100971021     C                   ENDIF
059200971021     C*
059300971023     C                   TESTN                   VHBNCD               30
059400971023     C  N30              TESTN                   VHBNCD                 30
059500971023     C  N30              TESTN                   VHBNCD                   30
059600971021     C                   MOVE      VHBNCD        W001A             1
059700971021     C     *IN30         IFEQ      *OFF
059800971021     C     W001A         ORLT      '0'
059900971023     C     W001A         ANDNE     *BLANK
060000971021     C                   SETON                                        32
060100971021     C                   MOVE      *ZERO         VABNCD
060200010604     C                   eval      vinmsg = %trimr(vinmsg)
060300010604     C                             + ' ' + 'VHBNCD'
060400971021     C                   ELSE
060500971021     C                   MOVE      VHBNCD        VABNCD
060600971021     C                   ENDIF
060700971021     C*
060800971023     C                   TESTN                   VHBNCA               30
060900971023     C  N30              TESTN                   VHBNCA                 30
061000971023     C  N30              TESTN                   VHBNCA                   30
061100971021     C                   MOVE      VHBNCA        W001A             1
061200971021     C     *IN30         IFEQ      *OFF
061300971021     C     W001A         ORLT      '0'
061400971023     C     W001A         ANDNE     *BLANK
061500971021     C                   SETON                                        32
061600971021     C                   MOVE      *ZERO         VABNCA
061700010604     C                   eval      vinmsg = %trimr(vinmsg)
061800010604     C                             + ' ' + 'VHBNCA'
061900971021     C                   ELSE
062000971021     C                   MOVE      VHBNCA        VABNCA
062100971021     C                   ENDIF
062200971021     C*
062300971023     C                   TESTN                   VHBZNC               30
062400971023     C  N30              TESTN                   VHBZNC                 30
062500971023     C  N30              TESTN                   VHBZNC                   30
062600971021     C                   MOVE      VHBZNC        W001A             1
062700971021     C     *IN30         IFEQ      *OFF
062800971021     C     W001A         ORLT      '0'
062900971023     C     W001A         ANDNE     *BLANK
063000971021     C                   SETON                                        32
063100971021     C                   MOVE      *ZERO         VABZNC
063200010604     C                   eval      vinmsg = %trimr(vinmsg)
063300010604     C                             + ' ' + 'VHBZNC'
063400971021     C                   ELSE
063500971021     C                   MOVE      VHBZNC        VABZNC
063600971021     C                   ENDIF
063700971021     C*
063800971023     C                   TESTN                   VHBDCR               30
063900971023     C  N30              TESTN                   VHBDCR                 30
064000971023     C  N30              TESTN                   VHBDCR                   30
064100971021     C                   MOVE      VHBDCR        W001A             1
064200971021     C     *IN30         IFEQ      *OFF
064300971021     C     W001A         ORLT      '0'
064400971023     C     W001A         ANDNE     *BLANK
064500971021     C                   SETON                                        32
064600971021     C                   MOVE      *ZERO         VABDCR
064700010604     C                   eval      vinmsg = %trimr(vinmsg)
064800010604     C                             + ' ' + 'VHBDCR'
064900971021     C                   ELSE
065000971021     C                   MOVE      VHBDCR        VABDCR
065100971021     C                   ENDIF
065200971021     C*
065300971023     C                   TESTN                   VHBHCR               30
065400971023     C  N30              TESTN                   VHBHCR                 30
065500971023     C  N30              TESTN                   VHBHCR                   30
065600971021     C                   MOVE      VHBHCR        W001A             1
065700971021     C     *IN30         IFEQ      *OFF
065800971021     C     W001A         ORLT      '0'
065900971023     C     W001A         ANDNE     *BLANK
066000971021     C                   SETON                                        32
066100971021     C                   MOVE      *ZERO         VABHCR
066200010604     C                   eval      vinmsg = %trimr(vinmsg)
066300010604     C                             + ' ' + 'VHBHCR'
066400971021     C                   ELSE
066500971021     C                   MOVE      VHBHCR        VABHCR
066600971021     C                   ENDIF
066700941229     C*
066800971021     C                   MOVE      *ZERO         VABVMD
066900971021     C                   MOVE      VHBVMD        WASC15
067000010719     C                   IF        WI15 = *blanks
067100010719     C                   EVAL      WI15 = *zeros
067200010719     C                   ENDIF
067300971023     C                   TESTN                   WI15                 30
067400971023     C  N30              TESTN                   WI15                   30
067500971023     C  N30              TESTN                   WI15                     30
067600971021     C                   MOVE      WI15          W001A             1
067700971021     C     *IN30         IFEQ      *OFF
067800971021     C     W001A         ORLT      '0'
067900971023     C     W001A         ANDNE     *BLANK
068000971021     C                   SETON                                        32
068100971021     C                   MOVE      *ZERO         VABVMD
068200010604     C                   eval      vinmsg = %trimr(vinmsg)
068300010604     C                             + ' ' + 'VHBVMD'
068400971021     C                   ELSE
068500971021     C                   MOVEL     WI15          VABVMD
068600010719     C                   IF        WD15 = *blanks
068700010719     C                   EVAL      WD15 = *zeros
068800010719     C                   ENDIF
068900971023     C                   TESTN                   WD15                 30
069000971023     C  N30              TESTN                   WD15                   30
069100971023     C  N30              TESTN                   WD15                     30
069200971021     C                   MOVE      WD15          W001A             1
069300971021     C     *IN30         IFEQ      *OFF
069400971021     C     W001A         ORLT      '0'
069500971023     C     W001A         ANDNE     *BLANK
069600971021     C                   SETON                                        32
069700971021     C                   MOVE      *ZERO         VABVMD
069800010604     C                   eval      vinmsg = %trimr(vinmsg)
069900010604     C                             + ' ' + 'VHBVMD'
070000971021     C                   ELSE
070100971021     C                   MOVE      WD15          VABVMD
070200971021     C                   ENDIF
070300050118     C                   ENDIF
070400971021     C*
070500971023     C                   TESTN                   VHBANT               30
070600971023     C  N30              TESTN                   VHBANT                 30
070700971023     C  N30              TESTN                   VHBANT                   30
070800971021     C                   MOVE      VHBANT        W001A             1
070900971021     C     *IN30         IFEQ      *OFF
071000971021     C     W001A         ORLT      '0'
071100971023     C     W001A         ANDNE     *BLANK
071200971021     C                   SETON                                        32
071300971021     C                   MOVE      *ZERO         VABANT
071400010604     C                   eval      vinmsg = %trimr(vinmsg)
071500010604     C                             + ' ' + 'VHBANT'
071600971021     C                   ELSE
071700971021     C                   MOVE      VHBANT        VABANT
071800971021     C                   ENDIF
071900971021     C*
072000971021     C* CAMPI ALFABETICI:
072100971021     C                   MOVE      VHBATB        VABATB
072200971021     C                   MOVE      VHBCBO        VABCBO
072300971021     C                   MOVE      VHBRSD        VABRSD
072400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
072500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
072600020117     C* ==
072700971021     C                   MOVE      VHBRD2        VABRD2
072800971021     C                   MOVE      VHBIND        VABIND
072900971021     C                   MOVE      VHBCAD        VABCAD
073000971021     C                   MOVE      VHBLOD        VABLOD
073100971021     C                   MOVE      VHBPRD        VABPRD
073200971021     C                   MOVE      VHBNZD        VABNZD
073300971021     C                   MOVE      VHBGC1        VABGC1
073400971021     C                   MOVE      VHBGC2        VABGC2
073500971021     C                   MOVE      VHBTSP        VABTSP
073600971021     C                   MOVE      VHBVAS        VABVAS
073700971021     C                   MOVE      VHBNAS        VABNAS
073800971021     C                   MOVE      VHBTIC        VABTIC
073900971021     C                   MOVE      VHBVCA        VABVCA
074000971021     C                   MOVE      VHBGCA        VABGCA
074100971021     C                   MOVE      VHBRMA        VABRMA
074200971021     C                   MOVE      VHBXCO        VABXCO
074300971021     C                   MOVE      VHBNOT        VABNOT
074400971021     C                   MOVE      VHBNT2        VABNT2
074500971021     C                   MOVE      VHBCTM        VABCTM
074600971021     C                   MOVE      VHBFFD        VABFFD
074700971021     C                   MOVE      VHBTCR        VABTCR
074800971021     C                   MOVE      VHBCTS        VABCTS
074900971021     C                   MOVE      VHBFTM        VABFTM
075000971021     C                   MOVE      VHBVAD        VABVAD
075100971021     C                   MOVE      VHBGMA        VABGMA
075200971021     C                   MOVE      VHBGGA        VABGGA
075300971021     C                   MOVE      VHBGVA        VABGVA
075400971021     C                   MOVE      VHBTC1        VABTC1
075500971021     C                   MOVE      VHBTC2        VABTC2
075600971021     C                   MOVE      VHBSCL        VABSCL
075700971021     C                   MOVE      VHBRMO        VABRMO
075800971021     C                   MOVE      VHBCMO        VABCMO
075900971021     C                   MOVE      VHBNMO        VABNMO
076000971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
076100060329     C* NELLA NAZIONE MITTENTE ORIGINALE
076200060331     C                   IF        %trim(VHBNMO) < 'A'
076300060329     C                   MOVE      *BLANKS       VABNMO
076400060329     C                   ENDIF
076500050418     C*
076600050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
076700050418     C                   IF        %trim(VABNZD) = 'I'   OR
076800050418     C                             %trim(VABNZD) = 'IT'  OR
076900050418     C                             %trim(VABNZD) = 'ITA'
077000050418     C                   EVAL      VABNZD = *blanks
077100050418     C                   ENDIF
077200041025     C*
077300041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
077400041025     C                   IF        *IN52 = *ON
077500041025     C* Reperisco la provincia dal CAP e dalla località
077600041025     C                   IF        VABCAD <> *blanks AND
077700050907     C                             VABPRD  = *blanks AND
077800050907     C                             VABNZD  = *blanks
077900041025     C                   CLEAR                   TISI95DS
078000041025     C                   EVAL      I95TCN = '3'
078100041025     C                   Z-ADD     datcor        I95DAT
078200041025     C                   EVAL      I95CAP = VABCAD
078300041025     C                   EVAL      I95LOC = VABLOD
078400041025     C                   CALL      'TISI95R'
078500041025     C                   PARM                    TISI95DS
078600041025     C                   EVAL      VABPRD = O95PRV
078700041025     C                   ENDIF
078800041025     C                   ENDIF
078900040630     C*
079000040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
079100040630     C                   exsr      exeBon
079200101027     C*
079300101027     C* forzature richieste
079400101027     C                   if        VABTC1 = 'F' or VABTC2 = 'F'
079500101027     C                   eval      VABNOT = 'CONSEGNA CON SPONDA IDRAULICA'
079600101027     C                   endif
079700930409     C*
079800010604     C  N31              ADD       1             §CTROK            7 0
079900010604     C   32              ADD       1             §CTRMO            7 0
080000010604     C   31              ADD       1             §CTRNO            7 0
080100020913     C  N31              WRITE     FIVAB000
080200990910     C*
080300010601     C                   if        *in31 = *off and
080400010601     C                             *in32 = *off
080500990910     C                   eval      vinflg = '1'
080600990910     C                   else
080700000621     C                   eval      recko = vindta
080800010612     C                   if        %open(tis7t5ps)
080900030407     C   50              except    rigadett
081000010612     C                   endif
081100990910     C                   eval      vinflg = '2'
081200990910     C                   endif
081300991022     C                   endif
081400990910     C                   endif
081500010601     C*
081600010601     C                   endif
081700000905     C*
081800000905     C                   else
081900000905     C                   eval      vinflg = '1'
082000000905     C                   endif
082100000905     C*
082200000905     C  N70              update    tivin000
082300991022     C*
082400991022     C  N70              ENDdo
082500010530     C
082600010530     C*
082700010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
082800010530     C                   if        cntNonEl = *zeros or
082900010530     C                             flgMulti = '0'
083000990910     C* Se non ci sono record con errori ...
083100010530     C                   if        §ctrno = 0 and
083200010601     C                             §ctrmo = 0 and
083300010530     C                             flgStato <> '2'
083400990910     C* ... restituisco esito OK.
083500990921     C                   eval      wrkesito = '0'
083600990910     C                   else
083700010601     C                   if        §ctrok > 0
083800990921     C                   eval      wrkesito = '1'
083900000710     C                   else
084000010615     C                   if        flgOk = '0'
084100010615     C                   eval      wrkesito = '2'
084200010615     C                   else
084300010615     C                   eval      wrkesito = '6'
084400010615     C                   endif
084500990910     C                   endif
084600000710     C                   endif
084700010530     C                   else
084800010530     C                   eval      wrkesito = '9'
084900010530     C                   endif
085000990910     C*
085100990914     C                   if        %open(tivin00r)
085200990908     C                   close     tivin00r
085300990914     C                   endif
085400020913     C                   if        %open(fivabwwr)
085500020913     C                   close     fivabwwr
085600990914     C                   endif
085700010530     C*
085800010530     C                   if        vlrpoi <> 999
085900010530     C                   eval      invfil = vlrpoi
086000010530     C                   endif
086100990910     C*
086200990920     C                   if        §ctrok > 0
086300010530     C                             and invfil > *zeros
086400000613     C                   exsr      invio
086500990920     C                   endif
086600010612     C*
086700010612     C                   if        flgGiro = '1'
086800010612     C                   exsr      endela
086900010612     C                   endif
087000990920     C*
087100910830     C                   ENDSR
087200000613     C***
087300990920
087400010530
087500010530
087600010530
087700010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
087800010530     C     repfil        BEGSR
087900010530     C*
088000010530     C                   if        invfil = *zeros and
088100010530     C                             depfil > *zeros and
088200010613     C                             (vinflg = *blanks or
088300010613     C                              vinflg = *zeros)
088400010530     C
088500010530     C                   eval      invfil = depfil
088600010530     C                   endif
088700010530     C*
088800010530     C                   if        depfil <> invfil and
088900010530     C                             invfil > *zeros
089000010530     C                   eval      flgMulti = '1'
089100010530     C                   if        vinflg = *blanks
089200010530     C                   add       1             cntNonEl
089300010530     C                   endif
089400010530     C                   endif
089500010530     C*
089600010530     C                   if        vinflg = '2'
089700010530     C                   eval      flgStato = '2'
089800010530     C                   endif
089900010530     C*
090000010530     C                   ENDSR
090100010530     C***
090200010530
090300010530
090400010530
090500010530
090600990920      /TITLE Invio dei dati al punto operativo.
090700000613     C     invio         BEGSR
090800990920     C*
090900990920     C                   reset                   dscmz
091000010530     C                   move      invfil        cmzdst
091100990920     C                   eval      cmzfld = vlrfou
091200990920     C                   eval      cmzmbd = vlrhdl
091300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
091400000710     C                   if        prmfir = *blanks
091500020913     C                   eval      cmzfla = 'FIVAB00F'
091600020913     C                   eval      cmzmba = 'FIVAB00F'
091700000710     C                   else
091800000710     C                   eval      cmzfla = prmfir
091900000710     C                   eval      cmzmba = prmfir
092000000710     C                   endif
092100990920     C                   eval      cmznrr = *zeros
092200990920     C                   move      §ctrok        cmznrr
092300021003     C                   eval      cmzlba = vlrfl1
092400990920     C                   call(e)   'TIS711C'
092500990920     C                   parm                    dscmz
092600990921     C                   parm      *blanks       esito
092700990920     C                   if        %error
092800990920     C                             or cmzerr = '1'
092900990921     C                             or esito  = '1'
093000000710     C                   eval      wrkesito = '3'
093100990920     C                   endif
093200990920     C*
093300000613     C                   ENDSR
093400000613     C***
093500010530
093600010530
093700010530
093800010530
093900010530      /TITLE Invio dei dati al punto operativo.
094000010530     C     opeini        BEGSR
094100010530     C*
094200010530     C* Inizializzo flag e contatori operativi
094300010530     C                   movel     '0'           flgGiro           1
094400010530     C                   movel     '0'           flgMulti          1
094500010530     C                   movel     '1'           flgStato          1
094600010615     C                   movel     '0'           flgOk             1
094700010530     C                   z-add     *zeros        cntNonEl         10 0
094800010530     C                   z-add     *zeros        depfil            3 0
094900010530     C                   z-add     *zeros        invfil            3 0
095000030407     C*
095100030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
095200030407     C                   if        %subst(vlrppt:1:1) = 'S'
095300030407     C                   seton                                        50
095400030407     C                   else
095500030407     C                   setoff                                       50
095600030407     C                   endif
095700041001     C*
095800041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
095900041001     C                   if        %subst(vlrppt:2:1) = 'S'
096000041001     C                   seton                                        51
096100041001     C                   else
096200041001     C                   setoff                                       51
096300041001     C                   endif
096400041025     C*
096500041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
096600041025     C* (naturalmente solo se nn già presente)
096700041025     C                   if        %subst(vlrppt:3:1) = 'S'
096800041025     C                   seton                                        52
096900041025     C                   else
097000041025     C                   setoff                                       52
097100041025     C                   endif
097200010530     C*
097300010530     C                   ENDSR
097400010530     C***
097500040407
097600040407
097700040407
097800040407
097900040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
098000040407     C     exeBon        BEGSR
098100040407     C*
098200040407     C* Imposto i parametri x il *pgm d "bonifica"
098300040407     C                   EVAL      ElencoChar = *blanks
098400040407     C                   EVAL      TipoElenco = *blanks
098500040407     C                   EVAL      CharSost   = *blanks
098600040629     C                   EVAL      UpperCase  = '1'
098700040407     C                   EVAL      ChkNull    = '1'
098800040407     C                   EVAL      CharNull   = *blanks
098900040629     C*
099000040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
099100040629     C                   EVAL      TxtInOut   = VABRSD
099200040629     C                   EVAL      EsitoBon   = *blanks
099300040407     C*
099400040407     C                   CALL      'XCHKCHAR'
099500040407     C                   PARM                    TxtInOut
099600040407     C                   PARM                    ElencoChar
099700040407     C                   PARM                    TipoElenco
099800040407     C                   PARM                    CharSost
099900040407     C                   PARM                    UpperCase
100000040407     C                   PARM                    ChkNull
100100040407     C                   PARM                    CharNull
100200040407     C                   PARM                    EsitoBon
100300040407     C*
100400040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
100500040407     C                   IF        EsitoBon = '1'
100600040629     C                   EVAL      VABRSD = TxtInOut
100700040407     C                   ENDIF
100800040629     C*
100900040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
101000040629     C                   EVAL      TxtInOut   = VABIND
101100040629     C                   EVAL      EsitoBon   = *blanks
101200040629     C*
101300040629     C                   CALL      'XCHKCHAR'
101400040629     C                   PARM                    TxtInOut
101500040629     C                   PARM                    ElencoChar
101600040629     C                   PARM                    TipoElenco
101700040629     C                   PARM                    CharSost
101800040629     C                   PARM                    UpperCase
101900040629     C                   PARM                    ChkNull
102000040629     C                   PARM                    CharNull
102100040629     C                   PARM                    EsitoBon
102200040629     C*
102300040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
102400040629     C                   IF        EsitoBon = '1'
102500040629     C                   EVAL      VABIND = TxtInOut
102600040629     C                   ENDIF
102700040629     C*
102800040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
102900040629     C                   EVAL      TxtInOut   = VABLOD
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
104400040629     C                   EVAL      VABLOD = TxtInOut
104500040629     C                   ENDIF
104600040629     C*
104700040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
104800040629     C                   EVAL      TxtInOut   = VABNOT
104900040629     C                   EVAL      EsitoBon   = *blanks
105000040630     C                   EVAL      UpperCase  = *blanks
105100040629     C*
105200040629     C                   CALL      'XCHKCHAR'
105300040629     C                   PARM                    TxtInOut
105400040629     C                   PARM                    ElencoChar
105500040629     C                   PARM                    TipoElenco
105600040629     C                   PARM                    CharSost
105700040629     C                   PARM                    UpperCase
105800040629     C                   PARM                    ChkNull
105900040629     C                   PARM                    CharNull
106000040629     C                   PARM                    EsitoBon
106100040629     C*
106200040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
106300040629     C                   IF        EsitoBon = '1'
106400040629     C                   EVAL      VABNOT = TxtInOut
106500040629     C                   ENDIF
106600040629     C*
106700040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
106800040629     C                   EVAL      TxtInOut   = VABNT2
106900040629     C                   EVAL      EsitoBon   = *blanks
107000040630     C                   EVAL      UpperCase  = *blanks
107100040629     C*
107200040629     C                   CALL      'XCHKCHAR'
107300040629     C                   PARM                    TxtInOut
107400040629     C                   PARM                    ElencoChar
107500040629     C                   PARM                    TipoElenco
107600040629     C                   PARM                    CharSost
107700040629     C                   PARM                    UpperCase
107800040629     C                   PARM                    ChkNull
107900040629     C                   PARM                    CharNull
108000040629     C                   PARM                    EsitoBon
108100040629     C*
108200040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
108300040629     C                   IF        EsitoBon = '1'
108400040629     C                   EVAL      VABNT2 = TxtInOut
108500040629     C                   ENDIF
108600040407     C*
108700040407     C                   ENDSR
108800010530
108900060410
109000060410     C*----------------------------------------------------*
109100060410     C*  CONTROLLO NUMERICITA' CAMPI
109200060410     C*----------------------------------------------------*
109300060410     C     CHKNUM        BEGSR
109400060410     C*
109500060410     C                   IF        PiDecChr = *blanks
109600060410     C                   EVAL      PiDecChr = ','
109700060410     C                   ENDIF
109800060410     C*
109900060410     C                   CALL(e)   'ISNUMERIC'
110000060410     C                   PARM                    PiStr            30
110100060410     C                   PARM                    PiDecChr          1
110200060410     C                   PARM      *ZEROS        PiVal            30 9
110300060410     C                   PARM      '0'           PiInt             1
110400060410     C                   PARM      '0'           PiNum             1
110500060410     C                   IF        %error
110600060410     C                   EVAL      PiNum=*off
110700060410     C                   ENDIF
110800060410     C*
110900060410     C                   ENDSR
111000060410     C***
111100060410
111200990910
111300000613     C     *inzsr        BEGSR
111400990910     C*
111500990910     C     *entry        plist
111600990920     C                   parm                    tivlrds
111700990921     C                   parm      wrkesito      esito
111800000724     C                   parm                    prmlit
111900000710     C                   parm                    prmfir
112000010606     C*
112100010606     C* CALCOLA LA DATA CORRENTE
112200010606     C                   time                    wn14             14 0
112300010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
112400010606     C                   z-add     wn8           g08dat
112500010606     C                   z-add     *zeros        g08inv
112600010606     C                   movel     '0'           g08err
112700010606     C                   call      'XSRDA8'
112800010606     C                   parm                    wlbda8
112900010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
113000000613     C*
113100000613     C                   ENDSR
113200000613     C***
113300990908
113400010601     Otis7t5p   E            riepilogo         2
113500990915     O                                              'Upload via Internet'
113600990915     O                                           +1 'Traduzione TIVIN00R -'
113700020913     O                                           +1 'FIVABWWR'
113800990915     O                                           +1 'Testate bolle'
113900990915     O          E            riepilogo   2
114000990915     O                       wrkdata
114100990915     O                       wrkora              +1
114200990915     O                       procname            +1
114300990915     O          E            riepilogo   2
114400990915     O                                              'Cliente..................:'
114500990915     O                       VABCCM        z     +1
114600990915     O          E            riepilogo   2
114700990920     O                                              'Riferimento Strategi.....:'
114800990920     O                       vlrhdl              +1
114900990915     O          E            riepilogo   2
115000990915     O                                              'Giusti...................:'
115100971022     O                       §CTROK        2   +  1
115200990915     O          E            riepilogo   2
115300990915     O                                              'Sbagliati e corretti.....:'
115400971022     O                       §CTRMO        2   +  1
115500990915     O          E            riepilogo   2
115600990915     O                                              'Sbagliati e scartati.....:'
115700971022     O                       §CTRNO        2   +  1
115800000613
115900010601     Otis7t5ps  E            testdett          2
116000000613     O                                              'Upload via Internet'
116100000613     O                                           +1 'Traduzione TIVIN00R -'
116200020913     O                                           +1 'FIVABWWR'
116300000613     O                                           +1 'Testate bolle'
116400000616     O          E            testdett    3
116500000613     O                                           +2 'N° rec'
116600000613     O                                           +3 'Anteprima contenuto'
116700000616     O          E            rigadett    2
116800000613     O                       rrnum               +2
116900000621     O                       recko               +3
117000000616     O          E            findett     2
117100000613     O                       wrkdata
117200000613     O                       wrkora              +1
117300000613     O                       procname            +1
117400000616     O          E            findett     2
117500000613     O                                              'Cliente..................:'
117600000613     O                       VABCCM        z     +1
117700000616     O          E            findett     2
117800000613     O                                              'Riferimento Strategi.....:'
117900000613     O                       vlrhdl              +1
118000000616     O          E            findett     2
118100000613     O                                              'Giusti...................:'
118200000613     O                       §CTROK        2   +  1
118300000616     O          E            findett     2
118400000613     O                                              'Sbagliati e corretti.....:'
118500000613     O                       §CTRMO        2   +  1
118600000616     O          E            findett     2
118700000613     O                                              'Sbagliati e scartati.....:'
118800000613     O                       §CTRNO        2   +  1
118900000512** CMD - COMANDI CL
119000011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T5P)  FORMTYPE(RICCLI) OUTQ(
119100011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T5PS) FORMTYPE(RICCLI) OUTQ(
119200010601DLTOVR FILE(TIS7T5P TIS7T5PS) LVL(*)
119300000512
119400000512
