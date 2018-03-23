000100050722      /TITLE Upload via Internet: traduzione in FIVAB/FIVAT.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050722     FFIVATwwr  O    E             DISK    usropn
000700020913     FFIVABwwr  O    E             DISK    usropn
000800010601     Ftis7t5p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100010601     Ftis7t5ps  O    f  198        PRINTER usropn
001200000621     F                                     oflind(*inob)
001300070411     F                                     infsr(*pssr)
001400990908
001500000512     D*------------
001600000512     D* COMANDI
001700000512     D*------------
001800011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001900000512     D*------------
002000990920     D dscmz         e ds                  inz
002100990910     D psds           sds
002200990910     D  procname         *PROC
002300941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002400941229     D* 10+3
002500941229     D                 DS
002600941229     D  WS15                   1      1
002700941229     D  WI15                   2     11
002800941229     D  WV15                  12     12
002900941229     D  WD15                  13     15
003000941229     D  WASC15                 1     15
003100941229     D*  6+1
003200941229     D                 DS
003300941229     D  WS09                   1      1
003400941229     D  WI09                   2      7
003500941229     D  WV09                   8      8
003600941229     D  WD09                   9      9
003700941229     D  WASC09                 1      9
003800941229     D*  2+3
003900941229     D                 DS
004000941229     D  WS07                   1      1
004100941229     D  WI07                   2      3
004200941229     D  WV07                   4      4
004300941229     D  WD07                   5      7
004400941229     D  WASC07                 1      7
004500990920     D tivlrds       e ds                  extname(tivlr00f)
004600041025     D tisi95ds      e ds
004700990910     D fnvabaar      e ds                  inz
004800990910     D esito           s              1
004900000724     D prmlit          s             10
005000000710     D prmfir          s             10
005100990921     D wrkesito        s                   like(esito)
005200990915     D wrkdata         s               d
005300990915     D wrkora          s               t
005400000613     D rrnum           s              6  0 INZ(*zeros)
005500000621     D recko           s            150    INZ(*blanks)
005600011123     D depcmd          s            150    INZ(*blanks)
005700050722     D parccm          s              8    INZ(*blanks)
005800050722     D parmbr          s             10    INZ(*blanks)
005900050722     D paropz          s              1    INZ(*blanks)
006000050722     D chkcall         s              1    INZ(*blanks)
006100010606     D*------------------
006200010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
006300010606     D*------------------
006400010606     D WLBDA8          DS                  INZ
006500010606     D  G08DAT                 1      8  0
006600010606     D  G08INV                 9     16  0
006700010606     D  G08ERR                17     17
006800010606     D  G08TGI                18     22  0
006900010606     D*
007000040407     D TxtInOut        S           2048
007100040407     D ElencoChar      S            256
007200040407     D TipoElenco      S              1
007300040407     D CharSost        S              1
007400040407     D UpperCase       S              1
007500040407     D ChkNull         S              1
007600040407     D CharNull        S              1
007700040407     D EsitoBon        S              1
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
015700050722     C*
015800050722     C* Effettuo la chiamata al CLLE preposto
015900050722     C                   call(e)   'TITVVTC'
016000050722     C                   parm                    parccm
016100050722     C                   parm                    parmbr
016200050722     C                   parm      '2'           paropz
016300000613     C*
016400010601     C                   if        %open(tis7t5ps)
016500030407     C   50              except    findett
016600010601     C                   close     tis7t5ps
016700000613     C                   endif
016800000616     C*
016900000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
017000011123     C                   Z-ADD     150           LENGH            15 5
017100010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017200000616     C                   PARM                    CMD(3)
017300000616     C                   PARM                    LENGH
017400010530     C*
017500010530     C                   ENDSR
017600010530     C***
017700000613
017800010530
017900010530
018000000613
018100000613     C*--------------------------------------------------------
018200000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018300000613     C*--------------------------------------------------------
018400000621     C     STPR          BEGSR
018500000613     C*
018600010601     C                   if        not %open(tis7t5p)
018700010601     C                   open      tis7t5p
018800990915     C                   endif
018900990915     C*
019000030407     C   50              except    riepilogo
019100990915     C*
019200010601     C                   if        %open(tis7t5p)
019300010601     C                   close     tis7t5p
019400990914     C                   endif
019500990910     C*
019600000613     C                   ENDSR
019700000613     C***
019800990908
019900010530
020000010530
020100010530
020200910830     C*--------------------------------------------------------
020300020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
020400910830     C*--------------------------------------------------------
020500941229     C     RWVAB         BEGSR
020600990910     C*
020700990914     C                   if        not %open(tivin00r)
020800990908     C                   open      tivin00r
020900990914     C                   endif
021000020913     C                   if        not %open(fivabwwr)
021100020913     C                   open      fivabwwr
021200990914     C                   endif
021300050722     C*
021400050722     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021500050722     C                   exsr      prevat
021600050722     C*
021700050722     C                   if        chkcall = '0'
021800050722     C*
021900050722     C                   if        not %open(fivatwwr)
022000050722     C                   open      fivatwwr
022100050722     C                   endif
022200990910     C*
022300050722     C                   clear                   §CTROKVB          7 0
022400050722     C                   clear                   §CTROKVT          7 0
022500050722     C                   clear                   §CTRMO            7 0
022600050722     C                   clear                   §CTRNO            7 0
022700990910     C*
022800921023     C                   DO        *HIVAL
022900990913     C*
023000990915     C                   READ      tivin00r                               70
023100010618     C*
023200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023300010618     C                   if        vinflg = '1'
023400010618     C                   eval      flgOk = '1'
023500010618     C                   endif
023600010705     C*
023700010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
023800010705     C                   if        %subst(vindta:1:1) = '='
023900010705     C                   eval      %subst(vindta:1:1) = *blanks
024000010705     C                   endif
024100010705     C*
024200000905     C                   if        vindta > *blanks
024300000613     C                   add       1             rrnum
024400010601     C* ...elimino eventuali segni
024500010601     C     '+':' '       xlate     vindta        vindta
024600990913     C*
024700010601     C                   if        *in70 = *off and
024800990913     C                             (vinflg = *blanks
024900010711     C                              or vinflg = '0'
025000010711     C                              or vinflg = '2')
025100990913     C*
025200020913     C                   clear                   FIVAB000
025300050722     C                   clear                   FIVAT000
025400010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025500010711     C                   if        vinflg = *blanks or vinflg = '0'
025600010711     C                   clear                   vinmsg
025700010711     C                   endif
025800010711     C*
025900990913     C                   eval      fnvabaar = vindta
026000991022     C*
026100991022     C                   testb     x'3F'         vhbatb                   02
026200991022     C                   if        not *in02
026300991022     C*
026400971021     C                   SETOFF                                       303132
026500971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
026600971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
026700971023     C*    OPPURE TUTTI BLANKS
026800010530     C*
026900010530     C                   TESTN                   VHBLNP               30
027000010530     C  N30              TESTN                   VHBLNP                 30
027100010530     C  N30              TESTN                   VHBLNP                   30
027200010530     C                   MOVE      VHBLNP        W001A             1
027300010530     C     *IN30         IFEQ      *OFF
027400010530     C     W001A         ORLT      '0'
027500010530     C                   SETON                                        31
027600010530     C                   eval      vinmsg = %trimr(vinmsg)
027700010530     C                             + ' ' + 'VHBLNP'
027800010530     C                   ELSE
027900010530     C                   MOVE      VHBLNP        VABLNP
028000010530     C     VABLNP        IFLE      *ZEROS
028100010530     C                   SETON                                        31
028200010530     C                   eval      vinmsg = %trimr(vinmsg)
028300010530     C                             + ' ' + 'VHBLNP'
028400010530     C                   ENDIF
028500010530     C                   ENDIF
028600010530     C*
028700010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028800010530     C                   eval      depfil = VABLNP
028900010530     C                   exsr      repfil
029000010601     C                   if        depfil = invfil
029100021014     C                   if        vlrpoi = 999
029200021010     C                   MOVE(P)   invfil        VABFGS
029300050722     C                   move(P)   invfil        VATFGS
029400021014     C                   else
029500021014     C                   MOVE(P)   vlrpoi        VABFGS
029600050722     C                   move(P)   vlrpoi        VATFGS
029700021014     C                   endif
029800010530     C*
029900010530     C                   exsr      PREELA
030000010530     C*
030100971023     C                   TESTN                   VHBCCM               30
030200971023     C  N30              TESTN                   VHBCCM                 30
030300971023     C  N30              TESTN                   VHBCCM                   30
030400971021     C                   MOVE      VHBCCM        W001A             1
030500971021     C     *IN30         IFEQ      *OFF
030600971021     C     W001A         ORLT      '0'
030700971021     C                   SETON                                        31
030800010604     C                   eval      vinmsg = %trimr(vinmsg)
030900010604     C                             + ' ' + 'VHBCCM'
031000971021     C                   ELSE
031100971021     C                   MOVE      VHBCCM        VABCCM
031200971024     C     VABCCM        IFLE      *ZEROS
031300971024     C                   SETON                                        31
031400010604     C                   eval      vinmsg = %trimr(vinmsg)
031500010604     C                             + ' ' + 'VHBCCM'
031600971024     C                   ENDIF
031700971021     C                   ENDIF
031800971021     C*
031900971023     C                   TESTN                   VHBAAS               30
032000971023     C  N30              TESTN                   VHBAAS                 30
032100971023     C  N30              TESTN                   VHBAAS                   30
032200971021     C                   MOVE      VHBAAS        W001A             1
032300971021     C     *IN30         IFEQ      *OFF
032400971021     C     W001A         ORLT      '0'
032500971023     C     W001A         ANDNE     *BLANK
032600971021     C                   SETON                                        32
032700010606     C                   MOVEL     datcor        VABAAS
032800010604     C                   eval      vinmsg = %trimr(vinmsg)
032900010604     C                             + ' ' + 'VHBAAS'
033000971021     C                   ELSE
033100971021     C                   MOVE      VHBAAS        VABAAS
033200971021     C                   ENDIF
033300971021     C*
033400971023     C                   TESTN                   VHBMGS               30
033500971023     C  N30              TESTN                   VHBMGS                 30
033600971023     C  N30              TESTN                   VHBMGS                   30
033700971021     C                   MOVE      VHBMGS        W001A             1
033800971021     C     *IN30         IFEQ      *OFF
033900971021     C     W001A         ORLT      '0'
034000971023     C     W001A         ANDNE     *BLANK
034100971021     C                   SETON                                        32
034200010601     C                   MOVE      '0101'        VABMGS
034300010604     C                   eval      vinmsg = %trimr(vinmsg)
034400010604     C                             + ' ' + 'VHBMGS'
034500971021     C                   ELSE
034600971021     C                   MOVE      VHBMGS        VABMGS
034700971021     C                   ENDIF
034800971021     C*
034900971023     C                   TESTN                   VHBNRS               30
035000971023     C  N30              TESTN                   VHBNRS                 30
035100971023     C  N30              TESTN                   VHBNRS                   30
035200971021     C                   MOVE      VHBNRS        W001A             1
035300971021     C     *IN30         IFEQ      *OFF
035400971021     C     W001A         ORLT      '0'
035500971023     C     W001A         ANDNE     *BLANK
035600971021     C                   SETON                                        32
035700971021     C                   MOVE      *ZERO         VABNRS
035800010604     C                   eval      vinmsg = %trimr(vinmsg)
035900010604     C                             + ' ' + 'VHBNRS'
036000971021     C                   ELSE
036100971021     C                   MOVE      VHBNRS        VABNRS
036200971021     C                   ENDIF
036300971021     C*
036400971023     C                   TESTN                   VHBNSP               30
036500971023     C  N30              TESTN                   VHBNSP                 30
036600971023     C  N30              TESTN                   VHBNSP                   30
036700971021     C                   MOVE      VHBNSP        W001A             1
036800971021     C     *IN30         IFEQ      *OFF
036900971021     C     W001A         ORLT      '0'
037000971023     C     W001A         ANDNE     *BLANK
037100971021     C                   SETON                                        32
037200971021     C                   MOVE      *ZERO         VABNSP
037300010604     C                   eval      vinmsg = %trimr(vinmsg)
037400010604     C                             + ' ' + 'VHBNSP'
037500971021     C                   ELSE
037600971021     C                   MOVE      VHBNSP        VABNSP
037700971021     C                   ENDIF
037800971021     C*
037900971023     C                   TESTN                   VHBLNA               30
038000971023     C  N30              TESTN                   VHBLNA                 30
038100971023     C  N30              TESTN                   VHBLNA                   30
038200971021     C                   MOVE      VHBLNA        W001A             1
038300971021     C     *IN30         IFEQ      *OFF
038400971021     C     W001A         ORLT      '0'
038500971023     C     W001A         ANDNE     *BLANK
038600971021     C                   SETON                                        32
038700971021     C                   MOVE      *ZERO         VABLNA
038800010604     C                   eval      vinmsg = %trimr(vinmsg)
038900010604     C                             + ' ' + 'VHBLNA'
039000971021     C                   ELSE
039100971021     C                   MOVE      VHBLNA        VABLNA
039200971021     C                   ENDIF
039300971021     C*
039400971023     C                   TESTN                   VHBCTR               30
039500971023     C  N30              TESTN                   VHBCTR                 30
039600971023     C  N30              TESTN                   VHBCTR                   30
039700971021     C                   MOVE      VHBCTR        W001A             1
039800971021     C     *IN30         IFEQ      *OFF
039900971021     C     W001A         ORLT      '0'
040000971023     C     W001A         ANDNE     *BLANK
040100971021     C                   SETON                                        32
040200971021     C                   MOVE      *ZERO         VABCTR
040300010604     C                   eval      vinmsg = %trimr(vinmsg)
040400010604     C                             + ' ' + 'VHBCTR'
040500971021     C                   ELSE
040600971021     C                   MOVE      VHBCTR        VABCTR
040700971021     C                   ENDIF
040800941229     C*
040900971021     C                   MOVE      *ZERO         VABIAS
041000941229     C                   MOVE      VHBIAS        WASC15
041100971023     C                   TESTN                   WI15                 30
041200971023     C  N30              TESTN                   WI15                   30
041300971023     C  N30              TESTN                   WI15                     30
041400971021     C                   MOVE      WI15          W001A             1
041500971021     C     *IN30         IFEQ      *OFF
041600971021     C     W001A         ORLT      '0'
041700971023     C     W001A         ANDNE     *BLANK
041800971021     C                   SETON                                        32
041900971021     C                   MOVE      *ZERO         VABIAS
042000010604     C                   eval      vinmsg = %trimr(vinmsg)
042100010604     C                             + ' ' + 'VHBIAS'
042200971021     C                   ELSE
042300971021     C                   MOVEL     WI15          VABIAS
042400971023     C                   TESTN                   WD15                 30
042500971023     C  N30              TESTN                   WD15                   30
042600971023     C  N30              TESTN                   WD15                     30
042700971021     C                   MOVE      WD15          W001A             1
042800971021     C     *IN30         IFEQ      *OFF
042900971021     C     W001A         ORLT      '0'
043000971023     C     W001A         ANDNE     *BLANK
043100971021     C                   SETON                                        32
043200971021     C                   MOVE      *ZERO         VABIAS
043300010604     C                   eval      vinmsg = %trimr(vinmsg)
043400010604     C                             + ' ' + 'VHBIAS'
043500971021     C                   ELSE
043600971021     C                   MOVE      WD15          VABIAS
043700971021     C                   ENDIF
043800050118     C                   ENDIF
043900941229     C*
044000971023     C                   TESTN                   VHBNCL               30
044100971023     C  N30              TESTN                   VHBNCL                 30
044200971023     C  N30              TESTN                   VHBNCL                   30
044300971021     C                   MOVE      VHBNCL        W001A             1
044400971021     C     *IN30         IFEQ      *OFF
044500971021     C     W001A         ORLT      '0'
044600971021     C                   SETON                                        32
044700971021     C                   MOVE      *ZERO         VABNCL
044800010604     C                   eval      vinmsg = %trimr(vinmsg)
044900010604     C                             + ' ' + 'VHBNCL'
045000971021     C                   ELSE
045100971021     C                   MOVE      VHBNCL        VABNCL
045200971021     C                   ENDIF
045300941229     C*
045400971021     C                   MOVE      *ZERO         VABPKB
045500971021     C                   MOVE      VHBPKB        WASC09
045600010719     C                   IF        WI09 = *blanks
045700010719     C                   EVAL      WI09 = *zeros
045800010719     C                   ENDIF
045900971023     C                   TESTN                   WI09                 30
046000971023     C  N30              TESTN                   WI09                   30
046100971023     C  N30              TESTN                   WI09                     30
046200971021     C                   MOVE      WI09          W001A             1
046300971021     C     *IN30         IFEQ      *OFF
046400971021     C     W001A         ORLT      '0'
046500971021     C                   SETON                                        32
046600971021     C                   MOVE      *ZERO         VABPKB
046700010604     C                   eval      vinmsg = %trimr(vinmsg)
046800010604     C                             + ' ' + 'VHBPKB'
046900971021     C                   ELSE
047000971021     C                   MOVEL     WI09          VABPKB
047100010719     C                   IF        WD09 = *blanks
047200010719     C                   EVAL      WD09 = *zeros
047300010719     C                   ENDIF
047400971023     C                   TESTN                   WD09                 30
047500971023     C  N30              TESTN                   WD09                     30
047600971021     C                   MOVE      WD09          W001A             1
047700971021     C     *IN30         IFEQ      *OFF
047800971021     C     W001A         ORLT      '0'
047900971023     C     W001A         ANDNE     *BLANK
048000971021     C                   SETON                                        32
048100971021     C                   MOVE      *ZERO         VABPKB
048200010604     C                   eval      vinmsg = %trimr(vinmsg)
048300010604     C                             + ' ' + 'VHBPKB'
048400971021     C                   ELSE
048500971021     C                   MOVE      WD09          VABPKB
048600971021     C                   ENDIF
048700050118     C                   ENDIF
048800971021     C*
048900041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
049000041001     C                   IF        *IN51 = *OFF
049100971021     C                   MOVE      *ZERO         VABVLB
049200971021     C                   MOVE      VHBVLB        WASC07
049300010719     C                   IF        WI07 = *blanks
049400010719     C                   EVAL      WI07 = *zeros
049500010719     C                   ENDIF
049600971023     C                   TESTN                   WI07                 30
049700971023     C  N30              TESTN                   WI07                   30
049800971023     C  N30              TESTN                   WI07                     30
049900971021     C                   MOVE      WI07          W001A             1
050000971021     C     *IN30         IFEQ      *OFF
050100971021     C     W001A         ORLT      '0'
050200971023     C     W001A         ANDNE     *BLANK
050300971021     C                   SETON                                        32
050400971021     C                   MOVE      *ZERO         VABVLB
050500010604     C                   eval      vinmsg = %trimr(vinmsg)
050600010604     C                             + ' ' + 'VHBVLB'
050700971021     C                   ELSE
050800971021     C                   MOVEL     WI07          VABVLB
050900010719     C                   IF        WD07 = *blanks
051000010719     C                   EVAL      WD07 = *zeros
051100010719     C                   ENDIF
051200971023     C                   TESTN                   WD07                 30
051300971023     C  N30              TESTN                   WD07                   30
051400971023     C  N30              TESTN                   WD07                     30
051500971021     C                   MOVE      WD07          W001A             1
051600971021     C     *IN30         IFEQ      *OFF
051700971021     C     W001A         ORLT      '0'
051800971023     C     W001A         ANDNE     *BLANK
051900971021     C                   SETON                                        32
052000971021     C                   MOVE      *ZERO         VABVLB
052100010604     C                   eval      vinmsg = %trimr(vinmsg)
052200010604     C                             + ' ' + 'VHBVLB'
052300971021     C                   ELSE
052400971021     C                   MOVE      WD07          VABVLB
052500971021     C                   ENDIF
052600050118     C                   ENDIF
052700041001     C                   ELSE
052800041001     C* ...altrimenti forzo azzeramento del volume
052900041001     C                   Z-ADD     *zeros        VABVLB
053000041001     C                   ENDIF
053100941229     C*
053200971021     C                   MOVE      *ZERO         VABQFT
053300971021     C                   MOVE      VHBQFT        WASC15
053400010719     C                   IF        WI15 = *blanks
053500010719     C                   EVAL      WI15 = *zeros
053600010719     C                   ENDIF
053700971023     C                   TESTN                   WI15                 30
053800971023     C  N30              TESTN                   WI15                   30
053900971023     C  N30              TESTN                   WI15                     30
054000971021     C                   MOVE      WI15          W001A             1
054100971021     C     *IN30         IFEQ      *OFF
054200971021     C     W001A         ORLT      '0'
054300971023     C     W001A         ANDNE     *BLANK
054400971021     C                   SETON                                        32
054500971021     C                   MOVE      *ZERO         VABQFT
054600010604     C                   eval      vinmsg = %trimr(vinmsg)
054700010604     C                             + ' ' + 'VHBQFT'
054800971021     C                   ELSE
054900971021     C                   MOVEL     WI15          VABQFT
055000010719     C                   IF        WD15 = *blanks
055100010719     C                   EVAL      WD15 = *zeros
055200010719     C                   ENDIF
055300971023     C                   TESTN                   WD15                 30
055400971023     C  N30              TESTN                   WD15                   30
055500971023     C  N30              TESTN                   WD15                     30
055600971021     C                   MOVE      WD15          W001A             1
055700971021     C     *IN30         IFEQ      *OFF
055800971021     C     W001A         ORLT      '0'
055900971023     C     W001A         ANDNE     *BLANK
056000971021     C                   SETON                                        32
056100971021     C                   MOVE      *ZERO         VABQFT
056200010604     C                   eval      vinmsg = %trimr(vinmsg)
056300010604     C                             + ' ' + 'VHBQFT'
056400971021     C                   ELSE
056500971021     C                   MOVE      WD15          VABQFT
056600971021     C                   ENDIF
056700050118     C                   ENDIF
056800941229     C*
056900971021     C                   MOVE      *ZERO         VABCAS
057000971021     C                   MOVE      VHBCAS        WASC15
057100010719     C                   IF        WI15 = *blanks
057200010719     C                   EVAL      WI15 = *zeros
057300010719     C                   ENDIF
057400971023     C                   TESTN                   WI15                 30
057500971023     C  N30              TESTN                   WI15                   30
057600971023     C  N30              TESTN                   WI15                     30
057700971021     C                   MOVE      WI15          W001A             1
057800971021     C     *IN30         IFEQ      *OFF
057900971021     C     W001A         ORLT      '0'
058000971023     C     W001A         ANDNE     *BLANK
058100971021     C                   SETON                                        32
058200971021     C                   MOVE      *ZERO         VABCAS
058300010604     C                   eval      vinmsg = %trimr(vinmsg)
058400010604     C                             + ' ' + 'VHBCAS'
058500971021     C                   ELSE
058600971021     C                   MOVEL     WI15          VABCAS
058700010719     C                   IF        WD15 = *blanks
058800010719     C                   EVAL      WD15 = *zeros
058900010719     C                   ENDIF
059000971023     C                   TESTN                   WD15                 30
059100971023     C  N30              TESTN                   WD15                   30
059200971023     C  N30              TESTN                   WD15                     30
059300971021     C                   MOVE      WD15          W001A             1
059400971021     C     *IN30         IFEQ      *OFF
059500971021     C     W001A         ORLT      '0'
059600971023     C     W001A         ANDNE     *BLANK
059700971021     C                   SETON                                        32
059800971021     C                   MOVE      *ZERO         VABCAS
059900010604     C                   eval      vinmsg = %trimr(vinmsg)
060000010604     C                             + ' ' + 'VHBCAS'
060100971021     C                   ELSE
060200971021     C                   MOVE      WD15          VABCAS
060300971021     C                   ENDIF
060400040827     C                   ENDIF
060500971021     C*
060600971023     C                   TESTN                   VHBRMN               30
060700971023     C  N30              TESTN                   VHBRMN                 30
060800971023     C  N30              TESTN                   VHBRMN                   30
060900971021     C                   MOVE      VHBRMN        W001A             1
061000971021     C     *IN30         IFEQ      *OFF
061100971021     C     W001A         ORLT      '0'
061200971021     C                   SETON                                        32
061300971021     C                   MOVE      *ZERO         VABRMN
061400010604     C                   eval      vinmsg = %trimr(vinmsg)
061500010604     C                             + ' ' + 'VHBRMN'
061600971021     C                   ELSE
061700971021     C                   MOVE      VHBRMN        VABRMN
061800971021     C                   ENDIF
061900971021     C*
062000971023     C                   TESTN                   VHBNCD               30
062100971023     C  N30              TESTN                   VHBNCD                 30
062200971023     C  N30              TESTN                   VHBNCD                   30
062300971021     C                   MOVE      VHBNCD        W001A             1
062400971021     C     *IN30         IFEQ      *OFF
062500971021     C     W001A         ORLT      '0'
062600971023     C     W001A         ANDNE     *BLANK
062700971021     C                   SETON                                        32
062800971021     C                   MOVE      *ZERO         VABNCD
062900010604     C                   eval      vinmsg = %trimr(vinmsg)
063000010604     C                             + ' ' + 'VHBNCD'
063100971021     C                   ELSE
063200971021     C                   MOVE      VHBNCD        VABNCD
063300971021     C                   ENDIF
063400971021     C*
063500971023     C                   TESTN                   VHBNCA               30
063600971023     C  N30              TESTN                   VHBNCA                 30
063700971023     C  N30              TESTN                   VHBNCA                   30
063800971021     C                   MOVE      VHBNCA        W001A             1
063900971021     C     *IN30         IFEQ      *OFF
064000971021     C     W001A         ORLT      '0'
064100971023     C     W001A         ANDNE     *BLANK
064200971021     C                   SETON                                        32
064300971021     C                   MOVE      *ZERO         VABNCA
064400010604     C                   eval      vinmsg = %trimr(vinmsg)
064500010604     C                             + ' ' + 'VHBNCA'
064600971021     C                   ELSE
064700971021     C                   MOVE      VHBNCA        VABNCA
064800971021     C                   ENDIF
064900971021     C*
065000971023     C                   TESTN                   VHBZNC               30
065100971023     C  N30              TESTN                   VHBZNC                 30
065200971023     C  N30              TESTN                   VHBZNC                   30
065300971021     C                   MOVE      VHBZNC        W001A             1
065400971021     C     *IN30         IFEQ      *OFF
065500971021     C     W001A         ORLT      '0'
065600971023     C     W001A         ANDNE     *BLANK
065700971021     C                   SETON                                        32
065800971021     C                   MOVE      *ZERO         VABZNC
065900010604     C                   eval      vinmsg = %trimr(vinmsg)
066000010604     C                             + ' ' + 'VHBZNC'
066100971021     C                   ELSE
066200971021     C                   MOVE      VHBZNC        VABZNC
066300971021     C                   ENDIF
066400971021     C*
066500971023     C                   TESTN                   VHBDCR               30
066600971023     C  N30              TESTN                   VHBDCR                 30
066700971023     C  N30              TESTN                   VHBDCR                   30
066800971021     C                   MOVE      VHBDCR        W001A             1
066900971021     C     *IN30         IFEQ      *OFF
067000971021     C     W001A         ORLT      '0'
067100971023     C     W001A         ANDNE     *BLANK
067200971021     C                   SETON                                        32
067300971021     C                   MOVE      *ZERO         VABDCR
067400010604     C                   eval      vinmsg = %trimr(vinmsg)
067500010604     C                             + ' ' + 'VHBDCR'
067600971021     C                   ELSE
067700971021     C                   MOVE      VHBDCR        VABDCR
067800971021     C                   ENDIF
067900971021     C*
068000971023     C                   TESTN                   VHBHCR               30
068100971023     C  N30              TESTN                   VHBHCR                 30
068200971023     C  N30              TESTN                   VHBHCR                   30
068300971021     C                   MOVE      VHBHCR        W001A             1
068400971021     C     *IN30         IFEQ      *OFF
068500971021     C     W001A         ORLT      '0'
068600971023     C     W001A         ANDNE     *BLANK
068700971021     C                   SETON                                        32
068800971021     C                   MOVE      *ZERO         VABHCR
068900010604     C                   eval      vinmsg = %trimr(vinmsg)
069000010604     C                             + ' ' + 'VHBHCR'
069100971021     C                   ELSE
069200971021     C                   MOVE      VHBHCR        VABHCR
069300971021     C                   ENDIF
069400941229     C*
069500971021     C                   MOVE      *ZERO         VABVMD
069600971021     C                   MOVE      VHBVMD        WASC15
069700010719     C                   IF        WI15 = *blanks
069800010719     C                   EVAL      WI15 = *zeros
069900010719     C                   ENDIF
070000971023     C                   TESTN                   WI15                 30
070100971023     C  N30              TESTN                   WI15                   30
070200971023     C  N30              TESTN                   WI15                     30
070300971021     C                   MOVE      WI15          W001A             1
070400971021     C     *IN30         IFEQ      *OFF
070500971021     C     W001A         ORLT      '0'
070600971023     C     W001A         ANDNE     *BLANK
070700971021     C                   SETON                                        32
070800971021     C                   MOVE      *ZERO         VABVMD
070900010604     C                   eval      vinmsg = %trimr(vinmsg)
071000010604     C                             + ' ' + 'VHBVMD'
071100971021     C                   ELSE
071200971021     C                   MOVEL     WI15          VABVMD
071300010719     C                   IF        WD15 = *blanks
071400010719     C                   EVAL      WD15 = *zeros
071500010719     C                   ENDIF
071600971023     C                   TESTN                   WD15                 30
071700971023     C  N30              TESTN                   WD15                   30
071800971023     C  N30              TESTN                   WD15                     30
071900971021     C                   MOVE      WD15          W001A             1
072000971021     C     *IN30         IFEQ      *OFF
072100971021     C     W001A         ORLT      '0'
072200971023     C     W001A         ANDNE     *BLANK
072300971021     C                   SETON                                        32
072400971021     C                   MOVE      *ZERO         VABVMD
072500010604     C                   eval      vinmsg = %trimr(vinmsg)
072600010604     C                             + ' ' + 'VHBVMD'
072700971021     C                   ELSE
072800971021     C                   MOVE      WD15          VABVMD
072900971021     C                   ENDIF
073000050118     C                   ENDIF
073100971021     C*
073200971021     C* CAMPI ALFABETICI:
073300971021     C                   MOVE      VHBATB        VABATB
073400971021     C                   MOVE      VHBCBO        VABCBO
073500971021     C                   MOVE      VHBRSD        VABRSD
073600020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
073700020117     C     '@':'A'       XLATE     VABRSD        VABRSD
073800020117     C* ==
073900070604     C                   MOVE      VHBRD2        VABRD2
074000971021     C                   MOVE      VHBIND        VABIND
074100971021     C                   MOVE      VHBCAD        VABCAD
074200971021     C                   MOVE      VHBLOD        VABLOD
074300971021     C                   MOVE      VHBPRD        VABPRD
074400971021     C                   MOVE      VHBNZD        VABNZD
074500971021     C                   MOVE      VHBGC1        VABGC1
074600971021     C                   MOVE      VHBGC2        VABGC2
074700971021     C                   MOVE      VHBTSP        VABTSP
074800971021     C                   MOVE      VHBVAS        VABVAS
074900971021     C                   MOVE      VHBNAS        VABNAS
075000971021     C                   MOVE      VHBTIC        VABTIC
075100971021     C                   MOVE      VHBVCA        VABVCA
075200971021     C                   MOVE      VHBGCA        VABGCA
075300971021     C                   MOVE      VHBRMA        VABRMA
075400971021     C                   MOVE      VHBXCO        VABXCO
075500971021     C                   MOVE      VHBNOT        VABNOT
075600971021     C                   MOVE      VHBNT2        VABNT2
075700971021     C                   MOVE      VHBCTM        VABCTM
075800971021     C                   MOVE      VHBFFD        VABFFD
075900971021     C                   MOVE      VHBTCR        VABTCR
076000971021     C                   MOVE      VHBCTS        VABCTS
076100971021     C                   MOVE      VHBFTM        VABFTM
076200971021     C                   MOVE      VHBVAD        VABVAD
076300971021     C                   MOVE      VHBGMA        VABGMA
076400971021     C                   MOVE      VHBGGA        VABGGA
076500971021     C                   MOVE      VHBGVA        VABGVA
076600971021     C                   MOVE      VHBTC1        VABTC1
076700971021     C                   MOVE      VHBTC2        VABTC2
076800971021     C                   MOVE      VHBSCL        VABSCL
076900971021     C                   MOVE      VHBRMO        VABRMO
077000971021     C                   MOVE      VHBCMO        VABCMO
077100971021     C                   MOVE      VHBNMO        VABNMO
077200971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
077300971021     C* NELLA NAZIONE ORIGINALE
077400971021     C                   MOVE      VHBNMO        WA1D              1
077500971021     C                   MOVEL     VHBNMO        WA1S              1
077600971021     C     WA1D          IFNE      *BLANK
077700971021     C     WA1S          ANDEQ     *BLANK
077800971021     C                   MOVE      *BLANK        VABNMO
077900971021     C                   END
078000050418     C*
078100050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
078200050418     C                   IF        %trim(VABNZD) = 'I'   OR
078300050418     C                             %trim(VABNZD) = 'IT'  OR
078400050418     C                             %trim(VABNZD) = 'ITA'
078500050418     C                   EVAL      VABNZD = *blanks
078600050418     C                   ENDIF
078700041025     C*
078800041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
078900041025     C                   IF        *IN52 = *ON
079000041025     C* Reperisco la provincia dal CAP e dalla località
079100041025     C                   IF        VABCAD <> *blanks AND
079200041025     C                             VABPRD  = *blanks
079300041025     C                   CLEAR                   TISI95DS
079400041025     C                   EVAL      I95TCN = '3'
079500041025     C                   Z-ADD     datcor        I95DAT
079600041025     C                   EVAL      I95CAP = VABCAD
079700041025     C                   EVAL      I95LOC = VABLOD
079800041025     C                   CALL      'TISI95R'
079900041025     C                   PARM                    TISI95DS
080000041025     C                   EVAL      VABPRD = O95PRV
080100041025     C                   ENDIF
080200041025     C                   ENDIF
080300040630     C*
080400040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
080500040630     C                   exsr      exeBon
080600930409     C*
080700050722     C  N31              ADD       1             §CTROKVB
080800050722     C   32              ADD       1             §CTRMO
080900050722     C   31              ADD       1             §CTRNO
081000020913     C  N31              WRITE     FIVAB000
081100050722     C  N31              exsr      wrivat
081200990910     C*
081300010601     C                   if        *in31 = *off and
081400010601     C                             *in32 = *off
081500990910     C                   eval      vinflg = '1'
081600990910     C                   else
081700000621     C                   eval      recko = vindta
081800010612     C                   if        %open(tis7t5ps)
081900030407     C   50              except    rigadett
082000010612     C                   endif
082100990910     C                   eval      vinflg = '2'
082200990910     C                   endif
082300991022     C                   endif
082400990910     C                   endif
082500010601     C*
082600010601     C                   endif
082700000905     C*
082800000905     C                   else
082900000905     C                   eval      vinflg = '1'
083000000905     C                   endif
083100000905     C*
083200000905     C  N70              update    tivin000
083300991022     C*
083400991022     C  N70              ENDdo
083500050722     C*
083600050722     C                   endif                                                  (endif - chkcall)
083700010530     C*
083800010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
083900010530     C                   if        cntNonEl = *zeros or
084000010530     C                             flgMulti = '0'
084100990910     C* Se non ci sono record con errori ...
084200010530     C                   if        §ctrno = 0 and
084300010601     C                             §ctrmo = 0 and
084400010530     C                             flgStato <> '2'
084500990910     C* ... restituisco esito OK.
084600990921     C                   eval      wrkesito = '0'
084700990910     C                   else
084800050722     C                   if        §ctrokvb > 0
084900990921     C                   eval      wrkesito = '1'
085000000710     C                   else
085100010615     C                   if        flgOk = '0'
085200010615     C                   eval      wrkesito = '2'
085300010615     C                   else
085400010615     C                   eval      wrkesito = '6'
085500010615     C                   endif
085600990910     C                   endif
085700000710     C                   endif
085800010530     C                   else
085900010530     C                   eval      wrkesito = '9'
086000010530     C                   endif
086100990910     C*
086200990914     C                   if        %open(tivin00r)
086300990908     C                   close     tivin00r
086400990914     C                   endif
086500020913     C                   if        %open(fivabwwr)
086600020913     C                   close     fivabwwr
086700990914     C                   endif
086800050722     C                   if        %open(fivatwwr)
086900050722     C                   close     fivatwwr
087000050722     C                   endif
087100010530     C*
087200010530     C                   if        vlrpoi <> 999
087300010530     C                   eval      invfil = vlrpoi
087400010530     C                   endif
087500990910     C*
087600050722     C                   if        §ctrokvb > 0
087700010530     C                             and invfil > *zeros
087800000613     C                   exsr      invio
087900990920     C                   endif
088000010612     C*
088100010612     C                   if        flgGiro = '1'
088200010612     C                   exsr      endela
088300010612     C                   endif
088400990920     C*
088500910830     C                   ENDSR
088600000613     C***
088700050722
088800050722
088900050722
089000050722     C*----------------------------------------------------*
089100050722     C*  SCARICAMENTO BUFFER RECORDS VAT
089200050722     C*----------------------------------------------------*
089300050722     C     WRIVAT        BEGSR
089400050722     C*
089500050722     C* Attribuisco la stessa chiave della testata bolle
089600050722     C                   EVAL      VATAAS = VABAAS
089700050722     C                   EVAL      VATLNP = VABLNP
089800050722     C                   EVAL      VATNRS = VABNRS
089900050722     C                   EVAL      VATNSP = VABNSP
090000050722     C                   EVAL      VATCCM = VABCCM
090100061023     C*
090200061023     C* Imposto il tipo record sul file VAT a 'A' x indicare il referente consegna
090300061023     C                   EVAL      VATTRC = 'A'
090400061023     C                   EVAL      VATNOT = '.'
090500061023     C*
090600061023     C* Scarico il buffer del FNVAT
090700061023     C                   WRITE     FIVAT000
090800061023     C                   ADD       1             §CTROKVT
090900050722     C*
091000050722     C* Imposto il tipo record sul file VAT a 'B' x indicare il telefono destinatario
091100050722     C                   EVAL      VATTRC = 'B'
091200061023     C                   EVAL      VATNOT = %trim(VABNOT)
091300050722     C*
091400050722     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
091500061023     C                   IF        VATNOT = *blanks
091600061023     C                   EVAL      VATNOT = '.'
091700050722     C                   ENDIF
091800061023     C*
091900061023     C* Scarico il buffer del FNVAT
092000061023     C                   WRITE     FIVAT000
092100061023     C                   ADD       1             §CTROKVT
092200050722     C*
092300050722     C                   ENDSR
092400050722
092500050722
092600050722
092700050722     C*----------------------------------------------------*
092800050722     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
092900050722     C*----------------------------------------------------*
093000050722     C     PREVAT        BEGSR
093100050722     C*
093200050722     C* Compongo il nome del membro da dare al FIVATWWR
093300050722     C                   eval      parmbr = vlrhdl
093400050722     C                   movel     'M'           parmbr
093500050722     C                   eval      parccm = vlrksc
093600050722     C                   eval      paropz = '1'
093700050722     C* Effettuo la chiamata al CLLE preposto
093800050722     C                   call(e)   'TITVVTC'
093900050722     C                   parm                    parccm
094000050722     C                   parm                    parmbr
094100050722     C                   parm                    paropz
094200050722     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
094300050722     C                   if        %error
094400050722     C                   movel     '1'           chkcall
094500050722     C                   else
094600050722     C                   movel     '0'           chkcall
094700050722     C                   endif
094800050722     C*
094900050722     C                   ENDSR
095000990920
095100010530
095200010530
095300010530
095400010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
095500010530     C     repfil        BEGSR
095600010530     C*
095700010530     C                   if        invfil = *zeros and
095800010530     C                             depfil > *zeros and
095900010613     C                             (vinflg = *blanks or
096000010613     C                              vinflg = *zeros)
096100010530     C
096200010530     C                   eval      invfil = depfil
096300010530     C                   endif
096400010530     C*
096500010530     C                   if        depfil <> invfil and
096600010530     C                             invfil > *zeros
096700010530     C                   eval      flgMulti = '1'
096800010530     C                   if        vinflg = *blanks
096900010530     C                   add       1             cntNonEl
097000010530     C                   endif
097100010530     C                   endif
097200010530     C*
097300010530     C                   if        vinflg = '2'
097400010530     C                   eval      flgStato = '2'
097500010530     C                   endif
097600010530     C*
097700010530     C                   ENDSR
097800010530     C***
097900010530
098000010530
098100010530
098200010530
098300990920      /TITLE Invio dei dati al punto operativo.
098400000613     C     invio         BEGSR
098500050722     C*
098600050722     C* 1° invio FIVAT
098700050722     C                   reset                   dscmz
098800050722     C                   move      vlrpoi        cmzdst
098900050722     C                   eval      cmzfld = 'FIVATWWR'
099000050722     C                   eval      cmzmbd = vlrhdl
099100050722     C                   eval      %subst(cmzmbd:1:1) = 'M'
099200050722     C***                if        prmfir = *blanks
099300050722     C                   eval      cmzfla = 'FIVAT00F'
099400050722     C                   eval      cmzmba = 'FIVAT00F'
099500050722     C***                else
099600050722     C***                eval      cmzfla = prmfir
099700050722     C***                eval      cmzmba = prmfir
099800050722     C***                endif
099900050722     C                   eval      cmznrr = *zeros
100000050722     C                   move      §ctrokvt      cmznrr
100100050722     C                   eval      cmzlba = vlrfl1
100200050722     C                   call(e)   'TIS711C'
100300050722     C                   parm                    dscmz
100400050722     C                   parm      *blanks       esito
100500050722     C                   if        %error
100600050722     C                             or cmzerr = '1'
100700050722     C                             or esito  = '1'
100800050722     C                   eval      wrkesito = '3'
100900050722     C                   else
101000050722     C*
101100050722     C* 2° invio FIVAB
101200050722     C                   reset                   dscmz
101300050722     C                   move      vlrpoi        cmzdst
101400050722     C                   eval      cmzfld = vlrfou
101500050722     C                   eval      cmzmbd = vlrhdl
101600050722     C                   eval      %subst(cmzmbd:1:1) = 'M'
101700050722     C***                if        prmfir = *blanks
101800050722     C                   eval      cmzfla = 'FIVAB00F'
101900050722     C                   eval      cmzmba = 'FIVAB00F'
102000050722     C***                else
102100050722     C***                eval      cmzfla = prmfir
102200050722     C***                eval      cmzmba = prmfir
102300050722     C***                endif
102400050722     C                   eval      cmznrr = *zeros
102500050722     C                   move      §ctrokvb      cmznrr
102600050722     C                   eval      cmzlba = vlrfl1
102700050722     C                   call(e)   'TIS711C'
102800050722     C                   parm                    dscmz
102900050722     C                   parm      *blanks       esito
103000050722     C                   if        %error
103100050722     C                             or cmzerr = '1'
103200050722     C                             or esito  = '1'
103300050722     C                   eval      wrkesito = '3'
103400050722     C                   endif
103500050722     C                   endif
103600990920     C*
103700000613     C                   ENDSR
103800000613     C***
103900010530
104000010530
104100010530
104200010530
104300010530      /TITLE Invio dei dati al punto operativo.
104400010530     C     opeini        BEGSR
104500010530     C*
104600010530     C* Inizializzo flag e contatori operativi
104700010530     C                   movel     '0'           flgGiro           1
104800010530     C                   movel     '0'           flgMulti          1
104900010530     C                   movel     '1'           flgStato          1
105000010615     C                   movel     '0'           flgOk             1
105100010530     C                   z-add     *zeros        cntNonEl         10 0
105200010530     C                   z-add     *zeros        depfil            3 0
105300010530     C                   z-add     *zeros        invfil            3 0
105400030407     C*
105500030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
105600030407     C                   if        %subst(vlrppt:1:1) = 'S'
105700030407     C                   seton                                        50
105800030407     C                   else
105900030407     C                   setoff                                       50
106000030407     C                   endif
106100041001     C*
106200041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
106300041001     C                   if        %subst(vlrppt:2:1) = 'S'
106400041001     C                   seton                                        51
106500041001     C                   else
106600041001     C                   setoff                                       51
106700041001     C                   endif
106800041025     C*
106900041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
107000041025     C* (naturalmente solo se nn già presente)
107100041025     C                   if        %subst(vlrppt:3:1) = 'S'
107200041025     C                   seton                                        52
107300041025     C                   else
107400041025     C                   setoff                                       52
107500041025     C                   endif
107600010530     C*
107700010530     C                   ENDSR
107800010530     C***
107900040407
108000040407
108100040407
108200040407
108300040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
108400040407     C     exeBon        BEGSR
108500040407     C*
108600040407     C* Imposto i parametri x il *pgm d "bonifica"
108700040407     C                   EVAL      ElencoChar = *blanks
108800040407     C                   EVAL      TipoElenco = *blanks
108900040407     C                   EVAL      CharSost   = *blanks
109000040629     C                   EVAL      UpperCase  = '1'
109100040407     C                   EVAL      ChkNull    = '1'
109200040407     C                   EVAL      CharNull   = *blanks
109300040629     C*
109400040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
109500040629     C                   EVAL      TxtInOut   = VABRSD
109600040629     C                   EVAL      EsitoBon   = *blanks
109700040407     C*
109800040407     C                   CALL      'XCHKCHAR'
109900040407     C                   PARM                    TxtInOut
110000040407     C                   PARM                    ElencoChar
110100040407     C                   PARM                    TipoElenco
110200040407     C                   PARM                    CharSost
110300040407     C                   PARM                    UpperCase
110400040407     C                   PARM                    ChkNull
110500040407     C                   PARM                    CharNull
110600040407     C                   PARM                    EsitoBon
110700040407     C*
110800040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
110900040407     C                   IF        EsitoBon = '1'
111000040629     C                   EVAL      VABRSD = TxtInOut
111100040407     C                   ENDIF
111200040629     C*
111300040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
111400040629     C                   EVAL      TxtInOut   = VABIND
111500040629     C                   EVAL      EsitoBon   = *blanks
111600040629     C*
111700040629     C                   CALL      'XCHKCHAR'
111800040629     C                   PARM                    TxtInOut
111900040629     C                   PARM                    ElencoChar
112000040629     C                   PARM                    TipoElenco
112100040629     C                   PARM                    CharSost
112200040629     C                   PARM                    UpperCase
112300040629     C                   PARM                    ChkNull
112400040629     C                   PARM                    CharNull
112500040629     C                   PARM                    EsitoBon
112600040629     C*
112700040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
112800040629     C                   IF        EsitoBon = '1'
112900040629     C                   EVAL      VABIND = TxtInOut
113000040629     C                   ENDIF
113100040629     C*
113200040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
113300040629     C                   EVAL      TxtInOut   = VABLOD
113400040629     C                   EVAL      EsitoBon   = *blanks
113500040629     C*
113600040629     C                   CALL      'XCHKCHAR'
113700040629     C                   PARM                    TxtInOut
113800040629     C                   PARM                    ElencoChar
113900040629     C                   PARM                    TipoElenco
114000040629     C                   PARM                    CharSost
114100040629     C                   PARM                    UpperCase
114200040629     C                   PARM                    ChkNull
114300040629     C                   PARM                    CharNull
114400040629     C                   PARM                    EsitoBon
114500040629     C*
114600040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
114700040629     C                   IF        EsitoBon = '1'
114800040629     C                   EVAL      VABLOD = TxtInOut
114900040629     C                   ENDIF
115000040629     C*
115100040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
115200040629     C                   EVAL      TxtInOut   = VABNOT
115300040629     C                   EVAL      EsitoBon   = *blanks
115400040630     C                   EVAL      UpperCase  = *blanks
115500040629     C*
115600040629     C                   CALL      'XCHKCHAR'
115700040629     C                   PARM                    TxtInOut
115800040629     C                   PARM                    ElencoChar
115900040629     C                   PARM                    TipoElenco
116000040629     C                   PARM                    CharSost
116100040629     C                   PARM                    UpperCase
116200040629     C                   PARM                    ChkNull
116300040629     C                   PARM                    CharNull
116400040629     C                   PARM                    EsitoBon
116500040629     C*
116600040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
116700040629     C                   IF        EsitoBon = '1'
116800040629     C                   EVAL      VABNOT = TxtInOut
116900040629     C                   ENDIF
117000040629     C*
117100040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
117200040629     C                   EVAL      TxtInOut   = VABNT2
117300040629     C                   EVAL      EsitoBon   = *blanks
117400040630     C                   EVAL      UpperCase  = *blanks
117500040629     C*
117600040629     C                   CALL      'XCHKCHAR'
117700040629     C                   PARM                    TxtInOut
117800040629     C                   PARM                    ElencoChar
117900040629     C                   PARM                    TipoElenco
118000040629     C                   PARM                    CharSost
118100040629     C                   PARM                    UpperCase
118200040629     C                   PARM                    ChkNull
118300040629     C                   PARM                    CharNull
118400040629     C                   PARM                    EsitoBon
118500040629     C*
118600040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
118700040629     C                   IF        EsitoBon = '1'
118800040629     C                   EVAL      VABNT2 = TxtInOut
118900040629     C                   ENDIF
119000040407     C*
119100040407     C                   ENDSR
119200010530
119300070411
119400070411     C     *pssr         BEGSR
119500070411     C*
119600070411     C                   if        %open(tivin00r)
119700070411     C                   close     tivin00r
119800070411     C                   endif
119900070411     C                   if        %open(fivabwwr)
120000070411     C                   close     fivabwwr
120100070411     C                   endif
120200070411     C                   if        %open(fivatwwr)
120300070411     C                   close     fivatwwr
120400070411     C                   endif
120500070411     C*
120600070411     C* Effettuo la chiamata al CLLE preposto
120700070411     C                   call(e)   'TITVVTC'
120800070411     C                   parm                    parccm
120900070411     C                   parm                    parmbr
121000070411     C                   parm      '2'           paropz
121100070411     C*
121200070411     C                   eval      wrkesito = '2'
121300070411     C*
121400070411     C                   seton                                        LR
121500070411     C*
121600070411     C                   ENDSR     '*CANCL'
121700070411     C***
121800070411
121900010530
122000010530
122100990910
122200000613     C     *inzsr        BEGSR
122300990910     C*
122400990910     C     *entry        plist
122500990920     C                   parm                    tivlrds
122600990921     C                   parm      wrkesito      esito
122700000724     C                   parm                    prmlit
122800000710     C                   parm                    prmfir
122900010606     C*
123000010606     C* CALCOLA LA DATA CORRENTE
123100010606     C                   time                    wn14             14 0
123200010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
123300010606     C                   z-add     wn8           g08dat
123400010606     C                   z-add     *zeros        g08inv
123500010606     C                   movel     '0'           g08err
123600010606     C                   call      'XSRDA8'
123700010606     C                   parm                    wlbda8
123800010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
123900000613     C*
124000000613     C                   ENDSR
124100000613     C***
124200990908
124300010601     Otis7t5p   E            riepilogo         2
124400990915     O                                              'Upload via Internet'
124500990915     O                                           +1 'Traduzione TIVIN00R -'
124600020913     O                                           +1 'FIVABWWR'
124700990915     O                                           +1 'Testate bolle'
124800990915     O          E            riepilogo   2
124900990915     O                       wrkdata
125000990915     O                       wrkora              +1
125100990915     O                       procname            +1
125200990915     O          E            riepilogo   2
125300990915     O                                              'Cliente..................:'
125400990915     O                       VABCCM        z     +1
125500990915     O          E            riepilogo   2
125600990920     O                                              'Riferimento Strategi.....:'
125700990920     O                       vlrhdl              +1
125800990915     O          E            riepilogo   2
125900990915     O                                              'Giusti...................:'
126000050722     O                       §CTROKVB      2   +  1
126100990915     O          E            riepilogo   2
126200990915     O                                              'Sbagliati e corretti.....:'
126300971022     O                       §CTRMO        2   +  1
126400990915     O          E            riepilogo   2
126500990915     O                                              'Sbagliati e scartati.....:'
126600971022     O                       §CTRNO        2   +  1
126700000613
126800010601     Otis7t5ps  E            testdett          2
126900000613     O                                              'Upload via Internet'
127000000613     O                                           +1 'Traduzione TIVIN00R -'
127100020913     O                                           +1 'FIVABWWR'
127200000613     O                                           +1 'Testate bolle'
127300000616     O          E            testdett    3
127400000613     O                                           +2 'N° rec'
127500000613     O                                           +3 'Anteprima contenuto'
127600000616     O          E            rigadett    2
127700000613     O                       rrnum               +2
127800000621     O                       recko               +3
127900000616     O          E            findett     2
128000000613     O                       wrkdata
128100000613     O                       wrkora              +1
128200000613     O                       procname            +1
128300000616     O          E            findett     2
128400000613     O                                              'Cliente..................:'
128500000613     O                       VABCCM        z     +1
128600000616     O          E            findett     2
128700000613     O                                              'Riferimento Strategi.....:'
128800000613     O                       vlrhdl              +1
128900000616     O          E            findett     2
129000000613     O                                              'Giusti...................:'
129100050722     O                       §CTROKVB      2   +  1
129200000616     O          E            findett     2
129300000613     O                                              'Sbagliati e corretti.....:'
129400000613     O                       §CTRMO        2   +  1
129500000616     O          E            findett     2
129600000613     O                                              'Sbagliati e scartati.....:'
129700000613     O                       §CTRNO        2   +  1
129800000512** CMD - COMANDI CL
129900011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T5P)  FORMTYPE(RICCLI) OUTQ(
130000011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T5PS) FORMTYPE(RICCLI) OUTQ(
130100010601DLTOVR FILE(TIS7T5P TIS7T5PS) LVL(*)
130200000512
130300000512
