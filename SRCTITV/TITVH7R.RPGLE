000100050209      /TITLE Upload via Internet: traduzione in FIVABWWR. (Cliente Boomerang - Cod. 0012487)
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600020913     FFIVABwwr  O    E             DISK    usropn
000700050209     FFIVATwwr  O    E             DISK    usropn
000800050209     Ftitvh7p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000070411     F                                     infsr(*pssr)
001100050209     Ftitvh7ps  O    f  198        PRINTER usropn
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
004600990910     D fnvabaar      e ds                  inz
004700990910     D esito           s              1
004800000724     D prmlit          s             10
004900000710     D prmfir          s             10
005000990921     D wrkesito        s                   like(esito)
005100990915     D wrkdata         s               d
005200990915     D wrkora          s               t
005300000613     D rrnum           s              6  0 INZ(*zeros)
005400000621     D recko           s            150    INZ(*blanks)
005500011123     D depcmd          s            150    INZ(*blanks)
005600050209     D parccm          s              8    INZ(*blanks)
005700050209     D parmbr          s             10    INZ(*blanks)
005800050209     D paropz          s              1    INZ(*blanks)
005900050209     D chkcall         s              1    INZ(*blanks)
006000010606     D*------------------
006100010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
006200010606     D*------------------
006300010606     D WLBDA8          DS                  INZ
006400010606     D  G08DAT                 1      8  0
006500010606     D  G08INV                 9     16  0
006600010606     D  G08ERR                17     17
006700010606     D  G08TGI                18     22  0
006800050209     D*------------------
006900050209     D* DS REPERIMENTO NUMERATORE
007000050209     D*------------------
007100050209     D trul33ds      e ds                  inz
007200050209     D*------------------
007300050209     D* DS ARCHITETTURA
007400050209     D*------------------
007500050209     D kpjba         e ds                  inz
007600050209     D*------------------
007700990908
007800050209
007900050209
008000050209     C* /MAIN
008100990915     C                   time                    wrkdata
008200990915     C                   time                    wrkora
008300000913     C                   reset                   rrnum
008400990921     C                   reset                   esito
008500990921     C                   reset                   wrkesito
008600010530     C*
008700010530     C                   exsr      opeini
008800010530     C                   exsr      rwvab
008900050209     C*
009000050209     C* Effettuo la chiamata al CLLE preposto
009100050209     C                   call(e)   'TITVVTC'
009200050209     C                   parm                    parccm
009300050209     C                   parm                    parmbr
009400050209     C                   parm      '2'           paropz
009500010530     C*
009600010530     C                   seton                                        lr
009700010530
009800010530
009900010530
010000010530
010100010530     C*--------------------------------------------------------
010200010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
010300010530     C*--------------------------------------------------------
010400010530     C     PREELA        BEGSR
010500000724     C*
010600000724     C* SE OCCORRE SPEDIRE IN FILIALE
010700010530     C                   if        invfil <> *zeros and
010800010530     C                             flgGiro = '0'
010900010530     C*
011000010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
011100010530     C                   eval      flgGiro = '1'
011200000724     C*
011300000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011400000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011500010530     C     invfil        chain     azorg01l
011600000724     C                   if        %found
011700000616     C                   movel(p)  CMD(1)        depcmd
011800020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011900000616     C*
012000000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
012100011123     C                   Z-ADD     150           LENGH            15 5
012200000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012300000616     C                   PARM                    depcmd
012400000616     C                   PARM                    LENGH
012500000724     C*
012600000724     C                   endif
012700000616     C*
012800000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012900000616     C                   movel(p)  CMD(2)        depcmd
013000000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
013100000616     C*
013200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
013300011123     C                   Z-ADD     150           LENGH            15 5
013400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013500000616     C                   PARM                    depcmd
013600000616     C                   PARM                    LENGH
013700000616     C*
013800050209     C                   if        not %open(titvh7ps)
013900050209     C                   open      titvh7ps
014000030407     C   50              except    testdett
014100000613     C                   endif
014200010530     C*
014300010530     C                   endif
014400010530     C*
014500010530     C                   ENDSR
014600010530     C***
014700010530
014800010530
014900010530
015000010530
015100010530
015200010530
015300010530     C*--------------------------------------------------------
015400010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015500010530     C*--------------------------------------------------------
015600010530     C     ENDELA        BEGSR
015700010530     C*
015800010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015900000613     C*
016000050209     C                   if        %open(titvh7ps)
016100030407     C   50              except    findett
016200050209     C                   close     titvh7ps
016300000613     C                   endif
016400000616     C*
016500000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016600011123     C                   Z-ADD     150           LENGH            15 5
016700010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
016800000616     C                   PARM                    CMD(3)
016900000616     C                   PARM                    LENGH
017000010530     C*
017100010530     C                   ENDSR
017200010530     C***
017300000613
017400010530
017500010530
017600000613
017700000613     C*--------------------------------------------------------
017800000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
017900000613     C*--------------------------------------------------------
018000000621     C     STPR          BEGSR
018100000613     C*
018200050209     C                   if        not %open(titvh7p)
018300050209     C                   open      titvh7p
018400990915     C                   endif
018500990915     C*
018600030407     C   50              except    riepilogo
018700990915     C*
018800050209     C                   if        %open(titvh7p)
018900050209     C                   close     titvh7p
019000990914     C                   endif
019100990910     C*
019200000613     C                   ENDSR
019300000613     C***
019400990908
019500010530
019600010530
019700010530
019800910830     C*--------------------------------------------------------
019900020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
020000910830     C*--------------------------------------------------------
020100941229     C     RWVAB         BEGSR
020200990910     C*
020300990914     C                   if        not %open(tivin00r)
020400990908     C                   open      tivin00r
020500990914     C                   endif
020600020913     C                   if        not %open(fivabwwr)
020700020913     C                   open      fivabwwr
020800990914     C                   endif
020900050209     C*
021000050209     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021100050209     C                   exsr      prevat
021200050209     C*
021300050209     C                   if        chkcall = '0'
021400050209     C*
021500050209     C                   if        not %open(fivatwwr)
021600050209     C                   open      fivatwwr
021700050209     C                   endif
021800050209     C*
021900990910     C                   clear                   §CTROK
022000990910     C                   clear                   §CTRMO
022100990910     C                   clear                   §CTRNO
022200990910     C*
022300921023     C                   DO        *HIVAL
022400990913     C*
022500990915     C                   READ      tivin00r                               70
022600010618     C*
022700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
022800010618     C                   if        vinflg = '1'
022900010618     C                   eval      flgOk = '1'
023000010618     C                   endif
023100010705     C*
023200010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
023300010705     C                   if        %subst(vindta:1:1) = '='
023400010705     C                   eval      %subst(vindta:1:1) = *blanks
023500010705     C                   endif
023600010705     C*
023700000905     C                   if        vindta > *blanks
023800000613     C                   add       1             rrnum
023900010601     C* ...elimino eventuali segni
024000010601     C     '+':' '       xlate     vindta        vindta
024100990913     C*
024200010601     C                   if        *in70 = *off and
024300990913     C                             (vinflg = *blanks
024400010711     C                              or vinflg = '0'
024500010711     C                              or vinflg = '2')
024600990913     C*
024700020913     C                   clear                   FIVAB000
024800050209     C                   clear                   FIVAT000
024900010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025000010711     C                   if        vinflg = *blanks or vinflg = '0'
025100010711     C                   clear                   vinmsg
025200010711     C                   endif
025300010711     C*
025400990913     C                   eval      fnvabaar = vindta
025500991022     C*
025600991022     C                   testb     x'3F'         vhbatb                   02
025700991022     C                   if        not *in02
025800991022     C*
025900971021     C                   SETOFF                                       303132
026000971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
026100971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
026200971023     C*    OPPURE TUTTI BLANKS
026300010530     C*
026400010530     C                   TESTN                   VHBLNP               30
026500010530     C  N30              TESTN                   VHBLNP                 30
026600010530     C  N30              TESTN                   VHBLNP                   30
026700010530     C                   MOVE      VHBLNP        W001A             1
026800010530     C     *IN30         IFEQ      *OFF
026900010530     C     W001A         ORLT      '0'
027000010530     C                   SETON                                        31
027100010530     C                   eval      vinmsg = %trimr(vinmsg)
027200010530     C                             + ' ' + 'VHBLNP'
027300010530     C                   ELSE
027400010530     C                   MOVE      VHBLNP        VABLNP
027500010530     C     VABLNP        IFLE      *ZEROS
027600010530     C                   SETON                                        31
027700010530     C                   eval      vinmsg = %trimr(vinmsg)
027800010530     C                             + ' ' + 'VHBLNP'
027900010530     C                   ENDIF
028000010530     C                   ENDIF
028100010530     C*
028200010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028300010530     C                   eval      depfil = VABLNP
028400010530     C                   exsr      repfil
028500010601     C                   if        depfil = invfil
028600021014     C                   if        vlrpoi = 999
028700021010     C                   MOVE(P)   invfil        VABFGS
028800021014     C                   else
028900021014     C                   MOVE(P)   vlrpoi        VABFGS
029000021014     C                   endif
029100010530     C*
029200010530     C                   exsr      PREELA
029300010530     C*
029400971023     C                   TESTN                   VHBCCM               30
029500971023     C  N30              TESTN                   VHBCCM                 30
029600971023     C  N30              TESTN                   VHBCCM                   30
029700971021     C                   MOVE      VHBCCM        W001A             1
029800971021     C     *IN30         IFEQ      *OFF
029900971021     C     W001A         ORLT      '0'
030000971021     C                   SETON                                        31
030100010604     C                   eval      vinmsg = %trimr(vinmsg)
030200010604     C                             + ' ' + 'VHBCCM'
030300971021     C                   ELSE
030400971021     C                   MOVE      VHBCCM        VABCCM
030500971024     C     VABCCM        IFLE      *ZEROS
030600971024     C                   SETON                                        31
030700010604     C                   eval      vinmsg = %trimr(vinmsg)
030800010604     C                             + ' ' + 'VHBCCM'
030900971024     C                   ENDIF
031000971021     C                   ENDIF
031100971021     C*
031200971023     C                   TESTN                   VHBAAS               30
031300971023     C  N30              TESTN                   VHBAAS                 30
031400971023     C  N30              TESTN                   VHBAAS                   30
031500971021     C                   MOVE      VHBAAS        W001A             1
031600971021     C     *IN30         IFEQ      *OFF
031700971021     C     W001A         ORLT      '0'
031800971023     C     W001A         ANDNE     *BLANK
031900971021     C                   SETON                                        32
032000010606     C                   MOVEL     datcor        VABAAS
032100010604     C                   eval      vinmsg = %trimr(vinmsg)
032200010604     C                             + ' ' + 'VHBAAS'
032300971021     C                   ELSE
032400971021     C                   MOVE      VHBAAS        VABAAS
032500971021     C                   ENDIF
032600971021     C*
032700971023     C                   TESTN                   VHBMGS               30
032800971023     C  N30              TESTN                   VHBMGS                 30
032900971023     C  N30              TESTN                   VHBMGS                   30
033000971021     C                   MOVE      VHBMGS        W001A             1
033100971021     C     *IN30         IFEQ      *OFF
033200971021     C     W001A         ORLT      '0'
033300971023     C     W001A         ANDNE     *BLANK
033400971021     C                   SETON                                        32
033500010601     C                   MOVE      '0101'        VABMGS
033600010604     C                   eval      vinmsg = %trimr(vinmsg)
033700010604     C                             + ' ' + 'VHBMGS'
033800971021     C                   ELSE
033900971021     C                   MOVE      VHBMGS        VABMGS
034000971021     C                   ENDIF
034100971021     C*
034200971023     C                   TESTN                   VHBNRS               30
034300971023     C  N30              TESTN                   VHBNRS                 30
034400971023     C  N30              TESTN                   VHBNRS                   30
034500971021     C                   MOVE      VHBNRS        W001A             1
034600971021     C     *IN30         IFEQ      *OFF
034700971021     C     W001A         ORLT      '0'
034800971023     C     W001A         ANDNE     *BLANK
034900971021     C                   SETON                                        32
035000971021     C                   MOVE      *ZERO         VABNRS
035100010604     C                   eval      vinmsg = %trimr(vinmsg)
035200010604     C                             + ' ' + 'VHBNRS'
035300971021     C                   ELSE
035400971021     C                   MOVE      VHBNRS        VABNRS
035500971021     C                   ENDIF
035600971021     C*
035700971023     C                   TESTN                   VHBNSP               30
035800971023     C  N30              TESTN                   VHBNSP                 30
035900971023     C  N30              TESTN                   VHBNSP                   30
036000971021     C                   MOVE      VHBNSP        W001A             1
036100971021     C     *IN30         IFEQ      *OFF
036200971021     C     W001A         ORLT      '0'
036300971023     C     W001A         ANDNE     *BLANK
036400971021     C                   SETON                                        32
036500971021     C                   MOVE      *ZERO         VABNSP
036600010604     C                   eval      vinmsg = %trimr(vinmsg)
036700010604     C                             + ' ' + 'VHBNSP'
036800971021     C                   ELSE
036900971021     C                   MOVE      VHBNSP        VABNSP
037000971021     C                   ENDIF
037100971021     C*
037200971023     C                   TESTN                   VHBLNA               30
037300971023     C  N30              TESTN                   VHBLNA                 30
037400971023     C  N30              TESTN                   VHBLNA                   30
037500971021     C                   MOVE      VHBLNA        W001A             1
037600971021     C     *IN30         IFEQ      *OFF
037700971021     C     W001A         ORLT      '0'
037800971023     C     W001A         ANDNE     *BLANK
037900971021     C                   SETON                                        32
038000971021     C                   MOVE      *ZERO         VABLNA
038100010604     C                   eval      vinmsg = %trimr(vinmsg)
038200010604     C                             + ' ' + 'VHBLNA'
038300971021     C                   ELSE
038400971021     C                   MOVE      VHBLNA        VABLNA
038500971021     C                   ENDIF
038600971021     C*
038700971023     C                   TESTN                   VHBCTR               30
038800971023     C  N30              TESTN                   VHBCTR                 30
038900971023     C  N30              TESTN                   VHBCTR                   30
039000971021     C                   MOVE      VHBCTR        W001A             1
039100971021     C     *IN30         IFEQ      *OFF
039200971021     C     W001A         ORLT      '0'
039300971023     C     W001A         ANDNE     *BLANK
039400971021     C                   SETON                                        32
039500971021     C                   MOVE      *ZERO         VABCTR
039600010604     C                   eval      vinmsg = %trimr(vinmsg)
039700010604     C                             + ' ' + 'VHBCTR'
039800971021     C                   ELSE
039900971021     C                   MOVE      VHBCTR        VABCTR
040000971021     C                   ENDIF
040100941229     C*
040200971021     C                   MOVE      *ZERO         VABIAS
040300941229     C                   MOVE      VHBIAS        WASC15
040400971023     C                   TESTN                   WI15                 30
040500971023     C  N30              TESTN                   WI15                   30
040600971023     C  N30              TESTN                   WI15                     30
040700971021     C                   MOVE      WI15          W001A             1
040800971021     C     *IN30         IFEQ      *OFF
040900971021     C     W001A         ORLT      '0'
041000971023     C     W001A         ANDNE     *BLANK
041100971021     C                   SETON                                        32
041200971021     C                   MOVE      *ZERO         VABIAS
041300010604     C                   eval      vinmsg = %trimr(vinmsg)
041400010604     C                             + ' ' + 'VHBIAS'
041500971021     C                   ELSE
041600971021     C                   MOVEL     WI15          VABIAS
041700971023     C                   TESTN                   WD15                 30
041800971023     C  N30              TESTN                   WD15                   30
041900971023     C  N30              TESTN                   WD15                     30
042000971021     C                   MOVE      WD15          W001A             1
042100971021     C     *IN30         IFEQ      *OFF
042200971021     C     W001A         ORLT      '0'
042300971023     C     W001A         ANDNE     *BLANK
042400971021     C                   SETON                                        32
042500971021     C                   MOVE      *ZERO         VABIAS
042600010604     C                   eval      vinmsg = %trimr(vinmsg)
042700010604     C                             + ' ' + 'VHBIAS'
042800971021     C                   ELSE
042900971021     C                   MOVE      WD15          VABIAS
043000971021     C                   ENDIF
043100050118     C                   ENDIF
043200941229     C*
043300971023     C                   TESTN                   VHBNCL               30
043400971023     C  N30              TESTN                   VHBNCL                 30
043500971023     C  N30              TESTN                   VHBNCL                   30
043600971021     C                   MOVE      VHBNCL        W001A             1
043700971021     C     *IN30         IFEQ      *OFF
043800971021     C     W001A         ORLT      '0'
043900971021     C                   SETON                                        32
044000971021     C                   MOVE      *ZERO         VABNCL
044100010604     C                   eval      vinmsg = %trimr(vinmsg)
044200010604     C                             + ' ' + 'VHBNCL'
044300971021     C                   ELSE
044400971021     C                   MOVE      VHBNCL        VABNCL
044500971021     C                   ENDIF
044600941229     C*
044700971021     C                   MOVE      *ZERO         VABPKB
044800971021     C                   MOVE      VHBPKB        WASC09
044900010719     C                   IF        WI09 = *blanks
045000010719     C                   EVAL      WI09 = *zeros
045100010719     C                   ENDIF
045200971023     C                   TESTN                   WI09                 30
045300971023     C  N30              TESTN                   WI09                   30
045400971023     C  N30              TESTN                   WI09                     30
045500971021     C                   MOVE      WI09          W001A             1
045600971021     C     *IN30         IFEQ      *OFF
045700971021     C     W001A         ORLT      '0'
045800971021     C                   SETON                                        32
045900971021     C                   MOVE      *ZERO         VABPKB
046000010604     C                   eval      vinmsg = %trimr(vinmsg)
046100010604     C                             + ' ' + 'VHBPKB'
046200971021     C                   ELSE
046300971021     C                   MOVEL     WI09          VABPKB
046400010719     C                   IF        WD09 = *blanks
046500010719     C                   EVAL      WD09 = *zeros
046600010719     C                   ENDIF
046700971023     C                   TESTN                   WD09                 30
046800971023     C  N30              TESTN                   WD09                     30
046900971021     C                   MOVE      WD09          W001A             1
047000971021     C     *IN30         IFEQ      *OFF
047100971021     C     W001A         ORLT      '0'
047200971023     C     W001A         ANDNE     *BLANK
047300971021     C                   SETON                                        32
047400971021     C                   MOVE      *ZERO         VABPKB
047500010604     C                   eval      vinmsg = %trimr(vinmsg)
047600010604     C                             + ' ' + 'VHBPKB'
047700971021     C                   ELSE
047800971021     C                   MOVE      WD09          VABPKB
047900971021     C                   ENDIF
048000050118     C                   ENDIF
048100971021     C*
048200041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
048300041001     C                   IF        *IN51 = *OFF
048400971021     C                   MOVE      *ZERO         VABVLB
048500971021     C                   MOVE      VHBVLB        WASC07
048600010719     C                   IF        WI07 = *blanks
048700010719     C                   EVAL      WI07 = *zeros
048800010719     C                   ENDIF
048900971023     C                   TESTN                   WI07                 30
049000971023     C  N30              TESTN                   WI07                   30
049100971023     C  N30              TESTN                   WI07                     30
049200971021     C                   MOVE      WI07          W001A             1
049300971021     C     *IN30         IFEQ      *OFF
049400971021     C     W001A         ORLT      '0'
049500971023     C     W001A         ANDNE     *BLANK
049600971021     C                   SETON                                        32
049700971021     C                   MOVE      *ZERO         VABVLB
049800010604     C                   eval      vinmsg = %trimr(vinmsg)
049900010604     C                             + ' ' + 'VHBVLB'
050000971021     C                   ELSE
050100971021     C                   MOVEL     WI07          VABVLB
050200010719     C                   IF        WD07 = *blanks
050300010719     C                   EVAL      WD07 = *zeros
050400010719     C                   ENDIF
050500971023     C                   TESTN                   WD07                 30
050600971023     C  N30              TESTN                   WD07                   30
050700971023     C  N30              TESTN                   WD07                     30
050800971021     C                   MOVE      WD07          W001A             1
050900971021     C     *IN30         IFEQ      *OFF
051000971021     C     W001A         ORLT      '0'
051100971023     C     W001A         ANDNE     *BLANK
051200971021     C                   SETON                                        32
051300971021     C                   MOVE      *ZERO         VABVLB
051400010604     C                   eval      vinmsg = %trimr(vinmsg)
051500010604     C                             + ' ' + 'VHBVLB'
051600971021     C                   ELSE
051700971021     C                   MOVE      WD07          VABVLB
051800971021     C                   ENDIF
051900050118     C                   ENDIF
052000041001     C                   ELSE
052100041001     C* ...altrimenti forzo azzeramento del volume
052200041001     C                   Z-ADD     *zeros        VABVLB
052300041001     C                   ENDIF
052400941229     C*
052500971021     C                   MOVE      *ZERO         VABQFT
052600971021     C                   MOVE      VHBQFT        WASC15
052700010719     C                   IF        WI15 = *blanks
052800010719     C                   EVAL      WI15 = *zeros
052900010719     C                   ENDIF
053000971023     C                   TESTN                   WI15                 30
053100971023     C  N30              TESTN                   WI15                   30
053200971023     C  N30              TESTN                   WI15                     30
053300971021     C                   MOVE      WI15          W001A             1
053400971021     C     *IN30         IFEQ      *OFF
053500971021     C     W001A         ORLT      '0'
053600971023     C     W001A         ANDNE     *BLANK
053700971021     C                   SETON                                        32
053800971021     C                   MOVE      *ZERO         VABQFT
053900010604     C                   eval      vinmsg = %trimr(vinmsg)
054000010604     C                             + ' ' + 'VHBQFT'
054100971021     C                   ELSE
054200971021     C                   MOVEL     WI15          VABQFT
054300010719     C                   IF        WD15 = *blanks
054400010719     C                   EVAL      WD15 = *zeros
054500010719     C                   ENDIF
054600971023     C                   TESTN                   WD15                 30
054700971023     C  N30              TESTN                   WD15                   30
054800971023     C  N30              TESTN                   WD15                     30
054900971021     C                   MOVE      WD15          W001A             1
055000971021     C     *IN30         IFEQ      *OFF
055100971021     C     W001A         ORLT      '0'
055200971023     C     W001A         ANDNE     *BLANK
055300971021     C                   SETON                                        32
055400971021     C                   MOVE      *ZERO         VABQFT
055500010604     C                   eval      vinmsg = %trimr(vinmsg)
055600010604     C                             + ' ' + 'VHBQFT'
055700971021     C                   ELSE
055800971021     C                   MOVE      WD15          VABQFT
055900971021     C                   ENDIF
056000050118     C                   ENDIF
056100941229     C*
056200971021     C                   MOVE      *ZERO         VABCAS
056300971021     C                   MOVE      VHBCAS        WASC15
056400010719     C                   IF        WI15 = *blanks
056500010719     C                   EVAL      WI15 = *zeros
056600010719     C                   ENDIF
056700971023     C                   TESTN                   WI15                 30
056800971023     C  N30              TESTN                   WI15                   30
056900971023     C  N30              TESTN                   WI15                     30
057000971021     C                   MOVE      WI15          W001A             1
057100971021     C     *IN30         IFEQ      *OFF
057200971021     C     W001A         ORLT      '0'
057300971023     C     W001A         ANDNE     *BLANK
057400971021     C                   SETON                                        32
057500971021     C                   MOVE      *ZERO         VABCAS
057600010604     C                   eval      vinmsg = %trimr(vinmsg)
057700010604     C                             + ' ' + 'VHBCAS'
057800971021     C                   ELSE
057900971021     C                   MOVEL     WI15          VABCAS
058000010719     C                   IF        WD15 = *blanks
058100010719     C                   EVAL      WD15 = *zeros
058200010719     C                   ENDIF
058300971023     C                   TESTN                   WD15                 30
058400971023     C  N30              TESTN                   WD15                   30
058500971023     C  N30              TESTN                   WD15                     30
058600971021     C                   MOVE      WD15          W001A             1
058700971021     C     *IN30         IFEQ      *OFF
058800971021     C     W001A         ORLT      '0'
058900971023     C     W001A         ANDNE     *BLANK
059000971021     C                   SETON                                        32
059100971021     C                   MOVE      *ZERO         VABCAS
059200010604     C                   eval      vinmsg = %trimr(vinmsg)
059300010604     C                             + ' ' + 'VHBCAS'
059400971021     C                   ELSE
059500971021     C                   MOVE      WD15          VABCAS
059600971021     C                   ENDIF
059700040827     C                   ENDIF
059800971021     C*
059900971023     C                   TESTN                   VHBRMN               30
060000971023     C  N30              TESTN                   VHBRMN                 30
060100971023     C  N30              TESTN                   VHBRMN                   30
060200971021     C                   MOVE      VHBRMN        W001A             1
060300971021     C     *IN30         IFEQ      *OFF
060400971021     C     W001A         ORLT      '0'
060500971021     C                   SETON                                        32
060600971021     C                   MOVE      *ZERO         VABRMN
060700010604     C                   eval      vinmsg = %trimr(vinmsg)
060800010604     C                             + ' ' + 'VHBRMN'
060900971021     C                   ELSE
061000971021     C                   MOVE      VHBRMN        VABRMN
061100971021     C                   ENDIF
061200971021     C*
061300971023     C                   TESTN                   VHBNCD               30
061400971023     C  N30              TESTN                   VHBNCD                 30
061500971023     C  N30              TESTN                   VHBNCD                   30
061600971021     C                   MOVE      VHBNCD        W001A             1
061700971021     C     *IN30         IFEQ      *OFF
061800971021     C     W001A         ORLT      '0'
061900971023     C     W001A         ANDNE     *BLANK
062000971021     C                   SETON                                        32
062100971021     C                   MOVE      *ZERO         VABNCD
062200010604     C                   eval      vinmsg = %trimr(vinmsg)
062300010604     C                             + ' ' + 'VHBNCD'
062400971021     C                   ELSE
062500971021     C                   MOVE      VHBNCD        VABNCD
062600971021     C                   ENDIF
062700971021     C*
062800971023     C                   TESTN                   VHBNCA               30
062900971023     C  N30              TESTN                   VHBNCA                 30
063000971023     C  N30              TESTN                   VHBNCA                   30
063100971021     C                   MOVE      VHBNCA        W001A             1
063200971021     C     *IN30         IFEQ      *OFF
063300971021     C     W001A         ORLT      '0'
063400971023     C     W001A         ANDNE     *BLANK
063500971021     C                   SETON                                        32
063600971021     C                   MOVE      *ZERO         VABNCA
063700010604     C                   eval      vinmsg = %trimr(vinmsg)
063800010604     C                             + ' ' + 'VHBNCA'
063900971021     C                   ELSE
064000971021     C                   MOVE      VHBNCA        VABNCA
064100971021     C                   ENDIF
064200971021     C*
064300971023     C                   TESTN                   VHBZNC               30
064400971023     C  N30              TESTN                   VHBZNC                 30
064500971023     C  N30              TESTN                   VHBZNC                   30
064600971021     C                   MOVE      VHBZNC        W001A             1
064700971021     C     *IN30         IFEQ      *OFF
064800971021     C     W001A         ORLT      '0'
064900971023     C     W001A         ANDNE     *BLANK
065000971021     C                   SETON                                        32
065100971021     C                   MOVE      *ZERO         VABZNC
065200010604     C                   eval      vinmsg = %trimr(vinmsg)
065300010604     C                             + ' ' + 'VHBZNC'
065400971021     C                   ELSE
065500971021     C                   MOVE      VHBZNC        VABZNC
065600971021     C                   ENDIF
065700971021     C*
065800971023     C                   TESTN                   VHBDCR               30
065900971023     C  N30              TESTN                   VHBDCR                 30
066000971023     C  N30              TESTN                   VHBDCR                   30
066100971021     C                   MOVE      VHBDCR        W001A             1
066200971021     C     *IN30         IFEQ      *OFF
066300971021     C     W001A         ORLT      '0'
066400971023     C     W001A         ANDNE     *BLANK
066500971021     C                   SETON                                        32
066600971021     C                   MOVE      *ZERO         VABDCR
066700010604     C                   eval      vinmsg = %trimr(vinmsg)
066800010604     C                             + ' ' + 'VHBDCR'
066900971021     C                   ELSE
067000971021     C                   MOVE      VHBDCR        VABDCR
067100971021     C                   ENDIF
067200971021     C*
067300971023     C                   TESTN                   VHBHCR               30
067400971023     C  N30              TESTN                   VHBHCR                 30
067500971023     C  N30              TESTN                   VHBHCR                   30
067600971021     C                   MOVE      VHBHCR        W001A             1
067700971021     C     *IN30         IFEQ      *OFF
067800971021     C     W001A         ORLT      '0'
067900971023     C     W001A         ANDNE     *BLANK
068000971021     C                   SETON                                        32
068100971021     C                   MOVE      *ZERO         VABHCR
068200010604     C                   eval      vinmsg = %trimr(vinmsg)
068300010604     C                             + ' ' + 'VHBHCR'
068400971021     C                   ELSE
068500971021     C                   MOVE      VHBHCR        VABHCR
068600971021     C                   ENDIF
068700941229     C*
068800971021     C                   MOVE      *ZERO         VABVMD
068900971021     C                   MOVE      VHBVMD        WASC15
069000010719     C                   IF        WI15 = *blanks
069100010719     C                   EVAL      WI15 = *zeros
069200010719     C                   ENDIF
069300971023     C                   TESTN                   WI15                 30
069400971023     C  N30              TESTN                   WI15                   30
069500971023     C  N30              TESTN                   WI15                     30
069600971021     C                   MOVE      WI15          W001A             1
069700971021     C     *IN30         IFEQ      *OFF
069800971021     C     W001A         ORLT      '0'
069900971023     C     W001A         ANDNE     *BLANK
070000971021     C                   SETON                                        32
070100971021     C                   MOVE      *ZERO         VABVMD
070200010604     C                   eval      vinmsg = %trimr(vinmsg)
070300010604     C                             + ' ' + 'VHBVMD'
070400971021     C                   ELSE
070500971021     C                   MOVEL     WI15          VABVMD
070600010719     C                   IF        WD15 = *blanks
070700010719     C                   EVAL      WD15 = *zeros
070800010719     C                   ENDIF
070900971023     C                   TESTN                   WD15                 30
071000971023     C  N30              TESTN                   WD15                   30
071100971023     C  N30              TESTN                   WD15                     30
071200971021     C                   MOVE      WD15          W001A             1
071300971021     C     *IN30         IFEQ      *OFF
071400971021     C     W001A         ORLT      '0'
071500971023     C     W001A         ANDNE     *BLANK
071600971021     C                   SETON                                        32
071700971021     C                   MOVE      *ZERO         VABVMD
071800010604     C                   eval      vinmsg = %trimr(vinmsg)
071900010604     C                             + ' ' + 'VHBVMD'
072000971021     C                   ELSE
072100971021     C                   MOVE      WD15          VABVMD
072200971021     C                   ENDIF
072300050118     C                   ENDIF
072400971021     C*
072500971023     C                   TESTN                   VHBANT               30
072600971023     C  N30              TESTN                   VHBANT                 30
072700971023     C  N30              TESTN                   VHBANT                   30
072800971021     C                   MOVE      VHBANT        W001A             1
072900971021     C     *IN30         IFEQ      *OFF
073000971021     C     W001A         ORLT      '0'
073100971023     C     W001A         ANDNE     *BLANK
073200971021     C                   SETON                                        32
073300971021     C                   MOVE      *ZERO         VABANT
073400010604     C                   eval      vinmsg = %trimr(vinmsg)
073500010604     C                             + ' ' + 'VHBANT'
073600971021     C                   ELSE
073700971021     C                   MOVE      VHBANT        VABANT
073800971021     C                   ENDIF
073900971021     C*
074000971021     C* CAMPI ALFABETICI:
074100971021     C                   MOVE      VHBATB        VABATB
074200971021     C                   MOVE      VHBCBO        VABCBO
074300971021     C                   MOVE      VHBRSD        VABRSD
074400020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
074500020117     C     '@':'A'       XLATE     VABRSD        VABRSD
074600020117     C* ==
074700971021     C                   MOVE      VHBRD2        VABRD2
074800971021     C                   MOVE      VHBIND        VABIND
074900971021     C                   MOVE      VHBCAD        VABCAD
075000971021     C                   MOVE      VHBLOD        VABLOD
075100971021     C                   MOVE      VHBPRD        VABPRD
075200971021     C                   MOVE      VHBNZD        VABNZD
075300971021     C                   MOVE      VHBGC1        VABGC1
075400971021     C                   MOVE      VHBGC2        VABGC2
075500971021     C                   MOVE      VHBTSP        VABTSP
075600971021     C                   MOVE      VHBVAS        VABVAS
075700971021     C                   MOVE      VHBNAS        VABNAS
075800971021     C                   MOVE      VHBTIC        VABTIC
075900971021     C                   MOVE      VHBVCA        VABVCA
076000971021     C                   MOVE      VHBGCA        VABGCA
076100971021     C                   MOVE      VHBRMA        VABRMA
076200971021     C                   MOVE      VHBXCO        VABXCO
076300971021     C                   MOVE      VHBNOT        VABNOT
076400971021     C                   MOVE      VHBNT2        VABNT2
076500971021     C                   MOVE      VHBCTM        VABCTM
076600971021     C                   MOVE      VHBFFD        VABFFD
076700971021     C                   MOVE      VHBTCR        VABTCR
076800971021     C                   MOVE      VHBCTS        VABCTS
076900971021     C                   MOVE      VHBFTM        VABFTM
077000971021     C                   MOVE      VHBVAD        VABVAD
077100971021     C                   MOVE      VHBGMA        VABGMA
077200971021     C                   MOVE      VHBGGA        VABGGA
077300971021     C                   MOVE      VHBGVA        VABGVA
077400971021     C                   MOVE      VHBTC1        VABTC1
077500971021     C                   MOVE      VHBTC2        VABTC2
077600971021     C                   MOVE      VHBSCL        VABSCL
077700971021     C                   MOVE      VHBRMO        VABRMO
077800971021     C                   MOVE      VHBCMO        VABCMO
077900971021     C                   MOVE      VHBNMO        VABNMO
078000971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
078100971021     C* NELLA NAZIONE ORIGINALE
078200971021     C                   MOVE      VHBNMO        WA1D              1
078300971021     C                   MOVEL     VHBNMO        WA1S              1
078400971021     C     WA1D          IFNE      *BLANK
078500971021     C     WA1S          ANDEQ     *BLANK
078600971021     C                   MOVE      *BLANK        VABNMO
078700971021     C                   END
078800050209     C*
078900050209     C* NSP => Stacco un numeratore da AZNUM x la valorizzazione del numero spedizione
079000050209     C                   clear                   TRUL33DS
079100050209     C                   eval      I33OPE = *zeros
079200050209     C                   eval      I33CNU = 302
079300050209     C                   eval      I33NUM = 1
079400050209     C                   movel     TRUL33DS      KPJBU
079500050209     C                   call      'TRUL33R'
079600050209     C                   parm                    KPJBA
079700050209     C                   movel     KPJBU         TRUL33DS
079800050209     C                   if        O33ERR = *zeros
079900050209     C                   z-add     O33NRF        VABNSP
080000050209     C                   z-add     O33NRF        VATNSP
080100050209     C                   else
080200050209     C                   SETON                                        31
080300050209     C                   EVAL      vinmsg = %trimr(vinmsg)
080400050209     C                             + ' ' + 'VABNSP VATNSP'
080500050209     C                   endif
080600050209     C*
080700050209     C* Forzo il codice trattamento merce a 7Q
080800050209     C                   EVAL      VABCTM = '7Q'
080900930409     C*
081000010604     C  N31              ADD       1             §CTROK            7 0
081100010604     C   32              ADD       1             §CTRMO            7 0
081200010604     C   31              ADD       1             §CTRNO            7 0
081300020913     C  N31              WRITE     FIVAB000
081400050209     C  N31              EXSR      WRIVAT
081500990910     C*
081600010601     C                   if        *in31 = *off and
081700010601     C                             *in32 = *off
081800990910     C                   eval      vinflg = '1'
081900990910     C                   else
082000000621     C                   eval      recko = vindta
082100050209     C                   if        %open(titvh7ps)
082200030407     C   50              except    rigadett
082300010612     C                   endif
082400990910     C                   eval      vinflg = '2'
082500990910     C                   endif
082600991022     C                   endif
082700990910     C                   endif
082800010601     C*
082900010601     C                   endif
083000000905     C*
083100000905     C                   else
083200000905     C                   eval      vinflg = '1'
083300000905     C                   endif
083400000905     C*
083500000905     C  N70              update    tivin000
083600991022     C*
083700991022     C  N70              ENDdo
083800010530     C
083900010530     C*
084000010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
084100010530     C                   if        cntNonEl = *zeros or
084200010530     C                             flgMulti = '0'
084300990910     C* Se non ci sono record con errori ...
084400010530     C                   if        §ctrno = 0 and
084500010601     C                             §ctrmo = 0 and
084600010530     C                             flgStato <> '2'
084700990910     C* ... restituisco esito OK.
084800990921     C                   eval      wrkesito = '0'
084900990910     C                   else
085000010601     C                   if        §ctrok > 0
085100990921     C                   eval      wrkesito = '1'
085200000710     C                   else
085300010615     C                   if        flgOk = '0'
085400010615     C                   eval      wrkesito = '2'
085500010615     C                   else
085600010615     C                   eval      wrkesito = '6'
085700010615     C                   endif
085800990910     C                   endif
085900000710     C                   endif
086000010530     C                   else
086100010530     C                   eval      wrkesito = '9'
086200010530     C                   endif
086300050209     C*
086400050209     C                   else
086500050209     C                   eval      wrkesito = '2'
086600050209     C                   endif
086700990910     C*
086800990914     C                   if        %open(tivin00r)
086900990908     C                   close     tivin00r
087000990914     C                   endif
087100020913     C                   if        %open(fivabwwr)
087200020913     C                   close     fivabwwr
087300990914     C                   endif
087400050209     C                   if        %open(fivatwwr)
087500050209     C                   close     fivatwwr
087600050209     C                   endif
087700010530     C*
087800010530     C                   if        vlrpoi <> 999
087900010530     C                   eval      invfil = vlrpoi
088000010530     C                   endif
088100990910     C*
088200990920     C                   if        §ctrok > 0
088300010530     C                             and invfil > *zeros
088400000613     C                   exsr      invio
088500990920     C                   endif
088600010612     C*
088700010612     C                   if        flgGiro = '1'
088800010612     C                   exsr      endela
088900010612     C                   endif
089000990920     C*
089100910830     C                   ENDSR
089200000613     C***
089300050209
089400050209
089500050209
089600050209     C*----------------------------------------------------*
089700050209     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
089800050209     C*----------------------------------------------------*
089900050209     C     PREVAT        BEGSR
090000050209     C*
090100050209     C* Compongo il nome del membro da dare al FIVATWWR
090200050209     C                   eval      parmbr = vlrhdl
090300050209     C                   movel     'M'           parmbr
090400050209     C                   eval      parccm = %subst(vlrKSC:2:7)
090500050209     C                   eval      paropz = '1'
090600050209     C* Effettuo la chiamata al CLLE preposto
090700050209     C                   call(e)   'TITVVTC'
090800050209     C                   parm                    parccm
090900050209     C                   parm                    parmbr
091000050209     C                   parm                    paropz
091100050209     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
091200050209     C                   if        %error
091300050209     C                   movel     '1'           chkcall
091400050209     C                   else
091500050209     C                   movel     '0'           chkcall
091600050209     C                   endif
091700050209     C*
091800050209     C                   ENDSR
091900050209
092000050209
092100990920
092200050209     C*----------------------------------------------------*
092300050209     C*  IMPOSTAZIONE CAMPI DA FLAT FILE (X FIVAT)
092400050209     C*----------------------------------------------------*
092500050209     C     WRIVAT        BEGSR
092600050209     C*
092700050209     C* Valorizzo il buffer del file dettagli barcode colli
092800050209     C                   EVAL      VATFGS = VABFGS
092900050209     C                   EVAL      VATAAS = VABAAS
093000050209     C                   EVAL      VATLNP = VABLNP
093100050209     C                   EVAL      VATNSP = VABNSP
093200050209     C                   EVAL      VATCCM = VABCCM
093300050209     C                   EVAL      VATTRC='E'
093400050209     C                   EVAL      VATNOT = %trim(%editc(VABRMN:'Z'))
093500050209     C                   write     FIVAT000
093600050209     C*
093700050209     C                   ENDSR
093800010530
093900010530
094000050209
094100010530
094200010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
094300010530     C     repfil        BEGSR
094400010530     C*
094500010530     C                   if        invfil = *zeros and
094600010530     C                             depfil > *zeros and
094700010613     C                             (vinflg = *blanks or
094800010613     C                              vinflg = *zeros)
094900010530     C
095000010530     C                   eval      invfil = depfil
095100010530     C                   endif
095200010530     C*
095300010530     C                   if        depfil <> invfil and
095400010530     C                             invfil > *zeros
095500010530     C                   eval      flgMulti = '1'
095600010530     C                   if        vinflg = *blanks
095700010530     C                   add       1             cntNonEl
095800010530     C                   endif
095900010530     C                   endif
096000010530     C*
096100010530     C                   if        vinflg = '2'
096200010530     C                   eval      flgStato = '2'
096300010530     C                   endif
096400010530     C*
096500010530     C                   ENDSR
096600010530     C***
096700050209
096800050209
096900050209
097000050209
097100050209      /TITLE Invio dei dati al punto operativo.
097200050209     C     invio         BEGSR
097300050209     C*
097400050209     C* 1° invio FIVAT
097500050209     C                   reset                   dscmz
097600050209     C                   move      vlrpoi        cmzdst
097700050209     C                   eval      cmzfld = 'FIVATWWR'
097800050209     C                   eval      cmzmbd = vlrhdl
097900050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
098000050209     C***                if        prmfir = *blanks
098100050209     C                   eval      cmzfla = 'FIVAT00F'
098200050209     C                   eval      cmzmba = 'FIVAT00F'
098300050209     C***                else
098400050209     C***                eval      cmzfla = prmfir
098500050209     C***                eval      cmzmba = prmfir
098600050209     C***                endif
098700050209     C                   eval      cmznrr = *zeros
098800050209     C                   move      §ctrok        cmznrr
098900050209     C                   eval      cmzlba = vlrfl1
099000050209     C                   call(e)   'TIS711C'
099100050209     C                   parm                    dscmz
099200050209     C                   parm      *blanks       esito
099300050209     C                   if        %error
099400050209     C                             or cmzerr = '1'
099500050209     C                             or esito  = '1'
099600050209     C                   eval      wrkesito = '3'
099700050209     C                   else
099800050209     C*
099900050209     C* 2° invio FIVAB
100000050209     C                   reset                   dscmz
100100050209     C                   move      vlrpoi        cmzdst
100200050209     C                   eval      cmzfld = vlrfou
100300050209     C                   eval      cmzmbd = vlrhdl
100400050209     C                   eval      %subst(cmzmbd:1:1) = 'M'
100500050209     C***                if        prmfir = *blanks
100600050209     C                   eval      cmzfla = 'FIVAB00F'
100700050209     C                   eval      cmzmba = 'FIVAB00F'
100800050209     C***                else
100900050209     C***                eval      cmzfla = prmfir
101000050209     C***                eval      cmzmba = prmfir
101100050209     C***                endif
101200050209     C                   eval      cmznrr = *zeros
101300050209     C                   move      §ctrok        cmznrr
101400050209     C                   eval      cmzlba = vlrfl1
101500050209     C                   call(e)   'TIS711C'
101600050209     C                   parm                    dscmz
101700050209     C                   parm      *blanks       esito
101800050209     C                   if        %error
101900050209     C                             or cmzerr = '1'
102000050209     C                             or esito  = '1'
102100050209     C                   eval      wrkesito = '3'
102200050209     C                   endif
102300050209     C                   endif
102400050209     C*
102500050209     C                   ENDSR
102600050209     C***
102700010530
102800010530
102900010530
103000010530
103100010530      /TITLE Invio dei dati al punto operativo.
103200010530     C     opeini        BEGSR
103300010530     C*
103400010530     C* Inizializzo flag e contatori operativi
103500010530     C                   movel     '0'           flgGiro           1
103600010530     C                   movel     '0'           flgMulti          1
103700010530     C                   movel     '1'           flgStato          1
103800010615     C                   movel     '0'           flgOk             1
103900010530     C                   z-add     *zeros        cntNonEl         10 0
104000010530     C                   z-add     *zeros        depfil            3 0
104100010530     C                   z-add     *zeros        invfil            3 0
104200030407     C*
104300050209     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
104400030407     C                   if        %subst(vlrppt:1:1) = 'S'
104500030407     C                   seton                                        50
104600030407     C                   else
104700030407     C                   setoff                                       50
104800030407     C                   endif
104900041001     C*
105000050209     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
105100041001     C                   if        %subst(vlrppt:2:1) = 'S'
105200041001     C                   seton                                        51
105300041001     C                   else
105400041001     C                   setoff                                       51
105500041001     C                   endif
105600010530     C*
105700010530     C                   ENDSR
105800010530     C***
105900010530
106000010530
106100070411
106200070411     C     *pssr         BEGSR
106300070411     C*
106400070411     C                   if        %open(tivin00r)
106500070411     C                   close     tivin00r
106600070411     C                   endif
106700070411     C                   if        %open(fivabwwr)
106800070411     C                   close     fivabwwr
106900070411     C                   endif
107000070411     C                   if        %open(fivatwwr)
107100070411     C                   close     fivatwwr
107200070411     C                   endif
107300070411     C*
107400070411     C* Effettuo la chiamata al CLLE preposto
107500070411     C                   call(e)   'TITVVTC'
107600070411     C                   parm                    parccm
107700070411     C                   parm                    parmbr
107800070411     C                   parm      '2'           paropz
107900070411     C*
108000070411     C                   eval      wrkesito = '2'
108100070411     C*
108200070411     C                   seton                                        LR
108300070411     C*
108400070411     C                   ENDSR     '*CANCL'
108500070411     C***
108600010530
108700990910
108800000613     C     *inzsr        BEGSR
108900990910     C*
109000990910     C     *entry        plist
109100990920     C                   parm                    tivlrds
109200990921     C                   parm      wrkesito      esito
109300000724     C                   parm                    prmlit
109400000710     C                   parm                    prmfir
109500010606     C*
109600010606     C* CALCOLA LA DATA CORRENTE
109700010606     C                   time                    wn14             14 0
109800010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
109900010606     C                   z-add     wn8           g08dat
110000010606     C                   z-add     *zeros        g08inv
110100010606     C                   movel     '0'           g08err
110200010606     C                   call      'XSRDA8'
110300010606     C                   parm                    wlbda8
110400010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
110500000613     C*
110600000613     C                   ENDSR
110700000613     C***
110800990908
110900050209     Otitvh7p   E            riepilogo         2
111000990915     O                                              'Upload via Internet'
111100990915     O                                           +1 'Traduzione TIVIN00R -'
111200020913     O                                           +1 'FIVABWWR'
111300990915     O                                           +1 'Testate bolle'
111400990915     O          E            riepilogo   2
111500990915     O                       wrkdata
111600990915     O                       wrkora              +1
111700990915     O                       procname            +1
111800990915     O          E            riepilogo   2
111900990915     O                                              'Cliente..................:'
112000990915     O                       VABCCM        z     +1
112100990915     O          E            riepilogo   2
112200990920     O                                              'Riferimento Strategi.....:'
112300990920     O                       vlrhdl              +1
112400990915     O          E            riepilogo   2
112500990915     O                                              'Giusti...................:'
112600971022     O                       §CTROK        2   +  1
112700990915     O          E            riepilogo   2
112800990915     O                                              'Sbagliati e corretti.....:'
112900971022     O                       §CTRMO        2   +  1
113000990915     O          E            riepilogo   2
113100990915     O                                              'Sbagliati e scartati.....:'
113200971022     O                       §CTRNO        2   +  1
113300000613
113400050209     Otitvh7ps  E            testdett          2
113500000613     O                                              'Upload via Internet'
113600000613     O                                           +1 'Traduzione TIVIN00R -'
113700020913     O                                           +1 'FIVABWWR'
113800000613     O                                           +1 'Testate bolle'
113900000616     O          E            testdett    3
114000000613     O                                           +2 'N° rec'
114100000613     O                                           +3 'Anteprima contenuto'
114200000616     O          E            rigadett    2
114300000613     O                       rrnum               +2
114400000621     O                       recko               +3
114500000616     O          E            findett     2
114600000613     O                       wrkdata
114700000613     O                       wrkora              +1
114800000613     O                       procname            +1
114900000616     O          E            findett     2
115000000613     O                                              'Cliente..................:'
115100000613     O                       VABCCM        z     +1
115200000616     O          E            findett     2
115300000613     O                                              'Riferimento Strategi.....:'
115400000613     O                       vlrhdl              +1
115500000616     O          E            findett     2
115600000613     O                                              'Giusti...................:'
115700000613     O                       §CTROK        2   +  1
115800000616     O          E            findett     2
115900000613     O                                              'Sbagliati e corretti.....:'
116000000613     O                       §CTRMO        2   +  1
116100000616     O          E            findett     2
116200000613     O                                              'Sbagliati e scartati.....:'
116300000613     O                       §CTRNO        2   +  1
116400000512** CMD - COMANDI CL
116500050209OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVH7P)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
116600050209OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TITVH7PS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
116700050209DLTOVR FILE(TITVH7P TITVH7PS) LVL(*)
116800000512
116900000512
