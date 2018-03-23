000100050722      /TITLE Upload via Internet: traduzione in FIVAB/FIVAT.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600050722     FFIVATwwr  O    E             DISK    usropn
000700020913     FFIVABwwr  O    E             DISK    usropn
000800010601     Ftis7t5p   O    f  132        PRINTER usropn
000900000621     F                                     oflind(*inoa)
001000010601     Ftis7t5ps  O    f  198        PRINTER usropn
001100000621     F                                     oflind(*inob)
001200990908
001300000512     D*------------
001400000512     D* COMANDI
001500000512     D*------------
001600011123     D cmd             S            100    DIM(5) CTDATA PERRCD(1)
001700000512     D*------------
001800990920     D dscmz         e ds                  inz
001900990910     D psds           sds
002000990910     D  procname         *PROC
002100941229     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002200941229     D* 10+3
002300941229     D                 DS
002400941229     D  WS15                   1      1
002500941229     D  WI15                   2     11
002600941229     D  WV15                  12     12
002700941229     D  WD15                  13     15
002800941229     D  WASC15                 1     15
002900941229     D*  6+1
003000941229     D                 DS
003100941229     D  WS09                   1      1
003200941229     D  WI09                   2      7
003300941229     D  WV09                   8      8
003400941229     D  WD09                   9      9
003500941229     D  WASC09                 1      9
003600941229     D*  2+3
003700941229     D                 DS
003800941229     D  WS07                   1      1
003900941229     D  WI07                   2      3
004000941229     D  WV07                   4      4
004100941229     D  WD07                   5      7
004200941229     D  WASC07                 1      7
004300990920     D tivlrds       e ds                  extname(tivlr00f)
004400041025     D tisi95ds      e ds
004500990910     D fnvabaar      e ds                  inz
004600990910     D esito           s              1
004700000724     D prmlit          s             10
004800000710     D prmfir          s             10
004900990921     D wrkesito        s                   like(esito)
005000990915     D wrkdata         s               d
005100990915     D wrkora          s               t
005200000613     D rrnum           s              6  0 INZ(*zeros)
005300000621     D recko           s            150    INZ(*blanks)
005400011123     D depcmd          s            150    INZ(*blanks)
005500050722     D parccm          s              8    INZ(*blanks)
005600050722     D parmbr          s             10    INZ(*blanks)
005700050722     D paropz          s              1    INZ(*blanks)
005800050722     D chkcall         s              1    INZ(*blanks)
005900010606     D*------------------
006000010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
006100010606     D*------------------
006200010606     D WLBDA8          DS                  INZ
006300010606     D  G08DAT                 1      8  0
006400010606     D  G08INV                 9     16  0
006500010606     D  G08ERR                17     17
006600010606     D  G08TGI                18     22  0
006700010606     D*
006800040407     D TxtInOut        S           2048
006900040407     D ElencoChar      S            256
007000040407     D TipoElenco      S              1
007100040407     D CharSost        S              1
007200040407     D UpperCase       S              1
007300040407     D ChkNull         S              1
007400040407     D CharNull        S              1
007500040407     D EsitoBon        S              1
007600990908
007700990915     C                   time                    wrkdata
007800990915     C                   time                    wrkora
007900000913     C                   reset                   rrnum
008000990921     C                   reset                   esito
008100990921     C                   reset                   wrkesito
008200010530     C*
008300010530     C                   exsr      opeini
008400010530     C                   exsr      rwvab
008500041025     C*
008600041025     C* Esegue lancio TISI95R solo x chiusura
008700041025     C                   CLEAR                   TISI95DS
008800041025     C                   EVAL      I95TLA = 'C'
008900041025     C                   CALL      'TISI95R'
009000041025     C                   PARM                    TISI95DS
009100010530     C*
009200010530     C                   seton                                        lr
009300010530
009400010530
009500010530
009600010530
009700010530     C*--------------------------------------------------------
009800010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009900010530     C*--------------------------------------------------------
010000010530     C     PREELA        BEGSR
010100000724     C*
010200000724     C* SE OCCORRE SPEDIRE IN FILIALE
010300010530     C                   if        invfil <> *zeros and
010400010530     C                             flgGiro = '0'
010500010530     C*
010600010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010700010530     C                   eval      flgGiro = '1'
010800000724     C*
010900000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
011000000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011100010530     C     invfil        chain     azorg01l
011200000724     C                   if        %found
011300000616     C                   movel(p)  CMD(1)        depcmd
011400020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
011500000616     C*
011600000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011700011123     C                   Z-ADD     150           LENGH            15 5
011800000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011900000616     C                   PARM                    depcmd
012000000616     C                   PARM                    LENGH
012100000724     C*
012200000724     C                   endif
012300000616     C*
012400000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
012500000616     C                   movel(p)  CMD(2)        depcmd
012600000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012700000616     C*
012800000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012900011123     C                   Z-ADD     150           LENGH            15 5
013000000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
013100000616     C                   PARM                    depcmd
013200000616     C                   PARM                    LENGH
013300000616     C*
013400010601     C                   if        not %open(tis7t5ps)
013500010601     C                   open      tis7t5ps
013600030407     C   50              except    testdett
013700000613     C                   endif
013800010530     C*
013900010530     C                   endif
014000010530     C*
014100010530     C                   ENDSR
014200010530     C***
014300010530
014400010530
014500010530
014600010530
014700010530
014800010530
014900010530     C*--------------------------------------------------------
015000010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
015100010530     C*--------------------------------------------------------
015200010530     C     ENDELA        BEGSR
015300010530     C*
015400010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
015500050722     C*
015600050722     C* Effettuo la chiamata al CLLE preposto
015700050722     C                   call(e)   'TITVVTC'
015800050722     C                   parm                    parccm
015900050722     C                   parm                    parmbr
016000050722     C                   parm      '2'           paropz
016100000613     C*
016200010601     C                   if        %open(tis7t5ps)
016300030407     C   50              except    findett
016400010601     C                   close     tis7t5ps
016500000613     C                   endif
016600000616     C*
016700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
016800011123     C                   Z-ADD     150           LENGH            15 5
016900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
017000000616     C                   PARM                    CMD(3)
017100000616     C                   PARM                    LENGH
017200010530     C*
017300010530     C                   ENDSR
017400010530     C***
017500000613
017600010530
017700010530
017800000613
017900000613     C*--------------------------------------------------------
018000000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
018100000613     C*--------------------------------------------------------
018200000621     C     STPR          BEGSR
018300000613     C*
018400010601     C                   if        not %open(tis7t5p)
018500010601     C                   open      tis7t5p
018600990915     C                   endif
018700990915     C*
018800030407     C   50              except    riepilogo
018900990915     C*
019000010601     C                   if        %open(tis7t5p)
019100010601     C                   close     tis7t5p
019200990914     C                   endif
019300990910     C*
019400000613     C                   ENDSR
019500000613     C***
019600990908
019700010530
019800010530
019900010530
020000910830     C*--------------------------------------------------------
020100020913     C* RWVAB   LEGGE tivin00r E SCRIVE FIVABWWF              *
020200910830     C*--------------------------------------------------------
020300941229     C     RWVAB         BEGSR
020400990910     C*
020500990914     C                   if        not %open(tivin00r)
020600990908     C                   open      tivin00r
020700990914     C                   endif
020800020913     C                   if        not %open(fivabwwr)
020900020913     C                   open      fivabwwr
021000990914     C                   endif
021100050722     C*
021200050722     C* Eseguo operazioni di aggiunta nuovo membro in FIVATWWR
021300050722     C                   exsr      prevat
021400050722     C*
021500050722     C                   if        chkcall = '0'
021600050722     C*
021700050722     C                   if        not %open(fivatwwr)
021800050722     C                   open      fivatwwr
021900050722     C                   endif
022000990910     C*
022100050722     C                   clear                   §CTROKVB          7 0
022200050722     C                   clear                   §CTROKVT          7 0
022300050722     C                   clear                   §CTRMO            7 0
022400050722     C                   clear                   §CTRNO            7 0
022500990910     C*
022600921023     C                   DO        *HIVAL
022700990913     C*
022800990915     C                   READ      tivin00r                               70
022900010618     C*
023000010618     C* Dopo ogni lettura verifico se ci sono stati record OK
023100010618     C                   if        vinflg = '1'
023200010618     C                   eval      flgOk = '1'
023300010618     C                   endif
023400010705     C*
023500010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
023600010705     C                   if        %subst(vindta:1:1) = '='
023700010705     C                   eval      %subst(vindta:1:1) = *blanks
023800010705     C                   endif
023900010705     C*
024000000905     C                   if        vindta > *blanks
024100000613     C                   add       1             rrnum
024200010601     C* ...elimino eventuali segni
024300010601     C     '+':' '       xlate     vindta        vindta
024400990913     C*
024500010601     C                   if        *in70 = *off and
024600990913     C                             (vinflg = *blanks
024700010711     C                              or vinflg = '0'
024800010711     C                              or vinflg = '2')
024900990913     C*
025000020913     C                   clear                   FIVAB000
025100050722     C                   clear                   FIVAT000
025200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
025300010711     C                   if        vinflg = *blanks or vinflg = '0'
025400010711     C                   clear                   vinmsg
025500010711     C                   endif
025600010711     C*
025700990913     C                   eval      fnvabaar = vindta
025800991022     C*
025900991022     C                   testb     x'3F'         vhbatb                   02
026000991022     C                   if        not *in02
026100991022     C*
026200971021     C                   SETOFF                                       303132
026300971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
026400971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
026500971023     C*    OPPURE TUTTI BLANKS
026600010530     C*
026700010530     C                   TESTN                   VHBLNP               30
026800010530     C  N30              TESTN                   VHBLNP                 30
026900010530     C  N30              TESTN                   VHBLNP                   30
027000010530     C                   MOVE      VHBLNP        W001A             1
027100010530     C     *IN30         IFEQ      *OFF
027200010530     C     W001A         ORLT      '0'
027300010530     C                   SETON                                        31
027400010530     C                   eval      vinmsg = %trimr(vinmsg)
027500010530     C                             + ' ' + 'VHBLNP'
027600010530     C                   ELSE
027700010530     C                   MOVE      VHBLNP        VABLNP
027800010530     C     VABLNP        IFLE      *ZEROS
027900010530     C                   SETON                                        31
028000010530     C                   eval      vinmsg = %trimr(vinmsg)
028100010530     C                             + ' ' + 'VHBLNP'
028200010530     C                   ENDIF
028300010530     C                   ENDIF
028400010530     C*
028500010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
028600010530     C                   eval      depfil = VABLNP
028700010530     C                   exsr      repfil
028800010601     C                   if        depfil = invfil
028900021014     C                   if        vlrpoi = 999
029000021010     C                   MOVE(P)   invfil        VABFGS
029100050722     C                   move(P)   invfil        VATFGS
029200021014     C                   else
029300021014     C                   MOVE(P)   vlrpoi        VABFGS
029400050722     C                   move(P)   vlrpoi        VATFGS
029500021014     C                   endif
029600010530     C*
029700010530     C                   exsr      PREELA
029800010530     C*
029900971023     C                   TESTN                   VHBCCM               30
030000971023     C  N30              TESTN                   VHBCCM                 30
030100971023     C  N30              TESTN                   VHBCCM                   30
030200971021     C                   MOVE      VHBCCM        W001A             1
030300971021     C     *IN30         IFEQ      *OFF
030400971021     C     W001A         ORLT      '0'
030500971021     C                   SETON                                        31
030600010604     C                   eval      vinmsg = %trimr(vinmsg)
030700010604     C                             + ' ' + 'VHBCCM'
030800971021     C                   ELSE
030900971021     C                   MOVE      VHBCCM        VABCCM
031000971024     C     VABCCM        IFLE      *ZEROS
031100971024     C                   SETON                                        31
031200010604     C                   eval      vinmsg = %trimr(vinmsg)
031300010604     C                             + ' ' + 'VHBCCM'
031400971024     C                   ENDIF
031500971021     C                   ENDIF
031600971021     C*
031700971023     C                   TESTN                   VHBAAS               30
031800971023     C  N30              TESTN                   VHBAAS                 30
031900971023     C  N30              TESTN                   VHBAAS                   30
032000971021     C                   MOVE      VHBAAS        W001A             1
032100971021     C     *IN30         IFEQ      *OFF
032200971021     C     W001A         ORLT      '0'
032300971023     C     W001A         ANDNE     *BLANK
032400971021     C                   SETON                                        32
032500010606     C                   MOVEL     datcor        VABAAS
032600010604     C                   eval      vinmsg = %trimr(vinmsg)
032700010604     C                             + ' ' + 'VHBAAS'
032800971021     C                   ELSE
032900971021     C                   MOVE      VHBAAS        VABAAS
033000971021     C                   ENDIF
033100971021     C*
033200971023     C                   TESTN                   VHBMGS               30
033300971023     C  N30              TESTN                   VHBMGS                 30
033400971023     C  N30              TESTN                   VHBMGS                   30
033500971021     C                   MOVE      VHBMGS        W001A             1
033600971021     C     *IN30         IFEQ      *OFF
033700971021     C     W001A         ORLT      '0'
033800971023     C     W001A         ANDNE     *BLANK
033900971021     C                   SETON                                        32
034000010601     C                   MOVE      '0101'        VABMGS
034100010604     C                   eval      vinmsg = %trimr(vinmsg)
034200010604     C                             + ' ' + 'VHBMGS'
034300971021     C                   ELSE
034400971021     C                   MOVE      VHBMGS        VABMGS
034500971021     C                   ENDIF
034600971021     C*
034700971023     C                   TESTN                   VHBNRS               30
034800971023     C  N30              TESTN                   VHBNRS                 30
034900971023     C  N30              TESTN                   VHBNRS                   30
035000971021     C                   MOVE      VHBNRS        W001A             1
035100971021     C     *IN30         IFEQ      *OFF
035200971021     C     W001A         ORLT      '0'
035300971023     C     W001A         ANDNE     *BLANK
035400971021     C                   SETON                                        32
035500971021     C                   MOVE      *ZERO         VABNRS
035600010604     C                   eval      vinmsg = %trimr(vinmsg)
035700010604     C                             + ' ' + 'VHBNRS'
035800971021     C                   ELSE
035900971021     C                   MOVE      VHBNRS        VABNRS
036000971021     C                   ENDIF
036100971021     C*
036200971023     C                   TESTN                   VHBNSP               30
036300971023     C  N30              TESTN                   VHBNSP                 30
036400971023     C  N30              TESTN                   VHBNSP                   30
036500971021     C                   MOVE      VHBNSP        W001A             1
036600971021     C     *IN30         IFEQ      *OFF
036700971021     C     W001A         ORLT      '0'
036800971023     C     W001A         ANDNE     *BLANK
036900971021     C                   SETON                                        32
037000971021     C                   MOVE      *ZERO         VABNSP
037100010604     C                   eval      vinmsg = %trimr(vinmsg)
037200010604     C                             + ' ' + 'VHBNSP'
037300971021     C                   ELSE
037400971021     C                   MOVE      VHBNSP        VABNSP
037500971021     C                   ENDIF
037600971021     C*
037700971023     C                   TESTN                   VHBLNA               30
037800971023     C  N30              TESTN                   VHBLNA                 30
037900971023     C  N30              TESTN                   VHBLNA                   30
038000971021     C                   MOVE      VHBLNA        W001A             1
038100971021     C     *IN30         IFEQ      *OFF
038200971021     C     W001A         ORLT      '0'
038300971023     C     W001A         ANDNE     *BLANK
038400971021     C                   SETON                                        32
038500971021     C                   MOVE      *ZERO         VABLNA
038600010604     C                   eval      vinmsg = %trimr(vinmsg)
038700010604     C                             + ' ' + 'VHBLNA'
038800971021     C                   ELSE
038900971021     C                   MOVE      VHBLNA        VABLNA
039000971021     C                   ENDIF
039100971021     C*
039200971023     C                   TESTN                   VHBCTR               30
039300971023     C  N30              TESTN                   VHBCTR                 30
039400971023     C  N30              TESTN                   VHBCTR                   30
039500971021     C                   MOVE      VHBCTR        W001A             1
039600971021     C     *IN30         IFEQ      *OFF
039700971021     C     W001A         ORLT      '0'
039800971023     C     W001A         ANDNE     *BLANK
039900971021     C                   SETON                                        32
040000971021     C                   MOVE      *ZERO         VABCTR
040100010604     C                   eval      vinmsg = %trimr(vinmsg)
040200010604     C                             + ' ' + 'VHBCTR'
040300971021     C                   ELSE
040400971021     C                   MOVE      VHBCTR        VABCTR
040500971021     C                   ENDIF
040600941229     C*
040700971021     C                   MOVE      *ZERO         VABIAS
040800941229     C                   MOVE      VHBIAS        WASC15
040900971023     C                   TESTN                   WI15                 30
041000971023     C  N30              TESTN                   WI15                   30
041100971023     C  N30              TESTN                   WI15                     30
041200971021     C                   MOVE      WI15          W001A             1
041300971021     C     *IN30         IFEQ      *OFF
041400971021     C     W001A         ORLT      '0'
041500971023     C     W001A         ANDNE     *BLANK
041600971021     C                   SETON                                        32
041700971021     C                   MOVE      *ZERO         VABIAS
041800010604     C                   eval      vinmsg = %trimr(vinmsg)
041900010604     C                             + ' ' + 'VHBIAS'
042000971021     C                   ELSE
042100971021     C                   MOVEL     WI15          VABIAS
042200971023     C                   TESTN                   WD15                 30
042300971023     C  N30              TESTN                   WD15                   30
042400971023     C  N30              TESTN                   WD15                     30
042500971021     C                   MOVE      WD15          W001A             1
042600971021     C     *IN30         IFEQ      *OFF
042700971021     C     W001A         ORLT      '0'
042800971023     C     W001A         ANDNE     *BLANK
042900971021     C                   SETON                                        32
043000971021     C                   MOVE      *ZERO         VABIAS
043100010604     C                   eval      vinmsg = %trimr(vinmsg)
043200010604     C                             + ' ' + 'VHBIAS'
043300971021     C                   ELSE
043400971021     C                   MOVE      WD15          VABIAS
043500971021     C                   ENDIF
043600050118     C                   ENDIF
043700941229     C*
043800971023     C                   TESTN                   VHBNCL               30
043900971023     C  N30              TESTN                   VHBNCL                 30
044000971023     C  N30              TESTN                   VHBNCL                   30
044100971021     C                   MOVE      VHBNCL        W001A             1
044200971021     C     *IN30         IFEQ      *OFF
044300971021     C     W001A         ORLT      '0'
044400971021     C                   SETON                                        32
044500971021     C                   MOVE      *ZERO         VABNCL
044600010604     C                   eval      vinmsg = %trimr(vinmsg)
044700010604     C                             + ' ' + 'VHBNCL'
044800971021     C                   ELSE
044900971021     C                   MOVE      VHBNCL        VABNCL
045000971021     C                   ENDIF
045100941229     C*
045200971021     C                   MOVE      *ZERO         VABPKB
045300971021     C                   MOVE      VHBPKB        WASC09
045400010719     C                   IF        WI09 = *blanks
045500010719     C                   EVAL      WI09 = *zeros
045600010719     C                   ENDIF
045700971023     C                   TESTN                   WI09                 30
045800971023     C  N30              TESTN                   WI09                   30
045900971023     C  N30              TESTN                   WI09                     30
046000971021     C                   MOVE      WI09          W001A             1
046100971021     C     *IN30         IFEQ      *OFF
046200971021     C     W001A         ORLT      '0'
046300971021     C                   SETON                                        32
046400971021     C                   MOVE      *ZERO         VABPKB
046500010604     C                   eval      vinmsg = %trimr(vinmsg)
046600010604     C                             + ' ' + 'VHBPKB'
046700971021     C                   ELSE
046800971021     C                   MOVEL     WI09          VABPKB
046900010719     C                   IF        WD09 = *blanks
047000010719     C                   EVAL      WD09 = *zeros
047100010719     C                   ENDIF
047200971023     C                   TESTN                   WD09                 30
047300971023     C  N30              TESTN                   WD09                     30
047400971021     C                   MOVE      WD09          W001A             1
047500971021     C     *IN30         IFEQ      *OFF
047600971021     C     W001A         ORLT      '0'
047700971023     C     W001A         ANDNE     *BLANK
047800971021     C                   SETON                                        32
047900971021     C                   MOVE      *ZERO         VABPKB
048000010604     C                   eval      vinmsg = %trimr(vinmsg)
048100010604     C                             + ' ' + 'VHBPKB'
048200971021     C                   ELSE
048300971021     C                   MOVE      WD09          VABPKB
048400971021     C                   ENDIF
048500050118     C                   ENDIF
048600971021     C*
048700041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
048800041001     C                   IF        *IN51 = *OFF
048900971021     C                   MOVE      *ZERO         VABVLB
049000971021     C                   MOVE      VHBVLB        WASC07
049100010719     C                   IF        WI07 = *blanks
049200010719     C                   EVAL      WI07 = *zeros
049300010719     C                   ENDIF
049400971023     C                   TESTN                   WI07                 30
049500971023     C  N30              TESTN                   WI07                   30
049600971023     C  N30              TESTN                   WI07                     30
049700971021     C                   MOVE      WI07          W001A             1
049800971021     C     *IN30         IFEQ      *OFF
049900971021     C     W001A         ORLT      '0'
050000971023     C     W001A         ANDNE     *BLANK
050100971021     C                   SETON                                        32
050200971021     C                   MOVE      *ZERO         VABVLB
050300010604     C                   eval      vinmsg = %trimr(vinmsg)
050400010604     C                             + ' ' + 'VHBVLB'
050500971021     C                   ELSE
050600971021     C                   MOVEL     WI07          VABVLB
050700010719     C                   IF        WD07 = *blanks
050800010719     C                   EVAL      WD07 = *zeros
050900010719     C                   ENDIF
051000971023     C                   TESTN                   WD07                 30
051100971023     C  N30              TESTN                   WD07                   30
051200971023     C  N30              TESTN                   WD07                     30
051300971021     C                   MOVE      WD07          W001A             1
051400971021     C     *IN30         IFEQ      *OFF
051500971021     C     W001A         ORLT      '0'
051600971023     C     W001A         ANDNE     *BLANK
051700971021     C                   SETON                                        32
051800971021     C                   MOVE      *ZERO         VABVLB
051900010604     C                   eval      vinmsg = %trimr(vinmsg)
052000010604     C                             + ' ' + 'VHBVLB'
052100971021     C                   ELSE
052200971021     C                   MOVE      WD07          VABVLB
052300971021     C                   ENDIF
052400050118     C                   ENDIF
052500041001     C                   ELSE
052600041001     C* ...altrimenti forzo azzeramento del volume
052700041001     C                   Z-ADD     *zeros        VABVLB
052800041001     C                   ENDIF
052900941229     C*
053000971021     C                   MOVE      *ZERO         VABQFT
053100971021     C                   MOVE      VHBQFT        WASC15
053200010719     C                   IF        WI15 = *blanks
053300010719     C                   EVAL      WI15 = *zeros
053400010719     C                   ENDIF
053500971023     C                   TESTN                   WI15                 30
053600971023     C  N30              TESTN                   WI15                   30
053700971023     C  N30              TESTN                   WI15                     30
053800971021     C                   MOVE      WI15          W001A             1
053900971021     C     *IN30         IFEQ      *OFF
054000971021     C     W001A         ORLT      '0'
054100971023     C     W001A         ANDNE     *BLANK
054200971021     C                   SETON                                        32
054300971021     C                   MOVE      *ZERO         VABQFT
054400010604     C                   eval      vinmsg = %trimr(vinmsg)
054500010604     C                             + ' ' + 'VHBQFT'
054600971021     C                   ELSE
054700971021     C                   MOVEL     WI15          VABQFT
054800010719     C                   IF        WD15 = *blanks
054900010719     C                   EVAL      WD15 = *zeros
055000010719     C                   ENDIF
055100971023     C                   TESTN                   WD15                 30
055200971023     C  N30              TESTN                   WD15                   30
055300971023     C  N30              TESTN                   WD15                     30
055400971021     C                   MOVE      WD15          W001A             1
055500971021     C     *IN30         IFEQ      *OFF
055600971021     C     W001A         ORLT      '0'
055700971023     C     W001A         ANDNE     *BLANK
055800971021     C                   SETON                                        32
055900971021     C                   MOVE      *ZERO         VABQFT
056000010604     C                   eval      vinmsg = %trimr(vinmsg)
056100010604     C                             + ' ' + 'VHBQFT'
056200971021     C                   ELSE
056300971021     C                   MOVE      WD15          VABQFT
056400971021     C                   ENDIF
056500050118     C                   ENDIF
056600941229     C*
056700971021     C                   MOVE      *ZERO         VABCAS
056800971021     C                   MOVE      VHBCAS        WASC15
056900010719     C                   IF        WI15 = *blanks
057000010719     C                   EVAL      WI15 = *zeros
057100010719     C                   ENDIF
057200971023     C                   TESTN                   WI15                 30
057300971023     C  N30              TESTN                   WI15                   30
057400971023     C  N30              TESTN                   WI15                     30
057500971021     C                   MOVE      WI15          W001A             1
057600971021     C     *IN30         IFEQ      *OFF
057700971021     C     W001A         ORLT      '0'
057800971023     C     W001A         ANDNE     *BLANK
057900971021     C                   SETON                                        32
058000971021     C                   MOVE      *ZERO         VABCAS
058100010604     C                   eval      vinmsg = %trimr(vinmsg)
058200010604     C                             + ' ' + 'VHBCAS'
058300971021     C                   ELSE
058400971021     C                   MOVEL     WI15          VABCAS
058500010719     C                   IF        WD15 = *blanks
058600010719     C                   EVAL      WD15 = *zeros
058700010719     C                   ENDIF
058800971023     C                   TESTN                   WD15                 30
058900971023     C  N30              TESTN                   WD15                   30
059000971023     C  N30              TESTN                   WD15                     30
059100971021     C                   MOVE      WD15          W001A             1
059200971021     C     *IN30         IFEQ      *OFF
059300971021     C     W001A         ORLT      '0'
059400971023     C     W001A         ANDNE     *BLANK
059500971021     C                   SETON                                        32
059600971021     C                   MOVE      *ZERO         VABCAS
059700010604     C                   eval      vinmsg = %trimr(vinmsg)
059800010604     C                             + ' ' + 'VHBCAS'
059900971021     C                   ELSE
060000971021     C                   MOVE      WD15          VABCAS
060100971021     C                   ENDIF
060200040827     C                   ENDIF
060300971021     C*
060400971023     C                   TESTN                   VHBRMN               30
060500971023     C  N30              TESTN                   VHBRMN                 30
060600971023     C  N30              TESTN                   VHBRMN                   30
060700971021     C                   MOVE      VHBRMN        W001A             1
060800971021     C     *IN30         IFEQ      *OFF
060900971021     C     W001A         ORLT      '0'
061000971021     C                   SETON                                        32
061100971021     C                   MOVE      *ZERO         VABRMN
061200010604     C                   eval      vinmsg = %trimr(vinmsg)
061300010604     C                             + ' ' + 'VHBRMN'
061400971021     C                   ELSE
061500971021     C                   MOVE      VHBRMN        VABRMN
061600971021     C                   ENDIF
061700971021     C*
061800971023     C                   TESTN                   VHBNCD               30
061900971023     C  N30              TESTN                   VHBNCD                 30
062000971023     C  N30              TESTN                   VHBNCD                   30
062100971021     C                   MOVE      VHBNCD        W001A             1
062200971021     C     *IN30         IFEQ      *OFF
062300971021     C     W001A         ORLT      '0'
062400971023     C     W001A         ANDNE     *BLANK
062500971021     C                   SETON                                        32
062600971021     C                   MOVE      *ZERO         VABNCD
062700010604     C                   eval      vinmsg = %trimr(vinmsg)
062800010604     C                             + ' ' + 'VHBNCD'
062900971021     C                   ELSE
063000971021     C                   MOVE      VHBNCD        VABNCD
063100971021     C                   ENDIF
063200971021     C*
063300971023     C                   TESTN                   VHBNCA               30
063400971023     C  N30              TESTN                   VHBNCA                 30
063500971023     C  N30              TESTN                   VHBNCA                   30
063600971021     C                   MOVE      VHBNCA        W001A             1
063700971021     C     *IN30         IFEQ      *OFF
063800971021     C     W001A         ORLT      '0'
063900971023     C     W001A         ANDNE     *BLANK
064000971021     C                   SETON                                        32
064100971021     C                   MOVE      *ZERO         VABNCA
064200010604     C                   eval      vinmsg = %trimr(vinmsg)
064300010604     C                             + ' ' + 'VHBNCA'
064400971021     C                   ELSE
064500971021     C                   MOVE      VHBNCA        VABNCA
064600971021     C                   ENDIF
064700971021     C*
064800971023     C                   TESTN                   VHBZNC               30
064900971023     C  N30              TESTN                   VHBZNC                 30
065000971023     C  N30              TESTN                   VHBZNC                   30
065100971021     C                   MOVE      VHBZNC        W001A             1
065200971021     C     *IN30         IFEQ      *OFF
065300971021     C     W001A         ORLT      '0'
065400971023     C     W001A         ANDNE     *BLANK
065500971021     C                   SETON                                        32
065600971021     C                   MOVE      *ZERO         VABZNC
065700010604     C                   eval      vinmsg = %trimr(vinmsg)
065800010604     C                             + ' ' + 'VHBZNC'
065900971021     C                   ELSE
066000971021     C                   MOVE      VHBZNC        VABZNC
066100971021     C                   ENDIF
066200971021     C*
066300971023     C                   TESTN                   VHBDCR               30
066400971023     C  N30              TESTN                   VHBDCR                 30
066500971023     C  N30              TESTN                   VHBDCR                   30
066600971021     C                   MOVE      VHBDCR        W001A             1
066700971021     C     *IN30         IFEQ      *OFF
066800971021     C     W001A         ORLT      '0'
066900971023     C     W001A         ANDNE     *BLANK
067000971021     C                   SETON                                        32
067100971021     C                   MOVE      *ZERO         VABDCR
067200010604     C                   eval      vinmsg = %trimr(vinmsg)
067300010604     C                             + ' ' + 'VHBDCR'
067400971021     C                   ELSE
067500971021     C                   MOVE      VHBDCR        VABDCR
067600971021     C                   ENDIF
067700971021     C*
067800971023     C                   TESTN                   VHBHCR               30
067900971023     C  N30              TESTN                   VHBHCR                 30
068000971023     C  N30              TESTN                   VHBHCR                   30
068100971021     C                   MOVE      VHBHCR        W001A             1
068200971021     C     *IN30         IFEQ      *OFF
068300971021     C     W001A         ORLT      '0'
068400971023     C     W001A         ANDNE     *BLANK
068500971021     C                   SETON                                        32
068600971021     C                   MOVE      *ZERO         VABHCR
068700010604     C                   eval      vinmsg = %trimr(vinmsg)
068800010604     C                             + ' ' + 'VHBHCR'
068900971021     C                   ELSE
069000971021     C                   MOVE      VHBHCR        VABHCR
069100971021     C                   ENDIF
069200941229     C*
069300971021     C                   MOVE      *ZERO         VABVMD
069400971021     C                   MOVE      VHBVMD        WASC15
069500010719     C                   IF        WI15 = *blanks
069600010719     C                   EVAL      WI15 = *zeros
069700010719     C                   ENDIF
069800971023     C                   TESTN                   WI15                 30
069900971023     C  N30              TESTN                   WI15                   30
070000971023     C  N30              TESTN                   WI15                     30
070100971021     C                   MOVE      WI15          W001A             1
070200971021     C     *IN30         IFEQ      *OFF
070300971021     C     W001A         ORLT      '0'
070400971023     C     W001A         ANDNE     *BLANK
070500971021     C                   SETON                                        32
070600971021     C                   MOVE      *ZERO         VABVMD
070700010604     C                   eval      vinmsg = %trimr(vinmsg)
070800010604     C                             + ' ' + 'VHBVMD'
070900971021     C                   ELSE
071000971021     C                   MOVEL     WI15          VABVMD
071100010719     C                   IF        WD15 = *blanks
071200010719     C                   EVAL      WD15 = *zeros
071300010719     C                   ENDIF
071400971023     C                   TESTN                   WD15                 30
071500971023     C  N30              TESTN                   WD15                   30
071600971023     C  N30              TESTN                   WD15                     30
071700971021     C                   MOVE      WD15          W001A             1
071800971021     C     *IN30         IFEQ      *OFF
071900971021     C     W001A         ORLT      '0'
072000971023     C     W001A         ANDNE     *BLANK
072100971021     C                   SETON                                        32
072200971021     C                   MOVE      *ZERO         VABVMD
072300010604     C                   eval      vinmsg = %trimr(vinmsg)
072400010604     C                             + ' ' + 'VHBVMD'
072500971021     C                   ELSE
072600971021     C                   MOVE      WD15          VABVMD
072700971021     C                   ENDIF
072800050118     C                   ENDIF
072900971021     C*
073000971023     C                   TESTN                   VHBANT               30
073100971023     C  N30              TESTN                   VHBANT                 30
073200971023     C  N30              TESTN                   VHBANT                   30
073300971021     C                   MOVE      VHBANT        W001A             1
073400971021     C     *IN30         IFEQ      *OFF
073500971021     C     W001A         ORLT      '0'
073600971023     C     W001A         ANDNE     *BLANK
073700971021     C                   SETON                                        32
073800971021     C                   MOVE      *ZERO         VABANT
073900010604     C                   eval      vinmsg = %trimr(vinmsg)
074000010604     C                             + ' ' + 'VHBANT'
074100971021     C                   ELSE
074200971021     C                   MOVE      VHBANT        VABANT
074300971021     C                   ENDIF
074400971021     C*
074500971021     C* CAMPI ALFABETICI:
074600971021     C                   MOVE      VHBATB        VABATB
074700971021     C                   MOVE      VHBCBO        VABCBO
074800971021     C                   MOVE      VHBRSD        VABRSD
074900020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
075000020117     C     '@':'A'       XLATE     VABRSD        VABRSD
075100020117     C* ==
075200050722     C***                MOVE      VHBRD2        VABRD2
075300971021     C                   MOVE      VHBIND        VABIND
075400971021     C                   MOVE      VHBCAD        VABCAD
075500971021     C                   MOVE      VHBLOD        VABLOD
075600971021     C                   MOVE      VHBPRD        VABPRD
075700971021     C                   MOVE      VHBNZD        VABNZD
075800971021     C                   MOVE      VHBGC1        VABGC1
075900971021     C                   MOVE      VHBGC2        VABGC2
076000971021     C                   MOVE      VHBTSP        VABTSP
076100971021     C                   MOVE      VHBVAS        VABVAS
076200971021     C                   MOVE      VHBNAS        VABNAS
076300971021     C                   MOVE      VHBTIC        VABTIC
076400971021     C                   MOVE      VHBVCA        VABVCA
076500971021     C                   MOVE      VHBGCA        VABGCA
076600971021     C                   MOVE      VHBRMA        VABRMA
076700971021     C                   MOVE      VHBXCO        VABXCO
076800971021     C                   MOVE      VHBNOT        VABNOT
076900971021     C                   MOVE      VHBNT2        VABNT2
077000971021     C                   MOVE      VHBCTM        VABCTM
077100971021     C                   MOVE      VHBFFD        VABFFD
077200971021     C                   MOVE      VHBTCR        VABTCR
077300971021     C                   MOVE      VHBCTS        VABCTS
077400971021     C                   MOVE      VHBFTM        VABFTM
077500971021     C                   MOVE      VHBVAD        VABVAD
077600971021     C                   MOVE      VHBGMA        VABGMA
077700971021     C                   MOVE      VHBGGA        VABGGA
077800971021     C                   MOVE      VHBGVA        VABGVA
077900971021     C                   MOVE      VHBTC1        VABTC1
078000971021     C                   MOVE      VHBTC2        VABTC2
078100971021     C                   MOVE      VHBSCL        VABSCL
078200971021     C                   MOVE      VHBRMO        VABRMO
078300971021     C                   MOVE      VHBCMO        VABCMO
078400971021     C                   MOVE      VHBNMO        VABNMO
078500971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
078600971021     C* NELLA NAZIONE ORIGINALE
078700971021     C                   MOVE      VHBNMO        WA1D              1
078800971021     C                   MOVEL     VHBNMO        WA1S              1
078900971021     C     WA1D          IFNE      *BLANK
079000971021     C     WA1S          ANDEQ     *BLANK
079100971021     C                   MOVE      *BLANK        VABNMO
079200971021     C                   END
079300050418     C*
079400050418     C* Effettuo alcuine considerazioni sulla nazione: se è I / ITA / IT => forzo a *blanks
079500050418     C                   IF        %trim(VABNZD) = 'I'   OR
079600050418     C                             %trim(VABNZD) = 'IT'  OR
079700050418     C                             %trim(VABNZD) = 'ITA'
079800050418     C                   EVAL      VABNZD = *blanks
079900050418     C                   ENDIF
080000041025     C*
080100041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
080200041025     C                   IF        *IN52 = *ON
080300041025     C* Reperisco la provincia dal CAP e dalla località
080400041025     C                   IF        VABCAD <> *blanks AND
080500041025     C                             VABPRD  = *blanks
080600041025     C                   CLEAR                   TISI95DS
080700041025     C                   EVAL      I95TCN = '3'
080800041025     C                   Z-ADD     datcor        I95DAT
080900041025     C                   EVAL      I95CAP = VABCAD
081000041025     C                   EVAL      I95LOC = VABLOD
081100041025     C                   CALL      'TISI95R'
081200041025     C                   PARM                    TISI95DS
081300041025     C                   EVAL      VABPRD = O95PRV
081400041025     C                   ENDIF
081500041025     C                   ENDIF
081600040630     C*
081700040630     C* "bonifico" i dati d input da eventuali caratteri indesiderati
081800040630     C                   exsr      exeBon
081900930409     C*
082000050722     C  N31              ADD       1             §CTROKVB
082100050722     C   32              ADD       1             §CTRMO
082200050722     C   31              ADD       1             §CTRNO
082300020913     C  N31              WRITE     FIVAB000
082400050722     C  N31              exsr      wrivat
082500990910     C*
082600010601     C                   if        *in31 = *off and
082700010601     C                             *in32 = *off
082800990910     C                   eval      vinflg = '1'
082900990910     C                   else
083000000621     C                   eval      recko = vindta
083100010612     C                   if        %open(tis7t5ps)
083200030407     C   50              except    rigadett
083300010612     C                   endif
083400990910     C                   eval      vinflg = '2'
083500990910     C                   endif
083600991022     C                   endif
083700990910     C                   endif
083800010601     C*
083900010601     C                   endif
084000000905     C*
084100000905     C                   else
084200000905     C                   eval      vinflg = '1'
084300000905     C                   endif
084400000905     C*
084500000905     C  N70              update    tivin000
084600991022     C*
084700991022     C  N70              ENDdo
084800050722     C*
084900050722     C                   endif                                                  (endif - chkcall)
085000010530     C*
085100010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
085200010530     C                   if        cntNonEl = *zeros or
085300010530     C                             flgMulti = '0'
085400990910     C* Se non ci sono record con errori ...
085500010530     C                   if        §ctrno = 0 and
085600010601     C                             §ctrmo = 0 and
085700010530     C                             flgStato <> '2'
085800990910     C* ... restituisco esito OK.
085900990921     C                   eval      wrkesito = '0'
086000990910     C                   else
086100050722     C                   if        §ctrokvb > 0
086200990921     C                   eval      wrkesito = '1'
086300000710     C                   else
086400010615     C                   if        flgOk = '0'
086500010615     C                   eval      wrkesito = '2'
086600010615     C                   else
086700010615     C                   eval      wrkesito = '6'
086800010615     C                   endif
086900990910     C                   endif
087000000710     C                   endif
087100010530     C                   else
087200010530     C                   eval      wrkesito = '9'
087300010530     C                   endif
087400990910     C*
087500990914     C                   if        %open(tivin00r)
087600990908     C                   close     tivin00r
087700990914     C                   endif
087800020913     C                   if        %open(fivabwwr)
087900020913     C                   close     fivabwwr
088000990914     C                   endif
088100050722     C                   if        %open(fivatwwr)
088200050722     C                   close     fivatwwr
088300050722     C                   endif
088400010530     C*
088500010530     C                   if        vlrpoi <> 999
088600010530     C                   eval      invfil = vlrpoi
088700010530     C                   endif
088800990910     C*
088900050722     C                   if        §ctrokvb > 0
089000010530     C                             and invfil > *zeros
089100000613     C                   exsr      invio
089200990920     C                   endif
089300010612     C*
089400010612     C                   if        flgGiro = '1'
089500010612     C                   exsr      endela
089600010612     C                   endif
089700990920     C*
089800910830     C                   ENDSR
089900000613     C***
090000050722
090100050722
090200050722
090300050722     C*----------------------------------------------------*
090400050722     C*  SCARICAMENTO BUFFER RECORDS VAT
090500050722     C*----------------------------------------------------*
090600050722     C     WRIVAT        BEGSR
090700050722     C*
090800050722     C* Attribuisco la stessa chiave della testata bolle
090900050722     C                   EVAL      VATAAS = VABAAS
091000050722     C                   EVAL      VATLNP = VABLNP
091100050722     C                   EVAL      VATNRS = VABNRS
091200050722     C                   EVAL      VATNSP = VABNSP
091300050722     C                   EVAL      VATCCM = VABCCM
091400050722     C*
091500050722     C* Imposto il tipo record sul file VAT a 'B' x indicare il telefono destinatario
091600050722     C                   EVAL      VATTRC = 'B'
091700050722     C                   EVAL      VATNOT = VHBRD2
091800050722     C*
091900050722     C* Quindi se valorizzato il VATNOT scarico il buffer e incremento il contatore x il CMZ
092000050722     C                   IF        VATNOT <> *blanks
092100050722     C                   WRITE     FIVAT000
092200050722     C                   ADD       1             §CTROKVT
092300050722     C                   ENDIF
092400050722     C*
092500050722     C                   ENDSR
092600050722
092700050722
092800050722
092900050722     C*----------------------------------------------------*
093000050722     C*  ESECUZIONE OPERAZIONI PRELIMINARI SU FILE FIVATWWR
093100050722     C*----------------------------------------------------*
093200050722     C     PREVAT        BEGSR
093300050722     C*
093400050722     C* Compongo il nome del membro da dare al FIVATWWR
093500050722     C                   eval      parmbr = vlrhdl
093600050722     C                   movel     'M'           parmbr
093700050722     C                   eval      parccm = vlrksc
093800050722     C                   eval      paropz = '1'
093900050722     C* Effettuo la chiamata al CLLE preposto
094000050722     C                   call(e)   'TITVVTC'
094100050722     C                   parm                    parccm
094200050722     C                   parm                    parmbr
094300050722     C                   parm                    paropz
094400050722     C* Testo eventuali errori che in questa fase sono assolutamente bloccanti
094500050722     C                   if        %error
094600050722     C                   movel     '1'           chkcall
094700050722     C                   else
094800050722     C                   movel     '0'           chkcall
094900050722     C                   endif
095000050722     C*
095100050722     C                   ENDSR
095200990920
095300010530
095400010530
095500010530
095600010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
095700010530     C     repfil        BEGSR
095800010530     C*
095900010530     C                   if        invfil = *zeros and
096000010530     C                             depfil > *zeros and
096100010613     C                             (vinflg = *blanks or
096200010613     C                              vinflg = *zeros)
096300010530     C
096400010530     C                   eval      invfil = depfil
096500010530     C                   endif
096600010530     C*
096700010530     C                   if        depfil <> invfil and
096800010530     C                             invfil > *zeros
096900010530     C                   eval      flgMulti = '1'
097000010530     C                   if        vinflg = *blanks
097100010530     C                   add       1             cntNonEl
097200010530     C                   endif
097300010530     C                   endif
097400010530     C*
097500010530     C                   if        vinflg = '2'
097600010530     C                   eval      flgStato = '2'
097700010530     C                   endif
097800010530     C*
097900010530     C                   ENDSR
098000010530     C***
098100010530
098200010530
098300010530
098400010530
098500990920      /TITLE Invio dei dati al punto operativo.
098600000613     C     invio         BEGSR
098700050722     C*
098800050722     C* 1° invio FIVAT
098900050722     C                   reset                   dscmz
099000050722     C                   move      vlrpoi        cmzdst
099100050722     C                   eval      cmzfld = 'FIVATWWR'
099200050722     C                   eval      cmzmbd = vlrhdl
099300050722     C                   eval      %subst(cmzmbd:1:1) = 'M'
099400050722     C***                if        prmfir = *blanks
099500050722     C                   eval      cmzfla = 'FIVAT00F'
099600050722     C                   eval      cmzmba = 'FIVAT00F'
099700050722     C***                else
099800050722     C***                eval      cmzfla = prmfir
099900050722     C***                eval      cmzmba = prmfir
100000050722     C***                endif
100100050722     C                   eval      cmznrr = *zeros
100200050722     C                   move      §ctrokvt      cmznrr
100300050722     C                   eval      cmzlba = vlrfl1
100400050722     C                   call(e)   'TIS711C'
100500050722     C                   parm                    dscmz
100600050722     C                   parm      *blanks       esito
100700050722     C                   if        %error
100800050722     C                             or cmzerr = '1'
100900050722     C                             or esito  = '1'
101000050722     C                   eval      wrkesito = '3'
101100050722     C                   else
101200050722     C*
101300050722     C* 2° invio FIVAB
101400050722     C                   reset                   dscmz
101500050722     C                   move      vlrpoi        cmzdst
101600050722     C                   eval      cmzfld = vlrfou
101700050722     C                   eval      cmzmbd = vlrhdl
101800050722     C                   eval      %subst(cmzmbd:1:1) = 'M'
101900050722     C***                if        prmfir = *blanks
102000050722     C                   eval      cmzfla = 'FIVAB00F'
102100050722     C                   eval      cmzmba = 'FIVAB00F'
102200050722     C***                else
102300050722     C***                eval      cmzfla = prmfir
102400050722     C***                eval      cmzmba = prmfir
102500050722     C***                endif
102600050722     C                   eval      cmznrr = *zeros
102700050722     C                   move      §ctrokvb      cmznrr
102800050722     C                   eval      cmzlba = vlrfl1
102900050722     C                   call(e)   'TIS711C'
103000050722     C                   parm                    dscmz
103100050722     C                   parm      *blanks       esito
103200050722     C                   if        %error
103300050722     C                             or cmzerr = '1'
103400050722     C                             or esito  = '1'
103500050722     C                   eval      wrkesito = '3'
103600050722     C                   endif
103700050722     C                   endif
103800990920     C*
103900000613     C                   ENDSR
104000000613     C***
104100010530
104200010530
104300010530
104400010530
104500010530      /TITLE Invio dei dati al punto operativo.
104600010530     C     opeini        BEGSR
104700010530     C*
104800010530     C* Inizializzo flag e contatori operativi
104900010530     C                   movel     '0'           flgGiro           1
105000010530     C                   movel     '0'           flgMulti          1
105100010530     C                   movel     '1'           flgStato          1
105200010615     C                   movel     '0'           flgOk             1
105300010530     C                   z-add     *zeros        cntNonEl         10 0
105400010530     C                   z-add     *zeros        depfil            3 0
105500010530     C                   z-add     *zeros        invfil            3 0
105600030407     C*
105700030407     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
105800030407     C                   if        %subst(vlrppt:1:1) = 'S'
105900030407     C                   seton                                        50
106000030407     C                   else
106100030407     C                   setoff                                       50
106200030407     C                   endif
106300041001     C*
106400041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
106500041001     C                   if        %subst(vlrppt:2:1) = 'S'
106600041001     C                   seton                                        51
106700041001     C                   else
106800041001     C                   setoff                                       51
106900041001     C                   endif
107000041025     C*
107100041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
107200041025     C* (naturalmente solo se nn già presente)
107300041025     C                   if        %subst(vlrppt:3:1) = 'S'
107400041025     C                   seton                                        52
107500041025     C                   else
107600041025     C                   setoff                                       52
107700041025     C                   endif
107800010530     C*
107900010530     C                   ENDSR
108000010530     C***
108100040407
108200040407
108300040407
108400040407
108500040407      /TITLE Routine x chiamata *pgm utilità d "bonifica" stringa
108600040407     C     exeBon        BEGSR
108700040407     C*
108800040407     C* Imposto i parametri x il *pgm d "bonifica"
108900040407     C                   EVAL      ElencoChar = *blanks
109000040407     C                   EVAL      TipoElenco = *blanks
109100040407     C                   EVAL      CharSost   = *blanks
109200040629     C                   EVAL      UpperCase  = '1'
109300040407     C                   EVAL      ChkNull    = '1'
109400040407     C                   EVAL      CharNull   = *blanks
109500040629     C*
109600040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABRSD
109700040629     C                   EVAL      TxtInOut   = VABRSD
109800040629     C                   EVAL      EsitoBon   = *blanks
109900040407     C*
110000040407     C                   CALL      'XCHKCHAR'
110100040407     C                   PARM                    TxtInOut
110200040407     C                   PARM                    ElencoChar
110300040407     C                   PARM                    TipoElenco
110400040407     C                   PARM                    CharSost
110500040407     C                   PARM                    UpperCase
110600040407     C                   PARM                    ChkNull
110700040407     C                   PARM                    CharNull
110800040407     C                   PARM                    EsitoBon
110900040407     C*
111000040407     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
111100040407     C                   IF        EsitoBon = '1'
111200040629     C                   EVAL      VABRSD = TxtInOut
111300040407     C                   ENDIF
111400040629     C*
111500040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABIND
111600040629     C                   EVAL      TxtInOut   = VABIND
111700040629     C                   EVAL      EsitoBon   = *blanks
111800040629     C*
111900040629     C                   CALL      'XCHKCHAR'
112000040629     C                   PARM                    TxtInOut
112100040629     C                   PARM                    ElencoChar
112200040629     C                   PARM                    TipoElenco
112300040629     C                   PARM                    CharSost
112400040629     C                   PARM                    UpperCase
112500040629     C                   PARM                    ChkNull
112600040629     C                   PARM                    CharNull
112700040629     C                   PARM                    EsitoBon
112800040629     C*
112900040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
113000040629     C                   IF        EsitoBon = '1'
113100040629     C                   EVAL      VABIND = TxtInOut
113200040629     C                   ENDIF
113300040629     C*
113400040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABLOD
113500040629     C                   EVAL      TxtInOut   = VABLOD
113600040629     C                   EVAL      EsitoBon   = *blanks
113700040629     C*
113800040629     C                   CALL      'XCHKCHAR'
113900040629     C                   PARM                    TxtInOut
114000040629     C                   PARM                    ElencoChar
114100040629     C                   PARM                    TipoElenco
114200040629     C                   PARM                    CharSost
114300040629     C                   PARM                    UpperCase
114400040629     C                   PARM                    ChkNull
114500040629     C                   PARM                    CharNull
114600040629     C                   PARM                    EsitoBon
114700040629     C*
114800040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
114900040629     C                   IF        EsitoBon = '1'
115000040629     C                   EVAL      VABLOD = TxtInOut
115100040629     C                   ENDIF
115200040629     C*
115300040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNOT
115400040629     C                   EVAL      TxtInOut   = VABNOT
115500040629     C                   EVAL      EsitoBon   = *blanks
115600040630     C                   EVAL      UpperCase  = *blanks
115700040629     C*
115800040629     C                   CALL      'XCHKCHAR'
115900040629     C                   PARM                    TxtInOut
116000040629     C                   PARM                    ElencoChar
116100040629     C                   PARM                    TipoElenco
116200040629     C                   PARM                    CharSost
116300040629     C                   PARM                    UpperCase
116400040629     C                   PARM                    ChkNull
116500040629     C                   PARM                    CharNull
116600040629     C                   PARM                    EsitoBon
116700040629     C*
116800040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
116900040629     C                   IF        EsitoBon = '1'
117000040629     C                   EVAL      VABNOT = TxtInOut
117100040629     C                   ENDIF
117200040629     C*
117300040629     C* Effettuo la chiamata al *pgm d "bonifica" x ili campo VABNT2
117400040629     C                   EVAL      TxtInOut   = VABNT2
117500040629     C                   EVAL      EsitoBon   = *blanks
117600040630     C                   EVAL      UpperCase  = *blanks
117700040629     C*
117800040629     C                   CALL      'XCHKCHAR'
117900040629     C                   PARM                    TxtInOut
118000040629     C                   PARM                    ElencoChar
118100040629     C                   PARM                    TipoElenco
118200040629     C                   PARM                    CharSost
118300040629     C                   PARM                    UpperCase
118400040629     C                   PARM                    ChkNull
118500040629     C                   PARM                    CharNull
118600040629     C                   PARM                    EsitoBon
118700040629     C*
118800040629     C* Se modificata stringa input re-imposto con la stringa così "bonificata"
118900040629     C                   IF        EsitoBon = '1'
119000040629     C                   EVAL      VABNT2 = TxtInOut
119100040629     C                   ENDIF
119200040407     C*
119300040407     C                   ENDSR
119400010530
119500010530
119600010530
119700990910
119800000613     C     *inzsr        BEGSR
119900990910     C*
120000990910     C     *entry        plist
120100990920     C                   parm                    tivlrds
120200990921     C                   parm      wrkesito      esito
120300000724     C                   parm                    prmlit
120400000710     C                   parm                    prmfir
120500010606     C*
120600010606     C* CALCOLA LA DATA CORRENTE
120700010606     C                   time                    wn14             14 0
120800010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
120900010606     C                   z-add     wn8           g08dat
121000010606     C                   z-add     *zeros        g08inv
121100010606     C                   movel     '0'           g08err
121200010606     C                   call      'XSRDA8'
121300010606     C                   parm                    wlbda8
121400010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
121500000613     C*
121600000613     C                   ENDSR
121700000613     C***
121800990908
121900010601     Otis7t5p   E            riepilogo         2
122000990915     O                                              'Upload via Internet'
122100990915     O                                           +1 'Traduzione TIVIN00R -'
122200020913     O                                           +1 'FIVABWWR'
122300990915     O                                           +1 'Testate bolle'
122400990915     O          E            riepilogo   2
122500990915     O                       wrkdata
122600990915     O                       wrkora              +1
122700990915     O                       procname            +1
122800990915     O          E            riepilogo   2
122900990915     O                                              'Cliente..................:'
123000990915     O                       VABCCM        z     +1
123100990915     O          E            riepilogo   2
123200990920     O                                              'Riferimento Strategi.....:'
123300990920     O                       vlrhdl              +1
123400990915     O          E            riepilogo   2
123500990915     O                                              'Giusti...................:'
123600050722     O                       §CTROKVB      2   +  1
123700990915     O          E            riepilogo   2
123800990915     O                                              'Sbagliati e corretti.....:'
123900971022     O                       §CTRMO        2   +  1
124000990915     O          E            riepilogo   2
124100990915     O                                              'Sbagliati e scartati.....:'
124200971022     O                       §CTRNO        2   +  1
124300000613
124400010601     Otis7t5ps  E            testdett          2
124500000613     O                                              'Upload via Internet'
124600000613     O                                           +1 'Traduzione TIVIN00R -'
124700020913     O                                           +1 'FIVABWWR'
124800000613     O                                           +1 'Testate bolle'
124900000616     O          E            testdett    3
125000000613     O                                           +2 'N° rec'
125100000613     O                                           +3 'Anteprima contenuto'
125200000616     O          E            rigadett    2
125300000613     O                       rrnum               +2
125400000621     O                       recko               +3
125500000616     O          E            findett     2
125600000613     O                       wrkdata
125700000613     O                       wrkora              +1
125800000613     O                       procname            +1
125900000616     O          E            findett     2
126000000613     O                                              'Cliente..................:'
126100000613     O                       VABCCM        z     +1
126200000616     O          E            findett     2
126300000613     O                                              'Riferimento Strategi.....:'
126400000613     O                       vlrhdl              +1
126500000616     O          E            findett     2
126600000613     O                                              'Giusti...................:'
126700050722     O                       §CTROKVB      2   +  1
126800000616     O          E            findett     2
126900000613     O                                              'Sbagliati e corretti.....:'
127000000613     O                       §CTRMO        2   +  1
127100000616     O          E            findett     2
127200000613     O                                              'Sbagliati e scartati.....:'
127300000613     O                       §CTRNO        2   +  1
127400000512** CMD - COMANDI CL
127500011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T5P)  FORMTYPE(RICCLI) OUTQ(
127600011123OVRPRTF  OVRSCOPE(*CALLLVL) FILE(TIS7T5PS) FORMTYPE(RICCLI) OUTQ(
127700010601DLTOVR FILE(TIS7T5P TIS7T5PS) LVL(*)
127800000512
127900000512
