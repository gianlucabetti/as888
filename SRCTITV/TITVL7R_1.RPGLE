000100030616      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200990908     H dftactgrp(*yes)
000300990908
000400000724     Fazorg01l  if   e           k disk
000500990910     Ftivin00r  uF   E             DISK    usropn
000600030616     FEDIVABWR  O    E             DISK    usropn
000700030710     Ftis7tebp  O    f  132        PRINTER usropn
000800000621     F                                     oflind(*inoa)
000900030710     Ftis7tebps O    f  198        PRINTER usropn
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
005400030616     D wNomeFile       s             30    INZ(*blanks)
005500030710     D wCMR            s             35    INZ(*blanks)
005600030710     D wPosDaA         s              2    INZ(*blanks)
005700030710     D wPosDa          s              2  0 INZ(*zeros)
005800030710     D wLungA          s              2    INZ(*blanks)
005900030710     D wLung           s              2  0 INZ(*zeros)
006000010606     D*------------------
006100010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
006200010606     D*------------------
006300010606     D WLBDA8          DS                  INZ
006400010606     D  G08DAT                 1      8  0
006500010606     D  G08INV                 9     16  0
006600010606     D  G08ERR                17     17
006700010606     D  G08TGI                18     22  0
006800010606     D*
006900990908
007000990915     C                   time                    wrkdata
007100990915     C                   time                    wrkora
007200000913     C                   reset                   rrnum
007300990921     C                   reset                   esito
007400990921     C                   reset                   wrkesito
007500010530     C*
007600010530     C                   exsr      opeini
007700010530     C                   exsr      rwvab
007800041025     C*
007900041025     C* Esegue lancio TISI95R solo x chiusura
008000041025     C                   CLEAR                   TISI95DS
008100041025     C                   EVAL      I95TLA = 'C'
008200041025     C                   CALL      'TISI95R'
008300041025     C                   PARM                    TISI95DS
008400010530     C*
008500010530     C                   seton                                        lr
008600010530
008700010530
008800010530
008900010530
009000010530     C*--------------------------------------------------------
009100010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
009200010530     C*--------------------------------------------------------
009300010530     C     PREELA        BEGSR
009400000724     C*
009500000724     C* SE OCCORRE SPEDIRE IN FILIALE
009600010530     C                   if        invfil <> *zeros and
009700010530     C                             flgGiro = '0'
009800010530     C*
009900010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
010000010530     C                   eval      flgGiro = '1'
010100000724     C*
010200000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
010300000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
010400030623     C     VABFGS        chain     azorg01l
010500000724     C                   if        %found
010600000616     C                   movel(p)  CMD(1)        depcmd
010700020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
010800000616     C*
010900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
011000011123     C                   Z-ADD     150           LENGH            15 5
011100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
011200000616     C                   PARM                    depcmd
011300000616     C                   PARM                    LENGH
011400000724     C*
011500000724     C                   endif
011600000616     C*
011700000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
011800000616     C                   movel(p)  CMD(2)        depcmd
011900000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
012000000616     C*
012100000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
012200011123     C                   Z-ADD     150           LENGH            15 5
012300000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
012400000616     C                   PARM                    depcmd
012500000616     C                   PARM                    LENGH
012600000616     C*
012700030710     C                   if        not %open(tis7tebps)
012800030710     C                   open      tis7tebps
012900030407     C   50              except    testdett
013000000613     C                   endif
013100010530     C*
013200010530     C                   endif
013300010530     C*
013400010530     C                   ENDSR
013500010530     C***
013600010530
013700010530
013800010530
013900010530
014000010530
014100010530
014200010530     C*--------------------------------------------------------
014300010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
014400010530     C*--------------------------------------------------------
014500010530     C     ENDELA        BEGSR
014600010530     C*
014700010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
014800000613     C*
014900030710     C                   if        %open(tis7tebps)
015000030407     C   50              except    findett
015100030710     C                   close     tis7tebps
015200000613     C                   endif
015300000616     C*
015400000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
015500011123     C                   Z-ADD     150           LENGH            15 5
015600010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
015700000616     C                   PARM                    CMD(3)
015800000616     C                   PARM                    LENGH
015900010530     C*
016000010530     C                   ENDSR
016100010530     C***
016200000613
016300010530
016400010530
016500000613
016600000613     C*--------------------------------------------------------
016700000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
016800000613     C*--------------------------------------------------------
016900000621     C     STPR          BEGSR
017000000613     C*
017100030710     C                   if        not %open(tis7tebp)
017200030710     C                   open      tis7tebp
017300990915     C                   endif
017400990915     C*
017500030407     C   50              except    riepilogo
017600990915     C*
017700030710     C                   if        %open(tis7tebp)
017800030710     C                   close     tis7tebp
017900990914     C                   endif
018000990910     C*
018100000613     C                   ENDSR
018200000613     C***
018300990908
018400010530
018500010530
018600010530
018700910830     C*--------------------------------------------------------
018800030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
018900910830     C*--------------------------------------------------------
019000941229     C     RWVAB         BEGSR
019100030710     C*
019200030710     C                   clear                   §CTROK
019300030710     C                   clear                   §CTRMO
019400030710     C                   clear                   §CTRNO
019500990910     C*
019600030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
019700030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
019800030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
019900030710     C                   if        *in60 = *on
020000990914     C                   if        not %open(tivin00r)
020100990908     C                   open      tivin00r
020200990914     C                   endif
020300030616     C                   if        not %open(edivabwr)
020400030616     C                   open      edivabwr
020500990914     C                   endif
020600990910     C*
020700921023     C                   DO        *HIVAL
020800990913     C*
020900990915     C                   READ      tivin00r                               70
021000010618     C*
021100010618     C* Dopo ogni lettura verifico se ci sono stati record OK
021200010618     C                   if        vinflg = '1'
021300010618     C                   eval      flgOk = '1'
021400010618     C                   endif
021500010705     C*
021600010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
021700010705     C                   if        %subst(vindta:1:1) = '='
021800010705     C                   eval      %subst(vindta:1:1) = *blanks
021900010705     C                   endif
022000010705     C*
022100000905     C                   if        vindta > *blanks
022200000613     C                   add       1             rrnum
022300010601     C* ...elimino eventuali segni
022400010601     C     '+':' '       xlate     vindta        vindta
022500990913     C*
022600010601     C                   if        *in70 = *off and
022700990913     C                             (vinflg = *blanks
022800010711     C                              or vinflg = '0'
022900010711     C                              or vinflg = '2')
023000990913     C*
023100030616     C                   clear                   EDIVAB00
023200010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
023300010711     C                   if        vinflg = *blanks or vinflg = '0'
023400010711     C                   clear                   vinmsg
023500010711     C                   endif
023600010711     C*
023700990913     C                   eval      fnvabaar = vindta
023800991022     C*
023900991022     C                   testb     x'3F'         vhbatb                   02
024000991022     C                   if        not *in02
024100991022     C*
024200971021     C                   SETOFF                                       303132
024300971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
024400971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
024500971023     C*    OPPURE TUTTI BLANKS
024600010530     C*
024700010530     C                   TESTN                   VHBLNP               30
024800010530     C  N30              TESTN                   VHBLNP                 30
024900010530     C  N30              TESTN                   VHBLNP                   30
025000010530     C                   MOVE      VHBLNP        W001A             1
025100010530     C     *IN30         IFEQ      *OFF
025200010530     C     W001A         ORLT      '0'
025300010530     C                   SETON                                        31
025400010530     C                   eval      vinmsg = %trimr(vinmsg)
025500010530     C                             + ' ' + 'VHBLNP'
025600010530     C                   ELSE
025700010530     C                   MOVE      VHBLNP        VABLNP
025800010530     C     VABLNP        IFLE      *ZEROS
025900010530     C                   SETON                                        31
026000010530     C                   eval      vinmsg = %trimr(vinmsg)
026100010530     C                             + ' ' + 'VHBLNP'
026200010530     C                   ENDIF
026300010530     C                   ENDIF
026400010530     C*
026500010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
026600010530     C                   eval      depfil = VABLNP
026700010530     C                   exsr      repfil
026800010601     C                   if        depfil = invfil
026900021014     C                   if        vlrpoi = 999
027000021010     C                   MOVE(P)   invfil        VABFGS
027100021014     C                   else
027200021014     C                   MOVE(P)   vlrpoi        VABFGS
027300021014     C                   endif
027400010530     C*
027500010530     C                   exsr      PREELA
027600010530     C*
027700971023     C                   TESTN                   VHBCCM               30
027800971023     C  N30              TESTN                   VHBCCM                 30
027900971023     C  N30              TESTN                   VHBCCM                   30
028000971021     C                   MOVE      VHBCCM        W001A             1
028100971021     C     *IN30         IFEQ      *OFF
028200971021     C     W001A         ORLT      '0'
028300971021     C                   SETON                                        31
028400010604     C                   eval      vinmsg = %trimr(vinmsg)
028500010604     C                             + ' ' + 'VHBCCM'
028600971021     C                   ELSE
028700971021     C                   MOVE      VHBCCM        VABCCM
028800971024     C     VABCCM        IFLE      *ZEROS
028900971024     C                   SETON                                        31
029000010604     C                   eval      vinmsg = %trimr(vinmsg)
029100010604     C                             + ' ' + 'VHBCCM'
029200971024     C                   ENDIF
029300971021     C                   ENDIF
029400971021     C*
029500971023     C                   TESTN                   VHBAAS               30
029600971023     C  N30              TESTN                   VHBAAS                 30
029700971023     C  N30              TESTN                   VHBAAS                   30
029800971021     C                   MOVE      VHBAAS        W001A             1
029900971021     C     *IN30         IFEQ      *OFF
030000971021     C     W001A         ORLT      '0'
030100971023     C     W001A         ANDNE     *BLANK
030200971021     C                   SETON                                        32
030300010606     C                   MOVEL     datcor        VABAAS
030400010604     C                   eval      vinmsg = %trimr(vinmsg)
030500010604     C                             + ' ' + 'VHBAAS'
030600971021     C                   ELSE
030700971021     C                   MOVE      VHBAAS        VABAAS
030800971021     C                   ENDIF
030900971021     C*
031000971023     C                   TESTN                   VHBMGS               30
031100971023     C  N30              TESTN                   VHBMGS                 30
031200971023     C  N30              TESTN                   VHBMGS                   30
031300971021     C                   MOVE      VHBMGS        W001A             1
031400971021     C     *IN30         IFEQ      *OFF
031500971021     C     W001A         ORLT      '0'
031600971023     C     W001A         ANDNE     *BLANK
031700971021     C                   SETON                                        32
031800010601     C                   MOVE      '0101'        VABMGS
031900010604     C                   eval      vinmsg = %trimr(vinmsg)
032000010604     C                             + ' ' + 'VHBMGS'
032100971021     C                   ELSE
032200971021     C                   MOVE      VHBMGS        VABMGS
032300971021     C                   ENDIF
032400971021     C*
032500971023     C                   TESTN                   VHBNRS               30
032600971023     C  N30              TESTN                   VHBNRS                 30
032700971023     C  N30              TESTN                   VHBNRS                   30
032800971021     C                   MOVE      VHBNRS        W001A             1
032900971021     C     *IN30         IFEQ      *OFF
033000971021     C     W001A         ORLT      '0'
033100971023     C     W001A         ANDNE     *BLANK
033200971021     C                   SETON                                        32
033300971021     C                   MOVE      *ZERO         VABNRS
033400010604     C                   eval      vinmsg = %trimr(vinmsg)
033500010604     C                             + ' ' + 'VHBNRS'
033600971021     C                   ELSE
033700971021     C                   MOVE      VHBNRS        VABNRS
033800971021     C                   ENDIF
033900971021     C*
034000971023     C                   TESTN                   VHBNSP               30
034100971023     C  N30              TESTN                   VHBNSP                 30
034200971023     C  N30              TESTN                   VHBNSP                   30
034300971021     C                   MOVE      VHBNSP        W001A             1
034400971021     C     *IN30         IFEQ      *OFF
034500971021     C     W001A         ORLT      '0'
034600971023     C     W001A         ANDNE     *BLANK
034700971021     C                   SETON                                        32
034800971021     C                   MOVE      *ZERO         VABNSP
034900010604     C                   eval      vinmsg = %trimr(vinmsg)
035000010604     C                             + ' ' + 'VHBNSP'
035100971021     C                   ELSE
035200971021     C                   MOVE      VHBNSP        VABNSP
035300971021     C                   ENDIF
035400971021     C*
035500971023     C                   TESTN                   VHBLNA               30
035600971023     C  N30              TESTN                   VHBLNA                 30
035700971023     C  N30              TESTN                   VHBLNA                   30
035800971021     C                   MOVE      VHBLNA        W001A             1
035900971021     C     *IN30         IFEQ      *OFF
036000971021     C     W001A         ORLT      '0'
036100971023     C     W001A         ANDNE     *BLANK
036200971021     C                   SETON                                        32
036300971021     C                   MOVE      *ZERO         VABLNA
036400010604     C                   eval      vinmsg = %trimr(vinmsg)
036500010604     C                             + ' ' + 'VHBLNA'
036600971021     C                   ELSE
036700971021     C                   MOVE      VHBLNA        VABLNA
036800971021     C                   ENDIF
036900971021     C*
037000971023     C                   TESTN                   VHBCTR               30
037100971023     C  N30              TESTN                   VHBCTR                 30
037200971023     C  N30              TESTN                   VHBCTR                   30
037300971021     C                   MOVE      VHBCTR        W001A             1
037400971021     C     *IN30         IFEQ      *OFF
037500971021     C     W001A         ORLT      '0'
037600971023     C     W001A         ANDNE     *BLANK
037700971021     C                   SETON                                        32
037800971021     C                   MOVE      *ZERO         VABCTR
037900010604     C                   eval      vinmsg = %trimr(vinmsg)
038000010604     C                             + ' ' + 'VHBCTR'
038100971021     C                   ELSE
038200971021     C                   MOVE      VHBCTR        VABCTR
038300971021     C                   ENDIF
038400941229     C*
038500971021     C                   MOVE      *ZERO         VABIAS
038600941229     C                   MOVE      VHBIAS        WASC15
038700971023     C                   TESTN                   WI15                 30
038800971023     C  N30              TESTN                   WI15                   30
038900971023     C  N30              TESTN                   WI15                     30
039000971021     C                   MOVE      WI15          W001A             1
039100971021     C     *IN30         IFEQ      *OFF
039200971021     C     W001A         ORLT      '0'
039300971023     C     W001A         ANDNE     *BLANK
039400971021     C                   SETON                                        32
039500971021     C                   MOVE      *ZERO         VABIAS
039600010604     C                   eval      vinmsg = %trimr(vinmsg)
039700010604     C                             + ' ' + 'VHBIAS'
039800971021     C                   ELSE
039900971021     C                   MOVEL     WI15          VABIAS
040000971023     C                   TESTN                   WD15                 30
040100971023     C  N30              TESTN                   WD15                   30
040200971023     C  N30              TESTN                   WD15                     30
040300971021     C                   MOVE      WD15          W001A             1
040400971021     C     *IN30         IFEQ      *OFF
040500971021     C     W001A         ORLT      '0'
040600971023     C     W001A         ANDNE     *BLANK
040700971021     C                   SETON                                        32
040800971021     C                   MOVE      *ZERO         VABIAS
040900010604     C                   eval      vinmsg = %trimr(vinmsg)
041000010604     C                             + ' ' + 'VHBIAS'
041100971021     C                   ELSE
041200971021     C                   MOVE      WD15          VABIAS
041300971021     C                   ENDIF
041400050118     C                   ENDIF
041500941229     C*
041600971023     C                   TESTN                   VHBNCL               30
041700971023     C  N30              TESTN                   VHBNCL                 30
041800971023     C  N30              TESTN                   VHBNCL                   30
041900971021     C                   MOVE      VHBNCL        W001A             1
042000971021     C     *IN30         IFEQ      *OFF
042100971021     C     W001A         ORLT      '0'
042200971021     C                   SETON                                        32
042300971021     C                   MOVE      *ZERO         VABNCL
042400010604     C                   eval      vinmsg = %trimr(vinmsg)
042500010604     C                             + ' ' + 'VHBNCL'
042600971021     C                   ELSE
042700971021     C                   MOVE      VHBNCL        VABNCL
042800971021     C                   ENDIF
042900941229     C*
043000971021     C                   MOVE      *ZERO         VABPKB
043100971021     C                   MOVE      VHBPKB        WASC09
043200010719     C                   IF        WI09 = *blanks
043300010719     C                   EVAL      WI09 = *zeros
043400010719     C                   ENDIF
043500971023     C                   TESTN                   WI09                 30
043600971023     C  N30              TESTN                   WI09                   30
043700971023     C  N30              TESTN                   WI09                     30
043800971021     C                   MOVE      WI09          W001A             1
043900971021     C     *IN30         IFEQ      *OFF
044000971021     C     W001A         ORLT      '0'
044100971021     C                   SETON                                        32
044200971021     C                   MOVE      *ZERO         VABPKB
044300010604     C                   eval      vinmsg = %trimr(vinmsg)
044400010604     C                             + ' ' + 'VHBPKB'
044500971021     C                   ELSE
044600971021     C                   MOVEL     WI09          VABPKB
044700010719     C                   IF        WD09 = *blanks
044800010719     C                   EVAL      WD09 = *zeros
044900010719     C                   ENDIF
045000971023     C                   TESTN                   WD09                 30
045100971023     C  N30              TESTN                   WD09                     30
045200971021     C                   MOVE      WD09          W001A             1
045300971021     C     *IN30         IFEQ      *OFF
045400971021     C     W001A         ORLT      '0'
045500971023     C     W001A         ANDNE     *BLANK
045600971021     C                   SETON                                        32
045700971021     C                   MOVE      *ZERO         VABPKB
045800010604     C                   eval      vinmsg = %trimr(vinmsg)
045900010604     C                             + ' ' + 'VHBPKB'
046000971021     C                   ELSE
046100971021     C                   MOVE      WD09          VABPKB
046200971021     C                   ENDIF
046300050118     C                   ENDIF
046400971021     C*
046500041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
046600041001     C                   IF        *IN51 = *OFF
046700971021     C                   MOVE      *ZERO         VABVLB
046800971021     C                   MOVE      VHBVLB        WASC07
046900010719     C                   IF        WI07 = *blanks
047000010719     C                   EVAL      WI07 = *zeros
047100010719     C                   ENDIF
047200971023     C                   TESTN                   WI07                 30
047300971023     C  N30              TESTN                   WI07                   30
047400971023     C  N30              TESTN                   WI07                     30
047500971021     C                   MOVE      WI07          W001A             1
047600971021     C     *IN30         IFEQ      *OFF
047700971021     C     W001A         ORLT      '0'
047800971023     C     W001A         ANDNE     *BLANK
047900971021     C                   SETON                                        32
048000971021     C                   MOVE      *ZERO         VABVLB
048100010604     C                   eval      vinmsg = %trimr(vinmsg)
048200010604     C                             + ' ' + 'VHBVLB'
048300971021     C                   ELSE
048400971021     C                   MOVEL     WI07          VABVLB
048500010719     C                   IF        WD07 = *blanks
048600010719     C                   EVAL      WD07 = *zeros
048700010719     C                   ENDIF
048800971023     C                   TESTN                   WD07                 30
048900971023     C  N30              TESTN                   WD07                   30
049000971023     C  N30              TESTN                   WD07                     30
049100971021     C                   MOVE      WD07          W001A             1
049200971021     C     *IN30         IFEQ      *OFF
049300971021     C     W001A         ORLT      '0'
049400971023     C     W001A         ANDNE     *BLANK
049500971021     C                   SETON                                        32
049600971021     C                   MOVE      *ZERO         VABVLB
049700010604     C                   eval      vinmsg = %trimr(vinmsg)
049800010604     C                             + ' ' + 'VHBVLB'
049900971021     C                   ELSE
050000971021     C                   MOVE      WD07          VABVLB
050100971021     C                   ENDIF
050200050118     C                   ENDIF
050300041001     C                   ELSE
050400041001     C* ...altrimenti forzo azzeramento del volume
050500041001     C                   Z-ADD     *zeros        VABVLB
050600041001     C                   ENDIF
050700941229     C*
050800971021     C                   MOVE      *ZERO         VABQFT
050900971021     C                   MOVE      VHBQFT        WASC15
051000010719     C                   IF        WI15 = *blanks
051100010719     C                   EVAL      WI15 = *zeros
051200010719     C                   ENDIF
051300971023     C                   TESTN                   WI15                 30
051400971023     C  N30              TESTN                   WI15                   30
051500971023     C  N30              TESTN                   WI15                     30
051600971021     C                   MOVE      WI15          W001A             1
051700971021     C     *IN30         IFEQ      *OFF
051800971021     C     W001A         ORLT      '0'
051900971023     C     W001A         ANDNE     *BLANK
052000971021     C                   SETON                                        32
052100971021     C                   MOVE      *ZERO         VABQFT
052200010604     C                   eval      vinmsg = %trimr(vinmsg)
052300010604     C                             + ' ' + 'VHBQFT'
052400971021     C                   ELSE
052500971021     C                   MOVEL     WI15          VABQFT
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
053700971021     C                   MOVE      *ZERO         VABQFT
053800010604     C                   eval      vinmsg = %trimr(vinmsg)
053900010604     C                             + ' ' + 'VHBQFT'
054000971021     C                   ELSE
054100971021     C                   MOVE      WD15          VABQFT
054200971021     C                   ENDIF
054300050118     C                   ENDIF
054400941229     C*
054500971021     C                   MOVE      *ZERO         VABCAS
054600971021     C                   MOVE      VHBCAS        WASC15
054700010719     C                   IF        WI15 = *blanks
054800010719     C                   EVAL      WI15 = *zeros
054900010719     C                   ENDIF
055000971023     C                   TESTN                   WI15                 30
055100971023     C  N30              TESTN                   WI15                   30
055200971023     C  N30              TESTN                   WI15                     30
055300971021     C                   MOVE      WI15          W001A             1
055400971021     C     *IN30         IFEQ      *OFF
055500971021     C     W001A         ORLT      '0'
055600971023     C     W001A         ANDNE     *BLANK
055700971021     C                   SETON                                        32
055800971021     C                   MOVE      *ZERO         VABCAS
055900010604     C                   eval      vinmsg = %trimr(vinmsg)
056000010604     C                             + ' ' + 'VHBCAS'
056100971021     C                   ELSE
056200971021     C                   MOVEL     WI15          VABCAS
056300010719     C                   IF        WD15 = *blanks
056400010719     C                   EVAL      WD15 = *zeros
056500010719     C                   ENDIF
056600971023     C                   TESTN                   WD15                 30
056700971023     C  N30              TESTN                   WD15                   30
056800971023     C  N30              TESTN                   WD15                     30
056900971021     C                   MOVE      WD15          W001A             1
057000971021     C     *IN30         IFEQ      *OFF
057100971021     C     W001A         ORLT      '0'
057200971023     C     W001A         ANDNE     *BLANK
057300971021     C                   SETON                                        32
057400971021     C                   MOVE      *ZERO         VABCAS
057500010604     C                   eval      vinmsg = %trimr(vinmsg)
057600010604     C                             + ' ' + 'VHBCAS'
057700971021     C                   ELSE
057800971021     C                   MOVE      WD15          VABCAS
057900971021     C                   ENDIF
058000050118     C                   ENDIF
058100971021     C*
058200971023     C                   TESTN                   VHBRMN               30
058300971023     C  N30              TESTN                   VHBRMN                 30
058400971023     C  N30              TESTN                   VHBRMN                   30
058500971021     C                   MOVE      VHBRMN        W001A             1
058600971021     C     *IN30         IFEQ      *OFF
058700971021     C     W001A         ORLT      '0'
058800971021     C                   SETON                                        32
058900971021     C                   MOVE      *ZERO         VABRMN
059000010604     C                   eval      vinmsg = %trimr(vinmsg)
059100010604     C                             + ' ' + 'VHBRMN'
059200971021     C                   ELSE
059300971021     C                   MOVE      VHBRMN        VABRMN
059400971021     C                   ENDIF
059500971021     C*
059600971023     C                   TESTN                   VHBNCD               30
059700971023     C  N30              TESTN                   VHBNCD                 30
059800971023     C  N30              TESTN                   VHBNCD                   30
059900971021     C                   MOVE      VHBNCD        W001A             1
060000971021     C     *IN30         IFEQ      *OFF
060100971021     C     W001A         ORLT      '0'
060200971023     C     W001A         ANDNE     *BLANK
060300971021     C                   SETON                                        32
060400971021     C                   MOVE      *ZERO         VABNCD
060500010604     C                   eval      vinmsg = %trimr(vinmsg)
060600010604     C                             + ' ' + 'VHBNCD'
060700971021     C                   ELSE
060800971021     C                   MOVE      VHBNCD        VABNCD
060900971021     C                   ENDIF
061000971021     C*
061100971023     C                   TESTN                   VHBNCA               30
061200971023     C  N30              TESTN                   VHBNCA                 30
061300971023     C  N30              TESTN                   VHBNCA                   30
061400971021     C                   MOVE      VHBNCA        W001A             1
061500971021     C     *IN30         IFEQ      *OFF
061600971021     C     W001A         ORLT      '0'
061700971023     C     W001A         ANDNE     *BLANK
061800971021     C                   SETON                                        32
061900971021     C                   MOVE      *ZERO         VABNCA
062000010604     C                   eval      vinmsg = %trimr(vinmsg)
062100010604     C                             + ' ' + 'VHBNCA'
062200971021     C                   ELSE
062300971021     C                   MOVE      VHBNCA        VABNCA
062400971021     C                   ENDIF
062500971021     C*
062600971023     C                   TESTN                   VHBZNC               30
062700971023     C  N30              TESTN                   VHBZNC                 30
062800971023     C  N30              TESTN                   VHBZNC                   30
062900971021     C                   MOVE      VHBZNC        W001A             1
063000971021     C     *IN30         IFEQ      *OFF
063100971021     C     W001A         ORLT      '0'
063200971023     C     W001A         ANDNE     *BLANK
063300971021     C                   SETON                                        32
063400971021     C                   MOVE      *ZERO         VABZNC
063500010604     C                   eval      vinmsg = %trimr(vinmsg)
063600010604     C                             + ' ' + 'VHBZNC'
063700971021     C                   ELSE
063800971021     C                   MOVE      VHBZNC        VABZNC
063900971021     C                   ENDIF
064000971021     C*
064100971023     C                   TESTN                   VHBDCR               30
064200971023     C  N30              TESTN                   VHBDCR                 30
064300971023     C  N30              TESTN                   VHBDCR                   30
064400971021     C                   MOVE      VHBDCR        W001A             1
064500971021     C     *IN30         IFEQ      *OFF
064600971021     C     W001A         ORLT      '0'
064700971023     C     W001A         ANDNE     *BLANK
064800971021     C                   SETON                                        32
064900971021     C                   MOVE      *ZERO         VABDCR
065000010604     C                   eval      vinmsg = %trimr(vinmsg)
065100010604     C                             + ' ' + 'VHBDCR'
065200971021     C                   ELSE
065300971021     C                   MOVE      VHBDCR        VABDCR
065400971021     C                   ENDIF
065500971021     C*
065600971023     C                   TESTN                   VHBHCR               30
065700971023     C  N30              TESTN                   VHBHCR                 30
065800971023     C  N30              TESTN                   VHBHCR                   30
065900971021     C                   MOVE      VHBHCR        W001A             1
066000971021     C     *IN30         IFEQ      *OFF
066100971021     C     W001A         ORLT      '0'
066200971023     C     W001A         ANDNE     *BLANK
066300971021     C                   SETON                                        32
066400971021     C                   MOVE      *ZERO         VABHCR
066500010604     C                   eval      vinmsg = %trimr(vinmsg)
066600010604     C                             + ' ' + 'VHBHCR'
066700971021     C                   ELSE
066800971021     C                   MOVE      VHBHCR        VABHCR
066900971021     C                   ENDIF
067000941229     C*
067100971021     C                   MOVE      *ZERO         VABVMD
067200971021     C                   MOVE      VHBVMD        WASC15
067300010719     C                   IF        WI15 = *blanks
067400010719     C                   EVAL      WI15 = *zeros
067500010719     C                   ENDIF
067600971023     C                   TESTN                   WI15                 30
067700971023     C  N30              TESTN                   WI15                   30
067800971023     C  N30              TESTN                   WI15                     30
067900971021     C                   MOVE      WI15          W001A             1
068000971021     C     *IN30         IFEQ      *OFF
068100971021     C     W001A         ORLT      '0'
068200971023     C     W001A         ANDNE     *BLANK
068300971021     C                   SETON                                        32
068400971021     C                   MOVE      *ZERO         VABVMD
068500010604     C                   eval      vinmsg = %trimr(vinmsg)
068600010604     C                             + ' ' + 'VHBVMD'
068700971021     C                   ELSE
068800971021     C                   MOVEL     WI15          VABVMD
068900010719     C                   IF        WD15 = *blanks
069000010719     C                   EVAL      WD15 = *zeros
069100010719     C                   ENDIF
069200971023     C                   TESTN                   WD15                 30
069300971023     C  N30              TESTN                   WD15                   30
069400971023     C  N30              TESTN                   WD15                     30
069500971021     C                   MOVE      WD15          W001A             1
069600971021     C     *IN30         IFEQ      *OFF
069700971021     C     W001A         ORLT      '0'
069800971023     C     W001A         ANDNE     *BLANK
069900971021     C                   SETON                                        32
070000971021     C                   MOVE      *ZERO         VABVMD
070100010604     C                   eval      vinmsg = %trimr(vinmsg)
070200010604     C                             + ' ' + 'VHBVMD'
070300971021     C                   ELSE
070400971021     C                   MOVE      WD15          VABVMD
070500971021     C                   ENDIF
070600050118     C                   ENDIF
070700971021     C*
070800971023     C                   TESTN                   VHBANT               30
070900971023     C  N30              TESTN                   VHBANT                 30
071000971023     C  N30              TESTN                   VHBANT                   30
071100971021     C                   MOVE      VHBANT        W001A             1
071200971021     C     *IN30         IFEQ      *OFF
071300971021     C     W001A         ORLT      '0'
071400971023     C     W001A         ANDNE     *BLANK
071500971021     C                   SETON                                        32
071600971021     C                   MOVE      *ZERO         VABANT
071700010604     C                   eval      vinmsg = %trimr(vinmsg)
071800010604     C                             + ' ' + 'VHBANT'
071900971021     C                   ELSE
072000971021     C                   MOVE      VHBANT        VABANT
072100971021     C                   ENDIF
072200971021     C*
072300971021     C* CAMPI ALFABETICI:
072400971021     C                   MOVE      VHBATB        VABATB
072500971021     C                   MOVE      VHBCBO        VABCBO
072600971021     C                   MOVE      VHBRSD        VABRSD
072700020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
072800020117     C     '@':'A'       XLATE     VABRSD        VABRSD
072900020117     C* ==
073000971021     C                   MOVE      VHBRD2        VABRD2
073100971021     C                   MOVE      VHBIND        VABIND
073200971021     C                   MOVE      VHBCAD        VABCAD
073300971021     C                   MOVE      VHBLOD        VABLOD
073400971021     C                   MOVE      VHBPRD        VABPRD
073500971021     C                   MOVE      VHBNZD        VABNZD
073600971021     C                   MOVE      VHBGC1        VABGC1
073700971021     C                   MOVE      VHBGC2        VABGC2
073800971021     C                   MOVE      VHBTSP        VABTSP
073900971021     C                   MOVE      VHBVAS        VABVAS
074000971021     C                   MOVE      VHBNAS        VABNAS
074100971021     C                   MOVE      VHBTIC        VABTIC
074200971021     C                   MOVE      VHBVCA        VABVCA
074300971021     C                   MOVE      VHBGCA        VABGCA
074400971021     C                   MOVE      VHBRMA        VABRMA
074500971021     C                   MOVE      VHBXCO        VABXCO
074600971021     C                   MOVE      VHBNOT        VABNOT
074700971021     C                   MOVE      VHBNT2        VABNT2
074800971021     C                   MOVE      VHBCTM        VABCTM
074900971021     C                   MOVE      VHBFFD        VABFFD
075000971021     C                   MOVE      VHBTCR        VABTCR
075100971021     C                   MOVE      VHBCTS        VABCTS
075200971021     C                   MOVE      VHBFTM        VABFTM
075300971021     C                   MOVE      VHBVAD        VABVAD
075400971021     C                   MOVE      VHBGMA        VABGMA
075500971021     C                   MOVE      VHBGGA        VABGGA
075600971021     C                   MOVE      VHBGVA        VABGVA
075700971021     C                   MOVE      VHBTC1        VABTC1
075800971021     C                   MOVE      VHBTC2        VABTC2
075900971021     C                   MOVE      VHBSCL        VABSCL
076000971021     C                   MOVE      VHBRMO        VABRMO
076100971021     C                   MOVE      VHBCMO        VABCMO
076200971021     C                   MOVE      VHBNMO        VABNMO
076300971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
076400971021     C* NELLA NAZIONE ORIGINALE
076500971021     C                   MOVE      VHBNMO        WA1D              1
076600971021     C                   MOVEL     VHBNMO        WA1S              1
076700971021     C     WA1D          IFNE      *BLANK
076800971021     C     WA1S          ANDEQ     *BLANK
076900971021     C                   MOVE      *BLANK        VABNMO
077000030616     C                   ENDIF
077100030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
077200030710     C                   EVAL      VABCMR = wCMR
077300030616     C                   EVAL      VABDCM = DATCOR
077400030616     C                   EVAL      VABDTS = DATCOR
077500040402     C                   EVAL      VABHMS = ORACOR
077600030616     C                   EVAL      VABCNT = 1
077700041025     C*
077800041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
077900041025     C                   IF        *IN52 = *ON
078000041025     C* Reperisco la provincia dal CAP e dalla località
078100041025     C                   IF        VABCAD <> *blanks AND
078200041025     C                             VABPRD  = *blanks
078300041025     C                   CLEAR                   TISI95DS
078400041025     C                   EVAL      I95TCN = '3'
078500041025     C                   Z-ADD     datcor        I95DAT
078600041025     C                   EVAL      I95CAP = VABCAD
078700041025     C                   EVAL      I95LOC = VABLOD
078800041025     C                   CALL      'TISI95R'
078900041025     C                   PARM                    TISI95DS
079000041025     C                   EVAL      VABPRD = O95PRV
079100041025     C                   ENDIF
079200041025     C                   ENDIF
079300930409     C*
079400010604     C  N31              ADD       1             §CTROK            7 0
079500010604     C   32              ADD       1             §CTRMO            7 0
079600010604     C   31              ADD       1             §CTRNO            7 0
079700030616     C  N31              WRITE     EDIVAB00
079800990910     C*
079900010601     C                   if        *in31 = *off and
080000010601     C                             *in32 = *off
080100990910     C                   eval      vinflg = '1'
080200990910     C                   else
080300000621     C                   eval      recko = vindta
080400030710     C                   if        %open(tis7tebps)
080500030407     C   50              except    rigadett
080600010612     C                   endif
080700990910     C                   eval      vinflg = '2'
080800990910     C                   endif
080900991022     C                   endif
081000990910     C                   endif
081100010601     C*
081200010601     C                   endif
081300000905     C*
081400000905     C                   else
081500000905     C                   eval      vinflg = '1'
081600000905     C                   endif
081700000905     C*
081800000905     C  N70              update    tivin000
081900991022     C*
082000991022     C  N70              ENDdo
082100010530     C
082200010530     C*
082300010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
082400010530     C                   if        cntNonEl = *zeros or
082500010530     C                             flgMulti = '0'
082600990910     C* Se non ci sono record con errori ...
082700010530     C                   if        §ctrno = 0 and
082800010601     C                             §ctrmo = 0 and
082900010530     C                             flgStato <> '2'
083000990910     C* ... restituisco esito OK.
083100990921     C                   eval      wrkesito = '0'
083200990910     C                   else
083300010601     C                   if        §ctrok > 0
083400990921     C                   eval      wrkesito = '1'
083500000710     C                   else
083600010615     C                   if        flgOk = '0'
083700010615     C                   eval      wrkesito = '2'
083800010615     C                   else
083900010615     C                   eval      wrkesito = '6'
084000010615     C                   endif
084100990910     C                   endif
084200000710     C                   endif
084300010530     C                   else
084400010530     C                   eval      wrkesito = '9'
084500010530     C                   endif
084600030710     C*
084700030710 X  1C                   else
084800030710     C                   eval      wrkesito = '8'
084900030710     C                   eval      flgGiro = '1'
085000030710 E  1C                   endif
085100030710     C*
085200990914     C                   if        %open(tivin00r)
085300990908     C                   close     tivin00r
085400990914     C                   endif
085500030616     C                   if        %open(edivabwr)
085600030616     C                   close     edivabwr
085700990914     C                   endif
085800010530     C*
085900010530     C                   if        vlrpoi <> 999
086000010530     C                   eval      invfil = vlrpoi
086100010530     C                   endif
086200990910     C*
086300990920     C                   if        §ctrok > 0
086400010530     C                             and invfil > *zeros
086500000613     C                   exsr      invio
086600990920     C                   endif
086700010612     C*
086800010612     C                   if        flgGiro = '1'
086900010612     C                   exsr      endela
087000010612     C                   endif
087100990920     C*
087200910830     C                   ENDSR
087300000613     C***
087400990920
087500010530
087600010530
087700010530
087800010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
087900010530     C     repfil        BEGSR
088000010530     C*
088100010530     C                   if        invfil = *zeros and
088200010530     C                             depfil > *zeros and
088300010613     C                             (vinflg = *blanks or
088400010613     C                              vinflg = *zeros)
088500010530     C
088600010530     C                   eval      invfil = depfil
088700010530     C                   endif
088800010530     C*
088900010530     C                   if        depfil <> invfil and
089000010530     C                             invfil > *zeros
089100010530     C                   eval      flgMulti = '1'
089200010530     C                   if        vinflg = *blanks
089300010530     C                   add       1             cntNonEl
089400010530     C                   endif
089500010530     C                   endif
089600010530     C*
089700010530     C                   if        vinflg = '2'
089800010530     C                   eval      flgStato = '2'
089900010530     C                   endif
090000010530     C*
090100010530     C                   ENDSR
090200010530     C***
090300010530
090400010530
090500010530
090600010530
090700990920      /TITLE Invio dei dati al punto operativo.
090800000613     C     invio         BEGSR
090900990920     C*
091000990920     C                   reset                   dscmz
091100030618     C                   move      invfil        cmzdst
091200030618     C***                move      '789'         cmzdst
091300990920     C                   eval      cmzfld = vlrfou
091400990920     C                   eval      cmzmbd = vlrhdl
091500990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
091600000710     C                   if        prmfir = *blanks
091700030616     C                   eval      cmzfla = 'EDIVAB0F'
091800030616     C                   eval      cmzmba = 'EDIVAB0F'
091900000710     C                   else
092000000710     C                   eval      cmzfla = prmfir
092100000710     C                   eval      cmzmba = prmfir
092200000710     C                   endif
092300990920     C                   eval      cmznrr = *zeros
092400990920     C                   move      §ctrok        cmznrr
092500021003     C                   eval      cmzlba = vlrfl1
092600990920     C                   call(e)   'TIS711C'
092700990920     C                   parm                    dscmz
092800990921     C                   parm      *blanks       esito
092900990920     C                   if        %error
093000990920     C                             or cmzerr = '1'
093100990921     C                             or esito  = '1'
093200000710     C                   eval      wrkesito = '3'
093300990920     C                   endif
093400990920     C*
093500000613     C                   ENDSR
093600000613     C***
093700010530
093800010530
093900010530
094000010530
094100010530      /TITLE Invio dei dati al punto operativo.
094200010530     C     opeini        BEGSR
094300010530     C*
094400010530     C* Inizializzo flag e contatori operativi
094500010530     C                   movel     '0'           flgGiro           1
094600010530     C                   movel     '0'           flgMulti          1
094700010530     C                   movel     '1'           flgStato          1
094800010615     C                   movel     '0'           flgOk             1
094900010530     C                   z-add     *zeros        cntNonEl         10 0
095000010530     C                   z-add     *zeros        depfil            3 0
095100010530     C                   z-add     *zeros        invfil            3 0
095200010530     C*
095300010530     C                   ENDSR
095400010530     C***
095500010530
095600010530
095700010530
095800990910
095900000613     C     *inzsr        BEGSR
096000990910     C*
096100990910     C     *entry        plist
096200990920     C                   parm                    tivlrds
096300990921     C                   parm      wrkesito      esito
096400000724     C                   parm                    prmlit
096500000710     C                   parm                    prmfir
096600030616     C*
096700030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
096800030710     C                   move(p)   vlrMSG        wNomeFile
096900030710     C*
097000030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
097100030710     C* del codice CMR dal nome del file importato.
097200030710     C                   if        wNomeFile <> *blanks AND
097300030710     C                             vlrppt<>*blanks
097400041001     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
097500030710     C                   if        %subst(vlrppt:1:1) = 'S'
097600030710     C                   seton                                        50
097700030710     C                   else
097800030710     C                   setoff                                       50
097900030710     C                   endif
098000041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
098100041001     C                   if        %subst(vlrppt:6:1) = 'S'
098200041001     C                   seton                                        51
098300041001     C                   else
098400041001     C                   setoff                                       51
098500041001     C                   endif
098600041025     C*
098700041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
098800041025     C* (naturalmente solo se nn già presente)
098900041025     C                   if        %subst(vlrppt:7:1) = 'S'
099000041025     C                   seton                                        52
099100041025     C                   else
099200041025     C                   setoff                                       52
099300041025     C                   endif
099400041001     C*
099500030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
099600030710     C                   move(p)   wPosDaA       wPosDa
099700030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
099800030710     C                   move(p)   wLungA        wLung
099900030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
100000030710     C                   seton                                        60
100100030710     C                   else
100200030710     C                   setoff                                       60
100300030710     C                   endif
100400010606     C*
100500010606     C* CALCOLA LA DATA CORRENTE
100600010606     C                   time                    wn14             14 0
100700010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
100800040402     C                   movel     wn14          oracor            6 0          *ORA
100900010606     C                   z-add     wn8           g08dat
101000010606     C                   z-add     *zeros        g08inv
101100010606     C                   movel     '0'           g08err
101200010606     C                   call      'XSRDA8'
101300010606     C                   parm                    wlbda8
101400010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
101500000613     C*
101600000613     C                   ENDSR
101700000613     C***
101800990908
101900030710     Otis7tebp  E            riepilogo         2
102000990915     O                                              'Upload via Internet'
102100990915     O                                           +1 'Traduzione TIVIN00R -'
102200030616     O                                           +1 'EDIVABWR'
102300990915     O                                           +1 'Testate bolle'
102400990915     O          E            riepilogo   2
102500990915     O                       wrkdata
102600990915     O                       wrkora              +1
102700990915     O                       procname            +1
102800990915     O          E            riepilogo   2
102900990915     O                                              'Cliente..................:'
103000990915     O                       VABCCM        z     +1
103100030618     O          E            riepilogo   2
103200030618     O                                              'Numero CMR...............:'
103300030618     O                       VABCMR              +1
103400990915     O          E            riepilogo   2
103500990920     O                                              'Riferimento Strategi.....:'
103600990920     O                       vlrhdl              +1
103700990915     O          E            riepilogo   2
103800990915     O                                              'Giusti...................:'
103900971022     O                       §CTROK        2   +  1
104000990915     O          E            riepilogo   2
104100990915     O                                              'Sbagliati e corretti.....:'
104200971022     O                       §CTRMO        2   +  1
104300990915     O          E            riepilogo   2
104400990915     O                                              'Sbagliati e scartati.....:'
104500971022     O                       §CTRNO        2   +  1
104600000613
104700030710     Otis7tebps E            testdett          2
104800000613     O                                              'Upload via Internet'
104900000613     O                                           +1 'Traduzione TIVIN00R -'
105000030616     O                                           +1 'EDIVABWR'
105100000613     O                                           +1 'Testate bolle'
105200000616     O          E            testdett    3
105300000613     O                                           +2 'N° rec'
105400000613     O                                           +3 'Anteprima contenuto'
105500000616     O          E            rigadett    2
105600000613     O                       rrnum               +2
105700000621     O                       recko               +3
105800000616     O          E            findett     2
105900000613     O                       wrkdata
106000000613     O                       wrkora              +1
106100000613     O                       procname            +1
106200000616     O          E            findett     2
106300000613     O                                              'Cliente..................:'
106400000613     O                       VABCCM        z     +1
106500030618     O          E            findett     2
106600030618     O                                              'Numero CMR...............:'
106700030618     O                       VABCMR              +1
106800000616     O          E            findett     2
106900000613     O                                              'Riferimento Strategi.....:'
107000000613     O                       vlrhdl              +1
107100000616     O          E            findett     2
107200000613     O                                              'Giusti...................:'
107300000613     O                       §CTROK        2   +  1
107400000616     O          E            findett     2
107500000613     O                                              'Sbagliati e corretti.....:'
107600000613     O                       §CTRMO        2   +  1
107700000616     O          E            findett     2
107800000613     O                                              'Sbagliati e scartati.....:'
107900000613     O                       §CTRNO        2   +  1
108000000512** CMD - COMANDI CL
108100030710OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7TEBP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
108200030710OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7TEBPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
108300030710DLTOVR FILE(TIS7TEBP TIS7TEBPS) LVL(*)
108400000512
108500000512
