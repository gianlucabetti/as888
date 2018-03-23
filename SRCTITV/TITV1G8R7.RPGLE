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
002000050118     D*
002100050118     D* DS PER EDITARE I CAMPI NUMERICI IN ASCII
002200050118     D* 10+3
002300050118     D                 DS
002400050118     D  WI15                   1     10
002500050118     D  WD15                  11     13
002600050118     D  WASC15                 1     13
002700050118     D*  6+1
002800050118     D                 DS
002900050118     D  WI09                   1      6
003000050118     D  WD09                   7      7
003100050118     D  WASC09                 1      7
003200050118     D*  2+3
003300050118     D                 DS
003400050118     D  WI07                   1      2
003500050118     D  WD07                   3      5
003600050118     D  WASC07                 1      5
003700990920     D tivlrds       e ds                  extname(tivlr00f)
003800041025     D tisi95ds      e ds
003900990910     D esito           s              1
004000000724     D prmlit          s             10
004100000710     D prmfir          s             10
004200990921     D wrkesito        s                   like(esito)
004300990915     D wrkdata         s               d
004400990915     D wrkora          s               t
004500000613     D rrnum           s              6  0 INZ(*zeros)
004600000621     D recko           s            150    INZ(*blanks)
004700011123     D depcmd          s            150    INZ(*blanks)
004800030616     D wNomeFile       s             30    INZ(*blanks)
004900030710     D wCMR            s             35    INZ(*blanks)
005000030710     D wPosDaA         s              2    INZ(*blanks)
005100030710     D wPosDa          s              2  0 INZ(*zeros)
005200030710     D wLungA          s              2    INZ(*blanks)
005300030710     D wLung           s              2  0 INZ(*zeros)
005400050914     D wDataOraAlfa    s             14    INZ(*blanks)
005500010606     D*------------------
005600010606     D* DS "XSRDA8" - CONTROLLA DATA (8)
005700010606     D*------------------
005800010606     D WLBDA8          DS                  INZ
005900010606     D  G08DAT                 1      8  0
006000010606     D  G08INV                 9     16  0
006100010606     D  G08ERR                17     17
006200010606     D  G08TGI                18     22  0
006300050118     D*---------------------------------------------------------
006400050118     D* DS X RIDEFINIZIONE "CAMPONE" DATI DI TIVIN00R
006500050118     D*---------------------------------------------------------
006600050118     D                 ds
006700050118     D vhbatb                  1      1
006800050118     D vhbccm                  2      8
006900050118     D vhblnp                  9     11
007000050118     D vhbaas                 12     15
007100050118     D vhbmgs                 16     19
007200050118     D vhbnrs                 20     21
007300050118     D vhbnsp                 22     28
007400050118     D vhbcbo                 29     30
007500050118     D vhblna                 31     33
007600050118     D vhbrsd                 34     68
007700050118     D vhbrd2                 69    103
007800050118     D vhbind                104    138
007900050118     D vhbcad                139    147
008000050118     D vhblod                148    182
008100050118     D vhbprd                183    184
008200050118     D vhbnzd                185    187
008300050118     D vhbgc1                188    189
008400050118     D vhbgc2                190    191
008500050118     D vhbctr                192    194
008600050118     D vhbtsp                195    195
008700050118     D vhbias                196    208
008800050118     D vhbvas                209    211
008900050118     D vhbnas                212    226
009000050118     D vhbncl                227    231
009100050118     D vhbpkb                232    238
009200050118     D vhbvlb                239    243
009300050118     D vhbqft                244    256
009400050118     D vhbcas                257    269
009500050118     D vhbtic                270    271
009600050118     D vhbvca                272    274
009700050118     D vhbgca                275    276
009800050118     D vhbrmn                277    291
009900050118     D vhbrma                292    306
010000050118     D vhbncd                307    313
010100050118     D vhbnca                314    320
010200050118     D vhbxco                321    321
010300050118     D vhbnot                322    356
010400050118     D vhbnt2                357    391
010500050118     D vhbznc                392    393
010600050118     D vhbctm                394    395
010700050118     D vhbffd                396    396
010800050118     D vhbdcr                397    404
010900050118     D vhbtcr                405    405
011000050118     D vhbhcr                406    409
011100050118     D vhbcts                410    411
011200050118     D vhbftm                412    412
011300050118     D vhbvmd                413    425
011400050118     D vhbvad                426    428
011500050118     D vhbgma                429    430
011600050118     D vhbgga                431    432
011700050118     D vhbgva                433    434
011800050118     D vhbtc1                435    435
011900050118     D vhbtc2                436    436
012000050118     D vhbscl                437    437
012100050118     D vhbant                438    446
012200050118     D vhbrmo                447    471
012300050118     D vhbcmo                472    480
012400050118     D vhbnmo                481    483
012500050118     D  X_rec                  1    483    inz
012600010606     D*
012700071018     D*-------------------
012800071018     D* COSTANTI
012900071018     D*-------------------
013000071018     D minu            c                   const('qwertyuiopasdfghjklzxcvbnm')  *alfabeto
013100071018     D maiu            c                   const('QWERTYUIOPASDFGHJKLZXCVBNM')  *ALFABETO
013200990908
013300990915     C                   time                    wrkdata
013400990915     C                   time                    wrkora
013500000913     C                   reset                   rrnum
013600990921     C                   reset                   esito
013700990921     C                   reset                   wrkesito
013800010530     C*
013900010530     C                   exsr      opeini
014000010530     C                   exsr      rwvab
014100041025     C*
014200041025     C* Esegue lancio TISI95R solo x chiusura
014300041025     C                   CLEAR                   TISI95DS
014400041025     C                   EVAL      I95TLA = 'C'
014500041025     C                   CALL      'TISI95R'
014600041025     C                   PARM                    TISI95DS
014700010530     C*
014800010530     C                   seton                                        lr
014900010530
015000010530
015100010530
015200010530
015300010530     C*--------------------------------------------------------
015400010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
015500010530     C*--------------------------------------------------------
015600010530     C     PREELA        BEGSR
015700000724     C*
015800000724     C* SE OCCORRE SPEDIRE IN FILIALE
015900010530     C                   if        invfil <> *zeros and
016000010530     C                             flgGiro = '0'
016100010530     C*
016200010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
016300010530     C                   eval      flgGiro = '1'
016400000724     C*
016500000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
016600000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
016700030623     C     VABFGS        chain     azorg01l
016800000724     C                   if        %found
016900000616     C                   movel(p)  CMD(1)        depcmd
017000020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
017100000616     C*
017200000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
017300011123     C                   Z-ADD     150           LENGH            15 5
017400000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
017500000616     C                   PARM                    depcmd
017600000616     C                   PARM                    LENGH
017700000724     C*
017800000724     C                   endif
017900000616     C*
018000000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
018100000616     C                   movel(p)  CMD(2)        depcmd
018200000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
018300000616     C*
018400000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
018500011123     C                   Z-ADD     150           LENGH            15 5
018600000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
018700000616     C                   PARM                    depcmd
018800000616     C                   PARM                    LENGH
018900000616     C*
019000030710     C                   if        not %open(tis7tebps)
019100030710     C                   open      tis7tebps
019200030407     C   50              except    testdett
019300000613     C                   endif
019400010530     C*
019500010530     C                   endif
019600010530     C*
019700010530     C                   ENDSR
019800010530     C***
019900010530
020000010530
020100010530
020200010530
020300010530
020400010530
020500010530     C*--------------------------------------------------------
020600010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
020700010530     C*--------------------------------------------------------
020800010530     C     ENDELA        BEGSR
020900010530     C*
021000010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
021100000613     C*
021200030710     C                   if        %open(tis7tebps)
021300030407     C   50              except    findett
021400030710     C                   close     tis7tebps
021500000613     C                   endif
021600000616     C*
021700000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
021800011123     C                   Z-ADD     150           LENGH            15 5
021900010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
022000000616     C                   PARM                    CMD(3)
022100000616     C                   PARM                    LENGH
022200010530     C*
022300010530     C                   ENDSR
022400010530     C***
022500000613
022600010530
022700010530
022800000613
022900000613     C*--------------------------------------------------------
023000000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
023100000613     C*--------------------------------------------------------
023200000621     C     STPR          BEGSR
023300000613     C*
023400030710     C                   if        not %open(tis7tebp)
023500030710     C                   open      tis7tebp
023600990915     C                   endif
023700990915     C*
023800030407     C   50              except    riepilogo
023900990915     C*
024000030710     C                   if        %open(tis7tebp)
024100030710     C                   close     tis7tebp
024200990914     C                   endif
024300990910     C*
024400000613     C                   ENDSR
024500000613     C***
024600990908
024700010530
024800010530
024900010530
025000910830     C*--------------------------------------------------------
025100030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
025200910830     C*--------------------------------------------------------
025300941229     C     RWVAB         BEGSR
025400030710     C*
025500030710     C                   clear                   §CTROK
025600030710     C                   clear                   §CTRMO
025700030710     C                   clear                   §CTRNO
025800990910     C*
025900030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
026000030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
026100030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
026200030710     C                   if        *in60 = *on
026300990914     C                   if        not %open(tivin00r)
026400990908     C                   open      tivin00r
026500990914     C                   endif
026600030616     C                   if        not %open(edivabwr)
026700030616     C                   open      edivabwr
026800990914     C                   endif
026900990910     C*
027000921023     C                   DO        *HIVAL
027100990913     C*
027200990915     C                   READ      tivin00r                               70
027300071018     C*
027400071018     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
027500071018     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
027600010618     C*
027700010618     C* Dopo ogni lettura verifico se ci sono stati record OK
027800010618     C                   if        vinflg = '1'
027900010618     C                   eval      flgOk = '1'
028000010618     C                   endif
028100010705     C*
028200010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
028300010705     C                   if        %subst(vindta:1:1) = '='
028400010705     C                   eval      %subst(vindta:1:1) = *blanks
028500010705     C                   endif
028600010705     C*
028700000905     C                   if        vindta > *blanks
028800000613     C                   add       1             rrnum
028900010601     C* ...elimino eventuali segni
029000010601     C     '+':' '       xlate     vindta        vindta
029100990913     C*
029200010601     C                   if        *in70 = *off and
029300990913     C                             (vinflg = *blanks
029400010711     C                              or vinflg = '0'
029500010711     C                              or vinflg = '2')
029600990913     C*
029700030616     C                   clear                   EDIVAB00
029800010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
029900010711     C                   if        vinflg = *blanks or vinflg = '0'
030000010711     C                   clear                   vinmsg
030100010711     C                   endif
030200010711     C*
030300050118     C                   eval      x_rec  = vindta
030400991022     C*
030500991022     C                   testb     x'3F'         vhbatb                   02
030600991022     C                   if        not *in02
030700991022     C*
030800971021     C                   SETOFF                                       303132
030900971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
031000971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
031100971023     C*    OPPURE TUTTI BLANKS
031200010530     C*
031300010530     C                   TESTN                   VHBLNP               30
031400010530     C  N30              TESTN                   VHBLNP                 30
031500010530     C  N30              TESTN                   VHBLNP                   30
031600010530     C                   MOVE      VHBLNP        W001A             1
031700010530     C     *IN30         IFEQ      *OFF
031800010530     C     W001A         ORLT      '0'
031900010530     C                   SETON                                        31
032000010530     C                   eval      vinmsg = %trimr(vinmsg)
032100010530     C                             + ' ' + 'VHBLNP'
032200010530     C                   ELSE
032300010530     C                   MOVE      VHBLNP        VABLNP
032400010530     C     VABLNP        IFLE      *ZEROS
032500010530     C                   SETON                                        31
032600010530     C                   eval      vinmsg = %trimr(vinmsg)
032700010530     C                             + ' ' + 'VHBLNP'
032800010530     C                   ENDIF
032900010530     C                   ENDIF
033000010530     C*
033100010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
033200010530     C                   eval      depfil = VABLNP
033300010530     C                   exsr      repfil
033400010601     C                   if        depfil = invfil
033500021014     C                   if        vlrpoi = 999
033600021010     C                   MOVE(P)   invfil        VABFGS
033700021014     C                   else
033800021014     C                   MOVE(P)   vlrpoi        VABFGS
033900021014     C                   endif
034000010530     C*
034100010530     C                   exsr      PREELA
034200010530     C*
034300971023     C                   TESTN                   VHBCCM               30
034400971023     C  N30              TESTN                   VHBCCM                 30
034500971023     C  N30              TESTN                   VHBCCM                   30
034600971021     C                   MOVE      VHBCCM        W001A             1
034700971021     C     *IN30         IFEQ      *OFF
034800971021     C     W001A         ORLT      '0'
034900971021     C                   SETON                                        31
035000010604     C                   eval      vinmsg = %trimr(vinmsg)
035100010604     C                             + ' ' + 'VHBCCM'
035200971021     C                   ELSE
035300971021     C                   MOVE      VHBCCM        VABCCM
035400971024     C     VABCCM        IFLE      *ZEROS
035500971024     C                   SETON                                        31
035600010604     C                   eval      vinmsg = %trimr(vinmsg)
035700010604     C                             + ' ' + 'VHBCCM'
035800971024     C                   ENDIF
035900971021     C                   ENDIF
036000971021     C*
036100971023     C                   TESTN                   VHBAAS               30
036200971023     C  N30              TESTN                   VHBAAS                 30
036300971023     C  N30              TESTN                   VHBAAS                   30
036400971021     C                   MOVE      VHBAAS        W001A             1
036500971021     C     *IN30         IFEQ      *OFF
036600971021     C     W001A         ORLT      '0'
036700971023     C     W001A         ANDNE     *BLANK
036800971021     C                   SETON                                        32
036900010606     C                   MOVEL     datcor        VABAAS
037000010604     C                   eval      vinmsg = %trimr(vinmsg)
037100010604     C                             + ' ' + 'VHBAAS'
037200971021     C                   ELSE
037300971021     C                   MOVE      VHBAAS        VABAAS
037400971021     C                   ENDIF
037500971021     C*
037600971023     C                   TESTN                   VHBMGS               30
037700971023     C  N30              TESTN                   VHBMGS                 30
037800971023     C  N30              TESTN                   VHBMGS                   30
037900971021     C                   MOVE      VHBMGS        W001A             1
038000971021     C     *IN30         IFEQ      *OFF
038100971021     C     W001A         ORLT      '0'
038200971023     C     W001A         ANDNE     *BLANK
038300971021     C                   SETON                                        32
038400010601     C                   MOVE      '0101'        VABMGS
038500010604     C                   eval      vinmsg = %trimr(vinmsg)
038600010604     C                             + ' ' + 'VHBMGS'
038700971021     C                   ELSE
038800971021     C                   MOVE      VHBMGS        VABMGS
038900971021     C                   ENDIF
039000971021     C*
039100971023     C                   TESTN                   VHBNRS               30
039200971023     C  N30              TESTN                   VHBNRS                 30
039300971023     C  N30              TESTN                   VHBNRS                   30
039400971021     C                   MOVE      VHBNRS        W001A             1
039500971021     C     *IN30         IFEQ      *OFF
039600971021     C     W001A         ORLT      '0'
039700971023     C     W001A         ANDNE     *BLANK
039800971021     C                   SETON                                        32
039900971021     C                   MOVE      *ZERO         VABNRS
040000010604     C                   eval      vinmsg = %trimr(vinmsg)
040100010604     C                             + ' ' + 'VHBNRS'
040200971021     C                   ELSE
040300971021     C                   MOVE      VHBNRS        VABNRS
040400971021     C                   ENDIF
040500971021     C*
040600971023     C                   TESTN                   VHBNSP               30
040700971023     C  N30              TESTN                   VHBNSP                 30
040800971023     C  N30              TESTN                   VHBNSP                   30
040900971021     C                   MOVE      VHBNSP        W001A             1
041000971021     C     *IN30         IFEQ      *OFF
041100971021     C     W001A         ORLT      '0'
041200971023     C     W001A         ANDNE     *BLANK
041300971021     C                   SETON                                        32
041400971021     C                   MOVE      *ZERO         VABNSP
041500010604     C                   eval      vinmsg = %trimr(vinmsg)
041600010604     C                             + ' ' + 'VHBNSP'
041700971021     C                   ELSE
041800971021     C                   MOVE      VHBNSP        VABNSP
041900971021     C                   ENDIF
042000971021     C*
042100971023     C                   TESTN                   VHBLNA               30
042200971023     C  N30              TESTN                   VHBLNA                 30
042300971023     C  N30              TESTN                   VHBLNA                   30
042400971021     C                   MOVE      VHBLNA        W001A             1
042500971021     C     *IN30         IFEQ      *OFF
042600971021     C     W001A         ORLT      '0'
042700971023     C     W001A         ANDNE     *BLANK
042800971021     C                   SETON                                        32
042900971021     C                   MOVE      *ZERO         VABLNA
043000010604     C                   eval      vinmsg = %trimr(vinmsg)
043100010604     C                             + ' ' + 'VHBLNA'
043200971021     C                   ELSE
043300971021     C                   MOVE      VHBLNA        VABLNA
043400971021     C                   ENDIF
043500971021     C*
043600971023     C                   TESTN                   VHBCTR               30
043700971023     C  N30              TESTN                   VHBCTR                 30
043800971023     C  N30              TESTN                   VHBCTR                   30
043900971021     C                   MOVE      VHBCTR        W001A             1
044000971021     C     *IN30         IFEQ      *OFF
044100971021     C     W001A         ORLT      '0'
044200971023     C     W001A         ANDNE     *BLANK
044300971021     C                   SETON                                        32
044400971021     C                   MOVE      *ZERO         VABCTR
044500010604     C                   eval      vinmsg = %trimr(vinmsg)
044600010604     C                             + ' ' + 'VHBCTR'
044700971021     C                   ELSE
044800971021     C                   MOVE      VHBCTR        VABCTR
044900971021     C                   ENDIF
045000941229     C*
045100971021     C                   MOVE      *ZERO         VABIAS
045200941229     C                   MOVE      VHBIAS        WASC15
045300971023     C                   TESTN                   WI15                 30
045400971023     C  N30              TESTN                   WI15                   30
045500971023     C  N30              TESTN                   WI15                     30
045600971021     C                   MOVE      WI15          W001A             1
045700971021     C     *IN30         IFEQ      *OFF
045800971021     C     W001A         ORLT      '0'
045900971023     C     W001A         ANDNE     *BLANK
046000971021     C                   SETON                                        32
046100971021     C                   MOVE      *ZERO         VABIAS
046200010604     C                   eval      vinmsg = %trimr(vinmsg)
046300010604     C                             + ' ' + 'VHBIAS'
046400971021     C                   ELSE
046500971021     C                   MOVEL     WI15          VABIAS
046600971023     C                   TESTN                   WD15                 30
046700971023     C  N30              TESTN                   WD15                   30
046800971023     C  N30              TESTN                   WD15                     30
046900971021     C                   MOVE      WD15          W001A             1
047000971021     C     *IN30         IFEQ      *OFF
047100971021     C     W001A         ORLT      '0'
047200971023     C     W001A         ANDNE     *BLANK
047300971021     C                   SETON                                        32
047400971021     C                   MOVE      *ZERO         VABIAS
047500010604     C                   eval      vinmsg = %trimr(vinmsg)
047600010604     C                             + ' ' + 'VHBIAS'
047700971021     C                   ELSE
047800971021     C                   MOVE      WD15          VABIAS
047900971021     C                   ENDIF
048000050118     C                   ENDIF
048100941229     C*
048200971023     C                   TESTN                   VHBNCL               30
048300971023     C  N30              TESTN                   VHBNCL                 30
048400971023     C  N30              TESTN                   VHBNCL                   30
048500971021     C                   MOVE      VHBNCL        W001A             1
048600971021     C     *IN30         IFEQ      *OFF
048700971021     C     W001A         ORLT      '0'
048800971021     C                   SETON                                        32
048900971021     C                   MOVE      *ZERO         VABNCL
049000010604     C                   eval      vinmsg = %trimr(vinmsg)
049100010604     C                             + ' ' + 'VHBNCL'
049200971021     C                   ELSE
049300971021     C                   MOVE      VHBNCL        VABNCL
049400971021     C                   ENDIF
049500941229     C*
049600971021     C                   MOVE      *ZERO         VABPKB
049700971021     C                   MOVE      VHBPKB        WASC09
049800010719     C                   IF        WI09 = *blanks
049900010719     C                   EVAL      WI09 = *zeros
050000010719     C                   ENDIF
050100971023     C                   TESTN                   WI09                 30
050200971023     C  N30              TESTN                   WI09                   30
050300971023     C  N30              TESTN                   WI09                     30
050400971021     C                   MOVE      WI09          W001A             1
050500971021     C     *IN30         IFEQ      *OFF
050600971021     C     W001A         ORLT      '0'
050700971021     C                   SETON                                        32
050800971021     C                   MOVE      *ZERO         VABPKB
050900010604     C                   eval      vinmsg = %trimr(vinmsg)
051000010604     C                             + ' ' + 'VHBPKB'
051100971021     C                   ELSE
051200971021     C                   MOVEL     WI09          VABPKB
051300010719     C                   IF        WD09 = *blanks
051400010719     C                   EVAL      WD09 = *zeros
051500010719     C                   ENDIF
051600971023     C                   TESTN                   WD09                 30
051700971023     C  N30              TESTN                   WD09                     30
051800971021     C                   MOVE      WD09          W001A             1
051900971021     C     *IN30         IFEQ      *OFF
052000971021     C     W001A         ORLT      '0'
052100971023     C     W001A         ANDNE     *BLANK
052200971021     C                   SETON                                        32
052300971021     C                   MOVE      *ZERO         VABPKB
052400010604     C                   eval      vinmsg = %trimr(vinmsg)
052500010604     C                             + ' ' + 'VHBPKB'
052600971021     C                   ELSE
052700971021     C                   MOVE      WD09          VABPKB
052800971021     C                   ENDIF
052900050118     C                   ENDIF
053000971021     C*
053100041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
053200041001     C                   IF        *IN51 = *OFF
053300971021     C                   MOVE      *ZERO         VABVLB
053400971021     C                   MOVE      VHBVLB        WASC07
053500010719     C                   IF        WI07 = *blanks
053600010719     C                   EVAL      WI07 = *zeros
053700010719     C                   ENDIF
053800971023     C                   TESTN                   WI07                 30
053900971023     C  N30              TESTN                   WI07                   30
054000971023     C  N30              TESTN                   WI07                     30
054100971021     C                   MOVE      WI07          W001A             1
054200971021     C     *IN30         IFEQ      *OFF
054300971021     C     W001A         ORLT      '0'
054400971023     C     W001A         ANDNE     *BLANK
054500971021     C                   SETON                                        32
054600971021     C                   MOVE      *ZERO         VABVLB
054700010604     C                   eval      vinmsg = %trimr(vinmsg)
054800010604     C                             + ' ' + 'VHBVLB'
054900971021     C                   ELSE
055000971021     C                   MOVEL     WI07          VABVLB
055100010719     C                   IF        WD07 = *blanks
055200010719     C                   EVAL      WD07 = *zeros
055300010719     C                   ENDIF
055400971023     C                   TESTN                   WD07                 30
055500971023     C  N30              TESTN                   WD07                   30
055600971023     C  N30              TESTN                   WD07                     30
055700971021     C                   MOVE      WD07          W001A             1
055800971021     C     *IN30         IFEQ      *OFF
055900971021     C     W001A         ORLT      '0'
056000971023     C     W001A         ANDNE     *BLANK
056100971021     C                   SETON                                        32
056200971021     C                   MOVE      *ZERO         VABVLB
056300010604     C                   eval      vinmsg = %trimr(vinmsg)
056400010604     C                             + ' ' + 'VHBVLB'
056500971021     C                   ELSE
056600971021     C                   MOVE      WD07          VABVLB
056700971021     C                   ENDIF
056800050118     C                   ENDIF
056900041001     C                   ELSE
057000041001     C* ...altrimenti forzo azzeramento del volume
057100041001     C                   Z-ADD     *zeros        VABVLB
057200041001     C                   ENDIF
057300941229     C*
057400971021     C                   MOVE      *ZERO         VABQFT
057500971021     C                   MOVE      VHBQFT        WASC15
057600010719     C                   IF        WI15 = *blanks
057700010719     C                   EVAL      WI15 = *zeros
057800010719     C                   ENDIF
057900971023     C                   TESTN                   WI15                 30
058000971023     C  N30              TESTN                   WI15                   30
058100971023     C  N30              TESTN                   WI15                     30
058200971021     C                   MOVE      WI15          W001A             1
058300971021     C     *IN30         IFEQ      *OFF
058400971021     C     W001A         ORLT      '0'
058500971023     C     W001A         ANDNE     *BLANK
058600971021     C                   SETON                                        32
058700971021     C                   MOVE      *ZERO         VABQFT
058800010604     C                   eval      vinmsg = %trimr(vinmsg)
058900010604     C                             + ' ' + 'VHBQFT'
059000971021     C                   ELSE
059100971021     C                   MOVEL     WI15          VABQFT
059200010719     C                   IF        WD15 = *blanks
059300010719     C                   EVAL      WD15 = *zeros
059400010719     C                   ENDIF
059500971023     C                   TESTN                   WD15                 30
059600971023     C  N30              TESTN                   WD15                   30
059700971023     C  N30              TESTN                   WD15                     30
059800971021     C                   MOVE      WD15          W001A             1
059900971021     C     *IN30         IFEQ      *OFF
060000971021     C     W001A         ORLT      '0'
060100971023     C     W001A         ANDNE     *BLANK
060200971021     C                   SETON                                        32
060300971021     C                   MOVE      *ZERO         VABQFT
060400010604     C                   eval      vinmsg = %trimr(vinmsg)
060500010604     C                             + ' ' + 'VHBQFT'
060600971021     C                   ELSE
060700971021     C                   MOVE      WD15          VABQFT
060800971021     C                   ENDIF
060900050118     C                   ENDIF
061000941229     C*
061100971021     C                   MOVE      *ZERO         VABCAS
061200971021     C                   MOVE      VHBCAS        WASC15
061300010719     C                   IF        WI15 = *blanks
061400010719     C                   EVAL      WI15 = *zeros
061500010719     C                   ENDIF
061600971023     C                   TESTN                   WI15                 30
061700971023     C  N30              TESTN                   WI15                   30
061800971023     C  N30              TESTN                   WI15                     30
061900971021     C                   MOVE      WI15          W001A             1
062000971021     C     *IN30         IFEQ      *OFF
062100971021     C     W001A         ORLT      '0'
062200971023     C     W001A         ANDNE     *BLANK
062300971021     C                   SETON                                        32
062400971021     C                   MOVE      *ZERO         VABCAS
062500010604     C                   eval      vinmsg = %trimr(vinmsg)
062600010604     C                             + ' ' + 'VHBCAS'
062700971021     C                   ELSE
062800971021     C                   MOVEL     WI15          VABCAS
062900010719     C                   IF        WD15 = *blanks
063000010719     C                   EVAL      WD15 = *zeros
063100010719     C                   ENDIF
063200971023     C                   TESTN                   WD15                 30
063300971023     C  N30              TESTN                   WD15                   30
063400971023     C  N30              TESTN                   WD15                     30
063500971021     C                   MOVE      WD15          W001A             1
063600971021     C     *IN30         IFEQ      *OFF
063700971021     C     W001A         ORLT      '0'
063800971023     C     W001A         ANDNE     *BLANK
063900971021     C                   SETON                                        32
064000971021     C                   MOVE      *ZERO         VABCAS
064100010604     C                   eval      vinmsg = %trimr(vinmsg)
064200010604     C                             + ' ' + 'VHBCAS'
064300971021     C                   ELSE
064400971021     C                   MOVE      WD15          VABCAS
064500971021     C                   ENDIF
064600050118     C                   ENDIF
064700971021     C*
064800971023     C                   TESTN                   VHBRMN               30
064900971023     C  N30              TESTN                   VHBRMN                 30
065000971023     C  N30              TESTN                   VHBRMN                   30
065100971021     C                   MOVE      VHBRMN        W001A             1
065200971021     C     *IN30         IFEQ      *OFF
065300971021     C     W001A         ORLT      '0'
065400971021     C                   SETON                                        32
065500971021     C                   MOVE      *ZERO         VABRMN
065600010604     C                   eval      vinmsg = %trimr(vinmsg)
065700010604     C                             + ' ' + 'VHBRMN'
065800971021     C                   ELSE
065900971021     C                   MOVE      VHBRMN        VABRMN
066000971021     C                   ENDIF
066100971021     C*
066200971023     C                   TESTN                   VHBNCD               30
066300971023     C  N30              TESTN                   VHBNCD                 30
066400971023     C  N30              TESTN                   VHBNCD                   30
066500971021     C                   MOVE      VHBNCD        W001A             1
066600971021     C     *IN30         IFEQ      *OFF
066700971021     C     W001A         ORLT      '0'
066800971023     C     W001A         ANDNE     *BLANK
066900971021     C                   SETON                                        32
067000971021     C                   MOVE      *ZERO         VABNCD
067100010604     C                   eval      vinmsg = %trimr(vinmsg)
067200010604     C                             + ' ' + 'VHBNCD'
067300971021     C                   ELSE
067400971021     C                   MOVE      VHBNCD        VABNCD
067500971021     C                   ENDIF
067600971021     C*
067700971023     C                   TESTN                   VHBNCA               30
067800971023     C  N30              TESTN                   VHBNCA                 30
067900971023     C  N30              TESTN                   VHBNCA                   30
068000971021     C                   MOVE      VHBNCA        W001A             1
068100971021     C     *IN30         IFEQ      *OFF
068200971021     C     W001A         ORLT      '0'
068300971023     C     W001A         ANDNE     *BLANK
068400971021     C                   SETON                                        32
068500971021     C                   MOVE      *ZERO         VABNCA
068600010604     C                   eval      vinmsg = %trimr(vinmsg)
068700010604     C                             + ' ' + 'VHBNCA'
068800971021     C                   ELSE
068900971021     C                   MOVE      VHBNCA        VABNCA
069000971021     C                   ENDIF
069100971021     C*
069200971023     C                   TESTN                   VHBZNC               30
069300971023     C  N30              TESTN                   VHBZNC                 30
069400971023     C  N30              TESTN                   VHBZNC                   30
069500971021     C                   MOVE      VHBZNC        W001A             1
069600971021     C     *IN30         IFEQ      *OFF
069700971021     C     W001A         ORLT      '0'
069800971023     C     W001A         ANDNE     *BLANK
069900971021     C                   SETON                                        32
070000971021     C                   MOVE      *ZERO         VABZNC
070100010604     C                   eval      vinmsg = %trimr(vinmsg)
070200010604     C                             + ' ' + 'VHBZNC'
070300971021     C                   ELSE
070400971021     C                   MOVE      VHBZNC        VABZNC
070500971021     C                   ENDIF
070600971021     C*
070700971023     C                   TESTN                   VHBDCR               30
070800971023     C  N30              TESTN                   VHBDCR                 30
070900971023     C  N30              TESTN                   VHBDCR                   30
071000971021     C                   MOVE      VHBDCR        W001A             1
071100971021     C     *IN30         IFEQ      *OFF
071200971021     C     W001A         ORLT      '0'
071300971023     C     W001A         ANDNE     *BLANK
071400971021     C                   SETON                                        32
071500971021     C                   MOVE      *ZERO         VABDCR
071600010604     C                   eval      vinmsg = %trimr(vinmsg)
071700010604     C                             + ' ' + 'VHBDCR'
071800971021     C                   ELSE
071900971021     C                   MOVE      VHBDCR        VABDCR
072000971021     C                   ENDIF
072100971021     C*
072200971023     C                   TESTN                   VHBHCR               30
072300971023     C  N30              TESTN                   VHBHCR                 30
072400971023     C  N30              TESTN                   VHBHCR                   30
072500971021     C                   MOVE      VHBHCR        W001A             1
072600971021     C     *IN30         IFEQ      *OFF
072700971021     C     W001A         ORLT      '0'
072800971023     C     W001A         ANDNE     *BLANK
072900971021     C                   SETON                                        32
073000971021     C                   MOVE      *ZERO         VABHCR
073100010604     C                   eval      vinmsg = %trimr(vinmsg)
073200010604     C                             + ' ' + 'VHBHCR'
073300971021     C                   ELSE
073400971021     C                   MOVE      VHBHCR        VABHCR
073500971021     C                   ENDIF
073600941229     C*
073700971021     C                   MOVE      *ZERO         VABVMD
073800971021     C                   MOVE      VHBVMD        WASC15
073900010719     C                   IF        WI15 = *blanks
074000010719     C                   EVAL      WI15 = *zeros
074100010719     C                   ENDIF
074200971023     C                   TESTN                   WI15                 30
074300971023     C  N30              TESTN                   WI15                   30
074400971023     C  N30              TESTN                   WI15                     30
074500971021     C                   MOVE      WI15          W001A             1
074600971021     C     *IN30         IFEQ      *OFF
074700971021     C     W001A         ORLT      '0'
074800971023     C     W001A         ANDNE     *BLANK
074900971021     C                   SETON                                        32
075000971021     C                   MOVE      *ZERO         VABVMD
075100010604     C                   eval      vinmsg = %trimr(vinmsg)
075200010604     C                             + ' ' + 'VHBVMD'
075300971021     C                   ELSE
075400971021     C                   MOVEL     WI15          VABVMD
075500010719     C                   IF        WD15 = *blanks
075600010719     C                   EVAL      WD15 = *zeros
075700010719     C                   ENDIF
075800971023     C                   TESTN                   WD15                 30
075900971023     C  N30              TESTN                   WD15                   30
076000971023     C  N30              TESTN                   WD15                     30
076100971021     C                   MOVE      WD15          W001A             1
076200971021     C     *IN30         IFEQ      *OFF
076300971021     C     W001A         ORLT      '0'
076400971023     C     W001A         ANDNE     *BLANK
076500971021     C                   SETON                                        32
076600971021     C                   MOVE      *ZERO         VABVMD
076700010604     C                   eval      vinmsg = %trimr(vinmsg)
076800010604     C                             + ' ' + 'VHBVMD'
076900971021     C                   ELSE
077000971021     C                   MOVE      WD15          VABVMD
077100971021     C                   ENDIF
077200050118     C                   ENDIF
077300971021     C*
077400971023     C                   TESTN                   VHBANT               30
077500971023     C  N30              TESTN                   VHBANT                 30
077600971023     C  N30              TESTN                   VHBANT                   30
077700971021     C                   MOVE      VHBANT        W001A             1
077800971021     C     *IN30         IFEQ      *OFF
077900971021     C     W001A         ORLT      '0'
078000971023     C     W001A         ANDNE     *BLANK
078100971021     C                   SETON                                        32
078200971021     C                   MOVE      *ZERO         VABANT
078300010604     C                   eval      vinmsg = %trimr(vinmsg)
078400010604     C                             + ' ' + 'VHBANT'
078500971021     C                   ELSE
078600971021     C                   MOVE      VHBANT        VABANT
078700971021     C                   ENDIF
078800971021     C*
078900971021     C* CAMPI ALFABETICI:
079000971021     C                   MOVE      VHBATB        VABATB
079100971021     C                   MOVE      VHBCBO        VABCBO
079200971021     C                   MOVE      VHBRSD        VABRSD
079300020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
079400020117     C     '@':'A'       XLATE     VABRSD        VABRSD
079500020117     C* ==
079600971021     C                   MOVE      VHBRD2        VABRD2
079700971021     C                   MOVE      VHBIND        VABIND
079800971021     C                   MOVE      VHBCAD        VABCAD
079900971021     C                   MOVE      VHBLOD        VABLOD
080000971021     C                   MOVE      VHBPRD        VABPRD
080100971021     C                   MOVE      VHBNZD        VABNZD
080200971021     C                   MOVE      VHBGC1        VABGC1
080300971021     C                   MOVE      VHBGC2        VABGC2
080400971021     C                   MOVE      VHBTSP        VABTSP
080500971021     C                   MOVE      VHBVAS        VABVAS
080600971021     C                   MOVE      VHBNAS        VABNAS
080700971021     C                   MOVE      VHBTIC        VABTIC
080800971021     C                   MOVE      VHBVCA        VABVCA
080900971021     C                   MOVE      VHBGCA        VABGCA
081000971021     C                   MOVE      VHBRMA        VABRMA
081100971021     C                   MOVE      VHBXCO        VABXCO
081200971021     C                   MOVE      VHBNOT        VABNOT
081300971021     C                   MOVE      VHBNT2        VABNT2
081400971021     C                   MOVE      VHBCTM        VABCTM
081500971021     C                   MOVE      VHBFFD        VABFFD
081600971021     C                   MOVE      VHBTCR        VABTCR
081700971021     C                   MOVE      VHBCTS        VABCTS
081800971021     C                   MOVE      VHBFTM        VABFTM
081900971021     C                   MOVE      VHBVAD        VABVAD
082000971021     C                   MOVE      VHBGMA        VABGMA
082100971021     C                   MOVE      VHBGGA        VABGGA
082200971021     C                   MOVE      VHBGVA        VABGVA
082300971021     C                   MOVE      VHBTC1        VABTC1
082400971021     C                   MOVE      VHBTC2        VABTC2
082500971021     C                   MOVE      VHBSCL        VABSCL
082600971021     C                   MOVE      VHBRMO        VABRMO
082700971021     C                   MOVE      VHBCMO        VABCMO
082800971021     C                   MOVE      VHBNMO        VABNMO
082900971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
083000971021     C* NELLA NAZIONE ORIGINALE
083100971021     C                   MOVE      VHBNMO        WA1D              1
083200971021     C                   MOVEL     VHBNMO        WA1S              1
083300971021     C     WA1D          IFNE      *BLANK
083400971021     C     WA1S          ANDEQ     *BLANK
083500971021     C                   MOVE      *BLANK        VABNMO
083600030616     C                   ENDIF
083700030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
083800030710     C                   EVAL      VABCMR = wCMR
083900030616     C                   EVAL      VABDCM = DATCOR
084000030616     C                   EVAL      VABDTS = DATCOR
084100040402     C                   EVAL      VABHMS = ORACOR
084200030616     C                   EVAL      VABCNT = 1
084300041025     C*
084400041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
084500041025     C                   IF        *IN52 = *ON
084600041025     C* Reperisco la provincia dal CAP e dalla località
084700041025     C                   IF        VABCAD <> *blanks AND
084800050907     C                             VABPRD  = *blanks AND
084900050907     C                             VABNZD  = *blanks
085000041025     C                   CLEAR                   TISI95DS
085100041025     C                   EVAL      I95TCN = '3'
085200041025     C                   Z-ADD     datcor        I95DAT
085300041025     C                   EVAL      I95CAP = VABCAD
085400041025     C                   EVAL      I95LOC = VABLOD
085500041025     C                   CALL      'TISI95R'
085600041025     C                   PARM                    TISI95DS
085700041025     C                   EVAL      VABPRD = O95PRV
085800041025     C                   ENDIF
085900041025     C                   ENDIF
086000080605     C*
086100080605     C* Forzatura x personalizzazione CEVA LOGISTIC - TIM
086200120223     C                   if        *in54 = *on AND VABCCM <> 0635734 AND
086300120223     C                                             VABCCM <> 0635526
086400080606     C                   eval      VABNOT = %subst(%editc(VABRMN:'X'):
086500080609     C                                      %len(VABRMN)-10+1)
086600080605     C                   eval      VABNT2 = *blanks
086700100519     C                   endif
086800930409     C*
086900010604     C  N31              ADD       1             §CTROK            7 0
087000010604     C   32              ADD       1             §CTRMO            7 0
087100010604     C   31              ADD       1             §CTRNO            7 0
087200030616     C  N31              WRITE     EDIVAB00
087300990910     C*
087400010601     C                   if        *in31 = *off and
087500010601     C                             *in32 = *off
087600990910     C                   eval      vinflg = '1'
087700990910     C                   else
087800000621     C                   eval      recko = vindta
087900030710     C                   if        %open(tis7tebps)
088000030407     C   50              except    rigadett
088100010612     C                   endif
088200990910     C                   eval      vinflg = '2'
088300990910     C                   endif
088400991022     C                   endif
088500990910     C                   endif
088600010601     C*
088700010601     C                   endif
088800000905     C*
088900000905     C                   else
089000000905     C                   eval      vinflg = '1'
089100000905     C                   endif
089200000905     C*
089300000905     C  N70              update    tivin000
089400991022     C*
089500991022     C  N70              ENDdo
089600010530     C
089700010530     C*
089800010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
089900010530     C                   if        cntNonEl = *zeros or
090000010530     C                             flgMulti = '0'
090100990910     C* Se non ci sono record con errori ...
090200010530     C                   if        §ctrno = 0 and
090300010601     C                             §ctrmo = 0 and
090400010530     C                             flgStato <> '2'
090500990910     C* ... restituisco esito OK.
090600990921     C                   eval      wrkesito = '0'
090700990910     C                   else
090800010601     C                   if        §ctrok > 0
090900990921     C                   eval      wrkesito = '1'
091000000710     C                   else
091100010615     C                   if        flgOk = '0'
091200010615     C                   eval      wrkesito = '2'
091300010615     C                   else
091400010615     C                   eval      wrkesito = '6'
091500010615     C                   endif
091600990910     C                   endif
091700000710     C                   endif
091800010530     C                   else
091900010530     C                   eval      wrkesito = '9'
092000010530     C                   endif
092100030710     C*
092200030710 X  1C                   else
092300030710     C                   eval      wrkesito = '8'
092400030710     C                   eval      flgGiro = '1'
092500030710 E  1C                   endif
092600030710     C*
092700990914     C                   if        %open(tivin00r)
092800990908     C                   close     tivin00r
092900990914     C                   endif
093000030616     C                   if        %open(edivabwr)
093100030616     C                   close     edivabwr
093200990914     C                   endif
093300010530     C*
093400010530     C                   if        vlrpoi <> 999
093500010530     C                   eval      invfil = vlrpoi
093600010530     C                   endif
093700990910     C*
093800990920     C                   if        §ctrok > 0
093900010530     C                             and invfil > *zeros
094000000613     C                   exsr      invio
094100990920     C                   endif
094200010612     C*
094300010612     C                   if        flgGiro = '1'
094400010612     C                   exsr      endela
094500010612     C                   endif
094600990920     C*
094700910830     C                   ENDSR
094800000613     C***
094900990920
095000010530
095100010530
095200010530
095300010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
095400010530     C     repfil        BEGSR
095500010530     C*
095600010530     C                   if        invfil = *zeros and
095700010530     C                             depfil > *zeros and
095800010613     C                             (vinflg = *blanks or
095900010613     C                              vinflg = *zeros)
096000010530     C
096100010530     C                   eval      invfil = depfil
096200010530     C                   endif
096300010530     C*
096400010530     C                   if        depfil <> invfil and
096500010530     C                             invfil > *zeros
096600010530     C                   eval      flgMulti = '1'
096700010530     C                   if        vinflg = *blanks
096800010530     C                   add       1             cntNonEl
096900010530     C                   endif
097000010530     C                   endif
097100010530     C*
097200010530     C                   if        vinflg = '2'
097300010530     C                   eval      flgStato = '2'
097400010530     C                   endif
097500010530     C*
097600010530     C                   ENDSR
097700010530     C***
097800010530
097900010530
098000010530
098100010530
098200990920      /TITLE Invio dei dati al punto operativo.
098300000613     C     invio         BEGSR
098400990920     C*
098500990920     C                   reset                   dscmz
098600030618     C                   move      invfil        cmzdst
098700030618     C***                move      '789'         cmzdst
098800990920     C                   eval      cmzfld = vlrfou
098900990920     C                   eval      cmzmbd = vlrhdl
099000990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
099100000710     C                   if        prmfir = *blanks
099200030616     C                   eval      cmzfla = 'EDIVAB0F'
099300030616     C                   eval      cmzmba = 'EDIVAB0F'
099400000710     C                   else
099500000710     C                   eval      cmzfla = prmfir
099600000710     C                   eval      cmzmba = prmfir
099700000710     C                   endif
099800990920     C                   eval      cmznrr = *zeros
099900990920     C                   move      §ctrok        cmznrr
100000021003     C                   eval      cmzlba = vlrfl1
100100990920     C                   call(e)   'TIS711C'
100200990920     C                   parm                    dscmz
100300990921     C                   parm      *blanks       esito
100400990920     C                   if        %error
100500990920     C                             or cmzerr = '1'
100600990921     C                             or esito  = '1'
100700000710     C                   eval      wrkesito = '3'
100800990920     C                   endif
100900990920     C*
101000000613     C                   ENDSR
101100000613     C***
101200010530
101300010530
101400010530
101500010530
101600010530      /TITLE Invio dei dati al punto operativo.
101700010530     C     opeini        BEGSR
101800010530     C*
101900010530     C* Inizializzo flag e contatori operativi
102000010530     C                   movel     '0'           flgGiro           1
102100010530     C                   movel     '0'           flgMulti          1
102200010530     C                   movel     '1'           flgStato          1
102300010615     C                   movel     '0'           flgOk             1
102400010530     C                   z-add     *zeros        cntNonEl         10 0
102500010530     C                   z-add     *zeros        depfil            3 0
102600010530     C                   z-add     *zeros        invfil            3 0
102700010530     C*
102800010530     C                   ENDSR
102900010530     C***
103000010530
103100010530
103200010530
103300990910
103400000613     C     *inzsr        BEGSR
103500990910     C*
103600990910     C     *entry        plist
103700990920     C                   parm                    tivlrds
103800990921     C                   parm      wrkesito      esito
103900000724     C                   parm                    prmlit
104000000710     C                   parm                    prmfir
104100030616     C*
104200030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
104300030710     C                   move(p)   vlrMSG        wNomeFile
104400030710     C*
104500030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
104600030710     C* del codice CMR dal nome del file importato.
104700030710     C                   if        wNomeFile <> *blanks AND
104800030710     C                             vlrppt<>*blanks
104900041001     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
105000030710     C                   if        %subst(vlrppt:1:1) = 'S'
105100030710     C                   seton                                        50
105200030710     C                   else
105300030710     C                   setoff                                       50
105400030710     C                   endif
105500041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
105600041001     C                   if        %subst(vlrppt:6:1) = 'S'
105700041001     C                   seton                                        51
105800041001     C                   else
105900041001     C                   setoff                                       51
106000041001     C                   endif
106100041025     C*
106200100519     C* Verifico se sui parametri del traduttore è richiesto il rerimento provincia
106300041025     C* (naturalmente solo se nn già presente)
106400041025     C                   if        %subst(vlrppt:7:1) = 'S'
106500041025     C                   seton                                        52
106600041025     C                   else
106700041025     C                   setoff                                       52
106800041025     C                   endif
106900100519     C*
107000100519     C* Verifico se sui parametri del traduttore è richiesto il trattamento "particolare"
107100100519     C* delle note
107200100519     C                   if        %subst(vlrppt:8:1) = 'S'
107300100519     C                   seton                                        54
107400100519     C                   else
107500100519     C                   setoff                                       54
107600100519     C                   endif
107700041001     C*
107800030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
107900030710     C                   move(p)   wPosDaA       wPosDa
108000030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
108100030710     C                   move(p)   wLungA        wLung
108200030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
108300030710     C                   seton                                        60
108400030710     C                   else
108500050914     C                   if        vlrisv = 'UL'
108600050914     C                   movel     datcor        wDataOraAlfa
108700050914     C                   move      oracor        wDataOraAlfa
108800050914     C                   eval      wCMR = 'FNVAB' + wDataOraAlfa
108900050914     C                   seton                                        60
109000050914     C                   else
109100030710     C                   setoff                                       60
109200050914     C                   endif
109300030710     C                   endif
109400010606     C*
109500010606     C* CALCOLA LA DATA CORRENTE
109600010606     C                   time                    wn14             14 0
109700010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
109800040402     C                   movel     wn14          oracor            6 0          *ORA
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
110900030710     Otis7tebp  E            riepilogo         2
111000990915     O                                              'Upload via Internet'
111100990915     O                                           +1 'Traduzione TIVIN00R -'
111200030616     O                                           +1 'EDIVABWR'
111300990915     O                                           +1 'Testate bolle'
111400990915     O          E            riepilogo   2
111500990915     O                       wrkdata
111600990915     O                       wrkora              +1
111700990915     O                       procname            +1
111800990915     O          E            riepilogo   2
111900990915     O                                              'Cliente..................:'
112000990915     O                       VABCCM        z     +1
112100030618     O          E            riepilogo   2
112200030618     O                                              'Numero CMR...............:'
112300030618     O                       VABCMR              +1
112400990915     O          E            riepilogo   2
112500990920     O                                              'Riferimento Strategi.....:'
112600990920     O                       vlrhdl              +1
112700990915     O          E            riepilogo   2
112800990915     O                                              'Giusti...................:'
112900971022     O                       §CTROK        2   +  1
113000990915     O          E            riepilogo   2
113100990915     O                                              'Sbagliati e corretti.....:'
113200971022     O                       §CTRMO        2   +  1
113300990915     O          E            riepilogo   2
113400990915     O                                              'Sbagliati e scartati.....:'
113500971022     O                       §CTRNO        2   +  1
113600000613
113700030710     Otis7tebps E            testdett          2
113800000613     O                                              'Upload via Internet'
113900000613     O                                           +1 'Traduzione TIVIN00R -'
114000030616     O                                           +1 'EDIVABWR'
114100000613     O                                           +1 'Testate bolle'
114200000616     O          E            testdett    3
114300000613     O                                           +2 'N° rec'
114400000613     O                                           +3 'Anteprima contenuto'
114500000616     O          E            rigadett    2
114600000613     O                       rrnum               +2
114700000621     O                       recko               +3
114800000616     O          E            findett     2
114900000613     O                       wrkdata
115000000613     O                       wrkora              +1
115100000613     O                       procname            +1
115200000616     O          E            findett     2
115300000613     O                                              'Cliente..................:'
115400000613     O                       VABCCM        z     +1
115500030618     O          E            findett     2
115600030618     O                                              'Numero CMR...............:'
115700030618     O                       VABCMR              +1
115800000616     O          E            findett     2
115900000613     O                                              'Riferimento Strategi.....:'
116000000613     O                       vlrhdl              +1
116100000616     O          E            findett     2
116200000613     O                                              'Giusti...................:'
116300000613     O                       §CTROK        2   +  1
116400000616     O          E            findett     2
116500000613     O                                              'Sbagliati e corretti.....:'
116600000613     O                       §CTRMO        2   +  1
116700000616     O          E            findett     2
116800000613     O                                              'Sbagliati e scartati.....:'
116900000613     O                       §CTRNO        2   +  1
117000000512** CMD - COMANDI CL
117100030710OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7TEBP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
117200030710OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7TEBPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
117300030710DLTOVR FILE(TIS7TEBP TIS7TEBPS) LVL(*)
117400000512
117500000512
