000100030616      /TITLE Upload via Internet: traduzione in EDIVABWR.
000200081202     H DFTACTGRP(*NO) BNDDIR('TRUL') ACTGRP('BARTVAS')
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
013200081202
013300081202     D*------------------
013400081202     D* PASSAGGIO PARAMETRI A PROCEDURE UBISNUM
013500081202     D*------------------
013600081202     D PiStr           S            100A   INZ
013700081202     D PiDecChr        S              1A   INZ
013800081202     D PiVal           S             63S30 INZ
013900081202     D PiInt           S               N   INZ
014000081202     D PiNum           S               N   INZ
014100081202
014200081202     D*------------------
014300081202     D* LINKING A DEFINIZIONI ESTERNE
014400081202     D*------------------
014500081202     D/COPY GAITRASRC/SRCPROTOPR,UBISNUM
014600081202
014700990908
014800990915     C                   time                    wrkdata
014900990915     C                   time                    wrkora
015000000913     C                   reset                   rrnum
015100990921     C                   reset                   esito
015200990921     C                   reset                   wrkesito
015300010530     C*
015400010530     C                   exsr      opeini
015500010530     C                   exsr      rwvab
015600041025     C*
015700041025     C* Esegue lancio TISI95R solo x chiusura
015800041025     C                   CLEAR                   TISI95DS
015900041025     C                   EVAL      I95TLA = 'C'
016000041025     C                   CALL      'TISI95R'
016100041025     C                   PARM                    TISI95DS
016200010530     C*
016300010530     C                   seton                                        lr
016400010530
016500010530
016600010530
016700010530
016800010530     C*--------------------------------------------------------
016900010530     C* PREELA - OPERAZIONI DI PRE-ELABORAZIONE               *
017000010530     C*--------------------------------------------------------
017100010530     C     PREELA        BEGSR
017200000724     C*
017300000724     C* SE OCCORRE SPEDIRE IN FILIALE
017400010530     C                   if        invfil <> *zeros and
017500010530     C                             flgGiro = '0'
017600010530     C*
017700010530     C* SFLEGGO SUBITO IL FLAG PREPOSTO X EFFETTUARE SOLO UNA VOLTA LE OPERAZINI DI QUESTA ROUTINE
017800010530     C                   eval      flgGiro = '1'
017900000724     C*
018000000724     C* REPERISCO L'AS/400 A CUI DOVER INVIARE E LA LIBRERIA DI AZIENDA DEL S.I.
018100000724     C* X COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
018200030623     C     VABFGS        chain     azorg01l
018300000724     C                   if        %found
018400000616     C                   movel(p)  CMD(1)        depcmd
018500020829     C                   EVAL      depcmd=%trim(depcmd)+%trim(orgde4)+')'
018600000616     C*
018700000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODE AS/400 DI FILIALE
018800011123     C                   Z-ADD     150           LENGH            15 5
018900000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
019000000616     C                   PARM                    depcmd
019100000616     C                   PARM                    LENGH
019200000724     C*
019300000724     C                   endif
019400000616     C*
019500000616     C* COSTRUZIONE COMANDO PER ISTRUZIONE SEGUENTE
019600000616     C                   movel(p)  CMD(2)        depcmd
019700000616     C                   EVAL      depcmd=%trim(depcmd)+'TRADUZIONI'+')'
019800000616     C*
019900000616     C* OVRPRTF PER SMISTAMENTO STAMPE SU CODA DI SEDE
020000011123     C                   Z-ADD     150           LENGH            15 5
020100000616     C                   CALL      'QCMDEXC'                                    *LANCIA CMD
020200000616     C                   PARM                    depcmd
020300000616     C                   PARM                    LENGH
020400000616     C*
020500030710     C                   if        not %open(tis7tebps)
020600030710     C                   open      tis7tebps
020700030407     C   50              except    testdett
020800000613     C                   endif
020900010530     C*
021000010530     C                   endif
021100010530     C*
021200010530     C                   ENDSR
021300010530     C***
021400010530
021500010530
021600010530
021700010530
021800010530
021900010530
022000010530     C*--------------------------------------------------------
022100010530     C* ENDELA - OPERAZIONI DI FINE-ELABORAZIONE              *
022200010530     C*--------------------------------------------------------
022300010530     C     ENDELA        BEGSR
022400010530     C*
022500010530     C                   EXSR      STPR                                         OP.DI STAMPA RIEPIL.
022600000613     C*
022700030710     C                   if        %open(tis7tebps)
022800030407     C   50              except    findett
022900030710     C                   close     tis7tebps
023000000613     C                   endif
023100000616     C*
023200000616     C* RIMUOVO LE SOSTITUZIONOI AI PRINTER FILE
023300011123     C                   Z-ADD     150           LENGH            15 5
023400010618     C                   CALL(e)   'QCMDEXC'                                    *LANCIA CMD
023500000616     C                   PARM                    CMD(3)
023600000616     C                   PARM                    LENGH
023700010530     C*
023800010530     C                   ENDSR
023900010530     C***
024000000613
024100010530
024200010530
024300000613
024400000613     C*--------------------------------------------------------
024500000621     C* STPR   - STAMPA IL RIEPILOGO (VA IN FILIALE)          *
024600000613     C*--------------------------------------------------------
024700000621     C     STPR          BEGSR
024800000613     C*
024900030710     C                   if        not %open(tis7tebp)
025000030710     C                   open      tis7tebp
025100990915     C                   endif
025200990915     C*
025300030407     C   50              except    riepilogo
025400990915     C*
025500030710     C                   if        %open(tis7tebp)
025600030710     C                   close     tis7tebp
025700990914     C                   endif
025800990910     C*
025900000613     C                   ENDSR
026000000613     C***
026100990908
026200010530
026300010530
026400010530
026500910830     C*--------------------------------------------------------
026600030616     C* RWVAB   LEGGE TIVIN00R E SCRIVE EDIVABWR              *
026700910830     C*--------------------------------------------------------
026800941229     C     RWVAB         BEGSR
026900030710     C*
027000030710     C                   clear                   §CTROK
027100030710     C                   clear                   §CTRMO
027200030710     C                   clear                   §CTRNO
027300990910     C*
027400030710     C* In caso d traduzione x numero CMR se nn sono riuscito a reperirlo (manca il nome del file
027500030710     C* oppure nn sono stati indicati correttamente i parametri x il reperimento del CMR dal nome
027600030710     C* file) esco direttamente e restituisco errore bloccante in fase d traduzione (no invio).
027700030710     C                   if        *in60 = *on
027800990914     C                   if        not %open(tivin00r)
027900990908     C                   open      tivin00r
028000990914     C                   endif
028100030616     C                   if        not %open(edivabwr)
028200030616     C                   open      edivabwr
028300990914     C                   endif
028400990910     C*
028500921023     C                   DO        *HIVAL
028600990913     C*
028700990915     C                   READ      tivin00r                               70
028800071018     C*
028900071018     C* Come primissima cosa narmalizzo il record di input da minuscolo a MAIUSCOLO
029000071018     C     minu:maiu     XLATE     vindta        vindta                         *Minus -> Maiuscolo
029100010618     C*
029200010618     C* Dopo ogni lettura verifico se ci sono stati record OK
029300010618     C                   if        vinflg = '1'
029400010618     C                   eval      flgOk = '1'
029500010618     C                   endif
029600010705     C*
029700010705     C* Chiodo x evitare segnalazioni di errore inconsistenti
029800010705     C                   if        %subst(vindta:1:1) = '='
029900010705     C                   eval      %subst(vindta:1:1) = *blanks
030000010705     C                   endif
030100010705     C*
030200000905     C                   if        vindta > *blanks
030300000613     C                   add       1             rrnum
030400010601     C* ...elimino eventuali segni
030500010601     C     '+':' '       xlate     vindta        vindta
030600990913     C*
030700010601     C                   if        *in70 = *off and
030800990913     C                             (vinflg = *blanks
030900010711     C                              or vinflg = '0'
031000010711     C                              or vinflg = '2')
031100990913     C*
031200030616     C                   clear                   EDIVAB00
031300010711     C* Se trattasi di record non ancora elaborato resetto il campo dei messaggi
031400010711     C                   if        vinflg = *blanks or vinflg = '0'
031500010711     C                   clear                   vinmsg
031600010711     C                   endif
031700010711     C*
031800050118     C                   eval      x_rec  = vindta
031900991022     C*
032000991022     C                   testb     x'3F'         vhbatb                   02
032100991022     C                   if        not *in02
032200991022     C*
032300971021     C                   SETOFF                                       303132
032400971021     C* PRIMA DI MUOVERE I CAMPI TESTO CHE I NUMERICI SIANO TALI
032500971023     C* OK SE PIENO DI NUMERI O BLANKS SEGUITI DA NUMERI
032600971023     C*    OPPURE TUTTI BLANKS
032700010530     C*
032800010530     C                   TESTN                   VHBLNP               30
032900010530     C  N30              TESTN                   VHBLNP                 30
033000010530     C  N30              TESTN                   VHBLNP                   30
033100010530     C                   MOVE      VHBLNP        W001A             1
033200010530     C     *IN30         IFEQ      *OFF
033300010530     C     W001A         ORLT      '0'
033400010530     C                   SETON                                        31
033500010530     C                   eval      vinmsg = %trimr(vinmsg)
033600010530     C                             + ' ' + 'VHBLNP'
033700010530     C                   ELSE
033800010530     C                   MOVE      VHBLNP        VABLNP
033900010530     C     VABLNP        IFLE      *ZEROS
034000010530     C                   SETON                                        31
034100010530     C                   eval      vinmsg = %trimr(vinmsg)
034200010530     C                             + ' ' + 'VHBLNP'
034300010530     C                   ENDIF
034400010530     C                   ENDIF
034500010530     C*
034600010530     C* Effettuo considerazioni x elaborazioni "multi-filiale"
034700010530     C                   eval      depfil = VABLNP
034800010530     C                   exsr      repfil
034900010601     C                   if        depfil = invfil
035000021014     C                   if        vlrpoi = 999
035100021010     C                   MOVE(P)   invfil        VABFGS
035200021014     C                   else
035300021014     C                   MOVE(P)   vlrpoi        VABFGS
035400021014     C                   endif
035500010530     C*
035600010530     C                   exsr      PREELA
035700010530     C*
035800971023     C                   TESTN                   VHBCCM               30
035900971023     C  N30              TESTN                   VHBCCM                 30
036000971023     C  N30              TESTN                   VHBCCM                   30
036100971021     C                   MOVE      VHBCCM        W001A             1
036200971021     C     *IN30         IFEQ      *OFF
036300971021     C     W001A         ORLT      '0'
036400971021     C                   SETON                                        31
036500010604     C                   eval      vinmsg = %trimr(vinmsg)
036600010604     C                             + ' ' + 'VHBCCM'
036700971021     C                   ELSE
036800971021     C                   MOVE      VHBCCM        VABCCM
036900971024     C     VABCCM        IFLE      *ZEROS
037000971024     C                   SETON                                        31
037100010604     C                   eval      vinmsg = %trimr(vinmsg)
037200010604     C                             + ' ' + 'VHBCCM'
037300971024     C                   ENDIF
037400971021     C                   ENDIF
037500971021     C*
037600971023     C                   TESTN                   VHBAAS               30
037700971023     C  N30              TESTN                   VHBAAS                 30
037800971023     C  N30              TESTN                   VHBAAS                   30
037900971021     C                   MOVE      VHBAAS        W001A             1
038000971021     C     *IN30         IFEQ      *OFF
038100971021     C     W001A         ORLT      '0'
038200971023     C     W001A         ANDNE     *BLANK
038300971021     C                   SETON                                        32
038400010606     C                   MOVEL     datcor        VABAAS
038500010604     C                   eval      vinmsg = %trimr(vinmsg)
038600010604     C                             + ' ' + 'VHBAAS'
038700971021     C                   ELSE
038800971021     C                   MOVE      VHBAAS        VABAAS
038900971021     C                   ENDIF
039000971021     C*
039100971023     C                   TESTN                   VHBMGS               30
039200971023     C  N30              TESTN                   VHBMGS                 30
039300971023     C  N30              TESTN                   VHBMGS                   30
039400971021     C                   MOVE      VHBMGS        W001A             1
039500971021     C     *IN30         IFEQ      *OFF
039600971021     C     W001A         ORLT      '0'
039700971023     C     W001A         ANDNE     *BLANK
039800971021     C                   SETON                                        32
039900010601     C                   MOVE      '0101'        VABMGS
040000010604     C                   eval      vinmsg = %trimr(vinmsg)
040100010604     C                             + ' ' + 'VHBMGS'
040200971021     C                   ELSE
040300971021     C                   MOVE      VHBMGS        VABMGS
040400971021     C                   ENDIF
040500971021     C*
040600971023     C                   TESTN                   VHBNRS               30
040700971023     C  N30              TESTN                   VHBNRS                 30
040800971023     C  N30              TESTN                   VHBNRS                   30
040900971021     C                   MOVE      VHBNRS        W001A             1
041000971021     C     *IN30         IFEQ      *OFF
041100971021     C     W001A         ORLT      '0'
041200971023     C     W001A         ANDNE     *BLANK
041300971021     C                   SETON                                        32
041400971021     C                   MOVE      *ZERO         VABNRS
041500010604     C                   eval      vinmsg = %trimr(vinmsg)
041600010604     C                             + ' ' + 'VHBNRS'
041700971021     C                   ELSE
041800971021     C                   MOVE      VHBNRS        VABNRS
041900971021     C                   ENDIF
042000971021     C*
042100971023     C                   TESTN                   VHBNSP               30
042200971023     C  N30              TESTN                   VHBNSP                 30
042300971023     C  N30              TESTN                   VHBNSP                   30
042400971021     C                   MOVE      VHBNSP        W001A             1
042500971021     C     *IN30         IFEQ      *OFF
042600971021     C     W001A         ORLT      '0'
042700971023     C     W001A         ANDNE     *BLANK
042800971021     C                   SETON                                        32
042900971021     C                   MOVE      *ZERO         VABNSP
043000010604     C                   eval      vinmsg = %trimr(vinmsg)
043100010604     C                             + ' ' + 'VHBNSP'
043200971021     C                   ELSE
043300971021     C                   MOVE      VHBNSP        VABNSP
043400971021     C                   ENDIF
043500971021     C*
043600971023     C                   TESTN                   VHBLNA               30
043700971023     C  N30              TESTN                   VHBLNA                 30
043800971023     C  N30              TESTN                   VHBLNA                   30
043900971021     C                   MOVE      VHBLNA        W001A             1
044000971021     C     *IN30         IFEQ      *OFF
044100971021     C     W001A         ORLT      '0'
044200971023     C     W001A         ANDNE     *BLANK
044300971021     C                   SETON                                        32
044400971021     C                   MOVE      *ZERO         VABLNA
044500010604     C                   eval      vinmsg = %trimr(vinmsg)
044600010604     C                             + ' ' + 'VHBLNA'
044700971021     C                   ELSE
044800971021     C                   MOVE      VHBLNA        VABLNA
044900971021     C                   ENDIF
045000971021     C*
045100971023     C                   TESTN                   VHBCTR               30
045200971023     C  N30              TESTN                   VHBCTR                 30
045300971023     C  N30              TESTN                   VHBCTR                   30
045400971021     C                   MOVE      VHBCTR        W001A             1
045500971021     C     *IN30         IFEQ      *OFF
045600971021     C     W001A         ORLT      '0'
045700971023     C     W001A         ANDNE     *BLANK
045800971021     C                   SETON                                        32
045900971021     C                   MOVE      *ZERO         VABCTR
046000010604     C                   eval      vinmsg = %trimr(vinmsg)
046100010604     C                             + ' ' + 'VHBCTR'
046200971021     C                   ELSE
046300971021     C                   MOVE      VHBCTR        VABCTR
046400971021     C                   ENDIF
046500941229     C*
046600971021     C                   MOVE      *ZERO         VABIAS
046700941229     C                   MOVE      VHBIAS        WASC15
046800971023     C                   TESTN                   WI15                 30
046900971023     C  N30              TESTN                   WI15                   30
047000971023     C  N30              TESTN                   WI15                     30
047100971021     C                   MOVE      WI15          W001A             1
047200971021     C     *IN30         IFEQ      *OFF
047300971021     C     W001A         ORLT      '0'
047400971023     C     W001A         ANDNE     *BLANK
047500971021     C                   SETON                                        32
047600971021     C                   MOVE      *ZERO         VABIAS
047700010604     C                   eval      vinmsg = %trimr(vinmsg)
047800010604     C                             + ' ' + 'VHBIAS'
047900971021     C                   ELSE
048000971021     C                   MOVEL     WI15          VABIAS
048100971023     C                   TESTN                   WD15                 30
048200971023     C  N30              TESTN                   WD15                   30
048300971023     C  N30              TESTN                   WD15                     30
048400971021     C                   MOVE      WD15          W001A             1
048500971021     C     *IN30         IFEQ      *OFF
048600971021     C     W001A         ORLT      '0'
048700971023     C     W001A         ANDNE     *BLANK
048800971021     C                   SETON                                        32
048900971021     C                   MOVE      *ZERO         VABIAS
049000010604     C                   eval      vinmsg = %trimr(vinmsg)
049100010604     C                             + ' ' + 'VHBIAS'
049200971021     C                   ELSE
049300971021     C                   MOVE      WD15          VABIAS
049400971021     C                   ENDIF
049500050118     C                   ENDIF
049600941229     C*
049700971023     C                   TESTN                   VHBNCL               30
049800971023     C  N30              TESTN                   VHBNCL                 30
049900971023     C  N30              TESTN                   VHBNCL                   30
050000971021     C                   MOVE      VHBNCL        W001A             1
050100971021     C     *IN30         IFEQ      *OFF
050200971021     C     W001A         ORLT      '0'
050300971021     C                   SETON                                        32
050400971021     C                   MOVE      *ZERO         VABNCL
050500010604     C                   eval      vinmsg = %trimr(vinmsg)
050600010604     C                             + ' ' + 'VHBNCL'
050700971021     C                   ELSE
050800971021     C                   MOVE      VHBNCL        VABNCL
050900971021     C                   ENDIF
051000941229     C*
051100971021     C                   MOVE      *ZERO         VABPKB
051200971021     C                   MOVE      VHBPKB        WASC09
051300010719     C                   IF        WI09 = *blanks
051400010719     C                   EVAL      WI09 = *zeros
051500010719     C                   ENDIF
051600971023     C                   TESTN                   WI09                 30
051700971023     C  N30              TESTN                   WI09                   30
051800971023     C  N30              TESTN                   WI09                     30
051900971021     C                   MOVE      WI09          W001A             1
052000971021     C     *IN30         IFEQ      *OFF
052100971021     C     W001A         ORLT      '0'
052200971021     C                   SETON                                        32
052300971021     C                   MOVE      *ZERO         VABPKB
052400010604     C                   eval      vinmsg = %trimr(vinmsg)
052500010604     C                             + ' ' + 'VHBPKB'
052600971021     C                   ELSE
052700971021     C                   MOVEL     WI09          VABPKB
052800010719     C                   IF        WD09 = *blanks
052900010719     C                   EVAL      WD09 = *zeros
053000010719     C                   ENDIF
053100971023     C                   TESTN                   WD09                 30
053200971023     C  N30              TESTN                   WD09                     30
053300971021     C                   MOVE      WD09          W001A             1
053400971021     C     *IN30         IFEQ      *OFF
053500971021     C     W001A         ORLT      '0'
053600971023     C     W001A         ANDNE     *BLANK
053700971021     C                   SETON                                        32
053800971021     C                   MOVE      *ZERO         VABPKB
053900010604     C                   eval      vinmsg = %trimr(vinmsg)
054000010604     C                             + ' ' + 'VHBPKB'
054100971021     C                   ELSE
054200971021     C                   MOVE      WD09          VABPKB
054300971021     C                   ENDIF
054400050118     C                   ENDIF
054500971021     C*
054600041001     C* Se nei parametri d ingresso NN è richiesto l'azzeramento del volume => traduco
054700041001     C                   IF        *IN51 = *OFF
054800971021     C                   MOVE      *ZERO         VABVLB
054900971021     C                   MOVE      VHBVLB        WASC07
055000010719     C                   IF        WI07 = *blanks
055100010719     C                   EVAL      WI07 = *zeros
055200010719     C                   ENDIF
055300971023     C                   TESTN                   WI07                 30
055400971023     C  N30              TESTN                   WI07                   30
055500971023     C  N30              TESTN                   WI07                     30
055600971021     C                   MOVE      WI07          W001A             1
055700971021     C     *IN30         IFEQ      *OFF
055800971021     C     W001A         ORLT      '0'
055900971023     C     W001A         ANDNE     *BLANK
056000971021     C                   SETON                                        32
056100971021     C                   MOVE      *ZERO         VABVLB
056200010604     C                   eval      vinmsg = %trimr(vinmsg)
056300010604     C                             + ' ' + 'VHBVLB'
056400971021     C                   ELSE
056500971021     C                   MOVEL     WI07          VABVLB
056600010719     C                   IF        WD07 = *blanks
056700010719     C                   EVAL      WD07 = *zeros
056800010719     C                   ENDIF
056900971023     C                   TESTN                   WD07                 30
057000971023     C  N30              TESTN                   WD07                   30
057100971023     C  N30              TESTN                   WD07                     30
057200971021     C                   MOVE      WD07          W001A             1
057300971021     C     *IN30         IFEQ      *OFF
057400971021     C     W001A         ORLT      '0'
057500971023     C     W001A         ANDNE     *BLANK
057600971021     C                   SETON                                        32
057700971021     C                   MOVE      *ZERO         VABVLB
057800010604     C                   eval      vinmsg = %trimr(vinmsg)
057900010604     C                             + ' ' + 'VHBVLB'
058000971021     C                   ELSE
058100971021     C                   MOVE      WD07          VABVLB
058200971021     C                   ENDIF
058300050118     C                   ENDIF
058400041001     C                   ELSE
058500041001     C* ...altrimenti forzo azzeramento del volume
058600041001     C                   Z-ADD     *zeros        VABVLB
058700041001     C                   ENDIF
058800941229     C*
058900971021     C                   MOVE      *ZERO         VABQFT
059000971021     C                   MOVE      VHBQFT        WASC15
059100010719     C                   IF        WI15 = *blanks
059200010719     C                   EVAL      WI15 = *zeros
059300010719     C                   ENDIF
059400971023     C                   TESTN                   WI15                 30
059500971023     C  N30              TESTN                   WI15                   30
059600971023     C  N30              TESTN                   WI15                     30
059700971021     C                   MOVE      WI15          W001A             1
059800971021     C     *IN30         IFEQ      *OFF
059900971021     C     W001A         ORLT      '0'
060000971023     C     W001A         ANDNE     *BLANK
060100971021     C                   SETON                                        32
060200971021     C                   MOVE      *ZERO         VABQFT
060300010604     C                   eval      vinmsg = %trimr(vinmsg)
060400010604     C                             + ' ' + 'VHBQFT'
060500971021     C                   ELSE
060600971021     C                   MOVEL     WI15          VABQFT
060700010719     C                   IF        WD15 = *blanks
060800010719     C                   EVAL      WD15 = *zeros
060900010719     C                   ENDIF
061000971023     C                   TESTN                   WD15                 30
061100971023     C  N30              TESTN                   WD15                   30
061200971023     C  N30              TESTN                   WD15                     30
061300971021     C                   MOVE      WD15          W001A             1
061400971021     C     *IN30         IFEQ      *OFF
061500971021     C     W001A         ORLT      '0'
061600971023     C     W001A         ANDNE     *BLANK
061700971021     C                   SETON                                        32
061800971021     C                   MOVE      *ZERO         VABQFT
061900010604     C                   eval      vinmsg = %trimr(vinmsg)
062000010604     C                             + ' ' + 'VHBQFT'
062100971021     C                   ELSE
062200971021     C                   MOVE      WD15          VABQFT
062300971021     C                   ENDIF
062400050118     C                   ENDIF
062500941229     C*
062600971021     C                   MOVE      *ZERO         VABCAS
062700971021     C                   MOVE      VHBCAS        WASC15
062800010719     C                   IF        WI15 = *blanks
062900010719     C                   EVAL      WI15 = *zeros
063000010719     C                   ENDIF
063100971023     C                   TESTN                   WI15                 30
063200971023     C  N30              TESTN                   WI15                   30
063300971023     C  N30              TESTN                   WI15                     30
063400971021     C                   MOVE      WI15          W001A             1
063500971021     C     *IN30         IFEQ      *OFF
063600971021     C     W001A         ORLT      '0'
063700971023     C     W001A         ANDNE     *BLANK
063800971021     C                   SETON                                        32
063900971021     C                   MOVE      *ZERO         VABCAS
064000010604     C                   eval      vinmsg = %trimr(vinmsg)
064100010604     C                             + ' ' + 'VHBCAS'
064200971021     C                   ELSE
064300971021     C                   MOVEL     WI15          VABCAS
064400010719     C                   IF        WD15 = *blanks
064500010719     C                   EVAL      WD15 = *zeros
064600010719     C                   ENDIF
064700971023     C                   TESTN                   WD15                 30
064800971023     C  N30              TESTN                   WD15                   30
064900971023     C  N30              TESTN                   WD15                     30
065000971021     C                   MOVE      WD15          W001A             1
065100971021     C     *IN30         IFEQ      *OFF
065200971021     C     W001A         ORLT      '0'
065300971023     C     W001A         ANDNE     *BLANK
065400971021     C                   SETON                                        32
065500971021     C                   MOVE      *ZERO         VABCAS
065600010604     C                   eval      vinmsg = %trimr(vinmsg)
065700010604     C                             + ' ' + 'VHBCAS'
065800971021     C                   ELSE
065900971021     C                   MOVE      WD15          VABCAS
066000971021     C                   ENDIF
066100050118     C                   ENDIF
066200971021     C*
066300971023     C                   TESTN                   VHBRMN               30
066400971023     C  N30              TESTN                   VHBRMN                 30
066500971023     C  N30              TESTN                   VHBRMN                   30
066600971021     C                   MOVE      VHBRMN        W001A             1
066700971021     C     *IN30         IFEQ      *OFF
066800971021     C     W001A         ORLT      '0'
066900971021     C                   SETON                                        32
067000971021     C                   MOVE      *ZERO         VABRMN
067100010604     C                   eval      vinmsg = %trimr(vinmsg)
067200010604     C                             + ' ' + 'VHBRMN'
067300971021     C                   ELSE
067400971021     C                   MOVE      VHBRMN        VABRMN
067500971021     C                   ENDIF
067600971021     C*
067700971023     C                   TESTN                   VHBNCD               30
067800971023     C  N30              TESTN                   VHBNCD                 30
067900971023     C  N30              TESTN                   VHBNCD                   30
068000971021     C                   MOVE      VHBNCD        W001A             1
068100971021     C     *IN30         IFEQ      *OFF
068200971021     C     W001A         ORLT      '0'
068300971023     C     W001A         ANDNE     *BLANK
068400971021     C                   SETON                                        32
068500971021     C                   MOVE      *ZERO         VABNCD
068600010604     C                   eval      vinmsg = %trimr(vinmsg)
068700010604     C                             + ' ' + 'VHBNCD'
068800971021     C                   ELSE
068900971021     C                   MOVE      VHBNCD        VABNCD
069000971021     C                   ENDIF
069100971021     C*
069200971023     C                   TESTN                   VHBNCA               30
069300971023     C  N30              TESTN                   VHBNCA                 30
069400971023     C  N30              TESTN                   VHBNCA                   30
069500971021     C                   MOVE      VHBNCA        W001A             1
069600971021     C     *IN30         IFEQ      *OFF
069700971021     C     W001A         ORLT      '0'
069800971023     C     W001A         ANDNE     *BLANK
069900971021     C                   SETON                                        32
070000971021     C                   MOVE      *ZERO         VABNCA
070100010604     C                   eval      vinmsg = %trimr(vinmsg)
070200010604     C                             + ' ' + 'VHBNCA'
070300971021     C                   ELSE
070400971021     C                   MOVE      VHBNCA        VABNCA
070500971021     C                   ENDIF
070600971021     C*
070700971023     C                   TESTN                   VHBZNC               30
070800971023     C  N30              TESTN                   VHBZNC                 30
070900971023     C  N30              TESTN                   VHBZNC                   30
071000971021     C                   MOVE      VHBZNC        W001A             1
071100971021     C     *IN30         IFEQ      *OFF
071200971021     C     W001A         ORLT      '0'
071300971023     C     W001A         ANDNE     *BLANK
071400971021     C                   SETON                                        32
071500971021     C                   MOVE      *ZERO         VABZNC
071600010604     C                   eval      vinmsg = %trimr(vinmsg)
071700010604     C                             + ' ' + 'VHBZNC'
071800971021     C                   ELSE
071900971021     C                   MOVE      VHBZNC        VABZNC
072000971021     C                   ENDIF
072100971021     C*
072200971023     C                   TESTN                   VHBDCR               30
072300971023     C  N30              TESTN                   VHBDCR                 30
072400971023     C  N30              TESTN                   VHBDCR                   30
072500971021     C                   MOVE      VHBDCR        W001A             1
072600971021     C     *IN30         IFEQ      *OFF
072700971021     C     W001A         ORLT      '0'
072800971023     C     W001A         ANDNE     *BLANK
072900971021     C                   SETON                                        32
073000971021     C                   MOVE      *ZERO         VABDCR
073100010604     C                   eval      vinmsg = %trimr(vinmsg)
073200010604     C                             + ' ' + 'VHBDCR'
073300971021     C                   ELSE
073400971021     C                   MOVE      VHBDCR        VABDCR
073500971021     C                   ENDIF
073600971021     C*
073700971023     C                   TESTN                   VHBHCR               30
073800971023     C  N30              TESTN                   VHBHCR                 30
073900971023     C  N30              TESTN                   VHBHCR                   30
074000971021     C                   MOVE      VHBHCR        W001A             1
074100971021     C     *IN30         IFEQ      *OFF
074200971021     C     W001A         ORLT      '0'
074300971023     C     W001A         ANDNE     *BLANK
074400971021     C                   SETON                                        32
074500971021     C                   MOVE      *ZERO         VABHCR
074600010604     C                   eval      vinmsg = %trimr(vinmsg)
074700010604     C                             + ' ' + 'VHBHCR'
074800971021     C                   ELSE
074900971021     C                   MOVE      VHBHCR        VABHCR
075000971021     C                   ENDIF
075100941229     C*
075200971021     C                   MOVE      *ZERO         VABVMD
075300971021     C                   MOVE      VHBVMD        WASC15
075400010719     C                   IF        WI15 = *blanks
075500010719     C                   EVAL      WI15 = *zeros
075600010719     C                   ENDIF
075700971023     C                   TESTN                   WI15                 30
075800971023     C  N30              TESTN                   WI15                   30
075900971023     C  N30              TESTN                   WI15                     30
076000971021     C                   MOVE      WI15          W001A             1
076100971021     C     *IN30         IFEQ      *OFF
076200971021     C     W001A         ORLT      '0'
076300971023     C     W001A         ANDNE     *BLANK
076400971021     C                   SETON                                        32
076500971021     C                   MOVE      *ZERO         VABVMD
076600010604     C                   eval      vinmsg = %trimr(vinmsg)
076700010604     C                             + ' ' + 'VHBVMD'
076800971021     C                   ELSE
076900971021     C                   MOVEL     WI15          VABVMD
077000010719     C                   IF        WD15 = *blanks
077100010719     C                   EVAL      WD15 = *zeros
077200010719     C                   ENDIF
077300971023     C                   TESTN                   WD15                 30
077400971023     C  N30              TESTN                   WD15                   30
077500971023     C  N30              TESTN                   WD15                     30
077600971021     C                   MOVE      WD15          W001A             1
077700971021     C     *IN30         IFEQ      *OFF
077800971021     C     W001A         ORLT      '0'
077900971023     C     W001A         ANDNE     *BLANK
078000971021     C                   SETON                                        32
078100971021     C                   MOVE      *ZERO         VABVMD
078200010604     C                   eval      vinmsg = %trimr(vinmsg)
078300010604     C                             + ' ' + 'VHBVMD'
078400971021     C                   ELSE
078500971021     C                   MOVE      WD15          VABVMD
078600971021     C                   ENDIF
078700050118     C                   ENDIF
078800971021     C*
078900971023     C                   TESTN                   VHBANT               30
079000971023     C  N30              TESTN                   VHBANT                 30
079100971023     C  N30              TESTN                   VHBANT                   30
079200971021     C                   MOVE      VHBANT        W001A             1
079300971021     C     *IN30         IFEQ      *OFF
079400971021     C     W001A         ORLT      '0'
079500971023     C     W001A         ANDNE     *BLANK
079600971021     C                   SETON                                        32
079700971021     C                   MOVE      *ZERO         VABANT
079800010604     C                   eval      vinmsg = %trimr(vinmsg)
079900010604     C                             + ' ' + 'VHBANT'
080000971021     C                   ELSE
080100971021     C                   MOVE      VHBANT        VABANT
080200971021     C                   ENDIF
080300971021     C*
080400971021     C* CAMPI ALFABETICI:
080500971021     C                   MOVE      VHBATB        VABATB
080600971021     C                   MOVE      VHBCBO        VABCBO
080700971021     C                   MOVE      VHBRSD        VABRSD
080800020117     C* == verifico se esiste il carattere @ che blocca POSTEL/piccione, se c'è diventa A ==
080900020117     C     '@':'A'       XLATE     VABRSD        VABRSD
081000020117     C* ==
081100971021     C                   MOVE      VHBRD2        VABRD2
081200971021     C                   MOVE      VHBIND        VABIND
081300971021     C                   MOVE      VHBCAD        VABCAD
081400971021     C                   MOVE      VHBLOD        VABLOD
081500971021     C                   MOVE      VHBPRD        VABPRD
081600971021     C                   MOVE      VHBNZD        VABNZD
081700971021     C                   MOVE      VHBGC1        VABGC1
081800971021     C                   MOVE      VHBGC2        VABGC2
081900971021     C                   MOVE      VHBTSP        VABTSP
082000971021     C                   MOVE      VHBVAS        VABVAS
082100971021     C                   MOVE      VHBNAS        VABNAS
082200971021     C                   MOVE      VHBTIC        VABTIC
082300971021     C                   MOVE      VHBVCA        VABVCA
082400971021     C                   MOVE      VHBGCA        VABGCA
082500971021     C                   MOVE      VHBRMA        VABRMA
082600971021     C                   MOVE      VHBXCO        VABXCO
082700971021     C                   MOVE      VHBNOT        VABNOT
082800971021     C                   MOVE      VHBNT2        VABNT2
082900971021     C                   MOVE      VHBCTM        VABCTM
083000971021     C                   MOVE      VHBFFD        VABFFD
083100971021     C                   MOVE      VHBTCR        VABTCR
083200971021     C                   MOVE      VHBCTS        VABCTS
083300971021     C                   MOVE      VHBFTM        VABFTM
083400971021     C                   MOVE      VHBVAD        VABVAD
083500971021     C                   MOVE      VHBGMA        VABGMA
083600971021     C                   MOVE      VHBGGA        VABGGA
083700971021     C                   MOVE      VHBGVA        VABGVA
083800971021     C                   MOVE      VHBTC1        VABTC1
083900971021     C                   MOVE      VHBTC2        VABTC2
084000971021     C                   MOVE      VHBSCL        VABSCL
084100971021     C                   MOVE      VHBRMO        VABRMO
084200971021     C                   MOVE      VHBCMO        VABCMO
084300971021     C                   MOVE      VHBNMO        VABNMO
084400971021     C* ABBLENCO SE INSERITO CARATTERE X PCSUPPORT>HALLEY
084500971021     C* NELLA NAZIONE ORIGINALE
084600971021     C                   MOVE      VHBNMO        WA1D              1
084700971021     C                   MOVEL     VHBNMO        WA1S              1
084800971021     C     WA1D          IFNE      *BLANK
084900971021     C     WA1S          ANDEQ     *BLANK
085000971021     C                   MOVE      *BLANK        VABNMO
085100030616     C                   ENDIF
085200030616     C* VALORIZZO CAMPI RELATIVI AL "CMR"
085300030710     C                   EVAL      VABCMR = wCMR
085400030616     C                   EVAL      VABDCM = DATCOR
085500030616     C                   EVAL      VABDTS = DATCOR
085600040402     C                   EVAL      VABHMS = ORACOR
085700030616     C                   EVAL      VABCNT = 1
085800041025     C*
085900041025     C* Se richiesto nei parametri d lancio reperisco in automatico la provincia tramite TISI95R
086000041025     C                   IF        *IN52 = *ON
086100041025     C* Reperisco la provincia dal CAP e dalla località
086200041025     C                   IF        VABCAD <> *blanks AND
086300050907     C                             VABPRD  = *blanks AND
086400050907     C                             VABNZD  = *blanks
086500041025     C                   CLEAR                   TISI95DS
086600041025     C                   EVAL      I95TCN = '3'
086700041025     C                   Z-ADD     datcor        I95DAT
086800041025     C                   EVAL      I95CAP = VABCAD
086900041025     C                   EVAL      I95LOC = VABLOD
087000041025     C                   CALL      'TISI95R'
087100041025     C                   PARM                    TISI95DS
087200041025     C                   EVAL      VABPRD = O95PRV
087300041025     C                   ENDIF
087400041025     C                   ENDIF
087500081202     C*
087600081202     C* Forzatura x NSP = RMA
087700081202     C                   EVAL      PiStr=%trim(VABRMA)
087800081202     C                   EXSR      CHKNUM
087900081202     C                   IF        PiInt=*on
088000081202     C                   Z-ADD     PiVal         VABNSP
088100081202     C                   ELSE
088200081202     C                   SETON                                        31
088300081202     C                   EVAL      vinmsg = %trimr(vinmsg)
088400081202     C                             + ' ' + 'VABNSP/RMA'
088500081202     C                   ENDIF
088600080605     C*
088700080605     C* Forzatura x personalizzazione CEVA LOGISTIC - TIM
088800080606     C                   eval      VABNOT = %subst(%editc(VABRMN:'X'):
088900080609     C                                      %len(VABRMN)-10+1)
089000080605     C                   eval      VABNT2 = *blanks
089100930409     C*
089200010604     C  N31              ADD       1             §CTROK            7 0
089300010604     C   32              ADD       1             §CTRMO            7 0
089400010604     C   31              ADD       1             §CTRNO            7 0
089500030616     C  N31              WRITE     EDIVAB00
089600990910     C*
089700010601     C                   if        *in31 = *off and
089800010601     C                             *in32 = *off
089900990910     C                   eval      vinflg = '1'
090000990910     C                   else
090100000621     C                   eval      recko = vindta
090200030710     C                   if        %open(tis7tebps)
090300030407     C   50              except    rigadett
090400010612     C                   endif
090500990910     C                   eval      vinflg = '2'
090600990910     C                   endif
090700991022     C                   endif
090800990910     C                   endif
090900010601     C*
091000010601     C                   endif
091100000905     C*
091200000905     C                   else
091300000905     C                   eval      vinflg = '1'
091400000905     C                   endif
091500000905     C*
091600000905     C  N70              update    tivin000
091700991022     C*
091800991022     C  N70              ENDdo
091900010530     C
092000010530     C*
092100010530     C* SOLO se sono stati elaborati tutti i record aggiorno lo stato del log
092200010530     C                   if        cntNonEl = *zeros or
092300010530     C                             flgMulti = '0'
092400990910     C* Se non ci sono record con errori ...
092500010530     C                   if        §ctrno = 0 and
092600010601     C                             §ctrmo = 0 and
092700010530     C                             flgStato <> '2'
092800990910     C* ... restituisco esito OK.
092900990921     C                   eval      wrkesito = '0'
093000990910     C                   else
093100010601     C                   if        §ctrok > 0
093200990921     C                   eval      wrkesito = '1'
093300000710     C                   else
093400010615     C                   if        flgOk = '0'
093500010615     C                   eval      wrkesito = '2'
093600010615     C                   else
093700010615     C                   eval      wrkesito = '6'
093800010615     C                   endif
093900990910     C                   endif
094000000710     C                   endif
094100010530     C                   else
094200010530     C                   eval      wrkesito = '9'
094300010530     C                   endif
094400030710     C*
094500030710 X  1C                   else
094600030710     C                   eval      wrkesito = '8'
094700030710     C                   eval      flgGiro = '1'
094800030710 E  1C                   endif
094900030710     C*
095000990914     C                   if        %open(tivin00r)
095100990908     C                   close     tivin00r
095200990914     C                   endif
095300030616     C                   if        %open(edivabwr)
095400030616     C                   close     edivabwr
095500990914     C                   endif
095600010530     C*
095700010530     C                   if        vlrpoi <> 999
095800010530     C                   eval      invfil = vlrpoi
095900010530     C                   endif
096000990910     C*
096100990920     C                   if        §ctrok > 0
096200010530     C                             and invfil > *zeros
096300000613     C                   exsr      invio
096400990920     C                   endif
096500010612     C*
096600010612     C                   if        flgGiro = '1'
096700010612     C                   exsr      endela
096800010612     C                   endif
096900990920     C*
097000910830     C                   ENDSR
097100000613     C***
097200990920
097300010530
097400010530
097500010530
097600010530      /TITLE Verifica tipo di invio ("Multi-filiale" o meno)
097700010530     C     repfil        BEGSR
097800010530     C*
097900010530     C                   if        invfil = *zeros and
098000010530     C                             depfil > *zeros and
098100010613     C                             (vinflg = *blanks or
098200010613     C                              vinflg = *zeros)
098300010530     C
098400010530     C                   eval      invfil = depfil
098500010530     C                   endif
098600010530     C*
098700010530     C                   if        depfil <> invfil and
098800010530     C                             invfil > *zeros
098900010530     C                   eval      flgMulti = '1'
099000010530     C                   if        vinflg = *blanks
099100010530     C                   add       1             cntNonEl
099200010530     C                   endif
099300010530     C                   endif
099400010530     C*
099500010530     C                   if        vinflg = '2'
099600010530     C                   eval      flgStato = '2'
099700010530     C                   endif
099800010530     C*
099900010530     C                   ENDSR
100000010530     C***
100100010530
100200010530
100300010530
100400010530
100500990920      /TITLE Invio dei dati al punto operativo.
100600000613     C     invio         BEGSR
100700990920     C*
100800990920     C                   reset                   dscmz
100900030618     C                   move      invfil        cmzdst
101000030618     C***                move      '789'         cmzdst
101100990920     C                   eval      cmzfld = vlrfou
101200990920     C                   eval      cmzmbd = vlrhdl
101300990920     C                   eval      %subst(cmzmbd:1:1) = 'M'
101400000710     C                   if        prmfir = *blanks
101500030616     C                   eval      cmzfla = 'EDIVAB0F'
101600030616     C                   eval      cmzmba = 'EDIVAB0F'
101700000710     C                   else
101800000710     C                   eval      cmzfla = prmfir
101900000710     C                   eval      cmzmba = prmfir
102000000710     C                   endif
102100990920     C                   eval      cmznrr = *zeros
102200990920     C                   move      §ctrok        cmznrr
102300021003     C                   eval      cmzlba = vlrfl1
102400990920     C                   call(e)   'TIS711C'
102500990920     C                   parm                    dscmz
102600990921     C                   parm      *blanks       esito
102700990920     C                   if        %error
102800990920     C                             or cmzerr = '1'
102900990921     C                             or esito  = '1'
103000000710     C                   eval      wrkesito = '3'
103100990920     C                   endif
103200990920     C*
103300000613     C                   ENDSR
103400000613     C***
103500010530
103600010530
103700010530
103800010530
103900010530      /TITLE Invio dei dati al punto operativo.
104000010530     C     opeini        BEGSR
104100010530     C*
104200010530     C* Inizializzo flag e contatori operativi
104300010530     C                   movel     '0'           flgGiro           1
104400010530     C                   movel     '0'           flgMulti          1
104500010530     C                   movel     '1'           flgStato          1
104600010615     C                   movel     '0'           flgOk             1
104700010530     C                   z-add     *zeros        cntNonEl         10 0
104800010530     C                   z-add     *zeros        depfil            3 0
104900010530     C                   z-add     *zeros        invfil            3 0
105000010530     C*
105100010530     C                   ENDSR
105200010530     C***
105300010530
105400081202
105500081202     C*----------------------------------------------------*
105600081202     C*  CONTROLLO NUMERICITA' CAMPI
105700081202     C*----------------------------------------------------*
105800081202     C     CHKNUM        BEGSR
105900081202     C*
106000081202     C                   IF        PiDecChr = *blanks
106100081202     C                   EVAL      PiDecChr = '.'
106200081202     C                   ENDIF
106300081202     C*
106400081202     C                   callp(e)  UBISNUM_Check(PiStr
106500081202     C                                          :PiDecChr
106600081202     C                                          :PiVal
106700081202     C                                          :PiNum
106800081202     C                                          :PiInt)
106900081202     C*
107000081202     C                   IF        %error
107100081202     C                   EVAL      PiInt=*off
107200081202     C                   ENDIF
107300081202     C*
107400081202     C                   ENDSR
107500081202     C***
107600010530
107700081202
107800990910
107900000613     C     *inzsr        BEGSR
108000990910     C*
108100990910     C     *entry        plist
108200990920     C                   parm                    tivlrds
108300990921     C                   parm      wrkesito      esito
108400000724     C                   parm                    prmlit
108500000710     C                   parm                    prmfir
108600030616     C*
108700030710     C* Reperisco subito il nome del file "UPLOADATO" (se c'è')
108800030710     C                   move(p)   vlrMSG        wNomeFile
108900030710     C*
109000030710     C* Reperisco la posizione iniziale ed il numero d byte da considerare x il reperimento
109100030710     C* del codice CMR dal nome del file importato.
109200030710     C                   if        wNomeFile <> *blanks AND
109300030710     C                             vlrppt<>*blanks
109400041001     C* Verifico se sui parametri del traduttore è richiesta la stampa delle traduzioni
109500030710     C                   if        %subst(vlrppt:1:1) = 'S'
109600030710     C                   seton                                        50
109700030710     C                   else
109800030710     C                   setoff                                       50
109900030710     C                   endif
110000041001     C* Verifico se sui parametri del traduttore è richiesto l'azzeramento del volume
110100041001     C                   if        %subst(vlrppt:6:1) = 'S'
110200041001     C                   seton                                        51
110300041001     C                   else
110400041001     C                   setoff                                       51
110500041001     C                   endif
110600041025     C*
110700041025     C* Verifico se sui parametri del traduttore è richiesto il rperimento provincia
110800041025     C* (naturalmente solo se nn già presente)
110900041025     C                   if        %subst(vlrppt:7:1) = 'S'
111000041025     C                   seton                                        52
111100041025     C                   else
111200041025     C                   setoff                                       52
111300041025     C                   endif
111400041001     C*
111500081202     C                   if        %subst(vlrppt:2:4) > *zeros
111600030710     C                   eval      wPosDaA = %subst(vlrppt:2:2)
111700030710     C                   move(p)   wPosDaA       wPosDa
111800030710     C                   eval      wLungA  = %subst(vlrppt:4:2)
111900030710     C                   move(p)   wLungA        wLung
112000030710     C                   eval      wCMR = %subst(wNomeFile:wPosDa:wLung)
112100081202     C                   else
112200081202     C                   eval      wCMR = 'CEVA - TIM'
112300081202     C                   endif
112400030710     C                   seton                                        60
112500030710     C                   else
112600050914     C                   if        vlrisv = 'UL'
112700050914     C                   movel     datcor        wDataOraAlfa
112800050914     C                   move      oracor        wDataOraAlfa
112900050914     C                   eval      wCMR = 'FNVAB' + wDataOraAlfa
113000050914     C                   seton                                        60
113100050914     C                   else
113200030710     C                   setoff                                       60
113300050914     C                   endif
113400030710     C                   endif
113500010606     C*
113600010606     C* CALCOLA LA DATA CORRENTE
113700010606     C                   time                    wn14             14 0
113800010606     C                   move      wn14          wn8               8 0          *DATA (8) IN G/M/AA
113900040402     C                   movel     wn14          oracor            6 0          *ORA
114000010606     C                   z-add     wn8           g08dat
114100010606     C                   z-add     *zeros        g08inv
114200010606     C                   movel     '0'           g08err
114300010606     C                   call      'XSRDA8'
114400010606     C                   parm                    wlbda8
114500010606     C                   z-add     g08inv        datcor            8 0          *DATA CORRENTE AA/M/
114600000613     C*
114700000613     C                   ENDSR
114800000613     C***
114900990908
115000030710     Otis7tebp  E            riepilogo         2
115100990915     O                                              'Upload via Internet'
115200990915     O                                           +1 'Traduzione TIVIN00R -'
115300030616     O                                           +1 'EDIVABWR'
115400990915     O                                           +1 'Testate bolle'
115500990915     O          E            riepilogo   2
115600990915     O                       wrkdata
115700990915     O                       wrkora              +1
115800990915     O                       procname            +1
115900990915     O          E            riepilogo   2
116000990915     O                                              'Cliente..................:'
116100990915     O                       VABCCM        z     +1
116200030618     O          E            riepilogo   2
116300030618     O                                              'Numero CMR...............:'
116400030618     O                       VABCMR              +1
116500990915     O          E            riepilogo   2
116600990920     O                                              'Riferimento Strategi.....:'
116700990920     O                       vlrhdl              +1
116800990915     O          E            riepilogo   2
116900990915     O                                              'Giusti...................:'
117000971022     O                       §CTROK        2   +  1
117100990915     O          E            riepilogo   2
117200990915     O                                              'Sbagliati e corretti.....:'
117300971022     O                       §CTRMO        2   +  1
117400990915     O          E            riepilogo   2
117500990915     O                                              'Sbagliati e scartati.....:'
117600971022     O                       §CTRNO        2   +  1
117700000613
117800030710     Otis7tebps E            testdett          2
117900000613     O                                              'Upload via Internet'
118000000613     O                                           +1 'Traduzione TIVIN00R -'
118100030616     O                                           +1 'EDIVABWR'
118200000613     O                                           +1 'Testate bolle'
118300000616     O          E            testdett    3
118400000613     O                                           +2 'N° rec'
118500000613     O                                           +3 'Anteprima contenuto'
118600000616     O          E            rigadett    2
118700000613     O                       rrnum               +2
118800000621     O                       recko               +3
118900000616     O          E            findett     2
119000000613     O                       wrkdata
119100000613     O                       wrkora              +1
119200000613     O                       procname            +1
119300000616     O          E            findett     2
119400000613     O                                              'Cliente..................:'
119500000613     O                       VABCCM        z     +1
119600030618     O          E            findett     2
119700030618     O                                              'Numero CMR...............:'
119800030618     O                       VABCMR              +1
119900000616     O          E            findett     2
120000000613     O                                              'Riferimento Strategi.....:'
120100000613     O                       vlrhdl              +1
120200000616     O          E            findett     2
120300000613     O                                              'Giusti...................:'
120400000613     O                       §CTROK        2   +  1
120500000616     O          E            findett     2
120600000613     O                                              'Sbagliati e corretti.....:'
120700000613     O                       §CTRMO        2   +  1
120800000616     O          E            findett     2
120900000613     O                                              'Sbagliati e scartati.....:'
121000000613     O                       §CTRNO        2   +  1
121100000512** CMD - COMANDI CL
121200030710OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7TEBP)  TOFILE(TIS7T1P)  FORMTYPE(RICCLI) OUTQ(
121300030710OVRPRTF OVRSCOPE(*CALLLVL) FILE(TIS7TEBPS) TOFILE(TIS7T1PS) FORMTYPE(RICCLI) OUTQ(
121400030710DLTOVR FILE(TIS7TEBP TIS7TEBPS) LVL(*)
121500000512
121600000512
