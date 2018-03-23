000100981211     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO)                ACTGRP(QILE)
000200981211     H*PARMS BNDDIR(PJXBND PJCBND)
000300951124     H*PARMS CVTOPT(*DATETIME)
000400951124     H DECEDIT('0,') DATEDIT(*DMY/)
000500951124     F*---------------------------------------------------------------*
000600951124     F*            STAMPA DI SERVIZIO MODELLO 770                     *
000700951124     F*                   RIGHE PERCIPIENTI                           *
000701161020R235 F*
000702161020R235 F* Modificata dicitura pgm,
000703161020R235 F*  da Stampa di servizio Mod.770 righe percip.
000704161020R235 F*   a Stampa di servizio CU percipienti
000800951124     F*---------------------------------------------------------------*
000900951124     FND77001L  IF   E           K DISK
001000951124     F*------------
001100951124     FQSYSPRT   O    F  132        PRINTER OFLIND(*IN39)
001200951124     D*---------------------------------------------------------------*
001300951124     D KPJBA         E DS
001400951124     D*------------
001500951124     D ndcr70ds      E DS
001600951124     D*------------
001700951124     D* Dati di ambiente ottenuti da XSOC
001800951124     D SOC001        E DS                  EXTNAME(XSOC001DS)
001900951124     D*------------
002000951124     D* DS Interna per dati di output di XSOC
002100951124     D XSOCDS          S           1000
002200951127     D*-------------
002300951127     D* Reperimento nome PGM
002400951127     D STATUS         SDS           333
002500951127     D  DSPGM            *PROC
002600951127     D  PARMS            *PARMS
002700951124     D*------------
002800951124     D SK              S             66    DIM(2) CTDATA PERRCD(1)              STAMPA
002900951124     D*------------
003000951124     D DatMin          C                   CONST('0001-01-01')
003100000117C1309D ZDateISO        S               D
003200951124     C*---------------------------------------------------------------*
003300000000     C     *ENTRY        PLIST                                                  *
003400000000     C                   PARM                    KPJBA                          *
003500951124     C*
003600951124     C                   MOVEL     KPJBU         ndcr70ds                       *
003700951124     C*
003800000000     C     KEY770        KLIST                                                  *
003900951124     C                   KFLD                    XSCSOC
004000951124     C                   KFLD                    N77ACF
004100951124     C*
004200890419     C     KEY771        KLIST                                                  *
004300951124     C                   KFLD                    XSCSOC                         *
004400951124     C                   KFLD                    N77ACF                         *
004500951124     C                   KFLD                    N77QUA                         *
004600951124     C*
004700951124     C* Chiamata a xsoc
004800951124     C                   move      socR70        SOCXSC
004900951124    >C                   MOVEL     'SOC001'      TIPXSC
005000951124     C                   CALL      'XSOC'
005100951124     C                   PARM                    TIPXSC            6
005200951124     C                   PARM                    SOCXSC            3
005300951124     C                   PARM                    CDSXSC            9 0
005400951124     C                   PARM                    MODXSC            3
005500951124     C                   PARM      *BLANKS       RTNXSC            1
005600951124     C                   PARM                    XSOCDS
005700951124     C                   PARM                    KPJBA
005800951124   1 C     RTNXSC        IFNE      '1'
005900951124    >C                   MOVEL     XSOCDS        SOC001
006000951124   1xC                   ELSE
006100951124     C                   GOTO      FINE
006200951124   1-C                   ENDIF
006300951127     C*
006400951127     C* Valorizzazione campi univoci testate
006500951127     C                   MOVEL     XSCDSI        NOMDIT           20
006600951127     C                   MOVEL     DSPGM         NOMPGM           10
006700951127     C* Reperisco nome sistema informatico
006800951127     C                   CALL      'XNETA'                              21
006900951127     C                   PARM                    NOMSYS            8
007000951124     C*
007100951124     C                   seton                                        39
007200000000     C*----------------------------------------------------*
007300951124     C                   z-add     annoR70       n77acf
007400951124     C     KEY770        SETLL     ND77001L                                     *
007500951124     C     KEY770        READE     ND77001L                               90    *
007600890419     C   90              GOTO      FINE                                         *
007700951124     C*
007800951124     C     flagDR70      IFEQ      *on                                          *
007900020415     C                   MOVE      'AU'          N77QUA                         *
008000951124     C     KEY771        SETLL     ND77001L                                     *
008100951124     C     KEY771        READE     ND77001L                               90    *
008200890419     C  N90              EXSR      ELAB                                         *
008300951124     C                   ENDIF
008400951124     C*
008500951124     C* Stampo totali generali
008600951124     C     *in39         IFEQ      *on                                          *
008700951127     C                   time                    ORA
008800890419     C                   EXCEPT    TESTA                                        *
008900951124     C                   setoff                                       39        *
009000951124     C                   ENDIF                                                  *
009100890419     C                   EXCEPT    TOTALE                                       *
009200951124     C*
009300890419     C     FINE          TAG                                                    *
009400890419     C                   SETON                                        LR        *
009500951124     C*
009600890419     C*----------------------------------------------------*
009700890419     C*  ELABORAZIONE DETTAGLIO                            *
009800890419     C*----------------------------------------------------*
009900890419     C     ELAB          BEGSR                                                  *
010000951124     C*
010100010525     C                   Z-ADD     0             QUAITC           15 2          *
010200010525     C                   Z-ADD     0             QUAIMN           15 2          *
010300010525     C                   Z-ADD     0             QUASNS           15 2          *
010400010525     C                   Z-ADD     0             QUAIMS           15 2          *
010500010525     C                   Z-ADD     0             QUARAC           15 2          *
010600150630R186 C                   Z-ADD     0             QUARIM           15 2          *
010700010525     C                   Z-ADD     0             QUANET           15 2          *
010800140516R148 C                   Z-ADD     0             QUAARS           15 2          *
010900140516R148 C                   Z-ADD     0             QUAARI           15 2          *
011000140516R148 C                   Z-ADD     0             QUAARG           15 2          *
011100140516R148 C                   Z-ADD     0             QUAACS           15 2          *
011200140516R148 C                   Z-ADD     0             QUAACG           15 2          *
011300140516R148 C                   Z-ADD     0             QUAACTA          15 2          *
011400100524     C                   MOVE      N77QUA        SAVQUA            2            *
011500100524R024 C*
011600100524  "  C                   Z-ADD     0             QUASIMP          15 2
011700100524  "  C                   Z-ADD     0             QUASIMS          15 2
011800100524  "  C                   Z-ADD     0             QUASRAC          15 2
011900100524  "  C                   Z-ADD     0             QUASRIM          15 2
012000100524  "  C                   Z-ADD     0             QUASRSO          15 2
012100100524  "  C                   Z-ADD     0             QUASARA          15 2
012200100524  "  C                   Z-ADD     0             QUASARI          15 2
012300100524R024 C                   Z-ADD     0             QUASARS          15 2
012400140516R148 C                   Z-ADD     0             QUASACA          15 2
012500140516  "  C                   Z-ADD     0             QUASACI          15 2
012600140516R148 C                   Z-ADD     0             QUASACS          15 2
012700951124     C*
012800890419     C                   DO        *HIVAL                                       *
012900951124     C                   MOVEL     N77COG        COG                            *
013000951124     C                   MOVEL     N77NOM        NOM                            *
013100951124     C                   MOVEL     N77LNC        LNC                            *
013200951124     C                   MOVEL     N77CIT        CIT                            *
013300130207R110 C****               MOVEL     N77C77        C77                            *
013400130207R110 C                   MOVEL     N77C77        C77                            *
013500130207R110 C                   MOVEL     N77Codice     Codice                         *
013600951124     C                   MOVEL     N77VIA        VIA                            *
013700951124     C     N77DNC        IFNE      DatMin
013800951124     C                   CALLb     'XDT4000'
013900951124     C                   PARM      N77DNC        XDTAMG           10
014000951124     C     DNC           PARM                    XDTGMA            6 0
014100951124     C                   PARM      2             XDTSTA            1 0
014200951124     C                   ELSE
014300951124     C                   Z-ADD     0             DNC
014400951124     C                   ENDIF
014500150630R186 C****               MOVE      N77CSE        CSE
014600150630R186 C                   MOVEL     N77CSE        CSE
014700010525     C                   Z-ADD     N77ITC        ITC              15 2
014800010525     C                   Z-ADD     N77IMN        IMN              15 2
014900010525     C                   Z-ADD     N77SNS        SNS              15 2
015000010525     C                   Z-ADD     N77IMS        IMS              15 2
015100010525     C                   Z-ADD     N77RAC        RAC              15 2
015200150630R186 C                   Z-ADD     N77RIM        RIM              15 2
015300010525     C                   Z-ADD     N77NET        NET              15 2
015400100524R024 C*
015500100524  "  C                   Setoff                                       78
015600110318R024 C                   If        N77ASCDF <> *blanks
015700110318R055 C*** ex R024                  or N77ASIMP <> 0
015800110318R055 C                             or N77CAUAS <> *blanks
015900110318R024 C                             or N77ASIMS <> 0
016000100524  "  C                             or N77ASRAC <> 0
016100100524  "  C                             or N77ASRIM <> 0
016200100524  "  C                             or N77ASRSO <> 0
016300100524  "  C                             or N77ASARA <> 0
016400100524  "  C                             or N77ASARI <> 0
016500140516R024 C                             or N77ASARS <> 0
016600140516R148 C                             or N77ASACS <> 0
016700140516  "  C                             or N77ASACI <> 0
016800140516R148 C                             or N77ASACA <> 0
016900140516R024 C                   Seton                                        78
017000110318R024 C                   MoveL     N77ASCDF      SCDF             16
017100110318R055 C*** ex R024        Z-ADD     N77ASIMP      SIMP             15 2
017200110318R055 C                   MoveL     N77CauAS      SCAU              1
017300110318R024 C                   Z-ADD     N77ASIMS      SIMS             15 2
017400100524  "  C                   Z-ADD     N77ASRAC      SRAC             15 2
017500100524  "  C                   Z-ADD     N77ASRIM      SRIM             15 2
017600100524  "  C                   Z-ADD     N77ASRSO      SRSO             15 2
017700100524  "  C                   Z-ADD     N77ASARA      SARA             15 2
017800100524  "  C                   Z-ADD     N77ASARI      SARI             15 2
017900140516R024 C                   Z-ADD     N77ASARS      SARS             15 2
018000140516R148 C                   Z-ADD     N77ASACS      SACS             15 2
018100140516  "  C                   Z-ADD     N77ASACI      SACI             15 2
018200140516R148 C                   Z-ADD     N77ASACA      SACA             15 2
018300140516R024 C                   EndIf
018400100524R024 C*
018500080131C2077C                   Clear                   IndEst           74
018600080131C2077C                   Movel     N77RCit       IndEst
018700080131C2077C                   Cat       N77RVia:1     IndEst
018800951124     C*
018900951124     C* Totalizzo movimenti parziali per quadro
019000951124     C                   ADD       N77ITC        QUAITC                         *
019100951124     C                   ADD       N77IMN        QUAIMN                         *
019200951124     C                   ADD       N77SNS        QUASNS                         *
019300951124     C                   ADD       N77IMS        QUAIMS                         *
019400951124     C                   ADD       N77RAC        QUARAC                         *
019500150630R186 C                   ADD       N77RIM        QUARIM                         *
019600951124     C                   ADD       N77NET        QUANET                         *
019700140516R148 C                   ADD       N77ARS        QUAARS           15 2          *
019800140516R148 C                   ADD       N77ARI        QUAARI           15 2          *
019900140516R148 C                   ADD       N77ARG        QUAARG           15 2          *
020000140516R148 C                   ADD       N77ACS        QUAACS           15 2          *
020100140516R148 C                   ADD       N77ACG        QUAACG           15 2          *
020200140516R148 C                   ADD       N77ACTA       QUAACTA          15 2          *
020300100524R024 C*
020400110318R055 C*** ex R024        ADD       N77ASIMP      QUASIMP          15 2
020500110318R024 C                   ADD       N77ASIMS      QUASIMS          15 2
020600100524  "  C                   ADD       N77ASRAC      QUASRAC          15 2
020700100524  "  C                   ADD       N77ASRIM      QUASRIM          15 2
020800100524  "  C                   ADD       N77ASRSO      QUASRSO          15 2
020900100524  "  C                   ADD       N77ASARA      QUASARA          15 2
021000100524  "  C                   ADD       N77ASARI      QUASARI          15 2
021100100524R024 C                   ADD       N77ASARS      QUASARS          15 2
021200140516R148 C                   ADD       N77ASACA      QUASACA          15 2
021300140516  "  C                   ADD       N77ASACI      QUASACI          15 2
021400140516R148 C                   ADD       N77ASACS      QUASACS          15 2
021500951124     C*
021600951124     C* Sommo nei totali generali
021700010525     C                   ADD       N77ITC        TOTITC           15 2          *
021800010525     C                   ADD       N77IMN        TOTIMN           15 2          *
021900010525     C                   ADD       N77SNS        TOTSNS           15 2          *
022000010525     C                   ADD       N77IMS        TOTIMS           15 2          *
022100010525     C                   ADD       N77RAC        TOTRAC           15 2          *
022200150630R186 C                   ADD       N77RIM        TOTRIM           15 2          *
022300010525     C                   ADD       N77NET        TOTNET           15 2          *
022400140516R148 C                   ADD       N77ARS        TOTARS           15 2          *
022500140516R148 C                   ADD       N77ARI        TOTARI           15 2          *
022600140516R148 C                   ADD       N77ARG        TOTARG           15 2          *
022700140516R148 C                   ADD       N77ACS        TOTACS           15 2          *
022800140516R148 C                   ADD       N77ACG        TOTACG           15 2          *
022900140516R148 C                   ADD       N77ACTA       TOTACTA          15 2          *
023000100524R024 C*
023100110318R055 C*** ex R024        ADD       N77ASIMP      TOTSIMP          15 2
023200110318R024 C                   ADD       N77ASIMS      TOTSIMS          15 2
023300100524  "  C                   ADD       N77ASRAC      TOTSRAC          15 2
023400100524  "  C                   ADD       N77ASRIM      TOTSRIM          15 2
023500100524  "  C                   ADD       N77ASRSO      TOTSRSO          15 2
023600100524  "  C                   ADD       N77ASARA      TOTSARA          15 2
023700100524  "  C                   ADD       N77ASARI      TOTSARI          15 2
023800100524R024 C                   ADD       N77ASARS      TOTSARS          15 2
023900140516R148 C                   ADD       N77ASACA      TOTSACA          15 2
024000140516  "  C                   ADD       N77ASACI      TOTSACI          15 2
024100140516R148 C                   ADD       N77ASACS      TOTSACS          15 2
024200940317     C*
024300951124     C* Stampa dettaglio
024400951124     C     *in39         IFEQ      *on                                          *
024500951127     C                   time                    ORA               6 0
024600890419     C                   EXCEPT    TESTA                                        *
024700951124     C                   setoff                                       39        *
024800951124     C                   ENDIF                                                  *
024801160204R211 C                   EXCEPT    DETPER
024802160204  "  C                   If        ITC < 0 or
024803160204  "  C                             IMN < 0 or
024804160204  "  C                             SNS < 0 or
024805160204  "  C                             IMS < 0 or
024806160204  "  C                             RAC < 0 or
024807160204  "  C                             RIM < 0 or
024808160204  "  C                             NET < 0
024809160204  "  C                   EXCEPT    DETNEG
024810160204R211 C                   Endif
024900000000     C                   EXCEPT    DET                                          *
025000951124     C*
025100951124     C                   MOVEL     *BLANK        COG              22            *
025200951124     C                   MOVEL     *BLANK        NOM              17            *
025300951124     C                   MOVEL     *BLANK        LNC              20            *
025400951127     C                   MOVEL     *BLANK        CIT              18            *
025500130207R110 C****               MOVEL     *BLANK        C77              11            *
025600150626R186 X**** ex R110       MOVEL     *BLANK        C77               1            *
025700150626R186 C                   MOVEL     *BLANK        C77               2            *
025800130207R110 C                   MOVEL     N77Codice     Codice            1            *
025900951124     C                   MOVEL     *BLANK        VIA              20            *
026000951124     C                   Z-ADD     0             DNC               6 0          *
026100951124     C                   MOVEL     *BLANK        CSE               4            *
026200951124     C*
026300951124     C     KEY771        READE     ND77001L                               90    *
026400951124     C  N90              ENDDO                                                  *
026500951124     C*
026600951124     C* Stampo totali parziali per quadro
026700951124     C     *in39         IFEQ      *on                                          *
026800951127     C                   time                    ORA
026900890419     C                   EXCEPT    TESTA                                        *
027000951124     C                   setoff                                       39        *
027100951124     C                   ENDIF                                                  *
027200890419     C                   EXCEPT    TOTQUA                                       *
027300951124     C*
027400890419     C                   ENDSR                                                  *
027500000117      *----------------------------------------------------
027600000117      * routine iniziale
027700000117      *----------------------------------------------------
027800000117     C     *INZSR        Begsr
027900000117C1309C     *DMY          Move      Udate         ZDateISO
028000000117C1309C     *JobRun       Move      ZDateISO      ZDate             6 0
028100000117      *
028200000117     C                   Endsr
028300000000     O*----------------------------------------------------*
028301160204R211 O* Trasformati i campi importi da edtcde'2'a edcde'K'
028302160204R211 O*----------------------------------------------------*
028400951124     OQSYSPRT   E            TESTA            02
028500951127     O                       NOMDIT              21
028600951127     O                                           48 '***-------------------'
028700951127     O                                           70 '----------------------'
028800951127     O                                           92 '----------------------'
028900951127     O                                           96 '-***'
029000951127     O                       KNMUS              110
029100000117C1309O*****                  UDATE         Y    120
029200000117C1309O                       ZDATE         Y    120
029300951127     O                                          126 'Pag.'
029400951127     O                       PAGE          Z    131
029500951127     O          E            TESTA       1
029600951127     O                       NOMSYS               9
029700951127     O                       KNSIF               20
029800951127     O                                           27 '*'
029801170302R2525X***                                        53 'Stampa di servizio rig'
029802170302  "  X***                                        75 'he percipienti per mod'
029803170302R2525X***                                        93 '. 770 quadro AU   '
029900170302R2525O                                           53 '        Stampa di serv'
030000170302  "  O                                           75 'izio righe percipienti'
030100170302R2525O                                           93 ' CU (Quadro AU)   '
030200951127     O                                           96 '*'
030300951127     O                       NOMPGM             110
030400951127     O                       ORA                120 '0 :  :  '
030500951127     O          E            TESTA       1
030600951127     O                                           48 '***-------------------'
030700951127     O                                           70 '----------------------'
030800951127     O                                           92 '----------------------'
030900951127     O                                           96 '-***'
031000951127     O          E            TESTA       2
031100000000     O                       SK(1)               66
031200000000     O                       SK(2)              132
031300000000     O          E            TESTA       1
031400951127     O                                           14 'Codice fiscale'
031500951127     O                                           28 'Cognome'
031600951127     O                                           48 'Nome'
031700951127     O                                           86 'Data / Luogo / Prov. di'
031800951127     O                                           94 'nascita'
031900951127     O                                           98 'Ses'
032000951127     O                                          120 'Comune / Pr di resid.'
032100130207R110 O****                                      132 'Causale 770'
032200130207R110 O                                          126 'Caus.'
032300130207R110 O                                          132 'Cod.'
032400000000     O          E            TESTA       1
032500951127     O                                            3 'Via'
032600951127     O                                           24 'Est'
032700951127     O                                           40 'Importo totale'
032800951127     O                                           56 'Impon.non sogg.'
032900951127     O                                           72 'Spese non sogg.'
033000951127     O                                           88 'Impon.soggetto'
033100000000     O                                           94 '% RA'
033200951127     O                                          111 'Ritenuta'
033300951127     O                                          131 'Netto corrisposto'
033400140516R148 O          E            TESTA       1
033500140516 "   O                                           12 'Addizionale:'
033600140516 "   O                                           25 'Cod.Regione'
033700140516 "   O                                           82 'Add.Reg.Acc.'
033800140516 "   O                                          107 'Add.Reg.Imp.'
033900140516R148 O                                          129 'Add.Reg.Sos.'
034000140516R148 O          E            TESTA       1
034100140516 "   O                                           25 'Cod.Comune '
034200140516 "   O                                           82 'Add.Com.Acc.'
034300140516 "   O                                          107 'Add.Com.Imp.'
034400140516R148 O                                          129 'Add.Com.Sos.'
034500940317     O          E            TESTA       1
034600080131C2077O*****                                      25 'Codice id. fiscale estero'
034700080131C2077O                                           25 'Codice id.fiscale estero '
034800951127     O                                           38 'Stato estero'
034900080131C2077O                                           84 'Localita / Indirizzo ester'
035000080131C2077O                                           85 'o'
035100100524R024 O          E            TESTA       1
035200100524  "  O                                           26 'Redditi da altri soggetti:'
035300110318R024 O                                           40 'Cod.Fiscale'
035400110318R055 O*** EX R024                                56 'Tot.Reddito'
035500110318R055 O                                           56 ' Causale   '
035600110318R055 O                                           80 'Imponibile'
035700100524  "  O                                          107 'Rit.Tit.Acc.'
035800100524  "  O                                          129 'Rit.Tit.Imp.'
035900100524  "  O          E            TESTA       1
036000100524  "  O                                           56 'Rit.Sospese'
036100100524  "  O                                           82 'Add.Reg.Acc.'
036200100524  "  O                                          107 'Add.Reg.Imp.'
036300100524R024 O                                          129 'Add.Reg.Sos.'
036400140516R148 O          E            TESTA       1
036500140516  "  O                                           82 'Add.Com.Acc.'
036600140516  "  O                                          107 'Add.Com.Imp.'
036700140516R148 O                                          129 'Add.Com.Sos.'
036800000000     O          E            TESTA       1
036900000000     O                       SK(1)               66
037000000000     O                       SK(2)              132
037100951124     O*
037200160204R211 X***       E            DET         1
037201160204R211 O          E            DETPER      1
037300951124     O                       N77CDF              20
037400951124     O                       COG                 43
037500951127     O                       NOM                 61
037600951127     O                       DNC           Y     70
037700951127     O                       LNC                 91
037800951127     O                       N77PNC              94
037900951127     O                       N77SES              97
038000951127     O                       CIT                117
038100951127     O                       N77PRV             120
038200130207R110 O****                   C77                132
038300130207R110 O                       C77                124
038400130207R110 O                       Codice             130
038401160204R211 O          E            DETNEG      1
038402160204  "  O                                        +   1 '** Attenzione, sono stati -
038403160204  "  O                                              rilevati importi negativi *-
038404160204R211 O                                              *'
038500000000     O          E            DET         1
038600000000     O                       VIA                 20
038700080207C2077O***                    N77NUM              24
038800150630R186 O****                   NET           2    131
038900160204R186 O                       RIM           K    131
039000160204     O                       RAC           K    111
039100010525     O                       N77PRA        2     95
039200160204     O                       IMS           K     88
039300160204     O                       SNS           K     72
039400160204     O                       IMN           K     56
039500160204     O                       ITC           K     40
039600080207C2077O                       N77NUM              24
039700140516R148 O          E            DET         1
039800140516  "  O                       N77REG              29
039900160204  "  O                       N77ARS        K    131
040000160204  "  O                       N77ARI        K    108
040100160204  "  O                       N77ARG        K     83
040200140516  "  O          E            DET         1
040300140516  "  O                       N77COM              29
040400160204  "  O                       N77ACS        K    131
040500160204  "  O                       N77ACG        K    108
040600160204R148 O                       N77ACTA       K     83
040700940317     O          E            DET         1
040800150630R186 O****                   N77CDE              25
040900150630R186 O****                   CSE                 30
041000150630R186 O****                   N77SER              66
041100080131C2077O                       IndESt             132
041200150630R186 O                       CSE                 30
041300150630R186 O                       N77CDE              25
041400100524R024 O          E    78      DET         1
041500100524  "  O                       SCDF                39
041600160204  "  O                       SRIM          K    131
041700160204  "  O                       SRAC          K    108
041800160204R024 O                       SIMS          K     83
041900110318R055 O*** ex R024            SIMP          2     59
042000110318R055 O                       SCAU                59
042100110318R024 O          E    78      DET         1
042200160204  "  O                       SARS          K    131
042300160204  "  O                       SARI          K    108
042400160204  "  O                       SARA          K     83
042500160204R024 O                       SRSO          K     59
042600140516R148 O          E    78      DET         1
042700160204  "  O                       SACS          K    131
042800160204  "  O                       SACI          K    108
042900160204R148 O                       SACA          K     83
043000000000     O          E            DET         1
043100000000     O                                            1 ' '
043200951124     O*
043300890419     O          E            TOTQUA      2
043400951127     O                                           13 'Totale quadro'
043500010525     O                       SAVQUA              16
043600150630R186 O****                   QUANET        2    131
043700160204R186 O                       QUARIM        K    131
043800160204     O                       QUARAC        K    111
043900160204     O                       QUAIMS        K     88
044000160204     O                       QUASNS        K     72
044100160204     O                       QUAIMN        K     56
044200160204     O                       QUAITC        K     40
044300890419     O          E            TOTQUA      0
044400951127     O                                           13 'Totale quadro'
044500890419     O                       SAVQUA              16
044600150630R186 O****                   QUANET        2    131
044700160204R186 O                       QUARIM        K    131
044800160204     O                       QUARAC        K    111
044900160204     O                       QUAIMS        K     88
045000160204     O                       QUASNS        K     72
045100160204     O                       QUAIMN        K     56
045200160204     O                       QUAITC        K     40
045300140516R148 O          E            TOTQUA      1
045400160204  "  O                       QUAARS        K    131
045500160204  "  O                       QUAARI        K    108
045600160204  "  O                       QUAARG        K     83
045700140516  "  O          E            TOTQUA      1
045800160204  "  O                       QUAACS        K    131
045900160204  "  O                       QUAACG        K    108
046000160204R148 O                       QUAACTA       K     83
046100100524R024 O          E            TOTQUA      1
046200100524     O                                           19 'Tot.Altri Redditi'
046300160204  "  O                       QUASRIM       K    131
046400160204  "  O                       QUASRAC       K    108
046500160204R024 O                       QUASIMS       K     83
046600110318R055 O*** ex R024            QUASIMP       2     59
046700110318R024 O          E            TOTQUA      1
046800160204  "  O                       QUASARS       K    131
046900160204  "  O                       QUASARI       K    108
047000160204  "  O                       QUASARA       K     83
047100160204R024 O                       QUASRSO       K     59
047200140516R148 O          E            TOTQUA      1
047300160204  "  O                       QUASACS       K    131
047400160204  "  O                       QUASACI       K    108
047500160204R148 O                       QUASACA       K     83
047600890419     O          E            TOTQUA      1
047700890419     O                                            1 ' '
047800951124     O*
047900890419     O          E            TOTALE      2
048000890419     O                                           15 'TOTALE GENERALE'
048100160204     O                       TOTRAC        K    111
048200160204     O                       TOTSNS        K     72
048300160204     O                       TOTITC        K     40
048400890419     O          E            TOTALE      0
048500890419     O                                           15 'TOTALE GENERALE'
048600160204     O                       TOTRAC        K    111
048700160204     O                       TOTSNS        K     72
048800160204     O                       TOTITC        K     40
048900010604     O*
049000010604     O          E            TOTALE      1
049100150630R186 O****                   TOTNET        2    131
049200160204R186 O                       TOTRIM        K    131
049300160204     O                       TOTIMS        K     88
049400160204     O                       TOTIMN        K     56
049500010604     O          E            TOTALE      0
049600150630R186 O****                   TOTNET        2    131
049700160204R186 O                       TOTRIM        K    131
049800160204     O                       TOTIMS        K     88
049900160204     O                       TOTIMN        K     56
050000140516R148 O          E            TOTALE      1
050100160204  "  O                       TOTARS        K    131
050200160204  "  O                       TOTARI        K    108
050300160204  "  O                       TOTARG        K     83
050400140516  "  O          E            TOTALE      1
050500160204  "  O                       TOTACS        K    131
050600160204  "  O                       TOTACG        K    108
050700160204R148 O                       TOTACTA       K     83
050800100524R024 O          E            TOTALE      1
050900160204  "  O                       TOTSRIM       K    131
051000160204  "  O                       TOTSRAC       K    108
051100160204R024 O                       TOTSIMS       K     83
051200110318R055 O*** ex R024            TOTSIMP       2     59
051300110318R024 O          E            TOTALE      1
051400160204  "  O                       TOTSARS       K    131
051500160204  "  O                       TOTSARI       K    108
051600160204  "  O                       TOTSARA       K     83
051700160204R024 O                       TOTSRSO       K     59
051800140516R148 O          E            TOTALE      1
051900160204  "  O                       TOTSACS       K    131
052000160204  "  O                       TOTSACI       K    108
052100160204R148 O                       TOTSACA       K     83
052200890419     O          E            TOTALE      1
052300890419     O                                            1 ' '
052400000000**
052500000000------------------------------------------------------------------
052600000000------------------------------------------------------------------
