000100930222     H DECEDIT('0,') DATEDIT(*DMY.)
000200991102     H* CNC0W9R *----------------------------------------------------*
000300930222     H*---------*                                                    *
000400930222     H*     -  DISTINTA DI VERSAMENTO IN BANCA  -                    *
000500940224?     *--------------------------------------------------------------*
000600991104     FCNASS02L  UF A E           K DISK    USROPN
000700991104     F                                     COMMIT
000800991102     F*----------
000900930222     FCNACO00F  IF   E           K DISK
001000991102     F*----------
001100930222     FCNABI00F  IF   E           K DISK
001200991102     F*----------
001300930222     FCNIND00F  IF   E           K DISK
001400991102     F*----------
001500930222     FTABEL00F  UF   E           K DISK
001600991102     F*----------
001700991102     FTNTBE01L  IF   E           K DISK
001800991102     F*----------
001900930222     FCNOPE00F  IF   E           K DISK
002000991102     F*----------
002100991102     FYNTGD01L  IF   E           K DISK
002200991103     F*----------
002300991104     FFIARM02L  UF   E           K DISK    USROPN
002400991104     F                                     COMMIT
002500991103     F*----------
002600991104     FFNMOV00T  O  A E             DISK    USROPN
002700991104     F                                     COMMIT
002800991102     F*----------
002900000612     FFNPLI00F  O  A E             DISK
003000000609     F                                     COMMIT
003100000612     FFNPLI01L  IF   E           K DISK
003200000612     F                                     RENAME(FNPLI000:FNPLI001)
003300000609     F*----------
003400930222     FDISBAN1   O    F  132        PRINTER
003500930222     FDISBAN2   O    F  132        PRINTER
003600930222     FDISBAN3   O    F  132        PRINTER
003700991102     F*----------
003800991102     FCNC0W9D   CF   E             WORKSTN
003900991102     F                                     SFILE(CN0W9S2:NR2)
004000991102     F                                     SFILE(CN0W9S4:NR4)
004100991102     F                                     SFILE(CN0W9S5:NR5)
004200991102?     *--------------------------------------------------------------*
004300991102     D SC              S             66    DIM(8) CTDATA PERRCD(1)
004400930927     D CSP             S              2    DIM(20)                              COD.SPECIE
004500000000     D DSP             S             25    DIM(20)                              DES.SPECIE
004600891201     D CAU             S              4    DIM(20)                              CAUSALE CONTABILE
004700930927     D CA1             S              4    DIM(20)                              CAUS.CONTAB. ALTRO I
004800991104     D IMP             S             15  2 DIM(20)                              IMPORTI PER SPECI
004900991104     D IM1             S             15  2 DIM(20)                              IMPORTI PER SPECI
005000891204     D FILA            S              3    DIM(29)                              FIL TBLUNI    TE
005100891205     D OPZ             S              1    DIM(10)                              FLAGS ANAGRAF0F
005200910722     D SKI             S              1    DIM(3)                               SKIERA DI COMODO
005300000609     D ERR             S             80    DIM(8) CTDATA PERRCD(1)              SCHIERA ERRORI
005400991104     D CMDD            S             46    DIM(5) CTDATA PERRCD(1)              CMD Exec
005500000000     D DTAVER          DS
005600000000     D  NUMVER                 1      6  0
005700991103     D PARNAC          DS
005800991103     D  PFLE                   1      3
005900991103     D  PCUC                   4      6
006000991103     D  PNMI                   7     13
006100991103     D  PDMI                  14     19
006200991104     D DSCMZ         E DS
006300000000     D TCUDS           DS
006400000000     D  F1                     1      1
006500000000     D  F3                     3      3
006600000000     D  F2                     2      2
006700000000     D  F4                     4      4
006800000000     D  F56                    5      6
006900991103     D YEURDS        E DS                  EXTNAME(YEURCODS)
007000991103     D TRUL52        E DS                  EXTNAME(TRUL52DS)
007100991103     D TRUL12        E DS                  EXTNAME(TRUL12DS)
007200910718     D*-
007300991103     D WLBDAT          DS
007400991102     D  G02DAT                 1      8  0
007500991102     D  G02INV                 9     16  0
007600991102     D  G02ERR                17     17
007700991102     D  G02TGI                18     22  0
007800910719      *
007900020503     D*                DS
008000020503     D* §SEI                   1      6
008100020503     D* §PRF                   1      3
008200941104      *
008300941104     D                SDS
008400941104     D  §PGM                   1     10
008500891205?    D*----------------------------------------------------*
008600900403     D CNCR80        E DS
008700991103     D CNCR40        E DS
008800000000     D KPJBA         E DS
008900000000     D  LIBSYS                92    100
009000000000     D  MSGERR               453    502
009100900403     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
009200000000     D  TCU                  398    697
009300000000     D                                     DIM(50)                              TAB FLAG CNT
009400000000     D  KCU                  698    847P 0
009500000000     D                                     DIM(50)                              TAB CAPO CONTO
009600000000     D                                     PACKEVEN
009700991104     D TIBS50        E DS                  EXTNAME(TIBS50DS)
009800991104     D DS4O          E DS
009900991118     D DS1A          E DS
010000910719     D DS4E          E DS
010100991102     D DSCV          E DS
010200991102     D DGED          E DS
010300930924     I*--------------------------------------------------------------------*
010400991102     IYNTGD000
010500991102     I              TGDDIVISA                   TGDDIV
010600991102     I              TGDSEQUENZ                  TGDSEQ
010700991102     I              TGDDESCR                    TGDDES
010800991102     I              TGDVALORE                   TGDVAL
010900991102?    I*----------------------------------------------------*
011000991102     C* LOOP FINO A F3
011100991102?    C*----------------------------------------------------*
011200910722     C     *INKC         DOWEQ     '0'
011300000608      *
011400000608     C* GESTIONE PRIMA VIDEATA
011500000608     C     VID01         TAG
011600000608      *
011700991102     C                   EXSR      GESD01
011800000608      *
011900000608      * SOLO PER IL GIRO "BANCA PER CONTANTE"
012000000612     C   66
012100000612     CANN03
012200000612     CANN28V1PLIC        IFNE      *ZERO
012300000608     C                   EXSR      WIND01
012400000608     C   KL              GOTO      VID01
012500000608     C                   ENDIF
012600000608      *
012700991102     C* PER NO ERRORI PROSEGUO
012800991103     C  N28
012900991103     CANNKC
013000991103     CANN91
013100991104     CANN92
013200991104     CANN39              DO
013300991104     C* Effettuo Ovrdbf su file movimenti
013400991104     C     §FLE          IFNE      WFIL
013500991104     C                   EXSR      OVRMOV
013600991118     C                   MOVE      §FLE          WFIL              3 0
013700991104     C                   END
013800910718     C* PULIZIA SUBFILE
013900910718     C                   EXSR      PULIZ
014000910716     C                   EXSR      CARICA
014100910724     C                   EXSR      AGG
014200991103     C                   ADD       NR2           TREC              4 0
014300000608     C                   Z-ADD     NR2           SAVNR2            4 0
014400910725     C     *IN12         DOWEQ     '0'
014500910725     C     *IN11         ANDEQ     '0'
014600910717     C     *IN90         OREQ      '1'
014700000608      *
014800000608      * CON IL (66) VISUALIZZO O BYPASSO QUESTO SCHERMO SE EFFETTUA
014900000608      *  IL BYPASS, DEVO ANCHE SIMULARE DI AVER PREMUTO IL TASTO DI
015000000608      *   CONFERMA PER LA VIDEATA SUCCESSIVA DEL CONTANTE.
015100000608     C  N66              WRITE     CN0W9Z2
015200000608     C  N66              EXFMT     CN0W9C2
015300000608     C   66              SETON                                        11
015400910725     C                   SETOFF                                       90
015500000608      *
015600000608      * SE F14=ON ESCLUSIONE DI TUTTE LE RIGHE CON "E" SU "VANN"
015700000608     C   66
015800000608     COR KN              EXSR      EXCLRD
015900000608      *
016000910725     C  N12              DO
016100910731     C                   SETOFF                                       82
016200910729     C                   EXSR      CONTR
016300910725     C   11              EXSR      CONTR
016400910726     C   11              SETOFF                                       1317
016500910725     C   11
016600910725     CANN90              EXSR      CONF
016700911125     C   69              GOTO      FINE
016800910719     C                   END
016900910731     C*                    SETOF                     12
017000910801     C   12
017100910801     CANN15*IN82         COMP      '0'                                121282
017200910731     C   82              SETOFF                                       12
017300910801     C   15              SETON                                        12
017400000608      *
017500000608      * PER IL GIRO "BANCA PER CONTANTE" AD F12 DEVE TORNARE AL
017600000608      * PRIMO VIDEO INIZIALE
017700000608     C   17
017800000608     CAN 66              SETON                                        12
017900000608      *
018000910731     C                   END
018100910722     C                   END
018200910722     C                   END
018300911125     C     FINE          TAG
018400991104     C* Se ho effettuato l'allocazione disalloco FNMOV
018500991104     C     §FLE          IFEQ      WFIL
018600991104     C                   EXSR      DLCMOV
018700991104     C                   END
018800991104     C                   COMMIT
018900921204     C*-                                                   *
019000910719     C                   SETON                                        LR
019100991102     C*----------------------------------------------------*
019200991102     C* GESTIONE DELLA PRIMA VIDEATA
019300991102     C*----------------------------------------------------*
019400991102     C     GESD01        BEGSR
019500000609      *
019600000609      * SOLO PER IL GIRO BANCA PER CONTANTE (66=ON)
019700000609     C*  Aggancio tabella 'SEC' x reperire la BANCA
019800000609     C     *IN66         IFEQ      *ON
019900000609     C                   MOVEL     'SEC'         KCOD1
020000000609     C                   MOVEL     *BLANKS       KKEY1
020100000609     C                   MOVEL     'SEC'         KKEY1
020200000609     C     KTBE          CHAIN     TNTBE01L                           13
020300000609     C     *IN13         IFEQ      '0'
020400000609     C                   MOVEL     TBEUNI        BANCA8            8 0
020500000609     C                   MOVE      BANCA8        VBAN
020600000609     C                   END
020700000609     C                   ENDIF
020800000609      *
020900991102     C                   SETOFF                                       1282
021000991102     C                   SETON                                        73
021100991102     C                   EXFMT     CN0W9D1
021200991104     C                   SETOFF                                       2891
021300991103     C*
021400000612     C* Controllo se ci sono dati in FNMOV00F
021500991103     C                   Z-ADD     0             NRRX             10 0
021600991103     C                   MOVEL     'FNMOV00T'    FILE
021700991103     C                   MOVEL     '*LIBL   '    LIB              10
021800991103     C                   MOVEL     'M   '        VAR2              4
021900991103     C                   MOVE      §FLE          VAR2
022000991103     C                   MOVEL     VAR2          MBR              10
022100991103     C                   CALL      'TRUL51C'
022200991103     C                   PARM                    LIB
022300991103     C                   PARM                    FILE             10
022400991103     C                   PARM                    MBR
022500991103     C                   PARM                    NRRX
022600991103     C     NRRX          IFNE      *ZEROS
022700991118     C     NRRX          ANDNE     *ALL'9'
022800991103     C                   SETON                                        28
022900991103     C                   MOVEL     ERR(6)        $MSG
023000991103     C                   GOTO      FINVD1
023100991103     C                   END
023200000609      *--------
023300991102     C     §FLE          IFNE      0
023400991102     C                   MOVE      §FLE          §FIL              3
023500991102     C     §FIL          LOOKUP    FILA                                   38
023600991102     C  N38              SETON                                        39
023700991102     C   38              SETOFF                                       39
023800991102     C                   ELSE
023900991102     C                   SETON                                        39
024000991102     C                   END
024100991102     C  NKC
024200991102     CANN39              DO
024300991102     C                   Z-ADD     VBAN          KSC
024400991102     C     VBAN          COMP      0                                      92
024500991102     C* SE "REM" LE PRIME 3 CIFRE COD.BANCA DEVONO = CON FILIALE
024600991102     C                   MOVEL     *BLANKS       ACORAG
024700991102     C  N92KEYACO        CHAIN     CNACO000                           92
024800991102     C  N92ACOFLG        COMP      '*'                                    92
024900991102     C  N92ACOABL        COMP      '*'                                    92
025000991102     C                   MOVEL     ACORAG        VIDBAN           30
025100991102     C                   MOVEA     ACOOPZ        OPZ                            *
025200991102     C                   MOVE      OPZ(7)        HOPZ7             1            OBBL.RAPP.BANCA
025300991102     C                   MOVE      OPZ(2)        HOPZ2             1            GEST.SCADENZNCA
025400991102     C     KEYACO        CHAIN     CNIND000                           21
025500991102     C                   Z-ADD     INDABI        ISTABI
025600991102     C                   Z-ADD     INDCAB        ISTCAB
025700991102     C   21              MOVE      *BLANK        INDCCB
025800991102     C                   MOVEL     ACORAG        VBANC
025900991102     C*
026000991102     C* Verifico se la divisa è corretta
026100991103     C  N92V1CDIV        IFNE      DIVALT
026200991209     C     V1CDIV        ANDNE     §GEDCN
026300991209     C     V1CDIV        OREQ      *BLANKS
026400991102     C                   SETON                                        2891
026500991102     C                   MOVEL     ERR(5)        $MSG
026600991102     C                   END
026700991103     C                   END
026800991118     C* Controllo se per la divisa immessa è ammesso il contante
026900991118     C                   SETON                                        94
027000991118     C     DIVALT        IFEQ      V1CDIV
027100991118     C                   MOVE      *BLANKS       KEY
027200991118     C                   MOVEL     'SC'          KEY
027300991118     C                   MOVE      '1A'          COD
027400991118     C     KTAB          CHAIN     TABEL                              24
027500991209     C                   MOVEL     TBLUNI        DS1A
027600991118     C     §1AEUR        IFNE      'S'
027700991118     C                   SETOFF                                       94
027800991118     C                   END
027900991118     C                   END
028000000608      *
028100000608      *  SOLO PER IL GIRO "BANCA PER CONTANTE"
028200000608      *   ESEGUO IL CONTROLLO DEL NR.PLICO OBBLIGATORIO
028300000608     C     *IN66         IFEQ      *ON
028400000608     C     V1PLIC        COMP      0                                      93
028500000608     C   93              SETON                                        28
028600000608     C   93              MOVEL     ERR(7)        $MSG
028700000608      *
028800000612      *  CONTROLLO DI ESISTENZA CHIAVE POC/NR.PLICO/DIVISA
028900000612     C                   MOVE      §FLE          KPOC              3 0
029000000612     C                   Z-ADD     V1PLIC        KPLI              9 0
029100000612     C                   MOVEL     V1CDIV        KDIV              3
029200000612     C     KPLIN         CHAIN     FNPLI01L                           29
029300000612     C  N29              SETON                                        2895
029400000612     C  N29              MOVEL     ERR(8)        $MSG
029500000612     C                   ENDIF
029600000612      *
029700991103     C     FINVD1        ENDSR
029800000608     C*----------------------------------------------------*
029900000608     C* IMPOSTA LA WINDOW CONFERMA PER "BANCA X CONTANTE"
030000000608     C*----------------------------------------------------*
030100000608     C     WIND01        BEGSR
030200000608      *
030300000608      * WINDOW DI CONFERMA NR. PLICO
030400000608     C                   Z-ADD     V1PLIC        W1PLIC
030500000608     C                   EXFMT     CN0W9W1
030600000608      *
030700000608     C                   ENDSR
030800000609     C*----------------------------------------------------*
030900000609     C* ESEGUO OVRDBF SU FNMOV
031000000609     C*----------------------------------------------------*
031100000609     C     OVRMOV        BEGSR
031200000609     C*
031300000609     C                   MOVE      ') '          VAR               5
031400000609     C                   MOVEL     §FLE          VAR
031500000609     C                   Z-ADD     46            LUNG             15 5
031600000609     C** CONTROLLO SE ESISTE IL MEMBRO X FNMOV00T
031700000609     C                   SETOFF                                       20
031800000609     C                   MOVE      VAR           CMDD(1)
031900000609     C                   MOVEL     *BLANKS       COMMAN
032000000609     C                   MOVEA     CMDD(1)       COMMAN           80
032100000609     C                   CALL      'QCMDEXC'                            20
032200000609     C                   PARM                    COMMAN
032300000609     C                   PARM                    LUNG
032400000609     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
032500000609     C   20              DO
032600000609     C                   MOVE      VAR           CMDD(2)
032700000609     C                   MOVEL     *BLANKS       COMMAN
032800000609     C                   MOVEA     CMDD(2)       COMMAN
032900000609     C                   CALL      'QCMDEXC'
033000000609     C                   PARM                    COMMAN
033100000609     C                   PARM                    LUNG
033200000609     C                   END
033300000609     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
033400000609     C                   MOVE      VAR           CMDD(3)
033500000609     C                   MOVEL     *BLANKS       COMMAN
033600000609     C                   MOVEA     CMDD(3)       COMMAN
033700000609     C                   CALL      'QCMDEXC'
033800000609     C                   PARM                    COMMAN
033900000609     C                   PARM                    LUNG
034000000609     C* GUARDO SE è ALLOCATO
034100000609     C                   MOVEL     VAR           VAR
034200000609     C                   MOVE      ')'           VAR
034300000609     C                   MOVE      VAR           CMDD(4)
034400000609     C                   MOVEA     CMDD(4)       COMMAN
034500000609     C                   Z-ADD     46            LUNCMD           15 5
034600000609     C                   CALL      'QCMDEXC'                            91
034700000609     C                   PARM                    COMMAN
034800000609     C                   PARM                    LUNCMD
034900000609     C*
035000000609     C  N91              OPEN      FNMOV00T                             91
035100000612     C   91              MOVEL     'NO'          KPJBU
035200000609     C   91              GOTO      FINE
035300000609     C*
035400000609     C                   ENDSR
035500991104     C*----------------------------------------------------*
035600991104     C* ESEGUO Dlcobj  FNMOV
035700991104     C*----------------------------------------------------*
035800991104     C     DLCMOV        BEGSR
035900991104     C*
036000991104     C* GUARDO SE è ALLOCATO
036100991104     C  N91              CLOSE     FNMOV00T                             91
036200991104     C*
036300991104     C                   MOVE      VAR           CMDD(5)
036400991104     C                   MOVEA     CMDD(5)       COMMAN
036500991104     C                   Z-ADD     46            LUNCMD           15 5
036600991104     C                   CALL      'QCMDEXC'                            91
036700991104     C                   PARM                    COMMAN
036800991104     C                   PARM                    LUNCMD
036900991104     C*
037000991104     C                   ENDSR
037100000609     C*----------------------------------------------------*
037200910717     C* CARICAMENTO SUBFILE CON ASSEGNI VERSABILI
037300991102     C*----------------------------------------------------*
037400910717     C     CARICA        BEGSR
037500911113     C                   SETOFF                                       555152
037600911113     C                   SETOFF                                       49
037700991105     C                   Z-ADD     0             TASS             15 3
037800941103     C     §FLE          SETLL     CNASS02L
037900910717     C     *IN55         DOWEQ     '0'
038000941103     C     §FLE          READE     CNASS02L                               55
038100941103     C  N55ASSFVE        IFEQ      'V'                                          VERSABILI
038200941103     C     ASSTPI        ANDNE     'M'
038300941103     C     ASSNDV        ANDEQ     0                                            NO ALTRE DISTIN
038400991102     C     ASSVCA        ANDEQ     V1CDIV                                       SOLO DIV VERSAM
038500991102     C*
038600910717     C* LA PRIMA VOLTA SALTO IL CONFRONTO
038700910717     C  N51              DO
038800910717     C                   EXSR      PREC
038900910717     C                   SETON                                        51
039000910717     C                   END
039100910717     C*
039200941103     C     ASSNRA        IFNE      PNRA
039300991102     C     ASSDTE        ORNE      PDTE
039400941103     C     ASSABI        ORNE      PABI
039500951016     C     ASSCAB        ORNE      PCAB
039600910717     C                   EXSR      MUOVI
039700991103     C                   ADD       1             NR2               4 0
039800991102     C                   WRITE     CN0W9S2
039900910717     C                   ELSE
040000941103     C                   ADD       ASSCAS        TASS
040100910717     C                   END
040200940831     C                   END
040300910717     C                   END
040400910722     C* CARICO L'ULTIMO ASSEGNO
040500910722     C   51              DO
040600910722     C                   EXSR      MUOVI
040700991103     C                   ADD       1             NR2
040800991102     C                   WRITE     CN0W9S2
040900910722     C                   END
041000910722     C                   ENDSR
041100991102     C*----------------------------------------------------*
041200991102     C* Sposta  i campi dai dati di salvataggio a video
041300991102     C*----------------------------------------------------*
041400910717     C     MUOVI         BEGSR
041500991102     C*
041600920520     C     PTPD          COMP      'V'                                6565
041700920520     C   65              MOVEL     'P'           VPAD
041800920520     C  N65              MOVEL     *BLANKS       VPAD
041900910719     C                   MOVEL     PNRA          VNRA
042000910719     C                   MOVE      PNRA          VNRA1
042100940126     C                   MOVEL     PNRA          SAVNRA
042200940126     C                   MOVE      PNRA          SAVNR1
042300910717     C                   MOVEL     PTPD          VTPD
042400910717     C                   Z-ADD     PLNP          VLNP
042500910717     C                   Z-ADD     PAAS          VAAS
042600910717     C                   Z-ADD     PNRS          VNRS
042700910717     C                   Z-ADD     PNSP          VNSP
042800941103     C                   Z-ADD     PDTE          G02INV
042900941103     C                   MOVE      *ZEROS        G02DAT
043000941103     C                   MOVE      '3'           G02ERR
043100991120     C                   CALL      'XSRDA8'
043200941103     C                   PARM                    WLBDAT
043300941103     C                   MOVE      G02DAT        VDTE
043400910717     C                   Z-ADD     PABI          VABI
043500910717     C                   Z-ADD     PCAB          VCAB
043600940126     C                   Z-ADD     PABI          SAVABI
043700940126     C                   Z-ADD     PCAB          SAVCAB
043800910723     C                   Z-ADD     TASS          VASS
043900921105     C* DECODIFICO ISTITUTO SOLO SE ABI/CAB<> 1000/1000
044000910717     C                   Z-ADD     PABI          KABI
044100910717     C                   Z-ADD     PCAB          KCAB
044200910725     C     KABI§         CHAIN     CNABI00F                           77
044300910725     C  N77              MOVEL     ABIIST        VDES
044400910725     C   77              MOVE      *BLANKS       VDES
044500910717     C*
044600911219     C  N77              MOVEL     ABICOM        VPIA
044700911219     C   77              MOVE      *BLANKS       VPIA
044800911219     C*
044900910717     C                   MOVEL     PTPA          VTPA
045000910717     C                   MOVEL     PTPI          VTPI
045100910717     C                   MOVEL     PTPP          VTPP
045200911219     C*                    MOVELPPIA      VPIA
045300910717     C                   MOVEL     PNOT          VNOT
045400910716     C*
045500910717     C* AZZERO TOTALIZZATORE ASSEGNI
045600910717     C                   Z-ADD     0             TASS
045700941103     C                   ADD       ASSCAS        TASS
045800910717     C* MEMORIZZO N° ASS.,ABI-CAB,DATA ..... PRECEDENTI
045900910717     C                   EXSR      PREC
046000910719     C                   ENDSR
046100991102     C*----------------------------------------------------*
046200991102     C* Sposta  i dati nei campi di salvataggio
046300991102     C*----------------------------------------------------*
046400910717     C     PREC          BEGSR
046500941103     C                   MOVEL     ASSNRA        PNRA             10
046600941103     C                   MOVEL     ASSTPD        PTPD              1
046700941103     C                   Z-ADD     ASSLNP        PLNP              3 0
046800941103     C                   Z-ADD     ASSAAS        PAAS              4 0
046900941103     C                   Z-ADD     ASSNRS        PNRS              2 0
047000941103     C                   Z-ADD     ASSNSP        PNSP              7 0
047100991102     C                   MOVE      ASSDTE        PDTE              8 0
047200941103     C                   Z-ADD     ASSABI        PABI              5 0
047300941103     C                   Z-ADD     ASSCAB        PCAB              5 0
047400941103     C                   MOVEL     ASSTPA        PTPA              1
047500941103     C                   MOVEL     ASSTPI        PTPI              1
047600941103     C                   MOVEL     ASSTPP        PTPP              1
047700941103     C*                    MOVELASSPIA    PPIA   10
047800941103     C                   MOVEL     ASSNOT        PNOT             20
047900941103     C                   MOVEL     ASSFVE        PFVE              1
048000910722     C                   ENDSR
048100910722     C*
048200000608     C*----------------------------------------------------*
048300000608      * SE RICHIESTO L'ESCLUSIONE TOTALE DI TUTTI GLI ASSEGNI
048400000608      * TRAMITE IL F14 IMPOSTA AUTOMATICAMENTE SU TUTTI
048500000608     C*----------------------------------------------------*
048600000608     C     EXCLRD        BEGSR
048700000608      *
048800000608     C                   SETOFF                                       9028
048900000608      *
049000000608     C                   DO        SAVNR2        CNT
049100000608      *
049200000608     C                   SETOFF                                       242762
049300000608     C                   SETOFF                                       495852
049400000608     C                   SETOFF                                       434445
049500000608     C                   SETOFF                                       464748
049600000608      *
049700000608     C     CNT           CHAIN     CN0W9S2                            60
049800000608      *
049900000608     C     VNRA          IFNE      *BLANK
050000000608     C                   MOVE      'E'           VANN
050100000608     C                   END
050200000608      *
050300000608     C                   UPDATE    CN0W9S2
050400000608      *
050500000608     C                   ENDDO
050600000608      *
050700000608     C                   ENDSR
050800991102     C*----------------------------------------------------*
050900991102     C* Controlli i dati del subfile
051000991102     C*----------------------------------------------------*
051100910717     C     CONTR         BEGSR
051200000609      *
051300951027     C                   SETOFF                                       9028
051400910717     C                   DO        TREC          CNT
051500991209     C                   SETOFF                                       242762
051600911115     C                   SETOFF                                       495852
051700911115     C                   SETOFF                                       434445
051800911115     C                   SETOFF                                       464748
051900000609      *
052000991102     C     CNT           CHAIN     CN0W9S2                            60
052100000608      *
052200000608      * SE RICHIESTO L'ESCLUSIONE TOTALE DI TUTTI GLI ASSEGNI
052300000608      * TRAMITE IL F14 IMPOSTA AUTOMATICAMENTE SU TUTTI
052400000608      *
052500910717      *-------------
052600910717     C* ESEGUE CONTR. SOLO SE NON SONO ASSEGNI PROVENIENTI DA DISTINTA
052700910717     C* PADRONCINO
052800911219     C     VPAD          IFNE      'P'
052900910723     C     VASS          ANDNE     0
053000910717     C     VANN          IFEQ      'A'
053100910729     C   KF              DO
053200910717     C                   MOVEL     VNRA          KNRA
053300910717     C                   MOVE      VNRA1         KNRA
053400910717     C                   Z-ADD     VABI          KABI
053500910717     C                   Z-ADD     VCAB          KCAB
053600941103     C     KASS          CHAIN     CNASS000                           57
053700951027     C     ASSFVE        IFEQ      'V'
053800951027     C     ASSTPI        ANDNE     'M'
053900941103     C  N57              DELETE    CNASS000
054000951027     C                   END
054100940126     C*
054200940126     C     SAVNRA        IFNE      VNRA
054300940126     C     SAVNR1        ORNE      VNRA1
054400940126     C     SAVABI        ORNE      VABI
054500940126     C     SAVCAB        ORNE      VCAB
054600940126     C                   MOVEL     SAVNRA        KNRA
054700940126     C                   MOVE      SAVNR1        KNRA
054800940126     C                   Z-ADD     SAVABI        KABI
054900940126     C                   Z-ADD     SAVCAB        KCAB
055000941103     C     KASS          CHAIN     CNASS000                           57
055100951027     C     ASSFVE        IFEQ      'V'
055200951027     C     ASSTPI        ANDNE     'M'
055300941103     C  N57              DELETE    CNASS000
055400940126     C                   END
055500951027     C                   END
055600940126     C*
055700910717     C                   EXSR      PULI
055800910729     C                   END
055900910717     C                   ELSE
056000910717     C                   EXSR      CTR
056100910717     C                   END
056200920520     C                   SETOFF                                       65
056300991102     C                   UPDATE    CN0W9S2
056400911219     C                   END
056500911115     C     VPAD          IFEQ      'P'
056600911219     C     VANN          IFEQ      'A'
056700911115     C                   SETON                                        5890
056800991102     C                   UPDATE    CN0W9S2
056900911219     C                   ELSE
057000920520     C                   SETON                                        65
057100911219     C                   EXSR      CTR
057200991102     C                   UPDATE    CN0W9S2
057300911115     C                   END
057400911219     C                   END
057500950303     C  N90              END
057600910717     C                   ENDSR
057700991102     C*----------------------------------------------------*
057800911115     C* COTROLLI SUBFILE
057900991102     C*----------------------------------------------------*
058000910717     C     CTR           BEGSR
058100910717     C*
058200991209     C* CONTROLLO CHE IL NUMERO DEI DECIMALI SIA CONGRUENTE
058300991209     C                   MOVE      VASS          WCOMO1            1 0
058400991209     C                   MOVE      VASS          WCOMO2            2 0
058500991209     C     V1CDIV        IFEQ      §GEDCN
058600991209     C     DECDIV        ANDLT     2
058700991209     C     WCOMO1        ANDGT     0
058800991209     C     V1CDIV        OREQ      §GEDCN
058900991209     C     DECDIV        ANDLT     1
059000991209     C     WCOMO2        ANDGT     0
059100991209     C     V1CDIV        OREQ      DIVALT
059200991209     C     DECALT        ANDLT     2
059300991209     C     WCOMO1        ANDGT     0
059400991209     C     V1CDIV        OREQ      DIVALT
059500991209     C     DECALT        ANDLT     1
059600991209     C     WCOMO2        ANDGT     0
059700991209     C                   SETON                                        62  90
059800991209     C                   END
059900910717      * CONTROLLO DATA ASSEGNO
060000910717      *
060100951027     C                   SETOFF                                       56
060200911113     C                   MOVE      VDTE          G02DAT
060300911113     C                   MOVE      *BLANKS       G02ERR
060400991102     C                   CALL      'XSRDA8'
060500910717     C                   PARM                    WLBDAT
060600910717     C     G02ERR        IFEQ      '1'
060700910717     C                   SETON                                        43  90
060800910717     C                   END
060900991104     C                   MOVE      G02DAT        VDTE
061000910717      * IMPORTO ASSEGNO OBBLIGATORIO
061100910717     C  N90VASS          IFEQ      0
061200910717     C                   SETON                                        45  90
061300910717     C                   END
061400910717      ****************************************
061500910717      * SE TIPO ASSEGNO E' MITTENTE
061600910717      * ABI, CAB E N° ASSEGNO SONO OBBLIGATORI
061700910717      ****************************************
061800910724     C  N90VABI          IFEQ      0
061900911127     C     VTPA          ANDNE     'P'
062000911127     C     VTPA          ANDNE     'V'
062100910717     C     VCAB          OREQ      0
062200911127     C     VTPA          ANDNE     'P'
062300911127     C     VTPA          ANDNE     'V'
062400911127     C                   SETON                                        49  90
062500911127     C                   END
062600911127     C  N90VNRA          IFEQ      *BLANKS
062700910717     C                   SETON                                        47  90
062800910717     C                   END
062900911115      *
063000911127      * CONTROLLO CODICE ABI/CAB SE TIPO ASSEGNO < > 'P' E 'V'
063100921105      * SOLO SE ABI/CAB NON E' 1000/1000
063200910717      *
063300911219     C*          VTPA      IFNE 'P'
063400911219     C*          VTPA      ANDNE'V'
063500921105     C     VABI          IFNE      1000
063600921105     C     VCAB          ANDNE     1000
063700910725     C                   Z-ADD     VABI          KABI
063800910725     C                   Z-ADD     VCAB          KCAB
063900910725     C     KABI§         CHAIN     CNABI00F                           49
064000910729     C  N49VPIA          IFEQ      *BLANKS
064100910729     C                   MOVEL     ABICOM        VPIA
064200910729     C                   END
064300910717     C  N49              MOVEL     ABIIST        VDES
064400910801     C   49              MOVE      *BLANKS       VDES
064500910801     C     VPIA          IFEQ      *BLANKS
064600911113     C  NKF
064700911113     CANNKA              SETON                                        5290
064800910801     C                   END
064900911113     C   49
065000911113     CANNKF
065100911113     CANNKA              SETON                                        90
065200921105     C                   END
065300910716     C*
065400910722      * CONTROLLO TIPO ASSEGNO
065500910722     C     VTPA          IFNE      *BLANKS
065600910722     C                   MOVEA     *BLANKS       SKI
065700910722     C                   MOVEA     VTPA          SKI
065800910722     C     '?'           LOOKUP    SKI                                    31
065900910722     C   31              DO
066000910722     C                   MOVEL     CODUT         §KUT              1 0
066100910722     C                   MOVEL     '4E'          §COD              2
066200910722     C                   MOVE      *BLANKS       VTPA
066300910722     C                   CALL      'X§TABER'
066400910722     C                   PARM                    §KUT
066500910722     C                   PARM                    §COD
066600910722     C                   PARM                    §KEY              8
066700910722     C                   PARM                    §DES             30
066800910722     C                   MOVEL     §KEY          VTPA
066900910722     C                   END
067000910722     C*-----------------------------------------------------*
067100910722     C* Se no si ricerca su TABEL se esiste
067200910722     C*-----------------------------------------------------*
067300910722     C  N31              DO
067400910722     C                   MOVE      *BLANKS       KEY
067500910722     C                   MOVEL     VTPA          KEY
067600910722     C                   MOVE      '4E'          COD
067700910722     C     KTAB          CHAIN     TABEL                              24
067800921105     C  N24              EXCEPT    RILTAB
067900910722     C  N24TBLFLG        COMP      '*'                                    24
068000910723     C   24              SETON                                          90
068100910722     C                   END
068200910722     C                   ELSE
068300910723     C                   SETON                                        2490
068400910722     C                   END
068500911113      * CONTROLLO METTO V SU TIPO DOCUMENTO
068600911113     C     VTPD          IFEQ      *BLANKS
068700911113     C                   MOVE      'V'           VTPD
068800911113     C                   END
068900951027     C* Se assegno vario controllo se esiste con quei
069000951027     C* riferimenti un assegno proveiente da una chiusura
069100951027     C     VPAD          IFNE      'P'
069200951027     C                   MOVEL     VNRA          KNRA
069300951027     C                   MOVE      VNRA1         KNRA
069400951027     C                   Z-ADD     VABI          KABI
069500951027     C                   Z-ADD     VCAB          KCAB
069600951027     C     KASS          CHAIN     CNASS02L                           57
069700951027     C     *IN57         IFEQ      '0'
069800951027     C     *IN57         DOWEQ     '0'
069900951027     C     ASSTPD        ANDEQ     'V'
070000951027     C     KASS          READE     CNASS02L                               57
070100951027     C                   END
070200951027     C     ASSTPD        IFNE      'V'
070300951027     C     ASSTPI        IFEQ      'M'
070400951027     C                   SETON                                        285690
070500951027     C                   MOVEL     ERR(3)        $MSG
070600951027     C                   ELSE
070700951027     C     ASSFVE        IFNE      'V'
070800951027     C                   SETON                                        285690
070900951027     C                   MOVEL     ERR(1)        $MSG
071000951027     C                   ELSE
071100951027     C                   SETON                                        285690
071200951027     C                   MOVEL     ERR(2)        $MSG
071300951027     C                   END
071400951027     C                   END
071500951027     C                   END
071600951027     C                   END
071700951027     C                   END
071800910722     C*
071900910729     C  N90
072000910729     CAN KF
072100910729     CANN11              DO
072200940126     C                   MOVEL     VNRA          KNRA
072300940126     C                   MOVE      VNRA1         KNRA
072400940126     C                   Z-ADD     VABI          KABI
072500940126     C                   Z-ADD     VCAB          KCAB
072600941103     C     KASS          CHAIN     CNASS02L                           57
072700940126     C   57              EXSR      SPOSTA
072800941103     C   57              WRITE     CNASS000
072900941103     C  N57              UPDATE    CNASS000
073000940126     C*
073100940126     C     SAVNRA        IFNE      VNRA
073200940126     C     SAVNR1        ORNE      VNRA1
073300940126     C     SAVABI        ORNE      VABI
073400940126     C     SAVCAB        ORNE      VCAB
073500940126     C                   MOVEL     SAVNRA        KNRA
073600940126     C                   MOVE      SAVNR1        KNRA
073700940126     C                   Z-ADD     SAVABI        KABI
073800940126     C                   Z-ADD     SAVCAB        KCAB
073900941103     C     KASS          CHAIN     CNASS000                           57
074000951027     C     ASSFVE        IFEQ      'V'
074100951027     C     ASSTPI        ANDNE     'M'
074200941103     C  N57              DELETE    CNASS000
074300940126     C                   END
074400951027     C                   END
074500951026     C                   MOVE      VNRA          SAVNRA
074600951026     C                   MOVE      VNRA1         SAVNR1
074700951026     C                   Z-ADD     VABI          SAVABI
074800951026     C                   Z-ADD     VCAB          SAVCAB
074900991104     C                   COMMIT
075000910723     C                   END
075100910717     C                   ENDSR
075200991102     C*-----------------------------------------------------*
075300991102     C* SCRIVO CNASS
075400991102     C*-----------------------------------------------------*
075500910723     C     SPOSTA        BEGSR
075600000609      *
075700941103     C                   CLEAR                   CNASS000
075800941103     C                   Z-ADD     §FLE          ASSFLE
075900941103     C                   MOVEL     VTPD          ASSTPD
076000941103     C                   Z-ADD     VASS          ASSCAS
076100941103     C                   MOVEL     VNRA          ASSNRA
076200991104     C                   MOVEL     V2CDIV        ASSVCA
076300941103     C                   MOVE      VNRA1         ASSNRA
076400941103     C                   Z-ADD     VDTE          G02DAT
076500941103     C                   MOVEL     *BLANKS       G02ERR
076600941103     C                   MOVE      *ZEROS        G02INV
076700991102     C                   CALL      'XSRDA8'
076800941103     C                   PARM                    WLBDAT
076900941103     C                   Z-ADD     G02INV        ASSDTE
077000941103     C                   Z-ADD     VABI          ASSABI
077100941103     C                   Z-ADD     VCAB          ASSCAB
077200941103     C                   MOVEL     VTPA          ASSTPA
077300941103     C                   MOVEL     VTPI          ASSTPI
077400941103     C                   MOVEL     VTPP          ASSTPP
077500941103     C                   MOVEL     VPIA          ASSPIA
077600941103     C                   MOVEL     VNOT          ASSNOT
077700941103     C                   MOVEL     'V'           ASSFVE
077800910723     C                   ENDSR
077900991102     C*-----------------------------------------------------
078000910723     C* CARICA RECORD VUOTI NEL SUBFILE
078100991102     C*-----------------------------------------------------
078200910717     C     AGG           BEGSR
078300920520     C                   SETOFF                                       65
078400910717     C                   EXSR      PULI
078500910717     C                   DO        50
078600991103     C                   ADD       1             NR2
078700991102     C                   WRITE     CN0W9S2
078800910717     C                   END
078900910722     C                   ENDSR
079000991102     C*-----------------------------------------------------
079100991102     C* CARICA RECORD VUOTI NEL SUBFILE
079200991102     C*-----------------------------------------------------
079300910717     C     PULI          BEGSR
079400910725     C                   MOVE      *BLANKS       VPAD
079500910717     C                   MOVE      *BLANKS       VANN
079600910717     C                   MOVE      *BLANKS       VNRA
079700910717     C                   MOVE      *BLANKS       VNRA1
079800940126     C                   MOVE      *BLANKS       SAVNRA
079900940126     C                   MOVE      *BLANKS       SAVNR1
080000910717     C                   Z-ADD     0             VDTE
080100910717     C                   Z-ADD     0             VASS
080200910717     C                   Z-ADD     0             VABI
080300910717     C                   Z-ADD     0             VCAB
080400940126     C                   Z-ADD     0             SAVABI
080500940126     C                   Z-ADD     0             SAVCAB
080600910717     C                   MOVE      *BLANKS       VTPA
080700910717     C                   MOVE      *BLANKS       VTPP
080800910717     C                   MOVE      *BLANKS       VTPI
080900910717     C                   MOVE      *BLANKS       VNOT
081000910717     C                   MOVE      *BLANKS       VTPD
081100910717     C                   Z-ADD     0             VLNP
081200910717     C                   Z-ADD     0             VAAS
081300910717     C                   Z-ADD     0             VNRS
081400910717     C                   Z-ADD     0             VNSP
081500910717     C                   MOVE      *BLANKS       VDES
081600910717     C                   MOVE      *BLANKS       VPIA
081700000608      *
081800910717     C                   ENDSR
081900991102     C*-----------------------------------------------------
082000991102     C* LEGGO SUBFILE E CALCOLO TOTALI X VIDEATA REGISTR.
082100991102     C*-----------------------------------------------------
082200910717     C     CONF          BEGSR
082300000608      *
082400910726     C                   Z-ADD     0             TOTASS
082500910726     C                   MOVEA     *ZEROS        IMP
082600930927     C                   MOVEA     *ZEROS        IM1
082700000608      *
082800910717     C                   DO        TREC          CNT               4 0
082900991102     C     CNT           CHAIN     CN0W9S2                            61
083000000608      *
083100910718     C* TOTALE ASSEGNI
083200910731     C     VANN          IFNE      'E'
083300910717     C                   ADD       VASS          TOTASS
083400910718     C* TOT. IMPORTI PER TIPO
083500910718     C                   Z-ADD     1             A                 3 0
083600910719     C                   MOVEL     VTPA          LKP               2
083700910719     C                   MOVE      VTPP          LKP
083800910719     C     LKP           LOOKUP    CSP(A)                                 21
083900930927     C*
084000930927     C     VABI          IFEQ      ISTABI
084100910718     C   21              ADD       VASS          IMP(A)
084200930927     C                   ELSE
084300930927     C   21              ADD       VASS          IM1(A)
084400910717     C                   END
084500930927     C*
084600930927     C                   END
084700910731     C                   END
084800910717     C* TOTALE PROVVISORIO DISTINTA
084900910717     C     TOTASS        ADD       TOTCON        TOTDIS
085000991102     C                   Z-ADD     GMAUD8        VIDDRG
085100991118     C                   Z-ADD     GMAUD8        VDTOPE
085200991102     C                   Z-ADD     GMAUD8        VIDDDS
085300941104     C                   MOVE      §FLE          VFPN
085400910718     C* PROPONGO I DATI
085500910718     C*
085600910718     C*  CONTROPARTITA
085700991103     C                   Z-ADD     §4OCC1        VIDCCP            4 0
085800991103     C                   Z-ADD     §4OSC1        VIDSCP            7 0
085900941108     C     *IN01         IFNE      '1'                                          *IN01-->
086000991103     C                   Z-ADD     KCAS          VIDCC1            4 0
086100991103     C                   Z-ADD     1             VIDSC1            7 0
086200910718     C                   MOVEL     VBAN          FILBAN            3 0
086300910726     C                   ELSE
086400991103     C                   Z-ADD     0             VIDCC1
086500991103     C                   Z-ADD     0             VIDSC1
086600941108     C                   ENDIF                                                  <--*IN01
086700991102     C* Imposto valuta distinta
086800991102     C                   MOVEL     V2CDIV        V3CDIV
086900000608     C                   MOVEL     V1PLIC        V3PLIC
087000910718     C*
087100910801 -   C*                    MOVEL'0'       *INKA
087200910725     C     *IN13         DOWEQ     '0'
087300910725     C     *IN17         ANDEQ     '0'
087400980114     C                   SETOFF                                       59
087500950906     C                   Z-ADD     §TCONT        TOTCON
087600950906     C     TOTASS        ADD       TOTCON        TOTDIS
087700991105     C                   Z-ADD     TOTDIS        TOTVER
087800991118     C     START3        TAG
087900991102     C                   EXFMT     CN0W9D3
088000980114     C                   SETOFF                                       28
088100991118     C* CONTROLLO LA DATA OPERAZIONE SE ERRATA OPPURE NO
088200991118     C                   MOVEL     *BLANKS       G02ERR
088300991118     C                   Z-ADD     VDTOPE        G02DAT
088400991118     C                   CALL      'XSRDA8'
088500991118     C                   PARM                    WLBDAT
088600991118     C                   Z-ADD     G02INV        AMGDOP            8 0
088700991118     C     G02ERR        IFEQ      '1'
088800991118     C                   SETON                                        51
088900991118     C                   GOTO      START3
089000991118     C                   END
089100991118     C                   MOVE      G02DAT        VDTOPE
089200910725     C  N17              DO
089300910723     C   KD              DO
089400000608      *
089500910723     C                   EXSR      GESCON                                       GEST.CONTANTE
089600000608      *
089700950906     C                   Z-ADD     §TCONT        TOTCON
089800950906     C     TOTASS        ADD       TOTCON        TOTDIS
089900991105     C                   Z-ADD     TOTDIS        TOTVER
090000991102     C                   Z-ADD     VIDDRG        VIDDDS
090100950906     C                   END
090200000608      *
090300910725     C   13              EXSR      CTR2
090400910726     C   59              SETOFF                                       13
090500910717     C                   END
090600000608      *
090700911125     C   17              SETOFF                                       1112
090800910717     C                   END
090900910717     C                   ENDSR
091000991102     C*-------------------------------------------------------------*
091100991102     C* CONTROLLI
091200991102     C*-------------------------------------------------------------*
091300910718     C     CTR2          BEGSR
091400930222     C*
091500910718     C                   Z-ADD     0             TOTVER
091600991103     C     TOTASS        ADD       TOTCON        TOTDIS
091700991105     C                   Z-ADD     TOTDIS        TOTVER
091800910718     C** NUMERO REG. OBBLIGATORIO
091900910723     C   72              SETON                                        59
092000910718     C   72              GOTO      START2
092100910718     C*  DATA REGISTRAZIONE
092200940329     C                   Z-ADD     VIDDRG        G02DAT
092300940329     C                   MOVEL     *BLANKS       G02ERR
092400991102     C                   CALL      'XSRDA8'
092500940329     C                   PARM                    WLBDAT
092600991102     C                   Z-ADD     G02INV        AMGDRG            8 0
092700991102     C                   Z-ADD     G02TGI        DATARE            8 0          *
092800910718     C*
092900000103     C* SOLO X LE PRIMO LIVELLO SE NON HO VERSATO NULLA
093000000103     C* PERMETTO FORZATURA X VERSAMENTO INTERO IMPORTO IN CASSA
093100000103     C     *IN01         IFEQ      '0'
093200000105     C     TOTVER        ANDEQ     0
093300000105     C* CONTROLLO SE CI SONO ALMENO DEGLI INCASSI
093400000105     C                   EXSR      CTRINC
093500000105     C     WINCA         IFEQ      'S'
093600000105     C                   EXFMT     CN0W9D6
093700000105     C  NKG              GOTO      START2
093800000105     C                   ELSE
093900000105     C                   GOTO      START2
094000000105     C                   END
094100000105     C                   ELSE
094200910726     C   13TOTVER        CABLE     *ZEROS        START2
094300000105     C                   END
094400910726     C  N13              GOTO      START2
094500910718     C*§ EMISSIONE ULTIMA VIDEATA
094600910718     C*  ------------------------
094700991103     C*  Eseguo loop X caricamento importi incassati
094800910718     C*  PULIZIA SFL 4
094900910718     C                   EXSR      PULI4
095000910718     C*  CARICAMENTO SFL 4
095100910718     C                   EXSR      CARI4
095200910726     C* ATTRIBUZIONE NR° DISTINTA SOLO LA PRIMA VOLTA
095300910726     C  N68              DO
095400910726     C                   MOVEL     '4D'          COD
095500910726     C                   MOVEL     §FLE          KEY
095600910726     C     KTAB          CHAIN     TABEL                              46
095700980114     C     *IN46         IFEQ      '1'
095800980114     C                   SETON                                        2859
095900980114     C                   MOVEL     ERR(4)        $MSG
096000980114     C                   GOTO      START2
096100980114     C                   END
096200910726     C                   MOVEL     TBLUNI        NUMVER
096300910726     C                   ADD       1             NUMVER
096400910726     C                   MOVEL     NUMVER        TBLUNI
096500980114     C  N46              UPDATE    TABEL
096600000609      * CON IL GIRO DELLA BANCA X CONTANTE (66=ON)
096700000609      *  IMPOSTA NEL NR.DISTINTA IL NR.DEL PLICO
096800000609     C  N66              Z-ADD     NUMVER        VIDNDS
096900000609     C   66              Z-ADD     V4PLIC        VIDNDS
097000991125     C                   Z-ADD     NUMVER        VIDNRG
097100991125     C                   MOVEL     §FLE          VIDNRG
097200910726     C                   SETON                                        68
097300910726     C                   END
097400910726     C*
097500910718     C     EMI4          TAG
097600991103     C                   WRITE     CN0W9Z5
097700991103     C                   EXFMT     CN0W9C5
097800910718     C*                    ____________
097900910725     C   16              SETON                                        59
098000931222     C   16              SETOFF                                       88
098100910725     C   16              GOTO      START2
098200910718     C*
098300910718     C*§ CONTROLLI CAMPI DEL SFL
098400910718     C*  -----------------------
098500991103     C                   Z-ADD     0             NR5
098600910718     C     *IN90         DOUEQ     '1'
098700991103     C                   ADD       1             NR5
098800991103     C     NR5           CHAIN     CN0W9S5                            90
098900910718     C  N90VIMP4         IFNE      *ZEROS
099000910718     C     VSPE4         IFEQ      'CO'
099100910718     C     VSPE4         OREQ      'RE'
099200910718     C     VSPE4         OREQ      'CA'
099300910718     C                   SETON                                        88
099400910718     C                   ELSE
099500910718     C                   SETOFF                                       88
099600910718     C                   END
099700910718     C*
099800910718     C*  CAUSALE CONTABILE
099900910718     C                   MOVEL     'PG'          OPECD1
100000910718     C                   MOVEL     VCAU4         OPECOD                         CAUSALE CONT
100100910718     C     KEYOPE        CHAIN     CNOPE000                           87
100200991103     C   87              UPDATE    CN0W9S5
100300910718     C   87              GOTO      EMI4
100400910718     C                   MOVEL     OPEDES        VDES4
100500910718     C                   END
100600910718     C                   END
100700910718     C*
100800910726     C   15              DO
100900910718     C     SUDT3         TAG                                                    *
101000910719     C*          *LOCK     IN   DTAVER                 46  *
101100910718     C                   EXSR      STADIS
101200910718     C*
101300991103     C                   Z-ADD     0             NR5
101400910718     C     *IN90         DOUEQ     '1'
101500991103     C                   ADD       1             NR5
101600991103     C     NR5           CHAIN     CN0W9S5                            90
101700910718     C  N90VIMP4         COMP      0                                      90
101800930924     C*----------------------------------------------------*
101900930924     C     *IN90         IFEQ      '0'                                          *
102000991103     C* Scrivo movimenti contabili
102100991103     C                   EXSR      CREMOV                                       *
102200991103     C* Aggiorno flag FIARM                                *
102300991103     C                   EXSR      AGGARM                                       *
102400930924     C                   END                                                    *
102500000612      *
102600930924     C*----------------------------------------------------*
102700930924     C                   END
102800000612     C*----------------------------------------------------*
102900000612      * SCRIVE IL FILE DEI PLICHI DA TRASMETTERE A "SICURDATA"
103000000612      *
103100000612     C   66              EXSR      WRIPLI
103200991104     C*----------------------------------------------------*
103300991104     C* Sottomette trasmissioni                            *
103400991108     C                   COMMIT
103500000612      *
103600991104     C                   EXSR      SBMTRA                                       *
103700000612      *
103800930924     C*----------------------------------------------------*
103900910718     C                   EXSR      ANNDIS
104000000612      *
104100910726     C                   SETON                                        12
104200910726     C                   MOVE      '1'           *INKC
104300910801     C                   GOTO      START2
104400910718     C                   END
104500910726     C  N16              GOTO      EMI4
104600910718     C     START2        TAG
104700910718     C*
104800910718     C                   ENDSR
104900991102     C**----------------------------------------------------------
105000000000     C** PULIZIA DEL SUBFILE
105100991102     C**----------------------------------------------------------
105200000000     C     PULIZ         BEGSR
105300000000     C                   SETON                                        70
105400991102     C                   WRITE     CN0W9C2
105500000000     C                   SETOFF                                       70
105600910722     C                   SETON                                        71
105700991103     C                   Z-ADD     0             NR2
105800910725     C* PULISCO CAMPI DI COMODO
105900991102     C                   MOVE      V1CDIV        V2CDIV
106000991102     C                   MOVE      *BLANKS       PNRA
106100910725     C                   Z-ADD     0             PDTE
106200910725     C                   Z-ADD     0             PABI
106300910725     C                   Z-ADD     0             PCAB
106400910725     C                   MOVE      *BLANKS       PTPA
106500910725     C                   MOVE      *BLANKS       PTPP
106600910725     C                   MOVE      *BLANKS       PFVE
106700910725     C                   MOVE      *BLANKS       PTPI
106800910725     C                   MOVE      *BLANKS       PNOT
106900910725     C                   MOVE      *BLANKS       PTPD
107000910725     C                   Z-ADD     0             PLNP
107100910725     C                   Z-ADD     0             PAAS
107200910725     C                   Z-ADD     0             PNRS
107300910725     C                   Z-ADD     0             PNSP
107400950906     C                   Z-ADD     0             §TCONT
107500911219     C*                    MOVE *BLANKS   PPIA
107600910725     C*
107700000000     C                   ENDSR
107800991103     C*-------------------------------------------------------
107900900517     ** GESTIONE CONTANTI/ASSEGNI
108000991103     **-------------------------------------------------------
108100900517     C     GESCON        BEGSR
108200000608      *
108300910725     C                   SETOFF                                       1813
108400991102     C     V3CDIV        COMP      TGDDIV                                 09
108500000608      *
108600900517     C  N09              EXSR      PULCON
108700000608      *
108800900517     C* CARICAMENTO SUBFILE
108900991102     C                   MOVEL     V3CDIV        V4CDIV
109000000608     C                   MOVEL     V3PLIC        V4PLIC
109100900517     C  N09              EXSR      CARCON
109200900517     C*                    ----------
109300000608      *
109400900517     C                   SETON                                        09        1° VOLTA
109500900517     C     EMICON        TAG
109600991102     C                   WRITE     CN0W9Z4
109700991102     C                   EXFMT     CN0W9C4
109800000608      *
109900910725     C   18              GOTO      ENDCON
110000000608      *
110100000608     C* CONTROLLO CONTANTI IMMESSI
110200000608     C                   EXSR      CTRCON
110300000608      *
110400900518     C* CALCOLO  TOTALE CONTANTI /
110500910726     C   14              EXSR      CALCON
110600900517     C*                    -----------
110700910726     C  N14
110800910726     CANN18              GOTO      EMICON
110900000608      *
111000900517     C     ENDCON        TAG
111100900517     C                   ENDSR
111200991103     C**----------------------------------------------------------
111300900518     C** PULIZIA DEL SUBFILE CONTANTI
111400991103     C**----------------------------------------------------------
111500900517     C     PULCON        BEGSR
111600900517     C                   SETON                                        70
111700991102     C                   SETOFF                                       71
111800991102     C                   WRITE     CN0W9C4
111900900517     C                   SETOFF                                       70
112000900517     C                   ENDSR
112100991103     C**----------------------------------------------------------
112200900518     C** CARICAMENTO SUBFILE CONTANTI
112300991103     C**----------------------------------------------------------
112400900517     C     CARCON        BEGSR
112500000608      *
112600900517     C* CARICO I TAGLI PER IL CONTANTE
112700900517     C* L'IMPORTO IN TABELLA è 0 IL CAMPO IMPORTO è SPROTETTO
112800900517     C* LA DESCRIZ. DEI TAGLI CONTANTE è PROTETTO
112900991102     C                   CLEAR                   VNRTGL
113000900517     C                   SETON                                        40
113100991103     C                   Z-ADD     0             NR4               4 0
113200991104     C                   MOVEL     V3CDIV        TGDDIV
113300991102     C     TGDDIV        CHAIN     YNTGD01L                           21
113400991102     C     *IN21         DOWEQ     '0'
113500991102     C                   MOVE      TGDDES        VDESCR
113600991104     C                   Z-ADD     TGDVAL        HIMTGL
113700900517     C     HIMTGL        COMP      0                                  20        PROTETTO
113800900517     C                   MOVEL     'C'           HIDTIP
113900991103     C                   ADD       1             NR4
114000991102     C                   WRITE     CN0W9S4
114100991102     C                   SETON                                        71
114200991102     C     TGDDIV        READE     YNTGD01L                               21
114300900517     C                   END
114400900517     C                   SETOFF                                       2040
114500000608      *
114600900517     C                   ENDSR
114700991103     C**----------------------------------------------------------
114800000608     C** CONTROLLO  CONTANTI  IMMESSI
114900991103     C**----------------------------------------------------------
115000000608     C     CTRCON        BEGSR
115100000608      *
115200000608      *  AGGIORNA SEMPLICEMENTE IL SFL
115300000608     C                   Z-ADD     0             NR4
115400000608 +++ C                   Z-ADD     0             §TCONT
115500000612     C                   Z-ADD     0             TOTIM            15 3
115600000608      *
115700000608     C                   DO        *HIVAL
115800000608     C                   ADD       1             NR4
115900000608     C     NR4           CHAIN     CN0W9S4                            63
116000000608      *
116100000608      *  PER FINE FILE ESCE DAL GIRO
116200000608     C     *IN63         IFEQ      *ON
116300000608     C                   LEAVE
116400000608     C                   END
116500000608      *
116600000608      *  CALCOLA LA RIGA
116700000608     C     HIDTIP        IFEQ      'C'
116800000608     C     HIMTGL        ANDNE     *ZEROS
116900000608     C     VNRTGL        MULT      HIMTGL        VIDIM
117000000612     C                   ADD       VIDIM         TOTIM
117100000608     C     HIMTGL        COMP      0                                  20        PROT.VIDIM
117200000608     C                   SETON                                        40
117300000608     C                   UPDATE    CN0W9S4
117400000608     C                   END
117500000608     C*
117600000608     C                   END
117700000608      *
117800000608     C                   ENDSR
117900000608     C**----------------------------------------------------------
118000000608     C** CALCOLO TOTALE  CONTANTI
118100000608     C**----------------------------------------------------------
118200000608     C     CALCON        BEGSR
118300000608      *
118400991103     C                   Z-ADD     0             NR4
118500910730 +++ C                   Z-ADD     0             §TCONT
118600900517     C                   DO        *HIVAL
118700000612      *
118800991103     C                   ADD       1             NR4
118900991103     C     NR4           CHAIN     CN0W9S4                            63
119000910731     C     *IN63         IFEQ      '0'
119100000612      *
119200900517     C     HIDTIP        IFEQ      'C'
119300900517     C     HIMTGL        IFNE      *ZEROS
119400900517     C     VNRTGL        MULT      HIMTGL        VIDIM
119500900517     C                   ADD       VIDIM         §TCONT
119600900517     C     HIMTGL        COMP      0                                  20        PROT.VIDIM
119700900517     C                   SETON                                        40
119800991102     C                   UPDATE    CN0W9S4
119900900517     C                   ELSE
120000900517     C                   ADD       VIDIM         §TCONT
120100900517     C                   END
120200000612      *
120300900517     C                   END
120400900517     C*
120500900517     C                   END
120600000612     C*
120700910731     C  N63              END
120800000608      *
120900900517     C                   ENDSR
121000991103     C*-------------------------------------------------------
121100991103     ** Calcolo totale incassato in lire
121200991103     **-------------------------------------------------------
121300991103     C     CALINC        BEGSR
121400991103     C*
121500991120     C                   Z-ADD     1             A
121600991120     C     '99'          LOOKUP    CSP(A)                                 21
121700991120     C     *IN21         IFEQ      '1'
121800991120     C                   MOVE      'A'           VDA4
121900991120     C                   MOVE      'CA'          VSPE4
122000991120     C                   MOVE      CAU(A)        VCAU4
122100991120     C                   END
122200991120     C                   Z-ADD     VIDCCP        VKCC4
122300991120     C                   Z-ADD     VIDSCP        VKSC4
122400991103     C* Calcolo il totale incassato in base ai mandati d'Introito
122500991103     C* già presi in carico in bas
122600991103     C                   Z-ADD     0             TOTINC           15 2
122700991103     C* Imposto i campi chiave per l'accesso al file
122800991103     C                   MOVE      §FLE          KFLE
122900991103     C                   MOVE      'S'           KFPC
123000991103     C     KARM          CHAIN     FIARM02L                           31
123100991103     C     *IN31         DOWEQ     '0'
123200991103     C* Verifico se il record ha il totale della divisa del
123300991103     C* versamento maggiore di zero e se non è gia stato
123400991103     C* versato
123500991103     C* ... Lire
123600991104     C     V3CDIV        IFEQ      'ITL'
123700991103     C     ARMTIL        IFGT      0
123800991104     C     ARMVIL        ANDEQ     ' '
123900991120     C                   ADD       ARMTIL        TOTINC
124000991120     C                   Z-ADD     ARMTIL        VIMP4
124100991120     C                   Z-ADD     ARMNMI        VNRP4
124200991120     C                   Z-ADD     ARMDMI        G02INV
124300991120     C                   MOVE      *ZEROS        G02DAT
124400991120     C                   MOVE      '3'           G02ERR
124500991120     C                   CALL      'XSRDA8'
124600991120     C                   PARM                    WLBDAT
124700991120     C                   MOVE      G02DAT        VDRP4
124800991120     C*
124900991120     C                   SETON                                        88
125000991120     C                   ADD       1             NR5
125100991120     C                   WRITE     CN0W9S5
125200991103     C                   END
125300991103     C* ... Euro
125400991103     C                   ELSE
125500991104     C     V3CDIV        IFEQ      'EUR'
125600991103     C     ARMTIE        IFGT      0
125700991104     C     ARMVIE        ANDEQ     ' '
125800991103     C                   ADD       ARMTIE        TOTINC
125900991120     C                   Z-ADD     ARMTIE        VIMP4
126000991120     C                   Z-ADD     ARMNMI        VNRP4
126100991120     C                   Z-ADD     ARMDMI        G02INV
126200991120     C                   MOVE      *ZEROS        G02DAT
126300991120     C                   MOVE      '3'           G02ERR
126400991120     C                   CALL      'XSRDA8'
126500991120     C                   PARM                    WLBDAT
126600991120     C                   MOVE      G02DAT        VDRP4
126700991120     C*
126800991120     C                   SETON                                        88
126900991120     C                   ADD       1             NR5
127000991120     C                   WRITE     CN0W9S5
127100991103     C                   END
127200991103     C                   END
127300991103     C                   END
127400991103     C* Lettura nuovo record FIARM
127500991103     C     KARM          READE     FIARM02L                               31
127600991103     C                   END
127700991103     C*
127800991103     C                   ENDSR
127900000105     C*-------------------------------------------------------
128000000105     ** CONTROLLO SE C'E' DELL'INCASSATO
128100000105     **-------------------------------------------------------
128200000105     C     CTRINC        BEGSR
128300000105     C*
128400000105     C* Calcolo il totale incassato in base ai mandati d'Introito
128500000105     C* già presi in carico in bas
128600000105     C                   MOVEL     'N'           WINCA             1
128700000105     C* Imposto i campi chiave per l'accesso al file
128800000105     C                   MOVE      §FLE          KFLE
128900000105     C                   MOVE      'S'           KFPC
129000000105     C     KARM          CHAIN     FIARM02L                           31
129100000105     C     *IN31         DOWEQ     '0'
129200000105     C     WINCA         ANDEQ     'N'
129300000105     C* Verifico se il record ha il totale della divisa del
129400000105     C* versamento maggiore di zero e se non è gia stato
129500000105     C* versato
129600000105     C* ... Lire
129700000105     C     V3CDIV        IFEQ      'ITL'
129800000105     C     ARMTIL        IFGT      0
129900000105     C     ARMVIL        ANDEQ     ' '
130000000105     C                   MOVEL     'S'           WINCA
130100000105     C                   END
130200000105     C* ... Euro
130300000105     C                   ELSE
130400000105     C     V3CDIV        IFEQ      'EUR'
130500000105     C     ARMTIE        IFGT      0
130600000105     C     ARMVIE        ANDEQ     ' '
130700000105     C                   MOVEL     'S'           WINCA
130800000105     C                   END
130900000105     C                   END
131000000105     C                   END
131100000105     C* Lettura nuovo record FIARM
131200000105     C     KARM          READE     FIARM02L                               31
131300000105     C                   END
131400000105     C*
131500000105     C                   ENDSR
131600991103     C**----------------------------------------------------------
131700891205     C** PULIZIA DELL'ULTIMO  SUBFILE
131800991103     C**----------------------------------------------------------
131900891205     C     PULI4         BEGSR
132000891205     C                   SETON                                        70
132100991102     C                   WRITE     CN0W9C5
132200891205     C                   SETOFF                                       70
132300891205     C                   ENDSR
132400991103     C**----------------------------------------------------------
132500891205     C** CARICAMENTO SUBFILE 4
132600991103     C**----------------------------------------------------------
132700891205     C     CARI4         BEGSR
132800891205     C*
132900991104     C                   MOVEL     V3CDIV        V5CDIV
133000000608     C                   MOVEL     V3PLIC        V5PLIC
133100991103     C                   Z-ADD     0             NR5               4 0
133200000608      *
133300891205     C                   DO        20            A
133400891205     C     IMP(A)        IFNE      *ZEROS
133500891205     C                   MOVE      CSP(A)        VSPE4
133600891205     C                   Z-ADD     IMP(A)        VIMP4
133700991120     C                   Z-ADD     0             VNRP4
133800991120     C                   Z-ADD     0             VDRP4
133900891205     C                   MOVE      'D'           VDA4
134000891205     C                   MOVE      CAU(A)        VCAU4
134100940831     C                   SETOFF                                       32
134200891205     C                   Z-ADD     KBNA          VKCC4
134300910726     C                   Z-ADD     VBAN          VKSC4
134400891205     C*
134500891205     C                   SETOFF                                       88
134600991102     C                   ADD       1             NR5
134700991102     C                   WRITE     CN0W9S5
134800991103     C                   END
134900930927     C*
135000930927     C*  CARICO  SUBFILE X IMPORTI CAUSALI ALTRO ISTITUTO
135100930927     C*
135200930927     C     IM1(A)        IFNE      *ZEROS
135300930927     C                   MOVE      CSP(A)        VSPE4
135400930927     C                   Z-ADD     IM1(A)        VIMP4
135500930927     C                   MOVE      'D'           VDA4
135600930927     C                   MOVE      CA1(A)        VCAU4
135700940831     C                   SETOFF                                       32
135800930927     C                   Z-ADD     KBNA          VKCC4
135900930927     C                   Z-ADD     VBAN          VKSC4
136000991120     C                   Z-ADD     0             VNRP4
136100991120     C                   Z-ADD     0             VDRP4
136200930927     C*
136300930927     C                   SETOFF                                       88
136400991103     C                   ADD       1             NR5
136500991102     C                   WRITE     CN0W9S5
136600930927     C                   END
136700930927     C*
136800891205     C                   END
136900991104     C                   Z-ADD     1             A
137000891205     C* CONTANTE
137100891205     C* --------
137200891205     C     TOTCON        IFGT      *ZEROS
137300891205     C* 88 ON RENDE MODIFICABILE LA CAUSALE
137400891205     C                   SETON                                        88
137500891205     C                   Z-ADD     1             A
137600891205     C                   Z-ADD     TOTCON        VIMP4
137700891205     C                   MOVE      'D'           VDA4
137800891205     C     '97'          LOOKUP    CSP(A)                                 21
137900891205     C     *IN21         IFEQ      '1'
138000891205     C                   MOVE      'CO'          VSPE4
138100891205     C                   MOVE      CAU(A)        VCAU4
138200940831     C                   SETOFF                                       32
138300891205     C                   Z-ADD     KBNA          VKCC4
138400910726     C                   Z-ADD     VBAN          VKSC4
138500991120     C                   Z-ADD     0             VNRP4
138600991120     C                   Z-ADD     0             VDRP4
138700891205     C*
138800991103     C                   ADD       1             NR5
138900991102     C                   WRITE     CN0W9S5
139000891205     C                   END
139100991104     C                   END
139200000608      *
139300991103     C* Per le filiali di primo livello imposto nella cassa
139400991103     C* la differenza fra il totale versato e quello incassato
139500991104     C     *IN01         IFEQ      '0'
139600000608      *
139700991120     C*  nella divisa del versamento
139800991120     C  N01              EXSR      CALINC
139900000608      *
140000991103     C     TOTVER        SUB       TOTINC        TOTCAS           15 2
140100000608      *
140200991103     C* Se il totale Versato è superiore al totale incassato
140300991103     C* la differenza positiva deve essere prelevata dalla
140400991103     C* cassa
140500991103     C     TOTCAS        IFGT      0
140600991103     C                   Z-ADD     TOTCAS        VIMP4
140700991103     C                   MOVE      'A'           VDA4
140800991103     C                   MOVE      'CA'          VSPE4
140900991103     C                   Z-ADD     VIDCC1        VKCC4
141000991103     C                   Z-ADD     VIDSC1        VKSC4
141100991103     C                   MOVE      CAU(A)        VCAU4
141200991120     C                   Z-ADD     0             VNRP4
141300991120     C                   Z-ADD     0             VDRP4
141400991103     C                   SETON                                        88
141500991103     C                   ADD       1             NR5
141600991103     C                   WRITE     CN0W9S5
141700000608      *
141800991103     C* .. altrimenti se il totale da versare è inferiore
141900991103     C* al totale incassato il rimanente verrà versato in
142000991103     C* cassa
142100991103     C                   ELSE
142200991103     C     TOTCAS        IFLT      0
142300991209     C                   Z-SUB     TOTCAS        VIMP4
142400991104     C                   MOVE      'D'           VDA4
142500991103     C                   MOVE      'CA'          VSPE4
142600991103     C                   Z-ADD     VIDCC1        VKCC4
142700991103     C                   Z-ADD     VIDSC1        VKSC4
142800991103     C                   MOVE      CAU(A)        VCAU4
142900991120     C                   Z-ADD     0             VNRP4
143000991120     C                   Z-ADD     0             VDRP4
143100991103     C                   SETON                                        88
143200991103     C                   ADD       1             NR5
143300991103     C                   WRITE     CN0W9S5
143400991103     C                   END
143500991103     C                   END
143600000608      *
143700991103     C* Imposto quindi il totale incassato per la scrittura
143800991103     C* dell'ultima riga
143900991103     C                   Z-ADD     TOTINC        WTOIMP           15 2
144000991103     C* ... Secondo Livello
144100991103     C                   ELSE
144200991103     C                   Z-ADD     TOTVER        WTOIMP
144300891205     C* CONTROPARTITA CASSA
144400991120     C                   EXSR      CALINC
144500991120     C     TOTVER        SUB       TOTINC        TOTCAS           15 2
144600991120     C     TOTCAS        IFNE      0
144700991120     C     TOTCAS        IFGT      0
144800991120     C                   Z-ADD     TOTCAS        WTOIMP
144900991120     C                   MOVE      'A'           VDA4
145000991120     C                   ELSE
145100991120     C     -1            MULT      TOTCAS        WTOIMP
145200991120     C                   MOVE      'D'           VDA4
145300991120     C                   END
145400891205     C* -------------------
145500891205     C                   Z-ADD     1             A
145600891205     C     '99'          LOOKUP    CSP(A)                                 21
145700891205     C     *IN21         IFEQ      '1'
145800991103     C                   Z-ADD     WTOIMP        VIMP4
145900991216     C*********+           MOVE 'A'       VDA4
146000891205     C                   MOVE      'CA'          VSPE4
146100891205     C                   MOVE      CAU(A)        VCAU4
146200891205     C                   END
146300891205     C                   Z-ADD     VIDCCP        VKCC4
146400891205     C                   Z-ADD     VIDSCP        VKSC4
146500991120     C                   Z-ADD     §FLE          VNRP4
146600991122     C                   Z-ADD     WTODAY        VDRP4
146700891205     C*
146800891206     C                   SETON                                        88
146900991102     C                   ADD       1             NR5
147000991102     C                   WRITE     CN0W9S5
147100991120     C                   END
147200991120     C                   END
147300991104     C*
147400891205     C                   ENDSR
147500891205     C/EJECT
147600000608     C**------------------------------------------------------------
147700000000     C** ANNULLAMENTO DISTINTA STAMPATA
147800000608     C**------------------------------------------------------------
147900000000     C     ANNDIS        BEGSR
148000000608     C*
148100910719     C                   DO        TREC          CNT
148200991103     C     CNT           CHAIN     CN0W9S2                            21
148300000608     C*
148400910731     C     VANN          IFNE      'E'
148500910726     C                   MOVEL     VNRA          KNRA
148600910726     C                   MOVE      VNRA1         KNRA
148700910726     C                   Z-ADD     VABI          KABI
148800910726     C                   Z-ADD     VCAB          KCAB
148900940126     C                   SETOFF                                       57
149000000608     C*
149100941103     C     KASS          SETLL     CNASS000                           57
149200000608     C*
149300920206     C     *IN57         DOWEQ     '0'
149400941103     C     KASS          READE     CNASS000                               57
149500941103     C*          KASS      CHAINCNASS000             57
149600910719     C*
149700951027     C     ASSFVE        IFEQ      'V'
149800951027     C     ASSTPI        ANDNE     'M'
149900941103     C  N57              DELETE    CNASS000
150000940126     C                   END
150100000608     C*
150200951027     C                   END
150300940126     C*
150400940126     C     SAVNRA        IFNE      VNRA
150500940126     C     SAVNR1        ORNE      VNRA1
150600940126     C     SAVABI        ORNE      VABI
150700940126     C     SAVCAB        ORNE      VCAB
150800000608     C*
150900940126     C                   MOVEL     SAVNRA        KNRA
151000940126     C                   MOVE      SAVNR1        KNRA
151100940126     C                   Z-ADD     SAVABI        KABI
151200940126     C                   Z-ADD     SAVCAB        KCAB
151300940126     C                   SETOFF                                       57
151400000608     C*
151500941103     C     KASS          SETLL     CNASS000                           57
151600000608     C*
151700940126     C     *IN57         DOWEQ     '0'
151800941103     C     KASS          READE     CNASS000                               57
151900941103     C*          KASS      CHAINCNASS000             57
152000940126     C*
152100951027     C     ASSFVE        IFEQ      'V'
152200951027     C     ASSTPI        ANDNE     'M'
152300941103     C  N57              DELETE    CNASS000
152400951027     C                   END
152500910719     C*
152600920206     C                   END
152700940126     C                   END
152800910719     C                   ELSE
152900901011     C                   EXSR      PULCON
153000901011     C                   EXSR      CARCON
153100910719     C                   END
153200910722     C                   END
153300911125     C                   SETON                                        69
153400000000     C                   ENDSR
153500000608     C**-------------------------------------------------------------
153600000608     C** SCRIVE L'ARCIHVIO DEI PLICHI DA TRASMETTERE A SICURDATA
153700000608     C**-------------------------------------------------------------
153800000608     C     WRIPLI        BEGSR
153900000612      *
154000000612      *  LEGGE IL SFL PER SCRIVERE IL FILE DEL PLICO DA TRASMETTERE
154100000612     C                   Z-ADD     0             NR4
154200000612     C                   Z-ADD     0             §TCONT
154300000612     C                   Z-ADD     0             NRGPLI            5 0
154400000612     C                   TIME                    FLD14            14 0
154500000612     C                   MOVEL     FLD14         ORAMIN            4 0
154600000612      *
154700000612     C                   DO        *HIVAL
154800000612     C                   ADD       1             NR4
154900000612     C     NR4           CHAIN     CN0W9S4                            63
155000000612      *
155100000612      *  PER FINE FILE ESCE DAL GIRO
155200000612     C     *IN63         IFEQ      *ON
155300000612     C                   LEAVE
155400000612     C                   END
155500000612      *
155600000612      * IMPOSTA I CAMPI DI SCRITTURA DEL FNPLI00F
155700000612     C     VNRTGL        IFGT      0
155800000612      *
155900000612     C                   CLEAR                   FNPLI000
156000000612     C                   ADD       1             NRGPLI
156100000612     C                   MOVEL     §FLE          PLIFIL
156200000612     C                   Z-ADD     V4PLIC        PLINUM
156300000612     C                   Z-ADD     NRGPLI        PLIRIG
156400000612     C                   Z-ADD     *DATE         G02DAT
156500000612     C                   MOVE      *ZEROS        G02INV
156600000612     C                   MOVEL     *BLANK        G02ERR
156700000612     C                   CALL      'XSRDA8'
156800000612     C                   PARM                    WLBDAT
156900000612     C                   MOVE      G02INV        PLIDIM
157000000612     C                   MOVE      ORAMIN        PLIHIM
157100000612     C                   MOVEL     V1CDIV        PLIDIV
157200000612     C                   Z-ADD     HIMTGL        PLITAG
157300000612     C                   Z-ADD     VNRTGL        PLIQTA
157400000612     C                   Z-ADD     VIDIM         PLITIM
157500000612      *
157600000612     C                   ADD       1             NRPLI             7 0
157700000612     C                   WRITE     FNPLI000
157800000612     C                   ENDIF
157900000612      *
158000000612      * AGGIORNA IL SFL
158100000612     C     HIMTGL        COMP      0                                  20        PROT.VIDIM
158200000612     C                   SETON                                        40
158300000612     C                   UPDATE    CN0W9S4
158400000612     C*
158500000612     C                   END
158600000612      *
158700000608     C                   ENDSR
158800000608     C**-------------------------------------------------------------
158900000608     C** CREAZIONE MOVIMENTI CONTABILI
159000000608     C**-------------------------------------------------------------
159100000608     C     CREMOV        BEGSR
159200000608     C*
159300991103     C                   MOVE      KNMUS         MOVNMU
159400991103     C                   MOVE      KNRAZ         MOVRIF
159500991103     C                   MOVEL     §FLE          MOVRIF
159600991103     C                   Z-ADD     VIDNRG        MOVSBN
159700991103     C                   ADD       1             REGSWD                         *PROGR.REGIS
159800991103     C                   Z-ADD     REGSWD        MOVSBR
159900991103     C                   MOVEL     'V'           MOVFGE
160000991103     C                   MOVE      KNSIF         WSOC              4
160100991103     C                   MOVEL     WSOC          MOVSOC
160200991103     C                   MOVEL     *BLANKS       MOVFPN
160300991103     C                   MOVEL     §FLE          MOVFPN
160400991103     C                   MOVEL     'D'           MOVTMO
160500991103     C                   MOVEL     'CG'          MOVCTB
160600000000     C                   Z-ADD     VIDNRG        MOVNRG
160700000000     C                   Z-ADD     AMGDRG        MOVDRG
160800991103     C                   MOVE      'COGE'        MOVSRR
160900991103     C                   MOVEL     VCAU4         MOVCOD
161000991103     C                   MOVE      *ZEROS        MOVKCC
161100991103     C                   MOVE      *ZEROS        MOVKSC
161200991103     C                   MOVE      VKCC4         MOVKCC
161300991103     C                   MOVE      VKSC4         MOVKSC
161400991103     C* Dare/Avere
161500991103     C     VDA4          IFEQ      'D'
161600991103     C                   Z-ADD     1             MOVDAR
161700991103     C                   Z-ADD     0             MOVAVE
161800991103     C                   ELSE
161900991103     C                   Z-ADD     0             MOVDAR
162000991103     C                   Z-ADD     1             MOVAVE
162100991103     C                   END
162200991103     C* Se la divisa di versamento è diversa da quella
162300991103     C* di conto effettuo la conversione per l'importo
162400991103     C* in ctb.
162500991103     C     V5CDIV        IFEQ      §GEDCN
162600991103     C                   Z-ADD     VIMP4         MOVIMM
162700991103     C                   Z-ADD     1             MOVCMB
162800991103     C                   ELSE
162900991103     C                   CLEAR                   YEURDS
163000991103     C                   MOVEL     V5CDIV        YECDVI
163100991103     C                   Z-ADD     VIMP4         YECIMI
163200991103     C                   MOVEL     §GEDCN        YECDVO
163300991103     C                   MOVEL     'H'           YECTAR
163400991103     C                   CALL      'YEURCO'
163500991103     C                   PARM                    YEURDS
163600991103     C                   Z-ADD     YECIMO        MOVIMM
163700991103     C                   Z-ADD     CMBALT        MOVCMB
163800991103     C                   END
163900991103     C                   Z-ADD     VIMP4         MOVIMD
164000991103     C                   MOVEL     V5CDIV        MOVDIV
164100991103     C                   Z-ADD     AMGDRG        MOVDRC
164200000000     C                   Z-ADD     VIDNDS        MOVNDC
164300991118     C                   Z-ADD     AMGDOP        MOVDDC
164400991103     C                   MOVE      *BLANKS       MOVSRD
164500000000     C                   Z-ADD     MOVNDC        MOVNRP
164600991104     C     VKCC4         IFEQ      §4OCC1                                       INC. ATTRIB.
164700991120     C                   Z-ADD     VNRP4         MOVNRP                         2° LIV.
164800920831     C                   END
164900991120     C                   MOVE      VDRP4         G02DAT
165000991120     C                   MOVE      *BLANKS       G02ERR
165100991120     C                   CALL      'XSRDA8'
165200991120     C                   PARM                    WLBDAT
165300991120     C                   Z-ADD     G02INV        MOVDRP
165400991103     C                   MOVEL     *BLANKS       MOVSRP
165500991105     C                   MOVEL     §GEDCN        MOVDIP
165600991103     C                   Z-ADD     MOVIMM        MOVIPM
165700991105     C                   Z-ADD     MOVIMM        MOVIPD
165800991103     C                   Z-ADD     0             MOVDSC                         *
165900991103     C                   MOVEL     VDES4         MOVDES
166000991103     C                   MOVE      *ZEROS        MOVCCP
166100991103     C                   MOVE      *ZEROS        MOVSCP
166200991103     C                   MOVE      VIDCCP        MOVCCP
166300991103     C                   MOVE      VIDSCP        MOVSCP
166400991103     C                   MOVEL     *BLANKS       MOVCDC
166500991103     C                   Z-ADD     0             MOVDCA
166600991103     C                   Z-ADD     MOVDRG        MOVDOP
166700921204     C*§
166800991104     C                   ADD       1             NRMOV             7 0
166900991103     C                   WRITE     FNMOV000
167000000000     C                   ENDSR
167100991103     C*-------------------------------------------------------
167200991103     ** Aggiorna file mnadnati d'introito
167300991103     **-------------------------------------------------------
167400991103     C     AGGARM        BEGSR
167500991103     C*
167600991103     C* Imposto i campi chiave per l'accesso al file
167700991103     C                   MOVE      §FLE          KFLE
167800991103     C                   MOVE      'S'           KFPC
167900991103     C     KARM          CHAIN     FIARM02L                           31
168000991103     C     *IN31         DOWEQ     '0'
168100991103     C* Verifico se il record ha il totale della divisa del
168200991103     C* versamento maggiore di zero e se non è gia stato
168300991103     C* versato
168400991103     C* ... Lire
168500991104     C     V3CDIV        IFEQ      'ITL'
168600991103     C     ARMTIL        IFGT      0
168700991105     C     ARMVIL        ANDEQ     ' '
168800991103     C* ... nel caso controllo se è da aggiornare o cancellare
168900991130     C***        ARMTIE    IFGT 0
169000991130     C***        ARMVIE    ANDEQ' '
169100991103     C                   MOVEL     'S'           ARMVIL
169200991103     C                   EXCEPT    AGGVIL
169300991130     C***                  ELSE
169400991130     C***                  DELETFIARM000
169500991130     C***                  END
169600991103     C                   END
169700991103     C* ... Euro
169800991103     C                   ELSE
169900991104     C     V3CDIV        IFEQ      'EUR'
170000991103     C     ARMTIE        IFGT      0
170100991105     C     ARMVIE        ANDEQ     ' '
170200991103     C* ... nel caso controllo se è da aggiornare o cancellare
170300991130     C***        ARMTIL    IFGT 0
170400991130     C***        ARMVIL    ANDEQ' '
170500991103     C                   MOVEL     'S'           ARMVIE
170600991103     C                   EXCEPT    AGGVIE
170700991130     C**                   ELSE
170800991130     C***                  DELETFIARM000
170900991130     C**                   END
171000991103     C                   END
171100991103     C                   END
171200991103     C                   END
171300991103     C* Lettura nuovo record FIARM
171400991103     C     KARM          READE     FIARM02L                               31
171500991103     C                   END
171600991103     C*
171700991103     C                   ENDSR
171800991103     C*-------------------------------------------------------
171900991103     ** Sottometto trasmissioni
172000991103     **-------------------------------------------------------
172100991103     C     SBMTRA        BEGSR
172200991104     C*
172300991104     C* Richiamo TIBS50R per conoscere librerie/S.Informativi
172400991104     C* da/a cui spedire
172500991104     C                   CLEAR                   TIBS50
172600991104     C                   MOVEL     'L'           I50TLA
172700991104     C                   MOVEL     KNSIF         I50PSI
172800991104     C                   MOVEL     SIMFEL        I50PPO
172900991104     C                   MOVEL     '046'         I50APO
173000991104     C                   CALL      'TIBS50R'
173100991104     C                   PARM                    TIBS50
173200991103     C*
173300991104     C                   MOVEL     '046'         CMZDST                          DEST.:SEDE
173400991104     C                   MOVEL     'FNMOV00T'    CMZFLD                          DA: >>>
173500991105     C                   MOVEL     'M   '        VAR4              4
173600991105     C                   MOVE      §FLE          VAR4
173700991104     C                   MOVEL     *BLANK        CMZMBD
173800991105     C                   MOVEL     VAR4          CMZMBD
173900991104     C                   MOVEL     O50PLA        CMZLBD
174000991104     C                   MOVEL     'FNMOV00R'    CMZFLA                          A: >>>
174100991104     C                   MOVEL     *BLANK        CMZMBA
174200991104     C                   MOVEL     VAR4          CMZMBA
174300991104     C                   MOVEL     O50ALA        CMZLBA
174400991104     C                   MOVE      '0000'        CMZORI                          ORA
174500991104     C                   MOVE      '0000000'     CMZDTI                          DATA
174600991104     C                   MOVE      'N'           CMZURG                          NOTTE
174700991104     C                   MOVE      'A'           CMZADD                          IN ADD
174800991104     C                   MOVE      'S'           CMZCLR                          CLEAR
174900991104     C                   MOVE      'S'           CMZFTR                          DA TRANS.
175000991104     C                   MOVE      *BLANK        CMZERR                          1= ERR.
175100991108     C                   MOVE      'Y029'        CMZAZN                          AZIONE
175200991104     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
175300991104     C                   MOVE      NRMOV         CMZNRR                          N.REC.
175400991104     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-REQ.
175500991104     C*
175600991104     C                   CLEAR                   W236
175700991104     C                   CALL      'TIBS52C'
175800991104     C                   PARM                    O50LCL
175900991104     C                   PARM                    CMZDST
176000991104     C                   PARM                    CMZFLD
176100991104     C                   PARM                    CMZMBD
176200991104     C                   PARM                    CMZLBD
176300991104     C                   PARM                    CMZFLA
176400991104     C                   PARM                    CMZMBA
176500991104     C                   PARM                    CMZLBA
176600991104     C                   PARM                    CMZORI
176700991104     C                   PARM                    CMZDTI
176800991104     C                   PARM                    CMZURG
176900991104     C                   PARM                    CMZADD
177000991104     C                   PARM                    CMZCLR
177100991104     C                   PARM                    CMZFTR
177200991104     C                   PARM                    CMZERR
177300991104     C                   PARM                    CMZAZN
177400991104     C                   PARM                    CMZSYF
177500991104     C                   PARM                    CMZNRR
177600991104     C                   PARM                    CMZPRR
177700991104     C                   PARM                    W236            236
177800000612     C*
177900991103     C*§ SOTTOMETTO STAMPA VARIAZ. A C/ASSEGNI E P.ASSEGNATI
178000991104     C     *IN01         IFEQ      '0'
178100991103     C                   MOVEL     'LR92'        KCOAZ
178200991103     C                   MOVEL     'KSSPECL'     KQEBI
178300991103     C                   CALL      'BCH10'
178400991103     C                   PARM                    KPJBA
178500991103     C*§ SOTTOMETTO STAMPA NOTE ACCREDITO OPERATORE 999 DEL GG PRECED.
178600991103     C                   MOVE      SIMFEL        PFLE
178700991103     C                   MOVE      *ALL'9'       PCUC
178800991103     C                   MOVE      *ALL'9'       PNMI
178900991103     C                   MOVE      DAT1GG        PDMI
179000991103     C                   MOVEL     PARNAC        KPJBU
179100991103     C                   MOVEL     'LR93'        KCOAZ
179200991103     C                   MOVEL     'KSSPECL'     KQEBI
179300991103     C                   CALL      'BCH10'
179400991103     C                   PARM                    KPJBA
179500991103     C*§ SOTTOMETTO STAMPA ELENCO SPEDIZIONI RIPRISTINATE DOPO CONTEGGI
179600991103     C                   MOVEL     *BLANKS       KPJBU
179700991103     C                   MOVEL     'LR94'        KCOAZ
179800991103     C                   MOVEL     'KSSPECL'     KQEBI
179900991103     C                   CALL      'BCH10'
180000991103     C                   PARM                    KPJBA
180100991104     C                   END
180200991103     C*
180300991103     C                   ENDSR
180400000612     C**---------------------------------------------------------------
180500000000     C** STAMPA DISTINTA BANCA
180600000612     C**---------------------------------------------------------------
180700000000     C     STADIS        BEGSR
180800910726     C                   EXCEPT    TESTA
180900910719     C                   DO        TREC          CNT
181000991103     C     CNT           CHAIN     CN0W9S2                            20
181100930927     C                   SETOFF                                       54
181200930927     C*
181300930927     C*  CONTROLLO SE ASSEGNO STESSO/ALTRO ISTITUTO
181400930927     C*
181500910731     C     VANN          IFNE      'E'
181600911127+++  C     VASS          ANDNE     0
181700930927     C*
181800930927     C                   SETON                                        54
181900930927     C*
182000930927     C*  CONTROLLO SE ASSEGNO STESSO/ALTRO ISTITUTO
182100930927     C     ISTABI        IFEQ      VABI
182200930927     C                   SETON                                        53
182300930927     C                   ELSE
182400930927     C                   SETOFF                                       53
182500930927     C                   END
182600991105     C                   Z-ADD     VDTE          GMADAS            8 0
182700000000     C                   Z-ADD     1             A                 3 0
182800910719     C                   MOVEL     VTPA          LKP
182900910719     C                   MOVE      VTPP          LKP
183000910719     C     LKP           LOOKUP    CSP(A)                                 21
183100000000     C  N21              Z-ADD     1             A
183200000000     C                   SETOFF                                       22
183300910719     C     VTPP          IFEQ      'F'
183400991104     C                   ADD       VASS          TOTFUO           12 2
183500000000     C                   ELSE
183600991104     C                   ADD       VASS          TOTPIA           12 2
183700000000     C                   SETON                                        22
183800000000     C                   END
183900000000     C** TOTALE ASSEGNO PER TIPO
184000000000     C                   ADD       1             CONRIG            2 0
184100000000     C     CONRIG        IFGT      50
184200000000     C                   EXCEPT    TESTA
184300000000     C                   Z-ADD     1             CONRIG
184400000000     C                   END
184500991104     C* Controllo se importo con decimali
184600991104     C                   MOVE      VASS          WCOMO2            2 0
184700991104     C     WCOMO2        COMP      0                                      02
184800000000     C                   EXCEPT    DET
184900910719     C                   END
185000910726     C                   END
185100991104     C                   MOVE      TOTPIA        WCOMO2            2 0
185200991104     C     WCOMO2        COMP      0                                      03
185300991104     C                   MOVE      TOTFUO        WCOMO2            2 0
185400991104     C     WCOMO2        COMP      0                                      04
185500991104     C                   MOVE      TOTASS        WCOMO2            2 0
185600991104     C     WCOMO2        COMP      0                                      06
185700000000     C                   EXCEPT    TOT
185800000000     C                   ADD       2             CONRIG
185900000000     C** totale assegni fine elenco
186000000000     C     TOTCON        IFGT      0
186100900518     C*  DETTAGLIO SFL CONTANTE
186200991103     C                   Z-ADD     0             NR4
186300900518     C                   DO        *HIVAL
186400991103     C                   ADD       1             NR4
186500991103     C     NR4           CHAIN     CN0W9S4                            63
186600910731     C     *IN63         IFEQ      '0'
186700900518     C     HIDTIP        ANDEQ     'C'
186800900518     C                   ADD       1             CONRIG            2 0
186900900518     C     CONRIG        IFGT      50
187000900518     C                   EXCEPT    TESTA
187100900518     C                   Z-ADD     1             CONRIG
187200900518     C                   END
187300991105     C* Controllo se importo da stampare a decimali
187400991105     C                   MOVE      VIDIM         WCOMO2
187500991105     C     WCOMO2        COMP      0                                      08
187600900518     C                   EXCEPT    DETSFL
187700900518     C                   END
187800910731     C  N63              END
187900991105     C* Controllo se importo da stampare a decimali
188000991105     C                   MOVE      TOTCON        WCOMO2
188100991105     C     WCOMO2        COMP      0                                      09
188200000000     C                   EXCEPT    TOT2
188300000000     C                   END
188400000000     C** STAMPA TOTALE CONTANTI
188500000000     C                   DO        20            A
188600000000     C     IMP(A)        IFGT      0
188700930927     C                   SETON                                        53
188800000000     C                   ADD       1             CONRIG
188900000000     C     CONRIG        IFGT      50
189000000000     C                   EXCEPT    TESTA
189100000000     C                   Z-ADD     1             CONRIG
189200000000     C                   END
189300991104     C                   MOVE      IMP(A)        WCOMO2
189400991104     C     WCOMO2        COMP      0                                      07
189500000000     C                   EXCEPT    TOT1
189600000000     C                   END
189700930927     C*
189800930927     C     IM1(A)        IFGT      0
189900930927     C                   SETOFF                                       53
190000930927     C                   ADD       1             CONRIG
190100930927     C     CONRIG        IFGT      50
190200930927     C                   EXCEPT    TESTA
190300930927     C                   Z-ADD     1             CONRIG
190400930927     C                   END
190500991104     C                   MOVE      IM1(A)        WCOMO2
190600991104     C     WCOMO2        COMP      0                                      07
190700930927     C                   EXCEPT    TOT1
190800930927     C                   END
190900930927     C*
191000000000     C                   END
191100991105     C* Controllo se importo da stampare a decimali
191200991105     C                   MOVE      TOTDIS        WCOMO2
191300991105     C     WCOMO2        COMP      0                                      02
191400991105     C                   MOVE      TOTVER        WCOMO2
191500991105     C     WCOMO2        COMP      0                                      04
191600000000     C** STAMPA TOTALI ASSEGNI PER SPECIE
191700000000     C                   EXCEPT    TOT3
191800000000     C** STAMPA TOTALI FINALI
191900000000     C     NOS           TAG
192000000000     C                   ENDSR
192100891205     C/EJECT
192200000000     C**----------------------------------
192300891204     C**  CARICAMENTO TABELLE
192400000000     C**----------------------------------
192500000000     C     CARTAB        BEGSR
192600991102     C*
192700891204     C**TABELLA FILIALI MECCANIZZ. CON RELATIVA FILIALE PRIMARIA
192800891204     C                   MOVEL     '£3'          COD
192900891204     C     KTAB1         SETLL     TABEL                                  23
193000891204     C     SU£3          TAG
193100900413     C     KTAB1         READE     TABEL                                  23
193200921105     C  N23              EXCEPT    RILTAB
193300991103     C  N23              GOTO      SU£3
193400991102     C*§ LEGGO TABELLA '4E'
193500991102     C                   MOVEL     '4E'          COD
193600991102     C     KTAB1         SETLL     TABEL                                  23
193700991102     C  N23              GOTO      NOTAB
193800991102     C                   Z-ADD     0             A
193900991102     C     SUTAB         TAG
194000991102     C     KTAB1         READE     TABEL                                  23
194100991102     C  N23              EXCEPT    RILTAB
194200991102     C   23              GOTO      NOTAB
194300991102     C                   MOVEL     TBLKEY        SPE               2
194400991102     C     SPE           IFNE      *BLANKS
194500991102     C                   ADD       1             A
194600991102     C                   MOVEL     TBLUNI        DS4E
194700991102     C                   MOVEL     SPE           CSP(A)
194800991102     C                   MOVEL     §SPDES        DSP(A)
194900991102     C                   MOVEL     §SPCAU        CAU(A)                         CAUSALE CONT
195000991102     C     §SPCA2        IFNE      *BLANKS
195100991102     C                   MOVEL     §SPCA2        CA1(A)                         CAUSALE CONT
195200991102     C                   ELSE
195300991102     C                   MOVEL     §SPCAU        CA1(A)                         CAUSALE CONT
195400991102     C                   END
195500991102     C                   MOVEL     §SPCAU        OPECOD                         CAUSALE CONT
195600991102     C                   MOVEL     'PG'          OPECD1
195700991102     C     KEYOPE        CHAIN     CNOPE000                           80
195800991102     C   80              GOTO      NOTAB
195900991102     C                   MOVEL     CA1(A)        OPECOD                         CAUSALE CONT
196000991102     C                   MOVEL     'PG'          OPECD1
196100991102     C     KEYOPE        CHAIN     CNOPE000                           80
196200991102     C   80              GOTO      NOTAB
196300991102     C*
196400991102     C* CARICO CAUSALE PER FUORI PIAZZA SE NON SONO CAUSALI CED
196500991102     C*
196600991102     C                   MOVEL     SPE           CT                1
196700991102     C     CT            IFNE      '9'
196800991102     C                   ADD       1             A
196900991102     C                   MOVEL     SPE           CSP(A)
197000991102     C                   MOVE      'F'           CSP(A)
197100991102     C                   MOVEL     §SPDE1        DSP(A)
197200991102     C                   MOVEL     §SPCA1        CAU(A)                         CAUSALE CONT
197300991102     C     §SPCA3        IFNE      *BLANKS
197400991102     C                   MOVEL     §SPCA3        CA1(A)                         CAUSALE CONT
197500991102     C                   ELSE
197600991102     C                   MOVEL     §SPCA1        CA1(A)                         CAUSALE CONT
197700991102     C                   END
197800991102     C                   MOVEL     §SPCA1        OPECOD                         CAUSALE CONT
197900991102     C                   MOVEL     'PG'          OPECD1
198000991102     C     KEYOPE        CHAIN     CNOPE000                           80
198100991102     C   80              GOTO      NOTAB
198200991102     C                   MOVEL     CA1(A)        OPECOD                         CAUSALE CONT
198300991102     C                   MOVEL     'PG'          OPECD1
198400991102     C     KEYOPE        CHAIN     CNOPE000                           80
198500991102     C   80              GOTO      NOTAB
198600991102     C                   END
198700991102     C*
198800991102     C*
198900991102     C                   END
199000991102     C                   GOTO      SUTAB
199100000000     C     NOTAB         TAG
199200920831     C* LEGGO TABELLA '4O'
199300920831     C                   MOVEL     '4O'          COD
199400920831     C                   MOVEL     *BLANK        KEY
199500920831     C                   MOVEL     §FLE          KEY
199600910726     C     KTAB          CHAIN     TABEL                              46
199700921105     C  N46              EXCEPT    RILTAB
199800920831     C  N46              MOVEL     TBLUNI        DS4O
199900920831     C   46              GOTO      FINE
200000991102     C*  Aggancio tabella 'GED' x reperire la divisa di conto
200100991102     C                   MOVEL     'GED'         KCOD1
200200991102     C                   MOVEL     *BLANKS       KKEY1
200300991102     C                   MOVEL     '1'           KKEY1
200400991102     C     KTBE          CHAIN     TNTBE01L                           13
200500991102     C     *IN13         IFEQ      '0'
200600991102     C                   MOVEL     TBEUNI        DGED
200700991102     C                   END
200800991102     C* leggo la tabella CV e reperisco la divisa alternativa a
200900991102     C* quella corrente
201000991102     C                   MOVEL     'CV'          COD
201100991102     C     KTAB1         CHAIN     TABEL00F                           50
201200991102     C     *IN50         DOWEQ     '0'
201300991102     C                   MOVEL     TBLUNI        DSCV
201400991103     C                   MOVEL     TBLKEY        WDIV              3
201500991102     C     §CVDVA        IFEQ      'S'
201600991102     C     §GEDCN        ANDNE     WDIV
201700991103     C                   MOVEL     WDIV          DIVALT            3
201800991103     C                   Z-ADD     §CVCMB        CMBALT           13 6
201900991209     C                   Z-ADD     §CVDEC        DECALT            2 0
202000991102     C                   END
202100991209     C     §GEDCN        IFEQ      WDIV
202200991209     C                   Z-ADD     §CVDEC        DECDIV            2 0
202300991209     C                   END
202400991102     C     KTAB1         READE     TABEL00F                               50
202500991102     C                   END
202600991102     C*
202700000000     C                   ENDSR
202800891205     C*----------------------------------------------------*
202900911115     C* CARICAMENTO FILIALI VALIDE
203000911115     C*----------------------------------------------------*
203100911115     C     CARTB         BEGSR
203200991102     C*
203300911115     C                   MOVEL     '£3'          COD
203400911115     C                   MOVEL     *BLANKS       KEY
203500911115     C                   MOVEL     SIMFEL        KEY
203600911115     C     KTAB          CHAIN     TABEL                              50
203700921105     C  N50              EXCEPT    RILTAB
203800911115     C  N50              MOVEA     TBLUNI        FILA
203900911115     C   50              MOVEA     '0'           FILA
204000991102     C*
204100911115     C                   ENDSR
204200940831     C*----------------------------------------------------*
204300940831     C*  OPERAZIONI INIZIALI                               *
204400940831     C*----------------------------------------------------*
204500940831     C     *INZSR        BEGSR                                                  *
204600940831     C*
204700940831     C     *ENTRY        PLIST
204800940831     C                   PARM                    KPJBA
204900000608      *
205000000608      * -----------------
205100000608      * IMPOSTA L'INDICATORE (66) PER CONTROLLARE L'AZIONE
205200000608      *  ORIGINALE O QUELLA NUOVA PER GESTIRE DIRETTAMENTE
205300000608      *   LA BANCA PER CONTANTE PER IL NUOVO GIRO POSTE.
205400000608     C     KCDAZ         COMP      '%0W9'                             6666
205500000608      * -----------------
205600000608      *
205700991104     C* Eseguo la Open dei file
205800991104     C                   OPEN      FIARM02L                             10
205900991104     C  N10              OPEN      CNASS02L                             10
206000991104     C   10              GOTO      FINE
206100991102     C* Richiamo XPARUT
206200940831     C                   Z-ADD     1             CODUT
206300940831     C                   CALL      'X§PARUT'
206400940831     C                   PARM                    UTEDSE
206500940831     C*          CODUT     CABEQ-1        FINE
206600940831     C                   MOVEL     RAGUT         RSUT             20
206700940831     C                   MOVEL     RAGUT         RSUT2            25
206800941103     C*§§
206900941103     C                   TIME                    WHHUDT           14 0
207000991102     C                   MOVE      WHHUDT        G02DAT
207100991102     C                   MOVE      *ZEROS        G02INV
207200991102     C                   MOVEL     *BLANK        G02ERR
207300941103     C                   CALL      'XSRDA8'
207400991103     C                   PARM                    WLBDAT
207500991102     C                   Z-ADD     G02INV        AMGUD8            8 0
207600991102     C                   Z-ADD     G02DAT        GMAUD8            8 0
207700991103     C                   Z-ADD     0             REGSWD            5 0          *AZZER.RIGA
207800940831     C*-----
207900940831     C                   SETON                                        05        COND.INTESTAZ.
208000940831     C*-                                                   *
208100940831     C                   MOVEL     REC80         CNCR80
208200991103     C                   MOVEL     REC40         CNCR40
208300940831     C*-                                                   *
208400940831     C*§ CONTROLLO SE SIAMO IN FILIALE
208500020503     C*                  MOVEL     KNMUS         §SEI              6
208600020503     C*    §PRF          COMP      'REM'                                  01
208700020503     C*  01              MOVE      §SEI          §FLE
208800020503     C* N01              DO
208900020503     c     simtpp        ifeq      '2'
209000020503     c     simtpp        oreq      *blanks
209100020503     c                   move      simpou        §fle
209200020503     c                   else
209300991102     C                   MOVE      SIMFEL        §FLE
209400940831     C                   END
209500940831     C*§  GESTITE LE BANCHE
209600940831     C                   CALL      'XKCDUS'                                     *
209700940831     C                   PARM                    KNMUS                          *
209800940831     C                   PARM                    KCDUS             3 0          *
209900940831     C*-                                                   *
210000940831     C*--- RICERCA CAPOCONTI------------------------------------------*
210100940831     C                   DO        50            X                 2 0
210200940831     C                   MOVE      TCU(X)        TCUDS
210300940831     C     F56           CABNE     'CG'          END1
210400940831     C     F4            IFEQ      '6'
210500940831     C                   Z-ADD     KCU(X)        KBNA              4 0
210600940831     C                   END
210700940831     C     F4            IFEQ      '4'
210800940831     C                   Z-ADD     KCU(X)        KCAS              4 0
210900940831     C                   END
211000940831     C     END1          TAG
211100940831     C                   END
211200940831     C*---------------------------------------------------------------*
211300940831     C*--- RICERCA RAG.SOCIALE CLIENTE     'CNACO00F       *
211400940831     C     KEYACO        KLIST                                                  *
211500940831     C                   KFLD                    CODUT                          *
211600940831     C                   KFLD                    KCC                            *
211700940831     C                   KFLD                    KSC                            *
211800940831     C                   Z-ADD     KBNA          KCC                            *
211900940831     C     KBAN          KLIST                                                  *
212000940831     C                   KFLD                    CODUT                          *
212100940831     C                   KFLD                    KBNA                           *
212200940831     C                   KFLD                    KSC
212300940831     C     KCP           KLIST
212400940831     C                   KFLD                    CODUT
212500940831     C                   KFLD                    VIDCCP
212600940831     C                   KFLD                    VIDSCP
212700940831     C     KTAB          KLIST
212800940831     C                   KFLD                    CODUT
212900940831     C                   KFLD                    COD
213000940831     C                   KFLD                    KEY
213100940831     C     KEYOPE        KLIST                                                  * CAUSALE TESTA
213200940831     C                   KFLD                    CODUT                          *
213300940831     C                   KFLD                    OPECD1                         *
213400940831     C                   KFLD                    OPECOD                         *
213500991102     C     KTAB1         KLIST
213600940831     C                   KFLD                    CODUT
213700940831     C                   KFLD                    COD
213800991102     C     KTBE          KLIST
213900991102     C                   KFLD                    KCOD1
214000991102     C                   KFLD                    KKEY1
214100940831     C     KABI§         KLIST
214200940831     C                   KFLD                    KABI              5 0
214300940831     C                   KFLD                    KCAB              5 0
214400940831     C     KASS          KLIST
214500940831     C                   KFLD                    §FLE
214600940831     C                   KFLD                    KNRA             10
214700940831     C                   KFLD                    KABI
214800940831     C                   KFLD                    KCAB
214900991103     C     KARM          KLIST
215000991103     C                   KFLD                    KFLE
215100991103     C                   KFLD                    KFPC
215200940831     C*
215300000612     C     KPLIN         KLIST
215400000612     C                   KFLD                    KPOC
215500000612     C                   KFLD                    KPLI
215600000612     C                   KFLD                    KDIV
215700000612     C*
215800940831     C     *LIKE         DEFINE    ACOKCC        KCC
215900940831     C     *LIKE         DEFINE    ACOKSC        KSC
216000991103     C     *LIKE         DEFINE    ARMFLE        KFLE
216100991103     C     *LIKE         DEFINE    ARMFPC        KFPC
216200940831     C     *LIKE         DEFINE    TBLCOD        COD
216300940831     C     *LIKE         DEFINE    TBLKEY        KEY
216400991102     C     *LIKE         DEFINE    TBECOD        KCOD1
216500991102     C     *LIKE         DEFINE    TBEKE1        KKEY1
216600940831     C     *LIKE         DEFINE    TOTCON        §TCONT
216700940831     C     *LIKE         DEFINE    INDABI        ISTABI
216800940831     C     *LIKE         DEFINE    INDCAB        ISTCAB
216900991120     C* REPERISCO DATA DEL GIORNO
217000991120     C                   TIME                    WHHUDT           14 0
217100991120     C                   MOVEL     WHHUDT        ORA               6 0
217200991120     C                   MOVE      WHHUDT        G02DAT
217300991120     C                   MOVE      *ZEROS        G02INV
217400991120     C                   MOVEL     *BLANK        G02ERR
217500991120     C                   CALL      'XSRDA8'
217600991120     C                   PARM                    WLBDAT
217700991120     C                   Z-ADD     G02INV        OGGI              8 0
217800991122     C                   Z-ADD     G02DAT        WTODAY            8 0
217900980130     C* Richiamo pgm per controllo modificabilità tabella
218000980130     C                   CLEAR                   TRUL12
218100980130     C                   MOVEL     'L'           I12TLA
218200980130     C                   MOVEL     KNSIF         I12NSI
218300980130     C                   MOVEL     '4D'          I12COD
218400980130     C                   CALL      'TRUL12R'
218500980130     C                   PARM                    TRUL12
218600980130     C     O12ERR        IFEQ      '1'
218700980130     C                   GOTO      FINE
218800980130     C                   END
218900980130     C*----------------------------------------------------*
219000940831     C                   MOVE      *ZEROS        AMGDRG
219100940831     C                   EXSR      CARTAB
219200940831     C                   EXSR      CARTB
219300991102     C   80              MOVEL     '1'           *INKC
219400991209     C                   MOVEL     §GEDCN        V1CDIV
219500991102     C*
219600991103     C* 2 GG PRIMA LA DATA DEL GIORNO CASSA SEGNALO
219700991104     C                   Z-ADD     0             NRMOV             7 0
219800000612     C                   Z-ADD     0             NRPLI             7 0
219900991103     C                   Z-ADD     0             D52DFI                         *              CRC90
220000991103     C                   Z-ADD     1             D52GIO                         *              CRC90
220100991103     C                   MOVE      AMGUD8        D52DIN                         *              CRC90
220200991103     C                   MOVE      '-'           D52SGN                         *              CRC90
220300991103     C                   CALL      'TRUL52R'
220400991103     C                   PARM                    TRUL52
220500991103     C                   MOVE      D52DFI        DAT1GG            8 0          *              CRC90
220600940831     C                   ENDSR
220700991103     C*---------------------------------------------------------------*
220800000000     ODISBAN1   E            TESTA            02
220900891205     O                       RSUT2
221000000000     O                                           60 '**---------------------'
221100000000     O                                              '------------**'
221200000000     O          E            TESTA            03
221300891205     O                       SIMVIA
221400930225     O                                           60 '**  DISTINTA  VERSAMENT'
221500930225     O                                              'O IN BANCA  **'
221600000000     O                       PAGE          Z    132
221700000000     O                                          128 'PAG.'
221800991103     O                                          120 'CNC0W9R'
221900000000     O          E            TESTA            04
222000891205     O                       SIMCIT
222100000000     O                                           60 '**---------------------'
222200000000     O                                              '------------**'
222300000000     O          E            TESTA            05
222400000000     O                                              'Versamento a mezzo Banca'
222500000000     O                       KCC           Z   +  1
222600910726     O                       VBAN          Z   +  1
222700000000     O                       VIDBAN            +  1
222800891205     O                                         +  1 'C/C'
222900891205     O                       INDCCB            +  1
223000991103     O                                         +  1 'Divisa Versamento:'
223100991103     O                       V1CDIV            +  1
223200000000     O          E            TESTA            06
223300000000     O                                              'Distinta N.'
223400000000     O                       VIDNDS        Z   +  1
223500000000     O                                         +  1 'del'
223600991103     O                       VIDDDS            +  1 '  /  /    '
223700000000     O          E            TESTA            08
223800000000     O                       SC(1)               66
223900000000     O                       SC(2)              132
224000000000     O          E            TESTA            09
224100000000     O                       SC(3)               66
224200000000     O                       SC(4)              132
224300000000     O          E            TESTA            10
224400000000     O                       SC(5)               66
224500000000     O                       SC(6)              132
224600000000     O          E            TESTA            11 12
224700000000     O                       SC(7)               66
224800000000     O                       SC(8)              132
224900000000     O          E            DET         1
225000910801     O                       VTPA                 3
225100910719     O                       VTPP                 4
225200910719     O                       VNRA                13
225300910719     O                       VNRA1               18
225400991105     O                       GMADAS              29 '  /  /    '
225500991104     O                       VDES                50
225600991104     O                       VPIA                62
225700991104     O               22      VASS          2     77
225800991104     O               22 02                       77 '   '
225900991104     O              N22      VASS          2     92
226000991104     O              N22 02                       92 '   '
226100991104     O               53 54                      102 'STESSO'
226200991104     O              N53 54                      102 'ALTRO '
226300000000     O          E            TOT         2
226400000000     O                       TOTPIA        2     77
226500991104     O                  03                       77 '   '
226600000000     O                       TOTFUO        2     92
226700991104     O                  04                       92 '   '
226800000000     O                                         +  1 '- Totale Assegni -'
226900000000     O                       TOTASS        2   +  1
227000991104     O                  06                      132 '   '
227100000000     O          E            TOT         0
227200000000     O                       TOTPIA        2     77
227300991104     O                  03                       77 '   '
227400000000     O                       TOTFUO        2     92
227500991104     O                  04                       92 '   '
227600000000     O                                         +  1 '- Totale Assegni -'
227700991104     O                       TOTASS        2    132
227800991104     O                  06                      132 '   '
227900000000     O          E            TOT         0  2
228000000000     O                       TOTPIA        2     77
228100991104     O                  03                       77 '   '
228200000000     O                       TOTFUO        2     92
228300991104     O                  04                       92 '   '
228400000000     O                                         +  1 '- Totale Assegni -'
228500991104     O                       TOTASS        2    132
228600991104     O                  06                      132 '   '
228700000000     O          E            TOT1        1
228800000000     O                       CSP(A)               3
228900000000     O                                         +  1 '->'
229000000000     O                       DSP(A)            +  1
229100991105     O               53                          48 'STESSO ISTITUTO'
229200991105     O              N53                          48 'ALTRO  ISTITUTO'
229300991104     O               53      IMP(A)        2     68
229400991104     O              N53      IM1(A)        2     68
229500991104     O                  07                       68 '   '
229600991104     O                       V1CDIV              72
229700900518     O          E            DETSFL         1
229800900518     O                                         +  1 'Nr.'
229900900518     O                       VNRTGL        Z   +  1
230000900518     O                       VDESCR            +  1
230100991104     O                       VIDIM         2     68
230200991104     O                  08                       68 '   '
230300900518     O          E            TOT2        1  2
230400000000     O                                         +  1 'Totale Contante'
230500991104     O                       TOTCON        2     68
230600991104     O                  09                       68 '   '
230700000000     O          E            TOT3        1
230800000000     O                                              '------------------------'
230900000000     O                                              '------------------------'
231000930927     O                                              '------------------------'
231100000000     O          E            TOT3        1
231200000000     O                                         +  1 'Totale Distinta'
231300991104     O                       TOTDIS        2     68
231400991104     O                  02                       68 '   '
231500991104     O                       V1CDIV              72
231600991104     O                  03                       68 '   '
231700000000     O          E            TOT3        2
231800000000     O                                         +  1 'Importo da Versare'
231900991104     O                       TOTVER        2     68
232000991104     O                  04                       68 '   '
232100991104     O                       V1CDIV              72
232200000000     O          E            TOT3        0
232300000000     O                                         +  1 'Importo da Versare'
232400991104     O                       TOTVER        2     68
232500991104     O                  04                       68 '   '
232600991104     O                       V1CDIV              72
232700000000     O          E            TOT3        0
232800000000     O                                         +  1 'Importo da Versare'
232900991104     O                       TOTVER        2     68
233000991104     O                  04                       68 '   '
233100991104     O                       V1CDIV              72
233200000000     O          E            TOT3             65
233300000000     O                                          130 'COPIA USO BANCA'
233400000000     ODISBAN2   E            TESTA            02
233500891205     O                       RSUT2
233600000000     O                                           60 '**---------------------'
233700000000     O                                              '------------**'
233800000000     O          E            TESTA            03
233900891205     O                       SIMVIA
234000930225     O                                           60 '**  DISTINTA  VERSAMENT'
234100930225     O                                              'O IN BANCA  **'
234200000000     O                       PAGE2         Z    132
234300000000     O                                          128 'PAG.'
234400991103     O                                          120 'CNC0W9R'
234500000000     O          E            TESTA            04
234600891205     O                       SIMCIT
234700000000     O                                           60 '**---------------------'
234800000000     O                                              '------------**'
234900000000     O          E            TESTA            05
235000000000     O                                              'Versamento a mezzo Banca'
235100000000     O                       KCC           Z   +  1
235200910726     O                       VBAN          Z   +  1
235300000000     O                       VIDBAN            +  1
235400891205     O                                         +  1 'C/C'
235500891205     O                       INDCCB            +  1
235600991103     O                                         +  1 'Divisa Versamento:'
235700991103     O                       V1CDIV            +  1
235800000000     O          E            TESTA            06
235900000000     O                                              'Distinta N.'
236000000000     O                       VIDNDS        Z   +  1
236100000000     O                                         +  1 'del'
236200991103     O                       VIDDDS            +  1 '  /  /    '
236300000000     O          E            TESTA            08
236400000000     O                       SC(1)               66
236500000000     O                       SC(2)              132
236600000000     O          E            TESTA            09
236700000000     O                       SC(3)               66
236800000000     O                       SC(4)              132
236900000000     O          E            TESTA            10
237000000000     O                       SC(5)               66
237100000000     O                       SC(6)              132
237200000000     O          E            TESTA            11 12
237300000000     O                       SC(7)               66
237400000000     O                       SC(8)              132
237500000000     O          E            DET         1
237600910801     O                       VTPA                 3
237700910719     O                       VTPP                 4
237800910719     O                       VNRA                13
237900910719     O                       VNRA1               18
238000991105     O                       GMADAS              29 '  /  /    '
238100991104     O                       VDES                50
238200991104     O                       VPIA                62
238300991104     O               22      VASS          2     77
238400991104     O               22 02                       77 '   '
238500991104     O              N22      VASS          2     92
238600991104     O              N22 02                       92 '   '
238700991104     O               53 54                      102 'STESSO'
238800991104     O              N53 54                      102 'ALTRO '
238900930927     O                       VNOT               132
239000000000     O          E            TOT         2
239100000000     O                       TOTPIA        2     77
239200991104     O                  03                       77 '   '
239300000000     O                       TOTFUO        2     92
239400991104     O                  04                       92 '   '
239500000000     O                                         +  1 '- Totale Assegni -'
239600991104     O                       TOTASS        2    132
239700991104     O                  06                      132 '   '
239800000000     O          E            TOT         0
239900000000     O                       TOTPIA        2     77
240000991104     O                  03                       77 '   '
240100000000     O                       TOTFUO        2     92
240200991104     O                  04                       92 '   '
240300000000     O                                         +  1 '- Totale Assegni -'
240400991104     O                       TOTASS        2    132
240500991104     O                  06                      132 '   '
240600000000     O          E            TOT         0  2
240700991104     O                       TOTPIA        2     77
240800991104     O                  03                       77 '   '
240900000000     O                       TOTFUO        2     92
241000991104     O                  04                       92 '   '
241100000000     O                                         +  1 '- Totale Assegni -'
241200991104     O                       TOTASS        2    132
241300991104     O                  06                      132 '   '
241400000000     O          E            TOT1        1
241500000000     O                       CSP(A)               3
241600000000     O                                         +  1 '->'
241700930927     O                       DSP(A)            +  1
241800991105     O               53                          48 'STESSO ISTITUTO'
241900991105     O              N53                          48 'ALTRO  ISTITUTO'
242000991104     O               53      IMP(A)        2     68
242100991104     O              N53      IM1(A)        2     68
242200991104     O                  07                       68 '   '
242300991105     O                       V1CDIV              72
242400900518     O          E            DETSFL         1
242500900518     O                                         +  1 'Nr.'
242600900518     O                       VNRTGL        Z   +  1
242700900518     O                       VDESCR            +  1
242800991104     O                       VIDIM         2     68
242900991104     O                  08                       68 '   '
243000900518     O          E            TOT2        1  2
243100000000     O                                         +  1 'Totale Contante'
243200991104     O                       TOTCON        2     68
243300991104     O                  09                       68 '   '
243400991105     O                       V1CDIV              72
243500000000     O          E            TOT3        1
243600000000     O                                              '------------------------'
243700000000     O                                              '------------------------'
243800930927     O                                              '------------------------'
243900000000     O          E            TOT3        1
244000000000     O                                         +  1 'Totale Distinta'
244100991104     O                       TOTDIS        2     68
244200991104     O                  02                       68 '   '
244300991105     O                       V1CDIV              72
244400000000     O          E            TOT3        2
244500000000     O                                         +  1 'Importo da Versare'
244600991104     O                       TOTVER        2     68
244700991104     O                  04                       68 '   '
244800991105     O                       V1CDIV              72
244900000000     O          E            TOT3        0
245000000000     O                                         +  1 'Importo da Versare'
245100991104     O                       TOTVER        2     68
245200991104     O                  04                       68 '   '
245300991105     O                       V1CDIV              72
245400000000     O          E            TOT3        0
245500000000     O                                         +  1 'Importo da Versare'
245600991104     O                       TOTVER        2     68
245700991104     O                  04                       68 '   '
245800991105     O                       V1CDIV              72
245900901011     O          E            TOT3        3
246000901011     O                                         +  1 'N.Registrazione'
246100901011     O                       VIDNRG        Z   +  1
246200901011     O                                         +  1 'del'
246300991103     O                       VIDDRG            +  1 '  /  /    '
246400930225     O                                         +  2 'Operaz. del'
246500991118     O                       VDTOPE            +  1 '  /  /    '
246600000000     O          E            TOT3             65
246700901011     O                                          130 'COPIA USO CASSA'
246800000000     ODISBAN3   E            TESTA            02
246900891205     O                       RSUT2
247000000000     O                                           60 '**---------------------'
247100000000     O                                              '------------**'
247200000000     O          E            TESTA            03
247300891205     O                       SIMVIA
247400930225     O                                           60 '**  DISTINTA  VERSAMENT'
247500930225     O                                              'O IN BANCA  **'
247600000000     O                       PAGE3         Z    132
247700000000     O                                          128 'PAG.'
247800991103     O                                          117 'CNC0W9R'
247900000000     O          E            TESTA            04
248000891205     O                       SIMCIT
248100000000     O                                           60 '**---------------------'
248200000000     O                                              '------------**'
248300000000     O          E            TESTA            05
248400000000     O                                              'Versamento a mezzo Banca'
248500000000     O                       KCC           Z   +  1
248600910726     O                       VBAN          Z   +  1
248700000000     O                       VIDBAN            +  1
248800891205     O                                         +  1 'C/C'
248900891205     O                       INDCCB            +  1
249000991104     O                                         +  1 'Divisa Versamento:'
249100991104     O                       V1CDIV            +  1
249200000000     O          E            TESTA            06
249300000000     O                                              'Distinta N.'
249400000000     O                       VIDNDS        Z   +  1
249500000000     O                                         +  1 'del'
249600991103     O                       VIDDDS            +  1 '  /  /    '
249700000000     O          E            TESTA            08
249800000000     O                       SC(1)               66
249900000000     O                       SC(2)              132
250000000000     O          E            TESTA            09
250100000000     O                       SC(3)               66
250200000000     O                       SC(4)              132
250300000000     O          E            TESTA            10
250400000000     O                       SC(5)               66
250500000000     O                       SC(6)              132
250600000000     O          E            TESTA            11 12
250700000000     O                       SC(7)               66
250800000000     O                       SC(8)              132
250900000000     O          E            DET         1
251000910801     O                       VTPA                 3
251100910719     O                       VTPP                 4
251200910719     O                       VNRA                13
251300910719     O                       VNRA1               18
251400991105     O                       GMADAS              29 '  /  /    '
251500991104     O                       VDES                50
251600991104     O                       VPIA                62
251700991104     O               22      VASS          2     77
251800991104     O               22 02                       77 '   '
251900991104     O              N22      VASS          2     92
252000991104     O              N22 02                       92 '   '
252100991104     O               53 54                      102 'STESSO'
252200991104     O              N53 54                      102 'ALTRO '
252300930927     O                       VNOT               132
252400000000     O          E            TOT         2
252500000000     O                       TOTPIA        2     77
252600991104     O                  03                       77 '   '
252700000000     O                       TOTFUO        2     92
252800991104     O                  04                       92 '   '
252900911127     O                                         +  1 '- Totale Assegni -'
253000991104     O                       TOTASS        2    132
253100991104     O                  06                      132 '   '
253200000000     O          E            TOT         0
253300000000     O                       TOTPIA        2     77
253400991104     O                  03                       77 '   '
253500000000     O                       TOTFUO        2     92
253600991104     O                  04                       92 '   '
253700000000     O                                         +  1 '- Totale Assegni -'
253800991104     O                       TOTASS        2    132
253900991104     O                  06                      132 '   '
254000000000     O          E            TOT         0  2
254100000000     O                       TOTPIA        2     77
254200991104     O                  03                       77 '   '
254300000000     O                       TOTFUO        2     92
254400991104     O                  04                       92 '   '
254500000000     O                                         +  1 '- Totale Assegni -'
254600991104     O                       TOTASS        2    132
254700991104     O                  06                      132 '   '
254800000000     O          E            TOT1        1
254900000000     O                       CSP(A)               3
255000000000     O                                         +  1 '->'
255100000000     O                       DSP(A)            +  1
255200991105     O               53                          48 'STESSO ISTITUTO'
255300991105     O              N53                          48 'ALTRO  ISTITUTO'
255400991104     O               53      IMP(A)        2     68
255500991104     O              N53      IM1(A)        2     68
255600991104     O                  07                       68 '   '
255700991104     O                       V1CDIV              72
255800900518     O          E            DETSFL         1
255900900518     O                                         +  1 'Nr.'
256000900518     O                       VNRTGL        Z   +  1
256100900518     O                       VDESCR            +  1
256200991104     O                       VIDIM         2     68
256300991104     O                  08                       68 '   '
256400900518     O          E            TOT2        1  2
256500000000     O                                         +  1 'Totale Contante'
256600991104     O                       TOTCON        2     68
256700991104     O                  09                       68 '   '
256800991104     O                       V1CDIV              72
256900000000     O          E            TOT3        1
257000000000     O                                              '------------------------'
257100000000     O                                              '------------------------'
257200930927     O                                              '------------------------'
257300000000     O          E            TOT3        1
257400000000     O                                         +  1 'Totale Distinta'
257500991104     O                       TOTDIS        2     68
257600991104     O                  02                       68 '   '
257700991105     O                       V1CDIV              72
257800000000     O          E            TOT3        2
257900000000     O                                         +  1 'Importo da Versare'
258000991104     O                       TOTVER        2     68
258100991104     O                  04                       68 '   '
258200991104     O                       V1CDIV              72
258300000000     O          E            TOT3        0
258400000000     O                                         +  1 'Importo da Versare'
258500991104     O                       TOTVER        2     68
258600991104     O                  04                       68 '   '
258700991104     O                       V1CDIV              72
258800000000     O          E            TOT3        0
258900000000     O                                         +  1 'Importo da Versare'
259000991104     O                       TOTVER        2     68
259100991104     O                  04                       68 '   '
259200991104     O                       V1CDIV              72
259300000000     O          E            TOT3        3
259400000000     O                                         +  1 'N.Registrazione'
259500000000     O                       VIDNRG        Z   +  1
259600000000     O                                         +  1 'del'
259700991103     O                       VIDDRG            +  1 '  /  /    '
259800930225     O                                         +  2 'Operaz. del'
259900991118     O                       VDTOPE            +  1 '  /  /    '
260000000000     O          E            TOT3             65
260100000000     O                                          130 'COPIA USO INTERNO'
260200991103     C*---------------------------------------------------------------*
260300991103     OTABEL     E            RILTAB
260400991103     C*---------------------------------------------------------------*
260500991103     OFIARM000  E            AGGVIL
260600991103     O                       ARMVIL
260700991103     OFIARM000  E            AGGVIE
260800991103     O                       ARMVIE
260900991104**
261000991104------+-----------+----------+--------------------+-----------+---
261100930927-----------+--------------+----------+---------------------------+
261200000000SPECIE    NUMERO     DATA    BANCA TRASSATA        PIAZZA       IM
261300930927PORTO ASS.   IMPORTO ASS.   ISTITUTO   N O T E                   '
261400000000          ASSEGNO  ASSEGNO                         PAGAMENTO    SU
261500930927 PIAZZA      FUORI PIAZZA                                        '
261600991105------+-----------+----------+--------------------+-----------+---
261700930927-----------+--------------+----------+---------------------------+
261800951027** SCHIERA ERRORI
261900991102Già inserito assegno. Eseguire chius.operatore/presa in carico mandato d'introto    01
262000991102Già inserito assegno in questa distinta.                                            02
262100991102Già inserito assegno con questi riferimenti intestato al mittente.                  03
262200991102ERRORE !! Manca il numeratore della distinta !!!!                                   04
262300991102Divisa di versamento non valida                                                     05
262400991103Precedente trasmissione di  movimenti contabili terminata in modo anomalo           06
262500000609Impostare un nr.PLICO corretto                                                      07
262600000612Con questo n°Plico sono stati già inseriti altri dati! Usare un altro Numero.       08
262700991104**
262800991104CHKOBJ OBJ(FNMOV00T) OBJTYPE(*FILE) MBR(M000)
262900991104ADDPFM FILE(FNMOV00T)               MBR(M000)
263000991104OVRDBF FILE(FNMOV00T)               MBR(M000)
263100991104ALCOBJ OBJ((FNMOV00T *FILE *EXCL        M000))
263200991104DLCOBJ OBJ((FNMOV00T *FILE *EXCL        M000))
