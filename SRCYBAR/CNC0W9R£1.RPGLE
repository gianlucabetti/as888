000100060414     H DFTACTGRP(*NO) ACTGRP('QILE') BNDDIR('PJXBND':'TRULCIF')
000200930222     H DECEDIT('0,') DATEDIT(*DMY.)
000300020926     H OPTION(*NODEBUGIO)
000400991102     H* CNC0W9R *----------------------------------------------------*
000500930222     H*---------*                                                    *
000600930222     H*     -  DISTINTA DI VERSAMENTO IN BANCA  -                    *
000700940224?     *--------------------------------------------------------------*
000800991104     FCNASS02L  UF A E           K DISK    USROPN
000900991104     F                                     COMMIT
001000060411     Fyfcas01L  UF   E           K DISK
001100060411     F                                     COMMIT
001200991102     F*----------
001300930222     FCNACO00F  IF   E           K DISK
001400991102     F*----------
001500930222     FCNABI00F  IF   E           K DISK
001600991102     F*----------
001700930222     FCNIND00F  IF   E           K DISK
001800991102     F*----------
001900930222     FTABEL00F  UF   E           K DISK
002000991102     F*----------
002100991102     FTNTBE01L  IF   E           K DISK
002200991102     F*----------
002300930222     FCNOPE00F  IF   E           K DISK
002400991102     F*----------
002500991102     FYNTGD01L  IF   E           K DISK
002600021118     F*----------
002700030320     Ffnvrt00f  o    E             DISK    commit
002800991103     F*----------
002900991104     FFNMOV00T  O  A E             DISK    USROPN
003000991104     F                                     COMMIT
003100991102     F*----------
003200050405     F*****FNPLI00F  O  A E             DISK
003300050405     F*****                                     COMMIT
003400050405     ******FFNPLI01L  IF   E           K DISK
003500050405     F*****                                     RENAME(FNPLI000:FNPLI001)
003600000609     F*----------
003700060412     Fyfqig01L  IF   E           K DISK
003800040921     FAZORG01L  IF   E           K DISK
003900021120     F*----------
004000930222     FDISBAN1   O    F  132        PRINTER
004100930222     FDISBAN2   O    F  132        PRINTER
004200930222     FDISBAN3   O    F  132        PRINTER
004300991102     F*----------
004400991102     FCNC0W9D   CF   E             WORKSTN
004500991102     F                                     SFILE(CN0W9S2:NR2)
004600991102     F                                     SFILE(CN0W9S4:NR4)
004700991102     F                                     SFILE(CN0W9S5:NR5)
004800991102?     *--------------------------------------------------------------*
004900991102     D SC              S             66    DIM(8) CTDATA PERRCD(1)
005000930927     D CSP             S              2    DIM(20)                              COD.SPECIE
005100000000     D DSP             S             25    DIM(20)                              DES.SPECIE
005200891201     D CAU             S              4    DIM(20)                              CAUSALE CO
005300930927     D CA1             S              4    DIM(20)                              CAUS.CONTA
005400991104     D IMP             S             15  2 DIM(20)                              IMPORTI PE
005500991104     D IM1             S             15  2 DIM(20)                              IMPORTI PE
005600020507     D FILA            S              3  0 DIM(30)                              FIL TBLUNI
005700891205     D OPZ             S              1    DIM(10)                              FLAGS ANAG
005800910722     D SKI             S              1    DIM(3)                               SKIERA DI
005900060403     D ERR             S             80    DIM(9) CTDATA PERRCD(1)              SCHIERA ER
006000020926     D CMDD            S             49    DIM(5) CTDATA PERRCD(1)              CMD Exec
006001150112     D lista           S            100    DIM(10)                              FIL TBLUNI
006002150112     D vuoto           S            100
006003150108     D xy              S              2  0
006004150108      *
006100000000     D DTAVER          DS
006200000000     D  NUMVER                 1      6  0
006300060403     D DGEI          E DS
006400991104     D DSCMZ         E DS
006500061120     D CNC0WD00S     E DS
006600000000     D TCUDS           DS
006700000000     D  F1                     1      1
006800000000     D  F3                     3      3
006900000000     D  F2                     2      2
007000000000     D  F4                     4      4
007100000000     D  F56                    5      6
007200991103     D YEURDS        E DS                  EXTNAME(YEURCODS)
007300991103     D TRUL52        E DS                  EXTNAME(TRUL52DS)
007400991103     D TRUL12        E DS                  EXTNAME(TRUL12DS)
007500910718     D*-
007600991103     D WLBDAT          DS
007700991102     D  G02DAT                 1      8  0
007800991102     D  G02INV                 9     16  0
007900991102     D  G02ERR                17     17
008000991102     D  G02TGI                18     22  0
008100941104      *
008200941104     D                SDS
008300941104     D  §PGM                   1     10
008400891205?    D*----------------------------------------------------*
008500021118     D ddatiute      e ds
008600021118     D azuteds       e ds                  extname(AZUTE00F)
008700021118     D tibs34ds      E DS                  inz
008800991103     D CNCR40        E DS
008900020507     D TR06DS        E DS                  EXTNAME(TRUL06DS)
009000020507     D  LIN                    1     90  0 dim(30)
009100000000     D KPJBA         E DS
009200000000     D  LIBSYS                92    100
009300000000     D  MSGERR               453    502
009400900403     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
009500000000     D  TCU                  398    697
009600000000     D                                     DIM(50)                              TAB FLAG C
009700000000     D  KCU                  698    847P 0
009800000000     D                                     DIM(50)                              TAB CAPO C
009900000000     D                                     PACKEVEN
010000020926     D TIBS55        E DS                  EXTNAME(TIBS55DS)
010100020729     D Dy4O          E DS
010200991118     D DS1A          E DS
010300910719     D DS4E          E DS
010400991102     D DSCV          E DS
010500011016     D DSCW          E DS
010600991102     D DGED          E DS
010700000621     D SPICCI          C                   CONST('Spiccioli')
010800011016     D*
010900060403     D §abb            S                   like(vass)
011000060411     D dtaiso          S               d
011001150109     D dataeur         S               d   datfmt(*eur)
011002150108     D dataiso         S               d   datfmt(*iso)
011003150108     D dataisom        S               d   datfmt(*iso)
011100060411     D PrmTpInc        S              2
011200011016     D PrmDivisa       S              3
011300011016     D PrmData         S               D
011400011016     D PrmEsito        S              1
011500011016     D PrmDS1A         S
011600011016     D                                     LIKE(DS1A)
011700011016     D PrmDSCW         S
011800011016     D                                     LIKE(DSCW)
011900020417     D WrkLstRrnS4     S              5I 0
012000020422     D WrkDtOpe        S               D
012100020422     D                                     DATFMT(*EUR)
012200020422     D WrkDtCur        S               D
012300020422     D WrkMonth        S              5I 0
012400060414     D chkDATAASSEGNO  PR            10I 0
012500060414     D  dte                          10D   DATFMT(*ISO)
012600060414     D  SAS
012700060414     D                                     LIKE(§1ASAS)
012800060414     D  esito                        10I 0
012900060414     D                                     OPTIONS(*OMIT)
013000060414     D  errc                          1A
013100060414     D                                     DIM(10)
013200060414     D  msgc                         80A   OPTIONS(*OMIT)
013300060414     D                                     DIM(10)
013400060414     D dte             S               d
013500060414     D sas             S                   like(§1asas)
013600060414     D esito           S             10I 0
013700060414     D errc            S              1    DIM(10)                              SKI CHIAVI
013800060414     D msgc            S             80    DIM(10)                              SKI CHIAVI
013900130409     D stanrt          S                   like(vnrman)
014000060824     D okcon           S              1    inz
014100100521     d* ordinamento
014200100521     d NotUsed         S             16A
014300100521     d ReturnSize      S              9B 0
014400100521     d SizeList        S              9B 0
014500100521     d WrkSort         S              1
014501150108     d wrkEml          s            253
014502150108     d wrkEmcc         s            253
014503150108     d wrkMsg          s           5000
014504150108     d wrkOgg          s             44
014505150108     d wrkesit         s              1
014506150108     d
014600100521     d*---
014700100521     d nr2             S              3  0                                      *n° rel sf
014800100521     d stmrec1         S              3  0                                      *n° rel sf
014900100521     d*---
015000100521     d MaxKey          C                   2
015100100521     d Ascendente      C                   1
015200100521     d Discendente     C                   2
015300100521     d Carattere       C                   6
015400100521     d Put             C                   1
015500100521     d EndPut          C                   2
015600100521     d Get             C                   3
015700100521     d Numerico        C                   8
015800100521     d*---
015900100521     d SflRcd          DS
016000100521     d  vpad
016100100521     d  SAVABI
016200100521     d  SAVCAB
016300100521     d  SAVNRA
016400100521     d  SAVNR1
016500100521     d  VANN
016600100521     d  VNRA
016700100521     d  VNRA1
016800100521     d  VDTE
016900100521     d  VASS
017000100521     d  VASSH
017100100521     d  VTPA
017200100521     d  VTPP
017300100521     d  VAbi
017400100521     d  Vcab
017500100521     d  Vdes
017600100521     d  Vtpi
017700100521     d  Vaas
017800100521     d  Vnrs
017900100521     d  Vnsp
018000100521     d  Vpia
018100100521     d  Vnot
018200100521     d  Selected                      1A
018300100521
018400100521      /COPY QSYSINC/QRPGLESRC,QLGSORT
018500100521     d QLGKL                         16    DIM(MaxKey)
018600100521     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
018700100521     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
018800100521     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
018900100521     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
019000100521
019100100521      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
019200100521      /COPY QSYSINC/QRPGLESRC,QUSEC
019300020417
019400991102     IYNTGD000
019500991102     I              TGDDIVISA                   TGDDIV
019600991102     I              TGDSEQUENZ                  TGDSEQ
019700991102     I              TGDDESCR                    TGDDES
019800991102     I              TGDVALORE                   TGDVAL
019900991102?    I*----------------------------------------------------*
020000991102     C* LOOP FINO A F3
020100991102?    C*----------------------------------------------------*
020200910722     C     *INKC         DOWEQ     '0'
020300000608      *
020400000608     C* GESTIONE PRIMA VIDEATA
020500000608     C     VID01         TAG
020600000608      *
020700991102     C                   EXSR      GESD01
020800000608      *
020900000608      * SOLO PER IL GIRO "BANCA PER CONTANTE"
021000050405     C**   adesso va fatto sempre anche se non contante quindi
021100050405     C**     via il gioco fatto con l'ind.66 x solo contante
021200050405     C**   66
021300050405     C**ANN03
021400050405     C**
021500050405     C  N03
021600000612     CANN28V1PLIC        IFNE      *ZERO
021700000608     C                   EXSR      WIND01
021800000608     C   KL              GOTO      VID01
021900000608     C                   ENDIF
022000000608      *
022100991102     C* PER NO ERRORI PROSEGUO
022200991103     C  N28
022300991103     CANNKC
022400991103     CANN91
022500991104     CANN92
022600991104     CANN39              DO
022700991104     C* Effettuo Ovrdbf su file movimenti
022800991104     C     §FLE          IFNE      WFIL
022900060426     c                   setoff                                       68
023000060414     C                   EXSR      DLCMOV
023100060414     C                   EXSR      OVRMOV
023200991118     C                   MOVE      §FLE          WFIL              3 0
023300991104     C                   END
023400910718     C* PULIZIA SUBFILE
023500910718     C                   EXSR      PULIZ
023600000717     C  N66              EXSR      CARICA
023700060411     C                   Z-ADD     NR2           SAVNR2            4 0
023800100521     c                   z-add     1             rcdnbr
023900100521     c                   eval      vordi = 'Importo'
024000060403     c* non è + possibile aggiungere assegni SOLO SEDE
024100060414     c                   if        dutlpo = 'S' or dutlpo = ' '
024200060403     C                   EXSR      AGG
024300060403     c                   end
024400060411     C                   z-add     NR2           TREC              4 0
024500060421     c     trec          comp      0                                      66
024600060421     C     *IN12         DOWEQ     '0'
024700910725     C     *IN11         ANDEQ     '0'
024800910717     C     *IN90         OREQ      '1'
024900000608      *
025000000608      * CON IL (66) VISUALIZZO O BYPASSO QUESTO SCHERMO SE EFFETTUA
025100000608      *  IL BYPASS, DEVO ANCHE SIMULARE DI AVER PREMUTO IL TASTO DI
025200000608      *   CONFERMA PER LA VIDEATA SUCCESSIVA DEL CONTANTE.
025300000608     C  N66              WRITE     CN0W9Z2
025400000608     C  N66              EXFMT     CN0W9C2
025500000608     C   66              SETON                                        11
025600910725     C                   SETOFF                                       90
025700100521     C* F11 = Cambia ordinamento
025800100521if  1C                   IF        *INKK
025900100521     C                   EXSR      F11GES
026000100521e   1C                   ENDIF
026100000608      *
026200000608      * SE F14=ON ESCLUSIONE DI TUTTE LE RIGHE CON "E" SU "VANN"
026300000608     C   66
026400000608     COR KN              EXSR      EXCLRD
026500000608      *
026600060421     C  N12              DO
026700910731     C                   SETOFF                                       82
026800910729     C                   EXSR      CONTR
026900910725     C   11              EXSR      CONTR
027000910726     C   11              SETOFF                                       1317
027100910725     C   11
027200910725     CANN90              EXSR      CONF
027300911125     C   69              GOTO      FINE
0274000604213    C                   ENDdo
027500060411     c* se premuto f12 e sono un utente di sede chiedo conferma per
027600060411     c* eventuali assegni inseriti
027700060426     c                   if        *in12 and *in19 and not *in15
027800060411     C     *IN82         COMP      '0'                                121282
027900060411     c                   end
028000060426     c                   if        *in82 and *in19
028100060411     C                   SETOFF                                       12
028200060411     c                   end
028300910801     C   15              SETON                                        12
028400000608      *
028500000608      * PER IL GIRO "BANCA PER CONTANTE" AD F12 DEVE TORNARE AL
028600000608      * PRIMO VIDEO INIZIALE
028700000608     C   17
028800000608     CAN 66              SETON                                        12
028900131125      *
029000131125      *------
029100131125      * Se si tratta di utente di SEDE che aggiorna il SFL ASSEGNI
029200131125      *  voglio che esca dopo aver dato F6.
029300131125     c                   if        dutlpo = 'S' or dutlpo = ' '
029400131125     c                   if        *in06
029500131125     C                   SETON                                        12
029600131125     c                   end
029700131125     c                   end
029800131125      *------
029900000608      *
030000910731     C                   END
030100060403     C                   END
030200910722     C                   END
030300911125     C     FINE          TAG
030400991104     C* Se ho effettuato l'allocazione disalloco FNMOV
030500991104     C     §FLE          IFEQ      WFIL
030600991104     C                   EXSR      DLCMOV
030700991104     C                   END
030800991104     C                   COMMIT
030900921204     C*-                                                   *
031000910719     C                   SETON                                        LR
031100100521      *-----------------------------------------------------------------------------------
031200100521      * F11GES - TASTO FUNZIONE F11 --> Ragione sociale/P.O
031300100521      *-----------------------------------------------------------------------------------
031400100521     C     F11GES        BEGSR
031500100521     C                   setoff                                       78
031600100521      * IMPOSTO ENTRATA AD 'A'
031700100521     C                   EVAL      rcdnbr  = 1
031800100521      *
031900100521     C                   CLEAR                   QLGSCB
032000100521     C                   CLEAR                   QLGSCB00
032100100521
032200100521     ** 1 campo chiave.
032300100521     C                   EVAL      QLGNBRK    = 1
032400100521
032500100521     C                   IF        *in50=*off
032600100521     c                   eval      vordi = 'Importo'
032700100521     ** IL numero ass. è a posizione 2, alfa, 4 byte, ascending.
032800100521     C                   EVAL      QLGSP      = 22
032900100521     C                   EVAL      QLGSS      = %SIZE(vnra)
033000100521     C                   EVAL      QLGDT      = carattere
033100100521     C                   EVAL      QLGSO      = Ascendente
033200100521     C                   EVAL      QLGKL(1)   = QLGSKL
033300100521     C                   seton                                        50
033400100521     C                   ELSE
033500100521      *
033600100521     c                   eval      vordi = 'Assegno'
033700100521     ** L'importo è a posizione 21, 11 byte, num., ascending.
033800100521     C                   EVAL      QLGSP      = 39
033900100521     C                   EVAL      QLGSS      = %SIZE(vass)
034000100521     C                   EVAL      QLGDT      = numerico
034100100521     C                   EVAL      QLGSO      = Ascendente
034200100521     C                   EVAL      QLGKL(1)   = QLGSKL
034300100521     C                   setoff                                       50
034400100521     C                   ENDIF
034500100521
034600100521      * Load other sort parameters.
034700100521     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
034800100521     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
034900100521     C                   EVAL      QLGRT     = 8
035000100521     C                   EVAL      QLGOKL    = 80
035100100521     C                   EVAL      QLGLKE    = 16
035200100521     C                   EVAL      QLGLSS    = 290
035300100521
035400100521      * Initialize Sort I/O API fields.
035500100521     C                   EVAL      QLGRL00  = QLGRL
035600100521     C                   EVAL      QLGRC00  = 1
035700100521     C                   CLEAR                   QUSEI
035800100521     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
035900100521
036000100521      * First step - Initialize the sort routine.
036100100521     C                   CALL      'QLGSORT'
036200100521     C                   PARM                    QLGSCB
036300100521     C                   PARM                    NotUsed
036400100521     C                   PARM                    NotUsed
036500100521     C                   PARM                    SizeList
036600100521     C                   PARM                    ReturnSize
036700100521     C                   PARM                    QUSEC
036800100521
036900100521      * Next step - Write records to I/O routine.
037000100521     C                   EVAL      QLGRT00 = Put
037100100521
037200100521     C                   DO        STMREC1       x                 3 0
037300100521
037400100521     C     x             CHAIN     CN0W9S2
037500100521
037600100521     ** Solo le righe con Selected = 'Y' sono riordinate,
037700100521     ** quindi per fare un ordinamento di tutte le righe
037800100521     ** metto 'Y' sempre.
037900100521     C                   EVAL      Selected = 'Y'
038000100521
038100100521     C                   CLEAR                   QUSEI
038200100521     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
038300100521     C                   CALL      'QLGSRTIO'
038400100521     C                   PARM                    QLGSCB00
038500100521     C                   PARM                    SflRcd
038600100521     C                   PARM                    NotUsed
038700100521     C                   PARM                    SizeList
038800100521     C                   PARM                    NotUsed
038900100521     C                   PARM                    QUSEC
039000100521
039100100521     C                   ENDDO
039200100521
039300100521      * Next step - Signal end of input, clear subfile for reload.
039400100521     C                   EVAL      QLGRT00 = EndPut
039500100521     C                   CLEAR                   QUSEI
039600100521     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
039700100521
039800100521     C                   CALL      'QLGSRTIO'
039900100521     C                   PARM                    QLGSCB00
040000100521     C                   PARM                    SflRcd
040100100521     C                   PARM                    NotUsed
040200100521     C                   PARM                    SizeList
040300100521     C                   PARM                    NotUsed
040400100521     C                   PARM                    QUSEC
040500100521
040600100521     C                   SETON                                            70    *
040700100521     C                   WRITE     CN0W9C2                                      *
040800100521     C                   SETOFF                                       70        *
040900100521     C                   WRITE     CN0W9Z2                                      *
041000100521
041100100521      * Final step - Write the records back to the subfile.
041200100521     C                   EVAL      QLGRT00 = Get
041300100521
041400100521     C                   DO        STMREC1       x
041500100521     C                   CLEAR                   QUSEI
041600100521     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
041700100521     C                   CALL      'QLGSRTIO'
041800100521     C                   PARM                    QLGSCB00
041900100521     C                   PARM                    NotUsed
042000100521     C                   PARM                    SflRcd
042100100521     C                   PARM                    QLGRL00
042200100521     C                   PARM                    NotUsed
042300100521     C                   PARM                    QUSEC
042400100521      *
042500100521     c                   z-add     x             nr2
042600100521     C                   WRITE     CN0W9S2
042700100521     C                   ENDDO
042800100521
042900100521     C                   EVAL      *IN70 = *off
043000100521      *
043100100521     C                   ENDSR
043200991102     C*----------------------------------------------------*
043300991102     C* GESTIONE DELLA PRIMA VIDEATA
043400991102     C*----------------------------------------------------*
043500991102     C     GESD01        BEGSR
043600000609      *
043700000609      * SOLO PER IL GIRO BANCA PER CONTANTE (66=ON)
043800000609     C*  Aggancio tabella 'SEC' x reperire la BANCA
043900000609     C     *IN66         IFEQ      *ON
044000000609     C                   MOVEL     'SEC'         KCOD1
044100000609     C                   MOVEL     *BLANKS       KKEY1
044200000609     C                   MOVEL     'SEC'         KKEY1
044300000609     C     KTBE          CHAIN     TNTBE01L                           13
044400000609     C     *IN13         IFEQ      '0'
044500000609     C                   MOVEL     TBEUNI        BANCA8            8 0
044600060421     C*                  MOVE      BANCA8        VBAN
044700000609     C                   END
044800000609     C                   ENDIF
044900000609      *
045000060421     C                   SETOFF                                       128211
045100991102     C                   SETON                                        73
045200991102     C                   EXFMT     CN0W9D1
045300991104     C                   SETOFF                                       2891
045400060421     C* LEGGO TABELLA 'Y4O'
045500060421     C                   MOVEL     'Y4O'         tbecod
045600060421     C                   MOVEL(p)  '201'         tbeke1
045700060421     C                   MOVEL(p)  §FLE          tbeke2
045800060421     C     KTbe1         CHAIN     Tntbe01l                           39
045900060421     C  N39              EXCEPT    RILTAB
046000060421     C  N39              MOVEL     TBeUNI        Dy4O
046100060421     C   39              SETON                                        28
046200060421     C   39              GOTO      FINvd1
046300021120     c* controllo che non sia già stata fatta la quadratura giornaliera
046400021120     c* se così fosse non lascio fare il versamento
046500060412     c     kqig          chain     yfqig01l
046600060412     c                   if        %found(yfqig01l)
046700021120     c                   seton                                        28
046800021120     C                   MOVEL     ERR(9)        $MSG
046900021120     C                   GOTO      FINVD1
047000021120     c                   end
047100000612     C* Controllo se ci sono dati in FNMOV00F
047200991103     C                   Z-ADD     0             NRRX             10 0
047300991103     C                   MOVEL     'FNMOV00T'    FILE
047400991103     C                   MOVEL     '*LIBL   '    LIB              10
047500020926     C                   EVAL      Mbr = 'M' + %EDITC(§Fle:'X') + '046'
047600991103     C                   CALL      'TRUL51C'
047700991103     C                   PARM                    LIB
047800991103     C                   PARM                    FILE             10
047900020926     C                   PARM                    MBR              10
048000991103     C                   PARM                    NRRX
048100991103     C     NRRX          IFNE      *ZEROS
048200991118     C     NRRX          ANDNE     *ALL'9'
048300991103     C                   SETON                                        28
048400991103     C                   MOVEL     ERR(6)        $MSG
048500991103     C                   GOTO      FINVD1
048600991103     C                   END
048700000609      *--------
048800991102     C     §FLE          IFNE      0
048900060421     c                   if        dutlpo <> ' ' and dutlpo <> 'S'
049000020507     C     §Fle          LOOKUP    FILA                                   38
049100991102     C  N38              SETON                                        39
049200991102     C   38              SETOFF                                       39
049300060421     c                   end
049400991102     C                   ELSE
049500991102     C                   SETON                                        39
049600991102     C                   END
049700991102     C  NKC
049800991102     CANN39              DO
049900991102     C                   Z-ADD     VBAN          KSC
050000991102     C     VBAN          COMP      0                                      92
050100991102     C                   MOVEL     *BLANKS       ACORAG
050200991102     C  N92KEYACO        CHAIN     CNACO000                           92
050300991102     C  N92ACOFLG        COMP      '*'                                    92
050400991102     C  N92ACOABL        COMP      '*'                                    92
050500991102     C                   MOVEL     ACORAG        VIDBAN           30
050600991102     C                   MOVEA     ACOOPZ        OPZ                            *
050700991102     C                   MOVE      OPZ(7)        HOPZ7             1            OBBL.RAPP.
050800991102     C                   MOVE      OPZ(2)        HOPZ2             1            GEST.SCADE
050900991102     C     KEYACO        CHAIN     CNIND000                           21
051000991102     C                   Z-ADD     INDABI        ISTABI
051100991102     C                   Z-ADD     INDCAB        ISTCAB
051200991102     C   21              MOVE      *BLANK        INDCCB
051300991102     C                   MOVEL     ACORAG        VBANC
051400991102     C*
051500991102     C* Verifico se la divisa è corretta
051600011016     C  N92V1CDIV        IFNE      DIVALT
051700011016     C     V1CDIV        ANDNE     §GEDCR
051800011016     C     V1CDIV        OREQ      *BLANKS
051900011016     C                   SETON                                        2891
052000011016     C                   MOVEL     ERR(5)        $MSG
052100011016     C                   END
052200011016     C* Controllo fine validità divisa - tipo incasso.
052300011016     C                   CALL      'X1ACW'
052400011016     C                   PARM      'SC'          PrmTpInc
052500011016     C                   PARM      V1CDiv        PrmDivisa
052600011016     C                   PARM                    PrmData
052700011016     C                   PARM      *BLANK        PrmEsito
052800011016     C     DS1A          PARM      *BLANK        PrmDS1A
052900011016     C     DSCW          PARM      *BLANK        PrmDSCW
053000011016     C                   IF        PrmEsito <> '0'
053100011016     C                   EVAL      *IN28 = *ON
053200011016     C                   EVAL      *IN91 = *ON
053300011016     C                   MOVEL     ERR(5)        $MSG
053400011016     C                   ENDIF
053500011016     C                   END
053600011016     C                   IF        PrmEsito = '0'
053700011016     C                   EVAL      *IN94 = *ON
053800011016     C                   ELSE
053900011016     C                   EVAL      *IN94 = *OFF
054000011016     C                   ENDIF
054100000608      *
054200000608      *  SOLO PER IL GIRO "BANCA PER CONTANTE"
054300000608      *   ESEGUO IL CONTROLLO DEL NR.PLICO OBBLIGATORIO
054400050405     C***  Adesso il controllo di immissione nr.plico lo deve fare sempre
054500050405     C***  *IN66         IFEQ      *ON
054600000608     C     V1PLIC        COMP      0                                      93
054700000608     C   93              SETON                                        28
054800000608     C   93              MOVEL     ERR(7)        $MSG
054900050405     C****               ENDIF
055000000608      *
055100000612      *  CONTROLLO DI ESISTENZA CHIAVE POC/NR.PLICO/DIVISA
055200050405     C***  Tolta gestione dei PLICHI
055300050405     C*******            IF        V1Plic <> 0
055400050405     C*******            MOVE      §FLE          KPOC              3 0
055500050405     C*******            Z-ADD     V1PLIC        KPLI              9 0
055600050405     C*******            MOVEL     V1CDIV        KDIV              3
055700020103      * con l'euro è necessario testare solo senza la divisa
055800020103      * per dare l'errore di Plico già utilizzato
055900050405     C**** KPLIN         CHAIN     FNPLI01L                           29
056000050405     C* N29              SETON                                        2895
056100050405     C* N29              MOVEL     ERR(8)        $MSG
056200050405     C*******            ENDIF
056300050405     C***  Tolta gestione dei PLICHI
056400000612      *
056500991103     C     FINVD1        ENDSR
056600000608     C*----------------------------------------------------*
056700000608     C* IMPOSTA LA WINDOW CONFERMA PER "BANCA X CONTANTE"
056800000608     C*----------------------------------------------------*
056900000608     C     WIND01        BEGSR
057000000608      *
057100000608      * WINDOW DI CONFERMA NR. PLICO
057200000608     C                   Z-ADD     V1PLIC        W1PLIC
057300000608     C                   EXFMT     CN0W9W1
057400000608      *
057500000608     C                   ENDSR
057600000609     C*----------------------------------------------------*
057700000609     C* ESEGUO OVRDBF SU FNMOV
057800000609     C*----------------------------------------------------*
057900000609     C     OVRMOV        BEGSR
058000000609     C*
058100020926     C                   MOVE      '046) '       VAR               8
058200000609     C                   MOVEL     §FLE          VAR
058300020926     C                   Z-ADD     49            LUNG             15 5
058400000609     C** CONTROLLO SE ESISTE IL MEMBRO X FNMOV00T
058500000609     C                   SETOFF                                       20
058600000609     C                   MOVE      VAR           CMDD(1)
058700000609     C                   MOVEL     *BLANKS       COMMAN
058800000609     C                   MOVEA     CMDD(1)       COMMAN           80
058900000609     C                   CALL      'QCMDEXC'                            20
059000000609     C                   PARM                    COMMAN
059100000609     C                   PARM                    LUNG
059200000609     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
059300000609     C   20              DO
059400000609     C                   MOVE      VAR           CMDD(2)
059500000609     C                   MOVEL     *BLANKS       COMMAN
059600000609     C                   MOVEA     CMDD(2)       COMMAN
059700000609     C                   CALL      'QCMDEXC'
059800000609     C                   PARM                    COMMAN
059900000609     C                   PARM                    LUNG
060000000609     C                   END
060100000609     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
060200000609     C                   MOVE      VAR           CMDD(3)
060300000609     C                   MOVEL     *BLANKS       COMMAN
060400000609     C                   MOVEA     CMDD(3)       COMMAN
060500000609     C                   CALL      'QCMDEXC'
060600000609     C                   PARM                    COMMAN
060700000609     C                   PARM                    LUNG
060800000609     C* GUARDO SE è ALLOCATO
060900000609     C                   MOVEL     VAR           VAR
061000000609     C                   MOVE      ')'           VAR
061100000609     C                   MOVE      VAR           CMDD(4)
061200000609     C                   MOVEA     CMDD(4)       COMMAN
061300020926     C                   Z-ADD     49            LUNCMD           15 5
061400000609     C                   CALL      'QCMDEXC'                            91
061500000609     C                   PARM                    COMMAN
061600000609     C                   PARM                    LUNCMD
061700000609     C*
061800000609     C  N91              OPEN      FNMOV00T                             91
061900030318     C   91              MOVEL(p)  'NO'          KPJBU
062000000609     C   91              GOTO      FINE
062100000609     C*
062200000609     C                   ENDSR
062300991104     C*----------------------------------------------------*
062400991104     C* ESEGUO Dlcobj  FNMOV
062500991104     C*----------------------------------------------------*
062600991104     C     DLCMOV        BEGSR
062700991104     C*
062800991104     C* GUARDO SE è ALLOCATO
062900991104     C  N91              CLOSE     FNMOV00T                             91
063000991104     C*
063100991104     C                   MOVE      VAR           CMDD(5)
063200991104     C                   MOVEA     CMDD(5)       COMMAN
063300020926     C                   Z-ADD     49            LUNCMD           15 5
063400991104     C                   CALL      'QCMDEXC'                            91
063500991104     C                   PARM                    COMMAN
063600991104     C                   PARM                    LUNCMD
063700991104     C*
063800991104     C                   ENDSR
063900000609     C*----------------------------------------------------*
064000910717     C* CARICAMENTO SUBFILE CON ASSEGNI VERSABILI
064100991102     C*----------------------------------------------------*
064200910717     C     CARICA        BEGSR
064300911113     C                   SETOFF                                       555152
064400911113     C                   SETOFF                                       49
064500991105     C                   Z-ADD     0             TASS             15 3
064600941103     C     §FLE          SETLL     CNASS02L
064700910717     C     *IN55         DOWEQ     '0'
064800941103     C     §FLE          READE     CNASS02L                               55
064900941103     C  N55ASSFVE        IFEQ      'V'                                          VERSABILI
065000941103     C     ASSTPI        ANDNE     'M'
065100941103     C     ASSNDV        ANDEQ     0                                            NO ALTRE D
065200991102     C     ASSVCA        ANDEQ     V1CDIV                                       SOLO DIV V
065300991102     C*
065400910717     C* LA PRIMA VOLTA SALTO IL CONFRONTO
065500910717     C  N51              DO
065600910717     C                   EXSR      PREC
065700910717     C                   SETON                                        51
065800910717     C                   END
065900910717     C*
066000941103     C     ASSNRA        IFNE      PNRA
066100991102     C     ASSDTE        ORNE      PDTE
066200941103     C     ASSABI        ORNE      PABI
066300951016     C     ASSCAB        ORNE      PCAB
066400910717     C                   EXSR      MUOVI
066500100521     C                   ADD       1             NR2
066600991102     C                   WRITE     CN0W9S2
066700910717     C                   ELSE
066800941103     C                   ADD       ASSCAS        TASS
066900910717     C                   END
067000940831     C                   END
067100910717     C                   END
067200910722     C* CARICO L'ULTIMO ASSEGNO
067300910722     C   51              DO
067400910722     C                   EXSR      MUOVI
067500991103     C                   ADD       1             NR2
067600991102     C                   WRITE     CN0W9S2
067700910722     C                   END
067800100521     C                   Z-ADD     nr2           STMREC1                        *n° rel sf
067900910722     C                   ENDSR
068000991102     C*----------------------------------------------------*
068100991102     C* Sposta  i campi dai dati di salvataggio a video
068200991102     C*----------------------------------------------------*
068300910717     C     MUOVI         BEGSR
068400991102     C*
068500920520     C     PTPD          COMP      'V'                                6565
068600920520     C   65              MOVEL     'P'           VPAD
068700920520     C  N65              MOVEL     *BLANKS       VPAD
068800910719     C                   MOVEL     PNRA          VNRA
068900910719     C                   MOVE      PNRA          VNRA1
069000940126     C                   MOVEL     PNRA          SAVNRA
069100940126     C                   MOVE      PNRA          SAVNR1
069200910717     C                   MOVEL     PTPD          VTPD
069300910717     C                   Z-ADD     PLNP          VLNP
069400910717     C                   Z-ADD     PAAS          VAAS
069500910717     C                   Z-ADD     PNRS          VNRS
069600910717     C                   Z-ADD     PNSP          VNSP
069700941103     C                   Z-ADD     PDTE          G02INV
069800941103     C                   MOVE      *ZEROS        G02DAT
069900941103     C                   MOVE      '3'           G02ERR
070000991120     C                   CALL      'XSRDA8'
070100941103     C                   PARM                    WLBDAT
070200941103     C                   MOVE      G02DAT        VDTE
070300910717     C                   Z-ADD     PABI          VABI
070400910717     C                   Z-ADD     PCAB          VCAB
070500940126     C                   Z-ADD     PABI          SAVABI
070600940126     C                   Z-ADD     PCAB          SAVCAB
070700910723     C                   Z-ADD     TASS          VASS
070800060403     C                   Z-ADD     TASS          VASSh
070900921105     C* DECODIFICO ISTITUTO SOLO SE ABI/CAB<> 1000/1000
071000910717     C                   Z-ADD     PABI          KABI
071100910717     C                   Z-ADD     PCAB          KCAB
071200910725     C     KABI§         CHAIN     CNABI00F                           77
071300910725     C  N77              MOVEL     ABIIST        VDES
071400910725     C   77              MOVE      *BLANKS       VDES
071500910717     C*
071600911219     C  N77              MOVEL     ABICOM        VPIA
071700911219     C   77              MOVE      *BLANKS       VPIA
071800911219     C*
071900910717     C                   MOVEL     PTPA          VTPA
072000910717     C                   MOVEL     PTPI          VTPI
072100910717     C                   MOVEL     PTPP          VTPP
072200911219     C*                    MOVELPPIA      VPIA
072300910717     C                   MOVEL     PNOT          VNOT
072400910716     C*
072500910717     C* AZZERO TOTALIZZATORE ASSEGNI
072600910717     C                   Z-ADD     0             TASS
072700941103     C                   ADD       ASSCAS        TASS
072800910717     C* MEMORIZZO N° ASS.,ABI-CAB,DATA ..... PRECEDENTI
072900910717     C                   EXSR      PREC
073000910719     C                   ENDSR
073100991102     C*----------------------------------------------------*
073200991102     C* Sposta  i dati nei campi di salvataggio
073300991102     C*----------------------------------------------------*
073400910717     C     PREC          BEGSR
073500941103     C                   MOVEL     ASSNRA        PNRA             10
073600941103     C                   MOVEL     ASSTPD        PTPD              1
073700941103     C                   Z-ADD     ASSLNP        PLNP              3 0
073800941103     C                   Z-ADD     ASSAAS        PAAS              4 0
073900941103     C                   Z-ADD     ASSNRS        PNRS              2 0
074000941103     C                   Z-ADD     ASSNSP        PNSP              7 0
074100991102     C                   MOVE      ASSDTE        PDTE              8 0
074200941103     C                   Z-ADD     ASSABI        PABI              5 0
074300941103     C                   Z-ADD     ASSCAB        PCAB              5 0
074400941103     C                   MOVEL     ASSTPA        PTPA              1
074500941103     C                   MOVEL     ASSTPI        PTPI              1
074600941103     C                   MOVEL     ASSTPP        PTPP              1
074700941103     C                   MOVEL     ASSNOT        PNOT             20
074800941103     C                   MOVEL     ASSFVE        PFVE              1
074900910722     C                   ENDSR
075000910722     C*
075100000608     C*----------------------------------------------------*
075200000608      * SE RICHIESTO L'ESCLUSIONE TOTALE DI TUTTI GLI ASSEGNI
075300000608      * TRAMITE IL F14 IMPOSTA AUTOMATICAMENTE SU TUTTI
075400000608     C*----------------------------------------------------*
075500000608     C     EXCLRD        BEGSR
075600000608      *
075700000608     C                   SETOFF                                       9028
075800000608      *
075900000608     C                   DO        SAVNR2        CNT
076000000608      *
076100000608     C                   SETOFF                                       242762
076200000608     C                   SETOFF                                       495852
076300000608     C                   SETOFF                                       434445
076400000608     C                   SETOFF                                       464748
076500000608      *
076600000608     C     CNT           CHAIN     CN0W9S2                            60
076700000608      *
076800000608     C     VNRA          IFNE      *BLANK
076900000608     C                   MOVE      'E'           VANN
077000000608     C                   END
077100000608      *
077200000608     C                   UPDATE    CN0W9S2
077300000608      *
077400000608     C                   ENDDO
077500000608      *
077600000608     C                   ENDSR
077700991102     C*----------------------------------------------------*
077800991102     C* Controlli i dati del subfile
077900991102     C*----------------------------------------------------*
078000910717     C     CONTR         BEGSR
078100000609      *
078200951027     C                   SETOFF                                       9028
078300910717     C                   DO        TREC          CNT
078400991209     C                   SETOFF                                       242762
078500911115     C                   SETOFF                                       495852
078600911115     C                   SETOFF                                       434445
078700911115     C                   SETOFF                                       464748
078800000609      *
078900991102     C     CNT           CHAIN     CN0W9S2                            60
079000060526     c* 19 acceso utente di sede
079100060526     c                   if        vann = 'A' and not *in19
079200060411     c                   clear                   vann
079300060411     c                   end
079400000608      *
079500000608      * SE RICHIESTO L'ESCLUSIONE TOTALE DI TUTTI GLI ASSEGNI
079600000608      * TRAMITE IL F14 IMPOSTA AUTOMATICAMENTE SU TUTTI
079700000608      *
079800910717      *-------------
079900910717     C* ESEGUE CONTR. SOLO SE NON SONO ASSEGNI PROVENIENTI DA DISTINTA
080000910717     C* PADRONCINO
080100060526     C     VASS          ifne      0
080200910717     C     VANN          IFEQ      'A'
080300910729     C   KF              DO
080400910717     C                   MOVEL     VNRA          KNRA
080500910717     C                   MOVE      VNRA1         KNRA
080600910717     C                   Z-ADD     VABI          KABI
080700910717     C                   Z-ADD     VCAB          KCAB
080800060526     C     KASS          setll     CNASS000
080900060526     c                   do        *hival
081000060526     C     KASS          reade     CNASS000
081100060526     c                   if        %eof(cnass02l)
081200060526     c                   leave
081300060526     c                   end
081400951027     C     ASSFVE        IFEQ      'V'
081500951027     C     ASSTPI        ANDNE     'M'
081600060526     C                   DELETE    CNASS000
081700951027     C                   END
081800060526     C                   ENDdo
081900940126     C*
082000940126     C     SAVNRA        IFNE      VNRA
082100940126     C     SAVNR1        ORNE      VNRA1
082200940126     C     SAVABI        ORNE      VABI
082300940126     C     SAVCAB        ORNE      VCAB
082400940126     C                   MOVEL     SAVNRA        KNRA
082500940126     C                   MOVE      SAVNR1        KNRA
082600940126     C                   Z-ADD     SAVABI        KABI
082700940126     C                   Z-ADD     SAVCAB        KCAB
082800060526     C     KASS          setll     CNASS000
082900060526     c                   do        *hival
083000060526     C     KASS          reade     CNASS000
083100060526     c                   if        %eof(cnass02l)
083200060526     c                   leave
083300060526     c                   end
083400951027     C     ASSFVE        IFEQ      'V'
083500951027     C     ASSTPI        ANDNE     'M'
083600060526     C                   DELETE    CNASS000
083700940126     C                   END
083800060526     C                   ENDdo
083900060411     C                   END
084000940126     C*
084100910717     C                   EXSR      PULI
084200060411     C                   ENDdo
084300910717     C                   ELSE
084400910717     C                   EXSR      CTR
084500910717     C                   END
084600920520     C                   SETOFF                                       65
084700991102     C                   UPDATE    CN0W9S2
084800911219     C                   END
084900950303     C  N90              END
085000910717     C                   ENDSR
085100991102     C*----------------------------------------------------*
085200911115     C* COTROLLI SUBFILE
085300991102     C*----------------------------------------------------*
085400910717     C     CTR           BEGSR
085500060421      * CONTROLLO METTO V SU TIPO DOCUMENTO
085600060421     C     VTPD          IFEQ      *BLANKS
085700060421     C                   MOVE      'V'           VTPD
085800060421     C                   END
085900910717     C*
086000991209     C* CONTROLLO CHE IL NUMERO DEI DECIMALI SIA CONGRUENTE
086100991209     C                   MOVE      VASS          WCOMO1            1 0
086200991209     C                   MOVE      VASS          WCOMO2            2 0
086300010626     C     V1CDIV        IFEQ      §GEDCR
086400991209     C     DECDIV        ANDLT     2
086500991209     C     WCOMO1        ANDGT     0
086600010626     C     V1CDIV        OREQ      §GEDCR
086700991209     C     DECDIV        ANDLT     1
086800991209     C     WCOMO2        ANDGT     0
086900991209     C     V1CDIV        OREQ      DIVALT
087000991209     C     DECALT        ANDLT     2
087100991209     C     WCOMO1        ANDGT     0
087200991209     C     V1CDIV        OREQ      DIVALT
087300991209     C     DECALT        ANDLT     1
087400991209     C     WCOMO2        ANDGT     0
087500991209     C                   SETON                                        62  90
087600991209     C                   END
087700910717      * CONTROLLO DATA ASSEGNO
087800910717      *
087900060414     c                   if        vann <> 'E'
088000951027     C                   SETOFF                                       56
088100911113     C                   MOVE      VDTE          G02DAT
088200911113     C                   MOVE      *BLANKS       G02ERR
088300991102     C                   CALL      'XSRDA8'
088400910717     C                   PARM                    WLBDAT
088500910717     C     G02ERR        IFEQ      '1'
088600060403     c                   SETON                                        43  90
088700060414     c                   else
088800060414     c                   move      g02inv        dte
088900060403     C                   END
089000060411     c* controllo che la data non sia considerata postdatato
089100060414     c                   eval      sas = vtpa
089200060414     c                   callp     chkdataassegno(dte:sas
089300060414     c                             :esito:err:*omit)
0894000604141    c                   if        esito <>0
089500060411     C                   SETON                                        43  90
089600060411     C                   END
089700060411     c* obbligo ad escludere gli assegno con data > oggi
089800060411     C     G02inv        IFgt      oggi
089900060411     c                   SETON                                        44
090000060403     C                   END
090100991104     C                   MOVE      G02DAT        VDTE
090200060403     c* IMPORTO ASSEGNO OBBLIGATORIO
090300060403     C  n90VASS          IFEQ      0
090400060414     C                   SETON                                        456290
090500910717     C                   END
090600060403     c* controllo che l'importo dell'assegno non sia variato + dell'importo
090700060403     c* di tolleranza
090800060403     C  n90VASS          IFne      vassh
090900060414     C     vtpd          andne     'V'
091000060403     c                   exsr      srtolle
091100060403     C                   END
091200060414     C                   END
091300910717      ****************************************
091400910717      * SE TIPO ASSEGNO E' MITTENTE
091500910717      * ABI, CAB E N° ASSEGNO SONO OBBLIGATORI
091600910717      ****************************************
091700060403     C  n90VABI          IFEQ      0
091800911127     C     VTPA          ANDNE     'P'
091900911127     C     VTPA          ANDNE     'V'
092000910717     C     VCAB          OREQ      0
092100911127     C     VTPA          ANDNE     'P'
092200911127     C     VTPA          ANDNE     'V'
092300911127     C                   SETON                                        49  90
092400911127     C                   END
092500060403     C  n90VNRA          IFEQ      *BLANKS
092600910717     C                   SETON                                        47  90
092700910717     C                   END
092800911127      * CONTROLLO CODICE ABI/CAB SE TIPO ASSEGNO < > 'P' E 'V'
092900921105      * SOLO SE ABI/CAB NON E' 1000/1000
093000921105     C     VABI          IFNE      1000
093100921105     C     VCAB          ANDNE     1000
093200910725     C                   Z-ADD     VABI          KABI
093300910725     C                   Z-ADD     VCAB          KCAB
093400910725     C     KABI§         CHAIN     CNABI00F                           49
093500910729     C  N49VPIA          IFEQ      *BLANKS
093600910729     C                   MOVEL     ABICOM        VPIA
093700910729     C                   END
093800910717     C  N49              MOVEL     ABIIST        VDES
093900910801     C   49              MOVE      *BLANKS       VDES
094000910801     C     VPIA          IFEQ      *BLANKS
094100911113     C  NKF
094200911113     CANNKA              SETON                                        5290
094300910801     C                   END
094400911113     C   49
094500911113     CANNKF
094600911113     CANNKA              SETON                                        90
094700921105     C                   END
094800910716     C*
094900910722      * CONTROLLO TIPO ASSEGNO
095000910722     C     VTPA          IFNE      *BLANKS
095100910722     C                   MOVEA     *BLANKS       SKI
095200910722     C                   MOVEA     VTPA          SKI
095300910722     C     '?'           LOOKUP    SKI                                    31
095400910722     C   31              DO
095500910722     C                   MOVEL     CODUT         §KUT              1 0
095600910722     C                   MOVEL     '4E'          §COD              2
095700910722     C                   MOVE      *BLANKS       VTPA
095800910722     C                   CALL      'X§TABER'
095900910722     C                   PARM                    §KUT
096000910722     C                   PARM                    §COD
096100910722     C                   PARM                    §KEY              8
096200910722     C                   PARM                    §DES             30
096300910722     C                   MOVEL     §KEY          VTPA
096400910722     C                   END
096500910722     C*-----------------------------------------------------*
096600910722     C* Se no si ricerca su TABEL se esiste
096700910722     C*-----------------------------------------------------*
096800910722     C  N31              DO
096900910722     C                   MOVE      *BLANKS       KEY
097000910722     C                   MOVEL     VTPA          KEY
097100910722     C                   MOVE      '4E'          COD
097200910722     C     KTAB          CHAIN     TABEL                              24
097300921105     C  N24              EXCEPT    RILTAB
097400910722     C  N24TBLFLG        COMP      '*'                                    24
097500910723     C   24              SETON                                          90
097600910722     C                   END
097700910722     C                   ELSE
097800910723     C                   SETON                                        2490
097900910722     C                   END
098000951027     C* Se assegno vario controllo se esiste con quei
098100951027     C* riferimenti un assegno proveiente da una chiusura
098200951027     C     VPAD          IFNE      'P'
098300951027     C                   MOVEL     VNRA          KNRA
098400951027     C                   MOVE      VNRA1         KNRA
098500951027     C                   Z-ADD     VABI          KABI
098600951027     C                   Z-ADD     VCAB          KCAB
098700951027     C     KASS          CHAIN     CNASS02L                           57
098800951027     C     *IN57         IFEQ      '0'
098900951027     C     *IN57         DOWEQ     '0'
099000951027     C     ASSTPD        ANDEQ     'V'
099100951027     C     KASS          READE     CNASS02L                               57
099200951027     C                   END
099300951027     C     ASSTPD        IFNE      'V'
099400951027     C     ASSTPI        IFEQ      'M'
099500951027     C                   SETON                                        285690
099600951027     C                   MOVEL     ERR(3)        $MSG
099700951027     C                   ELSE
099800951027     C     ASSFVE        IFNE      'V'
099900951027     C                   SETON                                        285690
100000951027     C                   MOVEL     ERR(1)        $MSG
100100951027     C                   ELSE
100200951027     C                   SETON                                        285690
100300951027     C                   MOVEL     ERR(2)        $MSG
100400951027     C                   END
100500951027     C                   END
100600951027     C                   END
100700951027     C                   END
100800951027     C                   END
100900910722     C*
101000910729     C  N90
101100910729     CAN KF
101200910729     CANN11              DO
101300940126     C                   MOVEL     VNRA          KNRA
101400940126     C                   MOVE      VNRA1         KNRA
101500940126     C                   Z-ADD     VABI          KABI
101600940126     C                   Z-ADD     VCAB          KCAB
101700941103     C     KASS          CHAIN     CNASS02L                           57
101800940126     C   57              EXSR      SPOSTA
101900941103     C   57              WRITE     CNASS000
102000941103     C  N57              UPDATE    CNASS000
102100940126     C*
102200940126     C     SAVNRA        IFNE      VNRA
102300940126     C     SAVNR1        ORNE      VNRA1
102400940126     C     SAVABI        ORNE      VABI
102500940126     C     SAVCAB        ORNE      VCAB
102600940126     C                   MOVEL     SAVNRA        KNRA
102700940126     C                   MOVE      SAVNR1        KNRA
102800940126     C                   Z-ADD     SAVABI        KABI
102900940126     C                   Z-ADD     SAVCAB        KCAB
103000941103     C     KASS          CHAIN     CNASS000                           57
103100951027     C     ASSFVE        IFEQ      'V'
103200951027     C     ASSTPI        ANDNE     'M'
103300941103     C  N57              DELETE    CNASS000
103400940126     C                   END
103500951027     C                   END
103600951026     C                   MOVE      VNRA          SAVNRA
103700951026     C                   MOVE      VNRA1         SAVNR1
103800951026     C                   Z-ADD     VABI          SAVABI
103900951026     C                   Z-ADD     VCAB          SAVCAB
104000991104     C                   COMMIT
104100910723     C                   END
104200910717     C                   ENDSR
104300991102     C*-----------------------------------------------------*
104400060403     C* tolleranza importo assegno
104500991102     C*-----------------------------------------------------*
104600060403     C     srtolle       BEGSR
104700060403     C     Vass          SUB       Vassh         §abb
104800060403     C* LO RENDO POSITIVO
104900060403     C     §ABB          IFLT      *ZEROS
105000060403     C     §ABB          MULT      -1            §ABB
105100060403     C                   END
105200060403     C* CONTROLLO IL LIMITE tollerabile DELLA differenza
105300060403     C                   MOVEL     'GEI'         KCOD1
105400060403     C                   MOVEL(p)  'EUR'         KKEY1
105500060403     c                   clear                   dgei
105600060403     C     KTBE          CHAIN     TNTBE01L
105700060403     c                   if        %found(tntbe01l)  and tbeatb = ' '
105800060403     C                   MOVEL     TBEUNI        DGEI
105900060403     c                   end
106000060403     C     §ABB          COMP      §GEtvr                             45
106100060403     C   45              SETON                                        9062
106200060403     C                   ENDSR
106300060403     C*-----------------------------------------------------*
106400060403     C* SCRIVO CNASS
106500060403     C*-----------------------------------------------------*
106600060403     C     SPOSTA        BEGSR
106700000609      *
106800941103     C                   CLEAR                   CNASS000
106900941103     C                   Z-ADD     §FLE          ASSFLE
107000941103     C                   MOVEL     VTPD          ASSTPD
107100941103     C                   Z-ADD     VASS          ASSCAS
107200941103     C                   MOVEL     VNRA          ASSNRA
107300991104     C                   MOVEL     V2CDIV        ASSVCA
107400941103     C                   MOVE      VNRA1         ASSNRA
107500941103     C                   Z-ADD     VDTE          G02DAT
107600941103     C                   MOVEL     *BLANKS       G02ERR
107700941103     C                   MOVE      *ZEROS        G02INV
107800991102     C                   CALL      'XSRDA8'
107900941103     C                   PARM                    WLBDAT
108000941103     C                   Z-ADD     G02INV        ASSDTE
108100941103     C                   Z-ADD     VABI          ASSABI
108200941103     C                   Z-ADD     VCAB          ASSCAB
108300941103     C                   MOVEL     VTPA          ASSTPA
108400941103     C                   MOVEL     VTPI          ASSTPI
108500941103     C                   MOVEL     VTPP          ASSTPP
108600941103     C                   MOVEL     VPIA          ASSPIA
108700941103     C                   MOVEL     VNOT          ASSNOT
108800941103     C                   MOVEL     'V'           ASSFVE
108900910723     C                   ENDSR
109000991102     C*-----------------------------------------------------
109100910723     C* CARICA RECORD VUOTI NEL SUBFILE
109200991102     C*-----------------------------------------------------
109300910717     C     AGG           BEGSR
109400920520     C                   SETOFF                                       65
109500910717     C                   EXSR      PULI
109600910717     C                   DO        50
109700991103     C                   ADD       1             NR2
109800991102     C                   WRITE     CN0W9S2
109900910717     C                   END
110000910722     C                   ENDSR
110100991102     C*-----------------------------------------------------
110200991102     C* CARICA RECORD VUOTI NEL SUBFILE
110300991102     C*-----------------------------------------------------
110400910717     C     PULI          BEGSR
110500910725     C                   MOVE      *BLANKS       VPAD
110600910717     C                   MOVE      *BLANKS       VANN
110700910717     C                   MOVE      *BLANKS       VNRA
110800910717     C                   MOVE      *BLANKS       VNRA1
110900940126     C                   MOVE      *BLANKS       SAVNRA
111000940126     C                   MOVE      *BLANKS       SAVNR1
111100910717     C                   Z-ADD     0             VDTE
111200910717     C                   Z-ADD     0             VASS
111300060403     C                   Z-ADD     0             VASSh
111400910717     C                   Z-ADD     0             VABI
111500910717     C                   Z-ADD     0             VCAB
111600940126     C                   Z-ADD     0             SAVABI
111700940126     C                   Z-ADD     0             SAVCAB
111800910717     C                   MOVE      *BLANKS       VTPA
111900910717     C                   MOVE      *BLANKS       VTPP
112000910717     C                   MOVE      *BLANKS       VTPI
112100910717     C                   MOVE      *BLANKS       VNOT
112200910717     C                   MOVE      *BLANKS       VTPD
112300910717     C                   Z-ADD     0             VLNP
112400910717     C                   Z-ADD     0             VAAS
112500910717     C                   Z-ADD     0             VNRS
112600910717     C                   Z-ADD     0             VNSP
112700910717     C                   MOVE      *BLANKS       VDES
112800910717     C                   MOVE      *BLANKS       VPIA
112900000608      *
113000910717     C                   ENDSR
113100991102     C*-----------------------------------------------------
113200991102     C* LEGGO SUBFILE E CALCOLO TOTALI X VIDEATA REGISTR.
113300991102     C*-----------------------------------------------------
113400910717     C     CONF          BEGSR
113500000608      *
113501150109     c                   clear                   lista
113600910726     C                   Z-ADD     0             TOTASS
113700910726     C                   MOVEA     *ZEROS        IMP
113800930927     C                   MOVEA     *ZEROS        IM1
113900000608      *
114000910717     C                   DO        TREC          CNT               4 0
114100991102     C     CNT           CHAIN     CN0W9S2                            61
114200000608      *
114300910718     C* TOTALE ASSEGNI
114400910731     C     VANN          IFNE      'E'
114500910717     C                   ADD       VASS          TOTASS
114600910718     C* TOT. IMPORTI PER TIPO
114700910718     C                   Z-ADD     1             A                 3 0
114800910719     C                   MOVEL     VTPA          LKP               2
114900910719     C                   MOVE      VTPP          LKP
115000910719     C     LKP           LOOKUP    CSP(A)                                 21
115100930927     C*
115200930927     C     VABI          IFEQ      ISTABI
115300910718     C   21              ADD       VASS          IMP(A)
115400930927     C                   ELSE
115500930927     C   21              ADD       VASS          IM1(A)
115600910717     C                   END
115601150108     c*esclusi
115602150108     c                   else
115603150108     c                   exsr      VerEsclusi
115700930927     C*
115800930927     C                   END
115900910731     C                   END
116000910717     C* TOTALE PROVVISORIO DISTINTA
116100910717     C     TOTASS        ADD       TOTCON        TOTDIS
116200991102     C                   Z-ADD     GMAUD8        VIDDRG
116300991118     C                   Z-ADD     GMAUD8        VDTOPE
116400991102     C                   Z-ADD     GMAUD8        VIDDDS
116500941104     C                   MOVE      §FLE          VFPN
116600910718     C* PROPONGO I DATI
116700060403     C                   move      §4occ1        VIDCC1            4 0
116800060403     C                   move      §4osc1        VIDSC1            7 0
116900991102     C* Imposto valuta distinta
117000991102     C                   MOVEL     V2CDIV        V3CDIV
117100000608     C                   MOVEL     V1PLIC        V3PLIC
117200910718     C*
117300910725     C     *IN13         DOWEQ     '0'
117400910725     C     *IN17         ANDEQ     '0'
117500980114     C                   SETOFF                                       59
117600950906     C                   Z-ADD     §TCONT        TOTCON
117700950906     C     TOTASS        ADD       TOTCON        TOTDIS
117800991105     C                   Z-ADD     TOTDIS        TOTVER
117900991118     C     START3        TAG
118000991102     C                   EXFMT     CN0W9D3
118100980114     C                   SETOFF                                       28
118200991118     C* CONTROLLO LA DATA OPERAZIONE SE ERRATA OPPURE NO
118300991118     C                   MOVEL     *BLANKS       G02ERR
118400991118     C                   Z-ADD     VDTOPE        G02DAT
118500991118     C                   CALL      'XSRDA8'
118600991118     C                   PARM                    WLBDAT
118700991118     C                   Z-ADD     G02INV        AMGDOP            8 0
118800991118     C     G02ERR        IFEQ      '1'
118900991118     C                   SETON                                        51
119000991118     C                   GOTO      START3
119100991118     C                   END
119200991118     C                   MOVE      G02DAT        VDTOPE
119300020422
119400020422     ** Eseguo sulla data operazione lo stesso controllo di PROJ,
119500020422     ** cioè la data non può discostare di più di 9 mesi dalla data
119600020422     ** corrente.
119700020422     C                   MOVE      VDtOpe        WrkDtOpe
119800020422     C                   TIME                    WrkDtCur
119900020422     C     WrkDtCur      SUBDUR    WrkDtOpe      WrkMonth:*M
120000020422     C                   IF        %ABS(WrkMonth) > 8
120100020422     C                   EVAL      *IN51 = *ON
120200020422     C                   GOTO      Start3
120300020422     C                   ENDIF
120400020422
120500910725     C  N17              DO
120600060824     c* se non ancora inserito contante e ci sono dei mandatini forzo KD
120700060824     c                   if        not *inkd and okcon=' '
120800060824     c     §fle          setll     yfcas01l                               33
120900060824     c                   if        *in33
121000060824     c                   eval      *inkd = '1'
121100060824     c                   eval      okcon = '1'
121200060824     c                   end
121300060824     c                   end
121400060824     c*
121500910723     C   KD              DO
121600910723     C                   EXSR      GESCON                                       GEST.CONTA
121700950906     C                   Z-ADD     §TCONT        TOTCON
121800950906     C     TOTASS        ADD       TOTCON        TOTDIS
121900991105     C                   Z-ADD     TOTDIS        TOTVER
122000991102     C                   Z-ADD     VIDDRG        VIDDDS
122100950906     C                   END
122200000608      *
122300910725     C   13              EXSR      CTR2
122400910726     C   59              SETOFF                                       13
122500910717     C                   END
122600000608      *
122700911125     C   17              SETOFF                                       1112
122800910717     C                   END
122900910717     C                   ENDSR
123000991102     C*-------------------------------------------------------------*
123100991102     C* CONTROLLI
123200991102     C*-------------------------------------------------------------*
123300910718     C     CTR2          BEGSR
123400930222     C*
123500910718     C                   Z-ADD     0             TOTVER
123600991103     C     TOTASS        ADD       TOTCON        TOTDIS
123700991105     C                   Z-ADD     TOTDIS        TOTVER
123800910718     C** NUMERO REG. OBBLIGATORIO
123900910723     C   72              SETON                                        59
124000910718     C   72              GOTO      START2
124100910718     C*  DATA REGISTRAZIONE
124200940329     C                   Z-ADD     VIDDRG        G02DAT
124300940329     C                   MOVEL     *BLANKS       G02ERR
124400991102     C                   CALL      'XSRDA8'
124500940329     C                   PARM                    WLBDAT
124600991102     C                   Z-ADD     G02INV        AMGDRG            8 0
124700991102     C                   Z-ADD     G02TGI        DATARE            8 0          *
124800021115     C* SE NON HO DA VERSARE NULLA VADO A FINE
124900060411     C     TOTVER        ifeq      0
125000021115     C                   GOTO      START2
125100060411     C                   End
125200021115     C  N13              GOTO      START2
125300910718     C*§ EMISSIONE ULTIMA VIDEATA
125400910718     C*  ------------------------
125500910718     C*  PULIZIA SFL 4
125600910718     C                   EXSR      PULI4
125700910718     C*  CARICAMENTO SFL 4
125800910718     C                   EXSR      CARI4
125900910726     C* ATTRIBUZIONE NR° DISTINTA SOLO LA PRIMA VOLTA
126000910726     C  N68              DO
126100910726     C                   MOVEL     '4D'          COD
126200910726     C                   MOVEL     §FLE          KEY
126300910726     C     KTAB          CHAIN     TABEL                              46
126400980114     C     *IN46         IFEQ      '1'
126500980114     C                   SETON                                        2859
126600980114     C                   MOVEL     ERR(4)        $MSG
126700980114     C                   GOTO      START2
126800980114     C                   END
126900910726     C                   MOVEL     TBLUNI        NUMVER
127000910726     C                   ADD       1             NUMVER
127100910726     C                   MOVEL     NUMVER        TBLUNI
127200980114     C  N46              UPDATE    TABEL
127300000609      * CON IL GIRO DELLA BANCA X CONTANTE (66=ON)
127400000609      *  IMPOSTA NEL NR.DISTINTA IL NR.DEL PLICO
127500000616     C                   Z-ADD     NUMVER        VIDNDS
127600991125     C                   Z-ADD     NUMVER        VIDNRG
127700991125     C                   MOVEL     §FLE          VIDNRG
127800910726     C                   SETON                                        68
127900910726     C                   END
128000910726     C*
128100910718     C     EMI4          TAG
128200991103     C                   WRITE     CN0W9Z5
128300991103     C                   EXFMT     CN0W9C5
128400910718     C*                    ____________
128500910725     C   16              SETON                                        59
128600931222     C   16              SETOFF                                       88
128700910725     C   16              GOTO      START2
128800910718     C*
128900910718     C*§ CONTROLLI CAMPI DEL SFL
129000910718     C*  -----------------------
129100991103     C                   Z-ADD     0             NR5
129200910718     C     *IN90         DOUEQ     '1'
129300991103     C                   ADD       1             NR5
129400991103     C     NR5           CHAIN     CN0W9S5                            90
129500910718     C  N90VIMP4         IFNE      *ZEROS
129600910718     C     VSPE4         IFEQ      'CO'
129700910718     C     VSPE4         OREQ      'RE'
129800910718     C     VSPE4         OREQ      'CA'
129900910718     C                   SETON                                        88
130000910718     C                   ELSE
130100910718     C                   SETOFF                                       88
130200910718     C                   END
130300910718     C*
130400910718     C*  CAUSALE CONTABILE
130500910718     C                   MOVEL     'PG'          OPECD1
130600910718     C                   MOVEL     VCAU4         OPECOD                         CAUSALE CO
130700910718     C     KEYOPE        CHAIN     CNOPE000                           87
130800991103     C   87              UPDATE    CN0W9S5
130900910718     C   87              GOTO      EMI4
131000910718     C                   MOVEL     OPEDES        VDES4
131100910718     C                   END
131200910718     C                   END
131300910718     C*
131400910726     C   15              DO
131500910718     C     SUDT3         TAG                                                    *
131600910718     C                   EXSR      STADIS
131601150109     c* verifica se esistono segnalazioni per mandare mail
131602150109     c                   if        lista(1) <> *blank
131603150109     c                   exsr      inviamail
131604150109     c                   endif
131700910718     C*
131800991103     C                   Z-ADD     0             NR5
131900910718     C     *IN90         DOUEQ     '1'
132000991103     C                   ADD       1             NR5
132100991103     C     NR5           CHAIN     CN0W9S5                            90
132200910718     C  N90VIMP4         COMP      0                                      90
132300930924     C*----------------------------------------------------*
132400930924     C     *IN90         IFEQ      '0'                                          *
132500991103     C* Scrivo movimenti contabili
132600991103     C                   EXSR      CREMOV                                       *
132700930924     C                   END                                                    *
132800000612      *
132900930924     C*----------------------------------------------------*
133000930924     C                   END
133100000612     C*----------------------------------------------------*
133200000612      * SCRIVE IL FILE DEI PLICHI DA TRASMETTERE A "SICURDATA"
133300000612      *
133400020321     C                   EXSR      WRIPLI
133500021118      * SCRIVE IL FILE DEI VERSAMENTI GIORNALIERI
133600021118      *
133700021121     C                   EXSR      WRTVRT
133800060411      * annulla i mandati cassaforte con ferritoia
133900060411      *
134000060922     c                   if        totcon <> 0
134100060411     C                   EXSR      annman
134200060922     c                   end
134300020321
134400991104     C*----------------------------------------------------*
134500991104     C* Sottomette trasmissioni                            *
134600991108     C                   COMMIT
134700000612      *
134800991104     C                   EXSR      SBMTRA                                       *
134900000612      *
135000930924     C*----------------------------------------------------*
135100910718     C                   EXSR      ANNDIS
135200000612      *
135300910726     C                   SETON                                        12
135400910726     C                   MOVE      '1'           *INKC
135500910801     C                   GOTO      START2
135600910718     C                   END
135700910726     C  N16              GOTO      EMI4
135800910718     C     START2        TAG
135900910718     C*
136000910718     C                   ENDSR
136100991102     C**----------------------------------------------------------
136200000000     C** PULIZIA DEL SUBFILE
136300991102     C**----------------------------------------------------------
136400000000     C     PULIZ         BEGSR
136500000000     C                   SETON                                        70
136600991102     C                   WRITE     CN0W9C2
136700000000     C                   SETOFF                                       70
136800910722     C                   SETON                                        71
136900991103     C                   Z-ADD     0             NR2
137000910725     C* PULISCO CAMPI DI COMODO
137100991102     C                   MOVE      V1CDIV        V2CDIV
137200991102     C                   MOVE      *BLANKS       PNRA
137300910725     C                   Z-ADD     0             PDTE
137400910725     C                   Z-ADD     0             PABI
137500910725     C                   Z-ADD     0             PCAB
137600910725     C                   MOVE      *BLANKS       PTPA
137700910725     C                   MOVE      *BLANKS       PTPP
137800910725     C                   MOVE      *BLANKS       PFVE
137900910725     C                   MOVE      *BLANKS       PTPI
138000910725     C                   MOVE      *BLANKS       PNOT
138100910725     C                   MOVE      *BLANKS       PTPD
138200910725     C                   Z-ADD     0             PLNP
138300910725     C                   Z-ADD     0             PAAS
138400910725     C                   Z-ADD     0             PNRS
138500910725     C                   Z-ADD     0             PNSP
138600950906     C                   Z-ADD     0             §TCONT
138700911219     C*                    MOVE *BLANKS   PPIA
138800910725     C*
138900000000     C                   ENDSR
139000991103     C*-------------------------------------------------------
139100900517     ** GESTIONE CONTANTI/ASSEGNI
139200991103     **-------------------------------------------------------
139300900517     C     GESCON        BEGSR
139400000608      *
139500910725     C                   SETOFF                                       1813
139600991102     C     V3CDIV        COMP      TGDDIV                                 09
139700000608      *
139800900517     C  N09              EXSR      PULCON
139900000608      *
140000900517     C* CARICAMENTO SUBFILE
140100991102     C                   MOVEL     V3CDIV        V4CDIV
140200000608     C                   MOVEL     V3PLIC        V4PLIC
140300060824     c                   if        not *in09
140400060824     C                   EXSR      CARCON
140500060824     C                   EXSR      CTRCON
140600060824     c                   end
140700900517     C*                    ----------
140800000608      *
140900900517     C                   SETON                                        09        1° VOLTA
141000900517     C     EMICON        TAG
141100991102     C                   WRITE     CN0W9Z4
141200991102     C                   EXFMT     CN0W9C4
141300000608      *
141400910725     C   18              GOTO      ENDCON
141500000608      *
141600000608     C* CONTROLLO CONTANTI IMMESSI
141700000608     C                   EXSR      CTRCON
141800000608      *
141900900518     C* CALCOLO  TOTALE CONTANTI /
142000910726     C   14              EXSR      CALCON
142100900517     C*                    -----------
142200910726     C  N14
142300910726     CANN18              GOTO      EMICON
142400000608      *
142500900517     C     ENDCON        TAG
142600900517     C                   ENDSR
142700991103     C**----------------------------------------------------------
142800900518     C** PULIZIA DEL SUBFILE CONTANTI
142900991103     C**----------------------------------------------------------
143000900517     C     PULCON        BEGSR
143100900517     C                   SETON                                        70
143200991102     C                   SETOFF                                       71
143300991102     C                   WRITE     CN0W9C4
143400900517     C                   SETOFF                                       70
143500900517     C                   ENDSR
143600991103     C**----------------------------------------------------------
143700900518     C** CARICAMENTO SUBFILE CONTANTI
143800991103     C**----------------------------------------------------------
143900900517     C     CARCON        BEGSR
144000000608      *
144100900517     C* CARICO I TAGLI PER IL CONTANTE
144200900517     C* L'IMPORTO IN TABELLA è 0 IL CAMPO IMPORTO è SPROTETTO
144300900517     C* LA DESCRIZ. DEI TAGLI CONTANTE è PROTETTO
144400991102     C                   CLEAR                   VNRTGL
144500900517     C                   SETON                                        40
144600991103     C                   Z-ADD     0             NR4               4 0
144700991104     C                   MOVEL     V3CDIV        TGDDIV
144800991102     C     TGDDIV        CHAIN     YNTGD01L                           21
144900991102     C     *IN21         DOWEQ     '0'
145000000621      *
145100000621     C     *IN66         IFEQ      *OFF
145200000621     C     *IN66         OREQ      *ON
145300000621     C     TGDVAL        ANDGT     0
145400991102     C                   MOVE      TGDDES        VDESCR
145500991104     C                   Z-ADD     TGDVAL        HIMTGL
145600900517     C     HIMTGL        COMP      0                                  20        PROTETTO
145700900517     C                   MOVEL     'C'           HIDTIP
145800060411     c                   exsr      srman
145900991103     C                   ADD       1             NR4
146000991102     C                   WRITE     CN0W9S4
146100991102     C                   SETON                                        71
146200000621     C                   END
146300000621      *
146400991102     C     TGDDIV        READE     YNTGD01L                               21
146500900517     C                   END
146600900517     C                   SETOFF                                       2040
146700020417     C                   EVAL      WrkLstRrnS4 = Nr4
146800000608      *
146900900517     C                   ENDSR
147000991103     C**----------------------------------------------------------
147100000608     C** CONTROLLO  CONTANTI  IMMESSI
147200991103     C**----------------------------------------------------------
147300000608     C     CTRCON        BEGSR
147400000608      *
147500020417     C                   IF        WrkLstRrnS4 > 0
147600000608      *  AGGIORNA SEMPLICEMENTE IL SFL
147700000608     C                   Z-ADD     0             NR4
147800000608 +++ C                   Z-ADD     0             §TCONT
147900000612     C                   Z-ADD     0             TOTIM            15 3
148000000608      *
148100000608     C                   DO        *HIVAL
148200000608     C                   ADD       1             NR4
148300000608     C     NR4           CHAIN     CN0W9S4                            63
148400000608      *
148500000608      *  PER FINE FILE ESCE DAL GIRO
148600000608     C     *IN63         IFEQ      *ON
148700000608     C                   LEAVE
148800000608     C                   END
148900000608      *
149000000608      *  CALCOLA LA RIGA
149100000608     C     HIDTIP        IFEQ      'C'
149200000608     C     HIMTGL        ANDNE     *ZEROS
149300060411     c                   eval      vidim = (vnrtgl + vnrman)*himtgl
149400000612     C                   ADD       VIDIM         TOTIM
149500000608     C     HIMTGL        COMP      0                                  20        PROT.VIDIM
149600000608     C                   SETON                                        40
149700000608     C                   UPDATE    CN0W9S4
149800000608     C                   END
149900000608     C*
150000000608     C                   END
150100020417      *
150200020417     C                   ENDIF
150300000608      *
150400000608     C                   ENDSR
150500000608     C**----------------------------------------------------------
150600000608     C** CALCOLO TOTALE  CONTANTI
150700000608     C**----------------------------------------------------------
150800000608     C     CALCON        BEGSR
150900020417      *
151000020417     C                   IF        WrkLstRrnS4 > 0
151100000608      *
151200991103     C                   Z-ADD     0             NR4
151300910730 +++ C                   Z-ADD     0             §TCONT
151400900517     C                   DO        *HIVAL
151500000612      *
151600991103     C                   ADD       1             NR4
151700991103     C     NR4           CHAIN     CN0W9S4                            63
151800910731     C     *IN63         IFEQ      '0'
151900000612      *
152000900517     C     HIDTIP        IFEQ      'C'
152100900517     C     HIMTGL        IFNE      *ZEROS
152200060411     c                   eval      vidim = (vnrtgl + vnrman)*himtgl
152300900517     C                   ADD       VIDIM         §TCONT
152400900517     C     HIMTGL        COMP      0                                  20        PROT.VIDIM
152500900517     C                   SETON                                        40
152600991102     C                   UPDATE    CN0W9S4
152700900517     C                   ELSE
152800900517     C                   ADD       VIDIM         §TCONT
152900900517     C                   END
153000000612      *
153100900517     C                   END
153200900517     C*
153300900517     C                   END
153400000612     C*
153500910731     C  N63              END
153600020417      *
153700020417     C                   ENDIF
153800000608      *
153900900517     C                   ENDSR
154000991103     C**----------------------------------------------------------
154100060411     C** aggiunge i mandati della cassaforte con ferritoia
154200991103     C**----------------------------------------------------------
154300060411     C     srman         BEGSR
154400060411     c                   clear                   vnrman
154500060411     c     §fle          setll     yfcas01l
154600060411     c                   do        *hival
154700060411     c     §fle          reade     yfcas01l
154800060411     c                   if        %eof(yfcas01l)
154900060411     c                   leave
155000060411     c                   end
155100060411     c                   if        CASVALORE = HIMTGL
155200060411     c                   add       casqta        vnrman
155300060411     c                   end
155400060411     c                   enddo
155500060411     C                   ENDSR
155600060411     C**----------------------------------------------------------
155700060411     C** annulla i mandati della cassaforte con ferritoia
155800060411     C**----------------------------------------------------------
155900060411     C     annman        BEGSR
156000061120     c* prima di cancellare chiamo il pgm per memorizzare in un archivio
156100061120     c* di sede i mandati
156200061120     c                   eval      fle= §fle
156300061120     c                   eval      kpjbu = CNC0WD00S
156400061120     c                   call      'CNC0WD00R'
156500061120     c                   parm                    kpjba
156600061120     C*
156700060411     c     §fle          setll     yfcas01l
156800060411     c                   do        *hival
156900060411     c     §fle          reade     yfcas01l
157000060411     c                   if        %eof(yfcas01l)
157100060411     c                   leave
157200060411     c                   end
157300060411     c                   delete    yfcas000
157400060411     c                   enddo
157500060411     C                   ENDSR
157600060411     C**----------------------------------------------------------
157700060411     C** PULIZIA DELL'ULTIMO  SUBFILE
157800060411     C**----------------------------------------------------------
157900060411     C     PULI4         BEGSR
158000891205     C                   SETON                                        70
158100991102     C                   WRITE     CN0W9C5
158200891205     C                   SETOFF                                       70
158300891205     C                   ENDSR
158400991103     C**----------------------------------------------------------
158500891205     C** CARICAMENTO SUBFILE 4
158600991103     C**----------------------------------------------------------
158700891205     C     CARI4         BEGSR
158800891205     C*
158900991104     C                   MOVEL     V3CDIV        V5CDIV
159000000608     C                   MOVEL     V3PLIC        V5PLIC
159100991103     C                   Z-ADD     0             NR5               4 0
159200000608      *
159300891205     C                   DO        20            A
159400891205     C     IMP(A)        IFNE      *ZEROS
159500891205     C                   MOVE      CSP(A)        VSPE4
159600891205     C                   Z-ADD     IMP(A)        VIMP4
159700991120     C                   Z-ADD     0             VNRP4
159800991120     C                   Z-ADD     0             VDRP4
159900891205     C                   MOVE      'D'           VDA4
160000891205     C                   MOVE      CAU(A)        VCAU4
160100940831     C                   SETOFF                                       32
160200891205     C                   Z-ADD     KBNA          VKCC4
160300910726     C                   Z-ADD     VBAN          VKSC4
160400891205     C*
160500891205     C                   SETOFF                                       88
160600991102     C                   ADD       1             NR5
160700991102     C                   WRITE     CN0W9S5
160800991103     C                   END
160900930927     C*
161000930927     C*  CARICO  SUBFILE X IMPORTI CAUSALI ALTRO ISTITUTO
161100930927     C*
161200930927     C     IM1(A)        IFNE      *ZEROS
161300930927     C                   MOVE      CSP(A)        VSPE4
161400930927     C                   Z-ADD     IM1(A)        VIMP4
161500930927     C                   MOVE      'D'           VDA4
161600930927     C                   MOVE      CA1(A)        VCAU4
161700940831     C                   SETOFF                                       32
161800930927     C                   Z-ADD     KBNA          VKCC4
161900930927     C                   Z-ADD     VBAN          VKSC4
162000991120     C                   Z-ADD     0             VNRP4
162100991120     C                   Z-ADD     0             VDRP4
162200930927     C*
162300930927     C                   SETOFF                                       88
162400991103     C                   ADD       1             NR5
162500991102     C                   WRITE     CN0W9S5
162600930927     C                   END
162700930927     C*
162800891205     C                   END
162900891205     C* CONTANTE
163000891205     C* --------
163100891205     C     TOTCON        IFGT      *ZEROS
163200891205     C* 88 ON RENDE MODIFICABILE LA CAUSALE
163300891205     C                   SETON                                        88
163400891205     C                   Z-ADD     1             A
163500891205     C                   Z-ADD     TOTCON        VIMP4
163600891205     C                   MOVE      'D'           VDA4
163700891205     C     '97'          LOOKUP    CSP(A)                                 21
163800891205     C     *IN21         IFEQ      '1'
163900891205     C                   MOVE      'CO'          VSPE4
164000891205     C                   MOVE      CAU(A)        VCAU4
164100940831     C                   SETOFF                                       32
164200891205     C                   Z-ADD     KBNA          VKCC4
164300910726     C                   Z-ADD     VBAN          VKSC4
164400991120     C                   Z-ADD     0             VNRP4
164500991120     C                   Z-ADD     0             VDRP4
164600891205     C*
164700991103     C                   ADD       1             NR5
164800991102     C                   WRITE     CN0W9S5
164900891205     C                   END
165000991104     C                   END
165100021114      * CONTROPARTITA
165200030115     c                   z-add     1             a
165300021114     C     '99'          LOOKUP    CSP(A)                                 21
165400021114     C     *IN21         IFEQ      '1'
165500021114     C                   Z-ADD     totver        VIMP4
165600021114     C                   MOVE      'CA'          VSPE4
165700021114     C                   MOVE      CAU(A)        VCAU4
165800021114     C                   MOVE      'A'           VDA4
165900021114     C* Per le filiali di primo livello ribalto il versato in cassa
166000021125     C* Per le Secondo Livello ribalto il versamento negli inc. da attrib.
166100060403     C                   Z-ADD     VIDCC1        VKCC4
166200060403     C                   Z-ADD     VIDSC1        VKSC4
166300991120     C                   Z-ADD     §FLE          VNRP4
166400991122     C                   Z-ADD     WTODAY        VDRP4
166500891205     C*
166600891206     C                   SETON                                        88
166700991102     C                   ADD       1             NR5
166800991102     C                   WRITE     CN0W9S5
166900021114     C                   END
167000991104     C*
167100891205     C                   ENDSR
167200891205     C/EJECT
167300000608     C**------------------------------------------------------------
167400000000     C** ANNULLAMENTO DISTINTA STAMPATA
167500000608     C**------------------------------------------------------------
167600000000     C     ANNDIS        BEGSR
167700000608     C*
167800910719     C                   DO        TREC          CNT
167900991103     C     CNT           CHAIN     CN0W9S2                            21
168000000608     C*
168100910731     C     VANN          IFNE      'E'
168200910726     C                   MOVEL     VNRA          KNRA
168300910726     C                   MOVE      VNRA1         KNRA
168400910726     C                   Z-ADD     VABI          KABI
168500910726     C                   Z-ADD     VCAB          KCAB
168600940126     C                   SETOFF                                       57
168700000608     C*
168800941103     C     KASS          SETLL     CNASS000                           57
168900000608     C*
169000920206     C     *IN57         DOWEQ     '0'
169100941103     C     KASS          READE     CNASS000                               57
169200910719     C*
169300951027     C     ASSFVE        IFEQ      'V'
169400951027     C     ASSTPI        ANDNE     'M'
169500941103     C  N57              DELETE    CNASS000
169600940126     C                   END
169700000608     C*
169800951027     C                   END
169900940126     C*
170000940126     C     SAVNRA        IFNE      VNRA
170100940126     C     SAVNR1        ORNE      VNRA1
170200940126     C     SAVABI        ORNE      VABI
170300940126     C     SAVCAB        ORNE      VCAB
170400000608     C*
170500940126     C                   MOVEL     SAVNRA        KNRA
170600940126     C                   MOVE      SAVNR1        KNRA
170700940126     C                   Z-ADD     SAVABI        KABI
170800940126     C                   Z-ADD     SAVCAB        KCAB
170900940126     C                   SETOFF                                       57
171000000608     C*
171100941103     C     KASS          SETLL     CNASS000                           57
171200000608     C*
171300940126     C     *IN57         DOWEQ     '0'
171400941103     C     KASS          READE     CNASS000                               57
171500941103     C*          KASS      CHAINCNASS000             57
171600940126     C*
171700951027     C     ASSFVE        IFEQ      'V'
171800951027     C     ASSTPI        ANDNE     'M'
171900941103     C  N57              DELETE    CNASS000
172000951027     C                   END
172100910719     C*
172200920206     C                   END
172300940126     C                   END
172400910719     C                   ELSE
172500901011     C                   EXSR      PULCON
172600901011     C                   EXSR      CARCON
172700910719     C                   END
172800910722     C                   END
172900911125     C                   SETON                                        69
173000000000     C                   ENDSR
173100000608     C**-------------------------------------------------------------
173200000608     C** SCRIVE L'ARCIHVIO DEI PLICHI DA TRASMETTERE A SICURDATA
173300000608     C**-------------------------------------------------------------
173400000608     C     WRIPLI        BEGSR
173500020417
173600020417     C                   IF        V1Plic <> 0
173700020417
173800000612      *  LEGGE IL SFL PER SCRIVERE IL FILE DEL PLICO DA TRASMETTERE
173900020417     C                   IF        WrkLstRrnS4 > 0
174000020417      *
174100000612     C                   Z-ADD     0             NR4
174200000612     C                   Z-ADD     0             §TCONT
174300000612     C                   Z-ADD     0             NRGPLI            5 0
174400000612     C                   TIME                    FLD14            14 0
174500000612     C                   MOVEL     FLD14         ORAMIN            4 0
174600000612      *
174700000612     C                   DO        *HIVAL
174800000612     C                   ADD       1             NR4
174900000612     C     NR4           CHAIN     CN0W9S4                            63
175000000612      *
175100000612      *  PER FINE FILE ESCE DAL GIRO
175200000612     C     *IN63         IFEQ      *ON
175300000612     C                   LEAVE
175400000612     C                   END
175500000612      *
175600050405      **** Tolta gestione Plichi
175700000612      * IMPOSTA I CAMPI DI SCRITTURA DEL FNPLI00F
175800050405     C**** VNRTGL        IFGT      0
175900050405      ****
176000050405     C****               EXSR      SetFnPli
176100050405     C****               ADD       1             NRGPLI
176200050405     C****               Z-ADD     NRGPLI        PLIRIG
176300050405     C****               Z-ADD     HIMTGL        PLITAG
176400050405     C****               Z-ADD     VNRTGL        PLIQTA
176500050405     C****               Z-ADD     VIDIM         PLITIM
176600050405     C****               EXSR      WrtFnPli
176700050405      ****
176800050405     C****               ENDIF
176900000612      *
177000000612      * AGGIORNA IL SFL
177100000612     C     HIMTGL        COMP      0                                  20        PROT.VIDIM
177200000612     C                   SETON                                        40
177300000612     C                   UPDATE    CN0W9S4
177400000612     C*
177500000612     C                   END
177600020417     C*
177700020417     C                   ENDIF
177800020326
177900050405     **   Tolta gestione dei PLICHI
178000020326     ** Scrittura di una riga aggiuntiva per gli assegni.
178100050405     C********           IF        TotAss > 0
178200050405     C********           EXSR      SetFnPli
178300050405     C********           EVAL      PliRig = *HIVAL
178400050405     C********           EVAL      PliTim = TotAss
178500050405     C********           EXSR      WrtFnPli
178600050405     C********           ENDIF
178700050405      ********
178800020417     C                   ENDIF
178900020326
179000000608     C                   ENDSR
179100020326
179200020326     ***********************************************************************
179300020326     **
179400020326     ** Impostazione dei campi fissi di FNPLI000.
179500050405     **      TOLTA gestione dei PLICHI
179600020326     ***********************************************************************
179700050405     C*****SetFnPli      BEGSR
179800050405      ********
179900050405     C********           CLEAR                   FNPLI000
180000050405     C********           MOVEL     §FLE          PLIFIL
180100050405     C********           Z-ADD     V1PLIC        PLINUM
180200050405     C********           Z-ADD     *DATE         G02DAT
180300050405     C********           MOVE      *ZEROS        G02INV
180400050405     C********           MOVEL     *BLANK        G02ERR
180500050405     C********           CALL      'XSRDA8'
180600050405     C********           PARM                    WLBDAT
180700050405     C********           MOVE      G02INV        PLIDIM
180800050405     C********           MOVE      ORAMIN        PLIHIM
180900050405     C********           MOVEL     V1CDIV        PLIDIV
181000050405      ********
181100050405     C********           ENDSR
181200020326
181300020326     ***********************************************************************
181400020326     **
181500020326     ** Impostazione dei campi fissi di FNPLI000.
181600050405     **      TOLTA gestione dei PLICHI
181700020326     ***********************************************************************
181800050405     C*****WrtFnPli      BEGSR
181900050405      ********
182000050405     C********           ADD       1             NRPLI
182100050405     C********           WRITE     FNPLI000
182200050405      ********
182300050405     C********           ENDSR
182400050405      ********
182500000608     C**-------------------------------------------------------------
182600021118     C** CREAZIONE ARCHIVIO DEI VERSAMENTI GIORNALIERI
182700000608     C**-------------------------------------------------------------
182800021118     C     WRTVRT        BEGSR
182900021118     C                   Z-ADD     §FLE          VRTFLE
183000021118     C                   Z-ADD     DUTCOU        VRTCUC
183100021118     C                   Z-ADD     NUMVER        VRTNDV
183200021118     C                   Z-ADD     AMGUD8        VRTDDV
183300021127     C                   Z-ADD     AMGdop        VRTope
183400021121     C                   Z-ADD     KBNA          vrtkcc
183500021121     C                   Z-ADD     VBAN          vrtksc
183600021118     C                   MOVEL     V5CDIV        VRTVCA
183700021118     C                   Z-ADD     TOTCON        VRTCON
183800021118     C                   Z-ADD     TOTASS        VRTASS
183900021120     c                   clear                   vrtflr
184000021118     C                   WRITE     FNVRT000
184100021118     C                   ENDSR
184200021118     C**-------------------------------------------------------------
184300021118     C** CREAZIONE MOVIMENTI CONTABILI
184400021118     C**-------------------------------------------------------------
184500021118     C     CREMOV        BEGSR
184600000608     C*
184700991103     C                   MOVE      KNMUS         MOVNMU
184800991103     C                   MOVE      KNRAZ         MOVRIF
184900991103     C                   MOVEL     §FLE          MOVRIF
185000991103     C                   Z-ADD     VIDNRG        MOVSBN
185100991103     C                   ADD       1             REGSWD                         *PROGR.REG
185200991103     C                   Z-ADD     REGSWD        MOVSBR
185300991103     C                   MOVEL     'V'           MOVFGE
185400010312     C                   MOVEL     '201'         MOVSOC
185500991103     C                   MOVEL     *BLANKS       MOVFPN
185600991103     C                   MOVEL     §FLE          MOVFPN
185700991103     C                   MOVEL     'D'           MOVTMO
185800991103     C                   MOVEL     'CG'          MOVCTB
185900000000     C                   Z-ADD     VIDNRG        MOVNRG
186000000000     C                   Z-ADD     AMGDRG        MOVDRG
186100991103     C                   MOVE      'COGE'        MOVSRR
186200991103     C                   MOVEL     VCAU4         MOVCOD
186300991103     C                   MOVE      *ZEROS        MOVKCC
186400991103     C                   MOVE      *ZEROS        MOVKSC
186500991103     C                   MOVE      VKCC4         MOVKCC
186600991103     C                   MOVE      VKSC4         MOVKSC
186700021125     C                   MOVE      *ZEROS        MOVCCP
186800021125     C                   MOVE      *ZEROS        MOVSCP
186900991103     C* Dare/Avere
187000991103     C     VDA4          IFEQ      'D'
187100060403     C                   MOVE      VIDCC1        MOVCCP
187200060403     C                   MOVE      VIDSC1        MOVSCP
187300991103     C                   Z-ADD     1             MOVDAR
187400991103     C                   Z-ADD     0             MOVAVE
187500991103     C                   ELSE
187600021125     C                   MOVE      kbna          MOVCCP
187700021125     C                   MOVE      vban          MOVSCP
187800991103     C                   Z-ADD     0             MOVDAR
187900991103     C                   Z-ADD     1             MOVAVE
188000991103     C                   END
188100991103     C* Se la divisa di versamento è diversa da quella
188200991103     C* di conto effettuo la conversione per l'importo
188300991103     C* in ctb.
188400010626     C     V5CDIV        IFEQ      §GEDMC
188500991103     C                   Z-ADD     VIMP4         MOVIMM
188600991103     C                   ELSE
188700991103     C                   CLEAR                   YEURDS
188800991103     C                   MOVEL     V5CDIV        YECDVI
188900991103     C                   Z-ADD     VIMP4         YECIMI
189000010626     C                   MOVEL     §GEDMC        YECDVO
189100991103     C                   MOVEL     'H'           YECTAR
189200991103     C                   CALL      'YEURCO'
189300991103     C                   PARM                    YEURDS
189400991103     C                   Z-ADD     YECIMO        MOVIMM
189500991103     C                   END
189600991103     C                   Z-ADD     VIMP4         MOVIMD
189700991103     C                   MOVEL     V5CDIV        MOVDIV
189800991103     C                   Z-ADD     AMGDRG        MOVDRC
189900020321
190000020321     C                   IF        V4Plic = 0
190100020321     C                   Z-ADD     VIDNDS        MOVNDC
190200020321     C                   ELSE
190300020321     C                   Z-ADD     V4PLIC        MOVNDC
190400020321     C                   ENDIF
190500020321
190600991118     C                   Z-ADD     AMGDOP        MOVDDC
190700991103     C                   MOVE      *BLANKS       MOVSRD
190800000000     C                   Z-ADD     MOVNDC        MOVNRP
190900021126     c                   move      §4occ1        comcc1            4 0
191000021126     C     VKCC4         IFEQ      comcc1                                       INC. ATTRI
191100021126     C                   Z-ADD     VNRP4         MOVNRP                         2° LIV.
191200021126     C                   END
191300991120     C                   MOVE      VDRP4         G02DAT
191400991120     C                   MOVE      *BLANKS       G02ERR
191500991120     C                   CALL      'XSRDA8'
191600991120     C                   PARM                    WLBDAT
191700991120     C                   Z-ADD     G02INV        MOVDRP
191800991103     C                   MOVEL     *BLANKS       MOVSRP
191900010626     C                   MOVEL     §GEDMC        MOVDIP
192000991103     C                   Z-ADD     MOVIMM        MOVIPM
192100991105     C                   Z-ADD     MOVIMM        MOVIPD
192200991103     C                   Z-ADD     0             MOVDSC                         *
192300991103     C                   MOVEL     VDES4         MOVDES
192400991103     C                   MOVEL     *BLANKS       MOVCDC
192500991103     C                   Z-ADD     0             MOVDCA
192600991103     C                   Z-ADD     MOVDRG        MOVDOP
192700010626     C                   Z-ADD     1             MOVCMB
192800030403     C                   ADD       1             NRMOV             7 0
192900991103     C                   WRITE     FNMOV000
193000000000     C                   ENDSR
193100991103     C*-------------------------------------------------------
193200991103     ** Sottometto trasmissioni
193300991103     **-------------------------------------------------------
193400991103     C     SBMTRA        BEGSR
193500991104     C*
193600020926     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
193700991104     C* da/a cui spedire
193800020926     C                   CLEAR                   TIBS55
193900991104     C                   MOVEL     'L'           I50TLA
194000991104     C                   MOVEL     KNSIF         I50PSI
194100060421     C                   MOVEL     §fle          I50PPO
194200991104     C                   MOVEL     '046'         I50APO
194300020926     C                   CALL      'TIBS55R'
194400020926     C                   PARM                    TIBS55
194500991103     C*
194600991104     C                   MOVEL     '046'         CMZDST                          DEST.:SED
194700991104     C                   MOVEL     'FNMOV00T'    CMZFLD                          DA: >>>
194800020926     C                   EVAL      CmzMbD = 'M' + %EDITC(§Fle:'X') + CmzDst
194900991104     C                   MOVEL     O50PLA        CMZLBD
195000991104     C                   MOVEL     'FNMOV00R'    CMZFLA                          A: >>>
195100020926     C                   MOVEL     CmzMbD        CMZMBA
195200991104     C                   MOVEL     O50ALA        CMZLBA
195300021001     C                   MOVEL     'GAITRA'      CMZLBA
195400021001     C                   MOVE      O50ALA        WQUATR            4
195500021001     C     WQUATR        IFEQ      '101 '
195600021001     C                   MOVE      '201 '        CMZLBA
195700021001     C                   ENDIF
195800991104     C                   MOVE      '0000'        CMZORI                          ORA
195900991104     C                   MOVE      '0000000'     CMZDTI                          DATA
196000991104     C                   MOVE      'N'           CMZURG                          NOTTE
196100991104     C                   MOVE      'A'           CMZADD                          IN ADD
196200991104     C                   MOVE      'S'           CMZCLR                          CLEAR
196300991104     C                   MOVE      'S'           CMZFTR                          DA TRANS.
196400991104     C                   MOVE      *BLANK        CMZERR                          1= ERR.
196500991108     C                   MOVE      'Y029'        CMZAZN                          AZIONE
196600991104     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
196700021001     C                   MOVEL     'GAITRA'      CMZSYF
196800021001     C                   MOVE      O50ASI        WQUATR
196900021001     C     WQUATR        IFEQ      '101 '
197000021001     C                   MOVE      '201 '        CMZSYF
197100021001     C                   ENDIF
197200991104     C                   MOVE      NRMOV         CMZNRR                          N.REC.
197300991104     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-
197400991104     C*
197500991104     C                   CLEAR                   W236
197600020926     C                   CALL      'TIBS53C'
197700991104     C                   PARM                    O50LCL
197800991104     C                   PARM                    CMZDST
197900991104     C                   PARM                    CMZFLD
198000991104     C                   PARM                    CMZMBD
198100991104     C                   PARM                    CMZLBD
198200991104     C                   PARM                    CMZFLA
198300991104     C                   PARM                    CMZMBA
198400991104     C                   PARM                    CMZLBA
198500991104     C                   PARM                    CMZORI
198600991104     C                   PARM                    CMZDTI
198700991104     C                   PARM                    CMZURG
198800991104     C                   PARM                    CMZADD
198900991104     C                   PARM                    CMZCLR
199000991104     C                   PARM                    CMZFTR
199100991104     C                   PARM                    CMZERR
199200991104     C                   PARM                    CMZAZN
199300991104     C                   PARM                    CMZSYF
199400991104     C                   PARM                    CMZNRR
199500991104     C                   PARM                    CMZPRR
199600991104     C                   PARM                    W236            236
199700991103     C*
199800991103     C                   ENDSR
199900000612     C**---------------------------------------------------------------
200000000000     C** STAMPA DISTINTA BANCA
200100000612     C**---------------------------------------------------------------
200200000000     C     STADIS        BEGSR
200300910726     C                   EXCEPT    TESTA
200400910719     C                   DO        TREC          CNT
200500991103     C     CNT           CHAIN     CN0W9S2                            20
200600930927     C                   SETOFF                                       54
200700930927     C*
200800930927     C*  CONTROLLO SE ASSEGNO STESSO/ALTRO ISTITUTO
200900930927     C*
201000910731     C     VANN          IFNE      'E'
201100911127+++  C     VASS          ANDNE     0
201200930927     C*
201300930927     C                   SETON                                        54
201400930927     C*
201500930927     C*  CONTROLLO SE ASSEGNO STESSO/ALTRO ISTITUTO
201600930927     C     ISTABI        IFEQ      VABI
201700930927     C                   SETON                                        53
201800930927     C                   ELSE
201900930927     C                   SETOFF                                       53
202000930927     C                   END
202100991105     C                   Z-ADD     VDTE          GMADAS            8 0
202200000000     C                   Z-ADD     1             A                 3 0
202300910719     C                   MOVEL     VTPA          LKP
202400910719     C                   MOVE      VTPP          LKP
202500910719     C     LKP           LOOKUP    CSP(A)                                 21
202600000000     C  N21              Z-ADD     1             A
202700000000     C                   SETOFF                                       22
202800910719     C     VTPP          IFEQ      'F'
202900991104     C                   ADD       VASS          TOTFUO           12 2
203000000000     C                   ELSE
203100991104     C                   ADD       VASS          TOTPIA           12 2
203200000000     C                   SETON                                        22
203300000000     C                   END
203400000000     C** TOTALE ASSEGNO PER TIPO
203500000000     C                   ADD       1             CONRIG            2 0
203600000000     C     CONRIG        IFGT      50
203700000000     C                   EXCEPT    TESTA
203800000000     C                   Z-ADD     1             CONRIG
203900000000     C                   END
204000991104     C* Controllo se importo con decimali
204100991104     C                   MOVE      VASS          WCOMO2            2 0
204200991104     C     WCOMO2        COMP      0                                      02
204300000000     C                   EXCEPT    DET
204400910719     C                   END
204500910726     C                   END
204600991104     C                   MOVE      TOTPIA        WCOMO2            2 0
204700991104     C     WCOMO2        COMP      0                                      03
204800991104     C                   MOVE      TOTFUO        WCOMO2            2 0
204900991104     C     WCOMO2        COMP      0                                      04
205000991104     C                   MOVE      TOTASS        WCOMO2            2 0
205100991104     C     WCOMO2        COMP      0                                      06
205200000000     C                   EXCEPT    TOT
205300000000     C                   ADD       2             CONRIG
205400000000     C** totale assegni fine elenco
205500000000     C     TOTCON        IFGT      0
205600900518     C*  DETTAGLIO SFL CONTANTE
205700991103     C                   Z-ADD     0             NR4
205800900518     C                   DO        *HIVAL
205900991103     C                   ADD       1             NR4
206000991103     C     NR4           CHAIN     CN0W9S4                            63
206100910731     C     *IN63         IFEQ      '0'
206200900518     C     HIDTIP        ANDEQ     'C'
206300900518     C                   ADD       1             CONRIG            2 0
206400900518     C     CONRIG        IFGT      50
206500900518     C                   EXCEPT    TESTA
206600900518     C                   Z-ADD     1             CONRIG
206700900518     C                   END
206800991105     C* Controllo se importo da stampare a decimali
206900991105     C                   MOVE      VIDIM         WCOMO2
207000991105     C     WCOMO2        COMP      0                                      08
207100060421     c                   eval      stanrt = vnrtgl + vnrman
207200900518     C                   EXCEPT    DETSFL
207300900518     C                   END
207400910731     C  N63              END
207500991105     C* Controllo se importo da stampare a decimali
207600991105     C                   MOVE      TOTCON        WCOMO2
207700991105     C     WCOMO2        COMP      0                                      09
207800000000     C                   EXCEPT    TOT2
207900000000     C                   END
208000000000     C** STAMPA TOTALE CONTANTI
208100000000     C                   DO        20            A
208200000000     C     IMP(A)        IFGT      0
208300930927     C                   SETON                                        53
208400000000     C                   ADD       1             CONRIG
208500000000     C     CONRIG        IFGT      50
208600000000     C                   EXCEPT    TESTA
208700000000     C                   Z-ADD     1             CONRIG
208800000000     C                   END
208900991104     C                   MOVE      IMP(A)        WCOMO2
209000991104     C     WCOMO2        COMP      0                                      07
209100000000     C                   EXCEPT    TOT1
209200000000     C                   END
209300930927     C*
209400930927     C     IM1(A)        IFGT      0
209500930927     C                   SETOFF                                       53
209600930927     C                   ADD       1             CONRIG
209700930927     C     CONRIG        IFGT      50
209800930927     C                   EXCEPT    TESTA
209900930927     C                   Z-ADD     1             CONRIG
210000930927     C                   END
210100991104     C                   MOVE      IM1(A)        WCOMO2
210200991104     C     WCOMO2        COMP      0                                      07
210300930927     C                   EXCEPT    TOT1
210400930927     C                   END
210500930927     C*
210600000000     C                   END
210700991105     C* Controllo se importo da stampare a decimali
210800991105     C                   MOVE      TOTDIS        WCOMO2
210900991105     C     WCOMO2        COMP      0                                      02
211000991105     C                   MOVE      TOTVER        WCOMO2
211100991105     C     WCOMO2        COMP      0                                      04
211200000000     C** STAMPA TOTALI ASSEGNI PER SPECIE
211300000000     C                   EXCEPT    TOT3
211400000000     C** STAMPA TOTALI FINALI
211500000000     C     NOS           TAG
211600000000     C                   ENDSR
211700891205     C/EJECT
211800000000     C**----------------------------------
211900891204     C**  CARICAMENTO TABELLE
212000000000     C**----------------------------------
212100000000     C     CARTAB        BEGSR
212200991102     C*
212300991102     C*§ LEGGO TABELLA '4E'
212400991102     C                   MOVEL     '4E'          COD
212500991102     C     KTAB1         SETLL     TABEL                                  23
212600991102     C  N23              GOTO      NOTAB
212700991102     C                   Z-ADD     0             A
212800991102     C     SUTAB         TAG
212900991102     C     KTAB1         READE     TABEL                                  23
213000991102     C  N23              EXCEPT    RILTAB
213100991102     C   23              GOTO      NOTAB
213200991102     C                   MOVEL     TBLKEY        SPE               2
213300991102     C     SPE           IFNE      *BLANKS
213400991102     C                   ADD       1             A
213500991102     C                   MOVEL     TBLUNI        DS4E
213600991102     C                   MOVEL     SPE           CSP(A)
213700991102     C                   MOVEL     §SPDES        DSP(A)
213800991102     C                   MOVEL     §SPCAU        CAU(A)                         CAUSALE CO
213900991102     C     §SPCA2        IFNE      *BLANKS
214000991102     C                   MOVEL     §SPCA2        CA1(A)                         CAUSALE CO
214100991102     C                   ELSE
214200991102     C                   MOVEL     §SPCAU        CA1(A)                         CAUSALE CO
214300991102     C                   END
214400991102     C                   MOVEL     §SPCAU        OPECOD                         CAUSALE CO
214500991102     C                   MOVEL     'PG'          OPECD1
214600991102     C     KEYOPE        CHAIN     CNOPE000                           80
214700991102     C   80              GOTO      NOTAB
214800991102     C                   MOVEL     CA1(A)        OPECOD                         CAUSALE CO
214900991102     C                   MOVEL     'PG'          OPECD1
215000991102     C     KEYOPE        CHAIN     CNOPE000                           80
215100991102     C   80              GOTO      NOTAB
215200991102     C*
215300991102     C* CARICO CAUSALE PER FUORI PIAZZA SE NON SONO CAUSALI CED
215400991102     C*
215500991102     C                   MOVEL     SPE           CT                1
215600991102     C     CT            IFNE      '9'
215700991102     C                   ADD       1             A
215800991102     C                   MOVEL     SPE           CSP(A)
215900991102     C                   MOVE      'F'           CSP(A)
216000991102     C                   MOVEL     §SPDE1        DSP(A)
216100991102     C                   MOVEL     §SPCA1        CAU(A)                         CAUSALE CO
216200991102     C     §SPCA3        IFNE      *BLANKS
216300991102     C                   MOVEL     §SPCA3        CA1(A)                         CAUSALE CO
216400991102     C                   ELSE
216500991102     C                   MOVEL     §SPCA1        CA1(A)                         CAUSALE CO
216600991102     C                   END
216700991102     C                   MOVEL     §SPCA1        OPECOD                         CAUSALE CO
216800991102     C                   MOVEL     'PG'          OPECD1
216900991102     C     KEYOPE        CHAIN     CNOPE000                           80
217000991102     C   80              GOTO      NOTAB
217100991102     C                   MOVEL     CA1(A)        OPECOD                         CAUSALE CO
217200991102     C                   MOVEL     'PG'          OPECD1
217300991102     C     KEYOPE        CHAIN     CNOPE000                           80
217400991102     C   80              GOTO      NOTAB
217500991102     C                   END
217600991102     C*
217700991102     C*
217800991102     C                   END
217900991102     C                   GOTO      SUTAB
218000000000     C     NOTAB         TAG
218100991102     C*  Aggancio tabella 'GED' x reperire la divisa di conto
218200991102     C                   MOVEL     'GED'         KCOD1
218300991102     C                   MOVEL     *BLANKS       KKEY1
218400991102     C                   MOVEL     '1'           KKEY1
218500991102     C     KTBE          CHAIN     TNTBE01L                           13
218600991102     C     *IN13         IFEQ      '0'
218700991102     C                   MOVEL     TBEUNI        DGED
218800991102     C                   END
218900991102     C* leggo la tabella CV e reperisco la divisa alternativa a
219000991102     C* quella corrente
219100991102     C                   MOVEL     'CV'          COD
219200991102     C     KTAB1         CHAIN     TABEL00F                           50
219300991102     C     *IN50         DOWEQ     '0'
219400991102     C                   MOVEL     TBLUNI        DSCV
219500991103     C                   MOVEL     TBLKEY        WDIV              3
219600991102     C     §CVDVA        IFEQ      'S'
219700010626     C     §GEDCR        ANDNE     WDIV
219800991103     C                   MOVEL     WDIV          DIVALT            3
219900011220     C**                 Z-ADD     §CVCMB        CMBALT           13 6
220000991209     C                   Z-ADD     §CVDEC        DECALT            2 0
220100991102     C                   END
220200010626     C     §GEDCR        IFEQ      WDIV
220300991209     C                   Z-ADD     §CVDEC        DECDIV            2 0
220400991209     C                   END
220500991102     C     KTAB1         READE     TABEL00F                               50
220600991102     C                   END
220700991102     C*
220800000000     C                   ENDSR
220900891205     C*----------------------------------------------------*
221000911115     C* CARICAMENTO FILIALI VALIDE
221100911115     C*----------------------------------------------------*
221200911115     C     CARTB         BEGSR
221300991102     C*
221400020507     C                   MOVEL     '£1'          D06COD
221500060421     C                   MOVEL     simfel        D06KEY
221600020507     C                   MOVEL     TR06DS        KPJBU
221700020507     C                   CALL      'TRUL06R'
221800020507     C                   PARM                    KPJBA
221900020507     C                   MOVEL     KPJBU         TR06DS
222000020507     C                   MOVEA     LIN           fila
222100021115     c*
222200911115     C                   ENDSR
222201150108     C*----------------------------------------------------*
222202150108     C*  Verifica se assegni esclusi devono essere segnalati
222203150108     C*----------------------------------------------------*
222204150108     C     VerEsclusi    BEGSR                                                  *
222206150109     c                   move      vdte          dataeur
222208150109     c                   move      dataeur       dataisom
222209150108     c     dataiso       subdur    dataisom      diffe:*d          3 0
222210150129     c                   if        diffe > 6
222211150108     c                   z-add     1             xy
222212150108     c     vuoto         lookup    lista(xy)                              99
222213150109     c   99              eval      lista(xy) = 'Assegno: ' + vnra + vnra1
222214150112     c                             + 'data ' +%editc(vdte:'Y') + ' ' +
222215150112     c                             'ABI: ' + %editc(vabi:'X') + ' ' +
222216150112     c                             'CAB: '+  %editc(vcab:'X') + ' ' +
222218150112     c                             'Importo: ' + %editc(vass:'2') + ' ' + vdes
222219150109     c  n99              clear                   lista(10)
222220150108     c  n99              eval      lista(10) = 'Presenti altri assegni oltre 10'
222221150108     c                   endif
222222150108     c
222223150108     C                   endsr
222224150108     C*----------------------------------------------------*
222225150108     C*  invia mail di segnalazione assegni non versati
222226150108     C*----------------------------------------------------*
222227150108     C     Inviamail     BEGSR                                                  *
222228150112     C                   eval      wrkEml= 'gestione.clienti@brt.it'
222230150108
222231150108     C                   eval      wrkOgg='Segnalazione assegni non versati '+
222232150108     C                             'Filiale ' + %EDITC(§Fle:'X')
222233150108     C                   eval      wrkMsg='Attenzione il versamento effettu' +
222234150108     C                             'ato dal profilo ' + knmus +' ha escluso '+
222235150204     C                             'assegni da oltre 6 giorni' +':/N'+
222236150108     C                             lista(1) +':/N'+
222237150108     C                             lista(2) +':/N'+
222238150108     C                             lista(3) +':/N'+
222239150108     C                             lista(4) +':/N'+
222240150108     C                             lista(5) +':/N'+
222241150108     C                             lista(6) +':/N'+
222242150108     C                             lista(7) +':/N'+
222243150108     C                             lista(8) +':/N'+
222244150108     C                             lista(9) +':/N'+
222245150108     C                             lista(10)
222246150108     C*
222247150108     C                   call(e)   'TIS701C1'
222248150108     C                   parm                    wrkEml
222249150108     C                   parm                    wrkEmcc
222250150108     C                   parm                    wrkOgg
222251150108     C                   parm                    wrkMsg
222252150108     C                   parm                    wrkesit
222253150108     C                   endsr
222300940831     C*----------------------------------------------------*
222400940831     C*  OPERAZIONI INIZIALI                               *
222500940831     C*----------------------------------------------------*
222600940831     C     *INZSR        BEGSR                                                  *
222700940831     C*
222800940831     C     *ENTRY        PLIST
222900940831     C                   PARM                    KPJBA
223000000608      *
223100000608      * -----------------
223200000608      * IMPOSTA L'INDICATORE (66) PER CONTROLLARE L'AZIONE
223300000608      *  ORIGINALE O QUELLA NUOVA PER GESTIRE DIRETTAMENTE
223400000608      *   LA BANCA PER CONTANTE PER IL NUOVO GIRO POSTE.
223500000608     C     KCDAZ         COMP      '%0W9'                             6666
223600000608      * -----------------
223700000608      *
223800991104     C* Eseguo la Open dei file
223900021114     C                   OPEN      CNASS02L                             10
224000991104     C   10              GOTO      FINE
224100021121     C* Richiamo XPARUT
224200021121     C                   Z-ADD     1             CODUT
224300021121     C                   CALL      'X§PARUT'
224400021121     C                   PARM                    UTEDSE
224500021121     C                   MOVEL     RAGUT         RSUT2            25
224600021121     C                   MOVEL     REC40         CNCR40
224700021118     c     *dtaara       define    §azute        azuteds
224800021118     c     *dtaara       define    §datiute      ddatiute
224900021118     C                   in(E)     *dtaara
225000021118     C                   IF        %Error  or  RSUT = *blanks
225100021118     C                   call      'TIBS34R'
225200021118     C                   parm                    Tibs34Ds
225300021118     C                   in        *dtaara
225400021118     c                   ENDIF
225500050111     C*§ CONTROLLO SE SIAMO IN FILIALE
225600050111     c                   move      dutpou        §fle              3 0
225700060426     c                   if        dutlpo = 'S' or dutlpo = ' '
225800060426     c                   seton                                        19
225900060426     c                   end
226000060526     c                   if        dutlpo = '2'
226100060526     c                   seton                                        01
226200060526     c                   end
226300040921     C     dutpou        CHAIN     AZORG01L
226400941103     C*§§
226500941103     C                   TIME                    WHHUDT           14 0
226600991102     C                   MOVE      WHHUDT        G02DAT
226700991102     C                   MOVE      *ZEROS        G02INV
226800991102     C                   MOVEL     *BLANK        G02ERR
226900941103     C                   CALL      'XSRDA8'
227000991103     C                   PARM                    WLBDAT
227100991102     C                   Z-ADD     G02INV        AMGUD8            8 0
227200991102     C                   Z-ADD     G02DAT        GMAUD8            8 0
227300991103     C                   Z-ADD     0             REGSWD            5 0          *AZZER.RIG
227301150109     c                   move      amgud8        dataiso
227400940831     C*-----
227500940831     C                   SETON                                        05        COND.INTES
227600940831     C*-                                                   *
227700940831     C*--- RICERCA CAPOCONTI------------------------------------------*
227800100521     C                   DO        50            X
227900940831     C                   MOVE      TCU(X)        TCUDS
228000940831     C     F56           CABNE     'CG'          END1
228100940831     C     F4            IFEQ      '6'
228200940831     C                   Z-ADD     KCU(X)        KBNA              4 0
228300940831     C                   END
228400940831     C     F4            IFEQ      '4'
228500940831     C                   Z-ADD     KCU(X)        KCAS              4 0
228600940831     C                   END
228700940831     C     END1          TAG
228800940831     C                   END
228900940831     C*---------------------------------------------------------------*
229000940831     C*--- RICERCA RAG.SOCIALE CLIENTE     'CNACO00F       *
229100940831     C     KEYACO        KLIST                                                  *
229200940831     C                   KFLD                    CODUT                          *
229300940831     C                   KFLD                    KCC                            *
229400940831     C                   KFLD                    KSC                            *
229500940831     C                   Z-ADD     KBNA          KCC                            *
229600940831     C     KBAN          KLIST                                                  *
229700940831     C                   KFLD                    CODUT                          *
229800940831     C                   KFLD                    KBNA                           *
229900940831     C                   KFLD                    KSC
230000940831     C     KTAB          KLIST
230100940831     C                   KFLD                    CODUT
230200940831     C                   KFLD                    COD
230300940831     C                   KFLD                    KEY
230400940831     C     KEYOPE        KLIST                                                  * CAUSALE
230500940831     C                   KFLD                    CODUT                          *
230600940831     C                   KFLD                    OPECD1                         *
230700940831     C                   KFLD                    OPECOD                         *
230800991102     C     KTAB1         KLIST
230900940831     C                   KFLD                    CODUT
231000940831     C                   KFLD                    COD
231100991102     C     KTBE          KLIST
231200991102     C                   KFLD                    KCOD1
231300991102     C                   KFLD                    KKEY1
231400020729     C     KTBE1         KLIST
231500020729     C                   KFLD                    tbecod
231600020729     C                   KFLD                    tbeke1
231700020729     C                   KFLD                    tbeke2
231800940831     C     KABI§         KLIST
231900940831     C                   KFLD                    KABI              5 0
232000940831     C                   KFLD                    KCAB              5 0
232100940831     C     KASS          KLIST
232200940831     C                   KFLD                    §FLE
232300940831     C                   KFLD                    KNRA             10
232400940831     C                   KFLD                    KABI
232500940831     C                   KFLD                    KCAB
232600021120      *
232700021120     C     Kqig          KLIST
232800021120     C                   KFLD                    qigann
232900021120     C                   KFLD                    §fle
233000021120     C                   KFLD                    amgud8
233100021120     c                   clear                   qigann
233200940831     C*
233300050405     C***** Tolta gestione dei PLICHI
233400020103      * con l'euro è necessario testare solo senza la divisa
233500020103      * per dare l'errore di Plico già utilizzato
233600050405     C**** KPLIN         KLIST
233700050405     C*******            KFLD                    KPOC
233800050405     C*******            KFLD                    KPLI
233900000612     C*
234000940831     C     *LIKE         DEFINE    ACOKCC        KCC
234100940831     C     *LIKE         DEFINE    ACOKSC        KSC
234200940831     C     *LIKE         DEFINE    TBLCOD        COD
234300940831     C     *LIKE         DEFINE    TBLKEY        KEY
234400991102     C     *LIKE         DEFINE    TBECOD        KCOD1
234500991102     C     *LIKE         DEFINE    TBEKE1        KKEY1
234600940831     C     *LIKE         DEFINE    TOTCON        §TCONT
234700940831     C     *LIKE         DEFINE    INDABI        ISTABI
234800940831     C     *LIKE         DEFINE    INDCAB        ISTCAB
234900991120     C* REPERISCO DATA DEL GIORNO
235000991120     C                   TIME                    WHHUDT           14 0
235100991120     C                   MOVEL     WHHUDT        ORA               6 0
235200991120     C                   MOVE      WHHUDT        G02DAT
235300991120     C                   MOVE      *ZEROS        G02INV
235400991120     C                   MOVEL     *BLANK        G02ERR
235500991120     C                   CALL      'XSRDA8'
235600991120     C                   PARM                    WLBDAT
235700991120     C                   Z-ADD     G02INV        OGGI              8 0
235800991122     C                   Z-ADD     G02DAT        WTODAY            8 0
235900980130     C* Richiamo pgm per controllo modificabilità tabella
236000980130     C                   CLEAR                   TRUL12
236100980130     C                   MOVEL     'L'           I12TLA
236200980130     C                   MOVEL     KNSIF         I12NSI
236300980130     C                   MOVEL     '4D'          I12COD
236400980130     C                   CALL      'TRUL12R'
236500980130     C                   PARM                    TRUL12
236600980130     C     O12ERR        IFEQ      '1'
236700980130     C                   GOTO      FINE
236800980130     C                   END
236900980130     C*----------------------------------------------------*
237000940831     C                   MOVE      *ZEROS        AMGDRG
237100940831     C                   EXSR      CARTAB
237200940831     C                   EXSR      CARTB
237300991102     C   80              MOVEL     '1'           *INKC
237400010626     C                   MOVEL     §GEDCR        V1CDIV
237500991102     C*
237600991103     C* 2 GG PRIMA LA DATA DEL GIORNO CASSA SEGNALO
237700991104     C                   Z-ADD     0             NRMOV             7 0
237800000612     C                   Z-ADD     0             NRPLI             7 0
237900991103     C                   Z-ADD     0             D52DFI                         *
238000991103     C                   Z-ADD     1             D52GIO                         *
238100991103     C                   MOVE      AMGUD8        D52DIN                         *
238200991103     C                   MOVE      '-'           D52SGN                         *
238300991103     C                   CALL      'TRUL52R'
238400991103     C                   PARM                    TRUL52
238500991103     C                   MOVE      D52DFI        DAT1GG            8 0          *
238600940831     C                   ENDSR
238700020418
238800020418     ***********************************************************************
238900000000     ODISBAN1   E            TESTA            02
239000891205     O                       RSUT2
239100000000     O                                           60 '**---------------------'
239200000000     O                                              '------------**'
239300020418
239400000000     O          E            TESTA            03
239500891205     O                       SIMVIA
239600060421     O                                           60 '**  DISTINTA  VERSAMENT'
239700060421     O                                              'O IN BANCA  **'
239800060421     O*              66                          60 '**      DISTINTA  VERSA'
239900060421     O*              66                             'MENTO       **'
240000000000     O                       PAGE          Z    132
240100000000     O                                          128 'PAG.'
240200991103     O                                          120 'CNC0W9R'
240300020418
240400000000     O          E            TESTA            04
240500891205     O                       SIMCIT
240600000000     O                                           60 '**---------------------'
240700000000     O                                              '------------**'
240800020418
240900000000     O          E            TESTA            05
241000000000     O                                              'Versamento a mezzo Banca'
241100000000     O                       KCC           Z   +  1
241200910726     O                       VBAN          Z   +  1
241300000000     O                       VIDBAN            +  1
241400891205     O                                         +  1 'C/C'
241500891205     O                       INDCCB            +  1
241600991103     O                                         +  1 'Divisa Versamento:'
241700991103     O                       V1CDIV            +  1
241800020418
241900020418     O          E            TESTA          1 06
242000000000     O                                              'Distinta N.'
242100000000     O                       VIDNDS        Z   +  1
242200000000     O                                         +  1 'del'
242300991103     O                       VIDDDS            +  1 '  /  /    '
242400020418     O                                           98 'Plico n.'
242500020418     O                       V1PLIC        Z   +  1
242600020418
242700000616     O          E   N66      TESTA            08
242800000616     O                       SC(1)               66
242900000616     O                       SC(2)              132
243000020418
243100000616     O          E   N66      TESTA            09
243200000616     O                       SC(3)               66
243300000616     O                       SC(4)              132
243400020418
243500000616     O          E   N66      TESTA            10
243600000616     O                       SC(5)               66
243700000616     O                       SC(6)              132
243800020418
243900000616     O          E   N66      TESTA            11 12
244000000616     O                       SC(7)               66
244100000616     O                       SC(8)              132
244200020418     **---------------------------------------------------------------------
244300000616     O          E   N66      DET         1
244400910801     O                       VTPA                 3
244500910719     O                       VTPP                 4
244600910719     O                       VNRA                13
244700910719     O                       VNRA1               18
244800991105     O                       GMADAS              29 '  /  /    '
244900991104     O                       VDES                50
245000991104     O                       VPIA                62
245100991104     O               22      VASS          2     77
245200991104     O               22 02                       77 '   '
245300991104     O              N22      VASS          2     92
245400991104     O              N22 02                       92 '   '
245500991104     O               53 54                      102 'STESSO'
245600991104     O              N53 54                      102 'ALTRO '
245700020418     **---------------------------------------------------------------------
245800000616     O          E   N66      TOT         2
245900000000     O                       TOTPIA        2     77
246000991104     O                  03                       77 '   '
246100000000     O                       TOTFUO        2     92
246200991104     O                  04                       92 '   '
246300000000     O                                         +  1 '- Totale Assegni -'
246400000000     O                       TOTASS        2   +  1
246500991104     O                  06                      132 '   '
246600020418
246700000616     O          E   N66      TOT         0
246800000000     O                       TOTPIA        2     77
246900991104     O                  03                       77 '   '
247000000000     O                       TOTFUO        2     92
247100991104     O                  04                       92 '   '
247200000000     O                                         +  1 '- Totale Assegni -'
247300991104     O                       TOTASS        2    132
247400991104     O                  06                      132 '   '
247500020418
247600000616     O          E   N66      TOT         0  2
247700000000     O                       TOTPIA        2     77
247800991104     O                  03                       77 '   '
247900000000     O                       TOTFUO        2     92
248000991104     O                  04                       92 '   '
248100000000     O                                         +  1 '- Totale Assegni -'
248200991104     O                       TOTASS        2    132
248300991104     O                  06                      132 '   '
248400020418     **---------------------------------------------------------------------
248500000616     O          E   N66      TOT1        1
248600000000     O                       CSP(A)               3
248700000000     O                                         +  1 '->'
248800000000     O                       DSP(A)            +  1
248900991105     O               53                          48 'STESSO ISTITUTO'
249000991105     O              N53                          48 'ALTRO  ISTITUTO'
249100991104     O               53      IMP(A)        2     68
249200991104     O              N53      IM1(A)        2     68
249300991104     O                  07                       68 '   '
249400991104     O                       V1CDIV              72
249500020418     **---------------------------------------------------------------------
249600900518     O          E            DETSFL         1
249700900518     O                                         +  1 'Nr.'
249800060421     O                       stanrt        Z   +  1
249900900518     O                       VDESCR            +  1
250000991104     O                       VIDIM         2     68
250100991104     O                  08                       68 '   '
250200020418     **---------------------------------------------------------------------
250300900518     O          E            TOT2        1  2
250400000000     O                                         +  1 'Totale Contante'
250500991104     O                       TOTCON        2     68
250600991104     O                  09                       68 '   '
250700020418     **---------------------------------------------------------------------
250800000000     O          E            TOT3        1
250900000000     O                                              '------------------------'
251000000000     O                                              '------------------------'
251100930927     O                                              '------------------------'
251200020418
251300000000     O          E            TOT3        1
251400000000     O                                         +  1 'Totale Distinta'
251500991104     O                       TOTDIS        2     68
251600991104     O                  02                       68 '   '
251700991104     O                       V1CDIV              72
251800991104     O                  03                       68 '   '
251900020418
252000000000     O          E            TOT3        2
252100000000     O                                         +  1 'Importo da Versare'
252200991104     O                       TOTVER        2     68
252300991104     O                  04                       68 '   '
252400991104     O                       V1CDIV              72
252500020418
252600000000     O          E            TOT3        0
252700000000     O                                         +  1 'Importo da Versare'
252800991104     O                       TOTVER        2     68
252900991104     O                  04                       68 '   '
253000991104     O                       V1CDIV              72
253100020418
253200000000     O          E            TOT3        0
253300000000     O                                         +  1 'Importo da Versare'
253400991104     O                       TOTVER        2     68
253500991104     O                  04                       68 '   '
253600991104     O                       V1CDIV              72
253700020418
253800000000     O          E            TOT3             65
253900000000     O                                          130 'COPIA USO BANCA'
254000020418     ***********************************************************************
254100000000     ODISBAN3   E            TESTA            02
254200891205     O                       RSUT2
254300000000     O                                           60 '**---------------------'
254400000000     O                                              '------------**'
254500020418
254600000000     O          E            TESTA            03
254700891205     O                       SIMVIA
254800060421     O                                           60 '**  DISTINTA  VERSAMENT'
254900060421     O                                              'O IN BANCA  **'
255000060421     O*              66                          60 '**      DISTINTA  VERSA'
255100060421     O*              66                             'MENTO       **'
255200000000     O                       PAGE3         Z    132
255300000000     O                                          128 'PAG.'
255400991103     O                                          117 'CNC0W9R'
255500020418
255600000000     O          E            TESTA            04
255700891205     O                       SIMCIT
255800000000     O                                           60 '**---------------------'
255900000000     O                                              '------------**'
256000020418
256100000000     O          E            TESTA            05
256200000000     O                                              'Versamento a mezzo Banca'
256300000000     O                       KCC           Z   +  1
256400910726     O                       VBAN          Z   +  1
256500000000     O                       VIDBAN            +  1
256600891205     O                                         +  1 'C/C'
256700891205     O                       INDCCB            +  1
256800991104     O                                         +  1 'Divisa Versamento:'
256900991104     O                       V1CDIV            +  1
257000020418
257100020418     O          E            TESTA          1 06
257200000000     O                                              'Distinta N.'
257300000000     O                       VIDNDS        Z   +  1
257400000000     O                                         +  1 'del'
257500991103     O                       VIDDDS            +  1 '  /  /    '
257600020418     O                                           98 'Plico n.'
257700020418     O                       V1PLIC        Z   +  1
257800020418
257900000616     O          E   N66      TESTA            08
258000000000     O                       SC(1)               66
258100000000     O                       SC(2)              132
258200020418
258300000616     O          E   N66      TESTA            09
258400000000     O                       SC(3)               66
258500000000     O                       SC(4)              132
258600020418
258700000616     O          E   N66      TESTA            10
258800000000     O                       SC(5)               66
258900000000     O                       SC(6)              132
259000020418
259100000616     O          E   N66      TESTA            11 12
259200000000     O                       SC(7)               66
259300000000     O                       SC(8)              132
259400020321
259500020418     **---------------------------------------------------------------------
259600000616     O          E   N66      DET         1
259700910801     O                       VTPA                 3
259800910719     O                       VTPP                 4
259900910719     O                       VNRA                13
260000910719     O                       VNRA1               18
260100991105     O                       GMADAS              29 '  /  /    '
260200991104     O                       VDES                50
260300991104     O                       VPIA                62
260400991104     O               22      VASS          2     77
260500991104     O               22 02                       77 '   '
260600991104     O              N22      VASS          2     92
260700991104     O              N22 02                       92 '   '
260800991104     O               53 54                      102 'STESSO'
260900991104     O              N53 54                      102 'ALTRO '
261000930927     O                       VNOT               132
261100020418     **---------------------------------------------------------------------
261200000616     O          E   N66      TOT         2
261300000000     O                       TOTPIA        2     77
261400991104     O                  03                       77 '   '
261500000000     O                       TOTFUO        2     92
261600991104     O                  04                       92 '   '
261700911127     O                                         +  1 '- Totale Assegni -'
261800991104     O                       TOTASS        2    132
261900991104     O                  06                      132 '   '
262000020418
262100000616     O          E   N66      TOT         0
262200000000     O                       TOTPIA        2     77
262300991104     O                  03                       77 '   '
262400000000     O                       TOTFUO        2     92
262500991104     O                  04                       92 '   '
262600000000     O                                         +  1 '- Totale Assegni -'
262700991104     O                       TOTASS        2    132
262800991104     O                  06                      132 '   '
262900020418
263000000616     O          E   N66      TOT         0  2
263100000000     O                       TOTPIA        2     77
263200991104     O                  03                       77 '   '
263300000000     O                       TOTFUO        2     92
263400991104     O                  04                       92 '   '
263500000000     O                                         +  1 '- Totale Assegni -'
263600991104     O                       TOTASS        2    132
263700991104     O                  06                      132 '   '
263800020418     **---------------------------------------------------------------------
263900000616     O          E   N66      TOT1        1
264000000000     O                       CSP(A)               3
264100000000     O                                         +  1 '->'
264200000000     O                       DSP(A)            +  1
264300991105     O               53                          48 'STESSO ISTITUTO'
264400991105     O              N53                          48 'ALTRO  ISTITUTO'
264500991104     O               53      IMP(A)        2     68
264600991104     O              N53      IM1(A)        2     68
264700991104     O                  07                       68 '   '
264800991104     O                       V1CDIV              72
264900020418     **---------------------------------------------------------------------
265000900518     O          E            DETSFL         1
265100900518     O                                         +  1 'Nr.'
265200060421     O                       stanrt        Z   +  1
265300900518     O                       VDESCR            +  1
265400991104     O                       VIDIM         2     68
265500991104     O                  08                       68 '   '
265600020418     **---------------------------------------------------------------------
265700900518     O          E            TOT2        1  2
265800000000     O                                         +  1 'Totale Contante'
265900991104     O                       TOTCON        2     68
266000991104     O                  09                       68 '   '
266100991104     O                       V1CDIV              72
266200020418     **---------------------------------------------------------------------
266300000000     O          E            TOT3        1
266400000000     O                                              '------------------------'
266500000000     O                                              '------------------------'
266600930927     O                                              '------------------------'
266700020418
266800000000     O          E            TOT3        1
266900000000     O                                         +  1 'Totale Distinta'
267000991104     O                       TOTDIS        2     68
267100991104     O                  02                       68 '   '
267200991105     O                       V1CDIV              72
267300020418
267400000000     O          E            TOT3        2
267500000000     O                                         +  1 'Importo da Versare'
267600991104     O                       TOTVER        2     68
267700991104     O                  04                       68 '   '
267800991104     O                       V1CDIV              72
267900020418
268000000000     O          E            TOT3        0
268100000000     O                                         +  1 'Importo da Versare'
268200991104     O                       TOTVER        2     68
268300991104     O                  04                       68 '   '
268400991104     O                       V1CDIV              72
268500020418
268600000000     O          E            TOT3        0
268700000000     O                                         +  1 'Importo da Versare'
268800991104     O                       TOTVER        2     68
268900991104     O                  04                       68 '   '
269000991104     O                       V1CDIV              72
269100020418
269200000000     O          E            TOT3        3
269300000000     O                                         +  1 'N.Registrazione'
269400000000     O                       VIDNRG        Z   +  1
269500000000     O                                         +  1 'del'
269600991103     O                       VIDDRG            +  1 '  /  /    '
269700930225     O                                         +  2 'Operaz. del'
269800991118     O                       VDTOPE            +  1 '  /  /    '
269900020418
270000000000     O          E            TOT3             65
270100000000     O                                          130 'COPIA USO INTERNO'
270200020418     ***********************************************************************
270300991103     OTABEL     E            RILTAB
270400020418     ***********************************************************************
270500991104**
270600991104------+-----------+----------+--------------------+-----------+---
270700930927-----------+--------------+----------+---------------------------+
270800000000SPECIE    NUMERO     DATA    BANCA TRASSATA        PIAZZA       IM
270900930927PORTO ASS.   IMPORTO ASS.   ISTITUTO   N O T E                   '
271000000000          ASSEGNO  ASSEGNO                         PAGAMENTO    SU
271100930927 PIAZZA      FUORI PIAZZA                                        '
271200991105------+-----------+----------+--------------------+-----------+---
271300930927-----------+--------------+----------+---------------------------+
271400951027** SCHIERA ERRORI
271500991102Già inserito assegno. Eseguire chius.operatore/presa in carico mandato d'introto    01
271600991102Già inserito assegno in questa distinta.                                            02
271700991102Già inserito assegno con questi riferimenti intestato al mittente.                  03
271800991102ERRORE !! Manca il numeratore della distinta !!!!                                   04
271900991102Divisa di versamento non valida                                                     05
272000991103Precedente trasmissione di  movimenti contabili terminata in modo anomalo           06
272100000609Impostare un nr.PLICO corretto                                                      07
272200000612Con questo n°Plico sono stati già inseriti altri dati! Usare un altro Numero.       08
272300060403ERRORE! Non è possibile continuare. Esiste già quadratura giornaliera.              09
272400991104**
272500020926CHKOBJ OBJ(FNMOV00T) OBJTYPE(*FILE) MBR(M000000)
272600020926ADDPFM FILE(FNMOV00T)               MBR(M000000)
272700020926OVRDBF FILE(FNMOV00T)               MBR(M000000)
272800020926ALCOBJ OBJ((FNMOV00T *FILE *EXCL        M000000))
272900020926DLCOBJ OBJ((FNMOV00T *FILE *EXCL        M000000))
