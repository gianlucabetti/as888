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
003100150313     F*----------
003200150313     Ftnass00f  o    E             DISK
003300991102     F*----------
003400050405     F*****FNPLI00F  O  A E             DISK
003500050405     F*****                                     COMMIT
003600050405     ******FFNPLI01L  IF   E           K DISK
003700050405     F*****                                     RENAME(FNPLI000:FNPLI001)
003800000609     F*----------
003900060412     Fyfqig01L  IF   E           K DISK
004000040921     FAZORG01L  IF   E           K DISK
004100021120     F*----------
004200930222     FDISBAN1   O    F  132        PRINTER
004300930222     FDISBAN2   O    F  132        PRINTER
004400930222     FDISBAN3   O    F  132        PRINTER
004500991102     F*----------
004600991102     FCNC0W9D   CF   E             WORKSTN
004700991102     F                                     SFILE(CN0W9S2:NR2)
004800991102     F                                     SFILE(CN0W9S4:NR4)
004900991102     F                                     SFILE(CN0W9S5:NR5)
005000991102?     *--------------------------------------------------------------*
005100991102     D SC              S             66    DIM(8) CTDATA PERRCD(1)
005200930927     D CSP             S              2    DIM(20)                              COD.SPECIE
005300000000     D DSP             S             25    DIM(20)                              DES.SPECIE
005400891201     D CAU             S              4    DIM(20)                              CAUSALE CO
005500930927     D CA1             S              4    DIM(20)                              CAUS.CONTA
005600991104     D IMP             S             15  2 DIM(20)                              IMPORTI PE
005700991104     D IM1             S             15  2 DIM(20)                              IMPORTI PE
005800020507     D FILA            S              3  0 DIM(30)                              FIL TBLUNI
005900891205     D OPZ             S              1    DIM(10)                              FLAGS ANAG
006000910722     D SKI             S              1    DIM(3)                               SKIERA DI
006100150319     D ERR             S             80    DIM(11) CTDATA PERRCD(1)              SK ERRORI
006200020926     D CMDD            S             49    DIM(5) CTDATA PERRCD(1)              CMD Exec
006300150112     D lista           S            100    DIM(10)                              FIL TBLUNI
006400150112     D vuoto           S            100
006500150108     D xy              S              2  0
006600150330     D dtaiso          S               d   datfmt(*iso)
006700150330     D dtaeur          S               d   datfmt(*eur)
006800150108      *
006900000000     D DTAVER          DS
007000000000     D  NUMVER                 1      6  0
007100060403     D DGEI          E DS
007200991104     D DSCMZ         E DS
007300061120     D CNC0WD00S     E DS
007400000000     D TCUDS           DS
007500000000     D  F1                     1      1
007600000000     D  F3                     3      3
007700000000     D  F2                     2      2
007800000000     D  F4                     4      4
007900000000     D  F56                    5      6
008000991103     D YEURDS        E DS                  EXTNAME(YEURCODS)
008100991103     D TRUL52        E DS                  EXTNAME(TRUL52DS)
008200991103     D TRUL12        E DS                  EXTNAME(TRUL12DS)
008300910718     D*-
008400991103     D WLBDAT          DS
008500991102     D  G02DAT                 1      8  0
008600991102     D  G02INV                 9     16  0
008700991102     D  G02ERR                17     17
008800991102     D  G02TGI                18     22  0
008900941104      *
009000941104     D                SDS
009100941104     D  §PGM                   1     10
009200891205?    D*----------------------------------------------------*
009300021118     D ddatiute      e ds
009400021118     D azuteds       e ds                  extname(AZUTE00F)
009500021118     D tibs34ds      E DS                  inz
009600991103     D CNCR40        E DS
009700020507     D TR06DS        E DS                  EXTNAME(TRUL06DS)
009800020507     D  LIN                    1     90  0 dim(30)
009900000000     D KPJBA         E DS
010000000000     D  LIBSYS                92    100
010100000000     D  MSGERR               453    502
010200900403     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
010300000000     D  TCU                  398    697
010400000000     D                                     DIM(50)                              TAB FLAG C
010500000000     D  KCU                  698    847P 0
010600000000     D                                     DIM(50)                              TAB CAPO C
010700000000     D                                     PACKEVEN
010800020926     D TIBS55        E DS                  EXTNAME(TIBS55DS)
010900020729     D Dy4O          E DS
011000991118     D DS1A          E DS
011100910719     D DS4E          E DS
011200991102     D DSCV          E DS
011300011016     D DSCW          E DS
011400991102     D DGED          E DS
011500000621     D SPICCI          C                   CONST('Spiccioli')
011600011016     D*
011700150313     D wban            S                   like(vban)
011800060403     D §abb            S                   like(vass)
011900150109     D dataeur         S               d   datfmt(*eur)
012000150108     D dataiso         S               d   datfmt(*iso)
012100150108     D dataisom        S               d   datfmt(*iso)
012200060411     D PrmTpInc        S              2
012300011016     D PrmDivisa       S              3
012400011016     D PrmData         S               D
012500011016     D PrmEsito        S              1
012600011016     D PrmDS1A         S
012700011016     D                                     LIKE(DS1A)
012800011016     D PrmDSCW         S
012900011016     D                                     LIKE(DSCW)
013000020417     D WrkLstRrnS4     S              5I 0
013100020422     D WrkDtOpe        S               D
013200020422     D                                     DATFMT(*EUR)
013300020422     D WrkDtCur        S               D
013400020422     D WrkMonth        S              5I 0
013500060414     D chkDATAASSEGNO  PR            10I 0
013600060414     D  dte                          10D   DATFMT(*ISO)
013700060414     D  SAS
013800060414     D                                     LIKE(§1ASAS)
013900060414     D  esito                        10I 0
014000060414     D                                     OPTIONS(*OMIT)
014100060414     D  errc                          1A
014200060414     D                                     DIM(10)
014300060414     D  msgc                         80A   OPTIONS(*OMIT)
014400060414     D                                     DIM(10)
014500060414     D dte             S               d
014600060414     D sas             S                   like(§1asas)
014700060414     D esito           S             10I 0
014800060414     D errc            S              1    DIM(10)                              SKI CHIAVI
014900060414     D msgc            S             80    DIM(10)                              SKI CHIAVI
015000130409     D stanrt          S                   like(vnrman)
015100060824     D okcon           S              1    inz
015200150407     D wfle            S                   like(§fle)
015300100521     d* ordinamento
015400100521     d NotUsed         S             16A
015500100521     d ReturnSize      S              9B 0
015600100521     d SizeList        S              9B 0
015700100521     d WrkSort         S              1
015800150108     d wrkEml          s            253
015900150108     d wrkEmcc         s            253
016000150108     d wrkMsg          s           5000
016100150108     d wrkOgg          s             44
016200150108     d wrkesit         s              1
016300150108     d
016400100521     d*---
016500100521     d nr2             S              3  0                                      *n° rel sf
016600100521     d stmrec1         S              3  0                                      *n° rel sf
016700100521     d*---
016800100521     d MaxKey          C                   2
016900100521     d Ascendente      C                   1
017000100521     d Discendente     C                   2
017100100521     d Carattere       C                   6
017200100521     d Put             C                   1
017300100521     d EndPut          C                   2
017400100521     d Get             C                   3
017500100521     d Numerico        C                   8
017600100521     d*---
017700100521     d SflRcd          DS
017800100521     d  vpad
017900100521     d  SAVABI
018000100521     d  SAVCAB
018100100521     d  SAVNRA
018200100521     d  SAVNR1
018300100521     d  VANN
018400100521     d  VNRA
018500100521     d  VNRA1
018600100521     d  VDTE
018700100521     d  VASS
018800100521     d  VASSH
018900100521     d  VTPA
019000100521     d  VTPP
019100100521     d  VAbi
019200100521     d  Vcab
019300100521     d  Vdes
019400100521     d  Vtpi
019500100521     d  Vaas
019600100521     d  Vnrs
019700100521     d  Vnsp
019800100521     d  Vpia
019900100521     d  Vnot
020000100521     d  Selected                      1A
020100100521
020200100521      /COPY QSYSINC/QRPGLESRC,QLGSORT
020300100521     d QLGKL                         16    DIM(MaxKey)
020400100521     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
020500100521     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
020600100521     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
020700100521     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
020800100521
020900100521      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
021000100521      /COPY QSYSINC/QRPGLESRC,QUSEC
021100020417
021200991102     IYNTGD000
021300991102     I              TGDDIVISA                   TGDDIV
021400991102     I              TGDSEQUENZ                  TGDSEQ
021500991102     I              TGDDESCR                    TGDDES
021600991102     I              TGDVALORE                   TGDVAL
021700991102?    I*----------------------------------------------------*
021800991102     C* LOOP FINO A F3
021900991102?    C*----------------------------------------------------*
022000910722     C     *INKC         DOWEQ     '0'
022100000608      *
022200000608     C* GESTIONE PRIMA VIDEATA
022300000608     C     VID01         TAG
022400000608      *
022500991102     C                   EXSR      GESD01
022600000608      *
022700000608      * SOLO PER IL GIRO "BANCA PER CONTANTE"
022800050405     C**   adesso va fatto sempre anche se non contante quindi
022900050405     C**     via il gioco fatto con l'ind.66 x solo contante
023000050405     C**   66
023100050405     C**ANN03
023200050405     C**
023300050405     C  N03
023400000612     CANN28V1PLIC        IFNE      *ZERO
023500000608     C                   EXSR      WIND01
023600000608     C   KL              GOTO      VID01
023700000608     C                   ENDIF
023800000608      *
023900991102     C* PER NO ERRORI PROSEGUO
024000991103     C  N28
024100991103     CANNKC
024200991103     CANN91
024300991104     CANN92
024400991104     CANN39              DO
024500991104     C* Effettuo Ovrdbf su file movimenti
024600991104     C     §FLE          IFNE      WFIL
024700060426     c                   setoff                                       68
024800060414     C                   EXSR      DLCMOV
024900060414     C                   EXSR      OVRMOV
025000991118     C                   MOVE      §FLE          WFIL              3 0
025100991104     C                   END
025200910718     C* PULIZIA SUBFILE
025300910718     C                   EXSR      PULIZ
025400000717     C  N66              EXSR      CARICA
025500060411     C                   Z-ADD     NR2           SAVNR2            4 0
025600100521     c                   z-add     1             rcdnbr
025700100521     c                   eval      vordi = 'Importo'
025800060403     c* non è + possibile aggiungere assegni SOLO SEDE
025900060414     c                   if        dutlpo = 'S' or dutlpo = ' '
026000060403     C                   EXSR      AGG
026100060403     c                   end
026200060411     C                   z-add     NR2           TREC              4 0
026300060421     c     trec          comp      0                                      66
026400060421     C     *IN12         DOWEQ     '0'
026500910725     C     *IN11         ANDEQ     '0'
026600910717     C     *IN90         OREQ      '1'
026700000608      *
026800000608      * CON IL (66) VISUALIZZO O BYPASSO QUESTO SCHERMO SE EFFETTUA
026900000608      *  IL BYPASS, DEVO ANCHE SIMULARE DI AVER PREMUTO IL TASTO DI
027000000608      *   CONFERMA PER LA VIDEATA SUCCESSIVA DEL CONTANTE.
027100000608     C  N66              WRITE     CN0W9Z2
027200000608     C  N66              EXFMT     CN0W9C2
027300000608     C   66              SETON                                        11
027400910725     C                   SETOFF                                       90
027500100521     C* F11 = Cambia ordinamento
027600100521if  1C                   IF        *INKK
027700100521     C                   EXSR      F11GES
027800100521e   1C                   ENDIF
027900000608      *
028000000608      * SE F14=ON ESCLUSIONE DI TUTTE LE RIGHE CON "E" SU "VANN"
028100000608     C   66
028200000608     COR KN              EXSR      EXCLRD
028300000608      *
028400060421     C  N12              DO
028500910731     C                   SETOFF                                       82
028600910729     C                   EXSR      CONTR
028700910725     C   11              EXSR      CONTR
028800910726     C   11              SETOFF                                       1317
028900910725     C   11
029000910725     CANN90              EXSR      CONF
029100911125     C   69              GOTO      FINE
0292000604213    C                   ENDdo
029300060411     c* se premuto f12 e sono un utente di sede chiedo conferma per
029400060411     c* eventuali assegni inseriti
029500060426     c                   if        *in12 and *in19 and not *in15
029600060411     C     *IN82         COMP      '0'                                121282
029700060411     c                   end
029800060426     c                   if        *in82 and *in19
029900060411     C                   SETOFF                                       12
030000060411     c                   end
030100910801     C   15              SETON                                        12
030200000608      *
030300000608      * PER IL GIRO "BANCA PER CONTANTE" AD F12 DEVE TORNARE AL
030400000608      * PRIMO VIDEO INIZIALE
030500000608     C   17
030600000608     CAN 66              SETON                                        12
030700131125      *
030800131125      *------
030900131125      * Se si tratta di utente di SEDE che aggiorna il SFL ASSEGNI
031000131125      *  voglio che esca dopo aver dato F6.
031100131125     c                   if        dutlpo = 'S' or dutlpo = ' '
031200131125     c                   if        *in06
031300131125     C                   SETON                                        12
031400131125     c                   end
031500131125     c                   end
031600131125      *------
031700000608      *
031800910731     C                   END
031900060403     C                   END
032000910722     C                   END
032100911125     C     FINE          TAG
032200991104     C* Se ho effettuato l'allocazione disalloco FNMOV
032300991104     C     §FLE          IFEQ      WFIL
032400991104     C                   EXSR      DLCMOV
032500991104     C                   END
032600991104     C                   COMMIT
032700921204     C*-                                                   *
032800910719     C                   SETON                                        LR
032900100521      *-----------------------------------------------------------------------------------
033000100521      * F11GES - TASTO FUNZIONE F11 --> Ragione sociale/P.O
033100100521      *-----------------------------------------------------------------------------------
033200100521     C     F11GES        BEGSR
033300100521     C                   setoff                                       78
033400100521      * IMPOSTO ENTRATA AD 'A'
033500100521     C                   EVAL      rcdnbr  = 1
033600100521      *
033700100521     C                   CLEAR                   QLGSCB
033800100521     C                   CLEAR                   QLGSCB00
033900100521
034000100521     ** 1 campo chiave.
034100100521     C                   EVAL      QLGNBRK    = 1
034200100521
034300100521     C                   IF        *in50=*off
034400100521     c                   eval      vordi = 'Importo'
034500100521     ** IL numero ass. è a posizione 2, alfa, 4 byte, ascending.
034600100521     C                   EVAL      QLGSP      = 22
034700100521     C                   EVAL      QLGSS      = %SIZE(vnra)
034800100521     C                   EVAL      QLGDT      = carattere
034900100521     C                   EVAL      QLGSO      = Ascendente
035000100521     C                   EVAL      QLGKL(1)   = QLGSKL
035100100521     C                   seton                                        50
035200100521     C                   ELSE
035300100521      *
035400100521     c                   eval      vordi = 'Assegno'
035500100521     ** L'importo è a posizione 21, 11 byte, num., ascending.
035600100521     C                   EVAL      QLGSP      = 39
035700100521     C                   EVAL      QLGSS      = %SIZE(vass)
035800100521     C                   EVAL      QLGDT      = numerico
035900100521     C                   EVAL      QLGSO      = Ascendente
036000100521     C                   EVAL      QLGKL(1)   = QLGSKL
036100100521     C                   setoff                                       50
036200100521     C                   ENDIF
036300100521
036400100521      * Load other sort parameters.
036500100521     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
036600100521     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
036700100521     C                   EVAL      QLGRT     = 8
036800100521     C                   EVAL      QLGOKL    = 80
036900100521     C                   EVAL      QLGLKE    = 16
037000100521     C                   EVAL      QLGLSS    = 290
037100100521
037200100521      * Initialize Sort I/O API fields.
037300100521     C                   EVAL      QLGRL00  = QLGRL
037400100521     C                   EVAL      QLGRC00  = 1
037500100521     C                   CLEAR                   QUSEI
037600100521     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
037700100521
037800100521      * First step - Initialize the sort routine.
037900100521     C                   CALL      'QLGSORT'
038000100521     C                   PARM                    QLGSCB
038100100521     C                   PARM                    NotUsed
038200100521     C                   PARM                    NotUsed
038300100521     C                   PARM                    SizeList
038400100521     C                   PARM                    ReturnSize
038500100521     C                   PARM                    QUSEC
038600100521
038700100521      * Next step - Write records to I/O routine.
038800100521     C                   EVAL      QLGRT00 = Put
038900100521
039000100521     C                   DO        STMREC1       x                 3 0
039100100521
039200100521     C     x             CHAIN     CN0W9S2
039300100521
039400100521     ** Solo le righe con Selected = 'Y' sono riordinate,
039500100521     ** quindi per fare un ordinamento di tutte le righe
039600100521     ** metto 'Y' sempre.
039700100521     C                   EVAL      Selected = 'Y'
039800100521
039900100521     C                   CLEAR                   QUSEI
040000100521     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
040100100521     C                   CALL      'QLGSRTIO'
040200100521     C                   PARM                    QLGSCB00
040300100521     C                   PARM                    SflRcd
040400100521     C                   PARM                    NotUsed
040500100521     C                   PARM                    SizeList
040600100521     C                   PARM                    NotUsed
040700100521     C                   PARM                    QUSEC
040800100521
040900100521     C                   ENDDO
041000100521
041100100521      * Next step - Signal end of input, clear subfile for reload.
041200100521     C                   EVAL      QLGRT00 = EndPut
041300100521     C                   CLEAR                   QUSEI
041400100521     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
041500100521
041600100521     C                   CALL      'QLGSRTIO'
041700100521     C                   PARM                    QLGSCB00
041800100521     C                   PARM                    SflRcd
041900100521     C                   PARM                    NotUsed
042000100521     C                   PARM                    SizeList
042100100521     C                   PARM                    NotUsed
042200100521     C                   PARM                    QUSEC
042300100521
042400100521     C                   SETON                                            70    *
042500100521     C                   WRITE     CN0W9C2                                      *
042600100521     C                   SETOFF                                       70        *
042700100521     C                   WRITE     CN0W9Z2                                      *
042800100521
042900100521      * Final step - Write the records back to the subfile.
043000100521     C                   EVAL      QLGRT00 = Get
043100100521
043200100521     C                   DO        STMREC1       x
043300100521     C                   CLEAR                   QUSEI
043400100521     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
043500100521     C                   CALL      'QLGSRTIO'
043600100521     C                   PARM                    QLGSCB00
043700100521     C                   PARM                    NotUsed
043800100521     C                   PARM                    SflRcd
043900100521     C                   PARM                    QLGRL00
044000100521     C                   PARM                    NotUsed
044100100521     C                   PARM                    QUSEC
044200100521      *
044300100521     c                   z-add     x             nr2
044400100521     C                   WRITE     CN0W9S2
044500100521     C                   ENDDO
044600100521
044700100521     C                   EVAL      *IN70 = *off
044800100521      *
044900100521     C                   ENDSR
045000991102     C*----------------------------------------------------*
045100991102     C* GESTIONE DELLA PRIMA VIDEATA
045200991102     C*----------------------------------------------------*
045300991102     C     GESD01        BEGSR
045400150410      *
045500150410     C                   z-add     §FLE          wfle
045600000609      *
045700000609      * SOLO PER IL GIRO BANCA PER CONTANTE (66=ON)
045800000609     C*  Aggancio tabella 'SEC' x reperire la BANCA
045900000609     C     *IN66         IFEQ      *ON
046000000609     C                   MOVEL     'SEC'         KCOD1
046100000609     C                   MOVEL     *BLANKS       KKEY1
046200000609     C                   MOVEL     'SEC'         KKEY1
046300000609     C     KTBE          CHAIN     TNTBE01L                           13
046400000609     C     *IN13         IFEQ      '0'
046500000609     C                   MOVEL     TBEUNI        BANCA8            8 0
046600060421     C*                  MOVE      BANCA8        VBAN
046700000609     C                   END
046800000609     C                   ENDIF
046900000609      *
047000150304     C* LEGGO TABELLA 'Y4O'
047100150312     C                   MOVEL     'Y4O'         tbecod
047200150312     C                   MOVEL(p)  '201'         tbeke1
047300150312     C                   MOVEL(p)  §FLE          tbeke2
047400150305      * sprotegge il campo di immissione codice Banca
047500150312     c                   setON                                        35
047600150312     C     KTbe1         CHAIN     Tntbe01l                           39
047700150312     C   39              SETON                                        28
047800150313     C   39              clear                   vban
047900150306      *
048000150312     c                   if        not *in39
048100150312     C                   EXCEPT    RILTAB
048200150312     C                   MOVEL     TBeUNI        Dy4O
048300150305      *
048400150305      * imposta automaticamente il codice Banca per eseguire la DISTINTA
048500150305      * di Versamento ed il campo viene protetto.
048600150312     c                   if        §4OSCB <> *blanks and
048700150312     c                             §4OSCB <> *zeros
048800150312     c                   setOFF                                       35
048900150312     c                   move      *zeros        VBAN
049000150312     c                   move      §4OSCB        VBAN
049100150313     c                   move      vban          wban
049200150312     c                   end
049300150312     c                   end
049400150304      *
049500150313     c     sopra         tag
049600060421     C                   SETOFF                                       128211
049700991102     C                   SETON                                        73
049800991102     C                   EXFMT     CN0W9D1
049900150313     c   kc              goto      fine
050000991104     C                   SETOFF                                       2891
050100150304      *
050200021120     c* controllo che non sia già stata fatta la quadratura giornaliera
050300021120     c* se così fosse non lascio fare il versamento
050400060412     c     kqig          chain     yfqig01l
050500060412     c                   if        %found(yfqig01l)
050600021120     c                   seton                                        28
050700021120     C                   MOVEL     ERR(9)        $MSG
050800021120     C                   GOTO      FINVD1
050900021120     c                   end
051000000612     C* Controllo se ci sono dati in FNMOV00F
051100991103     C                   Z-ADD     0             NRRX             10 0
051200991103     C                   MOVEL     'FNMOV00T'    FILE
051300991103     C                   MOVEL     '*LIBL   '    LIB              10
051400020926     C                   EVAL      Mbr = 'M' + %EDITC(§Fle:'X') + '046'
051500991103     C                   CALL      'TRUL51C'
051600991103     C                   PARM                    LIB
051700991103     C                   PARM                    FILE             10
051800020926     C                   PARM                    MBR              10
051900991103     C                   PARM                    NRRX
052000991103     C     NRRX          IFNE      *ZEROS
052100991118     C     NRRX          ANDNE     *ALL'9'
052200991103     C                   SETON                                        28
052300991103     C                   MOVEL     ERR(6)        $MSG
052400991103     C                   GOTO      FINVD1
052500991103     C                   END
052600000609      *--------
052700991102     C     §FLE          IFNE      0
052800060421     c                   if        dutlpo <> ' ' and dutlpo <> 'S'
052900150313     C     §Fle          LOOKUP    FILA                                   37
053000150312     C  N37              SETON                                        3938
053100150313     C  n37              GOTO      sopra
053200150312     C   37              SETOFF                                       3938
053300060421     c                   end
053400991102     C                   ELSE
053500150312     C                   SETON                                        39
053600150313     C                   GOTO      sopra
053700991102     C                   END
053800150312     C* LEGGO TABELLA 'Y4O'
053900150407     c                   if        Wfle <> §fle
054000150407     c                   clear                   vban
054100150407     C                   z-add     §FLE          wfle
054200150312     C                   MOVEL     'Y4O'         tbecod
054300150312     C                   MOVEL(p)  '201'         tbeke1
054400150312     C                   MOVEL(p)  §FLE          tbeke2
054500150312      * sprotegge il campo di immissione codice Banca
054600150312     c                   setON                                        35
054700150312     C     KTbe1         CHAIN     Tntbe01l                           39
054800150313     C   39              SETON                                        28
054900150407e1   c                   if        not *in39
055000150312     C                   EXCEPT    RILTAB
055100150312     C                   MOVEL     TBeUNI        Dy4O
055200150312      *
055300150312      * imposta automaticamente il codice Banca per eseguire la DISTINTA
055400150312      * di Versamento ed il campo viene protetto.
055500150407e2   c                   if        §4OSCB <> *blanks and
055600150313     c                             §4OSCB <> *zeros
055700150313     c                   move      *zeros        vBAN
055800150313     c                   move      §4OSCB        vBAN
055900150319     C                   setoff                                       35
056000150407     C                   GOTO      sopra
056100150407f2   c                   end
056200150407f1   c                   end
056300150407f1   c                   endif
056400150313      *
056500150313     C     VBAN          COMP      0                                      92
056600150407     c                   if        *in92
056700150313     c                   seton                                        28
056800150313     C                   MOVEL     ERR(10)       $MSG
056900150313     C                   GOTO      sopra
057000150407     c                   endif
057100150312     C***  Adesso il controllo di immissione nr.plico lo deve fare sempre
057200150312     C     V1PLIC        COMP      0                                      93
057300150407     c                   if        *in93
057400150313     c                   seton                                        28
057500150313     C                   MOVEL     ERR(7)        $MSG
057600150313     C                   GOTO      sopra
057700150407     c                   endif
057800150312     c
057900991102     C  NKC
058000150407     CANN39              DO
058100991102     C                   Z-ADD     VBAN          KSC
058200991102     C     VBAN          COMP      0                                      92
058300991102     C                   MOVEL     *BLANKS       ACORAG
058400991102     C  N92KEYACO        CHAIN     CNACO000                           92
058500991102     C  N92ACOFLG        COMP      '*'                                    92
058600991102     C  N92ACOABL        COMP      '*'                                    92
058700991102     C                   MOVEL     ACORAG        VIDBAN           30
058800991102     C                   MOVEA     ACOOPZ        OPZ                            *
058900991102     C                   MOVE      OPZ(7)        HOPZ7             1            OBBL.RAPP.
059000991102     C                   MOVE      OPZ(2)        HOPZ2             1            GEST.SCADE
059100991102     C     KEYACO        CHAIN     CNIND000                           21
059200991102     C                   Z-ADD     INDABI        ISTABI
059300991102     C                   Z-ADD     INDCAB        ISTCAB
059400991102     C   21              MOVE      *BLANK        INDCCB
059500991102     C                   MOVEL     ACORAG        VBANC
059600150407     c                   if        *in92
059700150319     c                   seton                                        28
059800150319     c                   movel     err(11)       $msg
059900150319     c                   goto      sopra
060000150407     c                   endif
060100991102     C*
060200991102     C* Verifico se la divisa è corretta
060300150407     C  N92V1CDIV        IFNE      DIVALT
060400011016     C     V1CDIV        ANDNE     §GEDCR
060500011016     C     V1CDIV        OREQ      *BLANKS
060600011016     C                   SETON                                        2891
060700011016     C                   MOVEL     ERR(5)        $MSG
060800150407     C                   END
060900011016     C* Controllo fine validità divisa - tipo incasso.
061000011016     C                   CALL      'X1ACW'
061100011016     C                   PARM      'SC'          PrmTpInc
061200011016     C                   PARM      V1CDiv        PrmDivisa
061300011016     C                   PARM                    PrmData
061400011016     C                   PARM      *BLANK        PrmEsito
061500011016     C     DS1A          PARM      *BLANK        PrmDS1A
061600011016     C     DSCW          PARM      *BLANK        PrmDSCW
061700150407     C                   IF        PrmEsito <> '0'
061800011016     C                   EVAL      *IN28 = *ON
061900011016     C                   EVAL      *IN91 = *ON
062000011016     C                   MOVEL     ERR(5)        $MSG
062100150407     C                   ENDIF
062200150407     C                   END
062300150407     C                   IF        PrmEsito = '0'
062400011016     C                   EVAL      *IN94 = *ON
062500150407     C                   ELSE
062600011016     C                   EVAL      *IN94 = *OFF
062700150407     C                   ENDIF
062800000608      *
062900000608      *  SOLO PER IL GIRO "BANCA PER CONTANTE"
063000000608      *   ESEGUO IL CONTROLLO DEL NR.PLICO OBBLIGATORIO
063100050405     C***  Adesso il controllo di immissione nr.plico lo deve fare sempre
063200150312     C*    V1PLIC        COMP      0                                      93
063300150312     C*  93              SETON                                        28
063400150312     C*  93              MOVEL     ERR(7)        $MSG
063500000608      *
063600000612      *
063700991103     C     FINVD1        ENDSR
063800000608     C*----------------------------------------------------*
063900000608     C* IMPOSTA LA WINDOW CONFERMA PER "BANCA X CONTANTE"
064000000608     C*----------------------------------------------------*
064100000608     C     WIND01        BEGSR
064200000608      *
064300000608      * WINDOW DI CONFERMA NR. PLICO
064400000608     C                   Z-ADD     V1PLIC        W1PLIC
064500000608     C                   EXFMT     CN0W9W1
064600000608      *
064700000608     C                   ENDSR
064800000609     C*----------------------------------------------------*
064900000609     C* ESEGUO OVRDBF SU FNMOV
065000000609     C*----------------------------------------------------*
065100000609     C     OVRMOV        BEGSR
065200000609     C*
065300020926     C                   MOVE      '046) '       VAR               8
065400000609     C                   MOVEL     §FLE          VAR
065500020926     C                   Z-ADD     49            LUNG             15 5
065600000609     C** CONTROLLO SE ESISTE IL MEMBRO X FNMOV00T
065700000609     C                   SETOFF                                       20
065800000609     C                   MOVE      VAR           CMDD(1)
065900000609     C                   MOVEL     *BLANKS       COMMAN
066000000609     C                   MOVEA     CMDD(1)       COMMAN           80
066100000609     C                   CALL      'QCMDEXC'                            20
066200000609     C                   PARM                    COMMAN
066300000609     C                   PARM                    LUNG
066400000609     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
066500000609     C   20              DO
066600000609     C                   MOVE      VAR           CMDD(2)
066700000609     C                   MOVEL     *BLANKS       COMMAN
066800000609     C                   MOVEA     CMDD(2)       COMMAN
066900000609     C                   CALL      'QCMDEXC'
067000000609     C                   PARM                    COMMAN
067100000609     C                   PARM                    LUNG
067200000609     C                   END
067300000609     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
067400000609     C                   MOVE      VAR           CMDD(3)
067500000609     C                   MOVEL     *BLANKS       COMMAN
067600000609     C                   MOVEA     CMDD(3)       COMMAN
067700000609     C                   CALL      'QCMDEXC'
067800000609     C                   PARM                    COMMAN
067900000609     C                   PARM                    LUNG
068000000609     C* GUARDO SE è ALLOCATO
068100000609     C                   MOVEL     VAR           VAR
068200000609     C                   MOVE      ')'           VAR
068300000609     C                   MOVE      VAR           CMDD(4)
068400000609     C                   MOVEA     CMDD(4)       COMMAN
068500020926     C                   Z-ADD     49            LUNCMD           15 5
068600000609     C                   CALL      'QCMDEXC'                            91
068700000609     C                   PARM                    COMMAN
068800000609     C                   PARM                    LUNCMD
068900000609     C*
069000000609     C  N91              OPEN      FNMOV00T                             91
069100030318     C   91              MOVEL(p)  'NO'          KPJBU
069200000609     C   91              GOTO      FINE
069300000609     C*
069400000609     C                   ENDSR
069500991104     C*----------------------------------------------------*
069600991104     C* ESEGUO Dlcobj  FNMOV
069700991104     C*----------------------------------------------------*
069800991104     C     DLCMOV        BEGSR
069900991104     C*
070000991104     C* GUARDO SE è ALLOCATO
070100991104     C  N91              CLOSE     FNMOV00T                             91
070200991104     C*
070300991104     C                   MOVE      VAR           CMDD(5)
070400991104     C                   MOVEA     CMDD(5)       COMMAN
070500020926     C                   Z-ADD     49            LUNCMD           15 5
070600991104     C                   CALL      'QCMDEXC'                            91
070700991104     C                   PARM                    COMMAN
070800991104     C                   PARM                    LUNCMD
070900991104     C*
071000991104     C                   ENDSR
071100000609     C*----------------------------------------------------*
071200910717     C* CARICAMENTO SUBFILE CON ASSEGNI VERSABILI
071300991102     C*----------------------------------------------------*
071400910717     C     CARICA        BEGSR
071500911113     C                   SETOFF                                       555152
071600911113     C                   SETOFF                                       49
071700991105     C                   Z-ADD     0             TASS             15 3
071800941103     C     §FLE          SETLL     CNASS02L
071900910717     C     *IN55         DOWEQ     '0'
072000941103     C     §FLE          READE     CNASS02L                               55
072100941103     C  N55ASSFVE        IFEQ      'V'                                          VERSABILI
072200941103     C     ASSTPI        ANDNE     'M'
072300941103     C     ASSNDV        ANDEQ     0                                            NO ALTRE D
072400991102     C     ASSVCA        ANDEQ     V1CDIV                                       SOLO DIV V
072500991102     C*
072600910717     C* LA PRIMA VOLTA SALTO IL CONFRONTO
072700910717     C  N51              DO
072800910717     C                   EXSR      PREC
072900910717     C                   SETON                                        51
073000910717     C                   END
073100910717     C*
073200941103     C     ASSNRA        IFNE      PNRA
073300991102     C     ASSDTE        ORNE      PDTE
073400941103     C     ASSABI        ORNE      PABI
073500951016     C     ASSCAB        ORNE      PCAB
073600910717     C                   EXSR      MUOVI
073700100521     C                   ADD       1             NR2
073800991102     C                   WRITE     CN0W9S2
073900910717     C                   ELSE
074000941103     C                   ADD       ASSCAS        TASS
074100910717     C                   END
074200940831     C                   END
074300910717     C                   END
074400910722     C* CARICO L'ULTIMO ASSEGNO
074500910722     C   51              DO
074600910722     C                   EXSR      MUOVI
074700991103     C                   ADD       1             NR2
074800991102     C                   WRITE     CN0W9S2
074900910722     C                   END
075000100521     C                   Z-ADD     nr2           STMREC1                        *n° rel sf
075100910722     C                   ENDSR
075200991102     C*----------------------------------------------------*
075300991102     C* Sposta  i campi dai dati di salvataggio a video
075400991102     C*----------------------------------------------------*
075500910717     C     MUOVI         BEGSR
075600991102     C*
075700920520     C     PTPD          COMP      'V'                                6565
075800920520     C   65              MOVEL     'P'           VPAD
075900920520     C  N65              MOVEL     *BLANKS       VPAD
076000910719     C                   MOVEL     PNRA          VNRA
076100910719     C                   MOVE      PNRA          VNRA1
076200940126     C                   MOVEL     PNRA          SAVNRA
076300940126     C                   MOVE      PNRA          SAVNR1
076400910717     C                   MOVEL     PTPD          VTPD
076500910717     C                   Z-ADD     PLNP          VLNP
076600910717     C                   Z-ADD     PAAS          VAAS
076700910717     C                   Z-ADD     PNRS          VNRS
076800910717     C                   Z-ADD     PNSP          VNSP
076900941103     C                   Z-ADD     PDTE          G02INV
077000941103     C                   MOVE      *ZEROS        G02DAT
077100941103     C                   MOVE      '3'           G02ERR
077200991120     C                   CALL      'XSRDA8'
077300941103     C                   PARM                    WLBDAT
077400941103     C                   MOVE      G02DAT        VDTE
077500910717     C                   Z-ADD     PABI          VABI
077600910717     C                   Z-ADD     PCAB          VCAB
077700940126     C                   Z-ADD     PABI          SAVABI
077800940126     C                   Z-ADD     PCAB          SAVCAB
077900910723     C                   Z-ADD     TASS          VASS
078000060403     C                   Z-ADD     TASS          VASSh
078100921105     C* DECODIFICO ISTITUTO SOLO SE ABI/CAB<> 1000/1000
078200910717     C                   Z-ADD     PABI          KABI
078300910717     C                   Z-ADD     PCAB          KCAB
078400910725     C     KABI§         CHAIN     CNABI00F                           77
078500910725     C  N77              MOVEL     ABIIST        VDES
078600910725     C   77              MOVE      *BLANKS       VDES
078700910717     C*
078800911219     C  N77              MOVEL     ABICOM        VPIA
078900911219     C   77              MOVE      *BLANKS       VPIA
079000911219     C*
079100910717     C                   MOVEL     PTPA          VTPA
079200910717     C                   MOVEL     PTPI          VTPI
079300910717     C                   MOVEL     PTPP          VTPP
079400911219     C*                    MOVELPPIA      VPIA
079500910717     C                   MOVEL     PNOT          VNOT
079600910716     C*
079700910717     C* AZZERO TOTALIZZATORE ASSEGNI
079800910717     C                   Z-ADD     0             TASS
079900941103     C                   ADD       ASSCAS        TASS
080000910717     C* MEMORIZZO N° ASS.,ABI-CAB,DATA ..... PRECEDENTI
080100910717     C                   EXSR      PREC
080200910719     C                   ENDSR
080300991102     C*----------------------------------------------------*
080400991102     C* Sposta  i dati nei campi di salvataggio
080500991102     C*----------------------------------------------------*
080600910717     C     PREC          BEGSR
080700941103     C                   MOVEL     ASSNRA        PNRA             10
080800941103     C                   MOVEL     ASSTPD        PTPD              1
080900941103     C                   Z-ADD     ASSLNP        PLNP              3 0
081000941103     C                   Z-ADD     ASSAAS        PAAS              4 0
081100941103     C                   Z-ADD     ASSNRS        PNRS              2 0
081200941103     C                   Z-ADD     ASSNSP        PNSP              7 0
081300991102     C                   MOVE      ASSDTE        PDTE              8 0
081400941103     C                   Z-ADD     ASSABI        PABI              5 0
081500941103     C                   Z-ADD     ASSCAB        PCAB              5 0
081600941103     C                   MOVEL     ASSTPA        PTPA              1
081700941103     C                   MOVEL     ASSTPI        PTPI              1
081800941103     C                   MOVEL     ASSTPP        PTPP              1
081900941103     C                   MOVEL     ASSNOT        PNOT             20
082000941103     C                   MOVEL     ASSFVE        PFVE              1
082100910722     C                   ENDSR
082200910722     C*
082300000608     C*----------------------------------------------------*
082400000608      * SE RICHIESTO L'ESCLUSIONE TOTALE DI TUTTI GLI ASSEGNI
082500000608      * TRAMITE IL F14 IMPOSTA AUTOMATICAMENTE SU TUTTI
082600000608     C*----------------------------------------------------*
082700000608     C     EXCLRD        BEGSR
082800000608      *
082900000608     C                   SETOFF                                       9028
083000000608      *
083100000608     C                   DO        SAVNR2        CNT
083200000608      *
083300000608     C                   SETOFF                                       242762
083400000608     C                   SETOFF                                       495852
083500000608     C                   SETOFF                                       434445
083600000608     C                   SETOFF                                       464748
083700000608      *
083800000608     C     CNT           CHAIN     CN0W9S2                            60
083900000608      *
084000000608     C     VNRA          IFNE      *BLANK
084100000608     C                   MOVE      'E'           VANN
084200000608     C                   END
084300000608      *
084400000608     C                   UPDATE    CN0W9S2
084500000608      *
084600000608     C                   ENDDO
084700000608      *
084800000608     C                   ENDSR
084900991102     C*----------------------------------------------------*
085000991102     C* Controlli i dati del subfile
085100991102     C*----------------------------------------------------*
085200910717     C     CONTR         BEGSR
085300000609      *
085400951027     C                   SETOFF                                       9028
085500910717     C                   DO        TREC          CNT
085600991209     C                   SETOFF                                       242762
085700911115     C                   SETOFF                                       495852
085800911115     C                   SETOFF                                       434445
085900911115     C                   SETOFF                                       464748
086000000609      *
086100991102     C     CNT           CHAIN     CN0W9S2                            60
086200060526     c* 19 acceso utente di sede
086300060526     c                   if        vann = 'A' and not *in19
086400060411     c                   clear                   vann
086500060411     c                   end
086600000608      *
086700000608      * SE RICHIESTO L'ESCLUSIONE TOTALE DI TUTTI GLI ASSEGNI
086800000608      * TRAMITE IL F14 IMPOSTA AUTOMATICAMENTE SU TUTTI
086900000608      *
087000910717      *-------------
087100910717     C* ESEGUE CONTR. SOLO SE NON SONO ASSEGNI PROVENIENTI DA DISTINTA
087200910717     C* PADRONCINO
087300060526     C     VASS          ifne      0
087400910717     C     VANN          IFEQ      'A'
087500910729     C   KF              DO
087600910717     C                   MOVEL     VNRA          KNRA
087700910717     C                   MOVE      VNRA1         KNRA
087800910717     C                   Z-ADD     VABI          KABI
087900910717     C                   Z-ADD     VCAB          KCAB
088000060526     C     KASS          setll     CNASS000
088100060526     c                   do        *hival
088200060526     C     KASS          reade     CNASS000
088300060526     c                   if        %eof(cnass02l)
088400060526     c                   leave
088500060526     c                   end
088600951027     C     ASSFVE        IFEQ      'V'
088700951027     C     ASSTPI        ANDNE     'M'
088800150319     c*                  exsr      wrttnass
088900060526     C                   DELETE    CNASS000
089000951027     C                   END
089100060526     C                   ENDdo
089200940126     C*
089300940126     C     SAVNRA        IFNE      VNRA
089400940126     C     SAVNR1        ORNE      VNRA1
089500940126     C     SAVABI        ORNE      VABI
089600940126     C     SAVCAB        ORNE      VCAB
089700940126     C                   MOVEL     SAVNRA        KNRA
089800940126     C                   MOVE      SAVNR1        KNRA
089900940126     C                   Z-ADD     SAVABI        KABI
090000940126     C                   Z-ADD     SAVCAB        KCAB
090100060526     C     KASS          setll     CNASS000
090200060526     c                   do        *hival
090300060526     C     KASS          reade     CNASS000
090400060526     c                   if        %eof(cnass02l)
090500060526     c                   leave
090600060526     c                   end
090700951027     C     ASSFVE        IFEQ      'V'
090800951027     C     ASSTPI        ANDNE     'M'
090900150319     c*                  exsr      wrttnass
091000060526     C                   DELETE    CNASS000
091100940126     C                   END
091200060526     C                   ENDdo
091300060411     C                   END
091400940126     C*
091500910717     C                   EXSR      PULI
091600060411     C                   ENDdo
091700910717     C                   ELSE
091800910717     C                   EXSR      CTR
091900910717     C                   END
092000920520     C                   SETOFF                                       65
092100991102     C                   UPDATE    CN0W9S2
092200911219     C                   END
092300950303     C  N90              END
092400910717     C                   ENDSR
092500991102     C*----------------------------------------------------*
092600911115     C* COTROLLI SUBFILE
092700991102     C*----------------------------------------------------*
092800910717     C     CTR           BEGSR
092900060421      * CONTROLLO METTO V SU TIPO DOCUMENTO
093000060421     C     VTPD          IFEQ      *BLANKS
093100060421     C                   MOVE      'V'           VTPD
093200060421     C                   END
093300910717     C*
093400991209     C* CONTROLLO CHE IL NUMERO DEI DECIMALI SIA CONGRUENTE
093500991209     C                   MOVE      VASS          WCOMO1            1 0
093600991209     C                   MOVE      VASS          WCOMO2            2 0
093700010626     C     V1CDIV        IFEQ      §GEDCR
093800991209     C     DECDIV        ANDLT     2
093900991209     C     WCOMO1        ANDGT     0
094000010626     C     V1CDIV        OREQ      §GEDCR
094100991209     C     DECDIV        ANDLT     1
094200991209     C     WCOMO2        ANDGT     0
094300991209     C     V1CDIV        OREQ      DIVALT
094400991209     C     DECALT        ANDLT     2
094500991209     C     WCOMO1        ANDGT     0
094600991209     C     V1CDIV        OREQ      DIVALT
094700991209     C     DECALT        ANDLT     1
094800991209     C     WCOMO2        ANDGT     0
094900991209     C                   SETON                                        62  90
095000991209     C                   END
095100910717      * CONTROLLO DATA ASSEGNO
095200910717      *
095300060414     c                   if        vann <> 'E'
095400951027     C                   SETOFF                                       56
095500911113     C                   MOVE      VDTE          G02DAT
095600911113     C                   MOVE      *BLANKS       G02ERR
095700991102     C                   CALL      'XSRDA8'
095800910717     C                   PARM                    WLBDAT
095900910717     C     G02ERR        IFEQ      '1'
096000060403     c                   SETON                                        43  90
096100060414     c                   else
096200060414     c                   move      g02inv        dte
096300060403     C                   END
096400060411     c* controllo che la data non sia considerata postdatato
096500060414     c                   eval      sas = vtpa
096600060414     c                   callp     chkdataassegno(dte:sas
096700060414     c                             :esito:err:*omit)
0968000604141    c                   if        esito <>0
096900060411     C                   SETON                                        43  90
097000060411     C                   END
097100060411     c* obbligo ad escludere gli assegno con data > oggi
097200060411     C     G02inv        IFgt      oggi
097300060411     c                   SETON                                        44
097400060403     C                   END
097500991104     C                   MOVE      G02DAT        VDTE
097600060403     c* IMPORTO ASSEGNO OBBLIGATORIO
097700060403     C  n90VASS          IFEQ      0
097800060414     C                   SETON                                        456290
097900910717     C                   END
098000060403     c* controllo che l'importo dell'assegno non sia variato + dell'importo
098100060403     c* di tolleranza
098200060403     C  n90VASS          IFne      vassh
098300060414     C     vtpd          andne     'V'
098400060403     c                   exsr      srtolle
098500060403     C                   END
098600060414     C                   END
098700910717      ****************************************
098800910717      * SE TIPO ASSEGNO E' MITTENTE
098900910717      * ABI, CAB E N° ASSEGNO SONO OBBLIGATORI
099000910717      ****************************************
099100060403     C  n90VABI          IFEQ      0
099200911127     C     VTPA          ANDNE     'P'
099300911127     C     VTPA          ANDNE     'V'
099400910717     C     VCAB          OREQ      0
099500911127     C     VTPA          ANDNE     'P'
099600911127     C     VTPA          ANDNE     'V'
099700911127     C                   SETON                                        49  90
099800911127     C                   END
099900060403     C  n90VNRA          IFEQ      *BLANKS
100000910717     C                   SETON                                        47  90
100100910717     C                   END
100200911127      * CONTROLLO CODICE ABI/CAB SE TIPO ASSEGNO < > 'P' E 'V'
100300921105      * SOLO SE ABI/CAB NON E' 1000/1000
100400921105     C     VABI          IFNE      1000
100500921105     C     VCAB          ANDNE     1000
100600910725     C                   Z-ADD     VABI          KABI
100700910725     C                   Z-ADD     VCAB          KCAB
100800910725     C     KABI§         CHAIN     CNABI00F                           49
100900910729     C  N49VPIA          IFEQ      *BLANKS
101000910729     C                   MOVEL     ABICOM        VPIA
101100910729     C                   END
101200910717     C  N49              MOVEL     ABIIST        VDES
101300910801     C   49              MOVE      *BLANKS       VDES
101400910801     C     VPIA          IFEQ      *BLANKS
101500911113     C  NKF
101600911113     CANNKA              SETON                                        5290
101700910801     C                   END
101800911113     C   49
101900911113     CANNKF
102000911113     CANNKA              SETON                                        90
102100921105     C                   END
102200910716     C*
102300910722      * CONTROLLO TIPO ASSEGNO
102400910722     C     VTPA          IFNE      *BLANKS
102500910722     C                   MOVEA     *BLANKS       SKI
102600910722     C                   MOVEA     VTPA          SKI
102700910722     C     '?'           LOOKUP    SKI                                    31
102800150319     C   31              DO
102900910722     C                   MOVEL     CODUT         §KUT              1 0
103000910722     C                   MOVEL     '4E'          §COD              2
103100910722     C                   MOVE      *BLANKS       VTPA
103200910722     C                   CALL      'X§TABER'
103300910722     C                   PARM                    §KUT
103400910722     C                   PARM                    §COD
103500910722     C                   PARM                    §KEY              8
103600910722     C                   PARM                    §DES             30
103700910722     C                   MOVEL     §KEY          VTPA
103800910722     C                   END
103900910722     C*-----------------------------------------------------*
104000910722     C* Se no si ricerca su TABEL se esiste
104100910722     C*-----------------------------------------------------*
104200910722     C  N31              DO
104300910722     C                   MOVE      *BLANKS       KEY
104400910722     C                   MOVEL     VTPA          KEY
104500910722     C                   MOVE      '4E'          COD
104600910722     C     KTAB          CHAIN     TABEL                              24
104700921105     C  N24              EXCEPT    RILTAB
104800910722     C  N24TBLFLG        COMP      '*'                                    24
104900910723     C   24              SETON                                          90
105000910722     C                   END
105100910722     C                   ELSE
105200910723     C                   SETON                                        2490
105300910722     C                   END
105400951027     C* Se assegno vario controllo se esiste con quei
105500951027     C* riferimenti un assegno proveiente da una chiusura
105600951027     C     VPAD          IFNE      'P'
105700951027     C                   MOVEL     VNRA          KNRA
105800951027     C                   MOVE      VNRA1         KNRA
105900951027     C                   Z-ADD     VABI          KABI
106000951027     C                   Z-ADD     VCAB          KCAB
106100951027     C     KASS          CHAIN     CNASS02L                           57
106200951027     C     *IN57         IFEQ      '0'
106300951027     C     *IN57         DOWEQ     '0'
106400951027     C     ASSTPD        ANDEQ     'V'
106500951027     C     KASS          READE     CNASS02L                               57
106600951027     C                   END
106700951027     C     ASSTPD        IFNE      'V'
106800951027     C     ASSTPI        IFEQ      'M'
106900951027     C                   SETON                                        285690
107000951027     C                   MOVEL     ERR(3)        $MSG
107100951027     C                   ELSE
107200951027     C     ASSFVE        IFNE      'V'
107300951027     C                   SETON                                        285690
107400951027     C                   MOVEL     ERR(1)        $MSG
107500951027     C                   ELSE
107600951027     C                   SETON                                        285690
107700951027     C                   MOVEL     ERR(2)        $MSG
107800951027     C                   END
107900951027     C                   END
108000951027     C                   END
108100951027     C                   END
108200951027     C                   END
108300910722     C*
108400910729     C  N90
108500910729     CAN KF
108600910729     CANN11              DO
108700940126     C                   MOVEL     VNRA          KNRA
108800940126     C                   MOVE      VNRA1         KNRA
108900940126     C                   Z-ADD     VABI          KABI
109000940126     C                   Z-ADD     VCAB          KCAB
109100941103     C     KASS          CHAIN     CNASS02L                           57
109200940126     C   57              EXSR      SPOSTA
109300941103     C   57              WRITE     CNASS000
109400941103     C  N57              UPDATE    CNASS000
109500940126     C*
109600940126     C     SAVNRA        IFNE      VNRA
109700940126     C     SAVNR1        ORNE      VNRA1
109800940126     C     SAVABI        ORNE      VABI
109900940126     C     SAVCAB        ORNE      VCAB
110000940126     C                   MOVEL     SAVNRA        KNRA
110100940126     C                   MOVE      SAVNR1        KNRA
110200940126     C                   Z-ADD     SAVABI        KABI
110300940126     C                   Z-ADD     SAVCAB        KCAB
110400941103     C     KASS          CHAIN     CNASS000                           57
110500951027     C     ASSFVE        IFEQ      'V'
110600951027     C     ASSTPI        ANDNE     'M'
110700150313     c  n57              exsr      wrttnass
110800941103     C  N57              DELETE    CNASS000
110900940126     C                   END
111000951027     C                   END
111100951026     C                   MOVE      VNRA          SAVNRA
111200951026     C                   MOVE      VNRA1         SAVNR1
111300951026     C                   Z-ADD     VABI          SAVABI
111400951026     C                   Z-ADD     VCAB          SAVCAB
111500991104     C                   COMMIT
111600910723     C                   END
111700910717     C                   ENDSR
111800991102     C*-----------------------------------------------------*
111900060403     C* tolleranza importo assegno
112000991102     C*-----------------------------------------------------*
112100060403     C     srtolle       BEGSR
112200060403     C     Vass          SUB       Vassh         §abb
112300060403     C* LO RENDO POSITIVO
112400060403     C     §ABB          IFLT      *ZEROS
112500060403     C     §ABB          MULT      -1            §ABB
112600060403     C                   END
112700060403     C* CONTROLLO IL LIMITE tollerabile DELLA differenza
112800060403     C                   MOVEL     'GEI'         KCOD1
112900060403     C                   MOVEL(p)  'EUR'         KKEY1
113000060403     c                   clear                   dgei
113100060403     C     KTBE          CHAIN     TNTBE01L
113200060403     c                   if        %found(tntbe01l)  and tbeatb = ' '
113300060403     C                   MOVEL     TBEUNI        DGEI
113400060403     c                   end
113500060403     C     §ABB          COMP      §GEtvr                             45
113600060403     C   45              SETON                                        9062
113700060403     C                   ENDSR
113800060403     C*-----------------------------------------------------*
113900060403     C* SCRIVO CNASS
114000060403     C*-----------------------------------------------------*
114100060403     C     SPOSTA        BEGSR
114200000609      *
114300941103     C                   CLEAR                   CNASS000
114400941103     C                   Z-ADD     §FLE          ASSFLE
114500941103     C                   MOVEL     VTPD          ASSTPD
114600941103     C                   Z-ADD     VASS          ASSCAS
114700941103     C                   MOVEL     VNRA          ASSNRA
114800991104     C                   MOVEL     V2CDIV        ASSVCA
114900941103     C                   MOVE      VNRA1         ASSNRA
115000941103     C                   Z-ADD     VDTE          G02DAT
115100941103     C                   MOVEL     *BLANKS       G02ERR
115200941103     C                   MOVE      *ZEROS        G02INV
115300991102     C                   CALL      'XSRDA8'
115400941103     C                   PARM                    WLBDAT
115500941103     C                   Z-ADD     G02INV        ASSDTE
115600941103     C                   Z-ADD     VABI          ASSABI
115700941103     C                   Z-ADD     VCAB          ASSCAB
115800941103     C                   MOVEL     VTPA          ASSTPA
115900941103     C                   MOVEL     VTPI          ASSTPI
116000941103     C                   MOVEL     VTPP          ASSTPP
116100941103     C                   MOVEL     VPIA          ASSPIA
116200941103     C                   MOVEL     VNOT          ASSNOT
116300941103     C                   MOVEL     'V'           ASSFVE
116400910723     C                   ENDSR
116500991102     C*-----------------------------------------------------
116600910723     C* CARICA RECORD VUOTI NEL SUBFILE
116700991102     C*-----------------------------------------------------
116800910717     C     AGG           BEGSR
116900920520     C                   SETOFF                                       65
117000910717     C                   EXSR      PULI
117100910717     C                   DO        50
117200991103     C                   ADD       1             NR2
117300991102     C                   WRITE     CN0W9S2
117400910717     C                   END
117500910722     C                   ENDSR
117600991102     C*-----------------------------------------------------
117700991102     C* CARICA RECORD VUOTI NEL SUBFILE
117800991102     C*-----------------------------------------------------
117900910717     C     PULI          BEGSR
118000910725     C                   MOVE      *BLANKS       VPAD
118100910717     C                   MOVE      *BLANKS       VANN
118200910717     C                   MOVE      *BLANKS       VNRA
118300910717     C                   MOVE      *BLANKS       VNRA1
118400940126     C                   MOVE      *BLANKS       SAVNRA
118500940126     C                   MOVE      *BLANKS       SAVNR1
118600910717     C                   Z-ADD     0             VDTE
118700910717     C                   Z-ADD     0             VASS
118800060403     C                   Z-ADD     0             VASSh
118900910717     C                   Z-ADD     0             VABI
119000910717     C                   Z-ADD     0             VCAB
119100940126     C                   Z-ADD     0             SAVABI
119200940126     C                   Z-ADD     0             SAVCAB
119300910717     C                   MOVE      *BLANKS       VTPA
119400910717     C                   MOVE      *BLANKS       VTPP
119500910717     C                   MOVE      *BLANKS       VTPI
119600910717     C                   MOVE      *BLANKS       VNOT
119700910717     C                   MOVE      *BLANKS       VTPD
119800910717     C                   Z-ADD     0             VLNP
119900910717     C                   Z-ADD     0             VAAS
120000910717     C                   Z-ADD     0             VNRS
120100910717     C                   Z-ADD     0             VNSP
120200910717     C                   MOVE      *BLANKS       VDES
120300910717     C                   MOVE      *BLANKS       VPIA
120400000608      *
120500910717     C                   ENDSR
120600991102     C*-----------------------------------------------------
120700991102     C* LEGGO SUBFILE E CALCOLO TOTALI X VIDEATA REGISTR.
120800991102     C*-----------------------------------------------------
120900910717     C     CONF          BEGSR
121000000608      *
121100150109     c                   clear                   lista
121200910726     C                   Z-ADD     0             TOTASS
121300910726     C                   MOVEA     *ZEROS        IMP
121400930927     C                   MOVEA     *ZEROS        IM1
121500000608      *
121600910717     C                   DO        TREC          CNT               4 0
121700991102     C     CNT           CHAIN     CN0W9S2                            61
121800000608      *
121900910718     C* TOTALE ASSEGNI
122000910731     C     VANN          IFNE      'E'
122100910717     C                   ADD       VASS          TOTASS
122200910718     C* TOT. IMPORTI PER TIPO
122300910718     C                   Z-ADD     1             A                 3 0
122400910719     C                   MOVEL     VTPA          LKP               2
122500910719     C                   MOVE      VTPP          LKP
122600910719     C     LKP           LOOKUP    CSP(A)                                 21
122700930927     C*
122800930927     C     VABI          IFEQ      ISTABI
122900910718     C   21              ADD       VASS          IMP(A)
123000930927     C                   ELSE
123100930927     C   21              ADD       VASS          IM1(A)
123200910717     C                   END
123300150108     c*esclusi
123400150108     c                   else
123500150108     c                   exsr      VerEsclusi
123600930927     C*
123700930927     C                   END
123800910731     C                   END
123900910717     C* TOTALE PROVVISORIO DISTINTA
124000910717     C     TOTASS        ADD       TOTCON        TOTDIS
124100991102     C                   Z-ADD     GMAUD8        VIDDRG
124200991118     C                   Z-ADD     GMAUD8        VDTOPE
124300991102     C                   Z-ADD     GMAUD8        VIDDDS
124400941104     C                   MOVE      §FLE          VFPN
124500910718     C* PROPONGO I DATI
124600060403     C                   move      §4occ1        VIDCC1            4 0
124700060403     C                   move      §4osc1        VIDSC1            7 0
124800991102     C* Imposto valuta distinta
124900991102     C                   MOVEL     V2CDIV        V3CDIV
125000000608     C                   MOVEL     V1PLIC        V3PLIC
125100910718     C*
125200910725     C     *IN13         DOWEQ     '0'
125300910725     C     *IN17         ANDEQ     '0'
125400980114     C                   SETOFF                                       59
125500950906     C                   Z-ADD     §TCONT        TOTCON
125600950906     C     TOTASS        ADD       TOTCON        TOTDIS
125700991105     C                   Z-ADD     TOTDIS        TOTVER
125800991118     C     START3        TAG
125900991102     C                   EXFMT     CN0W9D3
126000980114     C                   SETOFF                                       28
126100991118     C* CONTROLLO LA DATA OPERAZIONE SE ERRATA OPPURE NO
126200991118     C                   MOVEL     *BLANKS       G02ERR
126300991118     C                   Z-ADD     VDTOPE        G02DAT
126400991118     C                   CALL      'XSRDA8'
126500991118     C                   PARM                    WLBDAT
126600991118     C                   Z-ADD     G02INV        AMGDOP            8 0
126700991118     C     G02ERR        IFEQ      '1'
126800991118     C                   SETON                                        51
126900991118     C                   GOTO      START3
127000991118     C                   END
127100991118     C                   MOVE      G02DAT        VDTOPE
127200020422
127300020422     ** Eseguo sulla data operazione lo stesso controllo di PROJ,
127400020422     ** cioè la data non può discostare di più di 9 mesi dalla data
127500020422     ** corrente.
127600020422     C                   MOVE      VDtOpe        WrkDtOpe
127700020422     C                   TIME                    WrkDtCur
127800020422     C     WrkDtCur      SUBDUR    WrkDtOpe      WrkMonth:*M
127900020422     C                   IF        %ABS(WrkMonth) > 8
128000020422     C                   EVAL      *IN51 = *ON
128100020422     C                   GOTO      Start3
128200020422     C                   ENDIF
128300020422
128400910725     C  N17              DO
128500060824     c* se non ancora inserito contante e ci sono dei mandatini forzo KD
128600060824     c                   if        not *inkd and okcon=' '
128700060824     c     §fle          setll     yfcas01l                               33
128800060824     c                   if        *in33
128900060824     c                   eval      *inkd = '1'
129000060824     c                   eval      okcon = '1'
129100060824     c                   end
129200060824     c                   end
129300060824     c*
129400910723     C   KD              DO
129500910723     C                   EXSR      GESCON                                       GEST.CONTA
129600950906     C                   Z-ADD     §TCONT        TOTCON
129700950906     C     TOTASS        ADD       TOTCON        TOTDIS
129800991105     C                   Z-ADD     TOTDIS        TOTVER
129900991102     C                   Z-ADD     VIDDRG        VIDDDS
130000950906     C                   END
130100000608      *
130200910725     C   13              EXSR      CTR2
130300910726     C   59              SETOFF                                       13
130400910717     C                   END
130500000608      *
130600911125     C   17              SETOFF                                       1112
130700910717     C                   END
130800910717     C                   ENDSR
130900991102     C*-------------------------------------------------------------*
131000991102     C* CONTROLLI
131100991102     C*-------------------------------------------------------------*
131200910718     C     CTR2          BEGSR
131300930222     C*
131400910718     C                   Z-ADD     0             TOTVER
131500991103     C     TOTASS        ADD       TOTCON        TOTDIS
131600991105     C                   Z-ADD     TOTDIS        TOTVER
131700910718     C** NUMERO REG. OBBLIGATORIO
131800910723     C   72              SETON                                        59
131900910718     C   72              GOTO      START2
132000910718     C*  DATA REGISTRAZIONE
132100940329     C                   Z-ADD     VIDDRG        G02DAT
132200940329     C                   MOVEL     *BLANKS       G02ERR
132300991102     C                   CALL      'XSRDA8'
132400940329     C                   PARM                    WLBDAT
132500991102     C                   Z-ADD     G02INV        AMGDRG            8 0
132600991102     C                   Z-ADD     G02TGI        DATARE            8 0          *
132700021115     C* SE NON HO DA VERSARE NULLA VADO A FINE
132800060411     C     TOTVER        ifeq      0
132900021115     C                   GOTO      START2
133000060411     C                   End
133100021115     C  N13              GOTO      START2
133200910718     C*§ EMISSIONE ULTIMA VIDEATA
133300910718     C*  ------------------------
133400910718     C*  PULIZIA SFL 4
133500910718     C                   EXSR      PULI4
133600910718     C*  CARICAMENTO SFL 4
133700910718     C                   EXSR      CARI4
133800910726     C* ATTRIBUZIONE NR° DISTINTA SOLO LA PRIMA VOLTA
133900910726     C  N68              DO
134000910726     C                   MOVEL     '4D'          COD
134100910726     C                   MOVEL     §FLE          KEY
134200910726     C     KTAB          CHAIN     TABEL                              46
134300980114     C     *IN46         IFEQ      '1'
134400980114     C                   SETON                                        2859
134500980114     C                   MOVEL     ERR(4)        $MSG
134600980114     C                   GOTO      START2
134700980114     C                   END
134800910726     C                   MOVEL     TBLUNI        NUMVER
134900910726     C                   ADD       1             NUMVER
135000910726     C                   MOVEL     NUMVER        TBLUNI
135100980114     C  N46              UPDATE    TABEL
135200000609      * CON IL GIRO DELLA BANCA X CONTANTE (66=ON)
135300000609      *  IMPOSTA NEL NR.DISTINTA IL NR.DEL PLICO
135400000616     C                   Z-ADD     NUMVER        VIDNDS
135500991125     C                   Z-ADD     NUMVER        VIDNRG
135600991125     C                   MOVEL     §FLE          VIDNRG
135700910726     C                   SETON                                        68
135800910726     C                   END
135900910726     C*
136000910718     C     EMI4          TAG
136100991103     C                   WRITE     CN0W9Z5
136200991103     C                   EXFMT     CN0W9C5
136300910718     C*                    ____________
136400910725     C   16              SETON                                        59
136500931222     C   16              SETOFF                                       88
136600910725     C   16              GOTO      START2
136700910718     C*
136800910718     C*§ CONTROLLI CAMPI DEL SFL
136900910718     C*  -----------------------
137000991103     C                   Z-ADD     0             NR5
137100910718     C     *IN90         DOUEQ     '1'
137200991103     C                   ADD       1             NR5
137300991103     C     NR5           CHAIN     CN0W9S5                            90
137400910718     C  N90VIMP4         IFNE      *ZEROS
137500910718     C     VSPE4         IFEQ      'CO'
137600910718     C     VSPE4         OREQ      'RE'
137700910718     C     VSPE4         OREQ      'CA'
137800910718     C                   SETON                                        88
137900910718     C                   ELSE
138000910718     C                   SETOFF                                       88
138100910718     C                   END
138200910718     C*
138300910718     C*  CAUSALE CONTABILE
138400910718     C                   MOVEL     'PG'          OPECD1
138500910718     C                   MOVEL     VCAU4         OPECOD                         CAUSALE CO
138600910718     C     KEYOPE        CHAIN     CNOPE000                           87
138700991103     C   87              UPDATE    CN0W9S5
138800910718     C   87              GOTO      EMI4
138900910718     C                   MOVEL     OPEDES        VDES4
139000910718     C                   END
139100910718     C                   END
139200910718     C*
139300910726     C   15              DO
139400910718     C     SUDT3         TAG                                                    *
139500910718     C                   EXSR      STADIS
139600150109     c* verifica se esistono segnalazioni per mandare mail
139700150109     c                   if        lista(1) <> *blank
139800150109     c                   exsr      inviamail
139900150109     c                   endif
140000910718     C*
140100991103     C                   Z-ADD     0             NR5
140200910718     C     *IN90         DOUEQ     '1'
140300991103     C                   ADD       1             NR5
140400991103     C     NR5           CHAIN     CN0W9S5                            90
140500910718     C  N90VIMP4         COMP      0                                      90
140600930924     C*----------------------------------------------------*
140700930924     C     *IN90         IFEQ      '0'                                          *
140800991103     C* Scrivo movimenti contabili
140900991103     C                   EXSR      CREMOV                                       *
141000930924     C                   END                                                    *
141100000612      *
141200930924     C*----------------------------------------------------*
141300930924     C                   END
141400000612     C*----------------------------------------------------*
141500000612      * SCRIVE IL FILE DEI PLICHI DA TRASMETTERE A "SICURDATA"
141600000612      *
141700020321     C                   EXSR      WRIPLI
141800021118      * SCRIVE IL FILE DEI VERSAMENTI GIORNALIERI
141900021118      *
142000021121     C                   EXSR      WRTVRT
142100060411      * annulla i mandati cassaforte con ferritoia
142200060411      *
142300060922     c                   if        totcon <> 0
142400060411     C                   EXSR      annman
142500060922     c                   end
142600020321
142700991104     C*----------------------------------------------------*
142800991104     C* Sottomette trasmissioni                            *
142900991108     C                   COMMIT
143000000612      *
143100991104     C                   EXSR      SBMTRA                                       *
143200000612      *
143300930924     C*----------------------------------------------------*
143400910718     C                   EXSR      ANNDIS
143500000612      *
143600910726     C                   SETON                                        12
143700910726     C                   MOVE      '1'           *INKC
143800910801     C                   GOTO      START2
143900910718     C                   END
144000910726     C  N16              GOTO      EMI4
144100910718     C     START2        TAG
144200910718     C*
144300910718     C                   ENDSR
144400991102     C**----------------------------------------------------------
144500000000     C** PULIZIA DEL SUBFILE
144600991102     C**----------------------------------------------------------
144700000000     C     PULIZ         BEGSR
144800000000     C                   SETON                                        70
144900991102     C                   WRITE     CN0W9C2
145000000000     C                   SETOFF                                       70
145100910722     C                   SETON                                        71
145200991103     C                   Z-ADD     0             NR2
145300910725     C* PULISCO CAMPI DI COMODO
145400991102     C                   MOVE      V1CDIV        V2CDIV
145500991102     C                   MOVE      *BLANKS       PNRA
145600910725     C                   Z-ADD     0             PDTE
145700910725     C                   Z-ADD     0             PABI
145800910725     C                   Z-ADD     0             PCAB
145900910725     C                   MOVE      *BLANKS       PTPA
146000910725     C                   MOVE      *BLANKS       PTPP
146100910725     C                   MOVE      *BLANKS       PFVE
146200910725     C                   MOVE      *BLANKS       PTPI
146300910725     C                   MOVE      *BLANKS       PNOT
146400910725     C                   MOVE      *BLANKS       PTPD
146500910725     C                   Z-ADD     0             PLNP
146600910725     C                   Z-ADD     0             PAAS
146700910725     C                   Z-ADD     0             PNRS
146800910725     C                   Z-ADD     0             PNSP
146900950906     C                   Z-ADD     0             §TCONT
147000911219     C*                    MOVE *BLANKS   PPIA
147100910725     C*
147200000000     C                   ENDSR
147300991103     C*-------------------------------------------------------
147400900517     ** GESTIONE CONTANTI/ASSEGNI
147500991103     **-------------------------------------------------------
147600900517     C     GESCON        BEGSR
147700000608      *
147800910725     C                   SETOFF                                       1813
147900991102     C     V3CDIV        COMP      TGDDIV                                 09
148000000608      *
148100900517     C  N09              EXSR      PULCON
148200000608      *
148300900517     C* CARICAMENTO SUBFILE
148400991102     C                   MOVEL     V3CDIV        V4CDIV
148500000608     C                   MOVEL     V3PLIC        V4PLIC
148600060824     c                   if        not *in09
148700060824     C                   EXSR      CARCON
148800060824     C                   EXSR      CTRCON
148900060824     c                   end
149000900517     C*                    ----------
149100000608      *
149200900517     C                   SETON                                        09        1° VOLTA
149300900517     C     EMICON        TAG
149400991102     C                   WRITE     CN0W9Z4
149500991102     C                   EXFMT     CN0W9C4
149600000608      *
149700910725     C   18              GOTO      ENDCON
149800000608      *
149900000608     C* CONTROLLO CONTANTI IMMESSI
150000000608     C                   EXSR      CTRCON
150100000608      *
150200900518     C* CALCOLO  TOTALE CONTANTI /
150300910726     C   14              EXSR      CALCON
150400900517     C*                    -----------
150500910726     C  N14
150600910726     CANN18              GOTO      EMICON
150700000608      *
150800900517     C     ENDCON        TAG
150900900517     C                   ENDSR
151000991103     C**----------------------------------------------------------
151100900518     C** PULIZIA DEL SUBFILE CONTANTI
151200991103     C**----------------------------------------------------------
151300900517     C     PULCON        BEGSR
151400900517     C                   SETON                                        70
151500991102     C                   SETOFF                                       71
151600991102     C                   WRITE     CN0W9C4
151700900517     C                   SETOFF                                       70
151800900517     C                   ENDSR
151900991103     C**----------------------------------------------------------
152000900518     C** CARICAMENTO SUBFILE CONTANTI
152100991103     C**----------------------------------------------------------
152200900517     C     CARCON        BEGSR
152300000608      *
152400900517     C* CARICO I TAGLI PER IL CONTANTE
152500900517     C* L'IMPORTO IN TABELLA è 0 IL CAMPO IMPORTO è SPROTETTO
152600900517     C* LA DESCRIZ. DEI TAGLI CONTANTE è PROTETTO
152700991102     C                   CLEAR                   VNRTGL
152800900517     C                   SETON                                        40
152900991103     C                   Z-ADD     0             NR4               4 0
153000991104     C                   MOVEL     V3CDIV        TGDDIV
153100991102     C     TGDDIV        CHAIN     YNTGD01L                           21
153200991102     C     *IN21         DOWEQ     '0'
153300000621      *
153400000621     C     *IN66         IFEQ      *OFF
153500000621     C     *IN66         OREQ      *ON
153600000621     C     TGDVAL        ANDGT     0
153700991102     C                   MOVE      TGDDES        VDESCR
153800991104     C                   Z-ADD     TGDVAL        HIMTGL
153900900517     C     HIMTGL        COMP      0                                  20        PROTETTO
154000900517     C                   MOVEL     'C'           HIDTIP
154100060411     c                   exsr      srman
154200991103     C                   ADD       1             NR4
154300991102     C                   WRITE     CN0W9S4
154400991102     C                   SETON                                        71
154500000621     C                   END
154600000621      *
154700991102     C     TGDDIV        READE     YNTGD01L                               21
154800900517     C                   END
154900900517     C                   SETOFF                                       2040
155000020417     C                   EVAL      WrkLstRrnS4 = Nr4
155100000608      *
155200900517     C                   ENDSR
155300991103     C**----------------------------------------------------------
155400000608     C** CONTROLLO  CONTANTI  IMMESSI
155500991103     C**----------------------------------------------------------
155600000608     C     CTRCON        BEGSR
155700000608      *
155800020417     C                   IF        WrkLstRrnS4 > 0
155900000608      *  AGGIORNA SEMPLICEMENTE IL SFL
156000000608     C                   Z-ADD     0             NR4
156100000608 +++ C                   Z-ADD     0             §TCONT
156200000612     C                   Z-ADD     0             TOTIM            15 3
156300000608      *
156400000608     C                   DO        *HIVAL
156500000608     C                   ADD       1             NR4
156600000608     C     NR4           CHAIN     CN0W9S4                            63
156700000608      *
156800000608      *  PER FINE FILE ESCE DAL GIRO
156900000608     C     *IN63         IFEQ      *ON
157000000608     C                   LEAVE
157100000608     C                   END
157200000608      *
157300000608      *  CALCOLA LA RIGA
157400000608     C     HIDTIP        IFEQ      'C'
157500000608     C     HIMTGL        ANDNE     *ZEROS
157600060411     c                   eval      vidim = (vnrtgl + vnrman)*himtgl
157700000612     C                   ADD       VIDIM         TOTIM
157800000608     C     HIMTGL        COMP      0                                  20        PROT.VIDIM
157900000608     C                   SETON                                        40
158000000608     C                   UPDATE    CN0W9S4
158100000608     C                   END
158200000608     C*
158300000608     C                   END
158400020417      *
158500020417     C                   ENDIF
158600000608      *
158700000608     C                   ENDSR
158800000608     C**----------------------------------------------------------
158900000608     C** CALCOLO TOTALE  CONTANTI
159000000608     C**----------------------------------------------------------
159100000608     C     CALCON        BEGSR
159200020417      *
159300020417     C                   IF        WrkLstRrnS4 > 0
159400000608      *
159500991103     C                   Z-ADD     0             NR4
159600910730 +++ C                   Z-ADD     0             §TCONT
159700900517     C                   DO        *HIVAL
159800000612      *
159900991103     C                   ADD       1             NR4
160000991103     C     NR4           CHAIN     CN0W9S4                            63
160100910731     C     *IN63         IFEQ      '0'
160200000612      *
160300900517     C     HIDTIP        IFEQ      'C'
160400900517     C     HIMTGL        IFNE      *ZEROS
160500060411     c                   eval      vidim = (vnrtgl + vnrman)*himtgl
160600900517     C                   ADD       VIDIM         §TCONT
160700900517     C     HIMTGL        COMP      0                                  20        PROT.VIDIM
160800900517     C                   SETON                                        40
160900991102     C                   UPDATE    CN0W9S4
161000900517     C                   ELSE
161100900517     C                   ADD       VIDIM         §TCONT
161200900517     C                   END
161300000612      *
161400900517     C                   END
161500900517     C*
161600900517     C                   END
161700000612     C*
161800910731     C  N63              END
161900020417      *
162000020417     C                   ENDIF
162100000608      *
162200900517     C                   ENDSR
162300991103     C**----------------------------------------------------------
162400060411     C** aggiunge i mandati della cassaforte con ferritoia
162500991103     C**----------------------------------------------------------
162600060411     C     srman         BEGSR
162700060411     c                   clear                   vnrman
162800060411     c     §fle          setll     yfcas01l
162900060411     c                   do        *hival
163000060411     c     §fle          reade     yfcas01l
163100060411     c                   if        %eof(yfcas01l)
163200060411     c                   leave
163300060411     c                   end
163400060411     c                   if        CASVALORE = HIMTGL
163500060411     c                   add       casqta        vnrman
163600060411     c                   end
163700060411     c                   enddo
163800060411     C                   ENDSR
163900060411     C**----------------------------------------------------------
164000060411     C** annulla i mandati della cassaforte con ferritoia
164100060411     C**----------------------------------------------------------
164200060411     C     annman        BEGSR
164300061120     c* prima di cancellare chiamo il pgm per memorizzare in un archivio
164400061120     c* di sede i mandati
164500061120     c                   eval      fle= §fle
164600061120     c                   eval      kpjbu = CNC0WD00S
164700061120     c                   call      'CNC0WD00R'
164800061120     c                   parm                    kpjba
164900061120     C*
165000060411     c     §fle          setll     yfcas01l
165100060411     c                   do        *hival
165200060411     c     §fle          reade     yfcas01l
165300060411     c                   if        %eof(yfcas01l)
165400060411     c                   leave
165500060411     c                   end
165600060411     c                   delete    yfcas000
165700060411     c                   enddo
165800060411     C                   ENDSR
165900060411     C**----------------------------------------------------------
166000060411     C** PULIZIA DELL'ULTIMO  SUBFILE
166100060411     C**----------------------------------------------------------
166200060411     C     PULI4         BEGSR
166300891205     C                   SETON                                        70
166400991102     C                   WRITE     CN0W9C5
166500891205     C                   SETOFF                                       70
166600891205     C                   ENDSR
166700991103     C**----------------------------------------------------------
166800891205     C** CARICAMENTO SUBFILE 4
166900991103     C**----------------------------------------------------------
167000891205     C     CARI4         BEGSR
167100891205     C*
167200991104     C                   MOVEL     V3CDIV        V5CDIV
167300000608     C                   MOVEL     V3PLIC        V5PLIC
167400991103     C                   Z-ADD     0             NR5               4 0
167500000608      *
167600891205     C                   DO        20            A
167700891205     C     IMP(A)        IFNE      *ZEROS
167800891205     C                   MOVE      CSP(A)        VSPE4
167900891205     C                   Z-ADD     IMP(A)        VIMP4
168000991120     C                   Z-ADD     0             VNRP4
168100991120     C                   Z-ADD     0             VDRP4
168200891205     C                   MOVE      'D'           VDA4
168300891205     C                   MOVE      CAU(A)        VCAU4
168400940831     C                   SETOFF                                       32
168500891205     C                   Z-ADD     KBNA          VKCC4
168600910726     C                   Z-ADD     VBAN          VKSC4
168700891205     C*
168800891205     C                   SETOFF                                       88
168900991102     C                   ADD       1             NR5
169000991102     C                   WRITE     CN0W9S5
169100991103     C                   END
169200930927     C*
169300930927     C*  CARICO  SUBFILE X IMPORTI CAUSALI ALTRO ISTITUTO
169400930927     C*
169500930927     C     IM1(A)        IFNE      *ZEROS
169600930927     C                   MOVE      CSP(A)        VSPE4
169700930927     C                   Z-ADD     IM1(A)        VIMP4
169800930927     C                   MOVE      'D'           VDA4
169900930927     C                   MOVE      CA1(A)        VCAU4
170000940831     C                   SETOFF                                       32
170100930927     C                   Z-ADD     KBNA          VKCC4
170200930927     C                   Z-ADD     VBAN          VKSC4
170300991120     C                   Z-ADD     0             VNRP4
170400991120     C                   Z-ADD     0             VDRP4
170500930927     C*
170600930927     C                   SETOFF                                       88
170700991103     C                   ADD       1             NR5
170800991102     C                   WRITE     CN0W9S5
170900930927     C                   END
171000930927     C*
171100891205     C                   END
171200891205     C* CONTANTE
171300891205     C* --------
171400891205     C     TOTCON        IFGT      *ZEROS
171500891205     C* 88 ON RENDE MODIFICABILE LA CAUSALE
171600891205     C                   SETON                                        88
171700891205     C                   Z-ADD     1             A
171800891205     C                   Z-ADD     TOTCON        VIMP4
171900891205     C                   MOVE      'D'           VDA4
172000891205     C     '97'          LOOKUP    CSP(A)                                 21
172100891205     C     *IN21         IFEQ      '1'
172200891205     C                   MOVE      'CO'          VSPE4
172300891205     C                   MOVE      CAU(A)        VCAU4
172400940831     C                   SETOFF                                       32
172500891205     C                   Z-ADD     KBNA          VKCC4
172600910726     C                   Z-ADD     VBAN          VKSC4
172700991120     C                   Z-ADD     0             VNRP4
172800991120     C                   Z-ADD     0             VDRP4
172900891205     C*
173000991103     C                   ADD       1             NR5
173100991102     C                   WRITE     CN0W9S5
173200891205     C                   END
173300991104     C                   END
173400021114      * CONTROPARTITA
173500030115     c                   z-add     1             a
173600021114     C     '99'          LOOKUP    CSP(A)                                 21
173700021114     C     *IN21         IFEQ      '1'
173800021114     C                   Z-ADD     totver        VIMP4
173900021114     C                   MOVE      'CA'          VSPE4
174000021114     C                   MOVE      CAU(A)        VCAU4
174100021114     C                   MOVE      'A'           VDA4
174200021114     C* Per le filiali di primo livello ribalto il versato in cassa
174300021125     C* Per le Secondo Livello ribalto il versamento negli inc. da attrib.
174400060403     C                   Z-ADD     VIDCC1        VKCC4
174500060403     C                   Z-ADD     VIDSC1        VKSC4
174600991120     C                   Z-ADD     §FLE          VNRP4
174700991122     C                   Z-ADD     WTODAY        VDRP4
174800891205     C*
174900891206     C                   SETON                                        88
175000991102     C                   ADD       1             NR5
175100991102     C                   WRITE     CN0W9S5
175200021114     C                   END
175300991104     C*
175400891205     C                   ENDSR
175500891205     C/EJECT
175600000608     C**------------------------------------------------------------
175700000000     C** ANNULLAMENTO DISTINTA STAMPATA
175800000608     C**------------------------------------------------------------
175900000000     C     ANNDIS        BEGSR
176000000608     C*
176100910719     C                   DO        TREC          CNT
176200991103     C     CNT           CHAIN     CN0W9S2                            21
176300000608     C*
176400910731     C     VANN          IFNE      'E'
176500910726     C                   MOVEL     VNRA          KNRA
176600910726     C                   MOVE      VNRA1         KNRA
176700910726     C                   Z-ADD     VABI          KABI
176800910726     C                   Z-ADD     VCAB          KCAB
176900940126     C                   SETOFF                                       57
177000000608     C*
177100941103     C     KASS          SETLL     CNASS000                           57
177200000608     C*
177300920206     C     *IN57         DOWEQ     '0'
177400941103     C     KASS          READE     CNASS000                               57
177500910719     C*
177600951027     C     ASSFVE        IFEQ      'V'
177700951027     C     ASSTPI        ANDNE     'M'
177800150313     c  n57              exsr      wrttnass
177900941103     C  N57              DELETE    CNASS000
178000940126     C                   END
178100000608     C*
178200951027     C                   END
178300940126     C*
178400940126     C     SAVNRA        IFNE      VNRA
178500940126     C     SAVNR1        ORNE      VNRA1
178600940126     C     SAVABI        ORNE      VABI
178700940126     C     SAVCAB        ORNE      VCAB
178800000608     C*
178900940126     C                   MOVEL     SAVNRA        KNRA
179000940126     C                   MOVE      SAVNR1        KNRA
179100940126     C                   Z-ADD     SAVABI        KABI
179200940126     C                   Z-ADD     SAVCAB        KCAB
179300940126     C                   SETOFF                                       57
179400000608     C*
179500941103     C     KASS          SETLL     CNASS000                           57
179600000608     C*
179700940126     C     *IN57         DOWEQ     '0'
179800941103     C     KASS          READE     CNASS000                               57
179900941103     C*          KASS      CHAINCNASS000             57
180000940126     C*
180100951027     C     ASSFVE        IFEQ      'V'
180200951027     C     ASSTPI        ANDNE     'M'
180300150313     c  n57              exsr      wrttnass
180400941103     C  N57              DELETE    CNASS000
180500951027     C                   END
180600910719     C*
180700920206     C                   END
180800940126     C                   END
180900910719     C                   ELSE
181000901011     C                   EXSR      PULCON
181100901011     C                   EXSR      CARCON
181200910719     C                   END
181300910722     C                   END
181400911125     C                   SETON                                        69
181500000000     C                   ENDSR
181600000608     C**-------------------------------------------------------------
181700000608     C** SCRIVE L'ARCIHVIO DEI PLICHI DA TRASMETTERE A SICURDATA
181800000608     C**-------------------------------------------------------------
181900000608     C     WRIPLI        BEGSR
182000020417
182100020417     C                   IF        V1Plic <> 0
182200020417
182300000612      *  LEGGE IL SFL PER SCRIVERE IL FILE DEL PLICO DA TRASMETTERE
182400020417     C                   IF        WrkLstRrnS4 > 0
182500020417      *
182600000612     C                   Z-ADD     0             NR4
182700000612     C                   Z-ADD     0             §TCONT
182800000612     C                   Z-ADD     0             NRGPLI            5 0
182900000612     C                   TIME                    FLD14            14 0
183000000612     C                   MOVEL     FLD14         ORAMIN            4 0
183100000612      *
183200000612     C                   DO        *HIVAL
183300000612     C                   ADD       1             NR4
183400000612     C     NR4           CHAIN     CN0W9S4                            63
183500000612      *
183600000612      *  PER FINE FILE ESCE DAL GIRO
183700000612     C     *IN63         IFEQ      *ON
183800000612     C                   LEAVE
183900000612     C                   END
184000000612      *
184100050405      **** Tolta gestione Plichi
184200000612      * IMPOSTA I CAMPI DI SCRITTURA DEL FNPLI00F
184300050405     C**** VNRTGL        IFGT      0
184400050405      ****
184500050405     C****               EXSR      SetFnPli
184600050405     C****               ADD       1             NRGPLI
184700050405     C****               Z-ADD     NRGPLI        PLIRIG
184800050405     C****               Z-ADD     HIMTGL        PLITAG
184900050405     C****               Z-ADD     VNRTGL        PLIQTA
185000050405     C****               Z-ADD     VIDIM         PLITIM
185100050405     C****               EXSR      WrtFnPli
185200050405      ****
185300050405     C****               ENDIF
185400000612      *
185500000612      * AGGIORNA IL SFL
185600000612     C     HIMTGL        COMP      0                                  20        PROT.VIDIM
185700000612     C                   SETON                                        40
185800000612     C                   UPDATE    CN0W9S4
185900000612     C*
186000000612     C                   END
186100020417     C*
186200020417     C                   ENDIF
186300020326
186400050405     **   Tolta gestione dei PLICHI
186500020326     ** Scrittura di una riga aggiuntiva per gli assegni.
186600050405     C********           IF        TotAss > 0
186700050405     C********           EXSR      SetFnPli
186800050405     C********           EVAL      PliRig = *HIVAL
186900050405     C********           EVAL      PliTim = TotAss
187000050405     C********           EXSR      WrtFnPli
187100050405     C********           ENDIF
187200050405      ********
187300020417     C                   ENDIF
187400020326
187500000608     C                   ENDSR
187600020326
187700020326     ***********************************************************************
187800020326     **
187900020326     ** Impostazione dei campi fissi di FNPLI000.
188000050405     **      TOLTA gestione dei PLICHI
188100020326     ***********************************************************************
188200050405     C*****SetFnPli      BEGSR
188300050405      ********
188400050405     C********           CLEAR                   FNPLI000
188500050405     C********           MOVEL     §FLE          PLIFIL
188600050405     C********           Z-ADD     V1PLIC        PLINUM
188700050405     C********           Z-ADD     *DATE         G02DAT
188800050405     C********           MOVE      *ZEROS        G02INV
188900050405     C********           MOVEL     *BLANK        G02ERR
189000050405     C********           CALL      'XSRDA8'
189100050405     C********           PARM                    WLBDAT
189200050405     C********           MOVE      G02INV        PLIDIM
189300050405     C********           MOVE      ORAMIN        PLIHIM
189400050405     C********           MOVEL     V1CDIV        PLIDIV
189500050405      ********
189600050405     C********           ENDSR
189700020326
189800020326     ***********************************************************************
189900020326     **
190000020326     ** Impostazione dei campi fissi di FNPLI000.
190100050405     **      TOLTA gestione dei PLICHI
190200020326     ***********************************************************************
190300050405     C*****WrtFnPli      BEGSR
190400050405      ********
190500050405     C********           ADD       1             NRPLI
190600050405     C********           WRITE     FNPLI000
190700050405      ********
190800050405     C********           ENDSR
190900050405      ********
191000000608     C**-------------------------------------------------------------
191100021118     C** CREAZIONE ARCHIVIO DEI VERSAMENTI GIORNALIERI
191200000608     C**-------------------------------------------------------------
191300021118     C     WRTVRT        BEGSR
191400021118     C                   Z-ADD     §FLE          VRTFLE
191500021118     C                   Z-ADD     DUTCOU        VRTCUC
191600021118     C                   Z-ADD     NUMVER        VRTNDV
191700021118     C                   Z-ADD     AMGUD8        VRTDDV
191800021127     C                   Z-ADD     AMGdop        VRTope
191900021121     C                   Z-ADD     KBNA          vrtkcc
192000021121     C                   Z-ADD     VBAN          vrtksc
192100021118     C                   MOVEL     V5CDIV        VRTVCA
192200021118     C                   Z-ADD     TOTCON        VRTCON
192300021118     C                   Z-ADD     TOTASS        VRTASS
192400021120     c                   clear                   vrtflr
192500021118     C                   WRITE     FNVRT000
192600021118     C                   ENDSR
192700021118     C**-------------------------------------------------------------
192800021118     C** CREAZIONE MOVIMENTI CONTABILI
192900021118     C**-------------------------------------------------------------
193000021118     C     CREMOV        BEGSR
193100000608     C*
193200991103     C                   MOVE      KNMUS         MOVNMU
193300991103     C                   MOVE      KNRAZ         MOVRIF
193400991103     C                   MOVEL     §FLE          MOVRIF
193500991103     C                   Z-ADD     VIDNRG        MOVSBN
193600991103     C                   ADD       1             REGSWD                         *PROGR.REG
193700991103     C                   Z-ADD     REGSWD        MOVSBR
193800991103     C                   MOVEL     'V'           MOVFGE
193900010312     C                   MOVEL     '201'         MOVSOC
194000991103     C                   MOVEL     *BLANKS       MOVFPN
194100991103     C                   MOVEL     §FLE          MOVFPN
194200991103     C                   MOVEL     'D'           MOVTMO
194300991103     C                   MOVEL     'CG'          MOVCTB
194400000000     C                   Z-ADD     VIDNRG        MOVNRG
194500000000     C                   Z-ADD     AMGDRG        MOVDRG
194600991103     C                   MOVE      'COGE'        MOVSRR
194700991103     C                   MOVEL     VCAU4         MOVCOD
194800991103     C                   MOVE      *ZEROS        MOVKCC
194900991103     C                   MOVE      *ZEROS        MOVKSC
195000991103     C                   MOVE      VKCC4         MOVKCC
195100991103     C                   MOVE      VKSC4         MOVKSC
195200021125     C                   MOVE      *ZEROS        MOVCCP
195300021125     C                   MOVE      *ZEROS        MOVSCP
195400991103     C* Dare/Avere
195500991103     C     VDA4          IFEQ      'D'
195600060403     C                   MOVE      VIDCC1        MOVCCP
195700060403     C                   MOVE      VIDSC1        MOVSCP
195800991103     C                   Z-ADD     1             MOVDAR
195900991103     C                   Z-ADD     0             MOVAVE
196000991103     C                   ELSE
196100021125     C                   MOVE      kbna          MOVCCP
196200021125     C                   MOVE      vban          MOVSCP
196300991103     C                   Z-ADD     0             MOVDAR
196400991103     C                   Z-ADD     1             MOVAVE
196500991103     C                   END
196600991103     C* Se la divisa di versamento è diversa da quella
196700991103     C* di conto effettuo la conversione per l'importo
196800991103     C* in ctb.
196900010626     C     V5CDIV        IFEQ      §GEDMC
197000991103     C                   Z-ADD     VIMP4         MOVIMM
197100991103     C                   ELSE
197200991103     C                   CLEAR                   YEURDS
197300991103     C                   MOVEL     V5CDIV        YECDVI
197400991103     C                   Z-ADD     VIMP4         YECIMI
197500010626     C                   MOVEL     §GEDMC        YECDVO
197600991103     C                   MOVEL     'H'           YECTAR
197700991103     C                   CALL      'YEURCO'
197800991103     C                   PARM                    YEURDS
197900991103     C                   Z-ADD     YECIMO        MOVIMM
198000991103     C                   END
198100991103     C                   Z-ADD     VIMP4         MOVIMD
198200991103     C                   MOVEL     V5CDIV        MOVDIV
198300991103     C                   Z-ADD     AMGDRG        MOVDRC
198400020321
198500020321     C                   IF        V4Plic = 0
198600020321     C                   Z-ADD     VIDNDS        MOVNDC
198700020321     C                   ELSE
198800020321     C                   Z-ADD     V4PLIC        MOVNDC
198900020321     C                   ENDIF
199000020321
199100991118     C                   Z-ADD     AMGDOP        MOVDDC
199200991103     C                   MOVE      *BLANKS       MOVSRD
199300000000     C                   Z-ADD     MOVNDC        MOVNRP
199400021126     c                   move      §4occ1        comcc1            4 0
199500021126     C     VKCC4         IFEQ      comcc1                                       INC. ATTRI
199600021126     C                   Z-ADD     VNRP4         MOVNRP                         2° LIV.
199700021126     C                   END
199800991120     C                   MOVE      VDRP4         G02DAT
199900991120     C                   MOVE      *BLANKS       G02ERR
200000991120     C                   CALL      'XSRDA8'
200100991120     C                   PARM                    WLBDAT
200200991120     C                   Z-ADD     G02INV        MOVDRP
200300991103     C                   MOVEL     *BLANKS       MOVSRP
200400010626     C                   MOVEL     §GEDMC        MOVDIP
200500991103     C                   Z-ADD     MOVIMM        MOVIPM
200600991105     C                   Z-ADD     MOVIMM        MOVIPD
200700991103     C                   Z-ADD     0             MOVDSC                         *
200800991103     C                   MOVEL     VDES4         MOVDES
200900991103     C                   MOVEL     *BLANKS       MOVCDC
201000991103     C                   Z-ADD     0             MOVDCA
201100991103     C                   Z-ADD     MOVDRG        MOVDOP
201200010626     C                   Z-ADD     1             MOVCMB
201300030403     C                   ADD       1             NRMOV             7 0
201400991103     C                   WRITE     FNMOV000
201500000000     C                   ENDSR
201600150313     c*__________________________________________________________________
201700150313     c     wrttnass      begsr
201800150313     c*__________________________________________________________________
201900150409     C*                  IF        V4Plic = 0
202000150409     C*                  Z-ADD     VIDNDS        assNDv
202100150409     C*                  ELSE
202200150409     C*                  Z-ADD     V4PLIC        assNDv
202300150409     C*                  ENDIF
202400150313
202500150409     C                   Z-ADD     VIDnrg        assNDv
202600150422     C                   movel     0             assNDv
202700150326     C                   Z-ADD     vass          asscas
202800150330     C                   Z-ADD     vabi          assabi
202900150330     C                   Z-ADD     vcab          asscab
203000150330     C                   MOVEL     vnra          assnra
203100150330     C                   MOVE      vnra1         assnra
203200150330     C                   MOVEL     VTPA          asstpa
203300150330     C                   MOVEL     VTPI          asstpi
203400150330     C                   MOVEL     VTPP          asstpp
203500150330     C                   MOVEL     VPIA          asspia
203600150330     C                   MOVEL     VNOT          assnot
203700150313     C                   Z-ADD     AMGDOP        assDDv
203800150330     c                   move      vdte          dtaeur
203900150330     c                   move      dtaeur        dtaiso
204000150330     c                   move      dtaiso        assdte
204100150313     c                   write     tnass000
204200150313     c                   endsr
204300991103     C*-------------------------------------------------------
204400991103     ** Sottometto trasmissioni
204500991103     **-------------------------------------------------------
204600991103     C     SBMTRA        BEGSR
204700991104     C*
204800020926     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
204900991104     C* da/a cui spedire
205000020926     C                   CLEAR                   TIBS55
205100991104     C                   MOVEL     'L'           I50TLA
205200991104     C                   MOVEL     KNSIF         I50PSI
205300060421     C                   MOVEL     §fle          I50PPO
205400991104     C                   MOVEL     '046'         I50APO
205500020926     C                   CALL      'TIBS55R'
205600020926     C                   PARM                    TIBS55
205700991103     C*
205800991104     C                   MOVEL     '046'         CMZDST                          DEST.:SED
205900991104     C                   MOVEL     'FNMOV00T'    CMZFLD                          DA: >>>
206000020926     C                   EVAL      CmzMbD = 'M' + %EDITC(§Fle:'X') + CmzDst
206100991104     C                   MOVEL     O50PLA        CMZLBD
206200991104     C                   MOVEL     'FNMOV00R'    CMZFLA                          A: >>>
206300020926     C                   MOVEL     CmzMbD        CMZMBA
206400991104     C                   MOVEL     O50ALA        CMZLBA
206500021001     C                   MOVEL     'GAITRA'      CMZLBA
206600021001     C                   MOVE      O50ALA        WQUATR            4
206700021001     C     WQUATR        IFEQ      '101 '
206800021001     C                   MOVE      '201 '        CMZLBA
206900021001     C                   ENDIF
207000991104     C                   MOVE      '0000'        CMZORI                          ORA
207100991104     C                   MOVE      '0000000'     CMZDTI                          DATA
207200991104     C                   MOVE      'N'           CMZURG                          NOTTE
207300991104     C                   MOVE      'A'           CMZADD                          IN ADD
207400991104     C                   MOVE      'S'           CMZCLR                          CLEAR
207500991104     C                   MOVE      'S'           CMZFTR                          DA TRANS.
207600991104     C                   MOVE      *BLANK        CMZERR                          1= ERR.
207700991108     C                   MOVE      'Y029'        CMZAZN                          AZIONE
207800991104     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
207900021001     C                   MOVEL     'GAITRA'      CMZSYF
208000021001     C                   MOVE      O50ASI        WQUATR
208100021001     C     WQUATR        IFEQ      '101 '
208200021001     C                   MOVE      '201 '        CMZSYF
208300021001     C                   ENDIF
208400991104     C                   MOVE      NRMOV         CMZNRR                          N.REC.
208500991104     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-
208600991104     C*
208700991104     C                   CLEAR                   W236
208800020926     C                   CALL      'TIBS53C'
208900991104     C                   PARM                    O50LCL
209000991104     C                   PARM                    CMZDST
209100991104     C                   PARM                    CMZFLD
209200991104     C                   PARM                    CMZMBD
209300991104     C                   PARM                    CMZLBD
209400991104     C                   PARM                    CMZFLA
209500991104     C                   PARM                    CMZMBA
209600991104     C                   PARM                    CMZLBA
209700991104     C                   PARM                    CMZORI
209800991104     C                   PARM                    CMZDTI
209900991104     C                   PARM                    CMZURG
210000991104     C                   PARM                    CMZADD
210100991104     C                   PARM                    CMZCLR
210200991104     C                   PARM                    CMZFTR
210300991104     C                   PARM                    CMZERR
210400991104     C                   PARM                    CMZAZN
210500991104     C                   PARM                    CMZSYF
210600991104     C                   PARM                    CMZNRR
210700991104     C                   PARM                    CMZPRR
210800991104     C                   PARM                    W236            236
210900991103     C*
211000991103     C                   ENDSR
211100000612     C**---------------------------------------------------------------
211200000000     C** STAMPA DISTINTA BANCA
211300000612     C**---------------------------------------------------------------
211400000000     C     STADIS        BEGSR
211500910726     C                   EXCEPT    TESTA
211600910719     C                   DO        TREC          CNT
211700991103     C     CNT           CHAIN     CN0W9S2                            20
211800930927     C                   SETOFF                                       54
211900930927     C*
212000930927     C*  CONTROLLO SE ASSEGNO STESSO/ALTRO ISTITUTO
212100930927     C*
212200910731     C     VANN          IFNE      'E'
212300911127+++  C     VASS          ANDNE     0
212400930927     C*
212500930927     C                   SETON                                        54
212600930927     C*
212700930927     C*  CONTROLLO SE ASSEGNO STESSO/ALTRO ISTITUTO
212800930927     C     ISTABI        IFEQ      VABI
212900930927     C                   SETON                                        53
213000930927     C                   ELSE
213100930927     C                   SETOFF                                       53
213200930927     C                   END
213300991105     C                   Z-ADD     VDTE          GMADAS            8 0
213400000000     C                   Z-ADD     1             A                 3 0
213500910719     C                   MOVEL     VTPA          LKP
213600910719     C                   MOVE      VTPP          LKP
213700910719     C     LKP           LOOKUP    CSP(A)                                 21
213800000000     C  N21              Z-ADD     1             A
213900000000     C                   SETOFF                                       22
214000910719     C     VTPP          IFEQ      'F'
214100991104     C                   ADD       VASS          TOTFUO           12 2
214200000000     C                   ELSE
214300991104     C                   ADD       VASS          TOTPIA           12 2
214400000000     C                   SETON                                        22
214500000000     C                   END
214600000000     C** TOTALE ASSEGNO PER TIPO
214700000000     C                   ADD       1             CONRIG            2 0
214800000000     C     CONRIG        IFGT      50
214900000000     C                   EXCEPT    TESTA
215000000000     C                   Z-ADD     1             CONRIG
215100000000     C                   END
215200991104     C* Controllo se importo con decimali
215300991104     C                   MOVE      VASS          WCOMO2            2 0
215400991104     C     WCOMO2        COMP      0                                      02
215500000000     C                   EXCEPT    DET
215600910719     C                   END
215700910726     C                   END
215800991104     C                   MOVE      TOTPIA        WCOMO2            2 0
215900991104     C     WCOMO2        COMP      0                                      03
216000991104     C                   MOVE      TOTFUO        WCOMO2            2 0
216100991104     C     WCOMO2        COMP      0                                      04
216200991104     C                   MOVE      TOTASS        WCOMO2            2 0
216300991104     C     WCOMO2        COMP      0                                      06
216400000000     C                   EXCEPT    TOT
216500000000     C                   ADD       2             CONRIG
216600000000     C** totale assegni fine elenco
216700000000     C     TOTCON        IFGT      0
216800900518     C*  DETTAGLIO SFL CONTANTE
216900991103     C                   Z-ADD     0             NR4
217000900518     C                   DO        *HIVAL
217100991103     C                   ADD       1             NR4
217200991103     C     NR4           CHAIN     CN0W9S4                            63
217300910731     C     *IN63         IFEQ      '0'
217400900518     C     HIDTIP        ANDEQ     'C'
217500900518     C                   ADD       1             CONRIG            2 0
217600900518     C     CONRIG        IFGT      50
217700900518     C                   EXCEPT    TESTA
217800900518     C                   Z-ADD     1             CONRIG
217900900518     C                   END
218000991105     C* Controllo se importo da stampare a decimali
218100991105     C                   MOVE      VIDIM         WCOMO2
218200991105     C     WCOMO2        COMP      0                                      08
218300060421     c                   eval      stanrt = vnrtgl + vnrman
218400900518     C                   EXCEPT    DETSFL
218500900518     C                   END
218600910731     C  N63              END
218700991105     C* Controllo se importo da stampare a decimali
218800991105     C                   MOVE      TOTCON        WCOMO2
218900991105     C     WCOMO2        COMP      0                                      09
219000000000     C                   EXCEPT    TOT2
219100000000     C                   END
219200000000     C** STAMPA TOTALE CONTANTI
219300000000     C                   DO        20            A
219400000000     C     IMP(A)        IFGT      0
219500930927     C                   SETON                                        53
219600000000     C                   ADD       1             CONRIG
219700000000     C     CONRIG        IFGT      50
219800000000     C                   EXCEPT    TESTA
219900000000     C                   Z-ADD     1             CONRIG
220000000000     C                   END
220100991104     C                   MOVE      IMP(A)        WCOMO2
220200991104     C     WCOMO2        COMP      0                                      07
220300000000     C                   EXCEPT    TOT1
220400000000     C                   END
220500930927     C*
220600930927     C     IM1(A)        IFGT      0
220700930927     C                   SETOFF                                       53
220800930927     C                   ADD       1             CONRIG
220900930927     C     CONRIG        IFGT      50
221000930927     C                   EXCEPT    TESTA
221100930927     C                   Z-ADD     1             CONRIG
221200930927     C                   END
221300991104     C                   MOVE      IM1(A)        WCOMO2
221400991104     C     WCOMO2        COMP      0                                      07
221500930927     C                   EXCEPT    TOT1
221600930927     C                   END
221700930927     C*
221800000000     C                   END
221900991105     C* Controllo se importo da stampare a decimali
222000991105     C                   MOVE      TOTDIS        WCOMO2
222100991105     C     WCOMO2        COMP      0                                      02
222200991105     C                   MOVE      TOTVER        WCOMO2
222300991105     C     WCOMO2        COMP      0                                      04
222400000000     C** STAMPA TOTALI ASSEGNI PER SPECIE
222500000000     C                   EXCEPT    TOT3
222600000000     C** STAMPA TOTALI FINALI
222700000000     C     NOS           TAG
222800000000     C                   ENDSR
222900891205     C/EJECT
223000000000     C**----------------------------------
223100891204     C**  CARICAMENTO TABELLE
223200000000     C**----------------------------------
223300000000     C     CARTAB        BEGSR
223400991102     C*
223500991102     C*§ LEGGO TABELLA '4E'
223600991102     C                   MOVEL     '4E'          COD
223700991102     C     KTAB1         SETLL     TABEL                                  23
223800991102     C  N23              GOTO      NOTAB
223900991102     C                   Z-ADD     0             A
224000991102     C     SUTAB         TAG
224100991102     C     KTAB1         READE     TABEL                                  23
224200991102     C  N23              EXCEPT    RILTAB
224300991102     C   23              GOTO      NOTAB
224400991102     C                   MOVEL     TBLKEY        SPE               2
224500991102     C     SPE           IFNE      *BLANKS
224600991102     C                   ADD       1             A
224700991102     C                   MOVEL     TBLUNI        DS4E
224800991102     C                   MOVEL     SPE           CSP(A)
224900991102     C                   MOVEL     §SPDES        DSP(A)
225000991102     C                   MOVEL     §SPCAU        CAU(A)                         CAUSALE CO
225100991102     C     §SPCA2        IFNE      *BLANKS
225200991102     C                   MOVEL     §SPCA2        CA1(A)                         CAUSALE CO
225300991102     C                   ELSE
225400991102     C                   MOVEL     §SPCAU        CA1(A)                         CAUSALE CO
225500991102     C                   END
225600991102     C                   MOVEL     §SPCAU        OPECOD                         CAUSALE CO
225700991102     C                   MOVEL     'PG'          OPECD1
225800991102     C     KEYOPE        CHAIN     CNOPE000                           80
225900991102     C   80              GOTO      NOTAB
226000991102     C                   MOVEL     CA1(A)        OPECOD                         CAUSALE CO
226100991102     C                   MOVEL     'PG'          OPECD1
226200991102     C     KEYOPE        CHAIN     CNOPE000                           80
226300991102     C   80              GOTO      NOTAB
226400991102     C*
226500991102     C* CARICO CAUSALE PER FUORI PIAZZA SE NON SONO CAUSALI CED
226600991102     C*
226700991102     C                   MOVEL     SPE           CT                1
226800991102     C     CT            IFNE      '9'
226900991102     C                   ADD       1             A
227000991102     C                   MOVEL     SPE           CSP(A)
227100991102     C                   MOVE      'F'           CSP(A)
227200991102     C                   MOVEL     §SPDE1        DSP(A)
227300991102     C                   MOVEL     §SPCA1        CAU(A)                         CAUSALE CO
227400991102     C     §SPCA3        IFNE      *BLANKS
227500991102     C                   MOVEL     §SPCA3        CA1(A)                         CAUSALE CO
227600991102     C                   ELSE
227700991102     C                   MOVEL     §SPCA1        CA1(A)                         CAUSALE CO
227800991102     C                   END
227900991102     C                   MOVEL     §SPCA1        OPECOD                         CAUSALE CO
228000991102     C                   MOVEL     'PG'          OPECD1
228100991102     C     KEYOPE        CHAIN     CNOPE000                           80
228200991102     C   80              GOTO      NOTAB
228300991102     C                   MOVEL     CA1(A)        OPECOD                         CAUSALE CO
228400991102     C                   MOVEL     'PG'          OPECD1
228500991102     C     KEYOPE        CHAIN     CNOPE000                           80
228600991102     C   80              GOTO      NOTAB
228700991102     C                   END
228800991102     C*
228900991102     C*
229000991102     C                   END
229100991102     C                   GOTO      SUTAB
229200000000     C     NOTAB         TAG
229300991102     C*  Aggancio tabella 'GED' x reperire la divisa di conto
229400991102     C                   MOVEL     'GED'         KCOD1
229500991102     C                   MOVEL     *BLANKS       KKEY1
229600991102     C                   MOVEL     '1'           KKEY1
229700991102     C     KTBE          CHAIN     TNTBE01L                           13
229800991102     C     *IN13         IFEQ      '0'
229900991102     C                   MOVEL     TBEUNI        DGED
230000991102     C                   END
230100991102     C* leggo la tabella CV e reperisco la divisa alternativa a
230200991102     C* quella corrente
230300991102     C                   MOVEL     'CV'          COD
230400991102     C     KTAB1         CHAIN     TABEL00F                           50
230500991102     C     *IN50         DOWEQ     '0'
230600991102     C                   MOVEL     TBLUNI        DSCV
230700991103     C                   MOVEL     TBLKEY        WDIV              3
230800991102     C     §CVDVA        IFEQ      'S'
230900010626     C     §GEDCR        ANDNE     WDIV
231000991103     C                   MOVEL     WDIV          DIVALT            3
231100011220     C**                 Z-ADD     §CVCMB        CMBALT           13 6
231200991209     C                   Z-ADD     §CVDEC        DECALT            2 0
231300991102     C                   END
231400010626     C     §GEDCR        IFEQ      WDIV
231500991209     C                   Z-ADD     §CVDEC        DECDIV            2 0
231600991209     C                   END
231700991102     C     KTAB1         READE     TABEL00F                               50
231800991102     C                   END
231900991102     C*
232000000000     C                   ENDSR
232100891205     C*----------------------------------------------------*
232200911115     C* CARICAMENTO FILIALI VALIDE
232300911115     C*----------------------------------------------------*
232400911115     C     CARTB         BEGSR
232500991102     C*
232600020507     C                   MOVEL     '£1'          D06COD
232700060421     C                   MOVEL     simfel        D06KEY
232800020507     C                   MOVEL     TR06DS        KPJBU
232900020507     C                   CALL      'TRUL06R'
233000020507     C                   PARM                    KPJBA
233100020507     C                   MOVEL     KPJBU         TR06DS
233200020507     C                   MOVEA     LIN           fila
233300021115     c*
233400911115     C                   ENDSR
233500150108     C*----------------------------------------------------*
233600150108     C*  Verifica se assegni esclusi devono essere segnalati
233700150108     C*----------------------------------------------------*
233800150108     C     VerEsclusi    BEGSR                                                  *
233900150109     c                   move      vdte          dataeur
234000150109     c                   move      dataeur       dataisom
234100150108     c     dataiso       subdur    dataisom      diffe:*d          3 0
234200150129     c                   if        diffe > 6
234300150108     c                   z-add     1             xy
234400150108     c     vuoto         lookup    lista(xy)                              99
234500150109     c   99              eval      lista(xy) = 'Assegno: ' + vnra + vnra1
234600150112     c                             + 'data ' +%editc(vdte:'Y') + ' ' +
234700150112     c                             'ABI: ' + %editc(vabi:'X') + ' ' +
234800150112     c                             'CAB: '+  %editc(vcab:'X') + ' ' +
234900150112     c                             'Importo: ' + %editc(vass:'2') + ' ' + vdes
235000150109     c  n99              clear                   lista(10)
235100150108     c  n99              eval      lista(10) = 'Presenti altri assegni oltre 10'
235200150108     c                   endif
235300150108     c
235400150108     C                   endsr
235500150108     C*----------------------------------------------------*
235600150108     C*  invia mail di segnalazione assegni non versati
235700150108     C*----------------------------------------------------*
235800150108     C     Inviamail     BEGSR                                                  *
235900150112     C                   eval      wrkEml= 'gestione.clienti@brt.it'
236000150108
236100150108     C                   eval      wrkOgg='Segnalazione assegni non versati '+
236200150108     C                             'Filiale ' + %EDITC(§Fle:'X')
236300150108     C                   eval      wrkMsg='Attenzione il versamento effettu' +
236400150108     C                             'ato dal profilo ' + knmus +' ha escluso '+
236500150204     C                             'assegni da oltre 6 giorni' +':/N'+
236600150108     C                             lista(1) +':/N'+
236700150108     C                             lista(2) +':/N'+
236800150108     C                             lista(3) +':/N'+
236900150108     C                             lista(4) +':/N'+
237000150108     C                             lista(5) +':/N'+
237100150108     C                             lista(6) +':/N'+
237200150108     C                             lista(7) +':/N'+
237300150108     C                             lista(8) +':/N'+
237400150108     C                             lista(9) +':/N'+
237500150108     C                             lista(10)
237600150108     C*
237700150108     C                   call(e)   'TIS701C1'
237800150108     C                   parm                    wrkEml
237900150108     C                   parm                    wrkEmcc
238000150108     C                   parm                    wrkOgg
238100150108     C                   parm                    wrkMsg
238200150108     C                   parm                    wrkesit
238300150108     C                   endsr
238400940831     C*----------------------------------------------------*
238500940831     C*  OPERAZIONI INIZIALI                               *
238600940831     C*----------------------------------------------------*
238700940831     C     *INZSR        BEGSR                                                  *
238800940831     C*
238900940831     C     *ENTRY        PLIST
239000940831     C                   PARM                    KPJBA
239100000608      *
239200000608      * -----------------
239300000608      * IMPOSTA L'INDICATORE (66) PER CONTROLLARE L'AZIONE
239400000608      *  ORIGINALE O QUELLA NUOVA PER GESTIRE DIRETTAMENTE
239500000608      *   LA BANCA PER CONTANTE PER IL NUOVO GIRO POSTE.
239600000608     C     KCDAZ         COMP      '%0W9'                             6666
239700000608      * -----------------
239800000608      *
239900991104     C* Eseguo la Open dei file
240000021114     C                   OPEN      CNASS02L                             10
240100991104     C   10              GOTO      FINE
240200021121     C* Richiamo XPARUT
240300021121     C                   Z-ADD     1             CODUT
240400021121     C                   CALL      'X§PARUT'
240500021121     C                   PARM                    UTEDSE
240600021121     C                   MOVEL     RAGUT         RSUT2            25
240700021121     C                   MOVEL     REC40         CNCR40
240800021118     c     *dtaara       define    §azute        azuteds
240900021118     c     *dtaara       define    §datiute      ddatiute
241000021118     C                   in(E)     *dtaara
241100021118     C                   IF        %Error  or  RSUT = *blanks
241200021118     C                   call      'TIBS34R'
241300021118     C                   parm                    Tibs34Ds
241400021118     C                   in        *dtaara
241500021118     c                   ENDIF
241600050111     C*§ CONTROLLO SE SIAMO IN FILIALE
241700050111     c                   move      dutpou        §fle              3 0
241800060426     c                   if        dutlpo = 'S' or dutlpo = ' '
241900060426     c                   seton                                        19
242000060426     c                   end
242100060526     c                   if        dutlpo = '2'
242200060526     c                   seton                                        01
242300060526     c                   end
242400040921     C     dutpou        CHAIN     AZORG01L
242500941103     C*§§
242600941103     C                   TIME                    WHHUDT           14 0
242700991102     C                   MOVE      WHHUDT        G02DAT
242800991102     C                   MOVE      *ZEROS        G02INV
242900991102     C                   MOVEL     *BLANK        G02ERR
243000941103     C                   CALL      'XSRDA8'
243100991103     C                   PARM                    WLBDAT
243200991102     C                   Z-ADD     G02INV        AMGUD8            8 0
243300991102     C                   Z-ADD     G02DAT        GMAUD8            8 0
243400991103     C                   Z-ADD     0             REGSWD            5 0          *AZZER.RIG
243500150109     c                   move      amgud8        dataiso
243600940831     C*-----
243700940831     C                   SETON                                        05        COND.INTES
243800940831     C*-                                                   *
243900940831     C*--- RICERCA CAPOCONTI------------------------------------------*
244000100521     C                   DO        50            X
244100940831     C                   MOVE      TCU(X)        TCUDS
244200940831     C     F56           CABNE     'CG'          END1
244300940831     C     F4            IFEQ      '6'
244400940831     C                   Z-ADD     KCU(X)        KBNA              4 0
244500940831     C                   END
244600940831     C     F4            IFEQ      '4'
244700940831     C                   Z-ADD     KCU(X)        KCAS              4 0
244800940831     C                   END
244900940831     C     END1          TAG
245000940831     C                   END
245100940831     C*---------------------------------------------------------------*
245200940831     C*--- RICERCA RAG.SOCIALE CLIENTE     'CNACO00F       *
245300940831     C     KEYACO        KLIST                                                  *
245400940831     C                   KFLD                    CODUT                          *
245500940831     C                   KFLD                    KCC                            *
245600940831     C                   KFLD                    KSC                            *
245700940831     C                   Z-ADD     KBNA          KCC                            *
245800940831     C     KBAN          KLIST                                                  *
245900940831     C                   KFLD                    CODUT                          *
246000940831     C                   KFLD                    KBNA                           *
246100940831     C                   KFLD                    KSC
246200940831     C     KTAB          KLIST
246300940831     C                   KFLD                    CODUT
246400940831     C                   KFLD                    COD
246500940831     C                   KFLD                    KEY
246600940831     C     KEYOPE        KLIST                                                  * CAUSALE
246700940831     C                   KFLD                    CODUT                          *
246800940831     C                   KFLD                    OPECD1                         *
246900940831     C                   KFLD                    OPECOD                         *
247000991102     C     KTAB1         KLIST
247100940831     C                   KFLD                    CODUT
247200940831     C                   KFLD                    COD
247300991102     C     KTBE          KLIST
247400991102     C                   KFLD                    KCOD1
247500991102     C                   KFLD                    KKEY1
247600020729     C     KTBE1         KLIST
247700020729     C                   KFLD                    tbecod
247800020729     C                   KFLD                    tbeke1
247900020729     C                   KFLD                    tbeke2
248000940831     C     KABI§         KLIST
248100940831     C                   KFLD                    KABI              5 0
248200940831     C                   KFLD                    KCAB              5 0
248300940831     C     KASS          KLIST
248400940831     C                   KFLD                    §FLE
248500940831     C                   KFLD                    KNRA             10
248600940831     C                   KFLD                    KABI
248700940831     C                   KFLD                    KCAB
248800021120      *
248900021120     C     Kqig          KLIST
249000021120     C                   KFLD                    qigann
249100021120     C                   KFLD                    §fle
249200021120     C                   KFLD                    amgud8
249300021120     c                   clear                   qigann
249400940831     C*
249500050405     C***** Tolta gestione dei PLICHI
249600020103      * con l'euro è necessario testare solo senza la divisa
249700020103      * per dare l'errore di Plico già utilizzato
249800050405     C**** KPLIN         KLIST
249900050405     C*******            KFLD                    KPOC
250000050405     C*******            KFLD                    KPLI
250100000612     C*
250200940831     C     *LIKE         DEFINE    ACOKCC        KCC
250300940831     C     *LIKE         DEFINE    ACOKSC        KSC
250400940831     C     *LIKE         DEFINE    TBLCOD        COD
250500940831     C     *LIKE         DEFINE    TBLKEY        KEY
250600991102     C     *LIKE         DEFINE    TBECOD        KCOD1
250700991102     C     *LIKE         DEFINE    TBEKE1        KKEY1
250800940831     C     *LIKE         DEFINE    TOTCON        §TCONT
250900940831     C     *LIKE         DEFINE    INDABI        ISTABI
251000940831     C     *LIKE         DEFINE    INDCAB        ISTCAB
251100991120     C* REPERISCO DATA DEL GIORNO
251200991120     C                   TIME                    WHHUDT           14 0
251300991120     C                   MOVEL     WHHUDT        ORA               6 0
251400991120     C                   MOVE      WHHUDT        G02DAT
251500991120     C                   MOVE      *ZEROS        G02INV
251600991120     C                   MOVEL     *BLANK        G02ERR
251700991120     C                   CALL      'XSRDA8'
251800991120     C                   PARM                    WLBDAT
251900991120     C                   Z-ADD     G02INV        OGGI              8 0
252000991122     C                   Z-ADD     G02DAT        WTODAY            8 0
252100980130     C* Richiamo pgm per controllo modificabilità tabella
252200980130     C                   CLEAR                   TRUL12
252300980130     C                   MOVEL     'L'           I12TLA
252400980130     C                   MOVEL     KNSIF         I12NSI
252500980130     C                   MOVEL     '4D'          I12COD
252600980130     C                   CALL      'TRUL12R'
252700980130     C                   PARM                    TRUL12
252800980130     C     O12ERR        IFEQ      '1'
252900980130     C                   GOTO      FINE
253000980130     C                   END
253100980130     C*----------------------------------------------------*
253200940831     C                   MOVE      *ZEROS        AMGDRG
253300940831     C                   EXSR      CARTAB
253400940831     C                   EXSR      CARTB
253500991102     C   80              MOVEL     '1'           *INKC
253600010626     C                   MOVEL     §GEDCR        V1CDIV
253700991102     C*
253800991103     C* 2 GG PRIMA LA DATA DEL GIORNO CASSA SEGNALO
253900991104     C                   Z-ADD     0             NRMOV             7 0
254000000612     C                   Z-ADD     0             NRPLI             7 0
254100991103     C                   Z-ADD     0             D52DFI                         *
254200991103     C                   Z-ADD     1             D52GIO                         *
254300991103     C                   MOVE      AMGUD8        D52DIN                         *
254400991103     C                   MOVE      '-'           D52SGN                         *
254500991103     C                   CALL      'TRUL52R'
254600991103     C                   PARM                    TRUL52
254700991103     C                   MOVE      D52DFI        DAT1GG            8 0          *
254800940831     C                   ENDSR
254900020418
255000020418     ***********************************************************************
255100000000     ODISBAN1   E            TESTA            02
255200891205     O                       RSUT2
255300000000     O                                           60 '**---------------------'
255400000000     O                                              '------------**'
255500020418
255600000000     O          E            TESTA            03
255700891205     O                       SIMVIA
255800060421     O                                           60 '**  DISTINTA  VERSAMENT'
255900060421     O                                              'O IN BANCA  **'
256000060421     O*              66                          60 '**      DISTINTA  VERSA'
256100060421     O*              66                             'MENTO       **'
256200000000     O                       PAGE          Z    132
256300000000     O                                          128 'PAG.'
256400991103     O                                          120 'CNC0W9R'
256500020418
256600000000     O          E            TESTA            04
256700891205     O                       SIMCIT
256800000000     O                                           60 '**---------------------'
256900000000     O                                              '------------**'
257000020418
257100000000     O          E            TESTA            05
257200000000     O                                              'Versamento a mezzo Banca'
257300000000     O                       KCC           Z   +  1
257400910726     O                       VBAN          Z   +  1
257500000000     O                       VIDBAN            +  1
257600891205     O                                         +  1 'C/C'
257700891205     O                       INDCCB            +  1
257800991103     O                                         +  1 'Divisa Versamento:'
257900991103     O                       V1CDIV            +  1
258000020418
258100020418     O          E            TESTA          1 06
258200000000     O                                              'Distinta N.'
258300000000     O                       VIDNDS        Z   +  1
258400000000     O                                         +  1 'del'
258500991103     O                       VIDDDS            +  1 '  /  /    '
258600020418     O                                           98 'Plico n.'
258700020418     O                       V1PLIC        Z   +  1
258800020418
258900000616     O          E   N66      TESTA            08
259000000616     O                       SC(1)               66
259100000616     O                       SC(2)              132
259200020418
259300000616     O          E   N66      TESTA            09
259400000616     O                       SC(3)               66
259500000616     O                       SC(4)              132
259600020418
259700000616     O          E   N66      TESTA            10
259800000616     O                       SC(5)               66
259900000616     O                       SC(6)              132
260000020418
260100000616     O          E   N66      TESTA            11 12
260200000616     O                       SC(7)               66
260300000616     O                       SC(8)              132
260400020418     **---------------------------------------------------------------------
260500000616     O          E   N66      DET         1
260600910801     O                       VTPA                 3
260700910719     O                       VTPP                 4
260800910719     O                       VNRA                13
260900910719     O                       VNRA1               18
261000991105     O                       GMADAS              29 '  /  /    '
261100991104     O                       VDES                50
261200991104     O                       VPIA                62
261300991104     O               22      VASS          2     77
261400991104     O               22 02                       77 '   '
261500991104     O              N22      VASS          2     92
261600991104     O              N22 02                       92 '   '
261700991104     O               53 54                      102 'STESSO'
261800991104     O              N53 54                      102 'ALTRO '
261900020418     **---------------------------------------------------------------------
262000000616     O          E   N66      TOT         2
262100000000     O                       TOTPIA        2     77
262200991104     O                  03                       77 '   '
262300000000     O                       TOTFUO        2     92
262400991104     O                  04                       92 '   '
262500000000     O                                         +  1 '- Totale Assegni -'
262600000000     O                       TOTASS        2   +  1
262700991104     O                  06                      132 '   '
262800020418
262900000616     O          E   N66      TOT         0
263000000000     O                       TOTPIA        2     77
263100991104     O                  03                       77 '   '
263200000000     O                       TOTFUO        2     92
263300991104     O                  04                       92 '   '
263400000000     O                                         +  1 '- Totale Assegni -'
263500991104     O                       TOTASS        2    132
263600991104     O                  06                      132 '   '
263700020418
263800000616     O          E   N66      TOT         0  2
263900000000     O                       TOTPIA        2     77
264000991104     O                  03                       77 '   '
264100000000     O                       TOTFUO        2     92
264200991104     O                  04                       92 '   '
264300000000     O                                         +  1 '- Totale Assegni -'
264400991104     O                       TOTASS        2    132
264500991104     O                  06                      132 '   '
264600020418     **---------------------------------------------------------------------
264700000616     O          E   N66      TOT1        1
264800000000     O                       CSP(A)               3
264900000000     O                                         +  1 '->'
265000000000     O                       DSP(A)            +  1
265100991105     O               53                          48 'STESSO ISTITUTO'
265200991105     O              N53                          48 'ALTRO  ISTITUTO'
265300991104     O               53      IMP(A)        2     68
265400991104     O              N53      IM1(A)        2     68
265500991104     O                  07                       68 '   '
265600991104     O                       V1CDIV              72
265700020418     **---------------------------------------------------------------------
265800900518     O          E            DETSFL         1
265900900518     O                                         +  1 'Nr.'
266000060421     O                       stanrt        Z   +  1
266100900518     O                       VDESCR            +  1
266200991104     O                       VIDIM         2     68
266300991104     O                  08                       68 '   '
266400020418     **---------------------------------------------------------------------
266500900518     O          E            TOT2        1  2
266600000000     O                                         +  1 'Totale Contante'
266700991104     O                       TOTCON        2     68
266800991104     O                  09                       68 '   '
266900020418     **---------------------------------------------------------------------
267000000000     O          E            TOT3        1
267100000000     O                                              '------------------------'
267200000000     O                                              '------------------------'
267300930927     O                                              '------------------------'
267400020418
267500000000     O          E            TOT3        1
267600000000     O                                         +  1 'Totale Distinta'
267700991104     O                       TOTDIS        2     68
267800991104     O                  02                       68 '   '
267900991104     O                       V1CDIV              72
268000991104     O                  03                       68 '   '
268100020418
268200000000     O          E            TOT3        2
268300000000     O                                         +  1 'Importo da Versare'
268400991104     O                       TOTVER        2     68
268500991104     O                  04                       68 '   '
268600991104     O                       V1CDIV              72
268700020418
268800000000     O          E            TOT3        0
268900000000     O                                         +  1 'Importo da Versare'
269000991104     O                       TOTVER        2     68
269100991104     O                  04                       68 '   '
269200991104     O                       V1CDIV              72
269300020418
269400000000     O          E            TOT3        0
269500000000     O                                         +  1 'Importo da Versare'
269600991104     O                       TOTVER        2     68
269700991104     O                  04                       68 '   '
269800991104     O                       V1CDIV              72
269900020418
270000000000     O          E            TOT3             65
270100000000     O                                          130 'COPIA USO BANCA'
270200020418     ***********************************************************************
270300000000     ODISBAN3   E            TESTA            02
270400891205     O                       RSUT2
270500000000     O                                           60 '**---------------------'
270600000000     O                                              '------------**'
270700020418
270800000000     O          E            TESTA            03
270900891205     O                       SIMVIA
271000060421     O                                           60 '**  DISTINTA  VERSAMENT'
271100060421     O                                              'O IN BANCA  **'
271200060421     O*              66                          60 '**      DISTINTA  VERSA'
271300060421     O*              66                             'MENTO       **'
271400000000     O                       PAGE3         Z    132
271500000000     O                                          128 'PAG.'
271600991103     O                                          117 'CNC0W9R'
271700020418
271800000000     O          E            TESTA            04
271900891205     O                       SIMCIT
272000000000     O                                           60 '**---------------------'
272100000000     O                                              '------------**'
272200020418
272300000000     O          E            TESTA            05
272400000000     O                                              'Versamento a mezzo Banca'
272500000000     O                       KCC           Z   +  1
272600910726     O                       VBAN          Z   +  1
272700000000     O                       VIDBAN            +  1
272800891205     O                                         +  1 'C/C'
272900891205     O                       INDCCB            +  1
273000991104     O                                         +  1 'Divisa Versamento:'
273100991104     O                       V1CDIV            +  1
273200020418
273300020418     O          E            TESTA          1 06
273400000000     O                                              'Distinta N.'
273500000000     O                       VIDNDS        Z   +  1
273600000000     O                                         +  1 'del'
273700991103     O                       VIDDDS            +  1 '  /  /    '
273800020418     O                                           98 'Plico n.'
273900020418     O                       V1PLIC        Z   +  1
274000020418
274100000616     O          E   N66      TESTA            08
274200000000     O                       SC(1)               66
274300000000     O                       SC(2)              132
274400020418
274500000616     O          E   N66      TESTA            09
274600000000     O                       SC(3)               66
274700000000     O                       SC(4)              132
274800020418
274900000616     O          E   N66      TESTA            10
275000000000     O                       SC(5)               66
275100000000     O                       SC(6)              132
275200020418
275300000616     O          E   N66      TESTA            11 12
275400000000     O                       SC(7)               66
275500000000     O                       SC(8)              132
275600020321
275700020418     **---------------------------------------------------------------------
275800000616     O          E   N66      DET         1
275900910801     O                       VTPA                 3
276000910719     O                       VTPP                 4
276100910719     O                       VNRA                13
276200910719     O                       VNRA1               18
276300991105     O                       GMADAS              29 '  /  /    '
276400991104     O                       VDES                50
276500991104     O                       VPIA                62
276600991104     O               22      VASS          2     77
276700991104     O               22 02                       77 '   '
276800991104     O              N22      VASS          2     92
276900991104     O              N22 02                       92 '   '
277000991104     O               53 54                      102 'STESSO'
277100991104     O              N53 54                      102 'ALTRO '
277200930927     O                       VNOT               132
277300020418     **---------------------------------------------------------------------
277400000616     O          E   N66      TOT         2
277500000000     O                       TOTPIA        2     77
277600991104     O                  03                       77 '   '
277700000000     O                       TOTFUO        2     92
277800991104     O                  04                       92 '   '
277900911127     O                                         +  1 '- Totale Assegni -'
278000991104     O                       TOTASS        2    132
278100991104     O                  06                      132 '   '
278200020418
278300000616     O          E   N66      TOT         0
278400000000     O                       TOTPIA        2     77
278500991104     O                  03                       77 '   '
278600000000     O                       TOTFUO        2     92
278700991104     O                  04                       92 '   '
278800000000     O                                         +  1 '- Totale Assegni -'
278900991104     O                       TOTASS        2    132
279000991104     O                  06                      132 '   '
279100020418
279200000616     O          E   N66      TOT         0  2
279300000000     O                       TOTPIA        2     77
279400991104     O                  03                       77 '   '
279500000000     O                       TOTFUO        2     92
279600991104     O                  04                       92 '   '
279700000000     O                                         +  1 '- Totale Assegni -'
279800991104     O                       TOTASS        2    132
279900991104     O                  06                      132 '   '
280000020418     **---------------------------------------------------------------------
280100000616     O          E   N66      TOT1        1
280200000000     O                       CSP(A)               3
280300000000     O                                         +  1 '->'
280400000000     O                       DSP(A)            +  1
280500991105     O               53                          48 'STESSO ISTITUTO'
280600991105     O              N53                          48 'ALTRO  ISTITUTO'
280700991104     O               53      IMP(A)        2     68
280800991104     O              N53      IM1(A)        2     68
280900991104     O                  07                       68 '   '
281000991104     O                       V1CDIV              72
281100020418     **---------------------------------------------------------------------
281200900518     O          E            DETSFL         1
281300900518     O                                         +  1 'Nr.'
281400060421     O                       stanrt        Z   +  1
281500900518     O                       VDESCR            +  1
281600991104     O                       VIDIM         2     68
281700991104     O                  08                       68 '   '
281800020418     **---------------------------------------------------------------------
281900900518     O          E            TOT2        1  2
282000000000     O                                         +  1 'Totale Contante'
282100991104     O                       TOTCON        2     68
282200991104     O                  09                       68 '   '
282300991104     O                       V1CDIV              72
282400020418     **---------------------------------------------------------------------
282500000000     O          E            TOT3        1
282600000000     O                                              '------------------------'
282700000000     O                                              '------------------------'
282800930927     O                                              '------------------------'
282900020418
283000000000     O          E            TOT3        1
283100000000     O                                         +  1 'Totale Distinta'
283200991104     O                       TOTDIS        2     68
283300991104     O                  02                       68 '   '
283400991105     O                       V1CDIV              72
283500020418
283600000000     O          E            TOT3        2
283700000000     O                                         +  1 'Importo da Versare'
283800991104     O                       TOTVER        2     68
283900991104     O                  04                       68 '   '
284000991104     O                       V1CDIV              72
284100020418
284200000000     O          E            TOT3        0
284300000000     O                                         +  1 'Importo da Versare'
284400991104     O                       TOTVER        2     68
284500991104     O                  04                       68 '   '
284600991104     O                       V1CDIV              72
284700020418
284800000000     O          E            TOT3        0
284900000000     O                                         +  1 'Importo da Versare'
285000991104     O                       TOTVER        2     68
285100991104     O                  04                       68 '   '
285200991104     O                       V1CDIV              72
285300020418
285400000000     O          E            TOT3        3
285500000000     O                                         +  1 'N.Registrazione'
285600000000     O                       VIDNRG        Z   +  1
285700000000     O                                         +  1 'del'
285800991103     O                       VIDDRG            +  1 '  /  /    '
285900930225     O                                         +  2 'Operaz. del'
286000991118     O                       VDTOPE            +  1 '  /  /    '
286100020418
286200000000     O          E            TOT3             65
286300000000     O                                          130 'COPIA USO INTERNO'
286400020418     ***********************************************************************
286500991103     OTABEL     E            RILTAB
286600020418     ***********************************************************************
286700991104**
286800991104------+-----------+----------+--------------------+-----------+---
286900930927-----------+--------------+----------+---------------------------+
287000000000SPECIE    NUMERO     DATA    BANCA TRASSATA        PIAZZA       IM
287100930927PORTO ASS.   IMPORTO ASS.   ISTITUTO   N O T E                   '
287200000000          ASSEGNO  ASSEGNO                         PAGAMENTO    SU
287300930927 PIAZZA      FUORI PIAZZA                                        '
287400991105------+-----------+----------+--------------------+-----------+---
287500930927-----------+--------------+----------+---------------------------+
287600951027** SCHIERA ERRORI
287700991102Già inserito assegno. Eseguire chius.operatore/presa in carico mandato d'introto    01
287800991102Già inserito assegno in questa distinta.                                            02
287900991102Già inserito assegno con questi riferimenti intestato al mittente.                  03
288000991102ERRORE !! Manca il numeratore della distinta !!!!                                   04
288100991102Divisa di versamento non valida                                                     05
288200991103Precedente trasmissione di  movimenti contabili terminata in modo anomalo           06
288300000609Impostare un nr.PLICO corretto                                                      07
288400000612Con questo n°Plico sono stati già inseriti altri dati! Usare un altro Numero.       08
288500060403ERRORE! Non è possibile continuare. Esiste già quadratura giornaliera.              09
288600150313Codice Banca obbligatorio                                                           10
288700150319Codice Banca errato                                                                 11
288800991104**
288900020926CHKOBJ OBJ(FNMOV00T) OBJTYPE(*FILE) MBR(M000000)
289000020926ADDPFM FILE(FNMOV00T)               MBR(M000000)
289100020926OVRDBF FILE(FNMOV00T)               MBR(M000000)
289200020926ALCOBJ OBJ((FNMOV00T *FILE *EXCL        M000000))
289300020926DLCOBJ OBJ((FNMOV00T *FILE *EXCL        M000000))
