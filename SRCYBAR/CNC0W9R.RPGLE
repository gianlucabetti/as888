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
009600020507     D TR06DS        E DS                  EXTNAME(TRUL06DS)
009700020507     D  LIN                    1     90  0 dim(30)
009800000000     D KPJBA         E DS
009900000000     D  LIBSYS                92    100
010000000000     D  MSGERR               453    502
010100900403     D UTEDSE        E DS                  EXTNAME(UT§DSE0F)
010200000000     D  TCU                  398    697
010300000000     D                                     DIM(50)                              TAB FLAG C
010400000000     D  KCU                  698    847P 0
010500000000     D                                     DIM(50)                              TAB CAPO C
010600000000     D                                     PACKEVEN
010700020926     D TIBS55        E DS                  EXTNAME(TIBS55DS)
010800020729     D Dy4O          E DS
010900991118     D DS1A          E DS
011000910719     D DS4E          E DS
011100991102     D DSCV          E DS
011200011016     D DSCW          E DS
011300991102     D DGED          E DS
011400000621     D SPICCI          C                   CONST('Spiccioli')
011500170517     d  brt_via        C                   CONST('VIA MATTEI                  4-
011600170517     D                                     2')
011700170517     d  brt_cit        C                   CONST('BOLOGNA                      -
011800170517     D                                      ')
011900170517
012000170517     d  brt_PRV        C                   CONST('BO')
012100011016     D*
012200150313     D wban            S                   like(vban)
012300060403     D §abb            S                   like(vass)
012400150109     D dataeur         S               d   datfmt(*eur)
012500150108     D dataiso         S               d   datfmt(*iso)
012600150108     D dataisom        S               d   datfmt(*iso)
012700060411     D PrmTpInc        S              2
012800011016     D PrmDivisa       S              3
012900011016     D PrmData         S               D
013000011016     D PrmEsito        S              1
013100011016     D PrmDS1A         S
013200011016     D                                     LIKE(DS1A)
013300011016     D PrmDSCW         S
013400011016     D                                     LIKE(DSCW)
013500020417     D WrkLstRrnS4     S              5I 0
013600020422     D WrkDtOpe        S               D
013700020422     D                                     DATFMT(*EUR)
013800020422     D WrkDtCur        S               D
013900020422     D WrkMonth        S              5I 0
014000060414     D chkDATAASSEGNO  PR            10I 0
014100060414     D  dte                          10D   DATFMT(*ISO)
014200060414     D  SAS
014300060414     D                                     LIKE(§1ASAS)
014400060414     D  esito                        10I 0
014500060414     D                                     OPTIONS(*OMIT)
014600060414     D  errc                          1A
014700060414     D                                     DIM(10)
014800060414     D  msgc                         80A   OPTIONS(*OMIT)
014900060414     D                                     DIM(10)
015000060414     D dte             S               d
015100060414     D sas             S                   like(§1asas)
015200060414     D esito           S             10I 0
015300060414     D errc            S              1    DIM(10)                              SKI CHIAVI
015400060414     D msgc            S             80    DIM(10)                              SKI CHIAVI
015500130409     D stanrt          S                   like(vnrman)
015600060824     D okcon           S              1    inz
015700150407     D wfle            S                   like(§fle)
015800100521     d* ordinamento
015900100521     d NotUsed         S             16A
016000100521     d ReturnSize      S              9B 0
016100100521     d SizeList        S              9B 0
016200100521     d WrkSort         S              1
016300150108     d wrkEml          s            253
016400150108     d wrkEmcc         s            253
016500150108     d wrkMsg          s           5000
016600150108     d wrkOgg          s             44
016700150108     d wrkesit         s              1
016800150108     d
016900100521     d*---
017000100521     d nr2             S              3  0                                      *n° rel sf
017100100521     d stmrec1         S              3  0                                      *n° rel sf
017200100521     d*---
017300100521     d MaxKey          C                   2
017400100521     d Ascendente      C                   1
017500100521     d Discendente     C                   2
017600100521     d Carattere       C                   6
017700100521     d Put             C                   1
017800100521     d EndPut          C                   2
017900100521     d Get             C                   3
018000100521     d Numerico        C                   8
018100100521     d*---
018200100521     d SflRcd          DS
018300100521     d  vpad
018400100521     d  SAVABI
018500100521     d  SAVCAB
018600100521     d  SAVNRA
018700100521     d  SAVNR1
018800100521     d  VANN
018900100521     d  VNRA
019000100521     d  VNRA1
019100100521     d  VDTE
019200100521     d  VASS
019300100521     d  VASSH
019400100521     d  VTPA
019500100521     d  VTPP
019600100521     d  VAbi
019700100521     d  Vcab
019800100521     d  Vdes
019900100521     d  Vtpi
020000100521     d  Vaas
020100100521     d  Vnrs
020200100521     d  Vnsp
020300100521     d  Vpia
020400100521     d  Vnot
020500100521     d  Selected                      1A
020600100521
020700100521      /COPY QSYSINC/QRPGLESRC,QLGSORT
020800100521     d QLGKL                         16    DIM(MaxKey)
020900100521     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
021000100521     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
021100100521     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
021200100521     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
021300100521
021400100521      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
021500100521      /COPY QSYSINC/QRPGLESRC,QUSEC
021600020417
021700991102     IYNTGD000
021800991102     I              TGDDIVISA                   TGDDIV
021900991102     I              TGDSEQUENZ                  TGDSEQ
022000991102     I              TGDDESCR                    TGDDES
022100991102     I              TGDVALORE                   TGDVAL
022200991102?    I*----------------------------------------------------*
022300991102     C* LOOP FINO A F3
022400991102?    C*----------------------------------------------------*
022500910722     C     *INKC         DOWEQ     '0'
022600000608      *
022700000608     C* GESTIONE PRIMA VIDEATA
022800000608     C     VID01         TAG
022900000608      *
023000991102     C                   EXSR      GESD01
023100000608      *
023200000608      * SOLO PER IL GIRO "BANCA PER CONTANTE"
023300050405     C**   adesso va fatto sempre anche se non contante quindi
023400050405     C**     via il gioco fatto con l'ind.66 x solo contante
023500050405     C**   66
023600050405     C**ANN03
023700050405     C**
023800050405     C  N03
023900000612     CANN28V1PLIC        IFNE      *ZERO
024000000608     C                   EXSR      WIND01
024100000608     C   KL              GOTO      VID01
024200000608     C                   ENDIF
024300000608      *
024400991102     C* PER NO ERRORI PROSEGUO
024500991103     C  N28
024600991103     CANNKC
024700991103     CANN91
024800991104     CANN92
024900991104     CANN39              DO
025000991104     C* Effettuo Ovrdbf su file movimenti
025100991104     C     §FLE          IFNE      WFIL
025200060426     c                   setoff                                       68
025300060414     C                   EXSR      DLCMOV
025400060414     C                   EXSR      OVRMOV
025500991118     C                   MOVE      §FLE          WFIL              3 0
025600991104     C                   END
025700910718     C* PULIZIA SUBFILE
025800910718     C                   EXSR      PULIZ
025900000717     C  N66              EXSR      CARICA
026000060411     C                   Z-ADD     NR2           SAVNR2            4 0
026100100521     c                   z-add     1             rcdnbr
026200100521     c                   eval      vordi = 'Importo'
026300060403     c* non è + possibile aggiungere assegni SOLO SEDE
026400060414     c                   if        dutlpo = 'S' or dutlpo = ' '
026500060403     C                   EXSR      AGG
026600060403     c                   end
026700060411     C                   z-add     NR2           TREC              4 0
026800060421     c     trec          comp      0                                      66
026900060421     C     *IN12         DOWEQ     '0'
027000910725     C     *IN11         ANDEQ     '0'
027100910717     C     *IN90         OREQ      '1'
027200000608      *
027300000608      * CON IL (66) VISUALIZZO O BYPASSO QUESTO SCHERMO SE EFFETTUA
027400000608      *  IL BYPASS, DEVO ANCHE SIMULARE DI AVER PREMUTO IL TASTO DI
027500000608      *   CONFERMA PER LA VIDEATA SUCCESSIVA DEL CONTANTE.
027600000608     C  N66              WRITE     CN0W9Z2
027700000608     C  N66              EXFMT     CN0W9C2
027800000608     C   66              SETON                                        11
027900910725     C                   SETOFF                                       90
028000100521     C* F11 = Cambia ordinamento
028100100521if  1C                   IF        *INKK
028200100521     C                   EXSR      F11GES
028300100521e   1C                   ENDIF
028400000608      *
028500000608      * SE F14=ON ESCLUSIONE DI TUTTE LE RIGHE CON "E" SU "VANN"
028600000608     C   66
028700000608     COR KN              EXSR      EXCLRD
028800000608      *
028900060421     C  N12              DO
029000910731     C                   SETOFF                                       82
029100910729     C                   EXSR      CONTR
029200910725     C   11              EXSR      CONTR
029300910726     C   11              SETOFF                                       1317
029400910725     C   11
029500910725     CANN90              EXSR      CONF
029600911125     C   69              GOTO      FINE
0297000604213    C                   ENDdo
029800060411     c* se premuto f12 e sono un utente di sede chiedo conferma per
029900060411     c* eventuali assegni inseriti
030000060426     c                   if        *in12 and *in19 and not *in15
030100060411     C     *IN82         COMP      '0'                                121282
030200060411     c                   end
030300060426     c                   if        *in82 and *in19
030400060411     C                   SETOFF                                       12
030500060411     c                   end
030600910801     C   15              SETON                                        12
030700000608      *
030800000608      * PER IL GIRO "BANCA PER CONTANTE" AD F12 DEVE TORNARE AL
030900000608      * PRIMO VIDEO INIZIALE
031000000608     C   17
031100000608     CAN 66              SETON                                        12
031200131125      *
031300131125      *------
031400131125      * Se si tratta di utente di SEDE che aggiorna il SFL ASSEGNI
031500131125      *  voglio che esca dopo aver dato F6.
031600131125     c                   if        dutlpo = 'S' or dutlpo = ' '
031700131125     c                   if        *in06
031800131125     C                   SETON                                        12
031900131125     c                   end
032000131125     c                   end
032100131125      *------
032200000608      *
032300910731     C                   END
032400060403     C                   END
032500910722     C                   END
032600911125     C     FINE          TAG
032700991104     C* Se ho effettuato l'allocazione disalloco FNMOV
032800991104     C     §FLE          IFEQ      WFIL
032900991104     C                   EXSR      DLCMOV
033000991104     C                   END
033100991104     C                   COMMIT
033200921204     C*-                                                   *
033300910719     C                   SETON                                        LR
033400100521      *-----------------------------------------------------------------------------------
033500100521      * F11GES - TASTO FUNZIONE F11 --> Ragione sociale/P.O
033600100521      *-----------------------------------------------------------------------------------
033700100521     C     F11GES        BEGSR
033800100521     C                   setoff                                       78
033900100521      * IMPOSTO ENTRATA AD 'A'
034000100521     C                   EVAL      rcdnbr  = 1
034100100521      *
034200100521     C                   CLEAR                   QLGSCB
034300100521     C                   CLEAR                   QLGSCB00
034400100521
034500100521     ** 1 campo chiave.
034600100521     C                   EVAL      QLGNBRK    = 1
034700100521
034800100521     C                   IF        *in50=*off
034900100521     c                   eval      vordi = 'Importo'
035000100521     ** IL numero ass. è a posizione 2, alfa, 4 byte, ascending.
035100100521     C                   EVAL      QLGSP      = 22
035200100521     C                   EVAL      QLGSS      = %SIZE(vnra)
035300100521     C                   EVAL      QLGDT      = carattere
035400100521     C                   EVAL      QLGSO      = Ascendente
035500100521     C                   EVAL      QLGKL(1)   = QLGSKL
035600100521     C                   seton                                        50
035700100521     C                   ELSE
035800100521      *
035900100521     c                   eval      vordi = 'Assegno'
036000100521     ** L'importo è a posizione 21, 11 byte, num., ascending.
036100100521     C                   EVAL      QLGSP      = 39
036200100521     C                   EVAL      QLGSS      = %SIZE(vass)
036300100521     C                   EVAL      QLGDT      = numerico
036400100521     C                   EVAL      QLGSO      = Ascendente
036500100521     C                   EVAL      QLGKL(1)   = QLGSKL
036600100521     C                   setoff                                       50
036700100521     C                   ENDIF
036800100521
036900100521      * Load other sort parameters.
037000100521     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
037100100521     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
037200100521     C                   EVAL      QLGRT     = 8
037300100521     C                   EVAL      QLGOKL    = 80
037400100521     C                   EVAL      QLGLKE    = 16
037500100521     C                   EVAL      QLGLSS    = 290
037600100521
037700100521      * Initialize Sort I/O API fields.
037800100521     C                   EVAL      QLGRL00  = QLGRL
037900100521     C                   EVAL      QLGRC00  = 1
038000100521     C                   CLEAR                   QUSEI
038100100521     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
038200100521
038300100521      * First step - Initialize the sort routine.
038400100521     C                   CALL      'QLGSORT'
038500100521     C                   PARM                    QLGSCB
038600100521     C                   PARM                    NotUsed
038700100521     C                   PARM                    NotUsed
038800100521     C                   PARM                    SizeList
038900100521     C                   PARM                    ReturnSize
039000100521     C                   PARM                    QUSEC
039100100521
039200100521      * Next step - Write records to I/O routine.
039300100521     C                   EVAL      QLGRT00 = Put
039400100521
039500100521     C                   DO        STMREC1       x                 3 0
039600100521
039700100521     C     x             CHAIN     CN0W9S2
039800100521
039900100521     ** Solo le righe con Selected = 'Y' sono riordinate,
040000100521     ** quindi per fare un ordinamento di tutte le righe
040100100521     ** metto 'Y' sempre.
040200100521     C                   EVAL      Selected = 'Y'
040300100521
040400100521     C                   CLEAR                   QUSEI
040500100521     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
040600100521     C                   CALL      'QLGSRTIO'
040700100521     C                   PARM                    QLGSCB00
040800100521     C                   PARM                    SflRcd
040900100521     C                   PARM                    NotUsed
041000100521     C                   PARM                    SizeList
041100100521     C                   PARM                    NotUsed
041200100521     C                   PARM                    QUSEC
041300100521
041400100521     C                   ENDDO
041500100521
041600100521      * Next step - Signal end of input, clear subfile for reload.
041700100521     C                   EVAL      QLGRT00 = EndPut
041800100521     C                   CLEAR                   QUSEI
041900100521     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
042000100521
042100100521     C                   CALL      'QLGSRTIO'
042200100521     C                   PARM                    QLGSCB00
042300100521     C                   PARM                    SflRcd
042400100521     C                   PARM                    NotUsed
042500100521     C                   PARM                    SizeList
042600100521     C                   PARM                    NotUsed
042700100521     C                   PARM                    QUSEC
042800100521
042900100521     C                   SETON                                            70    *
043000100521     C                   WRITE     CN0W9C2                                      *
043100100521     C                   SETOFF                                       70        *
043200100521     C                   WRITE     CN0W9Z2                                      *
043300100521
043400100521      * Final step - Write the records back to the subfile.
043500100521     C                   EVAL      QLGRT00 = Get
043600100521
043700100521     C                   DO        STMREC1       x
043800100521     C                   CLEAR                   QUSEI
043900100521     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
044000100521     C                   CALL      'QLGSRTIO'
044100100521     C                   PARM                    QLGSCB00
044200100521     C                   PARM                    NotUsed
044300100521     C                   PARM                    SflRcd
044400100521     C                   PARM                    QLGRL00
044500100521     C                   PARM                    NotUsed
044600100521     C                   PARM                    QUSEC
044700100521      *
044800100521     c                   z-add     x             nr2
044900100521     C                   WRITE     CN0W9S2
045000100521     C                   ENDDO
045100100521
045200100521     C                   EVAL      *IN70 = *off
045300100521      *
045400100521     C                   ENDSR
045500991102     C*----------------------------------------------------*
045600991102     C* GESTIONE DELLA PRIMA VIDEATA
045700991102     C*----------------------------------------------------*
045800991102     C     GESD01        BEGSR
045900150410      *
046000150410     C                   z-add     §FLE          wfle
046100000609      *
046200000609      * SOLO PER IL GIRO BANCA PER CONTANTE (66=ON)
046300000609     C*  Aggancio tabella 'SEC' x reperire la BANCA
046400000609     C     *IN66         IFEQ      *ON
046500000609     C                   MOVEL     'SEC'         KCOD1
046600000609     C                   MOVEL     *BLANKS       KKEY1
046700000609     C                   MOVEL     'SEC'         KKEY1
046800000609     C     KTBE          CHAIN     TNTBE01L                           13
046900000609     C     *IN13         IFEQ      '0'
047000000609     C                   MOVEL     TBEUNI        BANCA8            8 0
047100060421     C*                  MOVE      BANCA8        VBAN
047200000609     C                   END
047300000609     C                   ENDIF
047400000609      *
047500150304     C* LEGGO TABELLA 'Y4O'
047600150312     C                   MOVEL     'Y4O'         tbecod
047700150312     C                   MOVEL(p)  '201'         tbeke1
047800150312     C                   MOVEL(p)  §FLE          tbeke2
047900150305      * sprotegge il campo di immissione codice Banca
048000150312     c                   setON                                        35
048100150312     C     KTbe1         CHAIN     Tntbe01l                           39
048200150312     C   39              SETON                                        28
048300150313     C   39              clear                   vban
048400150306      *
048500150312     c                   if        not *in39
048600150312     C                   EXCEPT    RILTAB
048700150312     C                   MOVEL     TBeUNI        Dy4O
048800150305      *
048900150305      * imposta automaticamente il codice Banca per eseguire la DISTINTA
049000150305      * di Versamento ed il campo viene protetto.
049100150312     c                   if        §4OSCB <> *blanks and
049200150312     c                             §4OSCB <> *zeros
049300150312     c                   setOFF                                       35
049400150312     c                   move      *zeros        VBAN
049500150312     c                   move      §4OSCB        VBAN
049600150313     c                   move      vban          wban
049700150312     c                   end
049800150312     c                   end
049900150304      *
050000150313     c     sopra         tag
050100060421     C                   SETOFF                                       128211
050200991102     C                   SETON                                        73
050300991102     C                   EXFMT     CN0W9D1
050400150313     c   kc              goto      fine
050500991104     C                   SETOFF                                       2891
050600150304      *
050700021120     c* controllo che non sia già stata fatta la quadratura giornaliera
050800021120     c* se così fosse non lascio fare il versamento
050900060412     c     kqig          chain     yfqig01l
051000060412     c                   if        %found(yfqig01l)
051100021120     c                   seton                                        28
051200021120     C                   MOVEL     ERR(9)        $MSG
051300021120     C                   GOTO      FINVD1
051400021120     c                   end
051500000612     C* Controllo se ci sono dati in FNMOV00F
051600991103     C                   Z-ADD     0             NRRX             10 0
051700991103     C                   MOVEL     'FNMOV00T'    FILE
051800991103     C                   MOVEL     '*LIBL   '    LIB              10
051900020926     C                   EVAL      Mbr = 'M' + %EDITC(§Fle:'X') + '046'
052000991103     C                   CALL      'TRUL51C'
052100991103     C                   PARM                    LIB
052200991103     C                   PARM                    FILE             10
052300020926     C                   PARM                    MBR              10
052400991103     C                   PARM                    NRRX
052500991103     C     NRRX          IFNE      *ZEROS
052600991118     C     NRRX          ANDNE     *ALL'9'
052700991103     C                   SETON                                        28
052800991103     C                   MOVEL     ERR(6)        $MSG
052900991103     C                   GOTO      FINVD1
053000991103     C                   END
053100000609      *--------
053200991102     C     §FLE          IFNE      0
053300060421     c                   if        dutlpo <> ' ' and dutlpo <> 'S'
053400150313     C     §Fle          LOOKUP    FILA                                   37
053500150312     C  N37              SETON                                        3938
053600150313     C  n37              GOTO      sopra
053700150312     C   37              SETOFF                                       3938
053800060421     c                   end
053900991102     C                   ELSE
054000150312     C                   SETON                                        39
054100150313     C                   GOTO      sopra
054200991102     C                   END
054300150312     C* LEGGO TABELLA 'Y4O'
054400150407     c                   if        Wfle <> §fle
054500150407     c                   clear                   vban
054600150407     C                   z-add     §FLE          wfle
054700150312     C                   MOVEL     'Y4O'         tbecod
054800150312     C                   MOVEL(p)  '201'         tbeke1
054900150312     C                   MOVEL(p)  §FLE          tbeke2
055000150312      * sprotegge il campo di immissione codice Banca
055100150312     c                   setON                                        35
055200150312     C     KTbe1         CHAIN     Tntbe01l                           39
055300150313     C   39              SETON                                        28
055400150407e1   c                   if        not *in39
055500150312     C                   EXCEPT    RILTAB
055600150312     C                   MOVEL     TBeUNI        Dy4O
055700150312      *
055800150312      * imposta automaticamente il codice Banca per eseguire la DISTINTA
055900150312      * di Versamento ed il campo viene protetto.
056000150407e2   c                   if        §4OSCB <> *blanks and
056100150313     c                             §4OSCB <> *zeros
056200150313     c                   move      *zeros        vBAN
056300150313     c                   move      §4OSCB        vBAN
056400150319     C                   setoff                                       35
056500150407     C                   GOTO      sopra
056600150407f2   c                   end
056700150407f1   c                   end
056800150407f1   c                   endif
056900150313      *
057000150313     C     VBAN          COMP      0                                      92
057100150407     c                   if        *in92
057200150313     c                   seton                                        28
057300150313     C                   MOVEL     ERR(10)       $MSG
057400150313     C                   GOTO      sopra
057500150407     c                   endif
057600150312     C***  Adesso il controllo di immissione nr.plico lo deve fare sempre
057700150312     C     V1PLIC        COMP      0                                      93
057800150407     c                   if        *in93
057900150313     c                   seton                                        28
058000150313     C                   MOVEL     ERR(7)        $MSG
058100150313     C                   GOTO      sopra
058200150407     c                   endif
058300150312     c
058400991102     C  NKC
058500150407     CANN39              DO
058600991102     C                   Z-ADD     VBAN          KSC
058700991102     C     VBAN          COMP      0                                      92
058800991102     C                   MOVEL     *BLANKS       ACORAG
058900991102     C  N92KEYACO        CHAIN     CNACO000                           92
059000991102     C  N92ACOFLG        COMP      '*'                                    92
059100991102     C  N92ACOABL        COMP      '*'                                    92
059200991102     C                   MOVEL     ACORAG        VIDBAN           30
059300991102     C                   MOVEA     ACOOPZ        OPZ                            *
059400991102     C                   MOVE      OPZ(7)        HOPZ7             1            OBBL.RAPP.
059500991102     C                   MOVE      OPZ(2)        HOPZ2             1            GEST.SCADE
059600991102     C     KEYACO        CHAIN     CNIND000                           21
059700991102     C                   Z-ADD     INDABI        ISTABI
059800991102     C                   Z-ADD     INDCAB        ISTCAB
059900991102     C   21              MOVE      *BLANK        INDCCB
060000991102     C                   MOVEL     ACORAG        VBANC
060100150407     c                   if        *in92
060200150319     c                   seton                                        28
060300150319     c                   movel     err(11)       $msg
060400150319     c                   goto      sopra
060500150407     c                   endif
060600991102     C*
060700991102     C* Verifico se la divisa è corretta
060800150407     C  N92V1CDIV        IFNE      DIVALT
060900011016     C     V1CDIV        ANDNE     §GEDCR
061000011016     C     V1CDIV        OREQ      *BLANKS
061100011016     C                   SETON                                        2891
061200011016     C                   MOVEL     ERR(5)        $MSG
061300150407     C                   END
061400011016     C* Controllo fine validità divisa - tipo incasso.
061500011016     C                   CALL      'X1ACW'
061600011016     C                   PARM      'SC'          PrmTpInc
061700011016     C                   PARM      V1CDiv        PrmDivisa
061800011016     C                   PARM                    PrmData
061900011016     C                   PARM      *BLANK        PrmEsito
062000011016     C     DS1A          PARM      *BLANK        PrmDS1A
062100011016     C     DSCW          PARM      *BLANK        PrmDSCW
062200150407     C                   IF        PrmEsito <> '0'
062300011016     C                   EVAL      *IN28 = *ON
062400011016     C                   EVAL      *IN91 = *ON
062500011016     C                   MOVEL     ERR(5)        $MSG
062600150407     C                   ENDIF
062700150407     C                   END
062800150407     C                   IF        PrmEsito = '0'
062900011016     C                   EVAL      *IN94 = *ON
063000150407     C                   ELSE
063100011016     C                   EVAL      *IN94 = *OFF
063200150407     C                   ENDIF
063300000608      *
063400000608      *  SOLO PER IL GIRO "BANCA PER CONTANTE"
063500000608      *   ESEGUO IL CONTROLLO DEL NR.PLICO OBBLIGATORIO
063600050405     C***  Adesso il controllo di immissione nr.plico lo deve fare sempre
063700150312     C*    V1PLIC        COMP      0                                      93
063800150312     C*  93              SETON                                        28
063900150312     C*  93              MOVEL     ERR(7)        $MSG
064000000608      *
064100000612      *
064200991103     C     FINVD1        ENDSR
064300000608     C*----------------------------------------------------*
064400000608     C* IMPOSTA LA WINDOW CONFERMA PER "BANCA X CONTANTE"
064500000608     C*----------------------------------------------------*
064600000608     C     WIND01        BEGSR
064700000608      *
064800000608      * WINDOW DI CONFERMA NR. PLICO
064900000608     C                   Z-ADD     V1PLIC        W1PLIC
065000000608     C                   EXFMT     CN0W9W1
065100000608      *
065200000608     C                   ENDSR
065300000609     C*----------------------------------------------------*
065400000609     C* ESEGUO OVRDBF SU FNMOV
065500000609     C*----------------------------------------------------*
065600000609     C     OVRMOV        BEGSR
065700000609     C*
065800020926     C                   MOVE      '046) '       VAR               8
065900000609     C                   MOVEL     §FLE          VAR
066000020926     C                   Z-ADD     49            LUNG             15 5
066100000609     C** CONTROLLO SE ESISTE IL MEMBRO X FNMOV00T
066200000609     C                   SETOFF                                       20
066300000609     C                   MOVE      VAR           CMDD(1)
066400000609     C                   MOVEL     *BLANKS       COMMAN
066500000609     C                   MOVEA     CMDD(1)       COMMAN           80
066600000609     C                   CALL      'QCMDEXC'                            20
066700000609     C                   PARM                    COMMAN
066800000609     C                   PARM                    LUNG
066900000609     C** ESEGUE ADDPFM DEL MEMBRO NUOVO
067000000609     C   20              DO
067100000609     C                   MOVE      VAR           CMDD(2)
067200000609     C                   MOVEL     *BLANKS       COMMAN
067300000609     C                   MOVEA     CMDD(2)       COMMAN
067400000609     C                   CALL      'QCMDEXC'
067500000609     C                   PARM                    COMMAN
067600000609     C                   PARM                    LUNG
067700000609     C                   END
067800000609     C*+ ESEGUE IN OGNI CASO OVRDBF DEL MEMBRO DESIDERATO
067900000609     C                   MOVE      VAR           CMDD(3)
068000000609     C                   MOVEL     *BLANKS       COMMAN
068100000609     C                   MOVEA     CMDD(3)       COMMAN
068200000609     C                   CALL      'QCMDEXC'
068300000609     C                   PARM                    COMMAN
068400000609     C                   PARM                    LUNG
068500000609     C* GUARDO SE è ALLOCATO
068600000609     C                   MOVEL     VAR           VAR
068700000609     C                   MOVE      ')'           VAR
068800000609     C                   MOVE      VAR           CMDD(4)
068900000609     C                   MOVEA     CMDD(4)       COMMAN
069000020926     C                   Z-ADD     49            LUNCMD           15 5
069100000609     C                   CALL      'QCMDEXC'                            91
069200000609     C                   PARM                    COMMAN
069300000609     C                   PARM                    LUNCMD
069400000609     C*
069500000609     C  N91              OPEN      FNMOV00T                             91
069600030318     C   91              MOVEL(p)  'NO'          KPJBU
069700000609     C   91              GOTO      FINE
069800000609     C*
069900000609     C                   ENDSR
070000991104     C*----------------------------------------------------*
070100991104     C* ESEGUO Dlcobj  FNMOV
070200991104     C*----------------------------------------------------*
070300991104     C     DLCMOV        BEGSR
070400991104     C*
070500991104     C* GUARDO SE è ALLOCATO
070600991104     C  N91              CLOSE     FNMOV00T                             91
070700991104     C*
070800991104     C                   MOVE      VAR           CMDD(5)
070900991104     C                   MOVEA     CMDD(5)       COMMAN
071000020926     C                   Z-ADD     49            LUNCMD           15 5
071100991104     C                   CALL      'QCMDEXC'                            91
071200991104     C                   PARM                    COMMAN
071300991104     C                   PARM                    LUNCMD
071400991104     C*
071500991104     C                   ENDSR
071600000609     C*----------------------------------------------------*
071700910717     C* CARICAMENTO SUBFILE CON ASSEGNI VERSABILI
071800991102     C*----------------------------------------------------*
071900910717     C     CARICA        BEGSR
072000911113     C                   SETOFF                                       555152
072100911113     C                   SETOFF                                       49
072200991105     C                   Z-ADD     0             TASS             15 3
072300941103     C     §FLE          SETLL     CNASS02L
072400910717     C     *IN55         DOWEQ     '0'
072500941103     C     §FLE          READE     CNASS02L                               55
072600941103     C  N55ASSFVE        IFEQ      'V'                                          VERSABILI
072700941103     C     ASSTPI        ANDNE     'M'
072800941103     C     ASSNDV        ANDEQ     0                                            NO ALTRE D
072900991102     C     ASSVCA        ANDEQ     V1CDIV                                       SOLO DIV V
073000991102     C*
073100910717     C* LA PRIMA VOLTA SALTO IL CONFRONTO
073200910717     C  N51              DO
073300910717     C                   EXSR      PREC
073400910717     C                   SETON                                        51
073500910717     C                   END
073600910717     C*
073700941103     C     ASSNRA        IFNE      PNRA
073800991102     C     ASSDTE        ORNE      PDTE
073900941103     C     ASSABI        ORNE      PABI
074000951016     C     ASSCAB        ORNE      PCAB
074100910717     C                   EXSR      MUOVI
074200100521     C                   ADD       1             NR2
074300991102     C                   WRITE     CN0W9S2
074400910717     C                   ELSE
074500941103     C                   ADD       ASSCAS        TASS
074600910717     C                   END
074700940831     C                   END
074800910717     C                   END
074900910722     C* CARICO L'ULTIMO ASSEGNO
075000910722     C   51              DO
075100910722     C                   EXSR      MUOVI
075200991103     C                   ADD       1             NR2
075300991102     C                   WRITE     CN0W9S2
075400910722     C                   END
075500100521     C                   Z-ADD     nr2           STMREC1                        *n° rel sf
075600910722     C                   ENDSR
075700991102     C*----------------------------------------------------*
075800991102     C* Sposta  i campi dai dati di salvataggio a video
075900991102     C*----------------------------------------------------*
076000910717     C     MUOVI         BEGSR
076100991102     C*
076200920520     C     PTPD          COMP      'V'                                6565
076300920520     C   65              MOVEL     'P'           VPAD
076400920520     C  N65              MOVEL     *BLANKS       VPAD
076500910719     C                   MOVEL     PNRA          VNRA
076600910719     C                   MOVE      PNRA          VNRA1
076700940126     C                   MOVEL     PNRA          SAVNRA
076800940126     C                   MOVE      PNRA          SAVNR1
076900910717     C                   MOVEL     PTPD          VTPD
077000910717     C                   Z-ADD     PLNP          VLNP
077100910717     C                   Z-ADD     PAAS          VAAS
077200910717     C                   Z-ADD     PNRS          VNRS
077300910717     C                   Z-ADD     PNSP          VNSP
077400941103     C                   Z-ADD     PDTE          G02INV
077500941103     C                   MOVE      *ZEROS        G02DAT
077600941103     C                   MOVE      '3'           G02ERR
077700991120     C                   CALL      'XSRDA8'
077800941103     C                   PARM                    WLBDAT
077900941103     C                   MOVE      G02DAT        VDTE
078000910717     C                   Z-ADD     PABI          VABI
078100910717     C                   Z-ADD     PCAB          VCAB
078200940126     C                   Z-ADD     PABI          SAVABI
078300940126     C                   Z-ADD     PCAB          SAVCAB
078400910723     C                   Z-ADD     TASS          VASS
078500060403     C                   Z-ADD     TASS          VASSh
078600921105     C* DECODIFICO ISTITUTO SOLO SE ABI/CAB<> 1000/1000
078700910717     C                   Z-ADD     PABI          KABI
078800910717     C                   Z-ADD     PCAB          KCAB
078900910725     C     KABI§         CHAIN     CNABI00F                           77
079000910725     C  N77              MOVEL     ABIIST        VDES
079100910725     C   77              MOVE      *BLANKS       VDES
079200910717     C*
079300911219     C  N77              MOVEL     ABICOM        VPIA
079400911219     C   77              MOVE      *BLANKS       VPIA
079500911219     C*
079600910717     C                   MOVEL     PTPA          VTPA
079700910717     C                   MOVEL     PTPI          VTPI
079800910717     C                   MOVEL     PTPP          VTPP
079900911219     C*                    MOVELPPIA      VPIA
080000910717     C                   MOVEL     PNOT          VNOT
080100910716     C*
080200910717     C* AZZERO TOTALIZZATORE ASSEGNI
080300910717     C                   Z-ADD     0             TASS
080400941103     C                   ADD       ASSCAS        TASS
080500910717     C* MEMORIZZO N° ASS.,ABI-CAB,DATA ..... PRECEDENTI
080600910717     C                   EXSR      PREC
080700910719     C                   ENDSR
080800991102     C*----------------------------------------------------*
080900991102     C* Sposta  i dati nei campi di salvataggio
081000991102     C*----------------------------------------------------*
081100910717     C     PREC          BEGSR
081200941103     C                   MOVEL     ASSNRA        PNRA             10
081300941103     C                   MOVEL     ASSTPD        PTPD              1
081400941103     C                   Z-ADD     ASSLNP        PLNP              3 0
081500941103     C                   Z-ADD     ASSAAS        PAAS              4 0
081600941103     C                   Z-ADD     ASSNRS        PNRS              2 0
081700941103     C                   Z-ADD     ASSNSP        PNSP              7 0
081800991102     C                   MOVE      ASSDTE        PDTE              8 0
081900941103     C                   Z-ADD     ASSABI        PABI              5 0
082000941103     C                   Z-ADD     ASSCAB        PCAB              5 0
082100941103     C                   MOVEL     ASSTPA        PTPA              1
082200941103     C                   MOVEL     ASSTPI        PTPI              1
082300941103     C                   MOVEL     ASSTPP        PTPP              1
082400941103     C                   MOVEL     ASSNOT        PNOT             20
082500941103     C                   MOVEL     ASSFVE        PFVE              1
082600910722     C                   ENDSR
082700910722     C*
082800000608     C*----------------------------------------------------*
082900000608      * SE RICHIESTO L'ESCLUSIONE TOTALE DI TUTTI GLI ASSEGNI
083000000608      * TRAMITE IL F14 IMPOSTA AUTOMATICAMENTE SU TUTTI
083100000608     C*----------------------------------------------------*
083200000608     C     EXCLRD        BEGSR
083300000608      *
083400000608     C                   SETOFF                                       9028
083500000608      *
083600000608     C                   DO        SAVNR2        CNT
083700000608      *
083800000608     C                   SETOFF                                       242762
083900000608     C                   SETOFF                                       495852
084000000608     C                   SETOFF                                       434445
084100000608     C                   SETOFF                                       464748
084200000608      *
084300000608     C     CNT           CHAIN     CN0W9S2                            60
084400000608      *
084500000608     C     VNRA          IFNE      *BLANK
084600000608     C                   MOVE      'E'           VANN
084700000608     C                   END
084800000608      *
084900000608     C                   UPDATE    CN0W9S2
085000000608      *
085100000608     C                   ENDDO
085200000608      *
085300000608     C                   ENDSR
085400991102     C*----------------------------------------------------*
085500991102     C* Controlli i dati del subfile
085600991102     C*----------------------------------------------------*
085700910717     C     CONTR         BEGSR
085800000609      *
085900951027     C                   SETOFF                                       9028
086000910717     C                   DO        TREC          CNT
086100991209     C                   SETOFF                                       242762
086200911115     C                   SETOFF                                       495852
086300911115     C                   SETOFF                                       434445
086400911115     C                   SETOFF                                       464748
086500000609      *
086600991102     C     CNT           CHAIN     CN0W9S2                            60
086700060526     c* 19 acceso utente di sede
086800060526     c                   if        vann = 'A' and not *in19
086900060411     c                   clear                   vann
087000060411     c                   end
087100000608      *
087200000608      * SE RICHIESTO L'ESCLUSIONE TOTALE DI TUTTI GLI ASSEGNI
087300000608      * TRAMITE IL F14 IMPOSTA AUTOMATICAMENTE SU TUTTI
087400000608      *
087500910717      *-------------
087600910717     C* ESEGUE CONTR. SOLO SE NON SONO ASSEGNI PROVENIENTI DA DISTINTA
087700910717     C* PADRONCINO
087800060526     C     VASS          ifne      0
087900910717     C     VANN          IFEQ      'A'
088000910729     C   KF              DO
088100910717     C                   MOVEL     VNRA          KNRA
088200910717     C                   MOVE      VNRA1         KNRA
088300910717     C                   Z-ADD     VABI          KABI
088400910717     C                   Z-ADD     VCAB          KCAB
088500060526     C     KASS          setll     CNASS000
088600060526     c                   do        *hival
088700060526     C     KASS          reade     CNASS000
088800060526     c                   if        %eof(cnass02l)
088900060526     c                   leave
089000060526     c                   end
089100951027     C     ASSFVE        IFEQ      'V'
089200951027     C     ASSTPI        ANDNE     'M'
089300150319     c*                  exsr      wrttnass
089400060526     C                   DELETE    CNASS000
089500951027     C                   END
089600060526     C                   ENDdo
089700940126     C*
089800940126     C     SAVNRA        IFNE      VNRA
089900940126     C     SAVNR1        ORNE      VNRA1
090000940126     C     SAVABI        ORNE      VABI
090100940126     C     SAVCAB        ORNE      VCAB
090200940126     C                   MOVEL     SAVNRA        KNRA
090300940126     C                   MOVE      SAVNR1        KNRA
090400940126     C                   Z-ADD     SAVABI        KABI
090500940126     C                   Z-ADD     SAVCAB        KCAB
090600060526     C     KASS          setll     CNASS000
090700060526     c                   do        *hival
090800060526     C     KASS          reade     CNASS000
090900060526     c                   if        %eof(cnass02l)
091000060526     c                   leave
091100060526     c                   end
091200951027     C     ASSFVE        IFEQ      'V'
091300951027     C     ASSTPI        ANDNE     'M'
091400150319     c*                  exsr      wrttnass
091500060526     C                   DELETE    CNASS000
091600940126     C                   END
091700060526     C                   ENDdo
091800060411     C                   END
091900940126     C*
092000910717     C                   EXSR      PULI
092100060411     C                   ENDdo
092200910717     C                   ELSE
092300910717     C                   EXSR      CTR
092400910717     C                   END
092500920520     C                   SETOFF                                       65
092600991102     C                   UPDATE    CN0W9S2
092700911219     C                   END
092800950303     C  N90              END
092900910717     C                   ENDSR
093000991102     C*----------------------------------------------------*
093100911115     C* COTROLLI SUBFILE
093200991102     C*----------------------------------------------------*
093300910717     C     CTR           BEGSR
093400060421      * CONTROLLO METTO V SU TIPO DOCUMENTO
093500060421     C     VTPD          IFEQ      *BLANKS
093600060421     C                   MOVE      'V'           VTPD
093700060421     C                   END
093800910717     C*
093900991209     C* CONTROLLO CHE IL NUMERO DEI DECIMALI SIA CONGRUENTE
094000991209     C                   MOVE      VASS          WCOMO1            1 0
094100991209     C                   MOVE      VASS          WCOMO2            2 0
094200010626     C     V1CDIV        IFEQ      §GEDCR
094300991209     C     DECDIV        ANDLT     2
094400991209     C     WCOMO1        ANDGT     0
094500010626     C     V1CDIV        OREQ      §GEDCR
094600991209     C     DECDIV        ANDLT     1
094700991209     C     WCOMO2        ANDGT     0
094800991209     C     V1CDIV        OREQ      DIVALT
094900991209     C     DECALT        ANDLT     2
095000991209     C     WCOMO1        ANDGT     0
095100991209     C     V1CDIV        OREQ      DIVALT
095200991209     C     DECALT        ANDLT     1
095300991209     C     WCOMO2        ANDGT     0
095400991209     C                   SETON                                        62  90
095500991209     C                   END
095600910717      * CONTROLLO DATA ASSEGNO
095700910717      *
095800060414     c                   if        vann <> 'E'
095900951027     C                   SETOFF                                       56
096000911113     C                   MOVE      VDTE          G02DAT
096100911113     C                   MOVE      *BLANKS       G02ERR
096200991102     C                   CALL      'XSRDA8'
096300910717     C                   PARM                    WLBDAT
096400910717     C     G02ERR        IFEQ      '1'
096500060403     c                   SETON                                        43  90
096600060414     c                   else
096700060414     c                   move      g02inv        dte
096800060403     C                   END
096900060411     c* controllo che la data non sia considerata postdatato
097000060414     c                   eval      sas = vtpa
097100060414     c                   callp     chkdataassegno(dte:sas
097200060414     c                             :esito:err:*omit)
0973000604141    c                   if        esito <>0
097400060411     C                   SETON                                        43  90
097500060411     C                   END
097600060411     c* obbligo ad escludere gli assegno con data > oggi
097700060411     C     G02inv        IFgt      oggi
097800060411     c                   SETON                                        44
097900060403     C                   END
098000991104     C                   MOVE      G02DAT        VDTE
098100060403     c* IMPORTO ASSEGNO OBBLIGATORIO
098200060403     C  n90VASS          IFEQ      0
098300060414     C                   SETON                                        456290
098400910717     C                   END
098500060403     c* controllo che l'importo dell'assegno non sia variato + dell'importo
098600060403     c* di tolleranza
098700060403     C  n90VASS          IFne      vassh
098800060414     C     vtpd          andne     'V'
098900060403     c                   exsr      srtolle
099000060403     C                   END
099100060414     C                   END
099200910717      ****************************************
099300910717      * SE TIPO ASSEGNO E' MITTENTE
099400910717      * ABI, CAB E N° ASSEGNO SONO OBBLIGATORI
099500910717      ****************************************
099600060403     C  n90VABI          IFEQ      0
099700911127     C     VTPA          ANDNE     'P'
099800911127     C     VTPA          ANDNE     'V'
099900910717     C     VCAB          OREQ      0
100000911127     C     VTPA          ANDNE     'P'
100100911127     C     VTPA          ANDNE     'V'
100200911127     C                   SETON                                        49  90
100300911127     C                   END
100400060403     C  n90VNRA          IFEQ      *BLANKS
100500910717     C                   SETON                                        47  90
100600910717     C                   END
100700911127      * CONTROLLO CODICE ABI/CAB SE TIPO ASSEGNO < > 'P' E 'V'
100800921105      * SOLO SE ABI/CAB NON E' 1000/1000
100900921105     C     VABI          IFNE      1000
101000921105     C     VCAB          ANDNE     1000
101100910725     C                   Z-ADD     VABI          KABI
101200910725     C                   Z-ADD     VCAB          KCAB
101300910725     C     KABI§         CHAIN     CNABI00F                           49
101400910729     C  N49VPIA          IFEQ      *BLANKS
101500910729     C                   MOVEL     ABICOM        VPIA
101600910729     C                   END
101700910717     C  N49              MOVEL     ABIIST        VDES
101800910801     C   49              MOVE      *BLANKS       VDES
101900910801     C     VPIA          IFEQ      *BLANKS
102000911113     C  NKF
102100911113     CANNKA              SETON                                        5290
102200910801     C                   END
102300911113     C   49
102400911113     CANNKF
102500911113     CANNKA              SETON                                        90
102600921105     C                   END
102700910716     C*
102800910722      * CONTROLLO TIPO ASSEGNO
102900910722     C     VTPA          IFNE      *BLANKS
103000910722     C                   MOVEA     *BLANKS       SKI
103100910722     C                   MOVEA     VTPA          SKI
103200910722     C     '?'           LOOKUP    SKI                                    31
103300150319     C   31              DO
103400910722     C                   MOVEL     CODUT         §KUT              1 0
103500910722     C                   MOVEL     '4E'          §COD              2
103600910722     C                   MOVE      *BLANKS       VTPA
103700910722     C                   CALL      'X§TABER'
103800910722     C                   PARM                    §KUT
103900910722     C                   PARM                    §COD
104000910722     C                   PARM                    §KEY              8
104100910722     C                   PARM                    §DES             30
104200910722     C                   MOVEL     §KEY          VTPA
104300910722     C                   END
104400910722     C*-----------------------------------------------------*
104500910722     C* Se no si ricerca su TABEL se esiste
104600910722     C*-----------------------------------------------------*
104700910722     C  N31              DO
104800910722     C                   MOVE      *BLANKS       KEY
104900910722     C                   MOVEL     VTPA          KEY
105000910722     C                   MOVE      '4E'          COD
105100910722     C     KTAB          CHAIN     TABEL                              24
105200921105     C  N24              EXCEPT    RILTAB
105300910722     C  N24TBLFLG        COMP      '*'                                    24
105400910723     C   24              SETON                                          90
105500910722     C                   END
105600910722     C                   ELSE
105700910723     C                   SETON                                        2490
105800910722     C                   END
105900951027     C* Se assegno vario controllo se esiste con quei
106000951027     C* riferimenti un assegno proveiente da una chiusura
106100951027     C     VPAD          IFNE      'P'
106200951027     C                   MOVEL     VNRA          KNRA
106300951027     C                   MOVE      VNRA1         KNRA
106400951027     C                   Z-ADD     VABI          KABI
106500951027     C                   Z-ADD     VCAB          KCAB
106600951027     C     KASS          CHAIN     CNASS02L                           57
106700951027     C     *IN57         IFEQ      '0'
106800951027     C     *IN57         DOWEQ     '0'
106900951027     C     ASSTPD        ANDEQ     'V'
107000951027     C     KASS          READE     CNASS02L                               57
107100951027     C                   END
107200951027     C     ASSTPD        IFNE      'V'
107300951027     C     ASSTPI        IFEQ      'M'
107400951027     C                   SETON                                        285690
107500951027     C                   MOVEL     ERR(3)        $MSG
107600951027     C                   ELSE
107700951027     C     ASSFVE        IFNE      'V'
107800951027     C                   SETON                                        285690
107900951027     C                   MOVEL     ERR(1)        $MSG
108000951027     C                   ELSE
108100951027     C                   SETON                                        285690
108200951027     C                   MOVEL     ERR(2)        $MSG
108300951027     C                   END
108400951027     C                   END
108500951027     C                   END
108600951027     C                   END
108700951027     C                   END
108800910722     C*
108900910729     C  N90
109000910729     CAN KF
109100910729     CANN11              DO
109200940126     C                   MOVEL     VNRA          KNRA
109300940126     C                   MOVE      VNRA1         KNRA
109400940126     C                   Z-ADD     VABI          KABI
109500940126     C                   Z-ADD     VCAB          KCAB
109600941103     C     KASS          CHAIN     CNASS02L                           57
109700940126     C   57              EXSR      SPOSTA
109800941103     C   57              WRITE     CNASS000
109900941103     C  N57              UPDATE    CNASS000
110000940126     C*
110100940126     C     SAVNRA        IFNE      VNRA
110200940126     C     SAVNR1        ORNE      VNRA1
110300940126     C     SAVABI        ORNE      VABI
110400940126     C     SAVCAB        ORNE      VCAB
110500940126     C                   MOVEL     SAVNRA        KNRA
110600940126     C                   MOVE      SAVNR1        KNRA
110700940126     C                   Z-ADD     SAVABI        KABI
110800940126     C                   Z-ADD     SAVCAB        KCAB
110900941103     C     KASS          CHAIN     CNASS000                           57
111000951027     C     ASSFVE        IFEQ      'V'
111100951027     C     ASSTPI        ANDNE     'M'
111200150313     c  n57              exsr      wrttnass
111300941103     C  N57              DELETE    CNASS000
111400940126     C                   END
111500951027     C                   END
111600951026     C                   MOVE      VNRA          SAVNRA
111700951026     C                   MOVE      VNRA1         SAVNR1
111800951026     C                   Z-ADD     VABI          SAVABI
111900951026     C                   Z-ADD     VCAB          SAVCAB
112000991104     C                   COMMIT
112100910723     C                   END
112200910717     C                   ENDSR
112300991102     C*-----------------------------------------------------*
112400060403     C* tolleranza importo assegno
112500991102     C*-----------------------------------------------------*
112600060403     C     srtolle       BEGSR
112700060403     C     Vass          SUB       Vassh         §abb
112800060403     C* LO RENDO POSITIVO
112900060403     C     §ABB          IFLT      *ZEROS
113000060403     C     §ABB          MULT      -1            §ABB
113100060403     C                   END
113200060403     C* CONTROLLO IL LIMITE tollerabile DELLA differenza
113300060403     C                   MOVEL     'GEI'         KCOD1
113400060403     C                   MOVEL(p)  'EUR'         KKEY1
113500060403     c                   clear                   dgei
113600060403     C     KTBE          CHAIN     TNTBE01L
113700060403     c                   if        %found(tntbe01l)  and tbeatb = ' '
113800060403     C                   MOVEL     TBEUNI        DGEI
113900060403     c                   end
114000060403     C     §ABB          COMP      §GEtvr                             45
114100060403     C   45              SETON                                        9062
114200060403     C                   ENDSR
114300060403     C*-----------------------------------------------------*
114400060403     C* SCRIVO CNASS
114500060403     C*-----------------------------------------------------*
114600060403     C     SPOSTA        BEGSR
114700000609      *
114800941103     C                   CLEAR                   CNASS000
114900941103     C                   Z-ADD     §FLE          ASSFLE
115000941103     C                   MOVEL     VTPD          ASSTPD
115100941103     C                   Z-ADD     VASS          ASSCAS
115200941103     C                   MOVEL     VNRA          ASSNRA
115300991104     C                   MOVEL     V2CDIV        ASSVCA
115400941103     C                   MOVE      VNRA1         ASSNRA
115500941103     C                   Z-ADD     VDTE          G02DAT
115600941103     C                   MOVEL     *BLANKS       G02ERR
115700941103     C                   MOVE      *ZEROS        G02INV
115800991102     C                   CALL      'XSRDA8'
115900941103     C                   PARM                    WLBDAT
116000941103     C                   Z-ADD     G02INV        ASSDTE
116100941103     C                   Z-ADD     VABI          ASSABI
116200941103     C                   Z-ADD     VCAB          ASSCAB
116300941103     C                   MOVEL     VTPA          ASSTPA
116400941103     C                   MOVEL     VTPI          ASSTPI
116500941103     C                   MOVEL     VTPP          ASSTPP
116600941103     C                   MOVEL     VPIA          ASSPIA
116700941103     C                   MOVEL     VNOT          ASSNOT
116800941103     C                   MOVEL     'V'           ASSFVE
116900910723     C                   ENDSR
117000991102     C*-----------------------------------------------------
117100910723     C* CARICA RECORD VUOTI NEL SUBFILE
117200991102     C*-----------------------------------------------------
117300910717     C     AGG           BEGSR
117400920520     C                   SETOFF                                       65
117500910717     C                   EXSR      PULI
117600910717     C                   DO        50
117700991103     C                   ADD       1             NR2
117800991102     C                   WRITE     CN0W9S2
117900910717     C                   END
118000910722     C                   ENDSR
118100991102     C*-----------------------------------------------------
118200991102     C* CARICA RECORD VUOTI NEL SUBFILE
118300991102     C*-----------------------------------------------------
118400910717     C     PULI          BEGSR
118500910725     C                   MOVE      *BLANKS       VPAD
118600910717     C                   MOVE      *BLANKS       VANN
118700910717     C                   MOVE      *BLANKS       VNRA
118800910717     C                   MOVE      *BLANKS       VNRA1
118900940126     C                   MOVE      *BLANKS       SAVNRA
119000940126     C                   MOVE      *BLANKS       SAVNR1
119100910717     C                   Z-ADD     0             VDTE
119200910717     C                   Z-ADD     0             VASS
119300060403     C                   Z-ADD     0             VASSh
119400910717     C                   Z-ADD     0             VABI
119500910717     C                   Z-ADD     0             VCAB
119600940126     C                   Z-ADD     0             SAVABI
119700940126     C                   Z-ADD     0             SAVCAB
119800910717     C                   MOVE      *BLANKS       VTPA
119900910717     C                   MOVE      *BLANKS       VTPP
120000910717     C                   MOVE      *BLANKS       VTPI
120100910717     C                   MOVE      *BLANKS       VNOT
120200910717     C                   MOVE      *BLANKS       VTPD
120300910717     C                   Z-ADD     0             VLNP
120400910717     C                   Z-ADD     0             VAAS
120500910717     C                   Z-ADD     0             VNRS
120600910717     C                   Z-ADD     0             VNSP
120700910717     C                   MOVE      *BLANKS       VDES
120800910717     C                   MOVE      *BLANKS       VPIA
120900000608      *
121000910717     C                   ENDSR
121100991102     C*-----------------------------------------------------
121200991102     C* LEGGO SUBFILE E CALCOLO TOTALI X VIDEATA REGISTR.
121300991102     C*-----------------------------------------------------
121400910717     C     CONF          BEGSR
121500000608      *
121600150109     c                   clear                   lista
121700910726     C                   Z-ADD     0             TOTASS
121800910726     C                   MOVEA     *ZEROS        IMP
121900930927     C                   MOVEA     *ZEROS        IM1
122000000608      *
122100910717     C                   DO        TREC          CNT               4 0
122200991102     C     CNT           CHAIN     CN0W9S2                            61
122300000608      *
122400910718     C* TOTALE ASSEGNI
122500910731     C     VANN          IFNE      'E'
122600910717     C                   ADD       VASS          TOTASS
122700910718     C* TOT. IMPORTI PER TIPO
122800910718     C                   Z-ADD     1             A                 3 0
122900910719     C                   MOVEL     VTPA          LKP               2
123000910719     C                   MOVE      VTPP          LKP
123100910719     C     LKP           LOOKUP    CSP(A)                                 21
123200930927     C*
123300930927     C     VABI          IFEQ      ISTABI
123400910718     C   21              ADD       VASS          IMP(A)
123500930927     C                   ELSE
123600930927     C   21              ADD       VASS          IM1(A)
123700910717     C                   END
123800150108     c*esclusi
123900150108     c                   else
124000150108     c                   exsr      VerEsclusi
124100930927     C*
124200930927     C                   END
124300910731     C                   END
124400910717     C* TOTALE PROVVISORIO DISTINTA
124500910717     C     TOTASS        ADD       TOTCON        TOTDIS
124600991102     C                   Z-ADD     GMAUD8        VIDDRG
124700991118     C                   Z-ADD     GMAUD8        VDTOPE
124800991102     C                   Z-ADD     GMAUD8        VIDDDS
124900941104     C                   MOVE      §FLE          VFPN
125000910718     C* PROPONGO I DATI
125100060403     C                   move      §4occ1        VIDCC1            4 0
125200060403     C                   move      §4osc1        VIDSC1            7 0
125300991102     C* Imposto valuta distinta
125400991102     C                   MOVEL     V2CDIV        V3CDIV
125500000608     C                   MOVEL     V1PLIC        V3PLIC
125600910718     C*
125700910725     C     *IN13         DOWEQ     '0'
125800910725     C     *IN17         ANDEQ     '0'
125900980114     C                   SETOFF                                       59
126000950906     C                   Z-ADD     §TCONT        TOTCON
126100950906     C     TOTASS        ADD       TOTCON        TOTDIS
126200991105     C                   Z-ADD     TOTDIS        TOTVER
126300991118     C     START3        TAG
126400991102     C                   EXFMT     CN0W9D3
126500980114     C                   SETOFF                                       28
126600991118     C* CONTROLLO LA DATA OPERAZIONE SE ERRATA OPPURE NO
126700991118     C                   MOVEL     *BLANKS       G02ERR
126800991118     C                   Z-ADD     VDTOPE        G02DAT
126900991118     C                   CALL      'XSRDA8'
127000991118     C                   PARM                    WLBDAT
127100991118     C                   Z-ADD     G02INV        AMGDOP            8 0
127200991118     C     G02ERR        IFEQ      '1'
127300991118     C                   SETON                                        51
127400991118     C                   GOTO      START3
127500991118     C                   END
127600991118     C                   MOVE      G02DAT        VDTOPE
127700020422
127800020422     ** Eseguo sulla data operazione lo stesso controllo di PROJ,
127900020422     ** cioè la data non può discostare di più di 9 mesi dalla data
128000020422     ** corrente.
128100020422     C                   MOVE      VDtOpe        WrkDtOpe
128200020422     C                   TIME                    WrkDtCur
128300020422     C     WrkDtCur      SUBDUR    WrkDtOpe      WrkMonth:*M
128400020422     C                   IF        %ABS(WrkMonth) > 8
128500020422     C                   EVAL      *IN51 = *ON
128600020422     C                   GOTO      Start3
128700020422     C                   ENDIF
128800020422
128900910725     C  N17              DO
129000060824     c* se non ancora inserito contante e ci sono dei mandatini forzo KD
129100060824     c                   if        not *inkd and okcon=' '
129200060824     c     §fle          setll     yfcas01l                               33
129300060824     c                   if        *in33
129400060824     c                   eval      *inkd = '1'
129500060824     c                   eval      okcon = '1'
129600060824     c                   end
129700060824     c                   end
129800060824     c*
129900910723     C   KD              DO
130000910723     C                   EXSR      GESCON                                       GEST.CONTA
130100950906     C                   Z-ADD     §TCONT        TOTCON
130200950906     C     TOTASS        ADD       TOTCON        TOTDIS
130300991105     C                   Z-ADD     TOTDIS        TOTVER
130400991102     C                   Z-ADD     VIDDRG        VIDDDS
130500950906     C                   END
130600000608      *
130700910725     C   13              EXSR      CTR2
130800910726     C   59              SETOFF                                       13
130900910717     C                   END
131000000608      *
131100911125     C   17              SETOFF                                       1112
131200910717     C                   END
131300910717     C                   ENDSR
131400991102     C*-------------------------------------------------------------*
131500991102     C* CONTROLLI
131600991102     C*-------------------------------------------------------------*
131700910718     C     CTR2          BEGSR
131800930222     C*
131900910718     C                   Z-ADD     0             TOTVER
132000991103     C     TOTASS        ADD       TOTCON        TOTDIS
132100991105     C                   Z-ADD     TOTDIS        TOTVER
132200910718     C** NUMERO REG. OBBLIGATORIO
132300910723     C   72              SETON                                        59
132400910718     C   72              GOTO      START2
132500910718     C*  DATA REGISTRAZIONE
132600940329     C                   Z-ADD     VIDDRG        G02DAT
132700940329     C                   MOVEL     *BLANKS       G02ERR
132800991102     C                   CALL      'XSRDA8'
132900940329     C                   PARM                    WLBDAT
133000991102     C                   Z-ADD     G02INV        AMGDRG            8 0
133100991102     C                   Z-ADD     G02TGI        DATARE            8 0          *
133200021115     C* SE NON HO DA VERSARE NULLA VADO A FINE
133300060411     C     TOTVER        ifeq      0
133400021115     C                   GOTO      START2
133500060411     C                   End
133600021115     C  N13              GOTO      START2
133700910718     C*§ EMISSIONE ULTIMA VIDEATA
133800910718     C*  ------------------------
133900910718     C*  PULIZIA SFL 4
134000910718     C                   EXSR      PULI4
134100910718     C*  CARICAMENTO SFL 4
134200910718     C                   EXSR      CARI4
134300910726     C* ATTRIBUZIONE NR° DISTINTA SOLO LA PRIMA VOLTA
134400910726     C  N68              DO
134500910726     C                   MOVEL     '4D'          COD
134600910726     C                   MOVEL     §FLE          KEY
134700910726     C     KTAB          CHAIN     TABEL                              46
134800980114     C     *IN46         IFEQ      '1'
134900980114     C                   SETON                                        2859
135000980114     C                   MOVEL     ERR(4)        $MSG
135100980114     C                   GOTO      START2
135200980114     C                   END
135300910726     C                   MOVEL     TBLUNI        NUMVER
135400910726     C                   ADD       1             NUMVER
135500910726     C                   MOVEL     NUMVER        TBLUNI
135600980114     C  N46              UPDATE    TABEL
135700000609      * CON IL GIRO DELLA BANCA X CONTANTE (66=ON)
135800000609      *  IMPOSTA NEL NR.DISTINTA IL NR.DEL PLICO
135900000616     C                   Z-ADD     NUMVER        VIDNDS
136000991125     C                   Z-ADD     NUMVER        VIDNRG
136100991125     C                   MOVEL     §FLE          VIDNRG
136200910726     C                   SETON                                        68
136300910726     C                   END
136400910726     C*
136500910718     C     EMI4          TAG
136600991103     C                   WRITE     CN0W9Z5
136700991103     C                   EXFMT     CN0W9C5
136800910718     C*                    ____________
136900910725     C   16              SETON                                        59
137000931222     C   16              SETOFF                                       88
137100910725     C   16              GOTO      START2
137200910718     C*
137300910718     C*§ CONTROLLI CAMPI DEL SFL
137400910718     C*  -----------------------
137500991103     C                   Z-ADD     0             NR5
137600910718     C     *IN90         DOUEQ     '1'
137700991103     C                   ADD       1             NR5
137800991103     C     NR5           CHAIN     CN0W9S5                            90
137900910718     C  N90VIMP4         IFNE      *ZEROS
138000910718     C     VSPE4         IFEQ      'CO'
138100910718     C     VSPE4         OREQ      'RE'
138200910718     C     VSPE4         OREQ      'CA'
138300910718     C                   SETON                                        88
138400910718     C                   ELSE
138500910718     C                   SETOFF                                       88
138600910718     C                   END
138700910718     C*
138800910718     C*  CAUSALE CONTABILE
138900910718     C                   MOVEL     'PG'          OPECD1
139000910718     C                   MOVEL     VCAU4         OPECOD                         CAUSALE CO
139100910718     C     KEYOPE        CHAIN     CNOPE000                           87
139200991103     C   87              UPDATE    CN0W9S5
139300910718     C   87              GOTO      EMI4
139400910718     C                   MOVEL     OPEDES        VDES4
139500910718     C                   END
139600910718     C                   END
139700910718     C*
139800910726     C   15              DO
139900910718     C     SUDT3         TAG                                                    *
140000910718     C                   EXSR      STADIS
140100150109     c* verifica se esistono segnalazioni per mandare mail
140200150109     c                   if        lista(1) <> *blank
140201170720      * non invia se sistema di prova
140202170720     c                   if        %subst(knsif:7:1) <> 'P'
140300150109     c                   exsr      inviamail
140301170720     c                   endif
140400150109     c                   endif
140500910718     C*
140600991103     C                   Z-ADD     0             NR5
140700910718     C     *IN90         DOUEQ     '1'
140800991103     C                   ADD       1             NR5
140900991103     C     NR5           CHAIN     CN0W9S5                            90
141000910718     C  N90VIMP4         COMP      0                                      90
141100930924     C*----------------------------------------------------*
141200930924     C     *IN90         IFEQ      '0'                                          *
141300991103     C* Scrivo movimenti contabili
141400991103     C                   EXSR      CREMOV                                       *
141500930924     C                   END                                                    *
141600000612      *
141700930924     C*----------------------------------------------------*
141800930924     C                   END
141900000612     C*----------------------------------------------------*
142000000612      * SCRIVE IL FILE DEI PLICHI DA TRASMETTERE A "SICURDATA"
142100000612      *
142200020321     C                   EXSR      WRIPLI
142300021118      * SCRIVE IL FILE DEI VERSAMENTI GIORNALIERI
142400021118      *
142500021121     C                   EXSR      WRTVRT
142600060411      * annulla i mandati cassaforte con ferritoia
142700060411      *
142800060922     c                   if        totcon <> 0
142900060411     C                   EXSR      annman
143000060922     c                   end
143100020321
143200991104     C*----------------------------------------------------*
143300991104     C* Sottomette trasmissioni                            *
143400991108     C                   COMMIT
143500000612      *
143600991104     C                   EXSR      SBMTRA                                       *
143700000612      *
143800930924     C*----------------------------------------------------*
143900910718     C                   EXSR      ANNDIS
144000000612      *
144100910726     C                   SETON                                        12
144200910726     C                   MOVE      '1'           *INKC
144300910801     C                   GOTO      START2
144400910718     C                   END
144500910726     C  N16              GOTO      EMI4
144600910718     C     START2        TAG
144700910718     C*
144800910718     C                   ENDSR
144900991102     C**----------------------------------------------------------
145000000000     C** PULIZIA DEL SUBFILE
145100991102     C**----------------------------------------------------------
145200000000     C     PULIZ         BEGSR
145300000000     C                   SETON                                        70
145400991102     C                   WRITE     CN0W9C2
145500000000     C                   SETOFF                                       70
145600910722     C                   SETON                                        71
145700991103     C                   Z-ADD     0             NR2
145800910725     C* PULISCO CAMPI DI COMODO
145900991102     C                   MOVE      V1CDIV        V2CDIV
146000991102     C                   MOVE      *BLANKS       PNRA
146100910725     C                   Z-ADD     0             PDTE
146200910725     C                   Z-ADD     0             PABI
146300910725     C                   Z-ADD     0             PCAB
146400910725     C                   MOVE      *BLANKS       PTPA
146500910725     C                   MOVE      *BLANKS       PTPP
146600910725     C                   MOVE      *BLANKS       PFVE
146700910725     C                   MOVE      *BLANKS       PTPI
146800910725     C                   MOVE      *BLANKS       PNOT
146900910725     C                   MOVE      *BLANKS       PTPD
147000910725     C                   Z-ADD     0             PLNP
147100910725     C                   Z-ADD     0             PAAS
147200910725     C                   Z-ADD     0             PNRS
147300910725     C                   Z-ADD     0             PNSP
147400950906     C                   Z-ADD     0             §TCONT
147500911219     C*                    MOVE *BLANKS   PPIA
147600910725     C*
147700000000     C                   ENDSR
147800991103     C*-------------------------------------------------------
147900900517     ** GESTIONE CONTANTI/ASSEGNI
148000991103     **-------------------------------------------------------
148100900517     C     GESCON        BEGSR
148200000608      *
148300910725     C                   SETOFF                                       1813
148400991102     C     V3CDIV        COMP      TGDDIV                                 09
148500000608      *
148600900517     C  N09              EXSR      PULCON
148700000608      *
148800900517     C* CARICAMENTO SUBFILE
148900991102     C                   MOVEL     V3CDIV        V4CDIV
149000000608     C                   MOVEL     V3PLIC        V4PLIC
149100060824     c                   if        not *in09
149200060824     C                   EXSR      CARCON
149300060824     C                   EXSR      CTRCON
149400060824     c                   end
149500900517     C*                    ----------
149600000608      *
149700900517     C                   SETON                                        09        1° VOLTA
149800900517     C     EMICON        TAG
149900991102     C                   WRITE     CN0W9Z4
150000991102     C                   EXFMT     CN0W9C4
150100000608      *
150200910725     C   18              GOTO      ENDCON
150300000608      *
150400000608     C* CONTROLLO CONTANTI IMMESSI
150500000608     C                   EXSR      CTRCON
150600000608      *
150700900518     C* CALCOLO  TOTALE CONTANTI /
150800910726     C   14              EXSR      CALCON
150900900517     C*                    -----------
151000910726     C  N14
151100910726     CANN18              GOTO      EMICON
151200000608      *
151300900517     C     ENDCON        TAG
151400900517     C                   ENDSR
151500991103     C**----------------------------------------------------------
151600900518     C** PULIZIA DEL SUBFILE CONTANTI
151700991103     C**----------------------------------------------------------
151800900517     C     PULCON        BEGSR
151900900517     C                   SETON                                        70
152000991102     C                   SETOFF                                       71
152100991102     C                   WRITE     CN0W9C4
152200900517     C                   SETOFF                                       70
152300900517     C                   ENDSR
152400991103     C**----------------------------------------------------------
152500900518     C** CARICAMENTO SUBFILE CONTANTI
152600991103     C**----------------------------------------------------------
152700900517     C     CARCON        BEGSR
152800000608      *
152900900517     C* CARICO I TAGLI PER IL CONTANTE
153000900517     C* L'IMPORTO IN TABELLA è 0 IL CAMPO IMPORTO è SPROTETTO
153100900517     C* LA DESCRIZ. DEI TAGLI CONTANTE è PROTETTO
153200991102     C                   CLEAR                   VNRTGL
153300900517     C                   SETON                                        40
153400991103     C                   Z-ADD     0             NR4               4 0
153500991104     C                   MOVEL     V3CDIV        TGDDIV
153600991102     C     TGDDIV        CHAIN     YNTGD01L                           21
153700991102     C     *IN21         DOWEQ     '0'
153800000621      *
153900000621     C     *IN66         IFEQ      *OFF
154000000621     C     *IN66         OREQ      *ON
154100000621     C     TGDVAL        ANDGT     0
154200991102     C                   MOVE      TGDDES        VDESCR
154300991104     C                   Z-ADD     TGDVAL        HIMTGL
154400900517     C     HIMTGL        COMP      0                                  20        PROTETTO
154500900517     C                   MOVEL     'C'           HIDTIP
154600060411     c                   exsr      srman
154700991103     C                   ADD       1             NR4
154800991102     C                   WRITE     CN0W9S4
154900991102     C                   SETON                                        71
155000000621     C                   END
155100000621      *
155200991102     C     TGDDIV        READE     YNTGD01L                               21
155300900517     C                   END
155400900517     C                   SETOFF                                       2040
155500020417     C                   EVAL      WrkLstRrnS4 = Nr4
155600000608      *
155700900517     C                   ENDSR
155800991103     C**----------------------------------------------------------
155900000608     C** CONTROLLO  CONTANTI  IMMESSI
156000991103     C**----------------------------------------------------------
156100000608     C     CTRCON        BEGSR
156200000608      *
156300020417     C                   IF        WrkLstRrnS4 > 0
156400000608      *  AGGIORNA SEMPLICEMENTE IL SFL
156500000608     C                   Z-ADD     0             NR4
156600000608 +++ C                   Z-ADD     0             §TCONT
156700000612     C                   Z-ADD     0             TOTIM            15 3
156800000608      *
156900000608     C                   DO        *HIVAL
157000000608     C                   ADD       1             NR4
157100000608     C     NR4           CHAIN     CN0W9S4                            63
157200000608      *
157300000608      *  PER FINE FILE ESCE DAL GIRO
157400000608     C     *IN63         IFEQ      *ON
157500000608     C                   LEAVE
157600000608     C                   END
157700000608      *
157800000608      *  CALCOLA LA RIGA
157900000608     C     HIDTIP        IFEQ      'C'
158000000608     C     HIMTGL        ANDNE     *ZEROS
158100060411     c                   eval      vidim = (vnrtgl + vnrman)*himtgl
158200000612     C                   ADD       VIDIM         TOTIM
158300000608     C     HIMTGL        COMP      0                                  20        PROT.VIDIM
158400000608     C                   SETON                                        40
158500000608     C                   UPDATE    CN0W9S4
158600000608     C                   END
158700000608     C*
158800000608     C                   END
158900020417      *
159000020417     C                   ENDIF
159100000608      *
159200000608     C                   ENDSR
159300000608     C**----------------------------------------------------------
159400000608     C** CALCOLO TOTALE  CONTANTI
159500000608     C**----------------------------------------------------------
159600000608     C     CALCON        BEGSR
159700020417      *
159800020417     C                   IF        WrkLstRrnS4 > 0
159900000608      *
160000991103     C                   Z-ADD     0             NR4
160100910730 +++ C                   Z-ADD     0             §TCONT
160200900517     C                   DO        *HIVAL
160300000612      *
160400991103     C                   ADD       1             NR4
160500991103     C     NR4           CHAIN     CN0W9S4                            63
160600910731     C     *IN63         IFEQ      '0'
160700000612      *
160800900517     C     HIDTIP        IFEQ      'C'
160900900517     C     HIMTGL        IFNE      *ZEROS
161000060411     c                   eval      vidim = (vnrtgl + vnrman)*himtgl
161100900517     C                   ADD       VIDIM         §TCONT
161200900517     C     HIMTGL        COMP      0                                  20        PROT.VIDIM
161300900517     C                   SETON                                        40
161400991102     C                   UPDATE    CN0W9S4
161500900517     C                   ELSE
161600900517     C                   ADD       VIDIM         §TCONT
161700900517     C                   END
161800000612      *
161900900517     C                   END
162000900517     C*
162100900517     C                   END
162200000612     C*
162300910731     C  N63              END
162400020417      *
162500020417     C                   ENDIF
162600000608      *
162700900517     C                   ENDSR
162800991103     C**----------------------------------------------------------
162900060411     C** aggiunge i mandati della cassaforte con ferritoia
163000991103     C**----------------------------------------------------------
163100060411     C     srman         BEGSR
163200060411     c                   clear                   vnrman
163300060411     c     §fle          setll     yfcas01l
163400060411     c                   do        *hival
163500060411     c     §fle          reade     yfcas01l
163600060411     c                   if        %eof(yfcas01l)
163700060411     c                   leave
163800060411     c                   end
163900060411     c                   if        CASVALORE = HIMTGL
164000060411     c                   add       casqta        vnrman
164100060411     c                   end
164200060411     c                   enddo
164300060411     C                   ENDSR
164400060411     C**----------------------------------------------------------
164500060411     C** annulla i mandati della cassaforte con ferritoia
164600060411     C**----------------------------------------------------------
164700060411     C     annman        BEGSR
164800061120     c* prima di cancellare chiamo il pgm per memorizzare in un archivio
164900061120     c* di sede i mandati
165000061120     c                   eval      fle= §fle
165100061120     c                   eval      kpjbu = CNC0WD00S
165200061120     c                   call      'CNC0WD00R'
165300061120     c                   parm                    kpjba
165400061120     C*
165500060411     c     §fle          setll     yfcas01l
165600060411     c                   do        *hival
165700060411     c     §fle          reade     yfcas01l
165800060411     c                   if        %eof(yfcas01l)
165900060411     c                   leave
166000060411     c                   end
166100060411     c                   delete    yfcas000
166200060411     c                   enddo
166300060411     C                   ENDSR
166400060411     C**----------------------------------------------------------
166500060411     C** PULIZIA DELL'ULTIMO  SUBFILE
166600060411     C**----------------------------------------------------------
166700060411     C     PULI4         BEGSR
166800891205     C                   SETON                                        70
166900991102     C                   WRITE     CN0W9C5
167000891205     C                   SETOFF                                       70
167100891205     C                   ENDSR
167200991103     C**----------------------------------------------------------
167300891205     C** CARICAMENTO SUBFILE 4
167400991103     C**----------------------------------------------------------
167500891205     C     CARI4         BEGSR
167600891205     C*
167700991104     C                   MOVEL     V3CDIV        V5CDIV
167800000608     C                   MOVEL     V3PLIC        V5PLIC
167900991103     C                   Z-ADD     0             NR5               4 0
168000000608      *
168100891205     C                   DO        20            A
168200891205     C     IMP(A)        IFNE      *ZEROS
168300891205     C                   MOVE      CSP(A)        VSPE4
168400891205     C                   Z-ADD     IMP(A)        VIMP4
168500991120     C                   Z-ADD     0             VNRP4
168600991120     C                   Z-ADD     0             VDRP4
168700891205     C                   MOVE      'D'           VDA4
168800891205     C                   MOVE      CAU(A)        VCAU4
168900940831     C                   SETOFF                                       32
169000891205     C                   Z-ADD     KBNA          VKCC4
169100910726     C                   Z-ADD     VBAN          VKSC4
169200891205     C*
169300891205     C                   SETOFF                                       88
169400991102     C                   ADD       1             NR5
169500991102     C                   WRITE     CN0W9S5
169600991103     C                   END
169700930927     C*
169800930927     C*  CARICO  SUBFILE X IMPORTI CAUSALI ALTRO ISTITUTO
169900930927     C*
170000930927     C     IM1(A)        IFNE      *ZEROS
170100930927     C                   MOVE      CSP(A)        VSPE4
170200930927     C                   Z-ADD     IM1(A)        VIMP4
170300930927     C                   MOVE      'D'           VDA4
170400930927     C                   MOVE      CA1(A)        VCAU4
170500940831     C                   SETOFF                                       32
170600930927     C                   Z-ADD     KBNA          VKCC4
170700930927     C                   Z-ADD     VBAN          VKSC4
170800991120     C                   Z-ADD     0             VNRP4
170900991120     C                   Z-ADD     0             VDRP4
171000930927     C*
171100930927     C                   SETOFF                                       88
171200991103     C                   ADD       1             NR5
171300991102     C                   WRITE     CN0W9S5
171400930927     C                   END
171500930927     C*
171600891205     C                   END
171700891205     C* CONTANTE
171800891205     C* --------
171900891205     C     TOTCON        IFGT      *ZEROS
172000891205     C* 88 ON RENDE MODIFICABILE LA CAUSALE
172100891205     C                   SETON                                        88
172200891205     C                   Z-ADD     1             A
172300891205     C                   Z-ADD     TOTCON        VIMP4
172400891205     C                   MOVE      'D'           VDA4
172500891205     C     '97'          LOOKUP    CSP(A)                                 21
172600891205     C     *IN21         IFEQ      '1'
172700891205     C                   MOVE      'CO'          VSPE4
172800891205     C                   MOVE      CAU(A)        VCAU4
172900940831     C                   SETOFF                                       32
173000891205     C                   Z-ADD     KBNA          VKCC4
173100910726     C                   Z-ADD     VBAN          VKSC4
173200991120     C                   Z-ADD     0             VNRP4
173300991120     C                   Z-ADD     0             VDRP4
173400891205     C*
173500991103     C                   ADD       1             NR5
173600991102     C                   WRITE     CN0W9S5
173700891205     C                   END
173800991104     C                   END
173900021114      * CONTROPARTITA
174000030115     c                   z-add     1             a
174100021114     C     '99'          LOOKUP    CSP(A)                                 21
174200021114     C     *IN21         IFEQ      '1'
174300021114     C                   Z-ADD     totver        VIMP4
174400021114     C                   MOVE      'CA'          VSPE4
174500021114     C                   MOVE      CAU(A)        VCAU4
174600021114     C                   MOVE      'A'           VDA4
174700021114     C* Per le filiali di primo livello ribalto il versato in cassa
174800021125     C* Per le Secondo Livello ribalto il versamento negli inc. da attrib.
174900060403     C                   Z-ADD     VIDCC1        VKCC4
175000060403     C                   Z-ADD     VIDSC1        VKSC4
175100991120     C                   Z-ADD     §FLE          VNRP4
175200991122     C                   Z-ADD     WTODAY        VDRP4
175300891205     C*
175400891206     C                   SETON                                        88
175500991102     C                   ADD       1             NR5
175600991102     C                   WRITE     CN0W9S5
175700021114     C                   END
175800991104     C*
175900891205     C                   ENDSR
176000891205     C/EJECT
176100000608     C**------------------------------------------------------------
176200000000     C** ANNULLAMENTO DISTINTA STAMPATA
176300000608     C**------------------------------------------------------------
176400000000     C     ANNDIS        BEGSR
176500000608     C*
176600910719     C                   DO        TREC          CNT
176700991103     C     CNT           CHAIN     CN0W9S2                            21
176800000608     C*
176900910731     C     VANN          IFNE      'E'
177000910726     C                   MOVEL     VNRA          KNRA
177100910726     C                   MOVE      VNRA1         KNRA
177200910726     C                   Z-ADD     VABI          KABI
177300910726     C                   Z-ADD     VCAB          KCAB
177400940126     C                   SETOFF                                       57
177500000608     C*
177600941103     C     KASS          SETLL     CNASS000                           57
177700000608     C*
177800920206     C     *IN57         DOWEQ     '0'
177900941103     C     KASS          READE     CNASS000                               57
178000910719     C*
178100951027     C     ASSFVE        IFEQ      'V'
178200951027     C     ASSTPI        ANDNE     'M'
178300150313     c  n57              exsr      wrttnass
178400941103     C  N57              DELETE    CNASS000
178500940126     C                   END
178600000608     C*
178700951027     C                   END
178800940126     C*
178900940126     C     SAVNRA        IFNE      VNRA
179000940126     C     SAVNR1        ORNE      VNRA1
179100940126     C     SAVABI        ORNE      VABI
179200940126     C     SAVCAB        ORNE      VCAB
179300000608     C*
179400940126     C                   MOVEL     SAVNRA        KNRA
179500940126     C                   MOVE      SAVNR1        KNRA
179600940126     C                   Z-ADD     SAVABI        KABI
179700940126     C                   Z-ADD     SAVCAB        KCAB
179800940126     C                   SETOFF                                       57
179900000608     C*
180000941103     C     KASS          SETLL     CNASS000                           57
180100000608     C*
180200940126     C     *IN57         DOWEQ     '0'
180300941103     C     KASS          READE     CNASS000                               57
180400941103     C*          KASS      CHAINCNASS000             57
180500940126     C*
180600951027     C     ASSFVE        IFEQ      'V'
180700951027     C     ASSTPI        ANDNE     'M'
180800150313     c  n57              exsr      wrttnass
180900941103     C  N57              DELETE    CNASS000
181000951027     C                   END
181100910719     C*
181200920206     C                   END
181300940126     C                   END
181400910719     C                   ELSE
181500901011     C                   EXSR      PULCON
181600901011     C                   EXSR      CARCON
181700910719     C                   END
181800910722     C                   END
181900911125     C                   SETON                                        69
182000000000     C                   ENDSR
182100000608     C**-------------------------------------------------------------
182200000608     C** SCRIVE L'ARCIHVIO DEI PLICHI DA TRASMETTERE A SICURDATA
182300000608     C**-------------------------------------------------------------
182400000608     C     WRIPLI        BEGSR
182500020417
182600020417     C                   IF        V1Plic <> 0
182700020417
182800000612      *  LEGGE IL SFL PER SCRIVERE IL FILE DEL PLICO DA TRASMETTERE
182900020417     C                   IF        WrkLstRrnS4 > 0
183000020417      *
183100000612     C                   Z-ADD     0             NR4
183200000612     C                   Z-ADD     0             §TCONT
183300000612     C                   Z-ADD     0             NRGPLI            5 0
183400000612     C                   TIME                    FLD14            14 0
183500000612     C                   MOVEL     FLD14         ORAMIN            4 0
183600000612      *
183700000612     C                   DO        *HIVAL
183800000612     C                   ADD       1             NR4
183900000612     C     NR4           CHAIN     CN0W9S4                            63
184000000612      *
184100000612      *  PER FINE FILE ESCE DAL GIRO
184200000612     C     *IN63         IFEQ      *ON
184300000612     C                   LEAVE
184400000612     C                   END
184500000612      *
184600050405      **** Tolta gestione Plichi
184700000612      * IMPOSTA I CAMPI DI SCRITTURA DEL FNPLI00F
184800050405     C**** VNRTGL        IFGT      0
184900050405      ****
185000050405     C****               EXSR      SetFnPli
185100050405     C****               ADD       1             NRGPLI
185200050405     C****               Z-ADD     NRGPLI        PLIRIG
185300050405     C****               Z-ADD     HIMTGL        PLITAG
185400050405     C****               Z-ADD     VNRTGL        PLIQTA
185500050405     C****               Z-ADD     VIDIM         PLITIM
185600050405     C****               EXSR      WrtFnPli
185700050405      ****
185800050405     C****               ENDIF
185900000612      *
186000000612      * AGGIORNA IL SFL
186100000612     C     HIMTGL        COMP      0                                  20        PROT.VIDIM
186200000612     C                   SETON                                        40
186300000612     C                   UPDATE    CN0W9S4
186400000612     C*
186500000612     C                   END
186600020417     C*
186700020417     C                   ENDIF
186800020326
186900050405     **   Tolta gestione dei PLICHI
187000020326     ** Scrittura di una riga aggiuntiva per gli assegni.
187100050405     C********           IF        TotAss > 0
187200050405     C********           EXSR      SetFnPli
187300050405     C********           EVAL      PliRig = *HIVAL
187400050405     C********           EVAL      PliTim = TotAss
187500050405     C********           EXSR      WrtFnPli
187600050405     C********           ENDIF
187700050405      ********
187800020417     C                   ENDIF
187900020326
188000000608     C                   ENDSR
188100020326
188200020326     ***********************************************************************
188300020326     **
188400020326     ** Impostazione dei campi fissi di FNPLI000.
188500050405     **      TOLTA gestione dei PLICHI
188600020326     ***********************************************************************
188700050405     C*****SetFnPli      BEGSR
188800050405      ********
188900050405     C********           CLEAR                   FNPLI000
189000050405     C********           MOVEL     §FLE          PLIFIL
189100050405     C********           Z-ADD     V1PLIC        PLINUM
189200050405     C********           Z-ADD     *DATE         G02DAT
189300050405     C********           MOVE      *ZEROS        G02INV
189400050405     C********           MOVEL     *BLANK        G02ERR
189500050405     C********           CALL      'XSRDA8'
189600050405     C********           PARM                    WLBDAT
189700050405     C********           MOVE      G02INV        PLIDIM
189800050405     C********           MOVE      ORAMIN        PLIHIM
189900050405     C********           MOVEL     V1CDIV        PLIDIV
190000050405      ********
190100050405     C********           ENDSR
190200020326
190300020326     ***********************************************************************
190400020326     **
190500020326     ** Impostazione dei campi fissi di FNPLI000.
190600050405     **      TOLTA gestione dei PLICHI
190700020326     ***********************************************************************
190800050405     C*****WrtFnPli      BEGSR
190900050405      ********
191000050405     C********           ADD       1             NRPLI
191100050405     C********           WRITE     FNPLI000
191200050405      ********
191300050405     C********           ENDSR
191400050405      ********
191500000608     C**-------------------------------------------------------------
191600021118     C** CREAZIONE ARCHIVIO DEI VERSAMENTI GIORNALIERI
191700000608     C**-------------------------------------------------------------
191800021118     C     WRTVRT        BEGSR
191900021118     C                   Z-ADD     §FLE          VRTFLE
192000021118     C                   Z-ADD     DUTCOU        VRTCUC
192100021118     C                   Z-ADD     NUMVER        VRTNDV
192200021118     C                   Z-ADD     AMGUD8        VRTDDV
192300021127     C                   Z-ADD     AMGdop        VRTope
192400021121     C                   Z-ADD     KBNA          vrtkcc
192500021121     C                   Z-ADD     VBAN          vrtksc
192600021118     C                   MOVEL     V5CDIV        VRTVCA
192700021118     C                   Z-ADD     TOTCON        VRTCON
192800021118     C                   Z-ADD     TOTASS        VRTASS
192900021120     c                   clear                   vrtflr
193000021118     C                   WRITE     FNVRT000
193100021118     C                   ENDSR
193200021118     C**-------------------------------------------------------------
193300021118     C** CREAZIONE MOVIMENTI CONTABILI
193400021118     C**-------------------------------------------------------------
193500021118     C     CREMOV        BEGSR
193600000608     C*
193700991103     C                   MOVE      KNMUS         MOVNMU
193800991103     C                   MOVE      KNRAZ         MOVRIF
193900991103     C                   MOVEL     §FLE          MOVRIF
194000991103     C                   Z-ADD     VIDNRG        MOVSBN
194100991103     C                   ADD       1             REGSWD                         *PROGR.REG
194200991103     C                   Z-ADD     REGSWD        MOVSBR
194300991103     C                   MOVEL     'V'           MOVFGE
194400010312     C                   MOVEL     '201'         MOVSOC
194500991103     C                   MOVEL     *BLANKS       MOVFPN
194600991103     C                   MOVEL     §FLE          MOVFPN
194700991103     C                   MOVEL     'D'           MOVTMO
194800991103     C                   MOVEL     'CG'          MOVCTB
194900000000     C                   Z-ADD     VIDNRG        MOVNRG
195000000000     C                   Z-ADD     AMGDRG        MOVDRG
195100991103     C                   MOVE      'COGE'        MOVSRR
195200991103     C                   MOVEL     VCAU4         MOVCOD
195300991103     C                   MOVE      *ZEROS        MOVKCC
195400991103     C                   MOVE      *ZEROS        MOVKSC
195500991103     C                   MOVE      VKCC4         MOVKCC
195600991103     C                   MOVE      VKSC4         MOVKSC
195700021125     C                   MOVE      *ZEROS        MOVCCP
195800021125     C                   MOVE      *ZEROS        MOVSCP
195900991103     C* Dare/Avere
196000991103     C     VDA4          IFEQ      'D'
196100060403     C                   MOVE      VIDCC1        MOVCCP
196200060403     C                   MOVE      VIDSC1        MOVSCP
196300991103     C                   Z-ADD     1             MOVDAR
196400991103     C                   Z-ADD     0             MOVAVE
196500991103     C                   ELSE
196600021125     C                   MOVE      kbna          MOVCCP
196700021125     C                   MOVE      vban          MOVSCP
196800991103     C                   Z-ADD     0             MOVDAR
196900991103     C                   Z-ADD     1             MOVAVE
197000991103     C                   END
197100991103     C* Se la divisa di versamento è diversa da quella
197200991103     C* di conto effettuo la conversione per l'importo
197300991103     C* in ctb.
197400010626     C     V5CDIV        IFEQ      §GEDMC
197500991103     C                   Z-ADD     VIMP4         MOVIMM
197600991103     C                   ELSE
197700991103     C                   CLEAR                   YEURDS
197800991103     C                   MOVEL     V5CDIV        YECDVI
197900991103     C                   Z-ADD     VIMP4         YECIMI
198000010626     C                   MOVEL     §GEDMC        YECDVO
198100991103     C                   MOVEL     'H'           YECTAR
198200991103     C                   CALL      'YEURCO'
198300991103     C                   PARM                    YEURDS
198400991103     C                   Z-ADD     YECIMO        MOVIMM
198500991103     C                   END
198600991103     C                   Z-ADD     VIMP4         MOVIMD
198700991103     C                   MOVEL     V5CDIV        MOVDIV
198800991103     C                   Z-ADD     AMGDRG        MOVDRC
198900020321
199000020321     C                   IF        V4Plic = 0
199100020321     C                   Z-ADD     VIDNDS        MOVNDC
199200020321     C                   ELSE
199300020321     C                   Z-ADD     V4PLIC        MOVNDC
199400020321     C                   ENDIF
199500020321
199600991118     C                   Z-ADD     AMGDOP        MOVDDC
199700991103     C                   MOVE      *BLANKS       MOVSRD
199800000000     C                   Z-ADD     MOVNDC        MOVNRP
199900021126     c                   move      §4occ1        comcc1            4 0
200000021126     C     VKCC4         IFEQ      comcc1                                       INC. ATTRI
200100021126     C                   Z-ADD     VNRP4         MOVNRP                         2° LIV.
200200021126     C                   END
200300991120     C                   MOVE      VDRP4         G02DAT
200400991120     C                   MOVE      *BLANKS       G02ERR
200500991120     C                   CALL      'XSRDA8'
200600991120     C                   PARM                    WLBDAT
200700991120     C                   Z-ADD     G02INV        MOVDRP
200800991103     C                   MOVEL     *BLANKS       MOVSRP
200900010626     C                   MOVEL     §GEDMC        MOVDIP
201000991103     C                   Z-ADD     MOVIMM        MOVIPM
201100991105     C                   Z-ADD     MOVIMM        MOVIPD
201200991103     C                   Z-ADD     0             MOVDSC                         *
201300991103     C                   MOVEL     VDES4         MOVDES
201400991103     C                   MOVEL     *BLANKS       MOVCDC
201500991103     C                   Z-ADD     0             MOVDCA
201600991103     C                   Z-ADD     MOVDRG        MOVDOP
201700010626     C                   Z-ADD     1             MOVCMB
201800030403     C                   ADD       1             NRMOV             7 0
201900991103     C                   WRITE     FNMOV000
202000000000     C                   ENDSR
202100150313     c*__________________________________________________________________
202200150313     c     wrttnass      begsr
202300150313     c*__________________________________________________________________
202400150409     C*                  IF        V4Plic = 0
202500150409     C*                  Z-ADD     VIDNDS        assNDv
202600150409     C*                  ELSE
202700150409     C*                  Z-ADD     V4PLIC        assNDv
202800150409     C*                  ENDIF
202900150313
203000150409     C                   Z-ADD     VIDnrg        assNDv
203100150422     C                   movel     0             assNDv
203200150326     C                   Z-ADD     vass          asscas
203300150330     C                   Z-ADD     vabi          assabi
203400150330     C                   Z-ADD     vcab          asscab
203500150330     C                   MOVEL     vnra          assnra
203600150330     C                   MOVE      vnra1         assnra
203700150330     C                   MOVEL     VTPA          asstpa
203800150330     C                   MOVEL     VTPI          asstpi
203900150330     C                   MOVEL     VTPP          asstpp
204000150330     C                   MOVEL     VPIA          asspia
204100150330     C                   MOVEL     VNOT          assnot
204200150313     C                   Z-ADD     AMGDOP        assDDv
204300150330     c                   move      vdte          dtaeur
204400150330     c                   move      dtaeur        dtaiso
204500150330     c                   move      dtaiso        assdte
204600150313     c                   write     tnass000
204700150313     c                   endsr
204800991103     C*-------------------------------------------------------
204900991103     ** Sottometto trasmissioni
205000991103     **-------------------------------------------------------
205100991103     C     SBMTRA        BEGSR
205200991104     C*
205300020926     C* Richiamo TIBS55R per conoscere librerie/S.Informativi
205400991104     C* da/a cui spedire
205500020926     C                   CLEAR                   TIBS55
205600991104     C                   MOVEL     'L'           I50TLA
205700991104     C                   MOVEL     KNSIF         I50PSI
205800060421     C                   MOVEL     §fle          I50PPO
205900991104     C                   MOVEL     '046'         I50APO
206000020926     C                   CALL      'TIBS55R'
206100020926     C                   PARM                    TIBS55
206200991103     C*
206300991104     C                   MOVEL     '046'         CMZDST                          DEST.:SED
206400991104     C                   MOVEL     'FNMOV00T'    CMZFLD                          DA: >>>
206500020926     C                   EVAL      CmzMbD = 'M' + %EDITC(§Fle:'X') + CmzDst
206600991104     C                   MOVEL     O50PLA        CMZLBD
206700991104     C                   MOVEL     'FNMOV00R'    CMZFLA                          A: >>>
206800020926     C                   MOVEL     CmzMbD        CMZMBA
206900991104     C                   MOVEL     O50ALA        CMZLBA
207000021001     C                   MOVEL     'GAITRA'      CMZLBA
207100021001     C                   MOVE      O50ALA        WQUATR            4
207200021001     C     WQUATR        IFEQ      '101 '
207300021001     C                   MOVE      '201 '        CMZLBA
207400021001     C                   ENDIF
207500991104     C                   MOVE      '0000'        CMZORI                          ORA
207600991104     C                   MOVE      '0000000'     CMZDTI                          DATA
207700991104     C                   MOVE      'N'           CMZURG                          NOTTE
207800991104     C                   MOVE      'A'           CMZADD                          IN ADD
207900991104     C                   MOVE      'S'           CMZCLR                          CLEAR
208000991104     C                   MOVE      'S'           CMZFTR                          DA TRANS.
208100991104     C                   MOVE      *BLANK        CMZERR                          1= ERR.
208200991108     C                   MOVE      'Y029'        CMZAZN                          AZIONE
208300991104     C                   MOVEL     O50ASI        CMZSYF                          SIST.INF.
208400021001     C                   MOVEL     'GAITRA'      CMZSYF
208500021001     C                   MOVE      O50ASI        WQUATR
208600021001     C     WQUATR        IFEQ      '101 '
208700021001     C                   MOVE      '201 '        CMZSYF
208800021001     C                   ENDIF
208900991104     C                   MOVE      NRMOV         CMZNRR                          N.REC.
209000991104     C                   MOVE      *BLANKS       CMZPRR                          FILE PRE-
209100991104     C*
209200991104     C                   CLEAR                   W236
209300020926     C                   CALL      'TIBS53C'
209400991104     C                   PARM                    O50LCL
209500991104     C                   PARM                    CMZDST
209600991104     C                   PARM                    CMZFLD
209700991104     C                   PARM                    CMZMBD
209800991104     C                   PARM                    CMZLBD
209900991104     C                   PARM                    CMZFLA
210000991104     C                   PARM                    CMZMBA
210100991104     C                   PARM                    CMZLBA
210200991104     C                   PARM                    CMZORI
210300991104     C                   PARM                    CMZDTI
210400991104     C                   PARM                    CMZURG
210500991104     C                   PARM                    CMZADD
210600991104     C                   PARM                    CMZCLR
210700991104     C                   PARM                    CMZFTR
210800991104     C                   PARM                    CMZERR
210900991104     C                   PARM                    CMZAZN
211000991104     C                   PARM                    CMZSYF
211100991104     C                   PARM                    CMZNRR
211200991104     C                   PARM                    CMZPRR
211300991104     C                   PARM                    W236            236
211400991103     C*
211500991103     C                   ENDSR
211600000612     C**---------------------------------------------------------------
211700000000     C** STAMPA DISTINTA BANCA
211800000612     C**---------------------------------------------------------------
211900000000     C     STADIS        BEGSR
212000910726     C                   EXCEPT    TESTA
212100910719     C                   DO        TREC          CNT
212200991103     C     CNT           CHAIN     CN0W9S2                            20
212300930927     C                   SETOFF                                       54
212400930927     C*
212500930927     C*  CONTROLLO SE ASSEGNO STESSO/ALTRO ISTITUTO
212600930927     C*
212700910731     C     VANN          IFNE      'E'
212800911127+++  C     VASS          ANDNE     0
212900930927     C*
213000930927     C                   SETON                                        54
213100930927     C*
213200930927     C*  CONTROLLO SE ASSEGNO STESSO/ALTRO ISTITUTO
213300930927     C     ISTABI        IFEQ      VABI
213400930927     C                   SETON                                        53
213500930927     C                   ELSE
213600930927     C                   SETOFF                                       53
213700930927     C                   END
213800991105     C                   Z-ADD     VDTE          GMADAS            8 0
213900000000     C                   Z-ADD     1             A                 3 0
214000910719     C                   MOVEL     VTPA          LKP
214100910719     C                   MOVE      VTPP          LKP
214200910719     C     LKP           LOOKUP    CSP(A)                                 21
214300000000     C  N21              Z-ADD     1             A
214400000000     C                   SETOFF                                       22
214500910719     C     VTPP          IFEQ      'F'
214600991104     C                   ADD       VASS          TOTFUO           12 2
214700000000     C                   ELSE
214800991104     C                   ADD       VASS          TOTPIA           12 2
214900000000     C                   SETON                                        22
215000000000     C                   END
215100000000     C** TOTALE ASSEGNO PER TIPO
215200000000     C                   ADD       1             CONRIG            2 0
215300000000     C     CONRIG        IFGT      50
215400000000     C                   EXCEPT    TESTA
215500000000     C                   Z-ADD     1             CONRIG
215600000000     C                   END
215700991104     C* Controllo se importo con decimali
215800991104     C                   MOVE      VASS          WCOMO2            2 0
215900991104     C     WCOMO2        COMP      0                                      02
216000000000     C                   EXCEPT    DET
216100910719     C                   END
216200910726     C                   END
216300991104     C                   MOVE      TOTPIA        WCOMO2            2 0
216400991104     C     WCOMO2        COMP      0                                      03
216500991104     C                   MOVE      TOTFUO        WCOMO2            2 0
216600991104     C     WCOMO2        COMP      0                                      04
216700991104     C                   MOVE      TOTASS        WCOMO2            2 0
216800991104     C     WCOMO2        COMP      0                                      06
216900000000     C                   EXCEPT    TOT
217000000000     C                   ADD       2             CONRIG
217100000000     C** totale assegni fine elenco
217200000000     C     TOTCON        IFGT      0
217300900518     C*  DETTAGLIO SFL CONTANTE
217400991103     C                   Z-ADD     0             NR4
217500900518     C                   DO        *HIVAL
217600991103     C                   ADD       1             NR4
217700991103     C     NR4           CHAIN     CN0W9S4                            63
217800910731     C     *IN63         IFEQ      '0'
217900900518     C     HIDTIP        ANDEQ     'C'
218000900518     C                   ADD       1             CONRIG            2 0
218100900518     C     CONRIG        IFGT      50
218200900518     C                   EXCEPT    TESTA
218300900518     C                   Z-ADD     1             CONRIG
218400900518     C                   END
218500991105     C* Controllo se importo da stampare a decimali
218600991105     C                   MOVE      VIDIM         WCOMO2
218700991105     C     WCOMO2        COMP      0                                      08
218800060421     c                   eval      stanrt = vnrtgl + vnrman
218900900518     C                   EXCEPT    DETSFL
219000900518     C                   END
219100910731     C  N63              END
219200991105     C* Controllo se importo da stampare a decimali
219300991105     C                   MOVE      TOTCON        WCOMO2
219400991105     C     WCOMO2        COMP      0                                      09
219500000000     C                   EXCEPT    TOT2
219600000000     C                   END
219700000000     C** STAMPA TOTALE CONTANTI
219800000000     C                   DO        20            A
219900000000     C     IMP(A)        IFGT      0
220000930927     C                   SETON                                        53
220100000000     C                   ADD       1             CONRIG
220200000000     C     CONRIG        IFGT      50
220300000000     C                   EXCEPT    TESTA
220400000000     C                   Z-ADD     1             CONRIG
220500000000     C                   END
220600991104     C                   MOVE      IMP(A)        WCOMO2
220700991104     C     WCOMO2        COMP      0                                      07
220800000000     C                   EXCEPT    TOT1
220900000000     C                   END
221000930927     C*
221100930927     C     IM1(A)        IFGT      0
221200930927     C                   SETOFF                                       53
221300930927     C                   ADD       1             CONRIG
221400930927     C     CONRIG        IFGT      50
221500930927     C                   EXCEPT    TESTA
221600930927     C                   Z-ADD     1             CONRIG
221700930927     C                   END
221800991104     C                   MOVE      IM1(A)        WCOMO2
221900991104     C     WCOMO2        COMP      0                                      07
222000930927     C                   EXCEPT    TOT1
222100930927     C                   END
222200930927     C*
222300000000     C                   END
222400991105     C* Controllo se importo da stampare a decimali
222500991105     C                   MOVE      TOTDIS        WCOMO2
222600991105     C     WCOMO2        COMP      0                                      02
222700991105     C                   MOVE      TOTVER        WCOMO2
222800991105     C     WCOMO2        COMP      0                                      04
222900000000     C** STAMPA TOTALI ASSEGNI PER SPECIE
223000000000     C                   EXCEPT    TOT3
223100000000     C** STAMPA TOTALI FINALI
223200000000     C     NOS           TAG
223300000000     C                   ENDSR
223400891205     C/EJECT
223500000000     C**----------------------------------
223600891204     C**  CARICAMENTO TABELLE
223700000000     C**----------------------------------
223800000000     C     CARTAB        BEGSR
223900991102     C*
224000991102     C*§ LEGGO TABELLA '4E'
224100991102     C                   MOVEL     '4E'          COD
224200991102     C     KTAB1         SETLL     TABEL                                  23
224300991102     C  N23              GOTO      NOTAB
224400991102     C                   Z-ADD     0             A
224500991102     C     SUTAB         TAG
224600991102     C     KTAB1         READE     TABEL                                  23
224700991102     C  N23              EXCEPT    RILTAB
224800991102     C   23              GOTO      NOTAB
224900991102     C                   MOVEL     TBLKEY        SPE               2
225000991102     C     SPE           IFNE      *BLANKS
225100991102     C                   ADD       1             A
225200991102     C                   MOVEL     TBLUNI        DS4E
225300991102     C                   MOVEL     SPE           CSP(A)
225400991102     C                   MOVEL     §SPDES        DSP(A)
225500991102     C                   MOVEL     §SPCAU        CAU(A)                         CAUSALE CO
225600991102     C     §SPCA2        IFNE      *BLANKS
225700991102     C                   MOVEL     §SPCA2        CA1(A)                         CAUSALE CO
225800991102     C                   ELSE
225900991102     C                   MOVEL     §SPCAU        CA1(A)                         CAUSALE CO
226000991102     C                   END
226100991102     C                   MOVEL     §SPCAU        OPECOD                         CAUSALE CO
226200991102     C                   MOVEL     'PG'          OPECD1
226300991102     C     KEYOPE        CHAIN     CNOPE000                           80
226400991102     C   80              GOTO      NOTAB
226500991102     C                   MOVEL     CA1(A)        OPECOD                         CAUSALE CO
226600991102     C                   MOVEL     'PG'          OPECD1
226700991102     C     KEYOPE        CHAIN     CNOPE000                           80
226800991102     C   80              GOTO      NOTAB
226900991102     C*
227000991102     C* CARICO CAUSALE PER FUORI PIAZZA SE NON SONO CAUSALI CED
227100991102     C*
227200991102     C                   MOVEL     SPE           CT                1
227300991102     C     CT            IFNE      '9'
227400991102     C                   ADD       1             A
227500991102     C                   MOVEL     SPE           CSP(A)
227600991102     C                   MOVE      'F'           CSP(A)
227700991102     C                   MOVEL     §SPDE1        DSP(A)
227800991102     C                   MOVEL     §SPCA1        CAU(A)                         CAUSALE CO
227900991102     C     §SPCA3        IFNE      *BLANKS
228000991102     C                   MOVEL     §SPCA3        CA1(A)                         CAUSALE CO
228100991102     C                   ELSE
228200991102     C                   MOVEL     §SPCA1        CA1(A)                         CAUSALE CO
228300991102     C                   END
228400991102     C                   MOVEL     §SPCA1        OPECOD                         CAUSALE CO
228500991102     C                   MOVEL     'PG'          OPECD1
228600991102     C     KEYOPE        CHAIN     CNOPE000                           80
228700991102     C   80              GOTO      NOTAB
228800991102     C                   MOVEL     CA1(A)        OPECOD                         CAUSALE CO
228900991102     C                   MOVEL     'PG'          OPECD1
229000991102     C     KEYOPE        CHAIN     CNOPE000                           80
229100991102     C   80              GOTO      NOTAB
229200991102     C                   END
229300991102     C*
229400991102     C*
229500991102     C                   END
229600991102     C                   GOTO      SUTAB
229700000000     C     NOTAB         TAG
229800991102     C*  Aggancio tabella 'GED' x reperire la divisa di conto
229900991102     C                   MOVEL     'GED'         KCOD1
230000991102     C                   MOVEL     *BLANKS       KKEY1
230100991102     C                   MOVEL     '1'           KKEY1
230200991102     C     KTBE          CHAIN     TNTBE01L                           13
230300991102     C     *IN13         IFEQ      '0'
230400991102     C                   MOVEL     TBEUNI        DGED
230500991102     C                   END
230600991102     C* leggo la tabella CV e reperisco la divisa alternativa a
230700991102     C* quella corrente
230800991102     C                   MOVEL     'CV'          COD
230900991102     C     KTAB1         CHAIN     TABEL00F                           50
231000991102     C     *IN50         DOWEQ     '0'
231100991102     C                   MOVEL     TBLUNI        DSCV
231200991103     C                   MOVEL     TBLKEY        WDIV              3
231300991102     C     §CVDVA        IFEQ      'S'
231400010626     C     §GEDCR        ANDNE     WDIV
231500991103     C                   MOVEL     WDIV          DIVALT            3
231600011220     C**                 Z-ADD     §CVCMB        CMBALT           13 6
231700991209     C                   Z-ADD     §CVDEC        DECALT            2 0
231800991102     C                   END
231900010626     C     §GEDCR        IFEQ      WDIV
232000991209     C                   Z-ADD     §CVDEC        DECDIV            2 0
232100991209     C                   END
232200991102     C     KTAB1         READE     TABEL00F                               50
232300991102     C                   END
232400991102     C*
232500000000     C                   ENDSR
232600891205     C*----------------------------------------------------*
232700911115     C* CARICAMENTO FILIALI VALIDE
232800911115     C*----------------------------------------------------*
232900911115     C     CARTB         BEGSR
233000991102     C*
233100020507     C                   MOVEL     '£1'          D06COD
233200060421     C                   MOVEL     simfel        D06KEY
233300020507     C                   MOVEL     TR06DS        KPJBU
233400020507     C                   CALL      'TRUL06R'
233500020507     C                   PARM                    KPJBA
233600020507     C                   MOVEL     KPJBU         TR06DS
233700020507     C                   MOVEA     LIN           fila
233800021115     c*
233900911115     C                   ENDSR
234000150108     C*----------------------------------------------------*
234100150108     C*  Verifica se assegni esclusi devono essere segnalati
234200150108     C*----------------------------------------------------*
234300150108     C     VerEsclusi    BEGSR                                                  *
234400150109     c                   move      vdte          dataeur
234500150109     c                   move      dataeur       dataisom
234600150108     c     dataiso       subdur    dataisom      diffe:*d          3 0
234700150129     c                   if        diffe > 6
234800150108     c                   z-add     1             xy
234900150108     c     vuoto         lookup    lista(xy)                              99
235000150109     c   99              eval      lista(xy) = 'Assegno: ' + vnra + vnra1
235100150112     c                             + 'data ' +%editc(vdte:'Y') + ' ' +
235200150112     c                             'ABI: ' + %editc(vabi:'X') + ' ' +
235300150112     c                             'CAB: '+  %editc(vcab:'X') + ' ' +
235400150112     c                             'Importo: ' + %editc(vass:'2') + ' ' + vdes
235500150109     c  n99              clear                   lista(10)
235600150108     c  n99              eval      lista(10) = 'Presenti altri assegni oltre 10'
235700150108     c                   endif
235800150108     c
235900150108     C                   endsr
236000150108     C*----------------------------------------------------*
236100150108     C*  invia mail di segnalazione assegni non versati
236200150108     C*----------------------------------------------------*
236300150108     C     Inviamail     BEGSR                                                  *
236400150112     C                   eval      wrkEml= 'gestione.clienti@brt.it'
236500150108
236600150108     C                   eval      wrkOgg='Segnalazione assegni non versati '+
236700150108     C                             'Filiale ' + %EDITC(§Fle:'X')
236800150108     C                   eval      wrkMsg='Attenzione il versamento effettu' +
236900150108     C                             'ato dal profilo ' + knmus +' ha escluso '+
237000150204     C                             'assegni da oltre 6 giorni' +':/N'+
237100150108     C                             lista(1) +':/N'+
237200150108     C                             lista(2) +':/N'+
237300150108     C                             lista(3) +':/N'+
237400150108     C                             lista(4) +':/N'+
237500150108     C                             lista(5) +':/N'+
237600150108     C                             lista(6) +':/N'+
237700150108     C                             lista(7) +':/N'+
237800150108     C                             lista(8) +':/N'+
237900150108     C                             lista(9) +':/N'+
238000150108     C                             lista(10)
238100150108     C*
238200150108     C                   call(e)   'TIS701C1'
238300150108     C                   parm                    wrkEml
238400150108     C                   parm                    wrkEmcc
238500150108     C                   parm                    wrkOgg
238600150108     C                   parm                    wrkMsg
238700150108     C                   parm                    wrkesit
238800150108     C                   endsr
238900940831     C*----------------------------------------------------*
239000940831     C*  OPERAZIONI INIZIALI                               *
239100940831     C*----------------------------------------------------*
239200940831     C     *INZSR        BEGSR                                                  *
239300940831     C*
239400940831     C     *ENTRY        PLIST
239500940831     C                   PARM                    KPJBA
239600000608      *
239700000608      * -----------------
239800000608      * IMPOSTA L'INDICATORE (66) PER CONTROLLARE L'AZIONE
239900000608      *  ORIGINALE O QUELLA NUOVA PER GESTIRE DIRETTAMENTE
240000000608      *   LA BANCA PER CONTANTE PER IL NUOVO GIRO POSTE.
240100000608     C     KCDAZ         COMP      '%0W9'                             6666
240200000608      * -----------------
240300000608      *
240400991104     C* Eseguo la Open dei file
240500021114     C                   OPEN      CNASS02L                             10
240600991104     C   10              GOTO      FINE
240700021121     C* Richiamo XPARUT
240800021121     C                   Z-ADD     1             CODUT
240900021121     C                   CALL      'X§PARUT'
241000021121     C                   PARM                    UTEDSE
241100021121     C                   MOVEL     RAGUT         RSUT2            25
241200170607
241300021118     c     *dtaara       define    §azute        azuteds
241400021118     c     *dtaara       define    §datiute      ddatiute
241500021118     C                   in(E)     *dtaara
241600021118     C                   IF        %Error  or  RSUT = *blanks
241700021118     C                   call      'TIBS34R'
241800021118     C                   parm                    Tibs34Ds
241900021118     C                   in        *dtaara
242000021118     c                   ENDIF
242100170607
242200050111     C*§ CONTROLLO SE SIAMO IN FILIALE
242300050111     c                   move      dutpou        §fle              3 0
242400060426     c                   if        dutlpo = 'S' or dutlpo = ' '
242500060426     c                   seton                                        19
242600060426     c                   end
242700060526     c                   if        dutlpo = '2'
242800060526     c                   seton                                        01
242900060526     c                   end
243000040921     C     dutpou        CHAIN     AZORG01L
243100941103     C*§§
243200941103     C                   TIME                    WHHUDT           14 0
243300991102     C                   MOVE      WHHUDT        G02DAT
243400991102     C                   MOVE      *ZEROS        G02INV
243500991102     C                   MOVEL     *BLANK        G02ERR
243600941103     C                   CALL      'XSRDA8'
243700991103     C                   PARM                    WLBDAT
243800991102     C                   Z-ADD     G02INV        AMGUD8            8 0
243900991102     C                   Z-ADD     G02DAT        GMAUD8            8 0
244000991103     C                   Z-ADD     0             REGSWD            5 0          *AZZER.RIG
244100150109     c                   move      amgud8        dataiso
244200940831     C*-----
244300940831     C                   SETON                                        05        COND.INTES
244400940831     C*-                                                   *
244500940831     C*--- RICERCA CAPOCONTI------------------------------------------*
244600100521     C                   DO        50            X
244700940831     C                   MOVE      TCU(X)        TCUDS
244800940831     C     F56           CABNE     'CG'          END1
244900940831     C     F4            IFEQ      '6'
245000940831     C                   Z-ADD     KCU(X)        KBNA              4 0
245100940831     C                   END
245200940831     C     F4            IFEQ      '4'
245300940831     C                   Z-ADD     KCU(X)        KCAS              4 0
245400940831     C                   END
245500940831     C     END1          TAG
245600940831     C                   END
245700940831     C*---------------------------------------------------------------*
245800940831     C*--- RICERCA RAG.SOCIALE CLIENTE     'CNACO00F       *
245900940831     C     KEYACO        KLIST                                                  *
246000940831     C                   KFLD                    CODUT                          *
246100940831     C                   KFLD                    KCC                            *
246200940831     C                   KFLD                    KSC                            *
246300940831     C                   Z-ADD     KBNA          KCC                            *
246400940831     C     KBAN          KLIST                                                  *
246500940831     C                   KFLD                    CODUT                          *
246600940831     C                   KFLD                    KBNA                           *
246700940831     C                   KFLD                    KSC
246800940831     C     KTAB          KLIST
246900940831     C                   KFLD                    CODUT
247000940831     C                   KFLD                    COD
247100940831     C                   KFLD                    KEY
247200940831     C     KEYOPE        KLIST                                                  * CAUSALE
247300940831     C                   KFLD                    CODUT                          *
247400940831     C                   KFLD                    OPECD1                         *
247500940831     C                   KFLD                    OPECOD                         *
247600991102     C     KTAB1         KLIST
247700940831     C                   KFLD                    CODUT
247800940831     C                   KFLD                    COD
247900991102     C     KTBE          KLIST
248000991102     C                   KFLD                    KCOD1
248100991102     C                   KFLD                    KKEY1
248200020729     C     KTBE1         KLIST
248300020729     C                   KFLD                    tbecod
248400020729     C                   KFLD                    tbeke1
248500020729     C                   KFLD                    tbeke2
248600940831     C     KABI§         KLIST
248700940831     C                   KFLD                    KABI              5 0
248800940831     C                   KFLD                    KCAB              5 0
248900940831     C     KASS          KLIST
249000940831     C                   KFLD                    §FLE
249100940831     C                   KFLD                    KNRA             10
249200940831     C                   KFLD                    KABI
249300940831     C                   KFLD                    KCAB
249400021120      *
249500021120     C     Kqig          KLIST
249600021120     C                   KFLD                    qigann
249700021120     C                   KFLD                    §fle
249800021120     C                   KFLD                    amgud8
249900021120     c                   clear                   qigann
250000940831     C*
250100050405     C***** Tolta gestione dei PLICHI
250200020103      * con l'euro è necessario testare solo senza la divisa
250300020103      * per dare l'errore di Plico già utilizzato
250400050405     C**** KPLIN         KLIST
250500050405     C*******            KFLD                    KPOC
250600050405     C*******            KFLD                    KPLI
250700000612     C*
250800940831     C     *LIKE         DEFINE    ACOKCC        KCC
250900940831     C     *LIKE         DEFINE    ACOKSC        KSC
251000940831     C     *LIKE         DEFINE    TBLCOD        COD
251100940831     C     *LIKE         DEFINE    TBLKEY        KEY
251200991102     C     *LIKE         DEFINE    TBECOD        KCOD1
251300991102     C     *LIKE         DEFINE    TBEKE1        KKEY1
251400940831     C     *LIKE         DEFINE    TOTCON        §TCONT
251500940831     C     *LIKE         DEFINE    INDABI        ISTABI
251600940831     C     *LIKE         DEFINE    INDCAB        ISTCAB
251700991120     C* REPERISCO DATA DEL GIORNO
251800991120     C                   TIME                    WHHUDT           14 0
251900991120     C                   MOVEL     WHHUDT        ORA               6 0
252000991120     C                   MOVE      WHHUDT        G02DAT
252100991120     C                   MOVE      *ZEROS        G02INV
252200991120     C                   MOVEL     *BLANK        G02ERR
252300991120     C                   CALL      'XSRDA8'
252400991120     C                   PARM                    WLBDAT
252500991120     C                   Z-ADD     G02INV        OGGI              8 0
252600991122     C                   Z-ADD     G02DAT        WTODAY            8 0
252700980130     C* Richiamo pgm per controllo modificabilità tabella
252800980130     C                   CLEAR                   TRUL12
252900980130     C                   MOVEL     'L'           I12TLA
253000980130     C                   MOVEL     KNSIF         I12NSI
253100980130     C                   MOVEL     '4D'          I12COD
253200980130     C                   CALL      'TRUL12R'
253300980130     C                   PARM                    TRUL12
253400980130     C     O12ERR        IFEQ      '1'
253500980130     C                   GOTO      FINE
253600980130     C                   END
253700980130     C*----------------------------------------------------*
253800940831     C                   MOVE      *ZEROS        AMGDRG
253900940831     C                   EXSR      CARTAB
254000940831     C                   EXSR      CARTB
254100991102     C   80              MOVEL     '1'           *INKC
254200010626     C                   MOVEL     §GEDCR        V1CDIV
254300991102     C*
254400991103     C* 2 GG PRIMA LA DATA DEL GIORNO CASSA SEGNALO
254500991104     C                   Z-ADD     0             NRMOV             7 0
254600000612     C                   Z-ADD     0             NRPLI             7 0
254700991103     C                   Z-ADD     0             D52DFI                         *
254800991103     C                   Z-ADD     1             D52GIO                         *
254900991103     C                   MOVE      AMGUD8        D52DIN                         *
255000991103     C                   MOVE      '-'           D52SGN                         *
255100991103     C                   CALL      'TRUL52R'
255200991103     C                   PARM                    TRUL52
255300991103     C                   MOVE      D52DFI        DAT1GG            8 0          *
255400940831     C                   ENDSR
255500020418
255600020418     ***********************************************************************
255700000000     ODISBAN1   E            TESTA            02
255800891205     O                       RSUT2
255900000000     O                                           60 '**---------------------'
256000000000     O                                              '------------**'
256100020418
256200000000     O          E            TESTA            03
256300170517     O                       brt_via
256400060421     O                                           60 '**  DISTINTA  VERSAMENT'
256500060421     O                                              'O IN BANCA  **'
256600060421     O*              66                          60 '**      DISTINTA  VERSA'
256700060421     O*              66                             'MENTO       **'
256800000000     O                       PAGE          Z    132
256900000000     O                                          128 'PAG.'
257000991103     O                                          120 'CNC0W9R'
257100020418
257200000000     O          E            TESTA            04
257300170517     O                       BRT_CIT
257400000000     O                                           60 '**---------------------'
257500000000     O                                              '------------**'
257600020418
257700000000     O          E            TESTA            05
257800000000     O                                              'Versamento a mezzo Banca'
257900000000     O                       KCC           Z   +  1
258000910726     O                       VBAN          Z   +  1
258100000000     O                       VIDBAN            +  1
258200891205     O                                         +  1 'C/C'
258300891205     O                       INDCCB            +  1
258400991103     O                                         +  1 'Divisa Versamento:'
258500991103     O                       V1CDIV            +  1
258600020418
258700020418     O          E            TESTA          1 06
258800000000     O                                              'Distinta N.'
258900000000     O                       VIDNDS        Z   +  1
259000000000     O                                         +  1 'del'
259100991103     O                       VIDDDS            +  1 '  /  /    '
259200020418     O                                           98 'Plico n.'
259300020418     O                       V1PLIC        Z   +  1
259400020418
259500000616     O          E   N66      TESTA            08
259600000616     O                       SC(1)               66
259700000616     O                       SC(2)              132
259800020418
259900000616     O          E   N66      TESTA            09
260000000616     O                       SC(3)               66
260100000616     O                       SC(4)              132
260200020418
260300000616     O          E   N66      TESTA            10
260400000616     O                       SC(5)               66
260500000616     O                       SC(6)              132
260600020418
260700000616     O          E   N66      TESTA            11 12
260800000616     O                       SC(7)               66
260900000616     O                       SC(8)              132
261000020418     **---------------------------------------------------------------------
261100000616     O          E   N66      DET         1
261200910801     O                       VTPA                 3
261300910719     O                       VTPP                 4
261400910719     O                       VNRA                13
261500910719     O                       VNRA1               18
261600991105     O                       GMADAS              29 '  /  /    '
261700991104     O                       VDES                50
261800991104     O                       VPIA                62
261900991104     O               22      VASS          2     77
262000991104     O               22 02                       77 '   '
262100991104     O              N22      VASS          2     92
262200991104     O              N22 02                       92 '   '
262300991104     O               53 54                      102 'STESSO'
262400991104     O              N53 54                      102 'ALTRO '
262500020418     **---------------------------------------------------------------------
262600000616     O          E   N66      TOT         2
262700000000     O                       TOTPIA        2     77
262800991104     O                  03                       77 '   '
262900000000     O                       TOTFUO        2     92
263000991104     O                  04                       92 '   '
263100000000     O                                         +  1 '- Totale Assegni -'
263200000000     O                       TOTASS        2   +  1
263300991104     O                  06                      132 '   '
263400020418
263500000616     O          E   N66      TOT         0
263600000000     O                       TOTPIA        2     77
263700991104     O                  03                       77 '   '
263800000000     O                       TOTFUO        2     92
263900991104     O                  04                       92 '   '
264000000000     O                                         +  1 '- Totale Assegni -'
264100991104     O                       TOTASS        2    132
264200991104     O                  06                      132 '   '
264300020418
264400000616     O          E   N66      TOT         0  2
264500000000     O                       TOTPIA        2     77
264600991104     O                  03                       77 '   '
264700000000     O                       TOTFUO        2     92
264800991104     O                  04                       92 '   '
264900000000     O                                         +  1 '- Totale Assegni -'
265000991104     O                       TOTASS        2    132
265100991104     O                  06                      132 '   '
265200020418     **---------------------------------------------------------------------
265300000616     O          E   N66      TOT1        1
265400000000     O                       CSP(A)               3
265500000000     O                                         +  1 '->'
265600000000     O                       DSP(A)            +  1
265700991105     O               53                          48 'STESSO ISTITUTO'
265800991105     O              N53                          48 'ALTRO  ISTITUTO'
265900991104     O               53      IMP(A)        2     68
266000991104     O              N53      IM1(A)        2     68
266100991104     O                  07                       68 '   '
266200991104     O                       V1CDIV              72
266300020418     **---------------------------------------------------------------------
266400900518     O          E            DETSFL         1
266500900518     O                                         +  1 'Nr.'
266600060421     O                       stanrt        Z   +  1
266700900518     O                       VDESCR            +  1
266800991104     O                       VIDIM         2     68
266900991104     O                  08                       68 '   '
267000020418     **---------------------------------------------------------------------
267100900518     O          E            TOT2        1  2
267200000000     O                                         +  1 'Totale Contante'
267300991104     O                       TOTCON        2     68
267400991104     O                  09                       68 '   '
267500020418     **---------------------------------------------------------------------
267600000000     O          E            TOT3        1
267700000000     O                                              '------------------------'
267800000000     O                                              '------------------------'
267900930927     O                                              '------------------------'
268000020418
268100000000     O          E            TOT3        1
268200000000     O                                         +  1 'Totale Distinta'
268300991104     O                       TOTDIS        2     68
268400991104     O                  02                       68 '   '
268500991104     O                       V1CDIV              72
268600991104     O                  03                       68 '   '
268700020418
268800000000     O          E            TOT3        2
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
270000000000     O          E            TOT3        0
270100000000     O                                         +  1 'Importo da Versare'
270200991104     O                       TOTVER        2     68
270300991104     O                  04                       68 '   '
270400991104     O                       V1CDIV              72
270500020418
270600000000     O          E            TOT3             65
270700000000     O                                          130 'COPIA USO BANCA'
270800020418     ***********************************************************************
270900000000     ODISBAN3   E            TESTA            02
271000891205     O                       RSUT2
271100000000     O                                           60 '**---------------------'
271200000000     O                                              '------------**'
271300020418
271400000000     O          E            TESTA            03
271500170517     O                       BRT_VIA
271600060421     O                                           60 '**  DISTINTA  VERSAMENT'
271700060421     O                                              'O IN BANCA  **'
271800060421     O*              66                          60 '**      DISTINTA  VERSA'
271900060421     O*              66                             'MENTO       **'
272000000000     O                       PAGE3         Z    132
272100000000     O                                          128 'PAG.'
272200991103     O                                          117 'CNC0W9R'
272300020418
272400000000     O          E            TESTA            04
272500170517     O                       BRT_CIT
272600000000     O                                           60 '**---------------------'
272700000000     O                                              '------------**'
272800020418
272900000000     O          E            TESTA            05
273000000000     O                                              'Versamento a mezzo Banca'
273100000000     O                       KCC           Z   +  1
273200910726     O                       VBAN          Z   +  1
273300000000     O                       VIDBAN            +  1
273400891205     O                                         +  1 'C/C'
273500891205     O                       INDCCB            +  1
273600991104     O                                         +  1 'Divisa Versamento:'
273700991104     O                       V1CDIV            +  1
273800020418
273900020418     O          E            TESTA          1 06
274000000000     O                                              'Distinta N.'
274100000000     O                       VIDNDS        Z   +  1
274200000000     O                                         +  1 'del'
274300991103     O                       VIDDDS            +  1 '  /  /    '
274400020418     O                                           98 'Plico n.'
274500020418     O                       V1PLIC        Z   +  1
274600020418
274700000616     O          E   N66      TESTA            08
274800000000     O                       SC(1)               66
274900000000     O                       SC(2)              132
275000020418
275100000616     O          E   N66      TESTA            09
275200000000     O                       SC(3)               66
275300000000     O                       SC(4)              132
275400020418
275500000616     O          E   N66      TESTA            10
275600000000     O                       SC(5)               66
275700000000     O                       SC(6)              132
275800020418
275900000616     O          E   N66      TESTA            11 12
276000000000     O                       SC(7)               66
276100000000     O                       SC(8)              132
276200020321
276300020418     **---------------------------------------------------------------------
276400000616     O          E   N66      DET         1
276500910801     O                       VTPA                 3
276600910719     O                       VTPP                 4
276700910719     O                       VNRA                13
276800910719     O                       VNRA1               18
276900991105     O                       GMADAS              29 '  /  /    '
277000991104     O                       VDES                50
277100991104     O                       VPIA                62
277200991104     O               22      VASS          2     77
277300991104     O               22 02                       77 '   '
277400991104     O              N22      VASS          2     92
277500991104     O              N22 02                       92 '   '
277600991104     O               53 54                      102 'STESSO'
277700991104     O              N53 54                      102 'ALTRO '
277800930927     O                       VNOT               132
277900020418     **---------------------------------------------------------------------
278000000616     O          E   N66      TOT         2
278100000000     O                       TOTPIA        2     77
278200991104     O                  03                       77 '   '
278300000000     O                       TOTFUO        2     92
278400991104     O                  04                       92 '   '
278500911127     O                                         +  1 '- Totale Assegni -'
278600991104     O                       TOTASS        2    132
278700991104     O                  06                      132 '   '
278800020418
278900000616     O          E   N66      TOT         0
279000000000     O                       TOTPIA        2     77
279100991104     O                  03                       77 '   '
279200000000     O                       TOTFUO        2     92
279300991104     O                  04                       92 '   '
279400000000     O                                         +  1 '- Totale Assegni -'
279500991104     O                       TOTASS        2    132
279600991104     O                  06                      132 '   '
279700020418
279800000616     O          E   N66      TOT         0  2
279900000000     O                       TOTPIA        2     77
280000991104     O                  03                       77 '   '
280100000000     O                       TOTFUO        2     92
280200991104     O                  04                       92 '   '
280300000000     O                                         +  1 '- Totale Assegni -'
280400991104     O                       TOTASS        2    132
280500991104     O                  06                      132 '   '
280600020418     **---------------------------------------------------------------------
280700000616     O          E   N66      TOT1        1
280800000000     O                       CSP(A)               3
280900000000     O                                         +  1 '->'
281000000000     O                       DSP(A)            +  1
281100991105     O               53                          48 'STESSO ISTITUTO'
281200991105     O              N53                          48 'ALTRO  ISTITUTO'
281300991104     O               53      IMP(A)        2     68
281400991104     O              N53      IM1(A)        2     68
281500991104     O                  07                       68 '   '
281600991104     O                       V1CDIV              72
281700020418     **---------------------------------------------------------------------
281800900518     O          E            DETSFL         1
281900900518     O                                         +  1 'Nr.'
282000060421     O                       stanrt        Z   +  1
282100900518     O                       VDESCR            +  1
282200991104     O                       VIDIM         2     68
282300991104     O                  08                       68 '   '
282400020418     **---------------------------------------------------------------------
282500900518     O          E            TOT2        1  2
282600000000     O                                         +  1 'Totale Contante'
282700991104     O                       TOTCON        2     68
282800991104     O                  09                       68 '   '
282900991104     O                       V1CDIV              72
283000020418     **---------------------------------------------------------------------
283100000000     O          E            TOT3        1
283200000000     O                                              '------------------------'
283300000000     O                                              '------------------------'
283400930927     O                                              '------------------------'
283500020418
283600000000     O          E            TOT3        1
283700000000     O                                         +  1 'Totale Distinta'
283800991104     O                       TOTDIS        2     68
283900991104     O                  02                       68 '   '
284000991105     O                       V1CDIV              72
284100020418
284200000000     O          E            TOT3        2
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
285400000000     O          E            TOT3        0
285500000000     O                                         +  1 'Importo da Versare'
285600991104     O                       TOTVER        2     68
285700991104     O                  04                       68 '   '
285800991104     O                       V1CDIV              72
285900020418
286000000000     O          E            TOT3        3
286100000000     O                                         +  1 'N.Registrazione'
286200000000     O                       VIDNRG        Z   +  1
286300000000     O                                         +  1 'del'
286400991103     O                       VIDDRG            +  1 '  /  /    '
286500930225     O                                         +  2 'Operaz. del'
286600991118     O                       VDTOPE            +  1 '  /  /    '
286700020418
286800000000     O          E            TOT3             65
286900000000     O                                          130 'COPIA USO INTERNO'
287000020418     ***********************************************************************
287100991103     OTABEL     E            RILTAB
287200020418     ***********************************************************************
287300991104**
287400991104------+-----------+----------+--------------------+-----------+---
287500930927-----------+--------------+----------+---------------------------+
287600000000SPECIE    NUMERO     DATA    BANCA TRASSATA        PIAZZA       IM
287700930927PORTO ASS.   IMPORTO ASS.   ISTITUTO   N O T E                   '
287800000000          ASSEGNO  ASSEGNO                         PAGAMENTO    SU
287900930927 PIAZZA      FUORI PIAZZA                                        '
288000991105------+-----------+----------+--------------------+-----------+---
288100930927-----------+--------------+----------+---------------------------+
288200951027** SCHIERA ERRORI
288300991102Già inserito assegno. Eseguire chius.operatore/presa in carico mandato d'introto    01
288400991102Già inserito assegno in questa distinta.                                            02
288500991102Già inserito assegno con questi riferimenti intestato al mittente.                  03
288600991102ERRORE !! Manca il numeratore della distinta !!!!                                   04
288700991102Divisa di versamento non valida                                                     05
288800991103Precedente trasmissione di  movimenti contabili terminata in modo anomalo           06
288900000609Impostare un nr.PLICO corretto                                                      07
289000000612Con questo n°Plico sono stati già inseriti altri dati! Usare un altro Numero.       08
289100060403ERRORE! Non è possibile continuare. Esiste già quadratura giornaliera.              09
289200150313Codice Banca obbligatorio                                                           10
289300150319Codice Banca errato                                                                 11
289400991104**
289500020926CHKOBJ OBJ(FNMOV00T) OBJTYPE(*FILE) MBR(M000000)
289600020926ADDPFM FILE(FNMOV00T)               MBR(M000000)
289700020926OVRDBF FILE(FNMOV00T)               MBR(M000000)
289800020926ALCOBJ OBJ((FNMOV00T *FILE *EXCL        M000000))
289900020926DLCOBJ OBJ((FNMOV00T *FILE *EXCL        M000000))
