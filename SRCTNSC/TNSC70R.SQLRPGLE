000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200900606?     *--------------------------------------------------------------*
000300941028      * TNSC70R                                                      *
000400900606      *                                                              *
000500910617      *        CONVALIDA RICEVIM. ASSEGNI DIRETTI                    *
000600910521      * 20 -INDICATORE DI COMODO                                     *
000700910701      * 30 -RICH. PER RAGGR. ARRIVO                                  *
000800910701      * 31 -RICH. PER RAGG.ARRIVO + NR.DIST                          *
000900910521      * 40 -ERRORE GENERICO VIDEO                                    *
001000910521      * 41 - 60 ERRORRI SINGOLI CAMPI                                *
001100040907      * 61 -assegno senza riferimenti                                *
001200910521      * 70 71 - GESTIONE SFL                                         *
001300910617      * 72    - GESTIONE SFL SFLNXTCHG                               *
001400900606?     *--------------------------------------------------------------*
001500941028     FTNSC70D   CF   E             WORKSTN
001600941028     F                                     SFILE(SC70DSF:NRR)
001700941031     FTNCSB01L  UF   E           K DISK
001800941031     FAZORG01L  IF   E           K DISK    USROPN
001900941031     FTABEL00F  IF   E           K DISK    USROPN
002000011018     FTnTbe01L  IF   E           K DISK
002100011018     F                                     USROPN
002200900515      *
002300900606     D FLU             S              1    DIM(32)
002400900606     D OPZ             S              1    DIM(10)
002500950119     D TCV             S              3    DIM(30)
002600950119     D DCV             S             89    DIM(30)
002700971118     D FIL             S              3  0 DIM(500)
002800971118     D DFI             S             20    DIM(500)
002900060515     d tibs55ds      E DS                  prefix(£)
003000900515      *
003100900514     D KPJBA         E DS
003200900514     D  LIBSYS                92    100
003300900516      *
003400900517     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003500000000     D  TCU                  398    697
003600000000     D                                     DIM(50)
003700000000     D  KCU                  698    847P 0
003800000000     D                                     DIM(50)
003900000000     D                                     PACKEVEN
004000000000     D  DTU                  848    895P 0
004100000000     D                                     DIM(12)
004200000000     D                                     PACKEVEN
004300010305     D*FTRUL15    E DSTRUL15DS
004400900606      *
004500011018     D DGed          E DS
004600011018     D                                     INZ
004700941031     D DSCV          E DS
004800941028     D TNSC71        E DS
004900990729     D TNSB50        E DS                  EXTNAME(TNSB50DS)
005000060515     d param           ds
005100060515     D kaas                           4s 0
005200060515     D klnp                           3s 0
005300060515     D knrs                           2s 0
005400060515     D knsp                           7s 0
005500060519     D parca                          1
005600060519     D paresito                       1
005700060515     D sifda                         10
005800060515     D sifa                          10
005900900606     D TCUDS           DS
006000900606     D  F1                     1      1
006100900606     D  F3                     3      3
006200900606     D  F2                     2      2
006300900606     D  F4                     4      4
006400900606     D  F56                    5      6
006500900524      *
006600910620     D                 DS
006700910617     D  CSBRGF                 1      3  0
006800910617     D  CSBNDT                 4     10  0
006900941031     D  CSBDDC                11     18  0
007000941031     D  DIST                   1     18
007100900515      *
007200910620     D                 DS
007300910617     D  PRERGF                 1      3  0
007400910617     D  PRENDT                 4     10  0
007500941031     D  PREDDC                11     18  0
007600941031     D  DISTPR                 1     18
007700900515      *
007800941031     D WLBDA8          DS
007900941031     D  G08DAT                 1      8  0
008000941031     D  G08INV                 9     16  0
008100941031     D  G08ERR                17     17
008200941031     D  G08TGI                18     22  0
008300911001     D PARDIR          DS
008400911001     D  RGF                    1      3
008500911001     D  NDS                    4     10
008600941031     D                SDS
008700941031     D  §PGM                   1     10
008800150526     d NotUsed         S             16A
008900150526     d ReturnSize      S              9B 0
009000150526     d SizeList        S              9B 0
009100150526     d WrkSort         S              1
009200150526     d*---
009300150526     d nrr             S              4  0
009400150526     d stmrec1         S              4  0
009500150526     d*---
009600150526     d MaxKey          C                   2
009700150526     d Ascendente      C                   1
009800150526     d Discendente     C                   2
009900150526     d Carattere       C                   6
010000150526     d Put             C                   1
010100150526     d EndPut          C                   2
010200150526     d Get             C                   3
010300150526     d Numerico        C                   8
010400150526     d*---
010500150526     d SflRcd          DS
010600150529     d  sddch
010700150526     d  srgf
010800150529     d  sddc
010900150526     d  IND60
011000150526     d  IND61
011100150526     d  IND62
011200150526     d  IND63
011300150526     d  SSCE
011400150526     d  sdrgf
011500150526     d  sndt
011600150526     d  snrass
011700150526     d  sdivis
011800150526     d  stdt
011900150526     d  snrass1
012000150526     d  sdivis1
012100150526     d  stdt1
012200150526     d  selected                      1A
012300150526
012400150526      /COPY QSYSINC/QRPGLESRC,QLGSORT
012500150526     d QLGKL                         16    DIM(MaxKey)
012600150526     d  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
012700150526     d  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
012800150526     d  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
012900150526     d  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
013000150526
013100150526      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
013200150526      /COPY QSYSINC/QRPGLESRC,QUSEC
013300150526     D*___________________________________________________________________
013400900524      *
013500910521     C                   EXSR      DEFCAM
013600910521      *          =============
013700910521     C     INIZIO        TAG
013800910521      *          =============
013900910521     C                   EXSR      PULIZ
014000910521      *
014100910521      *          =============
014200910521     C     FMT01         TAG
014300910521      *          =============
014400910521      *
014500941028     C                   WRITE     SC70D00
014600941028     C                   EXFMT     SC70D01
014700150526     c                   clear                   decf11
014800911001     C                   SETOFF                                       404243
014900971216     C                   SETOFF                                       44
015000941031     C   KC              GOTO      FINE
015100941031     C* INSERIMENTO DIST. MANAULE - CMD10
015200941031     C   KJ              DO
015300911001     C*  OBBLIGO RGF E NR DIST.
015400911001     C     V1RGF         IFEQ      *ZERO
015500911001     C                   SETON                                          4042
015600911001     C                   END
015700911001     C     V1NDT         IFEQ      *ZERO
015800911001     C                   SETON                                          4043
015900911001     C                   END
016000911001     C     *IN40         CABEQ     '1'           FMT01
016100971216      *
016200911001     C                   MOVEL     §PGM          PARPGM
016300911001     C                   MOVEL     *BLANKS       PARERR
016400990729     C                   MOVE      GMAUDT        V1DTA
016500911001     C                   MOVEL     *ZEROS        V1STA
016600911001     C                   MOVEL     ' '           §TPI
016700911001     C                   MOVE      V1RGF         PARRGF
016800911001     C                   MOVE      V1NDT         PARNDT
016900941031     C                   MOVE      AMGUDT        PARDDC
017000911001     C                   MOVEL     ' '           PARTPI
017100911001     C                   MOVEL     '13'          PARCMD
017200911001     C                   MOVE      V1STA         PARSTA
017300941128     C                   MOVE      KPJBU         KPJBUS
017400941128     C                   MOVEL     *BLANK        KPJBU
017500941028     C                   MOVEL     TNSC71        KPJBU
017600941028     C                   CALL      'TNSC71R'
017700911001     C                   PARM                    KPJBA
017800941128     C                   MOVE      KPJBUS        KPJBU
017900911001     C                   MOVEL     'M'           §TPI
018000911001     C                   GOTO      FMT01
018100911001     C                   END
018200911001     C**
018300910521     C                   EXSR      CTR1
018400910521     C* ERRORI
018500910521     C   40              GOTO      FMT01
018600910521      *
018700910701     C     RICALC        TAG
018800941028     C                   WRITE     SC70D00
018900910521     C                   EXSR      PULSFL
019000910521     C                   EXSR      RIEMPI
019100910617     C* NESSUNA DISTINTA TROVATA
019200910617      * CONTROLLO SE CI SONO RIGHE IN SFL
019300910620     C     NRR           IFEQ      1
019400910617     C                   SETON                                        4043
019500910617     C                   GOTO      FMT01
019600910617     C                   END
019700910521      *          =============
019800910521     C     FMTSF         TAG
019900910521      *          =============
020000150526      * imposta variabili per ordinamento solo alla prima emissione
020100150526     c                   if        v1rgf = *zero and decf11 = *blank
020200150526     c                   eval      decf11 = 'F11=Ord.Data'
020300150526     c                   seton                                        51
020400150526     c                   endif
020500150526      *
020600941028     C                   WRITE     SC70D00
020700941028     C                   WRITE     SC70D02
020800941028     C                   EXFMT     SC70DCT
020900941031     C   KL              GOTO      INIZIO
021000911001     C                   EXSR      CTR2
021100941107     C   KJ
021200941117     COR KN
021300941117     COR KB
021400941117     COR KA              GOTO      RICALC
021500040907     c   98
021600040907     CorNKF              GOTO      FMTSF
021700941031      * AGGIORNAMENTO - CMD06
021800060515     c* emetto window per aggiornamento filiale
021900060515     c                   if        *inkf
022000060522     c                   if        kcdaz = 'SC7B' or kcdaz = 'Y36B'
022100060515     c                   exsr      srwin
022200060515     C     *INkl         CABEQ     '1'           FMTsf
022300060515     c                   end
022400060515     C                   EXSR      AGGIOR
022500060628     c                   if        errfil='1' or *in46
022600060628     c                   move      errfil        *in45
022700060530     c                   goto      fmtsf
022800060515     c                   end
022900060530     c                   end
023000910521     C                   GOTO      INIZIO
023100910521      *
023200910521     C     FINE          TAG
023300910521     C                   SETON                                        LR
023400910521     C*
023500910521     C     DEFCAM        BEGSR
023600000000     C     *ENTRY        PLIST
023700000000     C                   PARM                    KPJBA
023800911001     C                   MOVEL     KPJBU         PARDIR
023900900516      *
024000000000     C                   Z-ADD     1             CODUT
024100900517     C                   CALL      'X§PARUT'
024200900517     C                   PARM                    UT§DSE
024300000000     C     CODUT         CABEQ     -1            FINE
024400000000     C                   MOVEL     RAGUT         RSUT             20
024500900515      *
024600941031     C                   TIME                    WHHUDT           14 0
024700941031     C                   MOVEL     WHHUDT        ORA               6 0
024800941031     C                   MOVE      WHHUDT        G08DAT
024900941031     C                   MOVE      *ZEROS        G08INV
025000941031     C                   MOVEL     *BLANK        G08ERR
025100941031     C                   CALL      'XSRDA8'
025200941031     C                   PARM                    WLBDA8
025300941031     C                   Z-ADD     G08INV        AMGUDT            8 0
025400941031     C                   Z-ADD     G08DAT        GMAUDT            8 0
025500900606      *
025600900606      *  RICERCA CAPOCONTI
025700900606     C                   DO        50            X                 2 0
025800900606     C                   MOVE      TCU(X)        TCUDS
025900900606     C     F56           CABNE     'CG'          END1
026000900606     C     F4            COMP      '1'                                    21
026100900606     C     F4            COMP      '2'                                    22
026200900606     C     F4            COMP      '3'                                    23
026300900606     C     F4            COMP      '6'                                    27
026400900606     C     F4            COMP      '4'                                    28
026500900606      ** 1 CLIENTI   21
026600900606      ** 2 FORNITORI 22
026700900606      ** 3 AGENTI    23
026800900606     C     F3            COMP      '0'                                242425
026900900606     C     F3            COMP      'I'                                    26
027000900606      ** 0 ITALIA   25
027100900606      ** 1 ESTERO   24
027200900606      ** I CAPO CONTO IVA
027300900606     C   21
027400900606     CAN 24              Z-ADD     KCU(X)        KCE               4 0
027500900606     C   21
027600900606     CAN 25              Z-ADD     KCU(X)        KCI               4 0
027700900606     C   22
027800900606     CAN 24              Z-ADD     KCU(X)        KFE               4 0
027900900606     C   22
028000900606     CAN 25              Z-ADD     KCU(X)        KFI               4 0
028100900606     C   23
028200900606     CAN 24              Z-ADD     KCU(X)        KAE               4 0
028300900606     C   23
028400900606     CAN 25              Z-ADD     KCU(X)        KAI               4 0
028500900606     C   26              Z-ADD     KCU(X)        KIVA              4 0
028600900606     C   27              Z-ADD     KCU(X)        KBNA              4 0
028700900606     C   28              Z-ADD     KCU(X)        KCAS              4 0
028800900606     C     END1          TAG
028900900606     C                   END
029000150526     C                   Z-ADD     1             NRR
029100941031      *
029200941031     C                   OPEN      TABEL00F
029300941031      * CARICO TUTTI I CODICI VALUTA  TAB.CV
029400941031     C                   MOVEL     'CV'          CODTAB
029500941031     C                   MOVE      *ZEROS        Y                 3 0
029600941031     C     KCOD          SETLL     TABEL00F
029700941031     C     *IN21         DOUEQ     '1'
029800941031     C     KCOD          READE     TABEL00F                               21
029900950119     C  N21TBLFLG        IFEQ      *BLANK
030000950119     C                   ADD       1             Y
030100950119     C                   MOVEL     TBLKEY        TCV(Y)
030200950119     C                   MOVEL     TBLUNI        DCV(Y)
030300950119     C                   ENDIF
030400950119     C                   ENDDO
030500941031      *
030600941031     C                   CLOSE     TABEL00F
030700941031      *
030800941031     C                   OPEN      AZORG01L
030900941031      * CARICO TUTTI I CODICI FILIALI
031000941031     C                   MOVE      *ZEROS        Y
031100941031     C     *LOVAL        SETLL     AZORG01L
031200941031     C     *IN21         DOUEQ     '1'
031300941031     C                   READ      AZORG01L                               21
031400941031     C  N21ORGFAG        IFEQ      'F'
031500941031     C     ORGFAG        OREQ      'A'
031600971216      * CARICO SOLO LE FILIALI DELLO STESSO  RAMO AZIENDALE
031700941031     C                   ADD       1             Y
031800941031     C                   MOVEL     ORGFIL        FIL(Y)
031900941031     C                   MOVEL     ORGDES        DFI(Y)
032000941031     C                   END
032100941031     C                   END
032200941031      *
032300941031     C                   CLOSE     AZORG01L
032400011018      *
032500011018     C                   EVAL      TbeCod = 'GED'
032600011018     C                   EVAL      TbeKe1 = '1'
032700011018     C                   OPEN      TnTbe01L
032800011018     C     K02Tbe01      CHAIN     TnTbe01L
032900011018     C                   CLOSE     TnTbe01L
033000011018     C                   IF        %FOUND
033100011018     C                   EVAL      DGed = TbeUni
033200011018     C                   ENDIF
033300900515      *---------------------------------------------------------------*
033400900515      * CHIAVI                                                        *
033500900515      *---------------------------------------------------------------*
033600910918     C* X TIPO INTEST
033700910701     C     KTPI          KLIST
033800910701     C                   KFLD                    §TPI
033900910918     C* X TIPO INTEST  +RAGG.FIL. ARRIVO
034000910617     C     KRGF          KLIST
034100910701     C                   KFLD                    §TPI
034200910617     C                   KFLD                    §RGF
034300910701      * = ALLA PREC. + NR.DIST.
034400910617     C     KNDTP         KLIST
034500910701     C                   KFLD                    §TPI
034600910617     C                   KFLD                    §RGF
034700910617     C                   KFLD                    §NDT
034800910617      * X FILIALE + NR.DIST. KEY-COMPLETA
034900910617     C     KNDTC         KLIST
035000910701     C                   KFLD                    §TPI
035100910617     C                   KFLD                    §RGF
035200910617     C                   KFLD                    §NDT
035300910617     C                   KFLD                    §DDC
035400941031      *
035500941031     C     KCOD          KLIST
035600941031     C                   KFLD                    CODUT
035700941031     C                   KFLD                    CODTAB            2
035800011018      *
035900011018     C     K02Tbe01      KLIST
036000011018     C                   KFLD                    TbeCod
036100011018     C                   KFLD                    TbeKe1
036200941031      *
036300910617     C     *LIKE         DEFINE    CSBRGF        §FLE
036400910701     C     *LIKE         DEFINE    CSBTPI        §TPI
036500910617     C     *LIKE         DEFINE    CSBRGF        §RGF
036600910617     C     *LIKE         DEFINE    CSBNDT        §NDT
036700910617     C     *LIKE         DEFINE    CSBDDC        §DDC
036800941031     C     *LIKE         DEFINE    CSBCAS        TDT
036900120131     C     *LIKE         DEFINE    CSBCAS        impo
037000120131     C     *LIKE         DEFINE    CSBCAS        TDTLIR
037100941031     C     *LIKE         DEFINE    CSBCAS        TDTVAL
037200120131     C     *LIKE         DEFINE    CSBCAS        TDTLIR1
037300120131     C     *LIKE         DEFINE    CSBCAS        TDTVAL1
037400941128     C                   MOVE      KPJBU         KPJBUS          256
037500910521     C                   ENDSR
037600900524      *
037700900515      *-----------------------------------------------------***********
037800900515      * PULIZIA CAMPI VIDEO                                 *  PULIZ  *
037900900515      *-----------------------------------------------------***********
038000900515     C     PULIZ         BEGSR
038100900524      *
038200911001     C     RGF           IFNE      *BLANKS
038300911001     C                   MOVEL     RGF           V1RGF
038400911001     C                   MOVEL     NDS           V1NDT
038500911001     C                   ELSE
038600910617     C                   MOVEL     *ZEROS        V1RGF
038700910614     C                   MOVE      *ZEROS        V1NDT
038800911001     C                   END
038900941031     C                   MOVE      GMAUDT        V1DTA
039000910702     C                   MOVEL     *ZEROS        V1STA
039100910918     C                   MOVEL     'M'           §TPI
039200900514     C                   ENDSR
039300900515      *-----------------------------------------------------***********
039400010306      * CONTROLLI FORMATO 1                                 *  CTR1   *
039500900515      *-----------------------------------------------------***********
039600900515     C     CTR1          BEGSR
039700900515      *
039800910701     C                   SETOFF                                       404142
039900910701     C     V1RGF         IFNE      *ZEROS
040000941031     C                   Z-ADD     1             Y
040100941031     C     V1RGF         LOOKUP    FIL(Y)                                 20
040200941031     C  N20              SETON                                        4042
040300941031     C  N20              GOTO      FICTR1
040400910701     C                   END
040500941031     C                   MOVE      V1DTA         G08DAT
040600941031     C                   MOVEL     *BLANK        G08ERR
040700941031     C                   CALL      'XSRDA8'
040800941031     C                   PARM                    WLBDA8
040900941031     C                   MOVE      G08INV        AMGDTA            8 0
041000941031     C                   MOVE      G08DAT        V1DTA
041100941031     C     G08ERR        IFEQ      '1'
041200910614     C                   SETON                                        4041
041300910617     C                   END
041400910620     C     AMGDTA        IFGT      AMGUDT
041500910620     C                   SETON                                        4041
041600910620     C                   END
041700910617     C*
041800910701     C     V1RGF         COMP      *ZEROS                             30
041900910701     C     V1NDT         COMP      *ZEROS                             31
042000910701     C  N30
042100910701     CAN 31              SETON                                        4042
042200910617     C*
042300900515     C     FICTR1        ENDSR
042400900607      *-----------------------------------------------------***********
042500910521      * PULIZIA     SFL                                     *  PULSFL *
042600900607      *-----------------------------------------------------***********
042700910521     C     PULSFL        BEGSR
042800150526     C                   Z-ADD     1             NRR
042900910620     C                   MOVE      *ZEROS        TDT
043000941031     C                   MOVE      *ZEROS        TDTLIR
043100941031     C                   MOVE      *ZEROS        TDTVAL
043200910701     C                   MOVE      *ZEROS        SNRASS
043300120131     C                   MOVE      *ZEROS        conta             4 0
043400120131     C                   MOVE      *ZEROS        NRLIRE            4 0
043500941031     C                   MOVE      *ZEROS        NRVALU            4 0
043600120612     C                   MOVE      *ZEROS        TDTLIR1
043700120131     C                   MOVE      *ZEROS        TDTVAL1
043800120131     C                   MOVE      *ZEROS        SNRASS1
043900120131     C                   MOVE      *ZEROS        NRLIRE1           4 0
044000120131     C                   MOVE      *ZEROS        NRVALU1           4 0
044100120613     c                   clear                   multi             1
044200900607     C                   SETON                                        70
044300941028     C                   WRITE     SC70DCT
044400900607     C                   SETOFF                                       70
044500900607     C                   SETON                                        71
044600900607     C                   ENDSR
044700910521      *-----------------------------------------------------***********
044800910521      * COPIA CAMPI RECORD IN CAMPI SFL                     *  RIEMPI *
044900910521      *-----------------------------------------------------***********
045000910521     C     RIEMPI        BEGSR
045100910521      *
045200910521      * RIEMPIO IL SFL
045300910521      *
045400040907     C                   SETOFF                                       996061
045500120629     C                   SETOFF                                       986263
045600150526     C                   Z-ADD     1             NRR
045700910614     C                   MOVE      *BLANK        SSCE
045800910614     C*
045900910617     C                   MOVE      V1RGF         §RGF
046000910617     C                   MOVE      V1NDT         §NDT
046100910701     C* 31 ON - IMMESSO NR. DISTINTA
046200910617     C*
046300941031     C  N31KRGF          SETLL     TNCSB01L
046400941031     C   31KNDTP         SETLL     TNCSB01L
046500910617     C*
0466009801121--> C     *IN21         DOUEQ     '1'
046700910701     C* SOLO TPI
046800941031     C  N30KTPI          READE     TNCSB01L                               21
046900910617     C* SOLO FILIALE
047000941031     C   30
047100941031     CANN31KRGF          READE     TNCSB01L                               21
047200910617     C* FILIALE + NR. DISTINTA
047300941031     C   30
047400941031     CAN 31KNDTP         READE     TNCSB01L                               21
047500120131     C* aggiungere emissione finestra che indica il riempimento sfl
047600011106     c                   if        nrr >= 9998  and not *in21
047700011106     c                   seton                                        21
047800011106     c                   end
047900910617     C*
0480009801122--> C     *IN21         IFEQ      '0'
048100910617     C* PRIMA VOLTA
0482009801123--> C     *IN99         IFEQ      '0'
048300910617     C                   MOVE      DIST          DISTPR
048400910617     C                   SETON                                        99
048500980112 <--3C                   END
048600120131     c* a rottura di distinta
0487009801123--> C     DIST          IFNE      DISTPR
048800120131     c                   exsr      srtotdis
048900120131     C                   MOVE      DIST          DISTPR
049000120131     c                   clear                   multi             1
049100120131     C                   MOVE      *ZEROS        SNRASS
049200120131     C                   MOVE      *ZEROS        TDTLIR
049300120131     C                   MOVE      *ZEROS        TDTVAL
049400120612     C                   MOVE      *ZEROS        NRLIRE
049500120131     C                   MOVE      *ZEROS        NRVALU
049600120131     C                   MOVE      *ZEROS        TDTLIR1
049700120131     C                   MOVE      *ZEROS        TDTVAL1
049800120131     C                   MOVE      *ZEROS        NRLIRE1
049900120131     C                   MOVE      *ZEROS        NRVALU1
050000980112 <--4C                   END
050100120131      * dettaglio CONTROLLO SE REC. DA CONSIDERARE :
050200120131      *           TIPO ASS.=MITT.
050300120131      *           DEVE ESSERE DEL RAMO AZIENDALE A CUI SI è COLLEGATI
050400980112     C                   Z-ADD     1             Y
050500980112     C     CSBRGF        LOOKUP    FIL(Y)                                 20
0506009801123--> C   20CSBTPI        IFEQ      'M'
050700910702     C     CSBSTA        ANDEQ     V1STA
050800120131     c*
050900120126     c     ' '           scan      csbnra                                 88
051000120126     c                   if        *in88
051100120131     c* emetto errore se non ho ri. assegno
051200040907     c                   if        csbabi = 0 or csbcab = 0 or
051300040907     c                             csbnra = ' '
051400040907     c                   seton                                        61
051500040907     c                   end
051600120126     c                   else
051700120126     c* mi memorizzo che per questa distinta ci sono dei multiassegni
051800120207     c* in modo da sommare gli assegni a rottura di distinta
051900120126     c                   eval      multi = '1'
052000120131     c                   seton                                        62
052100120629     c* verifiche se ho ragioni sociali diverse, lo segnalo
052200120629     c                   if        not *in63
052300120629     c                   eval      innra = csbnra
052400120629     c                   clear                   outnrcli
052500120629     c                   call      'TNSC76R1'
052600120629     C                   PARM                    innra            10
052700120629     C                   PARM                    outnrcli          2 0
052800120629     c                   if        outnrcli > 1
052900120629     c                   seton                                        63
053000120126     c                   end
053100120629     c                   end
053200120629     c                   end
053300040907     c*
053400941031     C                   ADD       CSBCAS        TDT
053500941031     C* CONTROLLO LA DIVISA
053600941031     C                   CLEAR                   DSCV
0537009801124--> C     CSBVCA        IFNE      *BLANKS
053800941031     C                   Z-ADD     1             Y
053900941031     C     CSBVCA        LOOKUP    TCV(Y)                                 22
054000941031     C   22              MOVEL     DCV(Y)        DSCV
054100941031     C                   ELSE
054200941031     C                   MOVEL     'I'           §CVITA
054300980112 <--4C                   END
054400941031      *
054500011018     C                   IF        CsbVca = §GedCn
054600941031     C                   ADD       CSBCAS        TDTLIR
054700941031     C                   ADD       1             NRLIRE
054800941031     C                   ELSE
054900941031     C                   ADD       CSBCAS        TDTVAL
055000941031     C                   ADD       1             NRVALU
055100980112 <--4C                   END
055200980112 <--3C                   END
055300980112 <--2C                   END
055400980112 <--1C                   END
055500910617     C* ULTIMA DISTINTA
055600910617     C* TOTALE DISTINTA > 0  SCRIVO SFL
055700910617      *--------------------------------
055800120126     c                   if        *in21
055900150526     C                   z-add     NRR           stmrec1
056000120131     c                   exsr      srtotdis
056100980112 <--1C                   END
056200910521     C                   ENDSR
056300120126      *-----------------------------------------------------***********
056400120126      * conta i multiassegni
056500120126      *-----------------------------------------------------***********
056600120131     C     srtotdis      BEGSR
056700120612     c                   clear                   tdtval1
056800120612     c                   clear                   nrvalu1
056900120612     c                   clear                   tdtlir1
057000120612     c                   clear                   nrlire1
057100120131     c* se ho dei multiassegni li sommo ora
057200120131     c                   if        multi = '1'
057300120131     c                   exsr      srconta
057400120613     c                   clear                   multi             1
057500120131     c                   end
057600120131     C* TOTALE DISTINTA > 0  SCRIVO SFL
057700120131      *--------------------------------
0578001201314--> C     TDT           IFNE      *ZEROS
057900120131     C                   MOVEL     *BLANK        SDRGF
058000120131     C                   Z-ADD     1             Y
058100120131     C     PRERGF        LOOKUP    FIL(Y)                                 20
058200120131     C   20              MOVEL     DFI(Y)        SDRGF
058300120131     C  N20              MOVEL     *ALL'?'       SDRGF
058400120131     C                   MOVEL     PRERGF        SRGF
058500120131     C                   MOVE      PREDDC        G08INV
058600120131     C                   MOVEL     '3'           G08ERR
058700120131     C                   CALL      'XSRDA8'
058800120131     C                   PARM                    WLBDA8
058900120131     C                   MOVE      G08DAT        SDDC
059000150529     C                   MOVE      G08inv        SDDCh
059100120131     C                   Z-ADD     PRENDT        SNDT
059200120131     C                   SETOFF                                       60
059300120131      * SCRIVO 2 TOTALI: IN LIRE E IN VALUTA
0594001201315--> C     NRLIRE        IFGT      0
059500120131     C                   SETOFF                                       60
059600120131     C                   Z-ADD     NRLIRE        SNRASS
059700120131     C                   Z-ADD     TDTLIR        STDT
059800120131     C                   EVAL      SDIVIS = §GedCn
059900120131     c                   if        nrlire1 = 0
060000120131     C                   Z-ADD     NRLIRE        SNRASS1
060100120131     C                   Z-ADD     TDTLIR        STDT1
060200120131     C                   EVAL      SDIVIS1 = §GedCn
060300120131     c                   else
060400120131     C                   Z-ADD     NRLIRE1       SNRASS1
060500120131     C                   Z-ADD     TDTLIR1       STDT1
060600120131     C                   EVAL      SDIVIS1 = §GedCn
060700120131     c                   end
060800120131     c                   movel     *in60         ind60
060900120131     c                   movel     *in61         ind61
061000120131     c                   movel     *in62         ind62
061100120629     c                   movel     *in63         ind63
061200120131     C                   WRITE     SC70DSF
061300120131     C                   ADD       1             NRR
061400120629     c                   setoff                                       616263
061500120131 <--5C                   END
0616001201315--> C     NRVALU        IFGT      0
061700120131      * NON VISUAL. DISTINTA (60ON) SOLO SE CI SONO ASSEGNI IN LIRE
0618001201316--> C     NRLIRE        IFGT      0
061900120131     C                   SETON                                        60
062000120131     C                   ENDIF
062100120131     C                   Z-ADD     NRVALU        SNRASS
062200120131     C                   Z-ADD     TDTVAL        STDT
062300120131     C                   MOVEL     'Val.'        SDIVIS
062400120131     c                   if        nrvalu1 = 0
062500120131     C                   Z-ADD     NRVALU        SNRASS1
062600120131     C                   Z-ADD     TDTVAL        STDT1
062700120131     C                   MOVEL     'Val.'        SDIVIS1
062800120131     c                   else
062900120131     C                   Z-ADD     NRVALU1       SNRASS1
063000120131     C                   Z-ADD     TDTVAL1       STDT1
063100120131     C                   MOVEL     'Val.'        SDIVIS1
063200120131     c                   end
063300120131     c                   movel     *in60         ind60
063400120131     c                   movel     *in61         ind61
063500120131     c                   movel     *in62         ind62
063600120629     c                   movel     *in63         ind63
063700120131     C                   WRITE     SC70DSF
063800120131     C                   ADD       1             NRR
063900120629     c                   setoff                                       616263
064000120131 <--6C                   END
064100120131 <--5C                   END
064200120131     C                   MOVE      *ZEROS        TDT
064300120131     C                   ENDSR
064400120131      *-----------------------------------------------------***********
064500120131      * conta i multiassegni
064600120131      *-----------------------------------------------------***********
064700120131     C     srconta       BEGSR
064800120131     C* conta gli assegni multipli in divisa
064900120131     c                   if        nrvalu <> 0
065000120131     C/EXEC SQL
065100120131     C+   select  sum(csmidc), count(*) into :impo, :conta
065200120131     C+   from (SELECT DISTINCT csbnra from tncsb00f
065300120131     C+   where csbrgf = :PRERGF and csbndt = :PRENDT and csbddc =
065400120220     C+   :PREDDC and substr(csbnra, 10, 1) <> ' ' and csbabi = 0 and
065500120131     C+   csbsta = :v1sta and csbtpi = 'M' and csbvca <> :§GedCn
065600120314     c+   and csbdta = 0
065700120131     C+   group by csbnra) as pippo join tncsm00f a on
065800120131     C+   pippo.csbnra = a.csmkey
065900120131     C/END-EXEC
066000120131     C                   eval      tdtval1 = impo
066100120131     c                   eval      nrvalu1 = conta
066200120131     c                   clear                   conta
066300120131     c                   clear                   impo
066400120131     C* conta gli assegni singoli in valuta
066500120131     C/EXEC SQL
066600120131     C+   select  sun(csbcas), count(*) into :impo, :conta
066700120207     C+   from tncsb00f
066800120131     C+   where csbrgf = :PRERGF and csbndt = :PRENDT and csbddc =
066900120220     C+   :PREDDC and substr(csbnra, 10, 1) = ' ' and csbabi <>0 and
067000120131     C+   csbcab <> 0 and csbsta = :v1sta and csbtpi = 'M'
067100120131     C+   and csbvca <> :§gedcn
067200120314     c+   and csbdta = 0
067300120131     C/END-EXEC
067400120131     c                   add       conta         nrvalu1
067500120131     c                   add       impo          tdtval1
067600120131     c                   end
067700120131     C* conta gli assegni multipli in eur
067800120131     c                   if        nrlire <> 0
067900120131     C/EXEC SQL
068000120131     C+   select  sum(csmidc), count(*) into :impo, :conta
068100120131     C+   from (SELECT DISTINCT csbnra from tncsb00f
068200120131     C+   where csbrgf = :PRERGF and csbndt = :PRENDT and csbddc =
068300120220     C+   :PREDDC and substr(csbnra, 10, 1) <> ' ' and csbabi = 0 and
068400120131     C+   csbsta = :v1sta and csbtpi = 'M' and csbvca = :§gedcn
068500120314     c+   and csbdta = 0
068600120131     C+   group by csbnra) as pippo join tncsm00f a on
068700120131     C+   pippo.csbnra = a.csmkey
068800120131     C/END-EXEC
068900120131     c                   eval      nrlire1 = conta
069000120131     c                   eval      tdtlir1 = impo
069100120131     c                   clear                   conta
069200120131     c                   clear                   impo
069300120131     C* conta gli assegni singoli in EUR
069400120131     C/EXEC SQL
069500120131     C+   select  sum(csbcas), count(*) into :impo, :conta
069600120207     C+   from tncsb00f
069700120131     C+   where csbrgf = :PRERGF and csbndt = :PRENDT and csbddc =
069800120220     C+   :PREDDC and substr(csbnra, 10, 1) = ' ' and csbabi <>0 and
069900120131     C+   csbcab <> 0 and csbsta = :v1sta and csbtpi = 'M'
070000120131     C+   and csbvca = :§GedCn
070100120314     c+   and csbdta = 0
070200120131     C/END-EXEC
070300120131     c                   add       conta         nrlire1
070400120131     c                   add       impo          tdtlir1
070500120131     c                   end
070600120126     C                   ENDSR
070700900515      *-----------------------------------------------------***********
070800900524      * CONTROLLI SUBFILE                                   *  CTR2   *
070900900515      *-----------------------------------------------------***********
071000900515     C     CTR2          BEGSR
071100900515      *
071200040907     c                   setoff                                       98
071300900524      *
071400910614     C     *IN20         DOUEQ     '1'
071500941028     C                   READC     SC70DSF                                20
071600150526     c                   if        *inkk  and v1rgf = 0
071700150526     c                   exsr      f11ges
071800150526     c                   endif
071900941117     C* AGGIUNTA C/ASS.  - CMD10
072000150526     c                   if        *inkj
072100150526     C                   MOVE      KPJBU         KPJBUS
072200150526     C                   MOVE      *BLANK        KPJBU
072300150526     C                   CALL      'TNSC03R'
072400911001     C                   PARM                    KPJBA
072500150526     C                   MOVE      KPJBUS        KPJBU
072600150526     C                   GOTO      FICTR2
072700150526     c                   endif
072800941117     C* MODIF. C/ASS     - CMD14
072900150526     c                   if        *inkn
073000150526     C                   MOVE      KPJBU         KPJBUS
073100150526     C                   MOVE      *BLANK        KPJBU
073200150526     C                   CALL      'TNSC95R'
073300941117     C                   PARM                    KPJBA
073400150526     C                   MOVE      KPJBUS        KPJBU
073500150526     C                   GOTO      FICTR2
073600150526     c                   endif
073700941107     C* INTERR.  BOLLE   - CMD08
073800150526     c                   if        *inkh
073900150526     C                   MOVE      KPJBU         KPJBUS
074000150526     C                   CLEAR                   TNSB50
074100150526     C                   MOVE      *BLANK        KPJBU
074200150526     C                   MOVEL     'C00'         I50OP0
074300150526     C                   MOVEL     TNSB50        KPJBU
074400150526     C                   CALL      'TNSB50R'
074500911001     C                   PARM                    KPJBA
074600150526     C                   MOVE      KPJBUS        KPJBU
074700150526     C                   GOTO      FICTR2
074800150526     c                   endif
074900941107     C* INTERR. C/ASS.   - CMD07
075000150526     c                   if        *inkg
075100150526     C                   MOVE      KPJBU         KPJBUS
075200150526     C                   MOVE      *BLANK        KPJBU
075300150526     C                   CALL      'TNSC40R'
075400911001     C                   PARM                    KPJBA
075500150526     C                   MOVE      KPJBUS        KPJBU
075600150526     C                   GOTO      FICTR2
075700150526     c                   endif
075800900524      *-------------
075900910620     C     *IN20         IFEQ      '0'
076000941117      *-------------   DETTAGLIO SPEDIZIONI  DELLA DISTINTA SCELTA CMD02
076100941117      *-------------   DETTAGLIO ALTRE SPEDIZIONI NON INCASS CMD1
076200941117     C     *INKA         IFEQ      '1'
076300941117     C     *INKB         OREQ      '1'
076400941031     C     SSCE          IFEQ      '1'
076500910701     C                   MOVEL     §PGM          PARPGM
076600910701     C                   MOVEL     *BLANKS       PARERR
076700910701     C                   MOVE      SRGF          PARRGF
076800910918     C                   MOVE      SNDT          PARNDT
076900941031     C                   MOVE      SDDC          G08DAT
077000941031     C                   MOVEL     *BLANK        G08ERR
077100941031     C                   CALL      'XSRDA8'
077200941031     C                   PARM                    WLBDA8
077300941031     C                   MOVE      G08INV        PARDDC
077400150529     C                   MOVE      G08INV        sddch
077500941117     C   KB              MOVEL     §TPI          PARTPI
077600941117     C   KB              MOVEL     '14'          PARCMD
077700941117     C   KA              MOVEL     ' '           PARTPI
077800941117     C   KA              MOVEL     '13'          PARCMD
077900910702     C                   MOVE      V1STA         PARSTA
078000941117     C   KBV1STA         IFEQ      0
078100910702     C                   MOVEL     'SSMA'        PARCAV
078200910918     C                   MOVEL     '01'          PARTXT
078300910702     C                   ELSE
078400910702     C                   MOVEL     'RSMA'        PARCAV
078500910702     C                   MOVEL     *BLANK        PARTXT
078600910702     C                   END
078700941128     C                   MOVE      KPJBU         KPJBUS
078800941028     C                   MOVEL     TNSC71        KPJBU
078900941028     C                   CALL      'TNSC71R'
079000910617     C                   PARM                    KPJBA
079100941128     C                   MOVE      KPJBUS        KPJBU
079200910701     C     PARERR        COMP      'E'                                    45
079300910617     C                   END
079400910919     C                   END
079500900516      *
079600011106     c                   movel     ind60         *in60
079700040907     c                   movel     ind61         *in61
079800120131     c                   movel     ind62         *in62
079900120629     c                   movel     ind63         *in63
080000040907     c                   if        *in61 and ssce = '1'
080100040907     c                   seton                                        98
080200040907     c                   end
080300980302      *
080400910701     C                   SETON                                        72
080500941028     C                   UPDATE    SC70DSF
080600011106     C                   SETOff                                       72
080700910620     C                   END
080800910617     C                   END
080900900515     C     FICTR2        ENDSR
081000900515      *-----------------------------------------------------***********
081100910614      * AGGIORNAMENTO DATA ARRIVO DISTINTA                  *  AGGIOR *
081200900515      *-----------------------------------------------------***********
081300900607     C     AGGIOR        BEGSR
081400060530     c                   seton                                        46
081500900515      *
081600910617     C     *IN20         DOUEQ     '1'
081700941028     C                   READC     SC70DSF                                20
081800900524      *-------------
081900941031     C  N20SSCE          IFEQ      '1'
082000060530     c                   setoff                                       46
082100910617     C                   MOVE      SRGF          §RGF
082200910617     C                   MOVE      SNDT          §NDT
082300941031     C                   MOVE      SDDC          G08DAT
082400941031     C                   MOVEL     *BLANK        G08ERR
082500941031     C                   CALL      'XSRDA8'
082600941031     C                   PARM                    WLBDA8
082700941031     C                   MOVE      G08INV        §DDC
082800060524     c                   move      '0'           errfil            1
082900060628     c*
083000941031     C     KNDTC         SETLL     TNCSB01L
083100910617     C     *IN21         DOUEQ     '1'
083200941031     C     KNDTC         READE     TNCSB01L                               21
083300910614     C*  PER TUTTI I RECORD DELLA DISTINTA
083400910620     C     *IN21         IFEQ      '0'
083500910920     C     CSBSTA        ANDNE     5
083600910620     C     CSBTPI        ANDEQ     'M'
083700060515     c* se scelto di aggiornare la filiale richiamo la funzione che chiude
083800060515     c* la spedizione in filiale
083900060522     c                   if        (kcdaz = 'SC7B' or kcdaz = 'Y36B')
084000060522     c                             and aggfil = 'S'
084100060515     c                   exsr      sraggfil
084200060522     c* se errore in filiale segnalo
084300060524     c                   if        *in45 and errfil = '0'
084400060522     c                   move      *in45         errfil
084500060627     c                   seton                                        72
084600060522     c                   update    SC70DSF
084700060515     C                   END
084800060522     C                   END
084900060522     c* se c'è errore non aggiorno c/ass
085000060522     c                   if        not *in45
085100910620     C                   MOVE      AMGDTA        CSBDTA
085200940415     C                   MOVE      'S'           CSBMAS
085300910620     C                   EXCEPT    AGGCSB
085400060522     C                   END
085500910617     C                   END
085600060512     C                   ENDdo
085700060522     c* se c'è errore non proseguo
085800060522     c                   if        errfil ='1'
085900060522     c                   goto      fineagg
086000060522     c                   end
086100060512     c* controllo se sono state escluse delle spedizioni, per ripristinarle
086200060517     c* solo se il numero della distinta in entrata è diverso
086300060512     C                   MOVEl     9             §NDT
086400060517     c                   if        sndt <> §ndt
086500060512     C     KNDTC         SETLL     TNCSB01L
086600060512     C     *IN21         DOUEQ     '1'
086700060512     C     KNDTC         READE     TNCSB01L                               21
086800060512     C*  PER TUTTI I RECORD DELLA DISTINTA
086900060512     C     *IN21         IFEQ      '0'
087000060512     C     CSBSTA        ANDNE     5
087100060512     C     CSBTPI        ANDEQ     'M'
087200060516     C     CSBdta        ANDEQ     0
087300060512     C                   MOVEl     0             CSBndt
087400060512     C                   update    tncsb000
087500060512     C                   END
087600060512     C                   ENDdo
087700060517     C                   END
087800060517     C                   MOVE      SNDT          §NDT
087900900524      *---------
088000900524     C                   END
088100060512     C                   ENDdo
088200060515     C     fineagg       ENDSR
088300060515     C*-------------------------------------------------------*
088400060515     C* Aggiorna gli archivi gestionali
088500060515     C*-------------------------------------------------------*
088600060515     C     sraggfil      BEGSR
088700060524     c                   setoff                                       45
088800060515     C* Richiamo TIBS55R per conoscere S.Informativi
088900060515     C* in cui immettere il lavoro
089000060515     C                   CLEAR                   TIBS55ds
089100060515     C                   MOVEL     'L'           £I50TLA
089200060515     C                   MOVEL     '046'         £I50PPO
089300060515     C                   MOVEL     '001'         £I50APO
089400060515     C                   CALL      'TIBS55R'
089500060515     C                   PARM                    TIBS55ds
089600060515     C* MI SALVO IL S.I. ORIGINALE XCHÈ SE NO NON LO REIMPOSTA
089700060515     c                   clear                   param
089800060515     c                   eval      sifda = KNSIF
089900060515     c                   eval      sifa = £O50ASI
090000060519     c                   eval      parca= 'C'
090100060519     c                   eval      kaas = csbaas
090200060515     c                   eval      klnp = csblnp
090300060515     c                   eval      knrs = csbnrs
090400060515     c                   eval      knsp = csbnsp
090500060515     C                   MOVEL(p)  PARAM         KPJBU
090600060515     c                   call      'CNC0W13C'
090700060515     C                   PARM                    KPJBA
090800060515     c                   movel     kpjbu         param
090900130308     c                   if        paresito <> *blank
091000060627     c                   seton                                        45
091100060515     c                   else
091200060515     c                   setoff                                       45
091300060515     c                   end
091400060515     C*
091500060515     C                   ENDSR
091600060512     C**-------------------------------------------------------------
091700060512     C** srwin - domnda se aggiornare filiale
091800060512     C**-------------------------------------------------------------
091900060512     C     srwin         BEGSR
092000060512     c                   do        *hival
092100060512     C                   EXFMT     SC70W01
092200060515     c                   if        *inkl
092300060515     c                   clear                   aggfil
092400060515     c                   leave
092500060515     c                   end
092600060512     c                   if        aggfil = ' '
092700060512     c                   seton                                        44
092800060512     c                   iter
092900060512     c                   else
093000060512     c                   setoff                                       44
093100060512     c                   end
093200060512     c                   if        *inkf
093300060512     c                   leave
093400060512     c                   end
093500060512     c                   enddo
093600060512     c                   endsr
093700150526      *-------------------------------------------------------------------------
093800150526      * F11GES - TASTO FUNZIONE F11 --> Ragione sociale/cod. aut.
093900150526      *-------------------------------------------------------------------------
094000150526     C     F11GES        BEGSR
094100150526      * IMPOSTO ENTRATA AD 'A'
094200150526     C                   EVAL      nrr  = 1
094300150526      *
094400150526     C                   CLEAR                   QLGSCB
094500150526     C                   CLEAR                   QLGSCB00
094600150526
094700150526     ** 1 campo chiave.
094800150526     C                   EVAL      QLGNBRK    = 1
094900150526
095000150526     C                   IF        *in51=*on
095100150526     c                   eval      decf11 = 'F11=Ord.Fil.'
095200150526     ** IL P.O è a posizione 3, numerico, 3 byte, ascending.
095300150526     C                   EVAL      QLGSP      = 1
095400150529     C                   EVAL      QLGSS      = %SIZE(sddch)
095500150526     C                   EVAL      QLGDT      = Numerico
095600150526     C                   EVAL      QLGSO      = Ascendente
095700150526     C                   EVAL      QLGKL(1)   = QLGSKL
095800150526     C                   setoff                                       51
095900150526     C                   ELSE
096000150526      *
096100150526     c                   eval      decf11 = 'F11=Ord.Data'
096200150526     ** La ragione sociale è a posizione 6, 20 byte, char, ascending.
096300150526     C                   EVAL      QLGSP      = 9
096400150526     C                   EVAL      QLGSS      = %SIZE(srgf)
096500150526     C                   EVAL      QLGDT      = Numerico
096600150526     C                   EVAL      QLGSO      = Ascendente
096700150526     C                   EVAL      QLGKL(1)   = QLGSKL
096800150526     C                   seton                                        51
096900150526     C                   ENDIF
097000150526
097100150526      * Load other sort parameters.
097200150526     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
097300150526     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
097400150526     C                   EVAL      QLGRT     = 8
097500150526     C                   EVAL      QLGOKL    = 80
097600150526     C                   EVAL      QLGLKE    = 16
097700150526     C                   EVAL      QLGLSS    = 290
097800150526
097900150526      * Initialize Sort I/O API fields.
098000150526     C                   EVAL      QLGRL00  = QLGRL
098100150526     C                   EVAL      QLGRC00  = 1
098200150526     C                   CLEAR                   QUSEI
098300150526     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
098400150526
098500150526      * First step - Initialize the sort routine.
098600150526     C                   CALL      'QLGSORT'
098700150526     C                   PARM                    QLGSCB
098800150526     C                   PARM                    NotUsed
098900150526     C                   PARM                    NotUsed
099000150526     C                   PARM                    SizeList
099100150526     C                   PARM                    ReturnSize
099200150526     C                   PARM                    QUSEC
099300150526
099400150526      * Next step - Write records to I/O routine.
099500150526     C                   EVAL      QLGRT00 = Put
099600150526
099700150526     C                   DO        STMREC1       xx                4 0
099800150526
099900150526     C     xx            CHAIN     sc70dsf
100000150526
100100150526     ** Solo le righe con Selected = 'Y' sono riordinate,
100200150526     ** quindi per fare un ordinamento di tutte le righe
100300150526     ** metto 'Y' sempre.
100400150526     C                   EVAL      Selected = 'Y'
100500150526
100600150526     C                   CLEAR                   QUSEI
100700150526     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
100800150526     C                   CALL      'QLGSRTIO'
100900150526     C                   PARM                    QLGSCB00
101000150526     C                   PARM                    SflRcd
101100150526     C                   PARM                    NotUsed
101200150526     C                   PARM                    SizeList
101300150526     C                   PARM                    NotUsed
101400150526     C                   PARM                    QUSEC
101500150526
101600150526     C                   ENDDO
101700150526
101800150526      * Next step - Signal end of input, clear subfile for reload.
101900150526     C                   EVAL      QLGRT00 = EndPut
102000150526     C                   CLEAR                   QUSEI
102100150526     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
102200150526
102300150526     C                   CALL      'QLGSRTIO'
102400150526     C                   PARM                    QLGSCB00
102500150526     C                   PARM                    SflRcd
102600150526     C                   PARM                    NotUsed
102700150526     C                   PARM                    SizeList
102800150526     C                   PARM                    NotUsed
102900150526     C                   PARM                    QUSEC
103000150526     C                   SETON                                        70
103100150526     C                   WRITE     SC70DCT
103200150526     C                   SETOFF                                       70
103300150526     C*                  SETON                                        71
103400150526
103500150526     C                   WRITE     sc70d00
103600150526     C                   WRITE     sc70d02
103700150526
103800150526      * Final step - Write the records back to the subfile.
103900150526     C                   EVAL      QLGRT00 = Get
104000150526
104100150526     C                   DO        STMREC1       xx
104200150526     C                   CLEAR                   QUSEI
104300150526     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
104400150526     C                   CALL      'QLGSRTIO'
104500150526     C                   PARM                    QLGSCB00
104600150526     C                   PARM                    NotUsed
104700150526     C                   PARM                    SflRcd
104800150526     C                   PARM                    QLGRL00
104900150526     C                   PARM                    NotUsed
105000150526     C                   PARM                    QUSEC
105100150526      *
105200150526     c                   z-add     xx            nrr
105300150526     c                   move      IND60         *IN60
105400150526     c                   move      IND61         *IN61
105500150526     c                   move      IND62         *IN62
105600150526     c                   move      IND63         *IN63
105700150526     C                   WRITE     sc70dsf
105800150526     C                   ENDDO
105900150526      *
106000150526     C                   ENDSR
106100150526      *_____________________________________________________________________
106200941031     OTNCSB000  E            AGGCSB
106300910617     O                       CSBDTA
106400940415     O                       CSBMAS
