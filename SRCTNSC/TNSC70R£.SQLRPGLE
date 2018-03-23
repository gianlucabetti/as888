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
008800900524      *
008900910521     C                   EXSR      DEFCAM
009000910521      *          =============
009100910521     C     INIZIO        TAG
009200910521      *          =============
009300910521     C                   EXSR      PULIZ
009400910521      *
009500910521      *          =============
009600910521     C     FMT01         TAG
009700910521      *          =============
009800910521      *
009900941028     C                   WRITE     SC70D00
010000941028     C                   EXFMT     SC70D01
010100911001     C                   SETOFF                                       404243
010200971216     C                   SETOFF                                       44
010300941031     C   KC              GOTO      FINE
010400941031     C* INSERIMENTO DIST. MANAULE - CMD10
010500941031     C   KJ              DO
010600911001     C*  OBBLIGO RGF E NR DIST.
010700911001     C     V1RGF         IFEQ      *ZERO
010800911001     C                   SETON                                          4042
010900911001     C                   END
011000911001     C     V1NDT         IFEQ      *ZERO
011100911001     C                   SETON                                          4043
011200911001     C                   END
011300911001     C     *IN40         CABEQ     '1'           FMT01
011400971216      *
011500911001     C                   MOVEL     §PGM          PARPGM
011600911001     C                   MOVEL     *BLANKS       PARERR
011700990729     C                   MOVE      GMAUDT        V1DTA
011800911001     C                   MOVEL     *ZEROS        V1STA
011900911001     C                   MOVEL     ' '           §TPI
012000911001     C                   MOVE      V1RGF         PARRGF
012100911001     C                   MOVE      V1NDT         PARNDT
012200941031     C                   MOVE      AMGUDT        PARDDC
012300911001     C                   MOVEL     ' '           PARTPI
012400911001     C                   MOVEL     '13'          PARCMD
012500911001     C                   MOVE      V1STA         PARSTA
012600941128     C                   MOVE      KPJBU         KPJBUS
012700941128     C                   MOVEL     *BLANK        KPJBU
012800941028     C                   MOVEL     TNSC71        KPJBU
012900941028     C                   CALL      'TNSC71R'
013000911001     C                   PARM                    KPJBA
013100941128     C                   MOVE      KPJBUS        KPJBU
013200911001     C                   MOVEL     'M'           §TPI
013300911001     C                   GOTO      FMT01
013400911001     C                   END
013500911001     C**
013600910521     C                   EXSR      CTR1
013700910521     C* ERRORI
013800910521     C   40              GOTO      FMT01
013900910521      *
014000910701     C     RICALC        TAG
014100941028     C                   WRITE     SC70D00
014200910521     C                   EXSR      PULSFL
014300910521     C                   EXSR      RIEMPI
014400910617     C* NESSUNA DISTINTA TROVATA
014500910617      * CONTROLLO SE CI SONO RIGHE IN SFL
014600910620     C     NRR           IFEQ      1
014700910617     C                   SETON                                        4043
014800910617     C                   GOTO      FMT01
014900910617     C                   END
015000910521      *          =============
015100910521     C     FMTSF         TAG
015200910521      *          =============
015300910521      *
015400941028     C                   WRITE     SC70D00
015500941028     C                   WRITE     SC70D02
015600941028     C                   EXFMT     SC70DCT
015700941031     C   KL              GOTO      INIZIO
015800911001     C                   EXSR      CTR2
015900941107     C   KJ
016000941117     COR KN
016100941117     COR KB
016200941117     COR KA              GOTO      RICALC
016300040907     c   98
016400040907     CorNKF              GOTO      FMTSF
016500941031      * AGGIORNAMENTO - CMD06
016600060515     c* emetto window per aggiornamento filiale
016700060515     c                   if        *inkf
016800060522     c                   if        kcdaz = 'SC7B' or kcdaz = 'Y36B'
016900060515     c                   exsr      srwin
017000060515     C     *INkl         CABEQ     '1'           FMTsf
017100060515     c                   end
017200060515     C                   EXSR      AGGIOR
017300060628     c                   if        errfil='1' or *in46
017400060628     c                   move      errfil        *in45
017500060530     c                   goto      fmtsf
017600060515     c                   end
017700060530     c                   end
017800910521     C                   GOTO      INIZIO
017900910521      *
018000910521     C     FINE          TAG
018100910521     C                   SETON                                        LR
018200910521     C*
018300910521     C     DEFCAM        BEGSR
018400000000     C     *ENTRY        PLIST
018500000000     C                   PARM                    KPJBA
018600911001     C                   MOVEL     KPJBU         PARDIR
018700900516      *
018800000000     C                   Z-ADD     1             CODUT
018900900517     C                   CALL      'X§PARUT'
019000900517     C                   PARM                    UT§DSE
019100000000     C     CODUT         CABEQ     -1            FINE
019200000000     C                   MOVEL     RAGUT         RSUT             20
019300900515      *
019400941031     C                   TIME                    WHHUDT           14 0
019500941031     C                   MOVEL     WHHUDT        ORA               6 0
019600941031     C                   MOVE      WHHUDT        G08DAT
019700941031     C                   MOVE      *ZEROS        G08INV
019800941031     C                   MOVEL     *BLANK        G08ERR
019900941031     C                   CALL      'XSRDA8'
020000941031     C                   PARM                    WLBDA8
020100941031     C                   Z-ADD     G08INV        AMGUDT            8 0
020200941031     C                   Z-ADD     G08DAT        GMAUDT            8 0
020300900606      *
020400900606      *  RICERCA CAPOCONTI
020500900606     C                   DO        50            X                 2 0
020600900606     C                   MOVE      TCU(X)        TCUDS
020700900606     C     F56           CABNE     'CG'          END1
020800900606     C     F4            COMP      '1'                                    21
020900900606     C     F4            COMP      '2'                                    22
021000900606     C     F4            COMP      '3'                                    23
021100900606     C     F4            COMP      '6'                                    27
021200900606     C     F4            COMP      '4'                                    28
021300900606      ** 1 CLIENTI   21
021400900606      ** 2 FORNITORI 22
021500900606      ** 3 AGENTI    23
021600900606     C     F3            COMP      '0'                                242425
021700900606     C     F3            COMP      'I'                                    26
021800900606      ** 0 ITALIA   25
021900900606      ** 1 ESTERO   24
022000900606      ** I CAPO CONTO IVA
022100900606     C   21
022200900606     CAN 24              Z-ADD     KCU(X)        KCE               4 0
022300900606     C   21
022400900606     CAN 25              Z-ADD     KCU(X)        KCI               4 0
022500900606     C   22
022600900606     CAN 24              Z-ADD     KCU(X)        KFE               4 0
022700900606     C   22
022800900606     CAN 25              Z-ADD     KCU(X)        KFI               4 0
022900900606     C   23
023000900606     CAN 24              Z-ADD     KCU(X)        KAE               4 0
023100900606     C   23
023200900606     CAN 25              Z-ADD     KCU(X)        KAI               4 0
023300900606     C   26              Z-ADD     KCU(X)        KIVA              4 0
023400900606     C   27              Z-ADD     KCU(X)        KBNA              4 0
023500900606     C   28              Z-ADD     KCU(X)        KCAS              4 0
023600900606     C     END1          TAG
023700900606     C                   END
023800011106     C                   Z-ADD     0             NRR               4 0
023900941031      *
024000941031     C                   OPEN      TABEL00F
024100941031      * CARICO TUTTI I CODICI VALUTA  TAB.CV
024200941031     C                   MOVEL     'CV'          CODTAB
024300941031     C                   MOVE      *ZEROS        Y                 3 0
024400941031     C     KCOD          SETLL     TABEL00F
024500941031     C     *IN21         DOUEQ     '1'
024600941031     C     KCOD          READE     TABEL00F                               21
024700950119     C  N21TBLFLG        IFEQ      *BLANK
024800950119     C                   ADD       1             Y
024900950119     C                   MOVEL     TBLKEY        TCV(Y)
025000950119     C                   MOVEL     TBLUNI        DCV(Y)
025100950119     C                   ENDIF
025200950119     C                   ENDDO
025300941031      *
025400941031     C                   CLOSE     TABEL00F
025500941031      *
025600941031     C                   OPEN      AZORG01L
025700941031      * CARICO TUTTI I CODICI FILIALI
025800941031     C                   MOVE      *ZEROS        Y
025900941031     C     *LOVAL        SETLL     AZORG01L
026000941031     C     *IN21         DOUEQ     '1'
026100941031     C                   READ      AZORG01L                               21
026200941031     C  N21ORGFAG        IFEQ      'F'
026300941031     C     ORGFAG        OREQ      'A'
026400971216      * CARICO SOLO LE FILIALI DELLO STESSO  RAMO AZIENDALE
026500941031     C                   ADD       1             Y
026600941031     C                   MOVEL     ORGFIL        FIL(Y)
026700941031     C                   MOVEL     ORGDES        DFI(Y)
026800941031     C                   END
026900941031     C                   END
027000941031      *
027100941031     C                   CLOSE     AZORG01L
027200011018      *
027300011018     C                   EVAL      TbeCod = 'GED'
027400011018     C                   EVAL      TbeKe1 = '1'
027500011018     C                   OPEN      TnTbe01L
027600011018     C     K02Tbe01      CHAIN     TnTbe01L
027700011018     C                   CLOSE     TnTbe01L
027800011018     C                   IF        %FOUND
027900011018     C                   EVAL      DGed = TbeUni
028000011018     C                   ENDIF
028100900515      *---------------------------------------------------------------*
028200900515      * CHIAVI                                                        *
028300900515      *---------------------------------------------------------------*
028400910918     C* X TIPO INTEST
028500910701     C     KTPI          KLIST
028600910701     C                   KFLD                    §TPI
028700910918     C* X TIPO INTEST  +RAGG.FIL. ARRIVO
028800910617     C     KRGF          KLIST
028900910701     C                   KFLD                    §TPI
029000910617     C                   KFLD                    §RGF
029100910701      * = ALLA PREC. + NR.DIST.
029200910617     C     KNDTP         KLIST
029300910701     C                   KFLD                    §TPI
029400910617     C                   KFLD                    §RGF
029500910617     C                   KFLD                    §NDT
029600910617      * X FILIALE + NR.DIST. KEY-COMPLETA
029700910617     C     KNDTC         KLIST
029800910701     C                   KFLD                    §TPI
029900910617     C                   KFLD                    §RGF
030000910617     C                   KFLD                    §NDT
030100910617     C                   KFLD                    §DDC
030200941031      *
030300941031     C     KCOD          KLIST
030400941031     C                   KFLD                    CODUT
030500941031     C                   KFLD                    CODTAB            2
030600011018      *
030700011018     C     K02Tbe01      KLIST
030800011018     C                   KFLD                    TbeCod
030900011018     C                   KFLD                    TbeKe1
031000941031      *
031100910617     C     *LIKE         DEFINE    CSBRGF        §FLE
031200910701     C     *LIKE         DEFINE    CSBTPI        §TPI
031300910617     C     *LIKE         DEFINE    CSBRGF        §RGF
031400910617     C     *LIKE         DEFINE    CSBNDT        §NDT
031500910617     C     *LIKE         DEFINE    CSBDDC        §DDC
031600941031     C     *LIKE         DEFINE    CSBCAS        TDT
031700120131     C     *LIKE         DEFINE    CSBCAS        impo
031800120131     C     *LIKE         DEFINE    CSBCAS        TDTLIR
031900941031     C     *LIKE         DEFINE    CSBCAS        TDTVAL
032000120131     C     *LIKE         DEFINE    CSBCAS        TDTLIR1
032100120131     C     *LIKE         DEFINE    CSBCAS        TDTVAL1
032200941128     C                   MOVE      KPJBU         KPJBUS          256
032300910521     C                   ENDSR
032400900524      *
032500900515      *-----------------------------------------------------***********
032600900515      * PULIZIA CAMPI VIDEO                                 *  PULIZ  *
032700900515      *-----------------------------------------------------***********
032800900515     C     PULIZ         BEGSR
032900900524      *
033000911001     C     RGF           IFNE      *BLANKS
033100911001     C                   MOVEL     RGF           V1RGF
033200911001     C                   MOVEL     NDS           V1NDT
033300911001     C                   ELSE
033400910617     C                   MOVEL     *ZEROS        V1RGF
033500910614     C                   MOVE      *ZEROS        V1NDT
033600911001     C                   END
033700941031     C                   MOVE      GMAUDT        V1DTA
033800910702     C                   MOVEL     *ZEROS        V1STA
033900910918     C                   MOVEL     'M'           §TPI
034000900514     C                   ENDSR
034100900515      *-----------------------------------------------------***********
034200010306      * CONTROLLI FORMATO 1                                 *  CTR1   *
034300900515      *-----------------------------------------------------***********
034400900515     C     CTR1          BEGSR
034500900515      *
034600910701     C                   SETOFF                                       404142
034700910701     C     V1RGF         IFNE      *ZEROS
034800941031     C                   Z-ADD     1             Y
034900941031     C     V1RGF         LOOKUP    FIL(Y)                                 20
035000941031     C  N20              SETON                                        4042
035100941031     C  N20              GOTO      FICTR1
035200910701     C                   END
035300941031     C                   MOVE      V1DTA         G08DAT
035400941031     C                   MOVEL     *BLANK        G08ERR
035500941031     C                   CALL      'XSRDA8'
035600941031     C                   PARM                    WLBDA8
035700941031     C                   MOVE      G08INV        AMGDTA            8 0
035800941031     C                   MOVE      G08DAT        V1DTA
035900941031     C     G08ERR        IFEQ      '1'
036000910614     C                   SETON                                        4041
036100910617     C                   END
036200910620     C     AMGDTA        IFGT      AMGUDT
036300910620     C                   SETON                                        4041
036400910620     C                   END
036500910617     C*
036600910701     C     V1RGF         COMP      *ZEROS                             30
036700910701     C     V1NDT         COMP      *ZEROS                             31
036800910701     C  N30
036900910701     CAN 31              SETON                                        4042
037000910617     C*
037100900515     C     FICTR1        ENDSR
037200900607      *-----------------------------------------------------***********
037300910521      * PULIZIA     SFL                                     *  PULSFL *
037400900607      *-----------------------------------------------------***********
037500910521     C     PULSFL        BEGSR
037600900607     C                   Z-ADD     1             NRR
037700910620     C                   MOVE      *ZEROS        TDT
037800941031     C                   MOVE      *ZEROS        TDTLIR
037900941031     C                   MOVE      *ZEROS        TDTVAL
038000910701     C                   MOVE      *ZEROS        SNRASS
038100120131     C                   MOVE      *ZEROS        conta             4 0
038200120131     C                   MOVE      *ZEROS        NRLIRE            4 0
038300941031     C                   MOVE      *ZEROS        NRVALU            4 0
038402120612     C                   MOVE      *ZEROS        TDTLIR1
038500120131     C                   MOVE      *ZEROS        TDTVAL1
038600120131     C                   MOVE      *ZEROS        SNRASS1
038700120131     C                   MOVE      *ZEROS        NRLIRE1           4 0
038800120131     C                   MOVE      *ZEROS        NRVALU1           4 0
038801120613     c                   clear                   multi             1
038900900607     C                   SETON                                        70
039000941028     C                   WRITE     SC70DCT
039100900607     C                   SETOFF                                       70
039200900607     C                   SETON                                        71
039300900607     C                   ENDSR
039400910521      *-----------------------------------------------------***********
039500910521      * COPIA CAMPI RECORD IN CAMPI SFL                     *  RIEMPI *
039600910521      *-----------------------------------------------------***********
039700910521     C     RIEMPI        BEGSR
039800910521      *
039900910521      * RIEMPIO IL SFL
040000910521      *
040100040907     C                   SETOFF                                       996061
040200120629     C                   SETOFF                                       986263
040300910521     C                   Z-ADD     1             NRR
040400910614     C                   MOVE      *BLANK        SSCE
040500910614     C*
040600910617     C                   MOVE      V1RGF         §RGF
040700910617     C                   MOVE      V1NDT         §NDT
040800910701     C* 31 ON - IMMESSO NR. DISTINTA
040900910617     C*
041000941031     C  N31KRGF          SETLL     TNCSB01L
041100941031     C   31KNDTP         SETLL     TNCSB01L
041200910617     C*
0413009801121--> C     *IN21         DOUEQ     '1'
041400910701     C* SOLO TPI
041500941031     C  N30KTPI          READE     TNCSB01L                               21
041600910617     C* SOLO FILIALE
041700941031     C   30
041800941031     CANN31KRGF          READE     TNCSB01L                               21
041900910617     C* FILIALE + NR. DISTINTA
042000941031     C   30
042100941031     CAN 31KNDTP         READE     TNCSB01L                               21
042200120131     C* aggiungere emissione finestra che indica il riempimento sfl
042300011106     c                   if        nrr >= 9998  and not *in21
042400011106     c                   seton                                        21
042500011106     c                   end
042600910617     C*
0427009801122--> C     *IN21         IFEQ      '0'
042800910617     C* PRIMA VOLTA
0429009801123--> C     *IN99         IFEQ      '0'
043000910617     C                   MOVE      DIST          DISTPR
043100910617     C                   SETON                                        99
043200980112 <--3C                   END
043300120131     c* a rottura di distinta
0434009801123--> C     DIST          IFNE      DISTPR
043500120131     c                   exsr      srtotdis
043600120131     C                   MOVE      DIST          DISTPR
043700120131     c                   clear                   multi             1
043800120131     C                   MOVE      *ZEROS        SNRASS
043900120131     C                   MOVE      *ZEROS        TDTLIR
044000120131     C                   MOVE      *ZEROS        TDTVAL
044102120612     C                   MOVE      *ZEROS        NRLIRE
044200120131     C                   MOVE      *ZEROS        NRVALU
044300120131     C                   MOVE      *ZEROS        TDTLIR1
044400120131     C                   MOVE      *ZEROS        TDTVAL1
044500120131     C                   MOVE      *ZEROS        NRLIRE1
044600120131     C                   MOVE      *ZEROS        NRVALU1
044700980112 <--4C                   END
044800120131      * dettaglio CONTROLLO SE REC. DA CONSIDERARE :
044900120131      *           TIPO ASS.=MITT.
045000120131      *           DEVE ESSERE DEL RAMO AZIENDALE A CUI SI è COLLEGATI
045100980112     C                   Z-ADD     1             Y
045200980112     C     CSBRGF        LOOKUP    FIL(Y)                                 20
0453009801123--> C   20CSBTPI        IFEQ      'M'
045400910702     C     CSBSTA        ANDEQ     V1STA
045500120131     c*
045600120126     c     ' '           scan      csbnra                                 88
045700120126     c                   if        *in88
045800120131     c* emetto errore se non ho ri. assegno
045900040907     c                   if        csbabi = 0 or csbcab = 0 or
046000040907     c                             csbnra = ' '
046100040907     c                   seton                                        61
046200040907     c                   end
046300120126     c                   else
046400120126     c* mi memorizzo che per questa distinta ci sono dei multiassegni
046500120207     c* in modo da sommare gli assegni a rottura di distinta
046600120126     c                   eval      multi = '1'
046700120131     c                   seton                                        62
046701120629     c* verifiche se ho ragioni sociali diverse, lo segnalo
046702120629     c                   if        not *in63
046703120629     c                   eval      innra = csbnra
046704120629     c                   clear                   outnrcli
046705120629     c                   call      'TNSC76R1'
046706120629     C                   PARM                    innra            10
046707120629     C                   PARM                    outnrcli          2 0
046708120629     c                   if        outnrcli > 1
046709120629     c                   seton                                        63
046800120126     c                   end
046801120629     c                   end
046802120629     c                   end
046900040907     c*
047000941031     C                   ADD       CSBCAS        TDT
047100941031     C* CONTROLLO LA DIVISA
047200941031     C                   CLEAR                   DSCV
0473009801124--> C     CSBVCA        IFNE      *BLANKS
047400941031     C                   Z-ADD     1             Y
047500941031     C     CSBVCA        LOOKUP    TCV(Y)                                 22
047600941031     C   22              MOVEL     DCV(Y)        DSCV
047700941031     C                   ELSE
047800941031     C                   MOVEL     'I'           §CVITA
047900980112 <--4C                   END
048000941031      *
048100011018     C                   IF        CsbVca = §GedCn
048200941031     C                   ADD       CSBCAS        TDTLIR
048300941031     C                   ADD       1             NRLIRE
048400941031     C                   ELSE
048500941031     C                   ADD       CSBCAS        TDTVAL
048600941031     C                   ADD       1             NRVALU
048700980112 <--4C                   END
048800980112 <--3C                   END
048900980112 <--2C                   END
049000980112 <--1C                   END
049100910617     C* ULTIMA DISTINTA
049200910617     C* TOTALE DISTINTA > 0  SCRIVO SFL
049300910617      *--------------------------------
049400120126     c                   if        *in21
049500120131     c                   exsr      srtotdis
049600980112 <--1C                   END
049700910521     C                   ENDSR
049800120126      *-----------------------------------------------------***********
049900120126      * conta i multiassegni
050000120126      *-----------------------------------------------------***********
050100120131     C     srtotdis      BEGSR
050101120612     c                   clear                   tdtval1
050102120612     c                   clear                   nrvalu1
050103120612     c                   clear                   tdtlir1
050104120612     c                   clear                   nrlire1
050200120131     c* se ho dei multiassegni li sommo ora
050300120131     c                   if        multi = '1'
050400120131     c                   exsr      srconta
050401120613     c                   clear                   multi             1
050500120131     c                   end
050600120131     C* TOTALE DISTINTA > 0  SCRIVO SFL
050700120131      *--------------------------------
0508001201314--> C     TDT           IFNE      *ZEROS
050900120131     C                   MOVEL     *BLANK        SDRGF
051000120131     C                   Z-ADD     1             Y
051100120131     C     PRERGF        LOOKUP    FIL(Y)                                 20
051200120131     C   20              MOVEL     DFI(Y)        SDRGF
051300120131     C  N20              MOVEL     *ALL'?'       SDRGF
051400120131     C                   MOVEL     PRERGF        SRGF
051500120131     C                   MOVE      PREDDC        G08INV
051600120131     C                   MOVEL     '3'           G08ERR
051700120131     C                   CALL      'XSRDA8'
051800120131     C                   PARM                    WLBDA8
051900120131     C                   MOVE      G08DAT        SDDC
052000120131     C                   Z-ADD     PRENDT        SNDT
052100120131     C                   SETOFF                                       60
052200120131      * SCRIVO 2 TOTALI: IN LIRE E IN VALUTA
0523001201315--> C     NRLIRE        IFGT      0
052400120131     C                   SETOFF                                       60
052500120131     C                   Z-ADD     NRLIRE        SNRASS
052600120131     C                   Z-ADD     TDTLIR        STDT
052700120131     C                   EVAL      SDIVIS = §GedCn
052800120131     c                   if        nrlire1 = 0
052900120131     C                   Z-ADD     NRLIRE        SNRASS1
053000120131     C                   Z-ADD     TDTLIR        STDT1
053100120131     C                   EVAL      SDIVIS1 = §GedCn
053200120131     c                   else
053300120131     C                   Z-ADD     NRLIRE1       SNRASS1
053400120131     C                   Z-ADD     TDTLIR1       STDT1
053500120131     C                   EVAL      SDIVIS1 = §GedCn
053600120131     c                   end
053700120131     c                   movel     *in60         ind60
053800120131     c                   movel     *in61         ind61
053900120131     c                   movel     *in62         ind62
053901120629     c                   movel     *in63         ind63
054000120131     C                   WRITE     SC70DSF
054100120131     C                   ADD       1             NRR
054200120629     c                   setoff                                       616263
054300120131 <--5C                   END
0544001201315--> C     NRVALU        IFGT      0
054500120131      * NON VISUAL. DISTINTA (60ON) SOLO SE CI SONO ASSEGNI IN LIRE
0546001201316--> C     NRLIRE        IFGT      0
054700120131     C                   SETON                                        60
054800120131     C                   ENDIF
054900120131     C                   Z-ADD     NRVALU        SNRASS
055000120131     C                   Z-ADD     TDTVAL        STDT
055100120131     C                   MOVEL     'Val.'        SDIVIS
055200120131     c                   if        nrvalu1 = 0
055300120131     C                   Z-ADD     NRVALU        SNRASS1
055400120131     C                   Z-ADD     TDTVAL        STDT1
055500120131     C                   MOVEL     'Val.'        SDIVIS1
055600120131     c                   else
055700120131     C                   Z-ADD     NRVALU1       SNRASS1
055800120131     C                   Z-ADD     TDTVAL1       STDT1
055900120131     C                   MOVEL     'Val.'        SDIVIS1
056000120131     c                   end
056100120131     c                   movel     *in60         ind60
056200120131     c                   movel     *in61         ind61
056300120131     c                   movel     *in62         ind62
056301120629     c                   movel     *in63         ind63
056400120131     C                   WRITE     SC70DSF
056500120131     C                   ADD       1             NRR
056600120629     c                   setoff                                       616263
056700120131 <--6C                   END
056800120131 <--5C                   END
056900120131     C                   MOVE      *ZEROS        TDT
057000120131     C                   ENDSR
057100120131      *-----------------------------------------------------***********
057200120131      * conta i multiassegni
057300120131      *-----------------------------------------------------***********
057400120131     C     srconta       BEGSR
057900120131     C* conta gli assegni multipli in divisa
058000120131     c                   if        nrvalu <> 0
058100120131     C/EXEC SQL
058200120131     C+   select  sum(csmidc), count(*) into :impo, :conta
058300120131     C+   from (SELECT DISTINCT csbnra from tncsb00f
058400120131     C+   where csbrgf = :PRERGF and csbndt = :PRENDT and csbddc =
058500120220     C+   :PREDDC and substr(csbnra, 10, 1) <> ' ' and csbabi = 0 and
058600120131     C+   csbsta = :v1sta and csbtpi = 'M' and csbvca <> :§GedCn
058601120314     c+   and csbdta = 0
058700120131     C+   group by csbnra) as pippo join tncsm00f a on
058800120131     C+   pippo.csbnra = a.csmkey
058900120131     C/END-EXEC
059000120131     C                   eval      tdtval1 = impo
059100120131     c                   eval      nrvalu1 = conta
059200120131     c                   clear                   conta
059300120131     c                   clear                   impo
059400120131     C* conta gli assegni singoli in valuta
059500120131     C/EXEC SQL
059600120131     C+   select  sun(csbcas), count(*) into :impo, :conta
059700120207     C+   from tncsb00f
059800120131     C+   where csbrgf = :PRERGF and csbndt = :PRENDT and csbddc =
059900120220     C+   :PREDDC and substr(csbnra, 10, 1) = ' ' and csbabi <>0 and
060000120131     C+   csbcab <> 0 and csbsta = :v1sta and csbtpi = 'M'
060100120131     C+   and csbvca <> :§gedcn
060101120314     c+   and csbdta = 0
060200120131     C/END-EXEC
060300120131     c                   add       conta         nrvalu1
060400120131     c                   add       impo          tdtval1
060500120131     c                   end
060600120131     C* conta gli assegni multipli in eur
060700120131     c                   if        nrlire <> 0
060800120131     C/EXEC SQL
060900120131     C+   select  sum(csmidc), count(*) into :impo, :conta
061000120131     C+   from (SELECT DISTINCT csbnra from tncsb00f
061100120131     C+   where csbrgf = :PRERGF and csbndt = :PRENDT and csbddc =
061200120220     C+   :PREDDC and substr(csbnra, 10, 1) <> ' ' and csbabi = 0 and
061300120131     C+   csbsta = :v1sta and csbtpi = 'M' and csbvca = :§gedcn
061301120314     c+   and csbdta = 0
061400120131     C+   group by csbnra) as pippo join tncsm00f a on
061500120131     C+   pippo.csbnra = a.csmkey
061600120131     C/END-EXEC
061700120131     c                   eval      nrlire1 = conta
061800120131     c                   eval      tdtlir1 = impo
061900120131     c                   clear                   conta
062000120131     c                   clear                   impo
062100120131     C* conta gli assegni singoli in EUR
062200120131     C/EXEC SQL
062300120131     C+   select  sum(csbcas), count(*) into :impo, :conta
062400120207     C+   from tncsb00f
062500120131     C+   where csbrgf = :PRERGF and csbndt = :PRENDT and csbddc =
062600120220     C+   :PREDDC and substr(csbnra, 10, 1) = ' ' and csbabi <>0 and
062700120131     C+   csbcab <> 0 and csbsta = :v1sta and csbtpi = 'M'
062800120131     C+   and csbvca = :§GedCn
062801120314     c+   and csbdta = 0
062900120131     C/END-EXEC
063000120131     c                   add       conta         nrlire1
063100120131     c                   add       impo          tdtlir1
063200120131     c                   end
063300120126     C                   ENDSR
063400900515      *-----------------------------------------------------***********
063500900524      * CONTROLLI SUBFILE                                   *  CTR2   *
063600900515      *-----------------------------------------------------***********
063700900515     C     CTR2          BEGSR
063800900515      *
063900040907     c                   setoff                                       98
064000900524      *
064100910614     C     *IN20         DOUEQ     '1'
064200941028     C                   READC     SC70DSF                                20
064300941117     C* AGGIUNTA C/ASS.  - CMD10
064400941128     C   KJ              MOVE      KPJBU         KPJBUS
064500941128     C   KJ              MOVE      *BLANK        KPJBU
064600941107     C   KJ              CALL      'TNSC03R'
064700911001     C                   PARM                    KPJBA
064800941128     C   KJ              MOVE      KPJBUS        KPJBU
064900941107     C   KJ              GOTO      FICTR2
065000941117     C* MODIF. C/ASS     - CMD14
065100941128     C   KN              MOVE      KPJBU         KPJBUS
065200941128     C   KN              MOVE      *BLANK        KPJBU
065300941117     C   KN              CALL      'TNSC95R'
065400941117     C                   PARM                    KPJBA
065500941128     C   KN              MOVE      KPJBUS        KPJBU
065600941117     C   KN              GOTO      FICTR2
065700941107     C* INTERR.  BOLLE   - CMD08
065800941128     C   KH              MOVE      KPJBU         KPJBUS
065900990729     C   KH              CLEAR                   TNSB50
066000941128     C   KH              MOVE      *BLANK        KPJBU
066100990729     C   KH              MOVEL     'C00'         I50OP0
066200990729     C   KH              MOVEL     TNSB50        KPJBU
066300990729     C   KH              CALL      'TNSB50R'
066400911001     C                   PARM                    KPJBA
066500941128     C   KH              MOVE      KPJBUS        KPJBU
066600941107     C   KH              GOTO      FICTR2
066700941107     C* INTERR. C/ASS.   - CMD07
066800941128     C   KG              MOVE      KPJBU         KPJBUS
066900941128     C   KG              MOVE      *BLANK        KPJBU
067000950720     C   KG              CALL      'TNSC40R'
067100911001     C                   PARM                    KPJBA
067200941128     C   KG              MOVE      KPJBUS        KPJBU
067300941031     C   KG              GOTO      FICTR2
067400900524      *-------------
067500910620     C     *IN20         IFEQ      '0'
067600941117      *-------------   DETTAGLIO SPEDIZIONI  DELLA DISTINTA SCELTA CMD02
067700941117      *-------------   DETTAGLIO ALTRE SPEDIZIONI NON INCASS CMD1
067800941117     C     *INKA         IFEQ      '1'
067900941117     C     *INKB         OREQ      '1'
068000941031     C     SSCE          IFEQ      '1'
068100910701     C                   MOVEL     §PGM          PARPGM
068200910701     C                   MOVEL     *BLANKS       PARERR
068300910701     C                   MOVE      SRGF          PARRGF
068400910918     C                   MOVE      SNDT          PARNDT
068500941031     C                   MOVE      SDDC          G08DAT
068600941031     C                   MOVEL     *BLANK        G08ERR
068700941031     C                   CALL      'XSRDA8'
068800941031     C                   PARM                    WLBDA8
068900941031     C                   MOVE      G08INV        PARDDC
069000941117     C   KB              MOVEL     §TPI          PARTPI
069100941117     C   KB              MOVEL     '14'          PARCMD
069200941117     C   KA              MOVEL     ' '           PARTPI
069300941117     C   KA              MOVEL     '13'          PARCMD
069400910702     C                   MOVE      V1STA         PARSTA
069500941117     C   KBV1STA         IFEQ      0
069600910702     C                   MOVEL     'SSMA'        PARCAV
069700910918     C                   MOVEL     '01'          PARTXT
069800910702     C                   ELSE
069900910702     C                   MOVEL     'RSMA'        PARCAV
070000910702     C                   MOVEL     *BLANK        PARTXT
070100910702     C                   END
070200941128     C                   MOVE      KPJBU         KPJBUS
070300941028     C                   MOVEL     TNSC71        KPJBU
070400941028     C                   CALL      'TNSC71R'
070500910617     C                   PARM                    KPJBA
070600941128     C                   MOVE      KPJBUS        KPJBU
070700910701     C     PARERR        COMP      'E'                                    45
070800910617     C                   END
070900910919     C                   END
071000900516      *
071100011106     c                   movel     ind60         *in60
071200040907     c                   movel     ind61         *in61
071300120131     c                   movel     ind62         *in62
071301120629     c                   movel     ind63         *in63
071400040907     c                   if        *in61 and ssce = '1'
071500040907     c                   seton                                        98
071600040907     c                   end
071700980302      *
071800910701     C                   SETON                                        72
071900941028     C                   UPDATE    SC70DSF
072000011106     C                   SETOff                                       72
072100910620     C                   END
072200910617     C                   END
072300900515     C     FICTR2        ENDSR
072400900515      *-----------------------------------------------------***********
072500910614      * AGGIORNAMENTO DATA ARRIVO DISTINTA                  *  AGGIOR *
072600900515      *-----------------------------------------------------***********
072700900607     C     AGGIOR        BEGSR
072800060530     c                   seton                                        46
072900900515      *
073000910617     C     *IN20         DOUEQ     '1'
073100941028     C                   READC     SC70DSF                                20
073200900524      *-------------
073300941031     C  N20SSCE          IFEQ      '1'
073400060530     c                   setoff                                       46
073500910617     C                   MOVE      SRGF          §RGF
073600910617     C                   MOVE      SNDT          §NDT
073700941031     C                   MOVE      SDDC          G08DAT
073800941031     C                   MOVEL     *BLANK        G08ERR
073900941031     C                   CALL      'XSRDA8'
074000941031     C                   PARM                    WLBDA8
074100941031     C                   MOVE      G08INV        §DDC
074200060524     c                   move      '0'           errfil            1
074300060628     c*
074400941031     C     KNDTC         SETLL     TNCSB01L
074500910617     C     *IN21         DOUEQ     '1'
074600941031     C     KNDTC         READE     TNCSB01L                               21
074700910614     C*  PER TUTTI I RECORD DELLA DISTINTA
074800910620     C     *IN21         IFEQ      '0'
074900910920     C     CSBSTA        ANDNE     5
075000910620     C     CSBTPI        ANDEQ     'M'
075100060515     c* se scelto di aggiornare la filiale richiamo la funzione che chiude
075200060515     c* la spedizione in filiale
075300060522     c                   if        (kcdaz = 'SC7B' or kcdaz = 'Y36B')
075400060522     c                             and aggfil = 'S'
075500060515     c                   exsr      sraggfil
075600060522     c* se errore in filiale segnalo
075700060524     c                   if        *in45 and errfil = '0'
075800060522     c                   move      *in45         errfil
075900060627     c                   seton                                        72
076000060522     c                   update    SC70DSF
076100060515     C                   END
076200060522     C                   END
076300060522     c* se c'è errore non aggiorno c/ass
076400060522     c                   if        not *in45
076500910620     C                   MOVE      AMGDTA        CSBDTA
076600940415     C                   MOVE      'S'           CSBMAS
076700910620     C                   EXCEPT    AGGCSB
076800060522     C                   END
076900910617     C                   END
077000060512     C                   ENDdo
077100060522     c* se c'è errore non proseguo
077200060522     c                   if        errfil ='1'
077300060522     c                   goto      fineagg
077400060522     c                   end
077500060512     c* controllo se sono state escluse delle spedizioni, per ripristinarle
077600060517     c* solo se il numero della distinta in entrata è diverso
077700060512     C                   MOVEl     9             §NDT
077800060517     c                   if        sndt <> §ndt
077900060512     C     KNDTC         SETLL     TNCSB01L
078000060512     C     *IN21         DOUEQ     '1'
078100060512     C     KNDTC         READE     TNCSB01L                               21
078200060512     C*  PER TUTTI I RECORD DELLA DISTINTA
078300060512     C     *IN21         IFEQ      '0'
078400060512     C     CSBSTA        ANDNE     5
078500060512     C     CSBTPI        ANDEQ     'M'
078600060516     C     CSBdta        ANDEQ     0
078700060512     C                   MOVEl     0             CSBndt
078800060512     C                   update    tncsb000
078900060512     C                   END
079000060512     C                   ENDdo
079100060517     C                   END
079200060517     C                   MOVE      SNDT          §NDT
079300900524      *---------
079400900524     C                   END
079500060512     C                   ENDdo
079600060515     C     fineagg       ENDSR
079700060515     C*-------------------------------------------------------*
079800060515     C* Aggiorna gli archivi gestionali
079900060515     C*-------------------------------------------------------*
080000060515     C     sraggfil      BEGSR
080100060524     c                   setoff                                       45
080200060515     C* Richiamo TIBS55R per conoscere S.Informativi
080300060515     C* in cui immettere il lavoro
080400060515     C                   CLEAR                   TIBS55ds
080500060515     C                   MOVEL     'L'           £I50TLA
080600060515     C                   MOVEL     '046'         £I50PPO
080700060515     C                   MOVEL     '001'         £I50APO
080800060515     C                   CALL      'TIBS55R'
080900060515     C                   PARM                    TIBS55ds
081000060515     C* MI SALVO IL S.I. ORIGINALE XCHÈ SE NO NON LO REIMPOSTA
081100060515     c                   clear                   param
081200060515     c                   eval      sifda = KNSIF
081300060515     c                   eval      sifa = £O50ASI
081400060519     c                   eval      parca= 'C'
081500060519     c                   eval      kaas = csbaas
081600060515     c                   eval      klnp = csblnp
081700060515     c                   eval      knrs = csbnrs
081800060515     c                   eval      knsp = csbnsp
081900060515     C                   MOVEL(p)  PARAM         KPJBU
082000060515     c                   call      'CNC0W13C'
082100060515     C                   PARM                    KPJBA
082200060515     c                   movel     kpjbu         param
082300130308     c                   if        paresito <> *blank
082400060627     c                   seton                                        45
082500060515     c                   else
082600060515     c                   setoff                                       45
082700060515     c                   end
082800060515     C*
082900060515     C                   ENDSR
083000060512     C**-------------------------------------------------------------
083100060512     C** srwin - domnda se aggiornare filiale
083200060512     C**-------------------------------------------------------------
083300060512     C     srwin         BEGSR
083400060512     c                   do        *hival
083500060512     C                   EXFMT     SC70W01
083600060515     c                   if        *inkl
083700060515     c                   clear                   aggfil
083800060515     c                   leave
083900060515     c                   end
084000060512     c                   if        aggfil = ' '
084100060512     c                   seton                                        44
084200060512     c                   iter
084300060512     c                   else
084400060512     c                   setoff                                       44
084500060512     c                   end
084600060512     c                   if        *inkf
084700060512     c                   leave
084800060512     c                   end
084900060512     c                   enddo
085000060512     c                   endsr
085100941031     OTNCSB000  E            AGGCSB
085200910617     O                       CSBDTA
085300940415     O                       CSBMAS
