000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200011127     H*--------------------------------------------------------------*
000300000000     H*             - IMMISSIONE SINISTRI -                          *
000400000000     H*--------------------------------------------------------------*
000500011127     FTRmz26D   CF   E             WORKSTN
000600000000     F                                     SFILE(GA26DSF1:NRR)
000700011212     Fmznot01L  IF   E           K DISK
000800011210     FmzSIN01L  IF   E           K DISK
000900011203     F                                     INFDS(sinINF)
001000011210     FmzSIN02L  UF   E           K DISK
001100011210     F                                     RENAME(mzSIN000:mzSIN002)
001200011127     FmzAME01L  IF   E           K DISK
001300011127     F                                     RENAME(mzAME000:mzAME1)
001400011127     FmzAME02L  IF   E           K DISK
001500891106     FTABEL00F  IF   E           K DISK
001600011203     D sinINF          DS
001700011203     D  sinNRR               397    400B 0
001800011127     D*GESTIONE SINISTRO
001900000000     D PARAM           DS
002000011127     D  parnrf                        7  0
002100011203     D  parnsi                        9  0
002200011203     D  parnig                        9  0
002300011128     D  parflg                        1
002400011127     c* modalità con cui chiamo il pgm
002500011127     D  parmod                        1
002600011127     c* Cmd di ritorno
002700011127     D  parret                        1
002800011127     D*GESTIONE NOTE
002900000000     D PAR191          DS
003000011129     D* numero riferimento
003100011129     D  P91nrf                        7  0
003200011129     D* tipo nota
003300011129     D  P91tip                        2
003400011129     D* numero nota
003500011129     D  P91NOT                        7  0
003600011127     c* modalità con cui chiamo il pgm
003700011127     D  p91mod                        1
003800011127     c* Cmd di ritorno
003900011127     D  p91ret                        1
004000000000      *
004100000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
004200000000     D KPJBA         E DS
004300011127      *
004400011127     D DTAiso          S               D   INZ
004500011127     D DTAEUR          S               D   DATFMT(*EUR) INZ
004600011127     D DTADAL          S               D   INZ
004700011127     D DTAAL           S               D   INZ
004800000000     C*----------------------------------------------------*
004900000000      * INDICATORI
005000000000      *
005100011127      * KC FINE LAVORO
005200011127      * KD RICERCA
005300011127      * KF IMMISSIONE NUOVO SINISTRO/CONFERMA
005400000000      * 06 READC SFL
005500000000      * 22 SERVIZIO
005600000000      * 50 N.RIFERIMENTO ERRATO
005700000000      * 51 N.TARGA ERRATO
005800000000      * 52 DATA ERRATA (DAL)
005900000000      * 53 DATA ERRATA (AL)
006000000000      * 61 PROTECT SCEL
006100000000      * 70 PULIZIA SUBF1
006200011210      * 80 SETLL E READE mzSIN
006300000000      * 89 ALTA INTENSITA'
006400000000     C*----------------------------------------------------*
006500011127     C                   IF        NOT *INLR
006600000000     C                   EXSR      AZZER
006700011127     C*
006800011127     C                   DO        *HIVAL
006900011127     c                   EXSR      CLR00
007000011127     c                   if        *in99
007100011127     c                   setoff                                       99
007200011127     c                   write     ga26d001
007300011127     c                   seton                                        99
007400011127     c                   end
007500000000     C                   EXFMT     GA26D001
007600000000     C                   SETOFF                                       99
007700011127     C* FINE LAVORO
007800011127     C   KC              LEAVE
007900011127     C* RICERCA
008000011127     C   KD              EXSR      RIC01
008100150918      *
008200150918     C* anagrafica automezzi
008300150918     c   kg              call      'TRMZ02R'
008400150918     c                   parm                    kpjba
008500150918     C   kg              ITER
008600150918      *
008700000000     C* SCELTA N.RIF. O
008800011127     C                   EXSR      CTR01
008900011127     C   99              ITER
009000000000     C* NUOVO SINISTRO
009100011127     C                   IF        *INKF
009200011203     C                   Z-ADD     0             hnrr
009300011127     C                   EXSR      IMMSI
009400011127     C                   ITER
009500011127     C                   END
009600000000     C* PULIZIA SUBFILE
009700000000     C                   EXSR      PULIZ
009800000000     C* CARICAMENTO SUBFILE
009900000000     C                   EXSR      CARIC
010000011127     C                   DO        *HIVAL
010100000000     C     SVNRR         COMP      0                                  7171
010200000000     C                   WRITE     GA26D002
010300000000     C                   EXFMT     GA26DSC1
010400011127     C* FINE  LAVORO
010500011127     C   KC              LEAVE
010600011128     C* FINE  LAVORO
010700011128     C   Kl              LEAVE
010800000000     C* NUOVO SINISTRO
010900011127     C                   IF        *INKF
011000011203     C                   Z-ADD     0             hnrr
011100011127     C                   EXSR      IMMSI
011200011127     C     PARRET        IFEQ      'F'
011300011127     c                   exsr      puliz
011400011127     c                   exsr      caric
011500011127     C                   END
011600011127     C                   ITER
011700000000     C                   END
011800000000     C* CONTROLLI
011900000000     C     SVNRR         IFNE      0
012000011127     C                   EXSR      CONsfl
012100000000     C                   END
012200011127     C                   ENDDO
012300011203     c                   if        *inkl
012400011203     c                   exsr      azzer
012500011203     C                   ENDIF
012600011127     C   KC              LEAVE
012700011127     C                   ENDDO
012800011127     C                   ENDIF
012900011127     c*
013000000000     C                   SETON                                        LR
013100011127     c**********************************************************************
013200011127     c* pulisce 1° formato
013300011127     c**********************************************************************
013400011127     C     clr00         BEGSR
013500011127     C                   Z-ADD     0             §RIGA
013600011127     C                   Z-ADD     0             §COLON
013700011127     C                   CLEAR                   §FLD
013800011127     C                   ENDSR
013900011127     C******************************************************
014000011127     C* CONTROLLI PRIMO VIDEO
014100011127     C******************************************************
014200011127     C     CTR01         BEGSR
014300011127     C                   DO
014400011127     C* N.TARGA
014500011127     C     VNRF          IFEQ      0
014600011127     C     VTGA          ANDEQ     *BLANKS
014700011127     C                   SETON                                        5099
014800011127     C                   LEAVE
014900011127     C                   END
015000011127     C     VNRF          IFNE      0
015100011127     C     VTGA          ANDNE     *BLANKS
015200011127     C                   MOVE      *BLANKS       VTGA
015300011127     C                   MOVE      *BLANKS       VTGN
015400011127     C                   END
015500011127     C* CONTROLLO TARGA
015600011127     C     VTGA          IFNE      *BLANKS
015700011127     C     VTGN          ANDNE     *BLANKS
015800011127     C     KTAR          CHAIN     MZAME000                           51
015900011127     C                   IF        NOT *IN51
016000011127     C                   Z-ADD     AMENRF        VNRF
016100011127     C                   ELSE
016200011127     C                   SETON                                        99
016300011127     C                   LEAVE
016400011127     C                   END
016500011127     C                   END
016600011127     C     VNRF          CHAIN     MZAME1                             50
016700011127     C                   IF        NOT *IN50
016800011127     C                   MOVEL     AMEMAR        VMAR
016900011127     C                   MOVEL     AMETIP        VTIP
017000011127     C     VTGA          IFEQ      *BLANKS
017100011127     C                   MOVEL     AMETGA        VTGA
017200011127     C                   MOVEL     AMETGN        VTGN
017300011127     C                   END
017400011127     C                   END
017500011128     C   50              MOVEL     'STORICO'     VTGN
017600011127     C* CONTROLLO DATE
017700011127     C                   MOVEL     *LOVAL        DTADAL
017800011127     C                   IF        VDAL <> 0
017900011127     C     *EUR          test(d)                 VDAL                   52
018000011127     c                   if        *in52
018100011127     C                   SETON                                        99
018200011127     C                   leave
018300011127     C                   END
018400011127     C                   MOVEL     VDAL          DTAEUR
018500011127     C                   MOVEL     DTAEUR        DTADAL
018600011127     C                   END
018700011127     C* CONTROLLO AL
018800011127     C                   MOVEL     *LOVAL        DTAAL
018900011127     C                   IF        VAL <> 0
019000011127     C     *EUR          test(d)                 VAL                    53
019100011127     c                   if        *in53
019200011127     C                   SETON                                        99
019300011127     C                   leave
019400011127     C                   END
019500011127     C                   MOVEL     VAL           DTAEUR
019600011127     C                   MOVEL     DTAEUR        DTAAL
019700011127     C                   END
019800011127     C*
019900011127     C                   IF        (VDAL <> 0 OR VAL <> 0) AND
020000011127     C                              DTADAL > DTAAL
020100011127     C                   SETON                                        525399
020200011127     C                   END
020300011127     C                   ENDDO
020400011127     C*
020500011127     C                   ENDSR
020600011127     C******************************************************
020700011127     C* RICERCA PRIMO VIDEO
020800011127     C******************************************************
020900011127     C     RIC01         BEGSR
021000011130     c                   select
021100011221     C                   when      §fld = 'VTGN' or §fld = 'VTGA'
021200011221     C                   CALL      'TRMZ02R4'    RICTAR
021300011130     C                   ENDsl
021400011130     C* POSIZIONAMENTO CURSORE
021500011130     C                   Z-ADD     §RIGA         §RIG
021600011130     C                   Z-ADD     §COLON        §COL
021700011127     C                   ENDSR
021800011127     C******************************************************
021900011127     C** CHIAMATA AL PGM DI GESTIONE SINISTRO
022000011127     C******************************************************
022100011127     C     IMMSI         BEGSR
022200011127     c*
022300011127     c                   clear                   param
022400011127     c                   move      vnrf          parnrf
022500011203     c                   move      hnrr          parnsi
022600011127     c                   if        not *inkf
022700011127     c                   move      scel          parmod
022800011127     c                   end
022900011127     c                   movel(P)  param         kpjbu
023000011127     C                   CALL      'TRMZ26R2'
023100000000     C                   PARM                    KPJBA
023200011127     C                   MOVEL(p)  KPJBU         PARAM
023300011127     c*
023400011127     C                   ENDSR
023500011127     C******************************************************
023600011127     C** PULIZIA DEL SUBFILE
023700000000     C******************************************************
023800000000     C     PULIZ         BEGSR
023900011127     c*
024000000000     C                   SETON                                        70
024100000000     C                   SETOFF                                       71
024200000000     C                   WRITE     GA26DSC1
024300000000     C                   SETOFF                                       70
024400000000     C                   SETON                                        71
024500011127     c*
024600000000     C                   ENDSR
024700000000     C******************************************************
024800000000     C** CARICAMENTO SUBFILE
024900000000     C** CON LETTURA X N.RIFER.
025000011127     C******************************************************
025100011127     C     CARIC         BEGSR
025200011127     c*
025300000000     C                   SETOFF                                           89
025400000000     C                   Z-ADD     0             NRR               4 0
025500011127     C                   move      Dtadal        SINDSI
025600011128     c                   if        val <> 0
025700011127     C                   move      Dtaal         al                8 0
025800011128     c                   else
025900011128     c                   z-add     99999999      al
026000011128     c                   end
026100011210     C     KSIN          SETLL     mzSIN000
026200011127     c                   do        *hival
026300011210     C     VNRF          READE     mzSIN000                               80
026400011127     C  n80SINDSI        COMP      AL                                 80
026500011127     c   80              leave
026600011127     C                   MOVEL     *BLANKS       SCEL
026700011203     C                   MOVE      SINnrr        hnrr
026800011203     C                   MOVE      SINDSI        Datsin            8 0
026900011127     C                   MOVE      SINDSI        Dtaiso
027000011127     C                   MOVE      dtaiso        Dtaeur
027100011127     C                   move      Dtaeur        SF1DSI
027200011127     C                   MOVEL     SINAUS        SF1AUS
027300011127     C                   MOVE      SINNOT        SF1NOT
027400011127     C* FLAG DI NOTA AGGIUNTIVA
027500011127     C                   MOVE      ' '           SF1D
027600011127     C     SF1NOT        IFNE      0
027700011212     c     knot          chain     mznot01l
027800011212     c                   if        %found(mznot01l)
027900011127     C                   MOVE      'D'           SF1D
028000011212     c                   else
028100011212     C                   MOVE      ' '           SF1D
028200011212     c                   z-add     0             sf1not
028300011127     C                   END
028400011212     C                   END
028500011127     C* decodifica autista
028600011127     C                   MOVEL     'XN'          COD
028700011127     C                   MOVEL     *BLANKS       KEY
028800011127     C                   MOVEL     SF1AUS        KEY
028900011127     C     KTAB          CHAIN     TABEL                              21
029000011127     C  N21              MOVEL     TBLUNI        SF1DAU
029100011127     C                   MOVEL     SINATT        SF1ATT
029200011127     C     SF1ATT        COMP      'A'                                    89
029300011127     C                   MOVEL     SINDE1        SF1DE1
029400011127     C                   MOVEL     SINDE2        SF1DE2
029500011127     C                   ADD       1             NRR
029600011127     C                   WRITE     GA26DSF1
029700011127     C                   ENDdo
029800000000     C                   Z-ADD     NRR           SVNRR             4 0
029900000000     C                   ENDSR
030000000000     C******************************************************
030100000000     C** CONTROLLI
030200011127     C******************************************************
030300011127     C     CONsfl        BEGSR
030400011127     c*
030500000000     C                   READC     GA26DSF1                               06
030600000000     C     *IN06         DOWNE     '1'
030700011221     C                   IF        scel = '2' or scel = '4' or scel = '5'
030800011127     C                   EXSR      immsi
030900011127     c                   exsr      puliz
031000011127     c                   exsr      caric
031100011129     c                   leave
031200000000     C                   END
031300000000     C* NOTE AGGIUNTIVE
031400000000     C     SCEL          IFEQ      'D'
031500011127     C                   clear                   PAR191
031600011129     C                   Z-ADD     vnrf          P91Nrf
031700011129     C                   Z-ADD     SF1NOT        P91NOT
031800011221     c* manutenzione note solo se non sono in visualizzazione
031900011221     c                   if        not *in20
032000011127     c                   if        sf1not <> 0
032100011127     C                   MOVE      '2'           P91mod
032200011127     c                   end
032300011221     c                   else
032400011221     C                   MOVE      '5'           P91mod
032500011221     c                   end
032600011127     C                   MOVEL(p)  PAR191        KPJBU
032700011129     C                   CALL      'TRMZ26R3'
032800000000     C                   PARM                    KPJBA
032900011127     C                   MOVEL(p)  KPJBU         PAR191
033000000000     C                   MOVE      ' '           SCEL
033100011127     c* modifica/inserimento nota
033200011129     c                   if        p91ret = 'F'
033300011127     C                   z-add     P91NOT        SF1NOT
033400011210     C     sinnsi        CHAIN     mzSIN002                           21
033500011127     c                   if        not *in21
033600011127     C                   MOVE      p91NOT        SINNOT
033700011210     C                   UPDATE    mzSIN002
033800011127     C                   END
033900011129     c                   end
034000011127     C                   END
034100011127     c*
034200000000     C     SF1NOT        IFNE      0
034300000000     C                   MOVE      'D'           SF1D
034400011127     c                   else
034500011127     C                   MOVE      ' '           SF1D
034600000000     C                   END
034700000000     C     SF1ATT        COMP      'A'                                    89
034800000000     C                   UPDATE    GA26DSF1
034900000000     C                   SETOFF                                           61
035000000000     C                   READC     GA26DSF1                               06
035100000000     C                   END
035200011127     c*
035300011127     C                   ENDSR
035400011127     C******************************************************
035500000000     C** PULIZIA CAMPI DS
035600011127     C******************************************************
035700011127     C     AZZER         BEGSR
035800000000     C                   Z-ADD     0             VNRF
035900000000     C                   MOVEL     *BLANKS       VTGA
036000000000     C                   MOVEL     *BLANKS       VTGN
036100000000     C                   MOVEL     *BLANKS       VMAR
036200000000     C                   MOVEL     *BLANKS       VTIP
036300000000     C                   Z-ADD     0             SF1DSI
036400000000     C                   MOVEL     *BLANKS       SF1AUS
036500000000     C                   MOVEL     *BLANKS       SF1ATT
036600000000     C                   MOVEL     *BLANKS       SF1DE1
036700000000     C                   MOVEL     *BLANKS       SF1DE2
036800000000     C                   Z-ADD     0             VDAL
036900000000     C                   Z-ADD     0             VAL
037000000000     C                   ENDSR
037100011127     C******************************************************
037200011127     C     *INZSR        BEGSR
037300011127     C******************************************************
037400011127     C* POSIZIONAMENTO TABELLE                             *
037500011127     C     KTAB          KLIST                                                  *
037600011127     C                   KFLD                    CODUT                          *
037700011127     C                   KFLD                    COD               2            *
037800011127     C                   KFLD                    KEY               8            *
037900011210     C* SINISTRI mzSIN02L
038000011127     C     KSIN          KLIST                                                  *
038100011127     C                   KFLD                    VNRF                           *
038200011127     C                   KFLD                    SINDSI                         *
038300011127     C* CHAIN X N.DI TARGA                                 *
038400011127     C     KTAR          KLIST                                                  *
038500011127     C                   KFLD                    VTGA                           *
038600011127     C                   KFLD                    VTGN                           *
038700011212     C* mznot01L
038800011212     C     Knot          KLIST                                                  *
038900011212     C                   KFLD                    nottip                         *
039000011212     C                   KFLD                    SF1not                         *
039100011127     C*----------------------------------------------------*
039200011221     C* CONTROLLO SE GESTIRE SOLO LA VISUALIZZAZIONE DEI RECORD IN BASE
039300011221     C* AL CODICE DELL'AZIONE
039400011221     C     KCDAZ         COMP      'MZ94'                                 20
039500011127     C* RICERCA TARGA X CITTA'
039600011127     C     RICTAR        PLIST
039700011127     C                   PARM                    CODUT
039800011127     C                   PARM                    RSUT
039900011127     C                   PARM                    VTGA
040000011127     C                   PARM                    VTGN
040100011127     C*
040200011127     C     *ENTRY        PLIST                                                  *
040300011127     C                   PARM                    KPJBA                          *
040400011127     C                   Z-ADD     1             CODUT
040500011127     C                   CALL      'XPARUT'
040600011127     C                   PARM                    PARUT
040700011127     C*--------- SE DITTA INESISTENTE FINE LAVORO          *
040800011127     C                   IF        CODUT = -1
040900011127     C                   SETON                                        LR
041000011127     C                   END
041100011127     C                   MOVEL     RAGUT         RSUT             20
041200011127     C                   ENDSR
