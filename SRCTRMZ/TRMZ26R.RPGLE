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
001600160518     FTntbe01l  IF   E           K DISK
001700011203     D sinINF          DS
001800011203     D  sinNRR               397    400B 0
001900011127     D*GESTIONE SINISTRO
002000000000     D PARAM           DS
002100011127     D  parnrf                        7  0
002200011203     D  parnsi                        9  0
002300011203     D  parnig                        9  0
002400011128     D  parflg                        1
002500011127     c* modalità con cui chiamo il pgm
002600011127     D  parmod                        1
002700011127     c* Cmd di ritorno
002800011127     D  parret                        1
002900011127     D*GESTIONE NOTE
003000000000     D PAR191          DS
003100011129     D* numero riferimento
003200011129     D  P91nrf                        7  0
003300011129     D* tipo nota
003400011129     D  P91tip                        2
003500011129     D* numero nota
003600011129     D  P91NOT                        7  0
003700011127     c* modalità con cui chiamo il pgm
003800011127     D  p91mod                        1
003900011127     c* Cmd di ritorno
004000011127     D  p91ret                        1
004100000000      *
004200000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
004300000000     D KPJBA         E DS
004400011127      *
004500160518     D dmau          E DS
004600160518     D dcau          E DS
004700011127     D DTAiso          S               D   INZ
004800011127     D DTAEUR          S               D   DATFMT(*EUR) INZ
004900011127     D DTADAL          S               D   INZ
005000011127     D DTAAL           S               D   INZ
005100000000     C*----------------------------------------------------*
005200000000      * INDICATORI
005300000000      *
005400011127      * KC FINE LAVORO
005500011127      * KD RICERCA
005600011127      * KF IMMISSIONE NUOVO SINISTRO/CONFERMA
005700000000      * 06 READC SFL
005800000000      * 22 SERVIZIO
005900000000      * 50 N.RIFERIMENTO ERRATO
006000000000      * 51 N.TARGA ERRATO
006100000000      * 52 DATA ERRATA (DAL)
006200000000      * 53 DATA ERRATA (AL)
006300000000      * 61 PROTECT SCEL
006400000000      * 70 PULIZIA SUBF1
006500011210      * 80 SETLL E READE mzSIN
006600000000      * 89 ALTA INTENSITA'
006700000000     C*----------------------------------------------------*
006800011127     C                   IF        NOT *INLR
006900000000     C                   EXSR      AZZER
007000011127     C*
007100011127     C                   DO        *HIVAL
007200011127     c                   EXSR      CLR00
007300011127     c                   if        *in99
007400011127     c                   setoff                                       99
007500011127     c                   write     ga26d001
007600011127     c                   seton                                        99
007700011127     c                   end
007800000000     C                   EXFMT     GA26D001
007900000000     C                   SETOFF                                       99
008000011127     C* FINE LAVORO
008100011127     C   KC              LEAVE
008200011127     C* RICERCA
008300011127     C   KD              EXSR      RIC01
008400150918      *
008500150918     C* anagrafica automezzi
008600150918     c   kg              call      'TRMZ02R'
008700150918     c                   parm                    kpjba
008800150918     C   kg              ITER
008900150918      *
009000000000     C* SCELTA N.RIF. O
009100011127     C                   EXSR      CTR01
009200011127     C   99              ITER
009300000000     C* NUOVO SINISTRO
009400011127     C                   IF        *INKF
009500011203     C                   Z-ADD     0             hnrr
009600011127     C                   EXSR      IMMSI
009700011127     C                   ITER
009800011127     C                   END
009900000000     C* PULIZIA SUBFILE
010000000000     C                   EXSR      PULIZ
010100000000     C* CARICAMENTO SUBFILE
010200000000     C                   EXSR      CARIC
010300011127     C                   DO        *HIVAL
010400000000     C     SVNRR         COMP      0                                  7171
010500000000     C                   WRITE     GA26D002
010600000000     C                   EXFMT     GA26DSC1
010700011127     C* FINE  LAVORO
010800011127     C   KC              LEAVE
010900011128     C* FINE  LAVORO
011000011128     C   Kl              LEAVE
011100000000     C* NUOVO SINISTRO
011200011127     C                   IF        *INKF
011300011203     C                   Z-ADD     0             hnrr
011400011127     C                   EXSR      IMMSI
011500011127     C     PARRET        IFEQ      'F'
011600011127     c                   exsr      puliz
011700011127     c                   exsr      caric
011800011127     C                   END
011900011127     C                   ITER
012000000000     C                   END
012100000000     C* CONTROLLI
012200000000     C     SVNRR         IFNE      0
012300011127     C                   EXSR      CONsfl
012400000000     C                   END
012500011127     C                   ENDDO
012600011203     c                   if        *inkl
012700011203     c                   exsr      azzer
012800011203     C                   ENDIF
012900011127     C   KC              LEAVE
013000011127     C                   ENDDO
013100011127     C                   ENDIF
013200011127     c*
013300000000     C                   SETON                                        LR
013400011127     c**********************************************************************
013500011127     c* pulisce 1° formato
013600011127     c**********************************************************************
013700011127     C     clr00         BEGSR
013800011127     C                   Z-ADD     0             §RIGA
013900011127     C                   Z-ADD     0             §COLON
014000011127     C                   CLEAR                   §FLD
014100011127     C                   ENDSR
014200011127     C******************************************************
014300011127     C* CONTROLLI PRIMO VIDEO
014400011127     C******************************************************
014500011127     C     CTR01         BEGSR
014600011127     C                   DO
014700011127     C* N.TARGA
014800011127     C     VNRF          IFEQ      0
014900011127     C     VTGA          ANDEQ     *BLANKS
015000011127     C                   SETON                                        5099
015100011127     C                   LEAVE
015200011127     C                   END
015300011127     C     VNRF          IFNE      0
015400011127     C     VTGA          ANDNE     *BLANKS
015500011127     C                   MOVE      *BLANKS       VTGA
015600011127     C                   MOVE      *BLANKS       VTGN
015700011127     C                   END
015800011127     C* CONTROLLO TARGA
015900011127     C     VTGA          IFNE      *BLANKS
016000011127     C     VTGN          ANDNE     *BLANKS
016100011127     C     KTAR          CHAIN     MZAME000                           51
016200011127     C                   IF        NOT *IN51
016300011127     C                   Z-ADD     AMENRF        VNRF
016400011127     C                   ELSE
016500011127     C                   SETON                                        99
016600011127     C                   LEAVE
016700011127     C                   END
016800011127     C                   END
016900011127     C     VNRF          CHAIN     MZAME1                             50
017000011127     C                   IF        NOT *IN50
017100160518     C                   MOVEL     AMEmau        VMAu
017200160518     C                   MOVEL     AMEcaut       Vcaut
017300160518     c                   movel     'MAU'         tbecod
017400160518     c                   movel(p)  amemau        tbeke1
017500160518     c     ktbe          chain     tntbe01l
017600160518     c                   if        %found(tntbe01l)
017700160518     c                   movel     tbeuni        dmau
017800160518     c                   movel     §maudes       vmaud
017900160518     c                   endif
018000160518     c                   movel     'CAU'         tbecod
018100160518     c                   movel(p)  amecaut       tbeke1
018200160518     c     ktbe          chain     tntbe01l
018300160518     c                   if        %found(tntbe01l)
018400160518     c                   movel     tbeuni        dcau
018500160518     c                   movel     §caudes       vcautd
018600160518     c                   endif
018700011127     C     VTGA          IFEQ      *BLANKS
018800011127     C                   MOVEL     AMETGA        VTGA
018900011127     C                   MOVEL     AMETGN        VTGN
019000011127     C                   END
019100011127     C                   END
019200011128     C   50              MOVEL     'STORICO'     VTGN
019300011127     C* CONTROLLO DATE
019400011127     C                   MOVEL     *LOVAL        DTADAL
019500011127     C                   IF        VDAL <> 0
019600011127     C     *EUR          test(d)                 VDAL                   52
019700011127     c                   if        *in52
019800011127     C                   SETON                                        99
019900011127     C                   leave
020000011127     C                   END
020100011127     C                   MOVEL     VDAL          DTAEUR
020200011127     C                   MOVEL     DTAEUR        DTADAL
020300011127     C                   END
020400011127     C* CONTROLLO AL
020500011127     C                   MOVEL     *LOVAL        DTAAL
020600011127     C                   IF        VAL <> 0
020700011127     C     *EUR          test(d)                 VAL                    53
020800011127     c                   if        *in53
020900011127     C                   SETON                                        99
021000011127     C                   leave
021100011127     C                   END
021200011127     C                   MOVEL     VAL           DTAEUR
021300011127     C                   MOVEL     DTAEUR        DTAAL
021400011127     C                   END
021500011127     C*
021600011127     C                   IF        (VDAL <> 0 OR VAL <> 0) AND
021700011127     C                              DTADAL > DTAAL
021800011127     C                   SETON                                        525399
021900011127     C                   END
022000011127     C                   ENDDO
022100011127     C*
022200011127     C                   ENDSR
022300011127     C******************************************************
022400011127     C* RICERCA PRIMO VIDEO
022500011127     C******************************************************
022600011127     C     RIC01         BEGSR
022700011130     c                   select
022800011221     C                   when      §fld = 'VTGN' or §fld = 'VTGA'
022900011221     C                   CALL      'TRMZ02R4'    RICTAR
023000011130     C                   ENDsl
023100011130     C* POSIZIONAMENTO CURSORE
023200011130     C                   Z-ADD     §RIGA         §RIG
023300011130     C                   Z-ADD     §COLON        §COL
023400011127     C                   ENDSR
023500011127     C******************************************************
023600011127     C** CHIAMATA AL PGM DI GESTIONE SINISTRO
023700011127     C******************************************************
023800011127     C     IMMSI         BEGSR
023900011127     c*
024000011127     c                   clear                   param
024100011127     c                   move      vnrf          parnrf
024200011203     c                   move      hnrr          parnsi
024300011127     c                   if        not *inkf
024400011127     c                   move      scel          parmod
024500011127     c                   end
024600011127     c                   movel(P)  param         kpjbu
024700011127     C                   CALL      'TRMZ26R2'
024800000000     C                   PARM                    KPJBA
024900011127     C                   MOVEL(p)  KPJBU         PARAM
025000011127     c*
025100011127     C                   ENDSR
025200011127     C******************************************************
025300011127     C** PULIZIA DEL SUBFILE
025400000000     C******************************************************
025500000000     C     PULIZ         BEGSR
025600011127     c*
025700000000     C                   SETON                                        70
025800000000     C                   SETOFF                                       71
025900000000     C                   WRITE     GA26DSC1
026000000000     C                   SETOFF                                       70
026100000000     C                   SETON                                        71
026200011127     c*
026300000000     C                   ENDSR
026400000000     C******************************************************
026500000000     C** CARICAMENTO SUBFILE
026600000000     C** CON LETTURA X N.RIFER.
026700011127     C******************************************************
026800011127     C     CARIC         BEGSR
026900011127     c*
027000000000     C                   SETOFF                                           89
027100000000     C                   Z-ADD     0             NRR               4 0
027200011127     C                   move      Dtadal        SINDSI
027300011128     c                   if        val <> 0
027400011127     C                   move      Dtaal         al                8 0
027500011128     c                   else
027600011128     c                   z-add     99999999      al
027700011128     c                   end
027800011210     C     KSIN          SETLL     mzSIN000
027900011127     c                   do        *hival
028000011210     C     VNRF          READE     mzSIN000                               80
028100011127     C  n80SINDSI        COMP      AL                                 80
028200011127     c   80              leave
028300011127     C                   MOVEL     *BLANKS       SCEL
028400011203     C                   MOVE      SINnrr        hnrr
028500011203     C                   MOVE      SINDSI        Datsin            8 0
028600011127     C                   MOVE      SINDSI        Dtaiso
028700011127     C                   MOVE      dtaiso        Dtaeur
028800011127     C                   move      Dtaeur        SF1DSI
028900011127     C                   MOVEL     SINAUS        SF1AUS
029000011127     C                   MOVE      SINNOT        SF1NOT
029100011127     C* FLAG DI NOTA AGGIUNTIVA
029200011127     C                   MOVE      ' '           SF1D
029300011127     C     SF1NOT        IFNE      0
029400011212     c     knot          chain     mznot01l
029500011212     c                   if        %found(mznot01l)
029600011127     C                   MOVE      'D'           SF1D
029700011212     c                   else
029800011212     C                   MOVE      ' '           SF1D
029900011212     c                   z-add     0             sf1not
030000011127     C                   END
030100011212     C                   END
030200011127     C* decodifica autista
030300011127     C                   MOVEL     'XN'          COD
030400011127     C                   MOVEL     *BLANKS       KEY
030500011127     C                   MOVEL     SF1AUS        KEY
030600011127     C     KTAB          CHAIN     TABEL                              21
030700011127     C  N21              MOVEL     TBLUNI        SF1DAU
030800011127     C                   MOVEL     SINATT        SF1ATT
030900011127     C     SF1ATT        COMP      'A'                                    89
031000011127     C                   MOVEL     SINDE1        SF1DE1
031100011127     C                   MOVEL     SINDE2        SF1DE2
031200011127     C                   ADD       1             NRR
031300011127     C                   WRITE     GA26DSF1
031400011127     C                   ENDdo
031500000000     C                   Z-ADD     NRR           SVNRR             4 0
031600000000     C                   ENDSR
031700000000     C******************************************************
031800000000     C** CONTROLLI
031900011127     C******************************************************
032000011127     C     CONsfl        BEGSR
032100011127     c*
032200000000     C                   READC     GA26DSF1                               06
032300000000     C     *IN06         DOWNE     '1'
032400011221     C                   IF        scel = '2' or scel = '4' or scel = '5'
032500011127     C                   EXSR      immsi
032600011127     c                   exsr      puliz
032700011127     c                   exsr      caric
032800011129     c                   leave
032900000000     C                   END
033000000000     C* NOTE AGGIUNTIVE
033100000000     C     SCEL          IFEQ      'D'
033200011127     C                   clear                   PAR191
033300011129     C                   Z-ADD     vnrf          P91Nrf
033400011129     C                   Z-ADD     SF1NOT        P91NOT
033500011221     c* manutenzione note solo se non sono in visualizzazione
033600011221     c                   if        not *in20
033700011127     c                   if        sf1not <> 0
033800011127     C                   MOVE      '2'           P91mod
033900011127     c                   end
034000011221     c                   else
034100011221     C                   MOVE      '5'           P91mod
034200011221     c                   end
034300011127     C                   MOVEL(p)  PAR191        KPJBU
034400011129     C                   CALL      'TRMZ26R3'
034500000000     C                   PARM                    KPJBA
034600011127     C                   MOVEL(p)  KPJBU         PAR191
034700000000     C                   MOVE      ' '           SCEL
034800011127     c* modifica/inserimento nota
034900011129     c                   if        p91ret = 'F'
035000011127     C                   z-add     P91NOT        SF1NOT
035100011210     C     sinnsi        CHAIN     mzSIN002                           21
035200011127     c                   if        not *in21
035300011127     C                   MOVE      p91NOT        SINNOT
035400011210     C                   UPDATE    mzSIN002
035500011127     C                   END
035600011129     c                   end
035700011127     C                   END
035800011127     c*
035900000000     C     SF1NOT        IFNE      0
036000000000     C                   MOVE      'D'           SF1D
036100011127     c                   else
036200011127     C                   MOVE      ' '           SF1D
036300000000     C                   END
036400000000     C     SF1ATT        COMP      'A'                                    89
036500000000     C                   UPDATE    GA26DSF1
036600000000     C                   SETOFF                                           61
036700000000     C                   READC     GA26DSF1                               06
036800000000     C                   END
036900011127     c*
037000011127     C                   ENDSR
037100011127     C******************************************************
037200000000     C** PULIZIA CAMPI DS
037300011127     C******************************************************
037400011127     C     AZZER         BEGSR
037500000000     C                   Z-ADD     0             VNRF
037600000000     C                   MOVEL     *BLANKS       VTGA
037700000000     C                   MOVEL     *BLANKS       VTGN
037800160518     C                   MOVEL     *BLANKS       VMAu
037900160518     C                   MOVEL     *BLANKS       Vcaut
038000000000     C                   Z-ADD     0             SF1DSI
038100000000     C                   MOVEL     *BLANKS       SF1AUS
038200000000     C                   MOVEL     *BLANKS       SF1ATT
038300000000     C                   MOVEL     *BLANKS       SF1DE1
038400000000     C                   MOVEL     *BLANKS       SF1DE2
038500000000     C                   Z-ADD     0             VDAL
038600000000     C                   Z-ADD     0             VAL
038700000000     C                   ENDSR
038800011127     C******************************************************
038900011127     C     *INZSR        BEGSR
039000011127     C******************************************************
039100011127     C* POSIZIONAMENTO TABELLE                             *
039200011127     C     KTAB          KLIST                                                  *
039300011127     C                   KFLD                    CODUT                          *
039400011127     C                   KFLD                    COD               2            *
039500011127     C                   KFLD                    KEY               8            *
039600160518      *
039700160518     C     KTbe          KLIST                                                  *
039800160518     C                   KFLD                    tbecod                         *
039900160518     C                   KFLD                    tbeke1                         *
040000011210     C* SINISTRI mzSIN02L
040100011127     C     KSIN          KLIST                                                  *
040200011127     C                   KFLD                    VNRF                           *
040300011127     C                   KFLD                    SINDSI                         *
040400011127     C* CHAIN X N.DI TARGA                                 *
040500011127     C     KTAR          KLIST                                                  *
040600011127     C                   KFLD                    VTGA                           *
040700011127     C                   KFLD                    VTGN                           *
040800011212     C* mznot01L
040900011212     C     Knot          KLIST                                                  *
041000011212     C                   KFLD                    nottip                         *
041100011212     C                   KFLD                    SF1not                         *
041200011127     C*----------------------------------------------------*
041300011221     C* CONTROLLO SE GESTIRE SOLO LA VISUALIZZAZIONE DEI RECORD IN BASE
041400011221     C* AL CODICE DELL'AZIONE
041500011221     C     KCDAZ         COMP      'MZ94'                                 20
041600011127     C* RICERCA TARGA X CITTA'
041700011127     C     RICTAR        PLIST
041800011127     C                   PARM                    CODUT
041900011127     C                   PARM                    RSUT
042000011127     C                   PARM                    VTGA
042100011127     C                   PARM                    VTGN
042200011127     C*
042300011127     C     *ENTRY        PLIST                                                  *
042400011127     C                   PARM                    KPJBA                          *
042500011127     C                   Z-ADD     1             CODUT
042600011127     C                   CALL      'XPARUT'
042700011127     C                   PARM                    PARUT
042800011127     C*--------- SE DITTA INESISTENTE FINE LAVORO          *
042900011127     C                   IF        CODUT = -1
043000011127     C                   SETON                                        LR
043100011127     C                   END
043200011127     C                   MOVEL     RAGUT         RSUT             20
043300011127     C                   ENDSR
