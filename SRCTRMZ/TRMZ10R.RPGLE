000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     F*          INTERROGAZIONE STORICO                    *
000300020607     Fansif01L  IF   E           K DISK
000400910507     FAZORG01L  IF   E           K DISK
000500910507     FTABEL00F  IF   E           K DISK
000600031128     FmzAMH01L  IF   E           K DISK    infds(dsamh)
000700031128     FmzAMH02L  IF   E           K DISK    infds(dsamh2)
000800011129     F                                     RENAME(mzAMH000:mzAMH2)
000900031128     FmzAMH03L  IF   E           K DISK    infds(dsamh3)
001000011129     F                                     RENAME(mzAMH000:mzAMH3)
001100031128     FmzAMH04L  IF   E           K DISK    infds(dsamh4)
001200011219     F                                     RENAME(mzAMH000:mzAMH4)
001300011129     FTRmz10D   CF   E             WORKSTN
001400000000     F                                     SFILE(GA10DSF1:NRR)
001500011129     D DTAiso          S               D   INZ
001600011129     D DTAEUR          S               D   DATFMT(*EUR) INZ
001700011130     D fld             S              1
001800011130     D tga             S                   like(vtga)
001900011130     D tgn             S                   like(vtgn)
002000000000     D KPJBA         E DS
002100000000     D  LIBSYS                92    100
002200000000     D  MSGERR               453    502
002300000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002400031128     D dsamh           DS
002500031128     D  rrnamh               397    400b 0
002600031128     D dsamh2          DS
002700031128     D  rrnamh2              397    400b 0
002800031128     D dsamh3          DS
002900031128     D  rrnamh3              397    400b 0
003000031128     D dsamh4          DS
003100031128     D  rrnamh4              397    400b 0
003200031128     D nrrpar          s              9S 0
003300000000     C*----------------------------------------------------*
003400000000      * INDICATORI
003500000000      * 40 N.RIFERIMENTO ERRATO
003600000000      * 41 N.TARGA ERRATO
003700011129     c                   exsr      clr01
003800011129     C  nlr              do        *hival
003900011130     c                   EXSR      CLR00
004000011130     c                   if        *in99
004100011130     c                   setoff                                       99
004200011130     c                   write     ga10d001
004300011130     c                   seton                                        99
004400011130     c                   end
004500000000     C                   EXFMT     GA10D001
004600011129     C* FINE LAVORO
004700011129     C   Kc              leave
004800011130     C* ricerca
004900011130     C                   if        *inkd
005000011130     C                   EXSR      RIC01
005100011130     c                   iter
005200011130     c                   end
005300011129     C* controlli
005400011129     c                   exsr      ctr01
005500011129     c   99              iter
005600000000     C* PULIZIA SUBFILE
005700000000     C                   EXSR      PULIZ
005800000000     C* CARICAMENTO SUBFILE
005900000000     C                   EXSR      CARIC
006000011129     C                   do        *hival
006100031128     C                   WRITE     GA10D002
006200000000     C                   EXFMT     GA10DSC1
006300011129     c   kc              leave
006400011129     C                   if        *inkl
006500011129     c                   exsr      clr01
006600011129     c                   leave
006700011129     c                   end
006800031128     c                   exsr      srctrsfl
006900011129     C                   enddo
007000011129     c   kc              leave
007100011129     C                   enddo
007200000000     C                   SETON                                        LR
007300011130     c**********************************************************************
007400011130     c* pulisce 1° formato
007500011130     c**********************************************************************
007600011130     C     clr00         BEGSR
007700011130     C                   Z-ADD     0             §RIGA
007800011130     C                   Z-ADD     0             §COLON
007900011130     C                   CLEAR                   §FLD
008000011130     C                   ENDSR
008100011130     c**********************************************************************
008200011130     C*  ricerca
008300011130     c**********************************************************************
008400011130     C     ric01         BEGSR
008500011130     c                   movel     '1'           fld
008600011130     c                   select
008700011221     C                   WHEN      §FLD = 'VTGN' or §fld = 'VTGA'
008800011130     c                   movel     vtga          tga
008900011130     c                   movel     vtgn          tgn
009000011221     C                   CALL      'TRMZ02R4'    RICTAr
009100011130     c                   movel     tga           vtga
009200011130     c                   movel     tgn           vtgn
009300011130     C                   WHEN      §FLD = 'VTNN'
009400011130     c                   movel     vtna          tga
009500011130     c                   movel     vtnn          tgn
009600140505     C                   CALL      'TRMZ02R4'    RICTAr
009700011130     c                   movel     tga           vtna
009800011130     c                   movel     tgn           vtnn
009900011219     C                   when      §fld = 'VCAU'
010000011219     c                   exsr      srvcau
010100011130     c                   endsl
010200011130     C* POSIZIONAMENTO CURSORE
010300011130     C                   Z-ADD     §RIGA         §RIG
010400011130     C                   Z-ADD     §COLON        §COL
010500011130     C                   ENDSR
010600011219     c**********************************************************************
010700011219     C** autista
010800011219     c**********************************************************************
010900011219     C     srvcau        BEGSR
011000011219     c                   movel     'XN'          parcod
011100011219     c                   movel(P)  VCAU          parkey
011200011219     c                   exsr      srtab
011300011219     c                   movel     parkey        vcau
011400011219     c                   if        *in99
011500011219     C                   MOVEL     *blanks       VDcAU
011600011219     C                   SETON                                        55
011700011219     C                   else
011800011219     C                   MOVEL     TBLUNI        VDcAU
011900011219     C                   END
012000011219     c*
012100011219     C                   ENDSR
012200011219     C**********************************************************************
012300011219     C** Controlli e decodifiche campi con tabel00f
012400011219     C**********************************************************************
012500011219     C     srtab         BEGSR
012600011219     c                   if        fld <> *blanks
012700011219     c                   movel     parkey        parkeys           8
012800011219     C                   CALL      'TRUL19R'
012900011219     c                   parm                    parcod            2
013000011219     c                   parm      '1'           parord            1
013100011219     c                   parm      *blanks       parkey            8
013200011219     c                   parm      *blanks       cmd               1
013300011219     c* ritorno senza scelta
013400011219     c                   if        cmd = 'L'
013500011219     c                   movel     parkeys       parkey
013600011219     C                   END
013700011219     C                   END
013800011219     C                   MOVEL     *blanks       tbluni
013900011219     C                   MOVEL     parcod        COD
014000011219     C                   MOVEL(p)  parkey        KEY
014100011219     C     KTAB          CHAIN     TABEL                              99
014200011219     c*
014300011219     C                   ENDSR
014400000000     C******************************************************
014500000000     C** PULIZIA DEL SUBFILE
014600011129     C******************************************************
014700011129     C     PULIZ         BEGSR
014800000000     C                   SETON                                        70
014900000000     C                   WRITE     GA10DSC1
015000000000     C                   SETOFF                                       70
015100000000     C                   ENDSR
015200000000     C******************************************************
015300000000     C** CARICAMENTO SUBFILE
015400011129     C******************************************************
015500011129     C     CARIC         BEGSR
015600011219     c                   select
015700000000     C* LETTURA X N.RIFERIMENTO
015800011219     C                   when      vnrf <> 0
015900011129     C     KRIF          SETLL     mzAMH000
016000011219     C                   when      vtga <> *blanks
016100011219     C* LETTURA X N.RIFERIMENTO
016200011129     C     KTAR          SETLL     mzAMH2
016300011219     C                   when      vtna <> *blanks
016400011129     C     KTAR1         SETLL     mzAMH3
016500011219     C                   when      vcau <> *blanks
016600011219     C     vcau          SETLL     mzAMH4
016700011219     C                   ENDsl
016800000000     C                   Z-ADD     0             NRR               4 0
016900000000     C                   DO        *HIVAL
017000011219     c                   select
017100011219     C                   when      vnrf <> 0
017200011129     C     KRIF          READE     mzAMH000                               10
017300031128     c                   z-add     rrnamh        h1nrr
017400011219     C                   when      vtga <> *blanks
017500011129     C     KTAR          READE     mzAMH2                                 10
017600031128     c                   z-add     rrnamh2       h1nrr
017700011219     C                   when      vtna <> *blanks
017800011129     C     KTAR1         READE     mzAMH3                                 10
017900031128     c                   z-add     rrnamh3       h1nrr
018000011219     C                   when      vcau <> *blanks
018100011219     C     vcau          READE     mzAMH4                                 10
018200031128     c                   z-add     rrnamh4       h1nrr
018300011219     C                   ENDsl
018400910507     C* DECODIFICA AUTISTA
018500910507     C                   MOVEL     'XN'          COD
018600910507     C                   MOVEL     *BLANKS       KEY
018700910507     C                   MOVEL     AMHCAU        KEY
018800910507     C     KTAB          CHAIN     TABEL                              22
018900910507     C  N22              MOVEL     TBLUNI        CAUDES
019000910507     C   22              MOVE      *BLANKS       CAUDES
019100910507     C* DECODIFICA SITUAZIONE PROPRIETA'
019200910507     C                   MOVEL     'XR'          COD
019300910507     C                   MOVEL     *BLANKS       KEY
019400910507     C                   MOVEL     AMHCSP        KEY
019500910507     C     KTAB          CHAIN     TABEL                              22
019600910507     C  N22              MOVEL     TBLUNI        CSPDES
019700910507     C   22              MOVE      *BLANKS       CSPDES
019800910507     C* DECODIFICA FILIALE DOTAZIONE
019900910507     C     AMHFIL        CHAIN     AZORG                              22
020000030116     C  N22              MOVEL     ORGDES        filDES
020100030116     C   22              MOVE      *BLANKS       filDES
020200020607     C* DECODIFICA societa in dotazione
020300020607     C     AMHsoc        CHAIN     Ansif01l                           22
020400030116     c  n22              movel     sifdesbrev    socdes
020500030116     C   22              MOVE      *BLANKS       socDES
020600910507     C*
020700910507     C                   MOVEL     AMHDS2        VDS2                           ACCORCIA 2 DESC
020800910507     C*
020900910507     C* GIRO DATA VARIAZIONE
021000011129     C                   MOVE      AMHDTA        Dtaiso
021100011129     C                   move      dtaiso        dtaeur
021200011129     C                   move      Dtaeur        S1DTA
021300910507     C* GIRO DATA AUTISTA
021400011129     c                   if        amhdal <> 0
021500011129     C                   MOVE      AMHDAL        Dtaiso
021600011129     C                   move      dtaiso        dtaeur
021700011129     C                   move      Dtaeur        S2DTA
021800031128     C                   ELSE
021900031128     C                   Z-ADD     0             S2DTA
022000011129     c                   end
022100910507     C*
022200000000     C  N10              ADD       1             NRR
022300000000     C  N10              WRITE     GA10DSF1
022400000000     C  N10              END
022500000000     C                   ENDSR
022600011129     C*----------------------------------------------------*
022700011129     c     ctr01         begsr
022800011129     c                   setoff                                       99
022900011129     c                   do
023000011129     C     VNRF          IFEQ      0
023100011129     C     VTGA          ANDEQ     *BLANKS
023200011129     C     VTNA          ANDEQ     *BLANKS
023300011219     C     Vcau          ANDEQ     *BLANKS
023400011129     C                   seton                                        99
023500011129     c                   leave
023600011129     C                   END
023700011129     C     VNRF          IFNE      0
023800011129     C     KRIF          CHAIN     mzAMH000                           40
023900011129     C   40              seton                                        99
024000011129     c                   leave
024100011129     C                   END
024200011129     C     VTGA          IFNE      *BLANKS
024300011129     C     KTAR          CHAIN     mzAMH2                             41
024400011129     C   41              seton                                        99
024500011129     c                   leave
024600011129     C                   END
024700011129     C     VTNA          IFNE      *BLANKS
024800011129     C     KTAR1         CHAIN     mzAMH3                             42
024900011129     C   42              seton                                        99
025000011129     c                   leave
025100011129     C                   END
025200011219     C     Vcau          IFNE      *BLANKS
025300011219     C     vcau          CHAIN     mzAMH4                             43
025400011219     C   43              seton                                        99
025500011219     c                   leave
025600011219     C                   END
025700011129     C                   ENDdo
025800011129     c                   endsr
025900011129     C*----------------------------------------------------*
026000011129     c     clr01         begsr
026100011129     C                   Z-ADD     0             VNRF
026200011129     C                   MOVEL     *BLANKS       VTGA
026300011129     C                   MOVEL     *BLANKS       VTGN
026400011129     C                   MOVEL     *BLANKS       VTNA
026500011129     C                   MOVEL     *BLANKS       VTNN
026600011219     C                   MOVEL     *BLANKS       Vcau
026700011219     C                   MOVEL     *BLANKS       Vdcau
026800011129     c                   endsr
026900031128?     *--------------------------------------------------------------*
027000031128?     *  srctrsfl                                                    *
027100031128?     *--------------------------------------------------------------*
027200031128     C     srctrsfl      BEGSR
027300031128      *  Solo i selezionati
027400031128     c                   do        *hival
027500031128     C                   READC     GA10DSF1                               29
027600031128     C                   if        *in29
027700031128     c                   leave
027800031128     c                   end
027900031128     C                   if        v1csce <> ' '
028000031128     c                   z-add     h1nrr         nrrpar
028100031128     c                   movel     nrrpar        kpjbu
028200031128     c                   call      'TRMZ02R6'
028300031128     c                   parm                    kpjba
028400031128     c                   end
028500031128     c                   movel     *blanks       v1csce
028600031128     C                   SETOFF                                       22
028700031128     C                   UPDATE    GA10DSF1
028800031128     c                   enddo
028900031128      *
029000031128     C                   ENDSR
029100011129     C*---------------------------------------------------------------*
029200011129     c     *inzsr        begsr
029300011129     C     *ENTRY        PLIST
029400011129     C                   PARM                    KPJBA
029500011129     C                   Z-ADD     1             CODUT
029600011129     C                   CALL      'XPARUT'
029700011129     C                   PARM                    UTEDSE
029800011129     C     CODUT         ifeq      -1
029900011129     c                   seton                                        lr
030000011129     c                   end
030100011129     C                   MOVEL     RAGUT         RSUT             20
030200011129     C                   MOVEL     RAGUT         RSUT2            25
030300011129     C*----------------------------------------------------*
030400011129     C* POSIZIONAMENTO X N.RIFERIMENTO                     *
030500011129     C     KRIF          KLIST                                                  *
030600011129     C                   KFLD                    VNRF                           *
030700011129     C* POSIZIONAMENTO X N.TARGA                           *
030800011129     C     KTAR          KLIST                                                  *
030900011129     C                   KFLD                    VTGA                           *
031000011129     C                   KFLD                    VTGN                           *
031100011129     C     KTAR1         KLIST                                                  *
031200011129     C                   KFLD                    VTNA                           *
031300011129     C                   KFLD                    VTNN                           *
031400011129     C* POSIZIONAMENTO TABELLE                             *
031500011129     C     KTAB          KLIST                                                  *
031600011129     C                   KFLD                    CODUT                          *
031700011129     C                   KFLD                    COD               2            *
031800011129     C                   KFLD                    KEY               8            *
031900011130     C* RICERCA TARGA X CITTA'
032000011130     C     RICTAR        PLIST
032100011130     C                   PARM                    CODUT
032200011130     C                   PARM                    RSUT
032300011130     C                   PARM                    tga
032400011130     C                   PARM                    tgn
032500011129     c                   endsr
032600011129     C*----------------------------------------------------*
