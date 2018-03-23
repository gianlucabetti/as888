000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000     F*          INTERROGAZIONE STORICO                    *
000401020607     Fansif01L  IF   E           K DISK
000900910507     FAZORG01L  IF   E           K DISK
001000910507     FTABEL00F  IF   E           K DISK
001100011129     FmzAMH01L  IF   E           K DISK
001200011129     FmzAMH02L  IF   E           K DISK
001300011129     F                                     RENAME(mzAMH000:mzAMH2)
001400011129     FmzAMH03L  IF   E           K DISK
001500011129     F                                     RENAME(mzAMH000:mzAMH3)
001501011219     FmzAMH04L  IF   E           K DISK
001502011219     F                                     RENAME(mzAMH000:mzAMH4)
001600011129     FTRmz10D   CF   E             WORKSTN
001700000000     F                                     SFILE(GA10DSF1:NRR)
001701011129     D DTAiso          S               D   INZ
001702011129     D DTAEUR          S               D   DATFMT(*EUR) INZ
001703011130     D fld             S              1
001704011130     D tga             S                   like(vtga)
001705011130     D tgn             S                   like(vtgn)
002400000000     D KPJBA         E DS
002500000000     D  LIBSYS                92    100
002600000000     D  MSGERR               453    502
002700000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002800000000     D  TCU                  398    697
002900000000     D                                     DIM(50)                              TAB FLAG CNT
003000000000     D  KCU                  698    847P 0
003100000000     D                                     DIM(50)                              TAB CAPO CONTO
003200000000     D                                     PACKEVEN
003300000000     D  DTU                  848    895P 0
003400000000     D                                     DIM(12)                              DATE PARAM
003500000000     D                                     PACKEVEN
003700000000     C*----------------------------------------------------*
003800000000      * INDICATORI
004200000000      * 40 N.RIFERIMENTO ERRATO
004300000000      * 41 N.TARGA ERRATO
006901011129     c                   exsr      clr01
007000011129     C  nlr              do        *hival
007001011130     c                   EXSR      CLR00
007002011130     c                   if        *in99
007003011130     c                   setoff                                       99
007004011130     c                   write     ga10d001
007005011130     c                   seton                                        99
007006011130     c                   end
007800000000     C                   EXFMT     GA10D001
007900011129     C* FINE LAVORO
008000011129     C   Kc              leave
008001011130     C* ricerca
008003011130     C                   if        *inkd
008004011130     C                   EXSR      RIC01
008005011130     c                   iter
008006011130     c                   end
008100011129     C* controlli
008101011129     c                   exsr      ctr01
008102011129     c   99              iter
009900000000     C* PULIZIA SUBFILE
010000000000     C                   EXSR      PULIZ
010100000000     C* CARICAMENTO SUBFILE
010200000000     C                   EXSR      CARIC
010300000000     C                   WRITE     GA10D002
010400011129     C                   do        *hival
010500000000     C                   EXFMT     GA10DSC1
010501011129     c   kc              leave
010600011129     C                   if        *inkl
010601011129     c                   exsr      clr01
010602011129     c                   leave
010603011129     c                   end
010700011129     C                   enddo
010701011129     c   kc              leave
010702011129     C                   enddo
010800000000     C                   SETON                                        LR
010801011130     c**********************************************************************
010802011130     c* pulisce 1° formato
010803011130     c**********************************************************************
010804011130     C     clr00         BEGSR
010805011130     C                   Z-ADD     0             §RIGA
010806011130     C                   Z-ADD     0             §COLON
010807011130     C                   CLEAR                   §FLD
010808011130     C                   ENDSR
010809011130     c**********************************************************************
010810011130     C*  ricerca
010811011130     c**********************************************************************
010812011130     C     ric01         BEGSR
010813011130     c                   movel     '1'           fld
010814011130     c                   select
010829011221     C                   WHEN      §FLD = 'VTGN' or §fld = 'VTGA'
010830011130     c                   movel     vtga          tga
010831011130     c                   movel     vtgn          tgn
010832011221     C                   CALL      'TRMZ02R4'    RICTAr
010833011130     c                   movel     tga           vtga
010834011130     c                   movel     tgn           vtgn
010835011130     C                   WHEN      §FLD = 'VTNN'
010836011130     c                   movel     vtna          tga
010837011130     c                   movel     vtnn          tgn
010838011130     C                   CALL      'TRGA02R4'    RICTAr
010839011130     c                   movel     tga           vtna
010840011130     c                   movel     tgn           vtnn
010841011219     C                   when      §fld = 'VCAU'
010842011219     c                   exsr      srvcau
010843011130     c                   endsl
010844011130     C* POSIZIONAMENTO CURSORE
010845011130     C                   Z-ADD     §RIGA         §RIG
010846011130     C                   Z-ADD     §COLON        §COL
010847011130     C                   ENDSR
010848011219     c**********************************************************************
010849011219     C** autista
010850011219     c**********************************************************************
010851011219     C     srvcau        BEGSR
010852011219     c                   movel     'XN'          parcod
010853011219     c                   movel(P)  VCAU          parkey
010854011219     c                   exsr      srtab
010855011219     c                   movel     parkey        vcau
010856011219     c                   if        *in99
010857011219     C                   MOVEL     *blanks       VDcAU
010858011219     C                   SETON                                        55
010859011219     C                   else
010860011219     C                   MOVEL     TBLUNI        VDcAU
010861011219     C                   END
010862011219     c*
010863011219     C                   ENDSR
010864011219     C**********************************************************************
010865011219     C** Controlli e decodifiche campi con tabel00f
010866011219     C**********************************************************************
010867011219     C     srtab         BEGSR
010868011219     c                   if        fld <> *blanks
010869011219     c                   movel     parkey        parkeys           8
010870011219     C                   CALL      'TRUL19R'
010871011219     c                   parm                    parcod            2
010872011219     c                   parm      '1'           parord            1
010873011219     c                   parm      *blanks       parkey            8
010874011219     c                   parm      *blanks       cmd               1
010875011219     c* ritorno senza scelta
010876011219     c                   if        cmd = 'L'
010879011219     c                   movel     parkeys       parkey
010880011219     C                   END
010881011219     C                   END
010882011219     C                   MOVEL     *blanks       tbluni
010883011219     C                   MOVEL     parcod        COD
010884011219     C                   MOVEL(p)  parkey        KEY
010885011219     C     KTAB          CHAIN     TABEL                              99
010886011219     c*
010887011219     C                   ENDSR
010900000000     C******************************************************
011200000000     C** PULIZIA DEL SUBFILE
011201011129     C******************************************************
011301011129     C     PULIZ         BEGSR
011400000000     C                   SETON                                        70
011500000000     C                   WRITE     GA10DSC1
011600000000     C                   SETOFF                                       70
011700000000     C                   ENDSR
011800000000     C******************************************************
012100000000     C** CARICAMENTO SUBFILE
012201011129     C******************************************************
012202011129     C     CARIC         BEGSR
012203011219     c                   select
012300000000     C* LETTURA X N.RIFERIMENTO
012400011219     C                   when      vnrf <> 0
012500011129     C     KRIF          SETLL     mzAMH000
012600011219     C                   when      vtga <> *blanks
012601011219     C* LETTURA X N.RIFERIMENTO
012800011129     C     KTAR          SETLL     mzAMH2
012901011219     C                   when      vtna <> *blanks
013000011129     C     KTAR1         SETLL     mzAMH3
013001011219     C                   when      vcau <> *blanks
013002011219     C     vcau          SETLL     mzAMH4
013200011219     C                   ENDsl
013300000000     C                   Z-ADD     0             NRR               4 0
013400000000     C                   DO        *HIVAL
013401011219     c                   select
013402011219     C                   when      vnrf <> 0
013600011129     C     KRIF          READE     mzAMH000                               10
013801011219     C                   when      vtga <> *blanks
013900011129     C     KTAR          READE     mzAMH2                                 10
014002011219     C                   when      vtna <> *blanks
014100011129     C     KTAR1         READE     mzAMH3                                 10
014101011219     C                   when      vcau <> *blanks
014102011219     C     vcau          READE     mzAMH4                                 10
014200011219     C                   ENDsl
014400910507     C* DECODIFICA AUTISTA
014500910507     C                   MOVEL     'XN'          COD
014600910507     C                   MOVEL     *BLANKS       KEY
014700910507     C                   MOVEL     AMHCAU        KEY
014800910507     C     KTAB          CHAIN     TABEL                              22
014900910507     C  N22              MOVEL     TBLUNI        CAUDES
015000910507     C   22              MOVE      *BLANKS       CAUDES
015100910507     C* DECODIFICA SITUAZIONE PROPRIETA'
015200910507     C                   MOVEL     'XR'          COD
015300910507     C                   MOVEL     *BLANKS       KEY
015400910507     C                   MOVEL     AMHCSP        KEY
015500910507     C     KTAB          CHAIN     TABEL                              22
015600910507     C  N22              MOVEL     TBLUNI        CSPDES
015700910507     C   22              MOVE      *BLANKS       CSPDES
015800910507     C* DECODIFICA FILIALE DOTAZIONE
015900910507     C     AMHFIL        CHAIN     AZORG                              22
015901030116     C  N22              MOVEL     ORGDES        filDES
016100030116     C   22              MOVE      *BLANKS       filDES
016101020607     C* DECODIFICA societa in dotazione
016102020607     C     AMHsoc        CHAIN     Ansif01l                           22
016104030116     c  n22              movel     sifdesbrev    socdes
016105030116     C   22              MOVE      *BLANKS       socDES
016200910507     C*
016300910507     C                   MOVEL     AMHDS2        VDS2                           ACCORCIA 2 DESC
016400910507     C*
016500910507     C* GIRO DATA VARIAZIONE
016600011129     C                   MOVE      AMHDTA        Dtaiso
016700011129     C                   move      dtaiso        dtaeur
016800011129     C                   move      Dtaeur        S1DTA
016900910507     C* GIRO DATA AUTISTA
016901011129     c                   if        amhdal <> 0
017000011129     C                   MOVE      AMHDAL        Dtaiso
017001011129     C                   move      dtaiso        dtaeur
017200011129     C                   move      Dtaeur        S2DTA
017201011129     c                   end
017300910507     C*
017400000000     C  N10              ADD       1             NRR
017500000000     C  N10              WRITE     GA10DSF1
017600000000     C  N10              END
017700000000     C                   ENDSR
020700011129     C*----------------------------------------------------*
020800011129     c     ctr01         begsr
020900011129     c                   setoff                                       99
021000011129     c                   do
021100011129     C     VNRF          IFEQ      0
021200011129     C     VTGA          ANDEQ     *BLANKS
021300011129     C     VTNA          ANDEQ     *BLANKS
021301011219     C     Vcau          ANDEQ     *BLANKS
021400011129     C                   seton                                        99
021500011129     c                   leave
021600011129     C                   END
021700011129     C     VNRF          IFNE      0
021800011129     C     KRIF          CHAIN     mzAMH000                           40
021900011129     C   40              seton                                        99
022000011129     c                   leave
022100011129     C                   END
022200011129     C     VTGA          IFNE      *BLANKS
022300011129     C     KTAR          CHAIN     mzAMH2                             41
022400011129     C   41              seton                                        99
022500011129     c                   leave
022600011129     C                   END
022700011129     C     VTNA          IFNE      *BLANKS
022800011129     C     KTAR1         CHAIN     mzAMH3                             42
022900011129     C   42              seton                                        99
023000011129     c                   leave
023100011129     C                   END
023101011219     C     Vcau          IFNE      *BLANKS
023102011219     C     vcau          CHAIN     mzAMH4                             43
023103011219     C   43              seton                                        99
023104011219     c                   leave
023105011219     C                   END
023106011129     C                   ENDdo
023200011129     c                   endsr
023300011129     C*----------------------------------------------------*
023400011129     c     clr01         begsr
023500011129     C                   Z-ADD     0             VNRF
023600011129     C                   MOVEL     *BLANKS       VTGA
023700011129     C                   MOVEL     *BLANKS       VTGN
023800011129     C                   MOVEL     *BLANKS       VTNA
023900011129     C                   MOVEL     *BLANKS       VTNN
023901011219     C                   MOVEL     *BLANKS       Vcau
023902011219     C                   MOVEL     *BLANKS       Vdcau
024000011129     c                   endsr
024001011129     C*---------------------------------------------------------------*
024002011129     c     *inzsr        begsr
024003011129     C     *ENTRY        PLIST
024004011129     C                   PARM                    KPJBA
024005011129     C                   Z-ADD     1             CODUT
024006011129     C                   CALL      'XPARUT'
024007011129     C                   PARM                    UTEDSE
024008011129     C     CODUT         ifeq      -1
024009011129     c                   seton                                        lr
024010011129     c                   end
024011011129     C                   MOVEL     RAGUT         RSUT             20
024012011129     C                   MOVEL     RAGUT         RSUT2            25
024013011129     C*----------------------------------------------------*
024014011129     C* POSIZIONAMENTO X N.RIFERIMENTO                     *
024015011129     C     KRIF          KLIST                                                  *
024016011129     C                   KFLD                    VNRF                           *
024017011129     C* POSIZIONAMENTO X N.TARGA                           *
024018011129     C     KTAR          KLIST                                                  *
024019011129     C                   KFLD                    VTGA                           *
024020011129     C                   KFLD                    VTGN                           *
024021011129     C     KTAR1         KLIST                                                  *
024022011129     C                   KFLD                    VTNA                           *
024023011129     C                   KFLD                    VTNN                           *
024024011129     C* POSIZIONAMENTO TABELLE                             *
024025011129     C     KTAB          KLIST                                                  *
024026011129     C                   KFLD                    CODUT                          *
024027011129     C                   KFLD                    COD               2            *
024028011129     C                   KFLD                    KEY               8            *
024029011130     C* RICERCA TARGA X CITTA'
024035011130     C     RICTAR        PLIST
024036011130     C                   PARM                    CODUT
024037011130     C                   PARM                    RSUT
024038011130     C                   PARM                    tga
024039011130     C                   PARM                    tgn
024040011129     c                   endsr
024100011129     C*----------------------------------------------------*
