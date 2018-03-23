000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000101011203     H*--------------------------------------------------------------*
000400000000     H*             - INTERROGAZIONE SINISTRO X AUSTISTA -           *
000500000000     H*--------------------------------------------------------------*
000600011203     FTRmz27D   CF   E             WORKSTN
000700000000     F                                     SFILE(GA27DSF1:NRR)
000800011210     FmzSIN03L  IF   E           K DISK
000900011203     FmzAME01L  IF   E           K DISK
001000000000     FTABEL00F  IF   E           K DISK
001800011203     D dtaiso          s               d
001801011203     D dtaeur          s               d   datfmt(*eur)
001802011203     D DTADAL          S               D   INZ
001803011203     D DTAAL           S               D   INZ
001804011203     D KPJBA         E DS
001900000000     D  LIBSYS                92    100
002000000000     D  MSGERR               453    502
002100000000     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002200000000     D  TCU                  398    697
002300000000     D                                     DIM(50)                              TAB FLAG CNT
002400000000     D  KCU                  698    847P 0
002500000000     D                                     DIM(50)                              TAB CAPO CONTO
002600000000     D                                     PACKEVEN
002700000000     D  DTU                  848    895P 0
002800000000     D                                     DIM(12)                              DATE PARAM
002900000000     D                                     PACKEVEN
003100000000     C*---------------------------------------------------------------*
003200000000      * INDICATORI
003500000000      * 52 DATA ERRATA DAL
003600000000      * 53 DATA ERRATA AL
003700910313     C* 60 NON CI SONO RECORD DA VISUALIZZARE
003800000000      * 70 PULIZIA SUBF
003900000000     C*---------------------------------------------------------------*
003901011203     C                   IF        NOT *INLR
006201011203     C                   EXSR      AZZER
006300011203     C                   do        *hival
006701011203     c                   EXSR      CLR00
006702011203     c                   if        *in99
006703011203     c                   setoff                                       99
006704011203     c                   write     ga27d001
006706011203     c                   seton                                        99
006707011203     c                   end
006708011203     C                   EXFMT     GA27D001
006709011203     C                   SETOFF                                       99
006710011203     C* FINE LAVORO
006711011203     C   KC              LEAVE
006712011203     C* RICERCA
006713011203     C   KD              EXSR      RIC01
006714011203     C* SCELTA N.RIF. O
006715011203     C                   EXSR      CTR01
006716011203     C   99              ITER
010900000000     C* PULIZIA SUBFILE
011000000000     C                   EXSR      PULIZ
011100000000     C* CARICAMENTO SUBFILE
011200000000     C                   EXSR      CARIC
011201011203     c   60              iter
011300000000     C                   WRITE     GA27D002
011400011203     C                   do        *hival
011700000000     C                   EXFMT     GA27DSC1
011800011203     C* FINE  LAVORO
011801011203     c                   if        *inkc or *inkl
011900011203     C                   leave
011901011203     C                   end
012000011203     C                   iter
012100011203     C                   enddo
012101011203     C   Kc              leave
012102011203     c                   if        *inkl
012103011203     c                   exsr      azzer
012104011203     C                   end
012105011203     C                   enddo
012106011203     C                   end
012200000000     C                   SETON                                        LR
012201011203     c**********************************************************************
012202011203     c* pulisce 1° formato
012203011203     c**********************************************************************
012204011203     C     clr00         BEGSR
012205011203     C                   Z-ADD     0             §RIGA
012206011203     C                   Z-ADD     0             §COLON
012207011203     C                   CLEAR                   §FLD
012208011203     C                   ENDSR
012209011203     C******************************************************
012210011203     C** CONTROLLI VIDEATA
012211011203     C******************************************************
012212011203     C     Ctr01         BEGSR
012213011203     C                   SETOFF                                         99
012214011203     c                   movel     ' '           fld               1
012215011203     C                   do
012216011203     c                   if        vaus = *blanks
012217011203     c                   seton                                        5499
012218011203     c                   leave
012219011203     c                   else
012220011203     c                   exsr      srvaus
012221011203     c   99              leave
012222011203     c                   end
012277011203     C* CONTROLLO DATE
012278011203     C                   MOVEL     *LOVAL        DTADAL
012279011203     C     VDAL          IFne      0
012281011203     C     *eur          test(d)                 vdal                   52
012282011203     c                   if        *in52
012283011203     C                   SETON                                        99
012284011203     C                   leave
012285011203     C                   END
012286011203     C                   MOVEL     VDAL          DTAEUR
012287011203     C                   MOVEL     DTAEUR        DTADAL
012288011203     C                   END
012289011203     C** DATA fattura
012290011203     C                   MOVEL     *LOVAL        DTAAL
012291011203     C     Val           IFNE      0
012292011203     C     *eur          test(d)                 val                    53
012293011203     c                   if        *in53
012294011203     C                   SETON                                        99
012295011203     C                   leave
012296011203     C                   END
012297011203     C                   MOVEL     VAL           DTAEUR
012298011203     C                   MOVEL     DTAEUR        DTAAL
012299011203     C                   END
012300011203     C*
012301011203     C                   IF        (VDAL <> 0 OR VAL <> 0) AND
012302011203     C                              DTADAL > DTAAL
012303011203     C                   SETON                                        525399
012304011203     C                   END
012305011203     C                   ENDdo
012306011203     C                   ENDSR
012307011203     C******************************************************
012308011203     c* ricerca
012309011203     C******************************************************
012310011203     C     ric01         BEGSR
012311011203     c                   movel     '1'           fld
012312011203     c                   select
012313011203     C*COD.autista
012314011203     C                   when      §fld = 'VAUS'
012315011203     c                   exsr      srvaus
012322011203     C                   endsl
012323011203     C* POSIZIONAMENTO CURSORE
012324011203     C                   Z-ADD     §RIGA         §RIG
012325011203     C                   Z-ADD     §COLON        §COL
012326011203     C                   ENDSR
012327011203     c**********************************************************************
012328011203     C** autista
012329011203     c**********************************************************************
012330011203     C     srvaus        BEGSR
012331011203     c*
012332011203     c                   movel     'XN'          parcod
012333011203     c                   movel(P)  vaus          parkey
012334011203     c                   exsr      srtab
012335011203     c                   movel     parkey        vaus
012336011203     c                   if        *in99
012337011203     C                   MOVEL     *blanks       VDaus
012338011203     C                   SETON                                        54
012339011203     C                   else
012340011203     C                   MOVEL     TBLUNI        VDaus
012341011203     C                   END
012342011203     c*
012343011203     C                   ENDSR
012378011203     C**********************************************************************
012379011203     C** Controlli e decodifiche campi con tabel00f
012380011203     C**********************************************************************
012381011203     C     srtab         BEGSR
012382011203     c                   if        fld <> *blanks
012383011203     c                   movel     parkey        parkeys           8
012384011203     C                   CALL      'TRUL19R'
012385011203     c                   parm                    parcod            2
012386011203     c                   parm      '1'           parord            1
012387011203     c                   parm      *blanks       parkey            8
012388011203     c                   parm      *blanks       cmd               1
012389011203     c* ritorno senza scelta
012390011203     c                   if        cmd = 'L'
012391011203     c                   movel     parkeys       parkey
012392011203     C                   END
012393011203     C                   END
012394011203     C                   MOVEL     *blanks       tbluni
012395011203     C                   MOVEL     parcod        COD
012396011203     C                   MOVEL(p)  parkey        KEY
012397011203     C     KTAB          CHAIN     TABEL00f                           99
012398011203     c*
012399011203     C                   ENDSR
012400000000     C******************************************************
012401011203     C** PULIZIA CAMPI
012402011203     C******************************************************
012403000000     C     AZZER         BEGSR
012800000000     C                   MOVEL     *BLANKS       VAUS
012900000000     C                   MOVEL     *BLANKS       VDAUS
013000000000     C                   Z-ADD     0             VDAL
013100000000     C                   Z-ADD     0             VAL
013200000000     C                   Z-ADD     0             SF1DSI
013300000000     C                   MOVEL     *BLANKS       SF1ATT
013400000000     C                   MOVEL     *BLANKS       SF1DE1
013500000000     C                   MOVEL     *BLANKS       SF1DE2
013600000000     C                   Z-ADD     0             VNRF
013700000000     C                   MOVEL     *BLANKS       VTGA
013800000000     C                   MOVEL     *BLANKS       VTGN
013900000000     C                   ENDSR
014000000000     C******************************************************
014001011203     C** PULIZIA DEL SUBFILE
014002011203     C******************************************************
014100000000     C     PULIZ         BEGSR
014500000000     C                   SETON                                        70
014600000000     C                   WRITE     GA27DSC1
014700000000     C                   SETOFF                                       70
014800000000     C                   ENDSR
014900000000     C******************************************************
015200000000     C** CARICAMENTO SUBFILE
015300000000     C** CON LETTURA X COD.AUTISTA
015301011203     C******************************************************
015401011203     C     CARIC         BEGSR
015500000000     C                   Z-ADD     0             NRR               4 0
015600011210     C     VAUS          SETLL     mzSIN000                               80
015700011203     C                   do        *hival
016100011210     C     VAUS          READE     mzSIN000                               80
016200011203     C   80              leave
016201011203     c                   move      sindsi        dtaiso
016300000000     C     VDAL          IFGT      0
016500011203     C     dtaiso        andlt     dtadal
016501011203     c                   iter
016502011203     c                   end
016503011203     C     Val           IFGT      0
016600011203     C     dtaiso        andgt     dtaal
016601011203     c                   leave
016700000000     C                   END
016800000000     C** AGGANCIO GAAME
016900011203     C     SINNRF        CHAIN     mzAME000
017000011203     C                   if        %found(mzame01l)
017100000000     C                   MOVEL     AMETGA        VTGA
017200000000     C                   MOVEL     AMETGN        VTGN
017300011203     C                   Else
017400011203     C                   MOVEL     'STORICO'     VTGN
017500011203     C                   MOVEL     *BLANKS       VTGA
017501011203     c                   end
017600000000     C                   Z-ADD     SINNRF        VNRF
017700011203     C                   MOVE      SINDSI        Dtaiso
017800011203     C                   move      dtaiso        dtaeur
017900011203     C                   move      Dtaeur        SF1DSI
018000000000     C                   MOVEL     SINATT        SF1ATT
018100000000     C                   MOVEL     SINDE1        SF1DE1
018200000000     C                   MOVEL     SINDE2        SF1DE2
018300000000     C                   ADD       1             NRR
018400000000     C                   WRITE     GA27DSF1
018500011203     C                   ENDdo
018700910313     C     NRR           COMP      0                                      60
018800000000     C                   ENDSR
018801011203     C******************************************************
024800011203     c     *inzsr        begsr
024801011203     C******************************************************
024900011203     C     *ENTRY        PLIST
025000011203     C                   PARM                    KPJBA
025100011203     C                   Z-ADD     1             CODUT
025200011203     C                   CALL      'XPARUT'
025300011203     C                   PARM                    UTEDSE
025400011203     C     CODUT         ifeq      -1
025500011203     c                   seton                                        lr
025600011203     c                   end
025700011203     C                   MOVEL     RAGUT         RSUT             20
025800011203     C                   MOVEL     RAGUT         RSUT2            25
026000011203     C* POSIZIONAMENTO TABELLE                             *
026100011203     C     KTAB          KLIST                                                  *
026200011203     C                   KFLD                    CODUT                          *
026300011203     C                   KFLD                    COD               2            *
026400011203     C                   KFLD                    KEY               8            *
027300011203     c                   endsr
