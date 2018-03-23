000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000400000000     H*             - IMMISSIONE GRANDI MANUTENZIONI                 *
000600011130     FTRmz28D   CF   E             WORKSTN
000700000000     F                                     SFILE(GA28DSF1:NRR)
000801011212     Fmznot01L  IF   E           K DISK
000802011212     FmzIGM01L  IF   E           K DISK
000900000000     F                                     INFDS(IGMINF)
001000011130     FmzIGM00F  UF   E             DISK
001100000000     F                                     RECNO(IGMNRR)
001200011130     F                                     RENAME(mzIGM000:mzIGM1)
001201011130     FmzAME01L  IF   E           K DISK
001202011130     F                                     RENAME(mzAME000:mzAME1)
001203011130     FmzAME02L  IF   E           K DISK
001600000000     FTABEL00F  IF   E           K DISK
001701011130     D DTAiso          S               D   INZ
001702011130     D DTAEUR          S               D   DATFMT(*EUR) INZ
001703011130     D DTADAL          S               D   INZ
001704011130     D DTAAL           S               D   INZ
001705151216     D dsxn          e ds
002100000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
002200000000     D KPJBA         E DS
002500000000     D*----------DS PER IL NUMERO RELATIVO ASSOLUTO-
002600000000     D IGMINF          DS
002700000000     D  IGMNRR               397    400B 0
002701011130     D*GESTIONE SINISTRO
002703011130     D PAR166          DS
002704011130     D  p66nrf                        7  0
002705011203     D  p66nsi                        9  0
002706011203     D  p66nig                        9  0
002707011130     D  p66flg                        1
002708011130     c* modalità con cui chiamo il pgm
002709011130     D  p66mod                        1
002710011130     c* Cmd di ritorno
002711011130     D  p66ret                        1
002712011130     D*GESTIONE NOTE
002713011130     D PAR191          DS
002714011130     D* numero riferimento
002715011130     D  P91nrf                        7  0
002716011130     D* tipo nota
002717011130     D  P91tip                        2
002718011130     D* numero nota
002719011130     D  P91NOT                        7  0
002720011130     c* modalità con cui chiamo il pgm
002721011130     D  p91mod                        1
002722011130     c* Cmd di ritorno
002723011130     D  p91ret                        1
009800000000     C*----------------------------------------------------*
009900000000      * INDICATORI
010000000000      *
010300000000      * 22 SERVIZIO
010400000000      * 50 N.RIFERIMENTO ERRATO
010500000000      * 51 N.TARGA ERRATO
010600000000      * 52 DATA ERRATA (DAL)
010700000000      * 53 DATA ERRATA (AL)
010800000000      * 54 COMP DATA
010900000000      * 70 PULIZIA SUBF1
015000000000     C*
015401011130     C  nlr              DO        *HIVAL
015402011130     c                   EXSR      CLR00
015403011130     c                   if        *in99
015404011130     c                   setoff                                       99
015405011130     c                   write     ga28d001
015406011130     c                   seton                                        99
015407011130     c                   end
015500000000     C                   EXFMT     GA28D001
015501011130     C* FINE LAVORO
015502011130     C   KC              LEAVE
015503011130     C* RICERCA
015504011130     C   KD              EXSR      RIC01
015505050720     C* anagrafica automezzi
015506050720     c   kg              call      'TRMZ02R'
015507050720     c                   parm                    kpjba
015508050720     C   kg              ITER
015800000000     C* CONTROLLI
015900000000     C                   EXSR      CTRL01
015901011130     C   99              ITER
016200000000     C* NUOVA MANUTENZIONE
016201011130     C                   IF        *INKF
016202011203     C                   Z-ADD     0             hnrr
016203011130     C                   EXSR      IMMSIn
016204011130     C                   ITER
016205011130     C                   END
016700000000     C* PULIZIA SUBFILE
016800000000     C                   EXSR      PULIZ
016900000000     C* CARICAMENTO SUBFILE
017000000000     C                   EXSR      CARIC
017001011130     C                   DO        *HIVAL
017100000000     C                   WRITE     GA28D002
017101011130     C     NRR           COMP      0                                  7171
017400000000     C                   EXFMT     GA28DSC1
017500000000     C                   SETON                                        71
018801011130     C* FINE  LAVORO
018802011130     C   KC              LEAVE
018803011130     C* FINE  LAVORO
018804011130     C   Kl              LEAVE
018805011130     C* NUOVO SINISTRO
018806011130     C                   IF        *INKF
018807011203     C                   Z-ADD     0             hnrr
018808011130     C                   EXSR      IMMSIn
018809011130     C     P66RET        IFEQ      'F'
018810011130     c                   exsr      puliz
018811011130     c                   exsr      caric
018812011130     C                   END
018813011130     C                   ITER
018814011130     C                   END
018900000000     C* CONTROLLI
019000000000     C     NRR           IFNE      0
019100000000     C                   EXSR      CONTR
019200000000     C                   END
019201011130     C                   ENDDO
019202011130     C   KC              LEAVE
019203011130     C                   ENDDO
019204011130     c*
019500000000     C                   SETON                                        LR
019501011130     c**********************************************************************
019502011130     c* pulisce 1° formato
019503011130     c**********************************************************************
019504011130     C     clr00         BEGSR
019505011130     C                   Z-ADD     0             §RIGA
019506011130     C                   Z-ADD     0             §COLON
019507011130     C                   CLEAR                   §FLD
019508011130     C                   ENDSR
019700000000     C******************************************************
020000000000     C** CONTROLLI FORMATO ENTRATA
020201011130     C******************************************************
020301011130     C     CTRL01        BEGSR
020302011203     C                   SETOFF                                       99
020303011130     C                   DO
020304011130     C* SCELTA N.RIF. O
020305011130     C* N.TARGA
020400000000     C     VNRF          IFEQ      0
020500000000     C     VTGA          ANDEQ     *BLANKS
020600011130     C                   SETON                                        5099
020700011130     C                   leave
020800000000     C                   END
020900000000     C     VNRF          IFNE      0
021000000000     C     VTGA          ANDNE     *BLANKS
021100000000     C                   MOVE      *BLANKS       VTGA
021200000000     C                   MOVE      *BLANKS       VTGN
021300000000     C                   END
021400000000     C* CONTROLLO TARGA
022000000000     C     VTGA          IFNE      *BLANKS
022100000000     C     VTGN          ANDNE     *BLANKS
022200011130     C     KTAR          CHAIN     mzAME000                           51
022201011130     C                   IF        NOT *IN51
022202011130     C                   Z-ADD     AMENRF        VNRF
022203011130     C                   ELSE
022204011130     C                   SETON                                        99
022205011130     C                   LEAVE
022206011130     C                   END
022500000000     C                   END
022600011130     C     VNRF          CHAIN     mzAME1                             50
022701011130     C                   IF        NOT *IN50
022702011130     C                   MOVEL     AMEMAR        VMAR
022703011130     C                   MOVEL     AMETIP        VTIP
022800000000     C                   MOVEL     AMEMAR        VMAR
022900000000     C                   MOVEL     AMETIP        VTIP
022901011130     C                   MOVEL     AMEdit        Vdit
022902151216     C                   MOVEL     AMEcau        Vcau
022903151216     c* decodifica autista
022904151216     c                   movel     'XN'          cod
022905151216     c                   movel(P)  VCAU          key
022906151216     C     KTAB          CHAIN     TABEL
022907151216     c                   if        %found(tabel00f)
022908151216     C                   MOVEl     TBLUNI        dsxn
022909151216     c                   eval      vaut = §XNDES
022910151216     c                   else
022911151216     C                   MOVEl     *BLANKS       dsxn
022912151216     c                   clear                   vaut
022913151216     c                   end
023000000000     C     VTGA          IFEQ      *BLANKS
023100000000     C                   MOVEL     AMETGA        VTGA
023200000000     C                   MOVEL     AMETGN        VTGN
023300000000     C                   END
023301011203     C                   ELSE
023302011203     C                   SETON                                        99
023303011203     C                   LEAVE
023400000000     C                   END
023600000000     C* CONTROLLO DATE
023601011219     C                   MOVEL     *LOVAL        DTADAL
023602011219     C                   IF        VDAL <> 0
023603011219     C     *EUR          test(d)                 Vdal                   52
023604011219     c                   if        *in52
023605011130     C                   SETON                                        99
023606011130     C                   leave
023607011130     C                   END
023608011219     C                   MOVEL     Vdal          DTAEUR
023609011219     C                   MOVEL     DTAEUR        DTADAL
023610011130     C                   END
023611011219     c*
023612011219     C                   MOVEL     *hival        DTAAL
023613011219     C                   IF        VAL <> 0
023614011219     C     *EUR          test(d)                 VAL                    53
023615011219     c                   if        *in53
023616011219     C                   SETON                                        99
023617011219     C                   leave
023618011219     C                   END
023619011219     C                   MOVEL     VAL           DTAEUR
023620011219     C                   MOVEL     DTAEUR        DTAAL
023621011219     C                   END
023622011130     C*
023623011130     C                   IF        (VDAL <> 0 OR VAL <> 0) AND
023624011130     C                              DTADAL > DTAAL
023625011130     C                   SETON                                        525399
023626011130     C                   END
023627011130     C                   ENDDO
027400000000     C                   ENDSR
027401011130     C******************************************************
027402011130     C* RICERCA PRIMO VIDEO
027403011130     C******************************************************
027404011130     C     RIC01         BEGSR
027405011130     c                   select
027406011221     C                   when      §fld = 'VTGN' or §fld= 'VTGA'
027407011221     C                   CALL      'TRMZ02R4'    RICTAR
027408011130     C                   ENDsl
027409011130     C* POSIZIONAMENTO CURSORE
027410011130     C                   Z-ADD     §RIGA         §RIG
027411011130     C                   Z-ADD     §COLON        §COL
027412011130     C                   ENDSR
027500000000     C******************************************************
027501011130     C** IMMISSIONE MANUTENZIONE
027502011130     C******************************************************
027600000000     C     IMMSIN        BEGSR
027601011130     c                   clear                   par166
027602011130     c                   move      vnrf          p66nrf
027603011203     c                   z-add     hnrr          p66nig
027604011130     c                   move      '1'           p66flg
028801011130     c                   if        not *inkf
028802011130     c                   move      scel          p66mod
028803011130     c                   end
028804011130     c                   movel(P)  par166        kpjbu
028805011130     C                   CALL      'TRMZ26R2'
028806011130     C                   PARM                    KPJBA
028807011130     C                   MOVEL(p)  KPJBU         PAR166
034200000000     C                   ENDSR
034300000000     C******************************************************
034301011130     C** PULIZIA DEL SUBFILE
034302011130     C******************************************************
034400000000     C     PULIZ         BEGSR
034800000000     C                   SETON                                        70
034900000000     C                   WRITE     GA28DSC1
035000000000     C                   SETOFF                                       70
035100000000     C                   Z-ADD     0             NRR               4 0
035200000000     C                   ENDSR
035300000000     C******************************************************
035301011130     C** CARICAMENTO SUBFILE
035302011130     C******************************************************
035400000000     C     CARIC         BEGSR
035900011130     C                   MOVEL     *BLANKS       fld               1
035901011130     C                   MOVEL     *BLANKS       SCEL
036000000000     C                   MOVE      VNRF          IGMNRF
036001011130     C                   move      Dtadal        igmdat
036002011130     c                   if        val <> 0
036003011130     C                   move      Dtaal         al                8 0
036004011130     c                   else
036005011130     c                   z-add     99999999      al
036006011130     c                   end
036200011130     C     KIGM          SETLL     mzIGM000
036201011130     c                   do        *hival
036300011130     C     igmnrf        READE     mzIGM000                               21
036400000000     C  N21IGMDAT        COMP      AL                                 21
036401011203     c   21              leave
036500000000     C*
036902011130     C                   MOVE      igmdat        Dtaiso
036903011130     C                   MOVE      dtaiso        Dtaeur
036904011130     C                   move      Dtaeur        sdat
037000000000     C                   MOVE      IGMKM         SKM
037001011130     c                   if        igmdtf <> 0
037301011130     C                   MOVE      igmdtf        Dtaiso
037302011130     C                   MOVE      dtaiso        Dtaeur
037303011130     C                   move      Dtaeur        sdtf
037304011130     c                   end
037400000000     C                   MOVE      IGMOFB        SOFB
037500011130     C                   z-add     IGMiPV        SOPV
037600011130     C                   z-add     IGMiEF        SOEF
037700000000     C                   MOVE      IGMINT        SINT
037800000000     C                   MOVE      IGMOFF        SOFF
037900000000     C                   MOVE      IGMNSI        HNSI
038000000000     C                   MOVE      IGMNOT        HNOT
038100000000     C                   MOVE      IGMNRR        HNRR
038200000000     C                   MOVE      ' '           SNOT
038201011130     C     IGMNOT        IFNE      0
038202011212     c     knot          chain     mznot01l
038203011212     c                   if        %found(mznot01l)
038400000000     C                   MOVE      'D'           SNOT
038401011212     c                   else
038402011212     C                   MOVE      ' '           Snot
038403011212     c                   z-add     0             hnot
038404011212     C                   END
038405011130     C                   END
038501011130     c* intervento
038502011130     C                   exsr      srsint
038503011130     c* officina
038504011130     C                   exsr      srsofb
039100000000     C                   ADD       1             NRR
039200000000     C                   WRITE     GA28DSF1
039500000000     C                   END
039600000000     C                   Z-ADD     NRR           SVNRR             4 0
039700000000     C                   ENDSR
042400000000     C*-----------------------------------------------------
042401011130     C** CONTROLLI
042402011130     C*-----------------------------------------------------
042500000000     C     CONTR         BEGSR
042501011130     C*
042900000000     C                   READC     GA28DSF1                               06
043100000000     C     *IN06         DOWNE     '1'
043101011221     C                   IF        scel = '2' or scel = '4' or scel = '5'
043103011130     C                   EXSR      immsin
043104011130     c                   exsr      puliz
043105011130     c                   exsr      caric
043106011130     c                   leave
043107011130     C                   END
043108011130     C* NOTE AGGIUNTIVE
043109011130     C     SCEL          IFEQ      'D'
043110011130     C                   clear                   PAR191
043111011130     C                   Z-ADD     vnrf          P91Nrf
043112011130     C                   Z-ADD     hnot          P91NOT
043113011221     c* manutenzione note solo se non sono in visualizzazione
043114011221     c                   if        not *in20
043115011221     c                   if        hnot <> 0
043116011130     C                   MOVE      '2'           P91mod
043118011130     c                   end
043119011221     c                   else
043120011221     C                   MOVE      '5'           P91mod
043121011221     c                   end
043122011130     C                   MOVEL(p)  PAR191        KPJBU
043123011130     C                   CALL      'TRMZ26R3'
043124011130     C                   PARM                    KPJBA
043125011130     C                   MOVEL(p)  KPJBU         PAR191
043126011130     C                   MOVE      ' '           SCEL
043127011130     c* modifica/inserimento nota
043128011130     c                   if        p91ret = 'F'
043129011130     C                   z-add     P91NOT        hNOT
043130011130     C     hnrr          CHAIN     mzigm1                             21
043131011130     c                   if        not *in21
043132011130     C                   MOVE      p91NOT        igmNOT
043133011130     C                   UPDATE    mzigm1
043134011130     C                   END
043135011130     c                   end
043136011130     C                   END
043137011130     C     hnot          IFNE      0
043138011130     C                   MOVE      'D'           Snot
043139011130     c                   else
043140011130     C                   MOVE      ' '           Snot
043141011130     C                   END
049300000000      *
049400000000     C                   UPDATE    GA28DSF1
049500000000     C                   SETOFF                                           61
049700000000     C                   READC     GA28DSF1                               06
049800000000     C                   END
049801011130      *
049900000000     C                   ENDSR
049928011130     c**********************************************************************
049929011130     C** intervento
049930011130     c**********************************************************************
049931011130     C     srsint        BEGSR
049932011130     c*
049933011130     C     SINT          IFNE      *BLANK
049934011130     c                   movel     'XE'          parcod
049935011130     c                   movel(P)  sint          parkey
049936011130     c                   exsr      srtab
049937011130     c                   movel     parkey        sint
049938011130     c                   if        *in99
049939011130     C                   MOVEL     *blanks       sintd
049940011130     C                   SETON                                        63
049941011130     C                   else
049942011130     C                   MOVEL     TBLUNI        sintd
049943011130     C                   END
049944011130     C                   ELSE
049945011130     C                   MOVEL(p)  'SINISTRO'    SINTD
049946011130     C                   END
049947011130     c*
049948011130     C                   ENDSR
049949011130     c**********************************************************************
049950011130     C** officina
049951011130     c**********************************************************************
049952011130     C     srsofb        BEGSR
049953011130     c*
049954011130     c                   movel     'XO'          parcod
049955011130     c                   movel(P)  sofb          parkey
049956011130     c                   exsr      srtab
049957011130     c                   movel     parkey        sofb
049958011130     c                   if        *in99
049959011130     C                   MOVEL     *blanks       sofbd
049960011130     C                   SETON                                        80
049961011130     C                   else
049962011130     C                   MOVEL     TBLUNI        sofbd
049963011130     C                   END
049964011130     c*
049965011130     C                   ENDSR
049966011130     C**********************************************************************
049967011130     C** Controlli e decodifiche campi con tabel00f
049968011130     C**********************************************************************
049969011130     C     srtab         BEGSR
049970011130     c                   if        fld <> *blanks
049971011130     c                   movel     parkey        parkeys           8
049972011130     C                   CALL      'TRUL19R'
049973011130     c                   parm                    parcod            2
049974011130     c                   parm      '1'           parord            1
049975011130     c                   parm      *blanks       parkey            8
049976011130     c                   parm      *blanks       cmd               1
049977011130     c* ritorno senza scelta
049978011130     c                   if        cmd = 'L'
049979011130     c                   movel     parkeys       parkey
049980011130     C                   END
049981011130     C                   END
049982011130     C                   MOVEL     *blanks       tbluni
049983011130     C                   MOVEL     parcod        COD
049984011130     C                   MOVEL(p)  parkey        KEY
049985011130     C     KTAB          CHAIN     TABEL00f                           99
049986011130     c*
049987011130     C                   ENDSR
050400011130     C*----------------------------------------------------*
050500011130     c     *inzsr        begsr
050600011130     C*----------------------------------------------------*
050700011130     C* POSIZIONAMENTO TABELLE                             *
050800011130     C     KTAB          KLIST                                                  *
050900011130     C                   KFLD                    CODUT                          *
051000011130     C                   KFLD                    COD               2            *
051100011130     C                   KFLD                    KEY               8            *
051200011130     C*       GRANDI INTERVENTI mzIGM03L                   *
051300011130     C     KIGM          KLIST                                                  *
051400011130     C                   KFLD                    IGMNRF                         *
051500011130     C                   KFLD                    IGMDAT                         *
051600011130     C* CHAIN X N.DI TARGA                                 *
051700011130     C     KTAR          KLIST                                                  *
051800011130     C                   KFLD                    VTGA                           *
051900011130     C                   KFLD                    VTGN                           *
051901011212     C* mznot01L
051902011212     C     Knot          KLIST                                                  *
051903011212     C                   KFLD                    nottip                         *
051904011212     C                   KFLD                    igmnot                         *
052000011130     C*----------------------------------------------------*
052100011130     C* RICERCA TARGA X CITTA'
052200011130     C     RICTAR        PLIST
052300011130     C                   PARM                    CODUT
052400011130     C                   PARM                    RSUT
052500011130     C                   PARM                    VTGA
052600011130     C                   PARM                    VTGN
052700011130     C*
052800011130     C*----------------------------------------------------*
052900011130      *  CARICO COSTANTI
053000011130      *
053100011130      *
053200011130     C     *ENTRY        PLIST                                                  *
053300011130     C                   PARM                    KPJBA                          *
053400011130     C                   MOVE      1             CODUT
053500011130     C                   CALL      'XPARUT'
053600011130     C                   PARM                    PARUT
053601011221     C* CONTROLLO SE GESTIRE SOLO LA VISUALIZZAZIONE DEI RECORD IN BASE
053602011221     C* AL CODICE DELL'AZIONE
053603011221     C     KCDAZ         COMP      'MZ93'                                 20
053700011130     C*--------- SE DITTA INESISTENTE FINE LAVORO          *
053800011130     C     CODUT         ifeq      -1
053900011130     c                   seton                                        lr
054000011130     c                   end
054100011130     C                   MOVEL     RAGUT         RSUT             20
054200011130     c                   endsr
