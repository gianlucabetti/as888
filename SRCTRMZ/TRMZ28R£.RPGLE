000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*             - IMMISSIONE GRANDI MANUTENZIONI                 *
000300011130     FTRmz28D   CF   E             WORKSTN
000400000000     F                                     SFILE(GA28DSF1:NRR)
000500011212     Fmznot01L  IF   E           K DISK
000600011212     FmzIGM01L  IF   E           K DISK
000700000000     F                                     INFDS(IGMINF)
000800011130     FmzIGM00F  UF   E             DISK
000900000000     F                                     RECNO(IGMNRR)
001000011130     F                                     RENAME(mzIGM000:mzIGM1)
001100011130     FmzAME01L  IF   E           K DISK
001200011130     F                                     RENAME(mzAME000:mzAME1)
001300011130     FmzAME02L  IF   E           K DISK
001400000000     FTABEL00F  IF   E           K DISK
001500160518     FTntbe01l  IF   E           K DISK
001600011130     D DTAiso          S               D   INZ
001700011130     D DTAEUR          S               D   DATFMT(*EUR) INZ
001800011130     D DTADAL          S               D   INZ
001900011130     D DTAAL           S               D   INZ
002000151216     D dsxn          e ds
002100160518     D dmau          E DS
002200160518     D dcau          E DS
002300000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
002400000000     D KPJBA         E DS
002500000000     D*----------DS PER IL NUMERO RELATIVO ASSOLUTO-
002600000000     D IGMINF          DS
002700000000     D  IGMNRR               397    400B 0
002800011130     D*GESTIONE SINISTRO
002900011130     D PAR166          DS
003000011130     D  p66nrf                        7  0
003100011203     D  p66nsi                        9  0
003200011203     D  p66nig                        9  0
003300011130     D  p66flg                        1
003400011130     c* modalità con cui chiamo il pgm
003500011130     D  p66mod                        1
003600011130     c* Cmd di ritorno
003700011130     D  p66ret                        1
003800011130     D*GESTIONE NOTE
003900011130     D PAR191          DS
004000011130     D* numero riferimento
004100011130     D  P91nrf                        7  0
004200011130     D* tipo nota
004300011130     D  P91tip                        2
004400011130     D* numero nota
004500011130     D  P91NOT                        7  0
004600011130     c* modalità con cui chiamo il pgm
004700011130     D  p91mod                        1
004800011130     c* Cmd di ritorno
004900011130     D  p91ret                        1
005000000000     C*----------------------------------------------------*
005100000000      * INDICATORI
005200000000      *
005300000000      * 22 SERVIZIO
005400000000      * 50 N.RIFERIMENTO ERRATO
005500000000      * 51 N.TARGA ERRATO
005600000000      * 52 DATA ERRATA (DAL)
005700000000      * 53 DATA ERRATA (AL)
005800000000      * 54 COMP DATA
005900000000      * 70 PULIZIA SUBF1
006000000000     C*
006100011130     C  nlr              DO        *HIVAL
006200011130     c                   EXSR      CLR00
006300011130     c                   if        *in99
006400011130     c                   setoff                                       99
006500011130     c                   write     ga28d001
006600011130     c                   seton                                        99
006700011130     c                   end
006800000000     C                   EXFMT     GA28D001
006900011130     C* FINE LAVORO
007000011130     C   KC              LEAVE
007100011130     C* RICERCA
007200011130     C   KD              EXSR      RIC01
007300050720     C* anagrafica automezzi
007400050720     c   kg              call      'TRMZ02R'
007500050720     c                   parm                    kpjba
007600050720     C   kg              ITER
007700000000     C* CONTROLLI
007800000000     C                   EXSR      CTRL01
007900011130     C   99              ITER
008000000000     C* NUOVA MANUTENZIONE
008100011130     C                   IF        *INKF
008200011203     C                   Z-ADD     0             hnrr
008300011130     C                   EXSR      IMMSIn
008400011130     C                   ITER
008500011130     C                   END
008600000000     C* PULIZIA SUBFILE
008700000000     C                   EXSR      PULIZ
008800000000     C* CARICAMENTO SUBFILE
008900000000     C                   EXSR      CARIC
009000011130     C                   DO        *HIVAL
009100000000     C                   WRITE     GA28D002
009200011130     C     NRR           COMP      0                                  7171
009300000000     C                   EXFMT     GA28DSC1
009400000000     C                   SETON                                        71
009500011130     C* FINE  LAVORO
009600011130     C   KC              LEAVE
009700011130     C* FINE  LAVORO
009800011130     C   Kl              LEAVE
009900011130     C* NUOVO SINISTRO
010000011130     C                   IF        *INKF
010100011203     C                   Z-ADD     0             hnrr
010200011130     C                   EXSR      IMMSIn
010300011130     C     P66RET        IFEQ      'F'
010400011130     c                   exsr      puliz
010500011130     c                   exsr      caric
010600011130     C                   END
010700011130     C                   ITER
010800011130     C                   END
010900000000     C* CONTROLLI
011000000000     C     NRR           IFNE      0
011100000000     C                   EXSR      CONTR
011200000000     C                   END
011300011130     C                   ENDDO
011400011130     C   KC              LEAVE
011500011130     C                   ENDDO
011600011130     c*
011700000000     C                   SETON                                        LR
011800011130     c**********************************************************************
011900011130     c* pulisce 1° formato
012000011130     c**********************************************************************
012100011130     C     clr00         BEGSR
012200011130     C                   Z-ADD     0             §RIGA
012300011130     C                   Z-ADD     0             §COLON
012400011130     C                   CLEAR                   §FLD
012500011130     C                   ENDSR
012600000000     C******************************************************
012700000000     C** CONTROLLI FORMATO ENTRATA
012800011130     C******************************************************
012900011130     C     CTRL01        BEGSR
013000011203     C                   SETOFF                                       99
013100011130     C                   DO
013200011130     C* SCELTA N.RIF. O
013300011130     C* N.TARGA
013400000000     C     VNRF          IFEQ      0
013500000000     C     VTGA          ANDEQ     *BLANKS
013600011130     C                   SETON                                        5099
013700011130     C                   leave
013800000000     C                   END
013900000000     C     VNRF          IFNE      0
014000000000     C     VTGA          ANDNE     *BLANKS
014100000000     C                   MOVE      *BLANKS       VTGA
014200000000     C                   MOVE      *BLANKS       VTGN
014300000000     C                   END
014400000000     C* CONTROLLO TARGA
014500000000     C     VTGA          IFNE      *BLANKS
014600000000     C     VTGN          ANDNE     *BLANKS
014700011130     C     KTAR          CHAIN     mzAME000                           51
014800011130     C                   IF        NOT *IN51
014900011130     C                   Z-ADD     AMENRF        VNRF
015000011130     C                   ELSE
015100011130     C                   SETON                                        99
015200011130     C                   LEAVE
015300011130     C                   END
015400000000     C                   END
015500011130     C     VNRF          CHAIN     mzAME1                             50
015600011130     C                   IF        NOT *IN50
015700011130     C                   MOVEL     AMEdit        Vdit
015800151216     C                   MOVEL     AMEcau        Vcau
015900160518     C                   MOVEL     AMEmau        VMAu
016000160518     C                   MOVEL     AMEcaut       Vcaut
016100160518     c                   movel     'MAU'         tbecod
016200160518     c                   movel(p)  amemau        tbeke1
016300160518     c     ktbe          chain     tntbe01l
016400160518     c                   if        %found(tntbe01l)
016500160518     c                   movel     tbeuni        dmau
016600160518     c                   movel     §maudes       vmaud
016700160518     c                   endif
016800160518     c                   movel     'CAU'         tbecod
016900160518     c                   movel(p)  amecaut       tbeke1
017000160518     c     ktbe          chain     tntbe01l
017100160518     c                   if        %found(tntbe01l)
017200160518     c                   movel     tbeuni        dcau
017300160518     c                   movel     §caudes       vcautd
017400160518     c                   endif
017500151216     c* decodifica autista
017600151216     c                   movel     'XN'          cod
017700151216     c                   movel(P)  VCAU          key
017800151216     C     KTAB          CHAIN     TABEL
017900151216     c                   if        %found(tabel00f)
018000151216     C                   MOVEl     TBLUNI        dsxn
018100151216     c                   eval      vaut = §XNDES
018200151216     c                   else
018300151216     C                   MOVEl     *BLANKS       dsxn
018400151216     c                   clear                   vaut
018500151216     c                   end
018600000000     C     VTGA          IFEQ      *BLANKS
018700000000     C                   MOVEL     AMETGA        VTGA
018800000000     C                   MOVEL     AMETGN        VTGN
018900000000     C                   END
019000011203     C                   ELSE
019100011203     C                   SETON                                        99
019200011203     C                   LEAVE
019300000000     C                   END
019400000000     C* CONTROLLO DATE
019500011219     C                   MOVEL     *LOVAL        DTADAL
019600011219     C                   IF        VDAL <> 0
019700011219     C     *EUR          test(d)                 Vdal                   52
019800011219     c                   if        *in52
019900011130     C                   SETON                                        99
020000011130     C                   leave
020100011130     C                   END
020200011219     C                   MOVEL     Vdal          DTAEUR
020300011219     C                   MOVEL     DTAEUR        DTADAL
020400011130     C                   END
020500011219     c*
020600011219     C                   MOVEL     *hival        DTAAL
020700011219     C                   IF        VAL <> 0
020800011219     C     *EUR          test(d)                 VAL                    53
020900011219     c                   if        *in53
021000011219     C                   SETON                                        99
021100011219     C                   leave
021200011219     C                   END
021300011219     C                   MOVEL     VAL           DTAEUR
021400011219     C                   MOVEL     DTAEUR        DTAAL
021500011219     C                   END
021600011130     C*
021700011130     C                   IF        (VDAL <> 0 OR VAL <> 0) AND
021800011130     C                              DTADAL > DTAAL
021900011130     C                   SETON                                        525399
022000011130     C                   END
022100011130     C                   ENDDO
022200000000     C                   ENDSR
022300011130     C******************************************************
022400011130     C* RICERCA PRIMO VIDEO
022500011130     C******************************************************
022600011130     C     RIC01         BEGSR
022700011130     c                   select
022800011221     C                   when      §fld = 'VTGN' or §fld= 'VTGA'
022900011221     C                   CALL      'TRMZ02R4'    RICTAR
023000011130     C                   ENDsl
023100011130     C* POSIZIONAMENTO CURSORE
023200011130     C                   Z-ADD     §RIGA         §RIG
023300011130     C                   Z-ADD     §COLON        §COL
023400011130     C                   ENDSR
023500000000     C******************************************************
023600011130     C** IMMISSIONE MANUTENZIONE
023700011130     C******************************************************
023800000000     C     IMMSIN        BEGSR
023900011130     c                   clear                   par166
024000011130     c                   move      vnrf          p66nrf
024100011203     c                   z-add     hnrr          p66nig
024200011130     c                   move      '1'           p66flg
024300011130     c                   if        not *inkf
024400011130     c                   move      scel          p66mod
024500011130     c                   end
024600011130     c                   movel(P)  par166        kpjbu
024700011130     C                   CALL      'TRMZ26R2'
024800011130     C                   PARM                    KPJBA
024900011130     C                   MOVEL(p)  KPJBU         PAR166
025000000000     C                   ENDSR
025100000000     C******************************************************
025200011130     C** PULIZIA DEL SUBFILE
025300011130     C******************************************************
025400000000     C     PULIZ         BEGSR
025500000000     C                   SETON                                        70
025600000000     C                   WRITE     GA28DSC1
025700000000     C                   SETOFF                                       70
025800000000     C                   Z-ADD     0             NRR               4 0
025900000000     C                   ENDSR
026000000000     C******************************************************
026100011130     C** CARICAMENTO SUBFILE
026200011130     C******************************************************
026300000000     C     CARIC         BEGSR
026400011130     C                   MOVEL     *BLANKS       fld               1
026500011130     C                   MOVEL     *BLANKS       SCEL
026600000000     C                   MOVE      VNRF          IGMNRF
026700011130     C                   move      Dtadal        igmdat
026800011130     c                   if        val <> 0
026900011130     C                   move      Dtaal         al                8 0
027000011130     c                   else
027100011130     c                   z-add     99999999      al
027200011130     c                   end
027300011130     C     KIGM          SETLL     mzIGM000
027400011130     c                   do        *hival
027500011130     C     igmnrf        READE     mzIGM000                               21
027600000000     C  N21IGMDAT        COMP      AL                                 21
027700011203     c   21              leave
027800000000     C*
027900011130     C                   MOVE      igmdat        Dtaiso
028000011130     C                   MOVE      dtaiso        Dtaeur
028100011130     C                   move      Dtaeur        sdat
028200000000     C                   MOVE      IGMKM         SKM
028300011130     c                   if        igmdtf <> 0
028400011130     C                   MOVE      igmdtf        Dtaiso
028500011130     C                   MOVE      dtaiso        Dtaeur
028600011130     C                   move      Dtaeur        sdtf
028700011130     c                   end
028800000000     C                   MOVE      IGMOFB        SOFB
028900011130     C                   z-add     IGMiPV        SOPV
029000011130     C                   z-add     IGMiEF        SOEF
029100000000     C                   MOVE      IGMINT        SINT
029200000000     C                   MOVE      IGMOFF        SOFF
029300000000     C                   MOVE      IGMNSI        HNSI
029400000000     C                   MOVE      IGMNOT        HNOT
029500000000     C                   MOVE      IGMNRR        HNRR
029600000000     C                   MOVE      ' '           SNOT
029700011130     C     IGMNOT        IFNE      0
029800011212     c     knot          chain     mznot01l
029900011212     c                   if        %found(mznot01l)
030000000000     C                   MOVE      'D'           SNOT
030100011212     c                   else
030200011212     C                   MOVE      ' '           Snot
030300011212     c                   z-add     0             hnot
030400011212     C                   END
030500011130     C                   END
030600011130     c* intervento
030700011130     C                   exsr      srsint
030800011130     c* officina
030900011130     C                   exsr      srsofb
031000000000     C                   ADD       1             NRR
031100000000     C                   WRITE     GA28DSF1
031200000000     C                   END
031300000000     C                   Z-ADD     NRR           SVNRR             4 0
031400000000     C                   ENDSR
031500000000     C*-----------------------------------------------------
031600011130     C** CONTROLLI
031700011130     C*-----------------------------------------------------
031800000000     C     CONTR         BEGSR
031900011130     C*
032000000000     C                   READC     GA28DSF1                               06
032100000000     C     *IN06         DOWNE     '1'
032200011221     C                   IF        scel = '2' or scel = '4' or scel = '5'
032300011130     C                   EXSR      immsin
032400011130     c                   exsr      puliz
032500011130     c                   exsr      caric
032600011130     c                   leave
032700011130     C                   END
032800011130     C* NOTE AGGIUNTIVE
032900011130     C     SCEL          IFEQ      'D'
033000011130     C                   clear                   PAR191
033100011130     C                   Z-ADD     vnrf          P91Nrf
033200011130     C                   Z-ADD     hnot          P91NOT
033300011221     c* manutenzione note solo se non sono in visualizzazione
033400011221     c                   if        not *in20
033500011221     c                   if        hnot <> 0
033600011130     C                   MOVE      '2'           P91mod
033700011130     c                   end
033800011221     c                   else
033900011221     C                   MOVE      '5'           P91mod
034000011221     c                   end
034100011130     C                   MOVEL(p)  PAR191        KPJBU
034200011130     C                   CALL      'TRMZ26R3'
034300011130     C                   PARM                    KPJBA
034400011130     C                   MOVEL(p)  KPJBU         PAR191
034500011130     C                   MOVE      ' '           SCEL
034600011130     c* modifica/inserimento nota
034700011130     c                   if        p91ret = 'F'
034800011130     C                   z-add     P91NOT        hNOT
034900011130     C     hnrr          CHAIN     mzigm1                             21
035000011130     c                   if        not *in21
035100011130     C                   MOVE      p91NOT        igmNOT
035200011130     C                   UPDATE    mzigm1
035300011130     C                   END
035400011130     c                   end
035500011130     C                   END
035600011130     C     hnot          IFNE      0
035700011130     C                   MOVE      'D'           Snot
035800011130     c                   else
035900011130     C                   MOVE      ' '           Snot
036000011130     C                   END
036100000000      *
036200000000     C                   UPDATE    GA28DSF1
036300000000     C                   SETOFF                                           61
036400000000     C                   READC     GA28DSF1                               06
036500000000     C                   END
036600011130      *
036700000000     C                   ENDSR
036800011130     c**********************************************************************
036900011130     C** intervento
037000011130     c**********************************************************************
037100011130     C     srsint        BEGSR
037200011130     c*
037300011130     C     SINT          IFNE      *BLANK
037400011130     c                   movel     'XE'          parcod
037500011130     c                   movel(P)  sint          parkey
037600011130     c                   exsr      srtab
037700011130     c                   movel     parkey        sint
037800011130     c                   if        *in99
037900011130     C                   MOVEL     *blanks       sintd
038000011130     C                   SETON                                        63
038100011130     C                   else
038200011130     C                   MOVEL     TBLUNI        sintd
038300011130     C                   END
038400011130     C                   ELSE
038500011130     C                   MOVEL(p)  'SINISTRO'    SINTD
038600011130     C                   END
038700011130     c*
038800011130     C                   ENDSR
038900011130     c**********************************************************************
039000011130     C** officina
039100011130     c**********************************************************************
039200011130     C     srsofb        BEGSR
039300011130     c*
039400011130     c                   movel     'XO'          parcod
039500011130     c                   movel(P)  sofb          parkey
039600011130     c                   exsr      srtab
039700011130     c                   movel     parkey        sofb
039800011130     c                   if        *in99
039900011130     C                   MOVEL     *blanks       sofbd
040000011130     C                   SETON                                        80
040100011130     C                   else
040200011130     C                   MOVEL     TBLUNI        sofbd
040300011130     C                   END
040400011130     c*
040500011130     C                   ENDSR
040600011130     C**********************************************************************
040700011130     C** Controlli e decodifiche campi con tabel00f
040800011130     C**********************************************************************
040900011130     C     srtab         BEGSR
041000011130     c                   if        fld <> *blanks
041100011130     c                   movel     parkey        parkeys           8
041200011130     C                   CALL      'TRUL19R'
041300011130     c                   parm                    parcod            2
041400011130     c                   parm      '1'           parord            1
041500011130     c                   parm      *blanks       parkey            8
041600011130     c                   parm      *blanks       cmd               1
041700011130     c* ritorno senza scelta
041800011130     c                   if        cmd = 'L'
041900011130     c                   movel     parkeys       parkey
042000011130     C                   END
042100011130     C                   END
042200011130     C                   MOVEL     *blanks       tbluni
042300011130     C                   MOVEL     parcod        COD
042400011130     C                   MOVEL(p)  parkey        KEY
042500011130     C     KTAB          CHAIN     TABEL00f                           99
042600011130     c*
042700011130     C                   ENDSR
042800011130     C*----------------------------------------------------*
042900011130     c     *inzsr        begsr
043000011130     C*----------------------------------------------------*
043100011130     C* POSIZIONAMENTO TABELLE                             *
043200011130     C     KTAB          KLIST                                                  *
043300011130     C                   KFLD                    CODUT                          *
043400011130     C                   KFLD                    COD               2            *
043500011130     C                   KFLD                    KEY               8            *
043600160518      *
043700160518     C     KTbe          KLIST                                                  *
043800160518     C                   KFLD                    tbecod                         *
043900160518     C                   KFLD                    tbeke1                         *
044000011130     C*       GRANDI INTERVENTI mzIGM03L                   *
044100011130     C     KIGM          KLIST                                                  *
044200011130     C                   KFLD                    IGMNRF                         *
044300011130     C                   KFLD                    IGMDAT                         *
044400011130     C* CHAIN X N.DI TARGA                                 *
044500011130     C     KTAR          KLIST                                                  *
044600011130     C                   KFLD                    VTGA                           *
044700011130     C                   KFLD                    VTGN                           *
044800011212     C* mznot01L
044900011212     C     Knot          KLIST                                                  *
045000011212     C                   KFLD                    nottip                         *
045100011212     C                   KFLD                    igmnot                         *
045200011130     C*----------------------------------------------------*
045300011130     C* RICERCA TARGA X CITTA'
045400011130     C     RICTAR        PLIST
045500011130     C                   PARM                    CODUT
045600011130     C                   PARM                    RSUT
045700011130     C                   PARM                    VTGA
045800011130     C                   PARM                    VTGN
045900011130     C*
046000011130     C*----------------------------------------------------*
046100011130      *  CARICO COSTANTI
046200011130      *
046300011130      *
046400011130     C     *ENTRY        PLIST                                                  *
046500011130     C                   PARM                    KPJBA                          *
046600011130     C                   MOVE      1             CODUT
046700011130     C                   CALL      'XPARUT'
046800011130     C                   PARM                    PARUT
046900011221     C* CONTROLLO SE GESTIRE SOLO LA VISUALIZZAZIONE DEI RECORD IN BASE
047000011221     C* AL CODICE DELL'AZIONE
047100011221     C     KCDAZ         COMP      'MZ93'                                 20
047200011130     C*--------- SE DITTA INESISTENTE FINE LAVORO          *
047300011130     C     CODUT         ifeq      -1
047400011130     c                   seton                                        lr
047500011130     c                   end
047600011130     C                   MOVEL     RAGUT         RSUT             20
047700011130     c                   endsr
