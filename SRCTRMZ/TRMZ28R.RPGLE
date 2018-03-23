000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200000000     H*             - IMMISSIONE GRANDI MANUTENZIONI                 *
000300011130     FTRmz28D   CF   E             WORKSTN
000400000000     F                                     SFILE(GA28DSF1:NRR)
000500170906     F                                     INFDS(DSFMT)
000600011212     Fmznot01L  IF   E           K DISK
000700011212     FmzIGM01L  IF   E           K DISK
000800000000     F                                     INFDS(IGMINF)
000900011130     FmzIGM00F  UF   E             DISK
001000000000     F                                     RECNO(IGMNRR)
001100011130     F                                     RENAME(mzIGM000:mzIGM1)
001200011130     FmzAME01L  IF   E           K DISK
001300011130     F                                     RENAME(mzAME000:mzAME1)
001400011130     FmzAME02L  IF   E           K DISK
001500000000     FTABEL00F  IF   E           K DISK
001600160518     FTntbe01l  IF   E           K DISK
001700011130     D DTAiso          S               D   INZ
001800011130     D DTAEUR          S               D   DATFMT(*EUR) INZ
001900011130     D DTADAL          S               D   INZ
002000011130     D DTAAL           S               D   INZ
002100151216     D dsxn          e ds
002200160518     D dmau          E DS
002300160518     D dcau          E DS
002400000000     D PARUT         E DS                  EXTNAME(UTEDSE0F)
002500000000     D KPJBA         E DS
002600170906     D*-------------
002700170906     D DSFMT           DS           512
002800170906     D  $TASTO               369    369
002900170906     D  NRG                  370    370
003000170906     D  NCL                  371    371
003100170906     D  SFLNRR               378    379B 0
003200000000     D*----------DS PER IL NUMERO RELATIVO ASSOLUTO-
003300000000     D IGMINF          DS
003400000000     D  IGMNRR               397    400B 0
003500011130     D*GESTIONE SINISTRO
003600011130     D PAR166          DS
003700011130     D  p66nrf                        7  0
003800011203     D  p66nsi                        9  0
003900011203     D  p66nig                        9  0
004000011130     D  p66flg                        1
004100011130     c* modalità con cui chiamo il pgm
004200011130     D  p66mod                        1
004300011130     c* Cmd di ritorno
004400011130     D  p66ret                        1
004500011130     D*GESTIONE NOTE
004600011130     D PAR191          DS
004700011130     D* numero riferimento
004800011130     D  P91nrf                        7  0
004900011130     D* tipo nota
005000011130     D  P91tip                        2
005100011130     D* numero nota
005200011130     D  P91NOT                        7  0
005300011130     c* modalità con cui chiamo il pgm
005400011130     D  p91mod                        1
005500011130     c* Cmd di ritorno
005600011130     D  p91ret                        1
005700170906      **********************************************************************
005800170906      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
005900170906      **********************************************************************
006000170906      *
006100170906      * ?Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
006200170906     D MaxKey          C                   1
006300170906$xxx D Ord_Ragsoc      C                   '1'
006400170906     D Ascendente      C                   1
006500170906     D Discendente     C                   2
006600170906     D Carattere       C                   6
006700170906     D Put             C                   1
006800170906     D EndPut          C                   2
006900170906     D Get             C                   3
007000170906     D Numerico        C                   8
007100170906      **********************************************************************
007200170906      * Campi utili:
007300170906      *     RRN1       - Numero relativo di record del Subfile
007400170906      *     SizeList   - Dimensione della lista
007500170906      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
007600170906      **********************************************************************
007700170906     D Rrn1            S              5I 0
007800170906     D NotUsed         S             16A
007900170906     D ReturnSize      S             10i 0
008000170906     D SizeList        S             10i 0
008100170906     D RrnLast         S              5I 0
008200170906     D WrkSort         S              1    inz('1')
008300170906      **********************************************************************
008400170906      * Data Structures
008500170906      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
008600170906      *     QLGSCB     - The sort request block for the QLGSORT API
008700170906      *     QLGSCB00   - The sort request block for the QLGSRTIO API
008800170906      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
008900170906      *     QUSEC      - Error structure for the QLGSORT API
009000170906      **********************************************************************
009100170906     D SflRcd          DS
009200170906     D  HDAT
009300170906     D  SDAT
009400170906     D  SCEL
009500170906     D  SNOT
009600170906     D  SINTD
009700170906     d  SKM
009800170906     d  SOFBD
009900170906     d  SDTF
010000170906     d  SOPV
010100170906     d  SOFF
010200170906     d  SOEF
010300170906     d  SOFB
010400170906     d  SINT
010500170906     d  HNSI
010600170906     d  HNOT
010700170906     d  HNRR
010800170906     D  Selected                      1A
010900170906
011000170906      /COPY QSYSINC/QRPGLESRC,QLGSORT
011100170906     D QLGKL                         16    DIM(MaxKey)
011200170906     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
011300170906     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
011400170906     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
011500170906     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
011600170906
011700170906      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
011800170906      /COPY QSYSINC/QRPGLESRC,QUSEC
011900170906
012000170906     C*----------------------------------------------------*
012100000000      * INDICATORI
012200000000      *
012300000000      * 22 SERVIZIO
012400000000      * 50 N.RIFERIMENTO ERRATO
012500000000      * 51 N.TARGA ERRATO
012600000000      * 52 DATA ERRATA (DAL)
012700000000      * 53 DATA ERRATA (AL)
012800000000      * 54 COMP DATA
012900000000      * 70 PULIZIA SUBF1
013000170906     C*----------------------------------------------------*
013100011130     C  nlr              DO        *HIVAL
013200011130     c                   EXSR      CLR00
013300011130     c                   if        *in99
013400011130     c                   setoff                                       99
013500011130     c                   write     ga28d001
013600011130     c                   seton                                        99
013700011130     c                   end
013800000000     C                   EXFMT     GA28D001
013900011130     C* FINE LAVORO
014000011130     C   KC              LEAVE
014100011130     C* RICERCA
014200011130     C   KD              EXSR      RIC01
014300050720     C* anagrafica automezzi
014400050720     c   kg              call      'TRMZ02R'
014500050720     c                   parm                    kpjba
014600050720     C   kg              ITER
014700000000     C* CONTROLLI
014800000000     C                   EXSR      CTRL01
014900011130     C   99              ITER
015000000000     C* NUOVA MANUTENZIONE
015100011130     C                   IF        *INKF
015200011203     C                   Z-ADD     0             hnrr
015300011130     C                   EXSR      IMMSIn
015400011130     C                   ITER
015500011130     C                   END
015600000000     C* PULIZIA SUBFILE
015700000000     C                   EXSR      PULIZ
015800000000     C* CARICAMENTO SUBFILE
015900000000     C                   EXSR      CARIC
016000011130     C                   DO        *HIVAL
016100000000     C                   WRITE     GA28D002
016200011130     C     NRR           COMP      0                                  7171
016300000000     C                   EXFMT     GA28DSC1
016400000000     C                   SETON                                        71
016500011130     C* FINE  LAVORO
016600011130     C   KC              LEAVE
016700011130     C* FINE  LAVORO
016800011130     C   Kl              LEAVE
016900011130     C* NUOVO SINISTRO
017000011130     C                   IF        *INKF
017100011203     C                   Z-ADD     0             hnrr
017200011130     C                   EXSR      IMMSIn
017300011130     C     P66RET        IFEQ      'F'
017400011130     c                   exsr      puliz
017500011130     c                   exsr      caric
017600011130     C                   END
017700011130     C                   ITER
017800011130     C                   END
017900000000     C* CONTROLLI
018000000000     C     NRR           IFNE      0
018100000000     C                   EXSR      CONTR
018200000000     C                   END
018300011130     C                   ENDDO
018400011130     C   KC              LEAVE
018500011130     C                   ENDDO
018600011130     c*
018700000000     C                   SETON                                        LR
018800011130     c**********************************************************************
018900011130     c* pulisce 1° formato
019000011130     c**********************************************************************
019100011130     C     clr00         BEGSR
019200011130     C                   Z-ADD     0             §RIGA
019300011130     C                   Z-ADD     0             §COLON
019400011130     C                   CLEAR                   §FLD
019500011130     C                   ENDSR
019600000000     C******************************************************
019700000000     C** CONTROLLI FORMATO ENTRATA
019800011130     C******************************************************
019900011130     C     CTRL01        BEGSR
020000011203     C                   SETOFF                                       99
020100011130     C                   DO
020200011130     C* SCELTA N.RIF. O
020300011130     C* N.TARGA
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
021500000000     C     VTGA          IFNE      *BLANKS
021600000000     C     VTGN          ANDNE     *BLANKS
021700011130     C     KTAR          CHAIN     mzAME000                           51
021800011130     C                   IF        NOT *IN51
021900011130     C                   Z-ADD     AMENRF        VNRF
022000011130     C                   ELSE
022100011130     C                   SETON                                        99
022200011130     C                   LEAVE
022300011130     C                   END
022400000000     C                   END
022500011130     C     VNRF          CHAIN     mzAME1                             50
022600011130     C                   IF        NOT *IN50
022700011130     C                   MOVEL     AMEdit        Vdit
022800151216     C                   MOVEL     AMEcau        Vcau
022900160518     C                   MOVEL     AMEmau        VMAu
023000160518     C                   MOVEL     AMEcaut       Vcaut
023100170906     **
023200170906     c                   z-add     ameFIL        vFIL
023300170906     c                   movel     ameCDC        vFCDC
023400170907     c                   movel     ameIMM        an2               2
023500170907     c                   move      ameIMM        me2               2
023600170907     c                   movel     me2           vMEAN
023700170907     c                   move      an2           vMEAN
023800170906     **
023900160518     c                   movel     'MAU'         tbecod
024000160518     c                   movel(p)  amemau        tbeke1
024100160518     c     ktbe          chain     tntbe01l
024200160518     c                   if        %found(tntbe01l)
024300160518     c                   movel     tbeuni        dmau
024400160518     c                   movel     §maudes       vmaud
024500160518     c                   endif
024600160518     c                   movel     'CAU'         tbecod
024700160518     c                   movel(p)  amecaut       tbeke1
024800160518     c     ktbe          chain     tntbe01l
024900160518     c                   if        %found(tntbe01l)
025000160518     c                   movel     tbeuni        dcau
025100160518     c                   movel     §caudes       vcautd
025200160518     c                   endif
025300151216     c* decodifica autista
025400151216     c                   movel     'XN'          cod
025500151216     c                   movel(P)  VCAU          key
025600151216     C     KTAB          CHAIN     TABEL
025700151216     c                   if        %found(tabel00f)
025800151216     C                   MOVEl     TBLUNI        dsxn
025900151216     c                   eval      vaut = §XNDES
026000170906     c                   eval      vfun = §XNFUN
026100151216     c                   else
026200151216     C                   MOVEl     *BLANKS       dsxn
026300151216     c                   clear                   vaut
026400170906     c                   clear                   vfun
026500151216     c                   end
026600000000     C     VTGA          IFEQ      *BLANKS
026700000000     C                   MOVEL     AMETGA        VTGA
026800000000     C                   MOVEL     AMETGN        VTGN
026900000000     C                   END
027000011203     C                   ELSE
027100011203     C                   SETON                                        99
027200011203     C                   LEAVE
027300000000     C                   END
027400000000     C* CONTROLLO DATE
027500011219     C                   MOVEL     *LOVAL        DTADAL
027600011219     C                   IF        VDAL <> 0
027700011219     C     *EUR          test(d)                 Vdal                   52
027800011219     c                   if        *in52
027900011130     C                   SETON                                        99
028000011130     C                   leave
028100011130     C                   END
028200011219     C                   MOVEL     Vdal          DTAEUR
028300011219     C                   MOVEL     DTAEUR        DTADAL
028400011130     C                   END
028500011219     c*
028600011219     C                   MOVEL     *hival        DTAAL
028700011219     C                   IF        VAL <> 0
028800011219     C     *EUR          test(d)                 VAL                    53
028900011219     c                   if        *in53
029000011219     C                   SETON                                        99
029100011219     C                   leave
029200011219     C                   END
029300011219     C                   MOVEL     VAL           DTAEUR
029400011219     C                   MOVEL     DTAEUR        DTAAL
029500011219     C                   END
029600011130     C*
029700011130     C                   IF        (VDAL <> 0 OR VAL <> 0) AND
029800011130     C                              DTADAL > DTAAL
029900011130     C                   SETON                                        525399
030000011130     C                   END
030100011130     C                   ENDDO
030200000000     C                   ENDSR
030300011130     C******************************************************
030400011130     C* RICERCA PRIMO VIDEO
030500011130     C******************************************************
030600011130     C     RIC01         BEGSR
030700011130     c                   select
030800011221     C                   when      §fld = 'VTGN' or §fld= 'VTGA'
030900011221     C                   CALL      'TRMZ02R4'    RICTAR
031000011130     C                   ENDsl
031100011130     C* POSIZIONAMENTO CURSORE
031200011130     C                   Z-ADD     §RIGA         §RIG
031300011130     C                   Z-ADD     §COLON        §COL
031400011130     C                   ENDSR
031500000000     C******************************************************
031600011130     C** IMMISSIONE MANUTENZIONE
031700011130     C******************************************************
031800000000     C     IMMSIN        BEGSR
031900011130     c                   clear                   par166
032000011130     c                   move      vnrf          p66nrf
032100011203     c                   z-add     hnrr          p66nig
032200011130     c                   move      '1'           p66flg
032300011130     c                   if        not *inkf
032400011130     c                   move      scel          p66mod
032500011130     c                   end
032600011130     c                   movel(P)  par166        kpjbu
032700011130     C                   CALL      'TRMZ26R2'
032800011130     C                   PARM                    KPJBA
032900011130     C                   MOVEL(p)  KPJBU         PAR166
033000000000     C                   ENDSR
033100000000     C******************************************************
033200011130     C** PULIZIA DEL SUBFILE
033300011130     C******************************************************
033400000000     C     PULIZ         BEGSR
033500000000     C                   SETON                                        70
033600000000     C                   WRITE     GA28DSC1
033700000000     C                   SETOFF                                       70
033800000000     C                   Z-ADD     0             NRR               4 0
033900000000     C                   ENDSR
034000000000     C******************************************************
034100011130     C** CARICAMENTO SUBFILE
034200011130     C******************************************************
034300000000     C     CARIC         BEGSR
034400011130     C                   MOVEL     *BLANKS       fld               1
034500011130     C                   MOVEL     *BLANKS       SCEL
034600000000     C                   MOVE      VNRF          IGMNRF
034700011130     C                   move      Dtadal        igmdat
034800170906      *
034900011130     c                   if        val <> 0
035000011130     C                   move      Dtaal         al                8 0
035100011130     c                   else
035200011130     c                   z-add     99999999      al
035300011130     c                   end
035400170906      *
035500011130     C     KIGM          SETLL     mzIGM000
035600170906      *
035700011130     c                   do        *hival
035800011130     C     igmnrf        READE     mzIGM000                               21
035900170906      *
036000000000     C  N21IGMDAT        COMP      AL                                 21
036100011203     c   21              leave
036200170906      *
036300170906     C                   clear                   GA28DSF1
036400000000     C*
036500011130     C                   MOVE      igmdat        Dtaiso
036600011130     C                   MOVE      dtaiso        Dtaeur
036700011130     C                   move      Dtaeur        sdat
036800170906     C                   MOVE      igmdat        HDAT
036900000000     C                   MOVE      IGMKM         SKM
037000011130     c                   if        igmdtf <> 0
037100011130     C                   MOVE      igmdtf        Dtaiso
037200011130     C                   MOVE      dtaiso        Dtaeur
037300011130     C                   move      Dtaeur        sdtf
037400011130     c                   end
037500000000     C                   MOVE      IGMOFB        SOFB
037600011130     C                   z-add     IGMiPV        SOPV
037700011130     C                   z-add     IGMiEF        SOEF
037800000000     C                   MOVE      IGMINT        SINT
037900000000     C                   MOVE      IGMOFF        SOFF
038000000000     C                   MOVE      IGMNSI        HNSI
038100000000     C                   MOVE      IGMNOT        HNOT
038200000000     C                   MOVE      IGMNRR        HNRR
038300000000     C                   MOVE      ' '           SNOT
038400011130     C     IGMNOT        IFNE      0
038500011212     c     knot          chain     mznot01l
038600011212     c                   if        %found(mznot01l)
038700000000     C                   MOVE      'D'           SNOT
038800011212     c                   else
038900011212     C                   MOVE      ' '           Snot
039000011212     c                   z-add     0             hnot
039100011212     C                   END
039200011130     C                   END
039300011130     c* intervento
039400011130     C                   exsr      srsint
039500011130     c* officina
039600011130     C                   exsr      srsofb
039700000000     C                   ADD       1             NRR
039800000000     C                   WRITE     GA28DSF1
039900000000     C                   END
040000000000     C                   Z-ADD     NRR           SVNRR             4 0
040100170906      ***
040200170906      **  riordina il SFL
040300170906     c                   exsr      ordina_s1
040400170906      ***
040500000000     C                   ENDSR
040600000000     C*-----------------------------------------------------
040700011130     C** CONTROLLI
040800011130     C*-----------------------------------------------------
040900000000     C     CONTR         BEGSR
041000011130     C*
041100000000     C                   READC     GA28DSF1                               06
041200000000     C     *IN06         DOWNE     '1'
041300011221     C                   IF        scel = '2' or scel = '4' or scel = '5'
041400011130     C                   EXSR      immsin
041500011130     c                   exsr      puliz
041600011130     c                   exsr      caric
041700011130     c                   leave
041800011130     C                   END
041900011130     C* NOTE AGGIUNTIVE
042000011130     C     SCEL          IFEQ      'D'
042100011130     C                   clear                   PAR191
042200011130     C                   Z-ADD     vnrf          P91Nrf
042300011130     C                   Z-ADD     hnot          P91NOT
042400011221     c* manutenzione note solo se non sono in visualizzazione
042500011221     c                   if        not *in20
042600011221     c                   if        hnot <> 0
042700011130     C                   MOVE      '2'           P91mod
042800011130     c                   end
042900011221     c                   else
043000011221     C                   MOVE      '5'           P91mod
043100011221     c                   end
043200011130     C                   MOVEL(p)  PAR191        KPJBU
043300011130     C                   CALL      'TRMZ26R3'
043400011130     C                   PARM                    KPJBA
043500011130     C                   MOVEL(p)  KPJBU         PAR191
043600011130     C                   MOVE      ' '           SCEL
043700011130     c* modifica/inserimento nota
043800011130     c                   if        p91ret = 'F'
043900011130     C                   z-add     P91NOT        hNOT
044000011130     C     hnrr          CHAIN     mzigm1                             21
044100011130     c                   if        not *in21
044200011130     C                   MOVE      p91NOT        igmNOT
044300011130     C                   UPDATE    mzigm1
044400011130     C                   END
044500011130     c                   end
044600011130     C                   END
044700011130     C     hnot          IFNE      0
044800011130     C                   MOVE      'D'           Snot
044900011130     c                   else
045000011130     C                   MOVE      ' '           Snot
045100011130     C                   END
045200000000      *
045300000000     C                   UPDATE    GA28DSF1
045400000000     C                   SETOFF                                           61
045500000000     C                   READC     GA28DSF1                               06
045600000000     C                   END
045700011130      *
045800000000     C                   ENDSR
045900011130     c**********************************************************************
046000011130     C** intervento
046100011130     c**********************************************************************
046200011130     C     srsint        BEGSR
046300011130     c*
046400011130     C     SINT          IFNE      *BLANK
046500011130     c                   movel     'XE'          parcod
046600011130     c                   movel(P)  sint          parkey
046700011130     c                   exsr      srtab
046800011130     c                   movel     parkey        sint
046900011130     c                   if        *in99
047000011130     C                   MOVEL     *blanks       sintd
047100011130     C                   SETON                                        63
047200011130     C                   else
047300011130     C                   MOVEL     TBLUNI        sintd
047400011130     C                   END
047500011130     C                   ELSE
047600011130     C                   MOVEL(p)  'SINISTRO'    SINTD
047700011130     C                   END
047800011130     c*
047900011130     C                   ENDSR
048000011130     c**********************************************************************
048100011130     C** officina
048200011130     c**********************************************************************
048300011130     C     srsofb        BEGSR
048400011130     c*
048500011130     c                   movel     'XO'          parcod
048600011130     c                   movel(P)  sofb          parkey
048700011130     c                   exsr      srtab
048800011130     c                   movel     parkey        sofb
048900011130     c                   if        *in99
049000011130     C                   MOVEL     *blanks       sofbd
049100011130     C                   SETON                                        80
049200011130     C                   else
049300011130     C                   MOVEL     TBLUNI        sofbd
049400011130     C                   END
049500011130     c*
049600011130     C                   ENDSR
049700011130     C**********************************************************************
049800011130     C** Controlli e decodifiche campi con tabel00f
049900011130     C**********************************************************************
050000011130     C     srtab         BEGSR
050100011130     c                   if        fld <> *blanks
050200011130     c                   movel     parkey        parkeys           8
050300011130     C                   CALL      'TRUL19R'
050400011130     c                   parm                    parcod            2
050500011130     c                   parm      '1'           parord            1
050600011130     c                   parm      *blanks       parkey            8
050700011130     c                   parm      *blanks       cmd               1
050800011130     c* ritorno senza scelta
050900011130     c                   if        cmd = 'L'
051000011130     c                   movel     parkeys       parkey
051100011130     C                   END
051200011130     C                   END
051300011130     C                   MOVEL     *blanks       tbluni
051400011130     C                   MOVEL     parcod        COD
051500011130     C                   MOVEL(p)  parkey        KEY
051600011130     C     KTAB          CHAIN     TABEL00f                           99
051700011130     c*
051800011130     C                   ENDSR
051900011130     C*----------------------------------------------------*
052000011130     c     *inzsr        begsr
052100011130     C*----------------------------------------------------*
052200011130     C* POSIZIONAMENTO TABELLE                             *
052300011130     C     KTAB          KLIST                                                  *
052400011130     C                   KFLD                    CODUT                          *
052500011130     C                   KFLD                    COD               2            *
052600011130     C                   KFLD                    KEY               8            *
052700160518      *
052800160518     C     KTbe          KLIST                                                  *
052900160518     C                   KFLD                    tbecod                         *
053000160518     C                   KFLD                    tbeke1                         *
053100011130     C*       GRANDI INTERVENTI mzIGM03L                   *
053200011130     C     KIGM          KLIST                                                  *
053300011130     C                   KFLD                    IGMNRF                         *
053400011130     C                   KFLD                    IGMDAT                         *
053500011130     C* CHAIN X N.DI TARGA                                 *
053600011130     C     KTAR          KLIST                                                  *
053700011130     C                   KFLD                    VTGA                           *
053800011130     C                   KFLD                    VTGN                           *
053900011212     C* mznot01L
054000011212     C     Knot          KLIST                                                  *
054100011212     C                   KFLD                    nottip                         *
054200011212     C                   KFLD                    igmnot                         *
054300011130     C*----------------------------------------------------*
054400011130     C* RICERCA TARGA X CITTA'
054500011130     C     RICTAR        PLIST
054600011130     C                   PARM                    CODUT
054700011130     C                   PARM                    RSUT
054800011130     C                   PARM                    VTGA
054900011130     C                   PARM                    VTGN
055000011130     C*
055100011130     C*----------------------------------------------------*
055200011130      *  CARICO COSTANTI
055300011130      *
055400011130      *
055500011130     C     *ENTRY        PLIST                                                  *
055600011130     C                   PARM                    KPJBA                          *
055700011130     C                   MOVE      1             CODUT
055800011130     C                   CALL      'XPARUT'
055900011130     C                   PARM                    PARUT
056000011221     C* CONTROLLO SE GESTIRE SOLO LA VISUALIZZAZIONE DEI RECORD IN BASE
056100011221     C* AL CODICE DELL'AZIONE
056200011221     C     KCDAZ         COMP      'MZ93'                                 20
056300011130     C*--------- SE DITTA INESISTENTE FINE LAVORO          *
056400011130     C     CODUT         ifeq      -1
056500011130     c                   seton                                        lr
056600011130     c                   end
056700011130     C                   MOVEL     RAGUT         RSUT             20
056800011130     c                   endsr
056900170906      * ?-------------------------------------------------------------*?
057000170906      *  Riordina comunque il SFL                                     ?
057100170906      * ?-------------------------------------------------------------*?
057200170906     C     Ordina_S1     BEGSR
057300170906     C*
057400170906      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
057500170906      * presente nel subfile -- "Selected"?-- questo è aggiunto al record.
057600170906      * Il campo è usato per selezionare i records dando un ordine a quelli
057700170906      * selezionati davanti ai non selezionati.
057800170906     C                   CLEAR                   QLGSCB
057900170906     C                   CLEAR                   QLGSCB00
058000170906      *
058100170906      *   Data INIZIO    ?
058200170906     C                   EVAL      QLGNBRK    = 1
058300170906
058400170906     ** la DATA è in posizione (1) 8 Bytes num descending
058500170906     C                   EVAL      QLGSP      = 1
058600170906     C                   EVAL      QLGSS      = %SIZE(Hdat)
058700170906     C                   EVAL      QLGDT      = Numerico
058800170906     C                   EVAL      QLGSO      = Discendente
058900170906     C                   EVAL      QLGKL(1)   = QLGSKL
059000170906      *------------
059100170906      * Load other sort parameters.
059200170906     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
059300170906     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
059400170906     C                   EVAL      QLGRT     = 8
059500170906     C                   EVAL      QLGOKL    = 80
059600170906     C                   EVAL      QLGLKE    = 16
059700170906     C                   EVAL      QLGLSS    = 290
059800170906
059900170906      * Initialize Sort I/O API fields.
060000170906     C                   EVAL      QLGRL00  = QLGRL
060100170906     C                   EVAL      QLGRC00  = 1
060200170906     C                   CLEAR                   QUSEI
060300170906     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
060400170906      * First step - Initialize the sort routine.
060500170906     C                   CALL      'QLGSORT'
060600170906     C                   PARM                    QLGSCB
060700170906     C                   PARM                    NotUsed
060800170906     C                   PARM                    NotUsed
060900170906     C                   PARM                    SizeList
061000170906     C                   PARM                    ReturnSize
061100170906     C                   PARM                    QUSEC
061200170906
061300170906      * Next step - Write records to I/O routine.
061400170906     C                   EVAL      QLGRT00 = Put
061500170906
061600170906     C                   DO        SVNRR         nrr
061700170906     C     NRR           CHAIN     GA28DSF1
061800170906
061900170906     ** Solo le righe con Selected = 'Y' sono riordinate,
062000170906     ** quindi per fare un ordinamento di tutte le righe
062100170906     ** metto 'Y' sempre.
062200170906     C                   EVAL      Selected  = 'Y'
062300170906
062400170906     C                   CLEAR                   QUSEI
062500170906     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
062600170906      *
062700170906     C                   CALL      'QLGSRTIO'
062800170906     C                   PARM                    QLGSCB00
062900170906     C                   PARM                    SflRcd
063000170906     C                   PARM                    NotUsed
063100170906     C                   PARM                    SizeList
063200170906     C                   PARM                    NotUsed
063300170906     C                   PARM                    QUSEC
063400170906
063500170906     C                   ENDDO
063600170906
063700170906      * Next step - Signal end of input, clear subfile for reload.
063800170906     C                   EVAL      QLGRT00 = EndPut
063900170906     C                   CLEAR                   QUSEI
064000170906     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
064100170906     C                   CALL      'QLGSRTIO'
064200170906     C                   PARM                    QLGSCB00
064300170906     C                   PARM                    SflRcd
064400170906     C                   PARM                    NotUsed
064500170906     C                   PARM                    SizeList
064600170906     C                   PARM                    NotUsed
064700170906     C                   PARM                    QUSEC
064800170906      *
064900170906    >C                   EXSR      PULIZ
065000170906
065100170906      * Final step - Write the records back to the subfile.
065200170906     C                   EVAL      QLGRT00 = Get
065300170906
065400170906     C                   DO        SVNRR         nrr
065500170906     C                   CLEAR                   QUSEI
065600170906     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
065700170906     C                   CALL      'QLGSRTIO'
065800170906     C                   PARM                    QLGSCB00
065900170906     C                   PARM                    NotUsed
066000170906     C                   PARM                    SflRcd
066100170906     C                   PARM                    QLGRL00
066200170906     C                   PARM                    NotUsed
066300170906     C                   PARM                    QUSEC
066400170906      *
066500170906     C                   WRITE     GA28DSF1
066600170906     C                   ENDDO
066700170906      *
066800170906      *  All'uscita di queste chiamate a routine di sistema il SFL record
066900170906      *   si incrementa stranamente quindi lo reimposto correttamente
067000170906      *    prima di lasciare la routine.
067100170906     C                   EVAL      nrr = SVNRR
067200170906     C*
067300170906     C                   ENDSR
067400170906     C/EJECT
067500170906      * ?-------------------------------------------------------------*?
