000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200130703     H* FNLSS6R *----------------------------------------------------*
000300130715     H*         - Filtro Stampa Spedizioni Spareggiate               *
000500000000     H*--------------------------------------------------------------*
000600130703     FFNLSS6D   CF   E             WORKSTN
000800940913     FFNFGV01L  IF   E           K DISK
000900000427     FFNFGW01L  IF   E           K DISK
001000960628     FTABEL00F  IF   E           K DISK
001100900509     FAZORG01L  IF   E           K DISK
001200130612     D MSG             S             78    DIM(10) CTDATA PERRCD(1)
001300931130     D****
001400130703     D* PASSAGGIO DATI A pgm batch
001500130703     D PARAM           DS
001600900518     D  VIDFFV                 1      3  0
001700130808     D  nfv                    4      8  0
003300941007     D***
008900130520     D* CHIAMATA AL PGM DI INTERROGAZ.FV PARTENZA             -FNLST3R-
009000130520     D fnlst3ds        DS
009100951108     D  PA3FLG                 1      1    INZ('R')
009200951108     D  PA3FL2                 2      2    INZ
009300951108     D  PA3NFV                 3      5P 0 INZ
009400951108     D  PA3DIN                 6     10P 0 INZ
009500000612     D  PA3DFI                11     15P 0
009600951108     D  PA3TFV                16     16    INZ('2')
009700951108     D* CAMPO USATO PER PASSARE AL PGM CHIAMANTE TUTTI I FOGLI VIAGGIO
009800951108     D*   SELEZIONATI ---> SE NON UTILIZZATO NON LO DEFINISCO A PGM
009900951108     D***                                 P  17 2410FVA
010000000612     D  PA3FGS               242    244  0
010100900530     D WLBDAT          DS
010200940913     D  G02DAT                 1      8  0
010300940913     D  G02INV                 9     16  0
010400940913     D  G02ERR                17     17
010500940913     D  G02TGI                18     22  0
010600900530     D WGIDAT          DS
010700940914     D  GIODAT                 1      8  0
010800940914     D  GIOINV                 9     16  0
010900940914     D  GIOTGI                17     21  0
012600900511     D KPJBA         E DS
012601130703     d Tibs34ds      e ds                  inz
012602130703     d §azute        e ds                  Extname(Azute00f)
012603130703     d                                     dtaara
012604130703     d §Datiute      e ds                  EXTname(ddatiute)
012605130703     d                                     dtaara
013200960628     D DSTV          E DS
013500020813     D OG143         e ds                  inz
013600130612     D tibs02ds      E DS
013601130808     D                 DS
013602130808     d  vuoto                  1      3
013603130808     D  FGVFFV                 4    243
013604130808     D  FGVFF2               244    450
013605130808     D  FGWFF3               451    690
013606130808     D  FGWFF4               691    900
013607130808     D**VUOTO                898    900
013608130808     D  FFV                    1    900
013609130808     D                                     DIM(300)
013800020813      *
013900020813      * Variabili
014000020813      *
014201130808     d i               s              4  0
014202130808     d codut           s              1  0 inz(1)
014300130612      //---------------------------------------------------------------
014400130612      /copy gaitrasrc/srcprotopr,tibs02r
014401130703      /copy gaitrasrc/srcprotopr,tibs34r
014500130612
014600900521     C****************************************************************
014700900521     C*  RIEPILOGO INDICATORI
014800900521     C***************************************************************
015100940315     C* 18    - TESTN SUL NUMERO FOGLIO VIAGGIO
015200900521     C* 40/45 - ERRORE
015300900521     C* 31/32 - DI COMODO
015600900521     C* 90    - ON INDICA CHE C'E' UN ERRORE NEI CAMPI
015800900521     C*
015900900521     C*****************************************************************
016000000000     C     *ENTRY        PLIST
016100000000     C                   PARM                    KPJBA
016101130703     c                   exsr      DatiJob
017500900511     C*---------------------------------------------------------------*
017600900521     C* ACCESSO   FILE NUMERATORE
017700960628     C     KTAB          KLIST
017800960628     C                   KFLD                    CODUT
017900960628     C                   KFLD                    COD               2
018000960628     C                   KFLD                    KEY               8
018100020807     C     K02FGV01      klist
018200020807     C                   kfld                    COMNFV
018300020807     C                   kfld                    SIMfel
018400020807     C     K02FGW01      klist
018500020807     C                   kfld                    FGVnfv
018600020807     C                   kfld                    FGVlnp
018700931130     C*---------------------------------------------------------------*
018800931130     C                   EXSR      CARTAB
018900020813     C*
019000020813     C* Imposto il P.O. di gestione
020300020813     C                   z-add     SIMfel        VIDpog
020400020813     C     VIDpog        chain     AZORG
020500020813if  2C                   if        %found(AZORG01L)
020600020813     C                   movel     ORGdes        DESpog
020700020813     C                   movel     ORGde3        OG143
020800020813x   2C                   else
020900020813     C                   clear                   DESpog
021000020813     C                   clear                   OG143
021100020813e   2C                   endif
021300931130     C*
021500940315     C                   MOVEL     '?    '       VIDNFV
021800931130     C*
021901130808     c                   dow       not *inkf or *in90
022000130715     C                   WRITE     LSs6T01
022100130715     C                   EXFMT     LSs6D01
022200940914     C                   SETOFF                                       28
022300940914     C*
022400130612     C** F3 - FINE LAVORO
022401130715     c                   if        *inkc
022600130715     C                   leave
022700130612     c                   endif
022800130612     c
022900931130     C* CONTROLLI FOGLIO VIAGGIO
023000931130     C                   EXSR      CONTR1
023001130715     c                   enddo
023002130715
023003130715     c* F6=conferma: lancio batch di stampa
023101130715     c                   if        *inkf
023102130808     C     K02FGV01      chain     FNFGW01L                           30
023103130808     C     *IN30         IFEQ      *ON
023104130808     C     FGWATB        ORNE      ' '
023105130808     C                   CLEAR                   FGWFF3
023106130808     C                   CLEAR                   FGWFF4
023107130808     C                   CLEAR                   FGWFL3
023108130808     C                   CLEAR                   FGWFL4
023109130808     C                   CLEAR                   FGWFGS
023110130808     C                   ENDIF
023111130808     c* richiamo batch per ogni lna del foglio
023112130808     c                   move      vidnfv        nfv
023113130808     c                   movel     fgvlna        vuoto
023114130808     c                   z-add     1             i
023115130808     c                   dow       ffv(i)>*zeros
023116130808     c                   move      ffv(i)        vidffv
023117130808     C                   MOVEL     PARAM         KPJBU
023118130808     C                   MOVEL     'LSS7'        KCOAZ
023119130808     C                   CALL      'BCH10'
023120130808     C                   PARM                    KPJBA
023121130808     c                   add       1             i
023122130808     c                   enddo
023123130715     c                   endif
036100931130     C*
036300000000     C                   SETON                                        LR
036400940913     C**************************************************************************
036500940913     C* CARICAMENTO INIZIALI
036600940913     C**************************************************************************
036700931130     C     CARTAB        BEGSR
036800931130     C*
036900940914     C* GIRO LA DATA DEL JOB
037000940914     C                   TIME                    WTIME            14 0
037100940914     C                   MOVE      WTIME         WDATE             8 0
037200940914     C                   MOVE      WDATE         G02DAT
037300940914     C                   MOVEL     *ZERO         G02INV
037400931130     C                   MOVEL     *BLANK        G02ERR
037500940913     C                   CALL      'XSRDA8'
037600931130     C                   PARM                    WLBDAT
037700940914     C                   Z-ADD     G02INV        DATEU             8 0
040400940913     C*
040500931130     C                   ENDSR
040600940913     C**************************************************************************
040700940913     C* CONTROLLI SUL FOGLIO VIAGGIO
040800940913     C**************************************************************************
040900931130     C     CONTR1        BEGSR
041000940913     C*
041100940914     C                   SETOFF                                       90  40
041200940315     C*
041300940315     C     '?'           SCAN      VIDNFV                                 90
041400940315     C     *IN90         IFEQ      *ON
041500130520     C                   RESET                   fnlst3ds
041600940914     C* DATA FINALE: DATA DEL GIORNO
041700951108     C                   Z-ADD     WDATE         PA3DFI
041800020813     C                   move      VIDpog        PA3fgs
041900940315     C*
042000130520     C                   MOVEL     fnlst3ds      KPJBU
042100130520     C                   CALL      'FNLST3R'
042200940315     C                   PARM                    KPJBA
042300130520     C                   MOVEL     KPJBU         fnlst3ds
042400940315     C                   MOVEL     PA3NFV        VIDNFV
042500940315     C                   GOTO      ENDCT1
042600940315     C                   ENDIF
042700940315     C*
042800940315     C                   TESTN                   VIDNFV               18
042900940914     C  N18              MOVEL     MSG(1)        VIDMSG
043000940914     C  N18              SETON                                        409028
043100940315     C  N18              GOTO      ENDCT1
043200940315     C*
043300931130     C* CONTROLLI SE ESISTE NUMERO DI FOGLIO VIAGGIO
043400940315     C                   MOVEL     VIDNFV        COMNFV            5 0
043600020807     C     K02FGV01      chain     FNFGV01L                           40
043700931130     C  N40FGVATB        IFNE      *BLANKS
043800931130     C                   SETON                                        40
043900931130     C                   END
044000940914     C   40              MOVEL     MSG(1)        VIDMSG
044100940914     C   40              SETON                                        2890
044200931130     C   90              GOTO      ENDCT1
044300931130     C*
044400931130     C* FOGLIO VIAGGIO GIA' CHIUSO
044500931130     C     FGVFCF        IFNE      *BLANKS
044600940914     C                   MOVEL     MSG(2)        VIDMSG
044700940914     C                   SETON                                        409028
044800931130     C                   GOTO      ENDCT1
044900931130     C                   END
045000960201     C* NON SI POSSONO UTILIZZARE TIPI TRAINO INOLTRO PER BORDERIZZARE
045100960628     C*  NEMMENO IL TIPO TRAINO DI DEFLUENZA
045200960628     C     FGVTTR        IFNE      'I'
045300960628     C                   MOVEL     'TV'          COD
045400960628     C                   MOVEL(P)  FGVTTR        KEY
045500960628     C     KTAB          CHAIN     TABEL                              31
045600960628     C     *IN31         IFEQ      *OFF
045700960628     C                   MOVEL     TBLUNI        DSTV
045800960628     C                   ELSE
045900960628     C                   CLEAR                   DSTV
046000960628     C                   ENDIF
046100960628     C*
046200960628     C                   ENDIF
046300960628     C*
046400960201     C     FGVTTR        IFEQ      'I'
046500960628     C     §TVDEF        OREQ      'S'
046600960201     C                   MOVEL     MSG(6)        VIDMSG
046700960201     C                   SETON                                        409028
046800960201     C                   GOTO      ENDCT1
046900960201     C                   END
048800931130     C*
048900931130     C     ENDCT1        ENDSR
065201130703      /free
065202130703       //-------------------------------------------------------------*
065203130703       //Reperimento Dati del job (Utente/Operativi)                  *
065204130703       //-------------------------------------------------------------*
065205130703       Begsr DatiJob;
065206130703
065207130703          in(e) §azute;
065208130703          if not %error;
065209130703           in(e) §datiute;
065210130703          endif;
065211130703          if %error or rsut = *blanks;
065212130703           tibs34r(tibs34ds);
065213130703           in §azute;
065214130703           in §datiute;
065215130703          endif;
065216130703
065217130703         endsr;
065218130703
065219130703      /end-free
065300940913     C**************************************************************************
065400000612** MESSAGGI DI ERRORE
065500940914Numero di foglio viaggio inesistente o annullato                               1
065600940914Foglio di viaggio gia' chiuso                                                  2
065700130715                                                                               3
065800130715                                                                               4
065900130715                                                                               5
066000130715FV INOLTRO o DEFLUENZA non ammessi                                             6
