000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200940727      * FNLS04R *----------------------------------------------------*
000300940727      *           INTERROGAZIONE BOLLE PARTENZE
000400000830      *--------------------------------------------------------------*
000500940809      * N.B.:  SE SI VUOLE CHIAMARE IL PGM PER SELEZIONARE UNA BOLLA,
000600940809      *        AVENDOLA INDIETRO NELLA KPJBA, OCCORRE METTERE
000700940809      *        PA3FLG='1' QUANDO SI FA LA CHIAMATA
000800941010      *        PA3FLG="A" VENGO DA  PGM DI INTERROGAZ BOLLE ARRIVI
000900941010      *        PA3FLG="M" SONO IN MANUTENZIONE BOLLE PARTENZE
001000940809      *--------------------------------------------------------------*
001100030612     FFNLS04D   CF   E             WORKSTN INFDS(CA02DS) SFILE(LS04S02:NRR2)
001200040220      *.......... B O L L E   P A R T E N Z E
001300940928     FFNBLP01L  IF   E           K DISK
001400041028     FFNarb01L  IF   E           K DISK
001500030612     FFNBLP31L  IF   E           K DISK    RENAME(FNBLP000:FNBLP031)
001600030612     FFNBLP26L  IF   E           K DISK    RENAME(FNBLP000:FNBLP026)
001700020614     ffnblp76l  if   e           k disk    rename(fnblp000:fnblp076)
001800101126     FFNBLP32L  IF   E           K DISK    RENAME(FNBLP000:FNBLP032)
001900101126     FFNBLP33L  IF   E           K DISK    RENAME(FNBLP000:FNBLP033)
002000030612     FFNBLP43L  IF   E           K DISK    RENAME(FNBLP000:FNBLP043)
002100030612     FFNBLP44L  IF   E           K DISK    RENAME(FNBLP000:FNBLP044)
002200040220      *.......... S E G N A C O L L I
002300040220     FFNBLT01L  IF   E           K DISK
002400040220     FFNBLT27L  IF   E           K DISK    RENAME(FNBLT000:FNBLT027)
002500051013     FFiarS02L  IF   E           K DISK
002600040220      *.......... E S T E N S I O N E   D E S C R I Z I O N I
002700060217     FFiar403L  IF   E           K DISK
002800060217     FFiar404L  IF   E           K DISK    RENAME(Fiar4000:Fiar404)
002900040220      *.......... D A N N I
003000040810     F**""*FNDCT02L  IF   E           K DISK
003100040220      *.......... B O L L E   I N   G I A C E N Z A
003200050405     Ftigcp01L  IF   E           K DISK
003300040220      *.......... O. R. M.
003400010219     FFNORM01L  IF   E           K DISK
003500040220      *.......... A N A G R A F I C O   P . D . C .
003600040220     FCNACO00F  IF   E           K DISK
003700040220      *.......... O R G A N I G R A M M A
003800040220     FAZORG01L  IF   E           K DISK
003900040220      *.......... V E C C H I E  T A B E L L E
004000040220     FTABEL00F  IF   E           K DISK
004100040220      *.......... N U O V E   T A B E L L E
004200030527     FTNTBE01L  IF   E           K DISK
004300040220      *
004400040220      *.......... S E G N A C O L L I   I N   S T A M P A
004500040220     FFNLV22P   O    F  132        PRINTER USROPN
004600040220      *--------------------------------------------------------------*
004700940727     D C1              S              1    DIM(15)
004800920206     D SEG             S              1    DIM(15)
004900111020     D**!!!CLI             S              7  0 DIM(300)
005000951010     D* SCHIERE PER CARICAMENTO L1
005100951010     D L1              S              3  0 DIM(30)
005200941011     D*
005300161202     D MSG             S             78    DIM(32) CTDATA PERRCD(1)
005400991206     D CM3             S              1    DIM(122) CTDATA PERRCD(61)
005500030612      * le schiere per OVRPRTF dei moduli LASER sono + lunghe
005600030612     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
005700030612     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
005800030612      *
005900991222     D CNM             S              1    DIM(11) CTDATA PERRCD(11)
006000950206     D REC             S              2    DIM(50)
006100000911      *  SCHIERA PER COMPORRE BARCODE SU LETTERA VETTURA DPD IMPORT
006200060609     D K11             S              1    DIM(11)
006300060202     d* p.o. abilitati
006400060202     D $pog            s              3    dim(250)
006500060202     D $DIG            s              1    dim(20)
006600060202     D $ARG            s              3    dim(50)
006700020306
006800060202     D savkpjbu        S                   LIKE(kpjbu)
006900020306     d lnantw          s                   like(§ogntw)
007000020306     d lnpntw          s                   like(§ogntw)
007100041028     d wcca            s                   like(blpcca)
007200041028     d wdcm            s                   like(blpdcm)
007300041028     d wdce            s                   like(blpdce)
007400041028     d wnrc            s                   like(blpnrc)
007500041028     d wdcp            s                   like(blpdcp)
007600020614     d klp1            s                   like(v1clp1)
007700060202     d wabi            s                   like(UteAut)
007800020614     d ww              s              2  0
007900030527     D*
008000030527     D* VARIABILI DI WRK X CONSIDERAZIONI SU BOLLE PT
008100030527     D wrkoggiiso      s               d
008200020306
008300941011      **
008400940729      * RICHIAMO A INTERROGAZIONE BOLLA SINGOLA     - FNLS05R -
008500941214     D PARAM0          DS
008600941214     D  PA0AAS                 1      4  0
008700941214     D  PA0LNP                 5      7  0
008800941214     D  PA0NRS                 8      9  0
008900941214     D  PA0NSP                10     16  0
009000941214     D  PA0F03                17     17
009100941214     D  PA0FLG                18     18
009200941214     D  PA0FA9                19     19
009300941214     D  PA0FB5                20     20
009400941214     D  PA0F75                21     21
009500941214     D  PA0S98                22     22
009600941214     D  PA0RSU                23     42
009700941214     D  PA0CML                43     43
009800941214     D  PA0FEL                44     46
009900941214      * PA0CLI indica se il pgm e' richiamato da un cliente (='S')
010000111020      * OBSOLETO --> non ci sono più clienti con il DDM <-- 20/10/2011
010100111020     D**!!!PA0CLI                47     47
010200941214      * PA0GIA indica se il pgm e' richiamato dalle giacenze
010300941214     D  PA0GIA                48     48
010400941011      **
010500940729      * RICHIAMO PGM DI INTERROGAZIONE BOLLE ARRIVI   - FNLR36R -
010600940729      * RICHIAMO PGM DI INTERROGAZIONE BOLLE TRANSITI - FNLR68R -
010700920203     D PARAM2          DS
010800940729     D  PA2FLG                31     31
010900940729     D  PA2BOL                62     62
011000940729      * PROGRAMMA RICHIAMATO: DISABILITO F7=INTERROGAZIONE BOLLE ARRIVI
011100920203     D PARAM3          DS
011200940809     D  PA3AAS                 1      4  0
011300940809     D  PA3LNP                 5      7  0
011400940809     D  PA3NRS                 8      9  0
011500940809     D  PA3NSP                10     16  0
011600940729     D  PA3FLG                31     31
011700940729      * PA3CLI indica se il pgm e' richiamato da un cliente (='S')
011800111020      * OBSOLETO --> non ci sono più clienti con DDM <-- 20/10/2011
011900111020     D**!!!PA3CLI               143    143
012000980603     D* PASSAGGIO DATI DA FILTRO PER CAMBIO AZIENDA - FNLV02R
012100980603     D  CALPGM               201    210
012200980603     D  PARAZI               211    211
012300980603     D  PARF19               212    212
012400980603     D  PARF3                213    213
012500941011      **
012600930701     D* DS PER CHIUSURA FILE PGM RICHIAMATI E CHIUSI CON RETRN
012700940729     D PARA75          DS
012800940729     D  P75FLG                31     31
012900941011      **
013000940729     D PARAB5          DS
013100941213     D  PB5AAS                14     17  0
013200941213     D  PB5LNP                18     20  0
013300941213     D  PB5NRS                21     22  0
013400941213     D  PB5NSP                23     29  0
013500940729     D  PB5FLG                31     31
013600941011      **
013700900516     D MMGG            DS
013800900516     D  MM                     1      2  0
013900900516     D  GG                     3      4  0
014000920206     D WLBDAT          DS
014100940727     D  G02DAT                 1      8  0
014200940727     D  G02INV                 9     16  0
014300940727     D  G02ERR                17     17
014400940727     D  G02TGI                18     22  0
014500901116     D                 DS
014600940728     D  V1CLNA                 1      3  0
014700940728     D  V1CLP1                 4      6  0
014800940729     D  V1CDSD                 7     14  0
014900940729     D  V1CDSA                15     22  0
015000940729     D  V1CRMN                23     37  0
015100160125     D**V1CRPP                38     52
015200160125     D  SEL1                   1     37
015300901116     D                 DS
015400940728     D  V1CLP2                 1      3  0
015500940728     D  V1CNRS                 4      5  0
015600940728     D  V1CNSP                 6     12  0
015700901116     D  SEL2                   1     12  0
015800940727     D                 DS
015900940728     D  V1CRSD                 1     15
016000940728     D  V1CRSM                16     30
016100940727     D  SEL3                   1     30
016200010216      *--
016300010216     D                 DS
016400010216     D  V1CPOE                 1      3  0
016500010216     D  V1CNSR                 4      5  0
016600010216     D  V1CNOR                 6     12  0
016700010216     D  V1CNRV                13     14  0
016800010216     D  SEL4                   1     14  0
016900010216      *--
017000900515     D                 DS
017100940729     D  BLPAAS                 1      4  0
017200940729     D  BLPMGS                 5      8  0
017300940729     D  BLPDSP                 1      8  0
017400910118     D                 DS
017500940728     D  VSCTSP                 1      1
017600940728     D  VSCGIA                 2      2
017700990118     D  VSCDAN                 3      3
017800990118     D  VSCPAR                 4      4
017900990118     D  VSCFLA                 1      4
018000961017     D                 DS
018100961017     D  V1CKLP                 1      3  0
018200961017     D  V1CKSC                 4      7  0
018300961017     D  V1CKSD                 1      7  0
018400991214     D**
018500160125     D                 DS
018600160125     D  v1cbarcode             1     15  0
018700160125     D  b_aas                  1      2  0
018800160125     D  b_lnp                  3      5  0
018900160125     D  b_nrs                  6      7  0
019000160125     D  b_nsp                  8     14  0
019100991214      * CALCOLO DEL CHCK DIGIT PER BARCODE DPD
019200991214     D                 DS
019300030612     D  §11                    1     11  0 DIM(11)
019400991214     D  DPDCHD                12     12  0
019500991214     D  DPDBRC                 1     12  0
019600900517     D CA02DS          DS
019700900517     D  PRIMA                378    379B 0
019800040220      *?definizione oggetto reclamo come spedizione?
019900040220     d ds_OggSped      ds                  inz
020000040220     d  VSClnp                             inz
020100040220     d  VSCnrs                             inz
020200040220     d  VSCnsp                             inz
020300040220     d  VSCaas                             inz
020400040220      *
020500040220     d Status         sds
020600040220     d  SDSprm           *parms
020700040220      *
020800900514     D KPJBA         E DS
020900040220     d FIDNA1ds      e ds
021000121031     d Fnlri1ds      e ds
021100040220      *
021200941222     D DSARBD        E DS
021300941222     D DSARBG        E DS
021400991012     D DARBT         E DS
021500941222     D DSARBK        E DS
021600060518     D*DSBL4F        E DS
021700060518     D DSBL4I        E DS
021800060202     d* Abilitazioni utente
021900060202     d Tibs02Ds      e ds
022000060202     d Azuteds       e ds                  extname(Azute00f)
022100060202     d dDatiute      e ds
022200060202     d dLat          e ds
022300060202     d dute01        e ds
022400060202     D TRUL31DS      E DS
022500060202     D TRUL31DS2     E DS
022600060202     D TIBS34DS      E DS
022700141030      // -?Reperimento Disposizioni di Consegna da WEB
022800141030     d FNLRY09DS2    e ds                  inz
022900030527     D*
023000940316      *
023100940316      * DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
023200970925     D DS99          E DS                  EXTNAME(TNSD99DS) INZ
023300941010      *
023400991020     D DBLP          E DS                  INZ
023500991020     D DTASV         E DS                  INZ
023600030612     D* DS PER TRUL90R - RICHIESTA STAMPANTI
023700030612     D TRUL90DS      E DS
023800941013     D* DS PER FNLR66R - CONTROLLO E VISUALIZZAZIONE CAUSALI VARIAZIONI
023900941013     D DSLR66        E DS                  EXTNAME(FNLR66DS)
024000941222     D* DS PER FNLR48R - MANUTENZIONE BOLLE
024100941222     D DSLR48        E DS                  EXTNAME(FNLR48DS)
024200951010     D*
024300951010     D* DS PER TRUL06R - CARICAMENTO £1
024400951010     D DSUL06        E DS                  EXTNAME(TRUL06DS)
024500030612     D  LIN                    1     90  0 DIM(30)
024600951010     D*
024700950206     D DS3A          E DS
024800111020     D**!!!DS3K          E DS
024900991214     D OG143         E DS
025000940909     D DS7O          E DS
025100040810
025200040810      * ds per il controllo della presenza di CA per la spedizione richiesta
025300040810     d FIDN12DS      e ds
025400040810     d  skKey                 26    305    dim(20)
025500040810     d  skAnn                306    325    dim(20)
025600040810     d  skDca                326    485  0 dim(20)
025700040810     d  skFca                486    545  0 dim(20)
025800040810     d  skDch                546    705  0 dim(20)
025900040810     d  skCch                706    745    dim(20)
026000040810     d*
026100040810     d dsKey           ds
026200040810     d  dsaac                         4  0
026300040810     d  dsfil                         3  0
026400040810     d  dsnca                         7  0
026500141030     d $Fine           s              1    inz(*off)
026600141030       // -?Reperimento Disposizioni di consegna da WEB
026700141030     d fnlry09r        pr                  extpgm('FNLRY09R')
026800141030     d  kpjba                              likeds(kpjba)
026900941010     D** COSTANTI
027000941010     D CINTER          C                   CONST('* INTERROGAZIONE BOL-
027100941010     D                                     LE PARTENZE *')
027200941010     D CMANUT          C                   CONST('** MANUTENZIONE BOLL-
027300941010     D                                     E PARTENZE **')
027400940316      *
027500940316      ****************************************************************
027600940316      *  RIEPILOGO INDICATORI
027700940316      ***************************************************************
027800940728      * 01    - IMMESSO CAMPO LINEA DI ARRIVO (V1CLNA)
027900940728      * 02    - IMMESSO CAMPO LINEA DI PARTENZA (V1CLP1)
028000111020      * 03    -
028100941028      * 04    -
028200121106      * 05    -
028300010219      * 06    - IMMESSO CAMPO NUMERO ORM (V1CNOR)
028400941028      * 07    -
028500941028      * 08    -
028600940728      * 11    - IMMESSO CAMPO MITTENTE (V1CRSM)
028700940728      * 12    - IMMESSO CAMPO RIFERIMENTO MITTENTE (V1CRMN)
028800951010      * 13    - IMMESSO CAMPO RIFERIMENTO PARTNER PARTENZA (V1CRPP)
028900940728      * 14    - IMMESSO CAMPO DESTINATARIO (V1CRSD)
029000101126      * 15    - IMMESSO CAMPO RIFERIMENTO MITTENTE ALFANUMERICO (V1CRMA)
029100940316      * 01-16 - INDICANO QUALI CAMPI HO SCELTO A VIDEO
029200940316      * 20    - DISABILITO CMD7 INTERROGAZIONE BOLLE ARRIVI
029300941010      * 21    -
029400940316      * 22/23 - STATO SPEDIZIONI SELEZIONATO
029500941028      * 24    -
029600941028      * 26    -
029700941010      * 27    - ALTA INTENSITA' PER SPED. PARZIALI O CON CONS. ANOMALA
029800941011      * 28    - EMETTO CAMPO V1CMSG DI VISUALIZZAZIONE MESSAGGI DI ERR
029900940316      * 30    - FINE LETTURA FILE
030000940316      * 31    - COMODO
030100940316      * 32    - BOLLA LETTA DA SCARTARE
030200951011      * 33    - DI COMODO
030300940316      * 34    - LETTURA SUBFILE PER RECORD SCELTI
030400961017      * 35    - OFF --> SFLDSP/SFLDSPCTL    ON --> SFLCLR
030500961017      * 36    - DI COMODO
030600941028      * 37    -
030700000831      * 40-58 - ERRORI
030800000831      * 59    - IMMESSO CAMPO A VIDEO: CODICE "CHI SONO"
030900000831      * 60    - ERRORE
031000940316      * 90    - INDICA LA PRESENZA DI UN ERRORE
031100941028      * 91    -
031200941011      * 94    - PROTEGGO E NON VISUALIZZO IL CAMPO DELLA SCELTA
031300941011      * 95    - POSIZIONAMENTO CURSORE    SUL CAMPO DELLA SCELTA
031400941010      * 96    - PGM RICHIAMATO CON POSSIBILITA' DI SELEZIONARE BOLLA
031500941010      * 97    - PROVENGO DA MANUTENZIONE BOLLE PARTENZE
031600941010      * 98    -
031700111020      * 99    -
031800940316      *****************************************************************
031900000000     C     *ENTRY        PLIST
032000000000     C                   PARM                    KPJBA
032100040220     c                   parm                    FIDNA1ds
032200040220      *
032300920203     C                   MOVEL     KPJBU         PARAM3
032400940809     C*
032500940809     C                   MOVEL     *ZERO         PA3AAS
032600940809     C                   MOVEL     *ZERO         PA3LNP
032700940809     C                   MOVEL     *ZERO         PA3NRS
032800940809     C                   MOVEL     *ZERO         PA3NSP
032900940809     C*
033000060202     C                   Z-ADD     1             CODUT             1 0
033100060202     c                   Movel     rsut          V1crsu
033200981123     C                   MOVEL     SIMFEL        PA0FEL
033300981123     C* CARICO TABELLE
033400950206     C                   EXSR      CARTAB
033500940322      *
033600940727      * Testo il flag che mi dice se sono richiamato da un cliente
033700111020     C**!!!              SETOFF                                       03
033800111020     C**!!!PA3CLI        IFEQ      'S'
033900940727      * Testo le abilitazioni del cliente
034000111020     C**!!!              EXSR      ABICLI
034100111020     C**!!!$3KINB        CABNE     'S'           FINE
034200111020     C**!!!              SETON                                        03
034300111020     C**!!!              ENDIF
034400930701     C*
034500941010     C* 20 OFF- PGM  RICHIAMATO --> NON ABILITO LE ALTRE INTERROGAZIONI
034600121106     C***                SETOFF                                       05
034700941010     C     PA3FLG        IFEQ      ' '
034800920224     C                   SETON                                        20
034900980603     C** TASTO FUNZIONALE F19 - CAMBIO AZIENDA SOLO SE NON RICHIAMATO
035000111020     C*!N03PARAZI        IFEQ      'S'
035100121106     C***  PARAZI        IFEQ      'S'
035200121106     C***                SETON                                        05
035300121106     C***                ENDIF
035400040715      ** se parametri più di uno spengo il 20 in quanto richiamato dai reclami
035500040715     c     SDSprm        ifgt      1
035600040715     C                   SETOff                                       20
035700040715     C                   ENDIF
035800040715     c
035900941010     C                   ENDIF
036000980603     C**
036100941010     C* SE E' ABILITATA LA RICEZIONE DEL NUM SPEDIZIONE CON '1'
036200941010     C* VISUALIZZO L'1  IN SFL
036300941010     C     PA3FLG        IFEQ      '1'
036400040715     c     SDSprm        orgt      1
036500941010     C                   SETON                                        96
036600941010     C                   ENDIF
036700941010     C*
036800941010     C* INTESTAZIONE PGM
036900941010     C                   MOVEL     CINTER        V1CTES
037000941010     C* MANUTENZIONE
037100941010     C     PA3FLG        IFEQ      'M'
037200941010     C                   MOVEL     CMANUT        V1CTES
037300941010     C                   SETON                                        97
037400950206     C* SE SONO IN MANUTENZIONE RICHIEDO LE STAMPANTI
037500030612     C                   CLEAR                   trul90DS
037600030612      *
037700030612     C                   MOVEL     'S'           d90RSE
037800030612     C                   MOVEL     'S'           d90RSB
037900030612     C                   CALL      'TRUL90R'
038000941011     C                   PARM                    KPJBA
038100030612     C                   PARM                    trul90DS
038200030612      *
038300030612      * F3 - FINE
038400030612     C     d90F3         IFEQ      '1'
038500941011     C                   GOTO      FINE
038600941011     C                   ENDIF
038700030612      *
038800030612      * OVRPRTF BOLLE modulo A4
038900030612     C                   MOVEA     D90PRB4       CMA4(30)
039000030612     C                   MOVEA     D90MDB4       CMA4(52)
039100030612     C*
039200080709     C                   Z-ADD     110           LUNG             15 5
039300080709     C                   CLEAR                   COMMAN          110
039400030612     C                   MOVEA     CMA4          COMMAN
039500030612     C                   CALL      'QCMDEXC'                            91
039600030612     C                   PARM                    COMMAN
039700030612     C                   PARM                    LUNG
039800030612      *
039900030612     C   91              GOTO      FINE
040000030612      *
040100030612      * OVRPRTF BOLLE modulo A5
040200030612     C                   MOVEA     D90PRB5       CMA5(30)
040300030612     C                   MOVEA     D90MDB5       CMA5(52)
040400030612     C*
040500030612     C                   Z-ADD     110           LUNG
040600030612     C                   CLEAR                   COMMAN
040700030612     C                   MOVEA     CMA5          COMMAN
040800030612     C                   CALL      'QCMDEXC'                            91
040900030612     C                   PARM                    COMMAN
041000030612     C                   PARM                    LUNG
041100030612      *
041200030612     C   91              GOTO      FINE
041300030612      *
041400030612      * OVRPRTF Segnaqcolli
041500030612     C                   MOVEA     d90PRE        CM3(28)
041600030612     C                   MOVEA     d90MDE        CM3(50)
041700941011     C*
041800941011     C                   CLEAR                   COMMAN
041900991206     C                   MOVEA     CM3           COMMAN
042000941011     C                   CALL      'QCMDEXC'                            91
042100941011     C                   PARM                    COMMAN
042200941011     C                   PARM                    LUNG
042300030612      *
042400950111     C  N91              OPEN      FNLV22P
042500941011     C   91              GOTO      FINE
042600941010     C                   ENDIF
042700900511     C*---------------------------------------------------------------*
042800930407     C*
042900900516     C* PULIZIA CAMPI CHE RIMANGONO INVARIATI IL SECONDO GIRO
043000940928     C                   MOVEL     '3'           V1CFTA
043100940728     C                   MOVEL     *ZEROS        V1CLNA
043200940728     C                   MOVEL     *ZEROS        V1CDSD
043300940728     C                   MOVEL     *ZEROS        V1CDSA
043400000831     C                   MOVE      *BLANKS       V1CNOT
043500940728     C                   Z-ADD     *ZERO         V1CRMN
043600951010     C                   MOVE      *BLANKS       V1CRPP
043700940728     C                   MOVE      *BLANKS       V1CRSD
043800940728     C                   MOVE      *BLANKS       V1CRSM
043900101124     c                   clear                   v1crma
044000940316      *
044100940727      * IMPOSTO LA LINEA DI PARTENZA
044200060202     c                   z-add     dutpou        v1clp1
044300160125     c                   if        sdsprm>1
044400160125     c                   z-add     999           v1clp1
044500160125     c                   clear                   v1dlp1
044600160125     c                   endif
044700060202     c     dutlpo        ifeq      '2'
044800060202     c     dutlpo        oreq      *blanks
044900060202     c                   z-add     dutpou        savmus            3 0
045000020429     c                   endif
045100941010     C                   MOVE      V1CLP1        NUM3              3 0
045200940727     C     NUM3          CHAIN     AZORG01L                           90
045300940728     C  N90              MOVEL     ORGDES        V1DLP1
045400940316      *
045500900509     C     INIZIO        TAG
045600900514     C                   EXSR      PUL01
045700940727     C*
045800940727     C* EMETTO IL PRIMO VIDEO DI RICHIESTA PARAMETRI
045900940727     C                   MOVEL     'S'           WRIEM
046000900509     C     FOR01         TAG
046100940727     C*
046200940727     C* RIEMETTO L'INTESTAZIONE SOLO SE NECESSARIO
046300940727     C     WRIEM         IFEQ      'S'
046400940727     C                   WRITE     LS04T01
046500940727     C                   MOVE      *BLANKS       WRIEM             1
046600940727     C                   ENDIF
046700940727     C*
046800940727     C                   EXFMT     LS04D01
046900940928     C                   SETOFF                                       212426
047000940928     C                   SETOFF                                       28
047100940729     C                   SETOFF                                       414244
047200940729     C                   SETOFF                                       454648
047300940729     C                   SETOFF                                       495051
047400941012     C                   SETOFF                                       525543
047500000831     C                   SETOFF                                       475383
047600060217     C                   SETOFF                                       545640
047700940729     C*
047800980603     C** F3 - FINE LAVORO
047900980603     C     *INKC         IFEQ      '1'
048000121106     C** 05              MOVEL     '1'           PARF3
048100980603     C                   GOTO      FINE
048200980603     C                   ENDIF
048300980603     C** F7  - INTERROGAZIONE BOLLE ARRIVI
048400920203     C   KG              DO
048500940729     C                   MOVEL     '4'           PA2FLG
048600920203     C                   MOVEL     PARAM2        KPJBU
048700940729     C                   CALL      'FNLR36R'
048800920203     C                   PARM                    KPJBA
048900940727     C                   MOVEL     'S'           WRIEM
049000920203     C                   GOTO      FOR01
049100920203     C                   END
049200980603     C** F14 - INTERROGAZIONE BOLLE TRANSITO
049300920423     C   KN              DO
049400940729     C                   MOVEL     'T'           PA2BOL
049500920423     C                   MOVEL     PARAM2        KPJBU
049600940729     C                   CALL      'FNLR68R'
049700920423     C                   PARM                    KPJBA
049800940727     C                   MOVEL     'S'           WRIEM
049900920423     C                   GOTO      FOR01
050000920423     C                   END
050100121031     C** F16 - Localizzazione filiale chi sono BRT
050200121031     C   KQ              DO
050300121031     c                   if        v1cnot<>*blanks
050400121031     C                   clear                   fnlri1ds
050500121031     c                   movel     v1cnot        ii1chi
050600121031     C                   MOVEL     fnlri1ds      KPJBU
050700121031     C                   CALL      'FNLRI1R'
050800121031     C                   PARM                    KPJBA
050900121031     C                   MOVEL     KPJBU         fnlri1ds
051000121031     c                   if        oi1err='1'
051100121031     c                   movel     oi1msg        v1cmsg
051200121031     C                   SETON                                        289083
051300121031     c                   endif
051400121105     c                   else
051500121105     c                   movel     msg(30)       v1cmsg
051600121105     C                   SETON                                        289083
051700121031     c                   endif
051800121105     C                   MOVEL     'S'           WRIEM
051900121105     C                   GOTO      FOR01
052000121031     C                   END
052100980603     C** F19 - CAMBIO AZIENDA
052200980603     C   KT              DO
052300980603     C                   MOVEL     '1'           PARF19
052400980603     C                   GOTO      FINE
052500980603     C                   ENDDO
052600900509     C* CONTROLLI FORMATO1
052700900509     C                   EXSR      CONTR
052800900509     C   90              GOTO      FOR01
052900900511     C* ELABORAZIONE
053000900515     C                   EXSR      ELAB
053100941010     C   90              GOTO      FOR01
053200901115     C*
053300901115     C* SE NUM SPE SECCO DOPO VISUALIZZAZIONE TORNO A VID. INIZIALE
053400901115     C     SEL2          IFNE      0
053500160125     c     v1cbarcode    orgt      0
053600901115     C                   GOTO      INIZIO
053700901115     C                   END
053800901115     C* SE NON CI SONO BOLLE SEGNALO ERRORE
053900940727     C     NRR2          IFEQ      0
054000940728     C                   MOVEL     MSG(1)        V1CMSG
054100940728     C                   SETON                                        28  90
054200900516     C                   GOTO      FOR01
054300900516     C                   END
054400940324     C* SE CARICATA SOLO UNA BOLLA LA VISUALIZZO  (SE RICHIESTA DA RIF)
054500940728     C     V1CRMN        IFGT      0
054600940727     C     NRR2          ANDEQ     1
054700101126     C     V1CRMa        orne      *blanks
054800101126     C     NRR2          ANDEQ     1
054900951010     C     V1CRPP        ORNE      *BLANKS
055000951010     C     NRR2          ANDEQ     1
055100000831     C     V1CNOT        ORNE      *BLANKS
055200000831     C     NRR2          ANDEQ     1
055300060217     C     V1Cnor        ORNE      0
055400060217     C     NRR2          ANDEQ     1
055500000919     C* SE RICHIAMATO PER SELEZIONE, DA ALTRO PROGRAMMA, ESCO
055600000919     C     PA3FLG        IFEQ      '1'
055700000919     C                   MOVEL     VSCAAS        PA3AAS
055800000919     C                   MOVEL     VSCLNP        PA3LNP
055900000919     C                   MOVEL     VSCNRS        PA3NRS
056000000919     C                   MOVEL     VSCNSP        PA3NSP
056100000919     C                   MOVEL     PARAM3        KPJBU
056200000919     C                   GOTO      FINE
056300000919     C                   ENDIF
056400000919     C  N97              EXSR      CALBOL
056500000919     C   97              EXSR      CALBO2
056600000831     C                   GOTO      INIZIO
056700940324     C                   END
056800940324     C*
056900900515     C** CARICAMENTO SUBFILE
057000900518     C     FORCT2        TAG
057100940727     C                   WRITE     LS04T01
057200940727     C                   WRITE     LS04Z02
057300900515     C** COMANDI SUBFILE
057400900515     C     FORCT         TAG
057500940727     C                   EXFMT     LS04C02
057600940729     C                   SETOFF                                       28
057700940729     C*
057800920203     C* CMD 3 - FINE
057900980603     C     *INKC         IFEQ      '1'
058000121106     C** 05              MOVEL     '1'           PARF3
058100980603     C                   GOTO      FINE
058200980603     C                   ENDIF
058300900515     C* CMD12 - RITORNO
058400940317     C   KL              GOTO      INIZIO
058500900515     C* ROLLUP
058600930701     C     *IN37         IFEQ      *ON
058700940728     C   30              SETON                                        28
058800940728     C   30              MOVEL     MSG(16)       V1CMSG
058900940728     C  N30              EXSR      CARSF
059000940727     C                   Z-ADD     COMOD         REC2
059100930701     C                   GOTO      FORCT
059200930701     C                   END
059300900515     C*
059400900516     C** CONTROLLO SE EFFETTUATA UNA SCELTA
059500940727     C                   Z-ADD     PRIMA         REC2
059600940727     C                   READC     LS04S02                                34
059700941011     C     *IN34         IFEQ      *ON
059800940728     C                   SETON                                        28
059900940728     C                   MOVEL     MSG(15)       V1CMSG
060000940728     C                   GOTO      FORCT
060100941011     C                   ENDIF
060200940728     C*
060300900516     C* VISUALIZZO BOLLA X BOLLA
060400941011    1C     *IN34         DOWEQ     *OFF
060500941214     C                   MOVE      *BLANKS       PA0F03
060600941011     C*
060700941011    2C     VSCSCE        IFEQ      '5'
060800940324     C                   EXSR      CALBOL
060900941011    2C                   END
061000940809     C*
061100941011    2C     VSCSCE        IFEQ      '1'
061200941010     C* SE RICHIAMATO PER SELEZIONE, DA ALTRO PROGRAMMA, ESCO
061300941011    3C     PA3FLG        IFEQ      '1'
061400940809     C                   MOVEL     VSCAAS        PA3AAS
061500940809     C                   MOVEL     VSCLNP        PA3LNP
061600940809     C                   MOVEL     VSCNRS        PA3NRS
061700940809     C                   MOVEL     VSCNSP        PA3NSP
061800940809     C                   MOVEL     PARAM3        KPJBU
061900940809     C                   GOTO      FINE
062000941011    3C                   ENDIF
062100040220if  3c                   if        SDSprm > 1
062200040220     c                   eval      IA1ins = 'S'
062300040220     c                   eval      IA1tor = 'S'
062400040220     c                   eval      IA1ogg = ds_OggSped
062500040220     c                   goto      FINE
062600040220e   3c                   endif
062700941010     C*
062800941010     C* SE MANUTENZIONE VADO IN MANUTENZIONE
062900941011    3C     *IN97         IFEQ      *ON
063000941010     C* NON PARTITA
063100941011    4C     VSCFT1        IFEQ      *BLANKS
063200111027     c* errore se richiesta la manutenzione per una bolla
063300111027     c     vsclnp        lookup    l1                                     31
063400111027    5c                   if        not *in31
063500111027     C                   MOVEL     msg(28)       V1CMSG
063600111027     C                   MOVEL     '1'           PA0F03
063700111027     C                   SETON                                        2895
063800111027   x5c                   else
063900941010     C                   MOVEL     VSCAAS        §LPAAS
064000941010     C                   MOVEL     VSCLNP        §LPLNP
064100941010     C                   MOVEL     VSCNRS        §LPNRS
064200941010     C                   MOVEL     VSCNSP        §LPNSP
064300941010     C                   MOVEL     'M'           §LPFPR
064400030612     C*                  MOVEL     PARAM4        KPJBU
064500941010     C                   CALL      'FNLS01R'
064600941010     C                   PARM                    KPJBA
064700991020     C                   PARM                    DBLP
064800991020     C                   PARM                    DTASV
064900030612     C                   PARM                    trul90ds
065000941115     C*
065100941010     C                   MOVEL     '1'           PA1S01
065200941011     C*
065300941011     C* SE FLAG PROVENIENZA ='A' SIGNIFICA CHE LA BOLLA E' STATA
065400941011     C*  ANNULLATA: PROTEGGO IL CAMPO DELLA SCELTA
065500060214     C     §LPFPR        IFEQ      '1'
065600941011     C                   SETON                                        94
065700941011     C                   ENDIF
065800161202     c* uscita automatica dopo 50 secondi
065900161202     C     §LPFPR        IFEQ      '6'
066000161202     C                   SETON                                        2895
066100161202     C                   MOVEL     msg(32)       V1CMSG
066200161202     C                   MOVEL     '1'           PA0F03
066300161202     C                   ENDIF
066400111027    5c                   endif
066500941010     C*
066600941011   X4C                   ELSE
066700941010     C*
066800941010     C* PARTITA
066900941222     C* VEDO SE E' MANUTENZIONABILE
067000060202     c***  dutlpo        ifeq      '2'
067100060202     c***  dutlpo        oreq      *blanks
067200060202     C***  VSCLNP        ifne      SAVMUS
067300060202     C***                MOVEL     D48MSG        V1CMSG
067400060202     C***                MOVEL     '1'           PA0F03
067500060202     C***                SETON                                        2895
067600060202    4C***                ENDIF
067700060202     c***                endif
067800060202     c
067900060202     c* controllo la skiera pog per lnp o p.o. utente se previsto
068000060203     c                   if        v1ccvb=*blanks
068100060203     c                   clear                   dslr66
068200060203     c                   endif
068300060203     c
068400060202     c                   movel     vsclnp        w003a
068500060202     c     w003a         lookup    $pog                                   31
068600060202     c                   if        not *in31 and d66pocl='S'
068700060202     c                   movel     vscksc        w003a
068800060202     c     w003a         lookup    $pog                                   31
068900060202     c                   endif
069000060202    5c                   if        not *in31
069100060202     C                   MOVEL     msg(28)       V1CMSG
069200060202     C                   MOVEL     '1'           PA0F03
069300060202     C                   SETON                                        2895
069400060202   x5C                   else
069500941222     C*
069600941222     C                   CLEAR                   DSLR48
069700941222     C                   MOVEL     VSCAAS        D48AAS
069800941222     C                   MOVEL     VSCLNP        D48LNP
069900941222     C                   MOVEL     VSCLNP        D48FGS
070000941222     C                   MOVEL     VSCNRS        D48NRS
070100941222     C                   MOVEL     VSCNSP        D48NSP
070200141030
070300941222     C                   MOVEL     VSCCBO        D48CBO
070400941222     C                   MOVEL     'P'           D48TBO
070500941222     C                   MOVEL     V1CCVB        D48CVB
070600950607     C                   MOVEL     'S'           D48F12
070700141030     c* Prima di richiamare lr48:
070800141030     c* routine per verifica dispo da esaminare
070900141030     c                   exsr      sr_wdwdc
071000141030
071100941222     C                   MOVEL     DSLR48        KPJBU
071200141030     c
071300941222     C                   CALL      'FNLR48R'
071400941010     C                   PARM                    KPJBA
071500941222     C                   PARM                    DSARBD
071600941222     C                   PARM                    DSARBK
071700991012     C                   PARM                    DARBT
071800941222     C                   PARM                    DSARBG
071900941222     C*
072000941222     C                   MOVEL     KPJBU         DSLR48
072100941222     C* ERRORE
072200060202    6C     D48ERR        IFNE      *BLANKS
072300941222     C                   MOVEL     D48MSG        V1CMSG
072400941222     C                   MOVEL     '1'           PA0F03
072500941222     C                   SETON                                        2895
072600060202    6C                   ENDIF
072700060202    5C                   ENDIF
072800950114     C* SE NON C'E' STATO ERRORE RICHANO LA BOLLA PER PRENDERE IL MITT
072900950114     C* E IL DESTINATARIO CHE POSSON ESSERE MODIFICATI
073000950114     C     KBLP3         CHAIN     FNBLP01L                           31
073100950114     C  N31              EXSR      RIESFL
073200941010     C*
073300941011    4C                   ENDIF
073400941011    3C                   ENDIF
073500941011    2C                   ENDIF
073600940809     C*
073700940728     C                   MOVEL     *BLANKS       VSCSCE
073800930607     C*
073900940728     C                   MOVEL     VSCDCM        ALF4              4
074000941011    2C     ALF4          IFNE      *BLANKS
074100930614     C                   TESTN                   ALF4                 33
074200930607     C  N33              SETON                                        27
074300941011    2C                   END
074400941010     C*
074500941011    2C     VSCFT1        IFEQ      '9'
074600941010     C                   SETON                                        29
074700941010     C                   ELSE
074800941010     C                   SETOFF                                       29
074900941011    2C                   ENDIF
075000930607     C*
075100940727     C                   UPDATE    LS04S02
075200941011     C                   SETOFF                                       279594
075300930607     C*
075400941109     C* SE USCITO DA BOLLA CON TASTO F3 TORNO AL SUBFILE
075500941214    2C     PA0F03        IFEQ      '1'
075600900530     C                   SETON                                        34
075700900530     C                   ELSE
075800940727     C                   READC     LS04S02                                34
075900941011    2C                   ENDIF
076000941011    1C                   ENDDO
076100941011     C*
076200900518     C                   GOTO      FORCT2
076300941011     C*
076400000000     C     FINE          TAG
076500930506     C*
076600930701     C* CALL PGM PER CHIUSURA FILES
076700941214     C     PA0S98        IFEQ      '1'
076800930506     C                   MOVEL     'C'           D98TLA
076900970925     C                   CALL      'TNSD99R'
077000970925     C                   PARM                    DS99
077100930506     C                   END
077200930701     C*
077300941214     C     PA0F75        IFEQ      '1'
077400940729     C                   MOVEL     'C'           P75FLG
077500940729     C                   MOVEL     PARA75        KPJBU
077600940729     C                   CALL      'FNLR75R'
077700930701     C                   PARM                    KPJBA
077800930701     C                   END
077900930701     C*
078000941214     C     PA0FB5        IFEQ      '1'
078100941213     C                   CLEAR                   PARAB5
078200940729     C                   MOVEL     'C'           PB5FLG
078300940729     C                   MOVEL     PARAB5        KPJBU
078400940729     C                   CALL      'FNLRB5R'
078500930701     C                   PARM                    KPJBA
078600930701     C                   END
078700941010     C*
078800941010     C     PA1S01        IFEQ      '1'
078900941010     C                   MOVEL     'C'           §LPFPR
079000941010     C                   CALL      'FNLS01R'
079100941010     C                   PARM                    KPJBA
079200991020     C                   PARM                    DBLP
079300991020     C                   PARM                    DTASV
079400030612     C                   PARM                    trul90ds
079500941010     C                   END
079600941013     C*
079700941013     C     *IN97         IFEQ      *ON
079800941013     C                   MOVEL     'C'           D66TLA
079900941014     C                   MOVEL     DSLR66        KPJBU
080000941013     C                   CALL      'FNLR66R'
080100941013     C                   PARM                    KPJBA
080200941222     C*
080300941222     C                   ENDIF
080400141030
080500141030     c                   eval      ilry09tla= 'C'
080600141030     c                   eval      kpjbu=fnlry09ds2
080700141030     c                   call      'FNLRY09R'
080800141030     c                   parm                    kpjba
080900980603     C**
081000980603     C                   MOVEL     PARAM3        KPJBU
081100930506     C*
081200040810     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
081300040810     c                   clear                   fidn12ds
081400040810     c                   eval      i12tla = 'C'
081500040810     c                   call      'FIDN12R'
081600040810     c                   parm                    fidn12ds
081700141030
081800040810     c*
081900000000     C                   SETON                                        LR
082000940317      **************************************************************************
082100940317      *    PULIZIA CAMPI FORMATO 1
082200940317      **************************************************************************
082300900514     C     PUL01         BEGSR
082400940317      *
082500940728     C                   MOVEL     *ZEROS        V1CLP2
082600940728     C                   MOVEL     *ZEROS        V1CNRS
082700940728     C                   MOVEL     *ZEROS        V1CNSP
082800940729     C                   Z-ADD     UYEAR         V1CAAS
082900940729     C     UYEAR         IFLT      60
083000940729     C                   MOVEL     20            V1CAAS
083100940729     C                   ELSE
083200940729     C                   MOVEL     19            V1CAAS
083300940729     C                   ENDIF
083400160125     c                   clear                   v1cbarcode
083500940728     C                   MOVEL     *BLANKS       V1CTSP
083600040216     C                   clear                   V1Ccbo
083700940728     C                   MOVEL     *ZEROS        V1CPSD
083800940728     C                   MOVEL     99            V1CPSA
083900940728     C                   MOVEL     *ZEROS        V1CSCA
084000940923     C                   MOVEL     *ZERO         V1CFLS
084100950216     C                   MOVEL     *BLANKS       V1DFLS
084200940728     C                   MOVEL     *ZEROS        V1CKSD
084300940728     C                   MOVE      *BLANKS       V1CRAG
084400940728     C                   MOVEL     *ZEROS        V1CSCD
084500940728     C                   MOVE      *BLANKS       V1CSSP
084600940729     C                   MOVE      *BLANKS       V1CTER
084700961118     C                   CLEAR                   V1CFBR
084800010216      *--
084900010216     C                   CLEAR                   V1CPOE
085000010216     C                   CLEAR                   V1CNSR
085100010216     C                   CLEAR                   V1CNOR
085200010216     C                   CLEAR                   V1CNRV
085300010216      *--
085400930630     C                   SETON                                        35
085500940727     C                   WRITE     LS04C02
085600930630     C                   SETOFF                                       35
085700940317      *
085800900514     C                   ENDSR
085900940317      **************************************************************************
086000940317      *    CONTROLLI FORMATO1
086100940317      **************************************************************************
086200900509     C     CONTR         BEGSR
086300940316      *
086400900515     C                   SETOFF                                       900102
086500040216     ***C                   SETOFF                                       040708
086600101126     C                   SETOFF                                       060708
086700941012     C                   SETOFF                                       121114
086800000831     C                   SETOFF                                       131659
086900160125     C                   SETOFF                                       1560
087000940728     C                   MOVEL     *BLANKS       VCDLNA
087100940728     C                   MOVEL     *BLANKS       VCDLNP
087200020306     c                   clear                   lnpntw
087300020306     c                   clear                   lnantw
087400060609     C                   clear                   KRPP             14
087500950103     C*
087600950103     C* SE L'ANNO E' IMMESSO DI DUE CIFRE, METTO LE ALTRE DUE
087700950103     C     V1CAAS        IFLT      100
087800950103     C     V1CAAS        IFGT      60
087900950103     C                   ADD       1900          V1CAAS
088000950103     C                   ELSE
088100950103     C                   ADD       2000          V1CAAS
088200950103     C                   ENDIF
088300950103     C                   ENDIF
088400950121     C*
088500950121      * SE SONO IN MANUTENZIONE CONTROLLO ANCHE LA CAUSALE VARIAZIONE
088600950121     C                   CLEAR                   DSLR66
088700950121    0C     V1CCVB        IFNE      *BLANKS
088800950121     C     '?'           SCAN      V1CCVB                                 90
088900950121     C* RICERCA
089000950121    1C     *IN90         IFEQ      *ON
089100950121     C                   CLEAR                   DSLR66
089200950121     C                   MOVEL     'V'           D66GES
089300950121     C                   MOVEL     'P'           D66TBO
089400950121     C*
089500950121     C* SE IMMESSO NUMERO SPEDIZIONE LO VISUALIZZO
089600950121    2C     V1CNSP        IFGT      0
089700950121     C                   MOVEL     V1CNSP        D66NSP
089800950121     C                   MOVEL     V1CAAS        D66AAS
089900950121     C                   MOVEL     V1CLP2        D66LNP
090000950121     C                   MOVEL     V1CNRS        D66NRS
090100950121    2C                   ENDIF
090200950121     C*
090300950121     C                   MOVEL     DSLR66        KPJBU
090400950121     C                   CALL      'FNLR66R'
090500950121     C                   PARM                    KPJBA
090600950121     C                   MOVEL     KPJBU         DSLR66
090700950121     C                   MOVEL     D66CVB        V1CCVB
090800950121     C                   MOVEL     D66DES        V1DCVB
090900950121     C                   MOVEL     'S'           WRIEM
091000950121   X1C                   ELSE
091100950121     C* CONTROLLO
091200950121     C                   CLEAR                   DSLR66
091300950121     C                   MOVEL     'C'           D66GES
091400950121     C                   MOVEL     'P'           D66TBO
091500950121     C                   MOVEL     V1CCVB        D66CVB
091600950121    2C     V1CNSP        IFGT      0
091700950121     C                   MOVEL     V1CNSP        D66NSP
091800950121     C                   MOVEL     V1CAAS        D66AAS
091900950121     C                   MOVEL     V1CLP2        D66LNP
092000950121     C                   MOVEL     V1CNRS        D66NRS
092100950121    2C                   ENDIF
092200950121     C*
092300950121     C                   MOVEL     DSLR66        KPJBU
092400950121     C                   CALL      'FNLR66R'
092500950121     C                   PARM                    KPJBA
092600950121     C                   MOVEL     KPJBU         DSLR66
092700950121     C                   MOVEL     D66CVB        V1CCVB
092800950121     C                   MOVEL     D66DES        V1DCVB
092900950121     C*
093000950121     C* ERRORE
093100950121    2C     D66MSG        IFNE      *BLANKS
093200950121     C                   MOVEL     D66MSG        V1CMSG
093300950121     C                   SETON                                        284390
093400950121     C                   GOTO      ENDCTR
093500950121     C*
093600950121   X2C                   ELSE
093700950121     C* DECODIFICA
093800950121     C                   MOVEL     D66DES        V1DCVB
093900950121    2C                   ENDIF
094000950121     C*
094100950121     C* NON POSSO MANUTENZIONARE BOLLE IN STB
094200950121    1C                   ENDIF
094300950123    0C                   ENDIF
094400950103     C*
094500901115     C* SELEZIONO PER DATA SPEDIZIONE O PER NUMERO SPEDIZIONE
094600000831      *
094700000831      * .. SE NESSUNA SELEZIONE VIENE EFFETTUATA DO ERRORE
094800000831      *
094900000831     C     SEL1          IFEQ      *ZERO
095000160125     c     v1crpp        andeq     *blanks
095100000831     C     SEL2          ANDEQ     0
095200000831     C     SEL3          ANDEQ     *BLANKS
095300000831     C     V1CNOT        ANDEQ     *BLANKS
095400010216     C     SEL4          ANDEQ     0
095500101124     C     V1Crma        ANDEQ     *BLANKS
095600160125     c     v1cbarcode    andeq     *zeros
095700940728     C                   SETON                                        284190
095800940728     C                   MOVEL     MSG(6)        V1CMSG
095900901115     C                   GOTO      ENDCTR
096000901115     C                   END
096100020614
096200020614      * se è manutenzione non si può richiedere la lnp 999
096300020614     c                   if        *in97
096400020614      * non si può richiedere la lnp 999
096500020614     c                   if        v1clp1 = 999
096600020614     c                   eval      *in42 = *on
096700020614     c                   eval      *in28 = *on
096800020614     c                   eval      *in90 = *on
096900020614     c                   eval      v1cmsg = msg(2)
097000020614     c                   goto      endctr
097100020614     c                   endif
097200020614      * la linea di partenza deve essere del mio terminal di partenza
097300060202     c**                 if        v1clp1 > *zeros
097400060202     c**                 movel     v1clp1        w003a
097500060202     c**   w003a         lookup    $pog                                   31
097600060202     c***  v1clp1        lookup    l1                                     31
097700060202     c**                 if        not *in31
097800060202     c**                 eval      *in42 = *on
097900060202     c**                 eval      *in28 = *on
098000060202     c**                 eval      *in90 = *on
098100060202     c**                 eval      v1cmsg = msg(27)
098200060202     c**                 goto      endctr
098300060202     c**                 endif
098400060202     c**                 endif
098500020614     c                   endif
098600930630     C***
098700930630     C* D A T A   S P E D I Z I O N E
098800930630     C***
098900000831     C     SEL2          IFEQ      0
099000010216     C     SEL4          ANDEQ     0
099100160217     C     v1cbarcode    ANDEQ     0
099200940321      *
099300930630     C* O LA LNP O LA LNA DEVONO ESSERCI
099400940728     C     V1CLNA        IFEQ      0
099500940728     C     V1CLP1        ANDEQ     0
099600000831     C     V1CNOT        ANDEQ     *BLANKS
099700000831     C     V1CRMN        ANDEQ     *ZERO
099800951010     C     V1CRPP        ANDEQ     *BLANKS
099900940728     C     V1CRSD        ANDEQ     *BLANKS
100000940728     C     V1CRSM        ANDEQ     *BLANKS
100100101124     c     v1crma        andeq     *blanks
100200160125     c     v1cbarcode    andeq     *zeros
100300940728     C                   SETON                                        284190
100400940728     C                   MOVEL     MSG(7)        V1CMSG
100500901115     C                   GOTO      ENDCTR
100600901115     C                   END
100700940728     C*
100800901115     C* DATA SPEDIZIONE DAL OBBLIGATORIA
100900940727     C* SOLO IN ALCUNI CASI
101000940728     C     V1CDSD        IFEQ      0
101100000831     C     V1CNOT        ANDEQ     *BLANKS
101200940728     C     V1CRMN        ANDEQ     *ZERO
101300951010     C     V1CRPP        ANDEQ     *BLANKS
101400940728     C     V1CRSD        ANDEQ     *BLANKS
101500940728     C     V1CRSM        ANDEQ     *BLANKS
101600101126     c     v1crma        andeq     *blanks
101700160125     c     v1cbarcode    andeq     *zeros
101800940728     C                   MOVEL     MSG(8)        V1CMSG
101900940728     C                   SETON                                        285190
102000901115     C                   GOTO      ENDCTR
102100901115     C                   END
102200020614
102300020614      * se inserita SOLO la linea di partenza questa non può essere 0 o 999
102400020614     c                   if        v1clna = *zeros and v1cnot = *blanks and
102500020614     c                             v1crmn = *zeros and v1crpp = *blanks and
102600101124     c                             sel3 = *blanks and  v1crma = *blanks and
102700020614     c                             (v1clp1 = *zeros or v1clp1 = 999)
102800160217     c                             and v1cbarcode = *zeros
102900020614     c                   eval      *in42 = *on
103000020614     c                   eval      *in28 = *on
103100020614     c                   eval      *in90 = *on
103200020614     c                   eval      v1cmsg = msg(2)
103300020614     c                   goto      endctr
103400020614     c                   endif
103500020614
103600900514     C* LINEA ARRIVO
103700941027     C                   MOVE      *BLANKS       V1DLNA
103800940728     C     V1CLNA        IFNE      0
103900940728     C                   Z-ADD     V1CLNA        NUM3
104000940923     C     NUM3          CHAIN     AZORG01L                           41
104100940728     C   41              DO
104200940728     C                   SETON                                        28  90
104300940728     C                   MOVEL     MSG(5)        V1CMSG
104400940728     C                   GOTO      ENDCTR
104500940728     C                   ENDDO
104600940728     C                   MOVEL     ORGDES        V1DLNA
104700940728     C                   MOVEL     ORGDES        VCDLNA
104800991222     C                   MOVEL     ORGDE3        OG143
104900020306     c                   eval      lnantw = §ogntw
105000900514     C                   SETON                                        01
105100901115     C                   END
105200901115     C* LINEA PARTENZA
105300940727     C* E' OBBLIGATORIA SE C'E' ALMENO UNA RAGIONE SOCIALE
105400941027     C                   MOVE      *BLANKS       V1DLP1
105500940728     C     V1CLP1        IFNE      0
105600940728     C     V1CRSD        ORNE      *BLANKS
105700940728     C     V1CRSM        ORNE      *BLANKS
105800020614      * se lnp richiesta è 999  o 000 non devo fare la chain
105900020614     c                   if        v1clp1 <> 999 and v1clp1 <> 0
106000940728     C                   Z-ADD     V1CLP1        NUM3
106100940728     C     NUM3          CHAIN     AZORG01L                           42
106200940728     C   42              DO
106300940728     C                   MOVEL     MSG(2)        V1CMSG
106400940728     C                   SETON                                        28  90
106500940728     C                   GOTO      ENDCTR
106600940728     C                   ENDDO
106700940728     C                   MOVEL     ORGDES        V1DLP1
106800940728     C                   MOVEL     ORGDES        VCDLNP
106900991214     C                   MOVEL     ORGDE3        OG143
107000020306     c                   eval      lnpntw = §ogntw
107100020614     c                   endif
107200901115     C                   SETON                                        02
107300901115     C                   END
107400951012     C*
107500000831     C* SI PUO' INSERIRE O "CHI SONO" O RIF.MITTENTE O  RIF.PARTNER
107600000831     C     V1CNOT        IFNE      *BLANKS
107700000831     C     V1CRMN        IFNE      0
107800000831     C     V1CRPP        ORNE      *BLANKS
107900101124     c     v1crma        orne      *blanks
108000991214     C                   MOVEL     MSG(18)       V1CMSG
108100000831     C                   SETON                                        288390
108200951012     C                   GOTO      ENDCTR
108300951012     C                   ENDIF
108400000831     C                   ENDIF
108500000831     C     V1CRMN        IFNE      0
108600101124     C     V1CRPP        ifne      *BLANKS
108700101124     C     V1Crma        orne      *BLANKS
108800000831     C                   MOVEL     MSG(18)       V1CMSG
108900000831     C                   SETON                                        284790
109000000831     C                   GOTO      ENDCTR
109100000831     C                   ENDIF
109200101124     C                   ENDIF
109300101124     C     V1CRPP        ifne      *BLANKS
109400101124     C     V1Crma        andne     *BLANKS
109500101124     C                   MOVEL     MSG(18)       V1CMSG
109600101124     C                   SETON                                        284790
109700101124     C                   GOTO      ENDCTR
109800101124     C                   ENDIF
109900000831     C*
110000060609     C* SE LNP DPD IL NUMERO  PUO' AVERE :
110100060609     c*  lunghezza 1 - 11 - 12 - 14
110200991214     C     V1CRMN        IFGT      0
110300020306     c     lnpntw        andeq     'DPD'
110400060609     c                   movel     v1crmn        w015a            15
110500060609     c                   select
110600060609     c* ok se lungo 1
110700060609     c                   when      %subst(w015a:1:14)=*zeros
110800060609     c* ok se lungo 11
110900060609     c                   when      %subst(w015a:1:4)=*zeros    and
111000060609     c                             %subst(w015a:5:1)>*zeros
111100060609     c* ok se lungo 12
111200060609     c                   when      %subst(w015a:1:3)=*zeros    and
111300060609     c                             %subst(w015a:4:1)>*zeros
111400060609     c* ok se lungo 14
111500060609     c                   when      %subst(w015a:1:1)=*zeros    and
111600060609     c                             %subst(w015a:2:1)>*zeros
111700060609     c                   other
111800060609     C                   MOVEL     MSG(22)       V1CMSG
111900060609     C                   SETON                                        28  90
112000060609     C                   GOTO      ENDCTR
112100060609     C                   endsl
112200060609     c
112300060609     c*
112400060609     C**                 MOVEL     V1CRMN        W003A             3
112500060609     C**                 MOVEL     V1CRMN        W002A             2
112600060609     C**                 MOVEL     V1CRMN        W001A             1
112700060609     C**   W003A         IFNE      '000'
112800060609     C**   W001A         ifne      '0'
112900060609     C**   W002A         oreq      '00'
113000060609     C**                 MOVEL     MSG(22)       V1CMSG
113100060609     C**                 SETON                                        28  90
113200060609     C**                 GOTO      ENDCTR
113300060609     C**                 ENDIF
113400060609     C**                 ENDIF
113500991214     C                   ENDIF
113600951012     C*
113700901115     C* DATA SPEDIZIONE DAL
113800940727     C                   Z-ADD     *ZERO         COMDSD
113900940727     C                   Z-ADD     *ZERO         COMAAD
114000940727     C                   Z-ADD     *ZERO         COMMGD
114100940728     C     V1CDSD        IFNE      *ZERO
114200940728     C                   MOVE      V1CDSD        G02DAT
114300920206     C                   MOVEL     *BLANK        G02ERR
114400940727     C                   CALL      'XSRDA8'
114500920206     C                   PARM                    WLBDAT
114600920206     C     G02ERR        IFEQ      '1'
114700950327     C                   SETON                                        285190
114800950327     C                   MOVEL     MSG(8)        V1CMSG
114900900515     C                   GOTO      ENDCTR
115000900515     C                   ELSE
115100940729     C                   MOVEL     G02DAT        V1CDSD
115200940727     C                   MOVE      G02INV        COMDSD            8 0
115300940727     C                   MOVEL     G02INV        COMAAD            4 0
115400920206     C                   MOVE      G02INV        COMMGD            4 0
115500900515     C                   SETON                                        16
115600900515     C                   END
115700940727     C                   ENDIF
115800900515     C* DATA SPEDIZIONE AL
115900940727     C                   Z-ADD     *ZERO         COMDSA
116000940728     C     V1CDSA        IFNE      0
116100940728     C                   MOVE      V1CDSA        G02DAT
116200920206     C                   MOVEL     *BLANK        G02ERR
116300940727     C                   CALL      'XSRDA8'
116400920206     C                   PARM                    WLBDAT
116500920206     C     G02ERR        IFEQ      '1'
116600940728     C                   SETON                                        285290
116700940728     C                   MOVEL     MSG(8)        V1CMSG
116800900515     C                   GOTO      ENDCTR
116900900515     C                   ELSE
117000940729     C                   MOVEL     G02DAT        V1CDSA
117100940727     C                   MOVE      G02INV        COMDSA            8 0
117200900515     C                   END
117300940728     c*
117400900515     C* DATA SPEDIZIONE AL MAGGIORE DATA SPEDIZIONE AL
117500900515     C     COMDSA        IFLT      COMDSD
117600940728     C                   SETON                                        285190
117700940728     C                   MOVEL     MSG(9)        V1CMSG
117800900515     C                   GOTO      ENDCTR
117900900515     C                   END
118000900515     C                   END
118100940321      *
118200940727      * SE DATA SPEDIZIONE AL E' VUOTA, LA IMPOSTO COMA LA DATA DAL
118300940727     C     COMDSA        IFEQ      *ZERO
118400940727     C     COMDSD        ANDNE     *ZERO
118500940727     C                   Z-ADD     COMDSD        COMDSA
118600940727     C                   ENDIF
118700901115     C***
118800940928     C* TIPO SERVIZIO
118900940729     C     V1CTSP        IFNE      *BLANKS
119000040216     ***C                   SETON                                        04
119100940728     C     V1CTSP        IFEQ      '?'
119200940728     C                   CALL      'X§TABER'
119300040217     C                   PARM      CodUt         §KUT
119400040217     C                   PARM      '5E'          §COD
119500040217     C                   PARM      *blanks       §KEY
119600040217     C                   PARM      *blanks       §DES             30
119700940729     C                   MOVEL     §KEY          V1CTSP
119800940728     C                   SETON                                        90
119900940728     C                   MOVEL     'S'           WRIEM
120000940728     C                   ENDIF
120100940728     C*
120200940729     C                   MOVEL     '5E'          KCOD
120300940728     C                   MOVEL     *BLANKS       KKEY
120400940728     C                   MOVEL     V1CTSP        KKEY
120500940729     C     KTAB2         CHAIN     TABEL00F                           46
120600961118     C     *IN46         IFEQ      *ON
120700961118     C     TBLFLG        ORNE      ' '
120800940728     C                   MOVEL     MSG(14)       V1CMSG
120900941014     C                   SETON                                        284690
121000940728     C                   GOTO      ENDCTR
121100940728     C                   ENDIF
121200940729     C                   ENDIF
121300040216      *
121400040216      * Tipo Bolla
121500040216if  1c                   if        V1Ccbo <> *blanks
121600040216     c                   eval      *in31  =  *off
121700040216     c     '?'           scan      V1Ccbo                                 31
121800040216if  2c                   if        *in31 = *on
121900040216     c                   call      'X§TABER'
122000040217     c                   parm      CodUt         §KUT
122100040217     c                   parm      '3A'          §COD
122200040217     c                   parm      *blanks       §KEY
122300040217     c                   parm      *blanks       §DES
122400040216     c                   movel     §KEY          V1Ccbo
122500040216     c                   eval      *in90 = *on
122600040216     c                   movel     'S'           wRiem
122700040216e   2c                   endif
122800040216      *
122900040216     c                   movel     '3A'          KCOD
123000040216     c                   movel(p)  V1Ccbo        KKEY
123100040216     c     KTAB2         chain     TABEL
123200040216if  2c                   if        NOT %found(TABEL00F)
123300040216     c                             or  TBLflg <> *blanks
123400040216     c                   movel     Msg(4)        V1Cmsg
123500040216     c                   seton                                        284090
123600040216     c                   goto      ENDCTR
123700040216e   2c                   endif
123800040216e   1c                   endif
123900940928     C*
124000901115     C* NUMERO SERIE AL DEVE ESSERE > NUMERO SERIE DAL
124100901115     C*  SE IMMESSO
124200940728     C     V1CPSD        IFGT      V1CPSA
124300940728     C                   SETON                                        284890
124400940728     C                   MOVEL     MSG(10)       V1CMSG
124500901115     C                   GOTO      ENDCTR
124600901115     C                   END
124700901115     C* NUMERO SEGNACOLLO AL DEVE ESSERE > NUMERO SEGNACOLLO DAL
124800940728     C     V1CSCA        IFGT      0
124900940728     C     V1CSCD        ANDGT     V1CSCA
125000940728     C                   SETON                                        284990
125100940728     C                   MOVEL     MSG(11)       V1CMSG
125200901115     C                   GOTO      ENDCTR
125300901115     C                   END
125400901115     C* SE IMMESSO AL IMMETTERE ANCHE DAL
125500940728     C     V1CSCA        IFGT      0
125600940728     C     V1CSCD        ANDEQ     0
125700940728     C                   SETON                                        284990
125800940728     C                   MOVEL     MSG(12)       V1CMSG
125900901115     C                   GOTO      ENDCTR
126000901115     C                   END
126100930630     C*
126200940728     C     V1CSCA        COMP      0                                  08
126300940728     C     V1CSCD        COMP      0                                  07
126400940923     C*
126500940923     C* LA FILIALE SEGNACOLLO DEVE ESSERE VALIDA, SE IMMESSSA
126600950216     C                   MOVEL     *BLANKS       V1DFLS
126700940923     C     V1CFLS        IFNE      *ZERO
126800940923     C     V1CFLS        CHAIN     AZORG01L                           21
126900940923     C   21              SETON                                        28  90
127000940923     C   21              MOVEL     MSG(17)       V1CMSG
127100940923     C   21              GOTO      ENDCTR
127200950216     C                   MOVEL     ORGDES        V1DFLS
127300940923     C                   ENDIF
127400940316      *
127500961017     C****  CODICE CLIENTE  ****
127600961017     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE ESISTENTE
127700961017     C     V1CKLP        IFGT      0
127800961017     C                   Z-ADD     V1CKLP        NUM3
127900961017     C     NUM3          CHAIN     AZORG01L                           36
128000961017     C     *IN36         IFEQ      *ON
128100961017     C                   SETON                                        285390
128200961017     C                   MOVEL     MSG(2)        V1CMSG
128300961017     C                   GOTO      ENDCTR
128400961017     C                   ENDIF
128500961017     C* SE INSERITA SOLO LA LINEA DEL CODICE ---> ERRORE
128600961017     C     V1CKSC        IFEQ      0
128700961017     C     V1CRAG        ANDEQ     *BLANKS
128800961017     C                   SETON                                        285090
128900961017     C                   MOVEL     MSG(13)       V1CMSG
129000961017     C                   GOTO      ENDCTR
129100961017     C                   ENDIF
129200961017     C*
129300961017     C                   ELSE
129400961017     C     V1CKSC        IFGT      0
129500961017     C                   SETON                                        285390
129600961017     C                   MOVEL     MSG(2)        V1CMSG
129700961017     C                   GOTO      ENDCTR
129800961017     C                   ENDIF
129900961017     C                   ENDIF
130000961017     C*
130100940727      * IL CODICE CLIENTE DEVE ESSERE ESISTENTE
130200961017     C     V1CKSC        IFEQ      0
130300940728     C     V1CRAG        IFNE      *BLANKS
130400940727      *
130500000831      * PAXSTA = 9  ESCLUDO ANNULLATI
130600000831     C                   Z-ADD     9             PAXSTA
130700940727     C                   Z-ADD     0             ACOKSC
130800000831     C                   MOVEL     V1CRSU        PAXDUT           30
130900940727     C                   MOVEL     *BLANKS       DESCR            48
131000940728     C                   MOVEL     V1CRAG        DESCR
131100060202     C                   Z-ADD     dutKCI        ACOKCC
131200000831     C                   MOVEL     V1CKLP        PAXFLR
131300000831     C                   CLEAR                   PAXDIT
131400000831     C                   Z-ADD     1             PAXNUM
131500000831     C                   CALL      'XALFA3BR'
131600000831     C                   PARM                    PAXDUT
131700940727     C                   PARM                    CODUT
131800940727     C                   PARM                    DESCR
131900940727     C                   PARM                    ACOKCC
132000000831     C                   PARM                    PAXSTA            1 0
132100000831     C                   PARM                    PAXFLR           90
132200000831     C                   PARM                    PAXDIT            3
132300000831     C                   PARM                    PAXNUM            2 0
132400000831     C                   PARM                    PAXKCM           80
132500000831     C                   PARM                    PAXKSM          140
132600000831     C                   PARM                    PAXKDM           60
132700000831     C     PAXSTA        IFNE      -1
132800000831     C                   MOVEL     PAXKSM        ACOKSC
132900940728     C                   MOVEL     ACOKSC        V1CKSD
133000961017     C                   MOVEL     DESCR         V1CRAG
133100940727     C                   ELSE
133200940728     C                   CLEAR                   V1CRAG
133300940727     C                   ENDIF
133400940727      *
133500000830     C                   MOVE      'S'           WRIEM
133600940727     C                   SETON                                        90
133700940727     C                   GOTO      ENDCTR
133800940727     C                   ENDIF
133900940727      *
134000940727     C                   ELSE
134100940727      *
134200940727      * CONTROLLO VALIDITA' DEL CODICE CLIENTE IMMESSO
134300060202     C                   Z-ADD     dutKCI        ACOKCC
134400950504     C                   MOVEL     V1CKSD        ACOKSC
134500940727     C     CNACOK        CHAIN     CNACO00F                           50
134600940727     C  N50ACOFLG        COMP      *BLANKS                            5050
134700940728     C     *IN50         IFEQ      *OFF
134800940728     C                   MOVEL     ACORAG        V1CRAG
134900940728     C                   ELSE
135000940728     C                   SETON                                        28  90
135100940728     C                   MOVEL     MSG(13)       V1CMSG
135200940728     C                   GOTO      ENDCTR
135300940728     C                   ENDIF
135400940728     C*
135500940727     C                   ENDIF
135600940316      *
135700000831     C* CODICE "CHI SONO"
135800000831     C     V1CNOT        IFNE      *BLANKS
135900000831     C                   SETON                                        59
136000000831     C                   ENDIF
136100951010     C* RIFERIMENTO MITTENTE
136200940728     C     V1CRMN        COMP      0                                  12
136300951010     C* RIFERIMENTO PARTNER PARTENZA
136400951010     C     V1CRPP        IFNE      *BLANKS
136500951010     C                   SETON                                        13
136600951010     C                   ENDIF
136700101126     C* RIFERIMENTO mittente alfanumerico
136800101126     C     V1CRma        IFNE      *BLANKS
136900101126     C                   SETON                                        15
137000101126     C                   ENDIF
137100930406     C*
137200930406     C* STATO SPEDIZIONE
137300010718     C     V1CSSP        COMP      'N'                                    22
137400010718     C     V1CSSP        COMP      'C'                                    23
137500930406     C*
137600901115     C* SE IMMESSA RAGIONE SOCIALE CERCO PRIMO BLANKS
137700940728     C     V1CRSM        IFNE      *BLANKS
137800021111      * Non si può fare per lnp 999
137900021111     c                   if        v1clp1 = 999
138000020614     c                   eval      *in42 = *on
138100020614     c                   eval      *in28 = *on
138200020614     c                   eval      *in90 = *on
138300020614     c                   eval      v1cmsg = msg(2)
138400020614     c                   goto      endctr
138500020614     c                   endif
138600901115     C                   SETON                                        11
138700940728     C                   MOVEA     V1CRSM        C1
138800940727     C                   Z-ADD     15            XX                2 0
138900930630     C     C1(XX)        DOWEQ     *BLANKS
139000930630     C                   SUB       1             XX
139100901116     C                   END
139200901116     C                   END
139300901115     C* IMMESSO DESTINATARIO: CERCO PRIMO BLANKS
139400940728     C     V1CRSD        IFNE      *BLANKS
139500021111      * Non si può fare per lnp 999
139600021111     c                   if        v1clp1 = 999
139700020614     c                   eval      *in42 = *on
139800020614     c                   eval      *in28 = *on
139900020614     c                   eval      *in90 = *on
140000020614     c                   eval      v1cmsg = msg(2)
140100020614     c                   goto      endctr
140200020614     c                   endif
140300901115     C                   SETON                                        14
140400940728     C                   MOVEA     V1CRSD        C1
140500940727     C                   Z-ADD     15            ZZ                2 0
140600930630     C     C1(ZZ)        DOWEQ     *BLANKS
140700930630     C                   SUB       1             ZZ
140800901116     C                   END
140900901115     C                   END
141000901115     C                   END
141100930630     C***
141200930630     C* N U M E R O   S P E D I Z I O N E
141300930630     C***
141400930630     C* LINEA PARTENZA
141500160125     C***  SEL2          IFNE      0
141600160125     c                   if        sel2<> 0 and v1cbarcode<=0
141700940728     C                   Z-ADD     V1CLP2        NUM3
141800940728     C     NUM3          CHAIN     AZORG01L                           45
141900940728     C   45              DO
142000940728     C                   MOVEL     MSG(2)        V1CMSG
142100940728     C                   SETON                                        28  90
142200940728     C                   GOTO      ENDCTR
142300940728     C                   ENDDO
142400940728     C*
142500940928     C     KLNP          CHAIN     FNBLP01L                           44
142600030527     C*
142700991029     C  N44              MOVEL     BLPFT1        WFT1              1
142800941115     C*
142900941115     C* ERRORE
143000940729     C   44              DO
143100940729     C                   SETON                                        28  90
143200940728     C                   MOVEL     MSG(3)        V1CMSG
143300940728     C                   GOTO      ENDCTR
143400940728     C                   ENDDO
143500020614      * se sono in manutenzione
143600020614     c                   if        *in97
143700020614      * la linea di partenza della bolla deve far parte del mio terminal
143800060202      * di partenza  se bolla non partita altrimenti la skiera POG
143900060202     c                   if        wft1=' '
144000060202     c     blplnp        lookup    l1                                     31
144100060202     c                   else
144200060203     c                   if        v1ccvb=*blanks
144300060203     c                   clear                   dslr66
144400060203     c                   endif
144500060609     c                   movel     blplnp        w003a             3
144600060202     c     w003a         lookup    $pog                                   31
144700060202     c* se non trovato e causale variaz.bolle anche per p.o. cliente
144800060202     c*  controllo il p.o. cliente nella skiera POG
144900060202     c                   if        not *in31 and d66pocl='S'
145000060202     c                   movel     blpksc        w003a
145100060202     c     w003a         lookup    $pog                                   31
145200060202     c                   endif
145300060202     c                   endif
145400060202     c
145500020614     c                   if        not *in31
145600020614     c                   eval      *in44 = *on
145700020614     c                   eval      *in28 = *on
145800020614     c                   eval      *in90 = *on
145900020614     c                   eval      v1cmsg = msg(28)
146000020614     c                   goto      endctr
146100020614     c                   endif
146200020614     c                   endif
146300901115     C*
146400901115     C                   END
146500160125     c***
146600160125     C* B A R C O D E   L D V
146700160125     c***
146800160125     c                   if        v1cbarcode>0
146900160125     c                   z-add     b_aas         blpaas
147000160125     C     b_aas         IFLT      100
147100160125     C     b_aas         IFGT      60
147200160125     C                   ADD       1900          blpaAS
147300160125     C                   ELSE
147400160125     C                   ADD       2000          blpAAS
147500160125     C                   ENDIF
147600160125     C                   ENDIF
147700160125     c                   z-add     b_lnp         blplnp
147800160125     c                   z-add     b_nrs         blpnrs
147900160125     c                   z-add     b_nsp         blpnsp
148000160125     C     Kblp          CHAIN     FNblp01L
148100160125     C*
148200160125     C                   if        not%found(fnblp01l)
148300160125     C                   MOVEL     MSG(31)       V1CMSG
148400160125     C                   SETON                                        286090
148500160125     C                   GOTO      ENDCTR
148600160125     C                   endif
148700160125     C                   endif
148800010219      ***
148900010219      * N U M E R O   O R M
149000010219      ***
149100010219     C     SEL4          IFNE      0
149200010219     C     SEL2          ANDEQ     0
149300160125     c     v1cbarcode    andeq     0
149400010219      * P.O. EMISSIONE
149500010219     C                   Z-ADD     V1CPOE        NUM3
149600010219     C     NUM3          CHAIN     AZORG01L                           54
149700010219     C     *IN54         IFEQ      *ON
149800010219     C                   MOVEL     MSG(25)       V1CMSG
149900010219     C                   SETON                                        28  90
150000010219     C                   GOTO      ENDCTR
150100010219     C                   ENDIF
150200010219      * N.ORM
150300010219     C     KFORM         CHAIN     FNORM01L                           56
150400010219     C     *IN56         IFEQ      *ON
150500010219     C                   MOVEL     MSG(26)       V1CMSG
150600010219     C                   SETON                                        28  90
150700010219     C                   GOTO      ENDCTR
150800010219     C                   ENDIF
150900010219      *
151000010219     C                   SETON                                        06
151100010219      *
151200010219     C                   ENDIF
151300941222     C*
151400020429     C* SE SONO secondo livello POSSO MANUTENZIONE SOLO LE MIE BOLLE
151500060202    1C** 97simtpp        IFEQ      '2'
151600060202    1C**   simtpp        orEQ      *blanks
151700941222     C*
151800060202    2C**   V1CLP1        IFNE      SAVMUS
151900060202     C**   V1CLP1        ANDNE     0
152000941222     C*
152100060202     C**   V1CLP2        ORNE      SAVMUS
152200060202     C**   V1CLP2        ANDNE     0
152300060202     C**                 MOVEL     MSG(21)       V1CMSG
152400060202     C**                 SETON                                        284590
152500060202     C**                 GOTO      ENDCTR
152600060202    2C**                 ENDIF
152700060202    1C**                 ENDIF
152800991222     C* SE RIFERIMENTO PARTNER ESTERO DPD-PARCEL- DEVE ESSERE COMPOSTO
152900060608     C*  SOLTANTO DA NUMERI se<=12
153000060608     C*  alfanumerico       se=14
153100050121     c                   clear                   wnodpd            1
153200060609    1c                   if        lnpntw = 'DPD' or lnantw = 'DPD'  or
153300050304     c                             lnpntw = *blanks or lnantw = *blanks
153400060609    2c                   if        v1crpp<>*blanks
153500060609    3c                   select
153600060609     c* ok se lungo 14
153700060609     c                   when      %subst(v1crpp:15:1)=' '  and
153800060609     c                             %subst(v1crpp:14:1)<>' '
153900060609     c* ok se lungo 11 ma tutto numerico
154000060609     c                   when      %subst(v1crpp:12:4)=*blanks and
154100060609     c                             %subst(v1crpp:11:1)<>' '
154200060609     C                   MOVEA     V1CRPP        K11
154300000328     C                   Z-ADD     1             YY
154400060609     c
154500060609    4C                   DO        11            YY
154600060609     C     K11(YY)       LOOKUP    CNM                                    30
154700060609    5C     *IN30         IFEQ      *OFF
154800060609     c* Se non è tutto numerico non è DPD
154900060609     c                   eval      wnodpd = '1'
155000060609     c                   leave
155100060609    5c                   endif
155200060609    4c                   enddo
155300060609   x3c                   other
155400060609     c
155500060609     c* se non è lungo 11 o 14 non è DPD
155600060609     c                   eval      wnodpd = '1'
155700060609    3c                   endsl
155800060608     c
155900060609    3c                   if        wnodpd='1'
156000060609    4c     lnantw        ifeq      'DPD'
156100050304     c     lnpntw        oreq      'DPD'
156200060609     C                   MOVEL     MSG(24)       V1CMSG
156300991222     C                   SETON                                        284790
156400050519     C                   GOTO      ENDCTR
156500060609    4C                   ENDIF
156600060609    3C                   ENDIF
156700060609     c
156800060609    2C                   ENDif
156900061116   x1c                   else
157000061116     c                   eval      wnodpd='1'
157100991222    1C                   ENDIF
157200940316      *
157300940727      * DEVO CONSIDERARE DELLE PRIORITA' PER LE SELEZIONI IMMESSE DALL'UTENTE:
157400010219      *    1°    N.ORM                   ( 06 ON )
157500010219      *    2°    CODICE "CHI SONO"       ( 59 ON )
157600010219      *    3°    RIFERIMENTO MITTENTE    ( 12 ON )
157700000831      *          RIFERIMENTO PARTNER     ( 13 ON )
157800101126      *          RIFERIMENTO alfanum     ( 15 ON )
157900010219      *    4°    DESTINATARIO            ( 14 ON )
158000010219      *    5°    MITTENTE                ( 11 ON )
158100010219      *    6°    LINEA DI ARRIVO         ( 01 ON )
158200010219      *    7°    LINEA DI PARTENZA       ( 02 ON )
158300940727      *
158400010219     C     *IN06         IFEQ      *ON
158500010219     C                   SETOFF                                       59
158600010219     C                   SETOFF                                       121314
158700010219     C                   SETOFF                                       110102
158800101126     C                   SETOFF                                       15
158900010219     C                   ENDIF
159000010219      *
159100000831     C     *IN59         IFEQ      *ON
159200000831     C                   SETOFF                                       121314
159300000831     C                   SETOFF                                       110102
159400101126     C                   SETOFF                                       15
159500000831     C                   ENDIF
159600101126     c   15
159700101126     Cor 13
159800951011     COR 12              SETOFF                                       141101
159900101126     c
160000101126     c   15
160100101126     Cor 13
160200951011     COR 12              SETOFF                                       02
160300940727     C   14              SETOFF                                       110102
160400940727     C   11              SETOFF                                       0102
160500940727     C   01              SETOFF                                       02
160600991214     C* SE LNP DPD DEVO CALCOLARE IL CHEK DIGITS E PORTARE IL NUMERO A
160700060609     C*  12 CIFRE PER LA RICERCA, se immesso lungo 11
160800991214     C                   CLEAR                   KRMN
160900060609     C
161000060609    1c                   if        lnpntw = 'DPD'  and v1crmn>0
161100991214     C                   MOVEL     V1CRMN        W004A             4
161200991214     C* SE CI SONO PIU' DI 11 NUMERO, NON CALCOLO IL CHKD
161300991222    3C     W004A         IFEQ      '0000'
161400991214     C                   CLEAR                   DPDBRC
161500991214     C                   MOVE      V1CRMN        W0110            11 0
161600991214     C                   MOVEL     W0110         DPDBRC
161700991214     C                   EXSR      M1031
161800991214     C                   Z-ADD     DPDBRC        KRMN
161900991214     C                   ELSE
162000991214     C* 12 NUMERI
162100991214     C                   Z-ADD     V1CRMN        KRMN
162200991222    3C                   ENDIF
162300991214     C***
162400991214     C                   ELSE
162500991214     C                   Z-ADD     V1CRMN        KRMN
162600991222    1C                   ENDIF
162700991222     C**
162800991222     C* SE LNA DPD DEVO CALCOLARE IL CHEK DIGITS E PORTARE IL NUMERO A
162900060518     C*  12 CIFRE PER LA RICERCA --> ora non più: il check digit per la
163000060609     c*  ricerca non serve più perchè campo a parte nella DBL4I
163100050304    1c                   if        v1crpp <> *blanks and (lnantw='DPD'
163200050304     c                             or lnpntw = 'DPD' or wnodpd=' ')
163300060518     C                   MOVEL     V1CRPP        krpp
163400991222    1C                   ENDIF
163500101126     c
163600900514     C*
163700900514     C     ENDCTR        ENDSR
163800940317      **************************************************************************
163900940317      *    ELABORA BOLLE DA VISUALIZZARE
164000940317      **************************************************************************
164100900515     C     ELAB          BEGSR
164200940317      *
164300901115     C* NUMERO DI SPEDIZIONE
164400160125    1C***  SEL2          IFNE      0
164500160125    1C                   IF        sel2<>0 or v1cbarcode<>0
164600941010     C* SONO IN INTERROGAIZONE BOLLE
164700941010    2C     *IN97         IFEQ      *OFF
164800160125     c                   select
164900160125     c                   when      sel2<>0
165000941214     C                   MOVEL     V1CAAS        PA0AAS
165100941214     C                   MOVEL     V1CLP2        PA0LNP
165200941214     C                   MOVEL     V1CNRS        PA0NRS
165300941214     C                   MOVEL     V1CNSP        PA0NSP
165400160125     c                   other
165500160125     C                   z-add     b_aas         PA0AAS
165600160125     C     b_aas         IFLT      100
165700160125     C     b_aas         IFGT      60
165800160125     C                   ADD       1900          Pa0aAS
165900160125     C                   ELSE
166000160125     C                   ADD       2000          PA0AAS
166100160125     C                   ENDIF
166200160125     C                   ENDIF
166300160125     C                   MOVEL     b_lnp         PA0LNP
166400160125     C                   MOVEL     b_nrs         PA0NRS
166500160125     C                   MOVEL     b_nsp         PA0NSP
166600160125     c                   endsl
166700941214     C  N20              MOVEL     'A'           PA0FLG
166800941214     C   20              MOVEL     ' '           PA0FLG
166900941214     C                   MOVEL     V1CRSU        PA0RSU
167000941214     C                   MOVEL     PARAM0        KPJBU
167100040409if  1c                   if        SDSprm = 1
167200940729     C                   CALL      'FNLS05R'
167300901115     C                   PARM                    KPJBA
167400040409x   1c                   else
167500040409     C                   call      'FNLS05R'
167600040409     C                   parm                    KPJBA
167700040409     C                   parm                    FIDNA1ds
167800040409e   1c                   endif
167900941010     C*
168000941010   X2C                   ELSE
168100941010     C*
168200941010     C* SONO IN MANUTENZIONE BOLLE
168300991020     C                   CLEAR                   DBLP
168400941010     C* SE BOLLA NON PARTITA RICHIAMO IL PGM FNLS01R
168500941010    3C     WFT1          IFEQ      ' '
168600941010     C                   MOVEL     BLPAAS        §LPAAS
168700941010     C                   MOVEL     BLPLNP        §LPLNP
168800941010     C                   MOVEL     BLPNRS        §LPNRS
168900941010     C                   MOVEL     BLPNSP        §LPNSP
169000941010     C                   MOVEL     'M'           §LPFPR
169100030612     C*                  MOVEL     PARAM4        KPJBU
169200941010     C                   CALL      'FNLS01R'
169300941010     C                   PARM                    KPJBA
169400991020     C                   PARM                    DBLP
169500991020     C                   PARM                    DTASV
169600030612     C                   PARM                    trul90ds
169700941115     C*
169800941010     C                   MOVEL     '1'           PA1S01            1
169900161202     c* uscita automatica dopo 50 secondi
170000161202     C     §LPFPR        IFEQ      '6'
170100161202     C                   SETON                                        284590
170200161202     C                   MOVEL     msg(32)       V1CMSG
170300161202     C                   ENDIF
170400941010     C*
170500941010   X3C                   ELSE
170600941010     C*
170700941222     C* SE BOLLA PARTITA RICHIAMO PGM FNLR48R
170800941222     C                   CLEAR                   DSLR48
170900941222     C                   MOVEL     BLPAAS        D48AAS
171000941222     C                   MOVEL     BLPLNP        D48LNP
171100941222     C                   MOVEL     BLPLNP        D48FGS
171200941222     C                   MOVEL     BLPNRS        D48NRS
171300941222     C                   MOVEL     BLPNSP        D48NSP
171400941222     C                   MOVEL     BLPCBO        D48CBO
171500941222     C                   MOVEL     'P'           D48TBO
171600941222     C                   MOVEL     V1CCVB        D48CVB
171700950607     C                   MOVEL     'S'           D48F12
171800141030
171900141030     c* routine per verifica dispo da esaminare
172000141030     c                   exsr      sr_wdwdc
172100141030
172200941222     C                   MOVEL     DSLR48        KPJBU
172300941222     C                   CALL      'FNLR48R'
172400941222     C                   PARM                    KPJBA
172500941222     C                   PARM                    DSARBD
172600941222     C                   PARM                    DSARBK
172700991012     C                   PARM                    DARBT
172800941222     C                   PARM                    DSARBG
172900030612     C                   PARM                    trul90ds
173000941222     C*
173100941222     C                   MOVEL     KPJBU         DSLR48
173200941222     C* ERRORE
173300941222     C     D48ERR        IFNE      *BLANKS
173400941222     C                   MOVEL     D48MSG        V1CMSG
173500941222     C                   SETON                                        284590
173600941222    4C                   ENDIF
173700941010     C*
173800941010    3C                   ENDIF
173900941010    2C                   ENDIF
174000901115     C*
174100940324   X1C                   ELSE
174200901115     C*
174300901115     C* DATA SPEDIZIONE
174400000831    2C     SEL1          IFNE      *ZERO
174500160125     c     v1crpp        orne      *blanks
174600000831     C     SEL3          ORNE      *BLANKS
174700000831     C     V1CNOT        ORNE      *BLANKS
174800010219     C     SEL4          ORNE      *ZEROS
174900101124     C     V1Crma        ORNE      *BLANKS
175000940727     C                   Z-ADD     1             REC2
175100900517     C*
175200901116     C                   MOVEL     *BLANKS       TSP
175300940728     C                   MOVEL     V1CTSP        TSP               1
175400940316      *
175500940727      * POSIZIONO IL FILE A SECONDA DELLA SELEZIONE VIDEO DELL'UTENTE
175600940727     C                   SELECT
175700010219      * N.ORM
175800010219     C     *IN06         WHENEQ    *ON
175900010219     C                   MOVEL     'M'           WTRC
176000010219     C                   MOVEL     SEL4          WN14
176100060217     C     Kar43         SETLL     Fiar404L
176200940727      *
176300000831     C     *IN59         WHENEQ    *ON
176400051013     C     KarS          SETLL     FIarS02L
176500000831      *
176600940727     C     *IN12         WHENEQ    *ON
176700940727     C     *IN16         IFEQ      *ON
176800010718     C     BLP33K        SETLL     FNBLP033
176900940727     C                   ELSE
177000010718     C     KRMN          SETLL     FNBLP033
177100940727     C                   ENDIF
177200940727      *
177300101126     C     *IN15         WHENEQ    *ON
177400101126     C     *IN16         IFEQ      *ON
177500101126     C     BLP32K        SETLL     FNBLP032
177600101126     C                   ELSE
177700101126     C     v1crma        SETLL     FNBLP032
177800101126     C                   ENDIF
177900101126      *
178000951011     C     *IN13         WHENEQ    *ON
178100050121     c                   clear                   wgiro             1
178200050304     c                   if        lnantw <> 'DPD' and lnpntw <> 'DPD'
178300991222     C                   MOVEL     'E'           WTRC
178400971212     C                   MOVEL(P)  V1CRPP        WN30             30
178500991222     C                   ELSE
178600010219     C* D P D
178700060518     C****               MOVEL     'F'           WTRC
178800060518     C                   MOVEL     'I'           WTRC
178900991222     C                   MOVEL(P)  KRPP          WN30             30
179000991222    1C                   ENDIF
179100991222     C*
179200060217     C     Kar4          SETLL     Fiar403L
179300951011      *
179400940727     C     *IN14         WHENEQ    *ON
179500940727     C                   Z-ADD     COMAAD        $AAS
179600940727     C                   Z-ADD     COMMGD        $MGS
179700021111     c                   if        v1clp1 <> *zeros
179800021111     c                   eval      klp1 = v1clp1
179900021111     c                   endif
180000021111     c                   if        v1clp1 = *zeros
180100021111     c                   eval      ww = 1
180200021111     c                   eval      klp1 = l1(WW)
180300021111     c                   endif
180400010718     C     BLP43K        SETLL     FNBLP043
180500940727      *
180600940727     C     *IN11         WHENEQ    *ON
180700940727     C                   Z-ADD     COMAAD        $AAS
180800940727     C                   Z-ADD     COMMGD        $MGS
180900021111     c                   if        v1clp1 <> *zeros
181000021111     c                   eval      klp1 = v1clp1
181100021111     c                   endif
181200021111     c                   if        v1clp1 = *zeros
181300021111     c                   eval      ww = 1
181400021111     c                   eval      klp1 = l1(WW)
181500021111     c                   endif
181600010718     C     BLP44K        SETLL     FNBLP044
181700940316      *
181800940316      * LETTURA PER LNA DATA SPEDIZ. TIPO SPEDIZIONE
181900940727     C     *IN01         WHENEQ    *ON
182000940316     C     *IN16         IFEQ      *ON
182100020627     c                   if        v1clp1 = 999
182200010718     C     KLNA1         SETLL     FNBLP026
182300020627     c                   endif
182400020627     c                   if        v1clp1 <> *zeros and v1clp1 <> 999
182500020627     c                   eval      klp1 = v1clp1
182600020627     c     klna3         setll     fnblp076
182700020627     c                   endif
182800020627     c                   if        v1clp1 = *zeros
182900020627     c                   eval      ww = 1
183000020627     c                   eval      klp1 = l1(WW)
183100020627     c     klna3         setll     fnblp076
183200020627     c                   endif
183300940316     C                   ELSE
183400940316      * LETTURA SOLO PER LNA
183500020614     c                   if        v1clp1 = 999
183600010718     C     V1CLNA        SETLL     FNBLP026
183700020614     c                   endif
183800020614     c                   if        v1clp1 <> *zeros and v1clp1 <> 999
183900020614     c                   eval      klp1 = v1clp1
184000020627     c     klna4         setll     fnblp076
184100020614     c                   endif
184200020614     c                   if        v1clp1 = *zeros
184300020614     c                   eval      ww = 1
184400020614     c                   eval      klp1 = l1(WW)
184500020627     c     klna4         setll     fnblp076
184600020614     c                   endif
184700940316     C                   ENDIF
184800940928     C*
184900901116     C* LETTURA PER LNP DATA SPEDIZ. SERIE
185000940727     C     *IN02         WHENEQ    *ON
185100010718     C     KLNP2         SETLL     FNBLP031
185200940316      *
185300940727     C                   ENDSL
185400940316      *
185500930630     C** CARICO SUBFILE
185600930630     C                   Z-ADD     0             COMOD
185700930630     C                   EXSR      CARSF
185800940324    2C                   END
185900010219      *
186000940324    1C                   END
186100900518     C*
186200901116     C                   ENDSR
186300940317      **************************************************************************
186400940317      *    CARICO SUBFILE IN AVANTI
186500940317      **************************************************************************
186600900515     C     CARSF         BEGSR
186700900517     C*
186800900518     C                   Z-ADD     1             B                 2 0
186900940727     C                   Z-ADD     COMOD         NRR2
187000991222    1C     B             DOWLE     18
187100051013     c                   setoff                                       32
187200941010     C*
187300010219      * N.ORM
187400010219    2C     *IN06         IFEQ      *ON
187500060217     C     Kar43         READE     Fiar404L                               30
187600010219     C  N30KBLP4         CHAIN     FNBLP01L                           33
187700010219    3C  N30*IN33         IFEQ      *OFF
187800020614      * se sono in manutenzione
187900020614  b4 c                   if        *in97
188000051013     c
188100020614      * la linea di partenza della bolla deve far parte del mio terminal di
188200060202      * partenza se bolla non partita
188300060202     c                   if        blpft1=' '
188400020614     c     blplnp        lookup    l1                                     31
188500060203     c                   else
188600060203     C                   SETON                                        31
188700060202     c***                movel     blplnp        w003a
188800060202     c***  w003a         lookup    $pog                                   31
188900060202     c                   endif
189000060202     c
189100020614  b5 c                   if        not *in31
189200020614     c                   eval      *in32 = *on
189300020614  e5 c                   endif
189400020614  e4 c                   endif
189500010219   X3C                   ELSE
189600010219     C                   SETON                                        32
189700010219    3C                   ENDIF
189800010219    2C                   ENDIF
189900000831      *
190000000831    2C     *IN59         IFEQ      *ON
190100051013     C     KarS          READE     FIarS02L                               30
190200051013     c                   if        not *in30
190300051013     C     KarSXT        CHAIN     FNBLT27L                           33
190400051013     C  N33KBLTXP        CHAIN     FNBLP01L                           33
190500051013    3C     *IN33         IFEQ      *OFF
190600000831   X3C                   ELSE
190700000831     C                   SETON                                        32
190800000831    3C                   ENDIF
190900000831    2C                   ENDIF
191000051013    2C                   ENDIF
191100940316      *
191200010718     C   12KRMN          READE     FNBLP033                               30
191300101126     C   15v1crma        READE     FNBLP032                               30
191400940316      *
191500991222    2C     *IN13         IFEQ      *ON
191600060217     c     leggar4       tag
191700991222     C*
191800050121    3C**** KRPP          IFEQ      *ZEROS
191900050304     c                   if        lnantw <> 'DPD' and lnpntw <> 'DPD'
192000050304     c                             and wgiro <> '2'
192100060217     C     Kar4          READE     Fiar403L                               30
192200991222     C* DPD
192300991222   X3C                   ELSE
192400060217     C     WTRC          READE     Fiar403L                               30
192500060518     c
192600060518     C  N30              MOVEL     ar4N30        DSBL4i
192700060609     c  n30              if        krpp <> §b4ipn
192800060518     c                   seton                                        30
192900060518     c                   endif
193000991222    3C                   ENDIF
193100050121     c*
193200050304     c                   if        *in30 and wgiro <> '2' and
193300050304     c                             lnpntw <> 'DPD' and lnantw <> 'DPD' and
193400050304     c                             wnodpd = *blanks
193500050121     c                   eval      wgiro = '2'
193600060518     C                   MOVEL     'I'           WTRC
193700050121     C                   MOVEL(P)  KRPP          WN30             30
193800060217     C     Kar4          SETLL     Fiar403L
193900060217     c                   goto      leggar4
194000050121     c                   endif
194100061116     c*
194200061116     c* Se rif.partner può essere DPD non export e immesso rif. lungo 11
194300061116     c* faccio tentaivo di ricerca con rif.truccato
194400061116     c                   if        *in30 and lnantw<>'DPD'and wnodpd=*blanks
194500061116     c                             and %subst(krpp:12:1)=' '
194600061116     C                   eval      krpp='0'+%subst(krpp:1:3)+'99'
194700061116     c                             +%subst(krpp:4:8)
194800061116     C                   MOVEL(P)  KRPP          WN30             30
194900061116     C     Kar4          SETLL     Fiar403L
195000061116     c                   goto      leggar4
195100061116     c                   endif
195200991222     C**
195300991222     C  N30KBLP4         CHAIN     FNBLP01L                           33
195400991222    3C  N30*IN33         IFEQ      *OFF
195500951011     C                   ELSE
195600951011     C                   SETON                                        32
195700991222    3C                   ENDIF
195800991222    2C                   ENDIF
195900951011      *
196000021111      * lettura per ragione sociale destinatario
196100021111     c                   if        *in14
196200021111     C     Klp1          READE     FNBLP043                               30
196300021111      * leggo un'altra linea di partenza
196400021111     c                   if        *in30 and v1clp1 = *zeros
196500021111     c                   eval      *in30 = *off
196600021111     c                   add       1             ww
196700021111     c                   if        l1(ww) = *zeros
196800021111     c                   eval      *in30 = *on
196900021111     c                   else
197000021111     c                   eval      klp1 = l1(ww)
197100021111     C                   Z-ADD     COMAAD        $AAS
197200021111     C                   Z-ADD     COMMGD        $MGS
197300021111     c     blp43k        Setll     FNBLP043
197400021111     c     Klp1          READE     FNBLP043                               30
197500021111     c                   endif
197600021111     c                   endif
197700021111     c                   endif
197800940316      *
197900021111      * lettura per ragione sociale mittente
198000021111     c                   if        *in11
198100021111     C     Klp1          READE     FNBLP044                               30
198200021111      * leggo un'altra linea di partenza
198300021111     c                   if        *in30 and v1clp1 = *zeros
198400021111     c                   eval      *in30 = *off
198500021111     c                   add       1             ww
198600021111     c                   if        l1(ww) = *zeros
198700021111     c                   eval      *in30 = *on
198800021111     c                   else
198900021111     c                   eval      klp1 = l1(ww)
199000021111     C                   Z-ADD     COMAAD        $AAS
199100021111     C                   Z-ADD     COMMGD        $MGS
199200021111     c     blp44k        Setll     FNBLP044
199300021111     c     Klp1          READE     FNBLP044                               30
199400021111     c                   endif
199500021111     c                   endif
199600021111     c                   endif
199700940316      *
199800020614      * lettura per linea di arrivo
199900020614     c                   if        *in01
200000020614      *  richiesta lnp 999
200100020614     c                   if        v1clp1 = 999
200200020614     C     V1CLNA        READE     FNBLP026                               30
200300020614     c                   endif
200400020614      *  richiesta lnp a 0 o una linea di partenza specifica
200500020614     c                   if        v1clp1 <> 999
200600020627     c     klna4         reade     fnblp076                               30
200700020627      * leggo un'altra linea di partenza
200800020627     c                   if        *in30 and v1clp1 = *zeros
200900020627     c                   eval      *in30 = *off
201000020614     c                   add       1             ww
201100020614     c                   if        l1(ww) = *zeros
201200020614     c                   eval      *in30 = *on
201300020627     c                   else
201400020614     c                   eval      klp1 = l1(ww)
201500020627     c     klna4         setll     fnblp076
201600021205     c     klna4         reade     fnblp076                               30
201700020614     c                   endif
201800020614     c                   endif
201900020627
202000020614     c                   endif
202100020614     c                   endif
202200900517     C*
202300010718     C   02V1CLP1        READE     FNBLP031                               30
202400060217     c  N32
202500051013     CanN30              EXSR      CONFR1
202600010718     C  N30
202700010718     CANN32              EXSR      EMISS1
202800020627      * se lettura per lna leggo un'altra linea di partenza
202900021111      * o per rag.soc. destinatario o rag.soc. mittente
203000021111     c                   if        (*in01 or *in11 or *in14) and
203100021111     c                              *in30 and V1clp1 = *zeros
203200020627     c                   eval      *in30 = *off
203300020627     c                   add       1             ww
203400020627     c                   if        l1(ww) = *zeros
203500020627     c                   eval      *in30 = *on
203600020627     c                   else
203700020627     c                   eval      klp1 = l1(ww)
203800021111     c                   if        *in01
203900020627     c     klna4         setll     fnblp076
204000020627     c     klna4         reade     fnblp076
204100021111     c                   endif
204200021111     c                   if        *in11
204300021111     C                   Z-ADD     COMAAD        $AAS
204400021111     C                   Z-ADD     COMMGD        $MGS
204500021111     c     blp44k        Setll     FNBLP044
204600021111     c                   endif
204700021111     c                   if        *in14
204800021111     C                   Z-ADD     COMAAD        $AAS
204900021111     C                   Z-ADD     COMMGD        $MGS
205000021111     c     blp43k        Setll     FNBLP043
205100021111     c                   endif
205200020627     c                   endif
205300020627     c                   endif
205400020627
205500900515     C  N30              END
205600940727     C                   Z-ADD     NRR2          COMOD             4 0
205700900516     C*
205800900515     C                   ENDSR
205900940727      **************************************************************************
206000940727      *  Riposizionamento (solo per data) per letture su bolle
206100940727      **************************************************************************
206200940727     C     RIPOS         BEGSR
206300940727      *
206400010718     C                   Z-ADD     BLPDSP        WDSP
206500950216     C*
206600950216     C     COMDSA        IFGT      WDSP
206700940727     C     COMDSA        OREQ      *ZERO
206800940727      *
206900940727      * Mi posiziono sul record maggiore per la stessa data bolla
207000010718     C                   Z-ADD     BLPAAS        $AAS
207100010718     C                   Z-ADD     BLPMGS        $MGS
207200021111     C   14BL43K         SETGT     FNBLP043
207300010718     C   11BL44K         SETGT     FNBLP044
207400940727      *
207500940727     C                   ELSE
207600940727     C                   MOVE      'S'           $ULTIM
207700940727     C                   ENDIF
207800940727      *
207900940727     C                   ENDSR
208000940727      **************************************************************************
208100940727      *  Riposizionamento (per data e rag.soc.) per letture su bolle
208200940727      **************************************************************************
208300940727     C     RIPOS1        BEGSR
208400940727      *
208500940727      * Mi posiziono sulla ragione sociale video
208600010718     C                   Z-ADD     BLPAAS        $AAS
208700010718     C                   Z-ADD     BLPMGS        $MGS
208800010718     C   14BLP43K        SETLL     FNBLP043
208900010718     C   11BLP44K        SETLL     FNBLP044
209000940727      *
209100940727     C                   ENDSR
209200940317      **************************************************************************
209300940317      *    CONFRONTO PER CERCARE BOLLE DA EMETTERE
209400940317      **************************************************************************
209500940928     C     CONFR1        BEGSR
209600940727      *
209700940727      * OCCHIO!!   Devo usare per primo il confronto che potrebbe portarmi a
209800940727      * fine ciclo: se interrogo per destinatario (14ON) ci deve essere prima il
209900940728      * confronto con V1CRSD, se invece sono per mittente devo fare prima quello
210000940728      * con V1CRSM.
210100940727     C                   MOVE      *BLANKS       $ULTIM            1
210200940727     C                   MOVE      *BLANKS       $SALTA            1
210300940329      *
210400940727      *??  R A G I O N E    S O C I A L E    D E S T I N A T A R I O  ?
210500940728     C     V1CRSD        IFNE      *BLANKS
210600940727     C     ZZ            SUBST(P)  BLPRSD        COMRAG           15
210700940727      * Se rag.soc. letta > video riposiziono solo per data con SETGT
210800940728     C     COMRAG        IFGT      V1CRSD
210900940727     C                   SETON                                            32
211000940727     C   14              EXSR      RIPOS
211100940727     C                   MOVE      'S'           $SALTA
211200940727     C                   ENDIF
211300940727      * Se rag.soc. letta < video riposiziono per stessa data + rag.soc.video
211400940728     C     COMRAG        IFLT      V1CRSD
211500940727     C                   SETON                                            32
211600940727     C   14              EXSR      RIPOS1
211700940727     C                   MOVE      'S'           $SALTA
211800940727     C                   ENDIF
211900940727     C                   ENDIF
212000940329     C*
212100940727      *??  R A G I O N E    S O C I A L E    M I T T E N T E  ?
212200940728     C     V1CRSM        IFNE      *BLANKS
212300940727     C     XX            SUBST(P)  BLPRSM        COMRAG           15
212400940727      * Se rag.soc. letta > video riposiziono solo per data con SETGT
212500940728     C     COMRAG        IFGT      V1CRSM
212600940727     C                   SETON                                            32
212700940727     C   11              EXSR      RIPOS
212800940727     C                   MOVE      'S'           $SALTA
212900940727     C                   ENDIF
213000940727      * Se rag.soc. letta < video riposiziono per stessa data + rag.soc.video
213100940728     C     COMRAG        IFLT      V1CRSM
213200940727     C                   SETON                                            32
213300940727     C   11              EXSR      RIPOS1
213400940727     C                   MOVE      'S'           $SALTA
213500940727     C                   ENDIF
213600940727     C                   ENDIF
213700940727      *
213800940727     C     $ULTIM        COMP      'S'                                    30
213900940727     C     $SALTA        CABEQ     'S'           ENDCON
214000940317      *
214100900518     C                   SETOFF                                       32
214200000531      *
214300000531      * SOLO BOLLE BORDERIZZATE O NON BORDERIZZATE
214400940729     C     V1CFTA        IFEQ      '1'
214500940729     C     BLPDBR        ANDNE     *ZERO
214600940729     C                   SETON                                            32
214700940729     C                   GOTO      ENDCON
214800940729     C                   ENDIF
214900940729     C     V1CFTA        IFEQ      '2'
215000940729     C     BLPDBR        ANDEQ     *ZERO
215100940729     C                   SETON                                            32
215200940729     C                   GOTO      ENDCON
215300940729     C                   ENDIF
215400940729     C*
215500900515     C** CONFRONTI SOLO PER LINEA ARRIVO
215600900515     C*  DATA SPEDIZIONE > DATA AL
215700940727     C   16BLPDSP        IFLT      COMDSD
215800940727     C     COMDSD        ANDNE     *ZERO
215900940727     C                   SETON                                            32
216000900515     C                   GOTO      ENDCON
216100900515     C                   END
216200940727     C   16BLPDSP        IFGT      COMDSA
216300940727     C     COMDSA        ANDNE     *ZERO
216400940727     C                   SETON                                            32
216500940727     C  N11
216600940727     CANN14              SETON                                            30
216700900518     C                   GOTO      ENDCON
216800900518     C                   END
216900020627      * controllo in base alla linea di partenza inserita
217000020627     c                   exsr      ctrlnp
217100020627     c                   if        *in32 = *on
217200020627     c                   goto      endcon
217300020627     c                   endif
217400940317      *
217500940729      * VERIFICO ANCHE LA LNA POICHE' POTREI AVER FATTO ALTRE SELEZIONI PRIMARIE
217600940728     C     BLPLNA        IFNE      V1CLNA
217700940728     C     V1CLNA        ANDNE     *ZERO
217800940727     C                   SETON                                            32
217900940727     C                   GOTO      ENDCON
218000940727     C                   END
218100940317      *
218200961118     C* RESO / FRANCO FRONTIERA
218300961118     C     V1CFBR        IFNE      ' '
218400961118     C                   MOVEL     V1CFBR        WFBR
218500961118     C     V1CFBR        IFEQ      'R'
218600961118     C                   MOVEL     'S'           WFBR
218700961118     C                   ENDIF
218800961118     C     BLPFBR        IFNE      WFBR
218900961118     C                   SETON                                            32
219000961118     C                   GOTO      ENDCON
219100961118     C                   ENDIF
219200961118     C                   ENDIF
219300900515     C* TIPO SPEDIZIONE
219400040216     ***C   04BLPTSP        IFNE      TSP
219500040216     C                   IF            TSP    <> *blanks
219600040216     C                             and BLPtsp <> TSP
219700900515     C                   SETON                                            32
219800900515     C                   GOTO      ENDCON
219900900515     C                   END
220000040216      * Tipo bolla
220100040216     c                   if            V1Ccbo <> *blanks
220200040216     c                             and V1Ccbo <> BLPcbo
220300040216     c                   eval      *in32 = *on
220400040216     c                   goto      EndCon
220500040216     c                   endif
220600900518     C* SERIE DAL + AL
220700940728     C     BLPNRS        IFLT      V1CPSD
220800900518     C                   SETON                                            32
220900900518     C                   GOTO      ENDCON
221000900518     C                   END
221100940728     C     BLPNRS        IFGT      V1CPSA
221200900515     C                   SETON                                            32
221300900515     C                   GOTO      ENDCON
221400900515     C                   END
221500900518     C* SEGNACOLLO DAL SOLO
221600930630     C     *IN07         IFEQ      *ON
221700930630     C     *IN08         ANDEQ     *OFF
221800940727     C     KBLP2         SETLL     FNBLT01L                               31
221900930630     C  N31              SETON                                        32
222000930630     C  N31              GOTO      ENDCON
222100930630     C                   END
222200900515     C* SEGNACOLLO DAL + AL
222300930630     C     *IN08         IFEQ      *ON
222400910827     C                   SETON                                        32
222500940727     C     KBLP          SETLL     FNBLT01L
222600940727     C     KBLP          READE     FNBLT01L                               31
222700910827     C     *IN31         DOWEQ     '0'
222800940729     C     BLTNSC        IFGE      V1CSCD
222900940729     C     BLTNSC        ANDLE     V1CSCA
223000910827     C                   SETON                                        31
223100910827     C                   SETOFF                                       32
223200940923     C                   ENDIF
223300910827     C*
223400940727     C  N31KBLP          READE     FNBLT01L                               31
223500940923     C                   ENDDO
223600910827     C*
223700910827     C   32              GOTO      ENDCON
223800940923     C                   ENDIF
223900940923     C* FILIALE SEGNACOLLO
224000940923     C     V1CFLS        IFNE      *ZERO
224100940923     C     V1CFLS        ANDNE     BLPFLS
224200940923     C                   SETON                                            32
224300940923     C                   GOTO      ENDCON
224400940926     C                   ENDIF
224500940316     C* CODICE CLIENTE
224600950504     C     V1CKSD        IFNE      *ZERO
224700950504     C     BLPKSC        IFNE      V1CKSD
224800900518     C                   SETON                                            32
224900900518     C                   END
225000950504     C* CONTROLLO PER ASSEGNATI IL COD CLIENTE MITTENTE
225100950504     C   32BLPCCM        IFEQ      V1CKSD
225200950504     C                   SETOFF                                           32
225300950504     C                   ENDIF
225400950504     C*
225500950504     C   32              GOTO      ENDCON
225600950504     C                   ENDIF
225700940923     C*
225800940923     C* SELEZIONE PER COLLI SOLO A TERRA O TUTTI
225900940923     C* BOLLE A TERRA:       DSP <= DATE RICHIESTE (SE CI SONO)
226000940923     C*                 AND  DUC = 0  OPPURE  DUC > DSP
226100940923     C     V1CTER        IFEQ      'T'
226200940923     C                   MOVEL     *BLANKS       WTERRA            1
226300950213     C*
226400950228     C* ESCLUDO TUTTE LA BOLLE SENZA COLLI
226500950228     C     BLPNRS        IFEQ      *ZERO
226600950228     C     BLPNCD        ANDEQ     *ZERO
226700950228     C     BLPNCA        ANDEQ     *ZERO
226800950228     C     BLPFNS        ANDEQ     *BLANKS
226900950213     C                   SETON                                            32
227000950213     C                   GOTO      ENDCON
227100950213     C                   ENDIF
227200940923     C*
227300940923     C     BLPDUC        IFEQ      *ZERO
227400940923     C     BLPDUC        ORGT      BLPDSP
227500940923     C                   MOVEL     'S'           WTERRA
227600940923     C                   ENDIF
227700940923     C*
227800950206     C* SE RICHIESTE SOLO LE BOLLE A TERRA, ESCLUDO I RECUPERI
227900950206     C     BLPCBO        LOOKUP    REC                                    31
228000950206     C     *IN31         IFEQ      *ON
228100950206     C                   SETON                                            32
228200950206     C                   GOTO      ENDCON
228300950206     C                   ENDIF
228400940923     C*
228500940923     C* SE IL FLAG E' ANCORA VUOTO LA SPEDIZIONE NON E' A TERRA
228600940923     C     WTERRA        IFEQ      *BLANKS
228700940923     C                   SETON                                            32
228800940923     C                   GOTO      ENDCON
228900940923     C                   ENDIF
229000940923     C*
229100940923     C                   ENDIF
229200940322      *
229300940729      * Se il pgm e' richiamato da un cliente (PA3CLI='S'), devo caricare
229400940727      * solo i clienti presenti nella schiera CLI (caricata inizialmente)
229500111020     C**!!!PA3CLI        IFEQ      'S'
229600111020     C**!!!BLPKSC        LOOKUP    CLI                                    31
229700111020     C*!N31              SETON                                            32
229800111020     C*!N31              GOTO      ENDCON
229900111020     C**!!!              ENDIF
230000930406     C*
230100930406     C* SPEDIZIONI DA CONSEGNARE
230200041028     c* chian su arb se c'e'
230300041028     c                   if        *in22 or *in23
230400041028     c     kblp          chain     fnarb01l
230500041028     c                   if        %found(fnarb01l)
230600041028     c                   eval      wdcm=arbdcm
230700041028     c                   else
230800041028     c                   eval      wdcm=blpdcm
230900041028     c                   endif
231000041028     c                   endif
231100041028     c
231200041028     C   22wdcm          IFNE      0
231300930406     C                   SETON                                            32
231400930406     C                   GOTO      ENDCON
231500930406     C                   END
231600930406     C* SPEDIZIONI CONSEGNATE
231700041028     C   23wdcm          IFEQ      0
231800930406     C                   SETON                                            32
231900930406     C                   GOTO      ENDCON
232000930406     C                   END
232100900515     C* RIFERIMENTI MITTENTE NUMERICI
232200991214     C     BLPRMN        IFNE      KRMN
232300991214     C     KRMN          ANDNE     *ZERO
232400900515     C                   SETON                                            32
232500900515     C                   GOTO      ENDCON
232600900515     C                   END
232700990118     C* CONTROLLO CA SE RICHIESTO
232800990118     C     V1CDAN        IFNE      ' '
232900990118     C                   EXSR      CONFCA
233000990118     C   32              GOTO      ENDCON
233100990118     C                   ENDIF
233200940317      *
233300900515     C     ENDCON        ENDSR
233400020627      *------------------------------------------------------------------------*
233500020627      *  Controllo la bolla letta in base alla linea di partenza
233600020627      *------------------------------------------------------------------------*
233700020627     c     ctrlnp        begsr
233800020627
233900020627      * se richiesta lnp a 0 vuol dire tutte le bolle del terminal di partenza
234000020627     c                   if        v1clp1 = *zeros
234100020627     c     blplnp        lookup    l1                                     31
234200020627     c                   if        not *in31
234300020627     c                   eval      *in32 = *on
234400020627     c                   endif
234500020627     c                   endif
234600020627      * se richiesta una lnp specifica la bolla deve essere di quella linea di
234700020627      * partenza
234800020627     c                   if        v1clp1 <> *zeros  and v1clp1 <> 999
234900020627     c                             and blplnp <> v1clp1
235000020627     c                   eval      *in32 = *on
235100020627     c                   endif
235200020627      * se richiesta lnp a 999 vuol dire tutte le bolle presenti
235300020627      * quindi non controllo
235400020627
235500020627     c                   endsr
235600990118      **************************************************************************
235700990118      * CONTROLLO CA APERTE E CHIUSE
235800990118      **************************************************************************
235900990118     C     CONFCA        BEGSR
236000990118     C                   SETON                                        32
236100040810     C**""*KBLP          CHAIN     FNDCT02L                           31
236200040810     C**""**IN31         DOWEQ     *OFF
236300990118     C* SOLO LE BUONE
236400040810     C**""*DCTATB        IFEQ      ' '
236500990118     C* APERTA
236600040810     C**""*DCTDCH        IFEQ      0
236700040810     C**""*V1CDAN        ANDEQ     'A'
236800040810     C**""*              SETOFF                                       32
236900040810     C**""*              SETON                                        31
237000040810     C**""*              ENDIF
237100990118     C* CHIUSE
237200040810     C**""*DCTDCH        IFGT      0
237300040810     C**""*V1CDAN        ANDEQ     'C'
237400040810     C**""*              SETOFF                                       32
237500040810     C**""*              SETON                                        31
237600040810     C**""*              ENDIF
237700990118     C** CA APERTA O CHIUSA
237800040810     C**""*V1CDAN        IFEQ      'E'
237900040810     C**""*              SETOFF                                       32
238000040810     C**""*              SETON                                        31
238100040810     C**""*              ENDIF
238200040810     C**""*              ENDIF
238300990118     C**
238400040810     C**""* N31KBLP          READE     FNDCT02L                               31
238500040810     C**""*              ENDDO
238600040810      *
238700040810      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
238800040810      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
238900040810      * legate alla spedizione che passo
239000040810     c                   clear                   fidn12ds
239100040810     c                   eval      i12aas = BLPAAS
239200040810     c                   eval      i12lnp = BLPLNP
239300040810     c                   eval      i12nrs = BLPNRS
239400040810     c                   eval      i12nsp = BLPNSP
239500040810     c                   eval      i12fel = 'E'
239600040810     c                   eval      i12fan = 'E'
239700040810     c*
239800040810     c                   call      'FIDN12R'
239900040810     c                   parm                    fidn12ds
240000040810     c*
240100040810     c                   setoff                                       31
240200040810     c*
240300040810     c* se non ci sono errori
240400040810     c                   if        o12err <> ' '
240500040810     c                   seton                                        31
240600040810     c                   else
240700040810     c* se numero di CA trovate maggiore di zero
240800040810     c                   if        o12nca = 0
240900040810     c                   seton                                        31
241000040810     c                   else
241100040810     c                   z-add     1             jj                2 0
241200040810     c                   dow       jj <= o12nca and *in31 = *off
241300040810     C* APERTA
241400040810     C     skDch(jj)     IFEQ      0
241500040810     C     V1CDAN        ANDEQ     'A'
241600040810     C                   SETOFF                                       32
241700040810     C                   SETON                                        31
241800040810     C                   ENDIF
241900040810     C* CHIUSE
242000040810     C     skDch(jj)     IFGT      0
242100040810     C     V1CDAN        ANDEQ     'C'
242200040810     C                   SETOFF                                       32
242300040810     C                   SETON                                        31
242400040810     C                   ENDIF
242500040810     C** CA APERTA O CHIUSA
242600040810     C     V1CDAN        IFEQ      'E'
242700040810     C                   SETOFF                                       32
242800040810     C                   SETON                                        31
242900040810     C                   ENDIF
243000040810     c*
243100040810     c                   add       1             jj
243200040810     c                   enddo
243300040810     c*
243400040810     c                   seton                                        31
243500040810     c*
243600040810     c                   endif
243700040810     c                   endif
243800040810     C**
243900990118     C                   ENDSR
244000940317      **************************************************************************
244100940928      * EMISSIONE SUBFILE DALLA LETTURA SU FNBLP
244200940317      **************************************************************************
244300940928     C     EMISS1        BEGSR
244400940317      *
244500930607     C                   SETOFF                                       27
244600990118     C                   CLEAR                   VSCFLA
244700940728     C                   MOVEL     *BLANKS       VSCSCE
244800940728     C   01              MOVEL     BLPLNP        VSCLIN
244900940728     C  N01              MOVEL     BLPLNA        VSCLIN
245000940728     C                   MOVEL     BLPLNP        VSCLNP
245100941222     C                   MOVEL     BLPCBO        VSCCBO
245200940808     C                   MOVEL     BLPAAS        VSCAAS
245300941222     C                   MOVEL     BLPFT2        VSCFT1
245400920206     C*
245500900516     C                   MOVEL     BLPMGS        MMGG
245600940728     C                   MOVEL     GG            VSCDSP
245700940728     C                   MOVE      MM            VSCDSP
245800900516     C*
245900940728     C                   MOVEL     BLPNSP        VSCNSP
246000950114     C* RIEMPO ALCUNI CAMPI DEL SFL
246100950114     C                   EXSR      RIESFL
246200920206     C*
246300050405     C     KBLP          CHAIN     tiGCP01L                           31
246400900515     C  N31GCPDCG        IFEQ      0
246500940728     C                   MOVEL     '*'           VSCGIA
246600910321     C                   ELSE
246700940728     C                   MOVEL     'C'           VSCGIA
246800900515     C                   END
246900990118     C* SPEDIZIONE IN DANNO
247000990118     C* SE MI HANNO CHIESTO LE CA CHIUSE VISUALIZZO IL FLAG "C"
247100990118     C* SE MI HANNO CHIESTO LE CA APERTE VISUALIZZO IL FLAG "*"
247200990118     C                   SELECT
247300990118     C     V1CDAN        WHENEQ    'C'
247400990118     C                   MOVEL     'C'           VSCDAN
247500990118     C     V1CDAN        WHENEQ    'A'
247600990118     C                   MOVEL     '*'           VSCDAN
247700990118     C**
247800990118     C                   OTHER
247900040810     C**""*KBLP          CHAIN     FNDCT02L                           31
248000040810     C**""**IN31         DOWEQ     *OFF
248100990118     C* SE NE TROVO UNA APERTA, DICO APERTA ALTRIMENTI CHIUSA
248200040810     C**""*DCTDCH        IFEQ      0
248300040810     C**""*DCTATB        ANDEQ     ' '
248400040810     C**""*              MOVEL     '*'           VSCDAN
248500040810     C**""*              SETON                                            31
248600040810     C**""*              ELSE
248700990118     C* CHIUSA MA VADO AVANTI A LEGGERE
248800040810     C**""*DCTATB        IFEQ      ' '
248900040810     C**""*              MOVEL     'C'           VSCDAN
249000040810     C**""*              ENDIF
249100040810     C**""*KBLP          READE     FNDCT02L                               31
249200040810     C**""*              ENDIF
249300040810     C**""*              ENDDO
249400040810      *
249500040810      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
249600040810      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
249700040810      * legate alla spedizione che passo
249800040810     c                   clear                   fidn12ds
249900040810     c                   eval      i12aas = BLPAAS
250000040810     c                   eval      i12lnp = BLPLNP
250100040810     c                   eval      i12nrs = BLPNRS
250200040810     c                   eval      i12nsp = BLPNSP
250300040810     c                   eval      i12fel = 'E'
250400040810     c                   eval      i12fan = 'E'
250500040810     c*
250600040810     c                   call      'FIDN12R'
250700040810     c                   parm                    fidn12ds
250800040810     c*
250900040810     c                   setoff                                       31
251000040810     c*
251100040810     c* se non ci sono errori
251200040810     c                   if        o12err <> ' '
251300040810     c                   seton                                        31
251400040810     c                   else
251500040810     c* se numero di CA trovate maggiore di zero
251600040810     c                   if        o12nca = 0
251700040810     c                   seton                                        31
251800040810     c                   else
251900040810     c                   z-add     1             jj                2 0
252000040810     c                   dow       jj <= o12nca and *in31 = *off
252100040810     C* SE NE TROVO UNA APERTA, DICO APERTA ALTRIMENTI CHIUSA
252200040810     C     skDch(jj)     IFEQ      0
252300040810     C                   MOVEL     '*'           VSCDAN
252400040810     C                   SETON                                            31
252500040810     C                   ELSE
252600040810     C* CHIUSA MA VADO AVANTI A LEGGERE
252700040810     C                   MOVEL     'C'           VSCDAN
252800040810     C                   ENDIF
252900040810     c*
253000040810     c                   add       1             jj
253100040810     c                   enddo
253200040810     c*
253300040810     c                   seton                                        31
253400040810     c*
253500040810     c                   endif
253600040810     c                   endif
253700990118     C**
253800990118     C                   ENDSL
253900900522     C* SPEDIZIONE PARTITA
254000911108     C     BLPFT2        IFNE      ' '
254100940728     C                   MOVE      'P'           VSCPAR
254200990118     C                   ELSE
254300990118     C* SPEDIZINE SOLO BORDERIZZATA
254400990118     C     BLPDBR        IFGT      0
254500990118     C                   MOVE      'B'           VSCPAR
254600900522     C                   END
254700990118     C                   END
254800041028     c
254900041028     c* Chain fnarb per prendere alcuni campi da quiù
255000041028     c                   clear                   wesarb            1
255100041028     C     KBLP          CHAIN     fnarb01l
255200041028     c                   if        %found(fnarb01l)
255300041028     c                   eval      wesarb='S'
255400041028     c                   else
255500041028     c                   eval      wesarb='N'
255600041028     c                   endif
255700930407     C*
255800930407     C* SE BLPCCA >< 0  METTO DECODIFICA AL POSTO DELLA DATA CONSEGNA
255900041028     c                   if        wesarb='S'
256000041028     c                   eval      wcca=arbcca
256100041028     c                   eval      wdcm=arbdcm
256200041028     c                   eval      wdce=arbdce
256300041028     c                   eval      wnrc=arbnrc
256400041028     c                   eval      wdcp=arbdcp
256500041028     c                   else
256600041028     c                   eval      wcca=blpcca
256700041028     c                   eval      wdcm=blpdcm
256800041028     c                   eval      wdce=blpdce
256900041028     c                   eval      wnrc=blpnrc
257000041028     c                   eval      wdcp=blpdcp
257100041028     c                   endif
257200041028     c
257300940912     C                   MOVEL     *BLANKS       COMDCM
257400041028    1C     wcca          IFNE      ' '
257500940727     C                   MOVEL     '7O'          KCOD
257600940727     C                   MOVEL     *BLANKS       KKEY
257700041028     C                   MOVEL     wcca          KKEY
257800940727     C     KTAB2         CHAIN     TABEL00F                           31
257900940909     C  N31              MOVEL     TBLUNI        DS7O
258000930407     C*
258100940909     C* NELLA DS7O C'E' SCRITTO SE E' PRIORITARIA L'ANOMALIA O LA DATA
258200941011    2C  N31§7OVCP        IFEQ      'S'
258300940912     C     §7OVCP        ORNE      'S'
258400041028     C     wdcm          ANDEQ     *ZERO
258500940909     C                   MOVEL     §7ODEI        COMDCM            4
258600940909     C                   SETON                                        27
258700941011   X2C                   ELSE
258800940909     C*
258900041028    3C     wdcm          IFNE      *ZERO
259000041028     C                   MOVE      wdcm          MMGG
259100940909     C                   MOVEL     GG            COMDCM
259200940909     C                   MOVE      MM            COMDCM
259300941011    3C                   ENDIF
259400941011    2C                   ENDIF
259500940909     C*
259600941011   X1C                   ELSE
259700041028    2C     wdcm          IFNE      *ZERO
259800041028     C                   MOVE      wdcm          MMGG
259900930407     C                   MOVEL     GG            COMDCM
260000930407     C                   MOVE      MM            COMDCM
260100941011    2C                   ENDIF
260200941011    1C                   ENDIF
260300940912     C*
260400940912     C* DATA CONSEGNA
260500960207     C                   CLEAR                   VSCDCM
260600941011     C                   MOVEL     COMDCM        VSCDCM
260700940912     C                   MOVEL     *BLANKS       COMFCC            1
260800041028    1C     wdcm          IFNE      0
260900930506     C*
261000041028     C* SE    DCE = 0  CALL PGM PER CALCOLO DELLA DATA CONSEGNA PREVISTA
261100930506     C*   E DELLE ORE DI RITARDO
261200041028    2C     wdce          IFEQ      0
261300930506     C                   MOVEL     ' '           D98TLA
261400930506     C                   MOVEL     'P'           D98TBO
261500930506     C                   MOVEL     BLPAAS        D98AAS
261600930506     C                   MOVEL     BLPLNP        D98LNP
261700930506     C                   MOVEL     BLPNRS        D98NRS
261800930506     C                   MOVEL     BLPNSP        D98NSP
261900930506     C*
262000970925     C                   CALL      'TNSD99R'
262100970925     C                   PARM                    DS99
262200930506     C*
262300930701     C* SE SE98 = 1  E' STATA FATTA CALL PGM
262400941214     C                   MOVEL     '1'           PA0S98
262500930506     C*
262600930506     C* SE D98CRC = S  LA SPEDIZ. E' STAT CONSEGNATA NEI TEMPI PREVISTI
262700110221     C     D98ERR        IFNE      '0'
262800960201     C                   MOVEL     'R'           COMFCC
262900960201     C                   ELSE
263000941011    3C     D98CRC        IFEQ      'S'
263100930506     C                   MOVEL     ' '           COMFCC
263200930506     C                   ELSE
263300930506     C                   MOVEL     'F'           COMFCC
263400941011    3C                   END
263500960201    3C                   END
263600930506     C*
263700941011  X2 C                   ELSE
263800041028     C*    DCE > 0
263900940729     C* SE BLPNRC > 0  LA SPEDIZIONE NON E' CONFORME
264000041028   3 C     wnrc          IFGT      0
264100930505     C                   MOVEL     'F'           COMFCC
264200930407     C                   ELSE
264300930505     C                   MOVEL     ' '           COMFCC
264400941011   3 C                   END
264500941011   2 C                   END
264600930407     C*
264700940728     C                   MOVE      COMFCC        VSCDCM
264800930407     C*
264900941011   X1C                   ELSE
265000930407     C* SE DATA CONSEGNA = 0  E DATA CONS. PARZIALE >< 0  MUOVO PARZ
265100041028    2C     wdcp          IFNE      0
265200940728     C                   MOVEL     'PARZ'        VSCDCM
265300930607     C                   SETON                                        27
265400941011    2C                   ENDIF
265500941011    1C                   ENDIF
265600941011     C*
265700940928     C                   SETOFF                                       29
265800930407     C*
265900900518     C                   ADD       1             B
266000940727     C                   ADD       1             NRR2              4 0
266100940727     C                   WRITE     LS04S02
266200900515     C                   ENDSR
266300950114      **************************************************************************
266400950114     C* RIEMPIMENTO DEL SFL
266500950114      **************************************************************************
266600950114     C     RIESFL        BEGSR
266700950114     C  N01              MOVEL     BLPLNA        VSCLIN
266800950114     C*
266900950114     C                   MOVEL     BLPRSM        VSCRSM
267000950114     C                   MOVEL     BLPRSD        VSCRSD
267100950114     C                   Z-ADD     BLPRMN        VSCRMN
267200950114     C                   MOVEL     BLPRMA        VSCRMA
267300950114     C                   MOVEL     BLPFLS        VSCFLS
267400950114     C                   MOVEL     BLPNRS        VSCNRS
267500060203     C                   MOVEL     BLPksc        VSCksc
267600950114     C* SEGNACOLLI
267700950114     C                   CLEAR                   SEG
267800950114     C                   MOVEL     BLPNCD        COMSCN            7
267900950114     C                   MOVEA     COMSCN        SEG(1)
268000950114     C     BLPNCA        IFNE      0
268100950114     C                   MOVEL     BLPNCA        COMSCN
268200950114     C                   MOVEA     COMSCN        SEG(9)
268300950114     C                   END
268400950114     C                   MOVEA     '/'           SEG(8)
268500950114     C* ELIMINO ZERO NON SIGNIFICATIVI
268600950114     C                   Z-ADD     1             X                 3 0
268700950114     C     SEG(X)        DOWEQ     '0'
268800950114     C                   MOVE      ' '           SEG(X)
268900950114     C                   ADD       1             X
269000950114     C                   END
269100950114     C*
269200950114     C     BLPNCA        IFNE      0
269300950114     C                   Z-ADD     9             X
269400950114     C     SEG(X)        DOWEQ     '0'
269500950114     C                   MOVE      ' '           SEG(X)
269600950114     C                   ADD       1             X
269700950114     C                   END
269800950114     C                   END
269900950114     C                   MOVEA     SEG           VSCSEG
270000950114     C* TIPO SERVIZIO
270100950114     C                   MOVEL     BLPTSP        VSCTSP
270200950114     C                   ENDSR
270300940324      **************************************************************************
270400940324      * RICHIAMA PGM VISUALIZZAZIONE BOLLA DA SFL
270500940324      **************************************************************************
270600940324     C     CALBOL        BEGSR
270700940729     C*
270800941214     C   20              MOVEL     *BLANKS       PA0FLG
270900941214     C  N20              MOVEL     'A'           PA0FLG
271000941214     C                   MOVEL     VSCAAS        PA0AAS
271100941214     C                   MOVEL     VSCLNP        PA0LNP
271200941214     C                   MOVEL     VSCNRS        PA0NRS
271300941214     C                   MOVEL     VSCNSP        PA0NSP
271400111020     C**!!!              MOVE      PA3CLI        PA0CLI
271500941214     C                   MOVEL     V1CRSU        PA0RSU
271600940324     C*
271700941214     C                   MOVEL     PARAM0        KPJBU
271800040220if  1c                   if        SDSprm = 1
271900940729     C                   CALL      'FNLS05R'
272000940324     C                   PARM                    KPJBA
272100040220x   1c                   else
272200040220     C                   call      'FNLS05R'
272300040220     C                   parm                    KPJBA
272400040220     C                   parm                    FIDNA1ds
272500040220e   1c                   endif
272600941214     C                   MOVEL     KPJBU         PARAM0
272700040220      * Se selezionata la bolla da FNLS05R per inserirla in reclami
272800040220      *  esco direttamente da questo pgm. (la bolla sarà già stata
272900040220      *  impostata nella ds FIDNA1DS da FNLS05R).
273000040220if  1c                   if            SDSprm > 1
273100040220     c                             and IA1ins = 'S'
273200040220     c                   goto      FINE
273300040220e   1c                   endif
273400940729     C*
273500940324     C                   ENDSR
273600000919      **************************************************************************
273700000919      * RICHIAMA PGM MANUTENZIONE SINGOLA BOLLA
273800000919      **************************************************************************
273900000919    1C     CALBO2        BEGSR
274000000919     C*
274100000919     C* NON PARTITA
274200000919    4C     VSCFT1        IFEQ      *BLANKS
274300000919     C                   MOVEL     VSCAAS        §LPAAS
274400000919     C                   MOVEL     VSCLNP        §LPLNP
274500000919     C                   MOVEL     VSCNRS        §LPNRS
274600000919     C                   MOVEL     VSCNSP        §LPNSP
274700000919     C                   MOVEL     'M'           §LPFPR
274800030612     C*                  MOVEL     PARAM4        KPJBU
274900000919     C                   CALL      'FNLS01R'
275000000919     C                   PARM                    KPJBA
275100000919     C                   PARM                    DBLP
275200000919     C                   PARM                    DTASV
275300030612     C                   PARM                    trul90ds
275400000919     C*
275500000919     C                   MOVEL     '1'           PA1S01
275600161202     c* uscita automatica dopo 50 secondi
275700161202     C     §LPFPR        IFEQ      '6'
275800161202     C                   SETON                                        28
275900161202     C                   MOVEL     msg(32)       V1CMSG
276000161202     C                   MOVEL     '1'           PA0F03
276100161202     C                   ENDIF
276200000919     C*
276300000919   X4C                   ELSE
276400000919     C*
276500000919     C* PARTITA
276600000919     C* VEDO SE E' MANUTENZIONABILE
276700060202     C**   simtpp        IFEQ      '2'
276800060202     C**   simtpp        oreq      *blanks
276900060202     C**   VSCLNP        ifne      SAVMUS
277000060202     C**                 MOVEL     D48MSG        V1CMSG
277100060202     C**                 MOVEL     '1'           PA0F03
277200060202     C**                 SETON                                        28
277300060202     c**                 endif
277400060202     C**                 ENDIF
277500060203     c
277600060203     c* controllo la skiera pog per lnp o p.o. utente se previsto
277700060203     c                   movel     vsclnp        w003a
277800060203     c     w003a         lookup    $pog                                   31
277900060203     c                   if        not *in31 and d66pocl='S'
278000060203     c                   movel     vscksc        w003a
278100060203     c     w003a         lookup    $pog                                   31
278200060203     c                   endif
278300060203    5c                   if        not *in31
278400060203     C                   MOVEL     msg(28)       V1CMSG
278500060203     C                   MOVEL     '1'           PA0F03
278600060203     C                   SETON                                        28
278700060203     c                   else
278800000919     C*
278900000919     C                   CLEAR                   DSLR48
279000000919     C                   MOVEL     VSCAAS        D48AAS
279100000919     C                   MOVEL     VSCLNP        D48LNP
279200000919     C                   MOVEL     VSCLNP        D48FGS
279300000919     C                   MOVEL     VSCNRS        D48NRS
279400000919     C                   MOVEL     VSCNSP        D48NSP
279500000919     C                   MOVEL     VSCCBO        D48CBO
279600000919     C                   MOVEL     'P'           D48TBO
279700000919     C                   MOVEL     V1CCVB        D48CVB
279800000919     C                   MOVEL     'S'           D48F12
279900141030     c
280000141030     c* routine per verifica dispo da esaminare
280100141030     c                   exsr      sr_wdwdc
280200141030
280300000919     C                   MOVEL     DSLR48        KPJBU
280400000919     C                   CALL      'FNLR48R'
280500000919     C                   PARM                    KPJBA
280600000919     C                   PARM                    DSARBD
280700000919     C                   PARM                    DSARBK
280800000919     C                   PARM                    DARBT
280900000919     C                   PARM                    DSARBG
281000030612     C                   PARM                    trul90ds
281100000919     C*
281200000919     C                   MOVEL     KPJBU         DSLR48
281300000919     C* ERRORE
281400000919     C     D48ERR        IFNE      *BLANKS
281500000919     C                   MOVEL     D48MSG        V1CMSG
281600000919     C                   MOVEL     '1'           PA0F03
281700000919     C                   SETON                                        28
281800060203     c                   else
281900060203     C* SE NON C'E' STATO ERRORE RICHANO LA BOLLA PER PRENDERE IL MITT
282000060203     C* E IL DESTINATARIO CHE POSSON ESSERE MODIFICATI
282100060203     C     KBLP3         CHAIN     FNBLP01L                           31
282200060203     C  N31              EXSR      RIESFL
282300000919    4C                   ENDIF
282400000919    4C                   ENDIF
282500060203    4C                   ENDIF
282600000919     C*
282700000919    1C                   ENDSR
282800940727      **************************************************************************
282900940727      * Nel caso di interrogazione da parte di un cliente, viene verificato se
283000940727      * egli puo' interrogare le bolle e se deve essere conteggiato il traffico
283100940727      * dei byte sulla linea di comunicazione.
283200940727      * Queste informazioni sono contenute nella tabella 3K.
283300940727      *
283400940727      * Inoltre, egli puo' interrogare solo alcuni clienti (quelli a lui
283500940727      * unificati.
283600940727      **************************************************************************
283700111020     C**!!!ABICLI        BEGSR
283800940727      *
283900111020     C**!!!*LIKE         DEFINE    §3KINB        $3KINB
284000090603     C***  *LIKE         DEFINE    §3KCTB        $3KCTB
284100111020     C**!!!              MOVE      *BLANKS       $3KINB
284200090603     C***                MOVE      *BLANKS       $3KCTB
284300940727      *
284400940727      * Leggo tutta la tabella 3K riempendo una schiera con i cod.clienti
284500111020     C**!!!              Z-ADD     *ZERO         $PRIMA            1 0
284600111020     C**!!!              Z-ADD     *ZERO         $A                3 0
284700111020     C**!!!              MOVE      '3K'          KCOD
284800111020     C**!!!KTAB1         SETLL     TABEL00F
284900111020     C**!!!              DO        *HIVAL
285000111020     C**!!!KTAB1         READE     TABEL00F                               90
285100111020     C*!N90TBLFLG        IFEQ      *BLANKS
285200111020     C**!!!              MOVEL     TBLUNI        DS3K
285300111020     C**!!!KNMUS         IFEQ      §3KPRF
285400940727      *
285500940727      * Devo prendere il cod.cliente, che e' la chiave di TABEL
285600111020     C**!!!              MOVEL     TBLKEY        $CODIC            7 0
285700111020     C**!!!$CODIC        LOOKUP    CLI                                    99
285800111020     C*!N99$A            IFLT      300
285900111020     C**!!!              ADD       1             $A
286000111020     C**!!!              Z-ADD     $CODIC        CLI($A)
286100111020     C**!!!              ENDIF
286200940727      *
286300940727      * Gia' dalla prima lettura ho i due flag che abilitano il profilo
286400111020     C**!!!$PRIMA        IFEQ      *ZERO
286500111020     C**!!!              MOVE      §3KINB        $3KINB
286600090603     C***                MOVE      §3KCTB        $3KCTB
286700111020     C**!!!§3KINB        COMP      'S'                                9090
286800111020     C**!!!              Z-ADD     1             $PRIMA
286900111020     C**!!!              ENDIF
287000940727      *
287100111020     C**!!!              ENDIF
287200111020     C**!!!              ENDIF
287300111020     C*!N90              ENDDO
287400940727      *
287500111020     C**!!!              ENDSR
287600991214     C**************************************************************************
287700991214      ** CALCOLA CHECK DIGIT
287800991214     C**************************************************************************
287900991214     C     M1031         BEGSR
288000991214      *
288100991214     C                   MOVE      *ZERO         §TT2              3 0
288200991214     C                   CLEAR                   W0030
288300991214      *
288400991214      * TOTALIZZO I VALORI, QUELLI NELLE POSIZIONI DISPARI PER 3
288500000328     C                   DO        11            YY                2 0
288600991214      *
288700000328     C     YY            DIV       2             §CTR              3 0
288800991214     C                   MVR                     §RST              3 030
288900991214      *
289000991214     C     *IN30         IFEQ      '1'
289100000328     C     §11(YY)       MULT      3             §CTR
289200991214     C                   ELSE
289300000328     C                   Z-ADD     §11(YY)       §CTR
289400991214     C                   ENDIF
289500991214      *
289600991214     C                   ADD       §CTR          W0030             3 0
289700991214     C                   ENDDO
289800991214      *
289900991214     C     W0030         ADD       9             §TT2
290000991214     C                   MOVE      0             §TT2
290100991214     C     §TT2          SUB       W0030         DPDCHD
290200991214      *
290300991214     C                   ENDSR
290400141030      /end-free
290500141030       //--------------------------------------------------------------
290600141030       //?Window per f19=Dispo di consegna da esaminare
290700141030       //--------------------------------------------------------------
290800141030       BEGSR sr_wdwdc   ;
290900141030          $fine=*off;
291000141030          clear fnlry09ds2;
291100141030          ilry09tch='C';
291200141030          ILRY09AAS=d48aas ;
291300141030          ILRY09LNP=d48LNP;
291400141030          ILRY09NRS=d48NRS ;
291500141030          ILRY09NSP=d48NSP ;
291600141030          kpjbu=fnlry09ds2;
291700141030          fnlry09r(kpjba);
291800141030          fnlry09ds2=kpjbu;
291900141030          if olry09esit='1' ;
292000141030             dow $fine=*off;
292100141030                exfmt ls04wddc;
292200141030                   if *inkt;
292300141030                       clear fnlry09ds2;
292400141030                       ilry09tch='E';
292500141030                       ILRY09AAS=d48aas ;
292600141030                       ILRY09LNP=d48LNP;
292700141030                       ILRY09NRS=d48NRS ;
292800141030                       ILRY09NSP=d48NSP ;
292900141030                       ILRY09DF12='S';
293000141030                       ILRY09DF21='N';
293100141030                       ILRY09DF06='N';
293200141030                       ILRY09EF12='S';
293300141030                       ILRY09EF21='N';
293400141030                       ILRY09EF06='N';
293500141030                       kpjbu=fnlry09ds2;
293600141030                       fnlry09r(kpjba);
293700141030                       $fine=*on;
293800141030                   endif;
293900141030             enddo;
294000141030          endif ;
294100141030
294200141030       ENDSR;
294300141030
294400141030
294500141030
294600141030      /end-free
294700950206     C**************************************************************************
294800950206     C*   CARICAMENTO TABELLE INZIALE
294900950206     C**************************************************************************
295000950206     C     CARTAB        BEGSR
295100950206     C*
295200950206     C* RIEMPO UNA SCHIERA CON TUTTI I TIPI BOLLA DI RECUPERO
295300950206     C                   MOVEL     '3A'          KCOD
295400950206     C                   Z-ADD     *ZERO         WA                3 0
295500950206     C     KTAB1         SETLL     TABEL00F
295600950206     C     KTAB1         READE     TABEL00F                               31
295700950206     C     *IN31         DOWEQ     *OFF
295800950206     C                   MOVEL     TBLUNI        DS3A
295900950206     C     §3ARBL        IFEQ      'R'
296000950206     C                   ADD       1             WA
296100950206     C                   MOVEL     TBLKEY        REC(WA)
296200950206     C                   ENDIF
296300950206     C     KTAB1         READE     TABEL00F                               31
296400950206     C                   ENDDO
296500950206     C*
296600951010     C****  CARICO TABELLA FILIALI GESTITE £1  ****
296700951010     C                   CLEAR                   DSUL06
296800951010     C                   MOVE      '£1'          D06COD
296900951010     C                   MOVEL     SIMFEL        D06KEY
297000951010     C                   MOVEL     DSUL06        KPJBU
297100951010     C                   CALL      'TRUL06R'
297200951010     C                   PARM                    KPJBA
297300951010     C                   MOVEL     KPJBU         DSUL06
297400951010     C                   MOVEA     LIN           L1
297500030527     C*
297600950206     C                   ENDSR
297700940727     C**************************************************************************
297800940727     C*    R O U T I N E     I N I Z I A L E
297900940727     C**************************************************************************
298000940727     C     *INZSR        BEGSR
298100940727     C*
298200051013     C     KarS          KLIST
298300000831     C                   KFLD                    V1CNOT
298400000831     C                   KFLD                    KKTRC             1
298500000911     C                   MOVE      'C'           KKTRC
298600051013     C     KarSXT        KLIST
298700051013     C                   KFLD                    arSFLS
298800051013     C                   KFLD                    arSLNA
298900051013     C                   KFLD                    arSNRS
299000051013     C                   KFLD                    arSNSC
299100000831     C     KBLTXP        KLIST
299200000831     C                   KFLD                    BLTAAS
299300000831     C                   KFLD                    BLTLNP
299400000831     C                   KFLD                    BLTNRS
299500000831     C                   KFLD                    BLTNSP
299600940727     C     KLNA1         KLIST
299700940728     C                   KFLD                    V1CLNA
299800940727     C                   KFLD                    COMAAD
299900940727     C                   KFLD                    COMMGD
300000940727     C                   KFLD                    TSP               1
300100940928     C     KLNA2         KLIST
300200940928     C                   KFLD                    V1CLNA
300300940928     C                   KFLD                    COMAAD
300400940928     C                   KFLD                    COMMGD
300500020627     c     klna3         klist
300600020627     c                   kfld                    klp1
300700020627     c                   kfld                    v1clna
300800020627     c                   kfld                    comaad
300900020627     c                   kfld                    commgd
301000020627     c                   kfld                    tsp
301100020627     c     klna4         klist
301200020627     c                   kfld                    klp1
301300020627     c                   kfld                    v1clna
301400940727     C     KLNP          KLIST
301500940728     C                   KFLD                    V1CAAS
301600940728     C                   KFLD                    V1CLP2
301700940728     C                   KFLD                    V1CNRS
301800940728     C                   KFLD                    V1CNSP
301900940727     C     KLNP2         KLIST
302000940728     C                   KFLD                    V1CLP1
302100940727     C                   KFLD                    COMAAD
302200940727     C                   KFLD                    COMMGD
302300940728     C                   KFLD                    V1CPSD
302400940727     C     KBLP          KLIST
302500940727     C                   KFLD                    BLPAAS
302600940727     C                   KFLD                    BLPLNP
302700940727     C                   KFLD                    BLPNRS
302800940727     C                   KFLD                    BLPNSP
302900940727     C     KBLP2         KLIST
303000940727     C                   KFLD                    BLPAAS
303100940727     C                   KFLD                    BLPLNP
303200940727     C                   KFLD                    BLPNRS
303300940727     C                   KFLD                    BLPNSP
303400940728     C                   KFLD                    V1CSCD
303500950114     C     KBLP3         KLIST
303600950114     C                   KFLD                    VSCAAS
303700950114     C                   KFLD                    VSCLNP
303800950114     C                   KFLD                    VSCNRS
303900950114     C                   KFLD                    VSCNSP
304000951011     C     KBLP4         KLIST
304100060217     C                   KFLD                    ar4AAS
304200060217     C                   KFLD                    ar4LNP
304300060217     C                   KFLD                    ar4NRS
304400060217     C                   KFLD                    ar4NSP
304500940727     C     KTAB2         KLIST
304600940727     C                   KFLD                    CODUT
304700940727     C                   KFLD                    KCOD
304800940727     C                   KFLD                    KKEY
304900940727     C     KTAB1         KLIST
305000940727     C                   KFLD                    CODUT
305100940727     C                   KFLD                    KCOD
305200940727     C     CNACOK        KLIST
305300940727     C                   KFLD                    CODUT
305400940727     C                   KFLD                    ACOKCC
305500940727     C                   KFLD                    ACOKSC
305600940729     C     BLP33K        KLIST
305700991214     C                   KFLD                    KRMN
305800940727     C                   KFLD                    COMAAD
305900940727     C                   KFLD                    COMMGD
306000101126     C     BLP32K        KLIST
306100101126     C                   KFLD                    v1crma
306200101126     C                   KFLD                    COMAAD
306300101126     C                   KFLD                    COMMGD
306400940729     C     BLP43K        KLIST
306500021111     C                   KFLD                    Klp1
306600940727     C                   KFLD                    $AAS
306700940727     C                   KFLD                    $MGS
306800940728     C                   KFLD                    V1CRSD
306900940729     C     BL43K         KLIST
307000021111     C                   KFLD                    Klp1
307100940727     C                   KFLD                    $AAS
307200940727     C                   KFLD                    $MGS
307300940729     C     BLP44K        KLIST
307400021111     C                   KFLD                    Klp1
307500940727     C                   KFLD                    $AAS
307600940727     C                   KFLD                    $MGS
307700940728     C                   KFLD                    V1CRSM
307800940729     C     BL44K         KLIST
307900021111     C                   KFLD                    Klp1
308000940727     C                   KFLD                    $AAS
308100940727     C                   KFLD                    $MGS
308200060217     C     Kar4          KLIST
308300951011     C                   KFLD                    WTRC
308400971212     C                   KFLD                    WN30
308500010219      *
308600060217     C     Kar43         KLIST
308700010219     C                   KFLD                    WTRC
308800010219     C                   KFLD                    WN14             14
308900010219      *
309000010219     C     KFORM         KLIST
309100010219     C                   KFLD                    V1CPOE
309200010219     C                   KFLD                    V1CNSR
309300010219     C                   KFLD                    V1CNOR
309400010219     C                   KFLD                    V1CNRV
309500030527     C*
309600030527     C* Chiave su TNTBE01L - Parziale
309700030527     C     KTBE01P       KLIST
309800030527     C                   KFLD                    TBECOD
309900030527     C                   KFLD                    TBEKE1
310000940727     C*
310100991214     C     *LIKE         DEFINE    V1CRMN        KRMN
310200991214     C     *LIKE         DEFINE    TBLCOD        KCOD
310300940727     C     *LIKE         DEFINE    TBLKEY        KKEY
310400940728     C     *LIKE         DEFINE    TBLKUT        §KUT
310500940728     C     *LIKE         DEFINE    TBLCOD        §COD
310600940728     C     *LIKE         DEFINE    TBLKEY        §KEY
310700940729     C     *LIKE         DEFINE    BLPAAS        $AAS
310800940729     C     *LIKE         DEFINE    BLPMGS        $MGS
310900961118     C     *LIKE         DEFINE    BLPFBR        WFBR
311000950216     C     *LIKE         DEFINE    BLPDSP        WDSP
311100060217     C     *LIKE         DEFINE    ar4TRC        WTRC
311200940727     C*
311300951011     C* IMPOSTAZIONE CAMPI FISSI
311400951011     C                   MOVEL     'E'           WTRC
311500060202     c
311600060202     c                   movel     kpjbu         savkpjbu
311700060202      *
311800060202     c     *dtaara       define    §azute        azuteds
311900060202     c     *dtaara       define    §datiute      ddatiute
312000060202     c                   in(E)     *dtaara
312100060202    1c                   If        %error  or RSUT = *blanks
312200060202     c                   Clear                   Tibs34ds
312300060202     c                   Call      'TIBS34R'
312400060202     c                   Parm                    Tibs34ds
312500060202     c                   In        *dtaara
312600060202    1c                   EndIf
312700060202
312800060202     c                   Clear                   wabi
312900060202     c                   Clear                   v1cmsg
313000060202     c                   Clear                   dLat
313100060202
313200060202      * Verifica errori e autorità profilo
313300060202s   1c                   Select
313400060202      * se ho errori nei dati utente esco dal pgm
313500060202w   1c                   When      DutErr = 'E'
313600060202     c                   GoTo      Fine
313700060202      * se non c'è l'abilitazione
313800060202      * --> se 1° livello, abilitazioni al terminal
313900060202      *     se 2° livello, abilitazioni al punto operativo
314000060202      *     se sede è impossibile ma se capita mando a fine il pgm
314100060202w   1c                   When      UteAut = *Blanks
314200060202if  2c                   If        DutLpo = '1'
314300060202     c                   Eval      wabi   = 'TP'
314400060202e   2c                   EndIf
314500060202if  2c                   If        DutLpo = '2'
314600060202     c                   Eval      wabi   = 'PO'
314700060202e   2c                   EndIf
314800060202if  2c                   If        DutLpo = 'S'
314900060202     c                   GoTo      Fine
315000060202e   2c                   EndIf
315100060202      * carica le abilitazioni del profilo
315200060202x   1c                   Other
315300060202     c                   Movel     UteFaf        Dute01
315400060202     c                   If        §Utembf <> *Blanks
315500060202     c                   Eval      wabi = §Utembf
315600060202     c                   Else
315700060202     c                   Eval      wabi = UteAut
315800060202     c                   EndIf
315900060202e   1c                   EndSl
316000060202
316100060202      * controllo se ok l'abilitazione dell'utente
316200060202     c                   Clear                   Tibs02ds
316300060202     c                   Eval      T02tla = 'L'
316400060202     c                   Eval      T02Mod = 'C'
316500060202     c                   Eval      T02Sif = knsif
316600060202     c                   Eval      T02Cod = 'LAT'
316700060202     c                   Movel(p)  wabi          T02Ke1
316800060202     c                   Call      'TIBS02R'
316900060202     c                   Parm                    kpjba
317000060202     c                   Parm                    Tibs02ds
317100060202if  1c                   If        T02Err = *Blanks
317200060202     c                   Eval      dLat = T02Uni
317300060202e   1c                   EndIf
317400060202      * errore o non abilitato
317500060202if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
317600060202     c                   Seton                                        28
317700060202     C                   movel     msg(29)       v1cmsg
317800060202   x1c                   Else
317900060202      * Reperimento dei P.O. gestibili dall'utente
318000060202     c                   clear                   TRUL31DS
318100060202     c                   clear                   TRUL31DS2
318200060202     c                   eval      I31abi = wabi
318300060202     c                   eval      I31cdi = DUTdis
318400060202     c                   eval      I31car = DUTare
318500060202     c                   eval      I31cpo = DUTpou
318600060202     c                   call      'TRUL31R'
318700060202     c                   parm                    KPJBA
318800060202     c                   parm                    TRUL31DS
318900060202     c                   parm                    TRUL31DS2
319000060202if  2c                   if        O31pog > *zeros
319100060202     c                   movea     O31pog        $pog
319200060202     c                   movea     O31arg        $arg
319300060202     c                   movea     O31dig        $dig
319400060202x   2c                   else
319500060202     c                   Seton                                        28
319600060202     c                   movel     MSG(29)       v1cmsg
319700060202e   2c                   endif
319800060202    1c                   endif
319900060202     c
320000060202     c                   movel     savkpjbu      kpjbu
320100951011     C*
320200940727     C                   ENDSR
320300950206     C**************************************************************************
320400950111     OFNLV22P   T    LRNLR
320500950206     O**************************************************************************
320600030612** msg
320700940728Nessuna bolla da visualizzare                                                  1
320800940728Immettere linea di partenza valida                                             2
320900940728Immettere numero spedizione esistente in archivio                              3
321000040216Immettere tipo bolla valido                                                    4
321100941010Immettere linea di arrivo valida                                               5
321200160125Effettuare una selezione                                                       6
321300940728Immettere linea di partenza oppure linea di arrivo                             7
321400940728Immettere data formalmente valida                                              8
321500940728Immettere data spedizione DAL minore o uguale alla data AL                     9
321600940728Immettere numero serie DAL minore o uguale al numero serie AL                 10
321700940728Immettere segnacollo DAL minore o uguale al segnacollo AL                     11
321800940728Se immesso segnacollo AL immettere anche sgnacollo DAL                        12
321900940728Immettere codice cliente esistente e non annullato                            13
322000940728Immettere tipo servizio bolle valido                                          14
322100940728Immettere almeno una selezione                                                15
322200940728Fine scorrimento                                                              16
322300961125Immettere punto operativo segnacollo valido                                   17
322400101124Immettere o "CHI SONO" o rif.mittente Num o alfabetico o rif.partner partenza 18
322500040216* LIBERO * Non possibile la selezione per ricerca nell'archivio storico bolle 19
322600040216* LIBERO * BOLLA NON MANUTENZIONABILE: gia' nello Storico Bolle               20
322700961125BOLLA NON MANUTENZIONABILE: in gestione ad altro punto operativo              21
322800060608Lunghezza riferimento Mittente errata per bolle DPD
322900060609
323000060614Riferimento partner estero errato x bolle DPD:o lungo 11 numerico o 14 alfanum
323100010219Immettere p.o. di emissione valido                                            25
323200010219Immettere numero orm esistente in archivio                                    26
323300060202Utente non abilitato alla gestione di questa linea di partenza                27
323400060202Utente non abilitato alla manutenzione di questa bolla                        28
323500060202Utente non abilitato alla Variazione bolle                                    29
323600121105Immettere il Cod. CHI SONO per attivare la ricerca                            30
323700160125non trovata bolla per il barcde digitato                                      31
323800161202ATTENZIONE! Bolla NON Manutenzionata.Uscita automatica x 50 sec di inattività 32
323900030612** cm3
324000950111OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
324100991227 SHARE(*YES)          USRDTA('Segnacolli')
324200030612**  cmA4
324300030612OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
324400030612 SHARE(*YES)          USRDTA('Man.BolA4')
324500030612**  cmA5
324600030612OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
324700030612 SHARE(*YES)          USRDTA('Man.BolA5')
324800030612** cnm
324900991222 0123456789
