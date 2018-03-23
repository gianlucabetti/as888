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
001800030612     FFNBLP33L  IF   E           K DISK    RENAME(FNBLP000:FNBLP033)
001900030612     FFNBLP43L  IF   E           K DISK    RENAME(FNBLP000:FNBLP043)
002000030612     FFNBLP44L  IF   E           K DISK    RENAME(FNBLP000:FNBLP044)
002100040220      *.......... S E G N A C O L L I
002200090226     FFiarT01L  IF   E           K DISK
002300090226     FFiarT27L  IF   E           K DISK    RENAME(FiarT000:FiarT027)
002400051013     FFiarS02L  IF   E           K DISK
002500040220      *.......... E S T E N S I O N E   D E S C R I Z I O N I
002600060217     FFiar403L  IF   E           K DISK
002700060217     FFiar404L  IF   E           K DISK    RENAME(Fiar4000:Fiar404)
002800040220      *.......... D A N N I
002900040810     F**""*FNDCT02L  IF   E           K DISK
003000040220      *.......... B O L L E   I N   G I A C E N Z A
003100050405     Ftigcp01L  IF   E           K DISK
003200040220      *.......... O. R. M.
003300010219     FFNORM01L  IF   E           K DISK
003400040220      *.......... A N A G R A F I C O   P . D . C .
003500040220     FCNACO00F  IF   E           K DISK
003600040220      *.......... O R G A N I G R A M M A
003700040220     FAZORG01L  IF   E           K DISK
003800040220      *.......... V E C C H I E  T A B E L L E
003900040220     FTABEL00F  IF   E           K DISK
004000040220      *.......... N U O V E   T A B E L L E
004100030527     FTNTBE01L  IF   E           K DISK
004200040220      *
004300040220      *.......... S E G N A C O L L I   I N   S T A M P A
004400040220     FFNLV22P   O    F  132        PRINTER USROPN
004500040220      *--------------------------------------------------------------*
004600940727     D C1              S              1    DIM(15)
004700920206     D SEG             S              1    DIM(15)
004800940727     D CLI             S              7  0 DIM(300)
004900951010     D* SCHIERE PER CARICAMENTO L1
005000951010     D L1              S              3  0 DIM(30)
005100030527     D*
005200030527     D* SCHIERA PER CARICAMENTO P.O. POSTE
005300030527     D SKPOPT          S              3  0 DIM(10)
005400941011     D*
005500060202     D MSG             S             78    DIM(29) CTDATA PERRCD(1)
005600991206     D CM3             S              1    DIM(122) CTDATA PERRCD(61)
005700030612      * le schiere per OVRPRTF dei moduli LASER sono + lunghe
005800030612     D CMA4            S              1    DIM(130) CTDATA PERRCD(65)
005900030612     D CMA5            S              1    DIM(130) CTDATA PERRCD(65)
006000030612      *
006100991222     D CNM             S              1    DIM(11) CTDATA PERRCD(11)
006200950206     D REC             S              2    DIM(50)
006300000911      *  SCHIERA PER COMPORRE BARCODE SU LETTERA VETTURA DPD IMPORT
006400060609     D K11             S              1    DIM(11)
006500060202     d* p.o. abilitati
006600060202     D $pog            s              3    dim(250)
006700060202     D $DIG            s              1    dim(20)
006800060202     D $ARG            s              3    dim(50)
006900020306
007000060202     D savkpjbu        S                   LIKE(kpjbu)
007100020306     d lnantw          s                   like(§ogntw)
007200020306     d lnpntw          s                   like(§ogntw)
007300041028     d wcca            s                   like(blpcca)
007400041028     d wdcm            s                   like(blpdcm)
007500041028     d wdce            s                   like(blpdce)
007600041028     d wnrc            s                   like(blpnrc)
007700041028     d wdcp            s                   like(blpdcp)
007800020614     d klp1            s                   like(v1clp1)
007900060202     d wabi            s                   like(UteAut)
008000020614     d ww              s              2  0
008100030527     D*
008200030527     D* VARIABILI DI WRK X CONSIDERAZIONI SU BOLLE PT
008300030527     D wrkoggiiso      s               d
008400030527     D wDataLimitePT   s              8  0
008500020306
008600941011      **
008700940729      * RICHIAMO A INTERROGAZIONE BOLLA SINGOLA     - FNLS05R -
008800941214     D PARAM0          DS
008900941214     D  PA0AAS                 1      4  0
009000941214     D  PA0LNP                 5      7  0
009100941214     D  PA0NRS                 8      9  0
009200941214     D  PA0NSP                10     16  0
009300941214     D  PA0F03                17     17
009400941214     D  PA0FLG                18     18
009500941214     D  PA0FA9                19     19
009600941214     D  PA0FB5                20     20
009700941214     D  PA0F75                21     21
009800941214     D  PA0S98                22     22
009900941214     D  PA0RSU                23     42
010000941214     D  PA0CML                43     43
010100941214     D  PA0FEL                44     46
010200941214      * PA0CLI indica se il pgm e' richiamato da un cliente (='S')
010300010219     D  PA0CLI                47     47
010400941214      * PA0GIA indica se il pgm e' richiamato dalle giacenze
010500941214     D  PA0GIA                48     48
010600941011      **
010700940729      * RICHIAMO PGM DI INTERROGAZIONE BOLLE ARRIVI   - FNLR36R -
010800940729      * RICHIAMO PGM DI INTERROGAZIONE BOLLE TRANSITI - FNLR68R -
010900920203     D PARAM2          DS
011000940729     D  PA2FLG                31     31
011100940729     D  PA2BOL                62     62
011200940729      * PROGRAMMA RICHIAMATO: DISABILITO F7=INTERROGAZIONE BOLLE ARRIVI
011300920203     D PARAM3          DS
011400940809     D  PA3AAS                 1      4  0
011500940809     D  PA3LNP                 5      7  0
011600940809     D  PA3NRS                 8      9  0
011700940809     D  PA3NSP                10     16  0
011800940729     D  PA3FLG                31     31
011900940729      * PA3CLI indica se il pgm e' richiamato da un cliente (='S')
012000940729     D  PA3CLI               143    143
012100980603     D* PASSAGGIO DATI DA FILTRO PER CAMBIO AZIENDA - FNLV02R
012200980603     D  CALPGM               201    210
012300980603     D  PARAZI               211    211
012400980603     D  PARF19               212    212
012500980603     D  PARF3                213    213
012600941011      **
012700930701     D* DS PER CHIUSURA FILE PGM RICHIAMATI E CHIUSI CON RETRN
012800940729     D PARA75          DS
012900940729     D  P75FLG                31     31
013000941011      **
013100940729     D PARAB5          DS
013200941213     D  PB5AAS                14     17  0
013300941213     D  PB5LNP                18     20  0
013400941213     D  PB5NRS                21     22  0
013500941213     D  PB5NSP                23     29  0
013600940729     D  PB5FLG                31     31
013700941011      **
013800900516     D MMGG            DS
013900900516     D  MM                     1      2  0
014000900516     D  GG                     3      4  0
014100920206     D WLBDAT          DS
014200940727     D  G02DAT                 1      8  0
014300940727     D  G02INV                 9     16  0
014400940727     D  G02ERR                17     17
014500940727     D  G02TGI                18     22  0
014600901116     D                 DS
014700940728     D  V1CLNA                 1      3  0
014800940728     D  V1CLP1                 4      6  0
014900940729     D  V1CDSD                 7     14  0
015000940729     D  V1CDSA                15     22  0
015100940729     D  V1CRMN                23     37  0
015200951010     D  V1CRPP                38     52
015300951010     D  SEL1                   1     52
015400901116     D                 DS
015500940728     D  V1CLP2                 1      3  0
015600940728     D  V1CNRS                 4      5  0
015700940728     D  V1CNSP                 6     12  0
015800901116     D  SEL2                   1     12  0
015900940727     D                 DS
016000940728     D  V1CRSD                 1     15
016100940728     D  V1CRSM                16     30
016200940727     D  SEL3                   1     30
016300010216      *--
016400010216     D                 DS
016500010216     D  V1CPOE                 1      3  0
016600010216     D  V1CNSR                 4      5  0
016700010216     D  V1CNOR                 6     12  0
016800010216     D  V1CNRV                13     14  0
016900010216     D  SEL4                   1     14  0
017000010216      *--
017100900515     D                 DS
017200940729     D  BLPAAS                 1      4  0
017300940729     D  BLPMGS                 5      8  0
017400940729     D  BLPDSP                 1      8  0
017500910118     D                 DS
017600940728     D  VSCTSP                 1      1
017700940728     D  VSCGIA                 2      2
017800990118     D  VSCDAN                 3      3
017900990118     D  VSCPAR                 4      4
018000990118     D  VSCFLA                 1      4
018100961017     D                 DS
018200961017     D  V1CKLP                 1      3  0
018300961017     D  V1CKSC                 4      7  0
018400961017     D  V1CKSD                 1      7  0
018500991214     D**
018600991214      * CALCOLO DEL CHCK DIGIT PER BARCODE DPD
018700991214     D                 DS
018800030612     D  §11                    1     11  0 DIM(11)
018900991214     D  DPDCHD                12     12  0
019000991214     D  DPDBRC                 1     12  0
019100900517     D CA02DS          DS
019200900517     D  PRIMA                378    379B 0
019300040220      *?definizione oggetto reclamo come spedizione?
019400040220     d ds_OggSped      ds                  inz
019500040220     d  VSClnp                             inz
019600040220     d  VSCnrs                             inz
019700040220     d  VSCnsp                             inz
019800040220     d  VSCaas                             inz
019900040220      *
020000040220     d Status         sds
020100040220     d  SDSprm           *parms
020200040220      *
020300900514     D KPJBA         E DS
020400040220     d FIDNA1ds      e ds
020500040220      *
020600941222     D DSARBD        E DS
020700941222     D DSARBG        E DS
020800991012     D DARBT         E DS
020900941222     D DSARBK        E DS
021000060518     D*DSBL4F        E DS
021100060518     D DSBL4I        E DS
021200060202     d* Abilitazioni utente
021300060202     d Tibs02Ds      e ds
021400060202     d Azuteds       e ds                  extname(Azute00f)
021500060202     d dDatiute      e ds
021600060202     d dLat          e ds
021700060202     d dute01        e ds
021800060202     D TRUL31DS      E DS
021900060202     D TRUL31DS2     E DS
022000060202     D TIBS34DS      E DS
022100030527     D*
022200030527     D* DS X REPERIMENTO VARIABILI PGM OPERATIVI
022300030527     D DVPO          E DS
022400940316      *
022500940316      * DS ESTERNA PER RICHIAMO PGM CHE CALCOLA I TEMPI DI DELIVERY
022600970925     D DS99          E DS                  EXTNAME(TNSD99DS) INZ
022700941010      *
022800991020     D DBLP          E DS                  INZ
022900991020     D DTASV         E DS                  INZ
023000030612     D* DS PER TRUL90R - RICHIESTA STAMPANTI
023100030612     D TRUL90DS      E DS
023200941013     D* DS PER FNLR66R - CONTROLLO E VISUALIZZAZIONE CAUSALI VARIAZIONI
023300941013     D DSLR66        E DS                  EXTNAME(FNLR66DS)
023400941222     D* DS PER FNLR48R - MANUTENZIONE BOLLE
023500941222     D DSLR48        E DS                  EXTNAME(FNLR48DS)
023600951010     D*
023700951010     D* DS PER TRUL06R - CARICAMENTO £1
023800951010     D DSUL06        E DS                  EXTNAME(TRUL06DS)
023900030612     D  LIN                    1     90  0 DIM(30)
024000951010     D*
024100950206     D DS3A          E DS
024200940727     D DS3K          E DS
024300991214     D OG143         E DS
024400940909     D DS7O          E DS
024500040810
024600040810      * ds per il controllo della presenza di CA per la spedizione richiesta
024700040810     d FIDN12DS      e ds
024800040810     d  skKey                 26    305    dim(20)
024900040810     d  skAnn                306    325    dim(20)
025000040810     d  skDca                326    485  0 dim(20)
025100040810     d  skFca                486    545  0 dim(20)
025200040810     d  skDch                546    705  0 dim(20)
025300040810     d  skCch                706    745    dim(20)
025400040810     d*
025500040810     d dsKey           ds
025600040810     d  dsaac                         4  0
025700040810     d  dsfil                         3  0
025800040810     d  dsnca                         7  0
025900941010     D** COSTANTI
026000941010     D CINTER          C                   CONST('* INTERROGAZIONE BOL-
026100941010     D                                     LE PARTENZE *')
026200941010     D CMANUT          C                   CONST('** MANUTENZIONE BOLL-
026300941010     D                                     E PARTENZE **')
026400940316      *
026500940316      ****************************************************************
026600940316      *  RIEPILOGO INDICATORI
026700940316      ***************************************************************
026800940728      * 01    - IMMESSO CAMPO LINEA DI ARRIVO (V1CLNA)
026900940728      * 02    - IMMESSO CAMPO LINEA DI PARTENZA (V1CLP1)
027000980603      * 03    - INTERROGAZIONE BOLLE DA CLIENTE
027100941028      * 04    -
027200980603      * 05    - ABILITO TASTO FUNZIONALE F19 DI CAMBIO AZIENDA
027300010219      * 06    - IMMESSO CAMPO NUMERO ORM (V1CNOR)
027400941028      * 07    -
027500941028      * 08    -
027600940728      * 11    - IMMESSO CAMPO MITTENTE (V1CRSM)
027700940728      * 12    - IMMESSO CAMPO RIFERIMENTO MITTENTE (V1CRMN)
027800951010      * 13    - IMMESSO CAMPO RIFERIMENTO PARTNER PARTENZA (V1CRPP)
027900940728      * 14    - IMMESSO CAMPO DESTINATARIO (V1CRSD)
028000940316      * 01-16 - INDICANO QUALI CAMPI HO SCELTO A VIDEO
028100940316      * 20    - DISABILITO CMD7 INTERROGAZIONE BOLLE ARRIVI
028200941010      * 21    -
028300940316      * 22/23 - STATO SPEDIZIONI SELEZIONATO
028400941028      * 24    -
028500941028      * 26    -
028600941010      * 27    - ALTA INTENSITA' PER SPED. PARZIALI O CON CONS. ANOMALA
028700941011      * 28    - EMETTO CAMPO V1CMSG DI VISUALIZZAZIONE MESSAGGI DI ERR
028800940316      * 30    - FINE LETTURA FILE
028900940316      * 31    - COMODO
029000940316      * 32    - BOLLA LETTA DA SCARTARE
029100951011      * 33    - DI COMODO
029200940316      * 34    - LETTURA SUBFILE PER RECORD SCELTI
029300961017      * 35    - OFF --> SFLDSP/SFLDSPCTL    ON --> SFLCLR
029400961017      * 36    - DI COMODO
029500941028      * 37    -
029600000831      * 40-58 - ERRORI
029700000831      * 59    - IMMESSO CAMPO A VIDEO: CODICE "CHI SONO"
029800000831      * 60    - ERRORE
029900000531      * 73    - UTILIZZATO PER RICERCA FILIALE POSTE
030000940316      * 90    - INDICA LA PRESENZA DI UN ERRORE
030100941028      * 91    -
030200941011      * 94    - PROTEGGO E NON VISUALIZZO IL CAMPO DELLA SCELTA
030300941011      * 95    - POSIZIONAMENTO CURSORE    SUL CAMPO DELLA SCELTA
030400941010      * 96    - PGM RICHIAMATO CON POSSIBILITA' DI SELEZIONARE BOLLA
030500941010      * 97    - PROVENGO DA MANUTENZIONE BOLLE PARTENZE
030600941010      * 98    -
030700941010      * 99    - ERRORE
030800940316      *****************************************************************
030900000000     C     *ENTRY        PLIST
031000000000     C                   PARM                    KPJBA
031100040220     c                   parm                    FIDNA1ds
031200040220      *
031300920203     C                   MOVEL     KPJBU         PARAM3
031400940809     C*
031500940809     C                   MOVEL     *ZERO         PA3AAS
031600940809     C                   MOVEL     *ZERO         PA3LNP
031700940809     C                   MOVEL     *ZERO         PA3NRS
031800940809     C                   MOVEL     *ZERO         PA3NSP
031900940809     C*
032000060202     C                   Z-ADD     1             CODUT             1 0
032100060202     c                   Movel     rsut          V1crsu
032200981123     C                   MOVEL     SIMFEL        PA0FEL
032300981123     C* CARICO TABELLE
032400950206     C                   EXSR      CARTAB
032500940322      *
032600940727      * Testo il flag che mi dice se sono richiamato da un cliente
032700940727     C                   SETOFF                                       03
032800940729     C     PA3CLI        IFEQ      'S'
032900940727      *
033000940727      * Testo le abilitazioni del cliente
033100940727     C                   EXSR      ABICLI
033200940727     C     $3KINB        CABNE     'S'           FINE
033300940727     C                   SETON                                        03
033400940727      *
033500940727     C                   ENDIF
033600930701     C*
033700941010     C* 20 OFF- PGM  RICHIAMATO --> NON ABILITO LE ALTRE INTERROGAZIONI
033800980603     C                   SETOFF                                       05
033900941010     C     PA3FLG        IFEQ      ' '
034000920224     C                   SETON                                        20
034100980603     C** TASTO FUNZIONALE F19 - CAMBIO AZIENDA SOLO SE NON RICHIAMATO
034200980603     C  N03PARAZI        IFEQ      'S'
034300980603     C                   SETON                                        05
034400980603     C                   ENDIF
034500040715      ** se parametri più di uno spengo il 20 in quanto richiamato dai reclami
034600040715     c     SDSprm        ifgt      1
034700040715     C                   SETOff                                       20
034800040715     C                   ENDIF
034900040715     c
035000941010     C                   ENDIF
035100980603     C**
035200941010     C* SE E' ABILITATA LA RICEZIONE DEL NUM SPEDIZIONE CON '1'
035300941010     C* VISUALIZZO L'1  IN SFL
035400941010     C     PA3FLG        IFEQ      '1'
035500040715     c     SDSprm        orgt      1
035600941010     C                   SETON                                        96
035700941010     C                   ENDIF
035800941010     C*
035900941010     C* INTESTAZIONE PGM
036000941010     C                   MOVEL     CINTER        V1CTES
036100941010     C* MANUTENZIONE
036200941010     C     PA3FLG        IFEQ      'M'
036300941010     C                   MOVEL     CMANUT        V1CTES
036400941010     C                   SETON                                        97
036500950206     C* SE SONO IN MANUTENZIONE RICHIEDO LE STAMPANTI
036600030612     C                   CLEAR                   trul90DS
036700030612      *
036800030612     C                   MOVEL     'S'           d90RSE
036900030612     C                   MOVEL     'S'           d90RSB
037000030612     C                   CALL      'TRUL90R'
037100941011     C                   PARM                    KPJBA
037200030612     C                   PARM                    trul90DS
037300030612      *
037400030612      * F3 - FINE
037500030612     C     d90F3         IFEQ      '1'
037600941011     C                   GOTO      FINE
037700941011     C                   ENDIF
037800030612      *
037900030612      * OVRPRTF BOLLE modulo A4
038000030612     C                   MOVEA     D90PRB4       CMA4(30)
038100030612     C                   MOVEA     D90MDB4       CMA4(52)
038200030612     C*
038300080709     C                   Z-ADD     110           LUNG             15 5
038400080709     C                   CLEAR                   COMMAN          110
038500030612     C                   MOVEA     CMA4          COMMAN
038600030612     C                   CALL      'QCMDEXC'                            91
038700030612     C                   PARM                    COMMAN
038800030612     C                   PARM                    LUNG
038900030612      *
039000030612     C   91              GOTO      FINE
039100030612      *
039200030612      * OVRPRTF BOLLE modulo A5
039300030612     C                   MOVEA     D90PRB5       CMA5(30)
039400030612     C                   MOVEA     D90MDB5       CMA5(52)
039500030612     C*
039600030612     C                   Z-ADD     110           LUNG
039700030612     C                   CLEAR                   COMMAN
039800030612     C                   MOVEA     CMA5          COMMAN
039900030612     C                   CALL      'QCMDEXC'                            91
040000030612     C                   PARM                    COMMAN
040100030612     C                   PARM                    LUNG
040200030612      *
040300030612     C   91              GOTO      FINE
040400030612      *
040500030612      * OVRPRTF Segnaqcolli
040600030612     C                   MOVEA     d90PRE        CM3(28)
040700030612     C                   MOVEA     d90MDE        CM3(50)
040800941011     C*
040900941011     C                   CLEAR                   COMMAN
041000991206     C                   MOVEA     CM3           COMMAN
041100941011     C                   CALL      'QCMDEXC'                            91
041200941011     C                   PARM                    COMMAN
041300941011     C                   PARM                    LUNG
041400030612      *
041500950111     C  N91              OPEN      FNLV22P
041600941011     C   91              GOTO      FINE
041700941010     C                   ENDIF
041800900511     C*---------------------------------------------------------------*
041900930407     C*
042000900516     C* PULIZIA CAMPI CHE RIMANGONO INVARIATI IL SECONDO GIRO
042100940928     C                   MOVEL     '3'           V1CFTA
042200020628     C                   MOVEL     'N'           V1CBPT
042300940728     C                   MOVEL     *ZEROS        V1CLNA
042400940728     C                   MOVEL     *ZEROS        V1CDSD
042500940728     C                   MOVEL     *ZEROS        V1CDSA
042600000831     C                   MOVE      *BLANKS       V1CNOT
042700940728     C                   Z-ADD     *ZERO         V1CRMN
042800951010     C                   MOVE      *BLANKS       V1CRPP
042900940728     C                   MOVE      *BLANKS       V1CRSD
043000940728     C                   MOVE      *BLANKS       V1CRSM
043100940316      *
043200940727      * IMPOSTO LA LINEA DI PARTENZA
043300060202     c                   z-add     dutpou        v1clp1
043400060202     c     dutlpo        ifeq      '2'
043500060202     c     dutlpo        oreq      *blanks
043600060202     c                   z-add     dutpou        savmus            3 0
043700020429     c                   endif
043800941010     C                   MOVE      V1CLP1        NUM3              3 0
043900940727     C     NUM3          CHAIN     AZORG01L                           90
044000940728     C  N90              MOVEL     ORGDES        V1DLP1
044100000531      *
044200000531      * SE LNP POSTE CAMBIO IL VALORE DI DEFAULT DEL PARAMETRO -BOLLE POSTE-
044300000531     C  N90              MOVEL     ORGDE3        OG143
044400020628     c                   if        §ogntw = 'PPT'
044500000531     C                   MOVEL     'P'           V1CBPT
044600000531     C                   ENDIF
044700940316      *
044800900509     C     INIZIO        TAG
044900900514     C                   EXSR      PUL01
045000940727     C*
045100940727     C* EMETTO IL PRIMO VIDEO DI RICHIESTA PARAMETRI
045200940727     C                   MOVEL     'S'           WRIEM
045300900509     C     FOR01         TAG
045400940727     C*
045500940727     C* RIEMETTO L'INTESTAZIONE SOLO SE NECESSARIO
045600940727     C     WRIEM         IFEQ      'S'
045700940727     C                   WRITE     LS04T01
045800940727     C                   MOVE      *BLANKS       WRIEM             1
045900940727     C                   ENDIF
046000940727     C*
046100940727     C                   EXFMT     LS04D01
046200940928     C                   SETOFF                                       212426
046300940928     C                   SETOFF                                       28
046400940729     C                   SETOFF                                       414244
046500940729     C                   SETOFF                                       454648
046600940729     C                   SETOFF                                       495051
046700941012     C                   SETOFF                                       525543
046800000831     C                   SETOFF                                       475383
046900060217     C                   SETOFF                                       545640
047000940729     C*
047100980603     C** F3 - FINE LAVORO
047200980603     C     *INKC         IFEQ      '1'
047300980603     C   05              MOVEL     '1'           PARF3
047400980603     C                   GOTO      FINE
047500980603     C                   ENDIF
047600980603     C** F7  - INTERROGAZIONE BOLLE ARRIVI
047700920203     C   KG              DO
047800940729     C                   MOVEL     '4'           PA2FLG
047900920203     C                   MOVEL     PARAM2        KPJBU
048000940729     C                   CALL      'FNLR36R'
048100920203     C                   PARM                    KPJBA
048200940727     C                   MOVEL     'S'           WRIEM
048300920203     C                   GOTO      FOR01
048400920203     C                   END
048500980603     C** F14 - INTERROGAZIONE BOLLE TRANSITO
048600920423     C   KN              DO
048700940729     C                   MOVEL     'T'           PA2BOL
048800920423     C                   MOVEL     PARAM2        KPJBU
048900940729     C                   CALL      'FNLR68R'
049000920423     C                   PARM                    KPJBA
049100940727     C                   MOVEL     'S'           WRIEM
049200920423     C                   GOTO      FOR01
049300920423     C                   END
049400980603     C** F19 - CAMBIO AZIENDA
049500980603     C   KT              DO
049600980603     C                   MOVEL     '1'           PARF19
049700980603     C                   GOTO      FINE
049800980603     C                   ENDDO
049900900509     C* CONTROLLI FORMATO1
050000900509     C                   EXSR      CONTR
050100900509     C   90              GOTO      FOR01
050200900511     C* ELABORAZIONE
050300900515     C                   EXSR      ELAB
050400941010     C   90              GOTO      FOR01
050500901115     C*
050600901115     C* SE NUM SPE SECCO DOPO VISUALIZZAZIONE TORNO A VID. INIZIALE
050700901115     C     SEL2          IFNE      0
050800901115     C                   GOTO      INIZIO
050900901115     C                   END
051000901115     C* SE NON CI SONO BOLLE SEGNALO ERRORE
051100940727     C     NRR2          IFEQ      0
051200940728     C                   MOVEL     MSG(1)        V1CMSG
051300940728     C                   SETON                                        28  90
051400900516     C                   GOTO      FOR01
051500900516     C                   END
051600940324     C* SE CARICATA SOLO UNA BOLLA LA VISUALIZZO  (SE RICHIESTA DA RIF)
051700940728     C     V1CRMN        IFGT      0
051800940727     C     NRR2          ANDEQ     1
051900951010     C     V1CRPP        ORNE      *BLANKS
052000951010     C     NRR2          ANDEQ     1
052100000831     C     V1CNOT        ORNE      *BLANKS
052200000831     C     NRR2          ANDEQ     1
052300060217     C     V1Cnor        ORNE      0
052400060217     C     NRR2          ANDEQ     1
052500000919     C* SE RICHIAMATO PER SELEZIONE, DA ALTRO PROGRAMMA, ESCO
052600000919     C     PA3FLG        IFEQ      '1'
052700000919     C                   MOVEL     VSCAAS        PA3AAS
052800000919     C                   MOVEL     VSCLNP        PA3LNP
052900000919     C                   MOVEL     VSCNRS        PA3NRS
053000000919     C                   MOVEL     VSCNSP        PA3NSP
053100000919     C                   MOVEL     PARAM3        KPJBU
053200000919     C                   GOTO      FINE
053300000919     C                   ENDIF
053400000919     C  N97              EXSR      CALBOL
053500000919     C   97              EXSR      CALBO2
053600000831     C                   GOTO      INIZIO
053700940324     C                   END
053800940324     C*
053900900515     C** CARICAMENTO SUBFILE
054000900518     C     FORCT2        TAG
054100940727     C                   WRITE     LS04T01
054200940727     C                   WRITE     LS04Z02
054300900515     C** COMANDI SUBFILE
054400900515     C     FORCT         TAG
054500940727     C                   EXFMT     LS04C02
054600940729     C                   SETOFF                                       28
054700940729     C*
054800920203     C* CMD 3 - FINE
054900980603     C     *INKC         IFEQ      '1'
055000980603     C   05              MOVEL     '1'           PARF3
055100980603     C                   GOTO      FINE
055200980603     C                   ENDIF
055300900515     C* CMD12 - RITORNO
055400940317     C   KL              GOTO      INIZIO
055500900515     C* ROLLUP
055600930701     C     *IN37         IFEQ      *ON
055700940728     C   30              SETON                                        28
055800940728     C   30              MOVEL     MSG(16)       V1CMSG
055900940728     C  N30              EXSR      CARSF
056000940727     C                   Z-ADD     COMOD         REC2
056100930701     C                   GOTO      FORCT
056200930701     C                   END
056300900515     C*
056400900516     C** CONTROLLO SE EFFETTUATA UNA SCELTA
056500940727     C                   Z-ADD     PRIMA         REC2
056600940727     C                   READC     LS04S02                                34
056700941011     C     *IN34         IFEQ      *ON
056800940728     C                   SETON                                        28
056900940728     C                   MOVEL     MSG(15)       V1CMSG
057000940728     C                   GOTO      FORCT
057100941011     C                   ENDIF
057200940728     C*
057300900516     C* VISUALIZZO BOLLA X BOLLA
057400941011    1C     *IN34         DOWEQ     *OFF
057500941214     C                   MOVE      *BLANKS       PA0F03
057600941011     C*
057700941011    2C     VSCSCE        IFEQ      '5'
057800940324     C                   EXSR      CALBOL
057900941011    2C                   END
058000940809     C*
058100941011    2C     VSCSCE        IFEQ      '1'
058200941010     C* SE RICHIAMATO PER SELEZIONE, DA ALTRO PROGRAMMA, ESCO
058300941011    3C     PA3FLG        IFEQ      '1'
058400940809     C                   MOVEL     VSCAAS        PA3AAS
058500940809     C                   MOVEL     VSCLNP        PA3LNP
058600940809     C                   MOVEL     VSCNRS        PA3NRS
058700940809     C                   MOVEL     VSCNSP        PA3NSP
058800940809     C                   MOVEL     PARAM3        KPJBU
058900940809     C                   GOTO      FINE
059000941011    3C                   ENDIF
059100040220if  3c                   if        SDSprm > 1
059200040220     c                   eval      IA1ins = 'S'
059300040220     c                   eval      IA1tor = 'S'
059400040220     c                   eval      IA1ogg = ds_OggSped
059500040220     c                   goto      FINE
059600040220e   3c                   endif
059700941010     C*
059800941010     C* SE MANUTENZIONE VADO IN MANUTENZIONE
059900941011    3C     *IN97         IFEQ      *ON
060000941010     C* NON PARTITA
060100941011    4C     VSCFT1        IFEQ      *BLANKS
060200941010     C                   MOVEL     VSCAAS        §LPAAS
060300941010     C                   MOVEL     VSCLNP        §LPLNP
060400941010     C                   MOVEL     VSCNRS        §LPNRS
060500941010     C                   MOVEL     VSCNSP        §LPNSP
060600941010     C                   MOVEL     'M'           §LPFPR
060700030612     C*                  MOVEL     PARAM4        KPJBU
060800941010     C                   CALL      'FNLS01R'
060900941010     C                   PARM                    KPJBA
061000991020     C                   PARM                    DBLP
061100991020     C                   PARM                    DTASV
061200030612     C                   PARM                    trul90ds
061300941115     C*
061400941010     C                   MOVEL     '1'           PA1S01
061500941011     C*
061600941011     C* SE FLAG PROVENIENZA ='A' SIGNIFICA CHE LA BOLLA E' STATA
061700941011     C*  ANNULLATA: PROTEGGO IL CAMPO DELLA SCELTA
061800060214     C     §LPFPR        IFEQ      '1'
061900941011     C                   SETON                                        94
062000941011     C                   ENDIF
062100941010     C*
062200941011   X4C                   ELSE
062300941010     C*
062400941010     C* PARTITA
062500941222     C* VEDO SE E' MANUTENZIONABILE
062600060202     c***  dutlpo        ifeq      '2'
062700060202     c***  dutlpo        oreq      *blanks
062800060202     C***  VSCLNP        ifne      SAVMUS
062900060202     C***                MOVEL     D48MSG        V1CMSG
063000060202     C***                MOVEL     '1'           PA0F03
063100060202     C***                SETON                                        2895
063200060202    4C***                ENDIF
063300060202     c***                endif
063400060202     c
063500060202     c* controllo la skiera pog per lnp o p.o. utente se previsto
063600060203     c                   if        v1ccvb=*blanks
063700060203     c                   clear                   dslr66
063800060203     c                   endif
063900060203     c
064000060202     c                   movel     vsclnp        w003a
064100060202     c     w003a         lookup    $pog                                   31
064200060202     c                   if        not *in31 and d66pocl='S'
064300060202     c                   movel     vscksc        w003a
064400060202     c     w003a         lookup    $pog                                   31
064500060202     c                   endif
064600060202    5c                   if        not *in31
064700060202     C                   MOVEL     msg(28)       V1CMSG
064800060202     C                   MOVEL     '1'           PA0F03
064900060202     C                   SETON                                        2895
065000060202   x5C                   else
065100941222     C*
065200941222     C                   CLEAR                   DSLR48
065300941222     C                   MOVEL     VSCAAS        D48AAS
065400941222     C                   MOVEL     VSCLNP        D48LNP
065500941222     C                   MOVEL     VSCLNP        D48FGS
065600941222     C                   MOVEL     VSCNRS        D48NRS
065700941222     C                   MOVEL     VSCNSP        D48NSP
065800941222     C                   MOVEL     VSCCBO        D48CBO
065900941222     C                   MOVEL     'P'           D48TBO
066000941222     C                   MOVEL     V1CCVB        D48CVB
066100950607     C                   MOVEL     'S'           D48F12
066200941222     C                   MOVEL     DSLR48        KPJBU
066300941222     C                   CALL      'FNLR48R'
066400941010     C                   PARM                    KPJBA
066500941222     C                   PARM                    DSARBD
066600941222     C                   PARM                    DSARBK
066700991012     C                   PARM                    DARBT
066800941222     C                   PARM                    DSARBG
066900941222     C*
067000941222     C                   MOVEL     KPJBU         DSLR48
067100941222     C* ERRORE
067200060202    6C     D48ERR        IFNE      *BLANKS
067300941222     C                   MOVEL     D48MSG        V1CMSG
067400941222     C                   MOVEL     '1'           PA0F03
067500941222     C                   SETON                                        2895
067600060202    6C                   ENDIF
067700060202    5C                   ENDIF
067800950114     C* SE NON C'E' STATO ERRORE RICHANO LA BOLLA PER PRENDERE IL MITT
067900950114     C* E IL DESTINATARIO CHE POSSON ESSERE MODIFICATI
068000950114     C     KBLP3         CHAIN     FNBLP01L                           31
068100950114     C  N31              EXSR      RIESFL
068200941010     C*
068300941011    4C                   ENDIF
068400941011    3C                   ENDIF
068500941011    2C                   ENDIF
068600940809     C*
068700940728     C                   MOVEL     *BLANKS       VSCSCE
068800930607     C*
068900940728     C                   MOVEL     VSCDCM        ALF4              4
069000941011    2C     ALF4          IFNE      *BLANKS
069100930614     C                   TESTN                   ALF4                 33
069200930607     C  N33              SETON                                        27
069300941011    2C                   END
069400941010     C*
069500941011    2C     VSCFT1        IFEQ      '9'
069600941010     C                   SETON                                        29
069700941010     C                   ELSE
069800941010     C                   SETOFF                                       29
069900941011    2C                   ENDIF
070000930607     C*
070100940727     C                   UPDATE    LS04S02
070200941011     C                   SETOFF                                       279594
070300930607     C*
070400941109     C* SE USCITO DA BOLLA CON TASTO F3 TORNO AL SUBFILE
070500941214    2C     PA0F03        IFEQ      '1'
070600900530     C                   SETON                                        34
070700900530     C                   ELSE
070800940727     C                   READC     LS04S02                                34
070900941011    2C                   ENDIF
071000941011    1C                   ENDDO
071100941011     C*
071200900518     C                   GOTO      FORCT2
071300941011     C*
071400000000     C     FINE          TAG
071500930506     C*
071600930701     C* CALL PGM PER CHIUSURA FILES
071700941214     C     PA0S98        IFEQ      '1'
071800930506     C                   MOVEL     'C'           D98TLA
071900970925     C                   CALL      'TNSD99R'
072000970925     C                   PARM                    DS99
072100930506     C                   END
072200930701     C*
072300941214     C     PA0F75        IFEQ      '1'
072400940729     C                   MOVEL     'C'           P75FLG
072500940729     C                   MOVEL     PARA75        KPJBU
072600940729     C                   CALL      'FNLR75R'
072700930701     C                   PARM                    KPJBA
072800930701     C                   END
072900930701     C*
073000941214     C     PA0FB5        IFEQ      '1'
073100941213     C                   CLEAR                   PARAB5
073200940729     C                   MOVEL     'C'           PB5FLG
073300940729     C                   MOVEL     PARAB5        KPJBU
073400940729     C                   CALL      'FNLRB5R'
073500930701     C                   PARM                    KPJBA
073600930701     C                   END
073700941010     C*
073800941010     C     PA1S01        IFEQ      '1'
073900941010     C                   MOVEL     'C'           §LPFPR
074000941010     C                   CALL      'FNLS01R'
074100941010     C                   PARM                    KPJBA
074200991020     C                   PARM                    DBLP
074300991020     C                   PARM                    DTASV
074400030612     C                   PARM                    trul90ds
074500941010     C                   END
074600941013     C*
074700941013     C     *IN97         IFEQ      *ON
074800941013     C                   MOVEL     'C'           D66TLA
074900941014     C                   MOVEL     DSLR66        KPJBU
075000941013     C                   CALL      'FNLR66R'
075100941013     C                   PARM                    KPJBA
075200941222     C*
075300941222     C                   ENDIF
075400980603     C**
075500980603     C                   MOVEL     PARAM3        KPJBU
075600930506     C*
075700040810     c* Richiamo il *pgm d utilità FIDN12R in modalità "solo chiusura"
075800040810     c                   clear                   fidn12ds
075900040810     c                   eval      i12tla = 'C'
076000040810     c                   call      'FIDN12R'
076100040810     c                   parm                    fidn12ds
076200040810     c*
076300000000     C                   SETON                                        LR
076400940317      **************************************************************************
076500940317      *    PULIZIA CAMPI FORMATO 1
076600940317      **************************************************************************
076700900514     C     PUL01         BEGSR
076800940317      *
076900940728     C                   MOVEL     *ZEROS        V1CLP2
077000940728     C                   MOVEL     *ZEROS        V1CNRS
077100940728     C                   MOVEL     *ZEROS        V1CNSP
077200940729     C                   Z-ADD     UYEAR         V1CAAS
077300940729     C     UYEAR         IFLT      60
077400940729     C                   MOVEL     20            V1CAAS
077500940729     C                   ELSE
077600940729     C                   MOVEL     19            V1CAAS
077700940729     C                   ENDIF
077800940728     C                   MOVEL     *BLANKS       V1CTSP
077900040216     C                   clear                   V1Ccbo
078000940728     C                   MOVEL     *ZEROS        V1CPSD
078100940728     C                   MOVEL     99            V1CPSA
078200940728     C                   MOVEL     *ZEROS        V1CSCA
078300940923     C                   MOVEL     *ZERO         V1CFLS
078400950216     C                   MOVEL     *BLANKS       V1DFLS
078500940728     C                   MOVEL     *ZEROS        V1CKSD
078600940728     C                   MOVE      *BLANKS       V1CRAG
078700940728     C                   MOVEL     *ZEROS        V1CSCD
078800940728     C                   MOVE      *BLANKS       V1CSSP
078900940729     C                   MOVE      *BLANKS       V1CTER
079000961118     C                   CLEAR                   V1CFBR
079100010216      *--
079200010216     C                   CLEAR                   V1CPOE
079300010216     C                   CLEAR                   V1CNSR
079400010216     C                   CLEAR                   V1CNOR
079500010216     C                   CLEAR                   V1CNRV
079600010216      *--
079700930630     C                   SETON                                        35
079800940727     C                   WRITE     LS04C02
079900930630     C                   SETOFF                                       35
080000940317      *
080100900514     C                   ENDSR
080200940317      **************************************************************************
080300940317      *    CONTROLLI FORMATO1
080400940317      **************************************************************************
080500900509     C     CONTR         BEGSR
080600940316      *
080700900515     C                   SETOFF                                       900102
080800040216     ***C                   SETOFF                                       040708
080900040216     C                   SETOFF                                         0708
081000941012     C                   SETOFF                                       121114
081100000831     C                   SETOFF                                       131659
081200010219     C                   SETOFF                                       06
081300940728     C                   MOVEL     *BLANKS       VCDLNA
081400940728     C                   MOVEL     *BLANKS       VCDLNP
081500020306     c                   clear                   lnpntw
081600020306     c                   clear                   lnantw
081700060518     C***                MOVEL     *ZEROS        KRPP             12 0
081800060609     C                   clear                   KRPP             14
081900950103     C*
082000950103     C* SE L'ANNO E' IMMESSO DI DUE CIFRE, METTO LE ALTRE DUE
082100950103     C     V1CAAS        IFLT      100
082200950103     C     V1CAAS        IFGT      60
082300950103     C                   ADD       1900          V1CAAS
082400950103     C                   ELSE
082500950103     C                   ADD       2000          V1CAAS
082600950103     C                   ENDIF
082700950103     C                   ENDIF
082800950121     C*
082900950121      * SE SONO IN MANUTENZIONE CONTROLLO ANCHE LA CAUSALE VARIAZIONE
083000950121     C                   CLEAR                   DSLR66
083100950121    0C     V1CCVB        IFNE      *BLANKS
083200950121     C     '?'           SCAN      V1CCVB                                 90
083300950121     C* RICERCA
083400950121    1C     *IN90         IFEQ      *ON
083500950121     C                   CLEAR                   DSLR66
083600950121     C                   MOVEL     'V'           D66GES
083700950121     C                   MOVEL     'P'           D66TBO
083800950121     C*
083900950121     C* SE IMMESSO NUMERO SPEDIZIONE LO VISUALIZZO
084000950121    2C     V1CNSP        IFGT      0
084100950121     C                   MOVEL     V1CNSP        D66NSP
084200950121     C                   MOVEL     V1CAAS        D66AAS
084300950121     C                   MOVEL     V1CLP2        D66LNP
084400950121     C                   MOVEL     V1CNRS        D66NRS
084500950121    2C                   ENDIF
084600950121     C*
084700950121     C                   MOVEL     DSLR66        KPJBU
084800950121     C                   CALL      'FNLR66R'
084900950121     C                   PARM                    KPJBA
085000950121     C                   MOVEL     KPJBU         DSLR66
085100950121     C                   MOVEL     D66CVB        V1CCVB
085200950121     C                   MOVEL     D66DES        V1DCVB
085300950121     C                   MOVEL     'S'           WRIEM
085400950121   X1C                   ELSE
085500950121     C* CONTROLLO
085600950121     C                   CLEAR                   DSLR66
085700950121     C                   MOVEL     'C'           D66GES
085800950121     C                   MOVEL     'P'           D66TBO
085900950121     C                   MOVEL     V1CCVB        D66CVB
086000950121    2C     V1CNSP        IFGT      0
086100950121     C                   MOVEL     V1CNSP        D66NSP
086200950121     C                   MOVEL     V1CAAS        D66AAS
086300950121     C                   MOVEL     V1CLP2        D66LNP
086400950121     C                   MOVEL     V1CNRS        D66NRS
086500950121    2C                   ENDIF
086600950121     C*
086700950121     C                   MOVEL     DSLR66        KPJBU
086800950121     C                   CALL      'FNLR66R'
086900950121     C                   PARM                    KPJBA
087000950121     C                   MOVEL     KPJBU         DSLR66
087100950121     C                   MOVEL     D66CVB        V1CCVB
087200950121     C                   MOVEL     D66DES        V1DCVB
087300950121     C*
087400950121     C* ERRORE
087500950121    2C     D66MSG        IFNE      *BLANKS
087600950121     C                   MOVEL     D66MSG        V1CMSG
087700950121     C                   SETON                                        284390
087800950121     C                   GOTO      ENDCTR
087900950121     C*
088000950121   X2C                   ELSE
088100950121     C* DECODIFICA
088200950121     C                   MOVEL     D66DES        V1DCVB
088300950121    2C                   ENDIF
088400950121     C*
088500950121     C* NON POSSO MANUTENZIONARE BOLLE IN STB
088600950121    1C                   ENDIF
088700950123    0C                   ENDIF
088800950103     C*
088900901115     C* SELEZIONO PER DATA SPEDIZIONE O PER NUMERO SPEDIZIONE
089000000831      *
089100000831      * .. SE NESSUNA SELEZIONE VIENE EFFETTUATA DO ERRORE
089200000831      *
089300000831     C     SEL1          IFEQ      *ZERO
089400000831     C     SEL2          ANDEQ     0
089500000831     C     SEL3          ANDEQ     *BLANKS
089600000831     C     V1CNOT        ANDEQ     *BLANKS
089700010216     C     SEL4          ANDEQ     0
089800940728     C                   SETON                                        284190
089900940728     C                   MOVEL     MSG(6)        V1CMSG
090000901115     C                   GOTO      ENDCTR
090100901115     C                   END
090200020614
090300020614      * se è manutenzione non si può richiedere la lnp 999
090400020614     c                   if        *in97
090500020614      * non si può richiedere la lnp 999
090600020614     c                   if        v1clp1 = 999
090700020614     c                   eval      *in42 = *on
090800020614     c                   eval      *in28 = *on
090900020614     c                   eval      *in90 = *on
091000020614     c                   eval      v1cmsg = msg(2)
091100020614     c                   goto      endctr
091200020614     c                   endif
091300020614      * la linea di partenza deve essere del mio terminal di partenza
091400060202     c**                 if        v1clp1 > *zeros
091500060202     c**                 movel     v1clp1        w003a
091600060202     c**   w003a         lookup    $pog                                   31
091700060202     c***  v1clp1        lookup    l1                                     31
091800060202     c**                 if        not *in31
091900060202     c**                 eval      *in42 = *on
092000060202     c**                 eval      *in28 = *on
092100060202     c**                 eval      *in90 = *on
092200060202     c**                 eval      v1cmsg = msg(27)
092300060202     c**                 goto      endctr
092400060202     c**                 endif
092500060202     c**                 endif
092600020614     c                   endif
092700930630     C***
092800930630     C* D A T A   S P E D I Z I O N E
092900930630     C***
093000000831     C     SEL2          IFEQ      0
093100010216     C     SEL4          ANDEQ     0
093200940321      *
093300930630     C* O LA LNP O LA LNA DEVONO ESSERCI
093400940728     C     V1CLNA        IFEQ      0
093500940728     C     V1CLP1        ANDEQ     0
093600000831     C     V1CNOT        ANDEQ     *BLANKS
093700000831     C     V1CRMN        ANDEQ     *ZERO
093800951010     C     V1CRPP        ANDEQ     *BLANKS
093900940728     C     V1CRSD        ANDEQ     *BLANKS
094000940728     C     V1CRSM        ANDEQ     *BLANKS
094100940728     C                   SETON                                        284190
094200940728     C                   MOVEL     MSG(7)        V1CMSG
094300901115     C                   GOTO      ENDCTR
094400901115     C                   END
094500940728     C*
094600901115     C* DATA SPEDIZIONE DAL OBBLIGATORIA
094700940727     C* SOLO IN ALCUNI CASI
094800940728     C     V1CDSD        IFEQ      0
094900000831     C     V1CNOT        ANDEQ     *BLANKS
095000940728     C     V1CRMN        ANDEQ     *ZERO
095100951010     C     V1CRPP        ANDEQ     *BLANKS
095200940728     C     V1CRSD        ANDEQ     *BLANKS
095300940728     C     V1CRSM        ANDEQ     *BLANKS
095400940728     C                   MOVEL     MSG(8)        V1CMSG
095500940728     C                   SETON                                        285190
095600901115     C                   GOTO      ENDCTR
095700901115     C                   END
095800020614
095900020614      * se inserita SOLO la linea di partenza questa non può essere 0 o 999
096000020614     c                   if        v1clna = *zeros and v1cnot = *blanks and
096100020614     c                             v1crmn = *zeros and v1crpp = *blanks and
096200020614     c                             sel3 = *blanks and
096300020614     c                             (v1clp1 = *zeros or v1clp1 = 999)
096400020614     c                   eval      *in42 = *on
096500020614     c                   eval      *in28 = *on
096600020614     c                   eval      *in90 = *on
096700020614     c                   eval      v1cmsg = msg(2)
096800020614     c                   goto      endctr
096900020614     c                   endif
097000020614
097100900514     C* LINEA ARRIVO
097200941027     C                   MOVE      *BLANKS       V1DLNA
097300940728     C     V1CLNA        IFNE      0
097400940728     C                   Z-ADD     V1CLNA        NUM3
097500940923     C     NUM3          CHAIN     AZORG01L                           41
097600940728     C   41              DO
097700940728     C                   SETON                                        28  90
097800940728     C                   MOVEL     MSG(5)        V1CMSG
097900940728     C                   GOTO      ENDCTR
098000940728     C                   ENDDO
098100940728     C                   MOVEL     ORGDES        V1DLNA
098200940728     C                   MOVEL     ORGDES        VCDLNA
098300991222     C                   MOVEL     ORGDE3        OG143
098400020306     c                   eval      lnantw = §ogntw
098500900514     C                   SETON                                        01
098600901115     C                   END
098700901115     C* LINEA PARTENZA
098800940727     C* E' OBBLIGATORIA SE C'E' ALMENO UNA RAGIONE SOCIALE
098900941027     C                   MOVE      *BLANKS       V1DLP1
099000940728     C     V1CLP1        IFNE      0
099100940728     C     V1CRSD        ORNE      *BLANKS
099200940728     C     V1CRSM        ORNE      *BLANKS
099300020614      * se lnp richiesta è 999  o 000 non devo fare la chain
099400020614     c                   if        v1clp1 <> 999 and v1clp1 <> 0
099500940728     C                   Z-ADD     V1CLP1        NUM3
099600940728     C     NUM3          CHAIN     AZORG01L                           42
099700940728     C   42              DO
099800940728     C                   MOVEL     MSG(2)        V1CMSG
099900940728     C                   SETON                                        28  90
100000940728     C                   GOTO      ENDCTR
100100940728     C                   ENDDO
100200940728     C                   MOVEL     ORGDES        V1DLP1
100300940728     C                   MOVEL     ORGDES        VCDLNP
100400991214     C                   MOVEL     ORGDE3        OG143
100500020306     c                   eval      lnpntw = §ogntw
100600020614     c                   endif
100700901115     C                   SETON                                        02
100800901115     C                   END
100900951012     C*
101000000831     C* SI PUO' INSERIRE O "CHI SONO" O RIF.MITTENTE O  RIF.PARTNER
101100000831     C     V1CNOT        IFNE      *BLANKS
101200000831     C     V1CRMN        IFNE      0
101300000831     C     V1CRPP        ORNE      *BLANKS
101400991214     C                   MOVEL     MSG(18)       V1CMSG
101500000831     C                   SETON                                        288390
101600951012     C                   GOTO      ENDCTR
101700951012     C                   ENDIF
101800000831     C                   ENDIF
101900000831     C     V1CRMN        IFNE      0
102000000831     C     V1CRPP        ANDNE     *BLANKS
102100000831     C                   MOVEL     MSG(18)       V1CMSG
102200000831     C                   SETON                                        284790
102300000831     C                   GOTO      ENDCTR
102400000831     C                   ENDIF
102500000831     C*
102600060609     C* SE LNP DPD IL NUMERO  PUO' AVERE :
102700060609     c*  lunghezza 1 - 11 - 12 - 14
102800991214     C     V1CRMN        IFGT      0
102900020306     c     lnpntw        andeq     'DPD'
103000060609     c                   movel     v1crmn        w015a            15
103100060609     c                   select
103200060609     c* ok se lungo 1
103300060609     c                   when      %subst(w015a:1:14)=*zeros
103400060609     c* ok se lungo 11
103500060609     c                   when      %subst(w015a:1:4)=*zeros    and
103600060609     c                             %subst(w015a:5:1)>*zeros
103700060609     c* ok se lungo 12
103800060609     c                   when      %subst(w015a:1:3)=*zeros    and
103900060609     c                             %subst(w015a:4:1)>*zeros
104000060609     c* ok se lungo 14
104100060609     c                   when      %subst(w015a:1:1)=*zeros    and
104200060609     c                             %subst(w015a:2:1)>*zeros
104300060609     c                   other
104400060609     C                   MOVEL     MSG(22)       V1CMSG
104500060609     C                   SETON                                        28  90
104600060609     C                   GOTO      ENDCTR
104700060609     C                   endsl
104800060609     c
104900060609     c*
105000060609     C**                 MOVEL     V1CRMN        W003A             3
105100060609     C**                 MOVEL     V1CRMN        W002A             2
105200060609     C**                 MOVEL     V1CRMN        W001A             1
105300060609     C**   W003A         IFNE      '000'
105400060609     C**   W001A         ifne      '0'
105500060609     C**   W002A         oreq      '00'
105600060609     C**                 MOVEL     MSG(22)       V1CMSG
105700060609     C**                 SETON                                        28  90
105800060609     C**                 GOTO      ENDCTR
105900060609     C**                 ENDIF
106000060609     C**                 ENDIF
106100991214     C                   ENDIF
106200951012     C*
106300901115     C* DATA SPEDIZIONE DAL
106400940727     C                   Z-ADD     *ZERO         COMDSD
106500940727     C                   Z-ADD     *ZERO         COMAAD
106600940727     C                   Z-ADD     *ZERO         COMMGD
106700940728     C     V1CDSD        IFNE      *ZERO
106800940728     C                   MOVE      V1CDSD        G02DAT
106900920206     C                   MOVEL     *BLANK        G02ERR
107000940727     C                   CALL      'XSRDA8'
107100920206     C                   PARM                    WLBDAT
107200920206     C     G02ERR        IFEQ      '1'
107300950327     C                   SETON                                        285190
107400950327     C                   MOVEL     MSG(8)        V1CMSG
107500900515     C                   GOTO      ENDCTR
107600900515     C                   ELSE
107700940729     C                   MOVEL     G02DAT        V1CDSD
107800940727     C                   MOVE      G02INV        COMDSD            8 0
107900940727     C                   MOVEL     G02INV        COMAAD            4 0
108000920206     C                   MOVE      G02INV        COMMGD            4 0
108100900515     C                   SETON                                        16
108200900515     C                   END
108300940727     C                   ENDIF
108400900515     C* DATA SPEDIZIONE AL
108500940727     C                   Z-ADD     *ZERO         COMDSA
108600940728     C     V1CDSA        IFNE      0
108700940728     C                   MOVE      V1CDSA        G02DAT
108800920206     C                   MOVEL     *BLANK        G02ERR
108900940727     C                   CALL      'XSRDA8'
109000920206     C                   PARM                    WLBDAT
109100920206     C     G02ERR        IFEQ      '1'
109200940728     C                   SETON                                        285290
109300940728     C                   MOVEL     MSG(8)        V1CMSG
109400900515     C                   GOTO      ENDCTR
109500900515     C                   ELSE
109600940729     C                   MOVEL     G02DAT        V1CDSA
109700940727     C                   MOVE      G02INV        COMDSA            8 0
109800900515     C                   END
109900940728     c*
110000900515     C* DATA SPEDIZIONE AL MAGGIORE DATA SPEDIZIONE AL
110100900515     C     COMDSA        IFLT      COMDSD
110200940728     C                   SETON                                        285190
110300940728     C                   MOVEL     MSG(9)        V1CMSG
110400900515     C                   GOTO      ENDCTR
110500900515     C                   END
110600900515     C                   END
110700940321      *
110800940727      * SE DATA SPEDIZIONE AL E' VUOTA, LA IMPOSTO COMA LA DATA DAL
110900940727     C     COMDSA        IFEQ      *ZERO
111000940727     C     COMDSD        ANDNE     *ZERO
111100940727     C                   Z-ADD     COMDSD        COMDSA
111200940727     C                   ENDIF
111300901115     C***
111400940928     C* TIPO SERVIZIO
111500940729     C     V1CTSP        IFNE      *BLANKS
111600040216     ***C                   SETON                                        04
111700940728     C     V1CTSP        IFEQ      '?'
111800940728     C                   CALL      'X§TABER'
111900040217     C                   PARM      CodUt         §KUT
112000040217     C                   PARM      '5E'          §COD
112100040217     C                   PARM      *blanks       §KEY
112200040217     C                   PARM      *blanks       §DES             30
112300940729     C                   MOVEL     §KEY          V1CTSP
112400940728     C                   SETON                                        90
112500940728     C                   MOVEL     'S'           WRIEM
112600940728     C                   ENDIF
112700940728     C*
112800940729     C                   MOVEL     '5E'          KCOD
112900940728     C                   MOVEL     *BLANKS       KKEY
113000940728     C                   MOVEL     V1CTSP        KKEY
113100940729     C     KTAB2         CHAIN     TABEL00F                           46
113200961118     C     *IN46         IFEQ      *ON
113300961118     C     TBLFLG        ORNE      ' '
113400940728     C                   MOVEL     MSG(14)       V1CMSG
113500941014     C                   SETON                                        284690
113600940728     C                   GOTO      ENDCTR
113700940728     C                   ENDIF
113800940729     C                   ENDIF
113900040216      *
114000040216      * Tipo Bolla
114100040216if  1c                   if        V1Ccbo <> *blanks
114200040216     c                   eval      *in31  =  *off
114300040216     c     '?'           scan      V1Ccbo                                 31
114400040216if  2c                   if        *in31 = *on
114500040216     c                   call      'X§TABER'
114600040217     c                   parm      CodUt         §KUT
114700040217     c                   parm      '3A'          §COD
114800040217     c                   parm      *blanks       §KEY
114900040217     c                   parm      *blanks       §DES
115000040216     c                   movel     §KEY          V1Ccbo
115100040216     c                   eval      *in90 = *on
115200040216     c                   movel     'S'           wRiem
115300040216e   2c                   endif
115400040216      *
115500040216     c                   movel     '3A'          KCOD
115600040216     c                   movel(p)  V1Ccbo        KKEY
115700040216     c     KTAB2         chain     TABEL
115800040216if  2c                   if        NOT %found(TABEL00F)
115900040216     c                             or  TBLflg <> *blanks
116000040216     c                   movel     Msg(4)        V1Cmsg
116100040216     c                   seton                                        284090
116200040216     c                   goto      ENDCTR
116300040216e   2c                   endif
116400040216e   1c                   endif
116500940928     C*
116600901115     C* NUMERO SERIE AL DEVE ESSERE > NUMERO SERIE DAL
116700901115     C*  SE IMMESSO
116800940728     C     V1CPSD        IFGT      V1CPSA
116900940728     C                   SETON                                        284890
117000940728     C                   MOVEL     MSG(10)       V1CMSG
117100901115     C                   GOTO      ENDCTR
117200901115     C                   END
117300901115     C* NUMERO SEGNACOLLO AL DEVE ESSERE > NUMERO SEGNACOLLO DAL
117400940728     C     V1CSCA        IFGT      0
117500940728     C     V1CSCD        ANDGT     V1CSCA
117600940728     C                   SETON                                        284990
117700940728     C                   MOVEL     MSG(11)       V1CMSG
117800901115     C                   GOTO      ENDCTR
117900901115     C                   END
118000901115     C* SE IMMESSO AL IMMETTERE ANCHE DAL
118100940728     C     V1CSCA        IFGT      0
118200940728     C     V1CSCD        ANDEQ     0
118300940728     C                   SETON                                        284990
118400940728     C                   MOVEL     MSG(12)       V1CMSG
118500901115     C                   GOTO      ENDCTR
118600901115     C                   END
118700930630     C*
118800940728     C     V1CSCA        COMP      0                                  08
118900940728     C     V1CSCD        COMP      0                                  07
119000940923     C*
119100940923     C* LA FILIALE SEGNACOLLO DEVE ESSERE VALIDA, SE IMMESSSA
119200950216     C                   MOVEL     *BLANKS       V1DFLS
119300940923     C     V1CFLS        IFNE      *ZERO
119400940923     C     V1CFLS        CHAIN     AZORG01L                           21
119500940923     C   21              SETON                                        28  90
119600940923     C   21              MOVEL     MSG(17)       V1CMSG
119700940923     C   21              GOTO      ENDCTR
119800950216     C                   MOVEL     ORGDES        V1DFLS
119900940923     C                   ENDIF
120000940316      *
120100961017     C****  CODICE CLIENTE  ****
120200961017     C* LA LINEA DEL CODICE CLIENTE DEVE ESSERE ESISTENTE
120300961017     C     V1CKLP        IFGT      0
120400961017     C                   Z-ADD     V1CKLP        NUM3
120500961017     C     NUM3          CHAIN     AZORG01L                           36
120600961017     C     *IN36         IFEQ      *ON
120700961017     C                   SETON                                        285390
120800961017     C                   MOVEL     MSG(2)        V1CMSG
120900961017     C                   GOTO      ENDCTR
121000961017     C                   ENDIF
121100961017     C* SE INSERITA SOLO LA LINEA DEL CODICE ---> ERRORE
121200961017     C     V1CKSC        IFEQ      0
121300961017     C     V1CRAG        ANDEQ     *BLANKS
121400961017     C                   SETON                                        285090
121500961017     C                   MOVEL     MSG(13)       V1CMSG
121600961017     C                   GOTO      ENDCTR
121700961017     C                   ENDIF
121800961017     C*
121900961017     C                   ELSE
122000961017     C     V1CKSC        IFGT      0
122100961017     C                   SETON                                        285390
122200961017     C                   MOVEL     MSG(2)        V1CMSG
122300961017     C                   GOTO      ENDCTR
122400961017     C                   ENDIF
122500961017     C                   ENDIF
122600961017     C*
122700940727      * IL CODICE CLIENTE DEVE ESSERE ESISTENTE
122800961017     C     V1CKSC        IFEQ      0
122900940728     C     V1CRAG        IFNE      *BLANKS
123000940727      *
123100000831      * PAXSTA = 9  ESCLUDO ANNULLATI
123200000831     C                   Z-ADD     9             PAXSTA
123300940727     C                   Z-ADD     0             ACOKSC
123400000831     C                   MOVEL     V1CRSU        PAXDUT           30
123500940727     C                   MOVEL     *BLANKS       DESCR            48
123600940728     C                   MOVEL     V1CRAG        DESCR
123700060202     C                   Z-ADD     dutKCI        ACOKCC
123800000831     C                   MOVEL     V1CKLP        PAXFLR
123900000831     C                   CLEAR                   PAXDIT
124000000831     C                   Z-ADD     1             PAXNUM
124100000831     C                   CALL      'XALFA3BR'
124200000831     C                   PARM                    PAXDUT
124300940727     C                   PARM                    CODUT
124400940727     C                   PARM                    DESCR
124500940727     C                   PARM                    ACOKCC
124600000831     C                   PARM                    PAXSTA            1 0
124700000831     C                   PARM                    PAXFLR           90
124800000831     C                   PARM                    PAXDIT            3
124900000831     C                   PARM                    PAXNUM            2 0
125000000831     C                   PARM                    PAXKCM           80
125100000831     C                   PARM                    PAXKSM          140
125200000831     C                   PARM                    PAXKDM           60
125300000831     C     PAXSTA        IFNE      -1
125400000831     C                   MOVEL     PAXKSM        ACOKSC
125500940728     C                   MOVEL     ACOKSC        V1CKSD
125600961017     C                   MOVEL     DESCR         V1CRAG
125700940727     C                   ELSE
125800940728     C                   CLEAR                   V1CRAG
125900940727     C                   ENDIF
126000940727      *
126100000830     C                   MOVE      'S'           WRIEM
126200940727     C                   SETON                                        90
126300940727     C                   GOTO      ENDCTR
126400940727     C                   ENDIF
126500940727      *
126600940727     C                   ELSE
126700940727      *
126800940727      * CONTROLLO VALIDITA' DEL CODICE CLIENTE IMMESSO
126900060202     C                   Z-ADD     dutKCI        ACOKCC
127000950504     C                   MOVEL     V1CKSD        ACOKSC
127100940727     C     CNACOK        CHAIN     CNACO00F                           50
127200940727     C  N50ACOFLG        COMP      *BLANKS                            5050
127300940728     C     *IN50         IFEQ      *OFF
127400940728     C                   MOVEL     ACORAG        V1CRAG
127500940728     C                   ELSE
127600940728     C                   SETON                                        28  90
127700940728     C                   MOVEL     MSG(13)       V1CMSG
127800940728     C                   GOTO      ENDCTR
127900940728     C                   ENDIF
128000940728     C*
128100940727     C                   ENDIF
128200940316      *
128300000831     C* CODICE "CHI SONO"
128400000831     C     V1CNOT        IFNE      *BLANKS
128500000831     C                   SETON                                        59
128600000831     C                   ENDIF
128700951010     C* RIFERIMENTO MITTENTE
128800940728     C     V1CRMN        COMP      0                                  12
128900951010     C* RIFERIMENTO PARTNER PARTENZA
129000951010     C     V1CRPP        IFNE      *BLANKS
129100951010     C                   SETON                                        13
129200951010     C                   ENDIF
129300930406     C*
129400930406     C* STATO SPEDIZIONE
129500010718     C     V1CSSP        COMP      'N'                                    22
129600010718     C     V1CSSP        COMP      'C'                                    23
129700930406     C*
129800901115     C* SE IMMESSA RAGIONE SOCIALE CERCO PRIMO BLANKS
129900940728     C     V1CRSM        IFNE      *BLANKS
130000021111      * Non si può fare per lnp 999
130100021111     c                   if        v1clp1 = 999
130200020614     c                   eval      *in42 = *on
130300020614     c                   eval      *in28 = *on
130400020614     c                   eval      *in90 = *on
130500020614     c                   eval      v1cmsg = msg(2)
130600020614     c                   goto      endctr
130700020614     c                   endif
130800901115     C                   SETON                                        11
130900940728     C                   MOVEA     V1CRSM        C1
131000940727     C                   Z-ADD     15            XX                2 0
131100930630     C     C1(XX)        DOWEQ     *BLANKS
131200930630     C                   SUB       1             XX
131300901116     C                   END
131400901116     C                   END
131500901115     C* IMMESSO DESTINATARIO: CERCO PRIMO BLANKS
131600940728     C     V1CRSD        IFNE      *BLANKS
131700021111      * Non si può fare per lnp 999
131800021111     c                   if        v1clp1 = 999
131900020614     c                   eval      *in42 = *on
132000020614     c                   eval      *in28 = *on
132100020614     c                   eval      *in90 = *on
132200020614     c                   eval      v1cmsg = msg(2)
132300020614     c                   goto      endctr
132400020614     c                   endif
132500901115     C                   SETON                                        14
132600940728     C                   MOVEA     V1CRSD        C1
132700940727     C                   Z-ADD     15            ZZ                2 0
132800930630     C     C1(ZZ)        DOWEQ     *BLANKS
132900930630     C                   SUB       1             ZZ
133000901116     C                   END
133100901115     C                   END
133200901115     C                   END
133300930630     C***
133400930630     C* N U M E R O   S P E D I Z I O N E
133500930630     C***
133600930630     C* LINEA PARTENZA
133700901115     C     SEL2          IFNE      0
133800940728     C                   Z-ADD     V1CLP2        NUM3
133900940728     C     NUM3          CHAIN     AZORG01L                           45
134000940728     C   45              DO
134100940728     C                   MOVEL     MSG(2)        V1CMSG
134200940728     C                   SETON                                        28  90
134300940728     C                   GOTO      ENDCTR
134400940728     C                   ENDDO
134500940728     C*
134600940928     C     KLNP          CHAIN     FNBLP01L                           44
134700030527     C*
134800030527     C* Inserisco qui i controlli x verificare la data limite x considerare le
134900030527     C* bolle PT (tale controllo nn viene effettuato se l'utente è del gruppo
135000030527     C* EDP o ISP).
135100030527     C                   if        *in44 = *off
135200030527     C                   if        %subst(KNMUS:1:3) = 'EDP' OR
135300030527     C                             %subst(KNMUS:1:3) = 'ISP'
135400030527     C                   else
135500030527     C     blpLNP        lookup    SKPOPT                                 77
135600030527     C                   if        *in77
135700041028     c* non aggancio fnarb tanto non mi interessa se e' appena stata
135800041028     c* consegna e la data non è ancora in partenza è sicuramente
135900041028     c*  > della data limite
136000030527     C                   if        blpDCM >  0
136100030527     C                   if        blpDCM >= wDataLimitePT
136200030527     C                   else
136300030527     C                   seton                                        44
136400030527     C                   endif
136500030527     C                   endif
136600030527     C                   endif
136700030527     C                   endif
136800030527     C                   endif
136900030527     C*
137000991029     C  N44              MOVEL     BLPFT1        WFT1              1
137100941115     C*
137200941115     C* ERRORE
137300940729     C   44              DO
137400940729     C                   SETON                                        28  90
137500940728     C                   MOVEL     MSG(3)        V1CMSG
137600940728     C                   GOTO      ENDCTR
137700940728     C                   ENDDO
137800020614      * se sono in manutenzione
137900020614     c                   if        *in97
138000020614      * la linea di partenza della bolla deve far parte del mio terminal
138100060202      * di partenza  se bolla non partita altrimenti la skiera POG
138200060202     c                   if        wft1=' '
138300060202     c     blplnp        lookup    l1                                     31
138400060202     c                   else
138500060203     c                   if        v1ccvb=*blanks
138600060203     c                   clear                   dslr66
138700060203     c                   endif
138800060609     c                   movel     blplnp        w003a             3
138900060202     c     w003a         lookup    $pog                                   31
139000060202     c* se non trovato e causale variaz.bolle anche per p.o. cliente
139100060202     c*  controllo il p.o. cliente nella skiera POG
139200060202     c                   if        not *in31 and d66pocl='S'
139300060202     c                   movel     blpksc        w003a
139400060202     c     w003a         lookup    $pog                                   31
139500060202     c                   endif
139600060202     c                   endif
139700060202     c
139800020614     c                   if        not *in31
139900020614     c                   eval      *in44 = *on
140000020614     c                   eval      *in28 = *on
140100020614     c                   eval      *in90 = *on
140200020614     c                   eval      v1cmsg = msg(28)
140300020614     c                   goto      endctr
140400020614     c                   endif
140500020614     c                   endif
140600901115     C*
140700901115     C                   END
140800010219      ***
140900010219      * N U M E R O   O R M
141000010219      ***
141100010219     C     SEL4          IFNE      0
141200010219     C     SEL2          ANDEQ     0
141300010219      * P.O. EMISSIONE
141400010219     C                   Z-ADD     V1CPOE        NUM3
141500010219     C     NUM3          CHAIN     AZORG01L                           54
141600010219     C     *IN54         IFEQ      *ON
141700010219     C                   MOVEL     MSG(25)       V1CMSG
141800010219     C                   SETON                                        28  90
141900010219     C                   GOTO      ENDCTR
142000010219     C                   ENDIF
142100010219      * N.ORM
142200010219     C     KFORM         CHAIN     FNORM01L                           56
142300010219     C     *IN56         IFEQ      *ON
142400010219     C                   MOVEL     MSG(26)       V1CMSG
142500010219     C                   SETON                                        28  90
142600010219     C                   GOTO      ENDCTR
142700010219     C                   ENDIF
142800010219      *
142900010219     C                   SETON                                        06
143000010219      *
143100010219     C                   ENDIF
143200941222     C*
143300020429     C* SE SONO secondo livello POSSO MANUTENZIONE SOLO LE MIE BOLLE
143400060202    1C** 97simtpp        IFEQ      '2'
143500060202    1C**   simtpp        orEQ      *blanks
143600941222     C*
143700060202    2C**   V1CLP1        IFNE      SAVMUS
143800060202     C**   V1CLP1        ANDNE     0
143900941222     C*
144000060202     C**   V1CLP2        ORNE      SAVMUS
144100060202     C**   V1CLP2        ANDNE     0
144200060202     C**                 MOVEL     MSG(21)       V1CMSG
144300060202     C**                 SETON                                        284590
144400060202     C**                 GOTO      ENDCTR
144500060202    2C**                 ENDIF
144600060202    1C**                 ENDIF
144700991222     C* SE RIFERIMENTO PARTNER ESTERO DPD-PARCEL- DEVE ESSERE COMPOSTO
144800060608     C*  SOLTANTO DA NUMERI se<=12
144900060608     C*  alfanumerico       se=14
145000050121     c                   clear                   wnodpd            1
145100060609    1c                   if        lnpntw = 'DPD' or lnantw = 'DPD'  or
145200050304     c                             lnpntw = *blanks or lnantw = *blanks
145300060609    2c                   if        v1crpp<>*blanks
145400060609    3c                   select
145500060609     c* ok se lungo 14
145600060609     c                   when      %subst(v1crpp:15:1)=' '  and
145700060609     c                             %subst(v1crpp:14:1)<>' '
145800060609     c* ok se lungo 11 ma tutto numerico
145900060609     c                   when      %subst(v1crpp:12:4)=*blanks and
146000060609     c                             %subst(v1crpp:11:1)<>' '
146100060609     C                   MOVEA     V1CRPP        K11
146200000328     C                   Z-ADD     1             YY
146300060609     c
146400060609    4C                   DO        11            YY
146500060609     C     K11(YY)       LOOKUP    CNM                                    30
146600060609    5C     *IN30         IFEQ      *OFF
146700060609     c* Se non è tutto numerico non è DPD
146800060609     c                   eval      wnodpd = '1'
146900060609     c                   leave
147000060609    5c                   endif
147100060609    4c                   enddo
147200060609   x3c                   other
147300060609     c
147400060609     c* se non è lungo 11 o 14 non è DPD
147500060609     c                   eval      wnodpd = '1'
147600060609    3c                   endsl
147700060608     c
147800060609    3c                   if        wnodpd='1'
147900060609    4c     lnantw        ifeq      'DPD'
148000050304     c     lnpntw        oreq      'DPD'
148100060609     C                   MOVEL     MSG(24)       V1CMSG
148200991222     C                   SETON                                        284790
148300050519     C                   GOTO      ENDCTR
148400060609    4C                   ENDIF
148500060609    3C                   ENDIF
148600060609     c
148700060609    2C                   ENDif
148800061116   x1c                   else
148900061116     c                   eval      wnodpd='1'
149000991222    1C                   ENDIF
149100940316      *
149200940727      * DEVO CONSIDERARE DELLE PRIORITA' PER LE SELEZIONI IMMESSE DALL'UTENTE:
149300010219      *    1°    N.ORM                   ( 06 ON )
149400010219      *    2°    CODICE "CHI SONO"       ( 59 ON )
149500010219      *    3°    RIFERIMENTO MITTENTE    ( 12 ON )
149600000831      *          RIFERIMENTO PARTNER     ( 13 ON )
149700010219      *    4°    DESTINATARIO            ( 14 ON )
149800010219      *    5°    MITTENTE                ( 11 ON )
149900010219      *    6°    LINEA DI ARRIVO         ( 01 ON )
150000010219      *    7°    LINEA DI PARTENZA       ( 02 ON )
150100940727      *
150200010219     C     *IN06         IFEQ      *ON
150300010219     C                   SETOFF                                       59
150400010219     C                   SETOFF                                       121314
150500010219     C                   SETOFF                                       110102
150600010219     C                   ENDIF
150700010219      *
150800000831     C     *IN59         IFEQ      *ON
150900000831     C                   SETOFF                                       121314
151000000831     C                   SETOFF                                       110102
151100000831     C                   ENDIF
151200951011     C   13
151300951011     COR 12              SETOFF                                       141101
151400951011     C   13
151500951011     COR 12              SETOFF                                       02
151600940727     C   14              SETOFF                                       110102
151700940727     C   11              SETOFF                                       0102
151800940727     C   01              SETOFF                                       02
151900991214     C* SE LNP DPD DEVO CALCOLARE IL CHEK DIGITS E PORTARE IL NUMERO A
152000060609     C*  12 CIFRE PER LA RICERCA, se immesso lungo 11
152100991214     C                   CLEAR                   KRMN
152200060609     C
152300060609    1c                   if        lnpntw = 'DPD'  and v1crmn>0
152400991214     C                   MOVEL     V1CRMN        W004A             4
152500991214     C* SE CI SONO PIU' DI 11 NUMERO, NON CALCOLO IL CHKD
152600991222    3C     W004A         IFEQ      '0000'
152700991214     C                   CLEAR                   DPDBRC
152800991214     C                   MOVE      V1CRMN        W0110            11 0
152900991214     C                   MOVEL     W0110         DPDBRC
153000991214     C                   EXSR      M1031
153100991214     C                   Z-ADD     DPDBRC        KRMN
153200991214     C                   ELSE
153300991214     C* 12 NUMERI
153400991214     C                   Z-ADD     V1CRMN        KRMN
153500991222    3C                   ENDIF
153600991214     C***
153700991214     C                   ELSE
153800991214     C                   Z-ADD     V1CRMN        KRMN
153900991222    1C                   ENDIF
154000991222     C**
154100991222     C* SE LNA DPD DEVO CALCOLARE IL CHEK DIGITS E PORTARE IL NUMERO A
154200060518     C*  12 CIFRE PER LA RICERCA --> ora non più: il check digit per la
154300060609     c*  ricerca non serve più perchè campo a parte nella DBL4I
154400050304    1c                   if        v1crpp <> *blanks and (lnantw='DPD'
154500050304     c                             or lnpntw = 'DPD' or wnodpd=' ')
154600060518     C                   MOVEL     V1CRPP        krpp
154700060518     C****               MOVE      V1CRPP        W004A             4
154800060518     C****               MOVEL     V1CRPP        W011A            11
154900991222     C* SE CI SONO PIU' DI 11 NUMERI, NON CALCOLO IL CHKD
155000060518    2C***  W004A         IFEQ      '    '
155100060518     C***  W011A         ANDGE     *ZEROS
155200060518     C***                CLEAR                   DPDBRC
155300060518     C***                MOVEL     V1CRPP        W0110            11 0
155400060518     C***                MOVEL     W0110         DPDBRC
155500060518     C***                EXSR      M1031
155600060518     C***                Z-ADD     DPDBRC        KRPP
155700060518     C***                ELSE
155800991222     C* 12 NUMERI
155900060518     C***                MOVEL     V1CRPP        KRPP
156000060518    2C***                ENDIF
156100991222     C***
156200991222    1C                   ENDIF
156300900514     C*
156400900514     C     ENDCTR        ENDSR
156500940317      **************************************************************************
156600940317      *    ELABORA BOLLE DA VISUALIZZARE
156700940317      **************************************************************************
156800900515     C     ELAB          BEGSR
156900940317      *
157000901115     C* NUMERO DI SPEDIZIONE
157100930701    1C     SEL2          IFNE      0
157200941010     C* SONO IN INTERROGAIZONE BOLLE
157300941010    2C     *IN97         IFEQ      *OFF
157400941214     C                   MOVEL     V1CAAS        PA0AAS
157500941214     C                   MOVEL     V1CLP2        PA0LNP
157600941214     C                   MOVEL     V1CNRS        PA0NRS
157700941214     C                   MOVEL     V1CNSP        PA0NSP
157800941214     C  N20              MOVEL     'A'           PA0FLG
157900941214     C   20              MOVEL     ' '           PA0FLG
158000941214     C                   MOVEL     V1CRSU        PA0RSU
158100941214     C                   MOVEL     PARAM0        KPJBU
158200040409if  1c                   if        SDSprm = 1
158300940729     C                   CALL      'FNLS05R'
158400901115     C                   PARM                    KPJBA
158500040409x   1c                   else
158600040409     C                   call      'FNLS05R'
158700040409     C                   parm                    KPJBA
158800040409     C                   parm                    FIDNA1ds
158900040409e   1c                   endif
159000941010     C*
159100941010   X2C                   ELSE
159200941010     C*
159300941010     C* SONO IN MANUTENZIONE BOLLE
159400991020     C                   CLEAR                   DBLP
159500941010     C* SE BOLLA NON PARTITA RICHIAMO IL PGM FNLS01R
159600941010    3C     WFT1          IFEQ      ' '
159700941010     C                   MOVEL     BLPAAS        §LPAAS
159800941010     C                   MOVEL     BLPLNP        §LPLNP
159900941010     C                   MOVEL     BLPNRS        §LPNRS
160000941010     C                   MOVEL     BLPNSP        §LPNSP
160100941010     C                   MOVEL     'M'           §LPFPR
160200030612     C*                  MOVEL     PARAM4        KPJBU
160300941010     C                   CALL      'FNLS01R'
160400941010     C                   PARM                    KPJBA
160500991020     C                   PARM                    DBLP
160600991020     C                   PARM                    DTASV
160700030612     C                   PARM                    trul90ds
160800941115     C*
160900941010     C                   MOVEL     '1'           PA1S01            1
161000941010     C*
161100941010   X3C                   ELSE
161200941010     C*
161300941222     C* SE BOLLA PARTITA RICHIAMO PGM FNLR48R
161400941222     C                   CLEAR                   DSLR48
161500941222     C                   MOVEL     BLPAAS        D48AAS
161600941222     C                   MOVEL     BLPLNP        D48LNP
161700941222     C                   MOVEL     BLPLNP        D48FGS
161800941222     C                   MOVEL     BLPNRS        D48NRS
161900941222     C                   MOVEL     BLPNSP        D48NSP
162000941222     C                   MOVEL     BLPCBO        D48CBO
162100941222     C                   MOVEL     'P'           D48TBO
162200941222     C                   MOVEL     V1CCVB        D48CVB
162300950607     C                   MOVEL     'S'           D48F12
162400941222     C                   MOVEL     DSLR48        KPJBU
162500941222     C                   CALL      'FNLR48R'
162600941222     C                   PARM                    KPJBA
162700941222     C                   PARM                    DSARBD
162800941222     C                   PARM                    DSARBK
162900991012     C                   PARM                    DARBT
163000941222     C                   PARM                    DSARBG
163100030612     C                   PARM                    trul90ds
163200941222     C*
163300941222     C                   MOVEL     KPJBU         DSLR48
163400941222     C* ERRORE
163500941222     C     D48ERR        IFNE      *BLANKS
163600941222     C                   MOVEL     D48MSG        V1CMSG
163700941222     C                   SETON                                        284590
163800941222    4C                   ENDIF
163900941010     C*
164000941010    3C                   ENDIF
164100941010    2C                   ENDIF
164200901115     C*
164300940324   X1C                   ELSE
164400901115     C*
164500901115     C* DATA SPEDIZIONE
164600000831    2C     SEL1          IFNE      *ZERO
164700000831     C     SEL3          ORNE      *BLANKS
164800000831     C     V1CNOT        ORNE      *BLANKS
164900010219     C     SEL4          ORNE      *ZEROS
165000940727     C                   Z-ADD     1             REC2
165100900517     C*
165200901116     C                   MOVEL     *BLANKS       TSP
165300940728     C                   MOVEL     V1CTSP        TSP               1
165400940316      *
165500940727      * POSIZIONO IL FILE A SECONDA DELLA SELEZIONE VIDEO DELL'UTENTE
165600940727     C                   SELECT
165700010219      * N.ORM
165800010219     C     *IN06         WHENEQ    *ON
165900010219     C                   MOVEL     'M'           WTRC
166000010219     C                   MOVEL     SEL4          WN14
166100060217     C     Kar43         SETLL     Fiar404L
166200940727      *
166300000831     C     *IN59         WHENEQ    *ON
166400051013     C     KarS          SETLL     FIarS02L
166500000831      *
166600940727     C     *IN12         WHENEQ    *ON
166700940727     C     *IN16         IFEQ      *ON
166800010718     C     BLP33K        SETLL     FNBLP033
166900940727     C                   ELSE
167000010718     C     KRMN          SETLL     FNBLP033
167100940727     C                   ENDIF
167200940727      *
167300951011     C     *IN13         WHENEQ    *ON
167400050121     c                   clear                   wgiro             1
167500050304     c                   if        lnantw <> 'DPD' and lnpntw <> 'DPD'
167600991222     C                   MOVEL     'E'           WTRC
167700971212     C                   MOVEL(P)  V1CRPP        WN30             30
167800991222     C                   ELSE
167900010219     C* D P D
168000060518     C****               MOVEL     'F'           WTRC
168100060518     C                   MOVEL     'I'           WTRC
168200991222     C                   MOVEL(P)  KRPP          WN30             30
168300991222    1C                   ENDIF
168400991222     C*
168500060217     C     Kar4          SETLL     Fiar403L
168600951011      *
168700940727     C     *IN14         WHENEQ    *ON
168800940727     C                   Z-ADD     COMAAD        $AAS
168900940727     C                   Z-ADD     COMMGD        $MGS
169000021111     c                   if        v1clp1 <> *zeros
169100021111     c                   eval      klp1 = v1clp1
169200021111     c                   endif
169300021111     c                   if        v1clp1 = *zeros
169400021111     c                   eval      ww = 1
169500021111     c                   eval      klp1 = l1(WW)
169600021111     c                   endif
169700010718     C     BLP43K        SETLL     FNBLP043
169800940727      *
169900940727     C     *IN11         WHENEQ    *ON
170000940727     C                   Z-ADD     COMAAD        $AAS
170100940727     C                   Z-ADD     COMMGD        $MGS
170200021111     c                   if        v1clp1 <> *zeros
170300021111     c                   eval      klp1 = v1clp1
170400021111     c                   endif
170500021111     c                   if        v1clp1 = *zeros
170600021111     c                   eval      ww = 1
170700021111     c                   eval      klp1 = l1(WW)
170800021111     c                   endif
170900010718     C     BLP44K        SETLL     FNBLP044
171000940316      *
171100940316      * LETTURA PER LNA DATA SPEDIZ. TIPO SPEDIZIONE
171200940727     C     *IN01         WHENEQ    *ON
171300940316     C     *IN16         IFEQ      *ON
171400020627     c                   if        v1clp1 = 999
171500010718     C     KLNA1         SETLL     FNBLP026
171600020627     c                   endif
171700020627     c                   if        v1clp1 <> *zeros and v1clp1 <> 999
171800020627     c                   eval      klp1 = v1clp1
171900020627     c     klna3         setll     fnblp076
172000020627     c                   endif
172100020627     c                   if        v1clp1 = *zeros
172200020627     c                   eval      ww = 1
172300020627     c                   eval      klp1 = l1(WW)
172400020627     c     klna3         setll     fnblp076
172500020627     c                   endif
172600940316     C                   ELSE
172700940316      * LETTURA SOLO PER LNA
172800020614     c                   if        v1clp1 = 999
172900010718     C     V1CLNA        SETLL     FNBLP026
173000020614     c                   endif
173100020614     c                   if        v1clp1 <> *zeros and v1clp1 <> 999
173200020614     c                   eval      klp1 = v1clp1
173300020627     c     klna4         setll     fnblp076
173400020614     c                   endif
173500020614     c                   if        v1clp1 = *zeros
173600020614     c                   eval      ww = 1
173700020614     c                   eval      klp1 = l1(WW)
173800020627     c     klna4         setll     fnblp076
173900020614     c                   endif
174000940316     C                   ENDIF
174100940928     C*
174200901116     C* LETTURA PER LNP DATA SPEDIZ. SERIE
174300940727     C     *IN02         WHENEQ    *ON
174400010718     C     KLNP2         SETLL     FNBLP031
174500940316      *
174600940727     C                   ENDSL
174700940316      *
174800930630     C** CARICO SUBFILE
174900930630     C                   Z-ADD     0             COMOD
175000930630     C                   EXSR      CARSF
175100940324    2C                   END
175200010219      *
175300940324    1C                   END
175400900518     C*
175500901116     C                   ENDSR
175600940317      **************************************************************************
175700940317      *    CARICO SUBFILE IN AVANTI
175800940317      **************************************************************************
175900900515     C     CARSF         BEGSR
176000900517     C*
176100900518     C                   Z-ADD     1             B                 2 0
176200940727     C                   Z-ADD     COMOD         NRR2
176300991222    1C     B             DOWLE     18
176400051013     c                   setoff                                       32
176500941010     C*
176600010219      * N.ORM
176700010219    2C     *IN06         IFEQ      *ON
176800060217     C     Kar43         READE     Fiar404L                               30
176900010219     C  N30KBLP4         CHAIN     FNBLP01L                           33
177000010219    3C  N30*IN33         IFEQ      *OFF
177100020614      * se sono in manutenzione
177200020614  b4 c                   if        *in97
177300051013     c
177400020614      * la linea di partenza della bolla deve far parte del mio terminal di
177500060202      * partenza se bolla non partita
177600060202     c                   if        blpft1=' '
177700020614     c     blplnp        lookup    l1                                     31
177800060203     c                   else
177900060203     C                   SETON                                        31
178000060202     c***                movel     blplnp        w003a
178100060202     c***  w003a         lookup    $pog                                   31
178200060202     c                   endif
178300060202     c
178400020614  b5 c                   if        not *in31
178500020614     c                   eval      *in32 = *on
178600020614  e5 c                   endif
178700020614  e4 c                   endif
178800010219   X3C                   ELSE
178900010219     C                   SETON                                        32
179000010219    3C                   ENDIF
179100010219    2C                   ENDIF
179200000831      *
179300000831    2C     *IN59         IFEQ      *ON
179400051013     C     KarS          READE     FIarS02L                               30
179500051013     c                   if        not *in30
179600090226     C     KarSXT        CHAIN     Fiart27L                           33
179700090226     C  N33KarTXP        CHAIN     FNBLP01L                           33
179800051013    3C     *IN33         IFEQ      *OFF
179900000831   X3C                   ELSE
180000000831     C                   SETON                                        32
180100000831    3C                   ENDIF
180200000831    2C                   ENDIF
180300051013    2C                   ENDIF
180400940316      *
180500010718     C   12KRMN          READE     FNBLP033                               30
180600940316      *
180700991222    2C     *IN13         IFEQ      *ON
180800060217     c     leggar4       tag
180900991222     C*
181000050304     c                   if        lnantw <> 'DPD' and lnpntw <> 'DPD'
181100050304     c                             and wgiro <> '2'
181200060217     C     Kar4          READE     Fiar403L                               30
181300991222     C* DPD
181400991222   X3C                   ELSE
181500060217     C     WTRC          READE     Fiar403L                               30
181600060518     c
181700060518     C  N30              MOVEL     ar4N30        DSBL4i
181800060609     c  n30              if        krpp <> §b4ipn
181900060518     c                   seton                                        30
182000060518     c                   endif
182100991222    3C                   ENDIF
182200050121     c*
182300050304     c                   if        *in30 and wgiro <> '2' and
182400050304     c                             lnpntw <> 'DPD' and lnantw <> 'DPD' and
182500050304     c                             wnodpd = *blanks
182600050121     c                   eval      wgiro = '2'
182700060518     C                   MOVEL     'I'           WTRC
182800050121     C                   MOVEL(P)  KRPP          WN30             30
182900060217     C     Kar4          SETLL     Fiar403L
183000060217     c                   goto      leggar4
183100050121     c                   endif
183200061116     c*
183300061116     c* Se rif.partner può essere DPD non export e immesso rif. lungo 11
183400061116     c* faccio tentaivo di ricerca con rif.truccato
183500061116     c                   if        *in30 and lnantw<>'DPD'and wnodpd=*blanks
183600061116     c                             and %subst(krpp:12:1)=' '
183700061116     C                   eval      krpp='0'+%subst(krpp:1:3)+'99'
183800061116     c                             +%subst(krpp:4:8)
183900061116     C                   MOVEL(P)  KRPP          WN30             30
184000061116     C     Kar4          SETLL     Fiar403L
184100061116     c                   goto      leggar4
184200061116     c                   endif
184300991222     C**
184400991222     C  N30KBLP4         CHAIN     FNBLP01L                           33
184500991222    3C  N30*IN33         IFEQ      *OFF
184600951011     C                   ELSE
184700951011     C                   SETON                                        32
184800991222    3C                   ENDIF
184900991222    2C                   ENDIF
185000951011      *
185100021111      * lettura per ragione sociale destinatario
185200021111     c                   if        *in14
185300021111     C     Klp1          READE     FNBLP043                               30
185400021111      * leggo un'altra linea di partenza
185500021111     c                   if        *in30 and v1clp1 = *zeros
185600021111     c                   eval      *in30 = *off
185700021111     c                   add       1             ww
185800021111     c                   if        l1(ww) = *zeros
185900021111     c                   eval      *in30 = *on
186000021111     c                   else
186100021111     c                   eval      klp1 = l1(ww)
186200021111     C                   Z-ADD     COMAAD        $AAS
186300021111     C                   Z-ADD     COMMGD        $MGS
186400021111     c     blp43k        Setll     FNBLP043
186500021111     c     Klp1          READE     FNBLP043                               30
186600021111     c                   endif
186700021111     c                   endif
186800021111     c                   endif
186900940316      *
187000021111      * lettura per ragione sociale mittente
187100021111     c                   if        *in11
187200021111     C     Klp1          READE     FNBLP044                               30
187300021111      * leggo un'altra linea di partenza
187400021111     c                   if        *in30 and v1clp1 = *zeros
187500021111     c                   eval      *in30 = *off
187600021111     c                   add       1             ww
187700021111     c                   if        l1(ww) = *zeros
187800021111     c                   eval      *in30 = *on
187900021111     c                   else
188000021111     c                   eval      klp1 = l1(ww)
188100021111     C                   Z-ADD     COMAAD        $AAS
188200021111     C                   Z-ADD     COMMGD        $MGS
188300021111     c     blp44k        Setll     FNBLP044
188400021111     c     Klp1          READE     FNBLP044                               30
188500021111     c                   endif
188600021111     c                   endif
188700021111     c                   endif
188800940316      *
188900020614      * lettura per linea di arrivo
189000020614     c                   if        *in01
189100020614      *  richiesta lnp 999
189200020614     c                   if        v1clp1 = 999
189300020614     C     V1CLNA        READE     FNBLP026                               30
189400020614     c                   endif
189500020614      *  richiesta lnp a 0 o una linea di partenza specifica
189600020614     c                   if        v1clp1 <> 999
189700020627     c     klna4         reade     fnblp076                               30
189800020627      * leggo un'altra linea di partenza
189900020627     c                   if        *in30 and v1clp1 = *zeros
190000020627     c                   eval      *in30 = *off
190100020614     c                   add       1             ww
190200020614     c                   if        l1(ww) = *zeros
190300020614     c                   eval      *in30 = *on
190400020627     c                   else
190500020614     c                   eval      klp1 = l1(ww)
190600020627     c     klna4         setll     fnblp076
190700021205     c     klna4         reade     fnblp076                               30
190800020614     c                   endif
190900020614     c                   endif
191000020627
191100020614     c                   endif
191200020614     c                   endif
191300900517     C*
191400010718     C   02V1CLP1        READE     FNBLP031                               30
191500060217     c  N32
191600051013     CanN30              EXSR      CONFR1
191700010718     C  N30
191800010718     CANN32              EXSR      EMISS1
191900020627      * se lettura per lna leggo un'altra linea di partenza
192000021111      * o per rag.soc. destinatario o rag.soc. mittente
192100021111     c                   if        (*in01 or *in11 or *in14) and
192200021111     c                              *in30 and V1clp1 = *zeros
192300020627     c                   eval      *in30 = *off
192400020627     c                   add       1             ww
192500020627     c                   if        l1(ww) = *zeros
192600020627     c                   eval      *in30 = *on
192700020627     c                   else
192800020627     c                   eval      klp1 = l1(ww)
192900021111     c                   if        *in01
193000020627     c     klna4         setll     fnblp076
193100020627     c     klna4         reade     fnblp076
193200021111     c                   endif
193300021111     c                   if        *in11
193400021111     C                   Z-ADD     COMAAD        $AAS
193500021111     C                   Z-ADD     COMMGD        $MGS
193600021111     c     blp44k        Setll     FNBLP044
193700021111     c                   endif
193800021111     c                   if        *in14
193900021111     C                   Z-ADD     COMAAD        $AAS
194000021111     C                   Z-ADD     COMMGD        $MGS
194100021111     c     blp43k        Setll     FNBLP043
194200021111     c                   endif
194300020627     c                   endif
194400020627     c                   endif
194500020627
194600900515     C  N30              END
194700940727     C                   Z-ADD     NRR2          COMOD             4 0
194800900516     C*
194900900515     C                   ENDSR
195000940727      **************************************************************************
195100940727      *  Riposizionamento (solo per data) per letture su bolle
195200940727      **************************************************************************
195300940727     C     RIPOS         BEGSR
195400940727      *
195500010718     C                   Z-ADD     BLPDSP        WDSP
195600950216     C*
195700950216     C     COMDSA        IFGT      WDSP
195800940727     C     COMDSA        OREQ      *ZERO
195900940727      *
196000940727      * Mi posiziono sul record maggiore per la stessa data bolla
196100010718     C                   Z-ADD     BLPAAS        $AAS
196200010718     C                   Z-ADD     BLPMGS        $MGS
196300021111     C   14BL43K         SETGT     FNBLP043
196400010718     C   11BL44K         SETGT     FNBLP044
196500940727      *
196600940727     C                   ELSE
196700940727     C                   MOVE      'S'           $ULTIM
196800940727     C                   ENDIF
196900940727      *
197000940727     C                   ENDSR
197100940727      **************************************************************************
197200940727      *  Riposizionamento (per data e rag.soc.) per letture su bolle
197300940727      **************************************************************************
197400940727     C     RIPOS1        BEGSR
197500940727      *
197600940727      * Mi posiziono sulla ragione sociale video
197700010718     C                   Z-ADD     BLPAAS        $AAS
197800010718     C                   Z-ADD     BLPMGS        $MGS
197900010718     C   14BLP43K        SETLL     FNBLP043
198000010718     C   11BLP44K        SETLL     FNBLP044
198100940727      *
198200940727     C                   ENDSR
198300940317      **************************************************************************
198400940317      *    CONFRONTO PER CERCARE BOLLE DA EMETTERE
198500940317      **************************************************************************
198600940928     C     CONFR1        BEGSR
198700940727      *
198800940727      * OCCHIO!!   Devo usare per primo il confronto che potrebbe portarmi a
198900940727      * fine ciclo: se interrogo per destinatario (14ON) ci deve essere prima il
199000940728      * confronto con V1CRSD, se invece sono per mittente devo fare prima quello
199100940728      * con V1CRSM.
199200940727     C                   MOVE      *BLANKS       $ULTIM            1
199300940727     C                   MOVE      *BLANKS       $SALTA            1
199400940329      *
199500940727      *??  R A G I O N E    S O C I A L E    D E S T I N A T A R I O  ?
199600940728     C     V1CRSD        IFNE      *BLANKS
199700940727     C     ZZ            SUBST(P)  BLPRSD        COMRAG           15
199800940727      * Se rag.soc. letta > video riposiziono solo per data con SETGT
199900940728     C     COMRAG        IFGT      V1CRSD
200000940727     C                   SETON                                            32
200100940727     C   14              EXSR      RIPOS
200200940727     C                   MOVE      'S'           $SALTA
200300940727     C                   ENDIF
200400940727      * Se rag.soc. letta < video riposiziono per stessa data + rag.soc.video
200500940728     C     COMRAG        IFLT      V1CRSD
200600940727     C                   SETON                                            32
200700940727     C   14              EXSR      RIPOS1
200800940727     C                   MOVE      'S'           $SALTA
200900940727     C                   ENDIF
201000940727     C                   ENDIF
201100940329     C*
201200940727      *??  R A G I O N E    S O C I A L E    M I T T E N T E  ?
201300940728     C     V1CRSM        IFNE      *BLANKS
201400940727     C     XX            SUBST(P)  BLPRSM        COMRAG           15
201500940727      * Se rag.soc. letta > video riposiziono solo per data con SETGT
201600940728     C     COMRAG        IFGT      V1CRSM
201700940727     C                   SETON                                            32
201800940727     C   11              EXSR      RIPOS
201900940727     C                   MOVE      'S'           $SALTA
202000940727     C                   ENDIF
202100940727      * Se rag.soc. letta < video riposiziono per stessa data + rag.soc.video
202200940728     C     COMRAG        IFLT      V1CRSM
202300940727     C                   SETON                                            32
202400940727     C   11              EXSR      RIPOS1
202500940727     C                   MOVE      'S'           $SALTA
202600940727     C                   ENDIF
202700940727     C                   ENDIF
202800940727      *
202900940727     C     $ULTIM        COMP      'S'                                    30
203000940727     C     $SALTA        CABEQ     'S'           ENDCON
203100940317      *
203200900518     C                   SETOFF                                       32
203300940729      *
203400000531      * SELEZIONE EVENTUALI BOLLE POSTE
203500020628     c     lnpntw        ifne      'PPT'
203600000531     C     BLPLNP        CHAIN     AZORG01L                           73
203700000531     C     *IN73         IFEQ      *OFF
203800000531     C                   MOVEL     ORGDE3        OG143
203900000531      *
204000000531     C     V1CBPT        IFEQ      'N'
204100020306     c     §ogntw        andeq     'PPT'
204200000531     C                   SETON                                            32
204300000531     C                   GOTO      ENDCON
204400000531     C                   ENDIF
204500000531      *
204600000531     C     V1CBPT        IFEQ      'P'
204700020306     c     §ogntw        andne     'PPT'
204800000531     C                   SETON                                            32
204900000531     C                   GOTO      ENDCON
205000000531     C                   ENDIF
205100000531      *
205200000531     C                   ENDIF
205300000531     C                   ENDIF
205400000531      *
205500000531      * SOLO BOLLE BORDERIZZATE O NON BORDERIZZATE
205600940729     C     V1CFTA        IFEQ      '1'
205700940729     C     BLPDBR        ANDNE     *ZERO
205800940729     C                   SETON                                            32
205900940729     C                   GOTO      ENDCON
206000940729     C                   ENDIF
206100940729     C     V1CFTA        IFEQ      '2'
206200940729     C     BLPDBR        ANDEQ     *ZERO
206300940729     C                   SETON                                            32
206400940729     C                   GOTO      ENDCON
206500940729     C                   ENDIF
206600940729     C*
206700900515     C** CONFRONTI SOLO PER LINEA ARRIVO
206800900515     C*  DATA SPEDIZIONE > DATA AL
206900940727     C   16BLPDSP        IFLT      COMDSD
207000940727     C     COMDSD        ANDNE     *ZERO
207100940727     C                   SETON                                            32
207200900515     C                   GOTO      ENDCON
207300900515     C                   END
207400940727     C   16BLPDSP        IFGT      COMDSA
207500940727     C     COMDSA        ANDNE     *ZERO
207600940727     C                   SETON                                            32
207700940727     C  N11
207800940727     CANN14              SETON                                            30
207900900518     C                   GOTO      ENDCON
208000900518     C                   END
208100020627      * controllo in base alla linea di partenza inserita
208200020627     c                   exsr      ctrlnp
208300020627     c                   if        *in32 = *on
208400020627     c                   goto      endcon
208500020627     c                   endif
208600940317      *
208700940729      * VERIFICO ANCHE LA LNA POICHE' POTREI AVER FATTO ALTRE SELEZIONI PRIMARIE
208800940728     C     BLPLNA        IFNE      V1CLNA
208900940728     C     V1CLNA        ANDNE     *ZERO
209000940727     C                   SETON                                            32
209100940727     C                   GOTO      ENDCON
209200940727     C                   END
209300940317      *
209400961118     C* RESO / FRANCO FRONTIERA
209500961118     C     V1CFBR        IFNE      ' '
209600961118     C                   MOVEL     V1CFBR        WFBR
209700961118     C     V1CFBR        IFEQ      'R'
209800961118     C                   MOVEL     'S'           WFBR
209900961118     C                   ENDIF
210000961118     C     BLPFBR        IFNE      WFBR
210100961118     C                   SETON                                            32
210200961118     C                   GOTO      ENDCON
210300961118     C                   ENDIF
210400961118     C                   ENDIF
210500900515     C* TIPO SPEDIZIONE
210600040216     ***C   04BLPTSP        IFNE      TSP
210700040216     C                   IF            TSP    <> *blanks
210800040216     C                             and BLPtsp <> TSP
210900900515     C                   SETON                                            32
211000900515     C                   GOTO      ENDCON
211100900515     C                   END
211200040216      * Tipo bolla
211300040216     c                   if            V1Ccbo <> *blanks
211400040216     c                             and V1Ccbo <> BLPcbo
211500040216     c                   eval      *in32 = *on
211600040216     c                   goto      EndCon
211700040216     c                   endif
211800900518     C* SERIE DAL + AL
211900940728     C     BLPNRS        IFLT      V1CPSD
212000900518     C                   SETON                                            32
212100900518     C                   GOTO      ENDCON
212200900518     C                   END
212300940728     C     BLPNRS        IFGT      V1CPSA
212400900515     C                   SETON                                            32
212500900515     C                   GOTO      ENDCON
212600900515     C                   END
212700900518     C* SEGNACOLLO DAL SOLO
212800930630     C     *IN07         IFEQ      *ON
212900930630     C     *IN08         ANDEQ     *OFF
213000090226     C     KBLP2         SETLL     Fiart01L                               31
213100930630     C  N31              SETON                                        32
213200930630     C  N31              GOTO      ENDCON
213300930630     C                   END
213400900515     C* SEGNACOLLO DAL + AL
213500930630     C     *IN08         IFEQ      *ON
213600910827     C                   SETON                                        32
213700090226     C     KBLP          SETLL     Fiart01L
213800090226     C     KBLP          READE     Fiart01L                               31
213900910827     C     *IN31         DOWEQ     '0'
214000090226     C     arTNSC        IFGE      V1CSCD
214100090226     C     arTNSC        ANDLE     V1CSCA
214200910827     C                   SETON                                        31
214300910827     C                   SETOFF                                       32
214400940923     C                   ENDIF
214500910827     C*
214600090226     C  N31KBLP          READE     FiarT01L                               31
214700940923     C                   ENDDO
214800910827     C*
214900910827     C   32              GOTO      ENDCON
215000940923     C                   ENDIF
215100940923     C* FILIALE SEGNACOLLO
215200940923     C     V1CFLS        IFNE      *ZERO
215300940923     C     V1CFLS        ANDNE     BLPFLS
215400940923     C                   SETON                                            32
215500940923     C                   GOTO      ENDCON
215600940926     C                   ENDIF
215700940316     C* CODICE CLIENTE
215800950504     C     V1CKSD        IFNE      *ZERO
215900950504     C     BLPKSC        IFNE      V1CKSD
216000900518     C                   SETON                                            32
216100900518     C                   END
216200950504     C* CONTROLLO PER ASSEGNATI IL COD CLIENTE MITTENTE
216300950504     C   32BLPCCM        IFEQ      V1CKSD
216400950504     C                   SETOFF                                           32
216500950504     C                   ENDIF
216600950504     C*
216700950504     C   32              GOTO      ENDCON
216800950504     C                   ENDIF
216900940923     C*
217000940923     C* SELEZIONE PER COLLI SOLO A TERRA O TUTTI
217100940923     C* BOLLE A TERRA:       DSP <= DATE RICHIESTE (SE CI SONO)
217200940923     C*                 AND  DUC = 0  OPPURE  DUC > DSP
217300940923     C     V1CTER        IFEQ      'T'
217400940923     C                   MOVEL     *BLANKS       WTERRA            1
217500950213     C*
217600950228     C* ESCLUDO TUTTE LA BOLLE SENZA COLLI
217700950228     C     BLPNRS        IFEQ      *ZERO
217800950228     C     BLPNCD        ANDEQ     *ZERO
217900950228     C     BLPNCA        ANDEQ     *ZERO
218000950228     C     BLPFNS        ANDEQ     *BLANKS
218100950213     C                   SETON                                            32
218200950213     C                   GOTO      ENDCON
218300950213     C                   ENDIF
218400940923     C*
218500940923     C     BLPDUC        IFEQ      *ZERO
218600940923     C     BLPDUC        ORGT      BLPDSP
218700940923     C                   MOVEL     'S'           WTERRA
218800940923     C                   ENDIF
218900940923     C*
219000950206     C* SE RICHIESTE SOLO LE BOLLE A TERRA, ESCLUDO I RECUPERI
219100950206     C     BLPCBO        LOOKUP    REC                                    31
219200950206     C     *IN31         IFEQ      *ON
219300950206     C                   SETON                                            32
219400950206     C                   GOTO      ENDCON
219500950206     C                   ENDIF
219600940923     C*
219700940923     C* SE IL FLAG E' ANCORA VUOTO LA SPEDIZIONE NON E' A TERRA
219800940923     C     WTERRA        IFEQ      *BLANKS
219900940923     C                   SETON                                            32
220000940923     C                   GOTO      ENDCON
220100940923     C                   ENDIF
220200940923     C*
220300940923     C                   ENDIF
220400940322      *
220500940729      * Se il pgm e' richiamato da un cliente (PA3CLI='S'), devo caricare
220600940727      * solo i clienti presenti nella schiera CLI (caricata inizialmente)
220700940729     C     PA3CLI        IFEQ      'S'
220800940727     C     BLPKSC        LOOKUP    CLI                                    31
220900940727     C  N31              SETON                                            32
221000940727     C  N31              GOTO      ENDCON
221100940727     C                   ENDIF
221200930406     C*
221300930406     C* SPEDIZIONI DA CONSEGNARE
221400041028     c* chian su arb se c'e'
221500041028     c                   if        *in22 or *in23
221600041028     c     kblp          chain     fnarb01l
221700041028     c                   if        %found(fnarb01l)
221800041028     c                   eval      wdcm=arbdcm
221900041028     c                   else
222000041028     c                   eval      wdcm=blpdcm
222100041028     c                   endif
222200041028     c                   endif
222300041028     c
222400041028     C   22wdcm          IFNE      0
222500930406     C                   SETON                                            32
222600930406     C                   GOTO      ENDCON
222700930406     C                   END
222800930406     C* SPEDIZIONI CONSEGNATE
222900041028     C   23wdcm          IFEQ      0
223000930406     C                   SETON                                            32
223100930406     C                   GOTO      ENDCON
223200930406     C                   END
223300900515     C* RIFERIMENTI MITTENTE NUMERICI
223400991214     C     BLPRMN        IFNE      KRMN
223500991214     C     KRMN          ANDNE     *ZERO
223600900515     C                   SETON                                            32
223700900515     C                   GOTO      ENDCON
223800900515     C                   END
223900990118     C* CONTROLLO CA SE RICHIESTO
224000990118     C     V1CDAN        IFNE      ' '
224100990118     C                   EXSR      CONFCA
224200990118     C   32              GOTO      ENDCON
224300990118     C                   ENDIF
224400030527     C*
224500030527     C* Inserisco qui i controlli x verificare la data limite x considerare le
224600030527     C* bolle PT (tale controllo nn viene effettuato se l'utente è del gruppo
224700030527     C* EDP o ISP).
224800030527     C                   if        %subst(KNMUS:1:3) = 'EDP' OR
224900030527     C                             %subst(KNMUS:1:3) = 'ISP'
225000030527     C                   else
225100030527     C     blpLNP        lookup    SKPOPT                                 77
225200030527     C                   if        *in77
225300030527     C                   if        blpDCM >  0
225400030527     C                   if        blpDCM >= wDataLimitePT
225500030527     C                   else
225600030527     C                   seton                                        32
225700030527     C                   GOTO      ENDCON
225800030527     C                   endif
225900030527     C                   endif
226000030527     C                   endif
226100030527     C                   endif
226200030527     C*
226300940317      *
226400900515     C     ENDCON        ENDSR
226500020627      *------------------------------------------------------------------------*
226600020627      *  Controllo la bolla letta in base alla linea di partenza
226700020627      *------------------------------------------------------------------------*
226800020627     c     ctrlnp        begsr
226900020627
227000020627      * se richiesta lnp a 0 vuol dire tutte le bolle del terminal di partenza
227100020627     c                   if        v1clp1 = *zeros
227200020627     c     blplnp        lookup    l1                                     31
227300020627     c                   if        not *in31
227400020627     c                   eval      *in32 = *on
227500020627     c                   endif
227600020627     c                   endif
227700020627      * se richiesta una lnp specifica la bolla deve essere di quella linea di
227800020627      * partenza
227900020627     c                   if        v1clp1 <> *zeros  and v1clp1 <> 999
228000020627     c                             and blplnp <> v1clp1
228100020627     c                   eval      *in32 = *on
228200020627     c                   endif
228300020627      * se richiesta lnp a 999 vuol dire tutte le bolle presenti
228400020627      * quindi non controllo
228500020627
228600020627     c                   endsr
228700990118      **************************************************************************
228800990118      * CONTROLLO CA APERTE E CHIUSE
228900990118      **************************************************************************
229000990118     C     CONFCA        BEGSR
229100990118     C                   SETON                                        32
229200040810     C**""*KBLP          CHAIN     FNDCT02L                           31
229300040810     C**""**IN31         DOWEQ     *OFF
229400990118     C* SOLO LE BUONE
229500040810     C**""*DCTATB        IFEQ      ' '
229600990118     C* APERTA
229700040810     C**""*DCTDCH        IFEQ      0
229800040810     C**""*V1CDAN        ANDEQ     'A'
229900040810     C**""*              SETOFF                                       32
230000040810     C**""*              SETON                                        31
230100040810     C**""*              ENDIF
230200990118     C* CHIUSE
230300040810     C**""*DCTDCH        IFGT      0
230400040810     C**""*V1CDAN        ANDEQ     'C'
230500040810     C**""*              SETOFF                                       32
230600040810     C**""*              SETON                                        31
230700040810     C**""*              ENDIF
230800990118     C** CA APERTA O CHIUSA
230900040810     C**""*V1CDAN        IFEQ      'E'
231000040810     C**""*              SETOFF                                       32
231100040810     C**""*              SETON                                        31
231200040810     C**""*              ENDIF
231300040810     C**""*              ENDIF
231400990118     C**
231500040810     C**""* N31KBLP          READE     FNDCT02L                               31
231600040810     C**""*              ENDDO
231700040810      *
231800040810      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
231900040810      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
232000040810      * legate alla spedizione che passo
232100040810     c                   clear                   fidn12ds
232200040810     c                   eval      i12aas = BLPAAS
232300040810     c                   eval      i12lnp = BLPLNP
232400040810     c                   eval      i12nrs = BLPNRS
232500040810     c                   eval      i12nsp = BLPNSP
232600040810     c                   eval      i12fel = 'E'
232700040810     c                   eval      i12fan = 'E'
232800040810     c*
232900040810     c                   call      'FIDN12R'
233000040810     c                   parm                    fidn12ds
233100040810     c*
233200040810     c                   setoff                                       31
233300040810     c*
233400040810     c* se non ci sono errori
233500040810     c                   if        o12err <> ' '
233600040810     c                   seton                                        31
233700040810     c                   else
233800040810     c* se numero di CA trovate maggiore di zero
233900040810     c                   if        o12nca = 0
234000040810     c                   seton                                        31
234100040810     c                   else
234200040810     c                   z-add     1             jj                2 0
234300040810     c                   dow       jj <= o12nca and *in31 = *off
234400040810     C* APERTA
234500040810     C     skDch(jj)     IFEQ      0
234600040810     C     V1CDAN        ANDEQ     'A'
234700040810     C                   SETOFF                                       32
234800040810     C                   SETON                                        31
234900040810     C                   ENDIF
235000040810     C* CHIUSE
235100040810     C     skDch(jj)     IFGT      0
235200040810     C     V1CDAN        ANDEQ     'C'
235300040810     C                   SETOFF                                       32
235400040810     C                   SETON                                        31
235500040810     C                   ENDIF
235600040810     C** CA APERTA O CHIUSA
235700040810     C     V1CDAN        IFEQ      'E'
235800040810     C                   SETOFF                                       32
235900040810     C                   SETON                                        31
236000040810     C                   ENDIF
236100040810     c*
236200040810     c                   add       1             jj
236300040810     c                   enddo
236400040810     c*
236500040810     c                   seton                                        31
236600040810     c*
236700040810     c                   endif
236800040810     c                   endif
236900040810     C**
237000990118     C                   ENDSR
237100940317      **************************************************************************
237200940928      * EMISSIONE SUBFILE DALLA LETTURA SU FNBLP
237300940317      **************************************************************************
237400940928     C     EMISS1        BEGSR
237500940317      *
237600930607     C                   SETOFF                                       27
237700990118     C                   CLEAR                   VSCFLA
237800940728     C                   MOVEL     *BLANKS       VSCSCE
237900940728     C   01              MOVEL     BLPLNP        VSCLIN
238000940728     C  N01              MOVEL     BLPLNA        VSCLIN
238100940728     C                   MOVEL     BLPLNP        VSCLNP
238200941222     C                   MOVEL     BLPCBO        VSCCBO
238300940808     C                   MOVEL     BLPAAS        VSCAAS
238400941222     C                   MOVEL     BLPFT2        VSCFT1
238500920206     C*
238600900516     C                   MOVEL     BLPMGS        MMGG
238700940728     C                   MOVEL     GG            VSCDSP
238800940728     C                   MOVE      MM            VSCDSP
238900900516     C*
239000940728     C                   MOVEL     BLPNSP        VSCNSP
239100950114     C* RIEMPO ALCUNI CAMPI DEL SFL
239200950114     C                   EXSR      RIESFL
239300920206     C*
239400050405     C     KBLP          CHAIN     tiGCP01L                           31
239500900515     C  N31GCPDCG        IFEQ      0
239600940728     C                   MOVEL     '*'           VSCGIA
239700910321     C                   ELSE
239800940728     C                   MOVEL     'C'           VSCGIA
239900900515     C                   END
240000990118     C* SPEDIZIONE IN DANNO
240100990118     C* SE MI HANNO CHIESTO LE CA CHIUSE VISUALIZZO IL FLAG "C"
240200990118     C* SE MI HANNO CHIESTO LE CA APERTE VISUALIZZO IL FLAG "*"
240300990118     C                   SELECT
240400990118     C     V1CDAN        WHENEQ    'C'
240500990118     C                   MOVEL     'C'           VSCDAN
240600990118     C     V1CDAN        WHENEQ    'A'
240700990118     C                   MOVEL     '*'           VSCDAN
240800990118     C**
240900990118     C                   OTHER
241000040810     C**""*KBLP          CHAIN     FNDCT02L                           31
241100040810     C**""**IN31         DOWEQ     *OFF
241200990118     C* SE NE TROVO UNA APERTA, DICO APERTA ALTRIMENTI CHIUSA
241300040810     C**""*DCTDCH        IFEQ      0
241400040810     C**""*DCTATB        ANDEQ     ' '
241500040810     C**""*              MOVEL     '*'           VSCDAN
241600040810     C**""*              SETON                                            31
241700040810     C**""*              ELSE
241800990118     C* CHIUSA MA VADO AVANTI A LEGGERE
241900040810     C**""*DCTATB        IFEQ      ' '
242000040810     C**""*              MOVEL     'C'           VSCDAN
242100040810     C**""*              ENDIF
242200040810     C**""*KBLP          READE     FNDCT02L                               31
242300040810     C**""*              ENDIF
242400040810     C**""*              ENDDO
242500040810      *
242600040810      * SOSTITUISCO LE ISTRUZIONI PRECEDENTI DI LETTURA FNDCT02L CON
242700040810      * richiamo il programma FIDN12R passando la DS richiedendo le CA non annul
242800040810      * legate alla spedizione che passo
242900040810     c                   clear                   fidn12ds
243000040810     c                   eval      i12aas = BLPAAS
243100040810     c                   eval      i12lnp = BLPLNP
243200040810     c                   eval      i12nrs = BLPNRS
243300040810     c                   eval      i12nsp = BLPNSP
243400040810     c                   eval      i12fel = 'E'
243500040810     c                   eval      i12fan = 'E'
243600040810     c*
243700040810     c                   call      'FIDN12R'
243800040810     c                   parm                    fidn12ds
243900040810     c*
244000040810     c                   setoff                                       31
244100040810     c*
244200040810     c* se non ci sono errori
244300040810     c                   if        o12err <> ' '
244400040810     c                   seton                                        31
244500040810     c                   else
244600040810     c* se numero di CA trovate maggiore di zero
244700040810     c                   if        o12nca = 0
244800040810     c                   seton                                        31
244900040810     c                   else
245000040810     c                   z-add     1             jj                2 0
245100040810     c                   dow       jj <= o12nca and *in31 = *off
245200040810     C* SE NE TROVO UNA APERTA, DICO APERTA ALTRIMENTI CHIUSA
245300040810     C     skDch(jj)     IFEQ      0
245400040810     C                   MOVEL     '*'           VSCDAN
245500040810     C                   SETON                                            31
245600040810     C                   ELSE
245700040810     C* CHIUSA MA VADO AVANTI A LEGGERE
245800040810     C                   MOVEL     'C'           VSCDAN
245900040810     C                   ENDIF
246000040810     c*
246100040810     c                   add       1             jj
246200040810     c                   enddo
246300040810     c*
246400040810     c                   seton                                        31
246500040810     c*
246600040810     c                   endif
246700040810     c                   endif
246800990118     C**
246900990118     C                   ENDSL
247000900522     C* SPEDIZIONE PARTITA
247100911108     C     BLPFT2        IFNE      ' '
247200940728     C                   MOVE      'P'           VSCPAR
247300990118     C                   ELSE
247400990118     C* SPEDIZINE SOLO BORDERIZZATA
247500990118     C     BLPDBR        IFGT      0
247600990118     C                   MOVE      'B'           VSCPAR
247700900522     C                   END
247800990118     C                   END
247900041028     c
248000041028     c* Chain fnarb per prendere alcuni campi da quiù
248100041028     c                   clear                   wesarb            1
248200041028     C     KBLP          CHAIN     fnarb01l
248300041028     c                   if        %found(fnarb01l)
248400041028     c                   eval      wesarb='S'
248500041028     c                   else
248600041028     c                   eval      wesarb='N'
248700041028     c                   endif
248800930407     C*
248900930407     C* SE BLPCCA >< 0  METTO DECODIFICA AL POSTO DELLA DATA CONSEGNA
249000041028     c                   if        wesarb='S'
249100041028     c                   eval      wcca=arbcca
249200041028     c                   eval      wdcm=arbdcm
249300041028     c                   eval      wdce=arbdce
249400041028     c                   eval      wnrc=arbnrc
249500041028     c                   eval      wdcp=arbdcp
249600041028     c                   else
249700041028     c                   eval      wcca=blpcca
249800041028     c                   eval      wdcm=blpdcm
249900041028     c                   eval      wdce=blpdce
250000041028     c                   eval      wnrc=blpnrc
250100041028     c                   eval      wdcp=blpdcp
250200041028     c                   endif
250300041028     c
250400940912     C                   MOVEL     *BLANKS       COMDCM
250500041028    1C     wcca          IFNE      ' '
250600940727     C                   MOVEL     '7O'          KCOD
250700940727     C                   MOVEL     *BLANKS       KKEY
250800041028     C                   MOVEL     wcca          KKEY
250900940727     C     KTAB2         CHAIN     TABEL00F                           31
251000940909     C  N31              MOVEL     TBLUNI        DS7O
251100930407     C*
251200940909     C* NELLA DS7O C'E' SCRITTO SE E' PRIORITARIA L'ANOMALIA O LA DATA
251300941011    2C  N31§7OVCP        IFEQ      'S'
251400940912     C     §7OVCP        ORNE      'S'
251500041028     C     wdcm          ANDEQ     *ZERO
251600940909     C                   MOVEL     §7ODEI        COMDCM            4
251700940909     C                   SETON                                        27
251800941011   X2C                   ELSE
251900940909     C*
252000041028    3C     wdcm          IFNE      *ZERO
252100041028     C                   MOVE      wdcm          MMGG
252200940909     C                   MOVEL     GG            COMDCM
252300940909     C                   MOVE      MM            COMDCM
252400941011    3C                   ENDIF
252500941011    2C                   ENDIF
252600940909     C*
252700941011   X1C                   ELSE
252800041028    2C     wdcm          IFNE      *ZERO
252900041028     C                   MOVE      wdcm          MMGG
253000930407     C                   MOVEL     GG            COMDCM
253100930407     C                   MOVE      MM            COMDCM
253200941011    2C                   ENDIF
253300941011    1C                   ENDIF
253400940912     C*
253500940912     C* DATA CONSEGNA
253600960207     C                   CLEAR                   VSCDCM
253700941011     C                   MOVEL     COMDCM        VSCDCM
253800940912     C                   MOVEL     *BLANKS       COMFCC            1
253900041028    1C     wdcm          IFNE      0
254000930506     C*
254100041028     C* SE    DCE = 0  CALL PGM PER CALCOLO DELLA DATA CONSEGNA PREVISTA
254200930506     C*   E DELLE ORE DI RITARDO
254300041028    2C     wdce          IFEQ      0
254400930506     C                   MOVEL     ' '           D98TLA
254500930506     C                   MOVEL     'P'           D98TBO
254600930506     C                   MOVEL     BLPAAS        D98AAS
254700930506     C                   MOVEL     BLPLNP        D98LNP
254800930506     C                   MOVEL     BLPNRS        D98NRS
254900930506     C                   MOVEL     BLPNSP        D98NSP
255000930506     C*
255100970925     C                   CALL      'TNSD99R'
255200970925     C                   PARM                    DS99
255300930506     C*
255400930701     C* SE SE98 = 1  E' STATA FATTA CALL PGM
255500941214     C                   MOVEL     '1'           PA0S98
255600930506     C*
255700930506     C* SE D98CRC = S  LA SPEDIZ. E' STAT CONSEGNATA NEI TEMPI PREVISTI
255800960201     C     D98ERR        IFNE      ' '
255900960201     C                   MOVEL     'R'           COMFCC
256000960201     C                   ELSE
256100941011    3C     D98CRC        IFEQ      'S'
256200930506     C                   MOVEL     ' '           COMFCC
256300930506     C                   ELSE
256400930506     C                   MOVEL     'F'           COMFCC
256500941011    3C                   END
256600960201    3C                   END
256700930506     C*
256800941011  X2 C                   ELSE
256900041028     C*    DCE > 0
257000940729     C* SE BLPNRC > 0  LA SPEDIZIONE NON E' CONFORME
257100041028   3 C     wnrc          IFGT      0
257200930505     C                   MOVEL     'F'           COMFCC
257300930407     C                   ELSE
257400930505     C                   MOVEL     ' '           COMFCC
257500941011   3 C                   END
257600941011   2 C                   END
257700930407     C*
257800940728     C                   MOVE      COMFCC        VSCDCM
257900930407     C*
258000941011   X1C                   ELSE
258100930407     C* SE DATA CONSEGNA = 0  E DATA CONS. PARZIALE >< 0  MUOVO PARZ
258200041028    2C     wdcp          IFNE      0
258300940728     C                   MOVEL     'PARZ'        VSCDCM
258400930607     C                   SETON                                        27
258500941011    2C                   ENDIF
258600941011    1C                   ENDIF
258700941011     C*
258800940928     C                   SETOFF                                       29
258900930407     C*
259000900518     C                   ADD       1             B
259100940727     C                   ADD       1             NRR2              4 0
259200940727     C                   WRITE     LS04S02
259300900515     C                   ENDSR
259400950114      **************************************************************************
259500950114     C* RIEMPIMENTO DEL SFL
259600950114      **************************************************************************
259700950114     C     RIESFL        BEGSR
259800950114     C  N01              MOVEL     BLPLNA        VSCLIN
259900950114     C*
260000950114     C                   MOVEL     BLPRSM        VSCRSM
260100950114     C                   MOVEL     BLPRSD        VSCRSD
260200950114     C                   Z-ADD     BLPRMN        VSCRMN
260300950114     C                   MOVEL     BLPRMA        VSCRMA
260400950114     C                   MOVEL     BLPFLS        VSCFLS
260500950114     C                   MOVEL     BLPNRS        VSCNRS
260600060203     C                   MOVEL     BLPksc        VSCksc
260700950114     C* SEGNACOLLI
260800950114     C                   CLEAR                   SEG
260900950114     C                   MOVEL     BLPNCD        COMSCN            7
261000950114     C                   MOVEA     COMSCN        SEG(1)
261100950114     C     BLPNCA        IFNE      0
261200950114     C                   MOVEL     BLPNCA        COMSCN
261300950114     C                   MOVEA     COMSCN        SEG(9)
261400950114     C                   END
261500950114     C                   MOVEA     '/'           SEG(8)
261600950114     C* ELIMINO ZERO NON SIGNIFICATIVI
261700950114     C                   Z-ADD     1             X                 3 0
261800950114     C     SEG(X)        DOWEQ     '0'
261900950114     C                   MOVE      ' '           SEG(X)
262000950114     C                   ADD       1             X
262100950114     C                   END
262200950114     C*
262300950114     C     BLPNCA        IFNE      0
262400950114     C                   Z-ADD     9             X
262500950114     C     SEG(X)        DOWEQ     '0'
262600950114     C                   MOVE      ' '           SEG(X)
262700950114     C                   ADD       1             X
262800950114     C                   END
262900950114     C                   END
263000950114     C                   MOVEA     SEG           VSCSEG
263100950114     C* TIPO SERVIZIO
263200950114     C                   MOVEL     BLPTSP        VSCTSP
263300950114     C                   ENDSR
263400940324      **************************************************************************
263500940324      * RICHIAMA PGM VISUALIZZAZIONE BOLLA DA SFL
263600940324      **************************************************************************
263700940324     C     CALBOL        BEGSR
263800940729     C*
263900941214     C   20              MOVEL     *BLANKS       PA0FLG
264000941214     C  N20              MOVEL     'A'           PA0FLG
264100941214     C                   MOVEL     VSCAAS        PA0AAS
264200941214     C                   MOVEL     VSCLNP        PA0LNP
264300941214     C                   MOVEL     VSCNRS        PA0NRS
264400941214     C                   MOVEL     VSCNSP        PA0NSP
264500941214     C                   MOVE      PA3CLI        PA0CLI
264600941214     C                   MOVEL     V1CRSU        PA0RSU
264700940324     C*
264800941214     C                   MOVEL     PARAM0        KPJBU
264900040220if  1c                   if        SDSprm = 1
265000940729     C                   CALL      'FNLS05R'
265100940324     C                   PARM                    KPJBA
265200040220x   1c                   else
265300040220     C                   call      'FNLS05R'
265400040220     C                   parm                    KPJBA
265500040220     C                   parm                    FIDNA1ds
265600040220e   1c                   endif
265700941214     C                   MOVEL     KPJBU         PARAM0
265800040220      * Se selezionata la bolla da FNLS05R per inserirla in reclami
265900040220      *  esco direttamente da questo pgm. (la bolla sarà già stata
266000040220      *  impostata nella ds FIDNA1DS da FNLS05R).
266100040220if  1c                   if            SDSprm > 1
266200040220     c                             and IA1ins = 'S'
266300040220     c                   goto      FINE
266400040220e   1c                   endif
266500940729     C*
266600940324     C                   ENDSR
266700000919      **************************************************************************
266800000919      * RICHIAMA PGM MANUTENZIONE SINGOLA BOLLA
266900000919      **************************************************************************
267000000919    1C     CALBO2        BEGSR
267100000919     C*
267200000919     C* NON PARTITA
267300000919    4C     VSCFT1        IFEQ      *BLANKS
267400000919     C                   MOVEL     VSCAAS        §LPAAS
267500000919     C                   MOVEL     VSCLNP        §LPLNP
267600000919     C                   MOVEL     VSCNRS        §LPNRS
267700000919     C                   MOVEL     VSCNSP        §LPNSP
267800000919     C                   MOVEL     'M'           §LPFPR
267900030612     C*                  MOVEL     PARAM4        KPJBU
268000000919     C                   CALL      'FNLS01R'
268100000919     C                   PARM                    KPJBA
268200000919     C                   PARM                    DBLP
268300000919     C                   PARM                    DTASV
268400030612     C                   PARM                    trul90ds
268500000919     C*
268600000919     C                   MOVEL     '1'           PA1S01
268700000919     C*
268800000919   X4C                   ELSE
268900000919     C*
269000000919     C* PARTITA
269100000919     C* VEDO SE E' MANUTENZIONABILE
269200060202     C**   simtpp        IFEQ      '2'
269300060202     C**   simtpp        oreq      *blanks
269400060202     C**   VSCLNP        ifne      SAVMUS
269500060202     C**                 MOVEL     D48MSG        V1CMSG
269600060202     C**                 MOVEL     '1'           PA0F03
269700060202     C**                 SETON                                        28
269800060202     c**                 endif
269900060202     C**                 ENDIF
270000060203     c
270100060203     c* controllo la skiera pog per lnp o p.o. utente se previsto
270200060203     c                   movel     vsclnp        w003a
270300060203     c     w003a         lookup    $pog                                   31
270400060203     c                   if        not *in31 and d66pocl='S'
270500060203     c                   movel     vscksc        w003a
270600060203     c     w003a         lookup    $pog                                   31
270700060203     c                   endif
270800060203    5c                   if        not *in31
270900060203     C                   MOVEL     msg(28)       V1CMSG
271000060203     C                   MOVEL     '1'           PA0F03
271100060203     C                   SETON                                        28
271200060203     c                   else
271300000919     C*
271400000919     C                   CLEAR                   DSLR48
271500000919     C                   MOVEL     VSCAAS        D48AAS
271600000919     C                   MOVEL     VSCLNP        D48LNP
271700000919     C                   MOVEL     VSCLNP        D48FGS
271800000919     C                   MOVEL     VSCNRS        D48NRS
271900000919     C                   MOVEL     VSCNSP        D48NSP
272000000919     C                   MOVEL     VSCCBO        D48CBO
272100000919     C                   MOVEL     'P'           D48TBO
272200000919     C                   MOVEL     V1CCVB        D48CVB
272300000919     C                   MOVEL     'S'           D48F12
272400000919     C                   MOVEL     DSLR48        KPJBU
272500000919     C                   CALL      'FNLR48R'
272600000919     C                   PARM                    KPJBA
272700000919     C                   PARM                    DSARBD
272800000919     C                   PARM                    DSARBK
272900000919     C                   PARM                    DARBT
273000000919     C                   PARM                    DSARBG
273100030612     C                   PARM                    trul90ds
273200000919     C*
273300000919     C                   MOVEL     KPJBU         DSLR48
273400000919     C* ERRORE
273500000919     C     D48ERR        IFNE      *BLANKS
273600000919     C                   MOVEL     D48MSG        V1CMSG
273700000919     C                   MOVEL     '1'           PA0F03
273800000919     C                   SETON                                        28
273900060203     c                   else
274000060203     C* SE NON C'E' STATO ERRORE RICHANO LA BOLLA PER PRENDERE IL MITT
274100060203     C* E IL DESTINATARIO CHE POSSON ESSERE MODIFICATI
274200060203     C     KBLP3         CHAIN     FNBLP01L                           31
274300060203     C  N31              EXSR      RIESFL
274400000919    4C                   ENDIF
274500000919    4C                   ENDIF
274600060203    4C                   ENDIF
274700000919     C*
274800000919    1C                   ENDSR
274900940727      **************************************************************************
275000940727      * Nel caso di interrogazione da parte di un cliente, viene verificato se
275100940727      * egli puo' interrogare le bolle e se deve essere conteggiato il traffico
275200940727      * dei byte sulla linea di comunicazione.
275300940727      * Queste informazioni sono contenute nella tabella 3K.
275400940727      *
275500940727      * Inoltre, egli puo' interrogare solo alcuni clienti (quelli a lui
275600940727      * unificati.
275700940727      **************************************************************************
275800940727     C     ABICLI        BEGSR
275900940727      *
276000940727     C     *LIKE         DEFINE    §3KINB        $3KINB
276100940727     C     *LIKE         DEFINE    §3KCTB        $3KCTB
276200940727     C                   MOVE      *BLANKS       $3KINB
276300940727     C                   MOVE      *BLANKS       $3KCTB
276400940727      *
276500940727      * Leggo tutta la tabella 3K riempendo una schiera con i cod.clienti
276600940727     C                   Z-ADD     *ZERO         $PRIMA            1 0
276700940727     C                   Z-ADD     *ZERO         $A                3 0
276800940727     C                   MOVE      '3K'          KCOD
276900940727     C     KTAB1         SETLL     TABEL00F
277000940727     C                   DO        *HIVAL
277100940727     C     KTAB1         READE     TABEL00F                               90
277200940727     C  N90TBLFLG        IFEQ      *BLANKS
277300940727     C                   MOVEL     TBLUNI        DS3K
277400940727     C     KNMUS         IFEQ      §3KPRF
277500940727      *
277600940727      * Devo prendere il cod.cliente, che e' la chiave di TABEL
277700940727     C                   MOVEL     TBLKEY        $CODIC            7 0
277800940727     C     $CODIC        LOOKUP    CLI                                    99
277900940727     C  N99$A            IFLT      300
278000940727     C                   ADD       1             $A
278100940727     C                   Z-ADD     $CODIC        CLI($A)
278200940727     C                   ENDIF
278300940727      *
278400940727      * Gia' dalla prima lettura ho i due flag che abilitano il profilo
278500940727     C     $PRIMA        IFEQ      *ZERO
278600940727     C                   MOVE      §3KINB        $3KINB
278700940727     C                   MOVE      §3KCTB        $3KCTB
278800940727     C     §3KINB        COMP      'S'                                9090
278900940727     C                   Z-ADD     1             $PRIMA
279000940727     C                   ENDIF
279100940727      *
279200940727     C                   ENDIF
279300940727     C                   ENDIF
279400940727     C  N90              ENDDO
279500940727      *
279600940727     C                   ENDSR
279700991214     C**************************************************************************
279800991214      ** CALCOLA CHECK DIGIT
279900991214     C**************************************************************************
280000991214     C     M1031         BEGSR
280100991214      *
280200991214     C                   MOVE      *ZERO         §TT2              3 0
280300991214     C                   CLEAR                   W0030
280400991214      *
280500991214      * TOTALIZZO I VALORI, QUELLI NELLE POSIZIONI DISPARI PER 3
280600000328     C                   DO        11            YY                2 0
280700991214      *
280800000328     C     YY            DIV       2             §CTR              3 0
280900991214     C                   MVR                     §RST              3 030
281000991214      *
281100991214     C     *IN30         IFEQ      '1'
281200000328     C     §11(YY)       MULT      3             §CTR
281300991214     C                   ELSE
281400000328     C                   Z-ADD     §11(YY)       §CTR
281500991214     C                   ENDIF
281600991214      *
281700991214     C                   ADD       §CTR          W0030             3 0
281800991214     C                   ENDDO
281900991214      *
282000991214     C     W0030         ADD       9             §TT2
282100991214     C                   MOVE      0             §TT2
282200991214     C     §TT2          SUB       W0030         DPDCHD
282300991214      *
282400991214     C                   ENDSR
282500950206     C**************************************************************************
282600950206     C*   CARICAMENTO TABELLE INZIALE
282700950206     C**************************************************************************
282800950206     C     CARTAB        BEGSR
282900950206     C*
283000950206     C* RIEMPO UNA SCHIERA CON TUTTI I TIPI BOLLA DI RECUPERO
283100950206     C                   MOVEL     '3A'          KCOD
283200950206     C                   Z-ADD     *ZERO         WA                3 0
283300950206     C     KTAB1         SETLL     TABEL00F
283400950206     C     KTAB1         READE     TABEL00F                               31
283500950206     C     *IN31         DOWEQ     *OFF
283600950206     C                   MOVEL     TBLUNI        DS3A
283700950206     C     §3ARBL        IFEQ      'R'
283800950206     C                   ADD       1             WA
283900950206     C                   MOVEL     TBLKEY        REC(WA)
284000950206     C                   ENDIF
284100950206     C     KTAB1         READE     TABEL00F                               31
284200950206     C                   ENDDO
284300950206     C*
284400951010     C****  CARICO TABELLA FILIALI GESTITE £1  ****
284500951010     C                   CLEAR                   DSUL06
284600951010     C                   MOVE      '£1'          D06COD
284700951010     C                   MOVEL     SIMFEL        D06KEY
284800951010     C                   MOVEL     DSUL06        KPJBU
284900951010     C                   CALL      'TRUL06R'
285000951010     C                   PARM                    KPJBA
285100951010     C                   MOVEL     KPJBU         DSUL06
285200951010     C                   MOVEA     LIN           L1
285300030527     C*
285400030527     C* Carico i P.O. di tipo PT
285500030527     C                   z-add     *zeros        i                 2 0
285600030527     C     *loval        setll     azorg01l
285700030527     C                   read      azorg01l
285800030527     C                   dow       not %eof(azorg01l)
285900030527     C                   movel(p)  orgDE3        OG143
286000030527     C                   if        §OGNTW = 'PPT'
286100030527     C                   add       1             i
286200030527     C                   eval      SKPOPT(i) = orgFIL
286300030527     C                   endif
286400030527     C                   read      azorg01l
286500030527     C                   enddo
286600030527     C*
286700030527     C* Partendo dalla data attuale reperisco la data limite olre la quale
286800030527     C* nn scendere nel considerare le bolle PT.
286900030527     C                   eval      wrkoggiiso = *loval
287000030527     C*
287100030527     C                   eval      tbeCOD = 'VPO'
287200030527     C                   eval      tbeKE1 = 'VPO'
287300030527     C     KTBE01P       chain     TNTBE01L
287400030527     C                   if        %found(TNTBE01L)
287500030527     C                   eval      DVPO = tbeUNI
287600030527     C*
287700030527     C* Se il valore in tabella è 9999 significa nessun blocco data limite
287800030527     C                   if        §VPOIPP <> 9999
287900030527     C                   time                    wrkoggiiso
288000030527     C     wrkoggiiso    subdur    §VPOIPP:*DAYS wrkoggiiso
288100030527     C                   endif
288200030527     C                   endif
288300030527     C     *iso          move      wrkoggiiso    wDataLimitePT
288400030527     C*
288500950206     C                   ENDSR
288600940727     C**************************************************************************
288700940727     C*    R O U T I N E     I N I Z I A L E
288800940727     C**************************************************************************
288900940727     C     *INZSR        BEGSR
289000940727     C*
289100051013     C     KarS          KLIST
289200000831     C                   KFLD                    V1CNOT
289300000831     C                   KFLD                    KKTRC             1
289400000911     C                   MOVE      'C'           KKTRC
289500051013     C     KarSXT        KLIST
289600051013     C                   KFLD                    arSFLS
289700051013     C                   KFLD                    arSLNA
289800051013     C                   KFLD                    arSNRS
289900051013     C                   KFLD                    arSNSC
290000090226     C     KarTXP        KLIST
290100090226     C                   KFLD                    arTAAS
290200090226     C                   KFLD                    arTLNP
290300090226     C                   KFLD                    arTNRS
290400090226     C                   KFLD                    arTNSP
290500940727     C     KLNA1         KLIST
290600940728     C                   KFLD                    V1CLNA
290700940727     C                   KFLD                    COMAAD
290800940727     C                   KFLD                    COMMGD
290900940727     C                   KFLD                    TSP               1
291000940928     C     KLNA2         KLIST
291100940928     C                   KFLD                    V1CLNA
291200940928     C                   KFLD                    COMAAD
291300940928     C                   KFLD                    COMMGD
291400020627     c     klna3         klist
291500020627     c                   kfld                    klp1
291600020627     c                   kfld                    v1clna
291700020627     c                   kfld                    comaad
291800020627     c                   kfld                    commgd
291900020627     c                   kfld                    tsp
292000020627     c     klna4         klist
292100020627     c                   kfld                    klp1
292200020627     c                   kfld                    v1clna
292300940727     C     KLNP          KLIST
292400940728     C                   KFLD                    V1CAAS
292500940728     C                   KFLD                    V1CLP2
292600940728     C                   KFLD                    V1CNRS
292700940728     C                   KFLD                    V1CNSP
292800940727     C     KLNP2         KLIST
292900940728     C                   KFLD                    V1CLP1
293000940727     C                   KFLD                    COMAAD
293100940727     C                   KFLD                    COMMGD
293200940728     C                   KFLD                    V1CPSD
293300940727     C     KBLP          KLIST
293400940727     C                   KFLD                    BLPAAS
293500940727     C                   KFLD                    BLPLNP
293600940727     C                   KFLD                    BLPNRS
293700940727     C                   KFLD                    BLPNSP
293800940727     C     KBLP2         KLIST
293900940727     C                   KFLD                    BLPAAS
294000940727     C                   KFLD                    BLPLNP
294100940727     C                   KFLD                    BLPNRS
294200940727     C                   KFLD                    BLPNSP
294300940728     C                   KFLD                    V1CSCD
294400950114     C     KBLP3         KLIST
294500950114     C                   KFLD                    VSCAAS
294600950114     C                   KFLD                    VSCLNP
294700950114     C                   KFLD                    VSCNRS
294800950114     C                   KFLD                    VSCNSP
294900951011     C     KBLP4         KLIST
295000060217     C                   KFLD                    ar4AAS
295100060217     C                   KFLD                    ar4LNP
295200060217     C                   KFLD                    ar4NRS
295300060217     C                   KFLD                    ar4NSP
295400940727     C     KTAB2         KLIST
295500940727     C                   KFLD                    CODUT
295600940727     C                   KFLD                    KCOD
295700940727     C                   KFLD                    KKEY
295800940727     C     KTAB1         KLIST
295900940727     C                   KFLD                    CODUT
296000940727     C                   KFLD                    KCOD
296100940727     C     CNACOK        KLIST
296200940727     C                   KFLD                    CODUT
296300940727     C                   KFLD                    ACOKCC
296400940727     C                   KFLD                    ACOKSC
296500940729     C     BLP33K        KLIST
296600991214     C                   KFLD                    KRMN
296700940727     C                   KFLD                    COMAAD
296800940727     C                   KFLD                    COMMGD
296900940729     C     BLP43K        KLIST
297000021111     C                   KFLD                    Klp1
297100940727     C                   KFLD                    $AAS
297200940727     C                   KFLD                    $MGS
297300940728     C                   KFLD                    V1CRSD
297400940729     C     BL43K         KLIST
297500021111     C                   KFLD                    Klp1
297600940727     C                   KFLD                    $AAS
297700940727     C                   KFLD                    $MGS
297800940729     C     BLP44K        KLIST
297900021111     C                   KFLD                    Klp1
298000940727     C                   KFLD                    $AAS
298100940727     C                   KFLD                    $MGS
298200940728     C                   KFLD                    V1CRSM
298300940729     C     BL44K         KLIST
298400021111     C                   KFLD                    Klp1
298500940727     C                   KFLD                    $AAS
298600940727     C                   KFLD                    $MGS
298700060217     C     Kar4          KLIST
298800951011     C                   KFLD                    WTRC
298900971212     C                   KFLD                    WN30
299000010219      *
299100060217     C     Kar43         KLIST
299200010219     C                   KFLD                    WTRC
299300010219     C                   KFLD                    WN14             14
299400010219      *
299500010219     C     KFORM         KLIST
299600010219     C                   KFLD                    V1CPOE
299700010219     C                   KFLD                    V1CNSR
299800010219     C                   KFLD                    V1CNOR
299900010219     C                   KFLD                    V1CNRV
300000030527     C*
300100030527     C* Chiave su TNTBE01L - Parziale
300200030527     C     KTBE01P       KLIST
300300030527     C                   KFLD                    TBECOD
300400030527     C                   KFLD                    TBEKE1
300500940727     C*
300600991214     C     *LIKE         DEFINE    V1CRMN        KRMN
300700991214     C     *LIKE         DEFINE    TBLCOD        KCOD
300800940727     C     *LIKE         DEFINE    TBLKEY        KKEY
300900940728     C     *LIKE         DEFINE    TBLKUT        §KUT
301000940728     C     *LIKE         DEFINE    TBLCOD        §COD
301100940728     C     *LIKE         DEFINE    TBLKEY        §KEY
301200940729     C     *LIKE         DEFINE    BLPAAS        $AAS
301300940729     C     *LIKE         DEFINE    BLPMGS        $MGS
301400961118     C     *LIKE         DEFINE    BLPFBR        WFBR
301500950216     C     *LIKE         DEFINE    BLPDSP        WDSP
301600060217     C     *LIKE         DEFINE    ar4TRC        WTRC
301700940727     C*
301800951011     C* IMPOSTAZIONE CAMPI FISSI
301900951011     C                   MOVEL     'E'           WTRC
302000060202     c
302100060202     c                   movel     kpjbu         savkpjbu
302200060202      *
302300060202     c     *dtaara       define    §azute        azuteds
302400060202     c     *dtaara       define    §datiute      ddatiute
302500060202     c                   in(E)     *dtaara
302600060202    1c                   If        %error  or RSUT = *blanks
302700060202     c                   Clear                   Tibs34ds
302800060202     c                   Call      'TIBS34R'
302900060202     c                   Parm                    Tibs34ds
303000060202     c                   In        *dtaara
303100060202    1c                   EndIf
303200060202
303300060202     c                   Clear                   wabi
303400060202     c                   Clear                   v1cmsg
303500060202     c                   Clear                   dLat
303600060202
303700060202      * Verifica errori e autorità profilo
303800060202s   1c                   Select
303900060202      * se ho errori nei dati utente esco dal pgm
304000060202w   1c                   When      DutErr = 'E'
304100060202     c                   GoTo      Fine
304200060202      * se non c'è l'abilitazione
304300060202      * --> se 1° livello, abilitazioni al terminal
304400060202      *     se 2° livello, abilitazioni al punto operativo
304500060202      *     se sede è impossibile ma se capita mando a fine il pgm
304600060202w   1c                   When      UteAut = *Blanks
304700060202if  2c                   If        DutLpo = '1'
304800060202     c                   Eval      wabi   = 'TP'
304900060202e   2c                   EndIf
305000060202if  2c                   If        DutLpo = '2'
305100060202     c                   Eval      wabi   = 'PO'
305200060202e   2c                   EndIf
305300060202if  2c                   If        DutLpo = 'S'
305400060202     c                   GoTo      Fine
305500060202e   2c                   EndIf
305600060202      * carica le abilitazioni del profilo
305700060202x   1c                   Other
305800060202     c                   Movel     UteFaf        Dute01
305900060202     c                   If        §Utembf <> *Blanks
306000060202     c                   Eval      wabi = §Utembf
306100060202     c                   Else
306200060202     c                   Eval      wabi = UteAut
306300060202     c                   EndIf
306400060202e   1c                   EndSl
306500060202
306600060202      * controllo se ok l'abilitazione dell'utente
306700060202     c                   Clear                   Tibs02ds
306800060202     c                   Eval      T02tla = 'L'
306900060202     c                   Eval      T02Mod = 'C'
307000060202     c                   Eval      T02Sif = knsif
307100060202     c                   Eval      T02Cod = 'LAT'
307200060202     c                   Movel(p)  wabi          T02Ke1
307300060202     c                   Call      'TIBS02R'
307400060202     c                   Parm                    kpjba
307500060202     c                   Parm                    Tibs02ds
307600060202if  1c                   If        T02Err = *Blanks
307700060202     c                   Eval      dLat = T02Uni
307800060202e   1c                   EndIf
307900060202      * errore o non abilitato
308000060202if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S'
308100060202     c                   Seton                                        28
308200060202     C                   movel     msg(29)       v1cmsg
308300060202   x1c                   Else
308400060202      * Reperimento dei P.O. gestibili dall'utente
308500060202     c                   clear                   TRUL31DS
308600060202     c                   clear                   TRUL31DS2
308700060202     c                   eval      I31abi = wabi
308800060202     c                   eval      I31cdi = DUTdis
308900060202     c                   eval      I31car = DUTare
309000060202     c                   eval      I31cpo = DUTpou
309100060202     c                   call      'TRUL31R'
309200060202     c                   parm                    KPJBA
309300060202     c                   parm                    TRUL31DS
309400060202     c                   parm                    TRUL31DS2
309500060202if  2c                   if        O31pog > *zeros
309600060202     c                   movea     O31pog        $pog
309700060202     c                   movea     O31arg        $arg
309800060202     c                   movea     O31dig        $dig
309900060202x   2c                   else
310000060202     c                   Seton                                        28
310100060202     c                   movel     MSG(29)       v1cmsg
310200060202e   2c                   endif
310300060202    1c                   endif
310400060202     c
310500060202     c                   movel     savkpjbu      kpjbu
310600951011     C*
310700940727     C                   ENDSR
310800950206     C**************************************************************************
310900950111     OFNLV22P   T    LRNLR
311000950206     O**************************************************************************
311100030612** msg
311200940728Nessuna bolla da visualizzare                                                  1
311300940728Immettere linea di partenza valida                                             2
311400940728Immettere numero spedizione esistente in archivio                              3
311500040216Immettere tipo bolla valido                                                    4
311600941010Immettere linea di arrivo valida                                               5
311700010219Selezionare per DATA o per NUMERO SPEDIZIONE o per ORM                         6
311800940728Immettere linea di partenza oppure linea di arrivo                             7
311900940728Immettere data formalmente valida                                              8
312000940728Immettere data spedizione DAL minore o uguale alla data AL                     9
312100940728Immettere numero serie DAL minore o uguale al numero serie AL                 10
312200940728Immettere segnacollo DAL minore o uguale al segnacollo AL                     11
312300940728Se immesso segnacollo AL immettere anche sgnacollo DAL                        12
312400940728Immettere codice cliente esistente e non annullato                            13
312500940728Immettere tipo servizio bolle valido                                          14
312600940728Immettere almeno una selezione                                                15
312700940728Fine scorrimento                                                              16
312800961125Immettere punto operativo segnacollo valido                                   17
312900000831Immettere o "CHI SONO" o rif.mittente o rif.partner partenza                  18
313000040216* LIBERO * Non possibile la selezione per ricerca nell'archivio storico bolle 19
313100040216* LIBERO * BOLLA NON MANUTENZIONABILE: gia' nello Storico Bolle               20
313200961125BOLLA NON MANUTENZIONABILE: in gestione ad altro punto operativo              21
313300060608Lunghezza riferimento Mittente errata per bolle DPD
313400060609
313500060614Riferimento partner estero errato x bolle DPD:o lungo 11 numerico o 14 alfanum
313600010219Immettere p.o. di emissione valido                                            25
313700010219Immettere numero orm esistente in archivio                                    26
313800060202Utente non abilitato alla gestione di questa linea di partenza                27
313900060202Utente non abilitato alla manutenzione di questa bolla                        28
314000060202Utente non abilitato alla Variazione bolle                                    29
314100030612** cm3
314200950111OVRPRTF FILE(FNLV22P) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
314300991227 SHARE(*YES)          USRDTA('Segnacolli')
314400030612**  cmA4
314500030612OVRPRTF FILE(FNLSB5PA4) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
314600030612 SHARE(*YES)          USRDTA('Man.BolA4')
314700030612**  cmA5
314800030612OVRPRTF FILE(FNLSB5PA5) OUTQ(XXXXXXXXXX) FORMTYPE('xxxxxxxxxx')
314900030612 SHARE(*YES)          USRDTA('Man.BolA5')
315000030612** cnm
315100991222 0123456789
