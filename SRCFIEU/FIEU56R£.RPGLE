000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
002100940224      *
002200940307      *  21           GENERICO OPERAZIONI I/O
002300940224      *  22           GENERICO ERRORE OPERAZIONI I/O
002400940224      *  30           SFLDSP
002500940224      * N31           SFLCLR
002600940224      *  31           SFLDSPCTL
002700940224      *  32           SFLNXTCHG
002800940224      *  33           SFLEND
002900940224      *  39           OF PRTF
003000940224      *  40 <---> 49  DSPATR ERRORI SU SFL
003100940608      *  Specificare l'uso dei singoli indicatori
003200940224      *  50 <---> 98  ERRORI SU VIDEO
003300940608      *  Specificare l'uso dei singoli indicatori
003400940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
003500940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
003600940224      *  99           INDIC. GENERALE DI ERRORE
003700940128     F*----------------------------------------------------*
003800060927     FTntbe01L  IF   E           K DISK
003900060927     Ffieu56D   CF   E             WORKSTN
004000940607     F                                     SFILE(S1:S1NRR)
004100940201     F                                     INFDS(DSFMT)
004200940128     D*----------------------------------------------------*
006300940211     D* Passaggio Parametri
006400940211     D KPJBA         E DS
006500030113      *-------------
006600940325     D* Parametri in ricezione
006700030113     D  TABDS          DS
006800030113     D  XTAOPZ                 1      2
006900030113     D  XTARET                 3      3
007000030113     D  XTAOPR                 4      4
007100030113     D  XTAERR                 5      5
007400030113     D  XTAKEY                 6     13
010600940211     D*-------------
010700940211     D DSFMT           DS           512
010800940506     D  $TASTO               369    369
010900940211     D  NRG                  370    370
011000940211     D  NCL                  371    371
011100940211     D  SFLNRR               378    379B 0
011200940207     D*-------------
011300940207     D* Nome PGM a video
011400940207     D                 DS
011500940207     D  PROGR                  1     10
011600940207     D  ASTER1                 1      1    INZ('*')
011700940207     D  SIGLA                  2      9
011800940207     D  ASTER2                10     10    INZ('*')
011900940127     D*-------------
012000940127     D* Reperimento nome PGM
012100940127     D STATUS         SDS           333
012200940127     D  DSPGM            *PROC
012201060927     D* descrizione tabella
012202060927$003 D***** EXTDS         E DS                  EXTNAME(DSOD)
012401030113     D*-------------
012402030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
012403030113$003 D S1NRR           S                   Like(C1rcd)
012404030113$003 D WSfl            S                   Like(C1nrr)
012405030113$003 D Wmax            S                   Like(C1rcd)
012406030113$003 D Wpag            S                   Like(C1rcd)
012407030113$003 D Winzs1          S                   Like($inzs1)
012500940207     D*-------------
012600940211     D* COSTANTI
012700940211     D*-------------
012800030113     D SFLPAG          C                   CONST(11)
013000940314     D* dimensione della schiera $MS1
013100940506     D*
013200940506     D* Tasti di funzione
013300940506     D F01             C                   CONST(X'31')
013400940506     D F02             C                   CONST(X'32')
013500940506     D F03             C                   CONST(X'33')
013600940506     D F04             C                   CONST(X'34')
013700940506     D F05             C                   CONST(X'35')
013800940506     D F06             C                   CONST(X'36')
013900940506     D F07             C                   CONST(X'37')
014000940506     D F08             C                   CONST(X'38')
014100940506     D F09             C                   CONST(X'39')
014200940506     D F10             C                   CONST(X'3A')
014300940506     D F11             C                   CONST(X'3B')
014400940506     D F12             C                   CONST(X'3C')
014500940506     D F13             C                   CONST(X'B1')
014600940506     D F14             C                   CONST(X'B2')
014700940506     D F15             C                   CONST(X'B3')
014800940506     D F16             C                   CONST(X'B4')
014900940506     D F17             C                   CONST(X'B5')
015000940506     D F18             C                   CONST(X'B6')
015100940506     D F19             C                   CONST(X'B7')
015200940506     D F20             C                   CONST(X'B8')
015300940506     D F21             C                   CONST(X'B9')
015400940506     D F22             C                   CONST(X'BA')
015500940506     D F23             C                   CONST(X'BB')
015600940506     D F24             C                   CONST(X'BC')
015700940506     D ENTER           C                   CONST(X'F1')
015800940506     D ROLDWN          C                   CONST(X'F4')
015900940506     D ROLLUP          C                   CONST(X'F5')
016500940127     C*----------------------------------------------------*
016600940127     C*                MAIN LINE PROGRAM
016700940127     C*----------------------------------------------------*
016900940223     C* inizializzazione variabili
017000940223     C                   EXSR      INZVAR
017100940223     C*
017200940223     C     $FINE         DOWEQ     *OFF
017300940131     C     $GEST         CASEQ     'S1'          GESS1
017400940117     C                   END
017500940117     C                   END
017700940325     C* fine programma
019200940325     C                   SETON                                            LR
019201030113     C************************************************************
019202030113     C* INIZIALIZZAZIONE VARIABILI
019203030113     C************************************************************
019204030113     C     INZVAR        BEGSR
019205030113     C*
019206030113     C* Pulizia campi e indicatori
019207030113     C                   MOVE      *ALL'0'       IN4049           10
019208030113     C                   MOVEA     IN4049        *IN(40)
019209030113     C                   CLEAR                   S1OPZ
019210030113     C* Variabili per gestione videate
019211030113     C*
019212030113     C                   MOVE      *OFF          $FINE
019213030113     C                   MOVE      *OFF          $INZS1
019214030113     C                   MOVE      *OFF          $EFILE
019215030113     C                   MOVE      *OFF          $ESCI
019216030113     C                   MOVE      *OFF          $RCDOK
019217030113     C                   Z-ADD     0             $ULKS1            3 0
019218030113     C*
019219030113     C                   MOVE      *ON           $INZS1
019220030113     C                   MOVE      'S1'          $GEST
019221030113     C*
019222030113     C* Variabili appoggio
019223030114     C                   Z-ADD     1             WPAG
019230030113     c*
019231030113     C                   ENDSR
040400940127     C************************************************************
040500940131     C* GESTIONE LISTA
040600940127     C************************************************************
040700940127     C     GESS1         BEGSR
040701030113     C*
040800940223     C* inizializzazione videata
040900940223     C     $INZS1        IFEQ      *ON
041000940127     C                   EXSR      INZS1
041100940223     C                   MOVE      *OFF          $INZS1
041200940127     C                   ENDIF
041201030113     C*
041300030113     C* emissione piede videata
041301030113     C                   WRITE     Z1
042100030113     C* Non ci sono records
042200940223     C     WMAX          IFEQ      0
042300940607     C                   WRITE     D1
042400030114     C                   Else
042401030114     C     Wsfl          IFgt      0
042403030114     C                   Z-ADD     wsfl          C1RCD
042404030114     C                   Else
042405030114     C     Wpag          IFgt      0
042407030114     C                   Z-ADD     wpag          C1RCD
042408030114     C                   EndIF
042409030114     C                   EndIF
042410030114     C                   ENDIF
042600940127     C*
042601030113     C*              *------------------*
042700940607     C                   EXFMT     C1
042701030113     C*              *------------------*
042702030113     C*
042800940204     C     C1NRR         IFNE      0
042900940204     C                   Z-ADD     C1NRR         WSFL
043000940204     C                   ENDIF
043100940127     C                   Z-ADD     SFLNRR        C1RCD
043800030113     C* Selezioni
0439009401271    C                   SELECT
044000940127     C* F3=Fine
044100940506     C     $TASTO        WHENEQ    F03
044200940309     C                   EXSR      F03S1
044300940131     C* F10=Immissione
044400940506     C     $TASTO        WHENEQ    F10
044500940309     C                   EXSR      F10S1
0459009401271O   C                   OTHER
046000940127     C* CONTROLLO DATI
046100940131     C                   EXSR      CTRC1
046200940201     C     *IN99         IFEQ      *OFF
046300940131     C                   EXSR      CTRS1
046400940131     C                   END
0465009401271-   C                   ENDSL
046600940127     C*
046800940127     C                   ENDSR
046900940224     C/EJECT
047000940127     C************************************************************
047100940131     C* INIZIALIZZAZIONE LISTA
047200940127     C************************************************************
047300940127     C     INZS1         BEGSR
047400940302     C* pulizia SFL
047500940128     C                   SETOFF                                         3031
047600940607     C                   WRITE     C1
047700940128     C                   SETON                                          31
049100940128     C*
049200030113     C* CARICAMENTO SFL totale
049400940201     C                   Z-ADD     0             S1NRR
049401030113     C                   Z-ADD     1             C1RCD
049500940128     C                   Z-ADD     0             WMAX
049600940224     C*
049700940224     C* Posizionamento su file pilota
049701060927     c     kTBE          setll     Tntbe01l
050000940608    >C                   EXSR      REDANA
050400030113     C* Carico il SFL
050500940127     C                   EXSR      ROLS1
050501030113     C*
050502030114     c                   if        xtaopr <> '1'
050600030114     C                   Z-ADD     1             WPAG
050601030114     c                   end
050700940127     C*
050800940127     C                   ENDSR
051000940127     C************************************************************
051100940131     C* CARICAMENTO PAGINA LISTA
051200940127     C************************************************************
051300940127     C     ROLS1         BEGSR
051400940127     C*
051500940128     C                   SETOFF                                       32
051600940223     C                   Z-ADD     0             Y
051700940127     C                   Z-ADD     WMAX          S1NRR
051800940127     C*
051900940127     C* Leggo dal file anagrafico per caricare la lista
0521009401311    C     $EFILE        DOWEQ     *OFF
052300940127     C*
052301030113     c                   clear                   s1opz
052302060927     c                   movel     tbeke1        s1cod
052303060927     c                   movel     tbeuni        s1des
052305030113     C*
052312030115     c                   clear                   S1Ann
052313060927     c                   if        tbeatb <>*blank
052314030115     c                   move      'A'           S1Ann
052315030115     c                   end
052500940127     C*
052600940127     C                   ADD       1             S1NRR
052700940127     C                   ADD       1             Y
052900940127     C*
053000940607     C                   WRITE     S1
053100940131     C*
053200940316    >C                   EXSR      REDANA
053300940128     C*
0534009401271-   C                   ENDDO
053500940127     C*
053600940223     C                   Z-ADD     S1NRR         WMAX                 30
053700940127     C*
053800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
053900940127     C*
054000030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
054100940127     C                   MVR                     RESTO             3 0
054200030114     C     PAGINE        MULT      SFLPAG        C1RCD
0543000301141    C     RESTO         IFGT      0
054400030114     C                   ADD       1             C1RCD
0545000301141E   C                   ELSE
054600030114     C                   SUB       SFLPAG        C1RCD
054700030114     C                   ADD       1             C1RCD
0548000301141-   C                   ENDIF
054900940128     C*
055000940127     C                   ENDSR
057600940128     C************************************************************
057700940131     C* LETTURA RCD ARCHIVIO PILOTA
057800940128     C************************************************************
057900940607     C     REDANA        BEGSR
058000940128     C*
058100940131     C                   MOVEL     *OFF          $EFILE
058200940131     C                   MOVEL     *OFF          $RCDOK
058300940131     C*
0584009401311    C     $EFILE        DOUEQ     *ON
058500940131     C     $RCDOK        OREQ      *ON
058600940131     C*
058601060927     c     kTBE          Reade     Tntbe01l
058602060927     c                   if        %eof(Tntbe01l)
058603030113     C                   MOVEL     *ON           $EFILE
058604030113     C                   MOVE      $EFILE        *IN33
058605030113     c                   else
058606030113     C                   MOVE      *ON           $RCDOK
058607030113     c                   end
060300940131     C*
0604009401311-   C                   ENDDO
060700940131     C*
060800940131     C                   ENDSR
061000940224     C************************************************************
061100940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
061200940224     C************************************************************
061300940224     C     CLCPAG        BEGSR
061400940224     C* Input :
061500940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
061600940224     C*          cursore
061700940224     C* - SFLPAG = numero rcd per pagina sfl
061800940224     C* Output :
061900940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
062000940224     C*
062100940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
062200940224     C                   MVR                     RESTO             3 0
062300940224     C     RESTO         IFGT      0
062400940224     C                   ADD       1             PAGINE
062500940224     C                   ENDIF
062600940224     C     PAGINE        MULT      SFLPAG        WPAG
062700940224     C*
062800940224     C                   ENDSR
067500940309     C************************************************************
067600940309     C* GESTIONE F03 VIDEO S1
067700940309     C************************************************************
067800940309     C     F03S1         BEGSR
067900940309     C*
068000940309     C                   MOVE      *ON           $FINE
068200940325     C* fine programma
068500940309     C                   ENDSR
068600940309     C/EJECT
068700940309     C************************************************************
068800940309     C* GESTIONE F10 VIDEO S1
068900940314     c* AGGIUNTA RECORD
069000940309     C************************************************************
069100940309     C     F10S1         BEGSR
069200060927     C******
069201030113     C                   RESET                   tabds
069202060927     C******             MOVEL     '01'          xtaopz
069203060927     C******             MOVE      *ZERO         xtaret
069204060927     C******             MOVE      *ZERO         xtaopr
069205060927     C******             MOVEl     *blank        xtakey
069206060927     C******             MOVE      *BLANKS       KPJBU
069207060927     C******             MOVEL     tabds         KPJBU
069208060927$004 C******             CALL      'TRTB88R2'
069209060927     C******             PARM                    KPJBA
069210060927     C******             MOVEL     KPJBU         tabds
070201030113      *
070300940309     C* ritorno da PGM gestione
070400940309     C                   EXSR      GESRET
070500940309     C     WINZS1        IFEQ      *ON
070600940309     C                   MOVE      *ON           $INZS1
070700940309     C* se esistevano già righe sul sfl
070800940309     C* calcolo pagina a cui deve posizionarsi
070900940309     C     WSFL          IFGT      0
071000940309     C                   EXSR      CLCPAG
071100940309     C* altrimenti carico solo la 1a pagina
071200940309     C                   ELSE
071300940309     C                   Z-ADD     SFLPAG        WPAG
071400940309     C                   ENDIF
071500940309     C                   ENDIF
071600940309     C*
071800940309     C                   ENDSR
076300940128     C************************************************************
076400940131     C* CONTROLLO TESTATA LISTA
076500940128     C************************************************************
076600940131     C     CTRC1         BEGSR
076700940128     C*
076800940201     C                   MOVE      *OFF          *IN99
076900940131     C*
077700940202     C                   ENDSR
082100940131     C************************************************************
082200940131     C* CONTROLLO OPZIONI LISTA
082300940131     C************************************************************
082400940131     C     CTRS1         BEGSR
082500940131     C*
082600940202     C                   MOVEL     *OFF          $ESCI
082700940201     C                   SETOFF                                       99
082800940131     C                   Z-ADD     0             S1OPZ
082900940131     C*
083000940127     C* Leggo il sfl solo se ci sono rcd
0831009401311    C     WMAX          IFGT      0
083200940607     C                   READC     S1                                     21
083300940127     C*
083400940131     C* esce se fine sfl o errore che richiede l'uscita
0835009401312    C     *IN21         DOWEQ     *OFF
083600940131     C     $ESCI         ANDEQ     *OFF
083700940201     C                   Z-ADD     S1NRR         C1RCD
083800940131     C* ctrl su riga
083900940131     C                   EXSR      RECS1
084000940131     C* gestione opzioni
0841009401313    C     S1OPZ         IFNE      0
084200940201     C     *IN99         ANDEQ     *OFF
084300940131     C                   EXSR      OPZS1
0844009401273-   C                   ENDIF
084500940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0846009402013    C     *IN99         IFEQ      *ON
084700940201     C     $ESCI         OREQ      *ON
084800940131     C                   MOVE      *ON           *IN32
084900940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
085000940204     C* la ripristinerò a conclusione del ciclo di READC
085100940223     C                   MOVE      *OFF          $INZS1
0852009402233-   C                   ENDIF
085300940223     C*
085400940131     C                   Z-ADD     0             S1OPZ
085500940223     C*
085700940607     C                   UPDATE    S1
085800940223     C*
085900940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0860009401313    C     $ESCI         IFEQ      *OFF
086100940607     C                   READC     S1                                     21
086200940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0863009402014    C     *IN21         IFEQ      *ON
086400940201     C                   MOVE      WINZS1        $INZS1
086500940204     C* calcolo pagina a cui deve posizionarsi
086600940224     C                   EXSR      CLCPAG
0867009402014-   C                   ENDIF
0868009402013-   C                   ENDIF
086900940131     C*
0870009401272-   C                   ENDDO
087100940127     C*
0872009401311-   C                   ENDIF
087300940131     C*
087400940127     C                   ENDSR
087500940127     C/EJECT
087600940127     C************************************************************
087700940131     C* CONTROLLO CAMPI I/O RIGA LISTA
087800940127     C************************************************************
087900940131     C     RECS1         BEGSR
088000940131     C*
088100940201     C* reset indicatori DSPATR
088200940201     C                   MOVE      *ALL'0'       IN4049           10
088300940201     C                   MOVEA     IN4049        *IN(40)
088400940201     C*
093500940131     C                   ENDSR
093700940131     C************************************************************
093800940131     C* GESTIONE OPZIONI LISTA
093900940131     C************************************************************
094000940131     C     OPZS1         BEGSR
094100060927     C*******
094301060927     C*******            RESET                   tabds
097600060927     C*******            MOVEL     S1OPZ         xtaopz
097700060927     C*******            MOVE      *ZERO         xtaret
097701060927     C*******            MOVE      *ZERO         xtaopr
097800060927     C*******            MOVEl     S1COD         xtakey
098100060927     C*******            MOVE      *BLANKS       KPJBU
098200060927     C*******            MOVEL     tabds         KPJBU
098300060927$004 C*******            CALL      'TRTB80R2'
098400060927     C*******            PARM                    KPJBA
098401060927     C*******            MOVEL     KPJBU         tabds
098600060927     C*******
098700060927     C* ritorno da PGM gestione
098800060927     C*******            EXSR      GESRET
098900060927     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0990000609272    C******IN99         IFEQ      *ON
099100060927     C*******            SETON                                        40
0992000609272-   C*******            ENDIF
099201060927     C*
099202060927     C                   if        S1OPZ = 1
099203060927     c                   movel     s1cod         kpjbu
099204060927     C                   MOVE      *ON           $ESCI
099205060927     C                   MOVE      *ON           $FINE
099206060927     c                   end
099600940225     C*
099700940131     C                   ENDSR
099800940223     C/EJECT
099900940223     C************************************************************
100000940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
100100940223     C************************************************************
100200940223     C     GESRET        BEGSR
100300940223     C*
100400940223     C* modo di ritorno
100500940223     C*
1006009402231    C                   SELECT
100700940314    >C* << questi modi di utilizzo dei valori di ritorno dal
100800940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
100900940314    >C*    proposte, normalmente sempre valide, ma modificabili
101000940314    >C*    per situazioni particolari >>
101100940223     C* 1 = F3
101200030113    >C     xtaret        WHENEQ    '1'
101300940224     C                   MOVE      *ON           $ESCI
101400940223     C                   MOVE      *ON           $FINE
101500940223     C* 2 = F12
101600030113    >C     xtaret        WHENEQ    '2'
101700940223     C                   MOVE      *ON           $ESCI
101800940223     C*
1019009402231-   C                   ENDSL
102000940223     C*
102100940223     C* operazione eseguite dal pgm chiamato
102200940223     C*
1023009402231    C                   SELECT
102400940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
102500030113    >C     xtaopr        WHENEQ    '1'
102600940223     C                   MOVE      *ON           WINZS1
102700940223     C*
1028009402231-   C                   ENDSL
102900940223     C*
103000940223     C* funzione non eseguibile per errore :
103100940223     C*
1032009402231    C                   SELECT
103300940223     C* 1 = funzione richiamata chiusa in errore
103400940316    >C*  eventualmente gestire altri codici di errore
103500030113    >C     xtaerr        WHENEQ    '1'
103600940223     C                   MOVE      *ON           $ESCI
103700940223     C                   SETON                                        5299
103800940223     C*
1039009402231-   C                   ENDSL
104000940223     C*
104100940223     C                   ENDSR
104200940223     C/EJECT
104300940131     C************************************************************
104400940131     C* OPERAZIONI INIZIALI
104500940131     C************************************************************
104600940131     C     *INZSR        BEGSR
104601030113     C*
104602030113     C* Reperimento parametri
104603030113     C     *ENTRY        PLIST
104604030113     C                   PARM                    KPJBA
104605030113     C*
104606030113     C* Variabili per gestione videate
104607030113     C                   MOVE      *BLANK        $GEST             2
104608030113     C                   MOVE      *BLANK        $FINE             1
104609030113     C                   MOVE      *BLANK        $INZS1            1
104612030113     C                   MOVE      *BLANK        $EFILE            1
104613030113     C                   MOVE      *BLANK        $ESCI             1
104614030113     C                   MOVE      *BLANK        $RCDOK            1
104615030113     C*
104616060927     C     KTBE          KLIST
104617060927     C                   KFLD                    TBECOD
104619030113     C* Indici
104620030113     C                   Z-ADD     0             X                 3 0
104621030113     C                   Z-ADD     0             Y                 3 0
105700940506     C*
105800940506     C* Reperimento tasti di funzione
106201060927     C                   MOVE      'SOD'         tbecod
106300940127     C*
106400940117     C                   ENDSR
144600030113     C************************************************************
