000100140529     H*  Nome programma:  TNSB08R1
000200140529     H*  Descrizione   :  INTERROGAZIONE COMPETENZE DI CONTO ECONOMICO
000300140529     H*                   SUBFILE DI VISUALIZZAZIONE
000400140529     H*  Autore        :  EDPAB
000500140529     H*  Data creazione:  GIUGNO 2014
000600140529     H*------------------------------------------------------------------------*
000700940211     H DECEDIT('0,') DATEDIT(*DMY.)
000800940224      *
000900040930      *  11           x selezione di un codice da ripassare al pgm chiamante
001000940307      *  21           GENERICO OPERAZIONI I/O
001100940224      *  22           GENERICO ERRORE OPERAZIONI I/O
001200940224      *  30           SFLDSP
001300940224      * N31           SFLCLR
001400940224      *  31           SFLDSPCTL
001500940224      *  32           SFLNXTCHG
001600940224      *  33           SFLEND
001700940224      *  39           OF PRTF
001800940224      *  40 <---> 49  DSPATR ERRORI SU SFL
001900940608      *  Specificare l'uso dei singoli indicatori
002000940224      *  50 <---> 98  ERRORI SU VIDEO
002100940608      *  Specificare l'uso dei singoli indicatori
002200940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002300940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002400940224      *  99           INDIC. GENERALE DI ERRORE
002500940128     F*----------------------------------------------------*
002600140529     FECCET30C  IF   E           K DISK
002700140529     FECCED30C  IF   E           K DISK
002800140530     FECCEV30C  IF   E           K DISK
002900140530     FECC2V30C  IF   E           K DISK
003000140529     FECECT01L  IF   E           K DISK
003100140529     FECEVT02L  IF   E           K DISK
003200140529     FTnsb08D1  CF   E             WORKSTN
003300940607     F                                     SFILE(S1:S1NRR)
003400940201     F                                     INFDS(DSFMT)
003500940128     D*----------------------------------------------------*
003600140529     D DATA_eur        S               D   DATFMT(*eur)
003700140530     D*----------------------------------------------------*
003800140529     D* DESCRIZIONE TOTALE COMPETENZE
003900140530     D*----------------------------------------------------*
004000140529     D TODIMP          C                   CONST('TOTALE COMPETENZE')
004100140530     D*----------------------------------------------------*
004200140529     D* COMPETENZE DI C/E
004300140530     D*----------------------------------------------------*
004400140529     D CMP             S              3  0 DIM(500)                             *CODICE
004500140530     D IMP             S             13  5 DIM(500)                             *IMP. COMP
004600140530     D CMD             S             30    DIM(500)                             *DES. COMP
004700140530     D IMV             S             13  5 DIM(500)                             *IMP. VOCE
004800140530     D VOD             S             30    DIM(500)                             *DES. VOCE
004900140529     D*----------------------------------------------------*
005000140530     D VOC             s              3  0 DIM(100)                             *COD. VOCE
005100140530     D IVC             s             13  5 DIM(100)                             *IMP. VOCE
005200140530     D*----------------------------------------------------*
005300140530     D* DS REPERIMENTO DATI UTENTE
005400140530     D*----------------------------------------------------*
005500140530     D TIBS34DS      E DS                                                       *Profili utente
005600140530     D DDATIUTE      E DS                                                       *Dati utente
005700140530     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
005800140530     D*------------------
005900140530     D* CALL XSRDA8 - CONTROLLO DATE
006000140530     D*------------------
006100140530     D WLBDA8          DS                  INZ
006200140530     D  G08DAT                 1      8  0
006300140530     D  G08INV                 9     16  0
006400140530     D  G08ERR                17     17
006500140530     D  G08TGI                18     22  0
006600140529     D*------------------
006700140529     D* DATA SPEDIZIONE
006800140529     D*------------------
006900140529     D                 DS
007000140529     D  TASAAS                 1      4  0
007100140529     D  TASMGS                 5      8  0
007200140529     D  TASDSP                 1      8  0
007300140529     D*------------------
007400940211     D* Passaggio Parametri
007500940211     D KPJBA         E DS
007600030113      *-------------
007700940325     D* Parametri in ricezione
007800140529     D  TNSB08DS1    E DS
007900940211     D*-------------
008000940211     D DSFMT           DS           512
008100940506     D  $TASTO               369    369
008200940211     D  NRG                  370    370
008300940211     D  NCL                  371    371
008400940211     D  SFLNRR               378    379B 0
008500940207     D*-------------
008600940207     D* Nome PGM a video
008700940207     D                 DS
008800940207     D  PROGR                  1     10
008900940207     D  ASTER1                 1      1    INZ('*')
009000940207     D  SIGLA                  2      9
009100940207     D  ASTER2                10     10    INZ('*')
009200940127     D*-------------
009300940127     D* Reperimento nome PGM
009400940127     D STATUS         SDS           333
009500940127     D  DSPGM            *PROC
009600030113     D*-------------
009700030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
009800030113$003 D S1NRR           S                   Like(C1rcd)
009900030113$003 D WSfl            S                   Like(C1nrr)
010000030113$003 D Wmax            S                   Like(C1rcd)
010100030113$003 D Wpag            S                   Like(C1rcd)
010200030113$003 D Winzs1          S                   Like($inzs1)
010300940207     D*-------------
010400940211     D* COSTANTI
010500940211     D*-------------
010600030113     D SFLPAG          C                   CONST(11)
010700940314     D* dimensione della schiera $MS1
010800940506     D*
010900940506     D* Tasti di funzione
011000940506     D F01             C                   CONST(X'31')
011100940506     D F02             C                   CONST(X'32')
011200940506     D F03             C                   CONST(X'33')
011300940506     D F04             C                   CONST(X'34')
011400940506     D F05             C                   CONST(X'35')
011500940506     D F06             C                   CONST(X'36')
011600940506     D F07             C                   CONST(X'37')
011700940506     D F08             C                   CONST(X'38')
011800940506     D F09             C                   CONST(X'39')
011900940506     D F10             C                   CONST(X'3A')
012000940506     D F11             C                   CONST(X'3B')
012100940506     D F12             C                   CONST(X'3C')
012200940506     D F13             C                   CONST(X'B1')
012300940506     D F14             C                   CONST(X'B2')
012400940506     D F15             C                   CONST(X'B3')
012500940506     D F16             C                   CONST(X'B4')
012600940506     D F17             C                   CONST(X'B5')
012700940506     D F18             C                   CONST(X'B6')
012800940506     D F19             C                   CONST(X'B7')
012900940506     D F20             C                   CONST(X'B8')
013000940506     D F21             C                   CONST(X'B9')
013100940506     D F22             C                   CONST(X'BA')
013200940506     D F23             C                   CONST(X'BB')
013300940506     D F24             C                   CONST(X'BC')
013400940506     D ENTER           C                   CONST(X'F1')
013500940506     D ROLDWN          C                   CONST(X'F4')
013600940506     D ROLLUP          C                   CONST(X'F5')
013700940127     C*----------------------------------------------------*
013800940127     C*                MAIN LINE PROGRAM
013900940127     C*----------------------------------------------------*
014000940223     C* inizializzazione variabili
014100940223     C                   EXSR      INZVAR
014200940223     C*
014300940223     C     $FINE         DOWEQ     *OFF
014400940131     C     $GEST         CASEQ     'S1'          GESS1
014500940117     C                   END
014600940117     C                   END
014700940325     C* fine programma
014800940325     C                   SETON                                            LR
014900030113     C************************************************************
015000030113     C* INIZIALIZZAZIONE VARIABILI
015100030113     C************************************************************
015200030113     C     INZVAR        BEGSR
015300030113     C*
015400030113     C* Pulizia campi e indicatori
015500030113     C                   MOVE      *ALL'0'       IN4049           10
015600030113     C                   MOVEA     IN4049        *IN(40)
015700030113     C*
015800140529     C* Variabili per gestione videate
015900030113     C                   MOVE      *OFF          $FINE
016000030113     C                   MOVE      *OFF          $INZS1
016100030113     C                   MOVE      *OFF          $EFILE
016200030113     C                   MOVE      *OFF          $ESCI
016300030113     C                   MOVE      *OFF          $RCDOK
016400030113     C                   Z-ADD     0             $ULKS1            3 0
016500030113     C*
016600030113     C                   MOVE      *ON           $INZS1
016700030113     C                   MOVE      'S1'          $GEST
016800030113     C*
016900030113     C* Variabili appoggio
017000030114     C                   Z-ADD     1             WPAG
017100030113     c*
017200030113     C                   ENDSR
017300940127     C************************************************************
017400940131     C* GESTIONE LISTA
017500940127     C************************************************************
017600940127     C     GESS1         BEGSR
017700030113     C*
017800940223     C* inizializzazione videata
017900940223     C     $INZS1        IFEQ      *ON
018000940127     C                   EXSR      INZS1
018100940223     C                   MOVE      *OFF          $INZS1
018200940127     C                   ENDIF
018300030113     C*
018400030113     C* emissione piede videata
018500030113     C                   WRITE     Z1
018600030113     C* Non ci sono records
018700940223     C     WMAX          IFEQ      0
018800940607     C                   WRITE     D1
018900030114     C                   Else
019000120412     C     Wsfl          IFgt      Wmax
019100120412     c                   z-add     wmax          Wsfl
019200120412     c                   end
019300030114     C     Wsfl          IFgt      0
019400030114     C                   Z-ADD     wsfl          C1RCD
019500030114     C                   Else
019600030114     C     Wpag          IFgt      0
019700030114     C                   Z-ADD     wpag          C1RCD
019800030114     C                   EndIF
019900030114     C                   EndIF
020000030114     C                   ENDIF
020100940127     C*
020200030113     C*              *------------------*
020300940607     C                   EXFMT     C1
020400030113     C*              *------------------*
020500030113     C*
020600940204     C     C1NRR         IFNE      0
020700940204     C                   Z-ADD     C1NRR         WSFL
020800940204     C                   ENDIF
020900940127     C                   Z-ADD     SFLNRR        C1RCD
021000030113     C* Selezioni
0211009401271    C                   SELECT
021200140529     C* F12=Fine
021300140529     C     $TASTO        WHENEQ    F12
021400140529     C                   EXSR      F12S1
0215009401271O   C                   OTHER
021600940127     C* CONTROLLO DATI
021700940131     C                   EXSR      CTRC1
021800940201     C     *IN99         IFEQ      *OFF
021900940131     C                   EXSR      CTRS1
022000940131     C                   END
0221009401271-   C                   ENDSL
022200940127     C*
022300940127     C                   ENDSR
022400940224     C/EJECT
022500940127     C************************************************************
022600940131     C* INIZIALIZZAZIONE LISTA
022700940127     C************************************************************
022800940127     C     INZS1         BEGSR
022900940302     C* pulizia SFL
023000940128     C                   SETOFF                                         3031
023100940607     C                   WRITE     C1
023200940128     C                   SETON                                          31
023300940128     C*
023400030113     C* CARICAMENTO SFL totale
023500940201     C                   Z-ADD     0             S1NRR
023600030113     C                   Z-ADD     1             C1RCD
023700940128     C                   Z-ADD     0             WMAX
023800120412     C                   Z-ADD     1             WPAG
023900140529     C*
024000140529     C* IMPOSTO ANCHE I DEPOSITI PER CONFRONTI SUCCESSIVI
024100140529     C                   Z-ADD     D08AAS        VCCAAS                         *ANNO
024200140529     C                   Z-ADD     D08LNP        VCCLNP                         *LINEA DI PARTENZA
024300140529     C                   Z-ADD     D08NRS        VCCNRS                         *SERIE
024400140529     C                   Z-ADD     D08NSP        VCCNSP                         *SPEDIZIONE
024500140529     C                   MOVEL     D08TBL        VCCTBL                         *TIPO BOLLA
024600140529     C                   Z-ADD     D08DSP        VCCDSP                         *DATA SPEDIZIONE
024700140529     c     *ISO          move      D08dsp        DATA_eur
024800140529     c                   move      DATA_eur      VCCdsp
024900140529     C*
025000140530     C* CARICA TUTTE LE VOCI       DELLA BOLLA IN SCHIERA
025100140530     C                   EXSR      CARVOC
025200140529     C*
025300140530     C* CARICA TUTTE LE COMPETENZE DELLA BOLLA IN SCHIERA
025400140530     C                   EXSR      CARCMP
025500140530     C*
025600140529     C* NUMERO BLOCCO COMPETENZE
025700140529     C                   Z-ADD     CETNBC        VCCNBC
025800140529     C*
025900140529     C* POSIZIONAMENTO ARCHIVIO (SCHIERE PRECEDENTEMENTE CARICATE)
026000140529     C* Posizionamento su file pilota
026100140529     C                   Z-ADD     *ZEROS        J
026200940224     C*
026300940608    >C                   EXSR      REDANA
026400030113     C* Carico il SFL
026500940127     C                   EXSR      ROLS1
026600030113     C*
026700030114     C                   Z-ADD     1             WPAG
026800940127     C*
026900940127     C                   ENDSR
027000940127     C************************************************************
027100940131     C* CARICAMENTO PAGINA LISTA
027200940127     C************************************************************
027300940127     C     ROLS1         BEGSR
027400940127     C*
027500940128     C                   SETOFF                                       32
027600940223     C                   Z-ADD     0             Y
027700940127     C                   Z-ADD     WMAX          S1NRR
027800940127     C*
027900940127     C* Leggo dal file anagrafico per caricare la lista
0280009401311    C     $EFILE        DOWEQ     *OFF
028100140529     C*
028200140529     c                   clear                   S1
028300140529     C*
028400140529     C*   Se COMPetenza
028500140529     c                   if        CMD(J) <> *blank
028600140529     C                   eval      s1cod = %editc(CMP(J):'Z')
028700140529     C                   eval      s1des = CMD(J)
028800140529     C                   Z-ADD     IMP(J)        S1IMP
028900140530     c                   setoff                                       45
029000140530      *   Se VOCE
029100140529     c                   elseif    VOD(J) <> *blank
029200140530      *
029300140529     C                   eval      %subst(s1cod:4:3) = %editc(CMP(J):'Z')
029400140530     C*******            eval      %subst(s1des:4:30) = VOD(J)
029500140530     C                   movel     VOD(J)        S1DES
029600140529     C                   Z-ADD     IMV(J)        S1IMP
029700140530     c                   seton                                        45
029800140529     c                   end
029900940127     C*
030000940127     C                   ADD       1             S1NRR
030100940127     C                   ADD       1             Y
030200940607     C                   WRITE     S1
030300940131     C*
030400940316    >C                   EXSR      REDANA
030500940128     C*
0306009401271-   C                   ENDDO
030700940127     C*
030800940223     C                   Z-ADD     S1NRR         WMAX                 30
030900940127     C*
031000940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
031100940127     C*
031200030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
031300940127     C                   MVR                     RESTO             3 0
031400030114     C     PAGINE        MULT      SFLPAG        C1RCD
0315000301141    C     RESTO         IFGT      0
031600030114     C                   ADD       1             C1RCD
0317000301141E   C                   ELSE
031800030114     C                   SUB       SFLPAG        C1RCD
031900030114     C                   ADD       1             C1RCD
0320000301141-   C                   ENDIF
032100940128     C*
032200940127     C                   ENDSR
032300940128     C************************************************************
032400940131     C* LETTURA RCD ARCHIVIO PILOTA
032500940128     C************************************************************
032600940607     C     REDANA        BEGSR
032700940128     C*
032800940131     C                   MOVEL     *OFF          $EFILE
032900940131     C                   MOVEL     *OFF          $RCDOK
033000940131     C*
0331009401311    C     $EFILE        DOUEQ     *ON
033200940131     C     $RCDOK        OREQ      *ON
033300940131     C*
033400140529     C                   ADD       1             J                              *INDICE SCHIERE VIDE
033500140529     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
033600140529     C     IMP(J)        IFEQ      *ZEROS                                       *SCHIERA VUOTA
033700140530     C     CMP(J)        andEQ     *ZEROS
033800140529     C                   MOVEL     *ON           $EFILE
033900140529     C                   MOVE      $EFILE        *IN33
034000140529     c                   else
034100140529     C                   MOVE      *ON           $RCDOK
034200140529E   1C                   ENDIF
034300940131     C*
0344009401311-   C                   ENDDO
034500940131     C*
034600940131     C                   ENDSR
034700940224     C************************************************************
034800940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
034900940224     C************************************************************
035000940224     C     CLCPAG        BEGSR
035100940224     C* Input :
035200940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
035300940224     C*          cursore
035400940224     C* - SFLPAG = numero rcd per pagina sfl
035500940224     C* Output :
035600940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
035700940224     C*
035800120412     c                   if        WSFL >0
035900940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
036000940224     C                   MVR                     RESTO             3 0
036100940224     C     RESTO         IFGT      0
036200940224     C                   ADD       1             PAGINE
036300940224     C                   ENDIF
036400940224     C     PAGINE        MULT      SFLPAG        WPAG
036500120412     c                   else
036600120412     C                   Z-ADD     1             WPAG
036700120412     c                   end
036800940224     C*
036900940224     C                   ENDSR
037000940309     C************************************************************
037100140529     C* GESTIONE F12 VIDEO S1
037200940309     C************************************************************
037300140529     C     F12S1         BEGSR
037400940309     C*
037500940309     C                   MOVE      *ON           $FINE
037600940325     C* fine programma
037700940309     C                   ENDSR
037800940309     C/EJECT
037900940128     C************************************************************
038000940131     C* CONTROLLO TESTATA LISTA
038100940128     C************************************************************
038200940131     C     CTRC1         BEGSR
038300940128     C*
038400940201     C                   MOVE      *OFF          *IN99
038500940131     C*
038600940202     C                   ENDSR
038700940131     C************************************************************
038800940131     C* CONTROLLO OPZIONI LISTA
038900940131     C************************************************************
039000940131     C     CTRS1         BEGSR
039100940131     C*
039200940202     C                   MOVEL     *OFF          $ESCI
039300940201     C                   SETOFF                                       99
039400940131     C*
039500940127     C* Leggo il sfl solo se ci sono rcd
0396009401311    C     WMAX          IFGT      0
039700940607     C                   READC     S1                                     21
039800940127     C*
039900940131     C* esce se fine sfl o errore che richiede l'uscita
0400009401312    C     *IN21         DOWEQ     *OFF
040100940131     C     $ESCI         ANDEQ     *OFF
040200940201     C                   Z-ADD     S1NRR         C1RCD
040300940131     C* ctrl su riga
040400940131     C                   EXSR      RECS1
040500140529      *
040600940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0407009402013    C     *IN99         IFEQ      *ON
040800940201     C     $ESCI         OREQ      *ON
040900940131     C                   MOVE      *ON           *IN32
041000940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
041100940204     C* la ripristinerò a conclusione del ciclo di READC
041200940223     C                   MOVE      *OFF          $INZS1
0413009402233-   C                   ENDIF
041400940223     C*
041500940607     C                   UPDATE    S1
041600940223     C*
041700940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0418009401313    C     $ESCI         IFEQ      *OFF
041900940607     C                   READC     S1                                     21
042000940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0421009402014    C     *IN21         IFEQ      *ON
042200940201     C                   MOVE      WINZS1        $INZS1
042300940204     C* calcolo pagina a cui deve posizionarsi
042400940224     C                   EXSR      CLCPAG
0425009402014-   C                   ENDIF
0426009402013-   C                   ENDIF
042700940131     C*
0428009401272-   C                   ENDDO
042900940127     C*
0430009401311-   C                   ENDIF
043100940131     C*
043200940127     C                   ENDSR
043300940127     C/EJECT
043400940127     C************************************************************
043500940131     C* CONTROLLO CAMPI I/O RIGA LISTA
043600940127     C************************************************************
043700940131     C     RECS1         BEGSR
043800940131     C*
043900940201     C* reset indicatori DSPATR
044000940201     C                   MOVE      *ALL'0'       IN4049           10
044100940201     C                   MOVEA     IN4049        *IN(40)
044200040930     C*
044300940131     C                   ENDSR
044400140530     C*--------------------------------------------------------------------------------------------*
044500140530     C* REPERISCE I DATI UTENTE
044600140530     C*--------------------------------------------------------------------------------------------*
044700140530     C     REPDATIUTE    BEGSR
044800140530     C*
044900140530     C* INIZIALIZZA VARIABILI DI WRK
045000140530     C                   CLEAR                   TIBS34DS
045100140530     C                   CLEAR                   AZUTEDS
045200140530     C                   CLEAR                   DDATIUTE
045300140530     C*
045400140530     C     *DTAARA       DEFINE    §azute        azuteds
045500140530     C     *DTAARA       DEFINE    §datiute      ddatiute
045600140530     C                   IN(E)     *DTAARA
045700140530if  1C                   IF        %Error
045800140530     c                   EVAL      I34Tla = 'L'
045900140530     C                   CALL      'TIBS34R'
046000140530     C                   PARM                    Tibs34Ds
046100140530     C                   IN        *DTAARA
046200140530e   1C                   ENDIF
046300140530     C*
046400140530     C                   ENDSR
046500140530     C*------------------------------------------------------------------------*
046600940131     C* OPERAZIONI INIZIALI
046700140530     C*------------------------------------------------------------------------*
046800940131     C     *INZSR        BEGSR
046900030113     C*
047000030113     C* Reperimento parametri
047100030113     C     *ENTRY        PLIST
047200030113     C                   PARM                    KPJBA
047300140529     C                   movel     kpjbu         tnsb08ds1
047400140529     C*--------------------
047500140529     C* VARIABILI RIFERITE AL DATA BASE
047600140529     C*--------------------
047700140529     C     *LIKE         DEFINE    CETAAS        KCTAAS                         *ECCET30C
047800140529     C     *LIKE         DEFINE    CETLNP        KCTLNP
047900140529     C     *LIKE         DEFINE    CETNRS        KCTNRS
048000140529     C     *LIKE         DEFINE    CETNSP        KCTNSP
048100140529     C     *LIKE         DEFINE    CETTBL        KCTTBL
048200140529     C     *LIKE         DEFINE    CEDAAS        KCDAAS                         *ECCED30C
048300140529     C     *LIKE         DEFINE    CEDLNP        KCDLNP
048400140529     C     *LIKE         DEFINE    CEDNRS        KCDNRS
048500140529     C     *LIKE         DEFINE    CEDNSP        KCDNSP
048600140529     C     *LIKE         DEFINE    CEDTBL        KCDTBL
048700140529     C     *LIKE         DEFINE    ECTNBC        KETNBC                         *ECECT01L
048800140529     C     *LIKE         DEFINE    ECTCMP        KETCMP
048900140529     C     *LIKE         DEFINE    ECTDDC        KETDDC
049000140529     C     *LIKE         DEFINE    D08AAS        WAAS
049100140529     C     *LIKE         DEFINE    D08LNP        WLNP
049200140529     C     *LIKE         DEFINE    D08NRS        WNRS
049300140529     C     *LIKE         DEFINE    D08NSP        WNSP
049400140529     C     *LIKE         DEFINE    D08TBL        WTBL
049500040930     C*
049600030113     C* Variabili per gestione videate
049700030113     C                   MOVE      *BLANK        $GEST             2
049800030113     C                   MOVE      *BLANK        $FINE             1
049900030113     C                   MOVE      *BLANK        $INZS1            1
050000030113     C                   MOVE      *BLANK        $EFILE            1
050100030113     C                   MOVE      *BLANK        $ESCI             1
050200030113     C                   MOVE      *BLANK        $RCDOK            1
050300030113     C*
050400140529     C* LETTURA ECCET30C - COMPLETA
050500140529     C     KEYCET        KLIST
050600140529     C                   KFLD                    KCTAAS                         *ANNO
050700140529     C                   KFLD                    KCTLNP                         *LINEA DI PARTENZA
050800140529     C                   KFLD                    KCTNRS                         *SERIE
050900140529     C                   KFLD                    KCTNSP                         *SPEDIZIONE
051000140529     C                   KFLD                    KCTTBL                         *TIPO BOLLA
051100140529     C* LETTURA ECCED30C - COMPLETA
051200140529     C     KEYCED        KLIST
051300140529     C                   KFLD                    KCDAAS                         *ANNO
051400140529     C                   KFLD                    KCDLNP                         *LINEA DI PARTENZA
051500140529     C                   KFLD                    KCDNRS                         *SERIE
051600140529     C                   KFLD                    KCDNSP                         *SPEDIZIONE
051700140529     C                   KFLD                    KCDTBL                         *TIPO BOLLA
051800140529     C* LETTURA ECECT01L - COMPLETA
051900140529     C     KEYECT        KLIST
052000140529     C                   KFLD                    KETNBC                         *NUMERO BLOCCO
052100140529     C                   KFLD                    KETCMP                         *CODICE COMPETENZA
052200140529     C                   KFLD                    KETDDC                         *DATA DECORRENZA
052300140529     C* LETTURA ECECT01L - PARZIALE
052400140529     C     KE2ECT        KLIST
052500140529     C                   KFLD                    KETNBC                         *NUMERO BLOCCO
052600140529     C                   KFLD                    KETCMP                         *CODICE COMPETENZA
052700030113     C* Indici
052800030113     C                   Z-ADD     0             X                 3 0
052900030113     C                   Z-ADD     0             Y                 3 0
053000940127     C*
053100140530     C* LANCIO ROUTINE PER VERIFICARE SE
053200140530     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
053300140530     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
053400140530     C                   EXSR      REPDATIUTE
053500140530     C*
053600940117     C                   ENDSR
053700140529     C*------------------------------------------------------------------------*
053800140530     C* CARVOC - CARICA TUTTE LE VOCI       DELLA BOLLA IN SCHIERA
053900140529     C*------------------------------------------------------------------------*
054000140530     C     CARVOC        BEGSR
054100140530     C*
054200140530     C* LEGGE LE VOCI
054300140530     C                   clear                   VoC
054400140530     C                   clear                   IvC
054500140530      *---
054600140530     C                   Z-ADD     VCCAAS        KCTAAS                         *ANNO
054700140530     C                   Z-ADD     VCCLNP        KCTLNP                         *LINEA DI PARTENZA
054800140530     C                   Z-ADD     VCCNRS        KCTNRS                         *SERIE
054900140530     C                   Z-ADD     VCCNSP        KCTNSP                         *SPEDIZIONE
055000140530     C                   MOVEL     VCCTBL        KCTTBL                         *TIPO BOLLA
055100140530      *---
055200140530     C     KEYCET        CHAIN     ECCEV30C
055300140530IF  1C                   IF        %FOUND(ECCEV30C)                             *TROVATE
055400140530     c                   eval      VOC(01) = cevc01
055500140530     c                   eval      VOC(02) = cevc02
055600140530     c                   eval      VOC(03) = cevc03
055700140530     c                   eval      VOC(04) = cevc04
055800140530     c                   eval      VOC(05) = cevc05
055900140530     c                   eval      VOC(06) = cevc06
056000140530     c                   eval      VOC(07) = cevc07
056100140530     c                   eval      VOC(08) = cevc08
056200140530     c                   eval      VOC(09) = cevc09
056300140530     c                   eval      VOC(10) = cevc10
056400140530     c                   eval      VOC(11) = cevc11
056500140530     c                   eval      VOC(12) = cevc12
056600140530     c                   eval      VOC(13) = cevc13
056700140530     c                   eval      VOC(14) = cevc14
056800140530     c                   eval      VOC(15) = cevc15
056900140530     c                   eval      VOC(16) = cevc16
057000140530     c                   eval      VOC(17) = cevc17
057100140530     c                   eval      VOC(18) = cevc18
057200140530     c                   eval      VOC(19) = cevc19
057300140530     c                   eval      VOC(20) = cevc20
057400140530     c                   eval      VOC(21) = cevc21
057500140530     c                   eval      VOC(22) = cevc22
057600140530     c                   eval      VOC(23) = cevc23
057700140530     c                   eval      VOC(24) = cevc24
057800140530     c                   eval      VOC(25) = cevc25
057900140530     c                   eval      VOC(26) = cevc26
058000140530     c                   eval      VOC(27) = cevc27
058100140530     c                   eval      VOC(28) = cevc28
058200140530     c                   eval      VOC(29) = cevc29
058300140530     c                   eval      VOC(30) = cevc30
058400140530     c                   eval      VOC(31) = cevc31
058500140530     c                   eval      VOC(32) = cevc32
058600140530     c                   eval      VOC(33) = cevc33
058700140530     c                   eval      VOC(34) = cevc34
058800140530     c                   eval      VOC(35) = cevc35
058900140530     c                   eval      VOC(36) = cevc36
059000140530     c                   eval      VOC(37) = cevc37
059100140530     c                   eval      VOC(38) = cevc38
059200140530     c                   eval      VOC(39) = cevc39
059300140530     c                   eval      VOC(40) = cevc40
059400140530     c                   eval      VOC(41) = cevc41
059500140530     c                   eval      VOC(42) = cevc42
059600140530     c                   eval      VOC(43) = cevc43
059700140530     c                   eval      VOC(44) = cevc44
059800140530     c                   eval      VOC(45) = cevc45
059900140530     c                   eval      VOC(46) = cevc46
060000140530     c                   eval      VOC(47) = cevc47
060100140530     c                   eval      VOC(48) = cevc48
060200140530     c                   eval      VOC(49) = cevc49
060300140530     c                   eval      VOC(50) = cevc50
060400140530      *--
060500140530     c                   eval      IVC(01) = cevi01
060600140530     c                   eval      IVC(02) = cevi02
060700140530     c                   eval      IVC(03) = cevi03
060800140530     c                   eval      IVC(04) = cevi04
060900140530     c                   eval      IVC(05) = cevi05
061000140530     c                   eval      IVC(06) = cevi06
061100140530     c                   eval      IVC(07) = cevi07
061200140530     c                   eval      IVC(08) = cevi08
061300140530     c                   eval      IVC(09) = cevi09
061400140530     c                   eval      IVC(10) = cevi10
061500140530     c                   eval      IVC(11) = cevi11
061600140530     c                   eval      IVC(12) = cevi12
061700140530     c                   eval      IVC(13) = cevi13
061800140530     c                   eval      IVC(14) = cevi14
061900140530     c                   eval      IVC(15) = cevi15
062000140530     c                   eval      IVC(16) = cevi16
062100140530     c                   eval      IVC(17) = cevi17
062200140530     c                   eval      IVC(18) = cevi18
062300140530     c                   eval      IVC(19) = cevi19
062400140530     c                   eval      IVC(20) = cevi20
062500140530     c                   eval      IVC(21) = cevi21
062600140530     c                   eval      IVC(22) = cevi22
062700140530     c                   eval      IVC(23) = cevi23
062800140530     c                   eval      IVC(24) = cevi24
062900140530     c                   eval      IVC(25) = cevi25
063000140530     c                   eval      IVC(26) = cevi26
063100140530     c                   eval      IVC(27) = cevi27
063200140530     c                   eval      IVC(28) = cevi28
063300140530     c                   eval      IVC(29) = cevi29
063400140530     c                   eval      IVC(30) = cevi30
063500140530     c                   eval      IVC(31) = cevi31
063600140530     c                   eval      IVC(32) = cevi32
063700140530     c                   eval      IVC(33) = cevi33
063800140530     c                   eval      IVC(34) = cevi34
063900140530     c                   eval      IVC(35) = cevi35
064000140530     c                   eval      IVC(36) = cevi36
064100140530     c                   eval      IVC(37) = cevi37
064200140530     c                   eval      IVC(38) = cevi38
064300140530     c                   eval      IVC(39) = cevi39
064400140530     c                   eval      IVC(40) = cevi40
064500140530     c                   eval      IVC(41) = cevi41
064600140530     c                   eval      IVC(42) = cevi42
064700140530     c                   eval      IVC(43) = cevi43
064800140530     c                   eval      IVC(44) = cevi44
064900140530     c                   eval      IVC(45) = cevi45
065000140530     c                   eval      IVC(46) = cevi46
065100140530     c                   eval      IVC(47) = cevi47
065200140530     c                   eval      IVC(48) = cevi48
065300140530     c                   eval      IVC(49) = cevi49
065400140530     c                   eval      IVC(50) = cevi50
065500140530     c                   END
065600140530     C*
065700140530     C     KEYCET        CHAIN     ECC2V30C
065800140530IF  1C                   IF        %FOUND(ECC2V30C)                             *TROVATE
065900140530     c                   eval      VOC(51) = c2vc51
066000140530     c                   eval      VOC(52) = c2vc52
066100140530     c                   eval      VOC(53) = c2vc53
066200140530     c                   eval      VOC(54) = c2vc54
066300140530     c                   eval      VOC(55) = c2vc55
066400140530     c                   eval      VOC(56) = c2vc56
066500140530     c                   eval      VOC(57) = c2vc57
066600140530     c                   eval      VOC(58) = c2vc58
066700140530     c                   eval      VOC(59) = c2vc59
066800140530     c                   eval      VOC(60) = c2vc60
066900140530     c                   eval      VOC(61) = c2vc61
067000140530     c                   eval      VOC(62) = c2vc62
067100140530     c                   eval      VOC(63) = c2vc63
067200140530     c                   eval      VOC(64) = c2vc64
067300140530     c                   eval      VOC(65) = c2vc65
067400140530     c                   eval      VOC(66) = c2vc66
067500140530     c                   eval      VOC(67) = c2vc67
067600140530     c                   eval      VOC(68) = c2vc68
067700140530     c                   eval      VOC(69) = c2vc69
067800140530     c                   eval      VOC(70) = c2vc70
067900140530     c                   eval      VOC(71) = c2vc71
068000140530     c                   eval      VOC(72) = c2vc72
068100140530     c                   eval      VOC(73) = c2vc73
068200140530     c                   eval      VOC(74) = c2vc74
068300140530     c                   eval      VOC(75) = c2vc75
068400140530      *--
068500140530     c                   eval      IVC(51) = c2vi51
068600140530     c                   eval      IVC(52) = c2vi52
068700140530     c                   eval      IVC(53) = c2vi53
068800140530     c                   eval      IVC(54) = c2vi54
068900140530     c                   eval      IVC(55) = c2vi55
069000140530     c                   eval      IVC(56) = c2vi56
069100140530     c                   eval      IVC(57) = c2vi57
069200140530     c                   eval      IVC(58) = c2vi58
069300140530     c                   eval      IVC(59) = c2vi59
069400140530     c                   eval      IVC(60) = c2vi60
069500140530     c                   eval      IVC(61) = c2vi61
069600140530     c                   eval      IVC(62) = c2vi62
069700140530     c                   eval      IVC(63) = c2vi63
069800140530     c                   eval      IVC(64) = c2vi64
069900140530     c                   eval      IVC(65) = c2vi65
070000140530     c                   eval      IVC(66) = c2vi66
070100140530     c                   eval      IVC(67) = c2vi67
070200140530     c                   eval      IVC(68) = c2vi68
070300140530     c                   eval      IVC(69) = c2vi69
070400140530     c                   eval      IVC(70) = c2vi70
070500140530     c                   eval      IVC(71) = c2vi71
070600140530     c                   eval      IVC(72) = c2vi72
070700140530     c                   eval      IVC(73) = c2vi73
070800140530     c                   eval      IVC(74) = c2vi74
070900140530     c                   eval      IVC(75) = c2vi75
071000140530     c                   END
071100140530     C*
071200140530     C                   ENDSR
071300140530     C*------------------------------------------------------------------------*
071400140530     C* CARCMP - CARICA TUTTE LE COMPETENZE DELLA BOLLA IN SCHIERA
071500140530     C*------------------------------------------------------------------------*
071600140530     C     CARCMP        BEGSR
071700140530     C*
071800140529     C     *LIKE         DEFINE    CEDIMP        TOTIMP
071900140529     C                   Z-ADD     *ZEROS        J                 3 0
072000140529     C*
072100140529     C* LEGGE LE COMPETENZE FISSE
072200140529     C                   Z-ADD     VCCAAS        KCTAAS                         *ANNO
072300140529     C                   Z-ADD     VCCLNP        KCTLNP                         *LINEA DI PARTENZA
072400140529     C                   Z-ADD     VCCNRS        KCTNRS                         *SERIE
072500140529     C                   Z-ADD     VCCNSP        KCTNSP                         *SPEDIZIONE
072600140529     C                   MOVEL     VCCTBL        KCTTBL                         *TIPO BOLLA
072700140529     C     KEYCET        CHAIN     ECCET30C                           99
072800140529IF  1C     *IN99         IFEQ      *OFF                                         *TROVATE
072900140529     C*
073000140529     C* CARICA LE COMPETENZE FISSE
073100140529IF  2C     CETPAR        IFGT      *ZEROS
073200140529     C                   ADD       1             J
073300140529     C                   Z-ADD     1             CMP(J)                         *CODICE  COMPETENZA
073400140529     C                   Z-ADD     CETPAR        IMP(J)                         *IMPORTO COMPETENZA
073500140529     c                   exsr      VOCI
073600140529E   2C                   ENDIF
073700140529     C*
073800140529IF  2C     CETPAM        IFGT      *ZEROS
073900140529     C                   ADD       1             J
074000140529     C                   Z-ADD     2             CMP(J)                         *CODICE  COMPETENZA
074100140529     C                   Z-ADD     CETPAM        IMP(J)                         *IMPORTO COMPETENZA
074200140529     c                   exsr      VOCI
074300140529E   2C                   ENDIF
074400140529     C*
074500140529IF  2C     CETPAU        IFGT      *ZEROS
074600140529     C                   ADD       1             J
074700140529     C                   Z-ADD     3             CMP(J)                         *CODICE  COMPETENZA
074800140529     C                   Z-ADD     CETPAU        IMP(J)                         *IMPORTO COMPETENZA
074900140529     c                   exsr      VOCI
075000140529E   2C                   ENDIF
075100140529     C*
075200140529IF  2C     CETPAV        IFGT      *ZEROS
075300140529     C                   ADD       1             J
075400140529     C                   Z-ADD     4             CMP(J)                         *CODICE  COMPETENZA
075500140529     C                   Z-ADD     CETPAV        IMP(J)                         *IMPORTO COMPETENZA
075600140529     c                   exsr      VOCI
075700140529E   2C                   ENDIF
075800140529     C*
075900140529IF  2C     CETARD        IFGT      *ZEROS
076000140529     C                   ADD       1             J
076100140529     C                   Z-ADD     5             CMP(J)                         *CODICE  COMPETENZA
076200140529     C                   Z-ADD     CETARD        IMP(J)                         *IMPORTO COMPETENZA
076300140529     c                   exsr      VOCI
076400140529E   2C                   ENDIF
076500140529     C*
076600140529IF  2C     CETARM        IFGT      *ZEROS
076700140529     C                   ADD       1             J
076800140529     C                   Z-ADD     6             CMP(J)                         *CODICE  COMPETENZA
076900140529     C                   Z-ADD     CETARM        IMP(J)                         *IMPORTO COMPETENZA
077000140529     c                   exsr      VOCI
077100140529E   2C                   ENDIF
077200140529     C*
077300140529IF  2C     CETARU        IFGT      *ZEROS
077400140529     C                   ADD       1             J
077500140529     C                   Z-ADD     7             CMP(J)                         *CODICE  COMPETENZA
077600140529     C                   Z-ADD     CETARU        IMP(J)                         *IMPORTO COMPETENZA
077700140529     c                   exsr      VOCI
077800140529E   2C                   ENDIF
077900140529     C*
078000140529IF  2C     CETARV        IFGT      *ZEROS
078100140529     C                   ADD       1             J
078200140529     C                   Z-ADD     8             CMP(J)                         *CODICE  COMPETENZA
078300140529     C                   Z-ADD     CETARV        IMP(J)                         *IMPORTO COMPETENZA
078400140529     c                   exsr      VOCI
078500140529E   2C                   ENDIF
078600140529     C*
078700140529IF  2C     CETTRL        IFGT      *ZEROS
078800140529     C                   ADD       1             J
078900140529     C                   Z-ADD     13            CMP(J)                         *CODICE  COMPETENZA
079000140529     C                   Z-ADD     CETTRL        IMP(J)                         *IMPORTO COMPETENZA
079100140529     c                   exsr      VOCI
079200140529E   2C                   ENDIF
079300140529     C*
079400140529IF  2C     CETPAG        IFGT      *ZEROS
079500140529     C                   ADD       1             J
079600140529     C                   Z-ADD     29            CMP(J)                         *CODICE  COMPETENZA
079700140529     C                   Z-ADD     CETPAG        IMP(J)                         *IMPORTO COMPETENZA
079800140529     c                   exsr      VOCI
079900140529E   2C                   ENDIF
080000140529     C*
080100140529IF  2C     CETSEA        IFGT      *ZEROS
080200140529     C                   ADD       1             J
080300140529     C                   Z-ADD     26            CMP(J)                         *CODICE  COMPETENZA
080400140529     C                   Z-ADD     CETSEA        IMP(J)                         *IMPORTO COMPETENZA
080500140529     c                   exsr      VOCI
080600140529E   2C                   ENDIF
080700140529     C*
080800140529IF  2C     CETSEC        IFGT      *ZEROS
080900140529     C                   ADD       1             J
081000140529     C                   Z-ADD     25            CMP(J)                         *CODICE  COMPETENZA
081100140529     C                   Z-ADD     CETSEC        IMP(J)                         *IMPORTO COMPETENZA
081200140529     c                   exsr      VOCI
081300140529E   2C                   ENDIF
081400140529     C*
081500140529IF  2C     CETRCV        IFGT      *ZEROS
081600140529     C                   ADD       1             J
081700140529     C                   Z-ADD     27            CMP(J)                         *CODICE  COMPETENZA
081800140529     C                   Z-ADD     CETRCV        IMP(J)                         *IMPORTO COMPETENZA
081900140529     c                   exsr      VOCI
082000140529E   2C                   ENDIF
082100140529     C*
082200140529IF  2C     CETMDE        IFGT      *ZEROS
082300140529     C                   ADD       1             J
082400140529     C                   Z-ADD     10            CMP(J)                         *CODICE  COMPETENZA
082500140529     C                   Z-ADD     CETMDE        IMP(J)                         *IMPORTO COMPETENZA
082600140529     c                   exsr      VOCI
082700140529E   2C                   ENDIF
082800140529     C*
082900140529IF  2C     CETTDE        IFGT      *ZEROS
083000140529     C                   ADD       1             J
083100140529     C                   Z-ADD     15            CMP(J)                         *CODICE  COMPETENZA
083200140529     C                   Z-ADD     CETTDE        IMP(J)                         *IMPORTO COMPETENZA
083300140529     c                   exsr      VOCI
083400140529E   2C                   ENDIF
083500140529     C*
083600140529IF  2C     CETMAF        IFGT      *ZEROS
083700140529     C                   ADD       1             J
083800140529     C                   Z-ADD     9             CMP(J)                         *CODICE  COMPETENZA
083900140529     C                   Z-ADD     CETMAF        IMP(J)                         *IMPORTO COMPETENZA
084000140529     c                   exsr      VOCI
084100140529E   2C                   ENDIF
084200140529     C*
084300140529IF  2C     CETTAF        IFGT      *ZEROS
084400140529     C                   ADD       1             J
084500140529     C                   Z-ADD     14            CMP(J)                         *CODICE  COMPETENZA
084600140529     C                   Z-ADD     CETTAF        IMP(J)                         *IMPORTO COMPETENZA
084700140529     c                   exsr      VOCI
084800140529E   2C                   ENDIF
084900140529     C*
085000140529IF  2C     CETSES        IFGT      *ZEROS
085100140529     C                   ADD       1             J
085200140529     C                   Z-ADD     33            CMP(J)                         *CODICE  COMPETENZA
085300140529     C                   Z-ADD     CETSES        IMP(J)                         *IMPORTO COMPETENZA
085400140529     c                   exsr      VOCI
085500140529E   2C                   ENDIF
085600140529     C*
085700140529     C* LEGGE LE COMPETENZE VARIABILI
085800140529IF  2C     CETDET        IFEQ      'S'                                          *SE CI SONO
085900140529     C                   Z-ADD     VCCAAS        KCDAAS                         *ANNO
086000140529     C                   Z-ADD     VCCLNP        KCDLNP                         *LINEA DI PARTENZA
086100140529     C                   Z-ADD     VCCNRS        KCDNRS                         *SERIE
086200140529     C                   Z-ADD     VCCNSP        KCDNSP                         *SPEDIZIONE
086300140529     C                   MOVEL     VCCTBL        KCDTBL                         *TIPO BOLLA
086400140530     C*
086500140529     C     KEYCED        SETLL     ECCED30C
086600140529     C     KEYCED        READE     ECCED30C                               99
086700140529DO  3C     *IN99         DOWEQ     *OFF
086800140529     C*
086900140529     C* CARICA LE COMPETENZE VARIABILI
087000140529IF  4C     CEDIMP        IFGT      *ZEROS
087100140529     C                   ADD       1             J
087200140529     C                   Z-ADD     CEDCMP        CMP(J)                         *CODICE  COMPETENZA
087300140529     C                   Z-ADD     CEDIMP        IMP(J)                         *IMPORTO COMPETENZA
087400140529     c                   exsr      VOCI
087500140529E   4C                   ENDIF
087600140529     C     KEYCED        READE     ECCED30C                               99
087700140529E   3C                   ENDDO
087800140529E   2C                   ENDIF
087900140529     C*
088000140529     C* CALCOLA IL TOTALE COMPETENZE
088100140529     C                   XFOOT     IMP           TOTIMP
088200140529     C                   ADD       1             J
088300140529     C                   Z-ADD     *ZEROS        CMP(J)                         *CODICE  COMPETENZA
088400140529     C                   Z-ADD     TOTIMP        IMP(J)                         *IMPORTO COMPETENZA
088500140529     C                   MOVEL     TODIMP        CMD(J)                         *DESCRIZ.COMPETENZA
088600140529E   1C                   ENDIF
088700140529     C*
088800140529     C                   ENDSR
088900140529     C*------------------------------------------------------------------------*
089000140529     C*  VOCI  - DECODIFICA LE SINGOLE VOCI UTILIZZATE
089100140529     C*------------------------------------------------------------------------*
089200140529     C     VOCI          BEGSR
089300140529     C*
089400140529     C*  la routine è chiamata mentre sta trattando una COMPETENZA
089500140529     C*    QUINDI:
089600140529     C*  J -> è caricato sulla riga di competenza e i campi voce devono essere vuoti
089700140529     C*
089800140529     C* Prima di passare alle singole Voci DECODIFICA la COMPETENZA appena impostata
089900140529     C*   e pulisce la parte voce
090000140529     C*
090100140529     C* DECODIFICA LE COMPETENZE (NON L'ULTIMO ELEMENTO CHE E' IL TOTALE)
090200140529IF  2C     IMP(J)        IFGT      *ZEROS
090300140529     C     CMP(J)        ANDGT     *ZEROS
090400140529     C                   Z-ADD     CETNBC        WNBC
090500140529     C                   Z-ADD     CMP(J)        WCMP
090600140529     C                   MOVEL     *BLANKS       WCMD
090700140529     C                   EXSR      CHKCMP
090800140529     C                   MOVEL     WCMD          CMD(J)
090900140529E   2C                   ENDIF
091000140529     C*
091100140529     c                   clear                   VOD(J)
091200140529     c                   clear                   IMV(J)
091300140529     C*
091400140529     C*  per ogni competenza aggiunge tante righe quante sono
091500140529     C*  le singole voci che la compongono
091600140530     c                   z-add     CMP(J)        EVTCMP
091700140530     c     EVTCMP        setll     ecEVT02l
091800140530     c     EVTCMP        reade     ecEVT02l
091900140529     C*
092000140530     c                   dow       not %EoF(ecEVT02l)
092100140530     C*
092200140530     C*  se la VOCE è quella attiva nel periodo
092300140530     c                   if        D08DSP >= EVTDDV and
092400140530     c                             D08DSP <= EVTDSV
092500140530     c                   z-add     1             vc                3 0
092600140530     c     evtVOC        lookup    VOC(vc)                                75
092700140530     c                   if        %equal
092800140530     c                   add       1             J
092900140530     c                   z-add     VOC(vc)       CMP(J)
093000140530     c                   clear                   IMP(J)
093100140530     c                   clear                   CMD(J)
093200140530     c                   z-add     IVC(vc)       IMV(J)
093300140530     c                   movel     evtVOD        VOD(J)
093400140530     c                   end
093500140530     c                   end
093600140530     C*
093700140530     c     EVTCMP        reade     ecEVT02l
093800140530     c                   endDO
093900140529     C*
094000140529     C                   ENDSR
094100140529     C*------------------------------------------------------------------------*
094200140529     C* CHKCMP - DECODIFICA LE COMPETENZE
094300140529     C*------------------------------------------------------------------------*
094400140529     C     CHKCMP        BEGSR
094500140529     C*
094600140529     C     *LIKE         DEFINE    ECTNBC        WNBC
094700140529     C     *LIKE         DEFINE    ECTCMP        WCMP
094800140529     C     *LIKE         DEFINE    ECTCMD        WCMD
094900140529     C*
095000140529     C* LEGGE, DEL BLOCCO, L'ULTIMA COMPETENZA IN DECORRENZA
095100140529     C                   Z-ADD     WNBC          KETNBC                         *NUMERO BLOCCO
095200140529     C                   Z-ADD     WCMP          KETCMP                         *CODICE COMPETENZA
095300140529     C                   Z-ADD     99999999      KETDDC                         *DATA DECORRENZA
095400140529     C     KEYECT        SETLL     ECECT01L
095500140529     C     KE2ECT        READPE    ECECT01L                               99
095600140529IF  1C     *IN99         IFEQ      *OFF
095700140529     C                   MOVEL     ECTCMD        WCMD
095800140529E   1C                   ENDIF
095900140529     C*
096000140529     C                   ENDSR
096100140529     C*------------------------------------------------------------------------*
