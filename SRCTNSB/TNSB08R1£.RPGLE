000100140529     H*  Nome programma:  TNSB08R1
000200140529     H*  Descrizione   :  INTERROGAZIONE COMPETENZE DI CONTO ECONOMICO
000300140529     H*                   SUBFILE DI VISUALIZZAZIONE
000400140529     H*  Autore        :  EDPAB
000500140529     H*  Data creazione:  GIUGNO 2014
000600140529     H*------------------------------------------------------------------------*
000700970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000800970214     H*PARMS CVTOPT(*DATETIME)
000900940211     H DECEDIT('0,') DATEDIT(*DMY.)
001000940224      *
001100040930      *  11           x selezione di un codice da ripassare al pgm chiamante
001200940307      *  21           GENERICO OPERAZIONI I/O
001300940224      *  22           GENERICO ERRORE OPERAZIONI I/O
001400940224      *  30           SFLDSP
001500940224      * N31           SFLCLR
001600940224      *  31           SFLDSPCTL
001700940224      *  32           SFLNXTCHG
001800940224      *  33           SFLEND
001900940224      *  39           OF PRTF
002000940224      *  40 <---> 49  DSPATR ERRORI SU SFL
002100940608      *  Specificare l'uso dei singoli indicatori
002200940224      *  50 <---> 98  ERRORI SU VIDEO
002300940608      *  Specificare l'uso dei singoli indicatori
002400940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002500940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002600940224      *  99           INDIC. GENERALE DI ERRORE
002700940128     F*----------------------------------------------------*
002800140529     FECCET30C  IF   E           K DISK
002900140529     FECCED30C  IF   E           K DISK
003000140530     FECCEV30C  IF   E           K DISK
003100140530     FECC2V30C  IF   E           K DISK
003200140529     FECECT01L  IF   E           K DISK
003300140529     FECEVT02L  IF   E           K DISK
003400140529     FTnsb08D1  CF   E             WORKSTN
003500940607     F                                     SFILE(S1:S1NRR)
003600940201     F                                     INFDS(DSFMT)
003700940128     D*----------------------------------------------------*
003800140529     D DATA_eur        S               D   DATFMT(*eur)
003900140530     D*----------------------------------------------------*
004000140529     D* DESCRIZIONE TOTALE COMPETENZE
004100140530     D*----------------------------------------------------*
004200140529     D TODIMP          C                   CONST('TOTALE COMPETENZE')
004300140530     D*----------------------------------------------------*
004400140529     D* COMPETENZE DI C/E
004500140530     D*----------------------------------------------------*
004600140529     D CMP             S              3  0 DIM(500)                             *CODICE
004700140530     D IMP             S             13  5 DIM(500)                             *IMP. COMP
004800140530     D CMD             S             30    DIM(500)                             *DES. COMP
004900140530     D IMV             S             13  5 DIM(500)                             *IMP. VOCE
005000140530     D VOD             S             30    DIM(500)                             *DES. VOCE
005100140529     D*----------------------------------------------------*
005200140530     D VOC             s              3  0 DIM(100)                             *COD. VOCE
005300140530     D IVC             s             13  5 DIM(100)                             *IMP. VOCE
005400140530     D*----------------------------------------------------*
005500140530     D* DS REPERIMENTO DATI UTENTE
005600140530     D*----------------------------------------------------*
005700140530     D TIBS34DS      E DS                                                       *Profili utente
005800140530     D DDATIUTE      E DS                                                       *Dati utente
005900140530     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
006000140530     D*------------------
006100140530     D* CALL XSRDA8 - CONTROLLO DATE
006200140530     D*------------------
006300140530     D WLBDA8          DS                  INZ
006400140530     D  G08DAT                 1      8  0
006500140530     D  G08INV                 9     16  0
006600140530     D  G08ERR                17     17
006700140530     D  G08TGI                18     22  0
006800140529     D*------------------
006900140529     D* DATA SPEDIZIONE
007000140529     D*------------------
007100140529     D                 DS
007200140529     D  TASAAS                 1      4  0
007300140529     D  TASMGS                 5      8  0
007400140529     D  TASDSP                 1      8  0
007500140529     D*------------------
007600940211     D* Passaggio Parametri
007700940211     D KPJBA         E DS
007800030113      *-------------
007900940325     D* Parametri in ricezione
008000140529     D  TNSB08DS1    E DS
008100940211     D*-------------
008200940211     D DSFMT           DS           512
008300940506     D  $TASTO               369    369
008400940211     D  NRG                  370    370
008500940211     D  NCL                  371    371
008600940211     D  SFLNRR               378    379B 0
008700940207     D*-------------
008800940207     D* Nome PGM a video
008900940207     D                 DS
009000940207     D  PROGR                  1     10
009100940207     D  ASTER1                 1      1    INZ('*')
009200940207     D  SIGLA                  2      9
009300940207     D  ASTER2                10     10    INZ('*')
009400940127     D*-------------
009500940127     D* Reperimento nome PGM
009600940127     D STATUS         SDS           333
009700940127     D  DSPGM            *PROC
009800030113     D*-------------
009900030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
010000030113$003 D S1NRR           S                   Like(C1rcd)
010100030113$003 D WSfl            S                   Like(C1nrr)
010200030113$003 D Wmax            S                   Like(C1rcd)
010300030113$003 D Wpag            S                   Like(C1rcd)
010400030113$003 D Winzs1          S                   Like($inzs1)
010500940207     D*-------------
010600940211     D* COSTANTI
010700940211     D*-------------
010800030113     D SFLPAG          C                   CONST(11)
010900940314     D* dimensione della schiera $MS1
011000940506     D*
011100940506     D* Tasti di funzione
011200940506     D F01             C                   CONST(X'31')
011300940506     D F02             C                   CONST(X'32')
011400940506     D F03             C                   CONST(X'33')
011500940506     D F04             C                   CONST(X'34')
011600940506     D F05             C                   CONST(X'35')
011700940506     D F06             C                   CONST(X'36')
011800940506     D F07             C                   CONST(X'37')
011900940506     D F08             C                   CONST(X'38')
012000940506     D F09             C                   CONST(X'39')
012100940506     D F10             C                   CONST(X'3A')
012200940506     D F11             C                   CONST(X'3B')
012300940506     D F12             C                   CONST(X'3C')
012400940506     D F13             C                   CONST(X'B1')
012500940506     D F14             C                   CONST(X'B2')
012600940506     D F15             C                   CONST(X'B3')
012700940506     D F16             C                   CONST(X'B4')
012800940506     D F17             C                   CONST(X'B5')
012900940506     D F18             C                   CONST(X'B6')
013000940506     D F19             C                   CONST(X'B7')
013100940506     D F20             C                   CONST(X'B8')
013200940506     D F21             C                   CONST(X'B9')
013300940506     D F22             C                   CONST(X'BA')
013400940506     D F23             C                   CONST(X'BB')
013500940506     D F24             C                   CONST(X'BC')
013600940506     D ENTER           C                   CONST(X'F1')
013700940506     D ROLDWN          C                   CONST(X'F4')
013800940506     D ROLLUP          C                   CONST(X'F5')
013900940127     C*----------------------------------------------------*
014000940127     C*                MAIN LINE PROGRAM
014100940127     C*----------------------------------------------------*
014200940223     C* inizializzazione variabili
014300940223     C                   EXSR      INZVAR
014400940223     C*
014500940223     C     $FINE         DOWEQ     *OFF
014600940131     C     $GEST         CASEQ     'S1'          GESS1
014700940117     C                   END
014800940117     C                   END
014900940325     C* fine programma
015000940325     C                   SETON                                            LR
015100030113     C************************************************************
015200030113     C* INIZIALIZZAZIONE VARIABILI
015300030113     C************************************************************
015400030113     C     INZVAR        BEGSR
015500030113     C*
015600030113     C* Pulizia campi e indicatori
015700030113     C                   MOVE      *ALL'0'       IN4049           10
015800030113     C                   MOVEA     IN4049        *IN(40)
015900030113     C*
016000140529     C* Variabili per gestione videate
016100030113     C                   MOVE      *OFF          $FINE
016200030113     C                   MOVE      *OFF          $INZS1
016300030113     C                   MOVE      *OFF          $EFILE
016400030113     C                   MOVE      *OFF          $ESCI
016500030113     C                   MOVE      *OFF          $RCDOK
016600030113     C                   Z-ADD     0             $ULKS1            3 0
016700030113     C*
016800030113     C                   MOVE      *ON           $INZS1
016900030113     C                   MOVE      'S1'          $GEST
017000030113     C*
017100030113     C* Variabili appoggio
017200030114     C                   Z-ADD     1             WPAG
017300030113     c*
017400030113     C                   ENDSR
017500940127     C************************************************************
017600940131     C* GESTIONE LISTA
017700940127     C************************************************************
017800940127     C     GESS1         BEGSR
017900030113     C*
018000940223     C* inizializzazione videata
018100940223     C     $INZS1        IFEQ      *ON
018200940127     C                   EXSR      INZS1
018300940223     C                   MOVE      *OFF          $INZS1
018400940127     C                   ENDIF
018500030113     C*
018600030113     C* emissione piede videata
018700030113     C                   WRITE     Z1
018800030113     C* Non ci sono records
018900940223     C     WMAX          IFEQ      0
019000940607     C                   WRITE     D1
019100030114     C                   Else
019200120412     C     Wsfl          IFgt      Wmax
019300120412     c                   z-add     wmax          Wsfl
019400120412     c                   end
019500030114     C     Wsfl          IFgt      0
019600030114     C                   Z-ADD     wsfl          C1RCD
019700030114     C                   Else
019800030114     C     Wpag          IFgt      0
019900030114     C                   Z-ADD     wpag          C1RCD
020000030114     C                   EndIF
020100030114     C                   EndIF
020200030114     C                   ENDIF
020300940127     C*
020400030113     C*              *------------------*
020500940607     C                   EXFMT     C1
020600030113     C*              *------------------*
020700030113     C*
020800940204     C     C1NRR         IFNE      0
020900940204     C                   Z-ADD     C1NRR         WSFL
021000940204     C                   ENDIF
021100940127     C                   Z-ADD     SFLNRR        C1RCD
021200030113     C* Selezioni
0213009401271    C                   SELECT
021400140529     C* F12=Fine
021500140529     C     $TASTO        WHENEQ    F12
021600140529     C                   EXSR      F12S1
0217009401271O   C                   OTHER
021800940127     C* CONTROLLO DATI
021900940131     C                   EXSR      CTRC1
022000940201     C     *IN99         IFEQ      *OFF
022100940131     C                   EXSR      CTRS1
022200940131     C                   END
0223009401271-   C                   ENDSL
022400940127     C*
022500940127     C                   ENDSR
022600940224     C/EJECT
022700940127     C************************************************************
022800940131     C* INIZIALIZZAZIONE LISTA
022900940127     C************************************************************
023000940127     C     INZS1         BEGSR
023100940302     C* pulizia SFL
023200940128     C                   SETOFF                                         3031
023300940607     C                   WRITE     C1
023400940128     C                   SETON                                          31
023500940128     C*
023600030113     C* CARICAMENTO SFL totale
023700940201     C                   Z-ADD     0             S1NRR
023800030113     C                   Z-ADD     1             C1RCD
023900940128     C                   Z-ADD     0             WMAX
024000120412     C                   Z-ADD     1             WPAG
024100140529     C*
024200140529     C* IMPOSTO ANCHE I DEPOSITI PER CONFRONTI SUCCESSIVI
024300140529     C                   Z-ADD     D08AAS        VCCAAS                         *ANNO
024400140529     C                   Z-ADD     D08LNP        VCCLNP                         *LINEA DI PARTENZA
024500140529     C                   Z-ADD     D08NRS        VCCNRS                         *SERIE
024600140529     C                   Z-ADD     D08NSP        VCCNSP                         *SPEDIZIONE
024700140529     C                   MOVEL     D08TBL        VCCTBL                         *TIPO BOLLA
024800140529     C                   Z-ADD     D08DSP        VCCDSP                         *DATA SPEDIZIONE
024900140529     c     *ISO          move      D08dsp        DATA_eur
025000140529     c                   move      DATA_eur      VCCdsp
025100140529     C*
025200140530     C* CARICA TUTTE LE VOCI       DELLA BOLLA IN SCHIERA
025300140530     C                   EXSR      CARVOC
025400140529     C*
025500140530     C* CARICA TUTTE LE COMPETENZE DELLA BOLLA IN SCHIERA
025600140530     C                   EXSR      CARCMP
025700140530     C*
025800140529     C* NUMERO BLOCCO COMPETENZE
025900140529     C                   Z-ADD     CETNBC        VCCNBC
026000140529     C*
026100140529     C* POSIZIONAMENTO ARCHIVIO (SCHIERE PRECEDENTEMENTE CARICATE)
026200140529     C* Posizionamento su file pilota
026300140529     C                   Z-ADD     *ZEROS        J
026400940224     C*
026500940608    >C                   EXSR      REDANA
026600030113     C* Carico il SFL
026700940127     C                   EXSR      ROLS1
026800030113     C*
026900030114     C                   Z-ADD     1             WPAG
027000940127     C*
027100940127     C                   ENDSR
027200940127     C************************************************************
027300940131     C* CARICAMENTO PAGINA LISTA
027400940127     C************************************************************
027500940127     C     ROLS1         BEGSR
027600940127     C*
027700940128     C                   SETOFF                                       32
027800940223     C                   Z-ADD     0             Y
027900940127     C                   Z-ADD     WMAX          S1NRR
028000940127     C*
028100940127     C* Leggo dal file anagrafico per caricare la lista
0282009401311    C     $EFILE        DOWEQ     *OFF
028300140529     C*
028400140529     c                   clear                   S1
028500140529     C*
028600140529     C*   Se COMPetenza
028700140529     c                   if        CMD(J) <> *blank
028800140529     C                   eval      s1cod = %editc(CMP(J):'Z')
028900140529     C                   eval      s1des = CMD(J)
029000140529     C                   Z-ADD     IMP(J)        S1IMP
029100140530     c                   setoff                                       45
029200140530      *   Se VOCE
029300140529     c                   elseif    VOD(J) <> *blank
029400140530      *
029500140529     C                   eval      %subst(s1cod:4:3) = %editc(CMP(J):'Z')
029600140530     C*******            eval      %subst(s1des:4:30) = VOD(J)
029700140530     C                   movel     VOD(J)        S1DES
029800140529     C                   Z-ADD     IMV(J)        S1IMP
029900140530     c                   seton                                        45
030000140529     c                   end
030100940127     C*
030200940127     C                   ADD       1             S1NRR
030300940127     C                   ADD       1             Y
030400940607     C                   WRITE     S1
030500940131     C*
030600940316    >C                   EXSR      REDANA
030700940128     C*
0308009401271-   C                   ENDDO
030900940127     C*
031000940223     C                   Z-ADD     S1NRR         WMAX                 30
031100940127     C*
031200940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
031300940127     C*
031400030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
031500940127     C                   MVR                     RESTO             3 0
031600030114     C     PAGINE        MULT      SFLPAG        C1RCD
0317000301141    C     RESTO         IFGT      0
031800030114     C                   ADD       1             C1RCD
0319000301141E   C                   ELSE
032000030114     C                   SUB       SFLPAG        C1RCD
032100030114     C                   ADD       1             C1RCD
0322000301141-   C                   ENDIF
032300940128     C*
032400940127     C                   ENDSR
032500940128     C************************************************************
032600940131     C* LETTURA RCD ARCHIVIO PILOTA
032700940128     C************************************************************
032800940607     C     REDANA        BEGSR
032900940128     C*
033000940131     C                   MOVEL     *OFF          $EFILE
033100940131     C                   MOVEL     *OFF          $RCDOK
033200940131     C*
0333009401311    C     $EFILE        DOUEQ     *ON
033400940131     C     $RCDOK        OREQ      *ON
033500940131     C*
033600140529     C                   ADD       1             J                              *INDICE SCHIERE VIDE
033700140529     C* IMPOSTA SWITCH DI FINE CARICAMENTO SFL (SFLEOF)
033800140529     C     IMP(J)        IFEQ      *ZEROS                                       *SCHIERA VUOTA
033900140530     C     CMP(J)        andEQ     *ZEROS
034000140529     C                   MOVEL     *ON           $EFILE
034100140529     C                   MOVE      $EFILE        *IN33
034200140529     c                   else
034300140529     C                   MOVE      *ON           $RCDOK
034400140529E   1C                   ENDIF
034500940131     C*
0346009401311-   C                   ENDDO
034700940131     C*
034800940131     C                   ENDSR
034900940224     C************************************************************
035000940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
035100940224     C************************************************************
035200940224     C     CLCPAG        BEGSR
035300940224     C* Input :
035400940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
035500940224     C*          cursore
035600940224     C* - SFLPAG = numero rcd per pagina sfl
035700940224     C* Output :
035800940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
035900940224     C*
036000120412     c                   if        WSFL >0
036100940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
036200940224     C                   MVR                     RESTO             3 0
036300940224     C     RESTO         IFGT      0
036400940224     C                   ADD       1             PAGINE
036500940224     C                   ENDIF
036600940224     C     PAGINE        MULT      SFLPAG        WPAG
036700120412     c                   else
036800120412     C                   Z-ADD     1             WPAG
036900120412     c                   end
037000940224     C*
037100940224     C                   ENDSR
037200940309     C************************************************************
037300140529     C* GESTIONE F12 VIDEO S1
037400940309     C************************************************************
037500140529     C     F12S1         BEGSR
037600940309     C*
037700940309     C                   MOVE      *ON           $FINE
037800940325     C* fine programma
037900940309     C                   ENDSR
038000940309     C/EJECT
038100940128     C************************************************************
038200940131     C* CONTROLLO TESTATA LISTA
038300940128     C************************************************************
038400940131     C     CTRC1         BEGSR
038500940128     C*
038600940201     C                   MOVE      *OFF          *IN99
038700940131     C*
038800940202     C                   ENDSR
038900940131     C************************************************************
039000940131     C* CONTROLLO OPZIONI LISTA
039100940131     C************************************************************
039200940131     C     CTRS1         BEGSR
039300940131     C*
039400940202     C                   MOVEL     *OFF          $ESCI
039500940201     C                   SETOFF                                       99
039600940131     C*
039700940127     C* Leggo il sfl solo se ci sono rcd
0398009401311    C     WMAX          IFGT      0
039900940607     C                   READC     S1                                     21
040000940127     C*
040100940131     C* esce se fine sfl o errore che richiede l'uscita
0402009401312    C     *IN21         DOWEQ     *OFF
040300940131     C     $ESCI         ANDEQ     *OFF
040400940201     C                   Z-ADD     S1NRR         C1RCD
040500940131     C* ctrl su riga
040600940131     C                   EXSR      RECS1
040700140529      *
040800940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0409009402013    C     *IN99         IFEQ      *ON
041000940201     C     $ESCI         OREQ      *ON
041100940131     C                   MOVE      *ON           *IN32
041200940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
041300940204     C* la ripristinerò a conclusione del ciclo di READC
041400940223     C                   MOVE      *OFF          $INZS1
0415009402233-   C                   ENDIF
041600940223     C*
041700940607     C                   UPDATE    S1
041800940223     C*
041900940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0420009401313    C     $ESCI         IFEQ      *OFF
042100940607     C                   READC     S1                                     21
042200940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0423009402014    C     *IN21         IFEQ      *ON
042400940201     C                   MOVE      WINZS1        $INZS1
042500940204     C* calcolo pagina a cui deve posizionarsi
042600940224     C                   EXSR      CLCPAG
0427009402014-   C                   ENDIF
0428009402013-   C                   ENDIF
042900940131     C*
0430009401272-   C                   ENDDO
043100940127     C*
0432009401311-   C                   ENDIF
043300940131     C*
043400940127     C                   ENDSR
043500940127     C/EJECT
043600940127     C************************************************************
043700940131     C* CONTROLLO CAMPI I/O RIGA LISTA
043800940127     C************************************************************
043900940131     C     RECS1         BEGSR
044000940131     C*
044100940201     C* reset indicatori DSPATR
044200940201     C                   MOVE      *ALL'0'       IN4049           10
044300940201     C                   MOVEA     IN4049        *IN(40)
044400040930     C*
044500940131     C                   ENDSR
044600140530     C*--------------------------------------------------------------------------------------------*
044700140530     C* REPERISCE I DATI UTENTE
044800140530     C*--------------------------------------------------------------------------------------------*
044900140530     C     REPDATIUTE    BEGSR
045000140530     C*
045100140530     C* INIZIALIZZA VARIABILI DI WRK
045200140530     C                   CLEAR                   TIBS34DS
045300140530     C                   CLEAR                   AZUTEDS
045400140530     C                   CLEAR                   DDATIUTE
045500140530     C*
045600140530     C     *DTAARA       DEFINE    §azute        azuteds
045700140530     C     *DTAARA       DEFINE    §datiute      ddatiute
045800140530     C                   IN(E)     *DTAARA
045900140530if  1C                   IF        %Error
046000140530     c                   EVAL      I34Tla = 'L'
046100140530     C                   CALL      'TIBS34R'
046200140530     C                   PARM                    Tibs34Ds
046300140530     C                   IN        *DTAARA
046400140530e   1C                   ENDIF
046500140530     C*
046600140530     C                   ENDSR
046700140530     C*------------------------------------------------------------------------*
046800940131     C* OPERAZIONI INIZIALI
046900140530     C*------------------------------------------------------------------------*
047000940131     C     *INZSR        BEGSR
047100030113     C*
047200030113     C* Reperimento parametri
047300030113     C     *ENTRY        PLIST
047400030113     C                   PARM                    KPJBA
047500140529     C                   movel     kpjbu         tnsb08ds1
047600140529     C*--------------------
047700140529     C* VARIABILI RIFERITE AL DATA BASE
047800140529     C*--------------------
047900140529     C     *LIKE         DEFINE    CETAAS        KCTAAS                         *ECCET30C
048000140529     C     *LIKE         DEFINE    CETLNP        KCTLNP
048100140529     C     *LIKE         DEFINE    CETNRS        KCTNRS
048200140529     C     *LIKE         DEFINE    CETNSP        KCTNSP
048300140529     C     *LIKE         DEFINE    CETTBL        KCTTBL
048400140529     C     *LIKE         DEFINE    CEDAAS        KCDAAS                         *ECCED30C
048500140529     C     *LIKE         DEFINE    CEDLNP        KCDLNP
048600140529     C     *LIKE         DEFINE    CEDNRS        KCDNRS
048700140529     C     *LIKE         DEFINE    CEDNSP        KCDNSP
048800140529     C     *LIKE         DEFINE    CEDTBL        KCDTBL
048900140529     C     *LIKE         DEFINE    ECTNBC        KETNBC                         *ECECT01L
049000140529     C     *LIKE         DEFINE    ECTCMP        KETCMP
049100140529     C     *LIKE         DEFINE    ECTDDC        KETDDC
049200140529     C     *LIKE         DEFINE    D08AAS        WAAS
049300140529     C     *LIKE         DEFINE    D08LNP        WLNP
049400140529     C     *LIKE         DEFINE    D08NRS        WNRS
049500140529     C     *LIKE         DEFINE    D08NSP        WNSP
049600140529     C     *LIKE         DEFINE    D08TBL        WTBL
049700040930     C*
049800030113     C* Variabili per gestione videate
049900030113     C                   MOVE      *BLANK        $GEST             2
050000030113     C                   MOVE      *BLANK        $FINE             1
050100030113     C                   MOVE      *BLANK        $INZS1            1
050200030113     C                   MOVE      *BLANK        $EFILE            1
050300030113     C                   MOVE      *BLANK        $ESCI             1
050400030113     C                   MOVE      *BLANK        $RCDOK            1
050500030113     C*
050600140529     C* LETTURA ECCET30C - COMPLETA
050700140529     C     KEYCET        KLIST
050800140529     C                   KFLD                    KCTAAS                         *ANNO
050900140529     C                   KFLD                    KCTLNP                         *LINEA DI PARTENZA
051000140529     C                   KFLD                    KCTNRS                         *SERIE
051100140529     C                   KFLD                    KCTNSP                         *SPEDIZIONE
051200140529     C                   KFLD                    KCTTBL                         *TIPO BOLLA
051300140529     C* LETTURA ECCED30C - COMPLETA
051400140529     C     KEYCED        KLIST
051500140529     C                   KFLD                    KCDAAS                         *ANNO
051600140529     C                   KFLD                    KCDLNP                         *LINEA DI PARTENZA
051700140529     C                   KFLD                    KCDNRS                         *SERIE
051800140529     C                   KFLD                    KCDNSP                         *SPEDIZIONE
051900140529     C                   KFLD                    KCDTBL                         *TIPO BOLLA
052000140529     C* LETTURA ECECT01L - COMPLETA
052100140529     C     KEYECT        KLIST
052200140529     C                   KFLD                    KETNBC                         *NUMERO BLOCCO
052300140529     C                   KFLD                    KETCMP                         *CODICE COMPETENZA
052400140529     C                   KFLD                    KETDDC                         *DATA DECORRENZA
052500140529     C* LETTURA ECECT01L - PARZIALE
052600140529     C     KE2ECT        KLIST
052700140529     C                   KFLD                    KETNBC                         *NUMERO BLOCCO
052800140529     C                   KFLD                    KETCMP                         *CODICE COMPETENZA
052900030113     C* Indici
053000030113     C                   Z-ADD     0             X                 3 0
053100030113     C                   Z-ADD     0             Y                 3 0
053200940127     C*
053300140530     C* LANCIO ROUTINE PER VERIFICARE SE
053400140530     C* SONO IN SEDE, IN FILIALE DI 1° LIVELLO O IN FILIALE DI 2° LIVELLO
053500140530     C* E PER RECUPERARE LA RAGIONE SOCIALE DELL'UTENTE
053600140530     C                   EXSR      REPDATIUTE
053700140530     C*
053800940117     C                   ENDSR
053900140529     C*------------------------------------------------------------------------*
054000140530     C* CARVOC - CARICA TUTTE LE VOCI       DELLA BOLLA IN SCHIERA
054100140529     C*------------------------------------------------------------------------*
054200140530     C     CARVOC        BEGSR
054300140530     C*
054400140530     C* LEGGE LE VOCI
054500140530     C                   clear                   VoC
054600140530     C                   clear                   IvC
054700140530      *---
054800140530     C                   Z-ADD     VCCAAS        KCTAAS                         *ANNO
054900140530     C                   Z-ADD     VCCLNP        KCTLNP                         *LINEA DI PARTENZA
055000140530     C                   Z-ADD     VCCNRS        KCTNRS                         *SERIE
055100140530     C                   Z-ADD     VCCNSP        KCTNSP                         *SPEDIZIONE
055200140530     C                   MOVEL     VCCTBL        KCTTBL                         *TIPO BOLLA
055300140530      *---
055400140530     C     KEYCET        CHAIN     ECCEV30C
055500140530IF  1C                   IF        %FOUND(ECCEV30C)                             *TROVATE
055600140530     c                   eval      VOC(01) = cevc01
055700140530     c                   eval      VOC(02) = cevc02
055800140530     c                   eval      VOC(03) = cevc03
055900140530     c                   eval      VOC(04) = cevc04
056000140530     c                   eval      VOC(05) = cevc05
056100140530     c                   eval      VOC(06) = cevc06
056200140530     c                   eval      VOC(07) = cevc07
056300140530     c                   eval      VOC(08) = cevc08
056400140530     c                   eval      VOC(09) = cevc09
056500140530     c                   eval      VOC(10) = cevc10
056600140530     c                   eval      VOC(11) = cevc11
056700140530     c                   eval      VOC(12) = cevc12
056800140530     c                   eval      VOC(13) = cevc13
056900140530     c                   eval      VOC(14) = cevc14
057000140530     c                   eval      VOC(15) = cevc15
057100140530     c                   eval      VOC(16) = cevc16
057200140530     c                   eval      VOC(17) = cevc17
057300140530     c                   eval      VOC(18) = cevc18
057400140530     c                   eval      VOC(19) = cevc19
057500140530     c                   eval      VOC(20) = cevc20
057600140530     c                   eval      VOC(21) = cevc21
057700140530     c                   eval      VOC(22) = cevc22
057800140530     c                   eval      VOC(23) = cevc23
057900140530     c                   eval      VOC(24) = cevc24
058000140530     c                   eval      VOC(25) = cevc25
058100140530     c                   eval      VOC(26) = cevc26
058200140530     c                   eval      VOC(27) = cevc27
058300140530     c                   eval      VOC(28) = cevc28
058400140530     c                   eval      VOC(29) = cevc29
058500140530     c                   eval      VOC(30) = cevc30
058600140530     c                   eval      VOC(31) = cevc31
058700140530     c                   eval      VOC(32) = cevc32
058800140530     c                   eval      VOC(33) = cevc33
058900140530     c                   eval      VOC(34) = cevc34
059000140530     c                   eval      VOC(35) = cevc35
059100140530     c                   eval      VOC(36) = cevc36
059200140530     c                   eval      VOC(37) = cevc37
059300140530     c                   eval      VOC(38) = cevc38
059400140530     c                   eval      VOC(39) = cevc39
059500140530     c                   eval      VOC(40) = cevc40
059600140530     c                   eval      VOC(41) = cevc41
059700140530     c                   eval      VOC(42) = cevc42
059800140530     c                   eval      VOC(43) = cevc43
059900140530     c                   eval      VOC(44) = cevc44
060000140530     c                   eval      VOC(45) = cevc45
060100140530     c                   eval      VOC(46) = cevc46
060200140530     c                   eval      VOC(47) = cevc47
060300140530     c                   eval      VOC(48) = cevc48
060400140530     c                   eval      VOC(49) = cevc49
060500140530     c                   eval      VOC(50) = cevc50
060600140530      *--
060700140530     c                   eval      IVC(01) = cevi01
060800140530     c                   eval      IVC(02) = cevi02
060900140530     c                   eval      IVC(03) = cevi03
061000140530     c                   eval      IVC(04) = cevi04
061100140530     c                   eval      IVC(05) = cevi05
061200140530     c                   eval      IVC(06) = cevi06
061300140530     c                   eval      IVC(07) = cevi07
061400140530     c                   eval      IVC(08) = cevi08
061500140530     c                   eval      IVC(09) = cevi09
061600140530     c                   eval      IVC(10) = cevi10
061700140530     c                   eval      IVC(11) = cevi11
061800140530     c                   eval      IVC(12) = cevi12
061900140530     c                   eval      IVC(13) = cevi13
062000140530     c                   eval      IVC(14) = cevi14
062100140530     c                   eval      IVC(15) = cevi15
062200140530     c                   eval      IVC(16) = cevi16
062300140530     c                   eval      IVC(17) = cevi17
062400140530     c                   eval      IVC(18) = cevi18
062500140530     c                   eval      IVC(19) = cevi19
062600140530     c                   eval      IVC(20) = cevi20
062700140530     c                   eval      IVC(21) = cevi21
062800140530     c                   eval      IVC(22) = cevi22
062900140530     c                   eval      IVC(23) = cevi23
063000140530     c                   eval      IVC(24) = cevi24
063100140530     c                   eval      IVC(25) = cevi25
063200140530     c                   eval      IVC(26) = cevi26
063300140530     c                   eval      IVC(27) = cevi27
063400140530     c                   eval      IVC(28) = cevi28
063500140530     c                   eval      IVC(29) = cevi29
063600140530     c                   eval      IVC(30) = cevi30
063700140530     c                   eval      IVC(31) = cevi31
063800140530     c                   eval      IVC(32) = cevi32
063900140530     c                   eval      IVC(33) = cevi33
064000140530     c                   eval      IVC(34) = cevi34
064100140530     c                   eval      IVC(35) = cevi35
064200140530     c                   eval      IVC(36) = cevi36
064300140530     c                   eval      IVC(37) = cevi37
064400140530     c                   eval      IVC(38) = cevi38
064500140530     c                   eval      IVC(39) = cevi39
064600140530     c                   eval      IVC(40) = cevi40
064700140530     c                   eval      IVC(41) = cevi41
064800140530     c                   eval      IVC(42) = cevi42
064900140530     c                   eval      IVC(43) = cevi43
065000140530     c                   eval      IVC(44) = cevi44
065100140530     c                   eval      IVC(45) = cevi45
065200140530     c                   eval      IVC(46) = cevi46
065300140530     c                   eval      IVC(47) = cevi47
065400140530     c                   eval      IVC(48) = cevi48
065500140530     c                   eval      IVC(49) = cevi49
065600140530     c                   eval      IVC(50) = cevi50
065700140530     c                   END
065800140530     C*
065900140530     C     KEYCET        CHAIN     ECC2V30C
066000140530IF  1C                   IF        %FOUND(ECC2V30C)                             *TROVATE
066100140530     c                   eval      VOC(51) = c2vc51
066200140530     c                   eval      VOC(52) = c2vc52
066300140530     c                   eval      VOC(53) = c2vc53
066400140530     c                   eval      VOC(54) = c2vc54
066500140530     c                   eval      VOC(55) = c2vc55
066600140530     c                   eval      VOC(56) = c2vc56
066700140530     c                   eval      VOC(57) = c2vc57
066800140530     c                   eval      VOC(58) = c2vc58
066900140530     c                   eval      VOC(59) = c2vc59
067000140530     c                   eval      VOC(60) = c2vc60
067100140530     c                   eval      VOC(61) = c2vc61
067200140530     c                   eval      VOC(62) = c2vc62
067300140530     c                   eval      VOC(63) = c2vc63
067400140530     c                   eval      VOC(64) = c2vc64
067500140530     c                   eval      VOC(65) = c2vc65
067600140530     c                   eval      VOC(66) = c2vc66
067700140530     c                   eval      VOC(67) = c2vc67
067800140530     c                   eval      VOC(68) = c2vc68
067900140530     c                   eval      VOC(69) = c2vc69
068000140530     c                   eval      VOC(70) = c2vc70
068100140530     c                   eval      VOC(71) = c2vc71
068200140530     c                   eval      VOC(72) = c2vc72
068300140530     c                   eval      VOC(73) = c2vc73
068400140530     c                   eval      VOC(74) = c2vc74
068500140530     c                   eval      VOC(75) = c2vc75
068600140530      *--
068700140530     c                   eval      IVC(51) = c2vi51
068800140530     c                   eval      IVC(52) = c2vi52
068900140530     c                   eval      IVC(53) = c2vi53
069000140530     c                   eval      IVC(54) = c2vi54
069100140530     c                   eval      IVC(55) = c2vi55
069200140530     c                   eval      IVC(56) = c2vi56
069300140530     c                   eval      IVC(57) = c2vi57
069400140530     c                   eval      IVC(58) = c2vi58
069500140530     c                   eval      IVC(59) = c2vi59
069600140530     c                   eval      IVC(60) = c2vi60
069700140530     c                   eval      IVC(61) = c2vi61
069800140530     c                   eval      IVC(62) = c2vi62
069900140530     c                   eval      IVC(63) = c2vi63
070000140530     c                   eval      IVC(64) = c2vi64
070100140530     c                   eval      IVC(65) = c2vi65
070200140530     c                   eval      IVC(66) = c2vi66
070300140530     c                   eval      IVC(67) = c2vi67
070400140530     c                   eval      IVC(68) = c2vi68
070500140530     c                   eval      IVC(69) = c2vi69
070600140530     c                   eval      IVC(70) = c2vi70
070700140530     c                   eval      IVC(71) = c2vi71
070800140530     c                   eval      IVC(72) = c2vi72
070900140530     c                   eval      IVC(73) = c2vi73
071000140530     c                   eval      IVC(74) = c2vi74
071100140530     c                   eval      IVC(75) = c2vi75
071200140530     c                   END
071300140530     C*
071400140530     C                   ENDSR
071500140530     C*------------------------------------------------------------------------*
071600140530     C* CARCMP - CARICA TUTTE LE COMPETENZE DELLA BOLLA IN SCHIERA
071700140530     C*------------------------------------------------------------------------*
071800140530     C     CARCMP        BEGSR
071900140530     C*
072000140529     C     *LIKE         DEFINE    CEDIMP        TOTIMP
072100140529     C                   Z-ADD     *ZEROS        J                 3 0
072200140529     C*
072300140529     C* LEGGE LE COMPETENZE FISSE
072400140529     C                   Z-ADD     VCCAAS        KCTAAS                         *ANNO
072500140529     C                   Z-ADD     VCCLNP        KCTLNP                         *LINEA DI PARTENZA
072600140529     C                   Z-ADD     VCCNRS        KCTNRS                         *SERIE
072700140529     C                   Z-ADD     VCCNSP        KCTNSP                         *SPEDIZIONE
072800140529     C                   MOVEL     VCCTBL        KCTTBL                         *TIPO BOLLA
072900140529     C     KEYCET        CHAIN     ECCET30C                           99
073000140529IF  1C     *IN99         IFEQ      *OFF                                         *TROVATE
073100140529     C*
073200140529     C* CARICA LE COMPETENZE FISSE
073300140529IF  2C     CETPAR        IFGT      *ZEROS
073400140529     C                   ADD       1             J
073500140529     C                   Z-ADD     1             CMP(J)                         *CODICE  COMPETENZA
073600140529     C                   Z-ADD     CETPAR        IMP(J)                         *IMPORTO COMPETENZA
073700140529     c                   exsr      VOCI
073800140529E   2C                   ENDIF
073900140529     C*
074000140529IF  2C     CETPAM        IFGT      *ZEROS
074100140529     C                   ADD       1             J
074200140529     C                   Z-ADD     2             CMP(J)                         *CODICE  COMPETENZA
074300140529     C                   Z-ADD     CETPAM        IMP(J)                         *IMPORTO COMPETENZA
074400140529     c                   exsr      VOCI
074500140529E   2C                   ENDIF
074600140529     C*
074700140529IF  2C     CETPAU        IFGT      *ZEROS
074800140529     C                   ADD       1             J
074900140529     C                   Z-ADD     3             CMP(J)                         *CODICE  COMPETENZA
075000140529     C                   Z-ADD     CETPAU        IMP(J)                         *IMPORTO COMPETENZA
075100140529     c                   exsr      VOCI
075200140529E   2C                   ENDIF
075300140529     C*
075400140529IF  2C     CETPAV        IFGT      *ZEROS
075500140529     C                   ADD       1             J
075600140529     C                   Z-ADD     4             CMP(J)                         *CODICE  COMPETENZA
075700140529     C                   Z-ADD     CETPAV        IMP(J)                         *IMPORTO COMPETENZA
075800140529     c                   exsr      VOCI
075900140529E   2C                   ENDIF
076000140529     C*
076100140529IF  2C     CETARD        IFGT      *ZEROS
076200140529     C                   ADD       1             J
076300140529     C                   Z-ADD     5             CMP(J)                         *CODICE  COMPETENZA
076400140529     C                   Z-ADD     CETARD        IMP(J)                         *IMPORTO COMPETENZA
076500140529     c                   exsr      VOCI
076600140529E   2C                   ENDIF
076700140529     C*
076800140529IF  2C     CETARM        IFGT      *ZEROS
076900140529     C                   ADD       1             J
077000140529     C                   Z-ADD     6             CMP(J)                         *CODICE  COMPETENZA
077100140529     C                   Z-ADD     CETARM        IMP(J)                         *IMPORTO COMPETENZA
077200140529     c                   exsr      VOCI
077300140529E   2C                   ENDIF
077400140529     C*
077500140529IF  2C     CETARU        IFGT      *ZEROS
077600140529     C                   ADD       1             J
077700140529     C                   Z-ADD     7             CMP(J)                         *CODICE  COMPETENZA
077800140529     C                   Z-ADD     CETARU        IMP(J)                         *IMPORTO COMPETENZA
077900140529     c                   exsr      VOCI
078000140529E   2C                   ENDIF
078100140529     C*
078200140529IF  2C     CETARV        IFGT      *ZEROS
078300140529     C                   ADD       1             J
078400140529     C                   Z-ADD     8             CMP(J)                         *CODICE  COMPETENZA
078500140529     C                   Z-ADD     CETARV        IMP(J)                         *IMPORTO COMPETENZA
078600140529     c                   exsr      VOCI
078700140529E   2C                   ENDIF
078800140529     C*
078900140529IF  2C     CETTRL        IFGT      *ZEROS
079000140529     C                   ADD       1             J
079100140529     C                   Z-ADD     13            CMP(J)                         *CODICE  COMPETENZA
079200140529     C                   Z-ADD     CETTRL        IMP(J)                         *IMPORTO COMPETENZA
079300140529     c                   exsr      VOCI
079400140529E   2C                   ENDIF
079500140529     C*
079600140529IF  2C     CETPAG        IFGT      *ZEROS
079700140529     C                   ADD       1             J
079800140529     C                   Z-ADD     29            CMP(J)                         *CODICE  COMPETENZA
079900140529     C                   Z-ADD     CETPAG        IMP(J)                         *IMPORTO COMPETENZA
080000140529     c                   exsr      VOCI
080100140529E   2C                   ENDIF
080200140529     C*
080300140529IF  2C     CETSEA        IFGT      *ZEROS
080400140529     C                   ADD       1             J
080500140529     C                   Z-ADD     26            CMP(J)                         *CODICE  COMPETENZA
080600140529     C                   Z-ADD     CETSEA        IMP(J)                         *IMPORTO COMPETENZA
080700140529     c                   exsr      VOCI
080800140529E   2C                   ENDIF
080900140529     C*
081000140529IF  2C     CETSEC        IFGT      *ZEROS
081100140529     C                   ADD       1             J
081200140529     C                   Z-ADD     25            CMP(J)                         *CODICE  COMPETENZA
081300140529     C                   Z-ADD     CETSEC        IMP(J)                         *IMPORTO COMPETENZA
081400140529     c                   exsr      VOCI
081500140529E   2C                   ENDIF
081600140529     C*
081700140529IF  2C     CETRCV        IFGT      *ZEROS
081800140529     C                   ADD       1             J
081900140529     C                   Z-ADD     27            CMP(J)                         *CODICE  COMPETENZA
082000140529     C                   Z-ADD     CETRCV        IMP(J)                         *IMPORTO COMPETENZA
082100140529     c                   exsr      VOCI
082200140529E   2C                   ENDIF
082300140529     C*
082400140529IF  2C     CETMDE        IFGT      *ZEROS
082500140529     C                   ADD       1             J
082600140529     C                   Z-ADD     10            CMP(J)                         *CODICE  COMPETENZA
082700140529     C                   Z-ADD     CETMDE        IMP(J)                         *IMPORTO COMPETENZA
082800140529     c                   exsr      VOCI
082900140529E   2C                   ENDIF
083000140529     C*
083100140529IF  2C     CETTDE        IFGT      *ZEROS
083200140529     C                   ADD       1             J
083300140529     C                   Z-ADD     15            CMP(J)                         *CODICE  COMPETENZA
083400140529     C                   Z-ADD     CETTDE        IMP(J)                         *IMPORTO COMPETENZA
083500140529     c                   exsr      VOCI
083600140529E   2C                   ENDIF
083700140529     C*
083800140529IF  2C     CETMAF        IFGT      *ZEROS
083900140529     C                   ADD       1             J
084000140529     C                   Z-ADD     9             CMP(J)                         *CODICE  COMPETENZA
084100140529     C                   Z-ADD     CETMAF        IMP(J)                         *IMPORTO COMPETENZA
084200140529     c                   exsr      VOCI
084300140529E   2C                   ENDIF
084400140529     C*
084500140529IF  2C     CETTAF        IFGT      *ZEROS
084600140529     C                   ADD       1             J
084700140529     C                   Z-ADD     14            CMP(J)                         *CODICE  COMPETENZA
084800140529     C                   Z-ADD     CETTAF        IMP(J)                         *IMPORTO COMPETENZA
084900140529     c                   exsr      VOCI
085000140529E   2C                   ENDIF
085100140529     C*
085200140529IF  2C     CETSES        IFGT      *ZEROS
085300140529     C                   ADD       1             J
085400140529     C                   Z-ADD     33            CMP(J)                         *CODICE  COMPETENZA
085500140529     C                   Z-ADD     CETSES        IMP(J)                         *IMPORTO COMPETENZA
085600140529     c                   exsr      VOCI
085700140529E   2C                   ENDIF
085800140529     C*
085900140529     C* LEGGE LE COMPETENZE VARIABILI
086000140529IF  2C     CETDET        IFEQ      'S'                                          *SE CI SONO
086100140529     C                   Z-ADD     VCCAAS        KCDAAS                         *ANNO
086200140529     C                   Z-ADD     VCCLNP        KCDLNP                         *LINEA DI PARTENZA
086300140529     C                   Z-ADD     VCCNRS        KCDNRS                         *SERIE
086400140529     C                   Z-ADD     VCCNSP        KCDNSP                         *SPEDIZIONE
086500140529     C                   MOVEL     VCCTBL        KCDTBL                         *TIPO BOLLA
086600140530     C*
086700140529     C     KEYCED        SETLL     ECCED30C
086800140529     C     KEYCED        READE     ECCED30C                               99
086900140529DO  3C     *IN99         DOWEQ     *OFF
087000140529     C*
087100140529     C* CARICA LE COMPETENZE VARIABILI
087200140529IF  4C     CEDIMP        IFGT      *ZEROS
087300140529     C                   ADD       1             J
087400140529     C                   Z-ADD     CEDCMP        CMP(J)                         *CODICE  COMPETENZA
087500140529     C                   Z-ADD     CEDIMP        IMP(J)                         *IMPORTO COMPETENZA
087600140529     c                   exsr      VOCI
087700140529E   4C                   ENDIF
087800140529     C     KEYCED        READE     ECCED30C                               99
087900140529E   3C                   ENDDO
088000140529E   2C                   ENDIF
088100140529     C*
088200140529     C* CALCOLA IL TOTALE COMPETENZE
088300140529     C                   XFOOT     IMP           TOTIMP
088400140529     C                   ADD       1             J
088500140529     C                   Z-ADD     *ZEROS        CMP(J)                         *CODICE  COMPETENZA
088600140529     C                   Z-ADD     TOTIMP        IMP(J)                         *IMPORTO COMPETENZA
088700140529     C                   MOVEL     TODIMP        CMD(J)                         *DESCRIZ.COMPETENZA
088800140529E   1C                   ENDIF
088900140529     C*
089000140529     C                   ENDSR
089100140529     C*------------------------------------------------------------------------*
089200140529     C*  VOCI  - DECODIFICA LE SINGOLE VOCI UTILIZZATE
089300140529     C*------------------------------------------------------------------------*
089400140529     C     VOCI          BEGSR
089500140529     C*
089600140529     C*  la routine è chiamata mentre sta trattando una COMPETENZA
089700140529     C*    QUINDI:
089800140529     C*  J -> è caricato sulla riga di competenza e i campi voce devono essere vuoti
089900140529     C*
090000140529     C* Prima di passare alle singole Voci DECODIFICA la COMPETENZA appena impostata
090100140529     C*   e pulisce la parte voce
090200140529     C*
090300140529     C* DECODIFICA LE COMPETENZE (NON L'ULTIMO ELEMENTO CHE E' IL TOTALE)
090400140529IF  2C     IMP(J)        IFGT      *ZEROS
090500140529     C     CMP(J)        ANDGT     *ZEROS
090600140529     C                   Z-ADD     CETNBC        WNBC
090700140529     C                   Z-ADD     CMP(J)        WCMP
090800140529     C                   MOVEL     *BLANKS       WCMD
090900140529     C                   EXSR      CHKCMP
091000140529     C                   MOVEL     WCMD          CMD(J)
091100140529E   2C                   ENDIF
091200140529     C*
091300140529     c                   clear                   VOD(J)
091400140529     c                   clear                   IMV(J)
091500140529     C*
091600140529     C*  per ogni competenza aggiunge tante righe quante sono
091700140529     C*  le singole voci che la compongono
091800140530     c                   z-add     CMP(J)        EVTCMP
091900140530     c     EVTCMP        setll     ecEVT02l
092000140530     c     EVTCMP        reade     ecEVT02l
092100140529     C*
092200140530     c                   dow       not %EoF(ecEVT02l)
092300140530     C*
092400140530     C*  se la VOCE è quella attiva nel periodo
092500140530     c                   if        D08DSP >= EVTDDV and
092600140530     c                             D08DSP <= EVTDSV
092700140530     c                   z-add     1             vc                3 0
092800140530     c     evtVOC        lookup    VOC(vc)                                75
092900140530     c                   if        %equal
093000140530     c                   add       1             J
093100140530     c                   z-add     VOC(vc)       CMP(J)
093200140530     c                   clear                   IMP(J)
093300140530     c                   clear                   CMD(J)
093400140530     c                   z-add     IVC(vc)       IMV(J)
093500140530     c                   movel     evtVOD        VOD(J)
093600140530     c                   end
093700140530     c                   end
093800140530     C*
093900140530     c     EVTCMP        reade     ecEVT02l
094000140530     c                   endDO
094100140529     C*
094200140529     C                   ENDSR
094300140529     C*------------------------------------------------------------------------*
094400140529     C* CHKCMP - DECODIFICA LE COMPETENZE
094500140529     C*------------------------------------------------------------------------*
094600140529     C     CHKCMP        BEGSR
094700140529     C*
094800140529     C     *LIKE         DEFINE    ECTNBC        WNBC
094900140529     C     *LIKE         DEFINE    ECTCMP        WCMP
095000140529     C     *LIKE         DEFINE    ECTCMD        WCMD
095100140529     C*
095200140529     C* LEGGE, DEL BLOCCO, L'ULTIMA COMPETENZA IN DECORRENZA
095300140529     C                   Z-ADD     WNBC          KETNBC                         *NUMERO BLOCCO
095400140529     C                   Z-ADD     WCMP          KETCMP                         *CODICE COMPETENZA
095500140529     C                   Z-ADD     99999999      KETDDC                         *DATA DECORRENZA
095600140529     C     KEYECT        SETLL     ECECT01L
095700140529     C     KE2ECT        READPE    ECECT01L                               99
095800140529IF  1C     *IN99         IFEQ      *OFF
095900140529     C                   MOVEL     ECTCMD        WCMD
096000140529E   1C                   ENDIF
096100140529     C*
096200140529     C                   ENDSR
096300140529     C*------------------------------------------------------------------------*
