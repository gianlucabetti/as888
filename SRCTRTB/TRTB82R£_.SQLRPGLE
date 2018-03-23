000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200940224      *
000300040930      *  11           x selezione di un codice da ripassare al pgm chiamante
000400940307      *  21           GENERICO OPERAZIONI I/O
000500940224      *  22           GENERICO ERRORE OPERAZIONI I/O
000600940224      *  30           SFLDSP
000700940224      * N31           SFLCLR
000800940224      *  31           SFLDSPCTL
000900940224      *  32           SFLNXTCHG
001000940224      *  33           SFLEND
001100940224      *  39           OF PRTF
001200940224      *  40 <---> 49  DSPATR ERRORI SU SFL
001300940608      *  Specificare l'uso dei singoli indicatori
001400940224      *  50 <---> 98  ERRORI SU VIDEO
001500940608      *  Specificare l'uso dei singoli indicatori
001600940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
001700940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
001800940224      *  99           INDIC. GENERALE DI ERRORE
001900940128     F*----------------------------------------------------*
002000030113     FTABEL00F  IF   E           K DISK
002100170123     FTRTB82D   CF   E             WORKSTN
002200940607     F                                     SFILE(S1:S1NRR)
002300940201     F                                     INFDS(DSFMT)
002400940128     D*----------------------------------------------------*
002500940211     D* Passaggio Parametri
002600940211     D KPJBA         E DS
002700030113      *-------------
002800940325     D* Parametri in ricezione
002900030113     D  TABDS          DS
003000170123     D  XTAfree                1      1
003100170123     D  XTAOPZ                 2      2
003200030113     D  XTARET                 3      3
003300030113     D  XTAOPR                 4      4
003400030113     D  XTAERR                 5      5
003500030113     D  XTAKEY                 6     13
003600940211     D*-------------
003700940211     D DSFMT           DS           512
003800940506     D  $TASTO               369    369
003900940211     D  NRG                  370    370
004000940211     D  NCL                  371    371
004100940211     D  SFLNRR               378    379B 0
004200940207     D*-------------
004300940207     D* Nome PGM a video
004400940207     D                 DS
004500940207     D  PROGR                  1     10
004600940207     D  ASTER1                 1      1    INZ('*')
004700940207     D  SIGLA                  2      9
004800940207     D  ASTER2                10     10    INZ('*')
004900940127     D*-------------
005000940127     D* Reperimento nome PGM
005100940127     D STATUS         SDS           333
005200940127     D  DSPGM            *PROC
005300030113     D*-------------
005400170124     D WrkSqlCmd       S           2048
005500170124     D CodPart         S              7
005600170124     D Chi             S              1
005700170124     D DesChi          S             35
005800170124     D*---
005900030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
006000030113$003 D S1NRR           S                   Like(C1rcd)
006100030113$003 D WSfl            S                   Like(C1nrr)
006200030113$003 D Wmax            S                   Like(C1rcd)
006300030113$003 D Wpag            S                   Like(C1rcd)
006400030113$003 D Winzs1          S                   Like($inzs1)
006500170124$003 D x1cod           S                   Like(s1chi)
006600940207     D*-------------
006700940211     D* COSTANTI
006800940211     D*-------------
006900030113     D SFLPAG          C                   CONST(11)
007000940314     D* dimensione della schiera $MS1
007100940506     D*
007200940506     D* Tasti di funzione
007300940506     D F01             C                   CONST(X'31')
007400940506     D F02             C                   CONST(X'32')
007500940506     D F03             C                   CONST(X'33')
007600940506     D F04             C                   CONST(X'34')
007700940506     D F05             C                   CONST(X'35')
007800940506     D F06             C                   CONST(X'36')
007900940506     D F07             C                   CONST(X'37')
008000940506     D F08             C                   CONST(X'38')
008100940506     D F09             C                   CONST(X'39')
008200940506     D F10             C                   CONST(X'3A')
008300940506     D F11             C                   CONST(X'3B')
008400940506     D F12             C                   CONST(X'3C')
008500940506     D F13             C                   CONST(X'B1')
008600940506     D F14             C                   CONST(X'B2')
008700940506     D F15             C                   CONST(X'B3')
008800940506     D F16             C                   CONST(X'B4')
008900940506     D F17             C                   CONST(X'B5')
009000940506     D F18             C                   CONST(X'B6')
009100940506     D F19             C                   CONST(X'B7')
009200940506     D F20             C                   CONST(X'B8')
009300940506     D F21             C                   CONST(X'B9')
009400940506     D F22             C                   CONST(X'BA')
009500940506     D F23             C                   CONST(X'BB')
009600940506     D F24             C                   CONST(X'BC')
009700940506     D ENTER           C                   CONST(X'F1')
009800940506     D ROLDWN          C                   CONST(X'F4')
009900940506     D ROLLUP          C                   CONST(X'F5')
010000940127     C*----------------------------------------------------*
010100940127     C*                MAIN LINE PROGRAM
010200940127     C*----------------------------------------------------*
010300940223     C* inizializzazione variabili
010400940223     C                   EXSR      INZVAR
010500940223     C*
010600940223     C     $FINE         DOWEQ     *OFF
010700940131     C     $GEST         CASEQ     'S1'          GESS1
010800940117     C                   END
010900940117     C                   END
011000170124     C/EXEC SQL
011100170124     C+ CLOSE A1
011200170124     C/END-EXEC
011300940325     C* fine programma
011400940325     C                   SETON                                            LR
011500030113     C************************************************************
011600030113     C* INIZIALIZZAZIONE VARIABILI
011700030113     C************************************************************
011800030113     C     INZVAR        BEGSR
011900030113     C*
012000030113     C* Pulizia campi e indicatori
012100030113     C                   MOVE      *ALL'0'       IN4049           10
012200030113     C                   MOVEA     IN4049        *IN(40)
012300030113     C                   CLEAR                   S1OPZ
012400030113     C* Variabili per gestione videate
012500030113     C*
012600030113     C                   MOVE      *OFF          $FINE
012700030113     C                   MOVE      *OFF          $INZS1
012800030113     C                   MOVE      *OFF          $EFILE
012900030113     C                   MOVE      *OFF          $ESCI
013000030113     C                   MOVE      *OFF          $RCDOK
013100030113     C                   Z-ADD     0             $ULKS1            3 0
013200030113     C*
013300030113     C                   MOVE      *ON           $INZS1
013400030113     C                   MOVE      'S1'          $GEST
013500030113     C*
013600030113     C* Variabili appoggio
013700030114     C                   Z-ADD     1             WPAG
013800030113     c*
013900170124     c                   setoff                                       22
014000170124     c*
014100030113     C                   ENDSR
014200940127     C************************************************************
014300940131     C* GESTIONE LISTA
014400940127     C************************************************************
014500940127     C     GESS1         BEGSR
014600030113     C*
014700940223     C* inizializzazione videata
014800940223     C     $INZS1        IFEQ      *ON
014900940127     C                   EXSR      INZS1
015000940223     C                   MOVE      *OFF          $INZS1
015100940127     C                   ENDIF
015200030113     C*
015300030113     C* emissione piede videata
015400030113     C                   WRITE     Z1
015500030113     C* Non ci sono records
015600940223     C     WMAX          IFEQ      0
015700940607     C                   WRITE     D1
015800030114     C                   Else
015900030114     C     Wsfl          IFgt      0
016000030114     C                   Z-ADD     wsfl          C1RCD
016100030114     C                   Else
016200030114     C     Wpag          IFgt      0
016300030114     C                   Z-ADD     wpag          C1RCD
016400030114     C                   EndIF
016500030114     C                   EndIF
016600030114     C                   ENDIF
016700940127     C*
016800030113     C*              *------------------*
016900940607     C                   EXFMT     C1
017000030113     C*              *------------------*
017100030113     C*
017200940204     C     C1NRR         IFNE      0
017300940204     C                   Z-ADD     C1NRR         WSFL
017400940204     C                   ENDIF
017500940127     C                   Z-ADD     SFLNRR        C1RCD
017600030113     C* Selezioni
0177009401271    C                   SELECT
017800940127     C* F3=Fine
017900940506     C     $TASTO        WHENEQ    F03
018000940309     C                   EXSR      F03S1
018100940131     C* F10=Immissione
018200940506     C     $TASTO        WHENEQ    F10
018300940309     C                   EXSR      F10S1
0184009401271O   C                   OTHER
018500940127     C* CONTROLLO DATI
018600940131     C                   EXSR      CTRC1
018700940201     C     *IN99         IFEQ      *OFF
018800940131     C                   EXSR      CTRS1
018900940131     C                   END
0190009401271-   C                   ENDSL
019100940127     C*
019200940127     C                   ENDSR
019300940224     C/EJECT
019400940127     C************************************************************
019500940131     C* INIZIALIZZAZIONE LISTA
019600940127     C************************************************************
019700940127     C     INZS1         BEGSR
019800940302     C* pulizia SFL
019900940128     C                   SETOFF                                         3031
020000940607     C                   WRITE     C1
020100940128     C                   SETON                                          31
020200940128     C*
020300030113     C* CARICAMENTO SFL totale
020400940201     C                   Z-ADD     0             S1NRR
020500030113     C                   Z-ADD     1             C1RCD
020600940128     C                   Z-ADD     0             WMAX
020700940224     C*
020800170124     C*  compone la lettura SQL
020900170124     c                   exsr      istruz_sql
021000170124      *
021100170124     C/EXEC SQL
021200170124     C+ PREPARE S1 FROM :WrkSqlCmd
021300170124     C/END-EXEC
021400170124
021500170124     C/EXEC SQL
021600170124     C+ DECLARE A1 CURSOR FOR S1
021700170124     C/END-EXEC
021800170124
021900170124     C/EXEC SQL
022000170124     C+ OPEN A1
022100170124     C/END-EXEC
022200170124
022300940224     C* Posizionamento su file pilota
022400940608    >C                   EXSR      REDANA
022500030113     C* Carico il SFL
022600940127     C                   EXSR      ROLS1
022700030113     C*
022800030114     c                   if        xtaopr <> '1'
022900030114     C                   Z-ADD     1             WPAG
023000030114     c                   end
023100940127     C*
023200940127     C                   ENDSR
023300940127     C************************************************************
023400940131     C* CARICAMENTO PAGINA LISTA
023500940127     C************************************************************
023600940127     C     ROLS1         BEGSR
023700940127     C*
023800940128     C                   SETOFF                                       32
023900940223     C                   Z-ADD     0             Y
024000940127     C                   Z-ADD     WMAX          S1NRR
024100940127     C*
024200940127     C* Leggo dal file anagrafico per caricare la lista
0243009401311    C     $EFILE        DOWEQ     *OFF
024400940127     C*
024500170124     c                   clear                   s1
024600170124     c                   movel     chi           s1chi
024700170124     c                   movel     deschi        s1des
024800170124     c                   if        %subst(codPart:1:4) ='CONS'
024900170124     c                   move      codPart       s1con
025000170124     c                   elseif    %subst(codPart:1:4) ='GIAC'
025100170124     c                   move      codPart       s1gia
025200170124     c                   end
025300170124     c
025400030115     c                   clear                   S1Ann
025500170124     c******             if        tblflg ='*'
025600170124     c******             move      'A'           S1Ann
025700170124     c******             end
025800940127     C*
025900940127     C                   ADD       1             S1NRR
026000940127     C                   ADD       1             Y
026100940127     C*
026200940607     C                   WRITE     S1
026300940131     C*
026400940316    >C                   EXSR      REDANA
026500940128     C*
0266009401271-   C                   ENDDO
026700940127     C*
026800940223     C                   Z-ADD     S1NRR         WMAX                 30
026900940127     C*
027000940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
027100940127     C*
027200030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
027300940127     C                   MVR                     RESTO             3 0
027400030114     C     PAGINE        MULT      SFLPAG        C1RCD
0275000301141    C     RESTO         IFGT      0
027600030114     C                   ADD       1             C1RCD
0277000301141E   C                   ELSE
027800030114     C                   SUB       SFLPAG        C1RCD
027900030114     C                   ADD       1             C1RCD
0280000301141-   C                   ENDIF
028100940128     C*
028200940127     C                   ENDSR
028300940128     C************************************************************
028400170124     C* Istruzione SQL
028500940128     C************************************************************
028600170124     C     ISTRUZ_SQL    BEGSR
028700940128     C*
028800170124     C                   EVAL      WrkSqlCmd ='WITH PARTGIAC as  (SELECT '  +
028900170124     C                             ' ''GIAC_'' concat TBLkey as COD, '      +
029000170124     C                             ' substr(TBLUNI, 41, 1) as AUTGIAC '     +
029100170124     C                             ' FROM tabel00f WHERE tblcod=''7W''), '  +
029200170124     C                             ' PARTCONS as (SELECT ''CONS_'' concat ' +
029300170124     C                             ' TBLkey as COD, substr(TBLUNI, 70, 1) ' +
029400170124     C                             ' as AUTCONS FROM tabel00f WHERE '       +
029500170124     C                             ' tblcod=''7R''), CHIAUTORIZZA as '      +
029600170124     C                             ' (SELECT substr(TBEke1, 1, 1) as CHI,'  +
029700170124     C                             ' TBEuni as DESCHI FROM tntbe00f WHERE ' +
029800170124     C                             ' tbecod=''RAC''), INFOGIAC as (SELECT ' +
029900170124     C                             ' COD, CHI, DESCHI from PARTGIAC left '  +
030000170124     C                             ' outer join CHIAUTORIZZA on AUTGIAC = ' +
030100170124     C                             ' CHI),  INFOCONS as (SELECT COD, CHI, ' +
030200170124     C                             ' DESCHI from PARTCONS left outer join ' +
030300170124     C                             ' CHIAUTORIZZA on AUTCONS = CHI)       ' +
030400170124     C                             ' select COD, case when CHI is null '    +
030500170124     C                             ' then '' '' else CHI end as CHICOD, '   +
030600170124     C                             ' case when DESCHI is null '             +
030700170124     C                             ' then '' '' else DESCHI end as DESCOD ' +
030800170124     C                             ' from  infocons '                       +
030900170124     C                             ' UNION '                                +
031000170124     C                             ' select COD, case when CHI is null '    +
031100170124     C                             ' then '' '' else CHI end as CHICOD, '   +
031200170124     C                             ' case when DESCHI is null '             +
031300170124     C                             ' then '' '' else DESCHI end as DESCOD ' +
031400170124     C                             ' from  infogiac  '
031500170124     C*
031600170124     C                   ENDSR
031700170124     C************************************************************
031800170124     C* LETTURA RCD ARCHIVIO PILOTA
031900170124     C************************************************************
032000170124     C     REDANA        BEGSR
032100170124     C*
032200940131     C                   MOVEL     *OFF          $EFILE
032300940131     C                   MOVEL     *OFF          $RCDOK
032400940131     C*
0325009401311    C     $EFILE        DOUEQ     *ON
032600940131     C     $RCDOK        OREQ      *ON
032700940131     C*
032800170124     C/EXEC SQL
032900170124     C+ FETCH NEXT FROM A1 INTO :codpart, :chi, :deschi
033000170124     C/END-EXEC
033100170124     C*
033200170124     c                   if        sqlcod <> 0
033300030113     C                   MOVEL     *ON           $EFILE
033400030113     C                   MOVE      $EFILE        *IN33
033500030113     c                   else
033600030113     C                   MOVE      *ON           $RCDOK
033700030113     c                   end
033800940131     C*
0339009401311-   C                   ENDDO
034000940131     C*
034100940131     C                   ENDSR
034200940224     C************************************************************
034300940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
034400940224     C************************************************************
034500940224     C     CLCPAG        BEGSR
034600940224     C* Input :
034700940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
034800940224     C*          cursore
034900940224     C* - SFLPAG = numero rcd per pagina sfl
035000940224     C* Output :
035100940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
035200940224     C*
035300940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
035400940224     C                   MVR                     RESTO             3 0
035500940224     C     RESTO         IFGT      0
035600940224     C                   ADD       1             PAGINE
035700940224     C                   ENDIF
035800940224     C     PAGINE        MULT      SFLPAG        WPAG
035900940224     C*
036000940224     C                   ENDSR
036100940309     C************************************************************
036200940309     C* GESTIONE F03 VIDEO S1
036300940309     C************************************************************
036400940309     C     F03S1         BEGSR
036500940309     C*
036600940309     C                   MOVE      *ON           $FINE
036700940325     C* fine programma
036800940309     C                   ENDSR
036900940309     C/EJECT
037000940309     C************************************************************
037100940309     C* GESTIONE F10 VIDEO S1
037200940314     c* AGGIUNTA RECORD
037300940309     C************************************************************
037400940309     C     F10S1         BEGSR
037500940309     C*
037600030113     C                   RESET                   tabds
037700170123     C                   MOVEL     '1'           xtaopz
037800030113     C                   MOVE      *ZERO         xtaret
037900030113     C                   MOVE      *ZERO         xtaopr
038000030114     C                   MOVEl     *blank        xtakey
038100030113     C                   MOVE      *BLANKS       KPJBU
038200030113     C                   MOVEL     tabds         KPJBU
038300040930$004 C                   CALL      'TRTB87R2'
038400030113     C                   PARM                    KPJBA
038500030114     C                   MOVEL     KPJBU         tabds
038600030113      *
038700940309     C* ritorno da PGM gestione
038800940309     C                   EXSR      GESRET
038900940309     C     WINZS1        IFEQ      *ON
039000940309     C                   MOVE      *ON           $INZS1
039100940309     C* se esistevano già righe sul sfl
039200940309     C* calcolo pagina a cui deve posizionarsi
039300940309     C     WSFL          IFGT      0
039400940309     C                   EXSR      CLCPAG
039500940309     C* altrimenti carico solo la 1a pagina
039600940309     C                   ELSE
039700940309     C                   Z-ADD     SFLPAG        WPAG
039800940309     C                   ENDIF
039900940309     C                   ENDIF
040000940309     C*
040100940309     C                   ENDSR
040200940128     C************************************************************
040300940131     C* CONTROLLO TESTATA LISTA
040400940128     C************************************************************
040500940131     C     CTRC1         BEGSR
040600940128     C*
040700940201     C                   MOVE      *OFF          *IN99
040800940131     C*
040900940202     C                   ENDSR
041000940131     C************************************************************
041100940131     C* CONTROLLO OPZIONI LISTA
041200940131     C************************************************************
041300940131     C     CTRS1         BEGSR
041400940131     C*
041500940202     C                   MOVEL     *OFF          $ESCI
041600940201     C                   SETOFF                                       99
041700940131     C                   Z-ADD     0             S1OPZ
041800040930     c                   clear                   x1cod
041900940131     C*
042000940127     C* Leggo il sfl solo se ci sono rcd
0421009401311    C     WMAX          IFGT      0
042200940607     C                   READC     S1                                     21
042300940127     C*
042400940131     C* esce se fine sfl o errore che richiede l'uscita
0425009401312    C     *IN21         DOWEQ     *OFF
042600940131     C     $ESCI         ANDEQ     *OFF
042700940201     C                   Z-ADD     S1NRR         C1RCD
042800940131     C* ctrl su riga
042900940131     C                   EXSR      RECS1
043000940131     C* gestione opzioni
0431009401313    C     S1OPZ         IFNE      0
043200940201     C     *IN99         ANDEQ     *OFF
043300940131     C                   EXSR      OPZS1
0434009401273-   C                   ENDIF
043500940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0436009402013    C     *IN99         IFEQ      *ON
043700940201     C     $ESCI         OREQ      *ON
043800940131     C                   MOVE      *ON           *IN32
043900940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
044000940204     C* la ripristinerò a conclusione del ciclo di READC
044100940223     C                   MOVE      *OFF          $INZS1
0442009402233-   C                   ENDIF
044300940223     C*
044400940131     C                   Z-ADD     0             S1OPZ
044500940223     C*
044600940607     C                   UPDATE    S1
044700940223     C*
044800940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0449009401313    C     $ESCI         IFEQ      *OFF
045000940607     C                   READC     S1                                     21
045100940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0452009402014    C     *IN21         IFEQ      *ON
045300940201     C                   MOVE      WINZS1        $INZS1
045400940204     C* calcolo pagina a cui deve posizionarsi
045500940224     C                   EXSR      CLCPAG
0456009402014-   C                   ENDIF
0457009402013-   C                   ENDIF
045800940131     C*
0459009401272-   C                   ENDDO
046000940127     C*
0461009401311-   C                   ENDIF
046200940131     C*
046300940127     C                   ENDSR
046400940127     C/EJECT
046500940127     C************************************************************
046600940131     C* CONTROLLO CAMPI I/O RIGA LISTA
046700940127     C************************************************************
046800940131     C     RECS1         BEGSR
046900940131     C*
047000940201     C* reset indicatori DSPATR
047100940201     C                   MOVE      *ALL'0'       IN4049           10
047200940201     C                   MOVEA     IN4049        *IN(40)
047300940201     C*
047400040930     C*  se richiamato x selezionare un codice 11 = *on
047500040930     C*   non deve utilizzare le opzioni di modifica
047600040930     C*  e viceversa
047700040930     c     *in11         ifeq      *on
0478000409303    C     S1OPZ         IFeq      2
0479000409303    C     S1OPZ         OReq      3
0480000409303    C     S1OPZ         OReq      4
048100040930     c                   seton                                        41  99
048200040930     C                   END
048300040930      * selezionato un codice da restituire al chiamante
0484000409303    C     S1OPZ         IFeq      1
048500170124     c                   move      s1chi         x1cod
048600040930     c                   clear                   kpjbu
048700040930     c                   movel     x1cod         kpjbu
048800040930     C                   MOVEL     *ON           $ESCI
048900040930     C                   MOVEL     *ON           $fine
049000040930     C                   END
049100040930     C                   END
049200040930     C*
049300040930     c     *in11         ifeq      *off
0494000409303    C     S1OPZ         ANDeq     1
049500040930     c                   seton                                        41  99
049600040930     C                   END
049700040930     C*
049800940131     C                   ENDSR
049900940131     C************************************************************
050000940131     C* GESTIONE OPZIONI LISTA
050100940131     C************************************************************
050200940131     C     OPZS1         BEGSR
050300940201     C*
0504000409302    C     *IN11         IFEQ      *Off
0505000409302    C     *IN11         oreq      *On
0506001701232    C     s1opz         andeq     5
050700040930     C*
050800030113     C                   RESET                   tabds
050900030113     C                   MOVEL     S1OPZ         xtaopz
051000030113     C                   MOVE      *ZERO         xtaret
051100030113     C                   MOVE      *ZERO         xtaopr
051200170124     C                   MOVEl     S1CHI         xtakey
051300940715     C                   MOVE      *BLANKS       KPJBU
051400030113     C                   MOVEL     tabds         KPJBU
051500040930$004 C                   CALL      'TRTB87R2'
051600940607     C                   PARM                    KPJBA
051700030114     C                   MOVEL     KPJBU         tabds
051800940201     C*
051900940223     C* ritorno da PGM gestione
052000940223     C                   EXSR      GESRET
052100040930     C*
052200940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0523009402252    C     *IN99         IFEQ      *ON
052400940223     C                   SETON                                        40
0525009402252-   C                   ENDIF
052600940225     C*
0527000409302-   C                   ENDIF
052800040930     C*
052900940131     C                   ENDSR
053000940223     C/EJECT
053100940223     C************************************************************
053200940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
053300940223     C************************************************************
053400940223     C     GESRET        BEGSR
053500940223     C*
053600940223     C* modo di ritorno
053700940223     C*
0538009402231    C                   SELECT
053900940314    >C* << questi modi di utilizzo dei valori di ritorno dal
054000940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
054100940314    >C*    proposte, normalmente sempre valide, ma modificabili
054200940314    >C*    per situazioni particolari >>
054300940223     C* 1 = F3
054400030113    >C     xtaret        WHENEQ    '1'
054500940224     C                   MOVE      *ON           $ESCI
054600940223     C                   MOVE      *ON           $FINE
054700940223     C* 2 = F12
054800030113    >C     xtaret        WHENEQ    '2'
054900940223     C                   MOVE      *ON           $ESCI
055000940223     C*
0551009402231-   C                   ENDSL
055200940223     C*
055300940223     C* operazione eseguite dal pgm chiamato
055400940223     C*
0555009402231    C                   SELECT
055600940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
055700030113    >C     xtaopr        WHENEQ    '1'
055800940223     C                   MOVE      *ON           WINZS1
055900940223     C*
0560009402231-   C                   ENDSL
056100940223     C*
056200940223     C* funzione non eseguibile per errore :
056300940223     C*
0564009402231    C                   SELECT
056500940223     C* 1 = funzione richiamata chiusa in errore
056600940316    >C*  eventualmente gestire altri codici di errore
056700030113    >C     xtaerr        WHENEQ    '1'
056800940223     C                   MOVE      *ON           $ESCI
056900940223     C                   SETON                                        5299
057000940223     C*
0571009402231-   C                   ENDSL
057200940223     C*
057300940223     C                   ENDSR
057400940223     C/EJECT
057500940131     C************************************************************
057600940131     C* OPERAZIONI INIZIALI
057700940131     C************************************************************
057800940131     C     *INZSR        BEGSR
057900030113     C*
058000030113     C* Reperimento parametri
058100030113     C     *ENTRY        PLIST
058200030113     C                   PARM                    KPJBA
058300040930     C                   movel     kpjbu         Selez             1
058400040930     C*
058500040930     C* Se chiamato x selezionare un codice
058600040930     c                   clear                   x1cod
058700040930     c                   if        selez = '?'
058800040930     c                   seton                                        11
058900040930     c                   end
059000030113     C*
059100030113     C* Variabili per gestione videate
059200030113     C                   MOVE      *BLANK        $GEST             2
059300030113     C                   MOVE      *BLANK        $FINE             1
059400030113     C                   MOVE      *BLANK        $INZS1            1
059500030113     C                   MOVE      *BLANK        $EFILE            1
059600030113     C                   MOVE      *BLANK        $ESCI             1
059700030113     C                   MOVE      *BLANK        $RCDOK            1
059800030113     C*
059900030113     C     KTBL          KLIST
060000030113     C                   KFLD                    TBLKUT
060100030113     C                   KFLD                    TBLCOD
060200030113     C* Indici
060300030113     C                   Z-ADD     0             X                 3 0
060400030113     C                   Z-ADD     0             Y                 3 0
060500940127     C*
060600940117     C                   ENDSR
060700030113     C************************************************************
