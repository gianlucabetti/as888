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
001700170131      *  98           ERRORE SPECIALE : RICERCA NON f partNELLA POSIZ.
001800940224      *  99           INDIC. GENERALE DI ERRORE
001900940128     F*----------------------------------------------------*
002000030113     FTABEL00F  IF   E           K DISK
002100170123     FTRTB82D   CF   E             WORKSTN
002200940607     F                                     SFILE(S1:S1NRR)
002300940201     F                                     INFDS(DSFMT)
002400940128     D*----------------------------------------------------*
002500170125     D UTEDSE        E DS                  EXTNAME(UTEDSE0F)
002600940211     D* Passaggio Parametri
002700940211     D KPJBA         E DS
002800030113      *-------------
002900170126     D DSTB70        E DS                  EXTNAME(DS7PQRS)
003000170126      *-------------
003100940325     D* Parametri in ricezione
003200030113     D  TABDS          DS
003300170123     D  XTAfree                1      1
003400170123     D  XTAOPZ                 2      2
003500030113     D  XTARET                 3      3
003600030113     D  XTAOPR                 4      4
003700030113     D  XTAERR                 5      5
003800030113     D  XTAKEY                 6     13
003900940211     D*-------------
004000940211     D DSFMT           DS           512
004100940506     D  $TASTO               369    369
004200940211     D  NRG                  370    370
004300940211     D  NCL                  371    371
004400940211     D  SFLNRR               378    379B 0
004500940207     D*-------------
004600940207     D* Nome PGM a video
004700940207     D                 DS
004800940207     D  PROGR                  1     10
004900940207     D  ASTER1                 1      1    INZ('*')
005000940207     D  SIGLA                  2      9
005100940207     D  ASTER2                10     10    INZ('*')
005200940127     D*-------------
005300940127     D* Reperimento nome PGM
005400940127     D STATUS         SDS           333
005500940127     D  DSPGM            *PROC
005600030113     D*-------------
005700170124     D WrkSqlCmd       S           2048
005800170124     D CodPart         S              7
005900170315     D Codice          S              2
006000170124     D Chi             S              1
006100170124     D DesChi          S             35
006200170124     D Ordina          S              1  0 INZ(0)
006300170124     D*---
006400030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
006500030113$003 D S1NRR           S                   Like(C1rcd)
006600030113$003 D WSfl            S                   Like(C1nrr)
006700030113$003 D Wmax            S                   Like(C1rcd)
006800030113$003 D Wpag            S                   Like(C1rcd)
006900030113$003 D Winzs1          S                   Like($inzs1)
007000170124$003 D x1cod           S                   Like(s1chi)
007100940207     D*-------------
007200940211     D* COSTANTI
007300940211     D*-------------
007400170125     D SFLPAG          C                   CONST(16)
007500940314     D* dimensione della schiera $MS1
007600940506     D*
007700940506     D* Tasti di funzione
007800940506     D F01             C                   CONST(X'31')
007900940506     D F02             C                   CONST(X'32')
008000940506     D F03             C                   CONST(X'33')
008100940506     D F04             C                   CONST(X'34')
008200940506     D F05             C                   CONST(X'35')
008300940506     D F06             C                   CONST(X'36')
008400940506     D F07             C                   CONST(X'37')
008500940506     D F08             C                   CONST(X'38')
008600940506     D F09             C                   CONST(X'39')
008700940506     D F10             C                   CONST(X'3A')
008800940506     D F11             C                   CONST(X'3B')
008900940506     D F12             C                   CONST(X'3C')
009000940506     D F13             C                   CONST(X'B1')
009100940506     D F14             C                   CONST(X'B2')
009200940506     D F15             C                   CONST(X'B3')
009300940506     D F16             C                   CONST(X'B4')
009400940506     D F17             C                   CONST(X'B5')
009500940506     D F18             C                   CONST(X'B6')
009600940506     D F19             C                   CONST(X'B7')
009700940506     D F20             C                   CONST(X'B8')
009800940506     D F21             C                   CONST(X'B9')
009900940506     D F22             C                   CONST(X'BA')
010000940506     D F23             C                   CONST(X'BB')
010100940506     D F24             C                   CONST(X'BC')
010200940506     D ENTER           C                   CONST(X'F1')
010300940506     D ROLDWN          C                   CONST(X'F4')
010400940506     D ROLLUP          C                   CONST(X'F5')
010500940127     C*----------------------------------------------------*
010600940127     C*                MAIN LINE PROGRAM
010700940127     C*----------------------------------------------------*
010800940223     C* inizializzazione variabili
010900940223     C                   EXSR      INZVAR
011000940223     C*
011100940223     C     $FINE         DOWEQ     *OFF
011200940131     C     $GEST         CASEQ     'S1'          GESS1
011300940117     C                   END
011400940117     C                   END
011500940325     C* fine programma
011600940325     C                   SETON                                            LR
011700030113     C************************************************************
011800030113     C* INIZIALIZZAZIONE VARIABILI
011900030113     C************************************************************
012000030113     C     INZVAR        BEGSR
012100030113     C*
012200030113     C* Pulizia campi e indicatori
012300030113     C                   MOVE      *ALL'0'       IN4049           10
012400030113     C                   MOVEA     IN4049        *IN(40)
012500030113     C                   CLEAR                   S1OPZ
012600030113     C* Variabili per gestione videate
012700030113     C*
012800030113     C                   MOVE      *OFF          $FINE
012900030113     C                   MOVE      *OFF          $INZS1
013000030113     C                   MOVE      *OFF          $EFILE
013100030113     C                   MOVE      *OFF          $ESCI
013200030113     C                   MOVE      *OFF          $RCDOK
013300030113     C                   Z-ADD     0             $ULKS1            3 0
013400030113     C*
013500030113     C                   MOVE      *ON           $INZS1
013600030113     C                   MOVE      'S1'          $GEST
013700030113     C*
013800030113     C* Variabili appoggio
013900030114     C                   Z-ADD     1             WPAG
014000030113     c*
014100170124     c                   setoff                                       22
014200170124     c*
014300030113     C                   ENDSR
014400940127     C************************************************************
014500940131     C* GESTIONE LISTA
014600940127     C************************************************************
014700940127     C     GESS1         BEGSR
014800030113     C*
014900940223     C* inizializzazione videata
015000940223     C     $INZS1        IFEQ      *ON
015100940127     C                   EXSR      INZS1
015200940223     C                   MOVE      *OFF          $INZS1
015300940127     C                   ENDIF
015400030113     C*
015500030113     C* emissione piede videata
015600030113     C                   WRITE     Z1
015700030113     C* Non ci sono records
015800940223     C     WMAX          IFEQ      0
015900940607     C                   WRITE     D1
016000030114     C                   Else
016100030114     C     Wsfl          IFgt      0
016200030114     C                   Z-ADD     wsfl          C1RCD
016300030114     C                   Else
016400030114     C     Wpag          IFgt      0
016500030114     C                   Z-ADD     wpag          C1RCD
016600030114     C                   EndIF
016700030114     C                   EndIF
016800030114     C                   ENDIF
016900940127     C*
017000030113     C*              *------------------*
017100940607     C                   EXFMT     C1
017200030113     C*              *------------------*
017300030113     C*
017400940204     C     C1NRR         IFNE      0
017500940204     C                   Z-ADD     C1NRR         WSFL
017600940204     C                   ENDIF
017700940127     C                   Z-ADD     SFLNRR        C1RCD
017800030113     C* Selezioni
0179009401271    C                   SELECT
018000940127     C* F3=Fine
018100940506     C     $TASTO        WHENEQ    F03
018200940309     C                   EXSR      F03S1
018300940131     C* F10=Immissione
018400940506     C     $TASTO        WHENEQ    F10
018500940309     C                   EXSR      F10S1
018600170131     C* F11=Visualizza Lista Clienti Abilitati alle Particolarità
018700170131     C     $TASTO        WHENEQ    F11
018800170131     C                   EXSR      F11S1
018900170124     C* F8=Cambio Ordinamento
019000170124     C     $TASTO        WHENEQ    F08
019100170124     C                   EXSR      F08S1
0192009401271O   C                   OTHER
019300940127     C* CONTROLLO DATI
019400940131     C                   EXSR      CTRC1
019500940201     C     *IN99         IFEQ      *OFF
019600940131     C                   EXSR      CTRS1
019700940131     C                   END
0198009401271-   C                   ENDSL
019900940127     C*
020000940127     C                   ENDSR
020100940224     C/EJECT
020200940127     C************************************************************
020300940131     C* INIZIALIZZAZIONE LISTA
020400940127     C************************************************************
020500940127     C     INZS1         BEGSR
020600940302     C* pulizia SFL
020700940128     C                   SETOFF                                         3031
020800940607     C                   WRITE     C1
020900940128     C                   SETON                                          31
021000940128     C*
021100170125     C                   MOVEL     RAGUT         c1RSU
021200170124     C                   Z-ADD     1             WPAG
021300170124     C                   Z-ADD     1             WSFL
021400030113     C* CARICAMENTO SFL totale
021500940201     C                   Z-ADD     0             S1NRR
021600030113     C                   Z-ADD     1             C1RCD
021700940128     C                   Z-ADD     0             WMAX
021800170124     c                   if        Ordina = 0
021900170131     c                   eval      z1funz = 'F8=Ordina x UFFICIO   '
022000170124     c                   elseif    Ordina = 1
022100170131     c                   eval      z1funz = 'F8=Ordina x COD.Part. '
022200170124     c                   end
022300170131     C*
022400170131     c                   eval      z1funz11 = 'F11=Cli.Abil.Part.'
022500170131     C*
022600170124     C*  compone la lettura SQL
022700170124     c                   exsr      istruz_sql
022800170124      *
022900170124     C/EXEC SQL
023000170124     C+ PREPARE S1 FROM :WrkSqlCmd
023100170124     C/END-EXEC
023200170124
023300170124     C/EXEC SQL
023400170124     C+ DECLARE A1 CURSOR FOR S1
023500170124     C/END-EXEC
023600170124
023700170124     C/EXEC SQL
023800170124     C+ OPEN A1
023900170124     C/END-EXEC
024000170124
024100940224     C* Posizionamento su file pilota
024200940608    >C                   EXSR      REDANA
024300030113     C* Carico il SFL
024400940127     C                   EXSR      ROLS1
024500030113     C*
024600030114     c                   if        xtaopr <> '1'
024700030114     C                   Z-ADD     1             WPAG
024800030114     c                   end
024900940127     C*
025000940127     C                   ENDSR
025100940127     C************************************************************
025200940131     C* CARICAMENTO PAGINA LISTA
025300940127     C************************************************************
025400940127     C     ROLS1         BEGSR
025500940127     C*
025600940128     C                   SETOFF                                       32
025700940223     C                   Z-ADD     0             Y
025800940127     C                   Z-ADD     WMAX          S1NRR
025900940127     C*
026000940127     C* Leggo dal file anagrafico per caricare la lista
0261009401311    C     $EFILE        DOWEQ     *OFF
026200940127     C*
026300170124     c                   clear                   s1
026400170124     c                   movel     chi           s1chi
026500170124     c                   movel     deschi        s1des
026600170125     c                   move      codPart       s1part
026700170125     c                   eval       s1tipo = %subst(codPart:1:1)
026800170124     c
026900030115     c                   clear                   S1Ann
027000170124     c******             if        tblflg ='*'
027100170124     c******             move      'A'           S1Ann
027200170124     c******             end
027300940127     C*
027400940127     C                   ADD       1             S1NRR
027500940127     C                   ADD       1             Y
027600940127     C*
027700940607     C                   WRITE     S1
027800940131     C*
027900940316    >C                   EXSR      REDANA
028000940128     C*
0281009401271-   C                   ENDDO
028200170124     C/EXEC SQL
028300170124     C+ CLOSE A1
028400170124     C/END-EXEC
028500940127     C*
028600940223     C                   Z-ADD     S1NRR         WMAX                 30
028700940127     C*
028800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
028900940127     C*
029000030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
029100940127     C                   MVR                     RESTO             3 0
029200030114     C     PAGINE        MULT      SFLPAG        C1RCD
0293000301141    C     RESTO         IFGT      0
029400030114     C                   ADD       1             C1RCD
0295000301141E   C                   ELSE
029600030114     C                   SUB       SFLPAG        C1RCD
029700030114     C                   ADD       1             C1RCD
0298000301141-   C                   ENDIF
029900940128     C*
030000940127     C                   ENDSR
030100940128     C************************************************************
030200170124     C* Istruzione SQL
030300940128     C************************************************************
030400170124     C     ISTRUZ_SQL    BEGSR
030500940128     C*
030600170124     C                   EVAL      WrkSqlCmd ='WITH PARTGIAC as  (SELECT '  +
030700170125     C                             ' ''Giac '' concat TBLkey as COD, '      +
030800170124     C                             ' substr(TBLUNI, 41, 1) as AUTGIAC '     +
030900170124     C                             ' FROM tabel00f WHERE tblcod=''7W''), '  +
031000170125     C                             ' PARTCONS as (SELECT ''Cons '' concat ' +
031100170124     C                             ' TBLkey as COD, substr(TBLUNI, 70, 1) ' +
031200170124     C                             ' as AUTCONS FROM tabel00f WHERE '       +
031300170124     C                             ' tblcod=''7R''), CHIAUTORIZZA as '      +
031400170124     C                             ' (SELECT substr(TBEke1, 1, 1) as CHI,'  +
031500170124     C                             ' TBEuni as DESCHI FROM tntbe00f WHERE ' +
031600170124     C                             ' tbecod=''RAC''), INFOGIAC as (SELECT ' +
031700170124     C                             ' COD, CHI, DESCHI from PARTGIAC left '  +
031800170124     C                             ' outer join CHIAUTORIZZA on AUTGIAC = ' +
031900170124     C                             ' CHI),  INFOCONS as (SELECT COD, CHI, ' +
032000170124     C                             ' DESCHI from PARTCONS left outer join ' +
032100170124     C                             ' CHIAUTORIZZA on AUTCONS = CHI)       ' +
032200170315     C                             ' select COD, substr(COD, 6, 2) as '     +
032300170315     C                             ' CODICE, case when CHI is null '        +
032400170124     C                             ' then '' '' else CHI end as CHICOD, '   +
032500170124     C                             ' case when DESCHI is null '             +
032600170124     C                             ' then '' '' else DESCHI end as DESCOD ' +
032700170124     C                             ' from  infocons '                       +
032800170124     C                             ' UNION '                                +
032900170315     C                             ' select COD, substr(COD, 6, 2) as '     +
033000170315     C                             ' CODICE, case when CHI is null '        +
033100170124     C                             ' then '' '' else CHI end as CHICOD, '   +
033200170124     C                             ' case when DESCHI is null '             +
033300170124     C                             ' then '' '' else DESCHI end as DESCOD ' +
033400170124     C                             ' from  infogiac  ORDER BY '
033500170124     C*
033600170124     c                   if        Ordina = 0
033700170124     C                   EVAL      WrkSqlCmd = %Trim(WrkSqlCmd)  +
033800170315     c                             ' CODICE, COD '
033900170124     c                   elseif    Ordina = 1
034000170124     C                   EVAL      WrkSqlCmd = %Trim(WrkSqlCmd)  +
034100170315     c                             ' CHICOD, CODICE, COD '
034200170124     c                   end
034300170124     C*
034400170124     C                   ENDSR
034500170124     C************************************************************
034600170124     C* LETTURA RCD ARCHIVIO PILOTA
034700170124     C************************************************************
034800170124     C     REDANA        BEGSR
034900170124     C*
035000940131     C                   MOVEL     *OFF          $EFILE
035100940131     C                   MOVEL     *OFF          $RCDOK
035200940131     C*
0353009401311    C     $EFILE        DOUEQ     *ON
035400940131     C     $RCDOK        OREQ      *ON
035500940131     C*
035600170124     C/EXEC SQL
035700170315     C+ FETCH NEXT FROM A1 INTO :codpart, :codice, :chi, :deschi
035800170124     C/END-EXEC
035900170124     C*
036000170124     c                   if        sqlcod <> 0
036100030113     C                   MOVEL     *ON           $EFILE
036200030113     C                   MOVE      $EFILE        *IN33
036300030113     c                   else
036400030113     C                   MOVE      *ON           $RCDOK
036500030113     c                   end
036600940131     C*
0367009401311-   C                   ENDDO
036800940131     C*
036900940131     C                   ENDSR
037000940224     C************************************************************
037100940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
037200940224     C************************************************************
037300940224     C     CLCPAG        BEGSR
037400940224     C* Input :
037500940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
037600940224     C*          cursore
037700940224     C* - SFLPAG = numero rcd per pagina sfl
037800940224     C* Output :
037900940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
038000940224     C*
038100940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
038200940224     C                   MVR                     RESTO             3 0
038300940224     C     RESTO         IFGT      0
038400940224     C                   ADD       1             PAGINE
038500940224     C                   ENDIF
038600940224     C     PAGINE        MULT      SFLPAG        WPAG
038700940224     C*
038800940224     C                   ENDSR
038900940309     C************************************************************
039000940309     C* GESTIONE F03 VIDEO S1
039100940309     C************************************************************
039200940309     C     F03S1         BEGSR
039300940309     C*
039400940309     C                   MOVE      *ON           $FINE
039500940325     C* fine programma
039600940309     C                   ENDSR
039700940309     C/EJECT
039800170124     C************************************************************
039900170124     C* GESTIONE F08 VIDEO S1
040000170124     C************************************************************
040100170124     C     F08S1         BEGSR
040200170124     C*
040300170124     C*  INVERTE
040400170124     c                   eval      $INZS1 = *ON
040500170124     c                   if        Ordina = 0
040600170124     c                   eval      Ordina = 1
040700170124     c                   elseif    Ordina = 1
040800170124     c                   eval      Ordina = 0
040900170124     c                   end
041000170124     C*
041100170124     C                   ENDSR
041200170124     C/EJECT
041300170131     C************************************************************
041400170131     C* GESTIONE F11 VIDEO S1
041500170131     C************************************************************
041600170131     C     F11S1         BEGSR
041700170131     C*
041800170131     C                   CALL      'TRTB71R1'
041900170131     C                   PARM                    KNSIF
042000170131     C                   PARM      *ALL' '       PCLI              8
042100170131     C                   PARM      *ALL'V'       PRAG             25
042200170131     C                   PARM                    KNMUS            10
042300170131     C                   PARM                    C1RSU
042400170131     C*
042500170131     C                   ENDSR
042600170131     C/EJECT
042700940309     C************************************************************
042800940309     C* GESTIONE F10 VIDEO S1
042900940314     c* AGGIUNTA RECORD
043000940309     C************************************************************
043100940309     C     F10S1         BEGSR
043200940309     C*
043300170124     C                   LEAVESR
043400170124     C*
043500030113     C                   RESET                   tabds
043600170123     C                   MOVEL     '1'           xtaopz
043700030113     C                   MOVE      *ZERO         xtaret
043800030113     C                   MOVE      *ZERO         xtaopr
043900030114     C                   MOVEl     *blank        xtakey
044000030113     C                   MOVE      *BLANKS       KPJBU
044100030113     C                   MOVEL     tabds         KPJBU
044200170124$004 C                   CALL      'TRTB82R2'
044300030113     C                   PARM                    KPJBA
044400030114     C                   MOVEL     KPJBU         tabds
044500030113      *
044600940309     C* ritorno da PGM gestione
044700940309     C                   EXSR      GESRET
044800940309     C     WINZS1        IFEQ      *ON
044900940309     C                   MOVE      *ON           $INZS1
045000940309     C* se esistevano già righe sul sfl
045100940309     C* calcolo pagina a cui deve posizionarsi
045200940309     C     WSFL          IFGT      0
045300940309     C                   EXSR      CLCPAG
045400940309     C* altrimenti carico solo la 1a pagina
045500940309     C                   ELSE
045600940309     C                   Z-ADD     SFLPAG        WPAG
045700940309     C                   ENDIF
045800940309     C                   ENDIF
045900940309     C*
046000940309     C                   ENDSR
046100940128     C************************************************************
046200940131     C* CONTROLLO TESTATA LISTA
046300940128     C************************************************************
046400940131     C     CTRC1         BEGSR
046500940128     C*
046600940201     C                   MOVE      *OFF          *IN99
046700940131     C*
046800940202     C                   ENDSR
046900940131     C************************************************************
047000940131     C* CONTROLLO OPZIONI LISTA
047100940131     C************************************************************
047200940131     C     CTRS1         BEGSR
047300940131     C*
047400940202     C                   MOVEL     *OFF          $ESCI
047500940201     C                   SETOFF                                       99
047600940131     C                   Z-ADD     0             S1OPZ
047700040930     c                   clear                   x1cod
047800940131     C*
047900940127     C* Leggo il sfl solo se ci sono rcd
0480009401311    C     WMAX          IFGT      0
048100940607     C                   READC     S1                                     21
048200940127     C*
048300940131     C* esce se fine sfl o errore che richiede l'uscita
0484009401312    C     *IN21         DOWEQ     *OFF
048500940131     C     $ESCI         ANDEQ     *OFF
048600940201     C                   Z-ADD     S1NRR         C1RCD
048700940131     C* ctrl su riga
048800940131     C                   EXSR      RECS1
048900940131     C* gestione opzioni
0490009401313    C     S1OPZ         IFNE      0
049100940201     C     *IN99         ANDEQ     *OFF
049200940131     C                   EXSR      OPZS1
0493009401273-   C                   ENDIF
049400940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0495009402013    C     *IN99         IFEQ      *ON
049600940201     C     $ESCI         OREQ      *ON
049700940131     C                   MOVE      *ON           *IN32
049800940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
049900940204     C* la ripristinerò a conclusione del ciclo di READC
050000940223     C                   MOVE      *OFF          $INZS1
0501009402233-   C                   ENDIF
050200940223     C*
050300940131     C                   Z-ADD     0             S1OPZ
050400940223     C*
050500940607     C                   UPDATE    S1
050600940223     C*
050700940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0508009401313    C     $ESCI         IFEQ      *OFF
050900940607     C                   READC     S1                                     21
051000940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0511009402014    C     *IN21         IFEQ      *ON
051200940201     C                   MOVE      WINZS1        $INZS1
051300940204     C* calcolo pagina a cui deve posizionarsi
051400940224     C                   EXSR      CLCPAG
0515009402014-   C                   ENDIF
0516009402013-   C                   ENDIF
051700940131     C*
0518009401272-   C                   ENDDO
051900940127     C*
0520009401311-   C                   ENDIF
052100940131     C*
052200940127     C                   ENDSR
052300940127     C/EJECT
052400940127     C************************************************************
052500940131     C* CONTROLLO CAMPI I/O RIGA LISTA
052600940127     C************************************************************
052700940131     C     RECS1         BEGSR
052800940131     C*
052900940201     C* reset indicatori DSPATR
053000940201     C                   MOVE      *ALL'0'       IN4049           10
053100940201     C                   MOVEA     IN4049        *IN(40)
053200940201     C*
053300040930     C*  se richiamato x selezionare un codice 11 = *on
053400040930     C*   non deve utilizzare le opzioni di modifica
053500040930     C*  e viceversa
053600040930     c     *in11         ifeq      *on
0537000409303    C     S1OPZ         IFeq      2
0538000409303    C     S1OPZ         OReq      3
0539000409303    C     S1OPZ         OReq      4
054000040930     c                   seton                                        41  99
054100040930     C                   END
054200040930      * selezionato un codice da restituire al chiamante
0543000409303    C     S1OPZ         IFeq      1
054400170124     c                   move      s1chi         x1cod
054500040930     c                   clear                   kpjbu
054600040930     c                   movel     x1cod         kpjbu
054700040930     C                   MOVEL     *ON           $ESCI
054800040930     C                   MOVEL     *ON           $fine
054900040930     C                   END
055000040930     C                   END
055100040930     C*
055200040930     c     *in11         ifeq      *off
0553000409303    C     S1OPZ         ANDeq     1
055400040930     c                   seton                                        41  99
055500040930     C                   END
055600040930     C*
055700940131     C                   ENDSR
055800940131     C************************************************************
055900940131     C* GESTIONE OPZIONI LISTA
056000940131     C************************************************************
056100940131     C     OPZS1         BEGSR
056200940201     C*
0563000409302    C     *IN11         IFEQ      *Off
0564000409302    C     *IN11         oreq      *On
0565001701232    C     s1opz         andeq     5
056600040930     C*
056700170126     c                   if        s1tipo = 'C    '
056800170126     C*
056900170126     c                   clear                   DSTB70
057000170126     C                   MOVEL     s1opz         DS7OPZ
057100170126     C                   MOVEL     s1part        DS7COD
057200170126     C                   MOVEL     DSTB70        KPJBU
057300170126     C                   CALL      'TRTB70R0'
057400170126     C                   PARM                    KPJBA
057500170126     C                   MOVEL     KPJBU         DSTB70
057600170126     C     DS7F03        IFEQ      '1'
057700170126     C                   MOVEL     DS7F03        xtaret
057800170126     C                   END
057900170126     C*
058000170126     c                   elseif    s1tipo = 'G    '
058100170126     C*
058200170126     c                   clear                   DSTB70
058300170126     C                   MOVEL     s1opz         DS7OPZ
058400170126     C                   MOVEL     s1part        DS7COD
058500170126     C                   MOVEL     DSTB70        KPJBU
058600170126     C                   CALL      'TRTB69R0'
058700170126     C                   PARM                    KPJBA
058800170126     C                   MOVEL     KPJBU         DSTB70
058900170126     C     DS7F03        IFEQ      '1'
059000170126     C                   MOVEL     DS7F03        xtaret
059100170126     C                   END
059200170126     C*
059300170126     C*
059400170125     c                   end
059500170125     C*
059600940201     C*
059700940223     C* ritorno da PGM gestione
059800940223     C                   EXSR      GESRET
059900040930     C*
060000940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0601009402252    C     *IN99         IFEQ      *ON
060200940223     C                   SETON                                        40
0603009402252-   C                   ENDIF
060400940225     C*
0605000409302-   C                   ENDIF
060600040930     C*
060700940131     C                   ENDSR
060800940223     C/EJECT
060900940223     C************************************************************
061000940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
061100940223     C************************************************************
061200940223     C     GESRET        BEGSR
061300940223     C*
061400940223     C* modo di ritorno
061500940223     C*
0616009402231    C                   SELECT
061700940314    >C* << questi modi di utilizzo dei valori di ritorno dal
061800940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
061900940314    >C*    proposte, normalmente sempre valide, ma modificabili
062000940314    >C*    per situazioni particolari >>
062100940223     C* 1 = F3
062200030113    >C     xtaret        WHENEQ    '1'
062300940224     C                   MOVE      *ON           $ESCI
062400940223     C                   MOVE      *ON           $FINE
062500940223     C* 2 = F12
062600030113    >C     xtaret        WHENEQ    '2'
062700940223     C                   MOVE      *ON           $ESCI
062800940223     C*
0629009402231-   C                   ENDSL
063000940223     C*
063100940223     C* operazione eseguite dal pgm chiamato
063200940223     C*
0633009402231    C                   SELECT
063400940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
063500030113    >C     xtaopr        WHENEQ    '1'
063600940223     C                   MOVE      *ON           WINZS1
063700940223     C*
0638009402231-   C                   ENDSL
063900940223     C*
064000940223     C* funzione non eseguibile per errore :
064100940223     C*
0642009402231    C                   SELECT
064300940223     C* 1 = funzione richiamata chiusa in errore
064400940316    >C*  eventualmente gestire altri codici di errore
064500030113    >C     xtaerr        WHENEQ    '1'
064600940223     C                   MOVE      *ON           $ESCI
064700940223     C                   SETON                                        5299
064800940223     C*
0649009402231-   C                   ENDSL
065000940223     C*
065100940223     C                   ENDSR
065200940223     C/EJECT
065300940131     C************************************************************
065400940131     C* OPERAZIONI INIZIALI
065500940131     C************************************************************
065600940131     C     *INZSR        BEGSR
065700030113     C*
065800030113     C* Reperimento parametri
065900030113     C     *ENTRY        PLIST
066000030113     C                   PARM                    KPJBA
066100040930     C                   movel     kpjbu         Selez             1
066200170125     C*  Richiamo XPARUT
066300170125     C                   Z-ADD     1             CODUT
066400170125     C                   CALL      'XPARUT'
066500170125     C                   PARM                    UTEDSE
066600040930     C*
066700040930     C* Se chiamato x selezionare un codice
066800040930     c                   clear                   x1cod
066900040930     c                   if        selez = '?'
067000040930     c                   seton                                        11
067100040930     c                   end
067200030113     C*
067300030113     C* Variabili per gestione videate
067400030113     C                   MOVE      *BLANK        $GEST             2
067500030113     C                   MOVE      *BLANK        $FINE             1
067600030113     C                   MOVE      *BLANK        $INZS1            1
067700030113     C                   MOVE      *BLANK        $EFILE            1
067800030113     C                   MOVE      *BLANK        $ESCI             1
067900030113     C                   MOVE      *BLANK        $RCDOK            1
068000030113     C*
068100030113     C     KTBL          KLIST
068200030113     C                   KFLD                    TBLKUT
068300030113     C                   KFLD                    TBLCOD
068400030113     C* Indici
068500030113     C                   Z-ADD     0             X                 3 0
068600030113     C                   Z-ADD     0             Y                 3 0
068700940127     C*
068800940117     C                   ENDSR
068900030113     C************************************************************
