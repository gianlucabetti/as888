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
005900170124     D Chi             S              1
006000170124     D DesChi          S             35
006100170124     D Ordina          S              1  0 INZ(0)
006200170124     D*---
006300030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
006400030113$003 D S1NRR           S                   Like(C1rcd)
006500030113$003 D WSfl            S                   Like(C1nrr)
006600030113$003 D Wmax            S                   Like(C1rcd)
006700030113$003 D Wpag            S                   Like(C1rcd)
006800030113$003 D Winzs1          S                   Like($inzs1)
006900170124$003 D x1cod           S                   Like(s1chi)
007000940207     D*-------------
007100940211     D* COSTANTI
007200940211     D*-------------
007300170125     D SFLPAG          C                   CONST(16)
007400940314     D* dimensione della schiera $MS1
007500940506     D*
007600940506     D* Tasti di funzione
007700940506     D F01             C                   CONST(X'31')
007800940506     D F02             C                   CONST(X'32')
007900940506     D F03             C                   CONST(X'33')
008000940506     D F04             C                   CONST(X'34')
008100940506     D F05             C                   CONST(X'35')
008200940506     D F06             C                   CONST(X'36')
008300940506     D F07             C                   CONST(X'37')
008400940506     D F08             C                   CONST(X'38')
008500940506     D F09             C                   CONST(X'39')
008600940506     D F10             C                   CONST(X'3A')
008700940506     D F11             C                   CONST(X'3B')
008800940506     D F12             C                   CONST(X'3C')
008900940506     D F13             C                   CONST(X'B1')
009000940506     D F14             C                   CONST(X'B2')
009100940506     D F15             C                   CONST(X'B3')
009200940506     D F16             C                   CONST(X'B4')
009300940506     D F17             C                   CONST(X'B5')
009400940506     D F18             C                   CONST(X'B6')
009500940506     D F19             C                   CONST(X'B7')
009600940506     D F20             C                   CONST(X'B8')
009700940506     D F21             C                   CONST(X'B9')
009800940506     D F22             C                   CONST(X'BA')
009900940506     D F23             C                   CONST(X'BB')
010000940506     D F24             C                   CONST(X'BC')
010100940506     D ENTER           C                   CONST(X'F1')
010200940506     D ROLDWN          C                   CONST(X'F4')
010300940506     D ROLLUP          C                   CONST(X'F5')
010400940127     C*----------------------------------------------------*
010500940127     C*                MAIN LINE PROGRAM
010600940127     C*----------------------------------------------------*
010700940223     C* inizializzazione variabili
010800940223     C                   EXSR      INZVAR
010900940223     C*
011000940223     C     $FINE         DOWEQ     *OFF
011100940131     C     $GEST         CASEQ     'S1'          GESS1
011200940117     C                   END
011300940117     C                   END
011400940325     C* fine programma
011500940325     C                   SETON                                            LR
011600030113     C************************************************************
011700030113     C* INIZIALIZZAZIONE VARIABILI
011800030113     C************************************************************
011900030113     C     INZVAR        BEGSR
012000030113     C*
012100030113     C* Pulizia campi e indicatori
012200030113     C                   MOVE      *ALL'0'       IN4049           10
012300030113     C                   MOVEA     IN4049        *IN(40)
012400030113     C                   CLEAR                   S1OPZ
012500030113     C* Variabili per gestione videate
012600030113     C*
012700030113     C                   MOVE      *OFF          $FINE
012800030113     C                   MOVE      *OFF          $INZS1
012900030113     C                   MOVE      *OFF          $EFILE
013000030113     C                   MOVE      *OFF          $ESCI
013100030113     C                   MOVE      *OFF          $RCDOK
013200030113     C                   Z-ADD     0             $ULKS1            3 0
013300030113     C*
013400030113     C                   MOVE      *ON           $INZS1
013500030113     C                   MOVE      'S1'          $GEST
013600030113     C*
013700030113     C* Variabili appoggio
013800030114     C                   Z-ADD     1             WPAG
013900030113     c*
014000170124     c                   setoff                                       22
014100170124     c*
014200030113     C                   ENDSR
014300940127     C************************************************************
014400940131     C* GESTIONE LISTA
014500940127     C************************************************************
014600940127     C     GESS1         BEGSR
014700030113     C*
014800940223     C* inizializzazione videata
014900940223     C     $INZS1        IFEQ      *ON
015000940127     C                   EXSR      INZS1
015100940223     C                   MOVE      *OFF          $INZS1
015200940127     C                   ENDIF
015300030113     C*
015400030113     C* emissione piede videata
015500030113     C                   WRITE     Z1
015600030113     C* Non ci sono records
015700940223     C     WMAX          IFEQ      0
015800940607     C                   WRITE     D1
015900030114     C                   Else
016000030114     C     Wsfl          IFgt      0
016100030114     C                   Z-ADD     wsfl          C1RCD
016200030114     C                   Else
016300030114     C     Wpag          IFgt      0
016400030114     C                   Z-ADD     wpag          C1RCD
016500030114     C                   EndIF
016600030114     C                   EndIF
016700030114     C                   ENDIF
016800940127     C*
016900030113     C*              *------------------*
017000940607     C                   EXFMT     C1
017100030113     C*              *------------------*
017200030113     C*
017300940204     C     C1NRR         IFNE      0
017400940204     C                   Z-ADD     C1NRR         WSFL
017500940204     C                   ENDIF
017600940127     C                   Z-ADD     SFLNRR        C1RCD
017700030113     C* Selezioni
0178009401271    C                   SELECT
017900940127     C* F3=Fine
018000940506     C     $TASTO        WHENEQ    F03
018100940309     C                   EXSR      F03S1
018200940131     C* F10=Immissione
018300940506     C     $TASTO        WHENEQ    F10
018400940309     C                   EXSR      F10S1
018500170131     C* F11=Visualizza Lista Clienti Abilitati alle Particolarità
018600170131     C     $TASTO        WHENEQ    F11
018700170131     C                   EXSR      F11S1
018800170124     C* F8=Cambio Ordinamento
018900170124     C     $TASTO        WHENEQ    F08
019000170124     C                   EXSR      F08S1
0191009401271O   C                   OTHER
019200940127     C* CONTROLLO DATI
019300940131     C                   EXSR      CTRC1
019400940201     C     *IN99         IFEQ      *OFF
019500940131     C                   EXSR      CTRS1
019600940131     C                   END
0197009401271-   C                   ENDSL
019800940127     C*
019900940127     C                   ENDSR
020000940224     C/EJECT
020100940127     C************************************************************
020200940131     C* INIZIALIZZAZIONE LISTA
020300940127     C************************************************************
020400940127     C     INZS1         BEGSR
020500940302     C* pulizia SFL
020600940128     C                   SETOFF                                         3031
020700940607     C                   WRITE     C1
020800940128     C                   SETON                                          31
020900940128     C*
021000170125     C                   MOVEL     RAGUT         c1RSU
021100170124     C                   Z-ADD     1             WPAG
021200170124     C                   Z-ADD     1             WSFL
021300030113     C* CARICAMENTO SFL totale
021400940201     C                   Z-ADD     0             S1NRR
021500030113     C                   Z-ADD     1             C1RCD
021600940128     C                   Z-ADD     0             WMAX
021700170124     c                   if        Ordina = 0
021800170131     c                   eval      z1funz = 'F8=Ordina x UFFICIO   '
021900170124     c                   elseif    Ordina = 1
022000170131     c                   eval      z1funz = 'F8=Ordina x COD.Part. '
022100170124     c                   end
022200170131     C*
022300170131     c                   eval      z1funz11 = 'F11=Cli.Abil.Part.'
022400170131     C*
022500170124     C*  compone la lettura SQL
022600170124     c                   exsr      istruz_sql
022700170124      *
022800170124     C/EXEC SQL
022900170124     C+ PREPARE S1 FROM :WrkSqlCmd
023000170124     C/END-EXEC
023100170124
023200170124     C/EXEC SQL
023300170124     C+ DECLARE A1 CURSOR FOR S1
023400170124     C/END-EXEC
023500170124
023600170124     C/EXEC SQL
023700170124     C+ OPEN A1
023800170124     C/END-EXEC
023900170124
024000940224     C* Posizionamento su file pilota
024100940608    >C                   EXSR      REDANA
024200030113     C* Carico il SFL
024300940127     C                   EXSR      ROLS1
024400030113     C*
024500030114     c                   if        xtaopr <> '1'
024600030114     C                   Z-ADD     1             WPAG
024700030114     c                   end
024800940127     C*
024900940127     C                   ENDSR
025000940127     C************************************************************
025100940131     C* CARICAMENTO PAGINA LISTA
025200940127     C************************************************************
025300940127     C     ROLS1         BEGSR
025400940127     C*
025500940128     C                   SETOFF                                       32
025600940223     C                   Z-ADD     0             Y
025700940127     C                   Z-ADD     WMAX          S1NRR
025800940127     C*
025900940127     C* Leggo dal file anagrafico per caricare la lista
0260009401311    C     $EFILE        DOWEQ     *OFF
026100940127     C*
026200170124     c                   clear                   s1
026300170124     c                   movel     chi           s1chi
026400170124     c                   movel     deschi        s1des
026500170125     c                   move      codPart       s1part
026600170125     c                   eval       s1tipo = %subst(codPart:1:1)
026700170124     c
026800030115     c                   clear                   S1Ann
026900170124     c******             if        tblflg ='*'
027000170124     c******             move      'A'           S1Ann
027100170124     c******             end
027200940127     C*
027300940127     C                   ADD       1             S1NRR
027400940127     C                   ADD       1             Y
027500940127     C*
027600940607     C                   WRITE     S1
027700940131     C*
027800940316    >C                   EXSR      REDANA
027900940128     C*
0280009401271-   C                   ENDDO
028100170124     C/EXEC SQL
028200170124     C+ CLOSE A1
028300170124     C/END-EXEC
028400940127     C*
028500940223     C                   Z-ADD     S1NRR         WMAX                 30
028600940127     C*
028700940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
028800940127     C*
028900030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
029000940127     C                   MVR                     RESTO             3 0
029100030114     C     PAGINE        MULT      SFLPAG        C1RCD
0292000301141    C     RESTO         IFGT      0
029300030114     C                   ADD       1             C1RCD
0294000301141E   C                   ELSE
029500030114     C                   SUB       SFLPAG        C1RCD
029600030114     C                   ADD       1             C1RCD
0297000301141-   C                   ENDIF
029800940128     C*
029900940127     C                   ENDSR
030000940128     C************************************************************
030100170124     C* Istruzione SQL
030200940128     C************************************************************
030300170124     C     ISTRUZ_SQL    BEGSR
030400940128     C*
030500170124     C                   EVAL      WrkSqlCmd ='WITH PARTGIAC as  (SELECT '  +
030600170125     C                             ' ''Giac '' concat TBLkey as COD, '      +
030700170124     C                             ' substr(TBLUNI, 41, 1) as AUTGIAC '     +
030800170124     C                             ' FROM tabel00f WHERE tblcod=''7W''), '  +
030900170125     C                             ' PARTCONS as (SELECT ''Cons '' concat ' +
031000170124     C                             ' TBLkey as COD, substr(TBLUNI, 70, 1) ' +
031100170124     C                             ' as AUTCONS FROM tabel00f WHERE '       +
031200170124     C                             ' tblcod=''7R''), CHIAUTORIZZA as '      +
031300170124     C                             ' (SELECT substr(TBEke1, 1, 1) as CHI,'  +
031400170124     C                             ' TBEuni as DESCHI FROM tntbe00f WHERE ' +
031500170124     C                             ' tbecod=''RAC''), INFOGIAC as (SELECT ' +
031600170124     C                             ' COD, CHI, DESCHI from PARTGIAC left '  +
031700170124     C                             ' outer join CHIAUTORIZZA on AUTGIAC = ' +
031800170124     C                             ' CHI),  INFOCONS as (SELECT COD, CHI, ' +
031900170124     C                             ' DESCHI from PARTCONS left outer join ' +
032000170124     C                             ' CHIAUTORIZZA on AUTCONS = CHI)       ' +
032100170124     C                             ' select COD, case when CHI is null '    +
032200170124     C                             ' then '' '' else CHI end as CHICOD, '   +
032300170124     C                             ' case when DESCHI is null '             +
032400170124     C                             ' then '' '' else DESCHI end as DESCOD ' +
032500170124     C                             ' from  infocons '                       +
032600170124     C                             ' UNION '                                +
032700170124     C                             ' select COD, case when CHI is null '    +
032800170124     C                             ' then '' '' else CHI end as CHICOD, '   +
032900170124     C                             ' case when DESCHI is null '             +
033000170124     C                             ' then '' '' else DESCHI end as DESCOD ' +
033100170124     C                             ' from  infogiac  ORDER BY '
033200170124     C*
033300170124     c                   if        Ordina = 0
033400170124     C                   EVAL      WrkSqlCmd = %Trim(WrkSqlCmd)  +
033500170124     c                             ' COD '
033600170124     c                   elseif    Ordina = 1
033700170124     C                   EVAL      WrkSqlCmd = %Trim(WrkSqlCmd)  +
033800170124     c                             ' CHICOD, COD '
033900170124     c                   end
034000170124     C*
034100170124     C                   ENDSR
034200170124     C************************************************************
034300170124     C* LETTURA RCD ARCHIVIO PILOTA
034400170124     C************************************************************
034500170124     C     REDANA        BEGSR
034600170124     C*
034700940131     C                   MOVEL     *OFF          $EFILE
034800940131     C                   MOVEL     *OFF          $RCDOK
034900940131     C*
0350009401311    C     $EFILE        DOUEQ     *ON
035100940131     C     $RCDOK        OREQ      *ON
035200940131     C*
035300170124     C/EXEC SQL
035400170124     C+ FETCH NEXT FROM A1 INTO :codpart, :chi, :deschi
035500170124     C/END-EXEC
035600170124     C*
035700170124     c                   if        sqlcod <> 0
035800030113     C                   MOVEL     *ON           $EFILE
035900030113     C                   MOVE      $EFILE        *IN33
036000030113     c                   else
036100030113     C                   MOVE      *ON           $RCDOK
036200030113     c                   end
036300940131     C*
0364009401311-   C                   ENDDO
036500940131     C*
036600940131     C                   ENDSR
036700940224     C************************************************************
036800940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
036900940224     C************************************************************
037000940224     C     CLCPAG        BEGSR
037100940224     C* Input :
037200940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
037300940224     C*          cursore
037400940224     C* - SFLPAG = numero rcd per pagina sfl
037500940224     C* Output :
037600940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
037700940224     C*
037800940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
037900940224     C                   MVR                     RESTO             3 0
038000940224     C     RESTO         IFGT      0
038100940224     C                   ADD       1             PAGINE
038200940224     C                   ENDIF
038300940224     C     PAGINE        MULT      SFLPAG        WPAG
038400940224     C*
038500940224     C                   ENDSR
038600940309     C************************************************************
038700940309     C* GESTIONE F03 VIDEO S1
038800940309     C************************************************************
038900940309     C     F03S1         BEGSR
039000940309     C*
039100940309     C                   MOVE      *ON           $FINE
039200940325     C* fine programma
039300940309     C                   ENDSR
039400940309     C/EJECT
039500170124     C************************************************************
039600170124     C* GESTIONE F08 VIDEO S1
039700170124     C************************************************************
039800170124     C     F08S1         BEGSR
039900170124     C*
040000170124     C*  INVERTE
040100170124     c                   eval      $INZS1 = *ON
040200170124     c                   if        Ordina = 0
040300170124     c                   eval      Ordina = 1
040400170124     c                   elseif    Ordina = 1
040500170124     c                   eval      Ordina = 0
040600170124     c                   end
040700170124     C*
040800170124     C                   ENDSR
040900170124     C/EJECT
041000170131     C************************************************************
041100170131     C* GESTIONE F11 VIDEO S1
041200170131     C************************************************************
041300170131     C     F11S1         BEGSR
041400170131     C*
041500170131     C                   CALL      'TRTB71R1'
041600170131     C                   PARM                    KNSIF
041700170131     C                   PARM      *ALL' '       PCLI              8
041800170131     C                   PARM      *ALL'V'       PRAG             25
041900170131     C                   PARM                    KNMUS            10
042000170131     C                   PARM                    C1RSU
042100170131     C*
042200170131     C                   ENDSR
042300170131     C/EJECT
042400940309     C************************************************************
042500940309     C* GESTIONE F10 VIDEO S1
042600940314     c* AGGIUNTA RECORD
042700940309     C************************************************************
042800940309     C     F10S1         BEGSR
042900940309     C*
043000170124     C                   LEAVESR
043100170124     C*
043200030113     C                   RESET                   tabds
043300170123     C                   MOVEL     '1'           xtaopz
043400030113     C                   MOVE      *ZERO         xtaret
043500030113     C                   MOVE      *ZERO         xtaopr
043600030114     C                   MOVEl     *blank        xtakey
043700030113     C                   MOVE      *BLANKS       KPJBU
043800030113     C                   MOVEL     tabds         KPJBU
043900170124$004 C                   CALL      'TRTB82R2'
044000030113     C                   PARM                    KPJBA
044100030114     C                   MOVEL     KPJBU         tabds
044200030113      *
044300940309     C* ritorno da PGM gestione
044400940309     C                   EXSR      GESRET
044500940309     C     WINZS1        IFEQ      *ON
044600940309     C                   MOVE      *ON           $INZS1
044700940309     C* se esistevano già righe sul sfl
044800940309     C* calcolo pagina a cui deve posizionarsi
044900940309     C     WSFL          IFGT      0
045000940309     C                   EXSR      CLCPAG
045100940309     C* altrimenti carico solo la 1a pagina
045200940309     C                   ELSE
045300940309     C                   Z-ADD     SFLPAG        WPAG
045400940309     C                   ENDIF
045500940309     C                   ENDIF
045600940309     C*
045700940309     C                   ENDSR
045800940128     C************************************************************
045900940131     C* CONTROLLO TESTATA LISTA
046000940128     C************************************************************
046100940131     C     CTRC1         BEGSR
046200940128     C*
046300940201     C                   MOVE      *OFF          *IN99
046400940131     C*
046500940202     C                   ENDSR
046600940131     C************************************************************
046700940131     C* CONTROLLO OPZIONI LISTA
046800940131     C************************************************************
046900940131     C     CTRS1         BEGSR
047000940131     C*
047100940202     C                   MOVEL     *OFF          $ESCI
047200940201     C                   SETOFF                                       99
047300940131     C                   Z-ADD     0             S1OPZ
047400040930     c                   clear                   x1cod
047500940131     C*
047600940127     C* Leggo il sfl solo se ci sono rcd
0477009401311    C     WMAX          IFGT      0
047800940607     C                   READC     S1                                     21
047900940127     C*
048000940131     C* esce se fine sfl o errore che richiede l'uscita
0481009401312    C     *IN21         DOWEQ     *OFF
048200940131     C     $ESCI         ANDEQ     *OFF
048300940201     C                   Z-ADD     S1NRR         C1RCD
048400940131     C* ctrl su riga
048500940131     C                   EXSR      RECS1
048600940131     C* gestione opzioni
0487009401313    C     S1OPZ         IFNE      0
048800940201     C     *IN99         ANDEQ     *OFF
048900940131     C                   EXSR      OPZS1
0490009401273-   C                   ENDIF
049100940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0492009402013    C     *IN99         IFEQ      *ON
049300940201     C     $ESCI         OREQ      *ON
049400940131     C                   MOVE      *ON           *IN32
049500940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
049600940204     C* la ripristinerò a conclusione del ciclo di READC
049700940223     C                   MOVE      *OFF          $INZS1
0498009402233-   C                   ENDIF
049900940223     C*
050000940131     C                   Z-ADD     0             S1OPZ
050100940223     C*
050200940607     C                   UPDATE    S1
050300940223     C*
050400940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0505009401313    C     $ESCI         IFEQ      *OFF
050600940607     C                   READC     S1                                     21
050700940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0508009402014    C     *IN21         IFEQ      *ON
050900940201     C                   MOVE      WINZS1        $INZS1
051000940204     C* calcolo pagina a cui deve posizionarsi
051100940224     C                   EXSR      CLCPAG
0512009402014-   C                   ENDIF
0513009402013-   C                   ENDIF
051400940131     C*
0515009401272-   C                   ENDDO
051600940127     C*
0517009401311-   C                   ENDIF
051800940131     C*
051900940127     C                   ENDSR
052000940127     C/EJECT
052100940127     C************************************************************
052200940131     C* CONTROLLO CAMPI I/O RIGA LISTA
052300940127     C************************************************************
052400940131     C     RECS1         BEGSR
052500940131     C*
052600940201     C* reset indicatori DSPATR
052700940201     C                   MOVE      *ALL'0'       IN4049           10
052800940201     C                   MOVEA     IN4049        *IN(40)
052900940201     C*
053000040930     C*  se richiamato x selezionare un codice 11 = *on
053100040930     C*   non deve utilizzare le opzioni di modifica
053200040930     C*  e viceversa
053300040930     c     *in11         ifeq      *on
0534000409303    C     S1OPZ         IFeq      2
0535000409303    C     S1OPZ         OReq      3
0536000409303    C     S1OPZ         OReq      4
053700040930     c                   seton                                        41  99
053800040930     C                   END
053900040930      * selezionato un codice da restituire al chiamante
0540000409303    C     S1OPZ         IFeq      1
054100170124     c                   move      s1chi         x1cod
054200040930     c                   clear                   kpjbu
054300040930     c                   movel     x1cod         kpjbu
054400040930     C                   MOVEL     *ON           $ESCI
054500040930     C                   MOVEL     *ON           $fine
054600040930     C                   END
054700040930     C                   END
054800040930     C*
054900040930     c     *in11         ifeq      *off
0550000409303    C     S1OPZ         ANDeq     1
055100040930     c                   seton                                        41  99
055200040930     C                   END
055300040930     C*
055400940131     C                   ENDSR
055500940131     C************************************************************
055600940131     C* GESTIONE OPZIONI LISTA
055700940131     C************************************************************
055800940131     C     OPZS1         BEGSR
055900940201     C*
0560000409302    C     *IN11         IFEQ      *Off
0561000409302    C     *IN11         oreq      *On
0562001701232    C     s1opz         andeq     5
056300040930     C*
056400170126     c                   if        s1tipo = 'C    '
056500170126     C*
056600170126     c                   clear                   DSTB70
056700170126     C                   MOVEL     s1opz         DS7OPZ
056800170126     C                   MOVEL     s1part        DS7COD
056900170126     C                   MOVEL     DSTB70        KPJBU
057000170126     C                   CALL      'TRTB70R0'
057100170126     C                   PARM                    KPJBA
057200170126     C                   MOVEL     KPJBU         DSTB70
057300170126     C     DS7F03        IFEQ      '1'
057400170126     C                   MOVEL     DS7F03        xtaret
057500170126     C                   END
057600170126     C*
057700170126     c                   elseif    s1tipo = 'G    '
057800170126     C*
057900170126     c                   clear                   DSTB70
058000170126     C                   MOVEL     s1opz         DS7OPZ
058100170126     C                   MOVEL     s1part        DS7COD
058200170126     C                   MOVEL     DSTB70        KPJBU
058300170126     C                   CALL      'TRTB69R0'
058400170126     C                   PARM                    KPJBA
058500170126     C                   MOVEL     KPJBU         DSTB70
058600170126     C     DS7F03        IFEQ      '1'
058700170126     C                   MOVEL     DS7F03        xtaret
058800170126     C                   END
058900170126     C*
059000170126     C*
059100170125     c                   end
059200170125     C*
059300940201     C*
059400940223     C* ritorno da PGM gestione
059500940223     C                   EXSR      GESRET
059600040930     C*
059700940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0598009402252    C     *IN99         IFEQ      *ON
059900940223     C                   SETON                                        40
0600009402252-   C                   ENDIF
060100940225     C*
0602000409302-   C                   ENDIF
060300040930     C*
060400940131     C                   ENDSR
060500940223     C/EJECT
060600940223     C************************************************************
060700940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
060800940223     C************************************************************
060900940223     C     GESRET        BEGSR
061000940223     C*
061100940223     C* modo di ritorno
061200940223     C*
0613009402231    C                   SELECT
061400940314    >C* << questi modi di utilizzo dei valori di ritorno dal
061500940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
061600940314    >C*    proposte, normalmente sempre valide, ma modificabili
061700940314    >C*    per situazioni particolari >>
061800940223     C* 1 = F3
061900030113    >C     xtaret        WHENEQ    '1'
062000940224     C                   MOVE      *ON           $ESCI
062100940223     C                   MOVE      *ON           $FINE
062200940223     C* 2 = F12
062300030113    >C     xtaret        WHENEQ    '2'
062400940223     C                   MOVE      *ON           $ESCI
062500940223     C*
0626009402231-   C                   ENDSL
062700940223     C*
062800940223     C* operazione eseguite dal pgm chiamato
062900940223     C*
0630009402231    C                   SELECT
063100940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
063200030113    >C     xtaopr        WHENEQ    '1'
063300940223     C                   MOVE      *ON           WINZS1
063400940223     C*
0635009402231-   C                   ENDSL
063600940223     C*
063700940223     C* funzione non eseguibile per errore :
063800940223     C*
0639009402231    C                   SELECT
064000940223     C* 1 = funzione richiamata chiusa in errore
064100940316    >C*  eventualmente gestire altri codici di errore
064200030113    >C     xtaerr        WHENEQ    '1'
064300940223     C                   MOVE      *ON           $ESCI
064400940223     C                   SETON                                        5299
064500940223     C*
0646009402231-   C                   ENDSL
064700940223     C*
064800940223     C                   ENDSR
064900940223     C/EJECT
065000940131     C************************************************************
065100940131     C* OPERAZIONI INIZIALI
065200940131     C************************************************************
065300940131     C     *INZSR        BEGSR
065400030113     C*
065500030113     C* Reperimento parametri
065600030113     C     *ENTRY        PLIST
065700030113     C                   PARM                    KPJBA
065800040930     C                   movel     kpjbu         Selez             1
065900170125     C*  Richiamo XPARUT
066000170125     C                   Z-ADD     1             CODUT
066100170125     C                   CALL      'XPARUT'
066200170125     C                   PARM                    UTEDSE
066300040930     C*
066400040930     C* Se chiamato x selezionare un codice
066500040930     c                   clear                   x1cod
066600040930     c                   if        selez = '?'
066700040930     c                   seton                                        11
066800040930     c                   end
066900030113     C*
067000030113     C* Variabili per gestione videate
067100030113     C                   MOVE      *BLANK        $GEST             2
067200030113     C                   MOVE      *BLANK        $FINE             1
067300030113     C                   MOVE      *BLANK        $INZS1            1
067400030113     C                   MOVE      *BLANK        $EFILE            1
067500030113     C                   MOVE      *BLANK        $ESCI             1
067600030113     C                   MOVE      *BLANK        $RCDOK            1
067700030113     C*
067800030113     C     KTBL          KLIST
067900030113     C                   KFLD                    TBLKUT
068000030113     C                   KFLD                    TBLCOD
068100030113     C* Indici
068200030113     C                   Z-ADD     0             X                 3 0
068300030113     C                   Z-ADD     0             Y                 3 0
068400940127     C*
068500940117     C                   ENDSR
068600030113     C************************************************************
