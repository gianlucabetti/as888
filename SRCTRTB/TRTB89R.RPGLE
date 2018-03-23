000100141127     H*
000200141127     H*
000300940211     H DECEDIT('0,') DATEDIT(*DMY.)
000400940224      *
000500940307      *  21           GENERICO OPERAZIONI I/O
000600940224      *  22           GENERICO ERRORE OPERAZIONI I/O
000700940224      *  30           SFLDSP
000800940224      * N31           SFLCLR
000900940224      *  31           SFLDSPCTL
001000940224      *  32           SFLNXTCHG
001100940224      *  33           SFLEND
001200940224      *  39           OF PRTF
001300940224      *  40 <---> 49  DSPATR ERRORI SU SFL
001400940608      *  Specificare l'uso dei singoli indicatori
001500940224      *  50 <---> 98  ERRORI SU VIDEO
001600940608      *  Specificare l'uso dei singoli indicatori
001700940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
001800940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
001900940224      *  99           INDIC. GENERALE DI ERRORE
002000940128     F*----------------------------------------------------*
002100030113     FTABEL00F  IF   E           K DISK
002200070628     FTRTB89D   CF   E             WORKSTN
002300940607     F                                     SFILE(S1:S1NRR)
002400940201     F                                     INFDS(DSFMT)
002500940128     D*----------------------------------------------------*
002600940211     D* Passaggio Parametri
002700940211     D KPJBA         E DS
002800030113      *-------------
002900940325     D* Parametri in ricezione
003000030113     D  TABDS          DS
003100030113     D  XTAOPZ                 1      2
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
005300940607     D* descrizione tabella
005400070627$003 D EXTDSA        E DS                  EXTNAME(DSCA)
005500070627$003 D SavKut          S                   Like(TblKut)
005600070627$003 D SavCod          S                   Like(TblCod)
005700070627$003 D SavKey          S                   Like(TblKey)
005800030113     D*-------------
005900030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
006000030113$003 D S1NRR           S                   Like(C1rcd)
006100030113$003 D WSfl            S                   Like(C1nrr)
006200030113$003 D Wmax            S                   Like(C1rcd)
006300030113$003 D Wpag            S                   Like(C1rcd)
006400030113$003 D Winzs1          S                   Like($inzs1)
006500940207     D*-------------
006600940211     D* COSTANTI
006700940211     D*-------------
006800030113     D SFLPAG          C                   CONST(11)
006900940314     D* dimensione della schiera $MS1
007000940506     D*
007100940506     D* Tasti di funzione
007200940506     D F01             C                   CONST(X'31')
007300940506     D F02             C                   CONST(X'32')
007400940506     D F03             C                   CONST(X'33')
007500940506     D F04             C                   CONST(X'34')
007600940506     D F05             C                   CONST(X'35')
007700940506     D F06             C                   CONST(X'36')
007800940506     D F07             C                   CONST(X'37')
007900940506     D F08             C                   CONST(X'38')
008000940506     D F09             C                   CONST(X'39')
008100940506     D F10             C                   CONST(X'3A')
008200940506     D F11             C                   CONST(X'3B')
008300940506     D F12             C                   CONST(X'3C')
008400940506     D F13             C                   CONST(X'B1')
008500940506     D F14             C                   CONST(X'B2')
008600940506     D F15             C                   CONST(X'B3')
008700940506     D F16             C                   CONST(X'B4')
008800940506     D F17             C                   CONST(X'B5')
008900940506     D F18             C                   CONST(X'B6')
009000940506     D F19             C                   CONST(X'B7')
009100940506     D F20             C                   CONST(X'B8')
009200940506     D F21             C                   CONST(X'B9')
009300940506     D F22             C                   CONST(X'BA')
009400940506     D F23             C                   CONST(X'BB')
009500940506     D F24             C                   CONST(X'BC')
009600940506     D ENTER           C                   CONST(X'F1')
009700940506     D ROLDWN          C                   CONST(X'F4')
009800940506     D ROLLUP          C                   CONST(X'F5')
009900070628      **********************************************************************
010000070628      * Constants
010100070628      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
010200070628      **********************************************************************
010300070628$xxx D Ord_Ord         C                   1
010400070628      * ?Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
010500070628     D MaxKey          C                   2
010600070628     D Ascendente      C                   1
010700070628     D Discendente     C                   2
010800070628     D Carattere       C                   6
010900070628     D Put             C                   1
011000070628     D EndPut          C                   2
011100070628     D Get             C                   3
011200070628     D Numerico        C                   8
011300070628      **********************************************************************
011400070628      * Campi utili:
011500070628      *     RRN1       - Numero relativo di record del Subfile
011600070628      *     SizeList   - Dimensione della lista
011700070628      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
011800070628      **********************************************************************
011900070628     D Rrn1            S              5I 0
012000070628     D NotUsed         S             16A
012100070628     D ReturnSize      S              9B 0
012200070628     D SizeList        S              9B 0
012300070628     D RrnLast         S              5I 0
012400070628      **********************************************************************
012500070628      * Data Structures
012600070628      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
012700070628      *     QLGSCB     - The sort request block for the QLGSORT API
012800070628      *     QLGSCB00   - The sort request block for the QLGSRTIO API
012900070628      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
013000070628      *     QUSEC      - Error structure for the QLGSORT API
013100070628      **********************************************************************
013200070628     D SflRcd          DS
013300070628     D  S1ord
013400070628     D  S1des
013500070628     D  S1cod
013600070628     D  S1stc
013700070628     D  S1opz
013800070628     D  S1ann
013900070628     D  Selected                      1A
014000070628
014100070628      /COPY QSYSINC/QRPGLESRC,QLGSORT
014200070628     D QLGKL                         16    DIM(MaxKey)
014300070628     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
014400070628     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
014500070628     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
014600070628     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
014700070628
014800070628      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
014900070628      /COPY QSYSINC/QRPGLESRC,QUSEC
015000940127     C*----------------------------------------------------*
015100940127     C*                MAIN LINE PROGRAM
015200940127     C*----------------------------------------------------*
015300940223     C* inizializzazione variabili
015400940223     C                   EXSR      INZVAR
015500940223     C*
015600940223     C     $FINE         DOWEQ     *OFF
015700940131     C     $GEST         CASEQ     'S1'          GESS1
015800940117     C                   END
015900940117     C                   END
016000940325     C* fine programma
016100940325     C                   SETON                                            LR
016200030113     C************************************************************
016300030113     C* INIZIALIZZAZIONE VARIABILI
016400030113     C************************************************************
016500030113     C     INZVAR        BEGSR
016600030113     C*
016700030113     C* Pulizia campi e indicatori
016800030113     C                   MOVE      *ALL'0'       IN4049           10
016900030113     C                   MOVEA     IN4049        *IN(40)
017000030113     C                   CLEAR                   S1OPZ
017100030113     C* Variabili per gestione videate
017200030113     C*
017300030113     C                   MOVE      *OFF          $FINE
017400030113     C                   MOVE      *OFF          $INZS1
017500030113     C                   MOVE      *OFF          $EFILE
017600030113     C                   MOVE      *OFF          $ESCI
017700030113     C                   MOVE      *OFF          $RCDOK
017800030113     C                   Z-ADD     0             $ULKS1            3 0
017900030113     C*
018000030113     C                   MOVE      *ON           $INZS1
018100030113     C                   MOVE      'S1'          $GEST
018200030113     C*
018300030113     C* Variabili appoggio
018400030114     C                   Z-ADD     1             WPAG
018500030113     c*
018600030113     C                   ENDSR
018700940127     C************************************************************
018800940131     C* GESTIONE LISTA
018900940127     C************************************************************
019000940127     C     GESS1         BEGSR
019100030113     C*
019200940223     C* inizializzazione videata
019300940223     C     $INZS1        IFEQ      *ON
019400940127     C                   EXSR      INZS1
019500940223     C                   MOVE      *OFF          $INZS1
019600940127     C                   ENDIF
019700030113     C*
019800030113     C* emissione piede videata
019900030113     C                   WRITE     Z1
020000030113     C* Non ci sono records
020100940223     C     WMAX          IFEQ      0
020200940607     C                   WRITE     D1
020300030114     C                   Else
020400030114     C     Wsfl          IFgt      0
020500030114     C                   Z-ADD     wsfl          C1RCD
020600030114     C                   Else
020700030114     C     Wpag          IFgt      0
020800030114     C                   Z-ADD     wpag          C1RCD
020900030114     C                   EndIF
021000030114     C                   EndIF
021100030114     C                   ENDIF
021200940127     C*
021300030113     C*              *------------------*
021400940607     C                   EXFMT     C1
021500030113     C*              *------------------*
021600030113     C*
021700940204     C     C1NRR         IFNE      0
021800940204     C                   Z-ADD     C1NRR         WSFL
021900940204     C                   ENDIF
022000940127     C                   Z-ADD     SFLNRR        C1RCD
022100030113     C* Selezioni
0222009401271    C                   SELECT
022300940127     C* F3=Fine
022400940506     C     $TASTO        WHENEQ    F03
022500940309     C                   EXSR      F03S1
022600940131     C* F10=Immissione
022700940506     C     $TASTO        WHENEQ    F10
022800940309     C                   EXSR      F10S1
0229009401271O   C                   OTHER
023000940127     C* CONTROLLO DATI
023100940131     C                   EXSR      CTRC1
023200940201     C     *IN99         IFEQ      *OFF
023300940131     C                   EXSR      CTRS1
023400940131     C                   END
0235009401271-   C                   ENDSL
023600940127     C*
023700940127     C                   ENDSR
023800940224     C/EJECT
023900940127     C************************************************************
024000940131     C* INIZIALIZZAZIONE LISTA
024100940127     C************************************************************
024200940127     C     INZS1         BEGSR
024300940302     C* pulizia SFL
024400940128     C                   SETOFF                                         3031
024500940607     C                   WRITE     C1
024600940128     C                   SETON                                          31
024700940128     C*
024800030113     C* CARICAMENTO SFL totale
024900940201     C                   Z-ADD     0             S1NRR
025000030113     C                   Z-ADD     1             C1RCD
025100940128     C                   Z-ADD     0             WMAX
025200940224     C*
025300940224     C* Posizionamento su file pilota
025400030113     c     kTBL          setll     TABEL00F
025500940608    >C                   EXSR      REDANA
025600030113     C* Carico il SFL
025700940127     C                   EXSR      ROLS1
025800030113     C*
025900030114     c                   if        xtaopr <> '1'
026000030114     C                   Z-ADD     1             WPAG
026100030114     c                   end
026200940127     C*
026300940127     C                   ENDSR
026400940127     C************************************************************
026500940131     C* CARICAMENTO PAGINA LISTA
026600940127     C************************************************************
026700940127     C     ROLS1         BEGSR
026800940127     C*
026900940128     C                   SETOFF                                       32
027000940223     C                   Z-ADD     0             Y
027100940127     C                   Z-ADD     WMAX          S1NRR
027200940127     C*
027300940127     C* Leggo dal file anagrafico per caricare la lista
0274009401311    C     $EFILE        DOWEQ     *OFF
027500940127     C*
027600030113     c                   clear                   s1opz
027700030113     c                   movel     tblkey        s1cod
027800070628     c                   movel     tbluni        extdsA
027900070628     c                   z-add     §CAORD        s1ord
028000070628     c                   movel     §CAdsc        s1des
028100070628     c                   movel     §CAstc        s1stc
028200070628     c                   setoff                                       66
028300070628     c                   if        s1stc = 'N'
028400070628     c                   seton                                        66
028500070628     c                   move      '1'           s1in66
028600070628     c                   end
028700070625     C* Se Annullato
028800030115     c                   clear                   S1Ann
028900030115     c                   if        tblflg ='*'
029000030115     c                   move      'A'           S1Ann
029100030115     c                   end
029200070627     C                   eval      SavKEy = TBLKEY
029300070627      *
029400940127     C                   ADD       1             S1NRR
029500940127     C                   ADD       1             Y
029600940127     C*
029700940607     C                   WRITE     S1
029800940131     C*
029900940316    >C                   EXSR      REDANA
030000940128     C*
0301009401271-   C                   ENDDO
030200940127     C*
030300940223     C                   Z-ADD     S1NRR         WMAX                 30
030400070628     C                   Z-ADD     S1NRR         RrnLast
030500070628     C*
030600070628     C* se ci sono almeno 2 records da ordinare fra loro
030700070628     c                   if        wmax > 1
030800070628      * ?   Esegue l'Ordinamento
030900070628     c                   exsr      Ordina_SFL
031000070628     c                   end
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
032500070628     C/EJECT
032600070628      * ?-------------------------------------------------------------*?
032700070628      *? Riordina comunque il SFL                                     ?
032800070628      * ?-------------------------------------------------------------*?
032900070628     C     Ordina_SFL    BEGSR
033000070628     C*
033100070628     C*  Totale Records Caricati nel SFL
033200070628     c                   eval      wsfl = 1
033300070628     C*
033400070628      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
033500070628      * presente nel subfile --?"Selected"?-- questo è aggiunto al record.
033600070628      * Il campo è usato per selezionare i records dando un ordine a quelli
033700070628      * selezionati davanti ai non selezionati.
033800070628     C                   CLEAR                   QLGSCB
033900070628     C                   CLEAR                   QLGSCB00
034000070628      *
034100070628     ** 1 campi chiave.   x Ordinamento
034200070628     C                   EVAL      QLGNBRK    = 1
034300070628
034400070628     ** Il GIRO è in posizione (1) 10 Bytes char ascending
034500070628     C                   EVAL      QLGSP      = 1
034600070628     C                   EVAL      QLGSS      = %SIZE(S1ORD)
034700070628     C                   EVAL      QLGDT      = Numerico
034800070628     C                   EVAL      QLGSO      = Ascendente
034900070628     C                   EVAL      QLGKL(1)   = QLGSKL
035000070628      *------------
035100070628      * Load other sort parameters.
035200070628     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
035300070628     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
035400070628     C                   EVAL      QLGRT     = 8
035500070628     C                   EVAL      QLGOKL    = 80
035600070628     C                   EVAL      QLGLKE    = 16
035700070628     C                   EVAL      QLGLSS    = 290
035800070628
035900070628      * Initialize Sort I/O API fields.
036000070628     C                   EVAL      QLGRL00  = QLGRL
036100070628     C                   EVAL      QLGRC00  = 1
036200070628     C                   CLEAR                   QUSEI
036300070628     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
036400070628
036500070628      * First step - Initialize the sort routine.
036600070628     C                   CALL      'QLGSORT'
036700070628     C                   PARM                    QLGSCB
036800070628     C                   PARM                    NotUsed
036900070628     C                   PARM                    NotUsed
037000070628     C                   PARM                    SizeList
037100070628     C                   PARM                    ReturnSize
037200070628     C                   PARM                    QUSEC
037300070628
037400070628      * Next step - Write records to I/O routine.
037500070628     C                   EVAL      QLGRT00 = Put
037600070628
037700070628     C                   DO        RrnLast       S1nrr_xx          5 0
037800070628     C     S1nrr_xx      CHAIN     S1
037900070628
038000070628     ** Solo le righe con Selected = 'Y' sono riordinate,
038100070628     ** quindi per fare un ordinamento di tutte le righe
038200070628     ** metto 'Y' sempre.
038300070628     C                   EVAL      Selected  = 'Y'
038400070628
038500070628     C                   CLEAR                   QUSEI
038600070628     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
038700070628      *
038800070628     C                   CALL      'QLGSRTIO'
038900070628     C                   PARM                    QLGSCB00
039000070628     C                   PARM                    SflRcd
039100070628     C                   PARM                    NotUsed
039200070628     C                   PARM                    SizeList
039300070628     C                   PARM                    NotUsed
039400070628     C                   PARM                    QUSEC
039500070628
039600070628     C                   ENDDO
039700070628
039800070628      * Next step - Signal end of input, clear subfile for reload.
039900070628     C                   EVAL      QLGRT00 = EndPut
040000070628     C                   CLEAR                   QUSEI
040100070628     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
040200070628
040300070628     C                   CALL      'QLGSRTIO'
040400070628     C                   PARM                    QLGSCB00
040500070628     C                   PARM                    SflRcd
040600070628     C                   PARM                    NotUsed
040700070628     C                   PARM                    SizeList
040800070628     C                   PARM                    NotUsed
040900070628     C                   PARM                    QUSEC
041000070628      *
  pulizia SFL  ?
041100070628     C                   SETOFF                                         3031
041200070628     C                   WRITE     C1
041300070628     C                   SETON                                          3031
041400070628     C                   Z-ADD     0             S1NRR
041500070628     C                   Z-ADD     1             C1RCD
041600070628
041700070628      * Final step - Write the records back to the subfile.
041800070628     C                   EVAL      QLGRT00 = Get
041900070628
042000070628     C                   DO        RrnLast       S1nrr_xx
042100070628     C                   CLEAR                   QUSEI
042200070628     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
042300070628     C                   CALL      'QLGSRTIO'
042400070628     C                   PARM                    QLGSCB00
042500070628     C                   PARM                    NotUsed
042600070628     C                   PARM                    SflRcd
042700070628     C                   PARM                    QLGRL00
042800070628     C                   PARM                    NotUsed
042900070628     C                   PARM                    QUSEC
043000070628     c                   setoff                                       66
043100070628     c                   if        s1stc = 'N'
043200070628     c                   seton                                        66
043300070628     c                   move      '1'           s1in66
043400070628     c                   end
043500070628     c                   eval      S1nrr = S1nrr_xx
043600070628     C                   WRITE     S1
043700070628     C                   ENDDO
043800070628      *
043900070628      *  All'uscita di queste chiamate a routine di sistema il SFL record
044000070628      *   si incrementa stranamente quindi lo reimposto correttamente
044100070628      *    prima di lasciare la routine.
044200070628     C                   EVAL      S1nrr = RrnLast
044300070628     C*
044400070628     C                   ENDSR
044500070628     C/EJECT
044600940128     C************************************************************
044700940131     C* LETTURA RCD ARCHIVIO PILOTA
044800940128     C************************************************************
044900940607     C     REDANA        BEGSR
045000940128     C*
045100940131     C                   MOVEL     *OFF          $EFILE
045200940131     C                   MOVEL     *OFF          $RCDOK
045300940131     C*
0454009401311    C     $EFILE        DOUEQ     *ON
045500940131     C     $RCDOK        OREQ      *ON
045600940131     C*
045700030113     c     kTBL          Reade     TABEL00F
045800030113     c                   if        %eof(Tabel00F)
045900030113     C                   MOVEL     *ON           $EFILE
046000030113     C                   MOVE      $EFILE        *IN33
046100030113     c                   else
046200030113     C                   MOVE      *ON           $RCDOK
046300030113     c                   end
046400940131     C*
0465009401311-   C                   ENDDO
046600940131     C*
046700940131     C                   ENDSR
046800940224     C************************************************************
046900940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
047000940224     C************************************************************
047100940224     C     CLCPAG        BEGSR
047200940224     C* Input :
047300940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
047400940224     C*          cursore
047500940224     C* - SFLPAG = numero rcd per pagina sfl
047600940224     C* Output :
047700940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
047800940224     C*
047900940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
048000940224     C                   MVR                     RESTO             3 0
048100940224     C     RESTO         IFGT      0
048200940224     C                   ADD       1             PAGINE
048300940224     C                   ENDIF
048400940224     C     PAGINE        MULT      SFLPAG        WPAG
048500940224     C*
048600940224     C                   ENDSR
048700940309     C************************************************************
048800940309     C* GESTIONE F03 VIDEO S1
048900940309     C************************************************************
049000940309     C     F03S1         BEGSR
049100940309     C*
049200940309     C                   MOVE      *ON           $FINE
049300940325     C* fine programma
049400940309     C                   ENDSR
049500940309     C/EJECT
049600940309     C************************************************************
049700940309     C* GESTIONE F10 VIDEO S1
049800940314     c* AGGIUNTA RECORD
049900940309     C************************************************************
050000940309     C     F10S1         BEGSR
050100940309     C*
050200030113     C                   RESET                   tabds
050300030113     C                   MOVEL     '01'          xtaopz
050400030113     C                   MOVE      *ZERO         xtaret
050500030113     C                   MOVE      *ZERO         xtaopr
050600030114     C                   MOVEl     *blank        xtakey
050700030113     C                   MOVE      *BLANKS       KPJBU
050800030113     C                   MOVEL     tabds         KPJBU
050900070628$004 C                   CALL      'TRTB89R2'
051000030113     C                   PARM                    KPJBA
051100030114     C                   MOVEL     KPJBU         tabds
051200030113      *
051300940309     C* ritorno da PGM gestione
051400940309     C                   EXSR      GESRET
051500940309     C     WINZS1        IFEQ      *ON
051600940309     C                   MOVE      *ON           $INZS1
051700940309     C* se esistevano già righe sul sfl
051800940309     C* calcolo pagina a cui deve posizionarsi
051900940309     C     WSFL          IFGT      0
052000940309     C                   EXSR      CLCPAG
052100940309     C* altrimenti carico solo la 1a pagina
052200940309     C                   ELSE
052300940309     C                   Z-ADD     SFLPAG        WPAG
052400940309     C                   ENDIF
052500940309     C                   ENDIF
052600940309     C*
052700940309     C                   ENDSR
052800940128     C************************************************************
052900940131     C* CONTROLLO TESTATA LISTA
053000940128     C************************************************************
053100940131     C     CTRC1         BEGSR
053200940128     C*
053300940201     C                   MOVE      *OFF          *IN99
053400940131     C*
053500940202     C                   ENDSR
053600940131     C************************************************************
053700940131     C* CONTROLLO OPZIONI LISTA
053800940131     C************************************************************
053900940131     C     CTRS1         BEGSR
054000940131     C*
054100940202     C                   MOVEL     *OFF          $ESCI
054200940201     C                   SETOFF                                       99
054300940131     C                   Z-ADD     0             S1OPZ
054400940131     C*
054500940127     C* Leggo il sfl solo se ci sono rcd
0546009401311    C     WMAX          IFGT      0
054700940607     C                   READC     S1                                     21
054800940127     C*
054900940131     C* esce se fine sfl o errore che richiede l'uscita
0550009401312    C     *IN21         DOWEQ     *OFF
055100940131     C     $ESCI         ANDEQ     *OFF
055200940201     C                   Z-ADD     S1NRR         C1RCD
055300940131     C* ctrl su riga
055400940131     C                   EXSR      RECS1
055500940131     C* gestione opzioni
0556009401313    C     S1OPZ         IFNE      0
055700940201     C     *IN99         ANDEQ     *OFF
055800940131     C                   EXSR      OPZS1
0559009401273-   C                   ENDIF
056000940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0561009402013    C     *IN99         IFEQ      *ON
056200940201     C     $ESCI         OREQ      *ON
056300940131     C                   MOVE      *ON           *IN32
056400940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
056500940204     C* la ripristinerò a conclusione del ciclo di READC
056600940223     C                   MOVE      *OFF          $INZS1
0567009402233-   C                   ENDIF
056800940223     C*
056900940131     C                   Z-ADD     0             S1OPZ
057000070628     c                   setoff                                       66
057100070628     c                   if        s1stc = 'N'
057200070628     c                   seton                                        66
057300070628     c                   move      '1'           s1in66
057400070628     c                   end
057500940223     C*
057600940607     C                   UPDATE    S1
057700940223     C*
057800940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0579009401313    C     $ESCI         IFEQ      *OFF
058000940607     C                   READC     S1                                     21
058100940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0582009402014    C     *IN21         IFEQ      *ON
058300940201     C                   MOVE      WINZS1        $INZS1
058400940204     C* calcolo pagina a cui deve posizionarsi
058500940224     C                   EXSR      CLCPAG
0586009402014-   C                   ENDIF
0587009402013-   C                   ENDIF
058800940131     C*
0589009401272-   C                   ENDDO
059000940127     C*
0591009401311-   C                   ENDIF
059200940131     C*
059300940127     C                   ENDSR
059400940127     C/EJECT
059500940127     C************************************************************
059600940131     C* CONTROLLO CAMPI I/O RIGA LISTA
059700940127     C************************************************************
059800940131     C     RECS1         BEGSR
059900940131     C*
060000940201     C* reset indicatori DSPATR
060100940201     C                   MOVE      *ALL'0'       IN4049           10
060200940201     C                   MOVEA     IN4049        *IN(40)
060300940201     C*
060400940131     C                   ENDSR
060500940131     C************************************************************
060600940131     C* GESTIONE OPZIONI LISTA
060700940131     C************************************************************
060800940131     C     OPZS1         BEGSR
060900940201     C*
061000030113     C                   RESET                   tabds
061100030113     C                   MOVEL     S1OPZ         xtaopz
061200030113     C                   MOVE      *ZERO         xtaret
061300030113     C                   MOVE      *ZERO         xtaopr
061400030113     C                   MOVEl     S1COD         xtakey
061500940715     C                   MOVE      *BLANKS       KPJBU
061600030113     C                   MOVEL     tabds         KPJBU
061700070628$004 C                   CALL      'TRTB89R2'
061800940607     C                   PARM                    KPJBA
061900030114     C                   MOVEL     KPJBU         tabds
062000940201     C*
062100940223     C* ritorno da PGM gestione
062200940223     C                   EXSR      GESRET
062300940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0624009402252    C     *IN99         IFEQ      *ON
062500940223     C                   SETON                                        40
0626009402252-   C                   ENDIF
062700940225     C*
062800940131     C                   ENDSR
062900940223     C/EJECT
063000940223     C************************************************************
063100940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
063200940223     C************************************************************
063300940223     C     GESRET        BEGSR
063400940223     C*
063500940223     C* modo di ritorno
063600940223     C*
0637009402231    C                   SELECT
063800940314    >C* << questi modi di utilizzo dei valori di ritorno dal
063900940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
064000940314    >C*    proposte, normalmente sempre valide, ma modificabili
064100940314    >C*    per situazioni particolari >>
064200940223     C* 1 = F3
064300030113    >C     xtaret        WHENEQ    '1'
064400940224     C                   MOVE      *ON           $ESCI
064500940223     C                   MOVE      *ON           $FINE
064600940223     C* 2 = F12
064700030113    >C     xtaret        WHENEQ    '2'
064800940223     C                   MOVE      *ON           $ESCI
064900940223     C*
0650009402231-   C                   ENDSL
065100940223     C*
065200940223     C* operazione eseguite dal pgm chiamato
065300940223     C*
0654009402231    C                   SELECT
065500940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
065600030113    >C     xtaopr        WHENEQ    '1'
065700940223     C                   MOVE      *ON           WINZS1
065800940223     C*
0659009402231-   C                   ENDSL
066000940223     C*
066100940223     C* funzione non eseguibile per errore :
066200940223     C*
0663009402231    C                   SELECT
066400940223     C* 1 = funzione richiamata chiusa in errore
066500940316    >C*  eventualmente gestire altri codici di errore
066600030113    >C     xtaerr        WHENEQ    '1'
066700940223     C                   MOVE      *ON           $ESCI
066800940223     C                   SETON                                        5299
066900940223     C*
0670009402231-   C                   ENDSL
067100940223     C*
067200940223     C                   ENDSR
067300940223     C/EJECT
067400940131     C************************************************************
067500940131     C* OPERAZIONI INIZIALI
067600940131     C************************************************************
067700940131     C     *INZSR        BEGSR
067800030113     C*
067900030113     C* Reperimento parametri
068000030113     C     *ENTRY        PLIST
068100030113     C                   PARM                    KPJBA
068200030113     C*
068300030113     C* Variabili per gestione videate
068400030113     C                   MOVE      *BLANK        $GEST             2
068500030113     C                   MOVE      *BLANK        $FINE             1
068600030113     C                   MOVE      *BLANK        $INZS1            1
068700030113     C                   MOVE      *BLANK        $EFILE            1
068800030113     C                   MOVE      *BLANK        $ESCI             1
068900030113     C                   MOVE      *BLANK        $RCDOK            1
069000030113     C*
069100030113     C     KTBL          KLIST
069200030113     C                   KFLD                    TBLKUT
069300030113     C                   KFLD                    TBLCOD
069400070627     C*
069500070627     C     KTBL3         KLIST
069600070627     C                   KFLD                    TBLKUT
069700070627     C                   KFLD                    TBLCOD
069800070627     C                   KFLD                    TBLKEY
069900030113     C* Indici
070000030113     C                   Z-ADD     0             X                 3 0
070100030113     C                   Z-ADD     0             Y                 3 0
070200940506     C*
070300940506     C* Reperimento tasti di funzione
070400030113     C                   MOVE      1             tblkut
070500070628     C                   MOVE      'CA'          tblcod
070600940127     C*
070700940117     C                   ENDSR
070800030113     C************************************************************
