000100141127     H*
000200141127     H*
000300970214     H DECEDIT('0,') DATEDIT(*DMY.)
000400940223      *
000500940223      *  02           PROTECT CAMPI CHIAVE SE OPZ=MODIFICA
000600940620      *  03           PROTECT TUTTI I CAMPI
000700940223      *  21           GENERICO OPERAZIONI I/O
000800940223      *  22           GENERICO ERRORE OPERAZIONI I/O
000900940223      *  30           SFLDSP
001000940223      * N31           SFLCLR
001100940128      *  31           SFLDSPCTL
001200940128      *  32           SFLNXTCHG
001300940128      *  33           SFLEND
001400940128      *  39           OF PRTF
001500940315      *  40 <---> 49  DSPATR ERRORI SU SFL
001600940317    > *  Specificare l'uso dei singoli indicatori
001700940315      *  50 <---> 98  ERRORI SU VIDEO
001800940317    > *  Specificare l'uso dei singoli indicatori
001900940510      *  96           ERRORE SPECIALE : ERRORI IN ALTRE VIDEATE
002000940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
002100940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
002200940128      *  99           INDIC. GENERALE DI ERRORE
002300940128     F*----------------------------------------------------*
002400030122$001 FTRTB81D2  CF   E             WORKSTN
002500940201     F                                     INFDS(DSFMT)
002600030113     FTABEL00F  UF A E           K DISK
002700940128     D*----------------------------------------------------*
002800030122$003 D EXTDS         E DS                  EXTNAME(DS8D) INZ
002900030113      *-------------
003000940127     D* Passaggio Parametri
003100940127     D KPJBA         E DS
003200030113      *-------------
003300030113     D* Parametri in ricezione
003400030113     D  TABDS          DS
003500030113     D  XTAOPZ                 1      2
003600030113     D  XTARET                 3      3
003700030113     D  XTAOPR                 4      4
003800030113     D  XTAERR                 5      5
003900030113     D  XTAKEY                 6     13
004000940201     D*-------------
004100940201     D DSFMT           DS
004200940506     D  $TASTO               369    369
004300940201     D  NRG                  370    370
004400940201     D  NCL                  371    371
004500940201     D*-------------
004600940201     D* posizione cursore
004700940201     D CURSOR          DS
004800940223     D  RIRI                   1      2B 0 INZ
004900940201     D  R$$                    2      2
005000940223     D  COCO                   3      4B 0 INZ
005100940201     D  C$$                    4      4
005200940207     D*-------------
005300940207     D* Reperimento nome PGM
005400940207     D STATUS         SDS           333
005500940207     D  DSPGM            *PROC
005600940225     D*-------------
005700940225     D* COSTANTI
005800940225     D*-------------
005900940506     D* Tasti di funzione
006000940506     D F01             C                   CONST(X'31')
006100940506     D F02             C                   CONST(X'32')
006200940506     D F03             C                   CONST(X'33')
006300940506     D F04             C                   CONST(X'34')
006400940506     D F05             C                   CONST(X'35')
006500940506     D F06             C                   CONST(X'36')
006600940506     D F07             C                   CONST(X'37')
006700940506     D F08             C                   CONST(X'38')
006800940506     D F09             C                   CONST(X'39')
006900940506     D F10             C                   CONST(X'3A')
007000940506     D F11             C                   CONST(X'3B')
007100940506     D F12             C                   CONST(X'3C')
007200940506     D F13             C                   CONST(X'B1')
007300940506     D F14             C                   CONST(X'B2')
007400940506     D F15             C                   CONST(X'B3')
007500940506     D F16             C                   CONST(X'B4')
007600940506     D F17             C                   CONST(X'B5')
007700940506     D F18             C                   CONST(X'B6')
007800940506     D F19             C                   CONST(X'B7')
007900940506     D F20             C                   CONST(X'B8')
008000940506     D F21             C                   CONST(X'B9')
008100940506     D F22             C                   CONST(X'BA')
008200940506     D F23             C                   CONST(X'BB')
008300940506     D F24             C                   CONST(X'BC')
008400940506     D ENTER           C                   CONST(X'F1')
008500940506     D ROLDWN          C                   CONST(X'F4')
008600940506     D ROLLUP          C                   CONST(X'F5')
008700940506     D*-------------
008800941108     D DATA            C                   CONST('0001-01-01')
008900030114     D Annull          C                   CONST('  ANNULLATO  ')
009000030114     D OPz_Imm         C                   CONST('IMMISSIONE')
009100030114     D OPz_Var         C                   CONST('VARIAZIONE')
009200030114     D OPz_Del         C                   CONST(' ANNULLA  ')
009300030114     D OPz_Vis         C                   CONST('VISUALIZZA')
009400030204      *
009500030204     D digits          C                   CONST('0123456789')
009600940127     C*----------------------------------------------------*
009700940127     C*                MAIN LINE PROGRAM
009800940127     C*----------------------------------------------------*
009900940223     C*
010000940223     C     $FINE         DOWEQ     *OFF
010100940202     C     $GEST         CASEQ     'D1'          GESD1
010200940117     C                   END
010300940117     C                   END
010400940325     C* fine programma
010500030113     c                   movel     tabds         kpjbu
010600030113     C                   Seton                                        LR
010700940131     C************************************************************
010800940510     C* GESTIONE VIDEO RECORD D1
010900940131     C************************************************************
011000940127     C     GESD1         BEGSR
011100030113      *
011200940225     C* inizializzazione videata
011300940223     C     $INZD1        IFEQ      *ON
011400940127     C                   EXSR      INZD1
011500940223     C                   MOVE      *OFF          $INZD1
011600940117     C                   END
011700030113      *
011800940225     C* emissione piede videata se proveniente da altra
011900940225     C* salvataggio valore formato di provenienza
012000940207     C     $LASTV        IFNE      'D1'
012100940614     C                   WRITE     T1
012200940202     C                   MOVE      $LASTV        $LASTG
012300940202     C                   MOVE      'D1'          $LASTV
012400940202     C                   END
012500940131     C*
012600940203     C                   SELECT
012700940203     C* Annullamento
012800030114     C* Visualizzazione
012900030114    >C     xtaopz        WHENEQ    '01'
013000030114     c                   movel     Opz_Imm       D1Fun
013100030114    >C     xtaopz        WHENEQ    '05'
013200030114     c                   movel     Opz_Vis       D1Fun
013300030114     C                   SETON                                        020306
013400030114    >C     xtaopz        WHENEQ    '04'
013500030114     c                   movel     Opz_Del       D1Fun
013600030114     C                   SETON                                        0203
013700030114     C                   SETOFF                                         06
013800940620     C* Manutenzione
013900030113    >C     xtaopz        WHENEQ    '02'
014000030114     c                   movel     Opz_Var       D1Fun
014100030114     C                   SETON                                        02
014200030114     C                   SETOFF                                       0306
014300940620     C*
014400940620     C                   OTHER
014500940620     C*
014600030114     C                   SETOFF                                       020306
014700940203     C                   ENDSL
014800030114     C*
014900030114     c                   movel     D1Fun         D2Fun
015000030113     C*
015100030114     c                   SETOFF                                         99
015200940620     C* Immissione/Modifica/Copia
015300030113     C*               *----------------*
015400940620     C                   EXFMT     D1
015500030113     C*               *----------------*
015600030114     C*
015700030114     C     *IN99         IFEQ      *OFF
015800030114     C*
0159000301141    C                   SELECT
016000030114     C* F3=Fine
016100030114     C     $TASTO        WHENEQ    F03
016200030114     C                   EXSR      F03D1
016300030114     C* F12=Ritorno
016400030114     C     $TASTO        WHENEQ    F12
016500030114     C                   EXSR      F12D1
016600030114     C*
0167000301141O   C                   OTHER
016800030114     C*
016900030114     C                   EXSR      CTRD1
017000030114     C* Video 2
017100940506     C     *IN99         IFEQ      *OFF
017200030122     c                   goto      no_Vid2
017300030114     C*               *----------------*
017400030122     C                   EXFMT     D2
017500030114     C*               *----------------*
017600030122     c     no_Vid2       tag
017700940131     C*
0178009402031    C                   SELECT
017900940131     C* F3=Fine
018000940506     C     $TASTO        WHENEQ    F03
018100940309     C                   EXSR      F03D1
018200940131     C* F12=Ritorno
018300940506     C     $TASTO        WHENEQ    F12
018400940309     C                   EXSR      F12D1
018500940131     C*
0186009402031O   C                   OTHER
018700030113     C*
018800030114     C                   EXSR      CTRD2
018900030113     C*
019000940616     C     *IN99         IFEQ      *OFF
019100940616     C* F6
0192000301142    C     $TASTO        andeq     F06
0193000301152    C     $TASTO        oreq      F23
019400030204     C* A chi Trasmette la Tabella ?
019500030204     c                   Exsr      ACHI
019600940224     C* eseguo aggiornamento
019700940315    >C                   EXSR      AGGANA
019800940224     C* eseguo operazioni del dopo-aggiornamento
019900030114     C  N99              EXSR      GESAGG
020000030114     C*
020100030114     C                   ENDIF
020200030114     C*
0203000301142-   C                   ENDSL
020400030114     C*
0205000301142-   C                   ENDIF
020600940224     C*
0207000301142-   C                   ENDSL
020800030114     C*
0209000301142-   C                   END
021000030114     C*
021100940117     C                   ENDSR
021200940117      ************************************************************
021300940207      * INIZIALIZZAZIONE VIDEATA DATI
021400940117      ************************************************************
021500940127     C     INZD1         BEGSR
021600940131     C*
021700940615     C                   CLEAR                   D1
021800030113     C                   CLEAR                   D2
021900030113     C                   MOVEl     xtakey        D1COD
022000030113     C                   MOVEl     xtakey        D2COD
022100030113     C                   MOVEl     xtakey        TBLkey
022200940207     C*
022300940224     C* valorizzo la chiave univoca per reperire i dati
022400940224     C* se non immissione
0225000301141   >C     xtaopz        IFNE      '01'
022600940315    >C*-------------------------------------------------------------
022700940315    >C* impostare i campi chiave ricevuti
022800940315    >C*-------------------------------------------------------------
022900940224     C* se Modifica/Annullamento, il rcd che leggo viene bloccato
0230000301132   >C     xtaopz        IFEQ      '02'
023100030113    >C     xtaopz        OREQ      '04'
023200030113     C     KTBL          chain     TABEL00F
023300940224     C* se Copia/Visualizzazione, il rcd che leggo NON viene bloccato
0234009406142E   C                   ELSE
023500030113     C     KTBL          chain(N)  TABEL00F
0236009406142-   C                   ENDIF
023700940614     C*
023800030113     C                   IF        %Found(Tabel00F)
023900030204     C* se ci sono campi numerici con dei blanks
024000030204     c                   exsr      CHK_Fld_Num
024100030204     C*
024200030113     C                   MOVEL     TBLuni        EXTDS
024300030114     c                   if        TblFLG = '*'
024400030115     c                   seton                                        08
024500030114     C                   MOVEl     Annull        D1Ann
024600030114     C                   MOVEl     D1Ann         D2Ann
024700030114     C                   end
024800030113     C*
024900940224     C* riempio la videata
025000940224     C                   EXSR      RIED1
025100030113      *
025200940224     C* richiamo routine dei ctrl per decodificarla
025300940614    >C                   EXSR      CTRD1
025400030113      *
025500960228     C                   MOVEL     *IN99         SAV99             1
025600030113      *
025700960228     C* Se la gestione è più videate
025800960305    >C                   EXSR      CTRD2
025900030113      *
026000960305    >C     *IN99         IFEQ      *ON
026100960305    >C                   MOVEL     *IN99         SAV99             1
026200960305    >C                   ENDIF
026300030113      *
026400940224     C                   MOVE      *ALL'0'       IN5098
026500940224     C                   MOVEA     IN5098        *IN(50)
026600030113      *
026700940224     C* definizione indicatori su video
026800940224     C                   SETOFF                                       02
0269009402243    C                   SELECT
027000030114    >C     xtaopz        WHENEQ    '02'
027100940224     C                   SETON                                        02
027200030114    >C     xtaopz        WHENEQ    '05'
0273009402243-   C                   ENDSL
027400940224     C*
0275009402242-   C                   ENDIF
027600030114     C*
0277000301141   >C                   Else
027800030114     C* Se in Immissione nuovo record
027900030114     c                   exsr      NewD1
028000940223     C*
0281009402241-   C                   ENDIF
028200940224     C*
028300940117     C                   ENDSR
028400940207     C/EJECT
028500940207     C************************************************************
028600940510     C* RIEMPIMENTO VIDEATA  D1
028700940207     C************************************************************
028800030114     C     NEWD1         BEGSR
028900940207     C*
029000030114     c                   clear                   d1
029100030114     c                   clear                   d2
029200030114     C*
029300030114     C                   ENDSR
029400030114     C************************************************************
029500030204     C* Controllo su campi numeirici con caratteri alfa
029600030114     C************************************************************
029700030204     C     CHK_Fld_num   BEGSR
029800030204     C********
029900030204     c********           clear                   fldchk            2
030000030204     c********           eval      fldchk = %Subst(Tbluni:27:2)
030100030204     C*****Digits        Check     Fldchk:1
030200030204     C********           If        %Found
030300030204     c********           eval      %Subst(Tbluni:27:2) = '00'
030400030204     c********           end
030500030204     c********           clear                   fldchk
030600030204     c********           eval      fldchk = %Subst(Tbluni:29:2)
030700030204     C*****Digits        Check     Fldchk:1
030800030204     C********           If        %Found
030900030204     c********           eval      %Subst(Tbluni:29:2) = '00'
031000030204     c********           end
031100030204     c********           clear                   fldchk
031200030204     c********           eval      fldchk = %Subst(Tbluni:31:2)
031300030204     C*****Digits        Check     Fldchk:1
031400030204     C********           If        %Found
031500030204     c********           eval      %Subst(Tbluni:31:2) = '00'
031600030204     c********           end
031700030204     C********
031800030204     C                   ENDSR
031900030204     C************************************************************
032000030204     C* RIEMPIMENTO VIDEATA  D1
032100030204     C************************************************************
032200030204     C     RIED1         BEGSR
032300030204     C*
032400030113     C* descrizione
032500030122     c                   eval      d1DES = §8DDES
032600030122      * Partenza/Arrivo
032700030122     c                   eval      d1PA  = §8DPA
032800940207     C*
032900940207     C                   ENDSR
033000940309     C************************************************************
033100940309     C* GESTIONE F03 VIDEO D1
033200940309     C************************************************************
033300940309     C     F03D1         BEGSR
033400940309     C*
033500940309     C                   MOVE      *ON           $FINE
033600030113    >C                   MOVE      '1'           xtaret
033700940325     C*
033800940325     C                   ENDSR
033900940309     C************************************************************
034000940309     C* GESTIONE F12 VIDEO D1
034100940309     C************************************************************
034200940309     C     F12D1         BEGSR
034300940309     C*
034400940309     C                   MOVE      *ON           $FINE
034500940309     C*
034600940309     C                   ENDSR
034700940131     C************************************************************
034800940207     C* CONTROLLO VIDEATA
034900940131     C************************************************************
035000940127     C     CTRD1         BEGSR
035100940131     C*
035200940127     C                   SETOFF                                       99
035300940127     C* non imposto pos. del cursore
035400940127     C                   Z-ADD     0             H1RIGA
035500940127     C                   Z-ADD     0             H1COLO
035600940621     C*
035700940223     C* CTRL DA NON EFFETTUARE SE ANNULLAMENTO
0358000301141   >C     xtaopz        IFNE      '04'
035900971110     C*
036000971110     C* controllo codice vuoto
036100971110$017 C     D1COD         IFEQ      *ALL' '
036200971110     C                   SETON                                        5099
036300030114     C                   Else
036400030114     c                   eval      d2cod = d1cod
036500971110     C                   ENDIF
036600030114     C*
036700030114     C* controllo codice vuoto
036800030114$017 C     D1DES         IFEQ      *ALL' '
036900030114     C                   SETON                                        5199
037000030114     C                   Else
037100030114     c                   eval      d2des = d1des
037200030114     C                   ENDIF
037300940207     C*
037400940224     C* CTRL INESISTENZA RCD SE OPZ=IMMISSIONE/COPIA
037500030114    >C     xtaopz        IFEQ      '01'
037600030114    >C     xtaopz        oreq      '03'
037700030114     c                   movel(p)  d1cod         tblkey
037800030114     C     KTBL          chain(N)  TABEL00F
037900030114     c                   if        %Found(TABEL00F)
038000030114     C                   SETON                                        5299
038100030114     C                   EndIF
038200030114     C                   ENDIF
038300030122     C*
038400030122     C* controllo codice vuoto
038500030122$017 C     D1PA          IFEQ      *ALL' '
038600030122     C                   SETON                                        5399
038700030122     C                   End
038800030114      *
0389009402241-   C                   END
039000940131     C*
039100940302     C     *IN99         IFEQ      *ON
039200940510     C     $GEST         ANDEQ     'D1'
039300940117     C                   MOVEA     *IN           WIN
039400940128     C                   MOVE      *ALL'0'       IN5098           49
039500940128     C                   MOVEA     IN5098        *IN(50)
039600940614     C                   WRITE     D1
039700940117     C                   MOVEA     WIN           *IN
039800940302     C                   ENDIF
039900940127     C*
040000940117     C                   ENDSR
040100960305     C/EJECT
040200960305     C************************************************************
040300960305     C* CONTROLLO VIDEATA D2
040400960305     C************************************************************
040500960305     C     CTRD2         BEGSR
040600960305     C*
040700960305     C                   ENDSR
040800030204     C/EJECT
040900030204     C************************************************************
041000030204     C* A chi deve trasmettere la Tabella
041100030204     C************************************************************
041200030204     C     ACHI          BEGSR
041300030204     C*
041400030204     c                   move      '1'           t1trs
041500030204     c                   clear                   t1fil
041600030204     c     su_win        tag
041700030204     C                   Exfmt     W1Tras
041800030204     c                   if        t1trs = '2' and
041900030204     c                             t1fil = 0
042000030204     c                   seton                                        77
042100030204     c                   goto      su_win
042200030204     c                   end
042300030204     C*
042400030204     C                   ENDSR
042500030204     C/EJECT
042600940224     C************************************************************
042700940315    >C* AGGIORNAMENTO ANAGRAFICA
042800940224     C************************************************************
042900940315    >C     AGGANA        BEGSR
043000971110     C*
043100030114$007 C                   MOVE      '1'           tblKUT
043200030122$007 C                   MOVE      '8D'          tblCOD
043300030114     C                   movel     *blanks       tblKEY
043400030114     C                   MOVEl     D1COD         tblKEY
043500030204     c                   movel     T1trs         tblFTT
043600030204     c                   move      T1fil         tblFLT
043700030114     c                   move      *zeros        tblDTR
043800030114     c                   move      *blanks       tblFTR
043900030114     c                   move      ' '           TBLFLG
044000940207     C*
044100940207     C                   SELECT
044200030114    >C     xtaopz        WHENEQ    '01'
044300030114    >C     xtaopz        OREQ      '03'
044400940315    >C                   EXSR      RIEANA
044500030114     c                   write     TABEL                                59
044600940318     C* se rcd non scrivibile attivo errore
044700030114    >C   59              SETON                                          99
044800030114      *
044900030114    >C     xtaopz        WHENEQ    '02'
045000940622    >C                   EXSR      RIEANA
045100030114     c                   update    TABEL                                59
045200030114    >C   59              SETON                                          99
045300030114    >C     xtaopz        WHENEQ    '04'
045400030114     c                   move      '*'           TBLFLG
045500030115     c                   if        $tasto = F23
045600030115     c                   delete    TABEL                                59
045700030115     c                   else
045800030114     c                   update    TABEL                                59
045900030115     c                   end
046000030114    >C   59              SETON                                          99
046100940621     C*
046200940207     C                   ENDSL
046300940203     C*
046400940203     C                   ENDSR
046500940224     C************************************************************
046600940224     C* RIEMPIMENTO FILE IN AGGIORNAMENTO
046700940224     C************************************************************
046800940616     C     RIEANA        BEGSR
046900030114     C*
047000030114     C                   CLEAR                   tblUNI
047100030114     C* descrizione
047200030122     c                   movel     d1DES         §8DDES
047300030122      * Partenza Arrivo
047400030122     c                   movel     d1PA          §8DPA
047500030114     C*
047600030114     C                   MOVEL     EXTDS         TBLUNI
047700030114     C*
047800940207     C                   ENDSR
047900940224     C************************************************************
048000940224     C* GESTIONE OPERAZIONI DOPO AGGIORNAMENTO
048100940224     C************************************************************
048200940224     C     GESAGG        BEGSR
048300940224     C*
048400940224     C* segnalo al pgm chiamante l'avvenuta conferma
048500030114    >C                   MOVE      *ON           xtaopr
048600940224     C* segnalo al pgm chiamante l'aver premuto l'F6
048700030114    >C                   MOVE      '0'           xtaret
048800940224     C*
0489009402241    C                   SELECT
049000940224     C* nel caso di immissione, ripristino la videata iniziale
049100030114    >C     xtaopz        WHENEQ    '01'
049200940224     C                   MOVE      *ON           $INZD1
049300030114     C                   MOVE      *ON           $FINE
049400940224     C* altrimenti ritorno al pgm chiamante
049500940224     C                   OTHER
049600940224     C                   MOVE      *ON           $FINE
0497009402241-   C                   ENDSL
049800940224     C*
049900940224     C                   ENDSR
050000940131     C************************************************************
050100940131     C* OPERAZIONI INIZIALI
050200940131     C************************************************************
050300940131     C     *INZSR        BEGSR
050400940131     C*
050500940127     C* Reperimento parametri
050600940127     C*
050700940117     C     *ENTRY        PLIST
050800940117     C                   PARM                    KPJBA
050900030113     C                   movel     kpjbu         tabds
051000030115     c                   setoff                                       08
051100030115     C*
051200940223     C* Variabili per gestione videate
051300940223     C                   MOVE      'D1'          $GEST
051400940223     C                   MOVE      *OFF          $FINE
051500940223     C                   MOVE      *ON           $INZD1
051600940224     C                   MOVE      *BLANK        $LASTG
051700940224     C                   MOVE      *BLANK        $LASTV
051800940506     C                   Z-ADD     0             $ULKD1            3 0
051900940127     C*
052000940223     C* Variabili appoggio
052100940224     C                   Z-ADD     0             CURR
052200940224     C                   Z-ADD     0             CURC
052300940207     C*
052400940207     C* Valorizzazione campi univoci testate
052500940614     C                   CLEAR                   T1
052600940614     C                   MOVEL     KNSIF         NOMSIF
052700030113     C******             MOVEL     XSCDSI        NOMDIT
052800941027     C                   MOVEL     DSPGM         NOMPGM
052900030113     C*
053000030113     C                   z-add     1             TBLkut
053100030122     C                   move      '8D'          TBLcod
053200030114     C                   movel     xtakey        TBLkey
053300940224     C*
053400940224     C* Inizializzazione/Decodifica prima videata
053500940224     C                   EXSR      INZD1
053600940224     C                   MOVE      *OFF          $INZD1
053700940127     C*
053800940117     C                   ENDSR
053900940128     C************************************************************
054000940223     C* DEFINIZIONE CAMPI : KLIST,VARIABILI,CONTATORI,INDICI...
054100940223     C************************************************************
054200940223     C     DEFCAM        BEGSR
054300940223     C*
054400940223     C* klist
054500030113     C     KTBL          KLIST
054600030113     C                   KFLD                    TBLkut
054700030113     C                   KFLD                    TBlCOD
054800030113     C                   KFLD                    TBlKEY
054900940207     C*
055000940223     C* Variabili per gestione videate
055100940223     C*
055200940223     C                   MOVE      *BLANK        $GEST             2
055300940223     C                   MOVE      *BLANK        $FINE             1
055400940223     C                   MOVE      *BLANK        $INZD1            1
055500940223     C                   MOVE      *BLANK        $LASTG            2
055600940223     C                   MOVE      *BLANK        $LASTV            2
055700940223     C*
055800940223     C* Variabili appoggio
055900940223     C                   Z-ADD     0             CURR              2 0
056000940223     C                   Z-ADD     0             CURC              2 0
056100940223     C                   MOVE      *ZEROS        WIN              99
056200940224     C                   MOVE      *BLANK        WTBERR            1
056300940223     C*
056400940223     C* Indici
056500940223     C                   Z-ADD     0             X                 3 0
056600940223     C                   Z-ADD     0             Y                 3 0
056700940127     C*
056800940117     C                   ENDSR
056900940128     C************************************************************
