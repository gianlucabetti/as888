000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200940224      *
000300040930      *  11           x selezione di un codice da ripassare al pgm chiamante
000400940307      *  21           GENERICO OPERAZIONI I/O
000500940224      *  22           GENERICO ERRORE OPERAZIONI I/O
000600161104      *  25           Alta intensità la data Scadenza in vigore
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
002100110926     Ftntbe01L  IF   E           K DISK
002200161103     F                                     INFDS(TBEDS)
002300161028     fTNTBE02L  if   e           k disk    rename(TNTBE000:TNTBE2)
002400161028     f                                     prefix(T2_)
002500161028      *
002600110926     Fazorg01L  IF   E           K DISK
002700161028     FTntba04d2 CF   E             WORKSTN
002800940607     F                                     SFILE(S1:S1NRR)
002900940201     F                                     INFDS(DSFMT)
003000940128     D*----------------------------------------------------*
003100940211     D* Passaggio Parametri
003200940211     D KPJBA         E DS
003300030113      *-------------
003400940325     D* Parametri in ricezione
003500030113     D  TABDS          DS
003600030113     D  XTAOPZ                 1      2
003700030113     D  XTARET                 3      3
003800030113     D  XTAOPR                 4      4
003900030113     D  XTAERR                 5      5
004000161028     D  XTAKE1                 6     20
004100161028     D  XTAKE2                21     35
004200161103     D  XTAtberec             36     44
004300161104      *
004400161104     d TIBS34ds      e ds                  inz
004500161104     d dDatiUte      e ds                  inz
004600161104     d AZUTEds       e ds                  extname(AZUTE00F) inz
004700161104      *
004800940211     D*-------------
004900940211     D DSFMT           DS           512
005000940506     D  $TASTO               369    369
005100940211     D  NRG                  370    370
005200940211     D  NCL                  371    371
005300940211     D  SFLNRR               378    379B 0
005400161103     D*-------------
005500161103     D TBEds           DS
005600161103     d  TBE_rec              397    400b 0
005700940207     D*-------------
005800940207     D* Nome PGM a video
005900940207     D                 DS
006000940207     D  PROGR                  1     10
006100940207     D  ASTER1                 1      1    INZ('*')
006200940207     D  SIGLA                  2      9
006300940207     D  ASTER2                10     10    INZ('*')
006400940127     D*-------------
006500940127     D* Reperimento nome PGM
006600940127     D STATUS         SDS           333
006700940127     D  DSPGM            *PROC
006800161028     D* descrizione chiavi
006900161028      *
007000161031      *---------------------------------------------------------------*
007100161031     d wDate           s               d   datfmt(*iso)  inz(*loval)
007200161031     D DATA_eur        S               D   DATFMT(*eur)
007300161031     D DATA_amg        S               D   DATFMT(*iso)
007400161031     D DATA_iso        S               D   DATFMT(*iso)
007500161031      *
007600161028     d dschiave        ds            15
007700161028     d  dskfil                 1      3
007800161028     d  dskasp                 4      4
007900161028     d  dskter                 5      5
008000161028     d  dskmas                 6      6
008100161028     d  dskaat                 7      7
008200161028     d  dskall                 8      8
008300161028      *
008400161028     D tibs02ds      E DS
008500161028      *
008600161028      * Tabella
008700161028     d dAIG          e ds                  inz
008800161028     D dTAI          E DS
008900161028     D dATS          E DS
009000161028     D dMAS          E DS
009100161028     D dAAT          E DS
009200161028     D dALL          E DS
009300030113     D*-------------
009400030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
009500030113$003 D S1NRR           S                   Like(C1rcd)
009600030113$003 D WSfl            S                   Like(C1nrr)
009700030113$003 D Wmax            S                   Like(C1rcd)
009800030113$003 D Wpag            S                   Like(C1rcd)
009900030113$003 D Winzs1          S                   Like($inzs1)
010000161028$003 D x1cod           S                   Like(h1cod)
010100160412$003 D x1cod2          S                   Like(h1cod2)
010200161028     D KCOD            S                   LIKE(TBECOD) inz('AIG')
010300110926     D KKE1            S                   LIKE(TBEKE1)
010400160412     D KKE2            S                   LIKE(TBEKE2)
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
013900940207     I*-------------
014000940607     IS1
014100161031$015 I              S1DES1                      deskey1
014200161104$015 I              S1SCAD                      datscad
014300161031$015 I              S1DES2                      deskey2
014400170517$015 I              S1DES3                      deskey3
014500940127     C*----------------------------------------------------*
014600940127     C*                MAIN LINE PROGRAM
014700940127     C*----------------------------------------------------*
014800940223     C* inizializzazione variabili
014900940223     C                   EXSR      INZVAR
015000940223     C*
015100940223     C     $FINE         DOWEQ     *OFF
015200940131     C     $GEST         CASEQ     'S1'          GESS1
015300940117     C                   END
015400940117     C                   END
015500940325     C* fine programma
015600940325     C                   SETON                                            LR
015700030113     C************************************************************
015800030113     C* INIZIALIZZAZIONE VARIABILI
015900030113     C************************************************************
016000030113     C     INZVAR        BEGSR
016100030113     C*
016200030113     C* Pulizia campi e indicatori
016300030113     C                   MOVE      *ALL'0'       IN4049           10
016400030113     C                   MOVEA     IN4049        *IN(40)
016500030113     C                   CLEAR                   S1OPZ
016600030113     C* Variabili per gestione videate
016700030113     C*
016800030113     C                   MOVE      *OFF          $FINE
016900030113     C                   MOVE      *OFF          $INZS1
017000030113     C                   MOVE      *OFF          $EFILE
017100030113     C                   MOVE      *OFF          $ESCI
017200030113     C                   MOVE      *OFF          $RCDOK
017300030113     C                   Z-ADD     0             $ULKS1            3 0
017400030113     C*
017500030113     C                   MOVE      *ON           $INZS1
017600030113     C                   MOVE      'S1'          $GEST
017700030113     C*
017800030113     C* Variabili appoggio
017900030114     C                   Z-ADD     1             WPAG
018000030113     c*
018100030113     C                   ENDSR
018200940127     C************************************************************
018300940131     C* GESTIONE LISTA
018400940127     C************************************************************
018500940127     C     GESS1         BEGSR
018600030113     C*
018700940223     C* inizializzazione videata
018800940223     C     $INZS1        IFEQ      *ON
018900940127     C                   EXSR      INZS1
019000940223     C                   MOVE      *OFF          $INZS1
019100940127     C                   ENDIF
019200030113     C*
019300030113     C* emissione piede videata
019400030113     C                   WRITE     Z1
019500030113     C* Non ci sono records
019600940223     C     WMAX          IFEQ      0
019700940607     C                   WRITE     D1
019800030114     C                   Else
019900120412     C     Wsfl          IFgt      Wmax
020000120412     c                   z-add     wmax          Wsfl
020100120412     c                   end
020200030114     C     Wsfl          IFgt      0
020300030114     C                   Z-ADD     wsfl          C1RCD
020400030114     C                   Else
020500030114     C     Wpag          IFgt      0
020600030114     C                   Z-ADD     wpag          C1RCD
020700030114     C                   EndIF
020800030114     C                   EndIF
020900030114     C                   ENDIF
021000940127     C*
021100030113     C*              *------------------*
021200940607     C                   EXFMT     C1
021300030113     C*              *------------------*
021400030113     C*
021500940204     C     C1NRR         IFNE      0
021600940204     C                   Z-ADD     C1NRR         WSFL
021700940204     C                   ENDIF
021800940127     C                   Z-ADD     SFLNRR        C1RCD
021900030113     C* Selezioni
0220009401271    C                   SELECT
022100940127     C* F3=Fine
022200161028     C     $TASTO        WHENEQ    F03
022300161028     C                   EXSR      F03S1
022400161028     C* F12=Ritorno
022500161028     C     $TASTO        WHENEQ    F12
022600161028     C                   EXSR      F12S1
022700940131     C* F10=Immissione
022800940506     C     $TASTO        WHENEQ    F10
022900940309     C                   EXSR      F10S1
0230009401271O   C                   OTHER
023100940127     C* CONTROLLO DATI
023200940131     C                   EXSR      CTRC1
023300940201     C     *IN99         IFEQ      *OFF
023400940131     C                   EXSR      CTRS1
023500940131     C                   END
0236009401271-   C                   ENDSL
023700940127     C*
023800940127     C                   ENDSR
023900940224     C/EJECT
024000940127     C************************************************************
024100940131     C* INIZIALIZZAZIONE LISTA
024200940127     C************************************************************
024300940127     C     INZS1         BEGSR
024400940302     C* pulizia SFL
024500940128     C                   SETOFF                                         3031
024600940607     C                   WRITE     C1
024700940128     C                   SETON                                          31
024800161028      **  filiale
024900161028     c                   if        filiale ='999'
025000161028     c                   movel(p)  'Azienda'     c1fild
025100161028     c                   movel(p)  '999'         c1fil
025200161028     c                   else
025300161028     c                   clear                   c1fild
025400161028     c                   movel(p)  filiale       c1fil
025500161028     c     c1fil         chain     azorg01l
025600161028     c                   if        %found(azorg01l)
025700161028     c                   movel     orgdes        c1fild
025800161028     c                   endif
025900161028     c                   endif
026000161104     c                   eval      rsutDES = rsut
026100161028      *
026200030113     C* CARICAMENTO SFL totale
026300940201     C                   Z-ADD     0             S1NRR
026400030113     C                   Z-ADD     1             C1RCD
026500940128     C                   Z-ADD     0             WMAX
026600120412     C                   Z-ADD     1             WPAG
026700161104     C                   CLEAR                   KKE1
026800940224     C*
026900940224     C* Posizionamento su file pilota
027000110926     c     kTBL          setll     tntbe01l
027100940608    >C                   EXSR      REDANA
027200030113     C* Carico il SFL
027300940127     C                   EXSR      ROLS1
027400030113     C*
027500030114     c                   if        xtaopr <> '1'
027600030114     C                   Z-ADD     1             WPAG
027700030114     c                   end
027800940127     C*
027900940127     C                   ENDSR
028000940127     C************************************************************
028100940131     C* CARICAMENTO PAGINA LISTA
028200940127     C************************************************************
028300940127     C     ROLS1         BEGSR
028400940127     C*
028500940128     C                   SETOFF                                       32
028600940223     C                   Z-ADD     0             Y
028700940127     C                   Z-ADD     WMAX          S1NRR
028800940127     C*
028900940127     C* Leggo dal file anagrafico per caricare la lista
0290009401311    C     $EFILE        DOWEQ     *OFF
029100940127     C*
029200030113     c                   clear                   s1opz
029300161031     c                   clear                   deskey1
029400161104     c                   clear                   datscad
029500161031     c                   clear                   deskey2
029600170517     c                   clear                   deskey3
029700120412     c                   clear                   Obbiettivo        3
029800161028     c                   clear                   h1cod
029900160412     c                   clear                   h1cod2
030000161028     c                   movel     tbeke1        h1cod
030100160412     c                   movel     tbeke2        h1cod2
030200120412      **
030300161028     c                   movel     tbeke1        dschiave
030400161028      **
030500161028      **  decodifica i singoli flags
030600161028      *
030700161028      **  Tipo aspettativa
030800161028     C                   CLEAR                   tibs02ds
030900161028     C                   MOVEL     KNSIF         T02SIF
031000161028     C                   MOVEL     'TAI'         T02COD
031100161028     C                   MOVEL     'C'           T02MOD
031200161028     C                   movel(p)  dskasp        T02KE1
031300161028     c                   if        dskasp <> *blank
031400161028     C                   CALL      'TIBS02R'
031500161028     C                   PARM                    KPJBA
031600161028     C                   PARM                    tibs02ds
031700161028     C     T02ERR        IFEQ      *BLANKS
031800161028     C                   MOVEL     T02UNI        dtai
031900161031     c                   movel     d§taides      v1Casp            9
032000161028     C                   ELSE
032100161028     c                   clear                   v1Casp
032200161028     C                   CLEAR                   Dtai
032300161028     C                   ENDIF
032400161028     c                   end
032500161028      *
032600161028      **  Territorio
032700161028     C                   CLEAR                   tibs02ds
032800161028     C                   MOVEL     KNSIF         T02SIF
032900161028     C                   MOVEL     'ATS'         T02COD
033000161028     C                   MOVEL     'C'           T02MOD
033100161028     C                   movel(p)  dskter        T02KE1
033200161028     c                   if        dskter <> *blank
033300161028     C                   CALL      'TIBS02R'
033400161028     C                   PARM                    KPJBA
033500161028     C                   PARM                    tibs02ds
033600161028     C     T02ERR        IFEQ      *BLANKS
033700161028     C                   MOVEL     T02UNI        dats
033800161028     c                   movel     d§atsdes      v1Cter            9
033900161028     C                   ELSE
034000161028     c                   clear                   v1Cter
034100161028     C                   ENDIF
034200161028     c                   end
034300161028      *
034400161028      **  Massa
034500161028     c     'MAS'         setll     tntbe02l
034600161028     c     'MAS'         reade     tntbe02l
034700161028     c                   dow       not %EoF(tntbe02l)
034800161028     c                   if        dskMas = %subst(T2_tbeKE2:1:1)
034900161028     C                   MOVEL     T2_tbeUNI     Dmas
035000161031     c                   movel     d§masdes      v1cMas           14
035100161028     c                   leave
035200161028     c                   end
035300161028     c     'MAS'         reade     tntbe02l
035400161028     c                   end
035500161028      *
035600161028      **  Tipo Attività
035700161028     C                   CLEAR                   tibs02ds
035800161028     C                   MOVEL     KNSIF         T02SIF
035900161028     C                   MOVEL     'AAT'         T02COD
036000161028     C                   MOVEL     'C'           T02MOD
036100161028     C                   movel(p)  dskaat        T02KE1
036200161028     c                   if        dskaat <> *blank
036300161028     C                   CALL      'TIBS02R'
036400161028     C                   PARM                    KPJBA
036500161028     C                   PARM                    tibs02ds
036600161028     C     T02ERR        IFEQ      *BLANKS
036700161028     C                   MOVEL     T02UNI        Daat
036800161028     c                   movel     d§aatdes      v1cAat            7
036900161028     C                   ELSE
037000161028     c                   clear                   v1cAat
037100161028     C                   ENDIF
037200161028     c                   end
037300161028      *
037400161028      **  Tipo Allestimento
037500161028     C                   CLEAR                   tibs02ds
037600161028     C                   MOVEL     KNSIF         T02SIF
037700161028     C                   MOVEL     'ALL'         T02COD
037800161028     C                   MOVEL     'C'           T02MOD
037900161028     C                   movel(p)  dskall        T02KE1
038000161028     C                   if        dskall <> *blank
038100161028     C                   CALL      'TIBS02R'
038200161028     C                   PARM                    KPJBA
038300161028     C                   PARM                    tibs02ds
038400161028     C     T02ERR        IFEQ      *BLANKS
038500161028     C                   MOVEL     T02UNI        Dall
038600161028     c                   movel     d§alldes      v1Call           17
038700161028     C                   ELSE
038800161028     c                   clear                   v1Call
038900161028     C                   ENDIF
039000161028     c                   end
039100161028      **    al momento lo riempio di puntini
039200161028     c                   eval      v1Call = *all'.'
039300161028      **
039400161028     c                   movel     TBEuni        dAIG
039500161028     c                   z-add     D§aigIMP      v1cIMP            9 2
039600170313     c                   z-add     D§aigQTAs     v1cQTAs          10 3
039700170313     c                   z-add     D§aigQTAv     v1cQTAv          10 3
039800161031     c                   movel     tbeke2        data8             8 0
039900161031     c     *iso          movel     data8         data_iso
040000161031     c                   move      data_iso      data_eur
040100161031     c                   move      data_eur      datascad          8 0
040200161028      *
040300161031     c                   eval      deskey1= v1Casp + ' ' + v1Cter + ' ' +
040400170313     c                              v1Cmas + ' ' + v1cAat + '  '
040500170517     c                             + %editc(v1cQTAs:'3')
040600170517      * Qtà Volumi tenuto a parte perchè la data scadenza una volta è stata portata
040700170517      ** in seconda riga poi è stata riportata in prima riga
040800170517      *   quindi se ci si ripensa ancora è più facile spostare le cose
040900170517     c                   eval      deskey3= %editc(v1cQTAv:'3')
041000161104     C*  Scadenza
041100161104     c                   eval      datscad= datascad
041200030113     C*
041300161031     c                   eval      deskey2= v1Call + ' '
041400170313     c                   clear                   campo15          15
041500170313     c                   eval      campo15 = 'imp.' + %editc(v1cIMP:'3')
041600170313     c                   move      campo15       deskey2
041700161031     C*
041800030115     c                   clear                   S1Ann
041900940127     C*
042000161104     C*  evidenzia a rottura della prima chiave quindi con la data in vigore
042100161104     c                   setoff                                       25
042200161104     c                   if        tbeke1 <> KKE1
042300161104     c                   EVAL       KKE1 = tbeke1
042400161104     c                   setoN                                        25
042500161104     C                   END
042600161104     C*
042700161103     C                   Z-ADD     TBE_rec       h1TBErec
042800940127     C                   ADD       1             S1NRR
042900940127     C                   ADD       1             Y
043000940127     C*
043100940607     C                   WRITE     S1
043200940131     C*
043300940316    >C                   EXSR      REDANA
043400940128     C*
0435009401271-   C                   ENDDO
043600940127     C*
043700940223     C                   Z-ADD     S1NRR         WMAX                 30
043800940127     C*
043900940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
044000940127     C*
044100030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
044200940127     C                   MVR                     RESTO             3 0
044300030114     C     PAGINE        MULT      SFLPAG        C1RCD
0444000301141    C     RESTO         IFGT      0
044500030114     C                   ADD       1             C1RCD
0446000301141E   C                   ELSE
044700030114     C                   SUB       SFLPAG        C1RCD
044800030114     C                   ADD       1             C1RCD
0449000301141-   C                   ENDIF
045000940128     C*
045100940127     C                   ENDSR
045200940128     C************************************************************
045300940131     C* LETTURA RCD ARCHIVIO PILOTA
045400940128     C************************************************************
045500940607     C     REDANA        BEGSR
045600940128     C*
045700940131     C                   MOVEL     *OFF          $EFILE
045800940131     C                   MOVEL     *OFF          $RCDOK
045900940131     C*
0460009401311    C     $EFILE        DOUEQ     *ON
046100940131     C     $RCDOK        OREQ      *ON
046200940131     C*
046300110926     c     kTBL          Reade     tntbe01l
046400110926     c                   if        %eof(tntbe01l)
046500030113     C                   MOVEL     *ON           $EFILE
046600030113     C                   MOVE      $EFILE        *IN33
046700030113     c                   else
046800030113     C                   MOVE      *ON           $RCDOK
046900030113     c                   end
047000940131     C*
047100161028     C* salta se filiale diversa
047200161028     c                   movel     tbeke1        keyfil            3
047300161028     c                   if          keyfil <> filiale
047400161028     C                   MOVE      *OFF          $RCDOK
047500161028     c                   end
047600161031      **  controlla la DATA
047700161031     c                   if        tbeke2 <> ' '
047800161031     c                   movel     tbeke2        data8
047900161031     c     *iso          movel     data8         data_iso
048000161031      *in lettura se la data scadenza è minore di udate non faccio più vedere la riga
048100161031     c                   if        data_iso < wdate
048200161031     C                   MOVE      *OFF          $RCDOK
048300161031     c                   endif
048400161031     c                   end
048500161028     C*
0486009401311-   C                   ENDDO
048700940131     C*
048800940131     C                   ENDSR
048900940224     C************************************************************
049000940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
049100940224     C************************************************************
049200940224     C     CLCPAG        BEGSR
049300940224     C* Input :
049400940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
049500940224     C*          cursore
049600940224     C* - SFLPAG = numero rcd per pagina sfl
049700940224     C* Output :
049800940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
049900940224     C*
050000120412     c                   if        WSFL >0
050100940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
050200940224     C                   MVR                     RESTO             3 0
050300940224     C     RESTO         IFGT      0
050400940224     C                   ADD       1             PAGINE
050500940224     C                   ENDIF
050600940224     C     PAGINE        MULT      SFLPAG        WPAG
050700120412     c                   else
050800120412     C                   Z-ADD     1             WPAG
050900120412     c                   end
051000940224     C*
051100940224     C                   ENDSR
051200940309     C************************************************************
051300161028     C* GESTIONE F12 VIDEO S1
051400940309     C************************************************************
051500161028     C     F12S1         BEGSR
051600940309     C*
051700940309     C                   MOVE      *ON           $FINE
051800940325     C* fine programma
051900940309     C                   ENDSR
052000161028     C************************************************************
052100161028     C* GESTIONE F03 VIDEO S1
052200161028     C************************************************************
052300161028     C     F03S1         BEGSR
052400161028     C*
052500161028     C                   MOVE      *ON           $FINE
052600161028     C                   MOVE      '03'          kpjbu
052700161028     C* fine programma
052800161028     C                   ENDSR
052900940309     C/EJECT
053000940309     C************************************************************
053100940309     C* GESTIONE F10 VIDEO S1
053200940314     c* AGGIUNTA RECORD
053300940309     C************************************************************
053400940309     C     F10S1         BEGSR
053500940309     C*
053600030113     C                   RESET                   tabds
053700030113     C                   MOVEL     '01'          xtaopz
053800030113     C                   MOVE      *ZERO         xtaret
053900030113     C                   MOVE      *ZERO         xtaopr
054000161031     C                   clear                   xtake1
054100161103     C                   clear                   xtake2
054200161103     C                   move      *zeros        xtatberec
054300030113     C                   MOVE      *BLANKS       KPJBU
054400030113     C                   MOVEL     tabds         KPJBU
054500161028$004 C                   CALL      'TNTBA04R3'
054600030113     C                   PARM                    KPJBA
054700030114     C                   MOVEL     KPJBU         tabds
054800030113      *
054900940309     C* ritorno da PGM gestione
055000940309     C                   EXSR      GESRET
055100940309     C     WINZS1        IFEQ      *ON
055200940309     C                   MOVE      *ON           $INZS1
055300940309     C* se esistevano già righe sul sfl
055400940309     C* calcolo pagina a cui deve posizionarsi
055500940309     C     WSFL          IFGT      0
055600940309     C                   EXSR      CLCPAG
055700940309     C* altrimenti carico solo la 1a pagina
055800940309     C                   ELSE
055900940309     C                   Z-ADD     SFLPAG        WPAG
056000940309     C                   ENDIF
056100940309     C                   ENDIF
056200940309     C*
056300940309     C                   ENDSR
056400940128     C************************************************************
056500940131     C* CONTROLLO TESTATA LISTA
056600940128     C************************************************************
056700940131     C     CTRC1         BEGSR
056800940128     C*
056900940201     C                   MOVE      *OFF          *IN99
057000940131     C*
057100940202     C                   ENDSR
057200940131     C************************************************************
057300940131     C* CONTROLLO OPZIONI LISTA
057400940131     C************************************************************
057500940131     C     CTRS1         BEGSR
057600940131     C*
057700940202     C                   MOVEL     *OFF          $ESCI
057800940201     C                   SETOFF                                       99
057900940131     C                   Z-ADD     0             S1OPZ
058000040930     c                   clear                   x1cod
058100160412     c                   clear                   h1cod2
058200940131     C*
058300940127     C* Leggo il sfl solo se ci sono rcd
0584009401311    C     WMAX          IFGT      0
058500940607     C                   READC     S1                                     21
058600940127     C*
058700940131     C* esce se fine sfl o errore che richiede l'uscita
0588009401312    C     *IN21         DOWEQ     *OFF
058900940131     C     $ESCI         ANDEQ     *OFF
059000940201     C                   Z-ADD     S1NRR         C1RCD
059100940131     C* ctrl su riga
059200940131     C                   EXSR      RECS1
059300940131     C* gestione opzioni
0594009401313    C     S1OPZ         IFNE      0
059500940201     C     *IN99         ANDEQ     *OFF
059600940131     C                   EXSR      OPZS1
0597009401273-   C                   ENDIF
059800940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0599009402013    C     *IN99         IFEQ      *ON
060000940201     C     $ESCI         OREQ      *ON
060100940131     C                   MOVE      *ON           *IN32
060200940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
060300940204     C* la ripristinerò a conclusione del ciclo di READC
060400940223     C                   MOVE      *OFF          $INZS1
0605009402233-   C                   ENDIF
060600940223     C*
060700940131     C                   Z-ADD     0             S1OPZ
060800940223     C*
060900940607     C                   UPDATE    S1
061000940223     C*
061100940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0612009401313    C     $ESCI         IFEQ      *OFF
061300940607     C                   READC     S1                                     21
061400940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0615009402014    C     *IN21         IFEQ      *ON
061600940201     C                   MOVE      WINZS1        $INZS1
061700940204     C* calcolo pagina a cui deve posizionarsi
061800940224     C                   EXSR      CLCPAG
0619009402014-   C                   ENDIF
0620009402013-   C                   ENDIF
062100940131     C*
0622009401272-   C                   ENDDO
062300940127     C*
0624009401311-   C                   ENDIF
062500940131     C*
062600940127     C                   ENDSR
062700940127     C/EJECT
062800940127     C************************************************************
062900940131     C* CONTROLLO CAMPI I/O RIGA LISTA
063000940127     C************************************************************
063100940131     C     RECS1         BEGSR
063200940131     C*
063300940201     C* reset indicatori DSPATR
063400940201     C                   MOVE      *ALL'0'       IN4049           10
063500940201     C                   MOVEA     IN4049        *IN(40)
063600940201     C*
063700040930     C*  se richiamato x selezionare un codice 11 = *on
063800040930     C*   non deve utilizzare le opzioni di modifica
063900040930     C*  e viceversa
064000040930     c     *in11         ifeq      *on
0641000409303    C     S1OPZ         IFeq      2
0642000409303    C     S1OPZ         OReq      3
0643000409303    C     S1OPZ         OReq      4
064400040930     c                   seton                                        41  99
064500040930     C                   END
064600040930      * selezionato un codice da restituire al chiamante
0647000409303    C     S1OPZ         IFeq      1
064800161028     c                   movel     h1cod         x1cod
064900160412     c                   movel     h1cod2        x1cod2
065000161103     c                   move      h1TBErec      xtatberec
065100040930     c                   clear                   kpjbu
065200160412     c                   eval      kpjbu = x1cod + x1cod2
065300040930     C                   MOVEL     *ON           $ESCI
065400040930     C                   MOVEL     *ON           $fine
065500040930     C                   END
065600040930     C                   END
065700040930     C*
065800040930     c     *in11         ifeq      *off
0659000409303    C     S1OPZ         ANDeq     1
066000040930     c                   seton                                        41  99
066100040930     C                   END
066200040930     C*
066300940131     C                   ENDSR
066400940131     C************************************************************
066500940131     C* GESTIONE OPZIONI LISTA
066600940131     C************************************************************
066700940131     C     OPZS1         BEGSR
066800940201     C*
0669000409302    C     *IN11         IFEQ      *Off
0670000409302    C     *IN11         oreq      *On
0671000409302    C     s1opz         andeq     05
067200040930     C*
067300030113     C                   RESET                   tabds
067400110902     C                   MOVE      *all'0'       xtaopz
067500110902     C                   MOVE      S1OPZ         xtaopz
067600030113     C                   MOVE      *ZERO         xtaret
067700030113     C                   MOVE      *ZERO         xtaopr
067800161028     C                   MOVEl     h1COD         xtake1
067900160412     C                   MOVEl     H1COD2        xtake2
068000161103     C                   MOVE      H1tbeREC      xtatbeREC
068100940715     C                   MOVE      *BLANKS       KPJBU
068200030113     C                   MOVEL     tabds         KPJBU
068300161028$004 C                   CALL      'TNTBA04R3'
068400940607     C                   PARM                    KPJBA
068500030114     C                   MOVEL     KPJBU         tabds
068600940201     C*
068700940223     C* ritorno da PGM gestione
068800940223     C                   EXSR      GESRET
068900040930     C*
069000940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0691009402252    C     *IN99         IFEQ      *ON
069200940223     C                   SETON                                        40
0693009402252-   C                   ENDIF
069400940225     C*
0695000409302-   C                   ENDIF
069600040930     C*
069700940131     C                   ENDSR
069800940223     C/EJECT
069900940223     C************************************************************
070000940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
070100940223     C************************************************************
070200940223     C     GESRET        BEGSR
070300940223     C*
070400940223     C* modo di ritorno
070500940223     C*
0706009402231    C                   SELECT
070700940314    >C* << questi modi di utilizzo dei valori di ritorno dal
070800940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
070900940314    >C*    proposte, normalmente sempre valide, ma modificabili
071000940314    >C*    per situazioni particolari >>
071100940223     C* 1 = F3
071200030113    >C     xtaret        WHENEQ    '1'
071300940224     C                   MOVE      *ON           $ESCI
071400940223     C                   MOVE      *ON           $FINE
071500940223     C* 2 = F12
071600030113    >C     xtaret        WHENEQ    '2'
071700940223     C                   MOVE      *ON           $ESCI
071800940223     C*
0719009402231-   C                   ENDSL
072000940223     C*
072100940223     C* operazione eseguite dal pgm chiamato
072200940223     C*
0723009402231    C                   SELECT
072400940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
072500030113    >C     xtaopr        WHENEQ    '1'
072600940223     C                   MOVE      *ON           WINZS1
072700940223     C*
0728009402231-   C                   ENDSL
072900940223     C*
073000940223     C* funzione non eseguibile per errore :
073100940223     C*
0732009402231    C                   SELECT
073300940223     C* 1 = funzione richiamata chiusa in errore
073400940316    >C*  eventualmente gestire altri codici di errore
073500030113    >C     xtaerr        WHENEQ    '1'
073600940223     C                   MOVE      *ON           $ESCI
073700940223     C                   SETON                                        5299
073800940223     C*
0739009402231-   C                   ENDSL
074000940223     C*
074100940223     C                   ENDSR
074200940223     C/EJECT
074300940131     C************************************************************
074400940131     C* OPERAZIONI INIZIALI
074500940131     C************************************************************
074600940131     C     *INZSR        BEGSR
074700030113     C*
074800030113     C* Reperimento parametri
074900030113     C     *ENTRY        PLIST
075000030113     C                   PARM                    KPJBA
075100161028     C                   movel     KPJBU         Filiale           3
075200161028     C                   clear                   kpjbu
075300040930     C*
075400040930     C* Se chiamato x selezionare un codice
075500040930     c                   clear                   x1cod
075600160412     c                   clear                   x1cod2
075700161028     C*
075800161031     c                   movel     *date         Wdate
075900161031     C*
076000161028     c                   clear                   selez             1
076100040930     c                   if        selez = '?'
076200040930     c                   seton                                        11
076300040930     c                   end
076400030113     C*
076500030113     C* Variabili per gestione videate
076600030113     C                   MOVE      *BLANK        $GEST             2
076700030113     C                   MOVE      *BLANK        $FINE             1
076800030113     C                   MOVE      *BLANK        $INZS1            1
076900030113     C                   MOVE      *BLANK        $EFILE            1
077000030113     C                   MOVE      *BLANK        $ESCI             1
077100030113     C                   MOVE      *BLANK        $RCDOK            1
077200030113     C*
077300030113     C     KTBL          KLIST
077400110926     C                   KFLD                    KCOD
077500030113     C* Indici
077600030113     C                   Z-ADD     0             X                 3 0
077700030113     C                   Z-ADD     0             Y                 3 0
077800940506     C*
077900161028     C                   move      'AIG'         KCOD
078000161104      *
078100161104      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
078200161104     c     *dtaara       define    §azute        AZUTEds
078300161104     c     *dtaara       define    §datiute      dDatiUte
078400161104      *
078500161104     c                   clear                   AZUTEds
078600161104     c                   clear                   dDatiUte
078700161104     c                   clear                   TIBS34ds
078800161104     c                   in(E)     *dtaara
078900161104if  1c                   if        %Error  or  RSUT = *blanks
079000161104     c                   call      'TIBS34R'
079100161104     c                   parm                    TIBS34ds
079200161104     c                   in        *dtaara
079300161104e   1c                   endif
079400161104      *
079500161104      *-- Verifica errori e autorità profilo
079600161104sel 1c                   SELECT
079700161104      *-- controllo se ho errori nei dati utente
079800161104      *--   nel qual caso NON risulta un profilo abilitato
079900161104w   1c                   WHEN      DUTerr  = 'E'
080000161104     c                   eval      $Fine   = *on
080100161104      *
080200161104      *-- CONTROLLO AUTORITA'
080300161104      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
080400161104      *-- se il chiamante non richiede autorità specifica verificare
080500161104      *--   quella del profilo
080600161104      *-- se il chiamante richiede autorità specifica verificarla,
080700161104      *--  se è blank verificare quella del profilo
080800161104      *
080900161104      * se UTEAUT = *BLANK non siamo in sede
081000161104w   1c                   WHEN      UTEaut  = *blank
081100161104     c                   eval      $Fine   = *on
081200161104      *
081300161104x   1c                   OTHER
081400161104      *
081500161104e   1c                   ENDSL
081600940127     C*
081700940117     C                   ENDSR
081800030113     C************************************************************
