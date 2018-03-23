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
014400940127     C*----------------------------------------------------*
014500940127     C*                MAIN LINE PROGRAM
014600940127     C*----------------------------------------------------*
014700940223     C* inizializzazione variabili
014800940223     C                   EXSR      INZVAR
014900940223     C*
015000940223     C     $FINE         DOWEQ     *OFF
015100940131     C     $GEST         CASEQ     'S1'          GESS1
015200940117     C                   END
015300940117     C                   END
015400940325     C* fine programma
015500940325     C                   SETON                                            LR
015600030113     C************************************************************
015700030113     C* INIZIALIZZAZIONE VARIABILI
015800030113     C************************************************************
015900030113     C     INZVAR        BEGSR
016000030113     C*
016100030113     C* Pulizia campi e indicatori
016200030113     C                   MOVE      *ALL'0'       IN4049           10
016300030113     C                   MOVEA     IN4049        *IN(40)
016400030113     C                   CLEAR                   S1OPZ
016500030113     C* Variabili per gestione videate
016600030113     C*
016700030113     C                   MOVE      *OFF          $FINE
016800030113     C                   MOVE      *OFF          $INZS1
016900030113     C                   MOVE      *OFF          $EFILE
017000030113     C                   MOVE      *OFF          $ESCI
017100030113     C                   MOVE      *OFF          $RCDOK
017200030113     C                   Z-ADD     0             $ULKS1            3 0
017300030113     C*
017400030113     C                   MOVE      *ON           $INZS1
017500030113     C                   MOVE      'S1'          $GEST
017600030113     C*
017700030113     C* Variabili appoggio
017800030114     C                   Z-ADD     1             WPAG
017900030113     c*
018000030113     C                   ENDSR
018100940127     C************************************************************
018200940131     C* GESTIONE LISTA
018300940127     C************************************************************
018400940127     C     GESS1         BEGSR
018500030113     C*
018600940223     C* inizializzazione videata
018700940223     C     $INZS1        IFEQ      *ON
018800940127     C                   EXSR      INZS1
018900940223     C                   MOVE      *OFF          $INZS1
019000940127     C                   ENDIF
019100030113     C*
019200030113     C* emissione piede videata
019300030113     C                   WRITE     Z1
019400030113     C* Non ci sono records
019500940223     C     WMAX          IFEQ      0
019600940607     C                   WRITE     D1
019700030114     C                   Else
019800120412     C     Wsfl          IFgt      Wmax
019900120412     c                   z-add     wmax          Wsfl
020000120412     c                   end
020100030114     C     Wsfl          IFgt      0
020200030114     C                   Z-ADD     wsfl          C1RCD
020300030114     C                   Else
020400030114     C     Wpag          IFgt      0
020500030114     C                   Z-ADD     wpag          C1RCD
020600030114     C                   EndIF
020700030114     C                   EndIF
020800030114     C                   ENDIF
020900940127     C*
021000030113     C*              *------------------*
021100940607     C                   EXFMT     C1
021200030113     C*              *------------------*
021300030113     C*
021400940204     C     C1NRR         IFNE      0
021500940204     C                   Z-ADD     C1NRR         WSFL
021600940204     C                   ENDIF
021700940127     C                   Z-ADD     SFLNRR        C1RCD
021800030113     C* Selezioni
0219009401271    C                   SELECT
022000940127     C* F3=Fine
022100161028     C     $TASTO        WHENEQ    F03
022200161028     C                   EXSR      F03S1
022300161028     C* F12=Ritorno
022400161028     C     $TASTO        WHENEQ    F12
022500161028     C                   EXSR      F12S1
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
024700161028      **  filiale
024800161028     c                   if        filiale ='999'
024900161028     c                   movel(p)  'Azienda'     c1fild
025000161028     c                   movel(p)  '999'         c1fil
025100161028     c                   else
025200161028     c                   clear                   c1fild
025300161028     c                   movel(p)  filiale       c1fil
025400161028     c     c1fil         chain     azorg01l
025500161028     c                   if        %found(azorg01l)
025600161028     c                   movel     orgdes        c1fild
025700161028     c                   endif
025800161028     c                   endif
025900161104     c                   eval      rsutDES = rsut
026000161028      *
026100030113     C* CARICAMENTO SFL totale
026200940201     C                   Z-ADD     0             S1NRR
026300030113     C                   Z-ADD     1             C1RCD
026400940128     C                   Z-ADD     0             WMAX
026500120412     C                   Z-ADD     1             WPAG
026600161104     C                   CLEAR                   KKE1
026700940224     C*
026800940224     C* Posizionamento su file pilota
026900110926     c     kTBL          setll     tntbe01l
027000940608    >C                   EXSR      REDANA
027100030113     C* Carico il SFL
027200940127     C                   EXSR      ROLS1
027300030113     C*
027400030114     c                   if        xtaopr <> '1'
027500030114     C                   Z-ADD     1             WPAG
027600030114     c                   end
027700940127     C*
027800940127     C                   ENDSR
027900940127     C************************************************************
028000940131     C* CARICAMENTO PAGINA LISTA
028100940127     C************************************************************
028200940127     C     ROLS1         BEGSR
028300940127     C*
028400940128     C                   SETOFF                                       32
028500940223     C                   Z-ADD     0             Y
028600940127     C                   Z-ADD     WMAX          S1NRR
028700940127     C*
028800940127     C* Leggo dal file anagrafico per caricare la lista
0289009401311    C     $EFILE        DOWEQ     *OFF
029000940127     C*
029100030113     c                   clear                   s1opz
029200161031     c                   clear                   deskey1
029300161104     c                   clear                   datscad
029400161031     c                   clear                   deskey2
029500120412     c                   clear                   Obbiettivo        3
029600161028     c                   clear                   h1cod
029700160412     c                   clear                   h1cod2
029800161028     c                   movel     tbeke1        h1cod
029900160412     c                   movel     tbeke2        h1cod2
030000120412      **
030100161028     c                   movel     tbeke1        dschiave
030200161028      **
030300161028      **  decodifica i singoli flags
030400161028      *
030500161028      **  Tipo aspettativa
030600161028     C                   CLEAR                   tibs02ds
030700161028     C                   MOVEL     KNSIF         T02SIF
030800161028     C                   MOVEL     'TAI'         T02COD
030900161028     C                   MOVEL     'C'           T02MOD
031000161028     C                   movel(p)  dskasp        T02KE1
031100161028     c                   if        dskasp <> *blank
031200161028     C                   CALL      'TIBS02R'
031300161028     C                   PARM                    KPJBA
031400161028     C                   PARM                    tibs02ds
031500161028     C     T02ERR        IFEQ      *BLANKS
031600161028     C                   MOVEL     T02UNI        dtai
031700161031     c                   movel     d§taides      v1Casp            9
031800161028     C                   ELSE
031900161028     c                   clear                   v1Casp
032000161028     C                   CLEAR                   Dtai
032100161028     C                   ENDIF
032200161028     c                   end
032300161028      *
032400161028      **  Territorio
032500161028     C                   CLEAR                   tibs02ds
032600161028     C                   MOVEL     KNSIF         T02SIF
032700161028     C                   MOVEL     'ATS'         T02COD
032800161028     C                   MOVEL     'C'           T02MOD
032900161028     C                   movel(p)  dskter        T02KE1
033000161028     c                   if        dskter <> *blank
033100161028     C                   CALL      'TIBS02R'
033200161028     C                   PARM                    KPJBA
033300161028     C                   PARM                    tibs02ds
033400161028     C     T02ERR        IFEQ      *BLANKS
033500161028     C                   MOVEL     T02UNI        dats
033600161028     c                   movel     d§atsdes      v1Cter            9
033700161028     C                   ELSE
033800161028     c                   clear                   v1Cter
033900161028     C                   ENDIF
034000161028     c                   end
034100161028      *
034200161028      **  Massa
034300161028     c     'MAS'         setll     tntbe02l
034400161028     c     'MAS'         reade     tntbe02l
034500161028     c                   dow       not %EoF(tntbe02l)
034600161028     c                   if        dskMas = %subst(T2_tbeKE2:1:1)
034700161028     C                   MOVEL     T2_tbeUNI     Dmas
034800161031     c                   movel     d§masdes      v1cMas           14
034900161028     c                   leave
035000161028     c                   end
035100161028     c     'MAS'         reade     tntbe02l
035200161028     c                   end
035300161028      *
035400161028      **  Tipo Attività
035500161028     C                   CLEAR                   tibs02ds
035600161028     C                   MOVEL     KNSIF         T02SIF
035700161028     C                   MOVEL     'AAT'         T02COD
035800161028     C                   MOVEL     'C'           T02MOD
035900161028     C                   movel(p)  dskaat        T02KE1
036000161028     c                   if        dskaat <> *blank
036100161028     C                   CALL      'TIBS02R'
036200161028     C                   PARM                    KPJBA
036300161028     C                   PARM                    tibs02ds
036400161028     C     T02ERR        IFEQ      *BLANKS
036500161028     C                   MOVEL     T02UNI        Daat
036600161028     c                   movel     d§aatdes      v1cAat            7
036700161028     C                   ELSE
036800161028     c                   clear                   v1cAat
036900161028     C                   ENDIF
037000161028     c                   end
037100161028      *
037200161028      **  Tipo Allestimento
037300161028     C                   CLEAR                   tibs02ds
037400161028     C                   MOVEL     KNSIF         T02SIF
037500161028     C                   MOVEL     'ALL'         T02COD
037600161028     C                   MOVEL     'C'           T02MOD
037700161028     C                   movel(p)  dskall        T02KE1
037800161028     C                   if        dskall <> *blank
037900161028     C                   CALL      'TIBS02R'
038000161028     C                   PARM                    KPJBA
038100161028     C                   PARM                    tibs02ds
038200161028     C     T02ERR        IFEQ      *BLANKS
038300161028     C                   MOVEL     T02UNI        Dall
038400161028     c                   movel     d§alldes      v1Call           17
038500161028     C                   ELSE
038600161028     c                   clear                   v1Call
038700161028     C                   ENDIF
038800161028     c                   end
038900161028      **    al momento lo riempio di puntini
039000161028     c                   eval      v1Call = *all'.'
039100161028      **
039200161028     c                   movel     TBEuni        dAIG
039300161028     c                   z-add     D§aigIMP      v1cIMP            9 2
039400170313     c                   z-add     D§aigQTAs     v1cQTAs          10 3
039500170313     c                   z-add     D§aigQTAv     v1cQTAv          10 3
039600161031     c                   movel     tbeke2        data8             8 0
039700161031     c     *iso          movel     data8         data_iso
039800161031     c                   move      data_iso      data_eur
039900161031     c                   move      data_eur      datascad          8 0
040000161028      *
040100161031     c                   eval      deskey1= v1Casp + ' ' + v1Cter + ' ' +
040200170313     c                              v1Cmas + ' ' + v1cAat + '  '
040300170313     c                             + %editc(v1cQTAs:'3')  + '  '
040400170313     c                             + %editc(v1cQTAv:'3')
040500161104     C*  Scadenza
040600161104     c                   eval      datscad= datascad
040700030113     C*
040800161031     c                   eval      deskey2= v1Call + ' '
040900170313     c                   clear                   campo15          15
041000170313     c                   eval      campo15 = 'imp.' + %editc(v1cIMP:'3')
041100170313     c                   move      campo15       deskey2
041200161031     C*
041300030115     c                   clear                   S1Ann
041400940127     C*
041500161104     C*  evidenzia a rottura della prima chiave quindi con la data in vigore
041600161104     c                   setoff                                       25
041700161104     c                   if        tbeke1 <> KKE1
041800161104     c                   EVAL       KKE1 = tbeke1
041900161104     c                   setoN                                        25
042000161104     C                   END
042100161104     C*
042200161103     C                   Z-ADD     TBE_rec       h1TBErec
042300940127     C                   ADD       1             S1NRR
042400940127     C                   ADD       1             Y
042500940127     C*
042600940607     C                   WRITE     S1
042700940131     C*
042800940316    >C                   EXSR      REDANA
042900940128     C*
0430009401271-   C                   ENDDO
043100940127     C*
043200940223     C                   Z-ADD     S1NRR         WMAX                 30
043300940127     C*
043400940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
043500940127     C*
043600030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
043700940127     C                   MVR                     RESTO             3 0
043800030114     C     PAGINE        MULT      SFLPAG        C1RCD
0439000301141    C     RESTO         IFGT      0
044000030114     C                   ADD       1             C1RCD
0441000301141E   C                   ELSE
044200030114     C                   SUB       SFLPAG        C1RCD
044300030114     C                   ADD       1             C1RCD
0444000301141-   C                   ENDIF
044500940128     C*
044600940127     C                   ENDSR
044700940128     C************************************************************
044800940131     C* LETTURA RCD ARCHIVIO PILOTA
044900940128     C************************************************************
045000940607     C     REDANA        BEGSR
045100940128     C*
045200940131     C                   MOVEL     *OFF          $EFILE
045300940131     C                   MOVEL     *OFF          $RCDOK
045400940131     C*
0455009401311    C     $EFILE        DOUEQ     *ON
045600940131     C     $RCDOK        OREQ      *ON
045700940131     C*
045800110926     c     kTBL          Reade     tntbe01l
045900110926     c                   if        %eof(tntbe01l)
046000030113     C                   MOVEL     *ON           $EFILE
046100030113     C                   MOVE      $EFILE        *IN33
046200030113     c                   else
046300030113     C                   MOVE      *ON           $RCDOK
046400030113     c                   end
046500940131     C*
046600161028     C* salta se filiale diversa
046700161028     c                   movel     tbeke1        keyfil            3
046800161028     c                   if          keyfil <> filiale
046900161028     C                   MOVE      *OFF          $RCDOK
047000161028     c                   end
047100161031      **  controlla la DATA
047200161031     c                   if        tbeke2 <> ' '
047300161031     c                   movel     tbeke2        data8
047400161031     c     *iso          movel     data8         data_iso
047500161031      *in lettura se la data scadenza è minore di udate non faccio più vedere la riga
047600161031     c                   if        data_iso < wdate
047700161031     C                   MOVE      *OFF          $RCDOK
047800161031     c                   endif
047900161031     c                   end
048000161028     C*
0481009401311-   C                   ENDDO
048200940131     C*
048300940131     C                   ENDSR
048400940224     C************************************************************
048500940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
048600940224     C************************************************************
048700940224     C     CLCPAG        BEGSR
048800940224     C* Input :
048900940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
049000940224     C*          cursore
049100940224     C* - SFLPAG = numero rcd per pagina sfl
049200940224     C* Output :
049300940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
049400940224     C*
049500120412     c                   if        WSFL >0
049600940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
049700940224     C                   MVR                     RESTO             3 0
049800940224     C     RESTO         IFGT      0
049900940224     C                   ADD       1             PAGINE
050000940224     C                   ENDIF
050100940224     C     PAGINE        MULT      SFLPAG        WPAG
050200120412     c                   else
050300120412     C                   Z-ADD     1             WPAG
050400120412     c                   end
050500940224     C*
050600940224     C                   ENDSR
050700940309     C************************************************************
050800161028     C* GESTIONE F12 VIDEO S1
050900940309     C************************************************************
051000161028     C     F12S1         BEGSR
051100940309     C*
051200940309     C                   MOVE      *ON           $FINE
051300940325     C* fine programma
051400940309     C                   ENDSR
051500161028     C************************************************************
051600161028     C* GESTIONE F03 VIDEO S1
051700161028     C************************************************************
051800161028     C     F03S1         BEGSR
051900161028     C*
052000161028     C                   MOVE      *ON           $FINE
052100161028     C                   MOVE      '03'          kpjbu
052200161028     C* fine programma
052300161028     C                   ENDSR
052400940309     C/EJECT
052500940309     C************************************************************
052600940309     C* GESTIONE F10 VIDEO S1
052700940314     c* AGGIUNTA RECORD
052800940309     C************************************************************
052900940309     C     F10S1         BEGSR
053000940309     C*
053100030113     C                   RESET                   tabds
053200030113     C                   MOVEL     '01'          xtaopz
053300030113     C                   MOVE      *ZERO         xtaret
053400030113     C                   MOVE      *ZERO         xtaopr
053500161031     C                   clear                   xtake1
053600161103     C                   clear                   xtake2
053700161103     C                   move      *zeros        xtatberec
053800030113     C                   MOVE      *BLANKS       KPJBU
053900030113     C                   MOVEL     tabds         KPJBU
054000161028$004 C                   CALL      'TNTBA04R3'
054100030113     C                   PARM                    KPJBA
054200030114     C                   MOVEL     KPJBU         tabds
054300030113      *
054400940309     C* ritorno da PGM gestione
054500940309     C                   EXSR      GESRET
054600940309     C     WINZS1        IFEQ      *ON
054700940309     C                   MOVE      *ON           $INZS1
054800940309     C* se esistevano già righe sul sfl
054900940309     C* calcolo pagina a cui deve posizionarsi
055000940309     C     WSFL          IFGT      0
055100940309     C                   EXSR      CLCPAG
055200940309     C* altrimenti carico solo la 1a pagina
055300940309     C                   ELSE
055400940309     C                   Z-ADD     SFLPAG        WPAG
055500940309     C                   ENDIF
055600940309     C                   ENDIF
055700940309     C*
055800940309     C                   ENDSR
055900940128     C************************************************************
056000940131     C* CONTROLLO TESTATA LISTA
056100940128     C************************************************************
056200940131     C     CTRC1         BEGSR
056300940128     C*
056400940201     C                   MOVE      *OFF          *IN99
056500940131     C*
056600940202     C                   ENDSR
056700940131     C************************************************************
056800940131     C* CONTROLLO OPZIONI LISTA
056900940131     C************************************************************
057000940131     C     CTRS1         BEGSR
057100940131     C*
057200940202     C                   MOVEL     *OFF          $ESCI
057300940201     C                   SETOFF                                       99
057400940131     C                   Z-ADD     0             S1OPZ
057500040930     c                   clear                   x1cod
057600160412     c                   clear                   h1cod2
057700940131     C*
057800940127     C* Leggo il sfl solo se ci sono rcd
0579009401311    C     WMAX          IFGT      0
058000940607     C                   READC     S1                                     21
058100940127     C*
058200940131     C* esce se fine sfl o errore che richiede l'uscita
0583009401312    C     *IN21         DOWEQ     *OFF
058400940131     C     $ESCI         ANDEQ     *OFF
058500940201     C                   Z-ADD     S1NRR         C1RCD
058600940131     C* ctrl su riga
058700940131     C                   EXSR      RECS1
058800940131     C* gestione opzioni
0589009401313    C     S1OPZ         IFNE      0
059000940201     C     *IN99         ANDEQ     *OFF
059100940131     C                   EXSR      OPZS1
0592009401273-   C                   ENDIF
059300940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0594009402013    C     *IN99         IFEQ      *ON
059500940201     C     $ESCI         OREQ      *ON
059600940131     C                   MOVE      *ON           *IN32
059700940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
059800940204     C* la ripristinerò a conclusione del ciclo di READC
059900940223     C                   MOVE      *OFF          $INZS1
0600009402233-   C                   ENDIF
060100940223     C*
060200940131     C                   Z-ADD     0             S1OPZ
060300940223     C*
060400940607     C                   UPDATE    S1
060500940223     C*
060600940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0607009401313    C     $ESCI         IFEQ      *OFF
060800940607     C                   READC     S1                                     21
060900940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0610009402014    C     *IN21         IFEQ      *ON
061100940201     C                   MOVE      WINZS1        $INZS1
061200940204     C* calcolo pagina a cui deve posizionarsi
061300940224     C                   EXSR      CLCPAG
0614009402014-   C                   ENDIF
0615009402013-   C                   ENDIF
061600940131     C*
0617009401272-   C                   ENDDO
061800940127     C*
0619009401311-   C                   ENDIF
062000940131     C*
062100940127     C                   ENDSR
062200940127     C/EJECT
062300940127     C************************************************************
062400940131     C* CONTROLLO CAMPI I/O RIGA LISTA
062500940127     C************************************************************
062600940131     C     RECS1         BEGSR
062700940131     C*
062800940201     C* reset indicatori DSPATR
062900940201     C                   MOVE      *ALL'0'       IN4049           10
063000940201     C                   MOVEA     IN4049        *IN(40)
063100940201     C*
063200040930     C*  se richiamato x selezionare un codice 11 = *on
063300040930     C*   non deve utilizzare le opzioni di modifica
063400040930     C*  e viceversa
063500040930     c     *in11         ifeq      *on
0636000409303    C     S1OPZ         IFeq      2
0637000409303    C     S1OPZ         OReq      3
0638000409303    C     S1OPZ         OReq      4
063900040930     c                   seton                                        41  99
064000040930     C                   END
064100040930      * selezionato un codice da restituire al chiamante
0642000409303    C     S1OPZ         IFeq      1
064300161028     c                   movel     h1cod         x1cod
064400160412     c                   movel     h1cod2        x1cod2
064500161103     c                   move      h1TBErec      xtatberec
064600040930     c                   clear                   kpjbu
064700160412     c                   eval      kpjbu = x1cod + x1cod2
064800040930     C                   MOVEL     *ON           $ESCI
064900040930     C                   MOVEL     *ON           $fine
065000040930     C                   END
065100040930     C                   END
065200040930     C*
065300040930     c     *in11         ifeq      *off
0654000409303    C     S1OPZ         ANDeq     1
065500040930     c                   seton                                        41  99
065600040930     C                   END
065700040930     C*
065800940131     C                   ENDSR
065900940131     C************************************************************
066000940131     C* GESTIONE OPZIONI LISTA
066100940131     C************************************************************
066200940131     C     OPZS1         BEGSR
066300940201     C*
0664000409302    C     *IN11         IFEQ      *Off
0665000409302    C     *IN11         oreq      *On
0666000409302    C     s1opz         andeq     05
066700040930     C*
066800030113     C                   RESET                   tabds
066900110902     C                   MOVE      *all'0'       xtaopz
067000110902     C                   MOVE      S1OPZ         xtaopz
067100030113     C                   MOVE      *ZERO         xtaret
067200030113     C                   MOVE      *ZERO         xtaopr
067300161028     C                   MOVEl     h1COD         xtake1
067400160412     C                   MOVEl     H1COD2        xtake2
067500161103     C                   MOVE      H1tbeREC      xtatbeREC
067600940715     C                   MOVE      *BLANKS       KPJBU
067700030113     C                   MOVEL     tabds         KPJBU
067800161028$004 C                   CALL      'TNTBA04R3'
067900940607     C                   PARM                    KPJBA
068000030114     C                   MOVEL     KPJBU         tabds
068100940201     C*
068200940223     C* ritorno da PGM gestione
068300940223     C                   EXSR      GESRET
068400040930     C*
068500940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0686009402252    C     *IN99         IFEQ      *ON
068700940223     C                   SETON                                        40
0688009402252-   C                   ENDIF
068900940225     C*
0690000409302-   C                   ENDIF
069100040930     C*
069200940131     C                   ENDSR
069300940223     C/EJECT
069400940223     C************************************************************
069500940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
069600940223     C************************************************************
069700940223     C     GESRET        BEGSR
069800940223     C*
069900940223     C* modo di ritorno
070000940223     C*
0701009402231    C                   SELECT
070200940314    >C* << questi modi di utilizzo dei valori di ritorno dal
070300940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
070400940314    >C*    proposte, normalmente sempre valide, ma modificabili
070500940314    >C*    per situazioni particolari >>
070600940223     C* 1 = F3
070700030113    >C     xtaret        WHENEQ    '1'
070800940224     C                   MOVE      *ON           $ESCI
070900940223     C                   MOVE      *ON           $FINE
071000940223     C* 2 = F12
071100030113    >C     xtaret        WHENEQ    '2'
071200940223     C                   MOVE      *ON           $ESCI
071300940223     C*
0714009402231-   C                   ENDSL
071500940223     C*
071600940223     C* operazione eseguite dal pgm chiamato
071700940223     C*
0718009402231    C                   SELECT
071900940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
072000030113    >C     xtaopr        WHENEQ    '1'
072100940223     C                   MOVE      *ON           WINZS1
072200940223     C*
0723009402231-   C                   ENDSL
072400940223     C*
072500940223     C* funzione non eseguibile per errore :
072600940223     C*
0727009402231    C                   SELECT
072800940223     C* 1 = funzione richiamata chiusa in errore
072900940316    >C*  eventualmente gestire altri codici di errore
073000030113    >C     xtaerr        WHENEQ    '1'
073100940223     C                   MOVE      *ON           $ESCI
073200940223     C                   SETON                                        5299
073300940223     C*
0734009402231-   C                   ENDSL
073500940223     C*
073600940223     C                   ENDSR
073700940223     C/EJECT
073800940131     C************************************************************
073900940131     C* OPERAZIONI INIZIALI
074000940131     C************************************************************
074100940131     C     *INZSR        BEGSR
074200030113     C*
074300030113     C* Reperimento parametri
074400030113     C     *ENTRY        PLIST
074500030113     C                   PARM                    KPJBA
074600161028     C                   movel     KPJBU         Filiale           3
074700161028     C                   clear                   kpjbu
074800040930     C*
074900040930     C* Se chiamato x selezionare un codice
075000040930     c                   clear                   x1cod
075100160412     c                   clear                   x1cod2
075200161028     C*
075300161031     c                   movel     *date         Wdate
075400161031     C*
075500161028     c                   clear                   selez             1
075600040930     c                   if        selez = '?'
075700040930     c                   seton                                        11
075800040930     c                   end
075900030113     C*
076000030113     C* Variabili per gestione videate
076100030113     C                   MOVE      *BLANK        $GEST             2
076200030113     C                   MOVE      *BLANK        $FINE             1
076300030113     C                   MOVE      *BLANK        $INZS1            1
076400030113     C                   MOVE      *BLANK        $EFILE            1
076500030113     C                   MOVE      *BLANK        $ESCI             1
076600030113     C                   MOVE      *BLANK        $RCDOK            1
076700030113     C*
076800030113     C     KTBL          KLIST
076900110926     C                   KFLD                    KCOD
077000030113     C* Indici
077100030113     C                   Z-ADD     0             X                 3 0
077200030113     C                   Z-ADD     0             Y                 3 0
077300940506     C*
077400161028     C                   move      'AIG'         KCOD
077500161104      *
077600161104      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
077700161104     c     *dtaara       define    §azute        AZUTEds
077800161104     c     *dtaara       define    §datiute      dDatiUte
077900161104      *
078000161104     c                   clear                   AZUTEds
078100161104     c                   clear                   dDatiUte
078200161104     c                   clear                   TIBS34ds
078300161104     c                   in(E)     *dtaara
078400161104if  1c                   if        %Error  or  RSUT = *blanks
078500161104     c                   call      'TIBS34R'
078600161104     c                   parm                    TIBS34ds
078700161104     c                   in        *dtaara
078800161104e   1c                   endif
078900161104      *
079000161104      *-- Verifica errori e autorità profilo
079100161104sel 1c                   SELECT
079200161104      *-- controllo se ho errori nei dati utente
079300161104      *--   nel qual caso NON risulta un profilo abilitato
079400161104w   1c                   WHEN      DUTerr  = 'E'
079500161104     c                   eval      $Fine   = *on
079600161104      *
079700161104      *-- CONTROLLO AUTORITA'
079800161104      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
079900161104      *-- se il chiamante non richiede autorità specifica verificare
080000161104      *--   quella del profilo
080100161104      *-- se il chiamante richiede autorità specifica verificarla,
080200161104      *--  se è blank verificare quella del profilo
080300161104      *
080400161104      * se UTEAUT = *BLANK non siamo in sede
080500161104w   1c                   WHEN      UTEaut  = *blank
080600161104     c                   eval      $Fine   = *on
080700161104      *
080800161104x   1c                   OTHER
080900161104      *
081000161104e   1c                   ENDSL
081100940127     C*
081200940117     C                   ENDSR
081300030113     C************************************************************
