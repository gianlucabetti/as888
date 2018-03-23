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
004201161104      *
004202161104     d TIBS34ds      e ds                  inz
004203161104     d dDatiUte      e ds                  inz
004204161104     d AZUTEds       e ds                  extname(AZUTE00F) inz
004205161104      *
004300940211     D*-------------
004400940211     D DSFMT           DS           512
004500940506     D  $TASTO               369    369
004600940211     D  NRG                  370    370
004700940211     D  NCL                  371    371
004800940211     D  SFLNRR               378    379B 0
004900161103     D*-------------
005000161103     D TBEds           DS
005100161103     d  TBE_rec              397    400b 0
005200940207     D*-------------
005300940207     D* Nome PGM a video
005400940207     D                 DS
005500940207     D  PROGR                  1     10
005600940207     D  ASTER1                 1      1    INZ('*')
005700940207     D  SIGLA                  2      9
005800940207     D  ASTER2                10     10    INZ('*')
005900940127     D*-------------
006000940127     D* Reperimento nome PGM
006100940127     D STATUS         SDS           333
006200940127     D  DSPGM            *PROC
006300161028     D* descrizione chiavi
006400161028      *
006500161031      *---------------------------------------------------------------*
006600161031     d wDate           s               d   datfmt(*iso)  inz(*loval)
006700161031     D DATA_eur        S               D   DATFMT(*eur)
006800161031     D DATA_amg        S               D   DATFMT(*iso)
006900161031     D DATA_iso        S               D   DATFMT(*iso)
007000161031      *
007100161028     d dschiave        ds            15
007200161028     d  dskfil                 1      3
007300161028     d  dskasp                 4      4
007400161028     d  dskter                 5      5
007500161028     d  dskmas                 6      6
007600161028     d  dskaat                 7      7
007700161028     d  dskall                 8      8
007800161028      *
007900161028     D tibs02ds      E DS
008000161028      *
008100161028      * Tabella
008200161028     d dAIG          e ds                  inz
008300161028     D dTAI          E DS
008400161028     D dATS          E DS
008500161028     D dMAS          E DS
008600161028     D dAAT          E DS
008700161028     D dALL          E DS
008800030113     D*-------------
008900030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
009000030113$003 D S1NRR           S                   Like(C1rcd)
009100030113$003 D WSfl            S                   Like(C1nrr)
009200030113$003 D Wmax            S                   Like(C1rcd)
009300030113$003 D Wpag            S                   Like(C1rcd)
009400030113$003 D Winzs1          S                   Like($inzs1)
009500161028$003 D x1cod           S                   Like(h1cod)
009600160412$003 D x1cod2          S                   Like(h1cod2)
009700161028     D KCOD            S                   LIKE(TBECOD) inz('AIG')
009800110926     D KKE1            S                   LIKE(TBEKE1)
009900160412     D KKE2            S                   LIKE(TBEKE2)
010000940207     D*-------------
010100940211     D* COSTANTI
010200940211     D*-------------
010300030113     D SFLPAG          C                   CONST(11)
010400940314     D* dimensione della schiera $MS1
010500940506     D*
010600940506     D* Tasti di funzione
010700940506     D F01             C                   CONST(X'31')
010800940506     D F02             C                   CONST(X'32')
010900940506     D F03             C                   CONST(X'33')
011000940506     D F04             C                   CONST(X'34')
011100940506     D F05             C                   CONST(X'35')
011200940506     D F06             C                   CONST(X'36')
011300940506     D F07             C                   CONST(X'37')
011400940506     D F08             C                   CONST(X'38')
011500940506     D F09             C                   CONST(X'39')
011600940506     D F10             C                   CONST(X'3A')
011700940506     D F11             C                   CONST(X'3B')
011800940506     D F12             C                   CONST(X'3C')
011900940506     D F13             C                   CONST(X'B1')
012000940506     D F14             C                   CONST(X'B2')
012100940506     D F15             C                   CONST(X'B3')
012200940506     D F16             C                   CONST(X'B4')
012300940506     D F17             C                   CONST(X'B5')
012400940506     D F18             C                   CONST(X'B6')
012500940506     D F19             C                   CONST(X'B7')
012600940506     D F20             C                   CONST(X'B8')
012700940506     D F21             C                   CONST(X'B9')
012800940506     D F22             C                   CONST(X'BA')
012900940506     D F23             C                   CONST(X'BB')
013000940506     D F24             C                   CONST(X'BC')
013100940506     D ENTER           C                   CONST(X'F1')
013200940506     D ROLDWN          C                   CONST(X'F4')
013300940506     D ROLLUP          C                   CONST(X'F5')
013400940207     I*-------------
013500940607     IS1
013600161031$015 I              S1DES1                      deskey1
013601161104$015 I              S1SCAD                      datscad
013700161031$015 I              S1DES2                      deskey2
013800940127     C*----------------------------------------------------*
013900940127     C*                MAIN LINE PROGRAM
014000940127     C*----------------------------------------------------*
014100940223     C* inizializzazione variabili
014200940223     C                   EXSR      INZVAR
014300940223     C*
014400940223     C     $FINE         DOWEQ     *OFF
014500940131     C     $GEST         CASEQ     'S1'          GESS1
014600940117     C                   END
014700940117     C                   END
014800940325     C* fine programma
014900940325     C                   SETON                                            LR
015000030113     C************************************************************
015100030113     C* INIZIALIZZAZIONE VARIABILI
015200030113     C************************************************************
015300030113     C     INZVAR        BEGSR
015400030113     C*
015500030113     C* Pulizia campi e indicatori
015600030113     C                   MOVE      *ALL'0'       IN4049           10
015700030113     C                   MOVEA     IN4049        *IN(40)
015800030113     C                   CLEAR                   S1OPZ
015900030113     C* Variabili per gestione videate
016000030113     C*
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
021400940127     C* F3=Fine
021500161028     C     $TASTO        WHENEQ    F03
021600161028     C                   EXSR      F03S1
021700161028     C* F12=Ritorno
021800161028     C     $TASTO        WHENEQ    F12
021900161028     C                   EXSR      F12S1
022000940131     C* F10=Immissione
022100940506     C     $TASTO        WHENEQ    F10
022200940309     C                   EXSR      F10S1
0223009401271O   C                   OTHER
022400940127     C* CONTROLLO DATI
022500940131     C                   EXSR      CTRC1
022600940201     C     *IN99         IFEQ      *OFF
022700940131     C                   EXSR      CTRS1
022800940131     C                   END
0229009401271-   C                   ENDSL
023000940127     C*
023100940127     C                   ENDSR
023200940224     C/EJECT
023300940127     C************************************************************
023400940131     C* INIZIALIZZAZIONE LISTA
023500940127     C************************************************************
023600940127     C     INZS1         BEGSR
023700940302     C* pulizia SFL
023800940128     C                   SETOFF                                         3031
023900940607     C                   WRITE     C1
024000940128     C                   SETON                                          31
024100161028      **  filiale
024200161028     c                   if        filiale ='999'
024300161028     c                   movel(p)  'Azienda'     c1fild
024400161028     c                   movel(p)  '999'         c1fil
024500161028     c                   else
024600161028     c                   clear                   c1fild
024700161028     c                   movel(p)  filiale       c1fil
024800161028     c     c1fil         chain     azorg01l
024900161028     c                   if        %found(azorg01l)
025000161028     c                   movel     orgdes        c1fild
025100161028     c                   endif
025200161028     c                   endif
025201161104     c                   eval      rsutDES = rsut
025300161028      *
025400030113     C* CARICAMENTO SFL totale
025500940201     C                   Z-ADD     0             S1NRR
025600030113     C                   Z-ADD     1             C1RCD
025700940128     C                   Z-ADD     0             WMAX
025800120412     C                   Z-ADD     1             WPAG
025900161104     C                   CLEAR                   KKE1
026000940224     C*
026100940224     C* Posizionamento su file pilota
026200110926     c     kTBL          setll     tntbe01l
026300940608    >C                   EXSR      REDANA
026400030113     C* Carico il SFL
026500940127     C                   EXSR      ROLS1
026600030113     C*
026700030114     c                   if        xtaopr <> '1'
026800030114     C                   Z-ADD     1             WPAG
026900030114     c                   end
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
028300940127     C*
028400030113     c                   clear                   s1opz
028500161031     c                   clear                   deskey1
028501161104     c                   clear                   datscad
028600161031     c                   clear                   deskey2
028700120412     c                   clear                   Obbiettivo        3
028800161028     c                   clear                   h1cod
028900160412     c                   clear                   h1cod2
029000161028     c                   movel     tbeke1        h1cod
029100160412     c                   movel     tbeke2        h1cod2
029200120412      **
029300161028     c                   movel     tbeke1        dschiave
029400161028      **
029500161028      **  decodifica i singoli flags
029600161028      *
029700161028      **  Tipo aspettativa
029800161028     C                   CLEAR                   tibs02ds
029900161028     C                   MOVEL     KNSIF         T02SIF
030000161028     C                   MOVEL     'TAI'         T02COD
030100161028     C                   MOVEL     'C'           T02MOD
030200161028     C                   movel(p)  dskasp        T02KE1
030300161028     c                   if        dskasp <> *blank
030400161028     C                   CALL      'TIBS02R'
030500161028     C                   PARM                    KPJBA
030600161028     C                   PARM                    tibs02ds
030700161028     C     T02ERR        IFEQ      *BLANKS
030800161028     C                   MOVEL     T02UNI        dtai
030900161031     c                   movel     d§taides      v1Casp            9
031000161028     C                   ELSE
031100161028     c                   clear                   v1Casp
031200161028     C                   CLEAR                   Dtai
031300161028     C                   ENDIF
031400161028     c                   end
031500161028      *
031600161028      **  Territorio
031700161028     C                   CLEAR                   tibs02ds
031800161028     C                   MOVEL     KNSIF         T02SIF
031900161028     C                   MOVEL     'ATS'         T02COD
032000161028     C                   MOVEL     'C'           T02MOD
032100161028     C                   movel(p)  dskter        T02KE1
032200161028     c                   if        dskter <> *blank
032300161028     C                   CALL      'TIBS02R'
032400161028     C                   PARM                    KPJBA
032500161028     C                   PARM                    tibs02ds
032600161028     C     T02ERR        IFEQ      *BLANKS
032700161028     C                   MOVEL     T02UNI        dats
032800161028     c                   movel     d§atsdes      v1Cter            9
032900161028     C                   ELSE
033000161028     c                   clear                   v1Cter
033100161028     C                   ENDIF
033200161028     c                   end
033300161028      *
033400161028      **  Massa
033500161028     c     'MAS'         setll     tntbe02l
033600161028     c     'MAS'         reade     tntbe02l
033700161028     c                   dow       not %EoF(tntbe02l)
033800161028     c                   if        dskMas = %subst(T2_tbeKE2:1:1)
033900161028     C                   MOVEL     T2_tbeUNI     Dmas
034000161031     c                   movel     d§masdes      v1cMas           14
034100161028     c                   leave
034200161028     c                   end
034300161028     c     'MAS'         reade     tntbe02l
034400161028     c                   end
034500161028      *
034600161028      **  Tipo Attività
034700161028     C                   CLEAR                   tibs02ds
034800161028     C                   MOVEL     KNSIF         T02SIF
034900161028     C                   MOVEL     'AAT'         T02COD
035000161028     C                   MOVEL     'C'           T02MOD
035100161028     C                   movel(p)  dskaat        T02KE1
035200161028     c                   if        dskaat <> *blank
035300161028     C                   CALL      'TIBS02R'
035400161028     C                   PARM                    KPJBA
035500161028     C                   PARM                    tibs02ds
035600161028     C     T02ERR        IFEQ      *BLANKS
035700161028     C                   MOVEL     T02UNI        Daat
035800161028     c                   movel     d§aatdes      v1cAat            7
035900161028     C                   ELSE
036000161028     c                   clear                   v1cAat
036100161028     C                   ENDIF
036200161028     c                   end
036300161028      *
036400161028      **  Tipo Allestimento
036500161028     C                   CLEAR                   tibs02ds
036600161028     C                   MOVEL     KNSIF         T02SIF
036700161028     C                   MOVEL     'ALL'         T02COD
036800161028     C                   MOVEL     'C'           T02MOD
036900161028     C                   movel(p)  dskall        T02KE1
037000161028     C                   if        dskall <> *blank
037100161028     C                   CALL      'TIBS02R'
037200161028     C                   PARM                    KPJBA
037300161028     C                   PARM                    tibs02ds
037400161028     C     T02ERR        IFEQ      *BLANKS
037500161028     C                   MOVEL     T02UNI        Dall
037600161028     c                   movel     d§alldes      v1Call           17
037700161028     C                   ELSE
037800161028     c                   clear                   v1Call
037900161028     C                   ENDIF
038000161028     c                   end
038100161028      **    al momento lo riempio di puntini
038200161028     c                   eval      v1Call = *all'.'
038300161028      **
038400161028     c                   movel     TBEuni        dAIG
038500161028     c                   z-add     D§aigIMP      v1cIMP            9 2
038600161028     c                   z-add     D§aigQTA      v1cQTA            7 3
038700161031     c                   movel     tbeke2        data8             8 0
038800161031     c     *iso          movel     data8         data_iso
038900161031     c                   move      data_iso      data_eur
039000161031     c                   move      data_eur      datascad          8 0
039100161028      *
039200161031     c                   eval      deskey1= v1Casp + ' ' + v1Cter + ' ' +
039300161104     c                              v1Cmas + ' ' + v1cAat + '  '
039400161104     c                             + %editc(v1cIMP:'3')
039501161104     C*  Scadenza
039502161104     c                   eval      datscad= datascad
039600030113     C*
039700161031     c                   eval      deskey2= v1Call + ' '
039800161031     c                   clear                   campo16          16
039900161031     c                   eval      campo16 = 'qta.' + %editc(v1cqta:'3')
040000161031     c                   move      campo16       deskey2
040100161031     C*
040200030115     c                   clear                   S1Ann
040300940127     C*
040400161104     C*  evidenzia a rottura della prima chiave quindi con la data in vigore
040500161104     c                   setoff                                       25
040600161104     c                   if        tbeke1 <> KKE1
040700161104     c                   EVAL       KKE1 = tbeke1
040800161104     c                   setoN                                        25
040900161104     C                   END
041000161104     C*
041100161103     C                   Z-ADD     TBE_rec       h1TBErec
041200940127     C                   ADD       1             S1NRR
041300940127     C                   ADD       1             Y
041400940127     C*
041500940607     C                   WRITE     S1
041600940131     C*
041700940316    >C                   EXSR      REDANA
041800940128     C*
0419009401271-   C                   ENDDO
042000940127     C*
042100940223     C                   Z-ADD     S1NRR         WMAX                 30
042200940127     C*
042300940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
042400940127     C*
042500030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
042600940127     C                   MVR                     RESTO             3 0
042700030114     C     PAGINE        MULT      SFLPAG        C1RCD
0428000301141    C     RESTO         IFGT      0
042900030114     C                   ADD       1             C1RCD
0430000301141E   C                   ELSE
043100030114     C                   SUB       SFLPAG        C1RCD
043200030114     C                   ADD       1             C1RCD
0433000301141-   C                   ENDIF
043400940128     C*
043500940127     C                   ENDSR
043600940128     C************************************************************
043700940131     C* LETTURA RCD ARCHIVIO PILOTA
043800940128     C************************************************************
043900940607     C     REDANA        BEGSR
044000940128     C*
044100940131     C                   MOVEL     *OFF          $EFILE
044200940131     C                   MOVEL     *OFF          $RCDOK
044300940131     C*
0444009401311    C     $EFILE        DOUEQ     *ON
044500940131     C     $RCDOK        OREQ      *ON
044600940131     C*
044700110926     c     kTBL          Reade     tntbe01l
044800110926     c                   if        %eof(tntbe01l)
044900030113     C                   MOVEL     *ON           $EFILE
045000030113     C                   MOVE      $EFILE        *IN33
045100030113     c                   else
045200030113     C                   MOVE      *ON           $RCDOK
045300030113     c                   end
045400940131     C*
045500161028     C* salta se filiale diversa
045600161028     c                   movel     tbeke1        keyfil            3
045700161028     c                   if          keyfil <> filiale
045800161028     C                   MOVE      *OFF          $RCDOK
045900161028     c                   end
046000161031      **  controlla la DATA
046100161031     c                   if        tbeke2 <> ' '
046200161031     c                   movel     tbeke2        data8
046300161031     c     *iso          movel     data8         data_iso
046400161031      *in lettura se la data scadenza è minore di udate non faccio più vedere la riga
046500161031     c                   if        data_iso < wdate
046600161031     C                   MOVE      *OFF          $RCDOK
046700161031     c                   endif
046800161031     c                   end
046900161028     C*
0470009401311-   C                   ENDDO
047100940131     C*
047200940131     C                   ENDSR
047300940224     C************************************************************
047400940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
047500940224     C************************************************************
047600940224     C     CLCPAG        BEGSR
047700940224     C* Input :
047800940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
047900940224     C*          cursore
048000940224     C* - SFLPAG = numero rcd per pagina sfl
048100940224     C* Output :
048200940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
048300940224     C*
048400120412     c                   if        WSFL >0
048500940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
048600940224     C                   MVR                     RESTO             3 0
048700940224     C     RESTO         IFGT      0
048800940224     C                   ADD       1             PAGINE
048900940224     C                   ENDIF
049000940224     C     PAGINE        MULT      SFLPAG        WPAG
049100120412     c                   else
049200120412     C                   Z-ADD     1             WPAG
049300120412     c                   end
049400940224     C*
049500940224     C                   ENDSR
049600940309     C************************************************************
049700161028     C* GESTIONE F12 VIDEO S1
049800940309     C************************************************************
049900161028     C     F12S1         BEGSR
050000940309     C*
050100940309     C                   MOVE      *ON           $FINE
050200940325     C* fine programma
050300940309     C                   ENDSR
050400161028     C************************************************************
050500161028     C* GESTIONE F03 VIDEO S1
050600161028     C************************************************************
050700161028     C     F03S1         BEGSR
050800161028     C*
050900161028     C                   MOVE      *ON           $FINE
051000161028     C                   MOVE      '03'          kpjbu
051100161028     C* fine programma
051200161028     C                   ENDSR
051300940309     C/EJECT
051400940309     C************************************************************
051500940309     C* GESTIONE F10 VIDEO S1
051600940314     c* AGGIUNTA RECORD
051700940309     C************************************************************
051800940309     C     F10S1         BEGSR
051900940309     C*
052000030113     C                   RESET                   tabds
052100030113     C                   MOVEL     '01'          xtaopz
052200030113     C                   MOVE      *ZERO         xtaret
052300030113     C                   MOVE      *ZERO         xtaopr
052400161031     C                   clear                   xtake1
052500161103     C                   clear                   xtake2
052600161103     C                   move      *zeros        xtatberec
052700030113     C                   MOVE      *BLANKS       KPJBU
052800030113     C                   MOVEL     tabds         KPJBU
052900161028$004 C                   CALL      'TNTBA04R3'
053000030113     C                   PARM                    KPJBA
053100030114     C                   MOVEL     KPJBU         tabds
053200030113      *
053300940309     C* ritorno da PGM gestione
053400940309     C                   EXSR      GESRET
053500940309     C     WINZS1        IFEQ      *ON
053600940309     C                   MOVE      *ON           $INZS1
053700940309     C* se esistevano già righe sul sfl
053800940309     C* calcolo pagina a cui deve posizionarsi
053900940309     C     WSFL          IFGT      0
054000940309     C                   EXSR      CLCPAG
054100940309     C* altrimenti carico solo la 1a pagina
054200940309     C                   ELSE
054300940309     C                   Z-ADD     SFLPAG        WPAG
054400940309     C                   ENDIF
054500940309     C                   ENDIF
054600940309     C*
054700940309     C                   ENDSR
054800940128     C************************************************************
054900940131     C* CONTROLLO TESTATA LISTA
055000940128     C************************************************************
055100940131     C     CTRC1         BEGSR
055200940128     C*
055300940201     C                   MOVE      *OFF          *IN99
055400940131     C*
055500940202     C                   ENDSR
055600940131     C************************************************************
055700940131     C* CONTROLLO OPZIONI LISTA
055800940131     C************************************************************
055900940131     C     CTRS1         BEGSR
056000940131     C*
056100940202     C                   MOVEL     *OFF          $ESCI
056200940201     C                   SETOFF                                       99
056300940131     C                   Z-ADD     0             S1OPZ
056400040930     c                   clear                   x1cod
056500160412     c                   clear                   h1cod2
056600940131     C*
056700940127     C* Leggo il sfl solo se ci sono rcd
0568009401311    C     WMAX          IFGT      0
056900940607     C                   READC     S1                                     21
057000940127     C*
057100940131     C* esce se fine sfl o errore che richiede l'uscita
0572009401312    C     *IN21         DOWEQ     *OFF
057300940131     C     $ESCI         ANDEQ     *OFF
057400940201     C                   Z-ADD     S1NRR         C1RCD
057500940131     C* ctrl su riga
057600940131     C                   EXSR      RECS1
057700940131     C* gestione opzioni
0578009401313    C     S1OPZ         IFNE      0
057900940201     C     *IN99         ANDEQ     *OFF
058000940131     C                   EXSR      OPZS1
0581009401273-   C                   ENDIF
058200940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0583009402013    C     *IN99         IFEQ      *ON
058400940201     C     $ESCI         OREQ      *ON
058500940131     C                   MOVE      *ON           *IN32
058600940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
058700940204     C* la ripristinerò a conclusione del ciclo di READC
058800940223     C                   MOVE      *OFF          $INZS1
0589009402233-   C                   ENDIF
059000940223     C*
059100940131     C                   Z-ADD     0             S1OPZ
059200940223     C*
059300940607     C                   UPDATE    S1
059400940223     C*
059500940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0596009401313    C     $ESCI         IFEQ      *OFF
059700940607     C                   READC     S1                                     21
059800940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0599009402014    C     *IN21         IFEQ      *ON
060000940201     C                   MOVE      WINZS1        $INZS1
060100940204     C* calcolo pagina a cui deve posizionarsi
060200940224     C                   EXSR      CLCPAG
0603009402014-   C                   ENDIF
0604009402013-   C                   ENDIF
060500940131     C*
0606009401272-   C                   ENDDO
060700940127     C*
0608009401311-   C                   ENDIF
060900940131     C*
061000940127     C                   ENDSR
061100940127     C/EJECT
061200940127     C************************************************************
061300940131     C* CONTROLLO CAMPI I/O RIGA LISTA
061400940127     C************************************************************
061500940131     C     RECS1         BEGSR
061600940131     C*
061700940201     C* reset indicatori DSPATR
061800940201     C                   MOVE      *ALL'0'       IN4049           10
061900940201     C                   MOVEA     IN4049        *IN(40)
062000940201     C*
062100040930     C*  se richiamato x selezionare un codice 11 = *on
062200040930     C*   non deve utilizzare le opzioni di modifica
062300040930     C*  e viceversa
062400040930     c     *in11         ifeq      *on
0625000409303    C     S1OPZ         IFeq      2
0626000409303    C     S1OPZ         OReq      3
0627000409303    C     S1OPZ         OReq      4
062800040930     c                   seton                                        41  99
062900040930     C                   END
063000040930      * selezionato un codice da restituire al chiamante
0631000409303    C     S1OPZ         IFeq      1
063200161028     c                   movel     h1cod         x1cod
063300160412     c                   movel     h1cod2        x1cod2
063400161103     c                   move      h1TBErec      xtatberec
063500040930     c                   clear                   kpjbu
063600160412     c                   eval      kpjbu = x1cod + x1cod2
063700040930     C                   MOVEL     *ON           $ESCI
063800040930     C                   MOVEL     *ON           $fine
063900040930     C                   END
064000040930     C                   END
064100040930     C*
064200040930     c     *in11         ifeq      *off
0643000409303    C     S1OPZ         ANDeq     1
064400040930     c                   seton                                        41  99
064500040930     C                   END
064600040930     C*
064700940131     C                   ENDSR
064800940131     C************************************************************
064900940131     C* GESTIONE OPZIONI LISTA
065000940131     C************************************************************
065100940131     C     OPZS1         BEGSR
065200940201     C*
0653000409302    C     *IN11         IFEQ      *Off
0654000409302    C     *IN11         oreq      *On
0655000409302    C     s1opz         andeq     05
065600040930     C*
065700030113     C                   RESET                   tabds
065800110902     C                   MOVE      *all'0'       xtaopz
065900110902     C                   MOVE      S1OPZ         xtaopz
066000030113     C                   MOVE      *ZERO         xtaret
066100030113     C                   MOVE      *ZERO         xtaopr
066200161028     C                   MOVEl     h1COD         xtake1
066300160412     C                   MOVEl     H1COD2        xtake2
066400161103     C                   MOVE      H1tbeREC      xtatbeREC
066500940715     C                   MOVE      *BLANKS       KPJBU
066600030113     C                   MOVEL     tabds         KPJBU
066700161028$004 C                   CALL      'TNTBA04R3'
066800940607     C                   PARM                    KPJBA
066900030114     C                   MOVEL     KPJBU         tabds
067000940201     C*
067100940223     C* ritorno da PGM gestione
067200940223     C                   EXSR      GESRET
067300040930     C*
067400940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0675009402252    C     *IN99         IFEQ      *ON
067600940223     C                   SETON                                        40
0677009402252-   C                   ENDIF
067800940225     C*
0679000409302-   C                   ENDIF
068000040930     C*
068100940131     C                   ENDSR
068200940223     C/EJECT
068300940223     C************************************************************
068400940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
068500940223     C************************************************************
068600940223     C     GESRET        BEGSR
068700940223     C*
068800940223     C* modo di ritorno
068900940223     C*
0690009402231    C                   SELECT
069100940314    >C* << questi modi di utilizzo dei valori di ritorno dal
069200940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
069300940314    >C*    proposte, normalmente sempre valide, ma modificabili
069400940314    >C*    per situazioni particolari >>
069500940223     C* 1 = F3
069600030113    >C     xtaret        WHENEQ    '1'
069700940224     C                   MOVE      *ON           $ESCI
069800940223     C                   MOVE      *ON           $FINE
069900940223     C* 2 = F12
070000030113    >C     xtaret        WHENEQ    '2'
070100940223     C                   MOVE      *ON           $ESCI
070200940223     C*
0703009402231-   C                   ENDSL
070400940223     C*
070500940223     C* operazione eseguite dal pgm chiamato
070600940223     C*
0707009402231    C                   SELECT
070800940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
070900030113    >C     xtaopr        WHENEQ    '1'
071000940223     C                   MOVE      *ON           WINZS1
071100940223     C*
0712009402231-   C                   ENDSL
071300940223     C*
071400940223     C* funzione non eseguibile per errore :
071500940223     C*
0716009402231    C                   SELECT
071700940223     C* 1 = funzione richiamata chiusa in errore
071800940316    >C*  eventualmente gestire altri codici di errore
071900030113    >C     xtaerr        WHENEQ    '1'
072000940223     C                   MOVE      *ON           $ESCI
072100940223     C                   SETON                                        5299
072200940223     C*
0723009402231-   C                   ENDSL
072400940223     C*
072500940223     C                   ENDSR
072600940223     C/EJECT
072700940131     C************************************************************
072800940131     C* OPERAZIONI INIZIALI
072900940131     C************************************************************
073000940131     C     *INZSR        BEGSR
073100030113     C*
073200030113     C* Reperimento parametri
073300030113     C     *ENTRY        PLIST
073400030113     C                   PARM                    KPJBA
073500161028     C                   movel     KPJBU         Filiale           3
073600161028     C                   clear                   kpjbu
073700040930     C*
073800040930     C* Se chiamato x selezionare un codice
073900040930     c                   clear                   x1cod
074000160412     c                   clear                   x1cod2
074100161028     C*
074200161031     c                   movel     *date         Wdate
074300161031     C*
074400161028     c                   clear                   selez             1
074500040930     c                   if        selez = '?'
074600040930     c                   seton                                        11
074700040930     c                   end
074800030113     C*
074900030113     C* Variabili per gestione videate
075000030113     C                   MOVE      *BLANK        $GEST             2
075100030113     C                   MOVE      *BLANK        $FINE             1
075200030113     C                   MOVE      *BLANK        $INZS1            1
075300030113     C                   MOVE      *BLANK        $EFILE            1
075400030113     C                   MOVE      *BLANK        $ESCI             1
075500030113     C                   MOVE      *BLANK        $RCDOK            1
075600030113     C*
075700030113     C     KTBL          KLIST
075800110926     C                   KFLD                    KCOD
075900030113     C* Indici
076000030113     C                   Z-ADD     0             X                 3 0
076100030113     C                   Z-ADD     0             Y                 3 0
076200940506     C*
076300161028     C                   move      'AIG'         KCOD
076301161104      *
076302161104      * Reperisco le aree dati necessarie (TUTTE IN UNA VOLTA SOLA)
076303161104     c     *dtaara       define    §azute        AZUTEds
076304161104     c     *dtaara       define    §datiute      dDatiUte
076305161104      *
076306161104     c                   clear                   AZUTEds
076307161104     c                   clear                   dDatiUte
076308161104     c                   clear                   TIBS34ds
076309161104     c                   in(E)     *dtaara
076310161104if  1c                   if        %Error  or  RSUT = *blanks
076311161104     c                   call      'TIBS34R'
076312161104     c                   parm                    TIBS34ds
076313161104     c                   in        *dtaara
076314161104e   1c                   endif
076316161104      *
076317161104      *-- Verifica errori e autorità profilo
076318161104sel 1c                   SELECT
076319161104      *-- controllo se ho errori nei dati utente
076320161104      *--   nel qual caso NON risulta un profilo abilitato
076321161104w   1c                   WHEN      DUTerr  = 'E'
076322161104     c                   eval      $Fine   = *on
076323161104      *
076324161104      *-- CONTROLLO AUTORITA'
076325161104      *--  POSSIBILE SOLO SULL'AS DI SEDE (UTEAUT <> *blank)
076326161104      *-- se il chiamante non richiede autorità specifica verificare
076327161104      *--   quella del profilo
076328161104      *-- se il chiamante richiede autorità specifica verificarla,
076329161104      *--  se è blank verificare quella del profilo
076330161104      *
076331161104      * se UTEAUT = *BLANK non siamo in sede
076332161104w   1c                   WHEN      UTEaut  = *blank
076333161104     c                   eval      $Fine   = *on
076334161104      *
076335161104x   1c                   OTHER
076336161104      *
076337161104e   1c                   ENDSL
076400940127     C*
076500940117     C                   ENDSR
076600030113     C************************************************************
