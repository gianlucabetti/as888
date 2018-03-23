000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
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
002100090312     Fazorg01l  if   E           k disk
002200090313     Faitrs01l  if   E           k disk    infds(dsaitrs)
002300090311      *
002400090826     FTRMZ65SD  CF   E             WORKSTN
002500940607     F                                     SFILE(S1:S1NRR)
002600940201     F                                     INFDS(DSFMT)
002700940128     D*----------------------------------------------------*
002800940211     D* Passaggio Parametri
002900940211     D KPJBA         E DS
003000090408     D KPJBus          s                   like(kpjbu)
003100090311     D anRCO98j      E DS
003200090826     D trMZ70s1DS    E DS
003300090311      *-------------
003400090311     D dsaitrs         DS
003500090311     D  rrnaitrs             397    400b 0
003600030113      *-------------
003700940325     D* Parametri in ricezione
003800090311     D xTABDS          DS
003900090311     D  XTAOPZ                 1      1
004000090311     D  XTAFLG                 2      2
004100030113     D  XTARET                 3      3
004200030113     D  XTAOPR                 4      4
004300030113     D  XTAERR                 5      5
004400090319     D  XTArecord              6     14  0
004500090319     D  XTAKEY                15     21  0
004600940211     D*-------------
004700940211     D DSFMT           DS           512
004800940506     D  $TASTO               369    369
004900940211     D  NRG                  370    370
005000940211     D  NCL                  371    371
005100940211     D  SFLNRR               378    379B 0
005200940207     D*-------------
005300940127     D* Reperimento nome PGM
005400940127     D STATUS         SDS           333
005500090312     D* Nome PGM a video
005600940127     D  DSPGM            *PROC
005700090311     D*-------------
005800090317     D sav_c1exl       S                   Like(c1exl) inz('S')
005900090313     D sav_c1fil       S                   Like(c1fil)
006000090313     D sav_c1soc       S                   Like(c1soc)
006100090316     D sav_pos         S                   Like(c1pos)
006200090416     D sav_pos_x_INZ   S                   Like(c1pos)
006300090316     D posizionamento  S              1    inz('N')
006400090316     D Wlen            S              3i 0
006500090316     D Wpos            S              3i 0
006600090316$003 D POS_C1RCD       S                   Like(C1rcd)
006700090316     D sav_cerca       S                   Like(c1cerca)
006800090311      *
006900090311     D dataiso         s               d   datfmt(*iso)
007000090311     D dataeur         s               d   datfmt(*eur)
007100090311     D dataDMY         s               d   datfmt(*dmy)
007200090311      *
007300030113     D*-------------
007400030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
007500030113$003 D S1NRR           S                   Like(C1rcd)
007600030113$003 D WSfl            S                   Like(C1nrr)
007700030113$003 D Wmax            S                   Like(C1rcd)
007800030113$003 D Wpag            S                   Like(C1rcd)
007900030113$003 D Winzs1          S                   Like($inzs1)
008000940207     D*-------------
008100940211     D* COSTANTI
008200940211     D*-------------
008300090326     D SFLPAG          C                   CONST(11)
008400940314     D* dimensione della schiera $MS1
008500940506     D*
008600940506     D* Tasti di funzione
008700940506     D F01             C                   CONST(X'31')
008800940506     D F02             C                   CONST(X'32')
008900940506     D F03             C                   CONST(X'33')
009000940506     D F04             C                   CONST(X'34')
009100940506     D F05             C                   CONST(X'35')
009200940506     D F06             C                   CONST(X'36')
009300940506     D F07             C                   CONST(X'37')
009400940506     D F08             C                   CONST(X'38')
009500940506     D F09             C                   CONST(X'39')
009600940506     D F10             C                   CONST(X'3A')
009700940506     D F11             C                   CONST(X'3B')
009800940506     D F12             C                   CONST(X'3C')
009900940506     D F13             C                   CONST(X'B1')
010000940506     D F14             C                   CONST(X'B2')
010100940506     D F15             C                   CONST(X'B3')
010200940506     D F16             C                   CONST(X'B4')
010300940506     D F17             C                   CONST(X'B5')
010400940506     D F18             C                   CONST(X'B6')
010500940506     D F19             C                   CONST(X'B7')
010600940506     D F20             C                   CONST(X'B8')
010700940506     D F21             C                   CONST(X'B9')
010800940506     D F22             C                   CONST(X'BA')
010900940506     D F23             C                   CONST(X'BB')
011000940506     D F24             C                   CONST(X'BC')
011100940506     D ENTER           C                   CONST(X'F1')
011200940506     D ROLDWN          C                   CONST(X'F4')
011300940506     D ROLLUP          C                   CONST(X'F5')
011400090311      **********************************************************************
011500090311      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
011600090311      **********************************************************************
011700090311      *
011800090311      * ?Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
011900090316     D MaxKey          C                   1
012000090316$xxx D Ord_Ragsoc      C                   '1'
012100090311     D Ascendente      C                   1
012200090311     D Discendente     C                   2
012300090311     D Carattere       C                   6
012400090311     D Put             C                   1
012500090311     D EndPut          C                   2
012600090311     D Get             C                   3
012700090311     D Numerico        C                   8
012800090311      **********************************************************************
012900090311      * Campi utili:
013000090311      *     RRN1       - Numero relativo di record del Subfile
013100090311      *     SizeList   - Dimensione della lista
013200090311      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamen
013300090311      **********************************************************************
013400090311     D Rrn1            S              5I 0
013500090311     D NotUsed         S             16A
013600090311     D ReturnSize      S              9B 0
013700090311     D SizeList        S              9B 0
013800090311     D RrnLast         S              5I 0
013900090316     D WrkSort         S              1    inz('1')
014000090311      **********************************************************************
014100090311      * Data Structures
014200090311      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
014300090311      *     QLGSCB     - The sort request block for the QLGSORT API
014400090311      *     QLGSCB00   - The sort request block for the QLGSRTIO API
014500090311      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
014600090311      *     QUSEC      - Error structure for the QLGSORT API
014700090311      **********************************************************************
014800090311     D SflRcd          DS
014900090311     D  S1des
015000090311     D  S1socG
015100090311     D  S1fil
015200090316     D  S1dec
015300090316     D  S1dfc
015400090311     D  S1opz
015500090316     d  H1NRC
015600090323     d  H1NRRTRS
015700090706     d  H1DRC
015800090311     D  Selected                      1A
015900090311
016000090311      /COPY QSYSINC/QRPGLESRC,QLGSORT
016100090311     D QLGKL                         16    DIM(MaxKey)
016200090311     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
016300090311     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
016400090311     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
016500090311     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
016600090311
016700090311      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
016800090311      /COPY QSYSINC/QRPGLESRC,QUSEC
016900090316
017000090316     ***************************************************************************
017100090316     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
017200090316     D ESITO_OK...
017300090316     D                 C                   0
017400090316     ***************************************************************************
017500090316     **
017600090316     ** Dichiarazione prototipi procedure esterne.
017700090316     **
017800090316     ***************************************************************************
017900090316      /DEFINE DFTACTGRP_YES
018000090316     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
018100090316      /UNDEFINE DFTACTGRP_YES
018200090316     ***************************************************************************
018300090316     **
018400090316     ** Definizione strutture dati.
018500090316     **
018600090316     ***************************************************************************
018700090316     D tibsSocI0...
018800090316     D               E DS                  QUALIFIED
018900090316     D                                     INZ
019000090316     D tibsSocO0...
019100090316     D               E DS                  QUALIFIED
019200090316     D                                     INZ
019300090316     ***************************************************************************
019400090316     **
019500090316     ** Definizione variabili modulo/programma.
019600090316     **
019700090316     ***************************************************************************
019800090316     D prmRqsOpCode...
019900090316     D                 S             10A
020000090316     D prmRpyOpCode...
020100090316     D                 S             10A
020200090316     D prmRpyIdMsg...
020300090316     D                 S             10I 0
020400090316     D prmRqsFormato...
020500090316     D                 S             10A
020600090316     D prmRqsDataSize...
020700090316     D                 S             10I 0
020800090316     D prmRpyFormato...
020900090316     D                 S             10A
021000090316     D prmRpyDataSize...
021100090316     D                 S             10I 0
021200090316
021300940127     C*----------------------------------------------------*
021400090316      *?     MAIN LINE PROGRAM     ?
021500940127     C*----------------------------------------------------*
021600940223     C* inizializzazione variabili
021700940223     C                   EXSR      INZVAR
021800940223     C*
021900940223     C     $FINE         DOWEQ     *OFF
022000940131     C     $GEST         CASEQ     'S1'          GESS1
022100940117     C                   END
022200940117     C                   END
022300090317     C*
022400090317     ** Chiudo il programma.
022500090317     C                   CALL      'TIBSSOCR'
022600090317     C                   PARM      'FINALIZE'    prmRqsOpCode
022700090317     C                   PARM                    prmRpyOpCode
022800090317     C                   PARM                    prmRpyIdMsg
022900940325     C* fine programma
023000940325     C                   SETON                                            LR
023100030113     C************************************************************
023200030113     C* INIZIALIZZAZIONE VARIABILI
023300030113     C************************************************************
023400030113     C     INZVAR        BEGSR
023500030113     C*
023600030113     C* Pulizia campi e indicatori
023700030113     C                   MOVE      *ALL'0'       IN4049           10
023800030113     C                   MOVEA     IN4049        *IN(40)
023900030113     C                   CLEAR                   S1OPZ
024000030113     C* Variabili per gestione videate
024100030113     C*
024200030113     C                   MOVE      *OFF          $FINE
024300030113     C                   MOVE      *OFF          $INZS1
024400030113     C                   MOVE      *OFF          $EFILE
024500030113     C                   MOVE      *OFF          $ESCI
024600030113     C                   MOVE      *OFF          $RCDOK
024700030113     C                   Z-ADD     0             $ULKS1            3 0
024800030113     C*
024900030113     C                   MOVE      *ON           $INZS1
025000030113     C                   MOVE      'S1'          $GEST
025100090316     C                   clear                   c1pos
025200090316     C                   clear                   sav_pos
025300030113     C*
025400030113     C* Variabili appoggio
025500030114     C                   Z-ADD     1             WPAG
025600030113     c*
025700030113     C                   ENDSR
025800940127     C************************************************************
025900940131     C* GESTIONE LISTA
026000940127     C************************************************************
026100940127     C     GESS1         BEGSR
026200030113     C*
026300940223     C* inizializzazione videata
026400940223     C     $INZS1        IFEQ      *ON
026500940127     C                   EXSR      INZS1
026600940223     C                   MOVE      *OFF          $INZS1
026700940127     C                   ENDIF
026800030113     C*
026900030113     C* emissione piede videata
027000030113     C                   WRITE     Z1
027100030113     C* Non ci sono records
027200940223     C     WMAX          IFEQ      0
027300940607     C                   WRITE     D1
027400090316     c                   setoff                                       30
027500030114     C                   Else
027600090316     c                   seton                                        30
027700090316      *
027800090316      * se si è effettuato un Posizionamento
027900090316     C                   IF        POS_c1rcd  > 0  and
028000090316     C                             POS_c1rcd  <= Wmax
028100090316     C                   Z-ADD     POS_c1rcd     C1RCD
028200090316     C                   Z-ADD     POS_c1rcd     C1nrr
028300090316     c                   else
028400030114     C     Wsfl          IFgt      0
028500030114     C                   Z-ADD     wsfl          C1RCD
028600030114     C                   Else
028700030114     C     Wpag          IFgt      0
028800030114     C                   Z-ADD     wpag          C1RCD
028900030114     C                   EndIF
029000090316     C                   EndIF
029100030114     C                   EndIF
029200030114     C                   ENDIF
029300090316      *
029400940127     C*
029500030113     C*              *------------------*
029600940607     C                   EXFMT     C1
029700030113     C*              *------------------*
029800030113     C*
029900940204     C     C1NRR         IFNE      0
030000940204     C                   Z-ADD     C1NRR         WSFL
030100090313     C                   Else
030200090313     C                   Z-ADD     SFLNRR        wsfl
030300090313     C                   ENDIF
030400090313      *
030500940127     C                   Z-ADD     SFLNRR        C1RCD
030600030113     C* Selezioni
0307009401271    C                   SELECT
030800940127     C* F3=Fine
030900940506     C     $TASTO        WHENEQ    F03
031000940309     C                   EXSR      F03S1
031100090313     C* F5=Ricarica SFL
031200090313     C     $TASTO        WHENEQ    F05
031300090313     C                   EXSR      F05S1
031400090311     C* F12=Ritorno
031500090311     C     $TASTO        WHENEQ    F12
031600090311     C                   EXSR      F12S1
0317009401271O   C                   OTHER
031800940127     C* CONTROLLO DATI
031900940131     C                   EXSR      CTRC1
032000940201     C     *IN99         IFEQ      *OFF
032100940131     C                   EXSR      CTRS1
032200940131     C                   END
0323009401271-   C                   ENDSL
032400940127     C*
032500940127     C                   ENDSR
032600940224     C/EJECT
032700940127     C************************************************************
032800940131     C* INIZIALIZZAZIONE LISTA
032900940127     C************************************************************
033000090312     C     INZC1         BEGSR
033100940302     C* pulizia SFL
033200090312     C                   SETOFF                                         3031
033300090312     C                   WRITE     C1
033400090312     C                   SETON                                          31
033500090312     C*
033600090312     C* CARICAMENTO SFL totale
033700090312     C                   Z-ADD     1             C1RCD
033800090312     c                   movel     DSPGM         c1pgm
033900090312     c                   movel     knsif         c1sif
034000090317     c                   movel     'S'           c1exl
034100090313     c                   if        sav_c1exl <> c1exl
034200090313     c                   eval      c1exl = sav_c1exl
034300090313     c                   end
034400090312     C*
034500090312     C                   ENDSR
034600090312     C/EJECT
034700090312     C************************************************************
034800090312     C* INIZIALIZZAZIONE LISTA
034900090312     C************************************************************
035000090312     C     INZS1         BEGSR
035100090312     C*
035200090312     C* pulizia SFL
035300090312    >C                   EXSR      INZC1
035400090312     C*
035500090312     C                   Z-ADD     0             S1NRR
035600090312     C                   Z-ADD     0             WMAX
035700090312     C                   Z-ADD     0             wsfl
035800940224     C*
035900940224     C* Posizionamento su file pilota
036000090313     c     *loval        setll     aitrs01l
036100940608    >C                   EXSR      REDANA
036200030113     C* Carico il SFL
036300940127     C                   EXSR      ROLS1
036400030113     C*
036500030114     c                   if        xtaopr <> '1'
036600030114     C                   Z-ADD     1             WPAG
036700030114     c                   end
036800940127     C*
036900940127     C                   ENDSR
037000940127     C************************************************************
037100940131     C* CARICAMENTO PAGINA LISTA
037200940127     C************************************************************
037300940127     C     ROLS1         BEGSR
037400940127     C*
037500940128     C                   SETOFF                                       32
037600940223     C                   Z-ADD     0             Y
037700940127     C                   Z-ADD     WMAX          S1NRR
037800090316     c                   clear                   Wpos
037900940127     C*
038000940127     C* Leggo dal file anagrafico per caricare la lista
0381009401311    C     $EFILE        DOWEQ     *OFF
038200940127     C*
038300030113     c                   clear                   s1opz
038400090311     C*
038500090316     c                   exsr      Decod_Rag_IVA
038600090316     C*
038700090316     C*   Ricerca x Stringa attivata
038800090316     c                   if        c1cerca <> *blank
038900090316     c                   eval      WLen = %len(%Trim(c1cerca))
039000090316     c                   eval      WPos =
039100090316     c                              %scan(%subst(c1cerca:1:WLen) : RagSocKSC)
039200090316     c                   end
039300090316     C*
039400090316     c                   if        sav_pos <=  RagSocKSC and sav_pos <> *blank
039500090316     c                             or
039600090316     c                             sav_pos  =  *blank
039700090316     C*
039800090316     c                   if        Wpos > 0 and c1cerca <> *BLANK
039900090316     c                             or
040000090316     c                             C1CERCA = *BLANK
040100090316     C*
040200090311     c                   eval      s1des   = RagSocKSC
040300090316     c                   eval      s1socG  = TRSSOCG
040400090316     c                   eval      s1fil   = TRSFIL
040500090316     c                   eval      h1NRC   = TRSNRC
040600090319     c                   eval      h1NRRtrs= rrnaitrs
040700090706     c                   eval      h1DRC   = trsDRC
040800090311     c*
040900090316     c                   z-add     0             s1dec
041000090316     c                   if        TRSDEC  > 0
041100090316     c                   move      TRSDEC        dataiso
041200090311     c                   move      dataiso       dataDMY
041300090316     c                   move      dataDMY       S1Dec
041400090311     c                   end
041500090311     c*
041600090316     c                   z-add     0             s1dfc
041700090316     c                   if        TRSDFC     > 0
041800090316     c                   move      TRSDFC        dataiso
041900090311     c                   move      dataiso       dataDMY
042000090316     c                   move      dataDMY       S1DFC
042100090311     c                   end
042200940127     C*
042300940127     C                   ADD       1             S1NRR
042400090311     C                   EVAL      RrnLast = S1nrr
042500940127     C                   ADD       1             Y
042600940127     C*
042700940607     C                   WRITE     S1
042800090316     c                   end
042900090313     C*
043000090313     C* esce x il max. records consentiti
043100090313     c                   if        s1nrr = 9999
043200090313     c                   leave
043300090313     c                   end
043400090316     c                   end
043500940131     C*
043600940316    >C                   EXSR      REDANA
043700940128     C*
0438009401271-   C                   ENDDO
043900940127     C*
044000940223     C                   Z-ADD     S1NRR         WMAX                 30
044100090311     C*
044200090311     C* Ordinamento SFL x RAG.SOCIALE
044300090311     C                   EXSR      Ordina_S1
044400090316     c*
044500090416     c                   if        sav_pos_x_INZ <> *blank
044600090416     c                   eval      sav_pos = sav_pos_x_INZ
044700090416     c                   eval      posizionamento = 'S'
044800090416     c                   clear                   sav_pos_x_INZ
044900090416     c                   end
045000090316     c* Posizionamento
045100090316     c                   if        posizionamento = 'S'
045200090316     c                   eval      posizionamento = 'N'
045300090316     C                   EXSR      INIZIA_CON
045400090316     c                   end
045500090316     C*----------
045600090316     C*  Reimposta i puntamenti e le ricerche
045700090316     c                   eval      c1pos   = sav_pos
045800090316     c                   eval      c1cerca = sav_cerca
045900090316     C*
046000940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
046100030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
046200940127     C                   MVR                     RESTO             3 0
046300030114     C     PAGINE        MULT      SFLPAG        C1RCD
0464000301141    C     RESTO         IFGT      0
046500030114     C                   ADD       1             C1RCD
0466000301141E   C                   ELSE
046700030114     C                   SUB       SFLPAG        C1RCD
046800030114     C                   ADD       1             C1RCD
0469000301141-   C                   ENDIF
047000940128     C*
047100940127     C                   ENDSR
047200940128     C************************************************************
047300940131     C* LETTURA RCD ARCHIVIO PILOTA
047400940128     C************************************************************
047500940607     C     REDANA        BEGSR
047600940128     C*
047700940131     C                   MOVEL     *OFF          $EFILE
047800940131     C*
0479009401311    C     $EFILE        DOUEQ     *ON
048000090313     C                   MOVEL     *ON           $RCDOK
048100090313     c                   Read      AITRS01L
048200090311     C*
048300090313     c                   if        %eof(AITRS01L)
048400030113     C                   MOVEL     *ON           $EFILE
048500090313     C                   MOVEL     *OFF          $RCDOK
048600030113     C                   MOVE      $EFILE        *IN33
048700030113     c                   else
048800090318      *  Esclude gli annullati
048900090318     c                   if         trsANN <>' '
049000090318     C                   MOVE      *OFF          $RCDOK
049100090318     c                   end
049200090311      *
049300090311      * O tutte le società oppure Escluse quelle che hanno una fine Contratto os
049400090311      *   solo quelle in essere
049500090316     c                   if        sav_c1EXL = 'S' and TRSDFC > 0
049600090313     C                   MOVE      *OFF          $RCDOK
049700030113     c                   end
049800090313      *
049900090312     C*  società filtrante
050000090313     c                   if        sav_c1soc <> *blank and
050100090316     c                             sav_c1soc <> TRSSOCG
050200090313     C                   MOVE      *OFF          $RCDOK
050300090312     c                   end
050400090312     C*
050500090312     C*  filiale filtrante
050600090313     c                   if        sav_c1fil > *zeros and
050700090316     c                             sav_c1fil <> TRSFIL
050800090313     C                   MOVE      *OFF          $RCDOK
050900090312     c                   end
051000090313     C*
051100090313     C*  senza P.IVA
051200090316     c                   if        TRSIVA = *blank
051300090313     C                   MOVE      *OFF          $RCDOK
051400090313     c                   end
051500090313     C*
051600090313     C* se il record è giusto esce dal ciclo
051700090313     C     $RCDOK        ifeq      *On
051800090313     c                   Leave
051900090313     c                   end
052000090312     C*
052100090311     c                   end
052200090311     C*
0523009401311-   C                   ENDDO
052400940131     C*
052500940131     C                   ENDSR
052600940224     C************************************************************
052700940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
052800940224     C************************************************************
052900940224     C     CLCPAG        BEGSR
053000940224     C* Input :
053100940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
053200940224     C*          cursore
053300940224     C* - SFLPAG = numero rcd per pagina sfl
053400940224     C* Output :
053500940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
053600940224     C*
053700940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
053800940224     C                   MVR                     RESTO             3 0
053900940224     C     RESTO         IFGT      0
054000940224     C                   ADD       1             PAGINE
054100940224     C                   ENDIF
054200940224     C     PAGINE        MULT      SFLPAG        WPAG
054300940224     C*
054400940224     C                   ENDSR
054500090311     C************************************************************
054600090311     C* GESTIONE F03 VIDEO S1
054700090311     C************************************************************
054800090311     C     F03S1         BEGSR
054900090311     C*
055000940309     C                   MOVE      *ON           $FINE
055100940325     C* fine programma
055200940309     C                   ENDSR
055300940309     C/EJECT
055400090313     C************************************************************
055500090313     C* GESTIONE F05 ricarice S1
055600090313     C************************************************************
055700090313     C     F05S1         BEGSR
055800090313     C*
055900090313     C                   eval      $INZS1 = *ON
056000090313     C                   MOVE      'S1'          $GEST
056100090313     C* fine programma
056200090313     C                   ENDSR
056300090313     C/EJECT
056400090311     C************************************************************
056500090311     C* GESTIONE F12 VIDEO S1
056600090311     C************************************************************
056700090311     C     F12S1         BEGSR
056800090311     C*
056900090311     C                   MOVE      *ON           $FINE
057000090311     C* fine programma
057100090311     C                   ENDSR
057200090311     C/EJECT
057300940128     C************************************************************
057400940131     C* CONTROLLO TESTATA LISTA
057500940128     C************************************************************
057600940131     C     CTRC1         BEGSR
057700940128     C*
057800940201     C                   MOVE      *OFF          *IN99
057900090312     c                   eval      posizionamento = 'N'
058000090316     c                   clear                   pos_c1rcd
058100090312      *  Posizionamento
058200090316     c                   if          sav_pos <> c1pos
058300090316     c                   eval      sav_pos = c1pos
058400090312     c                   eval      posizionamento = 'S'
058500090316     C                   exsr      INIZIA_CON
058600090316     C                   End
058700090312      *
058800090316     c                   if        sav_cerca <> c1cerca
058900090316     c                             or
059000090316     c                             c1cerca <> *blank
059100090316     c                   eval      sav_cerca = c1cerca
059200090316     C                   eval      $INZS1 = *ON
059300090316     C                   MOVE      'S1'          $GEST
059400090316     C                   End
059500090316      *
059600090312     C*  Decodifiche se inseriti Società o Filiale
059700090312     c                   clear                   c1dfil
059800090312     c                   if        c1fil > *zeros
059900090312     c     c1fil         chain     azorg01l
060000090312     c                   if        %Found(azorg01l)
060100090312     c                   eval      c1dfil = orgdes
060200090312     c                   end
060300090312     c                   end
060400090312     C*
060500090312     c                   clear                   c1dsoc
060600090312     c                   if        c1soc <> *blank
060700090316     C*
060800090316     ** Chiamo passando un id società che esiste.
060900090316     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
061000090316     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
061100090316     C                   RESET                   tibsSocI0
061200090316     C                   EVAL      tibsSocI0.idSocieta = c1soc
061300090316     C
061400090316     C                   CALL      'TIBSSOCR'
061500090316     C                   PARM      'GETANAGRAF'  prmRqsOpCode
061600090316     C                   PARM      *BLANK        prmRpyOpCode
061700090316     C                   PARM      *ZERO         prmRpyIdMsg
061800090316     C                   PARM      'TIBSSOCI0'   prmRqsFormato
061900090316     C                   PARM                    tibsSocI0
062000090316     C                   PARM                    prmRqsDataSize
062100090316     C                   PARM      'TIBSSOCO0'   prmRpyFormato
062200090316     C                   PARM                    tibsSocO0
062300090316     C                   PARM                    prmRpyDataSize
062400090316
062500090324     c                   if         PRMRPYIDMSG >= 0 and
062600090324     c                              TIBSSOCO0.IDSOCIETA <> *blank
062700090316     c                   eval      c1dsoc = tibsSocO0.RAGSOCIALE
062800090323     c                   else
062900090324     c                   eval      c1dsoc = 'Società Errata'
063000090323     c                   end
063100090316
063200090312     c                   end
063300090312     C*
063400090416     c                   clear                   sav_pos_x_INZ
063500090313     C*  Se cambiato
063600090313     C                   if        c1fil <> sav_c1fil  or
063700090313     c                             c1soc <> sav_c1soc  or
063800090416     c                             c1exl <> sav_c1exl
063900090313     C*
064000090313     C                   eval      $INZS1 = *ON
064100090313     C                   MOVE      'S1'          $GEST
064200090416     c                   clear                   sav_pos
064300090416     c                   movel     c1pos         sav_pos_x_INZ
064400090313     C*
064500090313      *  memorizza x verifica successiva
064600090313     c                   eval        sav_c1soc  =  c1soc
064700090313     C                   eval        sav_c1fil  =  c1fil
064800090313     C                   eval        sav_c1exl  =  c1exl
064900090316     C                   if          c1exl  =  ' '
065000090317     c                   eval        c1exl  =  'S'
065100090316     c                   end
065200090313      *
065300090313     c                   end
065400090316     C*
065500940202     C                   ENDSR
065600940131     C************************************************************
065700940131     C* CONTROLLO OPZIONI LISTA
065800940131     C************************************************************
065900090316     C     Inizia_con    BEGSR
066000940131     C*
066100090316      * ? Secondo ciclo x trovare il posizionamento dal record....
066200090316     C                   DO        RrnLast       S1Nrr
066300090316     C     S1nrr         CHAIN     S1
066400090316     C*
066500090316     c                   if           C1pos <= s1des
066600090316     C* Se deve posizionarsi all'inizio
066700090316     c                   if           C1pos = *blank
066800090316     c                   eval      pos_C1RCD = 1
066900090316     c                   end
067000090316     C* Esce
067100090316     c                   LEAVE
067200090316     c                   else
067300090316     C*
067400090316     c                   eval      pos_C1RCD = S1nrr + 1
067500090316     c                   if        pos_C1RCD > RrnLast
067600090316     c                   eval      pos_C1RCD = RrnLast
067700090316     c                   end
067800090316     C*
067900090316     C* Si tiene in memoria la riga toccata con SFLNXTCHG
068000090316     c                   if        s1opz <>' '
068100090316     C                   MOVE      *ON           *IN32
068200090316     c                   else
068300090316     C                   SetOFF                                       32
068400090316     c                   end
068500090316     C*
068600090316     C                   UPDATE    S1
068700090415     C                   SetOFF                                       32
068800090316     c                   end
068900090316     C*
069000090316     C                   ENDDO
069100090316     C*
069200090316     C                   ENDSR
069300090316     C************************************************************
069400090316     C* CONTROLLO OPZIONI LISTA
069500090316     C************************************************************
069600090316     C     CTRS1         BEGSR
069700090316     C*
069800940202     C                   MOVEL     *OFF          $ESCI
069900940201     C                   SETOFF                                       99
070000090311     C                   clear                   S1OPZ
070100940131     C*
070200940127     C* Leggo il sfl solo se ci sono rcd
0703009401311    C     WMAX          IFGT      0
070400940607     C                   READC     S1                                     21
070500940127     C*
070600940131     C* esce se fine sfl o errore che richiede l'uscita
0707009401312    C     *IN21         DOWEQ     *OFF
070800940131     C     $ESCI         ANDEQ     *OFF
070900940201     C                   Z-ADD     S1NRR         C1RCD
071000940131     C* ctrl su riga
071100940131     C                   EXSR      RECS1
071200090311      *
071300940131     C* gestione opzioni
0714000903113    C     S1OPZ         IFNE      *blank
071500940201     C     *IN99         ANDEQ     *OFF
071600940131     C                   EXSR      OPZS1
0717009401273-   C                   ENDIF
071800090311      *
071900090415     c                   setoff                                       32
072000940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0721009402013    C     *IN99         IFEQ      *ON
072200940201     C     $ESCI         OREQ      *ON
072300940131     C                   MOVE      *ON           *IN32
072400940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
072500940204     C* la ripristinerò a conclusione del ciclo di READC
072600940223     C                   MOVE      *OFF          $INZS1
0727009402233-   C                   ENDIF
072800940223     C*
072900090311     C                   clear                   S1OPZ
073000940223     C*
073100940607     C                   UPDATE    S1
073200940223     C*
073300940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0734009401313    C     $ESCI         IFEQ      *OFF
073500940607     C                   READC     S1                                     21
073600940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0737009402014    C     *IN21         IFEQ      *ON
073800940201     C                   MOVE      WINZS1        $INZS1
073900940204     C* calcolo pagina a cui deve posizionarsi
074000940224     C                   EXSR      CLCPAG
0741009402014-   C                   ENDIF
0742009402013-   C                   ENDIF
074300940131     C*
0744009401272-   C                   ENDDO
074500940127     C*
0746009401311-   C                   ENDIF
074700940131     C*
074800940127     C                   ENDSR
074900940127     C/EJECT
075000940127     C************************************************************
075100940131     C* CONTROLLO CAMPI I/O RIGA LISTA
075200940127     C************************************************************
075300940131     C     RECS1         BEGSR
075400940131     C*
075500940201     C* reset indicatori DSPATR
075600940201     C                   MOVE      *ALL'0'       IN4049           10
075700940201     C                   MOVEA     IN4049        *IN(40)
075800090415TOGLIC*
075900940131     C                   ENDSR
076000940131     C************************************************************
076100940131     C* GESTIONE OPZIONI LISTA
076200940131     C************************************************************
076300940131     C     OPZS1         BEGSR
076400940201     C*
076500090826     C*  Solo interrogare o
076600090311     C                   RESET                   xtabds
076700030113     C                   MOVEL     S1OPZ         xtaopz
076800030113     C                   MOVE      *ZERO         xtaret
076900030113     C                   MOVE      *ZERO         xtaopr
077000090317     C                   z-add     H1NRC         campo7_0          7 0
077100090317     C                   MOVEl(p)  campo7_0      xtakey
077200090319     C                   z-add     H1NRRtrs      xtarecord
077300940715     C                   MOVE      *BLANKS       KPJBU
077400090311     C                   MOVEL     xtabds        KPJBU
077500090826$004 C                   CALL      'TRMZ65SR2'
077600940607     C                   PARM                    KPJBA
077700090311     C                   MOVEL     KPJBU         xtabds
077800940201     C*
077900940223     C* ritorno da PGM gestione
078000940223     C                   EXSR      GESRET
078100090311     C*
078200940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0783009402252    C     *IN99         IFEQ      *ON
078400940223     C                   SETON                                        40
0785009402252-   C                   ENDIF
078600940225     C*
078700940131     C                   ENDSR
078800940223     C/EJECT
078900940223     C************************************************************
079000940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
079100940223     C************************************************************
079200940223     C     GESRET        BEGSR
079300940223     C*
079400090318     C                   MOVE      *OFF          WINZS1
079500940223     C* modo di ritorno
0796009402231    C                   SELECT
079700940314    >C* << questi modi di utilizzo dei valori di ritorno dal
079800940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
079900940314    >C*    proposte, normalmente sempre valide, ma modificabili
080000940314    >C*    per situazioni particolari >>
080100940223     C* 1 = F3
080200030113    >C     xtaret        WHENEQ    '1'
080300940224     C                   MOVE      *ON           $ESCI
080400940223     C                   MOVE      *ON           $FINE
080500940223     C* 2 = F12
080600030113    >C     xtaret        WHENEQ    '2'
080700940223     C                   MOVE      *ON           $ESCI
080800940223     C*
0809009402231-   C                   ENDSL
081000940223     C*
081100940223     C* operazione eseguite dal pgm chiamato
081200940223     C*
0813009402231    C                   SELECT
081400940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
081500030113    >C     xtaopr        WHENEQ    '1'
081600940223     C                   MOVE      *ON           WINZS1
081700090318     c                   clear                   c1pos
081800090318     c                   clear                   c1cerca
081900090318     c                   clear                   sav_pos
082000090318     c                   clear                   sav_cerca
082100940223     C*
0822009402231-   C                   ENDSL
082300940223     C*
082400940223     C* funzione non eseguibile per errore :
082500940223     C*
0826009402231    C                   SELECT
082700940223     C* 1 = funzione richiamata chiusa in errore
082800940316    >C*  eventualmente gestire altri codici di errore
082900030113    >C     xtaerr        WHENEQ    '1'
083000940223     C                   MOVE      *ON           $ESCI
083100940223     C                   SETON                                        5299
083200940223     C*
0833009402231-   C                   ENDSL
083400940223     C*
083500940223     C                   ENDSR
083600940223     C/EJECT
083700940131     C************************************************************
083800940131     C* OPERAZIONI INIZIALI
083900940131     C************************************************************
084000940131     C     *INZSR        BEGSR
084100030113     C*
084200030113     C* Reperimento parametri
084300030113     C     *ENTRY        PLIST
084400030113     C                   PARM                    KPJBA
084500030113     C*
084600030113     C* Variabili per gestione videate
084700030113     C                   MOVE      *BLANK        $GEST             2
084800030113     C                   MOVE      *BLANK        $FINE             1
084900030113     C                   MOVE      *BLANK        $INZS1            1
085000030113     C                   MOVE      *BLANK        $EFILE            1
085100030113     C                   MOVE      *BLANK        $ESCI             1
085200030113     C                   MOVE      *BLANK        $RCDOK            1
085300030113     C* Indici
085400030113     C                   Z-ADD     0             X                 3 0
085500030113     C                   Z-ADD     0             Y                 3 0
085600940506     C*
085700940506     C* Reperimento tasti di funzione
085800090317
085900090317     ** Inizializzo il programma.
086000090317     C                   CALL      'TIBSSOCR'
086100090317     C                   PARM      'INIT'        prmRqsOpCode
086200090317     C                   PARM                    prmRpyOpCode
086300090317     C                   PARM                    prmRpyIdMsg
086400090311     C*
086500090311     C*  Impostazioni di inizio programma
086600940127     C*
086700940117     C                   ENDSR
086800090311     c*--------------------------------------------------------------
086900090311     c* tramite Società e Unità decodifica P.IVA su PROJ
087000090311     c*--------------------------------------------------------------
087100090311     C     Decod_Rag_IVA BegSR
087200090311      **
087300090311      **   Si è portato fuori l'aggancio al Fornitore:
087400090311      **
087500090311      **  Routine x Reperire dati Fornitore:
087600090311      **    La routine in base alla sottonatura può funzionare
087700090311      **     x F=Fornitore/C=Cliente
087800090826     c                   clear                   trmz70s1DS
087900090316     C                   movel(p)  TRSiva        PartitaIVA
088000090408     C                   movel(p)  'F'           SottoNatur
088100090316     C                   movel(p)  TRSSOCG       Societa
088200090316     C                   movel(p)  TRSFIL        Unita
088300090408      *
088400090826     c                   call      'TRMZ70SR1'
088500090826     C                   PARM                    trmz70s1DS
088600090311     c*
088700090311     C                   ENDSR
088800090311     C/EJECT
088900090311      * ?-------------------------------------------------------------*?
089000090311      *? Riordina comunque il SFL                                     ?
089100090311      * ?-------------------------------------------------------------*?
089200090311     C     Ordina_S1     BEGSR
089300090311     C*
089400090311     C*  Totale Records Caricati nel SFL
089500090311     c                   eval      wsfl = 1
089600090311     C*
089700090311      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
089800090311      * presente nel subfile --?"Selected"?-- questo è aggiunto al record.
089900090311      * Il campo è usato per selezionare i records dando un ordine a quelli
090000090311      * selezionati davanti ai non selezionati.
090100090311     C                   CLEAR                   QLGSCB
090200090311     C                   CLEAR                   QLGSCB00
090300090311      *
090400090311     c                   select
090500090311      *
090600090311      *?  Ordinamento per Ragione Sociale    ?
090700090311     C                   when      WrkSort = Ord_RAGSOC
090800090311      *
090900090311     C                   EVAL      QLGNBRK    = 1
091000090311
091100090311     ** Il GIRO è in posizione (1) 10 Bytes char ascending
091200090311     C                   EVAL      QLGSP      = 1
091300090311     C                   EVAL      QLGSS      = %SIZE(S1DES)
091400090311     C                   EVAL      QLGDT      = Carattere
091500090311     C                   EVAL      QLGSO      = Ascendente
091600090311     C                   EVAL      QLGKL(1)   = QLGSKL
091700090311      *
091800090311     c                   other
091900090311      *
092000090311     c                   endSL
092100090311      *------------
092200090311      * Load other sort parameters.
092300090311     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
092400090311     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
092500090311     C                   EVAL      QLGRT     = 8
092600090311     C                   EVAL      QLGOKL    = 80
092700090311     C                   EVAL      QLGLKE    = 16
092800090311     C                   EVAL      QLGLSS    = 290
092900090311
093000090311      * Initialize Sort I/O API fields.
093100090311     C                   EVAL      QLGRL00  = QLGRL
093200090311     C                   EVAL      QLGRC00  = 1
093300090311     C                   CLEAR                   QUSEI
093400090311     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
093500090311
093600090311      * First step - Initialize the sort routine.
093700090311     C                   CALL      'QLGSORT'
093800090311     C                   PARM                    QLGSCB
093900090311     C                   PARM                    NotUsed
094000090311     C                   PARM                    NotUsed
094100090311     C                   PARM                    SizeList
094200090311     C                   PARM                    ReturnSize
094300090311     C                   PARM                    QUSEC
094400090311
094500090311      * Next step - Write records to I/O routine.
094600090311     C                   EVAL      QLGRT00 = Put
094700090311
094800090316     C                   DO        RrnLast       S1nrr
094900090316     C     S1nrr         CHAIN     S1
095000090311
095100090311     ** Solo le righe con Selected = 'Y' sono riordinate,
095200090311     ** quindi per fare un ordinamento di tutte le righe
095300090311     ** metto 'Y' sempre.
095400090311     C                   EVAL      Selected  = 'Y'
095500090311
095600090311     C                   CLEAR                   QUSEI
095700090311     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
095800090311      *
095900090311     C                   CALL      'QLGSRTIO'
096000090311     C                   PARM                    QLGSCB00
096100090311     C                   PARM                    SflRcd
096200090311     C                   PARM                    NotUsed
096300090311     C                   PARM                    SizeList
096400090311     C                   PARM                    NotUsed
096500090311     C                   PARM                    QUSEC
096600090311
096700090311     C                   ENDDO
096800090311
096900090311      * Next step - Signal end of input, clear subfile for reload.
097000090311     C                   EVAL      QLGRT00 = EndPut
097100090311     C                   CLEAR                   QUSEI
097200090311     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
097300090311
097400090311     C                   CALL      'QLGSRTIO'
097500090311     C                   PARM                    QLGSCB00
097600090311     C                   PARM                    SflRcd
097700090311     C                   PARM                    NotUsed
097800090311     C                   PARM                    SizeList
097900090311     C                   PARM                    NotUsed
098000090311     C                   PARM                    QUSEC
098100090311      *
  pulizia SFL   ?
098200090316    >C                   EXSR      INZC1
098300090311
098400090311      * Final step - Write the records back to the subfile.
098500090311     C                   EVAL      QLGRT00 = Get
098600090311
098700090316     C                   DO        RrnLast       S1nrr
098800090311     C                   CLEAR                   QUSEI
098900090311     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
099000090311     C                   CALL      'QLGSRTIO'
099100090311     C                   PARM                    QLGSCB00
099200090311     C                   PARM                    NotUsed
099300090311     C                   PARM                    SflRcd
099400090311     C                   PARM                    QLGRL00
099500090311     C                   PARM                    NotUsed
099600090311     C                   PARM                    QUSEC
099700090311      * SFLnxtCHG
099800090311     c                   if        s1opz <> *blank
099900090311     c                   seton                                        32
100000090311     c                   else
100100090311     C                   SetOFF                                       32
100200090311     c                   end
100300090316      *
100400090311     C                   WRITE     S1
100500090415     C                   SetOFF                                       32
100600090311     C                   ENDDO
100700090311      *
100800090311      *  All'uscita di queste chiamate a routine di sistema il SFL record
100900090311      *   si incrementa stranamente quindi lo reimposto correttamente
101000090311      *    prima di lasciare la routine.
101100090311     C                   EVAL      S1nrr = RrnLast
101200090311     C*
101300090311     C                   ENDSR
101400090311     C/EJECT
101500090311      * ?-------------------------------------------------------------*?
