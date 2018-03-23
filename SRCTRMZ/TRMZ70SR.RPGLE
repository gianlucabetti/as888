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
002100090312     Fazorg01l  if   E           k disk
002200090313     Faitrs01l  if   E           k disk    infds(dsaitrs)
002300090311      *
002400090311     FTRMZ70SD  CF   E             WORKSTN
002500940607     F                                     SFILE(S1:S1NRR)
002600940201     F                                     INFDS(DSFMT)
002700940128     D*----------------------------------------------------*
002800940211     D* Passaggio Parametri
002900940211     D KPJBA         E DS
003000090408     D KPJBus          s                   like(kpjbu)
003100090311     D anRCO98j      E DS
003200090408     D trMZ70s1DS    E DS
003201170908
003202170908     D tibs02ds      E DS
003203170908     D TIBS34DS      E DS                                                       *Profili utente
003204170908     D DDATIUTE      E DS                                                       *Dati utente
003205170908     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003206170908     D dute01        E DS                                                       *Utenti
003207170908     d dLat          e ds
003208170908     D TRUL31DS      E DS
003209170908     D TRUL31DS2     E DS
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
007201170908     d wabi            s                   like(UteAut)
007202170908     d wabisl          s              1
007203170908     d* p.o. abilitati
007204170908     D $pogn           s              3  0 dim(250)
007205170908     D $pog            s              3    dim(250)
007206170908     D $DIG            s              1    dim(20)
007207170908     D $ARG            s              3    dim(50)
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
013200090311      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
013300090311      **********************************************************************
013400090311     D Rrn1            S              5I 0
013500090311     D NotUsed         S             16A
013600170901     D ReturnSize      S             10i 0
013700170901     D SizeList        S             10i 0
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
031400940131     C* F10=Immissione
031500940506     C     $TASTO        WHENEQ    F10
031600940309     C                   EXSR      F10S1
031700090311     C* F12=Ritorno
031800090311     C     $TASTO        WHENEQ    F12
031900090311     C                   EXSR      F12S1
0320009401271O   C                   OTHER
032100940127     C* CONTROLLO DATI
032200940131     C                   EXSR      CTRC1
032300940201     C     *IN99         IFEQ      *OFF
032400940131     C                   EXSR      CTRS1
032500940131     C                   END
0326009401271-   C                   ENDSL
032700940127     C*
032800940127     C                   ENDSR
032900940224     C/EJECT
033000940127     C************************************************************
033100940131     C* INIZIALIZZAZIONE LISTA
033200940127     C************************************************************
033300090312     C     INZC1         BEGSR
033400940302     C* pulizia SFL
033500090312     C                   SETOFF                                         3031
033600090312     C                   WRITE     C1
033700090312     C                   SETON                                          31
033800090312     C*
033900090312     C* CARICAMENTO SFL totale
034000090312     C                   Z-ADD     1             C1RCD
034100090312     c                   movel     DSPGM         c1pgm
034200090312     c                   movel     knmus         c1mus
034300090312     c                   movel     knsif         c1sif
034400090317     c                   movel     'S'           c1exl
034500090313     c                   if        sav_c1exl <> c1exl
034600090313     c                   eval      c1exl = sav_c1exl
034700090313     c                   end
034800090312     C*
034900090312     C                   ENDSR
035000090312     C/EJECT
035100090312     C************************************************************
035200090312     C* INIZIALIZZAZIONE LISTA
035300090312     C************************************************************
035400090312     C     INZS1         BEGSR
035500090312     C*
035600090312     C* pulizia SFL
035700090312    >C                   EXSR      INZC1
035800090312     C*
035900090312     C                   Z-ADD     0             S1NRR
036000090312     C                   Z-ADD     0             WMAX
036100090312     C                   Z-ADD     0             wsfl
036200940224     C*
036300940224     C* Posizionamento su file pilota
036400090313     c     *loval        setll     aitrs01l
036500940608    >C                   EXSR      REDANA
036600030113     C* Carico il SFL
036700940127     C                   EXSR      ROLS1
036800030113     C*
036900030114     c                   if        xtaopr <> '1'
037000030114     C                   Z-ADD     1             WPAG
037100030114     c                   end
037200940127     C*
037300940127     C                   ENDSR
037400940127     C************************************************************
037500940131     C* CARICAMENTO PAGINA LISTA
037600940127     C************************************************************
037700940127     C     ROLS1         BEGSR
037800940127     C*
037900940128     C                   SETOFF                                       32
038000940223     C                   Z-ADD     0             Y
038100940127     C                   Z-ADD     WMAX          S1NRR
038200090316     c                   clear                   Wpos
038300940127     C*
038400940127     C* Leggo dal file anagrafico per caricare la lista
0385009401311    C     $EFILE        DOWEQ     *OFF
038600940127     C*
038700030113     c                   clear                   s1opz
038800090311     C*
038900090316     c                   exsr      Decod_Rag_IVA
039000090316     C*
039100090316     C*   Ricerca x Stringa attivata
039200090316     c                   if        c1cerca <> *blank
039300090316     c                   eval      WLen = %len(%Trim(c1cerca))
039400130910     c
039500130910     c******             eval      WPos =
039600130910     c******                        %scan(%subst(c1cerca:1:WLen) : RagSocKSC)
039700130910     c                   eval      WPos =
039800130910     c                              %scan(%trim(c1cerca) : RagSocKSC)
039900090316     c                   end
040000090316     C*
040100090316     c                   if        sav_pos <=  RagSocKSC and sav_pos <> *blank
040200090316     c                             or
040300090316     c                             sav_pos  =  *blank
040400090316     C*
040500090316     c                   if        Wpos > 0 and c1cerca <> *BLANK
040600090316     c                             or
040700090316     c                             C1CERCA = *BLANK
040800090316     C*
040900090311     c                   eval      s1des   = RagSocKSC
041000090316     c                   eval      s1socG  = TRSSOCG
041100090316     c                   eval      s1fil   = TRSFIL
041200090316     c                   eval      h1NRC   = TRSNRC
041300090319     c                   eval      h1NRRtrs= rrnaitrs
041400090706     c                   eval      h1DRC   = trsDRC
041500090311     c*
041600090316     c                   z-add     0             s1dec
041700090316     c                   if        TRSDEC  > 0
041800090316     c                   move      TRSDEC        dataiso
041900090311     c                   move      dataiso       dataDMY
042000090316     c                   move      dataDMY       S1Dec
042100090311     c                   end
042200090311     c*
042300090316     c                   z-add     0             s1dfc
042400090316     c                   if        TRSDFC     > 0
042500090316     c                   move      TRSDFC        dataiso
042600090311     c                   move      dataiso       dataDMY
042700090316     c                   move      dataDMY       S1DFC
042800090311     c                   end
042900940127     C*
043000940127     C                   ADD       1             S1NRR
043100090311     C                   EVAL      RrnLast = S1nrr
043200940127     C                   ADD       1             Y
043300940127     C*
043400940607     C                   WRITE     S1
043500090316     c                   end
043600090313     C*
043700090313     C* esce x il max. records consentiti
043800170901     c                   if        s1nrr = 9998
043900090313     c                   leave
044000090313     c                   end
044100090316     c                   end
044200940131     C*
044300940316    >C                   EXSR      REDANA
044400940128     C*
0445009401271-   C                   ENDDO
044600940127     C*
044700940223     C                   Z-ADD     S1NRR         WMAX                 30
044800090311     C*
044900090311     C* Ordinamento SFL x RAG.SOCIALE
045000090311     C                   EXSR      Ordina_S1
045100090316     c*
045200090416     c                   if        sav_pos_x_INZ <> *blank
045300090416     c                   eval      sav_pos = sav_pos_x_INZ
045400090416     c                   eval      posizionamento = 'S'
045500090416     c                   clear                   sav_pos_x_INZ
045600090416     c                   end
045700090316     c* Posizionamento
045800090316     c                   if        posizionamento = 'S'
045900090316     c                   eval      posizionamento = 'N'
046000090316     C                   EXSR      INIZIA_CON
046100090316     c                   end
046200090316     C*----------
046300090316     C*  Reimposta i puntamenti e le ricerche
046400090316     c                   eval      c1pos   = sav_pos
046500090316     c                   eval      c1cerca = sav_cerca
046600090316     C*
046700940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
046800030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
046900940127     C                   MVR                     RESTO             3 0
047000030114     C     PAGINE        MULT      SFLPAG        C1RCD
0471000301141    C     RESTO         IFGT      0
047200030114     C                   ADD       1             C1RCD
0473000301141E   C                   ELSE
047400030114     C                   SUB       SFLPAG        C1RCD
047500030114     C                   ADD       1             C1RCD
0476000301141-   C                   ENDIF
047700940128     C*
047800940127     C                   ENDSR
047900940128     C************************************************************
048000940131     C* LETTURA RCD ARCHIVIO PILOTA
048100940128     C************************************************************
048200940607     C     REDANA        BEGSR
048300940128     C*
048400940131     C                   MOVEL     *OFF          $EFILE
048500940131     C*
0486009401311    C     $EFILE        DOUEQ     *ON
048700090313     C                   MOVEL     *ON           $RCDOK
048800090313     c                   Read      AITRS01L
048900090311     C*
049000090313     c                   if        %eof(AITRS01L)
049100030113     C                   MOVEL     *ON           $EFILE
049200090313     C                   MOVEL     *OFF          $RCDOK
049300030113     C                   MOVE      $EFILE        *IN33
049400030113     c                   else
049500090318      *  Esclude gli annullati
049600090318     c                   if         trsANN <>' '
049700090318     C                   MOVE      *OFF          $RCDOK
049800090318     c                   end
049900090311      *
049901170908      *  Esclude se filiale non abilitata al profilo
049902170908     c     trsfil        lookup    $pogn                                  96
049903170908     C  n96              MOVE      *OFF          $RCDOK
049905170908      *
050000090311      * O tutte le società oppure Escluse quelle che hanno una fine Contratto ossia
050100090311      *   solo quelle in essere
050200090316     c                   if        sav_c1EXL = 'S' and TRSDFC > 0
050300090313     C                   MOVE      *OFF          $RCDOK
050400030113     c                   end
050500090313      *
050600090312     C*  società filtrante
050700090313     c                   if        sav_c1soc <> *blank and
050800090316     c                             sav_c1soc <> TRSSOCG
050900090313     C                   MOVE      *OFF          $RCDOK
051000090312     c                   end
051100090312     C*
051200090312     C*  filiale filtrante
051300090313     c                   if        sav_c1fil > *zeros and
051400090316     c                             sav_c1fil <> TRSFIL
051500090313     C                   MOVE      *OFF          $RCDOK
051600090312     c                   end
051700090313     C*
051800090313     C*  senza P.IVA
051900090316     c                   if        TRSIVA = *blank
052000090313     C                   MOVE      *OFF          $RCDOK
052100090313     c                   end
052200090313     C*
052300090313     C* se il record è giusto esce dal ciclo
052400090313     C     $RCDOK        ifeq      *On
052500090313     c                   Leave
052600090313     c                   end
052700090312     C*
052800090311     c                   end
052900090311     C*
0530009401311-   C                   ENDDO
053100940131     C*
053200940131     C                   ENDSR
053300940224     C************************************************************
053400940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
053500940224     C************************************************************
053600940224     C     CLCPAG        BEGSR
053700940224     C* Input :
053800940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
053900940224     C*          cursore
054000940224     C* - SFLPAG = numero rcd per pagina sfl
054100940224     C* Output :
054200940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
054300940224     C*
054400940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
054500940224     C                   MVR                     RESTO             3 0
054600940224     C     RESTO         IFGT      0
054700940224     C                   ADD       1             PAGINE
054800940224     C                   ENDIF
054900940224     C     PAGINE        MULT      SFLPAG        WPAG
055000940224     C*
055100940224     C                   ENDSR
055200090311     C************************************************************
055300090311     C* GESTIONE F03 VIDEO S1
055400090311     C************************************************************
055500090311     C     F03S1         BEGSR
055600090311     C*
055700940309     C                   MOVE      *ON           $FINE
055800940325     C* fine programma
055900940309     C                   ENDSR
056000940309     C/EJECT
056100090313     C************************************************************
056200090313     C* GESTIONE F05 ricarice S1
056300090313     C************************************************************
056400090313     C     F05S1         BEGSR
056500090313     C*
056600090313     C                   eval      $INZS1 = *ON
056700090313     C                   MOVE      'S1'          $GEST
056800090313     C* fine programma
056900090313     C                   ENDSR
057000090313     C/EJECT
057100940309     C************************************************************
057200940309     C* GESTIONE F10 VIDEO S1
057300940314     c* AGGIUNTA RECORD
057400940309     C************************************************************
057500940309     C     F10S1         BEGSR
057600940309     C*
057700090311     C                   RESET                   xtabds
057800090311     C                   MOVEL     '1'           xtaopz
057900030113     C                   MOVE      *ZERO         xtaret
058000030113     C                   MOVE      *ZERO         xtaopr
058100090311     C                   MOVEl     *zeros        xtakey
058200090319     C                   z-add     *zeros        xtarecord
058300030113     C                   MOVE      *BLANKS       KPJBU
058400090311     C                   MOVEL     xtabds        KPJBU
058500090311$004 C                   CALL      'TRMZ70SR2'
058600030113     C                   PARM                    KPJBA
058700090311     C                   MOVEL     KPJBU         xtabds
058800030113      *
058900940309     C* ritorno da PGM gestione
059000940309     C                   EXSR      GESRET
059100090311      *
059200940309     C     WINZS1        IFEQ      *ON
059300940309     C                   MOVE      *ON           $INZS1
059400940309     C* se esistevano già righe sul sfl
059500940309     C* calcolo pagina a cui deve posizionarsi
059600940309     C     WSFL          IFGT      0
059700940309     C                   EXSR      CLCPAG
059800940309     C* altrimenti carico solo la 1a pagina
059900940309     C                   ELSE
060000940309     C                   Z-ADD     SFLPAG        WPAG
060100940309     C                   ENDIF
060200940309     C                   ENDIF
060300940309     C*
060400940309     C                   ENDSR
060500090311     C************************************************************
060600090311     C* GESTIONE F12 VIDEO S1
060700090311     C************************************************************
060800090311     C     F12S1         BEGSR
060900090311     C*
061000090311     C                   MOVE      *ON           $FINE
061100090311     C* fine programma
061200090311     C                   ENDSR
061300090311     C/EJECT
061400940128     C************************************************************
061500940131     C* CONTROLLO TESTATA LISTA
061600940128     C************************************************************
061700940131     C     CTRC1         BEGSR
061800940128     C*
061900940201     C                   MOVE      *OFF          *IN99
062000090312     c                   eval      posizionamento = 'N'
062100090316     c                   clear                   pos_c1rcd
062200090312      *  Posizionamento
062300090316     c                   if          sav_pos <> c1pos
062400090316     c                   eval      sav_pos = c1pos
062500090312     c                   eval      posizionamento = 'S'
062600090316     C                   exsr      INIZIA_CON
062700090316     C                   End
062800090312      *
062900090316     c                   if        sav_cerca <> c1cerca
063000090316     c                             or
063100090316     c                             c1cerca <> *blank
063200090316     c                   eval      sav_cerca = c1cerca
063300090316     C                   eval      $INZS1 = *ON
063400090316     C                   MOVE      'S1'          $GEST
063500090316     C                   End
063600090316      *
063700090312     C*  Decodifiche se inseriti Società o Filiale
063800090312     c                   clear                   c1dfil
063900090312     c                   if        c1fil > *zeros
064000090312     c     c1fil         chain     azorg01l
064100090312     c                   if        %Found(azorg01l)
064200090312     c                   eval      c1dfil = orgdes
064300090312     c                   end
064400090312     c                   end
064500090312     C*
064600090312     c                   clear                   c1dsoc
064700090312     c                   if        c1soc <> *blank
064800090316     C*
064900090316     ** Chiamo passando un id società che esiste.
065000090316     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
065100090316     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
065200090316     C                   RESET                   tibsSocI0
065300090316     C                   EVAL      tibsSocI0.idSocieta = c1soc
065400090316     C
065500090316     C                   CALL      'TIBSSOCR'
065600090316     C                   PARM      'GETANAGRAF'  prmRqsOpCode
065700090316     C                   PARM      *BLANK        prmRpyOpCode
065800090316     C                   PARM      *ZERO         prmRpyIdMsg
065900090316     C                   PARM      'TIBSSOCI0'   prmRqsFormato
066000090316     C                   PARM                    tibsSocI0
066100090316     C                   PARM                    prmRqsDataSize
066200090316     C                   PARM      'TIBSSOCO0'   prmRpyFormato
066300090316     C                   PARM                    tibsSocO0
066400090316     C                   PARM                    prmRpyDataSize
066500090316
066600090324     c                   if         PRMRPYIDMSG >= 0 and
066700090324     c                              TIBSSOCO0.IDSOCIETA <> *blank
066800090316     c                   eval      c1dsoc = tibsSocO0.RAGSOCIALE
066900090323     c                   else
067000090324     c                   eval      c1dsoc = 'Società Errata'
067100090323     c                   end
067200090316
067300090312     c                   end
067400090312     C*
067500090416     c                   clear                   sav_pos_x_INZ
067600090313     C*  Se cambiato
067700090313     C                   if        c1fil <> sav_c1fil  or
067800090313     c                             c1soc <> sav_c1soc  or
067900090416     c                             c1exl <> sav_c1exl
068000090313     C*
068100090313     C                   eval      $INZS1 = *ON
068200090313     C                   MOVE      'S1'          $GEST
068300090416     c                   clear                   sav_pos
068400090416     c                   movel     c1pos         sav_pos_x_INZ
068500090313     C*
068600090313      *  memorizza x verifica successiva
068700090313     c                   eval        sav_c1soc  =  c1soc
068800090313     C                   eval        sav_c1fil  =  c1fil
068900090313     C                   eval        sav_c1exl  =  c1exl
069000090316     C                   if          c1exl  =  ' '
069100090317     c                   eval        c1exl  =  'S'
069200090316     c                   end
069300090313      *
069400090313     c                   end
069500090316     C*
069600940202     C                   ENDSR
069700940131     C************************************************************
069800940131     C* CONTROLLO OPZIONI LISTA
069900940131     C************************************************************
070000090316     C     Inizia_con    BEGSR
070100940131     C*
070200090316      * ? Secondo ciclo x trovare il posizionamento dal record....
070300090316     C                   DO        RrnLast       S1Nrr
070400090316     C     S1nrr         CHAIN     S1
070500090316     C*
070600090316     c                   if           C1pos <= s1des
070700090316     C* Se deve posizionarsi all'inizio
070800090316     c                   if           C1pos = *blank
070900090316     c                   eval      pos_C1RCD = 1
071000090316     c                   end
071100090316     C* Esce
071200090316     c                   LEAVE
071300090316     c                   else
071400090316     C*
071500090316     c                   eval      pos_C1RCD = S1nrr + 1
071600090316     c                   if        pos_C1RCD > RrnLast
071700090316     c                   eval      pos_C1RCD = RrnLast
071800090316     c                   end
071900090316     C*
072000090316     C* Si tiene in memoria la riga toccata con SFLNXTCHG
072100090316     c                   if        s1opz <>' '
072200090316     C                   MOVE      *ON           *IN32
072300090316     c                   else
072400090316     C                   SetOFF                                       32
072500090316     c                   end
072600090316     C*
072700090316     C                   UPDATE    S1
072800090415     C                   SetOFF                                       32
072900090316     c                   end
073000090316     C*
073100090316     C                   ENDDO
073200090316     C*
073300090316     C                   ENDSR
073400090316     C************************************************************
073500090316     C* CONTROLLO OPZIONI LISTA
073600090316     C************************************************************
073700090316     C     CTRS1         BEGSR
073800090316     C*
073900940202     C                   MOVEL     *OFF          $ESCI
074000940201     C                   SETOFF                                       99
074100090311     C                   clear                   S1OPZ
074200940131     C*
074300940127     C* Leggo il sfl solo se ci sono rcd
0744009401311    C     WMAX          IFGT      0
074500940607     C                   READC     S1                                     21
074600940127     C*
074700940131     C* esce se fine sfl o errore che richiede l'uscita
0748009401312    C     *IN21         DOWEQ     *OFF
074900940131     C     $ESCI         ANDEQ     *OFF
075000940201     C                   Z-ADD     S1NRR         C1RCD
075100940131     C* ctrl su riga
075200940131     C                   EXSR      RECS1
075300090311      *
075400940131     C* gestione opzioni
0755000903113    C     S1OPZ         IFNE      *blank
075600940201     C     *IN99         ANDEQ     *OFF
075700940131     C                   EXSR      OPZS1
0758009401273-   C                   ENDIF
075900090311      *
076000090415     c                   setoff                                       32
076100940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0762009402013    C     *IN99         IFEQ      *ON
076300940201     C     $ESCI         OREQ      *ON
076400940131     C                   MOVE      *ON           *IN32
076500940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
076600940204     C* la ripristinerò a conclusione del ciclo di READC
076700940223     C                   MOVE      *OFF          $INZS1
0768009402233-   C                   ENDIF
076900940223     C*
077000090311     C                   clear                   S1OPZ
077100940223     C*
077200940607     C                   UPDATE    S1
077300940223     C*
077400940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0775009401313    C     $ESCI         IFEQ      *OFF
077600940607     C                   READC     S1                                     21
077700940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0778009402014    C     *IN21         IFEQ      *ON
077900940201     C                   MOVE      WINZS1        $INZS1
078000940204     C* calcolo pagina a cui deve posizionarsi
078100940224     C                   EXSR      CLCPAG
0782009402014-   C                   ENDIF
0783009402013-   C                   ENDIF
078400940131     C*
0785009401272-   C                   ENDDO
078600940127     C*
0787009401311-   C                   ENDIF
078800940131     C*
078900940127     C                   ENDSR
079000940127     C/EJECT
079100940127     C************************************************************
079200940131     C* CONTROLLO CAMPI I/O RIGA LISTA
079300940127     C************************************************************
079400940131     C     RECS1         BEGSR
079500940131     C*
079600940201     C* reset indicatori DSPATR
079700940201     C                   MOVE      *ALL'0'       IN4049           10
079800940201     C                   MOVEA     IN4049        *IN(40)
079900090415TOGLIC*
080000090415TOGLIC*   da TOGLIERE in un secondo momento
080100090415TOGLIc                   if        s1opz <> *blank  and
080200090415TOGLIc                             %subst(knmus:1:3) <> 'EDP'
080300090415TOGLIc                   if        s1opz <> '2' and s1opz <> '1' and
080400090416TOGLIc                             s1opz <> '5' and s1opz <> 'A'
080500090417TOGLIc*********          seton                                        9941
080600090415TOGLIc                   end
080700090415TOGLIc                   end
080800090415     C*
080900940131     C                   ENDSR
081000940131     C************************************************************
081100940131     C* GESTIONE OPZIONI LISTA
081200940131     C************************************************************
081300940131     C     OPZS1         BEGSR
081400940201     C*
081500090318     C* Se c'è la data Fine Contratto si può solo visualizzare il dato
081600090318     C*   ma niente altro
081700120323      *** ECCEZIONE per ACCREDITAMENTO su CONTRATTO CHIUSO....si deve passare
081800120323      *** un opzione PARTICOLARE "X" al chiamato per poter Gestire questa
081900120323      *** particolarità in funzione di passaggi di contratti quali quelli di conversione
082000120323      *** come x il 1/4/2012 in cui si sono riaperti tutti i vecchi contratti per
082100120323      *** ristamparli alla nuova maniera.
082200120323     c                   if        s1opz =  'A' and s1DFC > 0
082300120323     c                   eval      s1opz  = 'X'
082400120323     c                   else
082500120323      **
082600090318     c                   if        s1opz <> '5' and s1DFC > 0
082700090706      ** se in modifica di un contratto ormai chiuso si può solo modificare la data
082800090706      **  di ricezione contratto in sede.
082900090706     c                   if        s1opz = '2' and h1drc = 0
083000090706     c                   eval      s1opz  = 'R'
083100090706     c                   else
083200090706     c                   eval      s1opz  = '5'
083300090706     c                   end
083400090318     c                   end
083500120323     c                   endif
083600090318     C*
083700090311     C                   RESET                   xtabds
083800030113     C                   MOVEL     S1OPZ         xtaopz
083900030113     C                   MOVE      *ZERO         xtaret
084000030113     C                   MOVE      *ZERO         xtaopr
084100090317     C                   z-add     H1NRC         campo7_0          7 0
084200090317     C                   MOVEl(p)  campo7_0      xtakey
084300090319     C                   z-add     H1NRRtrs      xtarecord
084400940715     C                   MOVE      *BLANKS       KPJBU
084500090311     C                   MOVEL     xtabds        KPJBU
084600090311$004 C                   CALL      'TRMZ70SR2'
084700940607     C                   PARM                    KPJBA
084800090311     C                   MOVEL     KPJBU         xtabds
084900940201     C*
085000940223     C* ritorno da PGM gestione
085100940223     C                   EXSR      GESRET
085200090311     C*
085300940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0854009402252    C     *IN99         IFEQ      *ON
085500940223     C                   SETON                                        40
0856009402252-   C                   ENDIF
085700940225     C*
085800940131     C                   ENDSR
085900940223     C/EJECT
086000940223     C************************************************************
086100940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
086200940223     C************************************************************
086300940223     C     GESRET        BEGSR
086400940223     C*
086500090318     C                   MOVE      *OFF          WINZS1
086600940223     C* modo di ritorno
0867009402231    C                   SELECT
086800940314    >C* << questi modi di utilizzo dei valori di ritorno dal
086900940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
087000940314    >C*    proposte, normalmente sempre valide, ma modificabili
087100940314    >C*    per situazioni particolari >>
087200940223     C* 1 = F3
087300030113    >C     xtaret        WHENEQ    '1'
087400940224     C                   MOVE      *ON           $ESCI
087500940223     C                   MOVE      *ON           $FINE
087600940223     C* 2 = F12
087700030113    >C     xtaret        WHENEQ    '2'
087800940223     C                   MOVE      *ON           $ESCI
087900940223     C*
0880009402231-   C                   ENDSL
088100940223     C*
088200940223     C* operazione eseguite dal pgm chiamato
088300940223     C*
0884009402231    C                   SELECT
088500940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
088600030113    >C     xtaopr        WHENEQ    '1'
088700940223     C                   MOVE      *ON           WINZS1
088800090318     c                   clear                   c1pos
088900090318     c                   clear                   c1cerca
089000090318     c                   clear                   sav_pos
089100090318     c                   clear                   sav_cerca
089200940223     C*
0893009402231-   C                   ENDSL
089400940223     C*
089500940223     C* funzione non eseguibile per errore :
089600940223     C*
0897009402231    C                   SELECT
089800940223     C* 1 = funzione richiamata chiusa in errore
089900940316    >C*  eventualmente gestire altri codici di errore
090000030113    >C     xtaerr        WHENEQ    '1'
090100940223     C                   MOVE      *ON           $ESCI
090200940223     C                   SETON                                        5299
090300940223     C*
0904009402231-   C                   ENDSL
090500940223     C*
090600940223     C                   ENDSR
090700940223     C/EJECT
090800940131     C************************************************************
090900940131     C* OPERAZIONI INIZIALI
091000940131     C************************************************************
091100940131     C     *INZSR        BEGSR
091200030113     C*
091300030113     C* Reperimento parametri
091400030113     C     *ENTRY        PLIST
091500030113     C                   PARM                    KPJBA
091600030113     C*
091700030113     C* Variabili per gestione videate
091800030113     C                   MOVE      *BLANK        $GEST             2
091900030113     C                   MOVE      *BLANK        $FINE             1
092000030113     C                   MOVE      *BLANK        $INZS1            1
092100030113     C                   MOVE      *BLANK        $EFILE            1
092200030113     C                   MOVE      *BLANK        $ESCI             1
092300030113     C                   MOVE      *BLANK        $RCDOK            1
092400030113     C* Indici
092500030113     C                   Z-ADD     0             X                 3 0
092600030113     C                   Z-ADD     0             Y                 3 0
092700940506     C*
092800940506     C* Reperimento tasti di funzione
092900090317
093000090317     ** Inizializzo il programma.
093100090317     C                   CALL      'TIBSSOCR'
093200090317     C                   PARM      'INIT'        prmRqsOpCode
093300090317     C                   PARM                    prmRpyOpCode
093400090317     C                   PARM                    prmRpyIdMsg
093500090311     C*
093600090311     C*  Impostazioni di inizio programma
093601170908     c* reperimento dati utente abilitazioni
093602170908     c                   exsr      repdatiute
093700940127     C*
093800940117     C                   ENDSR
093801170908     C*--------------------------------------------------------------------------------------------*
093802170908     C* REPERISCE I DATI UTENTE
093803170908     C*--------------------------------------------------------------------------------------------*
093804170908     C     REPDATIUTE    BEGSR
093805170908     C*
093806170908     C* INIZIALIZZA VARIABILI DI WRK
093807170908     C                   CLEAR                   TIBS34DS
093808170908     C                   CLEAR                   AZUTEDS
093809170908     C                   CLEAR                   DDATIUTE
093810170908     C*
093811170908     C     *DTAARA       DEFINE    §azute        azuteds
093812170908     C     *DTAARA       DEFINE    §datiute      ddatiute
093813170908     C                   IN(E)     *DTAARA
093814170908     C                   IF        %Error
093815170908     C                   EVAL      I34Tla = 'L'
093816170908     C                   CALL      'TIBS34R'
093817170908     C                   PARM                    Tibs34Ds
093818170908     C                   IN        *DTAARA
093819170908     C                   ENDIF
093820170908     c                   Clear                   wabi
093821170908     c                   Clear                   dLat
093822170908
093823170908      * Verifica errori e autorità profilo
093824170908s   1c                   Select
093825170908      * se ho errori nei dati utente esco dal pgm
093826170908w   1c                   When      DutErr = 'E'
093827170908     c                   seton                                        lr
093828170908     c                   return
093829170908      * se non c'è l'abilitazione
093830170908      * --> se 1° livello, abilitazioni al terminal
093831170908      *     se 2° livello, abilitazioni al punto operativo
093832170908w   1c                   When      UteAut = *Blanks
093833170908if  2c                   If        DutLpo = '1'
093834170908     c                   Eval      wabi   = 'TP'
093835170908e   2c                   EndIf
093836170908if  2c                   If        DutLpo = '2'
093837170908     c                   Eval      wabi   = 'PO'
093838170908e   2c                   EndIf
093839170908if  2c                   If        DutLpo = 'S'
093840170908     c                   Eval      wabi   = 'AZ'
093841170908e   2c                   EndIf
093842170908      * carica le abilitazioni del profilo
093843170908x   1c                   Other
093844170908     c                   Movel     UteFaf        Dute01
093850170908     c                   If        §Utecers <> *Blanks
093851170908     c                   Eval      wabi = §utecers
093852170908     c                   eval      wabisl = §utecersl
093853170908     c                   EndIf
093854170908e   1c                   EndSl
093855170908      * controllo se ok l'abilitazione dell'utente
093856170908     c                   Clear                   Tibs02ds
093857170908     c                   Eval      T02Mod = 'C'
093858170908     c                   Eval      T02Sif = knsif
093859170908     c                   Eval      T02Cod = 'LAT'
093860170908     c                   Movel(p)  wabi          T02Ke1
093861170908     c                   Call      'TIBS02R'
093862170908     c                   Parm                    kpjba
093863170908     c                   Parm                    Tibs02ds
093864170908if  1c                   If        T02Err = *Blanks
093865170908     c                   Eval      dLat = T02Uni
093866170908e   1c                   EndIf
093867170908      * errore o non abilitato (impossibile xchè controllato nel filtro)
093868170908if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S' or
093869170908     c                             wabi = 'NO' or wabi = *blank
093870170908     c                   seton                                        98
093871170908     c                   call      'XUTENOAB'
093872170908     c                   parm                    kpjba
093873170908     c                   seton                                        lr
093874170908     c                   return
093875170908   x1c                   endif
093876170908      * Reperimento dei P.O. gestibili dall'utente
093877170908     c                   clear                   TRUL31DS
093878170908     c                   clear                   TRUL31DS2
093879170908     c                   eval      I31abi = wabi
093880170908     c                   eval      I31cdi = DUTdis
093881170908     c                   eval      I31car = DUTare
093882170908     c                   eval      I31cpo = DUTpou
093883170908     c                   call      'TRUL31R'
093884170908     c                   parm                    KPJBA
093885170908     c                   parm                    TRUL31DS
093886170908     c                   parm                    TRUL31DS2
093887170908if  2c                   if        O31pog > *zeros
093888170908     c                   movea     O31pog        $pog
093889170908     c                   movea     O31arg        $arg
093890170908     c                   movea     O31dig        $dig
093891170908x   2c                   else
093892170908     c* impossibile perchè controllato nel filtrp
093893170908     c                   seton                                        lr
093894170908     c                   return
093895170908    1c                   endif
093896170908     c                   do        250           g                 3 0
093897170908     c                   if        $pog(g) > *zero
093898170908     c                   move      $pog(g)       $pogn(g)
093899170908     c                   else
093900170908     c                   move      999           $pogn(g)
093901170908     c                   endif
093902170908     c                   enddo
093903170908     c                   endsr
093904090311     c*--------------------------------------------------------------
094000090311     c* tramite Società e Unità decodifica P.IVA su PROJ
094100090311     c*--------------------------------------------------------------
094200090311     C     Decod_Rag_IVA BegSR
094300090311      **
094400090311      **   Si è portato fuori l'aggancio al Fornitore:
094500090311      **
094600090311      **  Routine x Reperire dati Fornitore:
094700090311      **    La routine in base alla sottonatura può funzionare
094800090311      **     x F=Fornitore/C=Cliente
094900090408     c                   clear                   trmz70s1DS
095000090316     C                   movel(p)  TRSiva        PartitaIVA                     Input
095100090408     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
095200090316     C                   movel(p)  TRSSOCG       Societa                        Input/Output
095300090316     C                   movel(p)  TRSFIL        Unita                          Input/Output
095400090408      *
095500090316     c                   call      'TRMZ70SR1'
095600090408     C                   PARM                    trmz70s1DS                     Input
095700090311     c*
095800090311     C                   ENDSR
095900090311     C/EJECT
096000090311      * ?-------------------------------------------------------------*?
096100090311      *? Riordina comunque il SFL                                     ?
096200090311      * ?-------------------------------------------------------------*?
096300090311     C     Ordina_S1     BEGSR
096400090311     C*
096500090311     C*  Totale Records Caricati nel SFL
096600090311     c                   eval      wsfl = 1
096700090311     C*
096800090311      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
096900090311      * presente nel subfile --?"Selected"?-- questo è aggiunto al record.
097000090311      * Il campo è usato per selezionare i records dando un ordine a quelli
097100090311      * selezionati davanti ai non selezionati.
097200090311     C                   CLEAR                   QLGSCB
097300090311     C                   CLEAR                   QLGSCB00
097400090311      *
097500090311     c                   select
097600090311      *
097700090311      *?  Ordinamento per Ragione Sociale    ?
097800090311     C                   when      WrkSort = Ord_RAGSOC
097900090311      *
098000090311     C                   EVAL      QLGNBRK    = 1
098100090311
098200090311     ** Il GIRO è in posizione (1) 10 Bytes char ascending
098300090311     C                   EVAL      QLGSP      = 1
098400090311     C                   EVAL      QLGSS      = %SIZE(S1DES)
098500090311     C                   EVAL      QLGDT      = Carattere
098600090311     C                   EVAL      QLGSO      = Ascendente
098700090311     C                   EVAL      QLGKL(1)   = QLGSKL
098800090311      *
098900090311     c                   other
099000090311      *
099100090311     c                   endSL
099200090311      *------------
099300090311      * Load other sort parameters.
099400090311     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
099500090311     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
099600090311     C                   EVAL      QLGRT     = 8
099700090311     C                   EVAL      QLGOKL    = 80
099800090311     C                   EVAL      QLGLKE    = 16
099900090311     C                   EVAL      QLGLSS    = 290
100000090311
100100090311      * Initialize Sort I/O API fields.
100200090311     C                   EVAL      QLGRL00  = QLGRL
100300090311     C                   EVAL      QLGRC00  = 1
100400090311     C                   CLEAR                   QUSEI
100500090311     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
100600090311
100700090311      * First step - Initialize the sort routine.
100800090311     C                   CALL      'QLGSORT'
100900090311     C                   PARM                    QLGSCB
101000090311     C                   PARM                    NotUsed
101100090311     C                   PARM                    NotUsed
101200090311     C                   PARM                    SizeList
101300090311     C                   PARM                    ReturnSize
101400090311     C                   PARM                    QUSEC
101500090311
101600090311      * Next step - Write records to I/O routine.
101700090311     C                   EVAL      QLGRT00 = Put
101800090311
101900090316     C                   DO        RrnLast       S1nrr
102000090316     C     S1nrr         CHAIN     S1
102100090311
102200090311     ** Solo le righe con Selected = 'Y' sono riordinate,
102300090311     ** quindi per fare un ordinamento di tutte le righe
102400090311     ** metto 'Y' sempre.
102500090311     C                   EVAL      Selected  = 'Y'
102600090311
102700090311     C                   CLEAR                   QUSEI
102800090311     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
102900090311      *
103000090311     C                   CALL      'QLGSRTIO'
103100090311     C                   PARM                    QLGSCB00
103200090311     C                   PARM                    SflRcd
103300090311     C                   PARM                    NotUsed
103400090311     C                   PARM                    SizeList
103500090311     C                   PARM                    NotUsed
103600090311     C                   PARM                    QUSEC
103700090311
103800090311     C                   ENDDO
103900090311
104000090311      * Next step - Signal end of input, clear subfile for reload.
104100090311     C                   EVAL      QLGRT00 = EndPut
104200090311     C                   CLEAR                   QUSEI
104300090311     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
104400090311
104500090311     C                   CALL      'QLGSRTIO'
104600090311     C                   PARM                    QLGSCB00
104700090311     C                   PARM                    SflRcd
104800090311     C                   PARM                    NotUsed
104900090311     C                   PARM                    SizeList
105000090311     C                   PARM                    NotUsed
105100090311     C                   PARM                    QUSEC
105200090311      *
  pulizia SFL   ?
105300090316    >C                   EXSR      INZC1
105400090311
105500090311      * Final step - Write the records back to the subfile.
105600090311     C                   EVAL      QLGRT00 = Get
105700090311
105800090316     C                   DO        RrnLast       S1nrr
105900090311     C                   CLEAR                   QUSEI
106000090311     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
106100090311     C                   CALL      'QLGSRTIO'
106200090311     C                   PARM                    QLGSCB00
106300090311     C                   PARM                    NotUsed
106400090311     C                   PARM                    SflRcd
106500090311     C                   PARM                    QLGRL00
106600090311     C                   PARM                    NotUsed
106700090311     C                   PARM                    QUSEC
106800090311      * SFLnxtCHG
106900090311     c                   if        s1opz <> *blank
107000090311     c                   seton                                        32
107100090311     c                   else
107200090311     C                   SetOFF                                       32
107300090311     c                   end
107400090316      *
107500090311     C                   WRITE     S1
107600090415     C                   SetOFF                                       32
107700090311     C                   ENDDO
107800090311      *
107900090311      *  All'uscita di queste chiamate a routine di sistema il SFL record
108000090311      *   si incrementa stranamente quindi lo reimposto correttamente
108100090311      *    prima di lasciare la routine.
108200090311     C                   EVAL      S1nrr = RrnLast
108300090311     C*
108400090311     C                   ENDSR
108500090311     C/EJECT
108600090311      * ?-------------------------------------------------------------*?
