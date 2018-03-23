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
002400090407     FTRMZ70SD5 CF   E             WORKSTN
002500940607     F                                     SFILE(S1:S1NRR)
002600940201     F                                     INFDS(DSFMT)
002700940128     D*----------------------------------------------------*
002800940211     D* Passaggio Parametri
002900940211     D KPJBA         E DS
003000090311     D anRCO98j      E DS
003100090410     D trmz70s1ds    E DS                  prefix(S1_)
003200090415
003201170908     D tibs02ds      E DS
003202170908     D TIBS34DS      E DS                                                       *Profili utente
003203170908     D DDATIUTE      E DS                                                       *Dati utente
003204170908     D AZUTEDS       E DS                  extname(AZUTE00F)                    *Utenti
003205170908     D dute01        E DS                                                       *Utenti
003206170908     d dLat          e ds
003207170908     D TRUL31DS      E DS
003208170908     D TRUL31DS2     E DS
003209170908      *
004000090311      *-------------
004100090311     D dsaitrs         DS
004200090311     D  rrnaitrs             397    400b 0
004300030113      *-------------
004400940325     D* Parametri in ricezione
004500090311     D xTABDS          DS
004600090311     D  XTAOPZ                 1      1
004700090311     D  XTAFLG                 2      2
004800030113     D  XTARET                 3      3
004900030113     D  XTAOPR                 4      4
005000030113     D  XTAERR                 5      5
005100090319     D  XTArecord              6     14  0
005200090319     D  XTAKEY                15     21  0
005300940211     D*-------------
005400940211     D DSFMT           DS           512
005500940506     D  $TASTO               369    369
005600940211     D  NRG                  370    370
005700940211     D  NCL                  371    371
005800940211     D  SFLNRR               378    379B 0
005900940207     D*-------------
006000940127     D* Reperimento nome PGM
006100940127     D STATUS         SDS           333
006200090312     D* Nome PGM a video
006300940127     D  DSPGM            *PROC
006400090311     D*-------------
006500090317     D sav_c1exl       S                   Like(c1exl) inz('S')
006600090313     D sav_c1fil       S                   Like(c1fil)
006700090313     D sav_c1soc       S                   Like(c1soc)
006800090316     D sav_pos         S                   Like(c1pos)
006900090316     D posizionamento  S              1    inz('N')
007000090316     D Wlen            S              3i 0
007100090316     D Wpos            S              3i 0
007200090316$003 D POS_C1RCD       S                   Like(C1rcd)
007300090316     D sav_cerca       S                   Like(c1cerca)
007400090311      *
007500090311     D dataiso         s               d   datfmt(*iso)
007600090311     D dataeur         s               d   datfmt(*eur)
007700090311     D dataDMY         s               d   datfmt(*dmy)
007800090311      *
007801170908     d wabi            s                   like(UteAut)
007802170908     d wabisl          s              1
007803170908     d* p.o. abilitati
007804170908     D $pogn           s              3  0 dim(250)
007805170908     D $pog            s              3    dim(250)
007806170908     D $DIG            s              1    dim(20)
007807170908     D $ARG            s              3    dim(50)
007900030113     D*-------------
008000030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
008100030113$003 D S1NRR           S                   Like(C1rcd)
008200030113$003 D WSfl            S                   Like(C1nrr)
008300030113$003 D Wmax            S                   Like(C1rcd)
008400030113$003 D Wpag            S                   Like(C1rcd)
008500030113$003 D Winzs1          S                   Like($inzs1)
008600940207     D*-------------
008700940211     D* COSTANTI
008800940211     D*-------------
008900090326     D SFLPAG          C                   CONST(11)
009000940314     D* dimensione della schiera $MS1
009100940506     D*
009200940506     D* Tasti di funzione
009300940506     D F01             C                   CONST(X'31')
009400940506     D F02             C                   CONST(X'32')
009500940506     D F03             C                   CONST(X'33')
009600940506     D F04             C                   CONST(X'34')
009700940506     D F05             C                   CONST(X'35')
009800940506     D F06             C                   CONST(X'36')
009900940506     D F07             C                   CONST(X'37')
010000940506     D F08             C                   CONST(X'38')
010100940506     D F09             C                   CONST(X'39')
010200940506     D F10             C                   CONST(X'3A')
010300940506     D F11             C                   CONST(X'3B')
010400940506     D F12             C                   CONST(X'3C')
010500940506     D F13             C                   CONST(X'B1')
010600940506     D F14             C                   CONST(X'B2')
010700940506     D F15             C                   CONST(X'B3')
010800940506     D F16             C                   CONST(X'B4')
010900940506     D F17             C                   CONST(X'B5')
011000940506     D F18             C                   CONST(X'B6')
011100940506     D F19             C                   CONST(X'B7')
011200940506     D F20             C                   CONST(X'B8')
011300940506     D F21             C                   CONST(X'B9')
011400940506     D F22             C                   CONST(X'BA')
011500940506     D F23             C                   CONST(X'BB')
011600940506     D F24             C                   CONST(X'BC')
011700940506     D ENTER           C                   CONST(X'F1')
011800940506     D ROLDWN          C                   CONST(X'F4')
011900940506     D ROLLUP          C                   CONST(X'F5')
012000090311      **********************************************************************
012100090311      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
012200090311      **********************************************************************
012300090311      *
012400090311      * ?Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
012500090316     D MaxKey          C                   1
012600090316$xxx D Ord_Ragsoc      C                   '1'
012700090311     D Ascendente      C                   1
012800090311     D Discendente     C                   2
012900090311     D Carattere       C                   6
013000090311     D Put             C                   1
013100090311     D EndPut          C                   2
013200090311     D Get             C                   3
013300090311     D Numerico        C                   8
013400090311      **********************************************************************
013500090311      * Campi utili:
013600090311      *     RRN1       - Numero relativo di record del Subfile
013700090311      *     SizeList   - Dimensione della lista
013800090311      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
013900090311      **********************************************************************
014000090311     D Rrn1            S              5I 0
014100090311     D NotUsed         S             16A
014200170908     D ReturnSize      S             10i 0
014300170908     D SizeList        S             10i 0
014400090311     D RrnLast         S              5I 0
014500090316     D WrkSort         S              1    inz('1')
014600090311      **********************************************************************
014700090311      * Data Structures
014800090311      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
014900090311      *     QLGSCB     - The sort request block for the QLGSORT API
015000090311      *     QLGSCB00   - The sort request block for the QLGSRTIO API
015100090311      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
015200090311      *     QUSEC      - Error structure for the QLGSORT API
015300090311      **********************************************************************
015400090311     D SflRcd          DS
015500090311     D  S1des
015600090311     D  S1socG
015700090311     D  S1fil
015800090316     D  S1dec
015900090316     D  S1dfc
016000090311     D  S1opz
016100090316     d  H1NRC
016200090323     d  H1NRRTRS
016300090311     D  Selected                      1A
016400090311
016500090311      /COPY QSYSINC/QRPGLESRC,QLGSORT
016600090311     D QLGKL                         16    DIM(MaxKey)
016700090311     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
016800090311     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
016900090311     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
017000090311     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
017100090311
017200090311      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
017300090311      /COPY QSYSINC/QRPGLESRC,QUSEC
017400090316
017500090316     ***************************************************************************
017600090316     D/COPY GAITRASRC/SRCCONST,TIBSSOCR
017700090316     D ESITO_OK...
017800090316     D                 C                   0
017900090316     ***************************************************************************
018000090316     **
018100090316     ** Dichiarazione prototipi procedure esterne.
018200090316     **
018300090316     ***************************************************************************
018400090316      /DEFINE DFTACTGRP_YES
018500090316     D/COPY GAITRASRC/SRCPROTOPR,TIBSSOCR
018600090316      /UNDEFINE DFTACTGRP_YES
018700090316     ***************************************************************************
018800090316     **
018900090316     ** Definizione strutture dati.
019000090316     **
019100090316     ***************************************************************************
019200090316     D tibsSocI0...
019300090316     D               E DS                  QUALIFIED
019400090316     D                                     INZ
019500090316     D tibsSocO0...
019600090316     D               E DS                  QUALIFIED
019700090316     D                                     INZ
019800090316     ***************************************************************************
019900090316     **
020000090316     ** Definizione variabili modulo/programma.
020100090316     **
020200090316     ***************************************************************************
020300090316     D prmRqsOpCode...
020400090316     D                 S             10A
020500090316     D prmRpyOpCode...
020600090316     D                 S             10A
020700090316     D prmRpyIdMsg...
020800090316     D                 S             10I 0
020900090316     D prmRqsFormato...
021000090316     D                 S             10A
021100090316     D prmRqsDataSize...
021200090316     D                 S             10I 0
021300090316     D prmRpyFormato...
021400090316     D                 S             10A
021500090316     D prmRpyDataSize...
021600090316     D                 S             10I 0
021700090316
021800940127     C*----------------------------------------------------*
021900090316      *?     MAIN LINE PROGRAM     ?
022000940127     C*----------------------------------------------------*
022100090415     C* Passaggio Parametri
022200090415     C                   EXSR      INIZIO
022300090415     C*
022400940223     C* inizializzazione variabili
022500940223     C                   EXSR      INZVAR
022600940223     C*
022700940223     C     $FINE         DOWEQ     *OFF
022800940131     C     $GEST         CASEQ     'S1'          GESS1
022900940117     C                   END
023000940117     C                   END
023100090317     C*
023200090317     ** Chiudo il programma.
023300090317     C                   CALL      'TIBSSOCR'
023400090317     C                   PARM      'FINALIZE'    prmRqsOpCode
023500090317     C                   PARM                    prmRpyOpCode
023600090317     C                   PARM                    prmRpyIdMsg
023700090415     C*
023800090415     c                   RETURN
023900030113     C************************************************************
024000030113     C* INIZIALIZZAZIONE VARIABILI
024100030113     C************************************************************
024200030113     C     INZVAR        BEGSR
024300030113     C*
024400030113     C* Pulizia campi e indicatori
024500030113     C                   MOVE      *ALL'0'       IN4049           10
024600030113     C                   MOVEA     IN4049        *IN(40)
024700030113     C                   CLEAR                   S1OPZ
024800030113     C* Variabili per gestione videate
024900030113     C*
025000030113     C                   MOVE      *OFF          $FINE
025100030113     C                   MOVE      *OFF          $INZS1
025200030113     C                   MOVE      *OFF          $EFILE
025300030113     C                   MOVE      *OFF          $ESCI
025400030113     C                   MOVE      *OFF          $RCDOK
025500030113     C                   Z-ADD     0             $ULKS1            3 0
025600030113     C*
025700030113     C                   MOVE      *ON           $INZS1
025800030113     C                   MOVE      'S1'          $GEST
025900090316     C                   clear                   c1pos
026000090316     C                   clear                   sav_pos
026100030113     C*
026200030113     C* Variabili appoggio
026300030114     C                   Z-ADD     1             WPAG
026400030113     c*
026500030113     C                   ENDSR
026600940127     C************************************************************
026700940131     C* GESTIONE LISTA
026800940127     C************************************************************
026900940127     C     GESS1         BEGSR
027000030113     C*
027100940223     C* inizializzazione videata
027200940223     C     $INZS1        IFEQ      *ON
027300940127     C                   EXSR      INZS1
027400940223     C                   MOVE      *OFF          $INZS1
027500940127     C                   ENDIF
027600030113     C*
027700030113     C* emissione piede videata
027800030113     C                   WRITE     Z1
027900030113     C* Non ci sono records
028000940223     C     WMAX          IFEQ      0
028100940607     C                   WRITE     D1
028200090316     c                   setoff                                       30
028300030114     C                   Else
028400090316     c                   seton                                        30
028500090316      *
028600090316      * se si è effettuato un Posizionamento
028700090316     C                   IF        POS_c1rcd  > 0  and
028800090316     C                             POS_c1rcd  <= Wmax
028900090316     C                   Z-ADD     POS_c1rcd     C1RCD
029000090316     C                   Z-ADD     POS_c1rcd     C1nrr
029100090316     c                   else
029200030114     C     Wsfl          IFgt      0
029300030114     C                   Z-ADD     wsfl          C1RCD
029400030114     C                   Else
029500030114     C     Wpag          IFgt      0
029600030114     C                   Z-ADD     wpag          C1RCD
029700030114     C                   EndIF
029800090316     C                   EndIF
029900030114     C                   EndIF
030000030114     C                   ENDIF
030100090316      *
030200940127     C*
030300030113     C*              *------------------*
030400940607     C                   EXFMT     C1
030500030113     C*              *------------------*
030600030113     C*
030700940204     C     C1NRR         IFNE      0
030800940204     C                   Z-ADD     C1NRR         WSFL
030900090313     C                   Else
031000090313     C                   Z-ADD     SFLNRR        wsfl
031100090313     C                   ENDIF
031200090313      *
031300940127     C                   Z-ADD     SFLNRR        C1RCD
031400030113     C* Selezioni
0315009401271    C                   SELECT
031600940127     C* F3=Fine
031700940506     C     $TASTO        WHENEQ    F03
031800940309     C                   EXSR      F03S1
031900090313     C* F5=Ricarica SFL
032000090313     C     $TASTO        WHENEQ    F05
032100090313     C                   EXSR      F05S1
032200940131     C* F10=Immissione
032300940506     C     $TASTO        WHENEQ    F10
032400940309     C                   EXSR      F10S1
032500090311     C* F12=Ritorno
032600090311     C     $TASTO        WHENEQ    F12
032700090311     C                   EXSR      F12S1
0328009401271O   C                   OTHER
032900940127     C* CONTROLLO DATI
033000940131     C                   EXSR      CTRC1
033100940201     C     *IN99         IFEQ      *OFF
033200940131     C                   EXSR      CTRS1
033300940131     C                   END
0334009401271-   C                   ENDSL
033500940127     C*
033600940127     C                   ENDSR
033700940224     C/EJECT
033800940127     C************************************************************
033900940131     C* INIZIALIZZAZIONE LISTA
034000940127     C************************************************************
034100090312     C     INZC1         BEGSR
034200940302     C* pulizia SFL
034300090312     C                   SETOFF                                         3031
034400090312     C                   WRITE     C1
034500090312     C                   SETON                                          31
034600090312     C*
034700090312     C* CARICAMENTO SFL totale
034800090312     C                   Z-ADD     1             C1RCD
034900090312     c                   movel     DSPGM         c1pgm
035000090312     c                   movel     knmus         c1mus
035100090312     c                   movel     knsif         c1sif
035200090317     c                   movel     'S'           c1exl
035300090313     c                   if        sav_c1exl <> c1exl
035400090313     c                   eval      c1exl = sav_c1exl
035500090313     c                   end
035600090312     C*
035700090312     C                   ENDSR
035800090312     C/EJECT
035900090312     C************************************************************
036000090312     C* INIZIALIZZAZIONE LISTA
036100090312     C************************************************************
036200090312     C     INZS1         BEGSR
036300090312     C*
036400090312     C* pulizia SFL
036500090312    >C                   EXSR      INZC1
036600090312     C*
036700090312     C                   Z-ADD     0             S1NRR
036800090312     C                   Z-ADD     0             WMAX
036900090312     C                   Z-ADD     0             wsfl
037000940224     C*
037100940224     C* Posizionamento su file pilota
037200090313     c     *loval        setll     aitrs01l
037300940608    >C                   EXSR      REDANA
037400030113     C* Carico il SFL
037500940127     C                   EXSR      ROLS1
037600030113     C*
037700030114     c                   if        xtaopr <> '1'
037800030114     C                   Z-ADD     1             WPAG
037900030114     c                   end
038000940127     C*
038100940127     C                   ENDSR
038200940127     C************************************************************
038300940131     C* CARICAMENTO PAGINA LISTA
038400940127     C************************************************************
038500940127     C     ROLS1         BEGSR
038600940127     C*
038700940128     C                   SETOFF                                       32
038800940223     C                   Z-ADD     0             Y
038900940127     C                   Z-ADD     WMAX          S1NRR
039000090316     c                   clear                   Wpos
039100940127     C*
039200940127     C* Leggo dal file anagrafico per caricare la lista
0393009401311    C     $EFILE        DOWEQ     *OFF
039400940127     C*
039500030113     c                   clear                   s1opz
039600090311     C*
039700090316     c                   exsr      Decod_Rag_IVA
039800090316     C*
039900090316     C*   Ricerca x Stringa attivata
040000090316     c                   if        c1cerca <> *blank
040100090316     c                   eval      WLen = %len(%Trim(c1cerca))
040200090316     c                   eval      WPos =
040300090410     c                              %scan(%subst(c1cerca:1:WLen): S1_RagSocKSC)
040400090316     c                   end
040500090316     C*
040600090410     c                   if        sav_pos <= S1_RagSocKSC and sav_pos <> *blank
040700090316     c                             or
040800090316     c                             sav_pos  =  *blank
040900090316     C*
041000090316     c                   if        Wpos > 0 and c1cerca <> *BLANK
041100090316     c                             or
041200090316     c                             C1CERCA = *BLANK
041300090316     C*
041400090410     c                   eval      s1des   = S1_RagSocKSC
041500090316     c                   eval      s1socG  = TRSSOCG
041600090316     c                   eval      s1fil   = TRSFIL
041700090316     c                   eval      h1NRC   = TRSNRC
041800090319     c                   eval      h1NRRtrs= rrnaitrs
041900090311     c*
042000090316     c                   z-add     0             s1dec
042100090316     c                   if        TRSDEC  > 0
042200090316     c                   move      TRSDEC        dataiso
042300090311     c                   move      dataiso       dataDMY
042400090316     c                   move      dataDMY       S1Dec
042500090311     c                   end
042600090311     c*
042700090316     c                   z-add     0             s1dfc
042800090316     c                   if        TRSDFC     > 0
042900090316     c                   move      TRSDFC        dataiso
043000090311     c                   move      dataiso       dataDMY
043100090316     c                   move      dataDMY       S1DFC
043200090311     c                   end
043300940127     C*
043400940127     C                   ADD       1             S1NRR
043500090311     C                   EVAL      RrnLast = S1nrr
043600940127     C                   ADD       1             Y
043700940127     C*
043800940607     C                   WRITE     S1
043900090316     c                   end
044000090313     C*
044100090313     C* esce x il max. records consentiti
044200170908     c                   if        s1nrr = 9998
044300090313     c                   leave
044400090313     c                   end
044500090316     c                   end
044600940131     C*
044700940316    >C                   EXSR      REDANA
044800940128     C*
0449009401271-   C                   ENDDO
045000940127     C*
045100940223     C                   Z-ADD     S1NRR         WMAX                 30
045200090311     C*
045300090311     C* Ordinamento SFL x RAG.SOCIALE
045400090311     C                   EXSR      Ordina_S1
045500090316     c*
045600090316     c* Posizionamento
045700090316     c                   if        posizionamento = 'S'
045800090316     c                   eval      posizionamento = 'N'
045900090316     C                   EXSR      INIZIA_CON
046000090316     c                   end
046100090316     C*----------
046200090316     C*  Reimposta i puntamenti e le ricerche
046300090316     c                   eval      c1pos   = sav_pos
046400090316     c                   eval      c1cerca = sav_cerca
046500090316     C*
046600940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
046700030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
046800940127     C                   MVR                     RESTO             3 0
046900030114     C     PAGINE        MULT      SFLPAG        C1RCD
0470000301141    C     RESTO         IFGT      0
047100030114     C                   ADD       1             C1RCD
0472000301141E   C                   ELSE
047300030114     C                   SUB       SFLPAG        C1RCD
047400030114     C                   ADD       1             C1RCD
0475000301141-   C                   ENDIF
047600940128     C*
047700940127     C                   ENDSR
047800940128     C************************************************************
047900940131     C* LETTURA RCD ARCHIVIO PILOTA
048000940128     C************************************************************
048100940607     C     REDANA        BEGSR
048200940128     C*
048300940131     C                   MOVEL     *OFF          $EFILE
048400940131     C*
0485009401311    C     $EFILE        DOUEQ     *ON
048600090313     C                   MOVEL     *ON           $RCDOK
048700090313     c                   Read      AITRS01L
048800090311     C*
048900090313     c                   if        %eof(AITRS01L)
049000030113     C                   MOVEL     *ON           $EFILE
049100090313     C                   MOVEL     *OFF          $RCDOK
049200030113     C                   MOVE      $EFILE        *IN33
049300030113     c                   else
049400090318      *  Esclude gli annullati
049500090318     c                   if         trsANN <>' '
049600090318     C                   MOVE      *OFF          $RCDOK
049700090318     c                   end
049800090311      *
049900090415      *  Esclude le Filiali non presenti nell'area di appartenenza
049901170908      *  Esclude se filiale non abilitata al profilo
049902170908     c     trsfil        lookup    $pogn                                  90
049903170908     C  n90              MOVE      *OFF          $RCDOK
050400090415      *
050500090311      * O tutte le società oppure Escluse quelle che hanno una fine Contratto ossia
050600090311      *   solo quelle in essere
050700090316     c                   if        sav_c1EXL = 'S' and TRSDFC > 0
050800090313     C                   MOVE      *OFF          $RCDOK
050900030113     c                   end
051000090313      *
051100090312     C*  società filtrante
051200090313     c                   if        sav_c1soc <> *blank and
051300090316     c                             sav_c1soc <> TRSSOCG
051400090313     C                   MOVE      *OFF          $RCDOK
051500090312     c                   end
051600090312     C*
051700090312     C*  filiale filtrante
051800090313     c                   if        sav_c1fil > *zeros and
051900090316     c                             sav_c1fil <> TRSFIL
052000090313     C                   MOVE      *OFF          $RCDOK
052100090312     c                   end
052200090313     C*
052300090313     C*  senza P.IVA
052400090316     c                   if        TRSIVA = *blank
052500090313     C                   MOVE      *OFF          $RCDOK
052600090313     c                   end
052700090313     C*
052800090313     C* se il record è giusto esce dal ciclo
052900090313     C     $RCDOK        ifeq      *On
053000090313     c                   Leave
053100090313     c                   end
053200090312     C*
053300090311     c                   end
053400090311     C*
0535009401311-   C                   ENDDO
053600940131     C*
053700940131     C                   ENDSR
053800940224     C************************************************************
053900940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
054000940224     C************************************************************
054100940224     C     CLCPAG        BEGSR
054200940224     C* Input :
054300940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
054400940224     C*          cursore
054500940224     C* - SFLPAG = numero rcd per pagina sfl
054600940224     C* Output :
054700940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
054800940224     C*
054900940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
055000940224     C                   MVR                     RESTO             3 0
055100940224     C     RESTO         IFGT      0
055200940224     C                   ADD       1             PAGINE
055300940224     C                   ENDIF
055400940224     C     PAGINE        MULT      SFLPAG        WPAG
055500940224     C*
055600940224     C                   ENDSR
055700090311     C************************************************************
055800090311     C* GESTIONE F03 VIDEO S1
055900090311     C************************************************************
056000090311     C     F03S1         BEGSR
056100090311     C*
056200940309     C                   MOVE      *ON           $FINE
056300940325     C* fine programma
056400940309     C                   ENDSR
056500940309     C/EJECT
056600090313     C************************************************************
056700090313     C* GESTIONE F05 ricarice S1
056800090313     C************************************************************
056900090313     C     F05S1         BEGSR
057000090313     C*
057100090313     C                   eval      $INZS1 = *ON
057200090313     C                   MOVE      'S1'          $GEST
057300090313     C* fine programma
057400090313     C                   ENDSR
057500090313     C/EJECT
057600940309     C************************************************************
057700940309     C* GESTIONE F10 VIDEO S1
057800940314     c* AGGIUNTA RECORD
057900940309     C************************************************************
058000940309     C     F10S1         BEGSR
058100940309     C*
058200090311     C                   RESET                   xtabds
058300090311     C                   MOVEL     '1'           xtaopz
058400030113     C                   MOVE      *ZERO         xtaret
058500030113     C                   MOVE      *ZERO         xtaopr
058600090311     C                   MOVEl     *zeros        xtakey
058700090319     C                   z-add     *zeros        xtarecord
058800030113     C                   MOVE      *BLANKS       KPJBU
058900090311     C                   MOVEL     xtabds        KPJBU
059000090407$004 C                   CALL      'TRMZ70SR6'
059100030113     C                   PARM                    KPJBA
059200090311     C                   MOVEL     KPJBU         xtabds
059300030113      *
059400940309     C* ritorno da PGM gestione
059500940309     C                   EXSR      GESRET
059600090311      *
059700940309     C     WINZS1        IFEQ      *ON
059800940309     C                   MOVE      *ON           $INZS1
059900940309     C* se esistevano già righe sul sfl
060000940309     C* calcolo pagina a cui deve posizionarsi
060100940309     C     WSFL          IFGT      0
060200940309     C                   EXSR      CLCPAG
060300940309     C* altrimenti carico solo la 1a pagina
060400940309     C                   ELSE
060500940309     C                   Z-ADD     SFLPAG        WPAG
060600940309     C                   ENDIF
060700940309     C                   ENDIF
060800940309     C*
060900940309     C                   ENDSR
061000090311     C************************************************************
061100090311     C* GESTIONE F12 VIDEO S1
061200090311     C************************************************************
061300090311     C     F12S1         BEGSR
061400090311     C*
061500090311     C                   MOVE      *ON           $FINE
061600090311     C* fine programma
061700090311     C                   ENDSR
061800090311     C/EJECT
061900940128     C************************************************************
062000940131     C* CONTROLLO TESTATA LISTA
062100940128     C************************************************************
062200940131     C     CTRC1         BEGSR
062300940128     C*
062400940201     C                   MOVE      *OFF          *IN99
062500090312     c                   eval      posizionamento = 'N'
062600090316     c                   clear                   pos_c1rcd
062700090312      *  Posizionamento
062800090316     c                   if          sav_pos <> c1pos
062900090316     c                   eval      sav_pos = c1pos
063000090312     c                   eval      posizionamento = 'S'
063100090316     C                   exsr      INIZIA_CON
063200090316     C                   End
063300090312      *
063400090316     c                   if        sav_cerca <> c1cerca
063500090316     c                             or
063600090316     c                             c1cerca <> *blank
063700090316     c                   eval      sav_cerca = c1cerca
063800090316     C                   eval      $INZS1 = *ON
063900090316     C                   MOVE      'S1'          $GEST
064000090316     C                   End
064100090316      *
064200090312     C*  Decodifiche se inseriti Società o Filiale
064300090312     c                   clear                   c1dfil
064400090312     c                   if        c1fil > *zeros
064500090312     c     c1fil         chain     azorg01l
064600090312     c                   if        %Found(azorg01l)
064700090312     c                   eval      c1dfil = orgdes
064800090312     c                   end
064900090312     c                   end
065000090312     C*
065100090312     c                   clear                   c1dsoc
065200090312     c                   if        c1soc <> *blank
065300090316     C*
065400090316     ** Chiamo passando un id società che esiste.
065500090316     C                   EVAL      prmRqsDataSize = %SIZE(tibsSocI0)
065600090316     C                   EVAL      prmRpyDataSize = %SIZE(tibsSocO0)
065700090316     C                   RESET                   tibsSocI0
065800090316     C                   EVAL      tibsSocI0.idSocieta = c1soc
065900090316     C
066000090316     C                   CALL      'TIBSSOCR'
066100090316     C                   PARM      'GETANAGRAF'  prmRqsOpCode
066200090316     C                   PARM      *BLANK        prmRpyOpCode
066300090316     C                   PARM      *ZERO         prmRpyIdMsg
066400090316     C                   PARM      'TIBSSOCI0'   prmRqsFormato
066500090316     C                   PARM                    tibsSocI0
066600090316     C                   PARM                    prmRqsDataSize
066700090316     C                   PARM      'TIBSSOCO0'   prmRpyFormato
066800090316     C                   PARM                    tibsSocO0
066900090316     C                   PARM                    prmRpyDataSize
067000090316
067100090324     c                   if         PRMRPYIDMSG >= 0 and
067200090324     c                              TIBSSOCO0.IDSOCIETA <> *blank
067300090316     c                   eval      c1dsoc = tibsSocO0.RAGSOCIALE
067400090323     c                   else
067500090324     c                   eval      c1dsoc = 'Società Errata'
067600090323     c                   end
067700090316
067800090312     c                   end
067900090312     C*
068000090313     C*  Se cambiato
068100090313     C                   if        c1fil <> sav_c1fil  or
068200090313     c                             c1soc <> sav_c1soc  or
068300090313     c                             c1exl <> sav_c1exl
068400090313     C*
068500090313     C                   eval      $INZS1 = *ON
068600090313     C                   MOVE      'S1'          $GEST
068700090313     C*
068800090313      *  memorizza x verifica successiva
068900090313     c                   eval        sav_c1soc  =  c1soc
069000090313     C                   eval        sav_c1fil  =  c1fil
069100090313     C                   eval        sav_c1exl  =  c1exl
069200090316     C                   if          c1exl  =  ' '
069300090317     c                   eval        c1exl  =  'S'
069400090316     c                   end
069500090313      *
069600090313     c                   end
069700090316     C*
069800940202     C                   ENDSR
069900940131     C************************************************************
070000940131     C* CONTROLLO OPZIONI LISTA
070100940131     C************************************************************
070200090316     C     Inizia_con    BEGSR
070300940131     C*
070400090316      * ? Secondo ciclo x trovare il posizionamento dal record....
070500090316     C                   DO        RrnLast       S1Nrr
070600090316     C     S1nrr         CHAIN     S1
070700090316     C*
070800090316     c                   if           C1pos <= s1des
070900090316     C* Se deve posizionarsi all'inizio
071000090316     c                   if           C1pos = *blank
071100090316     c                   eval      pos_C1RCD = 1
071200090316     c                   end
071300090316     C* Esce
071400090316     c                   LEAVE
071500090316     c                   else
071600090316     C*
071700090316     c                   eval      pos_C1RCD = S1nrr + 1
071800090316     c                   if        pos_C1RCD > RrnLast
071900090316     c                   eval      pos_C1RCD = RrnLast
072000090316     c                   end
072100090316     C*
072200090316     C* Si tiene in memoria la riga toccata con SFLNXTCHG
072300090316     c                   if        s1opz <>' '
072400090316     C                   MOVE      *ON           *IN32
072500090316     c                   else
072600090316     C                   SetOFF                                       32
072700090316     c                   end
072800090316     C*
072900090316     C                   UPDATE    S1
073000090316     c                   end
073100090316     C*
073200090316     C                   ENDDO
073300090316     C*
073400090316     C                   ENDSR
073500090316     C************************************************************
073600090316     C* CONTROLLO OPZIONI LISTA
073700090316     C************************************************************
073800090316     C     CTRS1         BEGSR
073900090316     C*
074000940202     C                   MOVEL     *OFF          $ESCI
074100940201     C                   SETOFF                                       99
074200090311     C                   clear                   S1OPZ
074300940131     C*
074400940127     C* Leggo il sfl solo se ci sono rcd
0745009401311    C     WMAX          IFGT      0
074600940607     C                   READC     S1                                     21
074700940127     C*
074800940131     C* esce se fine sfl o errore che richiede l'uscita
0749009401312    C     *IN21         DOWEQ     *OFF
075000940131     C     $ESCI         ANDEQ     *OFF
075100940201     C                   Z-ADD     S1NRR         C1RCD
075200940131     C* ctrl su riga
075300940131     C                   EXSR      RECS1
075400090311      *
075500940131     C* gestione opzioni
0756000903113    C     S1OPZ         IFNE      *blank
075700940201     C     *IN99         ANDEQ     *OFF
075800940131     C                   EXSR      OPZS1
0759009401273-   C                   ENDIF
076000090311      *
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
079900940201     C*
080000940131     C                   ENDSR
080100940131     C************************************************************
080200940131     C* GESTIONE OPZIONI LISTA
080300940131     C************************************************************
080400940131     C     OPZS1         BEGSR
080500940201     C*
080600090318     C* Se c'è la data Fine Contratto si può solo visualizzare il dato
080700090318     C*   ma niente altro
080800090318     c                   if        s1opz <> '5' and s1DFC > 0
080900090318     c                   eval      s1opz  = '5'
081000090318     c                   end
081100090318     C*
081200090311     C                   RESET                   xtabds
081300030113     C                   MOVEL     S1OPZ         xtaopz
081400030113     C                   MOVE      *ZERO         xtaret
081500030113     C                   MOVE      *ZERO         xtaopr
081600090317     C                   z-add     H1NRC         campo7_0          7 0
081700090317     C                   MOVEl(p)  campo7_0      xtakey
081800090319     C                   z-add     H1NRRtrs      xtarecord
081900940715     C                   MOVE      *BLANKS       KPJBU
082000090311     C                   MOVEL     xtabds        KPJBU
082100090407$004 C                   CALL      'TRMZ70SR6'
082200940607     C                   PARM                    KPJBA
082300090311     C                   MOVEL     KPJBU         xtabds
082400940201     C*
082500940223     C* ritorno da PGM gestione
082600940223     C                   EXSR      GESRET
082700090311     C*
082800940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0829009402252    C     *IN99         IFEQ      *ON
083000940223     C                   SETON                                        40
0831009402252-   C                   ENDIF
083200940225     C*
083300940131     C                   ENDSR
083400940223     C/EJECT
083500940223     C************************************************************
083600940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
083700940223     C************************************************************
083800940223     C     GESRET        BEGSR
083900940223     C*
084000090318     C                   MOVE      *OFF          WINZS1
084100940223     C* modo di ritorno
0842009402231    C                   SELECT
084300940314    >C* << questi modi di utilizzo dei valori di ritorno dal
084400940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
084500940314    >C*    proposte, normalmente sempre valide, ma modificabili
084600940314    >C*    per situazioni particolari >>
084700940223     C* 1 = F3
084800030113    >C     xtaret        WHENEQ    '1'
084900940224     C                   MOVE      *ON           $ESCI
085000940223     C                   MOVE      *ON           $FINE
085100940223     C* 2 = F12
085200030113    >C     xtaret        WHENEQ    '2'
085300940223     C                   MOVE      *ON           $ESCI
085400940223     C*
0855009402231-   C                   ENDSL
085600940223     C*
085700940223     C* operazione eseguite dal pgm chiamato
085800940223     C*
0859009402231    C                   SELECT
086000940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
086100030113    >C     xtaopr        WHENEQ    '1'
086200940223     C                   MOVE      *ON           WINZS1
086300090318     c                   clear                   c1pos
086400090318     c                   clear                   c1cerca
086500090318     c                   clear                   sav_pos
086600090318     c                   clear                   sav_cerca
086700940223     C*
0868009402231-   C                   ENDSL
086900940223     C*
087000940223     C* funzione non eseguibile per errore :
087100940223     C*
0872009402231    C                   SELECT
087300940223     C* 1 = funzione richiamata chiusa in errore
087400940316    >C*  eventualmente gestire altri codici di errore
087500030113    >C     xtaerr        WHENEQ    '1'
087600940223     C                   MOVE      *ON           $ESCI
087700940223     C                   SETON                                        5299
087800940223     C*
0879009402231-   C                   ENDSL
088000940223     C*
088100940223     C                   ENDSR
088200940223     C/EJECT
088300940131     C************************************************************
088400940131     C* OPERAZIONI INIZIALI
088500940131     C************************************************************
088600090415     C     INIZIO        BEGSR
088700030113     C*
088800030113     C* Reperimento parametri
088900030113     C     *ENTRY        PLIST
089000030113     C                   PARM                    KPJBA
089100090415     C*
089200090415     C* fine programma  lo accende subito
089300090415     C                   SETON                                            LR
089400030113     C*
089500030113     C* Variabili per gestione videate
089600030113     C                   MOVE      *BLANK        $GEST             2
089700030113     C                   MOVE      *BLANK        $FINE             1
089800030113     C                   MOVE      *BLANK        $INZS1            1
089900030113     C                   MOVE      *BLANK        $EFILE            1
090000030113     C                   MOVE      *BLANK        $ESCI             1
090100030113     C                   MOVE      *BLANK        $RCDOK            1
090200030113     C* Indici
090300030113     C                   Z-ADD     0             X                 3 0
090400030113     C                   Z-ADD     0             Y                 3 0
090500940506     C*
090600940506     C* Reperimento tasti di funzione
090700090317
090800090317     ** Inizializzo il programma.
090900090317     C                   CALL      'TIBSSOCR'
091000090317     C                   PARM      'INIT'        prmRqsOpCode
091100090317     C                   PARM                    prmRpyOpCode
091200090317     C                   PARM                    prmRpyIdMsg
091300090311     C*
091400090311     C*  Impostazioni di inizio programma
091500090415     C*
091501170908     c* reperimento dati utente abilitazioni
091502170908     c                   exsr      repdatiute
095300090415     C*
095400940117     C                   ENDSR
095401170908     C*--------------------------------------------------------------------------------------------*
095402170908     C* REPERISCE I DATI UTENTE
095403170908     C*--------------------------------------------------------------------------------------------*
095404170908     C     REPDATIUTE    BEGSR
095405170908     C*
095406170908     C* INIZIALIZZA VARIABILI DI WRK
095407170908     C                   CLEAR                   TIBS34DS
095408170908     C                   CLEAR                   AZUTEDS
095409170908     C                   CLEAR                   DDATIUTE
095410170908     C*
095411170908     C     *DTAARA       DEFINE    §azute        azuteds
095412170908     C     *DTAARA       DEFINE    §datiute      ddatiute
095413170908     C                   IN(E)     *DTAARA
095414170908     C                   IF        %Error
095415170908     C                   EVAL      I34Tla = 'L'
095416170908     C                   CALL      'TIBS34R'
095417170908     C                   PARM                    Tibs34Ds
095418170908     C                   IN        *DTAARA
095419170908     C                   ENDIF
095420170908     c                   Clear                   wabi
095421170908     c                   Clear                   dLat
095422170908
095423170908      * Verifica errori e autorità profilo
095424170908s   1c                   Select
095425170908      * se ho errori nei dati utente esco dal pgm
095426170908w   1c                   When      DutErr = 'E'
095427170908     c                   seton                                        lr
095428170908     c                   return
095429170908      * se non c'è l'abilitazione
095430170908      * --> se 1° livello, abilitazioni al terminal
095431170908      *     se 2° livello, abilitazioni al punto operativo
095432170908w   1c                   When      UteAut = *Blanks
095433170908if  2c                   If        DutLpo = '1'
095434170908     c                   Eval      wabi   = 'TP'
095435170908e   2c                   EndIf
095436170908if  2c                   If        DutLpo = '2'
095437170908     c                   Eval      wabi   = 'PO'
095438170908e   2c                   EndIf
095439170908if  2c                   If        DutLpo = 'S'
095440170908     c                   Eval      wabi   = 'AZ'
095441170908e   2c                   EndIf
095442170908      * carica le abilitazioni del profilo
095443170908x   1c                   Other
095444170908     c                   Movel     UteFaf        Dute01
095445170908     c                   If        §Utecers <> *Blanks
095446170908     c                   Eval      wabi = §utecers
095447170908     c                   eval      wabisl = §utecersl
095448170908     c                   EndIf
095449170908e   1c                   EndSl
095450170908      * controllo se ok l'abilitazione dell'utente
095451170908     c                   Clear                   Tibs02ds
095452170908     c                   Eval      T02Mod = 'C'
095453170908     c                   Eval      T02Sif = knsif
095454170908     c                   Eval      T02Cod = 'LAT'
095455170908     c                   Movel(p)  wabi          T02Ke1
095456170908     c                   Call      'TIBS02R'
095457170908     c                   Parm                    kpjba
095458170908     c                   Parm                    Tibs02ds
095459170908if  1c                   If        T02Err = *Blanks
095460170908     c                   Eval      dLat = T02Uni
095461170908e   1c                   EndIf
095462170908      * errore o non abilitato (impossibile xchè controllato nel filtro)
095463170908if  1c                   If        T02Err <> *Blanks or §LatAbi = 'S' or
095464170908     c                             wabi = 'NO' or wabi = *blank
095465170908     c                   seton                                        98
095466170908     c                   call      'XUTENOAB'
095467170908     c                   parm                    kpjba
095468170908     c                   seton                                        lr
095469170908     c                   return
095470170908   x1c                   endif
095471170908      * Reperimento dei P.O. gestibili dall'utente
095472170908     c                   clear                   TRUL31DS
095473170908     c                   clear                   TRUL31DS2
095474170908     c                   eval      I31abi = wabi
095475170908     c                   eval      I31cdi = DUTdis
095476170908     c                   eval      I31car = DUTare
095477170908     c                   eval      I31cpo = DUTpou
095478170908     c                   call      'TRUL31R'
095479170908     c                   parm                    KPJBA
095480170908     c                   parm                    TRUL31DS
095481170908     c                   parm                    TRUL31DS2
095482170908if  2c                   if        O31pog > *zeros
095483170908     c                   movea     O31pog        $pog
095484170908     c                   movea     O31arg        $arg
095485170908     c                   movea     O31dig        $dig
095486170908x   2c                   else
095487170908     c* impossibile perchè controllato nel filtrp
095488170908     c                   seton                                        lr
095489170908     c                   return
095490170908    1c                   endif
095491170908     c                   do        250           g                 3 0
095492170908     c                   if        $pog(g) > *zero
095493170908     c                   move      $pog(g)       $pogn(g)
095494170908     c                   else
095495170908     c                   move      999           $pogn(g)
095496170908     c                   endif
095497170908     c                   enddo
095498170908     c                   endsr
095500090311     c*--------------------------------------------------------------
095600090311     c* tramite Società e Unità decodifica P.IVA su PROJ
095700090311     c*--------------------------------------------------------------
095800090311     C     Decod_Rag_IVA BegSR
095900090311      **
096000090311      **   Si è portato fuori l'aggancio al Fornitore:
096100090311      **
096200090311      **  Routine x Reperire dati Fornitore:
096300090311      **    La routine in base alla sottonatura può funzionare
096400090311      **     x F=Fornitore/C=Cliente
096500090410     C                   clear                   trmz70s1ds                     Input
096600090410     C                   movel(p)  TRSiva        s1_PartitaIVA                  Input
096700090410     C                   movel(p)  'F'           s1_SottoNatur                  Input "F/C"
096800090410     C                   movel(p)  TRSSOCG       s1_Societa                     Input/Output
096900090410     C                   movel(p)  TRSFIL        s1_Unita                       Input/Output
097000090311      *
097100090316     c                   call      'TRMZ70SR1'
097200090410     C                   PARM                    trmz70s1ds                     Input
097300090311     c*
097400090311     C                   ENDSR
097500090311     C/EJECT
097600090311      * ?-------------------------------------------------------------*?
097700090311      *? Riordina comunque il SFL                                     ?
097800090311      * ?-------------------------------------------------------------*?
097900090311     C     Ordina_S1     BEGSR
098000090311     C*
098100090311     C*  Totale Records Caricati nel SFL
098200090311     c                   eval      wsfl = 1
098300090311     C*
098400090311      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
098500090311      * presente nel subfile --?"Selected"?-- questo è aggiunto al record.
098600090311      * Il campo è usato per selezionare i records dando un ordine a quelli
098700090311      * selezionati davanti ai non selezionati.
098800090311     C                   CLEAR                   QLGSCB
098900090311     C                   CLEAR                   QLGSCB00
099000090311      *
099100090311     c                   select
099200090311      *
099300090311      *?  Ordinamento per Ragione Sociale    ?
099400090311     C                   when      WrkSort = Ord_RAGSOC
099500090311      *
099600090311     C                   EVAL      QLGNBRK    = 1
099700090311
099800090311     ** Il GIRO è in posizione (1) 10 Bytes char ascending
099900090311     C                   EVAL      QLGSP      = 1
100000090311     C                   EVAL      QLGSS      = %SIZE(S1DES)
100100090311     C                   EVAL      QLGDT      = Carattere
100200090311     C                   EVAL      QLGSO      = Ascendente
100300090311     C                   EVAL      QLGKL(1)   = QLGSKL
100400090311      *
100500090311     c                   other
100600090311      *
100700090311     c                   endSL
100800090311      *------------
100900090311      * Load other sort parameters.
101000090311     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
101100090311     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
101200090311     C                   EVAL      QLGRT     = 8
101300090311     C                   EVAL      QLGOKL    = 80
101400090311     C                   EVAL      QLGLKE    = 16
101500090311     C                   EVAL      QLGLSS    = 290
101600090311
101700090311      * Initialize Sort I/O API fields.
101800090311     C                   EVAL      QLGRL00  = QLGRL
101900090311     C                   EVAL      QLGRC00  = 1
102000090311     C                   CLEAR                   QUSEI
102100090311     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
102200090311
102300090311      * First step - Initialize the sort routine.
102400090311     C                   CALL      'QLGSORT'
102500090311     C                   PARM                    QLGSCB
102600090311     C                   PARM                    NotUsed
102700090311     C                   PARM                    NotUsed
102800090311     C                   PARM                    SizeList
102900090311     C                   PARM                    ReturnSize
103000090311     C                   PARM                    QUSEC
103100090311
103200090311      * Next step - Write records to I/O routine.
103300090311     C                   EVAL      QLGRT00 = Put
103400090311
103500090316     C                   DO        RrnLast       S1nrr
103600090316     C     S1nrr         CHAIN     S1
103700090311
103800090311     ** Solo le righe con Selected = 'Y' sono riordinate,
103900090311     ** quindi per fare un ordinamento di tutte le righe
104000090311     ** metto 'Y' sempre.
104100090311     C                   EVAL      Selected  = 'Y'
104200090311
104300090311     C                   CLEAR                   QUSEI
104400090311     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
104500090311      *
104600090311     C                   CALL      'QLGSRTIO'
104700090311     C                   PARM                    QLGSCB00
104800090311     C                   PARM                    SflRcd
104900090311     C                   PARM                    NotUsed
105000090311     C                   PARM                    SizeList
105100090311     C                   PARM                    NotUsed
105200090311     C                   PARM                    QUSEC
105300090311
105400090311     C                   ENDDO
105500090311
105600090311      * Next step - Signal end of input, clear subfile for reload.
105700090311     C                   EVAL      QLGRT00 = EndPut
105800090311     C                   CLEAR                   QUSEI
105900090311     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
106000090311
106100090311     C                   CALL      'QLGSRTIO'
106200090311     C                   PARM                    QLGSCB00
106300090311     C                   PARM                    SflRcd
106400090311     C                   PARM                    NotUsed
106500090311     C                   PARM                    SizeList
106600090311     C                   PARM                    NotUsed
106700090311     C                   PARM                    QUSEC
106800090311      *
  pulizia SFL   ?
106900090316    >C                   EXSR      INZC1
107000090311
107100090311      * Final step - Write the records back to the subfile.
107200090311     C                   EVAL      QLGRT00 = Get
107300090311
107400090316     C                   DO        RrnLast       S1nrr
107500090311     C                   CLEAR                   QUSEI
107600090311     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
107700090311     C                   CALL      'QLGSRTIO'
107800090311     C                   PARM                    QLGSCB00
107900090311     C                   PARM                    NotUsed
108000090311     C                   PARM                    SflRcd
108100090311     C                   PARM                    QLGRL00
108200090311     C                   PARM                    NotUsed
108300090311     C                   PARM                    QUSEC
108400090311      * SFLnxtCHG
108500090311     c                   if        s1opz <> *blank
108600090311     c                   seton                                        32
108700090311     c                   else
108800090311     C                   SetOFF                                       32
108900090311     c                   end
109000090316      *
109100090311     C                   WRITE     S1
109200090311     C                   ENDDO
109300090311      *
109400090311      *  All'uscita di queste chiamate a routine di sistema il SFL record
109500090311      *   si incrementa stranamente quindi lo reimposto correttamente
109600090311      *    prima di lasciare la routine.
109700090311     C                   EVAL      S1nrr = RrnLast
109800090311     C*
109900090311     C                   ENDSR
110000090311     C/EJECT
110100090311      * ?-------------------------------------------------------------*?
