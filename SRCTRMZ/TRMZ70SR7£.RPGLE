000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200150612      *______________________________________________________________________________________
000300940307      *  21           GENERICO OPERAZIONI I/O
000400940224      *  22           GENERICO ERRORE OPERAZIONI I/O
000500940224      *  30           SFLDSP
000600940224      * N31           SFLCLR
000700940224      *  31           SFLDSPCTL
000800940224      *  32           SFLNXTCHG
000900940224      *  33           SFLEND
001000940224      *  39           OF PRTF
001100940224      *  40 <---> 49  DSPATR ERRORI SU SFL
001200940608      *  Specificare l'uso dei singoli indicatori
001300940224      *  50 <---> 98  ERRORI SU VIDEO
001400940608      *  Specificare l'uso dei singoli indicatori
001500940506      *  97           ERRORE SPECIALE : TASTO   NON ABIL.
001600940223      *  98           ERRORE SPECIALE : RICERCA NON ABIL. NELLA POSIZ.
001700940224      *  99           INDIC. GENERALE DI ERRORE
001800940128     F*----------------------------------------------------*
001900150611     Faitrc01l  if   E           k disk    infds(dsaitrc)
002000090311      *
002100150611     FTRMZ70SD7 CF   E             WORKSTN
002200940607     F                                     SFILE(S1:S1NRR)
002300940201     F                                     INFDS(DSFMT)
002400940128     D*----------------------------------------------------*
002500940211     D* Passaggio Parametri
002600940211     D KPJBA         E DS
002700090408     D KPJBus          s                   like(kpjbu)
002800090311     D anRCO98j      E DS
002900090408     D trMZ70s1DS    E DS
003000090311      *-------------
003100150611     D dsaitrc         DS
003200150611     D  rrnaitrc             397    400b 0
003300030113      *-------------
003400940325     D* Parametri in ricezione
003500150623     D TRMZ70S7DS    E DS
003600150622     D TRMZ70S8DS    E DS
003700940211     D*-------------
003800940211     D DSFMT           DS           512
003900940506     D  $TASTO               369    369
004000940211     D  NRG                  370    370
004100940211     D  NCL                  371    371
004200940211     D  SFLNRR               378    379B 0
004300940207     D*-------------
004400940127     D* Reperimento nome PGM
004500940127     D STATUS         SDS           333
004600090312     D* Nome PGM a video
004700940127     D  DSPGM            *PROC
004800090311     D*-------------
004900090316     D sav_pos         S                   Like(c1pos)
005000090416     D sav_pos_x_INZ   S                   Like(c1pos)
005100090316     D posizionamento  S              1    inz('N')
005200090316     D Wlen            S              3i 0
005300090316     D Wpos            S              3i 0
005400090316$003 D POS_C1RCD       S                   Like(C1rcd)
005500090316     D sav_cerca       S                   Like(c1cerca)
005600090311      *
005700090311     D dataiso         s               d   datfmt(*iso)
005800090311     D dataeur         s               d   datfmt(*eur)
005900090311     D dataDMY         s               d   datfmt(*dmy)
006000090311      *
006100030113     D*-------------
006200030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
006300030113$003 D S1NRR           S                   Like(C1rcd)
006400030113$003 D WSfl            S                   Like(C1nrr)
006500030113$003 D Wmax            S                   Like(C1rcd)
006600030113$003 D Wpag            S                   Like(C1rcd)
006700030113$003 D Winzs1          S                   Like($inzs1)
006800940207     D*-------------
006900940211     D* COSTANTI
007000940211     D*-------------
007100090326     D SFLPAG          C                   CONST(11)
007200940314     D* dimensione della schiera $MS1
007300940506     D*
007400940506     D* Tasti di funzione
007500940506     D F01             C                   CONST(X'31')
007600940506     D F02             C                   CONST(X'32')
007700940506     D F03             C                   CONST(X'33')
007800940506     D F04             C                   CONST(X'34')
007900940506     D F05             C                   CONST(X'35')
008000940506     D F06             C                   CONST(X'36')
008100940506     D F07             C                   CONST(X'37')
008200940506     D F08             C                   CONST(X'38')
008300940506     D F09             C                   CONST(X'39')
008400940506     D F10             C                   CONST(X'3A')
008500940506     D F11             C                   CONST(X'3B')
008600940506     D F12             C                   CONST(X'3C')
008700940506     D F13             C                   CONST(X'B1')
008800940506     D F14             C                   CONST(X'B2')
008900940506     D F15             C                   CONST(X'B3')
009000940506     D F16             C                   CONST(X'B4')
009100940506     D F17             C                   CONST(X'B5')
009200940506     D F18             C                   CONST(X'B6')
009300940506     D F19             C                   CONST(X'B7')
009400940506     D F20             C                   CONST(X'B8')
009500940506     D F21             C                   CONST(X'B9')
009600940506     D F22             C                   CONST(X'BA')
009700940506     D F23             C                   CONST(X'BB')
009800940506     D F24             C                   CONST(X'BC')
009900940506     D ENTER           C                   CONST(X'F1')
010000940506     D ROLDWN          C                   CONST(X'F4')
010100940506     D ROLLUP          C                   CONST(X'F5')
010200090311      **********************************************************************
010300090311      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
010400090311      **********************************************************************
010500090311      *
010600090311      * ?Indice di schiera dei campi chiave di ordinamento del SFL  (MAXkey)
010700090316     D MaxKey          C                   1
010800090316$xxx D Ord_Ragsoc      C                   '1'
010900090311     D Ascendente      C                   1
011000090311     D Discendente     C                   2
011100090311     D Carattere       C                   6
011200090311     D Put             C                   1
011300090311     D EndPut          C                   2
011400090311     D Get             C                   3
011500090311     D Numerico        C                   8
011600090311      **********************************************************************
011700090311      * Campi utili:
011800090311      *     RRN1       - Numero relativo di record del Subfile
011900090311      *     SizeList   - Dimensione della lista
012000090311      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
012100090311      **********************************************************************
012200090311     D Rrn1            S              5I 0
012300090311     D NotUsed         S             16A
012400090311     D ReturnSize      S              9B 0
012500090311     D SizeList        S              9B 0
012600090311     D RrnLast         S              5I 0
012700090316     D WrkSort         S              1    inz('1')
012800090311      **********************************************************************
012900090311      * Data Structures
013000090311      *     SflRcd     - L'intero record del SFL da passare x l'ordinamento
013100090311      *     QLGSCB     - The sort request block for the QLGSORT API
013200090311      *     QLGSCB00   - The sort request block for the QLGSRTIO API
013300090311      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
013400090311      *     QUSEC      - Error structure for the QLGSORT API
013500090311      **********************************************************************
013600090311     D SflRcd          DS
013700090311     D  S1des
013800150612     D  S1des2
013900090311     D  S1opz
014000150612     d  H1NRRTRc
014100150616     d  H1iva
014200150616     d  H1ivaa
014300090311     D  Selected                      1A
014400090311
014500090311      /COPY QSYSINC/QRPGLESRC,QLGSORT
014600090311     D QLGKL                         16    DIM(MaxKey)
014700090311     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
014800090311     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
014900090311     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
015000090311     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
015100090311
015200090311      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
015300090311      /COPY QSYSINC/QRPGLESRC,QUSEC
015400090316
015500090316     ***************************************************************************
015600090316     **
015700090316     ** Definizione variabili modulo/programma.
015800090316     **
015900090316     ***************************************************************************
016000090316     D prmRqsOpCode...
016100090316     D                 S             10A
016200090316     D prmRpyOpCode...
016300090316     D                 S             10A
016400090316     D prmRpyIdMsg...
016500090316     D                 S             10I 0
016600090316     D prmRqsFormato...
016700090316     D                 S             10A
016800090316     D prmRqsDataSize...
016900090316     D                 S             10I 0
017000090316     D prmRpyFormato...
017100090316     D                 S             10A
017200090316     D prmRpyDataSize...
017300090316     D                 S             10I 0
017400090316
017500940127     C*----------------------------------------------------*
017600090316      *?     MAIN LINE PROGRAM     ?
017700940127     C*----------------------------------------------------*
017800940223     C* inizializzazione variabili
017900940223     C                   EXSR      INZVAR
018000940223     C*
018100940223     C     $FINE         DOWEQ     *OFF
018200940131     C     $GEST         CASEQ     'S1'          GESS1
018300940117     C                   END
018400940117     C                   END
018500090317     C*
018600940325     C* fine programma
018700940325     C                   SETON                                            LR
018800030113     C************************************************************
018900030113     C* INIZIALIZZAZIONE VARIABILI
019000030113     C************************************************************
019100030113     C     INZVAR        BEGSR
019200030113     C*
019300030113     C* Pulizia campi e indicatori
019400030113     C                   MOVE      *ALL'0'       IN4049           10
019500030113     C                   MOVEA     IN4049        *IN(40)
019600030113     C                   CLEAR                   S1OPZ
019700030113     C* Variabili per gestione videate
019800030113     C*
019900030113     C                   MOVE      *OFF          $FINE
020000030113     C                   MOVE      *OFF          $INZS1
020100030113     C                   MOVE      *OFF          $EFILE
020200030113     C                   MOVE      *OFF          $ESCI
020300030113     C                   MOVE      *OFF          $RCDOK
020400030113     C                   Z-ADD     0             $ULKS1            3 0
020500030113     C*
020600030113     C                   MOVE      *ON           $INZS1
020700030113     C                   MOVE      'S1'          $GEST
020800090316     C                   clear                   c1pos
020900090316     C                   clear                   sav_pos
021000030113     C*
021100030113     C* Variabili appoggio
021200030114     C                   Z-ADD     1             WPAG
021300030113     c*
021400030113     C                   ENDSR
021500940127     C************************************************************
021600940131     C* GESTIONE LISTA
021700940127     C************************************************************
021800940127     C     GESS1         BEGSR
021900030113     C*
022000940223     C* inizializzazione videata
022100940223     C     $INZS1        IFEQ      *ON
022200940127     C                   EXSR      INZS1
022300940223     C                   MOVE      *OFF          $INZS1
022400940127     C                   ENDIF
022500030113     C*
022600030113     C* emissione piede videata
022700030113     C                   WRITE     Z1
022800030113     C* Non ci sono records
022900940223     C     WMAX          IFEQ      0
023000940607     C                   WRITE     D1
023100090316     c                   setoff                                       30
023200030114     C                   Else
023300090316     c                   seton                                        30
023400090316      *
023500090316      * se si è effettuato un Posizionamento
023600090316     C                   IF        POS_c1rcd  > 0  and
023700090316     C                             POS_c1rcd  <= Wmax
023800090316     C                   Z-ADD     POS_c1rcd     C1RCD
023900090316     C                   Z-ADD     POS_c1rcd     C1nrr
024000090316     c                   else
024100030114     C     Wsfl          IFgt      0
024200030114     C                   Z-ADD     wsfl          C1RCD
024300030114     C                   Else
024400030114     C     Wpag          IFgt      0
024500030114     C                   Z-ADD     wpag          C1RCD
024600030114     C                   EndIF
024700090316     C                   EndIF
024800030114     C                   EndIF
024900030114     C                   ENDIF
025000090316      *
025100940127     C*
025200030113     C*              *------------------*
025300940607     C                   EXFMT     C1
025400030113     C*              *------------------*
025500030113     C*
025600940204     C     C1NRR         IFNE      0
025700940204     C                   Z-ADD     C1NRR         WSFL
025800090313     C                   Else
025900090313     C                   Z-ADD     SFLNRR        wsfl
026000090313     C                   ENDIF
026100090313      *
026200940127     C                   Z-ADD     SFLNRR        C1RCD
026300030113     C* Selezioni
0264009401271    C                   SELECT
026500940127     C* F3=Fine
026600940506     C     $TASTO        WHENEQ    F03
026700940309     C                   EXSR      F03S1
026800090313     C* F5=Ricarica SFL
026900090313     C     $TASTO        WHENEQ    F05
027000090313     C                   EXSR      F05S1
027100940131     C* F10=Immissione
027200940506     C     $TASTO        WHENEQ    F10
027300940309     C                   EXSR      F10S1
027400090311     C* F12=Ritorno
027500090311     C     $TASTO        WHENEQ    F12
027600090311     C                   EXSR      F12S1
0277009401271O   C                   OTHER
027800940127     C* CONTROLLO DATI
027900940131     C                   EXSR      CTRC1
028000940201     C     *IN99         IFEQ      *OFF
028100940131     C                   EXSR      CTRS1
028200940131     C                   END
0283009401271-   C                   ENDSL
028400940127     C*
028500940127     C                   ENDSR
028600940224     C/EJECT
028700940127     C************************************************************
028800940131     C* INIZIALIZZAZIONE LISTA
028900940127     C************************************************************
029000090312     C     INZC1         BEGSR
029100940302     C* pulizia SFL
029200090312     C                   SETOFF                                         3031
029300090312     C                   WRITE     C1
029400090312     C                   SETON                                          31
029500090312     C*
029600090312     C* CARICAMENTO SFL totale
029700090312     C                   Z-ADD     1             C1RCD
029800090312     c                   movel     DSPGM         c1pgm
029900090312     c                   movel     knmus         c1mus
030000090312     c                   movel     knsif         c1sif
030100090312     C*
030200090312     C                   ENDSR
030300090312     C/EJECT
030400090312     C************************************************************
030500090312     C* INIZIALIZZAZIONE LISTA
030600090312     C************************************************************
030700090312     C     INZS1         BEGSR
030800090312     C*
030900090312     C* pulizia SFL
031000090312    >C                   EXSR      INZC1
031100090312     C*
031200090312     C                   Z-ADD     0             S1NRR
031300090312     C                   Z-ADD     0             WMAX
031400090312     C                   Z-ADD     0             wsfl
031500940224     C*
031600940224     C* Posizionamento su file pilota
031700150612     c     *loval        setll     aitrc01l
031800940608    >C                   EXSR      REDANA
031900030113     C* Carico il SFL
032000940127     C                   EXSR      ROLS1
032100030113     C*
032200150623     c                   if        ds7opr <> '1'
032300030114     C                   Z-ADD     1             WPAG
032400030114     c                   end
032500940127     C*
032600940127     C                   ENDSR
032700940127     C************************************************************
032800940131     C* CARICAMENTO PAGINA LISTA
032900940127     C************************************************************
033000940127     C     ROLS1         BEGSR
033100940127     C*
033200940128     C                   SETOFF                                       32
033300940223     C                   Z-ADD     0             Y
033400940127     C                   Z-ADD     WMAX          S1NRR
033500090316     c                   clear                   Wpos
033600940127     C*
033700940127     C* Leggo dal file anagrafico per caricare la lista
0338009401311    C     $EFILE        DOWEQ     *OFF
033900940127     C*
034000030113     c                   clear                   s1opz
034100090311     C*
034200090316     c                   exsr      Decod_Rag_IVA
034300090316     C*
034400090316     C*   Ricerca x Stringa attivata
034500090316     c                   if        c1cerca <> *blank
034600090316     c                   eval      WLen = %len(%Trim(c1cerca))
034700130910     c
034800130910     c                   eval      WPos =
034900130910     c                              %scan(%trim(c1cerca) : RagSocKSC)
035000090316     c                   end
035100090316     C*
035200090316     c                   if        sav_pos <=  RagSocKSC and sav_pos <> *blank
035300090316     c                             or
035400090316     c                             sav_pos  =  *blank
035500090316     C*
035600090316     c                   if        Wpos > 0 and c1cerca <> *BLANK
035700090316     c                             or
035800090316     c                             C1CERCA = *BLANK
035900090316     C*
036000090311     c                   eval      s1des   = RagSocKSC
036100150611     c                   eval      h1NRRtrc= rrnaitrc
036200150625     c                   eval      h1iva  = trciva
036300150616     c                   eval      h1ivaa = trcpicf
036400150616     c                   eval      s1des2 = trcragsoc
036500940127     C*
036600940127     C                   ADD       1             S1NRR
036700090311     C                   EVAL      RrnLast = S1nrr
036800940127     C                   ADD       1             Y
036900940127     C*
037000940607     C                   WRITE     S1
037100090316     c                   end
037200090313     C*
037300090313     C* esce x il max. records consentiti
037400090313     c                   if        s1nrr = 9999
037500090313     c                   leave
037600090313     c                   end
037700090316     c                   end
037800940131     C*
037900940316    >C                   EXSR      REDANA
038000940128     C*
0381009401271-   C                   ENDDO
038200940127     C*
038300940223     C                   Z-ADD     S1NRR         WMAX                 30
038400090311     C*
038500090311     C* Ordinamento SFL x RAG.SOCIALE
038600090311     C                   EXSR      Ordina_S1
038700090316     c*
038800090416     c                   if        sav_pos_x_INZ <> *blank
038900090416     c                   eval      sav_pos = sav_pos_x_INZ
039000090416     c                   eval      posizionamento = 'S'
039100090416     c                   clear                   sav_pos_x_INZ
039200090416     c                   end
039300090316     c* Posizionamento
039400090316     c                   if        posizionamento = 'S'
039500090316     c                   eval      posizionamento = 'N'
039600090316     C                   EXSR      INIZIA_CON
039700090316     c                   end
039800090316     C*----------
039900090316     C*  Reimposta i puntamenti e le ricerche
040000090316     c                   eval      c1pos   = sav_pos
040100090316     c                   eval      c1cerca = sav_cerca
040200090316     C*
040300940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
040400030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
040500940127     C                   MVR                     RESTO             3 0
040600030114     C     PAGINE        MULT      SFLPAG        C1RCD
0407000301141    C     RESTO         IFGT      0
040800030114     C                   ADD       1             C1RCD
0409000301141E   C                   ELSE
041000030114     C                   SUB       SFLPAG        C1RCD
041100030114     C                   ADD       1             C1RCD
0412000301141-   C                   ENDIF
041300940128     C*
041400940127     C                   ENDSR
041500940128     C************************************************************
041600940131     C* LETTURA RCD ARCHIVIO PILOTA
041700940128     C************************************************************
041800940607     C     REDANA        BEGSR
041900940128     C*
042000940131     C                   MOVEL     *OFF          $EFILE
042100940131     C*
0422009401311    C     $EFILE        DOUEQ     *ON
042300090313     C                   MOVEL     *ON           $RCDOK
042400150611     c                   Read      AITRc01L
042500090311     C*
042600150611     c                   if        %eof(AITRc01L)
042700030113     C                   MOVEL     *ON           $EFILE
042800090313     C                   MOVEL     *OFF          $RCDOK
042900030113     C                   MOVE      $EFILE        *IN33
043000030113     c                   else
043100090318      *  Esclude gli annullati
043200150612     c                   if         trcANN <>' '
043300090318     C                   MOVE      *OFF          $RCDOK
043400090318     c                   end
043500150625      *  Seleziona in caso di ricerca solo la partita iva
043600150618     c                   if         *in04 and
043700150625     c                               trciva <> ds7iva
043800150618     C                   iter
043900150618     c                   end
044000090313     C*
044100090313     C* se il record è giusto esce dal ciclo
044200090313     C     $RCDOK        ifeq      *On
044300090313     c                   Leave
044400090313     c                   end
044500090312     C*
044600090311     c                   end
044700090311     C*
0448009401311-   C                   ENDDO
044900940131     C*
045000940131     C                   ENDSR
045100940224     C************************************************************
045200940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
045300940224     C************************************************************
045400940224     C     CLCPAG        BEGSR
045500940224     C* Input :
045600940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
045700940224     C*          cursore
045800940224     C* - SFLPAG = numero rcd per pagina sfl
045900940224     C* Output :
046000940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
046100940224     C*
046200940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
046300940224     C                   MVR                     RESTO             3 0
046400940224     C     RESTO         IFGT      0
046500940224     C                   ADD       1             PAGINE
046600940224     C                   ENDIF
046700940224     C     PAGINE        MULT      SFLPAG        WPAG
046800940224     C*
046900940224     C                   ENDSR
047000090311     C************************************************************
047100090311     C* GESTIONE F03 VIDEO S1
047200090311     C************************************************************
047300090311     C     F03S1         BEGSR
047400090311     C*
047500940309     C                   MOVE      *ON           $FINE
047600940325     C* fine programma
047700940309     C                   ENDSR
047800940309     C/EJECT
047900090313     C************************************************************
048000090313     C* GESTIONE F05 ricarice S1
048100090313     C************************************************************
048200090313     C     F05S1         BEGSR
048300090313     C*
048400090313     C                   eval      $INZS1 = *ON
048500090313     C                   MOVE      'S1'          $GEST
048600090313     C* fine programma
048700090313     C                   ENDSR
048800090313     C/EJECT
048900940309     C************************************************************
049000940309     C* GESTIONE F10 VIDEO S1
049100940314     c* AGGIUNTA RECORD
049200940309     C************************************************************
049300940309     C     F10S1         BEGSR
049400940309     C*
049500150622     C                   RESET                   TRMZ70S8DS
049600150622     C                   MOVEL     '1'           ds8opz
049700150622     C                   MOVE      *ZERO         ds8ret
049800150622     C                   MOVE      '1'           ds8opr
049900030113     C                   MOVE      *BLANKS       KPJBU
050000150622     C                   MOVEL     TRMZ70S8DS    KPJBU
050100150611$004 C                   CALL      'TRMZ70SR8'
050200030113     C                   PARM                    KPJBA
050300150622     C                   MOVEL     KPJBU         TRMZ70S8DS
050400030113      *
050500940309     C* ritorno da PGM gestione
050600940309     C                   EXSR      GESRET
050700090311      *
050800940309     C     WINZS1        IFEQ      *ON
050900940309     C                   MOVE      *ON           $INZS1
051000940309     C* se esistevano già righe sul sfl
051100940309     C* calcolo pagina a cui deve posizionarsi
051200940309     C     WSFL          IFGT      0
051300940309     C                   EXSR      CLCPAG
051400940309     C* altrimenti carico solo la 1a pagina
051500940309     C                   ELSE
051600940309     C                   Z-ADD     SFLPAG        WPAG
051700940309     C                   ENDIF
051800940309     C                   ENDIF
051900940309     C*
052000940309     C                   ENDSR
052100090311     C************************************************************
052200090311     C* GESTIONE F12 VIDEO S1
052300090311     C************************************************************
052400090311     C     F12S1         BEGSR
052500090311     C*
052600150619     c                   clear                   kpjbu
052700090311     C                   MOVE      *ON           $FINE
052800090311     C* fine programma
052900090311     C                   ENDSR
053000090311     C/EJECT
053100940128     C************************************************************
053200940131     C* CONTROLLO TESTATA LISTA
053300940128     C************************************************************
053400940131     C     CTRC1         BEGSR
053500940128     C*
053600940201     C                   MOVE      *OFF          *IN99
053700090312     c                   eval      posizionamento = 'N'
053800090316     c                   clear                   pos_c1rcd
053900090312      *  Posizionamento
054000090316     c                   if          sav_pos <> c1pos
054100090316     c                   eval      sav_pos = c1pos
054200090312     c                   eval      posizionamento = 'S'
054300090316     C                   exsr      INIZIA_CON
054400090316     C                   End
054500090312      *
054600090316     c                   if        sav_cerca <> c1cerca
054700090316     c                             or
054800090316     c                             c1cerca <> *blank
054900090316     c                   eval      sav_cerca = c1cerca
055000090316     C                   eval      $INZS1 = *ON
055100090316     C                   MOVE      'S1'          $GEST
055200090316     C                   End
055300090312     C*
055400090416     c                   clear                   sav_pos_x_INZ
055500090313      *
055600940202     C                   ENDSR
055700940131     C************************************************************
055800940131     C* CONTROLLO OPZIONI LISTA
055900940131     C************************************************************
056000090316     C     Inizia_con    BEGSR
056100940131     C*
056200090316      * ? Secondo ciclo x trovare il posizionamento dal record....
056300090316     C                   DO        RrnLast       S1Nrr
056400090316     C     S1nrr         CHAIN     S1
056500090316     C*
056600090316     c                   if           C1pos <= s1des
056700090316     C* Se deve posizionarsi all'inizio
056800090316     c                   if           C1pos = *blank
056900090316     c                   eval      pos_C1RCD = 1
057000090316     c                   end
057100090316     C* Esce
057200090316     c                   LEAVE
057300090316     c                   else
057400090316     C*
057500090316     c                   eval      pos_C1RCD = S1nrr + 1
057600090316     c                   if        pos_C1RCD > RrnLast
057700090316     c                   eval      pos_C1RCD = RrnLast
057800090316     c                   end
057900090316     C*
058000090316     C* Si tiene in memoria la riga toccata con SFLNXTCHG
058100090316     c                   if        s1opz <>' '
058200090316     C                   MOVE      *ON           *IN32
058300090316     c                   else
058400090316     C                   SetOFF                                       32
058500090316     c                   end
058600090316     C*
058700090316     C                   UPDATE    S1
058800090415     C                   SetOFF                                       32
058900090316     c                   end
059000090316     C*
059100090316     C                   ENDDO
059200090316     C*
059300090316     C                   ENDSR
059400090316     C************************************************************
059500090316     C* CONTROLLO OPZIONI LISTA
059600090316     C************************************************************
059700090316     C     CTRS1         BEGSR
059800090316     C*
059900940202     C                   MOVEL     *OFF          $ESCI
060000940201     C                   SETOFF                                       99
060100090311     C                   clear                   S1OPZ
060200940131     C*
060300940127     C* Leggo il sfl solo se ci sono rcd
0604009401311    C     WMAX          IFGT      0
060500940607     C                   READC     S1                                     21
060600940127     C*
060700940131     C* esce se fine sfl o errore che richiede l'uscita
0608009401312    C     *IN21         DOWEQ     *OFF
060900940131     C     $ESCI         ANDEQ     *OFF
061000940201     C                   Z-ADD     S1NRR         C1RCD
061100940131     C* ctrl su riga
061200940131     C                   EXSR      RECS1
061300090311      *
061400940131     C* gestione opzioni
0615000903113    C     S1OPZ         IFNE      *blank
061600940201     C     *IN99         ANDEQ     *OFF
061700940131     C                   EXSR      OPZS1
0618009401273-   C                   ENDIF
061900090311      *
062000090415     c                   setoff                                       32
062100940201     C* se rilevato errore o richiesta uscita, attivo sflnxtchg
0622009402013    C     *IN99         IFEQ      *ON
062300940201     C     $ESCI         OREQ      *ON
062400940131     C                   MOVE      *ON           *IN32
062500940204     C* disabilito l'eventuale richiesta di reinizializzazione sfl;
062600940204     C* la ripristinerò a conclusione del ciclo di READC
062700940223     C                   MOVE      *OFF          $INZS1
0628009402233-   C                   ENDIF
062900940223     C*
063000090311     C                   clear                   S1OPZ
063100940223     C*
063200940607     C                   UPDATE    S1
063300940223     C*
063400940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0635009401313    C     $ESCI         IFEQ      *OFF
063600940607     C                   READC     S1                                     21
063700940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0638009402014    C     *IN21         IFEQ      *ON
063900940201     C                   MOVE      WINZS1        $INZS1
064000940204     C* calcolo pagina a cui deve posizionarsi
064100940224     C                   EXSR      CLCPAG
0642009402014-   C                   ENDIF
0643009402013-   C                   ENDIF
064400940131     C*
0645009401272-   C                   ENDDO
064600940127     C*
0647009401311-   C                   ENDIF
064800940131     C*
064900940127     C                   ENDSR
065000940127     C/EJECT
065100940127     C************************************************************
065200940131     C* CONTROLLO CAMPI I/O RIGA LISTA
065300940127     C************************************************************
065400940131     C     RECS1         BEGSR
065500940131     C*
065600940201     C* reset indicatori DSPATR
065700940201     C                   MOVE      *ALL'0'       IN4049           10
065800940201     C                   MOVEA     IN4049        *IN(40)
065900090415     C*
066000940131     C                   ENDSR
066100940131     C************************************************************
066200940131     C* GESTIONE OPZIONI LISTA
066300940131     C************************************************************
066400940131     C     OPZS1         BEGSR
066500150618     C*opzione di scelta
066600150625     c                   if        s1opz = '1'
066700150618     c                             and  *in04
066800150623     C                   MOVE      '1'           ds7ret
066900150623     C                   MOVE      *zero         ds7opr
067000150625     C                   MOVEl     h1iva         ds7iva
067100150625     C                   MOVEl     h1ivaa        ds7ivaa
067200150623     C                   MOVEL     TRMZ70S7DS    KPJBU
067300150623     C                   MOVE      *ON           $ESCI
067400150623     C                   MOVE      *ON           $FINE
067500150618      *
067600150618     c                   else
067700150618     C*altre opzioni
067800150618     c                   if        not *in04
067900150622     C                   RESET                   TRMZ70S8DS
068000150622     C                   MOVEL     S1OPZ         ds8opz
068100150622     C                   MOVE      *zero         ds8ret
068200150622     C                   MOVE      '1'           ds8opr
068300150625     C                   MOVEl     h1iva         ds8iva
068400150625     C                   MOVEl     h1ivaa        ds8ivaa
068500150618     C                   MOVE      *BLANKS       KPJBU
068600150622     C                   MOVEL     TRMZ70S8DS    KPJBU
068700150618$004 C                   CALL      'TRMZ70SR8'
068800150618     C                   PARM                    KPJBA
068900150622     C                   MOVEL     KPJBU         TRMZ70S8DS
069000150618     c                   endif
069100150618     c                   endif
069200940201     C*
069300940223     C* ritorno da PGM gestione
069400940223     C                   EXSR      GESRET
069500090311     C*
069600940223     C* se riscontrato un errore nella chiamata attivo DSPATR(RI PC)
0697009402252    C     *IN99         IFEQ      *ON
069800940223     C                   SETON                                        40
0699009402252-   C                   ENDIF
070000940225     C*
070100940131     C                   ENDSR
070200940223     C/EJECT
070300940223     C************************************************************
070400940223     C* GESTIONE RITORNO DA PGM DI GESTIONE
070500940223     C************************************************************
070600940223     C     GESRET        BEGSR
070700940223     C*
070800090318     C                   MOVE      *OFF          WINZS1
070900940223     C* modo di ritorno
0710009402231    C                   SELECT
071100940314    >C* << questi modi di utilizzo dei valori di ritorno dal
071200940314    >C*    pgm di manutenzione rcd di anagrafica sono delle
071300940314    >C*    proposte, normalmente sempre valide, ma modificabili
071400940314    >C*    per situazioni particolari >>
071500940223     C* 1 = F3
071600150622    >C     ds8ret        WHENEQ    '1'
071700940224     C                   MOVE      *ON           $ESCI
071800940223     C                   MOVE      *ON           $FINE
071900940223     C* 2 = F12
072000150622    >C     ds8ret        WHENEQ    '2'
072100940223     C                   MOVE      *ON           $ESCI
072200940223     C*
0723009402231-   C                   ENDSL
072400940223     C*
072500940223     C* operazione eseguite dal pgm chiamato
072600940223     C*
0727009402231    C                   SELECT
072800940314     C* 1 = eseguito aggiornamento (richiesto ricaricamento subfile)
072900150622    >C     ds8opr        WHENEQ    '1'
073000940223     C                   MOVE      *ON           WINZS1
073100090318     c                   clear                   c1pos
073200090318     c                   clear                   c1cerca
073300090318     c                   clear                   sav_pos
073400090318     c                   clear                   sav_cerca
073500940223     C*
0736009402231-   C                   ENDSL
073700940223     C*
073800940223     C* funzione non eseguibile per errore :
073900940223     C*
0740009402231    C                   SELECT
074100940223     C* 1 = funzione richiamata chiusa in errore
074200940316    >C*  eventualmente gestire altri codici di errore
074300150622    >C     ds8err        WHENEQ    '1'
074400940223     C                   MOVE      *ON           $ESCI
074500940223     C                   SETON                                        5299
074600940223     C*
0747009402231-   C                   ENDSL
074800940223     C*
074900940223     C                   ENDSR
075000940223     C/EJECT
075100940131     C************************************************************
075200940131     C* OPERAZIONI INIZIALI
075300940131     C************************************************************
075400940131     C     *INZSR        BEGSR
075500030113     C*
075600030113     C* Reperimento parametri
075700030113     C     *ENTRY        PLIST
075800030113     C                   PARM                    KPJBA
075900150618      * chiamata in scelta quindi funziona come selezione ricerca
076000150618      * abilita solo opzione S oppure inserimento
076100150618     C                   if        kpjbu <> *blank
076200150618     c                   seton                                        04
076300150623     c                   movel(p)  kpjbu         TRMZ70S7DS
076400150618     c                   endif
076500030113     C*
076600030113     C* Variabili per gestione videate
076700030113     C                   MOVE      *BLANK        $GEST             2
076800030113     C                   MOVE      *BLANK        $FINE             1
076900030113     C                   MOVE      *BLANK        $INZS1            1
077000030113     C                   MOVE      *BLANK        $EFILE            1
077100030113     C                   MOVE      *BLANK        $ESCI             1
077200030113     C                   MOVE      *BLANK        $RCDOK            1
077300030113     C* Indici
077400030113     C                   Z-ADD     0             X                 3 0
077500030113     C                   Z-ADD     0             Y                 3 0
077600940506     C*
077700940506     C* Reperimento tasti di funzione
077800090311     C*
077900090311     C*  Impostazioni di inizio programma
078000940127     C*
078100940117     C                   ENDSR
078200090311     c*--------------------------------------------------------------
078300150625     c* tramite Unità decodifica P.IVA su PROJ
078400090311     c*--------------------------------------------------------------
078500090311     C     Decod_Rag_IVA BegSR
078600090311      **
078700090311      **   Si è portato fuori l'aggancio al Fornitore:
078800090311      **
078900090311      **  Routine x Reperire dati Fornitore:
079000090311      **    La routine in base alla sottonatura può funzionare
079100090311      **     x F=Fornitore/C=Cliente
079200090408     c                   clear                   trmz70s1DS
079300150611     C                   movel(p)  TRciva        PartitaIVA                     Input
079400090408     C                   movel(p)  'F'           SottoNatur                     Input "F/C"
079500150625     C                   movel(p)  *blank        Societa                        Input/Output
079600090408      *
079700090316     c                   call      'TRMZ70SR1'
079800090408     C                   PARM                    trmz70s1DS                     Input
079900090311     c*
080000090311     C                   ENDSR
080100090311     C/EJECT
080200090311      * ?-------------------------------------------------------------*?
080300090311      *? Riordina comunque il SFL                                     ?
080400090311      * ?-------------------------------------------------------------*?
080500090311     C     Ordina_S1     BEGSR
080600090311     C*
080700090311     C*  Totale Records Caricati nel SFL
080800090311     c                   eval      wsfl = 1
080900090311     C*
081000090311      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
081100090311      * presente nel subfile --?"Selected"?-- questo è aggiunto al record.
081200090311      * Il campo è usato per selezionare i records dando un ordine a quelli
081300090311      * selezionati davanti ai non selezionati.
081400090311     C                   CLEAR                   QLGSCB
081500090311     C                   CLEAR                   QLGSCB00
081600090311      *
081700090311     c                   select
081800090311      *
081900090311      *?  Ordinamento per Ragione Sociale    ?
082000090311     C                   when      WrkSort = Ord_RAGSOC
082100090311      *
082200090311     C                   EVAL      QLGNBRK    = 1
082300090311
082400090311     ** Il GIRO è in posizione (1) 10 Bytes char ascending
082500090311     C                   EVAL      QLGSP      = 1
082600090311     C                   EVAL      QLGSS      = %SIZE(S1DES)
082700090311     C                   EVAL      QLGDT      = Carattere
082800090311     C                   EVAL      QLGSO      = Ascendente
082900090311     C                   EVAL      QLGKL(1)   = QLGSKL
083000090311      *
083100090311     c                   other
083200090311      *
083300090311     c                   endSL
083400090311      *------------
083500090311      * Load other sort parameters.
083600090311     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
083700090311     C                   EVAL      QLGRL     = %SIZE(SflRcd) - 1
083800090311     C                   EVAL      QLGRT     = 8
083900090311     C                   EVAL      QLGOKL    = 80
084000090311     C                   EVAL      QLGLKE    = 16
084100090311     C                   EVAL      QLGLSS    = 290
084200090311
084300090311      * Initialize Sort I/O API fields.
084400090311     C                   EVAL      QLGRL00  = QLGRL
084500090311     C                   EVAL      QLGRC00  = 1
084600090311     C                   CLEAR                   QUSEI
084700090311     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
084800090311
084900090311      * First step - Initialize the sort routine.
085000090311     C                   CALL      'QLGSORT'
085100090311     C                   PARM                    QLGSCB
085200090311     C                   PARM                    NotUsed
085300090311     C                   PARM                    NotUsed
085400090311     C                   PARM                    SizeList
085500090311     C                   PARM                    ReturnSize
085600090311     C                   PARM                    QUSEC
085700090311
085800090311      * Next step - Write records to I/O routine.
085900090311     C                   EVAL      QLGRT00 = Put
086000090311
086100090316     C                   DO        RrnLast       S1nrr
086200090316     C     S1nrr         CHAIN     S1
086300090311
086400090311     ** Solo le righe con Selected = 'Y' sono riordinate,
086500090311     ** quindi per fare un ordinamento di tutte le righe
086600090311     ** metto 'Y' sempre.
086700090311     C                   EVAL      Selected  = 'Y'
086800090311
086900090311     C                   CLEAR                   QUSEI
087000090311     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
087100090311      *
087200090311     C                   CALL      'QLGSRTIO'
087300090311     C                   PARM                    QLGSCB00
087400090311     C                   PARM                    SflRcd
087500090311     C                   PARM                    NotUsed
087600090311     C                   PARM                    SizeList
087700090311     C                   PARM                    NotUsed
087800090311     C                   PARM                    QUSEC
087900090311
088000090311     C                   ENDDO
088100090311
088200090311      * Next step - Signal end of input, clear subfile for reload.
088300090311     C                   EVAL      QLGRT00 = EndPut
088400090311     C                   CLEAR                   QUSEI
088500090311     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
088600090311
088700090311     C                   CALL      'QLGSRTIO'
088800090311     C                   PARM                    QLGSCB00
088900090311     C                   PARM                    SflRcd
089000090311     C                   PARM                    NotUsed
089100090311     C                   PARM                    SizeList
089200090311     C                   PARM                    NotUsed
089300090311     C                   PARM                    QUSEC
089400090311      *
  pulizia SFL   ?
089500090316    >C                   EXSR      INZC1
089600090311
089700090311      * Final step - Write the records back to the subfile.
089800090311     C                   EVAL      QLGRT00 = Get
089900090311
090000090316     C                   DO        RrnLast       S1nrr
090100090311     C                   CLEAR                   QUSEI
090200090311     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
090300090311     C                   CALL      'QLGSRTIO'
090400090311     C                   PARM                    QLGSCB00
090500090311     C                   PARM                    NotUsed
090600090311     C                   PARM                    SflRcd
090700090311     C                   PARM                    QLGRL00
090800090311     C                   PARM                    NotUsed
090900090311     C                   PARM                    QUSEC
091000090311      * SFLnxtCHG
091100090311     c                   if        s1opz <> *blank
091200090311     c                   seton                                        32
091300090311     c                   else
091400090311     C                   SetOFF                                       32
091500090311     c                   end
091600090316      *
091700090311     C                   WRITE     S1
091800090415     C                   SetOFF                                       32
091900090311     C                   ENDDO
092000090311      *
092100090311      *  All'uscita di queste chiamate a routine di sistema il SFL record
092200090311      *   si incrementa stranamente quindi lo reimposto correttamente
092300090311      *    prima di lasciare la routine.
092400090311     C                   EVAL      S1nrr = RrnLast
092500090311     C*
092600090311     C                   ENDSR
092700090311     C/EJECT
092800090311      * ?-------------------------------------------------------------*?
