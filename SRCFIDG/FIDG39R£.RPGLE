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
002100080901     FfiAPD01l  IF   E           K DISK
002200080624     FFIDST01L  IF   E           K DISK
002300080624     FFIDSF01L  IF   E           K DISK
002400080723     FTnTBE01L  IF   E           K DISK
002500080624     FFIDG39D   CF   E             WORKSTN
002600940607     F                                     SFILE(S1:S1NRR)
002700940201     F                                     INFDS(DSFMT)
002800940128     D*----------------------------------------------------*
002900940211     D* Passaggio Parametri
003000940211     D KPJBA         E DS
003100080624     D FIDG39DS      E DS
003200080723     D DDSF          E DS
003300080725     D FIDSFCRIDS    E DS                  prefix(§CRI:4)
003400080725     D FIDSFCUSDS    E DS                  prefix(§CUS:4)
003500080924     D FIDSFRCHDS    E DS                  prefix(§RCH:4)
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
005400030113$003 D EXTDS         E DS                  EXTNAME(DS8A)
005500030113     D*-------------
005600030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
005700030113$003 D S1NRR           S                   Like(C1rcd)
005800030113$003 D WSfl            S                   Like(C1nrr)
005900030113$003 D Wmax            S                   Like(C1rcd)
006000030113$003 D Wpag            S                   Like(C1rcd)
006100030113$003 D Winzs1          S                   Like($inzs1)
006200080723     d dataiso         s               d   datfmt(*iso)
006300080723     d dataeur         s               d   datfmt(*eur)
006400080901     d dst_trovato     s              1
006500080923     d stp_testata     s              1
006600940207     D*-------------
006700940211     D* COSTANTI
006800940211     D*-------------
006900030113     D SFLPAG          C                   CONST(11)
007000940314     D* dimensione della schiera $MS1
007100940506     D*
007200940506     D* Tasti di funzione
007300940506     D F01             C                   CONST(X'31')
007400940506     D F02             C                   CONST(X'32')
007500940506     D F03             C                   CONST(X'33')
007600940506     D F04             C                   CONST(X'34')
007700940506     D F05             C                   CONST(X'35')
007800940506     D F06             C                   CONST(X'36')
007900940506     D F07             C                   CONST(X'37')
008000940506     D F08             C                   CONST(X'38')
008100940506     D F09             C                   CONST(X'39')
008200940506     D F10             C                   CONST(X'3A')
008300940506     D F11             C                   CONST(X'3B')
008400940506     D F12             C                   CONST(X'3C')
008500940506     D F13             C                   CONST(X'B1')
008600940506     D F14             C                   CONST(X'B2')
008700940506     D F15             C                   CONST(X'B3')
008800940506     D F16             C                   CONST(X'B4')
008900940506     D F17             C                   CONST(X'B5')
009000940506     D F18             C                   CONST(X'B6')
009100940506     D F19             C                   CONST(X'B7')
009200940506     D F20             C                   CONST(X'B8')
009300940506     D F21             C                   CONST(X'B9')
009400940506     D F22             C                   CONST(X'BA')
009500940506     D F23             C                   CONST(X'BB')
009600940506     D F24             C                   CONST(X'BC')
009700940506     D ENTER           C                   CONST(X'F1')
009800940506     D ROLDWN          C                   CONST(X'F4')
009900940506     D ROLLUP          C                   CONST(X'F5')
010000080624      **********************************************************************
010100080624      * Constants
010200080624      *     MaxKey - è il max. num. di campi chiave permesso in questo pgm
010300080624      **********************************************************************
010400080624     D MaxKey          C                    3
010500080624$xxx D Data            C                   '1'
010600080624     D Ascendente      C                   1
010700080624     D Discendente     C                   2
010800080624     D Carattere       C                   6
010900080624     D Put             C                   1
011000080624     D EndPut          C                   2
011100080624     D Get             C                   3
011200080624     D Numerico        C                   8
011300080624      **********************************************************************
011400080624      * Campi utili:
011500080624      *     RRN1       - Numero relativo di record del Subfile
011600080624      *     SizeList   - Dimensione della lista
011700080624      *     ReturnSize - Dimensione della lista restituita dall'API di ordinamento
011800080624      **********************************************************************
011900080624     D Rrn1            S              5I 0
012000080624     D NotUsed         S             16A
012100080624     D ReturnSize      S              9B 0
012200080624     D SizeList        S              9B 0
012300080624     D RrnLast         S              5I 0
012400080624     D WrkSort         S              1
012500080624      **********************************************************************
012600080624      * Data Structures
012700080624      *     SflRcd1    - L'intero record del SFL da passare x l'ordinamento
012800080624      *     QLGSCB     - The sort request block for the QLGSORT API
012900080624      *     QLGSCB00   - The sort request block for the QLGSRTIO API
013000080624      *     QLGSKL     - Used to build the key entry (QLGKL ) in QLGSCB
013100080624      *     QUSEC      - Error structure for the QLGSORT API
013200080624      **********************************************************************
013300080624     D SflRcd1         DS
013400080624     D  S1cod
013500080624     D  S1des
013600080723     D  H1DtaOra
013700080723     D  S1dta
013800080723     D  S1ora
013900080725     D  S1pda
014000080725     D  S1nomSPC
014100080624     D  Selected1                     1A
014200080624      * ?------------------------------------------------------------------ */
014300080624      * ?set caratteri Maiuscoli/Minuscoli
014400080624     D Up              C                   CONST('ABCDEFGHIJKLMNOPQRSTUVWXYZ')
014500080624     D Lo              C                   CONST('abcdefghijklmnopqrstuvwxyz')
014600080624      * ?------------------------------------------------------------------ */
014700080624      /COPY QSYSINC/QRPGLESRC,QLGSORT
014800080624     D QLGKL                         16    DIM(MaxKey)
014900080624     D  QLGSP00                       9B 0 OVERLAY(QLGKL:00001)
015000080624     D  QLGSS00                       9B 0 OVERLAY(QLGKL:00005)
015100080624     D  QLGDT00                       9B 0 OVERLAY(QLGKL:00009)
015200080624     D  QLGSO00                       9B 0 OVERLAY(QLGKL:00013)
015300080624
015400080624      /COPY QSYSINC/QRPGLESRC,QLGSRTIO
015500080624      /COPY QSYSINC/QRPGLESRC,QUSEC
015600080624     C*----------------------------------------------------*
015700940127     C*                MAIN LINE PROGRAM
015800940127     C*----------------------------------------------------*
015900080625     C* Reperimento parametri
016000080625     C     *ENTRY        PLIST
016100080625     C                   PARM                    KPJBA
016200080625     C                   eval      fidg39ds = KPJBU
016300080625     C*
016400080625     C* Variabili per gestione videate
016500080625     C                   MOVE      *BLANK        $GEST             2
016600080625     C                   MOVE      *BLANK        $FINE             1
016700080625     C                   MOVE      *BLANK        $INZS1            1
016800080625     C                   MOVE      *BLANK        $EFILE            1
016900080625     C                   MOVE      *BLANK        $ESCI             1
017000080625     C                   MOVE      *BLANK        $RCDOK            1
017100080625     C*
017200080625     C     Kdsf          KLIST
017300080625     C                   KFLD                    DSFNPG
017400080625     C                   KFLD                    DSFNFV
017500080625     C                   KFLD                    DSFFGS
017600080625     C*
017700080625     C     Kdst          KLIST
017800080625     C                   KFLD                    dstNPG
017900080625     C                   KFLD                    dstNFV
018000080625     C                   KFLD                    dstFGS
018100080723      *
018200080723     C     KTbe1         KLIST
018300080723     C                   KFLD                    TBEcod
018400080723     C                   KFLD                    TBEke1
018500080901      *
018600080901     c     Kapd01        klist
018700080901     c                   kfld                    tipo_AUT          1
018800080901     c                   kfld                    dstPDR
018900080901     c                   move      'A'           tipo_AUT
019000080901      *
019100080625     C* Indici
019200080625     C                   Z-ADD     0             X                 3 0
019300080625     C                   Z-ADD     0             Y                 3 0
019400080625     C*
019500080625     C* imposto chiavi
019600080625     C                   z-add     fdg39NPG      DSFNPG
019700080625     C                   move      fdg39NFV      DSFNFV
019800080625     C                   move      fdg39FGS      DSFFGS
019900080625     C*
020000080625     C                   z-add     fdg39NPG      DStNPG
020100080625     C                   move      fdg39NFV      DStNFV
020200080625     C                   move      fdg39FGS      DStFGS
020300080625     C*
020400080901     C*  Aggancia la Distinta per successive decodifiche
020500080901     c                   eval      dst_trovato ='N'
020600080901     c     kDSt          chain     FIDSt01L
020700080901     c                   if        %Found(FIDSt01L)
020800080901     c                   eval      dst_trovato ='S'
020900080901     c                   end
021000080625     C************************************************************
021100940223     C* inizializzazione variabili
021200940223     C                   EXSR      INZVAR
021300940223     C*
021400940223     C     $FINE         DOWEQ     *OFF
021500940131     C     $GEST         CASEQ     'S1'          GESS1
021600940117     C                   END
021700940117     C                   END
021800940325     C* fine programma
021900080625     C                   eval      KPJBU = fidg39ds
022000940325     C                   SETON                                            LR
022100030113     C************************************************************
022200030113     C* INIZIALIZZAZIONE VARIABILI
022300030113     C************************************************************
022400030113     C     INZVAR        BEGSR
022500030113     C*
022600030113     C* Pulizia campi e indicatori
022700030113     C                   MOVE      *ALL'0'       IN4049           10
022800030113     C                   MOVEA     IN4049        *IN(40)
022900030113     C* Variabili per gestione videate
023000030113     C*
023100030113     C                   MOVE      *OFF          $FINE
023200030113     C                   MOVE      *OFF          $INZS1
023300030113     C                   MOVE      *OFF          $EFILE
023400030113     C                   MOVE      *OFF          $ESCI
023500030113     C                   MOVE      *OFF          $RCDOK
023600030113     C                   Z-ADD     0             $ULKS1            3 0
023700030113     C*
023800030113     C                   MOVE      *ON           $INZS1
023900030113     C                   MOVE      'S1'          $GEST
024000030113     C*
024100030113     C* Variabili appoggio
024200030114     C                   Z-ADD     1             WPAG
024300030113     c*
024400030113     C                   ENDSR
024500940127     C************************************************************
024600940131     C* GESTIONE LISTA
024700940127     C************************************************************
024800940127     C     GESS1         BEGSR
024900030113     C*
025000940223     C* inizializzazione videata
025100940223     C     $INZS1        IFEQ      *ON
025200940127     C                   EXSR      INZS1
025300940223     C                   MOVE      *OFF          $INZS1
025400940127     C                   ENDIF
025500030113     C*
025600080625     C*  Forza l'Uscita se non si vuole vedere il video e si vuole
025700080625     C*   avere solo l'informazione che ci sono delle Fasi sulla Distinta
025800080625     c                   if        fdg39VIDno = 'N' or
025900080625     c                             fdg39FASI <> 'S' and fdg39noREC ='N'
026000080625     C                   EXSR      F12S1
026100080625     c                   LeaveSR
026200080625     c                   end
026300080625      *
026400030113     C* emissione piede videata
026500030113     C                   WRITE     Z1
026600030113     C* Non ci sono records
026700940223     C     WMAX          IFEQ      0
026800940607     C                   WRITE     D1
026900030114     C                   Else
027000030114     C     Wsfl          IFgt      0
027100030114     C                   Z-ADD     wsfl          C1RCD
027200030114     C                   Else
027300030114     C     Wpag          IFgt      0
027400030114     C                   Z-ADD     wpag          C1RCD
027500030114     C                   EndIF
027600030114     C                   EndIF
027700030114     C                   ENDIF
027800940127     C*
027900030113     C*              *------------------*
028000940607     C                   EXFMT     C1
028100030113     C*              *------------------*
028200030113     C*
028300940204     C     C1NRR         IFNE      0
028400940204     C                   Z-ADD     C1NRR         WSFL
028500940204     C                   ENDIF
028600940127     C                   Z-ADD     SFLNRR        C1RCD
028700030113     C* Selezioni
0288009401271    C                   SELECT
028900940127     C* F3=Fine
029000080624     C     $TASTO        WHENEQ    F12
029100080624     C                   EXSR      F12S1
0292009401271O   C                   OTHER
029300940127     C* CONTROLLO DATI
029400940131     C                   EXSR      CTRC1
029500940201     C     *IN99         IFEQ      *OFF
029600940131     C                   EXSR      CTRS1
029700940131     C                   END
0298009401271-   C                   ENDSL
029900940127     C*
030000940127     C                   ENDSR
030100940224     C/EJECT
030200940127     C************************************************************
030300940131     C* INIZIALIZZAZIONE LISTA
030400940127     C************************************************************
030500940127     C     INZS1         BEGSR
030600940302     C* pulizia SFL
030700940128     C                   SETOFF                                         3031
030800940607     C                   WRITE     C1
030900940128     C                   SETON                                          31
031000940128     C*
031100030113     C* CARICAMENTO SFL totale
031200940201     C                   Z-ADD     0             S1NRR
031300030113     C                   Z-ADD     1             C1RCD
031400940128     C                   Z-ADD     0             WMAX
031500080624     C                   Z-ADD     fdg39NFV      C1NFV
031600080625     c                   clear                   fdg39FASI
031700940224     C*
031800080901     C* Decodifica l'Autista
031900080901     c                   if        DST_Trovato = 'S'
032000080901     c     Kapd01        chain     fiAPD01l
032100080901     c                   if        %Found(fiAPD01l)
032200080901     c                   eval      c1daut = APDRSC
032300080901     c                   end
032400080901     c                   end
032500080901     C*
032600940224     C* Posizionamento su file pilota
032700080624     c     kDSF          setll     FIDSF01L
032800940608    >C                   EXSR      REDANA
032900030113     C* Carico il SFL
033000940127     C                   EXSR      ROLS1
033100030113     C*
033200030114     C                   Z-ADD     1             WPAG
033300940127     C*
033400940127     C                   ENDSR
033500940127     C************************************************************
033600940131     C* CARICAMENTO PAGINA LISTA
033700940127     C************************************************************
033800940127     C     ROLS1         BEGSR
033900940127     C*
034000940128     C                   SETOFF                                       32
034100940223     C                   Z-ADD     0             Y
034200940127     C                   Z-ADD     WMAX          S1NRR
034300080923     c                   eval      stp_testata = 'S'
034400940127     C*
034500940127     C* Leggo dal file anagrafico per caricare la lista
0346009401311    C     $EFILE        DOWEQ     *OFF
034700940127     C*
034800080723     c                   clear                   S1
034900080624     c                   movel     dsfTRD        s1cod
035000080923     c                   if        dsfTRD ='STP'
035100080923     c                   eval      stp_testata = 'N'
035200080923     c                   end
035300080723     c                   movel     'DSF'         tbecod
035400080723     c                   movel(p)  dsfTRD        tbeke1
035500080723     c     ktbe1         chain     tntbe01l
035600080723     c                   if        %Found(tntbe01l)
035700080723     c                   movel     tbeuni        dDSF
035800080723     c                   movel     D§DSFDES      s1des
035900080723     c                   end
036000080723      *
036100080723     c                   z-add     DSFDTORIN     h1dtaOra
036200080723     c                   movel     DSFDTORIN     s1dta
036300080723     c                   move      S1DTA         dataiso
036400080723     c                   move      dataiso       dataeur
036500080723     c                   move      dataeur       S1dta
036600080723     c                   move      DSFDTORIN     s1ora
036700080725     c                   if        DSFPDAAS4 = 'P'
036800080725     c                   moveL     'Pda'         s1pda
036900080725     c                   ElseIf    DSFPDAAS4 = 'A'
037000080725     c                   moveL     'AS '         s1pda
037100080725     c                   end
037200080725      *
037300080725     c                   if        s1cod = 'CRI'
037400080725     c                   MoveL     DSFdati       FIDSFCRIDS
037500080725     c                   MoveL     §CRISPCDES    s1nomSPC
037600080725     c                   elseIf    s1cod = 'CUS'
037700080725     c                   MoveL     DSFdati       FIDSFCUSDS
037800080725     c                   MoveL     §CUSSPCDES    s1nomSPC
037900080924     c                   elseIf    s1cod = 'RCH'
038000080924     c                   MoveL     DSFdati       FIDSFRCHDS
038100080924     c                   MoveL     §RCHSPCDES    s1nomSPC
038200080725     c                   end
038300940127     C*
038400940127     C                   ADD       1             S1NRR
038500940127     C                   ADD       1             Y
038600080625     c                   move      'S'           fdg39FASI
038700080625     c                   if        fdg39VIDno = 'N'
038800080625     c                   leave
038900080625     c                   end
039000940127     C*
039100940607     C                   WRITE     S1
039200940131     C*
039300940316    >C                   EXSR      REDANA
039400940128     C*
0395009401271-   C                   ENDDO
039600940127     C*
039700080624     C*>>>>>>>>
039800080624     C*  Aggiunge un record se presente la Fase in testata
039900080625     c                   if        fdg39VIDno <> 'N'
040000080901     c*****kDSt          chain     FIDSt01L
040100080901     c********           if        %Found(FIDSt01L) and dstFASd <> *blank
040200080901     c                   if        dst_trovato = 'S' and dstFASd <> *blank
040300080923     c                             and stp_testata = 'S'
040400080723      *
040500080723     c                   clear                   S1
040600080624     c                   movel     dstFASd       s1cod
040700080723     c                   movel     'DSF'         tbecod
040800080723     c                   movel(p)  dstFASd       tbeke1
040900080723     c     ktbe1         chain     tntbe01l
041000080723     c                   if        %Found(tntbe01l)
041100080723     c                   movel     tbeuni        dDSF
041200080723     c                   movel     D§DSFDES      s1des
041300080723     c                   end
041400080723      *
041500080624     C                   ADD       1             S1NRR
041600080624     C                   ADD       1             Y
041700080624     C                   WRITE     S1
041800080624     c                   end
041900080625     c                   end
042000080624     C*>>>>>>>>
042100080624     C*
042200940223     C                   Z-ADD     S1NRR         WMAX                 30
042300080624     C                   EVAL      RrnLast = S1Nrr
042400940127     C*
042500940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
042600940127     C*
042700030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
042800940127     C                   MVR                     RESTO             3 0
042900030114     C     PAGINE        MULT      SFLPAG        C1RCD
0430000301141    C     RESTO         IFGT      0
043100030114     C                   ADD       1             C1RCD
0432000301141E   C                   ELSE
043300030114     C                   SUB       SFLPAG        C1RCD
043400030114     C                   ADD       1             C1RCD
0435000301141-   C                   ENDIF
043600940128     C*
043700080624     C* Ordina  x data
043800080624     C                   If        RrnLast > 1
043900080624     c                   EXSR      Ordina_SFL
044000080624     c                   end
044100080624     C*
044200940127     C                   ENDSR
044300940128     C************************************************************
044400940131     C* LETTURA RCD ARCHIVIO PILOTA
044500940128     C************************************************************
044600940607     C     REDANA        BEGSR
044700940128     C*
044800940131     C                   MOVEL     *OFF          $EFILE
044900940131     C                   MOVEL     *OFF          $RCDOK
045000940131     C*
0451009401311    C     $EFILE        DOUEQ     *ON
045200940131     C     $RCDOK        OREQ      *ON
045300940131     C*
045400080624     c     kDSF          Reade     FIDSF01L
045500080624     c                   if        %eof(FIDSF01L)
045600030113     C                   MOVEL     *ON           $EFILE
045700030113     C                   MOVE      $EFILE        *IN33
045800030113     c                   else
045900030113     C                   MOVE      *ON           $RCDOK
046000030113     c                   end
046100940131     C*
0462009401311-   C                   ENDDO
046300940131     C*
046400940131     C                   ENDSR
046500940224     C************************************************************
046600940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
046700940224     C************************************************************
046800940224     C     CLCPAG        BEGSR
046900940224     C* Input :
047000940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
047100940224     C*          cursore
047200940224     C* - SFLPAG = numero rcd per pagina sfl
047300940224     C* Output :
047400940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
047500940224     C*
047600940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
047700940224     C                   MVR                     RESTO             3 0
047800940224     C     RESTO         IFGT      0
047900940224     C                   ADD       1             PAGINE
048000940224     C                   ENDIF
048100940224     C     PAGINE        MULT      SFLPAG        WPAG
048200940224     C*
048300940224     C                   ENDSR
048400940309     C************************************************************
048500080624     C* Riordina il sfl x Data
048600940309     C************************************************************
048700080624     C     Ordina_SFL    BEGSR
048800080624      *
048900080624     c                   eval      wsfl = 1
049000080624      *
049100080624      * Inizializza i campi chiave x l'ordinamento. C'è un campo in più non
049200080624      * presente nel subfile -- Selected -- questo è aggiunto al record.
049300080624      * Il campo è usato per selezionare i records dando un ordine a quelli
049400080624      * selezionati davanti ai non selezionati.
049500080624     C                   CLEAR                   QLGSCB
049600080624     C                   CLEAR                   QLGSCB00
049700080624
049800080624      *?  Ordinamento per Data.  ?
049900080624     ** 1 campo chiave.
050000080624     C                   EVAL      QLGNBRK    = 1
050100080624     ** La descrizione è a posizione 4, 20 byte, char, descending
050200080723     C                   EVAL      QLGSP      = 1 + %SIZE(S1cod) + %SIZE(S1des)
050300080723     C                   EVAL      QLGSS      = %SIZE(H1DtaOra)
050400080624     C                   EVAL      QLGDT      = Numerico
050500080723     C                   EVAL      QLGSO      = Ascendente
050600080624     C                   EVAL      QLGKL(1)   = QLGSKL
050700080624      * Load other sort parameters.
050800080624     C                   EVAL      QLGLB     = 80 + 16 * MaxKey
050900080624     C                   EVAL      QLGRL     = %SIZE(SflRcd1) - 1
051000080624     C                   EVAL      QLGRT     = 8
051100080624     C                   EVAL      QLGOKL    = 80
051200080624     C                   EVAL      QLGLKE    = 16
051300080624     C                   EVAL      QLGLSS    = 290
051400080624      * Initialize Sort I/O API fields.
051500080624     C                   EVAL      QLGRL00  = QLGRL
051600080624     C                   EVAL      QLGRC00  = 1
051700080624     C                   CLEAR                   QUSEI
051800080624     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
051900080624      * First step - Initialize the sort routine.
052000080624     C                   CALL      'QLGSORT'
052100080624     C                   PARM                    QLGSCB
052200080624     C                   PARM                    NotUsed
052300080624     C                   PARM                    NotUsed
052400080624     C                   PARM                    SizeList
052500080624     C                   PARM                    ReturnSize
052600080624     C                   PARM                    QUSEC
052700080624      * Next step - Write records to I/O routine.
052800080624     C                   EVAL      QLGRT00 = Put
052900080624
053000080624     C                   DO        RrnLast       S1nrr
053100080624     C     S1nrr         CHAIN     S1
053200080624     ** Solo le righe con Selected = 'Y' sono riordinate,
053300080624     ** quindi per fare un ordinamento di tutte le righe
053400080624     ** metto 'Y' sempre.
053500080624     C                   EVAL      Selected1 = 'Y'
053600080624
053700080624     C                   CLEAR                   QUSEI
053800080624     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
053900080624     C                   CALL      'QLGSRTIO'
054000080624     C                   PARM                    QLGSCB00
054100080624     C                   PARM                    SflRcd1
054200080624     C                   PARM                    NotUsed
054300080624     C                   PARM                    SizeList
054400080624     C                   PARM                    NotUsed
054500080624     C                   PARM                    QUSEC
054600080624
054700080624     C                   ENDDO
054800080624
054900080624      * Next step - Signal end of input, clear subfile for reload.
055000080624     C                   EVAL      QLGRT00 = EndPut
055100080624     C                   CLEAR                   QUSEI
055200080624     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
055300080624     C                   CALL      'QLGSRTIO'
055400080624     C                   PARM                    QLGSCB00
055500080624     C                   PARM                    SflRcd1
055600080624     C                   PARM                    NotUsed
055700080624     C                   PARM                    SizeList
055800080624     C                   PARM                    NotUsed
055900080624     C                   PARM                    QUSEC
056000080624     C* pulizia SFL
056100080624     C                   SETOFF                                         3031
056200080624     C                   WRITE     C1
056300080624     C                   SETON                                          31
056400080624      * n°Distinta
056500080624     C                   Z-ADD     fdg39NFV      C1NFV
056600080624      *
056700080624      * Final step - Write the records back to the subfile.
056800080624     C                   EVAL      QLGRT00 = Get
056900080624
057000080624     C                   DO        RrnLast       s1nrr
057100080624     C                   CLEAR                   QUSEI
057200080624     C                   EVAL      QUSBPRV = %SIZE(QUSEC)
057300080624     C                   CALL      'QLGSRTIO'
057400080624     C                   PARM                    QLGSCB00
057500080624     C                   PARM                    NotUsed
057600080624     C                   PARM                    SflRcd1
057700080624     C                   PARM                    QLGRL00
057800080624     C                   PARM                    NotUsed
057900080624     C                   PARM                    QUSEC
058000080624     C                   WRITE     S1
058100080624     C                   ENDDO
058200080624      *
058300080624     C                   if        s1nrr >0
058400080624     C                   SETON                                          30
058500080624     c                   end
058600080624      *
058700080624     C     end_ordina1   ENDSR
058800080624     C************************************************************
058900080624     C* GESTIONE F12 VIDEO S1
059000080624     C************************************************************
059100080624     C     F12S1         BEGSR
059200080624     C*
059300940309     C                   MOVE      *ON           $FINE
059400940325     C* fine programma
059500940309     C                   ENDSR
059600940309     C/EJECT
059700940128     C************************************************************
059800940131     C* CONTROLLO TESTATA LISTA
059900940128     C************************************************************
060000940131     C     CTRC1         BEGSR
060100940128     C*
060200940201     C                   MOVE      *OFF          *IN99
060300940131     C*
060400940202     C                   ENDSR
060500940131     C************************************************************
060600940131     C* CONTROLLO OPZIONI LISTA
060700940131     C************************************************************
060800940131     C     CTRS1         BEGSR
060900940131     C*
061000940202     C                   MOVEL     *OFF          $ESCI
061100940201     C                   SETOFF                                       99
061200940131     C*
061300940127     C* Leggo il sfl solo se ci sono rcd
0614009401311    C     WMAX          IFGT      0
061500940607     C                   READC     S1                                     21
061600940127     C*
061700940131     C* esce se fine sfl o errore che richiede l'uscita
0618009401312    C     *IN21         DOWEQ     *OFF
061900940131     C     $ESCI         ANDEQ     *OFF
062000940201     C                   Z-ADD     S1NRR         C1RCD
062100940131     C* ctrl su riga
062200940131     C                   EXSR      RECS1
062300940607     C                   UPDATE    S1
062400940223     C*
062500940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0626009401313    C     $ESCI         IFEQ      *OFF
062700940607     C                   READC     S1                                     21
062800940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0629009402014    C     *IN21         IFEQ      *ON
063000940201     C                   MOVE      WINZS1        $INZS1
063100940204     C* calcolo pagina a cui deve posizionarsi
063200940224     C                   EXSR      CLCPAG
0633009402014-   C                   ENDIF
0634009402013-   C                   ENDIF
063500940131     C*
0636009401272-   C                   ENDDO
063700940127     C*
0638009401311-   C                   ENDIF
063900940131     C*
064000940127     C                   ENDSR
064100940127     C/EJECT
064200940127     C************************************************************
064300940131     C* CONTROLLO CAMPI I/O RIGA LISTA
064400940127     C************************************************************
064500940131     C     RECS1         BEGSR
064600940131     C*
064700940201     C* reset indicatori DSPATR
064800940201     C                   MOVE      *ALL'0'       IN4049           10
064900940201     C                   MOVEA     IN4049        *IN(40)
065000940201     C*
065100940131     C                   ENDSR
065200940223     C/EJECT
065300940131     C************************************************************
065400940131     C* OPERAZIONI INIZIALI
065500940131     C************************************************************
065600940131     C     *INZSR        BEGSR
065700940117     C                   ENDSR
065800030113     C************************************************************
