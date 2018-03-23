000100970214     H*PARMS OPTION(*NOXREF) DFTACTGRP(*NO) BNDDIR(PRNPGM) ACTGRP(QILE)
000200970214     H*PARMS CVTOPT(*DATETIME)
000300080915     H DECEDIT('0,') DATEDIT(*YMD.)
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
002100080915     FFifgtL1L  IF   E           K DISK    infds(dsFIFGT)
002200080915     FFifgtL0F  uF   E             DISK    rename(FIFGTL00:FIFGT0)
002300080915     FFiapd01L  IF   E           K DISK
002400080915     FTnvr8abD  CF   E             WORKSTN
002500940607     F                                     SFILE(S1:S1NRR)
002600940201     F                                     INFDS(DSFMT)
002700940128     D*----------------------------------------------------*
002800080915     D dsfifgt         DS
002900080915     D  rrnfifgt             397    400b 0
003000080915      *
003100940211     D* Passaggio Parametri
003200940211     D KPJBA         E DS
003300080915     D Tibs36ds      E DS
003400080915     D CNCR80        E DS
003500080915     D UT§DSE        E DS                  EXTNAME(UT§DSE0F)
003600080915     D*
003700080915     D WLBDAT          DS
003800080915     D  G02DAT                 1      8  0
003900080915     D  G02INV                 9     16  0
004000080915     D  G02ERR                17     17
004100080915     D  G02TGI                18     22  0
004200080915      *
004300940211     D*-------------
004400940211     D DSFMT           DS           512
004500940506     D  $TASTO               369    369
004600940211     D  NRG                  370    370
004700940211     D  NCL                  371    371
004800940211     D  SFLNRR               378    379B 0
004900940207     D*-------------
005000940207     D* Nome PGM a video
005100940207     D                 DS
005200940207     D  PROGR                  1     10
005300940207     D  ASTER1                 1      1    INZ('*')
005400940207     D  SIGLA                  2      9
005500940207     D  ASTER2                10     10    INZ('*')
005600940127     D*-------------
005700940127     D* Reperimento nome PGM
005800940127     D STATUS         SDS           333
005900940127     D  DSPGM            *PROC
006000030113     D*-------------
006100030113     C* Variabili appoggio sempre presenti in tutte le anagrafiche
006200030113$003 D S1NRR           S                   Like(C1rcd)
006300030113$003 D WSfl            S                   Like(C1nrr)
006400030113$003 D Wmax            S                   Like(C1rcd)
006500030113$003 D Wpag            S                   Like(C1rcd)
006600030113$003 D Winzs1          S                   Like($inzs1)
006700940207     D*-------------
006800940211     D* COSTANTI
006900940211     D*-------------
007000030113     D SFLPAG          C                   CONST(11)
007100940314     D* dimensione della schiera $MS1
007200940506     D*
007300940506     D* Tasti di funzione
007400940506     D F01             C                   CONST(X'31')
007500940506     D F02             C                   CONST(X'32')
007600940506     D F03             C                   CONST(X'33')
007700940506     D F04             C                   CONST(X'34')
007800940506     D F05             C                   CONST(X'35')
007900940506     D F06             C                   CONST(X'36')
008000940506     D F07             C                   CONST(X'37')
008100940506     D F08             C                   CONST(X'38')
008200940506     D F09             C                   CONST(X'39')
008300940506     D F10             C                   CONST(X'3A')
008400940506     D F11             C                   CONST(X'3B')
008500940506     D F12             C                   CONST(X'3C')
008600940506     D F13             C                   CONST(X'B1')
008700940506     D F14             C                   CONST(X'B2')
008800940506     D F15             C                   CONST(X'B3')
008900940506     D F16             C                   CONST(X'B4')
009000940506     D F17             C                   CONST(X'B5')
009100940506     D F18             C                   CONST(X'B6')
009200940506     D F19             C                   CONST(X'B7')
009300940506     D F20             C                   CONST(X'B8')
009400940506     D F21             C                   CONST(X'B9')
009500940506     D F22             C                   CONST(X'BA')
009600940506     D F23             C                   CONST(X'BB')
009700940506     D F24             C                   CONST(X'BC')
009800940506     D ENTER           C                   CONST(X'F1')
009900940506     D ROLDWN          C                   CONST(X'F4')
010000940506     D ROLLUP          C                   CONST(X'F5')
010100940127     C*----------------------------------------------------*
010200940127     C*                MAIN LINE PROGRAM
010300940127     C*----------------------------------------------------*
010400940223     C* inizializzazione variabili
010500940223     C                   EXSR      INZVAR
010600940223     C*
010700940223     C     $FINE         DOWEQ     *OFF
010800080915     C     $GEST         CASEQ     'D0'          GESD0
010900940131     C     $GEST         CASEQ     'S1'          GESS1
011000940117     C                   END
011100940117     C                   END
011200940325     C* fine programma
011300940325     C                   SETON                                            LR
011400030113     C************************************************************
011500030113     C* INIZIALIZZAZIONE VARIABILI
011600030113     C************************************************************
011700030113     C     INZVAR        BEGSR
011800030113     C*
011900030113     C* Pulizia campi e indicatori
012000030113     C                   MOVE      *ALL'0'       IN4049           10
012100030113     C                   MOVEA     IN4049        *IN(40)
012200030113     C                   CLEAR                   S1OPZ
012300030113     C* Variabili per gestione videate
012400030113     C                   MOVE      *OFF          $FINE
012500030113     C                   MOVE      *OFF          $INZS1
012600030113     C                   MOVE      *OFF          $EFILE
012700030113     C                   MOVE      *OFF          $ESCI
012800030113     C                   MOVE      *OFF          $RCDOK
012900030113     C*
013000080915     C                   MOVE      *ON           $INZD0
013100080915     C                   MOVE      'D0'          $GEST
013200030113     C*
013300030113     C* Variabili appoggio
013400030114     C                   Z-ADD     1             WPAG
013500030113     c*
013600030113     C                   ENDSR
013700940127     C************************************************************
013800080915     C* Primo Video Introduttivo con Filtro iniziale sulla Filiale
013900940127     C************************************************************
014000080915     C     GESD0         BEGSR
014100030113     C*
014200940223     C* inizializzazione videata
014300080915     C     $INZD0        IFEQ      *ON
014400080915     C                   EXSR      INZD0
014500080915     C                   MOVE      *OFF          $INZD0
014600940127     C                   ENDIF
014700940127     C*
014800030113     C*              *------------------*
014900080915     C                   EXFMT     D0
015000030113     C*              *------------------*
015100030113     C* Selezioni
0152009401271    C                   SELECT
015300940127     C* F3=Fine
015400940506     C     $TASTO        WHENEQ    F03
015500080915     C                   EXSR      F03Fine
015600080915     C* F18=cambio Filiale
015700080915     C     $TASTO        WHENEQ    F18
015800080915     C                   EXSR      F18Filiale
0159009401271O   C                   OTHER
016000940127     C* CONTROLLO DATI
016100080915     C                   EXSR      CTRD0
016200080915      *
016300940201     C     *IN99         IFEQ      *OFF
016400080915     C                   MOVE      'S1'          $GEST
016500080915     C                   MOVE      *ON           $INZS1
016600940131     C                   END
016700080915     C*
0168009401271-   C                   ENDSL
016900080915     C*
017000940127     C                   ENDSR
017100080915     C/EJECT
017200080915     C************************************************************
017300080915     C* INIZIALIZZAZIONE primo video di Filtro
017400080915     C************************************************************
017500080915     C     INZD0         BEGSR
017600080915     C*
017700080915     c                   setoff                                       01
017800080915     C                   MOVEL     o36POU        d0PO
017900080915     C                   clear                   d0dcnv
018000080915      *
018100080915     C                   ENDSR
018200940224     C/EJECT
018300940127     C************************************************************
018400080915     C* Controlla i campi del video di Filtro
018500940127     C************************************************************
018600080915     C     CTRD0         BEGSR
018700080915     C*
018800080915     c                   setoff                                       99
018900080915      *
019000080915      *   Controllo Filiale in Gestione - Filiale Amministrativa
019100080915      *
019200940127     C                   ENDSR
019300080915     C/EJECT
019400080915     C************************************************************
019500080915     C* Cambio di Filiale del video di Filtro
019600080915     C************************************************************
019700080915     C     F18Filiale    BEGSR
019800080915     C*
019900080915     c                   eval      *in01 = *on
020000080915      *
020100080915     C                   ENDSR
020200080915     C************************************************************
020300080915     C* GESTIONE LISTA
020400080915     C************************************************************
020500080915     C     GESS1         BEGSR
020600080915     C*
020700080915     C* inizializzazione videata
020800080915     C     $INZS1        IFEQ      *ON
020900080915     C                   EXSR      INZS1
021000080915     C                   MOVE      *OFF          $INZS1
021100080915     C                   ENDIF
021200080915     C*
021300080915     C* emissione piede videata
021400080915     C                   WRITE     Z1
021500080915     C*
021600080915     C* Non ci sono records
021700080915     C     WMAX          IFEQ      0
021800080915     C                   WRITE     D1
021900080915     C                   Else
022000080915     C     Wsfl          IFgt      0
022100080915     C                   Z-ADD     wsfl          C1RCD
022200080915     C                   Else
022300080915     C     Wpag          IFgt      0
022400080915     C                   Z-ADD     wpag          C1RCD
022500080915     C                   EndIF
022600080915     C                   EndIF
022700080915     C                   ENDIF
022800080915     C*
022900080915     C*              *------------------*
023000080915     C                   EXFMT     C1
023100080915     C*              *------------------*
023200080915     C*
023300080915     C     C1NRR         IFNE      0
023400080915     C                   Z-ADD     C1NRR         WSFL
023500080915     C                   ENDIF
023600080915     C                   Z-ADD     SFLNRR        C1RCD
023700080915     C* Selezioni
0238000809151    C                   SELECT
023900080915     C* F3=Fine
024000080915     C     $TASTO        WHENEQ    F03
024100080915     C                   EXSR      F03Fine
024200080915     C* F12=ritorno a video precedente
024300080915     C     $TASTO        WHENEQ    F12
024400080915     C                   MOVE      'D0'          $GEST
0245000809151O   C                   OTHER
024600080915     C* CONTROLLO DATI
024700080915     C                   EXSR      CTRC1
024800080915     C     *IN99         IFEQ      *OFF
024900080915     C                   EXSR      CTRS1
025000080915     C     $TASTO        ifeq      F06
025100080915     C                   MOVE      'D0'          $GEST
025200080915     c                   end
025300080915     C                   END
0254000809151-   C                   ENDSL
025500080915     C*
025600080915     C*
025700080915     C                   ENDSR
025800080915     C/EJECT
025900080915     C************************************************************
026000080915     C* INIZIALIZZAZIONE LISTA
026100080915     C************************************************************
026200080915     C     INZS1         BEGSR
026300080915     C* pulizia SFL
026400080915     C                   SETOFF                                         3031
026500080915     C                   WRITE     C1
026600080915     C                   SETON                                          31
026700080915     C*
026800080915     C* CARICAMENTO SFL totale
026900080915     C                   Z-ADD     0             S1NRR
027000080915     C                   Z-ADD     1             C1RCD
027100080915     C                   Z-ADD     0             WMAX
027200080915     C*
027300080915     c                   reset                   Set_Padr          7 0
027400080915     c                   movel     d0PO          Set_Padr
027500080915     C*
027600080915     C* Posizionamento su file pilota
027700080915     c     set_padr      setll     FIFGTl1L
027800080915    >C                   EXSR      REDANA
027900080915     C* Carico il SFL
028000080915     C                   EXSR      ROLS1
028100080915     C*
028200080915     C                   Z-ADD     1             WPAG
028300080915     C*
028400080915     C                   ENDSR
028500940127     C************************************************************
028600940131     C* CARICAMENTO PAGINA LISTA
028700940127     C************************************************************
028800940127     C     ROLS1         BEGSR
028900940127     C*
029000940128     C                   SETOFF                                       32
029100940223     C                   Z-ADD     0             Y
029200940127     C                   Z-ADD     WMAX          S1NRR
029300940127     C*
029400940127     C* Leggo dal file anagrafico per caricare la lista
0295009401311    C     $EFILE        DOWEQ     *OFF
029600940127     C*
029700030113     c                   clear                   s1opz
029800080915     c                   z-add     rrnfifgt      h1rrn
029900080915     c                   z-add     FGTLFGS       s1FGS
030000080915     c                   z-add     FGTLPDR       s1PDR
030100080915      *
030200080915     C                   Z-ADD     FGTLWTR       G02INV
030300080915     C                   MOVEL     '3'           G02ERR
030400080915     C                   CALL      'XSRDA8'
030500080915     C                   PARM                    WLBDAT
030600080915     C                   Z-ADD     G02dat        s1WTR
030700080915      *
030800080915     C                   Z-ADD     FGTLDDt       G02INV
030900080915     C                   MOVEL     '3'           G02ERR
031000080915     C                   CALL      'XSRDA8'
031100080915     C                   PARM                    WLBDAT
031200080915     C                   Z-ADD     G02dat        s1DDt
031300080915      *
031400080915     C                   Z-ADD     FGTLDCn       G02INV
031500080915     C                   MOVEL     '3'           G02ERR
031600080915     C                   CALL      'XSRDA8'
031700080915     C                   PARM                    WLBDAT
031800080915     C                   Z-ADD     G02dat        s1DCn
031900080915     c*
032000080915     C                   move      FGTLPDR       kpdr              7 0
032100080915     C     Kapd          chain     fiapd01l
032200080915     c                   if        %Found(fiapd01l)
032300080915     C                   MOVEL     apdrsc        s1pdrd
032400080915     C                   End
032500030113     C*
032600940127     C                   ADD       1             S1NRR
032700940127     C                   ADD       1             Y
032800080915     c     s1Dcn         comp      0                                  0202
032900940127     C*
033000940607     C                   WRITE     S1
033100940131     C*
033200940316    >C                   EXSR      REDANA
033300940128     C*
0334009401271-   C                   ENDDO
033500940127     C*
033600940223     C                   Z-ADD     S1NRR         WMAX                 30
033700940127     C*
033800940127     C* POSIZIONAMENTO AL 1° RCD DELLA PAGINA
033900940127     C*
034000030113     C     S1NRR         DIV       SFLPAG        PAGINE            4 0
034100940127     C                   MVR                     RESTO             3 0
034200030114     C     PAGINE        MULT      SFLPAG        C1RCD
0343000301141    C     RESTO         IFGT      0
034400030114     C                   ADD       1             C1RCD
0345000301141E   C                   ELSE
034600030114     C                   SUB       SFLPAG        C1RCD
034700030114     C                   ADD       1             C1RCD
0348000301141-   C                   ENDIF
034900940128     C*
035000940127     C                   ENDSR
035100940128     C************************************************************
035200940131     C* LETTURA RCD ARCHIVIO PILOTA
035300940128     C************************************************************
035400940607     C     REDANA        BEGSR
035500940128     C*
035600940131     C                   MOVEL     *OFF          $EFILE
035700940131     C                   MOVEL     *OFF          $RCDOK
035800940131     C*
0359009401311    C     $EFILE        DOUEQ     *ON
036000940131     C     $RCDOK        OREQ      *ON
036100940131     C*
036200080915     c                   Read      FIFGTL1L
036300080915     c                   if        %eof(FIFGTL1L)
036400030113     C                   MOVEL     *ON           $EFILE
036500030113     C                   MOVE      $EFILE        *IN33
036600030113     c                   else
036700030113     C                   MOVE      *ON           $RCDOK
036800030113     c                   end
036900940131     C*
037000080915     C*  se non rientra nei dati di filtro imposta OFF - $RCDOK
037100080915     c                   if        d0po <> FGTLFGS
037200080915     C                   MOVE      *Off          $RCDOK
0373000809151-   C                   ENDif
037400080915     C*
037500080915     C* SE RICHIESTa data convalida
037600080915     c                   if        d0DCnv = 'S' and FGTLDCN =0
037700080915     C                   MOVE      *Off          $RCDOK
0378000809151-   C                   ENDif
037900080915     C*
038000080915     c                   if        d0DCnv = 'N' and FGTLDCN >0
038100080915     C                   MOVE      *Off          $RCDOK
0382000809151-   C                   ENDif
038300080915     C*
0384009401311-   C                   ENDDO
038500940131     C*
038600940131     C                   ENDSR
038700940224     C************************************************************
038800940224     C* CALCOLO PAGINA FINO A CUI DEVE ESSERE RICARICATO IL SFL
038900940224     C************************************************************
039000940224     C     CLCPAG        BEGSR
039100940224     C* Input :
039200940224     C* - WSFL = numero dell'ultimo rcd su cui era POSIZIONATO il
039300940224     C*          cursore
039400940224     C* - SFLPAG = numero rcd per pagina sfl
039500940224     C* Output :
039600940224     C* - WPAG = pagina fino a cui deve essere ricaricato il sfl
039700940224     C*
039800940224     C     WSFL          DIV       SFLPAG        PAGINE            4 0
039900940224     C                   MVR                     RESTO             3 0
040000940224     C     RESTO         IFGT      0
040100940224     C                   ADD       1             PAGINE
040200940224     C                   ENDIF
040300940224     C     PAGINE        MULT      SFLPAG        WPAG
040400940224     C*
040500940224     C                   ENDSR
040600940309     C************************************************************
040700940309     C* GESTIONE F03 VIDEO S1
040800940309     C************************************************************
040900080915     C     F03Fine       BEGSR
041000940309     C*
041100940309     C                   MOVE      *ON           $FINE
041200940325     C* fine programma
041300940309     C                   ENDSR
041400940309     C/EJECT
041500940128     C************************************************************
041600940131     C* CONTROLLO TESTATA LISTA
041700940128     C************************************************************
041800940131     C     CTRC1         BEGSR
041900940128     C*
042000940201     C                   MOVE      *OFF          *IN99
042100940131     C*
042200940202     C                   ENDSR
042300940131     C************************************************************
042400940131     C* CONTROLLO OPZIONI LISTA
042500940131     C************************************************************
042600940131     C     CTRS1         BEGSR
042700940131     C*
042800940202     C                   MOVEL     *OFF          $ESCI
042900940201     C                   SETOFF                                       99
043000080915     C                   CLEAR                   S1OPZ
043100940131     C*
043200940127     C* Leggo il sfl solo se ci sono rcd
0433009401311    C     WMAX          IFGT      0
043400940607     C                   READC     S1                                     21
043500940127     C*
043600940131     C* esce se fine sfl o errore che richiede l'uscita
0437009401312    C     *IN21         DOWEQ     *OFF
043800940131     C     $ESCI         ANDEQ     *OFF
043900940201     C                   Z-ADD     S1NRR         C1RCD
044000940131     C* ctrl su riga
044100940131     C                   EXSR      RECS1
044200940131     C* gestione opzioni
0443000809153    C     S1OPZ         IFEq      'C'
044400940201     C     *IN99         ANDEQ     *OFF
044500080915     C     $TASTO        andEQ     F06
044600940131     C                   EXSR      OPZS1
0447009401273-   C                   ENDIF
044800080915      *
044900080915     C* attivo sflnxtchg
045000940131     C                   MOVE      *ON           *IN32
045100940223     C*
045200080915     C     $TASTO        ifeq      F06
045300080915     C                   CLEAR                   S1OPZ
045400080915     c                   end
045500940223     C*
045600940607     C                   UPDATE    S1
045700940223     C*
045800940131     C* leggo prossimo rcd dal sfl se no richiesta uscita ciclo
0459009401313    C     $ESCI         IFEQ      *OFF
046000940607     C                   READC     S1                                     21
046100940201     C* a fine ciclo ripristino il flag richiesta iniz.sfl
0462009402014    C     *IN21         IFEQ      *ON
046300940201     C                   MOVE      WINZS1        $INZS1
046400940204     C* calcolo pagina a cui deve posizionarsi
046500940224     C                   EXSR      CLCPAG
0466009402014-   C                   ENDIF
0467009402013-   C                   ENDIF
046800940131     C*
0469009401272-   C                   ENDDO
047000940127     C*
0471009401311-   C                   ENDIF
047200940131     C*
047300940127     C                   ENDSR
047400940127     C/EJECT
047500940127     C************************************************************
047600940131     C* CONTROLLO CAMPI I/O RIGA LISTA
047700940127     C************************************************************
047800940131     C     RECS1         BEGSR
047900940131     C*
048000940201     C* reset indicatori DSPATR
048100940201     C                   MOVE      *ALL'0'       IN4049           10
048200940201     C                   MOVEA     IN4049        *IN(40)
048300940201     C*
048400940131     C                   ENDSR
048500940131     C************************************************************
048600940131     C* GESTIONE OPZIONI LISTA
048700940131     C************************************************************
048800940131     C     OPZS1         BEGSR
048900940201     C*
049000080915     c                   if        s1DCN = 0
049100080915     c     h1rrn         chain     fiFGTL0F
049200080915     c                   if        %Found(fiFGTL0F)
049300080915     c                   move      *date         FGTLDCN
049400080915     c                   update    fifgt0
049500080915     c                   end
049600080915     c                   endIf
049700940225     C*
049800940131     C                   ENDSR
049900940223     C/EJECT
050000940131     C************************************************************
050100940131     C* OPERAZIONI INIZIALI
050200940131     C************************************************************
050300940131     C     *INZSR        BEGSR
050400030113     C*
050500030113     C* Reperimento parametri
050600030113     C     *ENTRY        PLIST
050700030113     C                   PARM                    KPJBA
050800030113     C*
050900030113     C* Variabili per gestione videate
051000030113     C                   MOVE      *BLANK        $GEST             2
051100030113     C                   MOVE      *BLANK        $FINE             1
051200030113     C                   MOVE      *BLANK        $INZS1            1
051300080915     C                   MOVE      *BLANK        $INZD0            1
051400030113     C                   MOVE      *BLANK        $EFILE            1
051500030113     C                   MOVE      *BLANK        $ESCI             1
051600030113     C                   MOVE      *BLANK        $RCDOK            1
051700030113     C*
051800030113     C* Indici
051900030113     C                   Z-ADD     0             X                 3 0
052000030113     C                   Z-ADD     0             Y                 3 0
052100940506     C*
052200080915     C* Imposta la UDATE  da TIME
052300080915     C                   TIME                    W0140            14 0
052400080915     C                   MOVE      W0140         UDATE8            8 0
052500080915     C*
052600080915      *  In base all'utente prende il P.O.
052700080915     C                   CLEAR                   Tibs36Ds
052800080915     c                   EVAL      I36ute = Knmus
052900080915     c                   EVAL      I36Tla = 'L'
053000080915     C                   CALL      'TIBS36R'
053100080915     C                   PARM                    Tibs36Ds
053200080915     c                   movel     KNMUS         ALFA3             3
053300080915     C*
053400080915     C                   Z-ADD     1             CODUT
053500080915     C                   CALL      'X§PARUT'
053600080915     C                   PARM                    UT§DSE
053700080915     C                   MOVEL     RAGUT         RSUT             20
053800080915     C                   MOVEL     RAGUT         RSUT2            20
053900080915     C                   MOVEL     REC80         CNCR80
054000080915     c*
054100080915     C     Kapd          KLIST
054200080915     C                   KFLD                    TipoAut
054300080915     C                   KFLD                    kpdr              7 0
054400080915     C                   move      'A'           TipoAut           1
054500080915      *
054600940117     C                   ENDSR
054700030113     C************************************************************
