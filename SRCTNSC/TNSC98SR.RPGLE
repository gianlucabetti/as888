000100940211     H DECEDIT('0,') DATEDIT(*DMY.)
000200940128     F*----------------------------------------------------*
000300140804     Ftncsv01l  if   E           k disk
000400090311      *
000500140804     Ftnsc98SD  CF   E             WORKSTN
000600940607     F                                     SFILE(S1:S1NRR)
000700940201     F                                     INFDS(DSFMT)
000800940128     D*----------------------------------------------------*
000900940211     D* Passaggio Parametri
001000940211     D KPJBA         E DS
001100090408     D KPJBus          s                   like(kpjbu)
001200140804     D tncsvds       E DS                  extname(tncsv01l)
001300140804     D tnsc94ds      E DS
001400940211     D*-------------
001500940211     D DSFMT           DS           512
001600940506     D  $TASTO               369    369
001700940211     D  NRG                  370    370
001800940211     D  NCL                  371    371
001900940211     D  SFLNRR               378    379B 0
002000940207     D*-------------
002100940127     D* Reperimento nome PGM
002200940127     D STATUS         SDS           333
002300090312     D* Nome PGM a video
002400940127     D  DSPGM            *PROC
002500090311     D*-------------
002600090311      *
002700140804     D s1nrr           s                   like(c1nrr)
002800140804     D rrnlast         s                   like(c1nrr)
002900090311     D dataiso         s               d   datfmt(*iso)
003000090311     D dataeur         s               d   datfmt(*eur)
003100090311      *
003200940207     D*-------------
003300940211     D* COSTANTI
003400940211     D*-------------
003500090326     D SFLPAG          C                   CONST(11)
003600940314     D* dimensione della schiera $MS1
003700940506     D*
003800940506     D* Tasti di funzione
003900940506     D F01             C                   CONST(X'31')
004000940506     D F02             C                   CONST(X'32')
004100940506     D F03             C                   CONST(X'33')
004200940506     D F04             C                   CONST(X'34')
004300940506     D F05             C                   CONST(X'35')
004400940506     D F06             C                   CONST(X'36')
004500940506     D F07             C                   CONST(X'37')
004600940506     D F08             C                   CONST(X'38')
004700940506     D F09             C                   CONST(X'39')
004800940506     D F10             C                   CONST(X'3A')
004900940506     D F11             C                   CONST(X'3B')
005000940506     D F12             C                   CONST(X'3C')
005100940506     D F13             C                   CONST(X'B1')
005200940506     D F14             C                   CONST(X'B2')
005300940506     D F15             C                   CONST(X'B3')
005400940506     D F16             C                   CONST(X'B4')
005500940506     D F17             C                   CONST(X'B5')
005600940506     D F18             C                   CONST(X'B6')
005700940506     D F19             C                   CONST(X'B7')
005800940506     D F20             C                   CONST(X'B8')
005900940506     D F21             C                   CONST(X'B9')
006000940506     D F22             C                   CONST(X'BA')
006100940506     D F23             C                   CONST(X'BB')
006200940506     D F24             C                   CONST(X'BC')
006300940506     D ENTER           C                   CONST(X'F1')
006400940506     D ROLDWN          C                   CONST(X'F4')
006500940506     D ROLLUP          C                   CONST(X'F5')
006600940127     C*----------------------------------------------------*
006700940223     C*
006800140804     C                   exsr      GESS1
006900940325     C* fine programma
007000940325     C                   SETON                                            LR
007100940127     C************************************************************
007200940131     C* GESTIONE LISTA
007300940127     C************************************************************
007400940127     C     GESS1         BEGSR
007500030113     C*
007600940127     C                   EXSR      INZS1
007700030113     C*
007800030113     C* emissione piede videata
007900140807     c     emetti        tag
008000140804     c                   seton                                        30
008100030113     C                   WRITE     Z1
008200030113     C*              *------------------*
008300940607     C                   EXFMT     C1
008400030113     C*              *------------------*
008500030113     C*
008600090313      *
008700940127     C                   Z-ADD     SFLNRR        C1RCD
008800030113     C* Selezioni
0089009401271    C                   SELECT
009000940127     C* F3=Fine
009100140804     C     $TASTO        WHENEQ    F12
009200140807     c                   clear                   tncsvds
009300140804     C                   EXSR      F12S1
009400140804     c                   other
009500140804     c                   exsr      ctrs1
009600140807     c                   goto      emetti
0097009401271-   C                   ENDSL
009800940127     C*
009900940127     C                   ENDSR
010000940224     C/EJECT
010100940127     C************************************************************
010200940131     C* INIZIALIZZAZIONE LISTA
010300940127     C************************************************************
010400090312     C     INZC1         BEGSR
010500940302     C* pulizia SFL
010600090312     C                   SETOFF                                         3031
010700090312     C                   WRITE     C1
010800090312     C                   SETON                                          31
010900090312     C*
011000090312     C* CARICAMENTO SFL totale
011100090312     C                   Z-ADD     1             C1RCD
011200090312     c                   movel     DSPGM         c1pgm
011300090312     c                   movel     knsif         c1sif
011400090312     C*
011500090312     C                   ENDSR
011600090312     C/EJECT
011700090312     C************************************************************
011800090312     C* INIZIALIZZAZIONE LISTA
011900090312     C************************************************************
012000090312     C     INZS1         BEGSR
012100090312     C*
012200090312     C* pulizia SFL
012300090312    >C                   EXSR      INZC1
012400090312     C*
012500090312     C                   Z-ADD     0             S1NRR
012600940224     C*
012700030113     C* Carico il SFL
012800940127     C                   EXSR      ROLS1
012900940127     C*
013000940127     C                   ENDSR
013100940127     C************************************************************
013200940131     C* CARICAMENTO PAGINA LISTA
013300940127     C************************************************************
013400940127     C     ROLS1         BEGSR
013500940127     C*
013600940128     C                   SETOFF                                       32
013700140804     c     kcsv          setll     tncsv01l
0138001408041    C                   DO        *hival
013900140804     c     kcsv          Reade     tncsv01l
014000140804     C*
014100140804     c                   if        %eof(tncsv01l)
014200140804     c                   leave
014300140804     c                   endif
014400140804      *  Esclude gli annullati
014500140804     c                   if         csvcav <>'ERIN'
014600140804     C                   iter
014700140804     c                   end
014800940127     C*
014900030113     c                   clear                   s1opz
015000090316     C*
015100140804     c                   eval      s1pov   = csvfev
015200140804     c                   eval      s1cau   = csvcav
015300140804     c                   eval      s1ora   = csvorv
015400140804     c                   eval      s1div   = csvvca
015500140804     c                   eval      s1imp   = csvcas
015600090311     c*
015700140804     c                   z-add     0             s1data
015800140804     c                   if        csvdtv   > 0
015900140804     c                   move      csvdtv        s1datah
016000140804     c                   move      csvdtv        dataiso
016100140804     c                   move      dataiso       dataeur
016200140804     c                   move      dataeur       S1Data
016300090311     c                   end
016400940127     C*
016500940127     C                   ADD       1             S1NRR
016600090311     C                   EVAL      RrnLast = S1nrr
016700940127     C*
016800940607     C                   WRITE     S1
016900090313     C* esce x il max. records consentiti
017000090313     c                   if        s1nrr = 9999
017100090313     c                   leave
017200090313     c                   end
0173009401271-   C                   ENDDO
017400140804     c                   if        s1nrr > 1
017500140804     c                   z-add     1             c1nrr
017600140804     c                   else
017700140804     c                   exsr      opzs1
017800140804     c                   end
017900090311     C*
018000940127     C                   ENDSR
018100090311     C************************************************************
018200090311     C* GESTIONE F03 VIDEO S1
018300090311     C************************************************************
018400140804     C     F12S1         BEGSR
018500090311     C*
018600140804     c                   return
018700940325     C* fine programma
018800940309     C                   ENDSR
018900940309     C/EJECT
019000090316     C************************************************************
019100090316     C* CONTROLLO OPZIONI LISTA
019200090316     C************************************************************
019300090316     C     CTRS1         BEGSR
019400090316     C*
019500940201     C                   SETOFF                                       99
019600090311     C                   clear                   S1OPZ
019700940131     C*
019800940607     C                   READC     S1                                     21
019900940131     C* gestione opzioni
0200000903113    C     S1OPZ         IFNE      *blank
020100940201     C     *IN99         ANDEQ     *OFF
020200940131     C                   EXSR      OPZS1
0203009401273-   C                   ENDIF
020400940131     C*
020500940127     C                   ENDSR
020600940127     C/EJECT
020700940131     C************************************************************
020800940131     C* GESTIONE OPZIONI LISTA
020900940131     C************************************************************
021000940131     C     OPZS1         BEGSR
021100940201     C*
021200090826     C*  Solo interrogare o
021300140804     C                   RESET                   tncsvds
021400140804     C     kcsv1         chain     tncsv01l
021500140804     c                   if        %found(tncsv01l)
021600140804     c                   return
021700140804     c                   endif
021800940225     C*
021900940131     C                   ENDSR
022000940131     C************************************************************
022100940131     C* OPERAZIONI INIZIALI
022200940131     C************************************************************
022300940131     C     *INZSR        BEGSR
022400030113     C*
022500030113     C* Reperimento parametri
022600030113     C     *ENTRY        PLIST
022700140804     C                   PARM                    tnsc94ds
022800140804     C                   PARM                    tncsvds
022900090311     C*  Impostazioni di inizio programma
023000140804     c     kcsv          klist
023100140804     c                   kfld                    d94AAS
023200140804     c                   kfld                    d94LNP
023300140804     c                   kfld                    d94NRS
023400140804     c                   kfld                    d94NSP
023500940127     C*
023600140804     c     kcsv1         klist
023700140804     c                   kfld                    d94AAS
023800140804     c                   kfld                    d94LNP
023900140804     c                   kfld                    d94NRS
024000140804     c                   kfld                    d94NSP
024100140804     c                   kfld                    s1datah
024200140804     c                   kfld                    s1ora
024300940117     C                   ENDSR
