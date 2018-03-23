000100011211      *---------*
000200011211      * FNLS12R *-----------------------------------------------------*
000300011211      *---------*                                                     *
000400011211      *           ELENCO INVERSIONI DISK C SU VDL         - FILTRO -  *
000500011211      *---------------------------------------------------------------*
000600011211
000700011211     H DECEDIT('0,') DATEDIT(*DMY.)
000800011211
000900011211      *---------------------------------------------------------------*
001000011211
001100020624     FAZORG01L  IF   E           K DISK
001200020624     F*
001300011211     FFNLS12D   CF   E             WORKSTN
001400011211
001500011211      *---------------------------------------------------------------*
001600011211
001700011211      *   S C H I E R E
001800011211
001900011211     D MSG             S             78    DIM(2) CTDATA PERRCD(1)
002000011211
002100011211      *   D S   E S T E R N E / I N T E R N E
002200011211
002300011211     D KPJBA         E DS
002400011211      *
002500011211      * PARAMETRI PER ELABORAZIONE BATCH         - FNLS13R -
002600011211     D FNLS13DS        DS                  INZ
002700011211     D  D13DDA                 1      8  0
002800011211     D  D13DAL                 9     16  0
002900020624     D  D13POG                17     19  0
003000011211      *
003100011211     D UT§DSE0F      E DS
003200020624     D CNCR80        E DS
003300020624      *
003400020624     D FNLV50DS      E DS
003500011211      *
003600011211      * PARAMETRI PER CONTROLLO ED INVERSIONE DATA
003700011211     D WLBDAT          DS
003800011211     D  G02DAT                 1      8  0
003900011211     D  G02INV                 9     16  0
004000011211     D  G02ERR                17     17
004100011211     D  G02TGI                18     22  0
004200011211
004300011211      *   V A R I A B I L I
004400011211
004500011211     D WDDA            S              8  0 INZ(*loval)
004600011211     D WDAL            S              8  0 INZ(*hival)
004700011211
004800011211      *---------------------------------------------------------------*
004900011211
005000011211      *****************************************************************
005100011211      * RIEPILOGO INDICATORI
005200011211      *****************************************************************
005300020624      * 05    - SPROTEGGO P.O. IN GESTIONE
005400020624      * 06    - NON ABILITO TASTO FUNZIONALE F18=Cambio P.O.
005500011211      * 28    - INDICATORE DI EMISSIONE MESSAGGIO DI ERRORE
005600011211      * 41/42 - ERRORI
005700011211      *****************************************************************
005800011211      *
005900011211     C     *ENTRY        plist
006000011211     C                   parm                    KPJBA
006100020624      *
006200020624      * Imposto il "P.O. IN GESTIONE"
006300020624     c                   move      SIMpou        V1Cpog
006400020624if  1c                   IF        SIMtpp = '2' or SIMtpp = *blanks
006500020624     c                   eval      *in06  = *on
006600020624e   1c                   ENDIF
006700020624     c                   exsr      CTRFGS
006800011211      *
006900011211      * Pulizia dei campi videata
007000011211     C                   clear                   V1Cdda
007100011211     C                   clear                   V1Cdal
007200011211      *
007300011211     C     EmiD01        tag
007400011211      *
007500011211     C                   exfmt     LS12D01
007600011211      *
007700011211      * Pulizia campo messaggio e indicatori di errore (*IN28, 41, 42)
007800011211     C                   clear                   V1Cmsg
007900011211     C                   setoff                                       28
008000011211     C                   setoff                                       4142
008100011211      *
008200011211      * F3=Fine lavoro
008300011211     C   KC              goto      FINE
008400020624      *
008500020624      * F18=Cambio P.O. in gestione
008600020624     c   KS              eval      *in05 = *on
008700020624     c   KS              goto      EmiD01
008800020624      *
008900020624      * 05 on --> controllo P.O. in gestione
009000020624     c                   if        *in05
009100020624     c                   clear                   V1Dpog
009200020624     c                   exsr      CTRFGS
009300020624     c   90              goto      EmiD01
009400020624     c                   eval      *in05 = *off
009500020624     c                   endif
009600011211      *
009700011211      * Controllo dati del formato D01
009800011211     C                   exsr      CtrD01
009900011211     C  NKF
010000011211     Cor 28              goto      EmiD01
010100011211      *
010200011211      * F6=Elaborazione
010300011211     C                   z-add     wDDA          D13dda
010400011211     C                   z-add     wDAL          D13dal
010500020624     C                   move      V1Cpog        D13pog
010600011211      *
010700020102     C                   movel(p)  FNLS13DS      KPJBU
010800011211     C                   movel     'LS13'        KCOAZ
010900081030     c                   if        KNMUS = *all'1'
011000081013     c                   call      'FNLS13R'
011100081013     c                   parm                    KPJBA
011200081030     c                   else
011300081013     c                   call      'BCH10'
011400081013     c                   parm                    KPJBA
011500081030     c                   endif
011600011211      *
011700011211     C     FINE          tag
011800011211     C                   movel     *on           *inLR
011900020624
012000020624      **********************************************************************
012100020624      * CONTROLLO IL P.O. IN IN GESTIONE
012200020624      **********************************************************************
012300020624     C     CTRFGS        BEGSR
012400020624      *
012500020624     C                   setoff                                       9028
012600020624     C                   clear                   FNLV50DS
012700020624     C                   movel     KNMUS         D50PRU
012800020624     C                   move      V1Cpog        D50FGS
012900020624     C                   call      'FNLV50R'
013000020624     C                   parm                    FNLV50DS
013100020624      *
013200020624     c                   clear                   V1Dpog
013300020624if  1C                   IF        D50ERR <> *blanks
013400020624      *
013500020624     C                   seton                                        9028
013600020624     c                   movel     D50MSG        V1CMSG
013700020624      *
013800020624x   1C                   else
013900020624      *
014000020624      * decodifica
014100020624     C     V1Cpog        chain     AZORG
014200020624if  2C                   IF        %found(AZORG01L)
014300020624     C                   movel     orgDES        V1Dpog
014400020624e   2C                   ENDIF
014500020624      *
014600020624e   1C                   ENDIF
014700020624      *
014800020624     C                   ENDSR
014900011211
015000011211      *---------------------------------------------------------------*
015100011211      * CTRD01 * CONTROLLI FORMATO D01                                *
015200011211      *---------------------------------------------------------------*
015300011211     C     CTRD01        BEGSR
015400011211      *
015500011211     C                   setoff                                       28
015600011211      *
015700011211      ****  DATA DAL  ****
015800011211     C                   clear                   WLBDAT
015900011211     C                   z-add     V1Cdda        G02dat
016000011211     C                   call      'XSRDA8'
016100011211     C                   parm                    WLBDAT
016200011211    2C                   if        G02err = *on
016300011211     C                   movel     MSG(1)        V1Cmsg
016400011211     C                   seton                                        4128
016500011211     C                   goto      ENDCTR
016600011211    2C                   endif
016700011211     C                   z-add     G02dat        V1Cdda
016800011211     C                   z-add     G02inv        wDDA
016900011211      *
017000011211      ****  DATA  AL  ****
017100011211    1C                   IF        V1Cdal > 0
017200011211     C                   clear                   WLBDAT
017300011211     C                   z-add     V1CDAL        G02dat
017400011211     C                   call      'XSRDA8'
017500011211     C                   parm                    WLBDAT
017600011211    2C                   if        G02err = *on
017700011211     C                   movel     MSG(1)        V1Cmsg
017800011211     C                   seton                                        4228
017900011211     C                   goto      ENDCTR
018000011211    2C                   endif
018100011211     C                   z-add     G02dat        V1Cdal
018200011211     C                   z-add     G02inv        wDAL
018300020624   x1C                   ELSE
018400011211     C                   z-add     V1Cdda        V1Cdal
018500011211     C                   z-add     wDDA          wDAL
018600011211    1C                   ENDIF
018700011211      *
018800011211      * DATA DAL MAGGIORE DI DATA AL
018900011211    1C                   if        wDDA > wDAL
019000011211     C                   movel     MSG(2)        V1Cmsg
019100011211     C                   seton                                        4128
019200011211     C                   goto      ENDCTR
019300011211    1C                   endif
019400011211      *
019500011211     C     ENDCTR        ENDSR
019600011211
019700011211      *---------------------------------------------------------------*
019800011211      * *INZSR * Operazioni Iniziali                                  *
019900011211      *---------------------------------------------------------------*
020000011211     C     *INZSR        BEGSR
020100011211      *
020200011211     C                   Z-ADD     1             CODUT
020300011211     C                   CALL      'X§PARUT'
020400011211     C                   PARM                    UT§DSE0F
020500011211     C                   MOVEL     RAGUT         V1CRSU
020600020624     C                   MOVEL     REC80         CNCR80
020700011211      *
020800011211     C                   ENDSR
020900011211
021000011211      *---------------------------------------------------------------*
021100011211
021200011211**  MSG
021300011211Data errata                                                                   01
021400011211La data DAL non puo' essere maggiore della data AL                            02
