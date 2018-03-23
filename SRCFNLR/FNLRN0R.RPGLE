000100960118     H DECEDIT('0,') DATEDIT(*DMY.)
000200960118     H* FNLRN0R *----------------------------------------------------*
000300960118     H*         RELEASE 11/94                                        *
000400960118     H*                                                              *
000500960118     H*       - TRASMISSIONE NOTE ALLA FILIALE DI PARTENZA      -    *
000600960118     H*--------------------------------------------------------------*
000700060301     Ftita430c  UF a E           K DISK
000800060220     FFiar402L  UF   E           K DISK
000900060220     FFiar401L  UF a E           K DISK    prefix(£)
001000060220     F                                     RENAME(Fiar4000:Fiar401)
001100960126     FFNLBL01L  IF   E           K DISK
001200060228     FFNblp01L  IF   E           K DISK
001300960118     FTABEL00F  IF   E           K DISK    USROPN
001400060223     D*
001500960118     D C7K             S              1    DIM(100)                             Cod.tp.record
001600960126     D T7K             S              1    DIM(100)                             tp.trasmissio
001700060301     D s7K             S              1    DIM(100)                             tp.trasmissio
001800060220     D FIAR4DS       E DS                  EXTNAME(FIAR400F)
001900060220     D FIAR4DS1      E DS                  EXTNAME(FIAR400F) PREFIX(£)
002000060223      *  Ds x definizione tipi record
002100960118     D DS7K          E DS
002200960118     D KPJBA         E DS
002300960118      *  Ds x scomposizione data
002400960118     D WLBDA8          DS
002500960118     D  G02DAT                 1      8  0
002600960118     D  G02INV                 9     16  0
002700960118     D  G02ERR                17     17
002800960118     D  G02TGI                18     22  0
002900060301     itita4000      10
003000060301     itita4p00      11
003100960118     C*-----------------------------------------------------------*
003200960118     C*  Ciclo principale
003300960118     C*-----------------------------------------------------------*
003400060220     c     *loval        setll     fiar402l
003500060220     c                   do        *hival
003600060301     C                   READ(e)   Fiar402L
003700060301     c                   if        %error
003800060301     c                   iter
003900060220     c                   end
004000060301     c                   if        %eof(fiar402l)
004100060301     c                   leave
004200060301     c                   end
004300060220     C* Controllo se devo scive il record alla lnp originale
004400960126     C                   EXSR      CTRREC                                       Ctr record ok
004500060220     C                   IF        wtraok <> 'S'
004600060220     c                   iter
004700060220     c                   end
004800960129     C*  Eseguo Posizionamento sul file dei legami bolla x controlla
004900060220     C*  se devo scrivere giacenza alla filiale originale
005000060220     C                   EXSR      CHNLBL
005100060220     C                   EXSR      GESREC
005200060220     C                   ENDdo                                                  Lnp = Wsvlnp
005300971120     C* SETON LR
005400960118     C                   SETON                                        LR
005500960118     C*-----------------------------------------------------------*
005600960118     C*  Controllo se tipo record rientra fra quelli da trasmett.
005700960118     C*-----------------------------------------------------------*
005800960118     C     CTRREC        BEGSR
005900960126      *  Loop fino a quando non trovo record da trasmett.
006000960126     C                   MOVEL     'N'           WTRAOK            1
006100060220      *  Verifico se tipo rcd. da trasmettere in sede o da scrive lnp orig.
006200960126     C                   Z-ADD     1             W                 3 0
006300960126     C     AR4TRC        LOOKUP    C7K(W)                                 32
006400960126     C     *IN32         IFEQ      '1'
006500960126     C                   MOVEL     'S'           WTRAOK
006600960126     C                   ELSE
006700960216      *  Aggiorno archivio note per non leggere più quelle da trasm.
006800960216     C                   Z-ADD     99999999      AR4DTR
006900960216     C                   EXCEPT    AGGTRA
007000960126     C                   END
007100960126      *
007200960118     C                   ENDSR
007300060220     C*-----------------------------------------------------------------*
007400060220     C*  Posizionamento su FNLBL
007500060220     C*-----------------------------------------------------------------*
007600060220     C     CHNLBL        BEGSR
007700060220     C*
007800060220     C                   CLEAR                   WAAS
007900060220     C                   CLEAR                   WLNP
008000060220     C                   CLEAR                   WNRS
008100060220     C                   CLEAR                   WNSP
008200060220     C* Se devo scrivere il record alla lnp orignale lo cerco
008300060220     C     T7K(W)        IFEQ      'O'
008400060220     C                   Z-ADD     AR4AAS        KAAS
008500060220     C                   Z-ADD     AR4LNP        KLNP
008600060220     C                   Z-ADD     AR4NRS        KNRS
008700060220     C                   Z-ADD     AR4NSP        KNSP
008800060220     C     KLBL          CHAIN     FNLBL01L                           33
008900060220     C*
009000060220     C     *IN33         ifeq      '0'
009100060220     C     LBLLPO        ANDNE     AR4LNP
009200060220     C                   MOVE      LBLAAO        WAAS
009300060220     C                   MOVE      LBLLPO        WLNP
009400060220     C                   MOVE      LBLNRO        WNRS
009500060220     C                   MOVE      LBLNSO        WNSP
009600060220     C                   END
009700060220     C                   END
009800060220     C*
009900060220     C                   ENDSR
010000960118     C*-----------------------------------------------------------*
010100960118     C*  Scrittura record x trasmissione
010200960118     C*-----------------------------------------------------------*
010300960118     C     GESREC        BEGSR
010400060228      * scrivo record fiar400f in lnp originale solo se esiste ancora
010500060228      * la spedizione in partenza
010600060220     c                   if        t7k(w) = 'O' AND WAAS <> 0
010700060228     C     Kblp          CHAIN     Fnblp01L
010800060228     c                   if        %found(fnblp01l)
010900060228     C     KAR4          CHAIN     FIAR401L
011000060220     C                   MOVEL     FIAR4DS       FIAR4DS1
011100060220     C                   Z-ADD     WAAS          £AR4AAS
011200060220     C                   Z-ADD     WLNP          £AR4LNP
011300060220     C                   Z-ADD     WNRS          £AR4NRS
011400060220     C                   Z-ADD     WNSP          £AR4NSP
011500060220     C                   Z-ADD     WOGGI         £AR4DTR
011600060220     C                   IF        %FOUND(FIAR401L)
011700060220     C                   UPDATE    Fiar401
011800060220     C                   ELSE
011900060220     C                   WRITE     Fiar401
012000060220     C                   END
012100060220     c                   end
012200060228     c                   end
012300060301     c* scivo tita4 in sede
012400060301     c                   if        s7k(w) = 'S'
012500060301     c                   setoff                                       1011
012600060301     c                   if        waas <> 0
012700060301     C     Kar4          CHAIN     tita430c
012800060301     C                   Z-ADD     waas          ta4AAS
012900060301     C                   Z-ADD     wlnp          ta4LNP
013000060301     C                   Z-ADD     wnrs          ta4NRS
013100060301     C                   Z-ADD     wnsp          ta4NSP
013200060301     c                   else
013300060301     C     Kta4          CHAIN     tita430c
013400060301     C                   Z-ADD     ar4aas        ta4AAS
013500060301     C                   Z-ADD     ar4lnp        ta4LNP
013600060301     C                   Z-ADD     ar4nrs        ta4NRS
013700060301     C                   Z-ADD     ar4nsp        ta4NSP
013800060301     c                   end
013900060301     C                   movel     ar4trc        ta4trc
014000060301     C                   movel     ar4not        ta4not
014100060301     C                   IF        %FOUND(tita430c)
014200060301     C   10              UPDATE    tita4000
014300060301     C   11              UPDATE    tita4p00
014400060301     C                   ELSE
014500060301     C                   write     tita4000
014600060301     C                   END
014700060301     C                   END
014800960118      *  Aggiorno archivio note da trasmettere
014900960118     C                   Z-ADD     WOGGI         AR4DTR
015000960118     C                   EXCEPT    AGGTRA
015100960118      *
015200960118     C                   ENDSR
015300960118     C*-----------------------------------------------------------*
015400960118     C*  Operazioni iniziali:
015500960118     C*-----------------------------------------------------------*
015600960118     C     *INZSR        BEGSR
015700960118      *
015800960118     C     *ENTRY        PLIST
015900960118     C                   PARM                    KPJBA
016000960118      *  Definizione chiavi
016100960126     C     KLBL          KLIST
016200960126     C                   KFLD                    KAAS
016300960126     C                   KFLD                    KLNP
016400960126     C                   KFLD                    KNRS
016500960126     C                   KFLD                    KNSP
016600060301     C     Kta4          KLIST
016700060301     C                   KFLD                    ar4AAS
016800060301     C                   KFLD                    ar4LNP
016900060301     C                   KFLD                    ar4NRS
017000060301     C                   KFLD                    ar4NSP
017100060220     C                   KFLD                    ar4trc
017200060301     c     Kar4          KLIST
017300060301     C                   KFLD                    WAAS
017400060301     C                   KFLD                    WLNP
017500060301     C                   KFLD                    WNRS
017600060301     C                   KFLD                    WNSP
017700060301     C                   KFLD                    ar4trc
017800060228     C     Kblp          KLIST
017900060228     C                   KFLD                    WAAS
018000060228     C                   KFLD                    WLNP
018100060228     C                   KFLD                    WNRS
018200060228     C                   KFLD                    WNSP
018300960118     C     KTAB1         KLIST
018400960118     C                   KFLD                    KKUT
018500960118     C                   KFLD                    KCOD
018600960118      *  Definizione variabili
018700960126     C     *LIKE         DEFINE    AR4AAS        WAAS
018800960126     C     *LIKE         DEFINE    AR4LNP        WLNP
018900960126     C     *LIKE         DEFINE    AR4NRS        WNRS
019000960126     C     *LIKE         DEFINE    AR4NSP        WNSP
019100960126     C     *LIKE         DEFINE    AR4AAS        KAAS
019200960126     C     *LIKE         DEFINE    AR4LNP        KLNP
019300960126     C     *LIKE         DEFINE    AR4NRS        KNRS
019400960126     C     *LIKE         DEFINE    AR4NSP        KNSP
019500960118     C     *LIKE         DEFINE    TBLKUT        KKUT
019600960118     C     *LIKE         DEFINE    TBLCOD        KCOD
019700960118      *  Data del giorno
019800960118     C                   TIME                    WAMGDT           14 0
019900960118     C                   MOVE      WAMGDT        G02DAT
020000960118     C                   MOVE      *ZEROS        G02INV
020100960118     C                   MOVEL     *BLANK        G02ERR
020200960118     C                   CALL      'XSRDA8'
020300960118     C                   PARM                    WLBDA8
020400960118     C                   Z-ADD     G02INV        WOGGI             8 0
020500960118     C                   TIME                    TIMES             6 0
020600960118      *
020700960118      *  Effettuo caricamento dei tipi record da trasmettere
020800960118     C                   OPEN      TABEL00F
020900060220     C                   Z-ADD     0             X                 3 0
021000960118     C                   Z-ADD     1             KKUT
021100960118     C                   MOVEL     '7K'          KCOD
021200960118     C     KTAB1         CHAIN     TABEL                              30
021300960118     C     *IN30         DOWEQ     '0'
021400960118     C     X             ANDLT     100
021500960118     C     TBLFLG        IFEQ      *BLANKS
021600960118     C                   MOVEL     TBLUNI        DS7K
021700060223     C     §7KTRA        IFeq      'O'
021800960118     C                   ADD       1             X
021900960118     C                   MOVEL     TBLKEY        C7K(X)
022000960126     C                   MOVEL     §7KTRA        T7K(X)
022100060301     C                   MOVEL     §7Ksed        s7K(X)
022200960118     C                   END
022300960118     C                   END
022400960118     C     KTAB1         READE     TABEL                                  30
022500960118     C                   END
022600960118     C*
022700960118     C                   ENDSR
022800960118     O*-----------------------------------------------------*
022900960118     O* Excpt x aggiornamento dati da trasmettere
023000960118     O*-----------------------------------------------------*
023100060223     Ofiar4000  E            AGGTRA
023200960118     O                       AR4DTR
