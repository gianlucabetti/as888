000100960118     H DECEDIT('0,') DATEDIT(*DMY.)
000200960118     H* FNLRN0R *----------------------------------------------------*
000300960118     H*         RELEASE 11/94                                        *
000400960118     H*                                                              *
000500960118     H*       - TRASMISSIONE NOTE ALLA FILIALE DI PARTENZA      -    *
000600960118     H*--------------------------------------------------------------*
000700060220     FFiar402L  UF   E           K DISK
000800060220     FFiar401L  UF a E           K DISK    prefix(£)
000900060220     F                                     RENAME(Fiar4000:Fiar401)
001000960126     FFNLBL01L  IF   E           K DISK
001100960118     FTABEL00F  IF   E           K DISK    USROPN
001200060223     D*
001300960118     D C7K             S              1    DIM(100)                             Cod.tp.record
001400960126     D T7K             S              1    DIM(100)                             tp.trasmissio
001500060220     D FIAR4DS       E DS                  EXTNAME(FIAR400F)
001600060220     D FIAR4DS1      E DS                  EXTNAME(FIAR400F) PREFIX(£)
001700060223      *  Ds x definizione tipi record
001800960118     D DS7K          E DS
001900960118     D KPJBA         E DS
002000960118      *  Ds x scomposizione data
002100960118     D WLBDA8          DS
002200960118     D  G02DAT                 1      8  0
002300960118     D  G02INV                 9     16  0
002400960118     D  G02ERR                17     17
002500960118     D  G02TGI                18     22  0
002600960118     C*-----------------------------------------------------------*
002700960118     C*  Ciclo principale
002800960118     C*-----------------------------------------------------------*
002900060220     c     *loval        setll     fiar402l
003000060220     c                   do        *hival
003100060220     C                   READ      Fiar402L
003200060220     c                   if        %eof(fiar402l)
003300060220     c                   leave
003400060220     c                   end
003500060220     C* Controllo se devo scive il record alla lnp originale
003600960126     C                   EXSR      CTRREC                                       Ctr record ok
003700060220     C                   IF        wtraok <> 'S'
003800060220     c                   iter
003900060220     c                   end
004000960129     C*  Eseguo Posizionamento sul file dei legami bolla x controlla
004100060220     C*  se devo scrivere giacenza alla filiale originale
004200060220     C                   EXSR      CHNLBL
004300060220     C                   EXSR      GESREC
004400060220     C                   ENDdo                                                  Lnp = Wsvlnp
004500971120     C* SETON LR
004600960118     C                   SETON                                        LR
004700960118     C*-----------------------------------------------------------*
004800960118     C*  Controllo se tipo record rientra fra quelli da trasmett.
004900960118     C*-----------------------------------------------------------*
005000960118     C     CTRREC        BEGSR
005100960126      *  Loop fino a quando non trovo record da trasmett.
005200960126     C                   MOVEL     'N'           WTRAOK            1
005300060220      *  Verifico se tipo rcd. da trasmettere in sede o da scrive lnp orig.
005400960126     C                   Z-ADD     1             W                 3 0
005500960126     C     AR4TRC        LOOKUP    C7K(W)                                 32
005600960126     C     *IN32         IFEQ      '1'
005700960126     C                   MOVEL     'S'           WTRAOK
005800960126     C                   ELSE
005900960216      *  Aggiorno archivio note per non leggere più quelle da trasm.
006000960216     C                   Z-ADD     99999999      AR4DTR
006100960216     C                   EXCEPT    AGGTRA
006200960126     C                   END
006300960126      *
006400960118     C                   ENDSR
006500060220     C*-----------------------------------------------------------------*
006600060220     C*  Posizionamento su FNLBL
006700060220     C*-----------------------------------------------------------------*
006800060220     C     CHNLBL        BEGSR
006900060220     C*
007000060220     C                   CLEAR                   WAAS
007100060220     C                   CLEAR                   WLNP
007200060220     C                   CLEAR                   WNRS
007300060220     C                   CLEAR                   WNSP
007400060220     C* Se devo scrivere il record alla lnp orignale lo cerco
007500060220     C     T7K(W)        IFEQ      'O'
007600060220     C                   Z-ADD     AR4AAS        KAAS
007700060220     C                   Z-ADD     AR4LNP        KLNP
007800060220     C                   Z-ADD     AR4NRS        KNRS
007900060220     C                   Z-ADD     AR4NSP        KNSP
008000060220     C     KLBL          CHAIN     FNLBL01L                           33
008100060220     C*
008200060220     C     *IN33         ifeq      '0'
008300060220     C     LBLLPO        ANDNE     AR4LNP
008400060220     C                   MOVE      LBLAAO        WAAS
008500060220     C                   MOVE      LBLLPO        WLNP
008600060220     C                   MOVE      LBLNRO        WNRS
008700060220     C                   MOVE      LBLNSO        WNSP
008800060220     C                   END
008900060220     C                   END
009000060220     C*
009100060220     C                   ENDSR
009200960118     C*-----------------------------------------------------------*
009300960118     C*  Scrittura record x trasmissione
009400960118     C*-----------------------------------------------------------*
009500960118     C     GESREC        BEGSR
009600060220      * scrivo record fiar400f in lnp originale
009700060220     c                   if        t7k(w) = 'O' AND WAAS <> 0
009800060220     C     KAR4          CHAIN     FIAR401L
009900060220     C                   MOVEL     FIAR4DS       FIAR4DS1
010000060220     C                   Z-ADD     WAAS          £AR4AAS
010100060220     C                   Z-ADD     WLNP          £AR4LNP
010200060220     C                   Z-ADD     WNRS          £AR4NRS
010300060220     C                   Z-ADD     WNSP          £AR4NSP
010400060220     C                   Z-ADD     WOGGI         £AR4DTR
010500060220     C                   IF        %FOUND(FIAR401L)
010600060220     C                   UPDATE    Fiar401
010700060220     C                   ELSE
010800060220     C                   WRITE     Fiar401
010900060220     C                   END
011000060220     c                   end
011100960118      *  Aggiorno archivio note da trasmettere
011200960118     C                   Z-ADD     WOGGI         AR4DTR
011300960118     C                   EXCEPT    AGGTRA
011400960118      *
011500960118     C                   ENDSR
011600960118     C*-----------------------------------------------------------*
011700960118     C*  Operazioni iniziali:
011800960118     C*-----------------------------------------------------------*
011900960118     C     *INZSR        BEGSR
012000960118      *
012100960118     C     *ENTRY        PLIST
012200960118     C                   PARM                    KPJBA
012300960118      *  Definizione chiavi
012400960126     C     KLBL          KLIST
012500960126     C                   KFLD                    KAAS
012600960126     C                   KFLD                    KLNP
012700960126     C                   KFLD                    KNRS
012800960126     C                   KFLD                    KNSP
012900060220     C     Kar4          KLIST
013000060220     C                   KFLD                    WAAS
013100060220     C                   KFLD                    WLNP
013200060220     C                   KFLD                    WNRS
013300060220     C                   KFLD                    WNSP
013400060220     C                   KFLD                    ar4trc
013500960118     C     KTAB1         KLIST
013600960118     C                   KFLD                    KKUT
013700960118     C                   KFLD                    KCOD
013800960118      *  Definizione variabili
013900960126     C     *LIKE         DEFINE    AR4AAS        WAAS
014000960126     C     *LIKE         DEFINE    AR4LNP        WLNP
014100960126     C     *LIKE         DEFINE    AR4NRS        WNRS
014200960126     C     *LIKE         DEFINE    AR4NSP        WNSP
014300960126     C     *LIKE         DEFINE    AR4AAS        KAAS
014400960126     C     *LIKE         DEFINE    AR4LNP        KLNP
014500960126     C     *LIKE         DEFINE    AR4NRS        KNRS
014600960126     C     *LIKE         DEFINE    AR4NSP        KNSP
014700960118     C     *LIKE         DEFINE    TBLKUT        KKUT
014800960118     C     *LIKE         DEFINE    TBLCOD        KCOD
014900960118      *  Data del giorno
015000960118     C                   TIME                    WAMGDT           14 0
015100960118     C                   MOVE      WAMGDT        G02DAT
015200960118     C                   MOVE      *ZEROS        G02INV
015300960118     C                   MOVEL     *BLANK        G02ERR
015400960118     C                   CALL      'XSRDA8'
015500960118     C                   PARM                    WLBDA8
015600960118     C                   Z-ADD     G02INV        WOGGI             8 0
015700960118     C                   TIME                    TIMES             6 0
015800960118      *
015900960118      *  Effettuo caricamento dei tipi record da trasmettere
016000960118     C                   OPEN      TABEL00F
016100060220     C                   Z-ADD     0             X                 3 0
016200960118     C                   Z-ADD     1             KKUT
016300960118     C                   MOVEL     '7K'          KCOD
016400960118     C     KTAB1         CHAIN     TABEL                              30
016500960118     C     *IN30         DOWEQ     '0'
016600960118     C     X             ANDLT     100
016700960118     C     TBLFLG        IFEQ      *BLANKS
016800960118     C                   MOVEL     TBLUNI        DS7K
016900060223     C     §7KTRA        IFeq      'O'
017000960118     C                   ADD       1             X
017100960118     C                   MOVEL     TBLKEY        C7K(X)
017200960126     C                   MOVEL     §7KTRA        T7K(X)
017300960118     C                   END
017400960118     C                   END
017500960118     C     KTAB1         READE     TABEL                                  30
017600960118     C                   END
017700960118     C*
017800960118     C                   ENDSR
017900960118     O*-----------------------------------------------------*
018000960118     O* Excpt x aggiornamento dati da trasmettere
018100960118     O*-----------------------------------------------------*
018200060223     Ofiar4000  E            AGGTRA
018300960118     O                       AR4DTR
