000100000118     H DECEDIT('0,') DATEDIT(*DMY.)
000200000118?     *--------------------------------------------------------------*
000300000118      * CNC0W7R                                                      *
000400000118      *                                                              *
000500000118      *        PULIZIA MANDATI D'INTROITO VERSATI E PRESI IN CARICO  *
000600021127      *        PULIZIA TESTATE VERSAMENTI BANCARI                    *
000700021127      *        PULIZIA QUADRATURA INCASSI GIORNALIERI                *
000701061120      *        PULIZIA MANDATI CASSAFORTE                            *
000800000118      *                                                              *
000900000118?     *--------------------------------------------------------------*
001000060216     Fyfmdi01L  UF   E           K DISK
001100021127     FFNVRT01L  UF   E           K DISK
001200060216     FyfQIG01L  UF   E           K DISK
001201061120     FTNCAS00f  UF   E             DISK
001300000118     F*---------
001400000118     FTABEL00F  IF   E           K DISK    USROPN
001500000118?     *--------------------------------------------------------------*
001600000118      * DS
001700000118?     *--------------------------------------------------------------*
001800000118     D KPJBA         E DS
001900000118     D* DS pulizia archivio arrivi
002000000118     D DS5A2         E DS
002100000118     D* DS calcolo date
002200000118     D WLBDAT          DS
002300000118     D  G02DAT                 1      8  0
002400000118     D  G02INV                 9     16  0
002500000118     D  G02ERR                17     17
002600000118     D  G02TGI                18     22  0
002700000118     D WGIDA8          DS
002800000118     D  GI8DAT                 1      8  0
002900000118     D  GI8INV                 9     16  0
003000000118     D  GI8TGI                17     21  0
003100000118?     *--------------------------------------------------------------*
003200000118      * Ciclo Principale
003300000118?     *--------------------------------------------------------------*
003400021127     C                   EXSR      PULARM
003500021127     C                   EXSR      PULVRT
003600021127     C                   EXSR      PULQIG
003601061120     C                   EXSR      PULCAS
003700021127      * Fine
003800021127     C     FINE          TAG
003900021127     C                   SETON                                        LR
004000021127?     *--------------------------------------------------------------*
004100021127     C* PULIZIA MANDATI D'INTROITO
004200021127     C     PULARM        BEGSR
004300021127?     *--------------------------------------------------------------*
004400000118      * Eseguo posizionamento ad inizio file Mandati d'intoito
004500060216     C     *LOVAL        SETLL     yfmdi01L
004600060216     C                   READ      yfmdi01L                               21
004700000118      * loop fino a che la data del mandato d'introto non è superiore
004800000118      * alla data limite pulizia oppure è finito il file
004900000118     C     *IN21         DOWEQ     '0'
005000060216     C     mdiDMI        ifle      DATPUL
005100060216      * cancello solo se è già stato versato
005200060216     C     mdivie        andeq     'S'
005300060216     C                   DELETE    yfmdi000
005400000118     C                   END
005500000118      * Leggo il record successivo
005600060216     C                   READ      yfmdi01L                               21
005700000118     C                   END
005800021127     C                   ENDSR
005900021127?     *--------------------------------------------------------------*
006000021127     C* PULIZIA TESTATE VERSAMENTI BANCARI
006100021127     C     PULVRT        BEGSR
006200021127?     *--------------------------------------------------------------*
006300021127     C     *LOVAL        SETLL     FNVRT01L
006400021127     c                   do        *hival
006500021127     C                   READ      FNVRT01L
006600021127     C                   if        %eof(fnvrt01l)
006700021127     c                   leave
006800021127     c                   end
006900021127     C     VRTDDV        ifle      DATPUL
007000021127     C                   DELETE    FNVRT000
007100021127     C                   END
007200021127     C                   ENDdo
007300021127     C                   ENDSR
007400021127?     *--------------------------------------------------------------*
007500021127     C* PULIZIA QUDRATURA INCASSI GIORNALIERI
007600021127     C     PULQIG        BEGSR
007700021127?     *--------------------------------------------------------------*
007800060216     C     *LOVAL        SETLL     yfqig01L
007900021127     c                   do        *hival
008000060216     C                   READ      yfqig01L
008100060216     C                   if        %eof(yfqig01l)
008200021127     c                   leave
008300021127     c                   end
008400021127     C     QIGDTA        ifle      DATPUL
008500060216     C                   DELETE    yfQIG000
008600021127     c                   end
008700021127     c                   enddo
008800021127     C                   ENDSR
008801061120?     *--------------------------------------------------------------*
008802061120     C* PULIZIA MANDATI CASSAFORTE
008803061120     C     PULCAS        BEGSR
008804061120?     *--------------------------------------------------------------*
008806061120     c                   do        *hival
008807061120     C                   READ      tncas00f
008808061120     C                   if        %eof(tncas00f)
008809061120     c                   leave
008810061120     c                   end
008811061120     C     casda8        ifle      DATPUL
008812061120     C                   DELETE    tncas000
008813061120     c                   end
008814061120     c                   enddo
008815061120     C                   ENDSR
008900000118?     *--------------------------------------------------------------*
009000000118      * Operazioni iniziali
009100000118?     *--------------------------------------------------------------*
009200000118     C     *INZSR        BEGSR
009300000118      *
009400000118     C     *ENTRY        PLIST
009500000118     C                   PARM                    KPJBA
009600000118      * Apertura file
009700000118     C                   OPEN      TABEL00F
009800000118      * Calcolo la data fino a cui pulire
009900000118     C     KTAB          KLIST
010000000118     C                   KFLD                    TBWKUT
010100000118     C                   KFLD                    TBWCOD
010200000118     C                   KFLD                    TBWKEY
010300000118     C* Definizione varibili
010400000118     C     *LIKE         DEFINE    TBLKUT        TBWKUT
010500000118     C     *LIKE         DEFINE    TBLCOD        TBWCOD
010600000118     C     *LIKE         DEFINE    TBLKEY        TBWKEY
010700000118     C*----------------------------------------------------*
010800000118     C                   Z-ADD     1             TBWKUT
010900000118     C                   MOVE      '5A'          TBWCOD
011000000118     C                   MOVEL     *BLANKS       TBWKEY
011100000118     C                   MOVEL     '2'           TBWKEY
011200000118     C                   CLEAR                   DS5A2
011300000118     C*----------------------------------------------------*
011400000118     C     KTAB          CHAIN     TABEL                              10
011500000118     C     *IN10         IFEQ      '0'
011600000118     C                   MOVEL     TBLUNI        DS5A2
011700000118     C                   ELSE
011800000118     C                   GOTO      FINE
011900000118     C                   END
012000000118      * Close del file
012100000118     C                   CLOSE     TABEL00F
012200000118      *
012300000118      * Reperisco la data  del giorno e la giro nel formato corretto
012400000118     C                   TIME                    WHHDAT           14 0
012500000118     C                   MOVE      WHHDAT        G02DAT
012600000118     C                   MOVE      *ZEROS        G02INV
012700000118     C                   MOVEL     *BLANKS       G02ERR
012800000118     C                   CALL      'XSRDA8'
012900000118     C                   PARM                    WLBDAT
013000000118     C* Sottraggo alla data del giorno i gg di pulizia
013100000118     C     G02TGI        SUB       §5ANMI        GI8TGI
013200000118     C                   MOVE      *ZEROS        GI8DAT
013300000118     C                   MOVE      *ZEROS        GI8INV
013400000118     C                   CALL      'XSRGI8'
013500000118     C                   PARM                    WGIDA8
013600000118     C                   MOVE      GI8INV        DATPUL            8 0
013700000118     C*
013800000118     C                   ENDSR
