000100000118     H DECEDIT('0,') DATEDIT(*DMY.)
000200000118?     *--------------------------------------------------------------*
000300000118      * CNC0W7R                                                      *
000400000118      *                                                              *
000500000118      *        PULIZIA MANDATI D'INTROITO VERSATI E PRESI IN CARICO  *
000600021127      *        PULIZIA TESTATE VERSAMENTI BANCARI                    *
000700021127      *        PULIZIA QUADRATURA INCASSI GIORNALIERI                *
000800061120      *        PULIZIA MANDATI CASSAFORTE                            *
000900000118      *                                                              *
001000000118?     *--------------------------------------------------------------*
001100060216     Fyfmdi01L  UF   E           K DISK
001200021127     FFNVRT01L  UF   E           K DISK
001300060216     FyfQIG01L  UF   E           K DISK
001400150714     Fyfqri01L  UF   E           K DISK
001500061120     FTNCAS00f  UF   E             DISK
001600000118     F*---------
001700000118     FTABEL00F  IF   E           K DISK    USROPN
001800000118?     *--------------------------------------------------------------*
001900000118      * DS
002000000118?     *--------------------------------------------------------------*
002100000118     D KPJBA         E DS
002200000118     D* DS pulizia archivio arrivi
002300000118     D DS5A2         E DS
002400000118     D* DS calcolo date
002500000118     D WLBDAT          DS
002600000118     D  G02DAT                 1      8  0
002700000118     D  G02INV                 9     16  0
002800000118     D  G02ERR                17     17
002900000118     D  G02TGI                18     22  0
003000000118     D WGIDA8          DS
003100000118     D  GI8DAT                 1      8  0
003200000118     D  GI8INV                 9     16  0
003300000118     D  GI8TGI                17     21  0
003400000118?     *--------------------------------------------------------------*
003500000118      * Ciclo Principale
003600000118?     *--------------------------------------------------------------*
003700021127     C                   EXSR      PULARM
003800021127     C                   EXSR      PULVRT
003900021127     C                   EXSR      PULQIG
004000061120     C                   EXSR      PULCAS
004100150714     C                   EXSR      PULQRI
004200021127      * Fine
004300021127     C     FINE          TAG
004400021127     C                   SETON                                        LR
004500021127?     *--------------------------------------------------------------*
004600021127     C* PULIZIA MANDATI D'INTROITO
004700021127     C     PULARM        BEGSR
004800021127?     *--------------------------------------------------------------*
004900000118      * Eseguo posizionamento ad inizio file Mandati d'intoito
005000060216     C     *LOVAL        SETLL     yfmdi01L
005100060216     C                   READ      yfmdi01L                               21
005200000118      * loop fino a che la data del mandato d'introto non è superiore
005300000118      * alla data limite pulizia oppure è finito il file
005400000118     C     *IN21         DOWEQ     '0'
005500060216     C     mdiDMI        ifle      DATPUL
005600060216      * cancello solo se è già stato versato
005700060216     C     mdivie        andeq     'S'
005800060216     C                   DELETE    yfmdi000
005900000118     C                   END
006000000118      * Leggo il record successivo
006100060216     C                   READ      yfmdi01L                               21
006200000118     C                   END
006300021127     C                   ENDSR
006400021127?     *--------------------------------------------------------------*
006500021127     C* PULIZIA TESTATE VERSAMENTI BANCARI
006600021127     C     PULVRT        BEGSR
006700021127?     *--------------------------------------------------------------*
006800021127     C     *LOVAL        SETLL     FNVRT01L
006900021127     c                   do        *hival
007000021127     C                   READ      FNVRT01L
007100021127     C                   if        %eof(fnvrt01l)
007200021127     c                   leave
007300021127     c                   end
007400021127     C     VRTDDV        ifle      DATPUL
007500021127     C                   DELETE    FNVRT000
007600021127     C                   END
007700021127     C                   ENDdo
007800021127     C                   ENDSR
007900021127?     *--------------------------------------------------------------*
008000021127     C* PULIZIA QUDRATURA INCASSI GIORNALIERI
008100021127     C     PULQIG        BEGSR
008200021127?     *--------------------------------------------------------------*
008300060216     C     *LOVAL        SETLL     yfqig01L
008400021127     c                   do        *hival
008500060216     C                   READ      yfqig01L
008600060216     C                   if        %eof(yfqig01l)
008700021127     c                   leave
008800021127     c                   end
008900021127     C     QIGDTA        ifle      DATPUL
009000060216     C                   DELETE    yfQIG000
009100021127     c                   end
009200021127     c                   enddo
009300021127     C                   ENDSR
009400150714?     *--------------------------------------------------------------*
009500150714     C* PULIZIA QUDRATURA INCASSI GIORNALIERI
009600150714     C     PULQRI        BEGSR
009700150714?     *--------------------------------------------------------------*
009800150714     C     *LOVAL        SETLL     yfqri01L
009900150714     c                   do        *hival
010000150714     C                   READ      yfqri01L
010100150714     C                   if        %eof(yfqri01l)
010200150714     c                   leave
010300150714     c                   end
010400150714     C     Qridtr        ifle      DATPUL
010500150714     C                   DELETE    yfQri000
010600150714     c                   end
010700150714     c                   enddo
010800150714     C                   ENDSR
010900061120?     *--------------------------------------------------------------*
011000061120     C* PULIZIA MANDATI CASSAFORTE
011100061120     C     PULCAS        BEGSR
011200061120?     *--------------------------------------------------------------*
011300061120     c                   do        *hival
011400061120     C                   READ      tncas00f
011500061120     C                   if        %eof(tncas00f)
011600061120     c                   leave
011700061120     c                   end
011800061120     C     casda8        ifle      DATPUL
011900061120     C                   DELETE    tncas000
012000061120     c                   end
012100061120     c                   enddo
012200061120     C                   ENDSR
012300000118?     *--------------------------------------------------------------*
012400000118      * Operazioni iniziali
012500000118?     *--------------------------------------------------------------*
012600000118     C     *INZSR        BEGSR
012700000118      *
012800000118     C     *ENTRY        PLIST
012900000118     C                   PARM                    KPJBA
013000000118      * Apertura file
013100000118     C                   OPEN      TABEL00F
013200000118      * Calcolo la data fino a cui pulire
013300000118     C     KTAB          KLIST
013400000118     C                   KFLD                    TBWKUT
013500000118     C                   KFLD                    TBWCOD
013600000118     C                   KFLD                    TBWKEY
013700000118     C* Definizione varibili
013800000118     C     *LIKE         DEFINE    TBLKUT        TBWKUT
013900000118     C     *LIKE         DEFINE    TBLCOD        TBWCOD
014000000118     C     *LIKE         DEFINE    TBLKEY        TBWKEY
014100000118     C*----------------------------------------------------*
014200000118     C                   Z-ADD     1             TBWKUT
014300000118     C                   MOVE      '5A'          TBWCOD
014400000118     C                   MOVEL     *BLANKS       TBWKEY
014500000118     C                   MOVEL     '2'           TBWKEY
014600000118     C                   CLEAR                   DS5A2
014700000118     C*----------------------------------------------------*
014800000118     C     KTAB          CHAIN     TABEL                              10
014900000118     C     *IN10         IFEQ      '0'
015000000118     C                   MOVEL     TBLUNI        DS5A2
015100000118     C                   ELSE
015200000118     C                   GOTO      FINE
015300000118     C                   END
015400000118      * Close del file
015500000118     C                   CLOSE     TABEL00F
015600000118      *
015700000118      * Reperisco la data  del giorno e la giro nel formato corretto
015800000118     C                   TIME                    WHHDAT           14 0
015900000118     C                   MOVE      WHHDAT        G02DAT
016000000118     C                   MOVE      *ZEROS        G02INV
016100000118     C                   MOVEL     *BLANKS       G02ERR
016200000118     C                   CALL      'XSRDA8'
016300000118     C                   PARM                    WLBDAT
016400000118     C* Sottraggo alla data del giorno i gg di pulizia
016500000118     C     G02TGI        SUB       §5ANMI        GI8TGI
016600000118     C                   MOVE      *ZEROS        GI8DAT
016700000118     C                   MOVE      *ZEROS        GI8INV
016800000118     C                   CALL      'XSRGI8'
016900000118     C                   PARM                    WGIDA8
017000000118     C                   MOVE      GI8INV        DATPUL            8 0
017100000118     C*
017200000118     C                   ENDSR
