000100170314     d pInDec          s             30  0
000200170314     d pInBase         s              2  0
000300170314     d pOutCvt         s             30
000400030618
000500170314     d dec             s             30  0
000600170314     d resto           s              2  0
000700170314     d xx              s              3  0
000800030618
000900170314     d Base16          c                   const('123456789ABCDEF')
001000170314     d Base32          c                   const('123456789ABCDEFGHIJKLMNOPQRST-
001100170314     d                                     UV')
001200170314     d Base64          c                   const('123456789abcdefghijklmnopqrst-
001300170314     d                                     uvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ$_')
001400030618
001500030618     c     *entry        plist
001600170314     c                   parm                    pInDec
001700170314     c                   parm                    pInBase
001800170314     c                   parm                    pOutCvt
001900030618
002000030618      /free
002100170314
002200170314        // Inizializzazioni
002300170314        dec = pInDec;
002400170314        pOutCvt = *blanks;
002500170314        xx = %len(pInDec)-2;
002600170314
002700170314        // Se base non specificata assumo esadecimale
002800170314        if pInBase = *zeros;
002900170314           pInBase = 16;
003000170314        endif;
003100170314
003200170314        // In relazione alla base di conversione richiesta eseguo specifica routine
003300170314        select;
003400170314           when pInBase = 16;
003500170314                exsr CvtBase16;
003600170314           when pInBase = 32;
003700170314                exsr CvtBase32;
003800170314           when pInBase = 64;
003900170314                exsr CvtBase64;
004000170314        endsl;
004100030618
004200170314        // Valorizzo il parametro di ritorno
004300170314        pOutCvt = %trim(pOutCvt);
004400170314
004500170314        *inlr = *on;
004600170314
004700170314
004800170314
004900170314        begsr CvtBase16;
005000170314
005100170314           dow dec <> *zeros;
005200170314              resto = %rem(dec:pInBase);
005300170314              dec = %div(dec:pInBase);
005400170314
005500170314              if resto <> *zeros;
005600170314                 %subst(pOutCvt:xx:1) = %subst(Base16:resto:1);
005700170314              else;
005800170314                 %subst(pOutCvt:xx:1) = '0';
005900170314              endif;
006000170314
006100170314              xx = xx -1;
006200170314           enddo;
006300170314
006400170314        endsr;
006500170314
006600170314
006700170314
006800170314        begsr CvtBase32;
006900170314
007000170314           dow dec <> *zeros;
007100170314              resto = %rem(dec:pInBase);
007200170314              dec = %div(dec:pInBase);
007300170314
007400170314              if resto <> *zeros;
007500170314                 %subst(pOutCvt:xx:1) = %subst(base32:resto:1);
007600170314              else;
007700170314                 %subst(pOutCvt:xx:1) = '0';
007800170314              endif;
007900170314
008000170314              xx = xx -1;
008100170314           enddo;
008200170314
008300170314        endsr;
008400170314
008500170314
008600170314
008700170314        begsr CvtBase64;
008800170314
008900170314           dow dec <> *zeros;
009000170314              resto = %rem(dec:pInBase);
009100170314              dec = %div(dec:pInBase);
009200170314
009300170314              if resto <> *zeros;
009400170314                 %subst(pOutCvt:xx:1) = %subst(base64:resto:1);
009500170314              else;
009600170314                 %subst(pOutCvt:xx:1) = '0';
009700170314              endif;
009800170314
009900170314              xx = xx -1;
010000170314           enddo;
010100170314
010200170314        endsr;
010300170314
010400030618
010500030618      /end-free
010600170314
