000100031017     h Decedit('0,') Datedit(*dmy.) Option(*nodebugio)
000200031017
000300031017      *---------------------------------------------------------------*
000400150820      *       correzione CAP sulle bolle
000500031017      *---------------------------------------------------------------*
000600151110     fFnarb54l  if   e           k Disk    prefix(abb:3)
000700151110     ffiarbf2c  if   e           k Disk
000800150820     fprtf198   o    f  198        Printer Oflind(*InOf)
000900150820     D TISI95DS      E DS
001000150820     D fnlv13ds      E DS
001100150820     D kpjba         E DS
001200150820     D W0140           S             14  0
001300150820     D WORA            S              6  0
001400150820     D XX              S              3  0 INZ
001500150820     D WDTGIO          S              8  0
001600150820     D DATEU           S              8  0
001700150820     D DATA_eur        S               D   DATFMT(*eur)
001800150820
001900150820
002000150820     D digits          c                   '0123456789 /-'
002100031017
002200151110     c     karb          klist
002300151110     c                   kfld                    klna
002400151110     c                   kfld                    kdcm
002500151110     c     karbf         klist
002600151110     c                   kfld                    abbaas
002700151110     c                   kfld                    abblnp
002800151110     c                   kfld                    abbnrs
002900151110     c                   kfld                    abbnsp
003000150820      * Recupero data e ora
003100150820     C                   TIME                    WORA
003200150820     C                   TIME                    W0140
003300150820      * UDATE IN GGMMAAAA
003400150820     C                   MOVE      W0140         WDTGIO
003500150820      * UDATE IN AAAAMMGG
003600150820     C     *eur          MOVEL     WDTGIO        DATA_eur
003700150820     C     *iso          MOVEL     DATA_eur      dateu
003800150820      *
003900151110     c
004000151110     c                   clear                   klna              3 0
004100151110     c                   z-add     20151026      kdcm              8 0
004200151110     c
004300151110     c     klna          setgt     fnarb54l
004400151110     c                   read      fnarb54l
004500151110
004600151110    0c                   dow       not %eof(fnarb54l)
004700151110     c                   eval      klna=abblna
004800151110     c     karb          setll     fnarb54l
004900151110     c     klna          reade     fnarb54l
005000151110    1c                   dow       not %eof(fnarb54l)
005100151110    2c                   if        abbdcm>20151031
005200151110     c                   leave
005300151110    2c                   endif
005400150820
005500151110    2c                   if        abbnzd=*blanks and abbnrs>0
005600151110     c                   add       1             totb              9 0
005700151110     c
005800150820     C                   CLEAR                   TISI95DS
005900151110     C                   MOVEL     abbCAD        I95CAP
006000150820     C                   MOVEL     DATEU         I95DAT
006100150820     C                   MOVEL     '3'           I95TCN
006200150820     C                   CALL      'TISI95R'
006300150820     C                   PARM                    TISI95DS
006400150820      *     Errore
006500151110    3C     O95ERR        IFeq      *BLANKS
006600151110    4c                   if        o95flg='S'
006700151110     c* conto cap con viario
006800151110     c                   add       1             contaflg          9 0
006900151110     c* conto se dirottata
007000151110    5c                   if        abbcca='1'
007100151110     c                   add       1             contadir          9 0
007200151110    5c                   endif
007300151110     c
007400151110   x4c                   else
007500151110     c* se non è con viario vedo se fatta modifica
007600151110     c
007700151110     c     karbf         setll     fiarbf2c
007800151110     c     karbf         reade     fiarbf2c
007900151110    5c                   dow       not  %eof(fiarbf2c)
008000151110    6c                   if        arbcvb='I0'  and arbcad<>abbcad
008100151110     C                   CLEAR                   TISI95DS
008200151110     C                   MOVEL     arbCAD        I95CAP
008300151110     C                   MOVEL     DATEU         I95DAT
008400151110     C                   MOVEL     '3'           I95TCN
008500151110     C                   CALL      'TISI95R'
008600151110     C                   PARM                    TISI95DS
008700151110      *     Errore
008800151110    7C     O95ERR        IFeq      *BLANKS
008900151110    8c                   if        o95flg='S'
009000151110     c* conto cap con viario
009100151110     c                   add       1             contaflg          9 0
009200151110     c* conto se dirottata
009300151110    9c                   if        abbcca='1'
009400151110     c                   add       1             contadir          9 0
009500151110    9c                   endif
009600151110     c
009700151110    8c                   endif
009800151110    7c                   endif
009900151110    c
010000151110     c                   leave
010100151110    6c                   endif
010200151110
010300151110     c     karbf         reade     fiarbf2c
010400151110    5c                   enddo
010500151110     c
010600151110    4c                   endif
010700151110     c
010800151110    3c                   endif
010900151110    2c                   endif
011000151110     c
011100151110     c     klna          reade     fnarb54l
011200151110    1c                   enddo
011300151110     c
011400151110     c     klna          setgt     fnarb54l
011500151110     c                   read      fnarb54l
011600151110    1c                   enddo
011700151110     c
011800151110     c                   except    totale
011900031017
012000031017     c                   Eval      *InLr = *On
012100031017
012200151110     oprtf198   e            totale         1
012300151110     o                                           +1 'totale bolle CAP generico'
012400151110     o                       contaflg            +1
012500151110     o                                           +1 ' di cui dirottate'
012600151110     o                       contadir            +1
012700151110     o                                           +1 'totale bolle disk B Italia'
012800151110     o                       totb                +1
