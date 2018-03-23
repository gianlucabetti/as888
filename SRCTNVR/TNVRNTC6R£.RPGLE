000100000000     H DECEDIT('0,') DATEDIT(*DMY.)
000200041210     Ffnarb26l  IF   E           K DISK
000300041210     Ffnar401l  IF   E           K DISK
000400041210     Ffiar601l  IF   E           K DISK
000500041209     Ftabel00F  IF   E           K DISK
000600050124     Fwfsas00f  O    E             DISK
000700041209     D
000800041210     d s3as            s              2    dim(30)
000900041210     d s3a             s              2    dim(30)
001000041210     d dsbl4a        E DS
001100041209     D DS3a          E DS
001200041209     D kpjba         E DS
001300041209     d*
001400041209     dparam            ds
001500041209     Dpardtd                   1      8  0
001600041209     Dpardta                   9     16  0
001700041210     d                 ds
001800041210     darbdsp                   1      8  0
001900041210     Darbaas                   1      4  0
002000041210     Darbmgs                   5      8  0
002100041209     d*
002200041209     c     *entry        plist
002300041209     c                   parm                    kpjba
002400041209     c                   movel     kpjbu         param
002500041210     c*
002600041210     c     karb          klist
002700041210     c                   kfld                    arblna
002800041210     c                   kfld                    waas
002900041210     c                   kfld                    wmgs
003000041210     c     kar6          klist
003100041210     c                   kfld                    arbaas
003200041210     c                   kfld                    arblnp
003300041210     c                   kfld                    arbnrs
003400041210     c                   kfld                    arbnsp
003500041210     c                   kfld                    wtrc
003600041210     C* ACCESSO   TABEL
003700041210     C     KTAB          KLIST
003800041210     C                   KFLD                    CODUT
003900041210     C                   KFLD                    COD               2
004000041210     C                   Z-ADD     1             CODUT             1 0
004100041210     c*
004200041210     c* carico schiera con codici bolla monovaria e schiera codici bolla
004300041210     c* assegnato (escludo assegnato seconda bolla per semplificare tanto
004400041210     c* ne bollettano pochi)
004500041210     c                   z-add     0             ii                2 0
004600041210     c                   z-add     0             i                 2 0
004700041210     c                   clear                   s3a
004800041210     c                   clear                   s3as
004900041210     c                   movel     '3A'          cod
005000041210     c     ktab          setll     tabel00f
005100041210     c                   do        *hival
005200041210     c     ktab          reade     tabel00f
005300041210     c                   if        %eof(tabel00f)
005400041210     c                   leave
005500041210     c                   endif
005600041210     c                   movel     tbluni        ds3a
005700041210     c                   if        %subst(§3atb1:1:1) = 'A'
005800041210     c                   add       1             i
005900041210     c                   movel     tblkey        s3a(i)
006000041210     c                   endif
006100041210     c                   if        §3asva <> *blanks  and
006200041210     c                             §3atb1 = 'A3'
006300041210     c                   add       1             ii
006400041210     c                   movel     tblkey        s3as(ii)
006500041210     c                   endif
006600041210     c                   enddo
006700041210     c*
006800041210     c     *like         define    arbaas        waas
006900041210     c     *like         define    arbmgs        wmgs
007000041210     c     *like         define    ar6trc        wtrc
007100041210     c     *like         define    arblna        savlna
007200041210     c                   movel     pardtd        waas
007300041210     c                   move      pardtd        wmgs
007400041210     c                   clear                   savlna
007500041210     c*
007600041210     c     *loval        setll     fnarb26l
007700041209     c                   do        *hival
007800041210     c                   read      fnarb26l
007900041210     c* fine file
008000041210     c                   if        %eof(fnarb26l)
008100041209     c                   leave
008200041209     c                   endif
008300041210     c* se data spedizione > del limite mi posiziono su nuova lna
008400041210     c     arbdsp        ifgt      pardta
008500041210     c     arblna        setgt     fnarb26l
008600041210     c                   iter
008700041210     c                   endif
008800041210     c* se data spedizione < del limite minimo mi posiziono con la data
008900041210     c     arbdsp        iflt      pardtd
009000041210     c     karb          setll     fnarb26l
009100041210     c                   iter
009200041210     c                   endif
009300041210     c* se non è assegnato ne leggo un'altra
009400041210     c     arbcbo        lookup    s3a                                    30
009500041210     c     *in30         ifeq      *off
009600041210     c                   iter
009700041210     c                   endif
009800041213     c                   eval      wtrc = '1'
009900041213     c     kar6          chain     fiar601l
010000041213     c* scarto record se non presente record fiar6
010100041213     c                   if        not %found(fiar601l)
010200041213     c                   iter
010300041213     c                   endif
010400041210     c* se cambiata lna scrivo record in file di work e azzero campi
010500041210     c     arblna        ifne      savlna
010600041210     c     savlna        ifgt      *zeros
010700041210     C                   Z-ADD     SAvlna        asslna
010800041210     c                   z-add     pardtd        assdtd
010900041210     c                   z-add     pardta        assdta
011000050124     c                   write     wfsas000
011100041210     c                   endif
011200050124     c                   clear                   wfsas000
011300041210     c                   z-add     arblna        savlna
011400041210     c                   endif
011500041210     c*
011600041210     c     arbcbo        lookup    s3as                                   30
011700041210     c                   select
011800041210     c* ASSEGNATI STANDARD
011900041210     c                   when      arbcbo = '2 ' or arbcbo = '6 ' or
012000041210     c                             arbcbo = 'FW'
012100041210     c* fattura immediata
012200041210     c                   if        ar6dft > 0
012300041210     c                   add       1             asssti
012400041210     c* segue fattura
012500041210     c                   else
012600041210     c                   add       1             asssts
012700041210     c                   endif
012800041210     C* ASSEGNATI RECUPERO MONOVARIA
012900041210     c                   when      *in30 = *on
013000041210     c* chaino ar4 trk "A" per vedere se presente cod.cliente
013100041210     c                   eval      wtrc = 'A'
013200041210     c     kar6          chain     fnar401l
013300041210     c                   if        %found(fnar401l)
013400041210     c                   eval      dsbl4a = ar4not
013500041210     c                   else
013600041210     c                   clear                   dsbl4a
013700041210     c                   endif
013800041210     c* fattura immediata
013900041210     c                   if        ar6dft > 0
014000041210     c                   select
014100041210     c                   when      arbcbo = 'F*'
014200041210     c                   add       1             assmofai
014300041210     c                   if        §b4aks > 0
014400041210     c                   add       1             assmofaik
014500041210     c                   endif
014600041210     c                   when      arbcbo = 'FO'
014700041210     c                   add       1             assmofoi
014800041210     c                   if        §b4aks > 0
014900041210     c                   add       1             assmofoik
015000041210     c                   endif
015100041210     c                   when      arbcbo = 'FT'
015200041210     c                   add       1             assmofti
015300041210     c                   if        §b4aks > 0
015400041210     c                   add       1             assmoftik
015500041210     c                   endif
015600041210     c                   when      arbcbo = 'FY'
015700041210     c                   add       1             assmofyi
015800041210     c                   if        §b4aks > 0
015900041210     c                   add       1             assmofyik
016000041210     c                   endif
016100041210     c                   endsl
016200041210     c* segue fattura
016300041210     c                   else
016400041210     c                   select
016500041210     c                   when      arbcbo = 'F*'
016600041210     c                   add       1             assmofas
016700041210     c                   if        §b4aks > 0
016800041210     c                   add       1             assmofask
016900041210     c                   endif
017000041210     c                   when      arbcbo = 'FO'
017100041210     c                   add       1             assmofos
017200041210     c                   if        §b4aks > 0
017300041210     c                   add       1             assmofosk
017400041210     c                   endif
017500041210     c                   when      arbcbo = 'FT'
017600041210     c                   add       1             assmofts
017700041210     c                   if        §b4aks > 0
017800041210     c                   add       1             assmoftsk
017900041210     c                   endif
018000041210     c                   when      arbcbo = 'FY'
018100041210     c                   add       1             assmofys
018200041210     c                   if        §b4aks > 0
018300041210     c                   add       1             assmofysk
018400041210     c                   endif
018500041210     c                   endsl
018600041210     c                   endif
018700041210     c                   other
018800041210     c* ASSEGNATI RESTANTI
018900041210     c* fattura immediata
019000041210     c                   if        ar6dft > 0
019100041210     c                   add       1             assrei
019200041210     c                   else
019300041210     c                   add       1             assres
019400041210     c                   endif
019500041210     c                   endsl
019600041210     c
019700041210     c
019800041210     c*
019900041209     c                   enddo
020000041213     c* se cambiata lna scrivo record in file di work e azzero campi
020100041213     c     savlna        ifgt      *zeros
020200041213     C                   Z-ADD     SAvlna        asslna
020300041213     c                   z-add     pardtd        assdtd
020400041213     c                   z-add     pardta        assdta
020500050124     c                   write     wfsas000
020600041213     c                   endif
020700961108     C*
020800000000     C                   SETON                                        LR
