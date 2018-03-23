000100040119      *===============================================================*
000200080715      * Totalizzo varia f prima e dopo
000300040119      *===============================================================*
000400040119
000500040119     h decedit('0,') datedit(*dmy/)
000600040119     h option(*nodebugio)
000700040119
000800040119      *---------------------------------------------------------------*
000900040119
001000080715     fwfcft01l  if   e           k disk
001100080715     ftnvrfuelp o    e             printer oflind(*in90)
001200080715      *
001300080715     d  sv             s              1    Dim(20)                              Sigle varie
001400080715     d  va             s             11  3 Dim(20)                              Importi varie
001500050607     d
001600080715      *
001700080715     d kpjba         e ds
001800050607     dwfcftxxds      e ds                  extname(wfcft00f)
001900050607     dsvpre                   31     50    dim(20)
002000050607     dvapre                   51    170p 3 dim(20)
002100050607     dsvcor                  184    203    dim(20)
002200050607     dvacor                  204    323p 3 dim(20)
002300050607     d*
002400040331      *
002500050526     D***
002600040331      *
002700040331      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
002800050526      *
002900040331      * - Indici di schiera
003000080715     d k               s              3  0 inz
003100080715     D totfuepiu       s             11  3
003200080715     D totfue          s             11  3
003300080715     D totimpf         s             13  3
003400080715     D totimpfp        s             13  3
003500080715     D totfat          s             13  3
003600080715     D totfatp         s             13  3
003700080715     D totadd          s             11  3
003800080715     D totaddp         s             11  3
003900080715     D totspef         s              9  0
004000080715     D totspefp        s              9  0
004100080715     D totspet         s              9  0
004200080715     D totspetp        s              9  0
004300060914      *
004400060915      *
004500040119
004600040119      *
004700060914      * mi posiziono con anno mese da elaborare
004800080715     c     *loval        setll     wfcft01l
004900050607     c                   do        *hival
005000080715     c                   read      wfcft01l
005100080715     c                   if        %eof(wfcft01l)
005200050607     c                   leave
005300050607     c                   endif
005400060915     c
005500080715      ******* senza l'1% in più ******
005600080715     c                   movea     vacor         va
005700080715     c                   movea     svcor         sv
005800061218      * cerco la varia "f" (supplemento carburante)
005900080715      * se c'è la sommo al totale fuel senza l'1% in più
006000061218     c                   z-add     1             k
006100061218     c     'f'           lookup    sv(k)                                  30
006200061218     c                   if        %found
006300080715     c                   add       va(k)         totfue
006400080715      * conto le spedizioni fuel
006500080715     c                   add       1             totspef
006600080715      * imponibile fuel
006700080715     c                   add       cftporc       totimpf
006800080715      * cerco  inoltro
006900080715     c                   z-add     1             k
007000080715     c     '2'           lookup    sv(k)                                  30
007100080715     c                   if        %found
007200080715     c                   add       va(k)         totimpf
007300080715     c                   endif
007400080715      * cerco  isola
007500080715     c                   z-add     1             k
007600080715     c     'J'           lookup    sv(k)                                  30
007700080715     c                   if        %found
007800080715     c                   add       va(k)         totimpf
007900080715     c                   endif
008000080715      * cerco  disagiate
008100080715     c                   z-add     1             k
008200080715     c     'K'           lookup    sv(k)                                  30
008300080715     c                   if        %found
008400080715     c                   add       va(k)         totimpf
008500080715     c                   endif
008600080715      * cerco  centro storico
008700080715     c                   z-add     1             k
008800080715     c     'Q'           lookup    sv(k)                                  30
008900080715     c                   if        %found
009000080715     c                   add       va(k)         totimpf
009100080715     c                   endif
009200080715      * cerco  espresso
009300080715     c                   z-add     1             k
009400080715     c     'e'           lookup    sv(k)                                  30
009500080715     c                   if        %found
009600080715     c                   add       va(k)         totimpf
009700080715     c                   endif
009800080715      * cerco la varia "b" "Z" "L" addizionali di gestione
009900080715     c                   z-add     1             k
010000080715     c     'b'           lookup    sv(k)                                  30
010100080715     c                   if        %found
010200080715     c                   add       va(k)         totadd
010300080715     c                   endif
010400080715
010500080715     c                   z-add     1             k
010600080715     c     'Z'           lookup    sv(k)                                  30
010700080715     c                   if        %found
010800080715     c                   add       va(k)         totadd
010900080715     c                   endif
011000080715
011100080715     c                   z-add     1             k
011200080715     c     'L'           lookup    sv(k)                                  30
011300080715     c                   if        %found
011400080715     c                   add       va(k)         totadd
011500080715     c                   endif
011600080715     c                   endif
011700080715      * conto le spedizioni tassate
011800080715     c                   add       1             totspet
011900080715      * totale imponibile fatturato
012000080715     c                   add       cftimvc       totfat
012100080715
012200080715      ******* con   l'1% in più ******
012300080715     c                   movea     vapre         va
012400080715     c                   movea     svpre         sv
012500080715      * cerco la varia "f" (supplemento carburante)
012600080715      * se c'è la sommo al totale fuel con   l'1% in più
012700080715     c                   z-add     1             k
012800080715     c     'f'           lookup    sv(k)                                  30
012900080715     c                   if        %found
013000080715     c                   add       va(k)         totfuepiu
013100080715      * conto le spedizioni fuel
013200080715     c                   add       1             totspefp
013300080715      * imponibile fuel
013400080715     c                   add       cftporp       totimpfp
013500080715      * cerco  inoltro
013600080715     c                   z-add     1             k
013700080715     c     '2'           lookup    sv(k)                                  30
013800080715     c                   if        %found
013900080715     c                   add       va(k)         totimpfp
014000080715     c                   endif
014100080715      * cerco  isola
014200080715     c                   z-add     1             k
014300080715     c     'J'           lookup    sv(k)                                  30
014400080715     c                   if        %found
014500080715     c                   add       va(k)         totimpfp
014600080715     c                   endif
014700080715      * cerco  disagiate
014800080715     c                   z-add     1             k
014900080715     c     'K'           lookup    sv(k)                                  30
015000080715     c                   if        %found
015100080715     c                   add       va(k)         totimpfp
015200080715     c                   endif
015300080715      * cerco  centro storico
015400080715     c                   z-add     1             k
015500080715     c     'Q'           lookup    sv(k)                                  30
015600080715     c                   if        %found
015700080715     c                   add       va(k)         totimpfp
015800080715     c                   endif
015900080715      * cerco  espresso
016000080715     c                   z-add     1             k
016100080715     c     'e'           lookup    sv(k)                                  30
016200080715     c                   if        %found
016300080715     c                   add       va(k)         totimpfp
016400080715     c                   endif
016500080715      * cerco la varia "b" "Z" "L" addizionali di gestione
016600080715     c                   z-add     1             k
016700080715     c     'b'           lookup    sv(k)                                  30
016800080715     c                   if        %found
016900080715     c                   add       va(k)         totaddp
017000080715     c                   endif
017100080715
017200080715     c                   z-add     1             k
017300080715     c     'Z'           lookup    sv(k)                                  30
017400080715     c                   if        %found
017500080715     c                   add       va(k)         totaddp
017600080715     c                   endif
017700080715
017800080715     c                   z-add     1             k
017900080715     c     'L'           lookup    sv(k)                                  30
018000080715     c                   if        %found
018100080715     c                   add       va(k)         totaddp
018200080715     c                   endif
018300080715     c                   endif
018400080715      * conto le spedizioni tassate
018500080715     c                   add       1             totspetp
018600080715      * totale imponibile fatturato
018700080715     c                   add       cftimvp       totfatp
018800080715
018900080715     c                   enddo
019000080715      * stampo  la fine elaborazione
019100080715     c                   write     vrfue1
019200080715
019300080715     c                   eval      *inLR = *on
019400080715     c
019500080715      *************************************************************************
019600080715      * Operazioni iniziali
019700080715      *************************************************************************
019800080715     C     *inzsr        BEGSR
019900080715     c*
020000080715     c
020100080715     c     *entry        plist
020200080715     c                   parm                    kpjba
020300080715
020400080715     c                   endsr
