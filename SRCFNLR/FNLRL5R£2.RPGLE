000100941109     H*------------------------------------------------------------------------*
000200070522     H* Azione schedulata scambio dati con TELLUS                              *
000300941109     H*------------------------------------------------------------------------*
000400930128     H DECEDIT('0,') DATEDIT(*DMY.)
000500941109     F*------------------------------------------------------------------------*
000600070522     fazorg01l  if   e           k disk
000700060828     d
000800070523     D liv2            s              3  0 dim(500)
000900070523     d
001000070522     D KPJBA         E DS
001100070522     D og148         E DS
001200070522     D FNLV55DS      E DS
001300070917     d
001400070917      * DATA E ORA SCHEDULAZIONE PER KPJBA
001500070917     D                 DS
001600070917     d* Data in ggmmaa
001700070917     D  D1DATS                 1      6  0
001800070917     D  D1ORAS                 7     10  0
001900070917     D  SCHEDS                 1     10  0
002000070522     d
002100070124     d* DS ricevuta: p.o. in gestione da elaborare
002200060831     d FNLRL1DS        ds
002300060831     d  ILRL1lna                      3s 0
002400070406     d  ILRL1dat                      8s 0
002500070618     d  ILRL1dis                      1a   inz
002600070618     d  ILRL1tst                      1a   inz
002700070522     d  ILRL1senGiro                  1a
002800070618     d  LRL1contrBL                   1a   inz
002900070618     d  LRL1botlls                    2s 0 inz
003000060831     d
003100070522     d Dataiso         s               d   datfmt(*iso)
003200070917     d Datadmy         s               d   datfmt(*dmy)
003300070917     d Oraiso          s               t   timfmt(*iso)
003400070917     d worasked        s               z
003500941109     C*------------------------------------------------------------------------*
003600070522     C     *ENTRY        PLIST
003700070522     C                   PARM                    KPJBA
003800070522     c* S=elabora solo bolle senza giro
003900070522     C                   PARM                    lrl5SenGiro       1
004000070522     C                   PARM                    lrl5ggela         1 0
004100070522     c
004200070522     c                   clear                   fnlrl1ds
004300070522     c*
004400070523     C* PARAMETRO --> DATA DI ELABORAZIONE GIRI DI CONSEGNA
004500070522     c                   time                    w0140            14 0
004600070522     c
004700070522     C                   MOVE      W0140         WDTGIO            8 0
004800070917     c*
004900070522     c     *eur          movel     wdtgio        dataiso
005000070522    1c                   if        lrl5ggela=0
005100070522     c                   movel     dataiso       ilrl1dat
005200070522    1c                   endif
005300070522     c
005400070522    1c                   if        lrl5ggela>0
005500070522     c* Elaboro sempre alla data + gg passati
005600070522     c     dataiso       adddur    lrl5ggela:*d  daTAISO
005700070522     c                   movel     dataiso       ilrl1dat
005800070522    1c                   endif
005900070523     C
006000070523     c* PARAMETRO --> BOLLE SENZA GIRO
006100070523     c                   eval      ilrl1SenGiro=lrl5SenGiro
006200070522     c
006300070523     C* PARAMETRO --> LINEA DA ELABORARE
006400070523     c                   z-add     0             x                 3 0
006500070523     c* Elaboro prima i terminal di arrivo e  poi i secondi livello
006600070522     c     *loval        setll     azorg01l
006700070522     c                   read      azorg01l
006800070522    1c                   dow       not %eof(azorg01l)
006900070522     c                   movel     orgde8        og148
007000070529    2c                   if        §ogtlls='S'
007100070522     c* controllo che non sia in £6
007200070522     c                   clear                   fnlv55ds
007300070522     c                   eval      d55tpt='6'
007400070522     c                   eval      d55lin=orgfil
007500070522     c                   eval      d55drf=ilrl1dat
007600070522     c                   call      'FNLV55R'
007700070522     c                   parm                    fnlv55ds
007800070522     c
007900070522    3c                   if        d55tfa=d55lin
008000070523     c
008100070523     c* Se secondo livello, memorizzo in schIera e lo faccio più tardi
008200070523     c                   clear                   fnlv55ds
008300070523     c                   eval      d55tpt='A'
008400070523     c                   eval      d55lin=orgfil
008500070523     c                   eval      d55drf=ilrl1dat
008600070523     c                   call      'FNLV55R'
008700070523     c                   parm                    fnlv55ds
008800070523     c
008900070523    3c                   if        d55tfa<>d55lin
009000070523     c                   add       1             x
009100070523     c                   eval      liv2(x)=orgfil
009200070523     c                   else
009300070522     c
009400070523     c* Sottometto azione di elaborazione TELLUS
009500070522     c                   eval      ilrl1lna=orgfil
009600070522     c                   movel     fnlrl1ds      kpjbu
009700070621     c                   eval      %subst(kbuff:1:10)='          '
009800070522     c                   movel     'LRL1'        kcoaz
009900070522     c                   call      'BCH10'
010000070522     c                   parm                    kpjba
010100070522     c
010200070522    3c                   endif
010300070522    2c                   endif
010400070523    2c                   endif
010500070523     c
010600070522     c                   read      azorg01l
010700070522    1c                   enddo
010800070523     c
010900070523     c* finita la sottomissione elaborazione dei terminal di arrivo
011000070523     c*  sottometto i secondi livello
011100070523     c                   z-add     1             x
011200070523     c                   dow       x<=500 and liv2(x)>0
011300070523     c
011400070523     c* Sottometto azione di elaborazione TELLUS
011500070523     c                   eval      ilrl1lna=liv2(x)
011600070917     c* schedulata 10 minuti più avanti
011700070620     c                   eval      %subst(kbuff:1:10)='          '
011800070917     c                   time                    worasked
011900070917     c
012000070917     c                   adddur    10:*mn        worasked
012100070917     c                   movel     worasked      dataiso
012200070917     c                   movel     dataiso       datadmy
012300070917     c                   movel     datadmy       d1dats
012400070917     c
012500070917     c                   move      worasked      oraiso
012600070917     c                   movel     oraiso        w0060             6 0
012700070917     c                   movel     w0060         d1oras
012800070917     c                   movel     scheds        kbuff
012900070917     c
013000070523     c                   movel     fnlrl1ds      kpjbu
013100070523     c                   movel     'LRL1'        kcoaz
013200070523     c                   call      'BCH10'
013300070523     c                   parm                    kpjba
013400070523     c
013500070523     c                   add       1             x
013600070523    3c                   enddo
013700070522     c
013800070522     c                   SETON                                        LR
