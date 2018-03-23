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
001300070522     d
001400070124     d* DS ricevuta: p.o. in gestione da elaborare
001500060831     d FNLRL1DS        ds
001600060831     d  ILRL1lna                      3s 0
001700070406     d  ILRL1dat                      8s 0
001800070618     d  ILRL1dis                      1a   inz
001900070618     d  ILRL1tst                      1a   inz
002000070522     d  ILRL1senGiro                  1a
002100070618     d  LRL1contrBL                   1a   inz
002200070618     d  LRL1botlls                    2s 0 inz
002300060831     d
002400070522     d Dataiso         s               d   datfmt(*iso)
002500941109     C*------------------------------------------------------------------------*
002600070522     C     *ENTRY        PLIST
002700070522     C                   PARM                    KPJBA
002800070522     c* S=elabora solo bolle senza giro
002900070522     C                   PARM                    lrl5SenGiro       1
003000070522     C                   PARM                    lrl5ggela         1 0
003100070522     c
003200070522     c                   clear                   fnlrl1ds
003300070522     c*
003400070523     C* PARAMETRO --> DATA DI ELABORAZIONE GIRI DI CONSEGNA
003500070522     c                   time                    w0140            14 0
003600070522     c
003700070522     C                   MOVE      W0140         WDTGIO            8 0
003800070522     c     *eur          movel     wdtgio        dataiso
003900070522    1c                   if        lrl5ggela=0
004000070522     c                   movel     dataiso       ilrl1dat
004100070522    1c                   endif
004200070522     c
004300070522    1c                   if        lrl5ggela>0
004400070522     c* Elaboro sempre alla data + gg passati
004500070522     c     dataiso       adddur    lrl5ggela:*d  daTAISO
004600070522     c                   movel     dataiso       ilrl1dat
004700070522    1c                   endif
004800070523     C
004900070523     c* PARAMETRO --> BOLLE SENZA GIRO
005000070523     c                   eval      ilrl1SenGiro=lrl5SenGiro
005100070522     c
005200070523     C* PARAMETRO --> LINEA DA ELABORARE
005300070523     c                   z-add     0             x                 3 0
005400070523     c* Elaboro prima i terminal di arrivo e  poi i secondi livello
005500070522     c     *loval        setll     azorg01l
005600070522     c                   read      azorg01l
005700070522    1c                   dow       not %eof(azorg01l)
005800070522     c                   movel     orgde8        og148
005900070529    2c                   if        §ogtlls='S'
006000070522     c* controllo che non sia in £6
006100070522     c                   clear                   fnlv55ds
006200070522     c                   eval      d55tpt='6'
006300070522     c                   eval      d55lin=orgfil
006400070522     c                   eval      d55drf=ilrl1dat
006500070522     c                   call      'FNLV55R'
006600070522     c                   parm                    fnlv55ds
006700070522     c
006800070522    3c                   if        d55tfa=d55lin
006900070523     c
007000070523     c* Se secondo livello, memorizzo in schIera e lo faccio più tardi
007100070523     c                   clear                   fnlv55ds
007200070523     c                   eval      d55tpt='A'
007300070523     c                   eval      d55lin=orgfil
007400070523     c                   eval      d55drf=ilrl1dat
007500070523     c                   call      'FNLV55R'
007600070523     c                   parm                    fnlv55ds
007700070523     c
007800070523    3c                   if        d55tfa<>d55lin
007900070523     c                   add       1             x
008000070523     c                   eval      liv2(x)=orgfil
008100070523     c                   else
008200070522     c
008300070523     c* Sottometto azione di elaborazione TELLUS
008400070522     c                   eval      ilrl1lna=orgfil
008500070522     c                   movel     fnlrl1ds      kpjbu
008600070621     c                   eval      %subst(kbuff:1:10)='          '
008700070522     c                   movel     'LRL1'        kcoaz
008800070522     c                   call      'BCH10'
008900070522     c                   parm                    kpjba
009000070522     c
009100070522    3c                   endif
009200070522    2c                   endif
009300070523    2c                   endif
009400070523     c
009500070522     c                   read      azorg01l
009600070522    1c                   enddo
009700070523     c
009800070523     c* finita la sottomissione elaborazione dei terminal di arrivo
009900070523     c*  sottometto i secondi livello
010000070523     c                   z-add     1             x
010100070523     c                   dow       x<=500 and liv2(x)>0
010200070523     c
010300070523     c* Sottometto azione di elaborazione TELLUS
010400070523     c                   eval      ilrl1lna=liv2(x)
010500070620     c                   eval      %subst(kbuff:1:10)='          '
010600070523     c                   movel     fnlrl1ds      kpjbu
010700070523     c                   movel     'LRL1'        kcoaz
010800070523     c                   call      'BCH10'
010900070523     c                   parm                    kpjba
011000070523     c
011100070523     c                   add       1             x
011200070523    3c                   enddo
011300070522     c
011400070522     c                   SETON                                        LR
