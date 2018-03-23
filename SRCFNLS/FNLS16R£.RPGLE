000100030714     h decedit('0,') datedit(*dmy.)
000200030714      * FNLS16R *----------------------------------------------------*
000300030714      *         RISTAMPA BOLLE FEDEX - FILTRO
000400030714      *--------------------------------------------------------------*
000500030714     fFNLS16D   cf   e             workstn
000600030714
000700030714      *
000800030714      *   S T R U T T U R E   D A T I
000900030714      *
001000030714     d kpjba         e ds
001100030714      * ds parametri per pgm. batch - FNLS17R
001200030714     d Fnls17ds      e ds                  inz
001300030714      *
001400030714      * ds parametri per pgm. richiesta stampanti - TRUL90R
001500030714     d Trul90ds      e ds                  inz
001600030714     d   D90rsb      e                     inz('S')
001700030714      * ds parametri per pgm. decodifica clienti -- TIBS69R
001800030714     d Tibs69ds      e ds                  inz
001900030714     d DS_cnaco      e ds                  extname(CNACO00F) inz
002000030714     d DS_cnind      e ds                  extname(CNIND00F) inz
002100030714     d DS_cnclp      e ds                  extname(CNCLP00F) inz
002200030714     d DS_fncls      e ds                  extname(FNCLS00F) inz
002300030714      *
002400030714     d ut§dse0f      e ds                  inz
002500030714      *
002600030714      * ds parametri per pgm. controllo data ------ XSRDA8
002700030714     d WLBDAT          ds                  inz
002800030714     d  G02dat                 1      8  0
002900030714     d  G02inv                 9     16  0
003000030714     d  G02err                17     17
003100030714     d  G02tgi                18     22  0
003200030714      *
003300030714     d Status         sds
003400030714     d  V1Cpgm           *proc
003500030714      *
003600030714      *   C O S T A N T I
003700030714      *
003800030714      * Codice cliente "FedEx"
003900030714     d K_CliFed        c                   const(0501331)
004000030714      *
004100030714      *---------------------------------------------------------------*
004200030714      *
004300030714      * Richiesta stampanti
004400030714     c                   call      'TRUL90R'
004500030714     c                   parm                    kpjba
004600030714     c                   parm                    Trul90ds
004700030714      * - F3 = Fine
004800030714do  1c                   dow       D90f3 <> *on
004900030714      *
005000030714      * Emissione video D01
005100030714     c                   exfmt     LS16D01
005200030714      * - F3 = Fine
005300030714if  2c                   if        *inkc = *on
005400030714     c                   leave
005500030714e   2c                   endif
005600030714      *
005700030714      * Controllo video D01
005800030714     c                   exsr      CONTR
005900030714      * - F6 = Lancio batch (se NO errori)
006000030714if  2c                   if            *inkf = *on
006100030714     c                             and *in90 = *off
006200030714     c                   exsr      srSbmJob
006300030714     c                   leave
006400030714e   2c                   endif
006500030714      *
006600030714e   1c                   enddo
006700030714      *
006800030714     c                   eval      *inlr = *on
006900030714
007000030714      *****************************************************************
007100030714      ** Operazioni Iniziali                                         **
007200030714      *****************************************************************
007300030714     c     *InzSr        BegSr
007400030714      *
007500030714     c     *Entry        plist
007600030714     c                   parm                    KPJBA
007700030714      *
007800030714     c                   call      'X§PARUT'
007900030714     c                   parm                    UT§DSE0F
008000030714     c                   movel     RAGUT         RSUT
008100030714      *
008200030714      * Impostazione iniziale dei campi a video
008300030714     c                   eval      V1Ccli = K_CliFed
008400030714     c                   clear                   V1Dcli
008500030714     c                   clear                   V1Cdsp
008600030714      * Decodifica cliente FedEx
008700030714     c                   clear                   Tibs69ds
008800030714     c                   eval      I69kac = V1Ccli
008900030714     c                   call      'TIBS69R'
009000030714     c                   parm                    Tibs69ds
009100030714     c                   parm                    DS_cnaco
009200030714     c                   parm                    DS_cnind
009300030714     c                   parm                    DS_cnclp
009400030714     c                   parm                    DS_fncls
009500030714if  1c                   if        O69err <> *on
009600030714     c                   movel     ACOrag        V1Dcli
009700030714e   1c                   endif
009800030714      *
009900030714     c                   EndSr
010000030714
010100030714      *****************************************************************
010200030714      ** Controlli Formato 1                                         **
010300030714      *****************************************************************
010400030714     c     Contr         BegSr
010500030714      *
010600030714     c                   eval      *in90 = *off
010700030714      * Data spedizione
010800030714     c                   clear                   WLBdat
010900030714     c                   z-add     V1Cdsp        G02dat
011000030714     c                   call      'XSRDA8'
011100030714     c                   parm                    WLBdat
011200030714     c                   if        G02err = *on
011300030714     c                   seton                                        40  90
011400030714     c                   goto      EndCtr
011500030714     c                   endif
011600030714     c                   z-add     G02dat        V1Cdsp
011700030714      *
011800030714     c     EndCtr        EndSr
011900030714
012000030714      *****************************************************************
012100030714      ** Lancio batch                                                **
012200030714      *****************************************************************
012300030714     c     srSbmJob      BegSr
012400030714      *
012500030714     c                   eval      I17psl  = D90psl
012600030714     c                   eval      I17prb4 = D90prb4
012700030714     c                   eval      I17prb5 = D90prb5
012800030714     c                   eval      I17mdb4 = D90mdb4
012900030714     c                   eval      I17mdb5 = D90mdb5
013000030714     c                   eval      I17prb  = D90prb
013100030714     c                   eval      I17mdb  = D90mdb
013200030714     c                   eval      I17cli  = V1Ccli
013300030714     c                   eval      I17dsp  = G02inv
013400030714      *
013500030714     c                   movel(p)  Fnls17ds      kpjbu
013600030714     c                   movel     'LS17'        kcoaz
013700030714     c                   movel     'BCH10     '  Wpgm             10
013800030714     c***                movel     'FNLS17C   '  Wpgm             10
013900030714     c                   call      Wpgm
014000030714     c                   parm                    kpjba
014100030714      *
014200030714     c                   EndSr
