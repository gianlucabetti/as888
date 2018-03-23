000100030714     h decedit('0,') datedit(*dmy.)
000200030714      * FNLS17R *----------------------------------------------------*
000300030714      *         RISTAMPA BOLLE FEDEX - BATCH
000400030714      *--------------------------------------------------------------*
000500030714     fFNBLP70L  if   e           k disk
000600030714
000700030714      *
000800030714      *   S T R U T T U R E   D A T I
000900030714      *
001000030714     d KPJBA         e ds
001100030714      * ds parametri
001200030714     d Fnls17ds      e ds
001300030714      * DS per pgm di (RI)STAMPA BOLLE
001400030714     d FnlsB5ds      e ds                  inz
001500030714     d   DB0tbo      e                     inz('P')
001600030714     d   DB0ris      e                     inz('F')
001700030714      *
001800030714      *   V A R I A B I L I
001900030714      *
002000030714     d $OK             s              1    inz(*off)
002100030714     d wPagA4          s              5  0 inz
002200030714     d wPagA5          s              5  0 inz
002300030714     d wPagB0          s              5  0 inz
002400030714
002500030714      *****************************************************************
002600030714      *  RIEPILOGO INDICATORI                                         *
002700030714      *---------------------------------------------------------------*
002800030714      *                                                               *
002900030714      *****************************************************************
003000030714
003100030714     c     *Entry        plist
003200030714     c                   parm                    KPJBA
003300030714     c                   movel     KPJBU         FNLS17DS
003400030714      *
003500030714      * Ciclo di lettura file FNBLP70L
003600030714     c                   movel     I17cli        BLPlnp
003700030714     c                   movel     I17dsp        BLPaas
003800030714     c                   move      I17dsp        BLPmgs
003900030714     c     K03BLP70      setll     FNBLP70L
004000030714     c     K03BLP70      reade     FNBLP70L
004100030714      *
004200030714do  1c                   dow       NOT %eof(FNBLP70L)
004300030714      *
004400030714      * - Selezione del record letto
004500030714     c                   exsr      SelezRec
004600030714      * - Stampa
004700030714cas 2c     $OK           caseq     *on           PrintBOLLA
004800030714e   2c                   endcs
004900030714      *
005000030714     c     K03BLP70      reade     FNBLP70L
005100030714      *
005200030714e   1c                   enddo
005300030714      *
005400030714      * Fine
005500030714     c                   clear                   FNLSB5DS
005600030714     c                   eval      DB0tla = 'C'
005700030714     c                   call      I17psl
005800030714     c                   parm                    FNLSB5DS
005900030714      *
006000030714     c                   eval      *inlr = *on
006100030714      *
006200030714      **-------------------------------------------------------------**
006300030714      ** Selezione del record in input                               **
006400030714      **-------------------------------------------------------------**
006500030714     c     SelezRec      BegSr
006600030714      *
006700030714     c                   reset                   $OK
006800030714      *
006900030714      * P.O. di partenza
007000030714      * (non serve: campo in chiave di lettura)
007100030714      *
007200030714      * Data Spedizione
007300030714      * (non serve: campo in chiave di lettura)
007400030714      *
007500030714      * Codice Cliente (FedEx)
007600030714if  1c                   if        BLPccm > *zeros
007700030714     c     BLPccm        cabne     I17cli        EndSel
007800030714x   1c                   else
007900030714     c     BLPksc        cabne     I17cli        EndSel
008000030714e   1c                   endif
008100030714      *
008200030714      * Eclusione dei PRE-pagati NON confermati
008300030714     c     BLPpdr        cabeq     *zeros        EndSel
008400030714      *
008500030714      * => Rec. OK
008600030714     c                   eval      $OK = *on
008700030714      *
008800030714     c     EndSel        EndSr
008900030714      *
009000030714      **-------------------------------------------------------------**
009100030714      ** Lancio Stampa LdV                                           **
009200030714      **-------------------------------------------------------------**
009300030714     c     PrintBOLLA    BegSr
009400030714      *
009500030714      * Imposto i parametri
009600030714     c                   reset                   FNLSB5DS
009700030714     c                   eval      DB0aas =  BLPaas
009800030714     c                   eval      DB0lnp =  BLPlnp
009900030714     c                   eval      DB0nrs =  BLPnrs
010000030714     c                   eval      DB0nsp =  BLPnsp
010100030714      *
010200030714      * Se ho già stampato 490 bolle:
010300030714if  1c                   if           (wPagA4 >= 490
010400030714     c                             or  wPagA5 >= 490
010500030714     c                             or  wPagB0 >= 490)
010600030714      *    1) chiedo la chiusura e riapertura dei prtf
010700030714     c                   eval      DB0fl3 =  *on
010800030714     c                   eval      DB0fl4 =  *on
010900030714     c                   eval      DB0fl6 =  *on
011000030714     c                   eval      DB0fl7 =  *on
011100030714     c                   eval      DB0fl8 =  *on
011200030714      *    2) e riparto con i conteggi
011300030714     c                   clear                   wPagA4
011400030714     c                   clear                   wPagA5
011500030714     c                   clear                   wPagB0
011600030714e   1c                   endif
011700030714      *
011800030714      * Richiamo pgm di stampa bolla
011900030714     c                   call      I17psl
012000030714     c                   parm                    FNLSB5DS
012100030714      *
012200030714      * Aggiorno il conteggio delle pagine stampate (x tipo)
012300030714sel 1c                   select
012400030714      * - formato A4 su stampante laser
012500030714w   1c                   when      DB0tms = '4'
012600030714     c                   add       1             wPagA4
012700030714      * - formato A5 su stampante laser
012800030714w   1c                   when      DB0tms = '5'
012900030714     c                   add       1             wPagA5
013000030714      * - altri..... su stampante ad aghi
013100030714x   1c                   other
013200030714     c                   add       1             wPagB0
013300030714e   1c                   endsl
013400030714      *
013500030714     c                   EndSr
013600030714      *
013700030714      **-------------------------------------------------------------**
013800030714      ** Definizione Key-List                                        **
013900030714      **-------------------------------------------------------------**
014000030714     c     DefKLIST      BegSr
014100030714      *
014200030714     c     K03BLP70      klist
014300030714     c                   kfld                    BLPlnp
014400030714     c                   kfld                    BLPaas
014500030714     c                   kfld                    BLPmgs
014600030714      *
014700030714     c                   EndSr
