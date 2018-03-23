000100020409     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020409      *------------------------------------------------------------------------*
000300020409      * pulisce tariffa particolare 'E' per tariffe estere o dpd               *
000400020409      *------------------------------------------------------------------------*
000500020409     ftntam01l  ip   e           k disk
000600020411     ftitpt01l  uf   e           k disk
000700020409     ftitpd01l  uf   e           k disk
000800020411     fprtf198   o    f  198        printer oflind(*inof) usropn
000900020409
001000020409     d kftc            s                   like(tpdftc) inz('E')
001100020411     d wpo             s              3  0
001200020411
001300020411     d cmd             s              1    dim(122) ctdata perrcd(62)
001400020411     d l1              s              3  0 dim(30)
001500020411
001600020411     d lung            s             15  5
001700020411     d comman          s            110
001800020409
001900020409     d dsta01        e ds
002000020411     d trul06ds      e ds
002100020411     d  lin                    1     90  0 dim(30)
002200020411     d cncr80        e ds
002300020411     d ut§dse0f      e ds
002400020411     d kpjba         e ds
002500020409
002600020409     c                   movel     tamflo        dsta01
002700020409
002800020411      * tariffa estera o DPD
002900020409     c                   if        tamfie = 'E' or §tadpd = 'S'
003000020411
003100020411      * File titpt
003200020411      *------------
003300020411     c     ktpd          chain     titpt01l
003400020411     c                   if        %found(titpt01l) and tptatb = *blanks
003500020411     c                   eval      tptatb = 'A'
003600020411     c                   movel     tptksc        wpo
003700020411
003800020411      * controllo se fa parte dell'area di partenza
003900020411      * sflaggo per la trasmissione
004000020411     c     wpo           lookup    l1                                     99
004100020411     c                   if        *in99
004200020411     c                   clear                   tptftr
004300020411     c                   endif
004400020411      * aggiorno
004500020411     c                   update    titpt000
004600020411     c                   endif
004700020411
004800020411      * File titpd
004900020411      *------------
005000020409     c     ktpd          setll     titpd01l
005100020409     c                   do        *hival
005200020409     c     ktpd          reade     titpd01l
005300020411      * fine file
005400020409     c                   if        %eof(titpd01l)
005500020409     c                   leave
005600020409     c                   endif
005700020411      * record annullato
005800020411     c                   if        tpdatb <> ' '
005900020411     c                   iter
006000020411     c                   endif
006100020411
006200020409     c                   eval      tpdatb = 'A'
006300020411     c                   movel     tpdksc        wpo
006400020411
006500020411      * controllo se fa parte dell'area di partenza
006600020411      * sflaggo per la trasmissione
006700020411     c     wpo           lookup    l1                                     99
006800020411     c                   if        *in99
006900020411     c                   clear                   tpdftr
007000020409     c                   except    stampa
007100020411     c                   endif
007200020411      * aggiorno
007300020409     c                   update    titpd000
007400020411
007500020409     c                   enddo
007600020409     c                   endif
007700020411
007800020411      *---------------------------------------------------------------*
007900020411      *   ROUTINE INIZIALE
008000020411      *---------------------------------------------------------------*
008100020411     c     *inzsr        begsr
008200020411
008300020411     c                   z-add     1             codut
008400020411     c                   call      'X§PARUT'
008500020411     c                   parm                    ut§dse0f
008600020411
008700020411     c                   movel     rec80         cncr80
008800020411
008900020411      *  Carico schiera P.O. gestiti
009000020411     c                   clear                   trul06ds
009100020411     c                   eval      d06tla = 'L'
009200020411     c                   move      '£1'          d06cod
009300020411     c                   movel     simfel        d06key
009400020411     c                   movel(p)  trul06ds      kpjbu
009500020411     c                   call      'TRUL06R'
009600020411     c                   parm                    kpjba
009700020411     c                   movel(p)  kpjbu         trul06ds
009800020411     c                   movea     lin           l1
009900020411
010000020411      * Ovrprtf
010100020411     c                   z-add     103           lung
010200020411     c                   clear                   comman
010300020411     c                   movea     cmd(1)        comman
010400020411     c                   call      'QCMDEXC'
010500020411     c                   parm                    comman
010600020411     c                   parm                    lung
010700020411     c                   open      prtf198
010800020411
010900020411     c     ktpd          klist
011000020411     c                   kfld                    tamksc
011100020411     c                   kfld                    tamctr
011200020411     c                   kfld                    tamprg
011300020411     c                   kfld                    kftc
011400020411
011500020411     c                   endsr
011600020411
011700020409     oprtf198   e            stampa      1
011800020409     o                       tpdksc            +  2
011900020409     o                       tpdctr            +  2
012000020409     o                       tpdprg        2   +  2
012100020409     o                       tamfie            +  2
012200020409     o                       §tadpd            +  2
012300020409     o                       tpdftc            +  2
012400020409     o                       tpdsgl        2   +  2
012500020409     o                       tpditr        2   +  2
012600020411** cmd
012700020411OVRPRTF FILE(PRTF198) OUTQ(QPRINTS)
