000100020409     H DECEDIT('0,') DATEDIT(*DMY.) option(*nodebugio)
000200020409      *------------------------------------------------------------------------*
000300020409      * pulisce tariffa particolare 'E' per tariffe estere o dpd               *
000400020409      *------------------------------------------------------------------------*
000500020409     ftnofm01l  ip   e           k disk
000600020411     ftiopt01l  uf   e           k disk
000700020409     ftiopd01l  uf   e           k disk
000800020411     fprtf198   o    f  198        printer oflind(*inof) usropn
000900020409
001000020409     d kftc            s                   like(tpdftc) inz('E')
001100020411
001200020411     d cmd             s              1    dim(122) ctdata perrcd(62)
001300020411
001400020411     d lung            s             15  5
001500020411     d comman          s            110
001600020409
001700020409     d dsta01        e ds
001800020409
001900020409     c                   movel     tamflo        dsta01
002000020409
002100020411      * tariffa estera o DPD
002200020409     c                   if        tamfie = 'E' or §tadpd = 'S'
002300020411
002400020411      * File tiopt
002500020411      *------------
002600020411     c     ktpd          chain     tiopt01l
002700020411     c                   if        %found(tiopt01l)
002800020411     c                   delete    titpt000
002900020411     c                   endif
003000020411
003100020411      * File tiopd
003200020411      *------------
003300020409     c     ktpd          setll     tiopd01l
003400020409     c                   do        *hival
003500020409     c     ktpd          reade     tiopd01l
003600020411      * fine file
003700020409     c                   if        %eof(tiopd01l)
003800020409     c                   leave
003900020409     c                   endif
004000020411
004100020409     c                   except    stampa
004200020411     c                   delete    titpd000
004300020411
004400020409     c                   enddo
004500020409     c                   endif
004600020411
004700020411      *---------------------------------------------------------------*
004800020411      *   ROUTINE INIZIALE
004900020411      *---------------------------------------------------------------*
005000020411     c     *inzsr        begsr
005100020411
005200020411      * Ovrprtf
005300020411     c                   z-add     103           lung
005400020411     c                   clear                   comman
005500020411     c                   movea     cmd(1)        comman
005600020411     c                   call      'QCMDEXC'
005700020411     c                   parm                    comman
005800020411     c                   parm                    lung
005900020411     c                   open      prtf198
006000020411
006100020411     c     ktpd          klist
006200020411     c                   kfld                    tamksc
006300020411     c                   kfld                    tamctr
006400020411     c                   kfld                    tamprg
006500020411     c                   kfld                    kftc
006600020411
006700020411     c                   endsr
006800020411
006900020409     oprtf198   e            stampa      1
007000020409     o                       tpdksc            +  2
007100020409     o                       tpdctr            +  2
007200020409     o                       tpdprg        2   +  2
007300020409     o                       tamfie            +  2
007400020409     o                       §tadpd            +  2
007500020409     o                       tpdftc            +  2
007600020409     o                       tpdsgl        2   +  2
007700020409     o                       tpditr        2   +  2
007800020411** cmd
007900020411OVRPRTF FILE(PRTF198) OUTQ(QPRINTS)
