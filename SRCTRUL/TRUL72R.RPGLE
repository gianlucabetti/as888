000100020412     H DECEDIT('0,') DATEDIT(*ymd.) option(*nodebugio)
000200020412
000300040922      * TRUL72R  *---------------------------------------------------*
000400040922      * - CARICAMENTO P.O. di contabilità
000500020412      *--------------------------------------------------------------*
000600020412
000800040922     fazorg01l  if   e           k disk
000900020412
002100040922     d trul72ds      e ds
002200041013     d  lin                    8    187s 0 dim(60)
002400020412     d kpjba         e ds
002401040922     D I               S              3  0
002402060529     d og143         e ds
002500020412
002600020412      *---------------------------------------------------------------
002700020412     c     *entry        plist
002800020412     c                   parm                    kpjba
002900040922     c                   movel     kpjbu         trul72ds
003800020412
003900040922      * I72tla = ' '  Elaboro e chiudo con RETURN
004000040922      * I72tla = 'L'  Elaboro e chiudo con LR
004100040922      * I72tla = 'C'            chiudo con LR  solo per chiusura file
004200020412
004300040922    1c                   if        I72tla = 'C'
004400020412     c                   eval      *inlr = *on
004500020412
004600020412   X1c                   else
007700020412
007701040922     c                   exsr      srsim
007800041013     c                   movel(p)  trul72ds      kpjbu
007900020412
008000040922    2c                   if        I72tla = 'L'
008100020412     c                   eval      *inlr = *on
008200020412   X2c                   else
008300020412     c                   return
008400020412    2c                   endif
008500020412
008600020412     c                   endif
008700020412
009000020412      *---------------------------------------------------------------*
009100040922     c     srsim         begsr
009200020412
009302040922     c                   clear                   o72sim
009303040922     c                   clear                   o72err
009304040922     c     i72pou        chain     azorg01l
009305040922     c                   if        %found(azorg01l)
009306040922     c                   movel     orgfc0        o72sim
009307040922     c                   exsr      srpo
009308040922     c                   else
009309040922     c                   eval      o72err='1'
009310040922     c                   end
009311040922     c                   endsr
032500020412
032501040922      *---------------------------------------------------------------*
032502040922     c     srpo          begsr
032503040922     c                   clear                   i
032504040922     c                   clear                   lin
032505040922     c     *loval        setll     azorg01l
032506040922     c                   do        *hival
032507040922     c                   read      azorg01l
032508040922     c                   if        %eof(azorg01l)
032509040922     c                   leave
032510040922     c                   end
032512040922     c* solo po validi e con po contabilità = a quello del po richiedente
032513040922IF  3C                   if        orgfva <> *BLANKS or orgfag = 'V'
032514040922IF  3C                             or orgfc0 <> o72sim
032515040922     c                   iter
032516040922     c                   end
032517060529     c* solo po ESTERI o ITALIA
032518060529     C                   MOVEL     ORGDE3        OG143
032519060529IF  3C                   if        i72est = 'E' and
032520060529     c                             (§ogNTW<>'FED' and §ogNTW<>'EEX' and
032521060529     c                              §ogNTW<>'EUP' and §ogNTW<>'DPD')
032523060529     c                   iter
032524060529     c                   end
032525060529     c                   If        i72est = 'I' and
032526060529     c                             (§ogNTW = 'FED' or §ogNTW = 'EEX' or
032527060529     c                              §ogNTW = 'EUP' or §ogNTW = 'DPD')
032528060529     c                   iter
032529060529     c                   end
032530060529     c*
032531040922     c                   add       1             i
032532040922     c                   eval      lin(i)= orgfil
032533040922     c                   enddo
032534040922
032600020412     c                   endsr
