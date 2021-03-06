000100020925     h Decedit('0,') Datedit(*dmy.) option(*nodebugio)
000200050119      *------------------------------------------------------------------------*
000300100226      *  calcolo codice importanza cliente potenziale
000400100226      *------------------------------------------------------------------------*
000500050119
000600050119      *------------------------------------------------------------------------*
000700050119      *   C A M P I   D I   L A V O R O
000800050119      *------------------------------------------------------------------------*
000900100226     d TRMK52ds      e ds
001000090520     d
001100100226     d potC_al         s             11  0  inz(15000)
001200100226     d potB_al         s             11  0  inz(65000)
001300100226     d potA_al         s             11  0  inz(250000)
001400100226     d potT_al         s             11  0  inz(1292000)
001500100302
001600100302     d perc            s                    like(i52atr)
001700050119      *------------------------------------------------------------------------*
001800090520     c     *entry        plist
001900100226     c                   parm                    trmk52ds
002000080227      /free
002100100226
002200100226       if   i52tla<>'C'   ;
002300100226
002400100226       // Se fatturato presunto --> non si applica %
002500100226       if i52spt =0   ;
002600100226        o52err='MANCA SPESA TRASPORTI'   ;
002700100226       ENDIF  ;
002800100226
002900100226       if o52err=*blanks  ;
003000100226        if  i52tspt='P'   or i52atr=0   ;
003100100226          o52sptc=i52spt   ;
003200100226        else  ;
003300100226
003400100302       if i52atr<100   ;
003500100302        perc=100-i52atr   ;
003600100302       else   ;
003700100302        perc=100 ;
003800100302       endif    ;
003900100302
004000100302       eval(h)  o52sptc = (i52spt*perc)/100   ;
004100100226
004200100226       ENDIF  ;
004300100226
004400100226       // Atttribuisce codice importanza in bvase al valore calcolato
004500100226
004600100226       exsr  AttrCOD   ;
004700100226
004800100226       endif   ;
004900100226       endif   ;
005000090520
005100100226       if   i52tla=' '   ;
005200100226       Eval      *Inrt = *On         ;
005300100226       else    ;
005400100226       Eval      *InLr = *On         ;
005500100226       endif   ;
005600090518       //--------------------------------------------------------------
005700100226       BEGSR AttrCOD   ;
005800100226
005900090518       select   ;
006000100226       when o52sptc  <=PotC_al    ;
006100100226       o52cimp='C'   ;
006200100226
006300100226       when o52sptc  <=PotB_al    ;
006400100226       o52cimp='B'   ;
006500100226
006600100226       when o52sptc  <=PotA_al    ;
006700100226       o52cimp='A'   ;
006800100226
006900100226       when o52sptc  <=PotT_al    ;
007000100226       o52cimp='T'   ;
007100100226
007200100226       other  ;
007300100226       o52cimp='D'   ;
007400090518       endsl   ;
007500090518       ENDSR   ;
007600090518
007700080227
007800080227      /end-free
