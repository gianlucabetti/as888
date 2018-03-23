000100011029     H DECEDIT('0,') DATEDIT(*DMY.)
000200120614     H* TRULFLBR *---------------------------------------------------*
000300120614     H*   - Reperimento linea di arrivo forzata per cliente mittente
000400060704     H*--------------------------------------------------------------*
000600100514     ftntbe01l  if   e           k disk
000800120614     D MSG             S             78    DIM(2) CTDATA PERRCD(1)
000900011019
001300120614     d trulflbDS     e ds
001600011030
001601120614     d tbeuni
001602120614     D  Scm                    1    252    dim(36)
001603120614     d
002200100514     d KE1             s                   like(tbeke1)
003000011019
003600060704      *----------------------------------------------------------------
003700011029     c     *entry        plist
003800120614     c                   parm                    trulflbds
003900011106
004000100514      /free
004100011019
004200120614       clear      OflbERR    ;
004300120614       clear      OflbMSG    ;
004301120614       clear      Oflblfz    ;
004700011019
004800120614 1     if IflbTLa<>'C'    ;
004900100514
005000100514 2     select   ;
005100120614       when  IflbCCM=*blanks;
005200100514
005300120614       OflbMSG= msg(1) ;
005400120614       OflbERR= 'E'   ;
005500100514
005600120614       when  IflbLNA=*blanks;
005700100514
005800120614       OflbMSG= msg(2) ;
005900120614       OflbERR= 'S'   ;
006000100514 x2    other          ;
006100100514
006200120614       exsr Recupera  ;
006300100514 2     endsl          ;
006400100514 1     endif ;
006500100514
006600120614 1     if        IflbTLA <> *blanks     ;
006700100514        *inlr = *on           ;
006800100514       else  ;
006900100514        *inrt = *on           ;
007000100514 1     endif ;
007100100514
007200100514       //-------------------------------------------------------
007300120614       //   Recupera linea di arrivo forzata se presente
007400100514       //-------------------------------------------------------
010200120614       BEGSR  Recupera      ;
010300100514
010400120614       ke1 = IflbCCM                    ;
010800100514
010900120614       setll  ('FLB': ke1) tntbe01l   ;
011000120614       reade  ('FLB': ke1) tntbe01l   ;
011100100514
011200120614 1     dow not %eof(tntbe01l) and OflbLFZ=*zeros  ;
011201120614
011300120614 2      if tbeatb=' '   ;
011301120614
011400120614          if %lookup(IflbCCM:Scm)>0 or %lookup('9999999':Scm)>0;
011402120614             OflbLFZ=%dec(%subst(tbeke2:1:3):3:0)            ;
011403120614          endif                                              ;
011600100514
013900120614 2      endif   ;
014000100514
014100120614       reade  ('FLB': ke1) tntbe01l   ;
014200100514 1     enddo   ;
016600120614       // Se non trovata forzatura linea imposto linea forzata=linea passata
016601120614       if OflbLFZ=*zeros;
016602120614           OflbLFZ=%dec(Iflblna:3:0);
016603120614       endif ;
016700100514       ENDSR  ;
024200060704**         MSG
024300120614Codice Cliente mancante                                                       01
024400120614Linea di arrivo mancante                                                      02
