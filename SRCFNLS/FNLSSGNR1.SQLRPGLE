000100110617      *---------------------------------------------------------------*
000200150701      * Aggiorna FISGN duplicati su altri Terminal di Partenza
000300110617      *---------------------------------------------------------------*
000400110617
000500110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700110617
000800110617      *---------------------------------------------------------------*
000900110617
001004150630     Ffisgn01l  iF   E           K DISK    prefix(O_)
001005150701     ffisgn05l  if   e           k disk    rename(fisgn000:fisgn05)
001006150701     f                                     infds(fisgninf)
001007150701     Ffisgn00F  uF   E             DISK    rename(fisgn000:fisgnfis)
001008150701
001009150701     D fnlssgnds     e ds
001010150701     D fisgninf        ds
001011150701     D  sgnnrr               397    400i 0
006100110617      * - Campi di comodo
008000130406
008001140908       //--------------------------------------------------------------
008002140908       //?Definizione procedure.
008003140908       //--------------------------------------------------------------
017200130406
017300110627     C     *ENTRY        PLIST
017400150701     C                   PARM                    fnlssgnds
017401150701     C*---------------------------------------------------------------*
017402150701     C* TIPO LANCIO, isgnTLA "C" -           CHIUSO CON LR
017403150701     C* TIPO LANCIO, isgnTLA "L" - ELABORO E CHIUDO CON LR
017404150701     C* TIPO LANCIO, isgnTLA " " - ELABORO E CHIUDO IN RETRN
017500130406
017600110617      /free
017601150701       if isgntla <>'C';
017602150701          chain (isgntnp:isgnwho) fisgn01l;
017603150630          setll (o_sgnaas:o_sgnlnp:o_sgnnrs:o_sgnnsp) fisgn05l ;
017604150630          reade (o_sgnaas:o_sgnlnp:o_sgnnrs:o_sgnnsp) fisgn05l ;
017605150630          dow not %eof(fisgn05l);
017606150701             if sgntnp<>O_sgntnp and sgnwho=o_sgnwho and sgnatb<>'A';
017608150701                 chain sgnnrr fisgn00f;
017609150701                 if %found(fisgn00f);
017610150701                         sgnatb=o_sgnatb;
017611150701                         sgnksc=o_sgnksc;
017612150701                         sgncdp=o_sgncdp;
017613150701                         sgnncd=o_sgnncd;
017614150701                         sgnlna=o_sgnlna;
017615150701                         sgndla=o_sgndla;
017616150701                         sgntna=o_sgntna;
017617150701                         sgntsp=o_sgntsp;
017618150701                         sgnzsc=o_sgnzsc;
017619150701                         sgnnc1=o_sgnnc1;
017620150701                         sgnnc2=o_sgnnc2;
017621150701                         sgnrsm=o_sgnrsm;
017622150701                         sgnprm=o_sgnprm;
017623150701                         sgnrsd=o_sgnrsd;
017624150701                         sgncad=o_sgncad;
017625150701                         sgnind=o_sgnind;
017626150701                         sgnlod=o_sgnlod;
017627150701                         sgnprd=o_sgnprd;
017628150701                         sgncpd=o_sgncpd;
017629150701                         sgnvol=o_sgnvol;
017630150701                         sgnpkg=o_sgnpkg;
017631150701                         sgnfvr=o_sgnfvr;
017632150701                         sgntst=o_sgntst;
017633150701                         sgnfca=o_sgnfca;
017634150701                         clear sgnt6a;
017635150701                         clear sgnt6b;
017636150701                         clear sgnt6c;
017637150701                         clear sgnt6d;
017638150701                         clear sgnt6e;
017639150701                         clear sgnt6f;
017641150701                         sgndatora = %char(%timestamp:*iso0);
017642150701                    update  fisgnfis;
017643150701                 endif;
017644150630             endif;
017645150630             reade (o_sgnaas:o_sgnlnp:o_sgnnrs:o_sgnnsp) fisgn05l ;
017646150630          enddo;
017900130625
017901150630       endif;
017903150626
017904150701       if isgntla =' ';
017905150630          eval *inrt=*on;
017907150630       else;
017908150630          eval *inlr=*on;
017909150630       endif;
020100130406
