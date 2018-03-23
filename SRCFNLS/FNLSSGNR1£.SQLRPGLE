000100110617      *---------------------------------------------------------------*
000200150630      * Aggiorna FISGN duplicati su altri TNP
000300110617      *---------------------------------------------------------------*
000400110617
000500110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700110617
000800110617      *---------------------------------------------------------------*
000900110617
001002150630     ffisgn05l  uf   e           k disk    rename(fisgn000:fisgn05)
001004150630     Ffisgn01l  iF   E           K DISK    prefix(O_)
006100110617      * - Campi di comodo
008000130406
008001140908       //--------------------------------------------------------------
008002140908       //?Definizione procedure.
008003140908       //--------------------------------------------------------------
017200130406
017300110627     C     *ENTRY        PLIST
017400150630     C                   PARM                    fnlssgnds1
017500130406
017600110617      /free
017601150630       if lssgntla <>'C';
017602150630          chain (lssgntnp:lssgnwho) fisgn01l;
017603150630          setll (o_sgnaas:o_sgnlnp:o_sgnnrs:o_sgnnsp) fisgn05l ;
017604150630          reade (o_sgnaas:o_sgnlnp:o_sgnnrs:o_sgnnsp) fisgn05l ;
017605150630          dow not %eof(fisgn05l);
017606150630             if sgntnp<>O_sgntnp and sgnwho=o_sgnwho;
017607150630                 update fisgn005;
017608150630             else;
017609150630                 unlock fisgn05l ;
017610150630             endif;
017611150630             reade (o_sgnaas:o_sgnlnp:o_sgnnrs:o_sgnnsp) fisgn05l ;
017612150630          enddo;
017900130625
017901150630       endif;
017903150626
017904150630       if lssgntla =' ';
017905150630          eval *inrt=*on;
017907150630       else;
017908150630          eval *inlr=*on;
017909150630       endif;
020100130406
