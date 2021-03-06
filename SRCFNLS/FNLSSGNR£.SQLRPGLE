000100110617      *---------------------------------------------------------------*
000200150626      * Duplica FISGN per fil/clienti di FIBSP
000300110617      *---------------------------------------------------------------*
000400110617
000500110617     h decedit('0,') datedit(*dmy/) option(*nodebugio)
000700110617
000800110617      *---------------------------------------------------------------*
000900110617
001001150626     ffibsp03l  if   e           k disk
001002150626     ffisgn13l  if   e           k disk    rename(fisgn000:fisgn13)
001003150626     ffisgn14l  if   e           k disk    rename(fisgn000:fisgn14)
001004150626     Ffisgn01l  uF   E           K DISK    prefix(O_)
001005150626     Ffisgn00F  iF a E             DISK    rename(fisgn000:fisgnfis)
001009141022     D fnlv55r         PR
001010141022     D                                     extpgm('FNLV55R')
001011141022     D fnlv55ds                            likeds(fnlv55ds)
004716141023     d fnlv55ds      e ds                  inz
004718150626     d                 ds
004719150626     d bsplin
004720150626     d sbsp                           3  0 dim(30) overlay(bsplin)
004721150630      * - Schiera dei terminal di partenza delle filiali della schiera sbsp
004722150630     d sbsp_t          s              3  0 dim(30)
006100110617      * - Campi di comodo
006101150626     d wtfp            s                   like(d55tfp)
006102150626     d ktfp            s                   like(d55tfp)
006104150626     d datacur         s               d   datfmt(*iso)
006105150626     d Ix              s              4  0
006106150626     d Ii              s              4  0
006108150626     d s_datora        s                   like(sgndatora)
006109150626     d elabdatora      s                   like(sgndatora)
006110150626     d elabora         s              1
006111150626     d w_sbsp          s                   like(sgntnp)
008000130406
008001140908       //--------------------------------------------------------------
008002140908       //?Definizione procedure.
008003140908       //--------------------------------------------------------------
017200130406
017300110627     C     *ENTRY        PLIST
017400110627     C                   PARM                    chiudi            1
017401150626     c                   parm                    s_datora
017500130406
017600110617      /free
017900130625
017901150626       // Reperisco la data corrente
017902150626          datacur=(%date());
017903150626
017904150626       // IMPOSTO IL CAMPO DATORA PER L'ELABORAZIONE CORRENTE:
017905150626       //  Vuoto la prima volta
017906150626          if S_datora=*blanks;
017907150626             clear elabdatora ;
017908150626          else;
017909150626       //  dalla seconda volta in poi
017910150626       //  Uguale alla data ora della volta precedente
017911150626             eval elabdatora=s_datora;
017914150626          endif;
017915150626
017916150626       //  Salvo Datora per la prossima elaborazione
017917150626             eval S_datora=%char(%timestamp:*iso0);
017918150626
018500150626       exsr sr_Elabora;
018501141024
018700130406
018800110627       if chiudi='S';
018900140623          eval *inlr=*on;
019400140623       else;
019500140623          eval *inrt=*on;
019600110627       endif;
020100130406
029600110617       //-------------------------------------------------------------*
029700150626       //Lettura FIBSP record da elaborare  e duplica fisgn           *
029800110617       //-------------------------------------------------------------*
029900110617       Begsr SR_Elabora;
030000150626        setll 'S' fibsp03l;
030100150626        reade 'S' fibsp03l;
030200130605
030600150626  1     dow not %eof(fibsp03l) ;
030700130406
030800130502        // controllo se � stata richiesta la chiusura del sottosistema
030900110627           if %shtdn;
031000110627               chiudi='S';
031100110627               leavesr;
031200110627           endif;
031202150630        // Memorizzo in schiera terminal i terminal corrispondenti alle filiali
031203150630        // CICLO per ogni filiale che pu� spuntare
031204150630          clear  sbsp_t;
031205150630          clear Ii;
031206150630          for Ix=1 to %elem(sbsp);
031207150630             if sbsp(Ix)=0;
031208150630                leave;
031209150630             endif;
031210150630        // Determino il terminal di partenza della filiale
031211150630             w_sbsp=sbsp(Ix);
031212150630             exsr sr_terfil;
031213150630             if %lookup(wtfp:sbsp_t)=0;
031214150630                ii+=1;
031215150630                sbsp_t(ii)=wtfp;
031216150630             endif;
031217150630          endfor;
032000150630        // CICLO per ogni terminal che pu� spuntare
032001150630          for Ix=1 to %elem(sbsp_t);
032002150630             if sbsp_t(Ix)=0;
032003150630                leave;
032004150630             endif;
032005150630             ktfp=sbsp_t(Ix);
032009150626        // se bsp per lnp duplico fisgn per lnp bolla
032010150626             if bspfil>0;
032011150626                exsr sr_DupxLnp;
032012150626             else;
032013150626        // se bsp per cliente duplico fisgn per cliente
032014150626                exsr sr_DupxKsc;
032015150626             endif;
032017150626          endfor;
032032141022
032037140926
032800130406
032900150626           reade 'S'    fibsp03l ;
033000110617        enddo;
033100130406
033200110617        endsr;
033201150626       //-----------------------------------------------------------------
033202150626       // Terminal di partenza della lineda di partenza
033203150626       //-----------------------------------------------------------------
033204150626       begsr sr_terfil;
033205150626             clear wtfp;
033206150626             reset fnlv55ds;
033207150626             D55lin = w_sbsp;
033208150626             d55drf = %dec(datacur:*iso);
033209150626             d55tpt = 'P';
033210150626             callp fnlv55r(fnlv55ds);
033211150626             if d55err = ' ';
033212150626                wtfp=d55tfp;
033213150626             endif;
033214150626       endsr;
033215150626       //-------------------------------------------------------------*
033216150626       //Duplica fisgn per Linea di partenza                          *
033217150626       //-------------------------------------------------------------*
033218150626       Begsr sr_DupxLnp;
033219150626       //
033220150626       setll (ktfp:bspfil:elabdatora) fisgn13l;
033221150626       reade (ktfp:bspfil) fisgn13l;
033222150626       dow not %eof(fisgn13l);
033223150626          exsr sr_spalma;
033238150626          reade (ktfp:bspfil) fisgn13l;
033239150626       enddo;
033240150626       endsr;
033241150626       //-------------------------------------------------------------*
033242150626       //Duplica fisgn per Codice cliente                             *
033243150626       //-------------------------------------------------------------*
033244150626       Begsr sr_DupxKsc;
033245150626       setll (ktfp:bspksc:elabdatora) fisgn14l;
033246150626       reade (ktfp:bspksc) fisgn14l;
033247150626       dow not %eof(fisgn14l);
033248150626          exsr sr_spalma ;
033249150626          reade (ktfp:bspksc) fisgn14l;
033250150626       enddo;
033251150626       endsr;
033252150626       //-------------------------------------------------------------*
033253150626       // Spalma il record  fisgn per gli altri terminal previsti     *
033254150626       //-------------------------------------------------------------*
033255150626       Begsr sr_spalma ;
033256150630          for ii=1 to %elem(sbsp_t);
033257150630            if sbsp_t(ii)=0;
033258150630                  leave;
033259150630            endif;
033260150626      // escludo l'elemento che sto considerando come "da cui duplicare"
033261150630             if sbsp_t(ii)<> sbsp_t(ix);
033266150626                  exsr sr_duplica;
033268150626             endif;
033270150626          endfor;
033271150626       endsr;
033272150626       //-------------------------------------------------------------*
033273150626       //Duplica fisgn su altro terminal di partenza                  *
033274150626       //-------------------------------------------------------------*
033275150626       Begsr SR_Duplica;
033276150626
033277150626          clear elabora  ;
033278150626       // Imposto sgntnp col terminal di partenza per il quale devo duplicare
033279150908          sgntnp=sbsp_t(ii);
033280150626
033281150626          chain (sgntnp:sgnwho) fisgn01l;
033282150626          if %found(fisgn01l);
033283150626       // Record gi� presente ma annullato con "*" --> lo posso cancellare
033284150701             if o_sgnatb='*' and sgnatb<>'*';
033285150626                delete fisgn000;
033286150626                elabora='S' ;
033287150626             else;
033288150626       // Record gi� presente --> non posso duplicare
033289150626                unlock fisgn01l;
033290150626             endif;
033291150626          else ;
033292150626             elabora='S';
033293150626          endif;
033294150626
033295150626       // Scrittura FISGN se record non presente
033296150626          if elabora='S';
033297150626             clear sgnt6a;
033298150626             clear sgnt6b;
033299150626             clear sgnt6c;
033300150626             clear sgnt6d;
033301150626             clear sgnt6e;
033302150626             clear sgnt6f;
033303150626             eval      sgndatora = %char(%timestamp:*iso0);
033304150626             WRITE     fisgnfis ;
033305150626          endif;
033306150626       endsr;
