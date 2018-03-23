000101081104     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200081104     F*--------------------------------------------------------------------
000300081104     F*   ROUTINE GENERALIZZATA PER CALCOLO GIORNI LAVORATIVI TRA 2 DATE
000500081104     F*--------------------------------------------------------------------
000600081104     F*   CAMPO DATADA - Deve contenere I  data formato -AAAAMMGG- o AAMMGG
000700081104     F*         DATAA  - Deve contenere II data formato -AAAAMMGG- o AAMMGG
000800081103     F*         GGL    - CONTERRA' IL NUMERO DI GIORNI LAVORATIVI
000801081120     f*                  Se numero giorni lavorativi maggiore di 99999 verrà
000802081120     f*                  restituito 99999
000803081106     F*
000804081106     F*   Per l'utilizzo di un calendario specifico passare un secondo
000805081106     F*   parametro facoltativo:
000806081106     f*         TFPTFA - Deve contenere un terminal di partenza e un termi-
000807081106     f*                  nal di arrivo con cui chainare il calendario
000808081106     f*   Se il secondo parametro non viene passato verrà letto il calenda-
000809081106     f*   rio avente tfp e tfa=0
000900081104     F*--------------------------------------------------------------------
001000990906     FAZCLN01L  IF   E           K DISK
001100081104     F*--------------------------------------------------------------------
001200920217     D*
001201081104     d ktfp            s                   like(clntfp)
001202081104     d ktfa            s                   like(clntfa)
001203081104     d kann            s                   like(clnann)
001204081104     d kmes            s                   like(clnmes)
001205081104     d ada             s              4  0
001206081104     d mda             s              2  0
001207081105     d me              s              2  0
001208081104     d gda             s              2  0
001209081104     d aa              s              4  0
001210081104     d ma              s              2  0
001211081104     d ga              s              2  0
001212081105     d gio             s              2  0
001213081105     d i               s              2  0
001214081105     d x               s              2  0
001215081104     d ggf             s              5  0
001217081104     d dataisoda       s               d   datfmt(*iso)
001218081103     d dataisoa        s               d   datfmt(*iso)
001219081103     d num_days        s             15p 0
001300920217     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)
001400081104     F*--------------------------------------------------------------------
002200940913     D WDAT8           DS
002300940913     D  DATADA                 1      8  0
002400940913     D  DATAA                  9     16  0
002500991223     D  GGL                   17     21  0
002501081106     D tfptfa          DS
002502081106     D  p_tfp                  1      3  0
002503081106     D  p_tfa                  4      6  0
003100081104     D GMEDS           DS
003200081104     D  GME                    1     31
003300081104     D                                     DIM(31)
005000920217     C*----------------------------------------------------*
005100880202     C     *ENTRY        PLIST
005200940913     C                   PARM                    WDAT8
005201081106     c                   parm                    tfptfa
005300940913     C*
005301081104     C                   clear                   ggl
005302081104
005304081106
005313081104      /free
005314081106
005320090107       if datada>0 and dataa>0;
005321090107          exsr sr_parametri;
005348081104
005349090107          // Calcola numero giorni festivi tra le due date
005350090107          exsr sr_calggf;
005364081104
005365090107          // Calcolo numero giorni totali fra le due date
005367090107          num_days=%diff(dataisoa: dataisoda: *days);
005368090107          num_days=num_days+1;
005369081106
005370090107          // Giorni lavorativi = giorni totali meno giorni festivi
005371090107          if (num_days-ggf)>99999;
005372090107             ggl=99999;
005373090107          else;
005374090107             ggl=num_days-ggf;
005375090107          endif;
005376081106
005377090107          if ggl<0;
005378090107             ggl=0;
005379090107          endif;
005380090107       endif;
005381081104
005382090107       *INRT = *ON;
005383081104
005384081106       //-----------------------------------------------------------------
005385081106       // verifiche e impostazioni parametri
005386081106       //-----------------------------------------------------------------
005387081106       begsr sr_parametri;
005388081106
005389081106       // Terminal di partenza e arrivo per chain su azcln
005390081106       if %parms>1;
005391081106         ktfp=p_tfp;
005392081106         ktfa=p_tfa;
005393081106       else;
005394081106         clear ktfp;
005395081106         clear ktfa;
005396081106       endif;
005397081106
005398081106       // data da --> sposto in campo data ed estraggo anno mese e giorno
005399081107       // se la data ricevuta non è in formato aaammgg allora do per scontato che sia
005400081107       // in formato aammgg
005401081106       test(DE) *iso datada;
005402081106       if %error;
005403081106       dataisoda=%date(datada:*ymd);
005404081106       else;
005405081106       dataisoda=%date(datada:*iso);
005406081106       endif;
005407081106       ada=%subdt(dataisoda:*years);
005408081106       mda=%subdt(dataisoda:*months);
005409081106       me=mda;
005410081106       gda=%subdt(dataisoda:*days);
005411081106
005412081106       // data a --> sposto in campo data ed estraggo anno mese e giorno
005413081107       // se la data ricevuta non è in formato aaammgg allora do per scontato che sia
005414081107       // in formato aammgg
005415081106       test(DE) *iso dataa;
005416081106       if %error;
005417081106       dataisoa=%date(dataa:*ymd);
005418081106       else;
005419081106       dataisoa=%date(dataa:*iso);
005420081106       endif;
005421081106       aa=%subdt(dataisoa:*years);
005422081106       ma=%subdt(dataisoa:*months);
005423081106       ga=%subdt(dataisoa:*days);
005424081106       endsr;
005425081105       //-----------------------------------------------------------------
005426081105       // Calcolo giorgni festivi fra le due date in base al calendario
005427081105       //-----------------------------------------------------------------
005428081105       begsr sr_calggf;
005429081106
005430081106       ggf=0;
005431081106
005432081106       DOW MDA <= MA
005433081104       OR ADA < AA;
005434081104         kmes=mda;
005435081106         kann=ada;
005436081106       // Se mi interessa febbraio verifico se anno bisestile
005437081106           if kmes=2;
005438081106              exsr sr_bisestile;
005439081106           endif;
005440081104         CHAIN (ktfp: ktfa: kann: kmes) azcln01l;
005441081104           if %found(azcln01l);
005442081104            gmeds=clnpom;
005443081104              if mda=ma
005444081104              and ada=aa;
005445081104                 gio=(ga-gda)+1;
005446081104                 x=gda;
005447081104                 exsr sr_ggf;
005448081104              else;
005449081106              IF MDA = ME;
005450081104                 gio=(XDTF(MDA)-GDA)+1;
005451081104                 x=gda;
005452081106              else;
005453081104                 gio=xdtf(mda);
005454081104                 x=1;
005455081106              endif;
005456081104              exsr sr_ggf;
005457081106              endif;
005458081106           ENDIF;
005459081104         mda=mda+1;
005460081104         gda=1;
005461081106            IF MDA = 13
005462081104            AND ADA <> AA;
005463081104               mda=1;
005464081104               ada=ada+1;
005465081106            ENDIF;
005466081106       ENDDO;
005467081104       endsr;
005468081105       //-----------------------------------------------------------------
005469081105       // Calcolo giorni festivi parziali
005470081105       //-----------------------------------------------------------------
005471081104       begsr sr_ggf;
005472081104       for i=1 to gio;
005473081104             IF GME(X) = 'F';
005474081104             ggf=ggf+1;
005475081104             ENDIF;
005476081104         x=x+1;
005477081104       ENDfor;
005478081104       endsr;
005479081106       //-----------------------------------------------------------------
005480081106       // Verifica anno bisestile e impostazione numero giorni di febbraio
005481081106       //-----------------------------------------------------------------
005482081106       begsr sr_bisestile;
005483081106             IF %rem(kann:4)=0;
005484081106             xdtf(2)=29;
005485081106             else;
005486081106             xdtf(2)=28;
005487081106             ENDIF;
005488081106       endsr;
005489081104      /end-free
016800050103**
016900050103312831303130313130313031
