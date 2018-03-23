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
005371090130          if %abs((num_days-ggf))>99999;
005372090107             ggl=99999;
005373090130             if num_days<0;
005374090130                ggl=0;
005375090130             endif;
005376090107          else;
005377090107             ggl=num_days-ggf;
005378090107          endif;
005379081106
005380090107          if ggl<0;
005381090107             ggl=0;
005382090107          endif;
005383090107       endif;
005384081104
005385090107       *INRT = *ON;
005386081104
005387081106       //-----------------------------------------------------------------
005388081106       // verifiche e impostazioni parametri
005389081106       //-----------------------------------------------------------------
005390081106       begsr sr_parametri;
005391081106
005392081106       // Terminal di partenza e arrivo per chain su azcln
005393081106       if %parms>1;
005394081106         ktfp=p_tfp;
005395081106         ktfa=p_tfa;
005396081106       else;
005397081106         clear ktfp;
005398081106         clear ktfa;
005399081106       endif;
005400081106
005401081106       // data da --> sposto in campo data ed estraggo anno mese e giorno
005402081107       // se la data ricevuta non è in formato aaammgg allora do per scontato che sia
005403081107       // in formato aammgg
005404081106       test(DE) *iso datada;
005405081106       if %error;
005406081106       dataisoda=%date(datada:*ymd);
005407081106       else;
005408081106       dataisoda=%date(datada:*iso);
005409081106       endif;
005410081106       ada=%subdt(dataisoda:*years);
005411081106       mda=%subdt(dataisoda:*months);
005412081106       me=mda;
005413081106       gda=%subdt(dataisoda:*days);
005414081106
005415081106       // data a --> sposto in campo data ed estraggo anno mese e giorno
005416081107       // se la data ricevuta non è in formato aaammgg allora do per scontato che sia
005417081107       // in formato aammgg
005418081106       test(DE) *iso dataa;
005419081106       if %error;
005420081106       dataisoa=%date(dataa:*ymd);
005421081106       else;
005422081106       dataisoa=%date(dataa:*iso);
005423081106       endif;
005424081106       aa=%subdt(dataisoa:*years);
005425081106       ma=%subdt(dataisoa:*months);
005426081106       ga=%subdt(dataisoa:*days);
005427081106       endsr;
005428081105       //-----------------------------------------------------------------
005429081105       // Calcolo giorgni festivi fra le due date in base al calendario
005430081105       //-----------------------------------------------------------------
005431081105       begsr sr_calggf;
005432081106
005433081106       ggf=0;
005434081106
005435081106       DOW MDA <= MA
005436081104       OR ADA < AA;
005437081104         kmes=mda;
005438081106         kann=ada;
005439081106       // Se mi interessa febbraio verifico se anno bisestile
005440081106           if kmes=2;
005441081106              exsr sr_bisestile;
005442081106           endif;
005443081104         CHAIN (ktfp: ktfa: kann: kmes) azcln01l;
005444081104           if %found(azcln01l);
005445081104            gmeds=clnpom;
005446081104              if mda=ma
005447081104              and ada=aa;
005448081104                 gio=(ga-gda)+1;
005449081104                 x=gda;
005450081104                 exsr sr_ggf;
005451081104              else;
005452081106              IF MDA = ME;
005453081104                 gio=(XDTF(MDA)-GDA)+1;
005454081104                 x=gda;
005455081106              else;
005456081104                 gio=xdtf(mda);
005457081104                 x=1;
005458081106              endif;
005459081104              exsr sr_ggf;
005460081106              endif;
005461081106           ENDIF;
005462081104         mda=mda+1;
005463081104         gda=1;
005464081106            IF MDA = 13
005465081104            AND ADA <> AA;
005466081104               mda=1;
005467081104               ada=ada+1;
005468081106            ENDIF;
005469081106       ENDDO;
005470081104       endsr;
005471081105       //-----------------------------------------------------------------
005472081105       // Calcolo giorni festivi parziali
005473081105       //-----------------------------------------------------------------
005474081104       begsr sr_ggf;
005475081104       for i=1 to gio;
005476081104             IF GME(X) = 'F';
005477081104             ggf=ggf+1;
005478081104             ENDIF;
005479081104         x=x+1;
005480081104       ENDfor;
005481081104       endsr;
005482081106       //-----------------------------------------------------------------
005483081106       // Verifica anno bisestile e impostazione numero giorni di febbraio
005484081106       //-----------------------------------------------------------------
005485081106       begsr sr_bisestile;
005486081106             IF %rem(kann:4)=0;
005487081106             xdtf(2)=29;
005488081106             else;
005489081106             xdtf(2)=28;
005490081106             ENDIF;
005491081106       endsr;
005492081104      /end-free
016800050103**
016900050103312831303130313130313031
