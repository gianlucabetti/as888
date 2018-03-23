000100081104     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200081104     F*--------------------------------------------------------------------
000300081104     F*   ROUTINE GENERALIZZATA PER CALCOLO GIORNI LAVORATIVI TRA 2 DATE
000400081104     F*--------------------------------------------------------------------
000500081104     F*   CAMPO DATADA - Deve contenere I  data formato -AAAAMMGG- o AAMMGG
000600081104     F*         DATAA  - Deve contenere II data formato -AAAAMMGG- o AAMMGG
000700081103     F*         GGL    - CONTERRA' IL NUMERO DI GIORNI LAVORATIVI
000800081120     f*                  Se numero giorni lavorativi maggiore di 99999 verrà
000900081120     f*                  restituito 99999
001000081106     F*
001100081106     F*   Per l'utilizzo di un calendario specifico passare un secondo
001200081106     F*   parametro facoltativo:
001300081106     f*         TFPTFA - Deve contenere un terminal di partenza e un termi-
001400081106     f*                  nal di arrivo con cui chainare il calendario
001500081106     f*   Se il secondo parametro non viene passato verrà letto il calenda-
001600081106     f*   rio avente tfp e tfa=0
001700081104     F*--------------------------------------------------------------------
001800990906     FAZCLN01L  IF   E           K DISK
001900081104     F*--------------------------------------------------------------------
002000920217     D*
002100081104     d ktfp            s                   like(clntfp)
002200081104     d ktfa            s                   like(clntfa)
002300081104     d kann            s                   like(clnann)
002400081104     d kmes            s                   like(clnmes)
002500081104     d ada             s              4  0
002600081104     d mda             s              2  0
002700081105     d me              s              2  0
002800081104     d gda             s              2  0
002900081104     d aa              s              4  0
003000081104     d ma              s              2  0
003100081104     d ga              s              2  0
003200081105     d gio             s              2  0
003300081105     d i               s              2  0
003400081105     d x               s              2  0
003500081104     d ggf             s              5  0
003600081104     d dataisoda       s               d   datfmt(*iso)
003700081103     d dataisoa        s               d   datfmt(*iso)
003800111230     d data1           s               d   datfmt(*iso)
003900111230     d data2           s               d   datfmt(*iso)
004000081103     d num_days        s             15p 0
004100920217     D XDTF            S              2  0 DIM(12) CTDATA PERRCD(12)
004200081104     F*--------------------------------------------------------------------
004300940913     D WDAT8           DS
004400940913     D  DATADA                 1      8  0
004500940913     D  DATAA                  9     16  0
004600991223     D  GGL                   17     21  0
004700081106     D tfptfa          DS
004800081106     D  p_tfp                  1      3  0
004900081106     D  p_tfa                  4      6  0
005000081104     D GMEDS           DS
005100081104     D  GME                    1     31
005200081104     D                                     DIM(31)
005300920217     C*----------------------------------------------------*
005400880202     C     *ENTRY        PLIST
005500940913     C                   PARM                    WDAT8
005600081106     c                   parm                    tfptfa
005700940913     C*
005800081104     C                   clear                   ggl
005900081104
006000081106
006100081104      /free
006200081106
006300090107       if datada>0 and dataa>0;
006400090107          exsr sr_parametri;
006500081104
006600090107          // Calcola numero giorni festivi tra le due date
006700090107          exsr sr_calggf;
006800081104
006900090107          // Calcolo numero giorni totali fra le due date
007000090107          num_days=%diff(dataisoa: dataisoda: *days);
007100090107          num_days=num_days+1;
007200081106
007300090107          // Giorni lavorativi = giorni totali meno giorni festivi
007400090130          if %abs((num_days-ggf))>99999;
007500090107             ggl=99999;
007600090130             if num_days<0;
007700090130                ggl=0;
007800090130             endif;
007900090107          else;
008000090107             ggl=num_days-ggf;
008100090107          endif;
008200081106
008300090107          if ggl<0;
008400090107             ggl=0;
008500090107          endif;
008600090107       endif;
008700081104
008800090107       *INRT = *ON;
008900081104
009000081106       //-----------------------------------------------------------------
009100081106       // verifiche e impostazioni parametri
009200081106       //-----------------------------------------------------------------
009300081106       begsr sr_parametri;
009400081106
009500081106       // Terminal di partenza e arrivo per chain su azcln
009600081106       if %parms>1;
009700081106         ktfp=p_tfp;
009800081106         ktfa=p_tfa;
009900081106       else;
010000081106         clear ktfp;
010100081106         clear ktfa;
010200081106       endif;
010300111230
010400111230       test(DE) *iso datada;
010500111230       if %error;
010600111230       data1    =%date(datada:*ymd);
010700111230       else;
010800111230       data1    =%date(datada:*iso);
010900111230       endif;
011000111230
011100111230       test(DE) *iso dataa;
011200111230       if %error;
011300111230       data2    =%date(dataa:*ymd);
011400111230       else;
011500111230       data2    =%date(dataa:*iso);
011600111230       endif;
011700081106
011800111230       // DATA DA --> data più bassa
011900111230       if data1 < data2  ;
012000111230       dataisoda= data1             ;
012100111230       dataisoa = data2   ;
012200111230       else  ;
012300111230       dataisoda= data2             ;
012400111230       dataisoa = data1   ;
012500111230       endif  ;
012600111230
012700081106       // data da --> sposto in campo data ed estraggo anno mese e giorno
012800081107       // se la data ricevuta non è in formato aaammgg allora do per scontato che sia
012900081107       // in formato aammgg
013000081106       ada=%subdt(dataisoda:*years);
013100081106       mda=%subdt(dataisoda:*months);
013200081106       me=mda;
013300081106       gda=%subdt(dataisoda:*days);
013400081106
013500111230       // DATA DA --> data più alta
013600081106       // data a --> sposto in campo data ed estraggo anno mese e giorno
013700081107       // se la data ricevuta non è in formato aaammgg allora do per scontato che sia
013800081107       // in formato aammgg
013900081106       aa=%subdt(dataisoa:*years);
014000081106       ma=%subdt(dataisoa:*months);
014100081106       ga=%subdt(dataisoa:*days);
014200111230
014300081106       endsr;
014400081105       //-----------------------------------------------------------------
014500081105       // Calcolo giorgni festivi fra le due date in base al calendario
014600081105       //-----------------------------------------------------------------
014700081105       begsr sr_calggf;
014800081106
014900081106       ggf=0;
015000081106
015100081106       DOW MDA <= MA
015200081104       OR ADA < AA;
015300081104         kmes=mda;
015400081106         kann=ada;
015500081106       // Se mi interessa febbraio verifico se anno bisestile
015600081106           if kmes=2;
015700081106              exsr sr_bisestile;
015800081106           endif;
015900081104         CHAIN (ktfp: ktfa: kann: kmes) azcln01l;
016000081104           if %found(azcln01l);
016100081104            gmeds=clnpom;
016200081104              if mda=ma
016300081104              and ada=aa;
016400081104                 gio=(ga-gda)+1;
016500081104                 x=gda;
016600081104                 exsr sr_ggf;
016700081104              else;
016800081106              IF MDA = ME;
016900081104                 gio=(XDTF(MDA)-GDA)+1;
017000081104                 x=gda;
017100081106              else;
017200081104                 gio=xdtf(mda);
017300081104                 x=1;
017400081106              endif;
017500081104              exsr sr_ggf;
017600081106              endif;
017700081106           ENDIF;
017800081104         mda=mda+1;
017900081104         gda=1;
018000081106            IF MDA = 13
018100081104            AND ADA <> AA;
018200081104               mda=1;
018300081104               ada=ada+1;
018400081106            ENDIF;
018500081106       ENDDO;
018600081104       endsr;
018700081105       //-----------------------------------------------------------------
018800081105       // Calcolo giorni festivi parziali
018900081105       //-----------------------------------------------------------------
019000081104       begsr sr_ggf;
019100081104       for i=1 to gio;
019200081104             IF GME(X) = 'F';
019300081104             ggf=ggf+1;
019400081104             ENDIF;
019500081104         x=x+1;
019600081104       ENDfor;
019700081104       endsr;
019800081106       //-----------------------------------------------------------------
019900081106       // Verifica anno bisestile e impostazione numero giorni di febbraio
020000081106       //-----------------------------------------------------------------
020100081106       begsr sr_bisestile;
020200081106             IF %rem(kann:4)=0;
020300081106             xdtf(2)=29;
020400081106             else;
020500081106             xdtf(2)=28;
020600081106             ENDIF;
020700081106       endsr;
020800081104      /end-free
020900050103**
021000050103312831303130313130313031
