000100081104     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000200081104     F*--------------------------------------------------------------------
000300110629     F*   Aggiornamento flag inoltro per centro Storico
000400081104     F*--------------------------------------------------------------------
000500110629     Ffnarb01l  IF   E           K DISK
000600110629     Ffiarg01l  IF   E           K DISK
000700110629     f
000800110629     Ffigrg04l  IF   E           K DISK
000900110629     FFNAGB01L  UF A E           K DISK
001000081104     F*--------------------------------------------------------------------
001100110627     D GiriSt          S             10    DIM(50)
001200110627     d
001300110629     D FNLVSTDS      E DS
001400110629
001500110629     D Dvpodeco      E DS
001600110627     D KPJBA         E DS
001700110627     D tibs02ds      E DS
001800110627     D dgrgflo       E DS
001900110627     d Datasys         s               d   datfmt(*iso) inz(*sys)
002000110627     d xx              s              3  0
002100110629     d Indx            s              3  0
002200110629     d datcor          s              8  0
002300110629     d ktbo            s                   like(agbtbo)
002400110629     d kagb            s                   like(agbagb)
002500110629     d savdat          s                   like(ilvstdat)
002600110701     d savpoc          s                   like(ilvstpoc)
002700920217     D*
002800110629      *----------------------------------------------------*
002900110629      /copy gaitrasrc/srcprotopr,tibs02r
003000110629      *----------------------------------------------------*
003100880202     C     *ENTRY        PLIST
003200110627     C                   PARM                    kpjba
003300110627     C                   PARM                    fnlvstds
003400940913     C*
003500081104
003600081104      /free
003700110706
003800110706       // Tipo lancio, I95tla = "C" -           chiudo con LR
003900110706       // Tipo lancio, I95tla = "L" - elaboro e chiudo con LR
004000110706       // Tipo lancio, I95tla = " " - elaboro e chiudo con return
004100110706
0042001107061      if ilvsttla<>'C';
004300110706
0044001107062      if olvstprop=' '   ;
004500110627
004600110627       clear olvsterr  ;
004700110627       clear olvstmsg  ;
004800110627       clear olvstblop ;
004900110627       clear olvstfinag;
005000110627       clear olvstfapag;
005100110627
005200110701       // A cambio filiale consegna e data carico i giri c.storico
005300110701 3     if (savdat<> ilvstdat) or (savpoc<>ilvstpoc)  ;
005400110629          exsr Carica_giri   ;
005500110629 3     endif  ;
005600110629
005700110629
005800110629 3     if olvsterr=' '  ;
005900110629            exsr CarDati ;
006000110629 3     endif  ;
006100110629
006200110629
006300110629 3     if olvsterr=' '  ;
006400110629
006500110629       // Se la bolla ha una data spedizione < dell'attivazione
006600110629       //  non faccio nulla
006700110629 4     if  (ilvstaas*10000)+ilvstmgs < §vpocsto ;
006800110629         olvstblop='N'  ;
006900110629 x4    else   ;
007000110629       EXSr    AggioFIN  ;
007100110629 4     endif;
007200110629 3     endif;
007300110629
007400110629       // Se devo aggiornare la bolla sottometto FNAGB con nuovo
007500110629       //         codice aggiornamento
007600110629 3     if olvstfinag='S'   ;
007700110629         exsr ScriviAGB  ;
007800110629 3     endif   ;
007900110629
008000110629 2     endif;
008100110627 1     endif;
008200090113
008300110627       if ilvsttla<>' ';
008400090113          *INLR = *ON;
008500090113       else;
008600090113          return;
008700090113       endif;
008800090112
008900110629       //---------------------------------------------------------------------
009000110627       BEGSR  *inzsr    ;
009100110627       // carico data attivazione centro storico
009200110627       clear tibs02ds  ;
009300110627       clear dvpodeco  ;
009400110627       t02mod='C'   ;
009500110627       t02sif=knsif  ;
009600110629       t02cod='VPO'  ;
009700110627       t02ke1='DECO'  ;
009800110629        TNTBE_RicercaControllo  (kpjba : tibs02ds);
009900110627       if t02err=*blanks  ;
010000110627       dvpodeco=t02uni    ;
010100110627       else   ;
010200110627       §vpocsto=20391231  ;
010300110627       endif  ;
010400110627
010500110627       // verifico se la procedura  è già operativa
010600110627       datcor=%dec(datasys)  ;
010700110627
010800110627       if  datcor<§vpocsto  ;
010900110627       olvstprop='N'  ;
011000110627       endif  ;
011100110627
011200110629       ENDSR  ;
011300110629       //---------------------------------------------------------------------
011400110629       BEGSR  Carica_giri  ;
011500110629
011600110629          test(DE) *iso ilvstdat;
011700110629 1        if %error;
011800110629             eval olvsterr='1';
011900110629             eval olvstmsg='Data errata';
012000110629 x1       else;
012100110701 2        if ilvstpoc=0  ;
012200110629             eval olvsterr='2';
012300110701             eval olvstmsg='Manca la filiale di consegna' ;
012400110629 2        endif   ;
012500110629 1        endif   ;
012600110629
012700110629          // Se ok carico i giri c.Storico
012800110629 1        if olvsterr=' '   ;
012900110629
013000110629          clear xx   ;
013100110701          setll (ilvstpoc)   figrg04l  ;
013200110701          reade (ilvstpoc)   figrg04l  ;
013300110629
013400110701          // Se non trovata alcuna distinta per la filiale consegna
013500110629          //   segnalo errore
013600110629 2        if %eof(figrg04l)  ;
013700110629             eval olvsterr='3';
013800110701 2           eval olvstmsg='Non esiste nessuna distinta per la fil cons.+
013900110701                            immessa' ;
014000110629          endif  ;
014100110629
014200110629 2        dow not %eof(figrg04l)  ;
014300110629 3        if argatb= ' '  and ILvstdat>=grgdde and Ilvstdat <=grgdscad ;
014400110629            dgrgflo=grgflo                                            ;
014500110629 4          if §grgcsto='S'   ;
014600110629              xx=xx+1  ;
014700110629              giriSt(xx)=grgcgi  ;
014800110629 4          endif  ;
014900110629 3        endif  ;
015000110629
015100110701          reade (ilvstpoc)   figrg04l  ;
015200110629 2        enddo  ;
015300110629 1        endif  ;
015400110629
015500110701          savpoc=ilvstpoc  ;
015600110629          savdat=ilvstdat  ;
015700110629       ENDSR  ;
015800110629
015900110629       //---------------------------------------------------------------------
016000110629       BEGSR CarDati   ;
016100110629
016200110629          // Se mancano i dati da bolla --> li carico
0163001106291      if ilvstmgs=0 or (ilvstfin=' ' and ilvsttino<>'A') or
016400110629             (ilvstfap=' ' and ilvsttino<>' ')    ;
016500110629
016600110629       chain (ilvstaas:ilvstlnp:ilvstnrs:ilvstnsp)  fnarb01l    ;
0167001106292      if not %found(fnarb01l)  ;
016800110629             eval olvsterr='4';
016900110629             eval olvstmsg='Non trovata bolla'  ;
017000110629x2     else  ;
017100110629       ilvstmgs=arbmgs ;
0172001106293      if ilvsttino<>'A'  ;
017300110629          olvstfin=arbfin  ;
0174001106293      endif  ;
0175001106293      if ilvsttino<>' '  ;
017600110629       olvstfap=arbfap  ;
0177001106293      endif  ;
0178001106292      endif  ;
017900110629
018000110629x1     else   ;
0181001106292      if ilvsttino<>'A'  ;
018200110629          olvstfin=ilvstfin  ;
0183001106292      endif  ;
0184001106292      if ilvsttino<>' '  ;
018500110629          olvstfap=ilvstfap  ;
0186001106292      endif  ;
018700110629
0188001106291      endif  ;
018900110629
0190001106291      if  ilvstcgi=*blanks  ;
019100110629       chain (ilvstaas:ilvstlnp:ilvstnrs:ilvstnsp)  fiarg01l    ;
0192001106292      if     %found(fiarg01l)  ;
019300110629       ilvstcgi=argcgi  ;
0194001106292      endif  ;
0195001106291      endif  ;
019600110629
019700110629       ENDSR     ;
019800110629       //---------------------------------------------------------------------
019900110629       BEGSR AggioFIN  ;
020000110629
020100110629       // Se è stato tolto il giro e c'e' T o Z --> reimposto il tipo normale
020200110629 1     if    ilvstcgi=*blanks  ;
020300110629
020400110629 2     if olvstfin='T'  ;
020500110629          olvstfin='C'  ;
020600110629          olvstfinag='S'  ;
020700110629 2     endif  ;
020800110629 2     if olvstfin='Z'  ;
020900110629          olvstfin='P'  ;
021000110629          olvstfinag='S'  ;
021100110629 2     endif  ;
021200110629
021300110629       // Se c'e' giro verifico se è Centro Storico
021400110629 x1    else    ;
021500110629       Indx=%lookup(ilvstcgi:giriSt)    ;
021600110629       // il giro é centro storico  --> imposto T o Z se mancanno
021700110629
021800110629 2     if  Indx>0     ;
021900110629 3     if olvstfin='C'  ;
022000110629          olvstfin='T'  ;
022100110629          olvstfinag='S'  ;
022200110629 3     endif  ;
022300110629 3     if olvstfin='P'  ;
022400110629          olvstfin='Z'  ;
022500110629          olvstfinag='S'  ;
022600110629 3     endif  ;
022700110629
022800110629 x2    else  ;
022900110629
023000110629 3     if olvstfin='T'  ;
023100110629          olvstfin='C'  ;
023200110629          olvstfinag='S'  ;
023300110629 3     endif  ;
023400110629 3     if olvstfin='Z'  ;
023500110629          olvstfin='P'  ;
023600110629          olvstfinag='S'  ;
023700110629 3     endif  ;
023800110629
023900110629 2     endif  ;
024000110629 1     endif  ;
024100110629
024200110629       ENDSR     ;
024300110629       //---------------------------------------------------------------------
024400110629       BEGSR ScriviAGB ;
024500110629
024600110629       kagb  ='ST'  ;
024700110629
024800110629       ktbo  ='A'   ;
024900110629       exsr Ctr_AGB  ;
025000110629
025100110629       ktbo  ='P'   ;
025200110629       exsr Ctr_AGB  ;
025300110629
025400110629       ktbo  ='S'   ;
025500110629       exsr Ctr_AGB  ;
025600110629
025700110629       ENDSR     ;
025800110629       //---------------------------------------------------------------------
025900110629       BEGSR  CTR_AGB   ;
026000110629
026100110629       chain (ktbo:ilvstaas:ilvstlnp:ilvstnrs:ilvstnsp:kagb) fnagb01l    ;
026200110629       if not %found(fnagb01l)  ;
026300110629        clear fnagb000  ;
026400110629       endif  ;
026500110629
026600110629       agbtbo=ktbo ;
026700110629       agbaas =ilvstaas ;
026800110629       agblnp =ilvstlnp ;
026900110629       agbnrs =ilvstnrs ;
027000110629       agbnsp =ilvstnsp ;
027100110629       agbagb=kagb  ;
027200110629       agbfbs=olvstfin ;
027300110629       if not %found(fnagb01l)  ;
027400110629       write fnagb000 ;
027500110629       else  ;
027600110629       update fnagb000 ;
027700110629       endif  ;
027800110629
027900110629       ENDSR  ;
