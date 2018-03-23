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
001100110930     D GiriSt          S             10    DIM(499)
001200110627     d
001300110629     D FNLVSTDS      E DS
001400110629
001500110930     d* DS x controllo riempimento schiere
001600110930     d trul0sds      e ds
001700110629     D Dvpodeco      E DS
001800110627     D KPJBA         E DS
001900110627     D tibs02ds      E DS
002000110627     D dgrgflo       E DS
002100110627     d Datasys         s               d   datfmt(*iso) inz(*sys)
002200110627     d xx              s              3  0
002300110629     d Indx            s              3  0
002400110629     d datcor          s              8  0
002500110629     d ktbo            s                   like(agbtbo)
002600110629     d kagb            s                   like(agbagb)
002700110629     d savdat          s                   like(ilvstdat)
002800110701     d savpoc          s                   like(ilvstpoc)
002900920217     D*
003000110629      *----------------------------------------------------*
003100110629      /copy gaitrasrc/srcprotopr,tibs02r
003200110629      *----------------------------------------------------*
003300880202     C     *ENTRY        PLIST
003400110627     C                   PARM                    kpjba
003500110627     C                   PARM                    fnlvstds
003600940913     C*
003700081104
003800081104      /free
003900110706
004000110706       // Tipo lancio, I95tla = "C" -           chiudo con LR
004100110706       // Tipo lancio, I95tla = "L" - elaboro e chiudo con LR
004200110706       // Tipo lancio, I95tla = " " - elaboro e chiudo con return
004300110706
0044001107061      if ilvsttla<>'C';
004500110706
0046001107062      if olvstprop=' '   ;
004700110627
004800110627       clear olvsterr  ;
004900110627       clear olvstmsg  ;
005000110627       clear olvstblop ;
005100110627       clear olvstfinag;
005200110627       clear olvstfapag;
005300110627
005400110701       // A cambio filiale consegna e data carico i giri c.storico
005500110701 3     if (savdat<> ilvstdat) or (savpoc<>ilvstpoc)  ;
005600110629          exsr Carica_giri   ;
005700110629 3     endif  ;
005800110629
005900110629
006000110629 3     if olvsterr=' '  ;
006100110629            exsr CarDati ;
006200110629 3     endif  ;
006300110629
006400110629
006500110629 3     if olvsterr=' '  ;
006600110629
006700110629       // Se la bolla ha una data spedizione < dell'attivazione
006800110629       //  non faccio nulla
006900110629 4     if  (ilvstaas*10000)+ilvstmgs < §vpocsto ;
007000110629         olvstblop='N'  ;
007100110629 x4    else   ;
007200110629       EXSr    AggioFIN  ;
007300110629 4     endif;
007400110629 3     endif;
007500110629
007600110629       // Se devo aggiornare la bolla sottometto FNAGB con nuovo
007700110629       //         codice aggiornamento
007800110629 3     if olvstfinag='S'   ;
007900110629         exsr ScriviAGB  ;
008000110629 3     endif   ;
008100110629
008200110629 2     endif;
008300110627 1     endif;
008400090113
008500110627       if ilvsttla<>' ';
008600090113          *INLR = *ON;
008700090113       else;
008800090113          return;
008900090113       endif;
009000090112
009100110629       //---------------------------------------------------------------------
009200110627       BEGSR  *inzsr    ;
009300110627       // carico data attivazione centro storico
009400110627       clear tibs02ds  ;
009500110627       clear dvpodeco  ;
009600110627       t02mod='C'   ;
009700110627       t02sif=knsif  ;
009800110629       t02cod='VPO'  ;
009900110627       t02ke1='DECO'  ;
010000110629        TNTBE_RicercaControllo  (kpjba : tibs02ds);
010100110627       if t02err=*blanks  ;
010200110627       dvpodeco=t02uni    ;
010300110627       else   ;
010400110627       §vpocsto=20391231  ;
010500110627       endif  ;
010600110627
010700110627       // verifico se la procedura  è già operativa
010800110627       datcor=%dec(datasys)  ;
010900110627
011000110627       if  datcor<§vpocsto  ;
011100110627       olvstprop='N'  ;
011200110627       endif  ;
011300110627
011400110629       ENDSR  ;
011500110629       //---------------------------------------------------------------------
011600110629       BEGSR  Carica_giri  ;
011700110629
011800110629          test(DE) *iso ilvstdat;
011900110629 1        if %error;
012000110629             eval olvsterr='1';
012100110629             eval olvstmsg='Data errata';
012200110629 x1       else;
012300110701 2        if ilvstpoc=0  ;
012400110629             eval olvsterr='2';
012500110701             eval olvstmsg='Manca la filiale di consegna' ;
012600110629 2        endif   ;
012700110629 1        endif   ;
012800110629
012900110629          // Se ok carico i giri c.Storico
013000110629 1        if olvsterr=' '   ;
013100110629
013200110629          clear xx   ;
013300110701          setll (ilvstpoc)   figrg04l  ;
013400110701          reade (ilvstpoc)   figrg04l  ;
013500110629
013600110701          // Se non trovata alcuna distinta per la filiale consegna
013700110629          //   segnalo errore
013800110629 2        if %eof(figrg04l)  ;
013900110629             eval olvsterr='3';
014000110701 2           eval olvstmsg='Non esiste nessuna distinta per la fil cons.+
014100110701                            immessa' ;
014200110629          endif  ;
014300110629
014400110629 2        dow not %eof(figrg04l)  ;
014500110629 3        if argatb= ' '  and ILvstdat>=grgdde and Ilvstdat <=grgdscad ;
014600110629            dgrgflo=grgflo                                            ;
014700110629 4          if §grgcsto='S'   ;
014800110629              xx=xx+1  ;
014900110629              giriSt(xx)=grgcgi  ;
015000110629 4          endif  ;
015100110629 3        endif  ;
015200110629
015300110701          reade (ilvstpoc)   figrg04l  ;
015400110629 2        enddo  ;
015500110629 1        endif  ;
015600110930      /end-free
015700110629
015800110930          // controllo riempimento skiera
015900110930     c**
016000110930     c                   clear                   trul0sds
016100110930     c                   eval      i0sski='Girist'
016200110930     c                   eval      i0sele=%elem(GiriSt)
016300110930     c                   eval      i0spie=xx
016400110930     c                   eval      i0scod='     '
016500110930     c                   eval      i0sfile='FIGRG00F  '
016600110930     c                   eval      i0ssif=knsif
016700110930     c                   eval      i0spgm='FNLVSTR'
016800110930     c                   movel     trul0sds      kpjbu
016900110930     c                   call      'TRUL0SR'
017000110930     c                   parm                    kpjba
017100110930      /free
017200110930
017300110701          savpoc=ilvstpoc  ;
017400110629          savdat=ilvstdat  ;
017500110629       ENDSR  ;
017600110629
017700110629       //---------------------------------------------------------------------
017800110629       BEGSR CarDati   ;
017900110629
018000110629          // Se mancano i dati da bolla --> li carico
0181001106291      if ilvstmgs=0 or (ilvstfin=' ' and ilvsttino<>'A') or
018200110629             (ilvstfap=' ' and ilvsttino<>' ')    ;
018300110629
018400110629       chain (ilvstaas:ilvstlnp:ilvstnrs:ilvstnsp)  fnarb01l    ;
0185001106292      if not %found(fnarb01l)  ;
018600110629             eval olvsterr='4';
018700110629             eval olvstmsg='Non trovata bolla'  ;
018800110629x2     else  ;
018900110629       ilvstmgs=arbmgs ;
0190001106293      if ilvsttino<>'A'  ;
019100110629          olvstfin=arbfin  ;
0192001106293      endif  ;
0193001106293      if ilvsttino<>' '  ;
019400110629       olvstfap=arbfap  ;
0195001106293      endif  ;
0196001106292      endif  ;
019700110629
019800110629x1     else   ;
0199001106292      if ilvsttino<>'A'  ;
020000110629          olvstfin=ilvstfin  ;
0201001106292      endif  ;
0202001106292      if ilvsttino<>' '  ;
020300110629          olvstfap=ilvstfap  ;
0204001106292      endif  ;
020500110629
0206001106291      endif  ;
020700110629
0208001106291      if  ilvstcgi=*blanks  ;
020900110629       chain (ilvstaas:ilvstlnp:ilvstnrs:ilvstnsp)  fiarg01l    ;
0210001106292      if     %found(fiarg01l)  ;
021100110629       ilvstcgi=argcgi  ;
0212001106292      endif  ;
0213001106291      endif  ;
021400110629
021500110629       ENDSR     ;
021600110629       //---------------------------------------------------------------------
021700110629       BEGSR AggioFIN  ;
021800110629
021900110629       // Se è stato tolto il giro e c'e' T o Z --> reimposto il tipo normale
022000110629 1     if    ilvstcgi=*blanks  ;
022100110629
022200110629 2     if olvstfin='T'  ;
022300110629          olvstfin='C'  ;
022400110629          olvstfinag='S'  ;
022500110629 2     endif  ;
022600110629 2     if olvstfin='Z'  ;
022700110629          olvstfin='P'  ;
022800110629          olvstfinag='S'  ;
022900110629 2     endif  ;
023000110629
023100110629       // Se c'e' giro verifico se è Centro Storico
023200110629 x1    else    ;
023300110629       Indx=%lookup(ilvstcgi:giriSt)    ;
023400110629       // il giro é centro storico  --> imposto T o Z se mancanno
023500110629
023600110629 2     if  Indx>0     ;
023700110629 3     if olvstfin='C'  ;
023800110629          olvstfin='T'  ;
023900110629          olvstfinag='S'  ;
024000110629 3     endif  ;
024100110629 3     if olvstfin='P'  ;
024200110629          olvstfin='Z'  ;
024300110629          olvstfinag='S'  ;
024400110629 3     endif  ;
024500110629
024600110629 x2    else  ;
024700110629
024800110629 3     if olvstfin='T'  ;
024900110629          olvstfin='C'  ;
025000110629          olvstfinag='S'  ;
025100110629 3     endif  ;
025200110629 3     if olvstfin='Z'  ;
025300110629          olvstfin='P'  ;
025400110629          olvstfinag='S'  ;
025500110629 3     endif  ;
025600110629
025700110629 2     endif  ;
025800110629 1     endif  ;
025900110629
026000110629       ENDSR     ;
026100110629       //---------------------------------------------------------------------
026200110629       BEGSR ScriviAGB ;
026300110629
026400110629       kagb  ='ST'  ;
026500110629
026600110629       ktbo  ='A'   ;
026700110629       exsr Ctr_AGB  ;
026800110629
026900110629       ktbo  ='P'   ;
027000110629       exsr Ctr_AGB  ;
027100110629
027200110629       ktbo  ='S'   ;
027300110629       exsr Ctr_AGB  ;
027400110629
027500110629       ENDSR     ;
027600110629       //---------------------------------------------------------------------
027700110629       BEGSR  CTR_AGB   ;
027800110629
027900110629       chain (ktbo:ilvstaas:ilvstlnp:ilvstnrs:ilvstnsp:kagb) fnagb01l    ;
028000110629       if not %found(fnagb01l)  ;
028100110629        clear fnagb000  ;
028200110629       endif  ;
028300110629
028400110629       agbtbo=ktbo ;
028500110629       agbaas =ilvstaas ;
028600110629       agblnp =ilvstlnp ;
028700110629       agbnrs =ilvstnrs ;
028800110629       agbnsp =ilvstnsp ;
028900110629       agbagb=kagb  ;
029000110629       agbfbs=olvstfin ;
029100110629       if not %found(fnagb01l)  ;
029200110629       write fnagb000 ;
029300110629       else  ;
029400110629       update fnagb000 ;
029500110629       endif  ;
029600110629
029700110629       ENDSR  ;
