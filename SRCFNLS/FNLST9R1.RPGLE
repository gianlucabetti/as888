000100110620     /*PRM dbgview(*source)
001000040107      *---------------------------------------------------------------*
001100130528      * Stampa Etichette borderò                                      *
001200040107      *---------------------------------------------------------------*
001300040107
001400040108     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001500090528     h dftactgrp(*no) actgrp(*caller)
001600040107
001700040107      *---------------------------------------------------------------*
001800040107
001801130529     ffnfge02l  uf   e           k disk
001802130530     ffnfge01l  uf   e           k disk    rename(fnfge000:fnfge1)
002001130524     ffnfgv01l  if   e           k disk
002002130528     ffnfgw01l  if   e           k disk
002003130530     fazorg01l  if   e           k disk
002005130528     FFNLV22P   O    F  132        PRINTER OFLIND(*INOE)
002700040112      *
002800040107      *---------------------------------------------------------------*
003400040107      *
003500040107      *   S C H I E R E   - - - - - - - - - - - - - - - - - - - - - - *
003600040107      *
004700040107      *
004800040107      *   D S   - - - - - - - - - - - - - - - - - - - - - - - - - - - *
004900040107      *
004901130528     D                 DS
004902130530     d  fgvlna                 1      3  0
004903130530     D  FGVFFV                 4    243
004904130530     D  FGVFF2               244    450
004905130530     D  FGWFF3               451    690
004906130530     D  FGWFF4               691    900
004908130528     D  FFV                    1    900
004909130528     D                                     DIM(300)
004919130528     D*
004975130530     d fnlv55ds      e ds
004976130530     d fnlst9ds      e ds
005000090922     d Tibs34ds      e ds                  inz
005100090922     d §azute        e ds                  Extname(Azute00f)
005200090922     d                                     dtaara
005300090922     d §Datiute      e ds                  EXTname(ddatiute)
005301130524     d                                     dtaara
006200040107     d KPJBA         e ds
006201130530     d dsblb         e ds
008200040123      *
008900040109      *
008901130530     d wffv            s                   like(fgelna)
009200090409
009300040107      *
009400040107      *   V A R I A B I L I   - - - - - - - - - - - - - - - - - - - - *
009500040107      *
010700040213      * - Indici di schiera / Contatori
010800090827     d i               s              3  0 inz
011000040109      *
011206130621     d datadmy         s               d   datfmt(*dmy)
018500090828      *
018600090828      * - Flags booleani
018800081023
018900090402      // ? PROTOTIPI ?
019000090402      /copy gaitrasrc/srcprotopr,tibs34r
019001130530      /copy gaitrasrc/srcprotopr,fnlv55r
019500040107
019600040107      *===============================================================*
019700040107      *  RIEPILOGO INDICATORI                                         *
019800040107      *---------------------------------------------------------------*
019900090827      *  XX    -
020000040107      *===============================================================*
020100040107
020200040107     c     *Entry        plist
020300040107     c                   parm                    KPJBA
020301130528     C                   MOVEL     KPJBU         fnlst9ds
020400090827     c
020600040123      *
020700090331      /free
020701130528       select;
020702130528       when lst9dfv>0;
020703130529          exsr sr_dfv;
020704130528       other;
020705130528          exsr fviaggio;
020706130528       endsl;
020900090610
026100090827       eval *inlr=*on;
026103130524
026104130529       //-------------------------------------------------------------*
026105130530       // Stampa etichette per data foglio                            *
026106130529       //-------------------------------------------------------------*
026107130529       begsr sr_dfv  ;
026108130603       //if lst9fog='T';
026109130603       //   setll (lst9dfv: simfel) fnfge02l;
026110130603       //   reade (lst9dfv: simfel) fnfge02l;
026111130603       //else;
026112130603       //   setll (lst9dfv: simfel: ' ') fnfge02l;
026113130603       //   reade (lst9dfv: simfel: ' ') fnfge02l;
026114130603       //endif;
026115130603       setll (lst9dfv: simfel) fnfge02l;
026116130603       reade (lst9dfv: simfel) fnfge02l;
026117130529
026118130529          dow not %eof(fnfge02l);
026119130530
026120130603           // Elaboro il record solo se richieste tutte le etichette (stampate
026121130603           // e non) OPPURE se lna ancora da stampare
026122130603            If lst9fog='T' or fgefst=' ';
026123130530             exsr sr_etich;
026124130530
026180130529             // aggiorno flag di stampata etichetta
026181130530             if fgefst=' ';
026182130530                fgefst='S';
026183130530                update fnfge000;
026184130530             else;
026185130530                unlock fnfge02l;
026186130530             endif;
026187130603            endif;
026189130603             reade (lst9dfv: simfel) fnfge02l;
026190130603       //    if lst9fog='T';
026191130603       //       setll (lst9dfv: simfel) fnfge02l;
026192130603       //       reade (lst9dfv: simfel) fnfge02l;
026193130603       //    else;
026194130603       //       setll (lst9dfv: simfel: ' ') fnfge02l;
026195130603       //       reade (lst9dfv: simfel: ' ') fnfge02l;
026196130603       //    endif;
026197130529          enddo;
026198130529       endsr;
026199130528       //-------------------------------------------------------------*
026200130530       // Stampa etichette in base al foglio viaggio                  *
026201130528       //-------------------------------------------------------------*
026202130528       begsr fviaggio;
026203130607       chain (lst9nfv: simfel) fnfgv01l;
026204130607       chain (lst9nfv: simfel) fnfgW01l;
026205130530       i=1;
026206130530       // Per ogni linea di arrivo chaino file di appoggio e stampo
026207130530       dow ffv(i)>*zeros and i<=300;
026208130530          wffv=%dec(ffv(i):3:0);
026209130530          chain (fgvdfv: fgvlnp: Wffv) fnfge01l;
026210130530          if %found(fnfge01l);
026211130530             exsr sr_etich;
026212130530             // aggiorno flag di stampata etichetta
026213130530             if fgefst=' ';
026214130530                fgefst='S';
026215130530                update fnfge1;
026216130530             else;
026217130530                unlock fnfge01l;
026218130530             endif;
026219130530          endif;
026220130530          i+=1;
026221130530       enddo;
026222130528       endsr;
026223130530       //-------------------------------------------------------------*
026224130530       // Stampa etichette                                            *
026225130530       //-------------------------------------------------------------*
026226130530       begsr sr_etich;
026227130530
026228130530             clear §lbnfv ;
026229130530             // data in formato gg/mm/aa in 6 cifre
026230130621             datadmy=%date(fgedfv:*iso);
026231130621             §lbdat=%dec(datadmy);
026232130530             clear §lbnsp;
026233130530             clear §lbvet;
026234130530             clear §lbcon;
026235130530             clear §lbarr;
026236130530             clear §lbnbr;
026237130530             §lblnp=%editc(fgelnp:'X');
026238130530
026239130530             // Decodifico lnp
026240130530             clear §lbdlp;
026241130530             chain (fgelnp) azorg01l;
026242130530             if %found(azorg01l);
026243130530                §lbdlp=orgdes;
026244130530             endif;
026245130530
026246130530             §lblna=%editc(fgelna:'X');
026247130530
026248130530             // Decodifico lna
026249130530             clear §lbdla;
026250130530             chain (fgelna) azorg01l;
026251130530             if %found(azorg01l);
026252130530                §lbdla=orgdes;
026253130530             endif;
026254130530
026255130530
026256130530             // Impostazione di §lbino
026257130530             clear fnlv55ds;
026258130530             d55tpt='A';
026259130530             d55lnp=fgelnp;
026260130530             d55lin=fgelna;
026261130530             d55drf=fgedfv;
026262130530             fnlv55r(fnlv55ds);
026263130607             clear §lbino;
026264130607             clear §lbsca;
026265130530             // C'è transito: stapo SCARICO + VIA
026266130530             if fgelna<>fgeflp and d55tfa<>fgeflp;
026267130530                // Filiale di scarico
026268130530                chain (fgeflp) azorg01l;
026269130530                if %found(azorg01l);
026270130530                   §lbsca=%editc(fgeflp:'X');
026271130530                endif;
026272130530                chain (d55tfa) azorg01l;
026273130530                if %found(azorg01l);
026274130530                   §lbino=orgdes;
026275130530                endif;
026276130530             else;
026277130530             // Non c'è transito stampo solo "VIA"
026278130530                chain (fgeflp) azorg01l;
026279130530                if %found(azorg01l);
026280130530                   §lbino=orgdes;
026281130530                endif;
026282130530             endif;
026283130530             // Richiamo pgm che stampa l'etichetta
026284130530      /end-free
026285130530     C                   call      lst9pgmse
026286130530     C                   parm                    dsblb
026287130530      /free
026288130530       endsr;
026762090402       //-------------------------------------------------------------*
026763090402       //Routine Iniziale                                             *
026764090402       //-------------------------------------------------------------*
026765090414       begsr *inzsr;
026766090402
026800090402       //Reperisco dati job
026900090917         exsr datijob;
030300090402
030400090402       EndSr;
030500040108
034400090402       //-------------------------------------------------------------*
034500090402       //Reperimento Dati del job (Utente/Operativi)                  *
034600090402       //-------------------------------------------------------------*
034700090918       Begsr DatiJob;
034800090403
034900090917          in(e) §azute;
035000090917          if not %error;
035100090917           in(e) §datiute;
035200090917          endif;
035300090917          if %error or rsut = *blanks;
035400090917           tibs34r(tibs34ds);
035500090917           in §azute;
035600090917           in §datiute;
035700090917          endif;
035701111205
036000090917         endsr;
036100040108
036102130530      /end-free
036300130528     OFNLV22P   T    LRNLR
