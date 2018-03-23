000100121218      //---------------------------------------------------------------
000200121218      //
000300150903      //?        Controllo codice tassazione
000400121218      //
000500121218      //---------------------------------------------------------------
000600121218
000700121218     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000800040119
000900040119      *---------------------------------------------------------------*
001000121218
001100160205     ftitas38c  if   e           k disk
001200160205     ftita430c  if   e           k disk
001300160205     ffnorm01l  if   e           k disk
001400160205     fwfrmassl  uf a e           k disk
001500040119
001600040119      *---------------------------------------------------------------*
001700121218
001800160205     d TIBS69DS      E DS                  INZ
001900160205     d DS_cnaco      E DS                  extname(CNACO00F) prefix(ds_)
002000160205     d DS_cnind      E DS                  extname(CNIND00F) prefix(ds_)
002100160205     d DS_cnclp      E DS                  extname(CNCLP00F) prefix(ds_)
002200160205     d DS_fncls      E DS                  extname(FNCLS00F) prefix(ds_)
002300121218       // - Campi di comodo
002400140613
002500140613
002600140613      // - Campi di comodo data
002700140613     d wData_EUR       s               d   datfmt(*eur)
002800140613     d wData_ISO       s               d   datfmt(*iso)
002900140613     d wOggi           s              8  0 inz
003000121218
003100121218       // - flag booleani
003200140613
003300140613       //--------------------------------------------------------------
003400140613       //?Definizione strutture dati.                                  ?
003500140613       //--------------------------------------------------------------
003600140613     d KPJBA         e ds
003700160205     d dsbl4m        e ds
003800140616
003900160205      /copy gaitrasrc/srcprotopr,tibs69r
004000040119
004100140613       //--------------------------------------------------------------
004200140613       //?M A I N - L I N E
004300140613       //---------------------------------------------------------------
004400140613
004500040119
004600121218      /free
004700140613
004800140613
004900140613         wOggi     = %dec(%date());
005000140613         wData_ISO = %date(wOggi:*ISO);
005100140613         wData_EUR = wData_ISO;
005200140613
005300140616
005400160205       // Leggo titas38c
005500160205         setll (2016:0101) titas38c;
005600160205         reade (2016:0101) titas38c;
005700160205         dow not %eof(titas38c) and tasmgs<=0131;
005800160205       //   elaboro solo A2 e ksc codificato
005900160205            if tastbl='A2' and %subst(%editc(tasksc:'X'):4:4)<>'9999';
006000160205       //   vedo se presente ORM
006100160205               chain (tasaas:taslnp:tasnrs:tasnsp:'M') tita430c;
006200160205               if %found(tita430c);
006300160205       // spedizione con ORM
006400160205                  dsbl4m=ta4not;
006500160205                  chain (§B4POE:§B4NSR:§B4NOR:§B4NRV) fnorm01l ;
006600160205               else;
006700160205       // spedizione senza ORM
006800160205               endif;
006900160205
007000160205       // memorizzo dati nel wf;
007100160205              chain (tasksc) wfrmassl ;
007200160205              if    %found(wfrmassl);
007300160205                 exsr sr_impocampi;
007400160205                 update wfrmass00  ;
007500160205              else    ;
007600160205                 clear  wfrmass00;
007700160205                 rmksc=tasksc;
007800160205       //        decodifico ragione sociale
007900160205           clear DS_cnaco;
008000160205           clear DS_cnind;
008100160205           clear DS_cnclp;
008200160205           clear DS_fncls;
008300160205           I69kac=TASKSC ;
008400160205           tibs69r(tibs69ds:DS_cnaco:DS_cnind:DS_cnclp:DS_fncls);
008500160205                 RMRAG=ds_acorag;
008600160205                 exsr sr_impocampi;
008700160205                 write  wfrmass00   ;
008800160205              endif;
008900160205
009000160205            endif;
009100160205            reade (2016)  titas38c;
009200150903         enddo;
009300140616
009400140613
009500121218
009600140613         *inLR = *ON;
009700140616
009800160205       BEGSR  sr_impocampi ;
009900160205                 rmtass+=1;
010000160205                 if not %found(tita430c);
010100160205                   rmtassnO+=1;
010200160205                 else;
010300160205                   select;
010400160205                   when ormtco='E';
010500160205                     rmtassE+=1;
010600160205                   when ormtco='F';
010700160205                     rmtassF+=1;
010800160205                   when ormtco='I';
010900160205                     rmtassI+=1;
011000160205                   when ormtco='M';
011100160205                     rmtassM+=1;
011200160205                   when ormtco='S';
011300160205                     rmtassS+=1;
011400160205                   endsl;
011500160205                 endif;
011600160205       endsr;
011700121218      /end-free
011800140331
