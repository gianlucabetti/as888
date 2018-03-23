000100050929     h Decedit('0,') Datedit(*ymd.) Option(*nodebugio)
000200050929
000300061122       // ------------------------------------------------------------------------
000400061122       //
000500061122       //               SCRITTURA FASE ORM
000600061122       //
000700061122       // ------------------------------------------------------------------------
000800050929
000900061121     ffnorm01l  uf   e           k disk
001000061122     ffnorf01l  if a e           k disk
001100050929
001200061122       // ------------------------------------------------------------------------
001300050929
001400061122     d savfgs          s                   like(orffgs)
001500061122     d savndc          s                   like(orfndc)
001600061122     d savddc          s                   like(orfddc)
001700061122
001800061121     d keyorm          ds                  likerec(fnorm000:*key)
001900061122     d keyorf          ds                  likerec(fnorf000:*key)
002000061122
002100070716     d dfar          e ds
002200061121     d fior51ds      e ds
002300070716     d kpjba         e ds
002400070716     d tibs02ds      e ds
002500070716
002600070716       // ------------------------------------------------------------------------
002700070716     d tibs02r         pr                  extpgm('TIBS02R')
002800070716     d  kpjba                              likeds(kpjba)
002900070716     d  tibs02ds                           likeds(tibs02ds)
003000050929
003100061122       // ------------------------------------------------------------------------
003200061121
003300061121      /free
003400061121
003500061121       clear o51err;
003600061121       clear o51msg;
003700050929
003800061121       // controllo i dati passati
003900061121       select;
004000061121
004100061121       // p.o. emissione
004200061121       when i51poe = *zeros;
004300061121       o51err = '1';
004400061121       o51msg = 'Manca p.o. emissione ORM';
004500061121
004600061121       // numero orm
004700061121       when i51nor = *zeros;
004800061121       o51err = '1';
004900061121       o51msg = 'Manca il numero ORM';
005000061121
005100061121       // fase orm
005200061121       when i51far = *zeros;
005300061121       o51err = '1';
005400061121       o51msg = 'Manca la fase ORM';
005500061121
005600061121       endsl;
005700061121
005800061121       // se tutto ok avanzo l'orm
005900061121       if o51err = *blanks;
006000061121       exsr sr_elabora;
006100061121       endif;
006200050929
006300061122       *inlr = *on;
006400061121
006500061121       // ------------------------------------------------------------------------
006600061121       // routine elabora
006700061121       // ------------------------------------------------------------------------
006800061121       begsr sr_elabora;
006900061121
007000061121       // preparo la key dell'orm
007100061121       keyorm.ormpoe = i51poe;
007200061122       keyorm.ormnsr = i51nsr;
007300061121       keyorm.ormnor = i51nor;
007400061121       keyorm.ormnrv = i51nrv;
007500061121
007600061121       chain %kds(keyorm) fnorm01l;
007700061121
007800061121       // se non trovo l'orm errore
007900061121       if not %found(fnorm01l);
008000061121       o51err = '1';
008100061121       o51msg = 'ORM non trovato';
008200061121       leavesr;
008300061121       endif;
008400061121
008500080423       // solo quando richiamato da FNLS01R i51tip = *blanks
008600080423       if i51tip = *blanks;
008700080423
008800061121       // l'orm deve essere in fase inferiore a quella passata
008900061122       if ormfao > i51far;
009000061121       o51err = '1';
009100061121       o51msg = 'ORM in fase errata';
009200061121       leavesr;
009300061121       endif;
009400061121
009500061121       // se passata fase 999 l'orm deve essere in fase < di 900
009600061122       if i51far = 999 and ormfao > 600;
009700061121       o51err = '1';
009800061121       o51msg = 'ORM in fase errata';
009900061121       leavesr;
010000061121       endif;
010100080423       endif;
010200070716
010300061122       // preparo la key della fase
010400061122       keyorf.orfpoe = ormpoe;
010500061122       keyorf.orfnsr = ormnsr;
010600061122       keyorf.orfnor = ormnor;
010700061122       keyorf.orfnrv = ormnrv;
010800061122       keyorf.orfdae = ormdfo;
010900061122       keyorf.orfore = ormofo;
011000061122       keyorf.orffar = ormfao;
011100061122
011200061122       chain %kds(keyorf) fnorf01l;
011300061122       clear savfgs;
011400061122       clear savndc;
011500061122       clear savddc;
011600061122       if %found(fnorf01l);
011700061122       savfgs = orffgs;
011800061122       savndc = orfndc;
011900061122       savddc = orfddc;
012000061122       endif;
012100061121
012200070716       // tabella fase
012300070716       clear tibs02ds;
012400070716       t02mod = 'C';
012500070716       t02sif = knsif;
012600070716       t02cod = 'FAR';
012700070716       t02ke1 = %editc(i51far:'X');
012800070716       tibs02r(kpjba:tibs02ds);
012900070716       dfar = t02uni;
013000070716
013100061121       // aggiorno l'ORM
013200061122       ormdfo = %dec(%date());
013300061122       ormofo = %dec(%time());
013400061122       ormfao = i51far;
013500061122       ormdtt = *date;
013600070716       ormeti = d§farass;
013700061122       update fnorm000;
013800061121
013900061122       // scrivo la fase
014000080423       // non imposto il campo dorf01 --> non serve
014100061122       clear fnorf000;
014200061122       orfpoe = ormpoe;
014300061122       orfnsr = ormnsr;
014400061122       orfnor = ormnor;
014500061122       orfnrv = ormnrv;
014600061122       orfpog = ormpor;
014700061122       orfdae = ormdfo;
014800061122       orfore = ormofo;
014900061122       orffar = ormfao;
015000061122       orfcar = i51car;
015100080423       // se richiamato da LS01
015200080423       if i51tip = *blanks;
015300061122       orfpue = 'ANNULL_DDT';
015400080423       // se richiamato da altri
015500080423       else;
015600080423       orfpue = 'AUTOMATICA';
015700080423       endif;
015800080423       // se la nuova fase non prevede la distinta non la memorizzo
015900080423       // per ora controllo solo la fase 100 tanto questo programma viene richiamato
016000080423       // per chiudere l'ORM (ls01) o per riaprire l'ORM (DPD)
016100080423       if orffar = 100;
016200080423       else;
016300061122       orffgs = savfgs;
016400061122       orfndc = savndc;
016500061122       orfddc = savddc;
016600080423       endif;
016700061122       write fnorf000;
016800061122
016900061121       endsr;
017000050929
017100061121       // ------------------------------------------------------------------------
017200061121       // routine iniziale
017300061121       // ------------------------------------------------------------------------
017400061121         begsr *inzsr;
017500061121
017600061121      /end-free
017700061121
017800061121     c     *entry        plist
017900061121     c                   parm                    fior51ds
018000061121
018100061121     c                   endsr
018200050929
