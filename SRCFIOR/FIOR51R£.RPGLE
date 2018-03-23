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
008500061121       // l'orm deve essere in fase inferiore a quella passata
008600061122       if ormfao > i51far;
008700061121       o51err = '1';
008800061121       o51msg = 'ORM in fase errata';
008900061121       leavesr;
009000061121       endif;
009100061121
009200061121       // se passata fase 999 l'orm deve essere in fase < di 900
009300061122       if i51far = 999 and ormfao > 600;
009400061121       o51err = '1';
009500061121       o51msg = 'ORM in fase errata';
009600061121       leavesr;
009700061121       endif;
009800070716
009900061122       // preparo la key della fase
010000061122       keyorf.orfpoe = ormpoe;
010100061122       keyorf.orfnsr = ormnsr;
010200061122       keyorf.orfnor = ormnor;
010300061122       keyorf.orfnrv = ormnrv;
010400061122       keyorf.orfdae = ormdfo;
010500061122       keyorf.orfore = ormofo;
010600061122       keyorf.orffar = ormfao;
010700061122
010800061122       chain %kds(keyorf) fnorf01l;
010900061122       clear savfgs;
011000061122       clear savndc;
011100061122       clear savddc;
011200061122       if %found(fnorf01l);
011300061122       savfgs = orffgs;
011400061122       savndc = orfndc;
011500061122       savddc = orfddc;
011600061122       endif;
011700061121
011800070716       // tabella fase
011900070716       clear tibs02ds;
012000070716       t02mod = 'C';
012100070716       t02sif = knsif;
012200070716       t02cod = 'FAR';
012300070716       t02ke1 = %editc(i51far:'X');
012400070716       tibs02r(kpjba:tibs02ds);
012500070716       dfar = t02uni;
012600070716
012700061121       // aggiorno l'ORM
012800061122       ormdfo = %dec(%date());
012900061122       ormofo = %dec(%time());
013000061122       ormfao = i51far;
013100061122       ormdtt = *date;
013200070716       ormeti = d§farass;
013300061122       update fnorm000;
013400061121
013500061122       // scrivo la fase
013600070716       // non imposto il campo dorf01 in quanto per ora richiamato solo da LS01
013700070716       // per scrittura fase 999 cioè chiusura ORM e non assegnazione
013800061122       clear fnorf000;
013900061122       orfpoe = ormpoe;
014000061122       orfnsr = ormnsr;
014100061122       orfnor = ormnor;
014200061122       orfnrv = ormnrv;
014300061122       orfpog = ormpor;
014400061122       orfdae = ormdfo;
014500061122       orfore = ormofo;
014600061122       orffar = ormfao;
014700061122       orfcar = i51car;
014800061122       orfpue = 'ANNULL_DDT';
014900061122       orffgs = savfgs;
015000061122       orfndc = savndc;
015100061122       orfddc = savddc;
015200061122       write fnorf000;
015300061122
015400061121       endsr;
015500050929
015600061121       // ------------------------------------------------------------------------
015700061121       // routine iniziale
015800061121       // ------------------------------------------------------------------------
015900061121         begsr *inzsr;
016000061121
016100061121      /end-free
016200061121
016300061121     c     *entry        plist
016400061121     c                   parm                    fior51ds
016500061121
016600061121     c                   endsr
016700050929
