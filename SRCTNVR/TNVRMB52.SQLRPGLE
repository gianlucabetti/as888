000100090127     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000200090127      // ----------------------------------------------------------------------
000300090127      //
000400090127      //         Estrazione clienti con IBAN estero  ?
000500090127      //
000600090127      // ----------------------------------------------------------------------
000700090127     fPrtf198   o    f  198        Printer Oflind(*InOf)
000800090127
000900090128     d waa             s              2
001000090128     d wmm             s              2
001100090128     d wgg             s              2
001200090128     d wnota           s             30
001300090128     d wdata           s              8
001400090127     d $fine           s               n
001500090127
001600090127     d cnclpds       e ds                  extname(cnclp00f)
001700090127     d cnacods       e ds                  extname(cnaco00f)
001800090128     d cnindds       e ds                  extname(cnind00f)
001900090128     d fnspeds       e ds                  extname(fnspe00f)
002000090127
002100090127      // ----------------------------------------------------------------------
002200090127
002300090127      /free
002400090128
002500090128         clear waa;
002600090128         clear wgg;
002700090128         clear wmm;
002800090128
002900090128         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
003000090127
003100090127       //?estraggo i clienti che hanno IBAN = 'IT' per pagamento contrassegni
003200090127         exec sql
003300090128          declare clienti cursor for select acokcc, acoksc, acorag, clpdus,
003400090128          clpscf, acoabl from cnclp00f join cnaco00f on clpkcc = acokcc and
003500090127          clpksc = acoksc where substr(clpbab, 1, 2) = 'IT' and acokcc = 151
003600090127          and acoabl <> '8'
003700090127          order by cnclp00f.clpksc;
003800090127
003900090127         exec sql open clienti;
004000090127
004100090127         dow not $fine;
004200090127
004300090128          exec sql fetch next from clienti into :acokcc, :acoksc, :acorag,
004400090128           :clpdus, :clpscf, :acoabl;
004500090127
004600090127          if sqlcod = 100 or sqlcod < 0;
004700090127           $fine = *on;
004800090127           leave;
004900090127          endif;
005000090128
005100090128       //?imposto la data ultima spedizione fatturata x la stampa
005200090128          clear waa;
005300090128          clear wgg;
005400090128          clear wmm;
005500090128          wgg = %subst(%editc(%dec(clpdus):'X'):5:2);
005600090128          wmm = %subst(%editc(%dec(clpdus):'X'):3:2);
005700090128          waa = %subst(%editc(%dec(clpdus):'X'):1:2);
005800090128          wdata = wgg + '/' + wmm + '/' + waa;
005900090127
006000090127       //?prima controllo se il luogo 555 ha indirizzo estero
006100090127          exec sql
006200090127           select spenaz into :spenaz from fnspe00f
006300090127           where speflg = ' ' and spefls = 'L' and
006400090127           specli = :acoksc and specod = '555';
006500090128       //?non trovo il rcd corrispondente pulisco il campo della nazione per poi
006600090128       //?controllare l'indirizzo del cliente
006700090127          if sqlcod <> 0;
006800090127           clear spenaz;
006900090127          endif;
007000090128       //?trovo il rcd ed è estero stampo
007100090128          if spenaz <> *blanks;
007200090128           if not *in99 or *inof;
007300090128            except testa;
007400090128            *in99 = *on;
007500090128           endif;
007600090128           wnota = 'Luogo 555 del codice';
007700090128           wnota = %trim(wnota) + ' ' + %editc(acoksc:'1');
007800090128           except riga;
007900090128           iter;
008000090128          endif;
008100090127
008200090128       //?se luogo 555 italia o non trovato controllo se anagrafica cliente ha indirizzo estero
008300090128          exec sql
008400090128           select indsta into :indsta from cnind00f
008500090128           where indkcc = :acokcc and indksc = :acoksc;
008600090128       //?non trovo il rcd corrispondente pulisco il campo della nazione per poi
008700090128       //?controllare con il codice intestazione fattura
008800090128          if sqlcod <> 0;
008900090128           clear indsta;
009000090128          endif;
009100090128       //?trovo il rcd ed è estero stampo
009200090128          if indsta <> *blanks;
009300090128           if not *in99 or *inof;
009400090128            except testa;
009500090128            *in99 = *on;
009600090128           endif;
009700090128           wnota = 'Indirizzo del codice';
009800090128           wnota = %trim(wnota) + ' ' + %editc(acoksc:'1');
009900090128           except riga;
010000090128           iter;
010100090128          endif;
010200090128
010300090127       //?se luogo 555 e anagrafica cliente italia controllo il cod.fatturazione se <>
010400090128          if acoksc <> clpscf;
010500090127       //?prima controllo se il luogo 555 ha indirizzo estero
010600090128          exec sql
010700090128           select spenaz into :spenaz from fnspe00f
010800090128           where speflg = ' ' and spefls = 'L' and
010900090128           specli = :clpscf and specod = '555';
011000090128       //?non trovo il rcd corrispondente pulisco il campo della nazione per poi
011100090128       //?controllare l'indirizzo del cliente
011200090128          if sqlcod <> 0;
011300090128           clear spenaz;
011400090128          endif;
011500090128       //?trovo il rcd ed è estero stampo
011600090128          if spenaz <> *blanks;
011700090128           if not *in99 or *inof;
011800090128            except testa;
011900090128            *in99 = *on;
012000090128           endif;
012100090128           wnota = 'Luogo 555 del codice';
012200090128           wnota = %trim(wnota) + ' ' + %editc(clpscf:'1');
012300090128           except riga;
012400090128           iter;
012500090128          endif;
012600090128
012700090127       //?se luogo 555 italia controllo se anagrafica cliente ha indirizzo estero
012800090128          exec sql
012900090128           select indsta into :indsta from cnind00f
013000090128           where indkcc = :acokcc and indksc = :clpscf;
013100090128       //?non trovo il rcd corrispondente pulisco il campo della nazione per poi
013200090128       //?controllare con il codice intestazione fattura
013300090128          if sqlcod <> 0;
013400090128           clear indsta;
013500090128          endif;
013600090128       //?trovo il rcd ed è estero stampo
013700090128          if indsta <> *blanks;
013800090128           if not *in99 or *inof;
013900090128            except testa;
014000090128            *in99 = *on;
014100090128           endif;
014200090128           wnota = 'Indirizzo del codice';
014300090128           wnota = %trim(wnota) + ' ' + %editc(clpscf:'1');
014400090128           except riga;
014500090128           iter;
014600090128          endif;
014700090127          endif;
014800090127
014900090127         enddo;
015000090127
015100090127         exec sql close clienti;
015200090127
015300090127         *inlr = *on;
015400090128
015500090128      /end-free
015600090128
015700030113
015800030113     oPrtf198   e            Testa          2 02
015900030113     o                                           20 'BARTOLINI S.p.A.'
016000090128     o                                           60 '**  CLIENTI CON IBAN'
016100090128     o                                         +  1 'ESTERO **'
016200030113     o                       Udate              110 '  /  /  '
016300090128     o                                          120 'TNVRMB52'
016400030113     o                                          128 'Pag.'
016500030113     o                       Page          Z    132
016600030113     o          e            Testa          2
016700090128     o                                            7 'Cliente'
016800030113     o                                           27 'Ragione Sociale'
016900090128     o                                           84 'Nota'
017000090128     o                                          120 'Data ult.sped.fatt.'
017100030113     o          e            Riga        1
017200090128     o                       acoksc        z      7
017300090128     o                       acorag            +  1
017400090128     o                       wnota             +  1
017500090128     o                       wdata             +  1
