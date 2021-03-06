000100131001      //---------------------------------------------------------------
000200131001      //
000300160419      //?      Gira gli orari delle telfonte
000400160419      //?      da verticale a orizzontale
000500131001      //
000600131001      //---------------------------------------------------------------
000700131001
000800080520     h decedit('0,') datedit(*ymd.) option(*nodebugio)
000900150909     h dftactgrp(*no) actgrp(*caller)
001000040511
001100131001      //---------------------------------------------------------------
001200131001      //?Dichiarazione file.
001300131001      //---------------------------------------------------------------
001400160122      // -?File di work a totale
001500160122     fAPPMB     o    e             disk    rename(appmb:appmb1)
001600160121
001700131001      //---------------------------------------------------------------
001800131001      //?Definizione costanti.
001900131001      //---------------------------------------------------------------
002000131001
002100131001      //---------------------------------------------------------------
002200131001      //?Definizione schiere.
002300131001      //---------------------------------------------------------------
002400131001
002500131001      //---------------------------------------------------------------
002600131001      //?Definizione aree dati.
002700131001      //---------------------------------------------------------------
002800131001
002900131001      //---------------------------------------------------------------
003000131001      //?Definizione strutture dati.
003100131001      //---------------------------------------------------------------
003200150909      // -?Parametri ricevuti?
003300131001     d kpjba         e ds
003400160122
003500160122      // -?File di Work      ?
003600160419     d telxora       e ds                  extname(telxora)
003700160419     d                                     qualified
003800131001
003900131001      //---------------------------------------------------------------
004000131001      //?Definizione variabili globali.
004100131001      //---------------------------------------------------------------
004200131001      // -?Flag booleani
004300160122     d Fine            s               n   inz(*off)
004400131001
004500131001      // -?Campi di comodo
004600131001
004700131001      //---------------------------------------------------------------
004800131001      //?Definizione procedure esterne.
004900131001      //---------------------------------------------------------------
005000131001
005100131001      //---------------------------------------------------------------
005200131001      //?Definizione prototipi.
005300131001      //---------------------------------------------------------------
005400131001
005500131001      //---------------------------------------------------------------
005600131001      //?Definizione key-list.
005700131001      //---------------------------------------------------------------
005800131001
005900131001      //---------------------------------------------------------------
006000131001      //?Riepilogo indicatori.
006100131001      //---------------------------------------------------------------
006200131001
006300131001      //---------------------------------------------------------------
006400131001      //?M A I N - L I N E
006500131001      //---------------------------------------------------------------
006600131001
006700131001     c     *Entry        plist
006800131001     c                   parm                    kpjba
006900131001
007000131001      /free
007100131001
007200131001       //?Operazioni iniziali
007300131001       exsr RoutInz;
007400131001
007500131001       //?Elabora file
007600131001       exsr Elabora;
007700131001
007800131001       //?Operazioni finali
007900131001       exsr RoutEnd;
008000131001
008100131001       //--------------------------------------------------------------
008200131001       //?Operazioni iniziali.
008300131001       //--------------------------------------------------------------
008400131001       BEGSR RoutInz;
008500160419
008600160419         exec sql set option dynusrprf = *owner, closqlcsr = *endmod;
008700160419
008800160419       //?Pulisco i file di work
008900160419         exec sql
009000160419         DELETE from edpmbstat/APPMB;
009100131001
009200131001       ENDSR;
009300131001
009400131001       //--------------------------------------------------------------
009500131001       //?Elabora
009600131001       //--------------------------------------------------------------
009700150909       BEGSR Elabora;
009800131001
009900160122         Fine = *off;
010000131001
010100160122       //?Leggo il file di work creato da ND
010200160122         exec sql DECLARE WRK cursor for
010300160419                  SELECT * from TELXORA
010400160419                  ORDER BY filiale, tipo, ora;
010500160122
010600160122       //?Apertura del cursore
010700160122         exec sql
010800160122         OPEN WRK;
010900160122
011000160122         IF sqlcode < 0;
011100160122           exec sql
011200160122           CLOSE WRK;
011300160122           leavesr;
011400160122         ENDIF;
011500160122
011600160122         DOW  not Fine;
011700160122           exec sql
011800160419           FETCH NEXT from WRK into :telxora;
011900160419           IF  sqlcod = 100 or sqlcod < 0;
012000160122             Fine = *on;
012100160122             leave;
012200160122           ENDIF;
012300160122
012400160122       //?Scrivo a rottura di filiale/tipo
012500160419           IF  telxora.filiale <> Filiale and Filiale > 0;
012600160122             write appmb1;
012700160122             clear appmb1;
012800160122           ENDIF;
012900160419           IF  telxora.tipo <> Tipo and Tipo <> *blanks;
013000160122             write appmb1;
013100160122             clear appmb1;
013200160122           ENDIF;
013300160122
013400160419           Filiale = telxora.filiale;
013500160419           Tipo = telxora.tipo;
013600160122
013700160122       //?imposto il totale degli appuntamenti per orario
013800160419           IF  telxora.ora = '00';
013900160419             zero = telxora.SEL0004;
014000160122           ENDIF;
014100160419           IF  telxora.ora = '01';
014200160419             uno = telxora.SEL0004;
014300160122           ENDIF;
014400160419           IF  telxora.ora = '02';
014500160419             due = telxora.SEL0004;
014600160122           ENDIF;
014700160419           IF  telxora.ora = '03';
014800160419             tre = telxora.SEL0004;
014900160122           ENDIF;
015000160419           IF  telxora.ora = '04';
015100160419             quattro = telxora.SEL0004;
015200160122           ENDIF;
015300160419           IF  telxora.ora = '05';
015400160419             cinque = telxora.SEL0004;
015500160122           ENDIF;
015600160419           IF  telxora.ora = '06';
015700160419             sei = telxora.SEL0004;
015800160122           ENDIF;
015900160419           IF  telxora.ora = '07';
016000160419             sette = telxora.SEL0004;
016100160122           ENDIF;
016200160419           IF  telxora.ora = '08';
016300160419             otto = telxora.SEL0004;
016400160122           ENDIF;
016500160419           IF  telxora.ora = '09';
016600160419             nove = telxora.SEL0004;
016700160122           ENDIF;
016800160419           IF  telxora.ora = '10';
016900160419             dieci = telxora.SEL0004;
017000160122           ENDIF;
017100160419           IF  telxora.ora = '11';
017200160419             undici = telxora.SEL0004;
017300160122           ENDIF;
017400160419           IF  telxora.ora = '12';
017500160419             dodici = telxora.SEL0004;
017600160122           ENDIF;
017700160419           IF  telxora.ora = '13';
017800160419             tredici = telxora.SEL0004;
017900160122           ENDIF;
018000160419           IF  telxora.ora = '14';
018100160419             quattordi = telxora.SEL0004;
018200160122           ENDIF;
018300160419           IF  telxora.ora = '15';
018400160419             quindici = telxora.SEL0004;
018500160122           ENDIF;
018600160419           IF  telxora.ora = '16';
018700160419             sedici = telxora.SEL0004;
018800160122           ENDIF;
018900160419           IF  telxora.ora = '17';
019000160419             diciasette = telxora.SEL0004;
019100160122           ENDIF;
019200160419           IF  telxora.ora = '18';
019300160419             diciotto = telxora.SEL0004;
019400160122           ENDIF;
019500160419           IF  telxora.ora = '19';
019600160419             diciannove = telxora.SEL0004;
019700160122           ENDIF;
019800160419           IF  telxora.ora = '20';
019900160419             venti = telxora.SEL0004;
020000160122           ENDIF;
020100160419           IF  telxora.ora = '21';
020200160419             ventuno = telxora.SEL0004;
020300160122           ENDIF;
020400160419           IF  telxora.ora = '22';
020500160419             ventidue = telxora.SEL0004;
020600160122           ENDIF;
020700160419           IF  telxora.ora = '23';
020800160419             ventitre = telxora.SEL0004;
020900160122           ENDIF;
021000160122
021100160122         ENDDO;
021200160122
021300160122       //?Chiusura del cursore
021400160122         exec sql
021500160122         CLOSE WRK;
021600131001
021700131001       ENDSR;
021800131001
021900131001       //--------------------------------------------------------------
022000131001       //?Operazioni finali.
022100131001       //--------------------------------------------------------------
022200131001       BEGSR RoutEnd;
022300131001
022400131001         *inLR = *on;
022500131001         return;
022600131001
022700131001       ENDSR;
022800131001
022900131001      /end-free
