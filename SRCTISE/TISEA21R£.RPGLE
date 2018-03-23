000100130208       //==============================================================
000200130208       //?Statistiche Delta per Fasce di Peso - batch da "TISE01R"     ?
000300130208       //==============================================================
000400130208
000500130208       //--------------------------------------------------------------
000600130208       //?Parametri di compilazione (vedi *cmd UBCRTOBJ).              ?
000700130208       //--------------------------------------------------------------
000800130208
000900130208     /*PRM  dbgview(*source)
001000130208     /*END
001100130208
001200130208       //--------------------------------------------------------------
001300130208       //?Specifiche di controllo.                                     ?
001400130208       //--------------------------------------------------------------
001500130208
001600130208     h decedit('0,') datedit(*dmy/) option(*nodebugio)
001700130208     h dftactgrp(*no)
001800130208     h bnddir('TRUL')
001900130208
002000130208       //--------------------------------------------------------------
002100130208       //?Dichiarazione file.                                          ?
002200130208       //--------------------------------------------------------------
002300130208
002400130208
002500130208       //--------------------------------------------------------------
002600130208       //?Definizione costanti.                                        ?
002700130208       //--------------------------------------------------------------
002800130208
002900130208
003000130208       //--------------------------------------------------------------
003100130208       //?Definizione schiere.                                         ?
003200130208       //--------------------------------------------------------------
003300130208
003400130208
003500130208       //--------------------------------------------------------------
003600130208       //?Definizione aree dati.                                       ?
003700130208       //--------------------------------------------------------------
003800130208
003900130208       // -?Dati utente?
004000130208     d §AzUte        e ds                  extname(AZUTE00F)
004100130208     d                                     dtaara
004200130208     d §DatiUte      e ds                  extname(dDatiUte)
004300130208     d                                     dtaara
004400130208
004500130208       //--------------------------------------------------------------
004600130208       //?Definizione strutture dati.                                  ?
004700130208       //--------------------------------------------------------------
004800130208
004900130208       // -?DS Dati vari per Statistiche -DI GRUPPO-?
005000130208     d ds5S          e ds                  inz
005100130208
005200130208       // -?Parametri ricevuti?
005300130208     d KPJBA         e ds
005400130208
005500130208     d TISEA21ds       ds                  inz
005600130208     d   SE01aamm              1      6  0 inz
005700130208
005800130208       // -?Parametri per TISEA2R (Statistiche Delta per Fasce di Peso)?
005900130208     d TISEA2ds        ds                  inz
006000130208     d   SEA2ami               1      6  0 inz                                  *AaMm iniziali
006100130208     d   SEA2amf               7     12  0 inz                                  *AaMm finali
006200130208     d   SEA2ioe              13     13    inz                                  *Italia/Estero/Tutto
006300160906     d   SEA2pt               14     14    inz                                  *Stat. x Poste
006400130208     d   SEA2tot              15     15    inz('S')                             *Solo Totali
006500130208
006600130208       //--------------------------------------------------------------
006700130208       //?Definizione variabili globali.                               ?
006800130208       //--------------------------------------------------------------
006900130208
007000130208     d wDateI          s               d   inz  datfmt(*iso)
007100130208     d wDateF          s               d   inz  datfmt(*iso)
007200130208
007300130208       //--------------------------------------------------------------
007400130208       //?Definizione prototipi procedure.                             ?
007500130208       //--------------------------------------------------------------
007600130208
007700130208       // -?Reperimento dati utente?
007800130208     d TIBS34ds      e ds
007900130208      /copy gaitrasrc/srcProtoPR,TIBS34R
008000130208
008100130208       // -?Costante identificativa del file TABEL per *srvpgm TRULTAB?
008200130208     d c_TABEL         c                   const('1')
008300130208       // -?Costante identificativa del file TNTBE per *srvpgm TRULTAB?
008400130208     d c_TNTBE         c                   const('2')
008500130208       // -?Reperimento dati tabelle?
008600130208      /copy gaitrasrc/srcProtoPR,TRULTAB
008700130208
008800130208       // -?Statistiche Delta per Fasce di Peso?
008900130208     d tiseA2r         pr                  extpgm('TISEA2R')
009000130208     d   kpjba                             likeds(KPJBA)
009100130208
009200130208       //--------------------------------------------------------------
009300130208       //?Definizione key-list.                                        ?
009400130208       //--------------------------------------------------------------
009500130208
009600130208
009700130208       //--------------------------------------------------------------
009800130208       //?M A I N - L I N E                                            ?
009900130208       //--------------------------------------------------------------
010000130208
010100130208     c     *Entry        plist
010200130208     c                   parm                    KPJBA
010300130208
010400130208      /free
010500130208
010600130208       *inLR = *on;
010700130208
010800130208       // -?Ricezione parametri?
010900130208       TISEA21ds = KPJBU;
011000130208
011100130208       // -?Reperimento dati job?
011200130208       exsr  sr_DatiJob;
011300130208
011400130208       // -?Reperimento tab. "5S"=Dati statistiche Sede/Filiale?
011500130208       clear  ds5S;
011600130208       if  getTabella ( c_Tabel : '5S'  : *blank :
011700130208                        *omit : *omit : *omit :
011800130208                        *omit : *omit :
011900130208                        *omit : *omit : *omit : *omit :
012000130208                        *omit : *omit : *omit : *omit :
012100130208                        ds_TNTBE.TBEatb : ds_TNTBE.TBEuni )
012200130208                      = *zero      AND
012300130208           ds_TNTBE.TBEatb = *blank;
012400130208         ds5S   = ds_TNTBE.TBEuni;
012500130208       endif;
012600130208
012700130208       // -?Impostazione parametri per TISEA2R?
012800130208       reset  TISEA2ds;
012900130208
013000130208       // ·?Data Fine = Data Ultimo Aggiornamento - 1 mese?
013100130208       clear  wDateF;
013200130211       If  §5Sdac > *zero;
013300130208         wDateF = %date( §5Sdac : *iso ) - %months(1);
013400130211       EndIf;
013500130211       If  %dec( wDateF ) > ( SE01aamm * 100 ) + 01;
013600130208         wDateF = %date( ( SE01aamm * 100 ) + 01 : *iso );
013700130208       EndIf;
013800130208       SEA2amf  = ( %subdt( wDateF : *years ) * 100 ) +
013900130208                    %subdt( wDateF : *months );
014000130208
014100130208       // ·?Data Inizio = Data Ultima Cancellazione + 1 mese?
014200130208       //                ?(se Data Ultima Cancellazione > Data Fine)??oppure?
014300130208       //                ?Inizio anno della Data Fine?
014400130208       clear  wDateI;
014500130208       if  §5Sdcc > *zero;
014600130208         wDateI = %date( §5Sdcc : *iso ) + %months(1);
014700130208       endif;
014800130208       If  %dec(wDateI) > %subdt( wDateF : *years ) * 10000 + 0101;
014900130208         SEA2ami = ( %subdt( wDateI : *years ) * 100 ) +
015000130208                     %subdt( wDateI : *months );
015100130208       Else;
015200130208         SEA2ami = %subdt( wDateF : *years ) * 100 + 01;
015300130208       EndIf;
015400130208
015500130208       // -?Richiamo diretto *pgm TISEA2R?
015600130208       //KCOAZ = 'SEA2';
015700130208       KPJBU = TISEA2ds;
015800130208       tiseA2r ( kpjba );
015900130208
016000130208       // -?Uscita?
016100130208       return;
016200130208
016300130208       //--------------------------------------------------------------
016400130208       //?Reperimento Dati del job (Utente/Operativi).                 ?
016500130208       //--------------------------------------------------------------
016600130208       BEGSR  sr_DatiJob;
016700130208
016800130208         in(E) §AzUte;
016900130208         if NOT %error;
017000130208           in(E) §DatiUte;
017100130208         endif;
017200130208         if %error or RSut = *blanks;
017300130208           clear TIBS34ds;
017400130208           tibs34r ( tibs34ds );
017500130208           in §AzUte;
017600130208           in §DatiUte;
017700130208         endif;
017800130208
017900130208       ENDSR;
