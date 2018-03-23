000100100413      //---------------------------------------------------------------
000200120109      //?TRULBLCR - Verifica data inserimento blocco cliente
000300100413      //---------------------------------------------------------------
000400100413
000500100413     h decedit('0,') datedit(*ymd/) option(*nodebugio)
000600100413
000700100413      //---------------------------------------------------------------
000800100413      //?Dichiarazione file.
000900100413      //---------------------------------------------------------------
001000110201      // - File Variazioni anagrafiche
001100130221     fCNACVT1L  if   e           k disk
001200130212     fCNACVD1L  if   e           k disk
001300100413      //---------------------------------------------------------------
001400100413      //?Definizione costanti.
001500100413      //---------------------------------------------------------------
001600100413
001700100413      //---------------------------------------------------------------
001800100413      //?Definizione schiere.
001900100413      //---------------------------------------------------------------
002000110323
002100100413      //---------------------------------------------------------------
002200100413      //?Definizione aree dati.
002300100413      //---------------------------------------------------------------
002400100413
002500100413      //---------------------------------------------------------------
002600100413      //?Definizione strutture dati.
002700100413      //---------------------------------------------------------------
002800100415
002900110131      // - Parametri ricevuti
003000100415     d KPJBA         e ds
003100110211
003200120109      // - Ricerca data insrimento blocco cliente
003300120109     d TRULBLCDS     e ds
003400130211
003500130211      // - ds variazione BLOCCO
003600130211     d dACV_Bp       e ds                  extname(dACV_B) prefix(P)
003700130211     d dACV_Bd       e ds                  extname(dACV_B) prefix(D)
003800110131
003900100413      //---------------------------------------------------------------
004000100413      //?Definizione variabili globali.
004100100413      //---------------------------------------------------------------
004200100413
004300100413      // - Flags booleani
004400100413     d $End            s               n   inz(*off)
004500110201     d $SiAcv          s               n   inz(*off)
004600110323
004700120109      // campi di comodo
004800120109     d K_acvkcc        s                   like(ACVkcc)
004900120109     d K_acvksc        s                   like(ACVksc)
005000110201     d wDatBlc         s                   like(ACVdav)
005100110201     d wOraBlc         s                   like(ACVorv)
005200110502
005300100413
005400100413      //---------------------------------------------------------------
005500100413      //?Definizione key-list.
005600100413      //---------------------------------------------------------------
005700100413
005800100413      //---------------------------------------------------------------
005900100413      //?Riepilogo indicatori.
006000100413      //---------------------------------------------------------------
006100100413
006200100413      //---------------------------------------------------------------
006300100413      //?M A I N - L I N E
006400100413      //---------------------------------------------------------------
006500100413
006600100413     c     *Entry        plist
006700110131     c                   parm                    kpjba
006800120109     c                   parm                    Trulblcds
006900100413
007000100413      /free
007100100413
007200100726       //?Elaboro
007300120110       If iblctla <> 'C' and iblcksc > 0;
007400120109         wDatBlc = 0 ;
007500120109         K_acvkcc = iblckcc ;
007600120109         K_acvKsc = iblcksc ;
007700120109         exsr Elabora;
007800120109       Endif ;
007900100413
008000100413       //?Operazioni finali
008100100513       exsr RoutEnd;
008200100413
008300100413       //--------------------------------------------------------------
008400110131       //?Elabora.
008500100413       //--------------------------------------------------------------
008600100513       BEGSR Elabora;
008700100413
008800100413         $End = *off;
008900130221       //?Cerco la data della variazione
009000130221         $SiAcv = *off;
009100130221
009200130221       //?Leggo testata delle variazioni in ordine decrescente
009300130221         setll (K_ACVkcc:K_ACVksc) CNACVT1L;
009400130221         reade (K_ACVkcc:K_ACVksc) CNACVT1L;
009500130221         DOW  not %eof(CNACVT1L);
009600130221           IF  %eof(CNACVT1L);
009700130221             leave;
009800130221           ENDIF;
009900130221           IF  ACVcta = 'M' and
010000130221               %scan('B':ACVflo) > 0;
010100130221             exsr immBLC;
010200130221             IF  $siacv;
010300130221               leave;
010400130221             ENDIF;
010500130221           ENDIF;
010600130221           reade (K_ACVkcc:K_ACVksc) CNACVT1L;
010700130221         ENDDO;
010800130215
010900120109         //?se non trovo variazioni pulisco i campi della data e ora variazione
011000120109         //?e memorizzo la causale blocco
011100130221         IF not $SiAcv and wDatBlc = 0;
011200130221           clear ACVdav;
011300130221           clear ACVorv;
011400130221           wDatBlc = 00010101;
011500130221         ENDIF;
011600120109
011700120109         //?se data più alta memorizzo la data/ora variazione
011800130221         IF  ACVdav > wDatBlc or
011900130221             ACVdav = wDatBlc ;
012000130221           wDatBlc = ACVdav;
012100130221         ENDIF;
012200110315
012300100413       ENDSR;
012400130221
012500130221       //--------------------------------------------------------------
012600130221       //?Cerco l'ultima data immissione BLOCCO.
012700130221       //--------------------------------------------------------------
012800130221       BEGSR immBLC;
012900130221
013000130221       //?Leggo il dettaglio delle variazioni
013100130221         setll (ACVkcc:ACVksc:ACVdav:ACVorv:ACVnoj:'B') CNACVD1L;
013200130221         reade (ACVkcc:ACVksc:ACVdav:ACVorv:ACVnoj:'B') CNACVD1L;
013300130221         DOW  not %eof(CNACVD1L);
013400130221       //?passo i campi "PRIMA" e "DOPO" nelle ds
013500130221           dACV_Bp = ACVprima;
013600130221           dACV_Bd = ACVdopo;
013700130221         //?Se non c'è il blocco "PRIMA" ma c'è il bloccoe "DOPO"
013800130221         //?vuol dire che ho immesso il BLOCCO
013900130221           IF  pACOabl = *blanks and dACOabl <> *blanks;
014000130221             $siAcv = *on;
014100130221             leave;
014200130221           ENDIF;
014300130221           reade (ACVkcc:ACVksc:ACVdav:ACVorv:ACVnoj:'B') CNACVD1L;
014400130221         ENDDO;
014500130221
014600130221       ENDSR;
014700110201
014800100413       //--------------------------------------------------------------
014900100413       //?Operazioni finali.
015000100413       //--------------------------------------------------------------
015100100513       BEGSR RoutEnd;
015200100413
015300120109         oblcdav = WdatBlc ;
015400120109
015500120109         If iblctla = 'L' ;
015600120109           *inRT = *on;
015700120109         else ;
015800120109           *inLR = *on;
015900120109         Endif ;
016000100413
016100100413       ENDSR;
016200100413
016300100413      /end-free
